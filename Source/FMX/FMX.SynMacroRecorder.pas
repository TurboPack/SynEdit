{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

FMX Macro Recorder component. Uses shared macro event types from
SynMacroRecorderShared.pas.
-------------------------------------------------------------------------------}

unit FMX.SynMacroRecorder;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  SynEditTypes,
  SynEditKeyCmds,
  SynMacroRecorderShared;

type
  TCustomFMXSynMacroRecorder = class;

  TSynFMXUserCommandEvent = procedure(aSender: TCustomFMXSynMacroRecorder;
    aCmd: TSynEditorCommand; var aEvent: TSynMacroEvent) of object;

  TCustomFMXSynMacroRecorder = class(TComponent)
  private
    FEditor: TComponent;
    FState: TSynMacroState;
    FEvents: TList;
    FCurrentEditor: TComponent;
    FShortCuts: array[TSynMacroCommand] of TShortCut;
    FOnStateChange: TNotifyEvent;
    FOnUserCommand: TSynFMXUserCommandEvent;
    FMacroName: string;
    FSaveMarkerPos: Boolean;
    function GetEvent(aIndex: Integer): TSynMacroEvent;
    function GetEventCount: Integer;
    function GetIsEmpty: Boolean;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetRecordShortCut: TShortCut;
    function GetPlaybackShortCut: TShortCut;
    procedure SetRecordShortCut(const Value: TShortCut);
    procedure SetPlaybackShortCut(const Value: TShortCut);
    procedure SetEditor(Value: TComponent);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure EditorKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure OnCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: Pointer);
    function CreateMacroEvent(aCmd: TSynEditorCommand): TSynMacroEvent;
    procedure StateChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecordMacro(aEditor: TComponent);
    procedure PlaybackMacro(aEditor: TComponent);
    procedure Stop;
    procedure Pause;
    procedure Resume;
    procedure Clear;
    procedure Error(const aMsg: string);
    // Event management
    procedure AddEvent(aCmd: TSynEditorCommand; aChar: WideChar;
      aData: Pointer);
    procedure InsertEvent(aIndex: Integer; aCmd: TSynEditorCommand;
      aChar: WideChar; aData: Pointer);
    procedure AddCustomEvent(aEvent: TSynMacroEvent);
    procedure InsertCustomEvent(aIndex: Integer; aEvent: TSynMacroEvent);
    procedure DeleteEvent(aIndex: Integer);
    // Serialization
    procedure LoadFromStream(aSrc: TStream);
    procedure LoadFromStreamEx(aSrc: TStream; aClear: Boolean);
    procedure SaveToStream(aDest: TStream);
    procedure LoadFromFile(aFilename: string);
    procedure SaveToFile(aFilename: string);
    // Properties
    property Editor: TComponent read FEditor write SetEditor;
    property State: TSynMacroState read FState;
    property IsEmpty: Boolean read GetIsEmpty;
    property EventCount: Integer read GetEventCount;
    property Events[aIndex: Integer]: TSynMacroEvent read GetEvent;
    property AsString: string read GetAsString write SetAsString;
    property MacroName: string read FMacroName write FMacroName;
    property SaveMarkerPos: Boolean read FSaveMarkerPos
      write FSaveMarkerPos default False;
    property RecordShortCut: TShortCut read GetRecordShortCut
      write SetRecordShortCut;
    property PlaybackShortCut: TShortCut read GetPlaybackShortCut
      write SetPlaybackShortCut;
    property OnStateChange: TNotifyEvent read FOnStateChange
      write FOnStateChange;
    property OnUserCommand: TSynFMXUserCommandEvent read FOnUserCommand
      write FOnUserCommand;
  end;

  TFMXSynMacroRecorder = class(TCustomFMXSynMacroRecorder)
  published
    property Editor;
    property SaveMarkerPos;
    property RecordShortCut;
    property PlaybackShortCut;
    property OnStateChange;
    property OnUserCommand;
  end;

implementation

uses
  FMX.SynEdit,
  SynEditMiscProcs;

{ Helper to convert TShiftState to shortcut modifier flags }
function MakeShortCut(Key: Word; Shift: TShiftState): TShortCut;
begin
  Result := Key;
  if ssShift in Shift then
    Result := Result or scShift;
  if ssCtrl in Shift then
    Result := Result or scCtrl;
  if ssAlt in Shift then
    Result := Result or scAlt;
end;

{ TCustomFMXSynMacroRecorder }

constructor TCustomFMXSynMacroRecorder.Create(AOwner: TComponent);
begin
  inherited;
  FMacroName := 'unnamed';
  FShortCuts[mcRecord] := Ord('R') or scCtrl or scShift;
  FShortCuts[mcPlayback] := Ord('P') or scCtrl or scShift;
end;

destructor TCustomFMXSynMacroRecorder.Destroy;
begin
  Editor := nil;
  Clear;
  inherited;
end;

procedure TCustomFMXSynMacroRecorder.SetEditor(Value: TComponent);
begin
  if FEditor = Value then
    Exit;

  // Unhook from old editor
  if FEditor <> nil then
  begin
    FEditor.RemoveFreeNotification(Self);
    if FEditor is TCustomFMXSynEdit then
    begin
      TCustomFMXSynEdit(FEditor).RemoveKeyDownHandler(EditorKeyDown);
      TCustomFMXSynEdit(FEditor).UnregisterCommandHandler(OnCommand);
    end;
  end;

  FEditor := Value;

  // Hook into new editor
  if FEditor <> nil then
  begin
    FEditor.FreeNotification(Self);
    if FEditor is TCustomFMXSynEdit then
    begin
      TCustomFMXSynEdit(FEditor).AddKeyDownHandler(EditorKeyDown);
      TCustomFMXSynEdit(FEditor).RegisterCommandHandler(OnCommand, nil);
    end;
  end;
end;

procedure TCustomFMXSynMacroRecorder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FEditor) then
  begin
    // Editor is being destroyed - unhook without calling back
    FEditor := nil;
    if FState <> msStopped then
    begin
      FState := msStopped;
      FCurrentEditor := nil;
    end;
  end;
end;

procedure TCustomFMXSynMacroRecorder.EditorKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
var
  SC: TShortCut;
begin
  if Key = 0 then Exit;
  SC := MakeShortCut(Key, Shift);

  if SC = FShortCuts[mcRecord] then
  begin
    case FState of
      msStopped: RecordMacro(TComponent(Sender));
      msRecording: Stop;
    end;
    Key := 0;
  end
  else if SC = FShortCuts[mcPlayback] then
  begin
    case FState of
      msStopped: PlaybackMacro(TComponent(Sender));
      msRecording: Pause;
      msPaused: Resume;
    end;
    Key := 0;
  end;
end;

procedure TCustomFMXSynMacroRecorder.OnCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar;
  Data: Pointer; HandlerData: Pointer);
var
  iEvent: TSynMacroEvent;
begin
  if AfterProcessing then
  begin
    if (Sender = FCurrentEditor) and (FState = msRecording) and
      (not Handled) then
    begin
      iEvent := CreateMacroEvent(Command);
      iEvent.Initialize(Command, AChar, Data);
      FEvents.Add(iEvent);
      if SaveMarkerPos and (Command >= ecSetMarker0) and
        (Command <= ecSetMarker9) and (Data = nil) then
      begin
        TSynPositionEvent(iEvent).Position :=
          TCustomFMXSynEdit(FCurrentEditor).CaretXY;
      end;
    end;
  end;
end;

function TCustomFMXSynMacroRecorder.CreateMacroEvent(
  aCmd: TSynEditorCommand): TSynMacroEvent;

  function WantDefaultEvent(var aEvent: TSynMacroEvent): Boolean;
  begin
    if Assigned(FOnUserCommand) then
      FOnUserCommand(Self, aCmd, aEvent);
    Result := aEvent = nil;
  end;

begin
  case aCmd of
    ecGotoXY, ecSelGotoXY, ecSetMarker0..ecSetMarker9:
      begin
        Result := TSynPositionEvent.Create;
        TSynPositionEvent(Result).Command := aCmd;
      end;
    ecChar:
      Result := TSynCharEvent.Create;
    ecString:
      Result := TSynStringEvent.Create;
  else
    begin
      Result := nil;
      if (aCmd < ecUserFirst) or WantDefaultEvent(Result) then
      begin
        Result := TSynBasicEvent.Create;
        TSynBasicEvent(Result).Command := aCmd;
      end;
    end;
  end;
end;

procedure TCustomFMXSynMacroRecorder.RecordMacro(aEditor: TComponent);
begin
  if FState <> msStopped then
    Error(sCannotRecord);
  Clear;
  FEvents := TList.Create;
  FEvents.Capacity := 512;
  FState := msRecording;
  FCurrentEditor := aEditor;
  StateChanged;
end;

procedure TCustomFMXSynMacroRecorder.PlaybackMacro(aEditor: TComponent);
var
  cEvent: Integer;
  Ed: TCustomFMXSynEdit;
begin
  if FState <> msStopped then
    Error(sCannotPlay);
  FState := msPlaying;
  try
    StateChanged;
    if aEditor is TCustomFMXSynEdit then
    begin
      Ed := TCustomFMXSynEdit(aEditor);
      for cEvent := 0 to EventCount - 1 do
      begin
        Events[cEvent].PlaybackTo(Ed.CommandProcessor);
        if FState <> msPlaying then
          Break;
      end;
    end;
  finally
    if FState = msPlaying then
    begin
      FState := msStopped;
      StateChanged;
    end;
  end;
end;

procedure TCustomFMXSynMacroRecorder.Stop;
begin
  if FState = msStopped then
    Exit;
  FState := msStopped;
  FCurrentEditor := nil;
  if (FEvents <> nil) and (FEvents.Count = 0) then
    FreeAndNil(FEvents);
  StateChanged;
end;

procedure TCustomFMXSynMacroRecorder.Pause;
begin
  if FState <> msRecording then
    Error(sCannotPause);
  FState := msPaused;
  StateChanged;
end;

procedure TCustomFMXSynMacroRecorder.Resume;
begin
  if FState <> msPaused then
    Error(sCannotResume);
  FState := msRecording;
  StateChanged;
end;

procedure TCustomFMXSynMacroRecorder.StateChanged;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TCustomFMXSynMacroRecorder.Error(const aMsg: string);
begin
  raise Exception.Create(aMsg);
end;

procedure TCustomFMXSynMacroRecorder.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  if Assigned(FEvents) then
  begin
    for I := FEvents.Count - 1 downto 0 do
    begin
      Obj := FEvents[I];
      FEvents.Delete(I);
      Obj.Free;
    end;
    FreeAndNil(FEvents);
  end;
end;

procedure TCustomFMXSynMacroRecorder.AddEvent(aCmd: TSynEditorCommand;
  aChar: WideChar; aData: Pointer);
begin
  InsertEvent(EventCount, aCmd, aChar, aData);
end;

procedure TCustomFMXSynMacroRecorder.InsertEvent(aIndex: Integer;
  aCmd: TSynEditorCommand; aChar: WideChar; aData: Pointer);
var
  iEvent: TSynMacroEvent;
begin
  iEvent := CreateMacroEvent(aCmd);
  try
    iEvent.Initialize(aCmd, aChar, aData);
    InsertCustomEvent(aIndex, iEvent);
  except
    iEvent.Free;
    raise;
  end;
end;

procedure TCustomFMXSynMacroRecorder.AddCustomEvent(aEvent: TSynMacroEvent);
begin
  InsertCustomEvent(EventCount, aEvent);
end;

procedure TCustomFMXSynMacroRecorder.InsertCustomEvent(aIndex: Integer;
  aEvent: TSynMacroEvent);
begin
  if FEvents = nil then
    FEvents := TList.Create;
  FEvents.Insert(aIndex, aEvent);
end;

procedure TCustomFMXSynMacroRecorder.DeleteEvent(aIndex: Integer);
var
  iObj: Pointer;
begin
  iObj := FEvents[aIndex];
  FEvents.Delete(aIndex);
  TObject(iObj).Free;
end;

function TCustomFMXSynMacroRecorder.GetEvent(
  aIndex: Integer): TSynMacroEvent;
begin
  Result := TSynMacroEvent(FEvents[aIndex]);
end;

function TCustomFMXSynMacroRecorder.GetEventCount: Integer;
begin
  if FEvents = nil then
    Result := 0
  else
    Result := FEvents.Count;
end;

function TCustomFMXSynMacroRecorder.GetIsEmpty: Boolean;
begin
  Result := (FEvents = nil) or (FEvents.Count = 0);
end;

procedure TCustomFMXSynMacroRecorder.LoadFromStream(aSrc: TStream);
begin
  LoadFromStreamEx(aSrc, True);
end;

procedure TCustomFMXSynMacroRecorder.LoadFromStreamEx(aSrc: TStream;
  aClear: Boolean);
var
  iCommand: TSynEditorCommand;
  iEvent: TSynMacroEvent;
  cnt, i: Integer;
begin
  Stop;
  if aClear then
    Clear;
  FEvents := TList.Create;
  aSrc.Read(cnt, SizeOf(cnt));
  i := 0;
  FEvents.Capacity := aSrc.Size div SizeOf(TSynEditorCommand);
  while (aSrc.Position < aSrc.Size) and (i < cnt) do
  begin
    aSrc.Read(iCommand, SizeOf(TSynEditorCommand));
    iEvent := CreateMacroEvent(iCommand);
    iEvent.Initialize(iCommand, #0, nil);
    iEvent.LoadFromStream(aSrc);
    FEvents.Add(iEvent);
    Inc(i);
  end;
end;

procedure TCustomFMXSynMacroRecorder.SaveToStream(aDest: TStream);
var
  cEvent, eCnt: Integer;
begin
  eCnt := EventCount;
  aDest.Write(eCnt, SizeOf(eCnt));
  for cEvent := 0 to eCnt - 1 do
    Events[cEvent].SaveToStream(aDest);
end;

procedure TCustomFMXSynMacroRecorder.LoadFromFile(aFilename: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadFromStream(F);
    MacroName := ChangeFileExt(ExtractFileName(aFilename), '');
  finally
    F.Free;
  end;
end;

procedure TCustomFMXSynMacroRecorder.SaveToFile(aFilename: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFilename, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

function TCustomFMXSynMacroRecorder.GetAsString: string;
var
  i: Integer;
  eStr: string;
begin
  Result := 'macro ' + MacroName + #13#10 + 'begin' + #13#10;
  if Assigned(FEvents) then
  begin
    for i := 0 to FEvents.Count - 1 do
    begin
      eStr := Events[i].AsString;
      if eStr <> '' then
        Result := Result + '  ' + eStr + #13#10;
    end;
  end;
  Result := Result + 'end';
end;

procedure TCustomFMXSynMacroRecorder.SetAsString(const Value: string);
var
  i, p, Cmd: Integer;
  S: TStrings;
  cmdStr: string;
  iEvent: TSynMacroEvent;
begin
  Stop;
  Clear;
  FEvents := TList.Create;
  S := TStringList.Create;
  try
    S.Text := Value;
    for i := 0 to S.Count - 1 do
    begin
      cmdStr := Trim(S[i]);
      p := Pos(' ', cmdStr);
      if p = 0 then
        p := Length(cmdStr) + 1;
      Cmd := ecNone;
      if IdentToEditorCommand(Copy(cmdStr, 1, p - 1), Cmd) then
      begin
        Delete(cmdStr, 1, p);
        iEvent := CreateMacroEvent(Cmd);
        try
          FEvents.Add(iEvent);
          iEvent.InitEventParameters(cmdStr);
        except
          iEvent.Free;
        end;
      end;
    end;
  finally
    S.Free;
  end;
end;

function TCustomFMXSynMacroRecorder.GetRecordShortCut: TShortCut;
begin
  Result := FShortCuts[mcRecord];
end;

function TCustomFMXSynMacroRecorder.GetPlaybackShortCut: TShortCut;
begin
  Result := FShortCuts[mcPlayback];
end;

procedure TCustomFMXSynMacroRecorder.SetRecordShortCut(
  const Value: TShortCut);
begin
  FShortCuts[mcRecord] := Value;
end;

procedure TCustomFMXSynMacroRecorder.SetPlaybackShortCut(
  const Value: TShortCut);
begin
  FShortCuts[mcPlayback] := Value;
end;

end.
