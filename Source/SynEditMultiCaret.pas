{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is SynEditWordWrap.pas by Flávio Etrusco, released 2003-12-11.
  Unicode translation by Maël Hörz.
  All Rights Reserved.

  Contributors to the SynEdit and mwEdit projects are listed in the
  Contributors.txt file.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.
  ------------------------------------------------------------------------------- }
unit SynEditMultiCaret;

interface
uses
  Math,
  Graphics,
  SysUtils,
  ExtCtrls,
  Classes,
  SynEditKeyCmds,
  SynEditTextBuffer,
  SynEditTypes,
  System.Types,
  System.Generics.Collections;

const
  // Editor commands that will be intercepted and executed in SandBox
  SANDBOX_COMMANDS: array[0..12] of Integer = (ecChar, ecPaste, ecLineBreak,
    ecMoveLineDown, ecMoveLineUp, ecCopyLineDown, ecCopyLineUp,
    ecDeleteLastChar, ecDeleteChar, ecSelLeft, ecSelRight, ecSelUp, ecSelDown);

type

  TCaretItem = class
  type
    TOnMoved = procedure(Sender: TCaretItem; const PointFrom: TPoint;
      const PointTo: TPoint) of object;
    TOnSelectionChanged = procedure(Sender: TCaretItem; const ValueFrom: TSelection;
      const ValueTo: TSelection) of object;
    TOnVisibleChanged = procedure(Sender: TCaretItem) of object;
  strict private
    // review: Do we need to store the index?
    FIndex: Integer;
    // review:  Instead of Client Coordinates I think it is better just to store
    // TBufferCoord.  (Pixel coordinates are volatile, resizing window etc.)
    // You can always conver Buffer coordinatest to DisplayPos and then to Client
    // coordinates
    // In this way TCaretItem would map directly to CaretXY which is TBufferCoord
    FPosX: Integer;
    FPosY: Integer;
    FVisible: Boolean;
    FSelection: TSelection;
    FOnMoved: TOnMoved;
    FOnVisibleChanged: TOnVisibleChanged;
    FOnSelectionChanged: TOnSelectionChanged;
    procedure SetPosX(const Value: Integer);
    procedure SetPosY(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetSelection(const Value: TSelection);
  protected
    property Index: Integer read FIndex write FIndex;
    property OnMoved: TOnMoved read FOnMoved write FOnMoved;
    property OnSelectionChanged: TOnSelectionChanged read FOnSelectionChanged
      write FOnSelectionChanged;
    property OnVisibleChanged: TOnVisibleChanged read FOnVisibleChanged
      write FOnVisibleChanged;
    procedure SaveToStream(S: TStream);
    function LoadFromStream(S: TStream): Boolean;
  public
    constructor Create; overload;
    constructor Create(PosX, PosY: Integer); overload;
    destructor Destroy; override;
    function ToPoint: TPoint;
    property PosX: Integer read FPosX write SetPosX;
    property PosY: INteger read FPosY write SetPosY;
    property Visible: Boolean read FVisible write SetVisible;
    property Selection: TSelection read FSelection write SetSelection;
  end;

  TCarets = class
  strict private
    FList: TList<TCaretItem>;
    FLine: TList<TCaretItem>;
    FColumn: TList<TCaretItem>;
    FSortedList: TList<TCaretItem>;
    FOnChanged: TNotifyEvent;
    FOnBeforeClear: TNotifyEvent;
    FOnAfterClear: TNotifyEvent;
    FOnBeforeCaretDelete: TNotifyEvent;
    function GetItem(Index: Integer): TCaretItem;
    function CompareCarets(const Left, Right: TCaretItem): Integer;
  private
    FDefaultCaret: TCaretItem;
    function GetDefaultCaretSafe: TCaretItem;
    procedure SaveToStream(S: TStream);
    function LoadFromStream(S: TStream): Boolean;
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnBeforeClear: TNotifyEvent read FOnBeforeClear
      write FOnBeforeClear;
    property OnAfterClear: TNotifyEvent read FOnAfterClear write FOnAfterClear;
    property OnBeforeCaretDelete: TNotifyEvent read FOnBeforeCaretDelete
      write FOnBeforeCaretDelete;
    function GetLineNeighboursOnRight(Caret: TCaretItem): TList<TCaretItem>;
    function GetColumnNeighboursOnBottom(Caret: TCaretItem): TList<TCaretItem>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(APosX, APosY: Integer): TCaretItem;
    function NewDefaultCaret: TCaretItem;
    procedure Clear(ExcludeDefaultCaret: Boolean = True);
    procedure Delete(Index: Integer);
    function Count: Integer;
    function InRange(N: Integer): Boolean;
    function Sorted:  TList<TCaretItem>;
    property Items[N: Integer]: TCaretItem read GetItem; default;
    property DefaultCaret: TCaretItem read GetDefaultCaretSafe;
    function IndexOf(APosX, APosY: Integer): Integer;
    function IsLineListed(APosY: Integer): Boolean;
    function GetEnumerator: TEnumerator<TCaretItem>;
    function Store: TBytes;
    function Load(const B: TBytes): Boolean;
  end;

  IAbstractEditor = interface
    function GetCanvas: TCanvas;
    function GetClientRect: TRect;
    function GetUndoList: TSynEditUndoList;
    function GetBlockBegin: TBufferCoord;
    function GetBlockEnd: TBufferCoord;
    function GetCaretXY: TBufferCoord;
    function GetDisplayXY: TDisplayCoord;
    function GetLines: TStrings;
    function DisplayCoord2CaretXY(const Coord: TDisplayCoord): TPoint;
    function PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TBufferCoord);
    property Canvas: TCanvas read GetCanvas;
    property ClientRect: TRect read GetClientRect;
    property UndoList: TSynEditUndoList read GetUndoList;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property Lines: TStrings read GetLines;
    procedure ComputeCaret(X, Y: Integer);
    procedure RegisterCommandHandler(const AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    function BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Refresh;
  end;

  TCaretShape = record
    Width: Integer;
    Height: Integer;
    Offset: TPoint;
  public
    constructor Create(const aWidth, aHeight: Integer); overload;
    constructor Create(const aWidth, aHeight: Integer; const aOffset: TPoint); overload;
    procedure SetToDefault;
    class operator Equal(a: TCaretShape; b: TCaretShape): Boolean;
  end;

  TMultiCaretController = class
  strict private
    {$IFDEF DEBUG}
    FLastDebugState: string;
    {$ENDIF}
    FEditor: IAbstractEditor;
    FCarets: TCarets;
    FBlinkTimer: TTimer;
    FShown: Boolean;
    FActive: Boolean;
    FShape: TCaretShape;
    FCommandsList: TList<Integer>;
    FSandBoxContext: Boolean;
    FCommand: TSynEditorCommand;
    FIsDestroying: Boolean;
    procedure RefreshSelection;
    function CaretPointToRect(const CaretPoint: TPoint): TRect;
    procedure ClearSelection;
    procedure SetActive(const Value: Boolean);
    procedure SetShape(const Value: TCaretShape);
    procedure InvertCaretsRects;
    procedure Blink(Sender: TObject);
    procedure DoCaretsChanged(Sender: TObject);
    procedure DoBeforeCaretsClear(Sender: TObject);
    procedure DoAfterCaretsClear(Sender: TObject);
    procedure DoCaretMoved(Sender: TCaretItem; const PointFrom: TPoint;
      const PointTo: TPoint);
    procedure DefaultCaretMoved(const PointFrom: TPoint; const PointTo: TPoint);
    procedure DoCaretSelectionChanged(Sender: TCaretItem; const ValueFrom: TSelection;
      const ValueTo: TSelection);
    procedure DoCaretVisibleChanged(Sender: TCaretItem);
    // Entry point of SandBox for executing commands
    procedure EditorCommandSandBoxEntryPoint(Sender: TObject;
      AfterProcessing: Boolean; var Handled: Boolean;
      var Command: TSynEditorCommand; var AChar: WideChar;
      Data: pointer; HandlerData: pointer);
    procedure SandBox(Command: TSynEditorCommand; AChar: WideChar;
      Data: Pointer);
    procedure HandleInputAction(Command: TSynEditorCommand; AChar: WideChar;
      Data: Pointer);
    procedure HandleSelectionAction(Command: TSynEditorCommand; AChar: WideChar;
      Data: Pointer); 
    function IsSelectionCommand(Command: TSynEditorCommand): Boolean;
  public
    constructor Create(Editor: IAbstractEditor);
    destructor Destroy; override;
    procedure Paint;
    procedure Flash;
    procedure MoveY(Delta: Integer);
    procedure MoveX(Delta: Integer);
    procedure Unselect;
    property Active: Boolean read FActive write SetActive;
    property Carets: TCarets read FCarets;
    property Shape: TCaretShape read FShape write SetShape;
    function Exists(const PosX: Integer; const PosY: Integer): Boolean;
    function HasSelection: Boolean;
    procedure CalcMultiSelection(out Values: TMultiSelectionArray);
    {$IFDEF DEBUG}
    procedure ShowDebugState;
    procedure InvertShown;
    {$ENDIF}
  end;

implementation
uses Windows, System.Generics.Defaults;


{ TCarets }

function TCarets.Add(APosX, APosY: Integer): TCaretItem;
begin
  Result := TCaretItem.Create(APosX, APosY);
  FList.Add(Result);
  Result.Index := FList.Count-1;
  if Assigned(FOnChanged) then
    FOnChanged(Self)
end;

procedure TCarets.Clear(ExcludeDefaultCaret: Boolean);
var
  Item: TCaretItem;
  Def: TCaretItem;
begin
  if ExcludeDefaultCaret then begin
    if Count < 2 then
      Exit;
    if Assigned(FOnBeforeClear) then
      FOnBeforeClear(Self);
    Def := DefaultCaret;
    for Item in FList do begin
      if Item <> Def then
        Item.Free;
    end;
    FList.Clear;
    FList.Add(Def);
    Def.Index := 0;
    if Assigned(FOnAfterClear) then
      FOnAfterClear(Self);
  end
  else begin
    if Count < 1 then
      Exit;
    if Assigned(FOnBeforeClear) then
      FOnBeforeClear(Self);
    for Item in FList do
      Item.Free;
    FList.Clear;
    FDefaultCaret := nil;
    if Assigned(FOnAfterClear) then
      FOnAfterClear(Self);
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self)
end;

function TCarets.CompareCarets(const Left, Right: TCaretItem): Integer;
begin
  Result := Left.PosY - Right.PosY;
  if Result = 0 then
    Result := Left.PosX - Right.PosX
end;

function TCarets.Count: Integer;
begin
  Result := FList.Count
end;

constructor TCarets.Create;
begin
  inherited;
  FList:= TList<TCaretItem>.Create;
  FLine:= TList<TCaretItem>.Create;
  FColumn := TList<TCaretItem>.Create;
  FSortedList := TList<TCaretItem>.Create;
end;

procedure TCarets.Delete(Index: Integer);
var
  I: Integer;
begin
  if InRange(Index) then
  begin
    if Assigned(FOnBeforeCaretDelete) then
      FOnBeforeCaretDelete(FList[Index]);
    if FList[Index] = FDefaultCaret then
      FDefaultCaret := nil;
    FList[Index].Free;
    FList.Delete(Index);
    for I := Index to FList.Count-1 do
      FList[I].Index := FList[I].Index - 1;
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self)
end;

destructor TCarets.Destroy;
begin
  Clear(False);
  FList.Free;
  FLine.Free;
  FColumn.Free;
  FSortedList.Free;
  inherited;
end;

function TCarets.GetColumnNeighboursOnBottom(
  Caret: TCaretItem): TList<TCaretItem>;
var
  Iter: TCaretItem;
begin
  FColumn.Clear;
  for Iter in FList do begin
    if (Iter <> Caret) and (Iter.PosY > Caret.PosY) then begin
      FColumn.Add(Iter)
    end;
  end;
  FColumn.Sort(TComparer<TCaretItem>.Construct(CompareCarets));
  Result := FColumn;
end;

function TCarets.GetDefaultCaretSafe: TCaretItem;
var
  Caret: TCaretItem;
begin
  if FList.Count = 0 then begin
    Caret := Add(0, 0);
    Caret.Visible := False;
    FDefaultCaret := Caret;
  end;
  Result := FDefaultCaret;
end;

function TCarets.GetEnumerator: TEnumerator<TCaretItem>;
begin
  Result := FList.GetEnumerator
end;

function TCarets.GetItem(Index: Integer): TCaretItem;
begin
  if InRange(Index) then
    Result:= FList[Index]
  else
    Result:= nil;
end;

function TCarets.GetLineNeighboursOnRight(Caret: TCaretItem): TList<TCaretItem>;
var
  Iter: TCaretItem;
begin
  FLine.Clear;
  for Iter in FList do begin
    if (Iter <> Caret) and (Iter.PosY = Caret.PosY) and (Iter.PosX > Caret.PosX) then begin
      FLine.Add(Iter)
    end;
  end;
  FLine.Sort(TComparer<TCaretItem>.Construct(CompareCarets));
  Result := FLine;
end;

function TCarets.IndexOf(APosX, APosY: Integer): Integer;
var
  I: Integer;
  Item: TCaretItem;
begin
  Result:= -1;
  for I := 0 to FList.Count-1 do begin
    Item := FList[I];
    if (Item.PosX = APosX) and (Item.PosY = APosY)  then
      Exit(I)
  end;
end;

function TCarets.InRange(N: Integer): Boolean;
begin
  Result := Math.InRange(N, 0, FList.Count-1)
end;

function TCarets.IsLineListed(APosY: Integer): boolean;
var
  Item: TCaretItem;
begin
  Result:= False;
  for Item in FList do begin
    if Item.PosY = APosY then
      Exit(True)
  end;
end;

function TCarets.Load(const B: TBytes): Boolean;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    M.Write(Pointer(B)^, Length(B));
    M.Seek(0, soFromBeginning);
    Result := LoadFromStream(M);
  finally
    M.Free
  end;
end;

function TCarets.LoadFromStream(S: TStream): Boolean;
var
  DefCaretIndex, Count, I, J: Integer;
  Pos, LastPos: Int64;
  NewList: TList<TCaretItem>;
  Caret: TCaretItem;
begin
  Result := False;
  Pos := S.Position;
  LastPos := S.Seek(0, soFromEnd);
  if (LastPos - Pos) < (SizeOf(DefCaretIndex) + SizeOf(Count)) then
    Exit(False);
  S.Position := Pos;
  S.Read(DefCaretIndex, SizeOf(DefCaretIndex));
  S.Read(Count, SizeOf(Count));
  NewList := TList<TCaretItem>.Create;
  try
    try
      for I := 1 to Count do begin
        Caret := TCaretItem.Create;
        Caret.Index := I-1;
        if not Caret.LoadFromStream(S) then
          Abort;
        NewList.Add(Caret);
      end;
      Clear(False);
      for I := 0 to NewList.Count-1 do begin
        Caret := NewList[I];
        FList.Add(Caret);
        Caret.Index := I;
        if Caret.Index = DefCaretIndex then
          FDefaultCaret := Caret;
      end;
      Result := True;
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    except on EAbort do
      for J := 0 to NewList.Count-1 do
        NewList[J].Free;
    end;
  finally
    NewList.Free
  end;
end;

function TCarets.NewDefaultCaret: TCaretItem;
begin
  Result := Add(0, 0);
  FDefaultCaret := Result;
end;

procedure TCarets.SaveToStream(S: TStream);
var
  DefCaretIndex, Count: Integer;
  Caret: TCaretItem;
begin
  if Assigned(FDefaultCaret) then
    DefCaretIndex := FDefaultCaret.Index
  else
    DefCaretIndex := -1;
  S.Write(DefCaretIndex, SizeOf(DefCaretIndex));
  Count := FList.Count;
  S.Write(Count, SizeOf(Count));
  for Caret in FList do
    Caret.SaveToStream(S);
end;

function TCarets.Sorted: TList<TCaretItem>;
var
  Iter: TCaretItem;
begin
  FSortedList.Clear;
  for Iter in FList do
    FSortedList.Add(Iter);
  FSortedList.Sort(TComparer<TCaretItem>.Construct(CompareCarets));
  Result := FSortedList;
end;

function TCarets.Store: TBytes;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    SaveToStream(M);
    SetLength(Result, M.Position);
    M.Seek(0, soFromBeginning);
    M.Read(Pointer(Result)^, Length(Result))
  finally
    M.Free
  end;
end;

{ TCaretItem }

constructor TCaretItem.Create;
begin
  FPosX := -1;
  FPosY := -1;
  FVisible := True;
  FSelection := TSelection.Empty;
end;

constructor TCaretItem.Create(PosX, PosY: Integer);
begin
  Create;
  FPosX := PosX;
  FPosY := PosY;
end;

destructor TCaretItem.Destroy;
begin
  // Raise events to repaint Editor rect
  Visible := False;
  inherited;
end;

function TCaretItem.LoadFromStream(S: TStream): Boolean;
begin
  Result := (S.Read(FPosX, SizeOf(FPosX)) = SizeOf(FPosX))
    and (S.Read(FPosY, SizeOf(FPosY)) = SizeOf(FPosY))
    and (S.Read(FVisible, SizeOf(FVisible)) = SizeOf(FVisible));
  if Result then
    Selection := TSelection.Empty
end;

procedure TCaretItem.SaveToStream(S: TStream);
begin
  S.Write(FPosX, SizeOf(FPosX));
  S.Write(FPosY, SizeOf(FPosY));
  S.Write(FVisible, SizeOf(FVisible));
end;

procedure TCaretItem.SetPosX(const Value: Integer);
var
  PointFrom: TPoint;
begin
  if Value <> FPosX then begin
    PointFrom := ToPoint;
    FPosX := Value;
    if Assigned(FOnMoved) then
      FOnMoved(Self, PointFrom, ToPoint)
  end;
end;

procedure TCaretItem.SetPosY(const Value: Integer);
var
  PointFrom: TPoint;
begin
  if Value <> FPosY then begin
    PointFrom := ToPoint;
    FPosY := Value;
    if Assigned(FOnMoved) then
      FOnMoved(Self, PointFrom, ToPoint)
  end;
end;

procedure TCaretItem.SetSelection(const Value: TSelection);
var
  ValueFrom: TSelection;

begin
  if FSelection <> Value then begin
    ValueFrom := FSelection;
    FSelection := Value;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self, ValueFrom, Value)
  end;
end;

procedure TCaretItem.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then begin
    FVisible := Value;
    if Assigned(FOnVisibleChanged) then
      FOnVisibleChanged(Self)
  end;
end;

function TCaretItem.ToPoint: TPoint;
begin
  Result := Point(FPosX, FPosY);
end;

{ TMultiCaretController }

procedure TMultiCaretController.Blink(Sender: TObject);
begin
  FShown := not FShown;
  InvertCaretsRects;
  ShowDebugState;
end;

procedure TMultiCaretController.CalcMultiSelection(
  out Values: TMultiSelectionArray);
var
  ActualSelCount: Integer;
  CaretItem: TCaretItem;

begin
  SetLength(Values, 0);
  if HasSelection then begin
    ActualSelCount := 0;
    SetLength(Values, FCarets.Count);
    for CaretItem in FCarets.Sorted do begin
      if not CaretItem.Selection.IsEmpty then begin
        Values[ActualSelCount] := CaretItem.Selection.Normalize;
        Inc(ActualSelCount);
      end;
    end;
    SetLength(Values, ActualSelCount);
  end;
end;

function TMultiCaretController.CaretPointToRect(const CaretPoint: TPoint): TRect;
var
  P: TPoint;
  CaretHeight, CaretWidth: Integer;

begin
  CaretHeight := FShape.Height;
  CaretWidth := FShape.Width;
  P := CaretPoint;
  Inc(P.Y, FShape.Offset.Y);
  Inc(P.X, FShape.Offset.X);
  Result := Rect(P.X, P.Y, P.X + CaretWidth, P.Y + CaretHeight);
end;

procedure TMultiCaretController.ClearSelection;
var
  Caret: TCaretItem;
begin
  for Caret in FCarets do
    Caret.Selection := TSelection.Empty
end;

constructor TMultiCaretController.Create(Editor: IAbstractEditor);
var
  I: Integer;

begin
  FBlinkTimer := TTimer.Create(nil);
  FBlinkTimer.Interval := GetCaretBlinkTime;
  FBlinkTimer.OnTimer := Blink;
  FBlinkTimer.Enabled := False;

  FCarets := TCarets.Create;
  FCarets.OnChanged := DoCaretsChanged;
  FCarets.OnBeforeClear := DoBeforeCaretsClear;
  FCarets.OnAfterClear := DoAfterCaretsClear;

  FCommandsList := TList<Integer>.Create;
  for I := 0 to High(SANDBOX_COMMANDS) do
    FCommandsList.Add(SANDBOX_COMMANDS[I]);
  FCommandsList.Sort;

  FEditor := Editor;
  FEditor.RegisterCommandHandler(EditorCommandSandBoxEntryPoint, nil);
end;

procedure TMultiCaretController.DefaultCaretMoved(const PointFrom,
  PointTo: TPoint);
var
  Caret: TCaretItem;
  Delta: TPoint;
  NewPos: TPoint;
begin
  if FSandBoxContext then begin
    if not IsSelectionCommand(FCommand) then begin
      Delta.X := PointTo.X - PointFrom.X;
      Delta.Y := PointTo.Y - PointFrom.Y;
      for Caret in FCarets do begin
        if Caret = Carets.FDefaultCaret then
          Continue;
        // prepare
        NewPos := TPoint.Create(Caret.PosX + Delta.X, Caret.PosY + Delta.Y);
        NewPos := FEditor.DisplayCoord2CaretXY(FEditor.PixelsToNearestRowColumn(NewPos.X, NewPos.Y));
        // apply new values
        Caret.PosX := NewPos.X;
        Caret.PosY := NewPos.Y;
      end;
    end;
  end;
end;

destructor TMultiCaretController.Destroy;
begin
  FIsDestroying := True;
  FBlinkTimer.Free;
  FCarets.Free;
  FCommandsList.Free;
  inherited;
end;

procedure TMultiCaretController.InvertCaretsRects;
var
  Caret: TCaretItem;
  R, R2: TRect;

  procedure ProcessCaret(Crt: TCaretItem);
  begin
    if Crt.Visible then begin
      R := CaretPointToRect(Crt.ToPoint);
      if IntersectRect(R2, R, FEditor.GetClientRect) then
        InvertRect(FEditor.GetCanvas.Handle, R);
    end;
  end;

begin
  for Caret in FCarets do
    ProcessCaret(Caret);
end;


procedure TMultiCaretController.InvertShown;
begin
  FShown := not FShown
end;

function TMultiCaretController.IsSelectionCommand(Command: TSynEditorCommand): Boolean;
begin
  Result := InRange(Command, ecSelLeft, ecSelGotoXY)
end;

procedure TMultiCaretController.MoveX(Delta: Integer);
var
  Caret: TCaretItem;
begin
  for Caret in FCarets do begin
    Caret.PosX := Caret.PosX + Delta
  end;
end;

procedure TMultiCaretController.MoveY(Delta: Integer);
var
  Caret: TCaretItem;
begin
  for Caret in FCarets do begin
    Caret.PosY := Caret.PosY + Delta
  end;
end;

procedure TMultiCaretController.DoAfterCaretsClear(Sender: TObject);
begin
  if FIsDestroying then Exit;

  if FShown then
    InvertCaretsRects;
  if HasSelection then begin
    ClearSelection;
    FEditor.SetSelectionMode(smNormal);
    FEditor.Refresh;
  end;
  RefreshSelection;
end;

procedure TMultiCaretController.DoBeforeCaretsClear(Sender: TObject);
begin
  if not FIsDestroying and FShown then
    InvertCaretsRects;
end;

procedure TMultiCaretController.DoCaretMoved(Sender: TCaretItem;
  const PointFrom, PointTo: TPoint);
var
  RectFrom, RectTo, R2: TRect;

begin
  if FShown then begin
    RectFrom := CaretPointToRect(PointFrom);
    RectTo := CaretPointToRect(PointTo);
    if IntersectRect(R2, RectFrom, FEditor.GetClientRect) then
      InvertRect(FEditor.GetCanvas.Handle, RectFrom);
    if IntersectRect(R2, RectTo, FEditor.GetClientRect) then
      InvertRect(FEditor.GetCanvas.Handle, RectTo);
  end;
  if Sender = Carets.FDefaultCaret then
    DefaultCaretMoved(PointFrom, PointTo);
end;

procedure TMultiCaretController.Paint;
begin
  if FShown then
    InvertCaretsRects;
end;

procedure TMultiCaretController.RefreshSelection;
begin
  if HasSelection and (FCarets.Count > 1) then
      FEditor.SetSelectionMode(smMultiCaret)
  else
    FEditor.SetSelectionMode(smNormal)
end;

procedure TMultiCaretController.SandBox(Command: TSynEditorCommand;
  AChar: WideChar; Data: Pointer);
begin
  // Store context
  FCommand := Command;
  try
    if IsSelectionCommand(Command) then
      HandleSelectionAction(Command, AChar, Data)
    else
      HandleInputAction(Command, AChar, Data);
  finally
    FCommand := ecNone
  end;
end;

procedure TMultiCaretController.SetActive(const Value: Boolean);
begin
  if FActive <> Value then begin
    if FShown then
      InvertCaretsRects;
    FActive := Value;
    FShown := False;
    FBlinkTimer.Enabled := Value;
  end;
end;

procedure TMultiCaretController.SetShape(const Value: TCaretShape);
begin
  if not (FShape = Value) then begin
    if FShown then
      InvertCaretsRects;
    FShape := Value;
    if FShown then
      InvertCaretsRects;
  end;
end;

{$IFDEF DEBUG}
procedure TMultiCaretController.ShowDebugState;
var
  Comma: TStringList;
  Caret: TCaretItem;
  S: string;
begin
  Comma := TStringList.Create;
  try
    Comma.Add('Carets: ');
    for Caret in FCarets do begin
      S := Format('[X: %d; Y: %d; Default: %s]', [Caret.PosX, Caret.PosY, BoolToStr(Caret = FCarets.DefaultCaret, True)]);
      Comma.Add(S)
    end;
    S := Comma.CommaText;
    if S <> FLastDebugState then begin
      FLastDebugState := S;
      OutputDebugString(PChar(S));
    end;
  finally
    Comma.Free;
  end;
end;
procedure TMultiCaretController.Unselect;
var
  Caret: TCaretItem;
begin
  for Caret in FCarets do begin
    Caret.Selection := TSelection.Empty
  end;
end;

{$ENDIF}

procedure TMultiCaretController.DoCaretsChanged(Sender: TObject);
var
  Caret: TCaretItem;
begin
  for Caret in FCarets do begin
    if not Assigned(Caret.OnMoved) then
      Caret.OnMoved := DoCaretMoved;
    if not Assigned(Caret.OnVisibleChanged) then
      Caret.OnVisibleChanged := DoCaretVisibleChanged;
    if not Assigned(Caret.OnSelectionChanged) then
      Caret.OnSelectionChanged := DoCaretSelectionChanged;
  end;
  FEditor.Refresh;
end;

procedure TMultiCaretController.DoCaretSelectionChanged(Sender: TCaretItem;
  const ValueFrom, ValueTo: TSelection);
begin
  RefreshSelection
end;

procedure TMultiCaretController.DoCaretVisibleChanged(Sender: TCaretItem);
var
  R, R2: TRect;

begin
  if not FIsDestroying and FShown then begin
    R := CaretPointToRect(Sender.ToPoint);
      if IntersectRect(R2, R, FEditor.GetClientRect) then
        InvertRect(FEditor.GetCanvas.Handle, R);
  end;
end;

procedure TMultiCaretController.EditorCommandSandBoxEntryPoint(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data,
  HandlerData: pointer);
begin
  if FCarets.Count > 1 then begin
    Handled := (not AfterProcessing) and (FCommandsList.IndexOf(Command) <> -1);
    if Handled then begin
      if not FSandBoxContext then begin
        // review: if FSandBoxContext is True the Command will not be executed but
        // appear handled.  Is this OK?
        FSandBoxContext := True;
        try
          SandBox(Command, AChar, Data);
        finally
          FSandBoxContext := False;
        end;
      end;
      // review: If FCarets.Count > 1 and the command is not handled should we
      // clear multiple carets?
    end
  end
  else
    Handled := False
end;

function TMultiCaretController.Exists(const PosX, PosY: Integer): Boolean;
var
  Iter: TCaretItem;
  IterRect, ShotRect: TRect;

begin
  Result := False;
  for Iter in FCarets do begin
    IterRect := CaretPointToRect(Iter.ToPoint);
    ShotRect := CaretPointToRect(TPoint.Create(PosX, PosY));
    if IterRect.IntersectsWith(ShotRect) then
      Exit(True)
  end;
end;

procedure TMultiCaretController.Flash;
begin
  {$IFDEF DEBUG}
  OutputDebugString('Flash');
  {$ENDIF}
  if not FShown then begin
    FShown := True;
    InvertCaretsRects;
    // restart blink timer
    FBlinkTimer.Enabled := False;
    FBlinkTimer.Enabled := True;
  end;
end;

procedure TMultiCaretController.HandleInputAction(Command: TSynEditorCommand;
  AChar: WideChar; Data: Pointer);
var
  DefCaret, ActiveCaret: TCaretItem;
  BeforeXY, AfterXY, DeltaXY: TPoint;
  BeforeDisplay, AfterDisplay: TDisplayCoord;
  BlockBegin, BlockEnd: TBufferCoord;
  RightLineSide, BottomColumnSide: TList<TCaretItem>;
  Neighbour: TCaretItem;
begin
  DefCaret := FCarets.FDefaultCaret;
  BlockBegin := FEditor.BlockBegin;
  BlockEnd := FEditor.BlockEnd;
  //
  FEditor.BeginUpdate;
  FEditor.UndoList.BeginMultiBlock;
  FEditor.UndoList.AddMultiCaretChange(FCarets.Store);
  try
    for ActiveCaret in FCarets do begin
      // implicitly set default caret
      FCarets.FDefaultCaret := ActiveCaret;
      FEditor.ComputeCaret(ActiveCaret.PosX, ActiveCaret.PosY);
      BeforeDisplay := FEditor.GetDisplayXY;
      BeforeXY := FEditor.DisplayCoord2CaretXY(BeforeDisplay);
      // neighbours
      RightLineSide := FCarets.GetLineNeighboursOnRight(ActiveCaret);
      BottomColumnSide := FCarets.GetColumnNeighboursOnBottom(ActiveCaret);
      // store Editor context
      FEditor.BlockBegin := FEditor.DisplayToBufferPos(FEditor.GetDisplayXY);
      FEditor.ExecuteCommand(Command, AChar, Data);
      // deltas
      AfterDisplay := FEditor.BufferToDisplayPos(FEditor.GetCaretXY);
      AfterXY := FEditor.DisplayCoord2CaretXY(AfterDisplay);
      DeltaXY := AfterXY.Subtract(BeforeXY);
      // correct neighbours coords according to deltas
      if (RightLineSide.Count > 0) and (DeltaXY.X > 0) then begin
        for Neighbour in RightLineSide do
          Neighbour.PosX := Neighbour.PosX + DeltaXY.X
      end;
      if (BottomColumnSide.Count > 0) and (DeltaXY.Y > 0) then begin
        for Neighbour in BottomColumnSide do
          Neighbour.PosY := Neighbour.PosY + DeltaXY.Y
      end;
    end;
  finally
    FEditor.UndoList.EndMultiBlock;
    FEditor.EndUpdate;
    // Restore context
    FCarets.FDefaultCaret := DefCaret;
    FEditor.BlockBegin := BlockBegin;
    FEditor.BlockEnd := BlockEnd;
  end;
end;

procedure TMultiCaretController.HandleSelectionAction(
  Command: TSynEditorCommand; AChar: WideChar; Data: Pointer);
var
  ActiveCaret, DefCaret: TCaretItem;
  CaretBefore, CaretAfter, CaretDiff: TDisplayCoord;
  BlockBeginOrig, BlockEndOrig: TBufferCoord;
  LoLine, HiLine: Integer;

  procedure JoinIntersectedCarets;
  var
    Index: Integer;
    CaretXY: TDisplayCoord;
    Cur, Next: TCaretItem;
    SortedCarets: TList<TCaretItem>;
  begin
    SortedCarets := FCarets.Sorted;
    Index := 0;
    while Index < SortedCarets.Count-1 do begin
      Cur := SortedCarets[Index];
      Next := SortedCarets[Index+1];
      if Cur.Selection.HasIntersection(Next.Selection) then begin
        if Command in [ecSelUp, ecSelLeft] then begin
          Cur.Selection.Join(Next.Selection);
          if Next = FCarets.DefaultCaret then
            FCarets.FDefaultCaret := Cur;
          FCarets.Delete(Next.Index);
        end
        else if Command in [ecSelDown, ecSelRight] then begin
          Next.Selection.Join(Cur.Selection);
          if Cur = FCarets.DefaultCaret then
            FCarets.FDefaultCaret := Next;
          FCarets.Delete(Cur.Index);
        end
        else
          Inc(Index)
      end
      else
        Inc(Index)
    end;
    if HasSelection and (FCarets.Count = 1) then begin
      // Restore single caret mechanism
      CaretXY := FCarets.DefaultCaret.Selection.Stop;
      FEditor.SetSelectionMode(smNormal);
      FEditor.SetCaretAndSelection(
        FEditor.DisplayToBufferPos(CaretXY),
        FEditor.DisplayToBufferPos(FCarets.DefaultCaret.Selection.Start),
        FEditor.DisplayToBufferPos(FCarets.DefaultCaret.Selection.Stop)
      );
    end
  end;

begin
  // Store context
  DefCaret := FCarets.FDefaultCaret;
  BlockBeginOrig := FEditor.BlockBegin;
  BlockEndOrig := FEditor.BlockEnd;
  LoLine := MaxInt;
  HiLine := 0;

  try
    for ActiveCaret in FCarets do begin
      FCarets.FDefaultCaret := ActiveCaret;
      FEditor.ComputeCaret(ActiveCaret.PosX, ActiveCaret.PosY);
      CaretBefore := FEditor.BufferToDisplayPos(FEditor.GetCaretXY);
      // execute command
      FEditor.ExecuteCommand(Command, AChar, Data);
      // deltas
      CaretAfter := FEditor.BufferToDisplayPos(FEditor.GetCaretXY);
      CaretDiff := CaretAfter - CaretBefore;
      //
      if ActiveCaret.Selection.IsEmpty then
        ActiveCaret.Selection := TSelection.Create(CaretBefore, CaretAfter)
      else
        ActiveCaret.Selection := TSelection.Create(ActiveCaret.Selection.Start, CaretAfter);
      LoLine := Min(LoLine, ActiveCaret.Selection.Normalize.Start.Row);
      HiLine := Max(HiLine, ActiveCaret.Selection.Normalize.Stop.Row);
    end;
  finally
    FCarets.FDefaultCaret := DefCaret;
    FEditor.BlockBegin := BlockBeginOrig;
    FEditor.BlockEnd := BlockEndOrig;
  end;
  JoinIntersectedCarets;
  FEditor.Refresh;
end;

function TMultiCaretController.HasSelection: Boolean;
var
  Caret: TCaretItem;
begin
  Result := False;
  for Caret in FCarets do
    if not Caret.Selection.IsEmpty then
      Exit(True)
end;

{ TCaretShape }

constructor TCaretShape.Create(const aWidth, aHeight: Integer;
  const aOffset: TPoint);
begin
  Create(aWidth, aHeight);
  Offset := aOffset;
end;

constructor TCaretShape.Create(const aWidth, aHeight: Integer);
begin
  Width := aWidth;
  Height := aHeight;
  FillChar(Offset, SizeOf(Offset), 0);
end;

class operator TCaretShape.Equal(a, b: TCaretShape): Boolean;
begin
  Result := (a.Width = b.Width) and (a.Height = b.Height) and (a.Offset = b.Offset)
end;

procedure TCaretShape.SetToDefault;
begin
  Width := 2;
  Height := 10;
  Offset := Point(0, 0);
end;

end.