{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition
-------------------------------------------------------------------------------}

unit FMX.SynEditKbdHandler;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  FMX.Types,
  SynEditTypes;

type
  { FMX equivalent of VCL TKeyPressEvent }
  TKeyPressEvent = procedure(Sender: TObject; var Key: Char) of object;

  TMouseCursorEvent = procedure(Sender: TObject;
    const aLineCharPos: TBufferCoord; var aCursor: TCursor) of object;

  TMethodList = class
  private
    fData: TList;
    function GetItem(Index: Integer): TMethod;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aHandler: TMethod);
    procedure Remove(aHandler: TMethod);
    property Items[Index: Integer]: TMethod read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TSynEditKbdHandler = class(TObject)
  private
    fKeyPressChain: TMethodList;
    fKeyDownChain: TMethodList;
    fKeyUpChain: TMethodList;
    fMouseDownChain: TMethodList;
    fMouseUpChain: TMethodList;
    fMouseCursorChain: TMethodList;
    fInKeyDown: Boolean;
    fInKeyUp: Boolean;
    fInKeyPress: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure ExecuteKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure ExecuteKeyPress(Sender: TObject; var Key: Char);
    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressEvent);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
  end;

implementation

{ TMethodList }

constructor TMethodList.Create;
begin
  fData := TList.Create;
end;

destructor TMethodList.Destroy;
begin
  fData.Free;
  inherited;
end;

procedure TMethodList.Add(aHandler: TMethod);
begin
  fData.Add(aHandler.Code);
  fData.Add(aHandler.Data);
end;

procedure TMethodList.Remove(aHandler: TMethod);
var
  I: Integer;
begin
  I := 0;
  while I < fData.Count - 1 do
  begin
    if (fData[I] = aHandler.Code) and (fData[I + 1] = aHandler.Data) then
    begin
      fData.Delete(I);
      fData.Delete(I);
      Exit;
    end;
    Inc(I, 2);
  end;
end;

function TMethodList.GetCount: Integer;
begin
  Result := fData.Count div 2;
end;

function TMethodList.GetItem(Index: Integer): TMethod;
begin
  Index := Index * 2;
  Result.Code := fData[Index];
  Result.Data := fData[Index + 1];
end;

{ TSynEditKbdHandler }

constructor TSynEditKbdHandler.Create;
begin
  inherited;
  fKeyPressChain := TMethodList.Create;
  fKeyDownChain := TMethodList.Create;
  fKeyUpChain := TMethodList.Create;
  fMouseDownChain := TMethodList.Create;
  fMouseUpChain := TMethodList.Create;
  fMouseCursorChain := TMethodList.Create;
end;

destructor TSynEditKbdHandler.Destroy;
begin
  fKeyPressChain.Free;
  fKeyDownChain.Free;
  fKeyUpChain.Free;
  fMouseDownChain.Free;
  fMouseUpChain.Free;
  fMouseCursorChain.Free;
  inherited;
end;

procedure TSynEditKbdHandler.ExecuteKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  idx: Integer;
begin
  if fInKeyDown then
    Exit;
  fInKeyDown := True;
  try
    for idx := fKeyDownChain.Count - 1 downto 0 do
    begin
      TKeyEvent(fKeyDownChain[idx])(Sender, Key, KeyChar, Shift);
      if (Key = 0) and (KeyChar = #0) then
        Exit;
    end;
  finally
    fInKeyDown := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
var
  idx: Integer;
begin
  if fInKeyUp then
    Exit;
  fInKeyUp := True;
  try
    for idx := fKeyUpChain.Count - 1 downto 0 do
    begin
      TKeyEvent(fKeyUpChain[idx])(Sender, Key, KeyChar, Shift);
      if (Key = 0) and (KeyChar = #0) then
        Exit;
    end;
  finally
    fInKeyUp := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteKeyPress(Sender: TObject; var Key: Char);
var
  idx: Integer;
begin
  if fInKeyPress then
    Exit;
  fInKeyPress := True;
  try
    for idx := fKeyPressChain.Count - 1 downto 0 do
    begin
      TKeyPressEvent(fKeyPressChain[idx])(Sender, Key);
      if Key = #0 then
        Exit;
    end;
  finally
    fInKeyPress := False;
  end;
end;

procedure TSynEditKbdHandler.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  fKeyDownChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  fKeyDownChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  fKeyUpChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  fKeyUpChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKeyPressChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKeyPressChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  fMouseDownChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  fMouseDownChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  fMouseUpChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  fMouseUpChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fMouseCursorChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fMouseCursorChain.Remove(TMethod(aHandler));
end;

end.
