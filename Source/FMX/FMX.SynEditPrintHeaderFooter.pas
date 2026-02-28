{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
CONTENTS:
  Classes handling header and footer rendering for FMX printing.

  THeaderFooterItem:
    An item in a header or footer. Has text, Font, LineNumber, and Alignment
    (multiple items can share the same line with different fonts).

  TSynEditPrintHeaderFooter:
    Collection of THeaderFooterItem objects.
    Properties:
      FrameTypes     : Frame around header/footer (ftLine, ftBox, ftShaded).
      ShadedColor    : Fill color for ftShaded.
      LineColor      : Color for ftLine or ftBox frame.
      DefaultFont    : Default font for items.
      RomanNumbers   : Print page numbers as Roman numerals.
      MirrorPosition : Mirror left/right alignment on even pages.
    Methods:
      Add            : Add an item with text, font, alignment, line number.
      Delete/Clear   : Remove items.
      InitPrint      : Prepare for printing.
      Print          : Render header/footer to a TCanvas.
    Text macros:
      $PAGENUM$      : Current page number.
      $PAGECOUNT$    : Total page count.
      $TITLE$        : Document title.
      $DATE$         : Current date.
      $TIME$         : Current time.
      $DATETIME$     : Date then time.
      $TIMEDATE$     : Time then date.

  THeader / TFooter:
    Subclasses that set FType to hftHeader / hftFooter.

  Rendering accepts a TCanvas parameter (FMX canvas) - works with any
  FMX canvas implementation.
-------------------------------------------------------------------------------}

unit FMX.SynEditPrintHeaderFooter;

{$M+}
{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.TextLayout,
  FMX.SynEditPrintTypes,
  FMX.SynEditPrintMargins;

type
  THeaderFooterType = (hftHeader, hftFooter);

  { An item in a header or footer }
  THeaderFooterItem = class
  private
    FText: string;
    FFont: TFont;
    FLineNumber: Integer;
    FAlignment: TAlignment;
    FIndex: Integer;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    function GetText(NumPages, PageNum: Integer; Roman: Boolean;
      Title, ATime, ADate: string): string;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  public
    property Alignment: TAlignment read FAlignment write FAlignment;
    property AsString: string read GetAsString write SetAsString;
    property Font: TFont read FFont write SetFont;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property Text: string read FText write FText;
  end;

  { Header/footer collection class }
  TSynEditPrintHeaderFooter = class(TPersistent)
  private
    FType: THeaderFooterType;
    FFrameTypes: TFrameTypes;
    FShadedColor: TColor;
    FLineColor: TColor;
    FItems: TList;
    FDefaultFont: TFont;
    FDate, FTime: string;
    FNumPages: Integer;
    FTitle: string;
    FMargins: TSynEditPrintMargins;
    FFrameHeight: Integer;
    FRomanNumbers: Boolean;
    FLineHeights: TArray<Integer>;
    FLineCount: Integer;
    FMirrorPosition: Boolean;
    procedure SetDefaultFont(const Value: TFont);
    procedure DrawFrame(Canvas: TCanvas);
    procedure CalcHeight;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Text: string; Font: TFont; Alignment: TAlignment;
      LineNumber: Integer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;
    function Get(Index: Integer): THeaderFooterItem;
    procedure InitPrint(NumPages: Integer; Title: string;
      Margins: TSynEditPrintMargins);
    procedure Print(Canvas: TCanvas; PageNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure FixLines;
    property AsString: string read GetAsString write SetAsString;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property FrameTypes: TFrameTypes read FFrameTypes write FFrameTypes
      default [ftLine];
    property ShadedColor: TColor read FShadedColor write FShadedColor
      default TColors.Silver;
    property LineColor: TColor read FLineColor write FLineColor
      default TColors.Black;
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    property RomanNumbers: Boolean read FRomanNumbers write FRomanNumbers
      default False;
    property MirrorPosition: Boolean read FMirrorPosition write FMirrorPosition
      default False;
  end;

  { Header and Footer - set FType in constructor }
  THeader = class(TSynEditPrintHeaderFooter)
  public
    constructor Create;
  end;

  TFooter = class(TSynEditPrintHeaderFooter)
  public
    constructor Create;
  end;

implementation

uses
  SynEditMiscProcs,
  FMX.SynEditRenderer;

{ Helper: extract first element from a delimited string }
function GetFirstEl(var Value: string; Delim: WideChar): string;
var
  P: Integer;
begin
  P := Pos(Delim, Value);
  if P = 0 then
    P := Length(Value) + 1;
  Result := Copy(Value, 1, P - 1);
  System.Delete(Value, 1, P);
end;

{ THeaderFooterItem }

constructor THeaderFooterItem.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.Family := 'Arial';
  FFont.Size := 10;
end;

destructor THeaderFooterItem.Destroy;
begin
  FFont.Free;
  inherited;
end;

function THeaderFooterItem.GetAsString: string;
begin
  Result :=
    EncodeString(FText) + '/' +
    IntToStr(0) + '/' + { charset placeholder }
    IntToStr(Integer(TColors.Black)) + '/' + { color }
    IntToStr(Round(FFont.Size)) + '/' + { height as size }
    EncodeString(FFont.Family) + '/' +
    IntToStr(0) + '/' + { pitch placeholder }
    IntToStr(96) + '/' + { ppi placeholder }
    IntToStr(Round(FFont.Size)) + '/' +
    IntToStr(Byte(FFont.Style)) + '/' +
    IntToStr(FLineNumber) + '/' +
    IntToStr(Ord(FAlignment));
end;

function THeaderFooterItem.GetText(NumPages, PageNum: Integer;
  Roman: Boolean; Title, ATime, ADate: string): string;
var
  Len, Start, Run: Integer;
  AStr: string;

  procedure DoAppend(AText: string);
  begin
    Result := Result + AText;
  end;

  procedure TryAppend(var First: Integer; After: Integer);
  begin
    if After > First then
    begin
      DoAppend(Copy(AStr, First, After - First));
      First := After;
    end;
  end;

  function TryExecuteMacro: Boolean;
  var
    Macro: string;
  begin
    Result := True;
    Macro := System.SysUtils.UpperCase(Copy(FText, Start, Run - Start + 1));
    if Macro = '$PAGENUM$' then
    begin
      if Roman then
        DoAppend(IntToRoman(PageNum))
      else
        DoAppend(IntToStr(PageNum));
      Exit;
    end;
    if Macro = '$PAGECOUNT$' then
    begin
      if Roman then
        DoAppend(IntToRoman(NumPages))
      else
        DoAppend(IntToStr(NumPages));
      Exit;
    end;
    if Macro = '$TITLE$' then
    begin
      DoAppend(Title);
      Exit;
    end;
    if Macro = '$DATE$' then
    begin
      DoAppend(ADate);
      Exit;
    end;
    if Macro = '$TIME$' then
    begin
      DoAppend(ATime);
      Exit;
    end;
    if Macro = '$DATETIME$' then
    begin
      DoAppend(ADate + ' ' + ATime);
      Exit;
    end;
    if Macro = '$TIMEDATE$' then
    begin
      DoAppend(ATime + ' ' + ADate);
      Exit;
    end;
    Result := False;
  end;

begin
  Result := '';
  AStr := FText;
  if Trim(AStr) = '' then
    Exit;
  Len := Length(AStr);
  if Len > 0 then
  begin
    Start := 1;
    Run := 1;
    while Run <= Len do
    begin
      if AStr[Run] = '$' then
      begin
        TryAppend(Start, Run);
        Inc(Run);
        while Run <= Len do
        begin
          if AStr[Run] = '$' then
          begin
            if TryExecuteMacro then
            begin
              Inc(Run);
              Start := Run;
              Break;
            end
            else
            begin
              TryAppend(Start, Run);
              Inc(Run);
            end;
          end
          else
            Inc(Run);
        end;
      end
      else
        Inc(Run);
    end;
    TryAppend(Start, Run);
  end;
end;

procedure THeaderFooterItem.LoadFromStream(AStream: TStream);
var
  Len, BufferSize: Integer;
  Buffer: Pointer;
  aSize: Integer;
  aStyle: TFontStyles;
  aName: string;
  aNameLen: Integer;
  aNameBuf: PAnsiChar;
begin
  with AStream do
  begin
    Read(Len, SizeOf(Len));
    BufferSize := Len * SizeOf(WideChar);
    GetMem(Buffer, BufferSize + SizeOf(WideChar));
    try
      Read(Buffer^, BufferSize);
      PWideChar(Buffer)[BufferSize div SizeOf(WideChar)] := #0;
      FText := PWideChar(Buffer);
    finally
      FreeMem(Buffer);
    end;
    Read(FLineNumber, SizeOf(FLineNumber));
    { Font: read charset (skip), color (skip), height (skip) }
    Read(Len, SizeOf(Len)); { charset }
    Read(Len, SizeOf(Len)); { color }
    Read(Len, SizeOf(Len)); { height }
    { Font name }
    Read(aNameLen, SizeOf(aNameLen));
    GetMem(aNameBuf, aNameLen + 1);
    try
      Read(aNameBuf^, aNameLen);
      aNameBuf[aNameLen] := #0;
      aName := string(aNameBuf);
    finally
      FreeMem(aNameBuf);
    end;
    Read(Len, SizeOf(Len)); { pitch }
    Read(aSize, SizeOf(aSize));
    Read(aStyle, SizeOf(aStyle));
    FFont.Family := aName;
    FFont.Size := aSize;
    FFont.Style := aStyle;
    Read(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure THeaderFooterItem.SaveToStream(AStream: TStream);
var
  aLen: Integer;
  aStyle: TFontStyles;
  aSize: Integer;
  aNameAnsi: AnsiString;
  Zero: Integer;
begin
  with AStream do
  begin
    aLen := Length(FText);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(FText)^, aLen * SizeOf(WideChar));
    Write(FLineNumber, SizeOf(FLineNumber));
    { Font fields for compatibility }
    Zero := 0;
    Write(Zero, SizeOf(Zero)); { charset }
    Write(Zero, SizeOf(Zero)); { color }
    aSize := Round(FFont.Size);
    Write(aSize, SizeOf(aSize)); { height }
    aNameAnsi := AnsiString(FFont.Family);
    aLen := Length(aNameAnsi);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(aNameAnsi)^, aLen);
    Write(Zero, SizeOf(Zero)); { pitch }
    Write(aSize, SizeOf(aSize)); { size }
    aStyle := FFont.Style;
    Write(aStyle, SizeOf(aStyle));
    Write(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure THeaderFooterItem.SetAsString(const Value: string);
var
  S: string;
  Sty: TFontStyles;
begin
  S := Value;
  FText := DecodeString(GetFirstEl(S, '/'));
  GetFirstEl(S, '/'); { charset - skip }
  GetFirstEl(S, '/'); { color - skip }
  GetFirstEl(S, '/'); { height - skip }
  FFont.Family := DecodeString(GetFirstEl(S, '/'));
  GetFirstEl(S, '/'); { pitch - skip }
  GetFirstEl(S, '/'); { ppi - skip }
  FFont.Size := StrToIntDef(GetFirstEl(S, '/'), 10);
  Byte(Sty) := StrToIntDef(GetFirstEl(S, '/'), 0);
  FFont.Style := Sty;
  FLineNumber := StrToIntDef(GetFirstEl(S, '/'), 0);
  FAlignment := TAlignment(StrToIntDef(GetFirstEl(S, '/'), 0));
end;

procedure THeaderFooterItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ TSynEditPrintHeaderFooter }

constructor TSynEditPrintHeaderFooter.Create;
begin
  inherited;
  FFrameTypes := [ftLine];
  FShadedColor := TColors.Silver;
  FLineColor := TColors.Black;
  FItems := TList.Create;
  FDefaultFont := TFont.Create;
  FRomanNumbers := False;
  FMirrorPosition := False;
  FDefaultFont.Family := 'Arial';
  FDefaultFont.Size := 10;
end;

destructor TSynEditPrintHeaderFooter.Destroy;
begin
  Clear;
  FItems.Free;
  FDefaultFont.Free;
  inherited;
end;

function TSynEditPrintHeaderFooter.Add(Text: string; Font: TFont;
  Alignment: TAlignment; LineNumber: Integer): Integer;
var
  AItem: THeaderFooterItem;
begin
  AItem := THeaderFooterItem.Create;
  if Font = nil then
    AItem.Font := FDefaultFont
  else
    AItem.Font := Font;
  AItem.Alignment := Alignment;
  AItem.LineNumber := LineNumber;
  AItem.FIndex := FItems.Add(AItem);
  AItem.Text := Text;
  Result := AItem.FIndex;
end;

procedure TSynEditPrintHeaderFooter.Delete(Index: Integer);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[I]).FIndex = Index then
    begin
      THeaderFooterItem(FItems[I]).Free;
      FItems.Delete(I);
      Break;
    end;
  end;
end;

procedure TSynEditPrintHeaderFooter.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    THeaderFooterItem(FItems[I]).Free;
  FItems.Clear;
end;

procedure TSynEditPrintHeaderFooter.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

procedure TSynEditPrintHeaderFooter.FixLines;
var
  I, CurLine: Integer;
begin
  SetLength(FLineHeights, 0);
  CurLine := 0;
  FLineCount := 0;
  for I := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[I]).LineNumber <> CurLine then
    begin
      CurLine := THeaderFooterItem(FItems[I]).LineNumber;
      Inc(FLineCount);
    end;
    THeaderFooterItem(FItems[I]).LineNumber := FLineCount;
  end;
  SetLength(FLineHeights, FLineCount);
end;

procedure TSynEditPrintHeaderFooter.CalcHeight;
var
  I, CurLine: Integer;
  AItem: THeaderFooterItem;
  FOrgHeight: Integer;
  ItemHeight: Integer;
begin
  FFrameHeight := -1;
  if FItems.Count <= 0 then Exit;

  CurLine := 1;
  FFrameHeight := 0;
  FOrgHeight := FFrameHeight;
  for I := 0 to FItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(FItems[I]);
    if AItem.LineNumber <> CurLine then
    begin
      CurLine := AItem.LineNumber;
      FOrgHeight := FFrameHeight;
    end;
    ItemHeight := MeasureTextHeight(AItem.Font, 'Wg');
    FLineHeights[CurLine - 1] := Max(FLineHeights[CurLine - 1], ItemHeight);
    FFrameHeight := Max(FFrameHeight, FOrgHeight + FLineHeights[CurLine - 1]);
  end;
  FFrameHeight := FFrameHeight + 2 * FMargins.PHFInternalMargin;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
begin
  Result := THeaderFooterItem(Item1).LineNumber -
    THeaderFooterItem(Item2).LineNumber;
  if Result = 0 then
    Result := NativeInt(Item1) - NativeInt(Item2);
end;

procedure TSynEditPrintHeaderFooter.InitPrint(NumPages: Integer;
  Title: string; Margins: TSynEditPrintMargins);
begin
  FDate := DateToStr(Now);
  FTime := TimeToStr(Now);
  FNumPages := NumPages;
  FMargins := Margins;
  FTitle := Title;
  FItems.Sort(CompareItems);
  FixLines;
  CalcHeight;
end;

procedure TSynEditPrintHeaderFooter.DrawFrame(Canvas: TCanvas);
var
  BoxRect: TRectF;
  AlphaShaded, AlphaLine: TAlphaColor;
begin
  if FrameTypes = [] then Exit;

  AlphaShaded := TColorToAlphaColor(FShadedColor);
  AlphaLine := TColorToAlphaColor(FLineColor);

  with FMargins do
  begin
    if FType = hftHeader then
      BoxRect := RectF(PLeft, PHeader - FFrameHeight, PRight, PHeader)
    else
      BoxRect := RectF(PLeft, PFooter, PRight, PFooter + FFrameHeight);

    if ftShaded in FrameTypes then
    begin
      Canvas.Fill.Color := AlphaShaded;
      Canvas.FillRect(BoxRect, 0, 0, AllCorners, 1.0);
    end;

    if ftBox in FrameTypes then
    begin
      Canvas.Stroke.Color := AlphaLine;
      Canvas.Stroke.Thickness := 1.0;
      Canvas.DrawRect(BoxRect, 0, 0, AllCorners, 1.0);
    end;

    if ftLine in FrameTypes then
    begin
      Canvas.Stroke.Color := AlphaLine;
      Canvas.Stroke.Thickness := 1.0;
      if FType = hftHeader then
        Canvas.DrawLine(PointF(PLeft, PHeader), PointF(PRight, PHeader), 1.0)
      else
        Canvas.DrawLine(PointF(PLeft, PFooter), PointF(PRight, PFooter), 1.0);
    end;
  end;
end;

procedure TSynEditPrintHeaderFooter.Print(Canvas: TCanvas;
  PageNum: Integer);
var
  I, Y, CurLine: Integer;
  AStr: string;
  AItem: THeaderFooterItem;
  TheAlignment: TAlignment;
  TextRect: TRectF;
  HAlign: TTextAlign;
begin
  if FFrameHeight <= 0 then Exit;

  DrawFrame(Canvas);

  if FType = hftHeader then
    Y := FMargins.PHeader - FFrameHeight
  else
    Y := FMargins.PFooter;
  Y := Y + FMargins.PHFInternalMargin;

  CurLine := 1;
  for I := 0 to FItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(FItems[I]);

    if AItem.LineNumber <> CurLine then
    begin
      Y := Y + FLineHeights[CurLine - 1];
      CurLine := AItem.LineNumber;
    end;

    AStr := AItem.GetText(FNumPages, PageNum, FRomanNumbers, FTitle,
      FTime, FDate);

    { Determine alignment, handle mirroring }
    TheAlignment := AItem.Alignment;
    if MirrorPosition and ((PageNum mod 2) = 0) then
    begin
      case AItem.Alignment of
        taRightJustify: TheAlignment := taLeftJustify;
        taLeftJustify: TheAlignment := taRightJustify;
      end;
    end;

    case TheAlignment of
      taLeftJustify: HAlign := TTextAlign.Leading;
      taRightJustify: HAlign := TTextAlign.Trailing;
      taCenter: HAlign := TTextAlign.Center;
    else
      HAlign := TTextAlign.Leading;
    end;

    TextRect := RectF(FMargins.PLeftHFTextIndent, Y,
      FMargins.PRightHFTextIndent, Y + FLineHeights[CurLine - 1]);

    Canvas.Font.Assign(AItem.Font);
    Canvas.Fill.Color := TColorToAlphaColor(TColors.Black);
    Canvas.FillText(TextRect, AStr, False, 1.0, [], HAlign,
      TTextAlign.Trailing);
  end;
end;

procedure TSynEditPrintHeaderFooter.Assign(Source: TPersistent);
var
  Src: TSynEditPrintHeaderFooter;
  I: Integer;
begin
  if (Source <> nil) and (Source is TSynEditPrintHeaderFooter) then
  begin
    Src := TSynEditPrintHeaderFooter(Source);
    Clear;
    FType := Src.FType;
    FFrameTypes := Src.FFrameTypes;
    FShadedColor := Src.FShadedColor;
    FLineColor := Src.FLineColor;
    for I := 0 to Src.FItems.Count - 1 do
    begin
      with THeaderFooterItem(Src.FItems[I]) do
        Add(Text, Font, Alignment, LineNumber);
    end;
    FDefaultFont.Assign(Src.FDefaultFont);
    FRomanNumbers := Src.FRomanNumbers;
    FMirrorPosition := Src.FMirrorPosition;
  end
  else
    inherited Assign(Source);
end;

function TSynEditPrintHeaderFooter.Count: Integer;
begin
  Result := FItems.Count;
end;

function TSynEditPrintHeaderFooter.Get(Index: Integer): THeaderFooterItem;
begin
  Result := THeaderFooterItem(FItems[Index]);
end;

function TSynEditPrintHeaderFooter.GetAsString: string;
var
  I: Integer;
begin
  FixLines;
  Result := '';
  for I := 0 to FItems.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + '/';
    Result := Result + EncodeString(THeaderFooterItem(FItems[I]).AsString);
  end;
end;

procedure TSynEditPrintHeaderFooter.SetAsString(const Value: string);
var
  Item: THeaderFooterItem;
  S: string;
begin
  Clear;
  Item := THeaderFooterItem.Create;
  try
    S := Value;
    while S <> '' do
    begin
      Item.AsString := DecodeString(GetFirstEl(S, '/'));
      Add(Item.Text, Item.Font, Item.Alignment, Item.LineNumber);
    end;
  finally
    Item.Free;
  end;
end;

procedure TSynEditPrintHeaderFooter.LoadFromStream(AStream: TStream);
var
  Num, I: Integer;
  aSize: Integer;
  aStyle: TFontStyles;
  aNameLen: Integer;
  aNameBuf: PAnsiChar;
  aName: string;
  Zero: Integer;
begin
  with AStream do
  begin
    Read(FFrameTypes, SizeOf(FFrameTypes));
    Read(FShadedColor, SizeOf(FShadedColor));
    Read(FLineColor, SizeOf(FLineColor));
    Read(FRomanNumbers, SizeOf(FRomanNumbers));
    Read(FMirrorPosition, SizeOf(FMirrorPosition));
    { Font }
    Read(Zero, SizeOf(Zero)); { charset }
    Read(Zero, SizeOf(Zero)); { color }
    Read(Zero, SizeOf(Zero)); { height }
    Read(aNameLen, SizeOf(aNameLen));
    GetMem(aNameBuf, aNameLen + 1);
    try
      Read(aNameBuf^, aNameLen);
      aNameBuf[aNameLen] := #0;
      aName := string(aNameBuf);
    finally
      FreeMem(aNameBuf);
    end;
    Read(Zero, SizeOf(Zero)); { pitch }
    Read(aSize, SizeOf(aSize));
    Read(aStyle, SizeOf(aStyle));
    FDefaultFont.Family := aName;
    FDefaultFont.Size := aSize;
    FDefaultFont.Style := aStyle;
    { Items }
    Read(Num, SizeOf(Num));
    while Num > 0 do
    begin
      I := Add('', nil, taLeftJustify, 1);
      Get(I).LoadFromStream(AStream);
      Dec(Num);
    end;
  end;
end;

procedure TSynEditPrintHeaderFooter.SaveToStream(AStream: TStream);
var
  I, Num: Integer;
  aSize: Integer;
  aStyle: TFontStyles;
  aNameAnsi: AnsiString;
  aLen: Integer;
  Zero: Integer;
begin
  with AStream do
  begin
    Write(FFrameTypes, SizeOf(FFrameTypes));
    Write(FShadedColor, SizeOf(FShadedColor));
    Write(FLineColor, SizeOf(FLineColor));
    Write(FRomanNumbers, SizeOf(FRomanNumbers));
    Write(FMirrorPosition, SizeOf(FMirrorPosition));
    { Font }
    Zero := 0;
    Write(Zero, SizeOf(Zero)); { charset }
    Write(Zero, SizeOf(Zero)); { color }
    aSize := Round(FDefaultFont.Size);
    Write(aSize, SizeOf(aSize)); { height }
    aNameAnsi := AnsiString(FDefaultFont.Family);
    aLen := Length(aNameAnsi);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(aNameAnsi)^, aLen);
    Write(Zero, SizeOf(Zero)); { pitch }
    Write(aSize, SizeOf(aSize));
    aStyle := FDefaultFont.Style;
    Write(aStyle, SizeOf(aStyle));
    { Items }
    Num := Count;
    Write(Num, SizeOf(Num));
    for I := 0 to Num - 1 do
      Get(I).SaveToStream(AStream);
  end;
end;

{ THeader }

constructor THeader.Create;
begin
  inherited;
  FType := hftHeader;
end;

{ TFooter }

constructor TFooter.Create;
begin
  inherited;
  FType := hftFooter;
end;

end.
