{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintHeaderFooter.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
Portions written by Michael Hieke are copyright 2000 Michael Hieke.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Classes handling info about headers and footers.

  THeaderFooterItem:
    Class handling an item in a header or footer. An item has a text,Font,
    LineNumber and Alignment (i.e. two items can be on the same line but have
    different fonts). Used internally.

  THeaderFooter:
    Collection of THeaderFooterItem's
    Design-time properties:
      FrameTypes : Frame around header or footer - can be any combination of:
                   ftLine   : Line under header or line above footer
                   ftBox    : Box around header or footer
                   ftShaded : Filled box (without frame) around header or footer.
      ShadedColor : Fill color if ftShaded is in FrameTypes
      LineColor   : Color of line or box if ftLine or ftBox is in FrameTypes
      DefaultFont : Default font for THeaderFooterItem's. This can be used to
                    set the header/footer font once for all items.
      RomanNumbers : Print page numbers as Roman numbers.
      MirrorPosition : Mirror position of left/right aligned THeaderFooterItem's
                       Can be used when printing 2-sided.
    Run-time methods:
      function Add(Text: string; Font: TFont;
                   Alignment: TAlignment;
                   LineNumber: Integer) : Integer;
        Add a THeaderFooterItem. If Font is nil or not specified then DefaultFont
        is used. Returned value is the index of the added item.
        The Text parameter can contain the following macros:
          $PAGECOUNT$  : Print total number of pages
          $PAGENUM$    : Print current page number
          $TITLE$      : Print the title
          $DATE$       : Print the date
          $TIME$       : Print the time
          $DATETIME$   : Print the date and then the time
          $TIMEDATE$   : Print the time and then the date
      procedure Delete(Index: Integer);
        Delete THeaderFooterItem with index Index.
      procedure Clear;
        Clear all THeaderFooterItems.
      function Count: Integer;
        Returns number of THeaderFooterItems.
      function Get(Index: Integer): THeaderFooterItem;
        Returns THeaderFooterItem with Index.
      procedure SetPixPrInch(Value: Integer);
        Corrects the PixPerInch property of fonts. Used internally by
        TSynEditPrint.
      procedure InitPrint
        Prepares the header or footer for printing. Used internally by
        TSynEditPrint.
      procedure Print
        Prints the header or footer. Used internally by TSynEditPrint.

-------------------------------------------------------------------------------}

unit SynEditPrintHeaderFooter;
{$M+}

{$I SynEdit.inc}

interface

uses
  Winapi.Windows,
  Winapi.D2D1,
  Vcl.Graphics,
  SynEditPrintTypes,
  SynEditPrintMargins,
  SynUnicode,
  System.Classes,
  System.SysUtils;

type
  //An item in a header or footer. An item has a text,Font,LineNumber and
  //Alignment (i.e. two items can be on the same line but have different
  //fonts).
  THeaderFooterItem = class
  private
    FText: string;
    FFont: TFont;
    FLineNumber: Integer;
    FAlignment: TAlignment;
        {Used to store the original Index when the item was added - the index
         might change when the list is sorted}
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

  THeaderFooterType = (hftHeader, hftFooter);

  //The header/footer class
  THeaderFooter = class(TPersistent)
  private
    FType: THeaderFooterType; // Indicates if header or footer
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
    FPPI: Integer;
    procedure SetDefaultFont(const Value: TFont);
    procedure DrawFrame(RT: ID2D1RenderTarget);
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
    procedure SetPixPrInch(Value: Integer);
    procedure InitPrint(NumPages: Integer; Title: string; Margins:
        TSynEditPrintMargins);
    procedure Print(RT: ID2D1RenderTarget; PageNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure FixLines;
    property AsString: string read GetAsString write SetAsString;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property FrameTypes: TFrameTypes read FFrameTypes write FFrameTypes
    default [ftLine];
    property ShadedColor: TColor read FShadedColor write FShadedColor
    default clSilver;
    property LineColor: TColor read FLineColor write FLineColor default clBlack;
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    property RomanNumbers: Boolean read FRomanNumbers write FRomanNumbers
    default False;
    property MirrorPosition: Boolean read FMirrorPosition write FMirrorPosition
    default False;
  end;

  //The header and footer - does nothing but set the value of FType in
  //THeaderFooter
  THeader = class(THeaderFooter)
  public
    constructor Create;
  end;

  TFooter = class(THeaderFooter)
  public
    constructor Create;
  end;

implementation

uses
  System.Types,
  System.UITypes,
  System.Math,
  SynDWrite,
  SynEditMiscProcs;

// Helper routine for AsString processing.
function GetFirstEl(var Value: string; Delim: WideChar): string;
var
  p: Integer;
begin
  p := Pos(Delim, Value);
  if p = 0 then
    p := Length(Value) + 1;
  Result := Copy(Value, 1, p - 1);
  Delete(Value, 1, p);
end;


{ THeaderFooterItem }

constructor THeaderFooterItem.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.PixelsPerInch := 96;
end;

destructor THeaderFooterItem.Destroy;
begin
  inherited;
  FFont.Free;
end;

// Returns string representation of THeaderFooterItem to alleviate storing
// items into external storage (registry, ini file).
function THeaderFooterItem.GetAsString: string;
begin
  Result :=
    EncodeString(FText) + '/' +
    IntToStr(FFont.Charset) + '/' +
    IntToStr(FFont.Color) + '/' +
    IntToStr(FFont.Height) + '/' +
    EncodeString(FFont.Name) + '/' +
    IntToStr(Ord(FFont.Pitch)) + '/' +
    IntToStr(FFont.PixelsPerInch) + '/' +
    IntToStr(FFont.Size) + '/' +
    IntToStr(byte(FFont.Style)) + '/' +
    IntToStr(FLineNumber) + '/' +
    IntToStr(Ord(FAlignment));
end;


{ This is basically copied from original SynEditPrint.pas. Returns the
  header/footer text with macros expanded }
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
    Macro := System.SysUtils.AnsiUpperCase(Copy(FText, Start, Run - Start + 1));
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
  // parse the line
  Len := Length(AStr);
  if Len > 0 then
  begin
      // start with left-aligned text
    Start := 1;
    Run := 1;
    while Run <= Len do
    begin
          // test for embedded macro
      if AStr[Run] = '$' then
      begin
        TryAppend(Start, Run);
        Inc(Run);
          // search for next '$' which could mark the end of a macro
        while Run <= Len do begin
          if AStr[Run] = '$' then
          begin
            // if this is a macro execute it and skip the chars from output
            if TryExecuteMacro then
            begin
              Inc(Run); // also the '$'
              Start := Run;
              Break;
            end
            else
            begin
                // this '$' might again be the start of a macro
              TryAppend(Start, Run);
              Inc(Run);                                                         //ek 2001-08-02
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
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  Len, BufferSize: Integer;
  Buffer: Pointer;
begin
  with AStream do
  begin
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      PWideChar(Buffer)[BufferSize div sizeof(WideChar)] := #0;
      FText := PWideChar(Buffer);
    finally
      FreeMem(Buffer);
    end;
    Read(FLineNumber, sizeof(FLineNumber));
    // font
    Read(aCharset, sizeof(aCharset));
    Read(aColor, sizeof(aColor));
    Read(aHeight, sizeof(aHeight));
    Read(BufferSize, sizeof(BufferSize));
    GetMem(Buffer, BufferSize + 1);
    try
      Read(Buffer^, BufferSize);
      PAnsiChar(Buffer)[BufferSize div sizeof(AnsiChar)] := #0;
      aName := string(PAnsiChar(Buffer));
    finally
      FreeMem(Buffer);
    end;
    Read(aPitch, sizeof(aPitch));
    Read(aSize, sizeof(aSize));
    Read(aStyle, sizeof(aStyle));
    FFont.Charset := aCharset;
    FFont.Color := aColor;
    FFont.Height := aHeight;
    FFont.Name := aName;
    FFont.Pitch := aPitch;
    FFont.Size := aSize;
    FFont.Style := aStyle;
    Read(FAlignment, sizeof(FAlignment));
  end;
end;

procedure THeaderFooterItem.SaveToStream(AStream: TStream);
var
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen: Integer;
begin
  with AStream do
  begin
    aLen := Length(FText);
    Write(aLen, sizeof(aLen));
    Write(PWideChar(FText)^, aLen * sizeof(WideChar));
    Write(FLineNumber, sizeof(FLineNumber));
    // font
    aCharset := FFont.Charset;
    aColor   := FFont.Color;
    aHeight  := FFont.Height;
    aName    := FFont.Name;
    aPitch   := FFont.Pitch;
    aSize    := FFont.Size;
    aStyle   := FFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(aName);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(AnsiString(aName))^, aLen);
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));
    Write(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure THeaderFooterItem.SetAsString(const Value: string);
var
  s: string;
  sty: TFontStyles;
begin
  s := Value;
  FText := DecodeString(GetFirstEl(s, '/'));
  FFont.Charset := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Color := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Height := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Name := DecodeString(GetFirstEl(s, '/'));
  FFont.Pitch := TFontPitch(StrToIntDef(GetFirstEl(s, '/'), 0));
  FFont.PixelsPerInch := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Size := StrToIntDef(GetFirstEl(s, '/'), 0);
  byte(sty) := StrToIntDef(GetFirstEl(s, '/'), 0);
  FFont.Style := sty;
  FLineNumber := StrToIntDef(GetFirstEl(s, '/'), 0);
  FAlignment := TAlignment(StrToIntDef(GetFirstEl(s, '/'), 0));
end;

procedure THeaderFooterItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ THeaderFooter }

constructor THeaderFooter.Create;
begin
  inherited;
  FFrameTypes := [ftLine];
  FShadedColor := clSilver;
  FLineColor := clBlack;
  FItems := TList.Create;
  FDefaultFont := TFont.Create;
  FRomanNumbers := False;
  FMirrorPosition := False;
  FPPI := 96;
  with FDefaultFont do
  begin
    Name := 'Arial';
    Color := clBlack;
    PixelsPerInch := 96;
    Size := 10;
  end;
end;

destructor THeaderFooter.Destroy;
begin
  Clear;
  FItems.Free;
  FDefaultFont.Free;
  inherited;
end;

function THeaderFooter.Add(Text: string; Font: TFont;
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

procedure THeaderFooter.Delete(Index: Integer);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[i]).FIndex = Index then
    begin
      FItems.Delete(i);
      Break;
    end;
  end;
end;

procedure THeaderFooter.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    THeaderFooterItem(FItems[i]).Free;
  FItems.Clear;
end;

procedure THeaderFooter.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

{ Counts number of lines in header/footer and changes the line-number so they
  start with 1 (the user might add header/footer items starting at line 2) }
procedure THeaderFooter.FixLines;
var
  i, CurLine: Integer;
begin
  SetLength(FLineHeights, 0);
  CurLine := 0;
  FLineCount := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[i]).LineNumber <> CurLine then
    begin
      CurLine := THeaderFooterItem(FItems[i]).LineNumber;
      Inc(FLineCount);
    end;
    THeaderFooterItem(FItems[i]).LineNumber := FLineCount;
  end;
  SetLength(FLineHeights, FLineCount);
end;

{ Calculates the height of the header/footer, finds the line height for each line
  and calculates the font baseline where text is to be written }
procedure THeaderFooter.CalcHeight;
var
  I, CurLine: Integer;
  AItem: THeaderFooterItem;
  FOrgHeight: Integer;
  TextFormat: TSynTextFormat;
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

    TextFormat:= TSynTextFormat.Create(AItem.Font);
    FLineHeights[CurLine - 1] := Max(FLineHeights[CurLine - 1], TextFormat.LineHeight);
    FFrameHeight := Max(FFrameHeight, FOrgHeight + FLineHeights[CurLine - 1]);
  end;
  FFrameHeight := FFrameHeight + 2 * FMargins.PHFInternalMargin;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
//Used to sort header/footer items
begin
  Result := THeaderFooterItem(Item1).LineNumber - THeaderFooterItem(Item2).LineNumber;
  if Result = 0 then
    Result := Integer(Item1) - Integer(Item2);
end;

procedure THeaderFooter.SetPixPrInch(Value: Integer);
var
  i, TmpSize: Integer;
  AFont: TFont;
begin
  FPPI := Value;
  for i := 0 to FItems.Count - 1 do
  begin
    AFont := THeaderFooterItem(FItems[i]).Font;
    TmpSize := AFont.Size;
    AFont.PixelsPerInch := Value;
    AFont.Size := TmpSize;
  end;
end;

procedure THeaderFooter.InitPrint(NumPages: Integer; Title: string;
  Margins: TSynEditPrintMargins);
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

procedure THeaderFooter.DrawFrame(RT: ID2D1RenderTarget);
//Draws frame around header/footer
var
  BoxRect: TD2D1RectF;
begin
  if (FrameTypes = []) then Exit;
  with FMargins do begin
    if FType = hftHeader then
      BoxRect := Rect(PLeft, PHeader - FFrameHeight, PRight, PHeader)
    else
      BoxRect := Rect(PLeft, PFooter, PRight, PFooter + FFrameHeight);

    if ftShaded in FrameTypes then
      RT.FillRectangle(BoxRect, TSynDWrite.SolidBrush(ShadedColor));
    if ftBox in FrameTypes then
      RT.DrawRectangle(BoxRect, TSynDWrite.SolidBrush(LineColor), FPPI/96);
    if ftLine in FrameTypes then begin
      if FType = hftHeader then
        RT.DrawLine(Point(PLeft, PHeader), Point(PRight, PHeader),
          TSynDWrite.SolidBrush(LineColor), FPPI/96)
      else
        RT.DrawLine(Point(PLeft, PFooter), Point(PRight, PFooter),
          TSynDWrite.SolidBrush(LineColor), FPPI/96);
    end;
  end;
end;

procedure THeaderFooter.Print(RT: ID2D1RenderTarget; PageNum: Integer);
const
  DWriteTextAlignment: array[TAlignment] of DWRITE_TEXT_ALIGNMENT =
    (DWRITE_TEXT_ALIGNMENT_LEADING, DWRITE_TEXT_ALIGNMENT_TRAILING, DWRITE_TEXT_ALIGNMENT_CENTER);
var
  I, Y, CurLine: Integer;
  AStr: string;
  AItem: THeaderFooterItem;
  TheAlignment: TAlignment;
  TextFormat: TSynTextFormat;
  Layout: TSynTextLayout;
begin
  if (FFrameHeight <= 0) then Exit; // No header/footer

  DrawFrame(RT);

  if FType = hftHeader then
    Y := FMargins.PHeader - FFrameHeight
  else
    Y := FMargins.PFooter;
  Y := Y + FMargins.PHFInternalMargin; // Add the specified internal margin

  CurLine := 1;
  for I := 0 to FItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(FItems[I]);

    TextFormat := TSynTextFormat.Create(AItem.Font);
    if AItem.LineNumber <> CurLine then
    begin
      Y := Y + FLineHeights[CurLine - 1];
      CurLine := AItem.LineNumber;
    end;

    AStr := AItem.GetText(FNumPages, PageNum, FRomanNumbers, FTitle, FTime, FDate);

    Layout := TSynTextLayout.Create(TextFormat, PChar(AStr), AStr.Length,
      FMargins.PRightHFTextIndent - FMargins.PLeftHFTextIndent,
      FLineHeights[CurLine - 1]);
    Layout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
    //Find the alignment of the header/footer item - check for MirrorPosition
    TheAlignment := AItem.Alignment;
    if MirrorPosition and ((PageNum mod 2) = 0) then
    begin
      case AItem.Alignment of
        taRightJustify: TheAlignment := taLeftJustify;
        taLeftJustify: TheAlignment := taRightJustify;
      end;
    end;
    Layout.SetTextAlignment(DWriteTextAlignment[TheAlignment]);

    Layout.Draw(RT, FMargins.PLeftHFTextIndent, Y, AItem.Font.Color);
  end;
end;

procedure THeaderFooter.Assign(Source: TPersistent);
var
  Src: THeaderFooter;
  i: Integer;
begin
  if (Source <> nil) and (Source is THeaderFooter) then begin
    Src := THeaderFooter(Source);
    Clear;
    FType := Src.FType;
    FFrameTypes := Src.FFrameTypes;
    FShadedColor := Src.FShadedColor;
    FLineColor := Src.FLineColor;
    for i := 0 to Src.FItems.Count - 1 do begin
      with THeaderFooterItem(Src.FItems[i]) do
        Add(Text, Font, Alignment, LineNumber);
    end;
    FDefaultFont.Assign(Src.FDefaultFont);
    FRomanNumbers := Src.FRomanNumbers;
    FMirrorPosition := Src.FMirrorPosition;
  end else
    inherited Assign(Source);
end;

function THeaderFooter.Count: Integer;
begin
  Result := FItems.Count;
end;

function THeaderFooter.Get(Index: Integer): THeaderFooterItem;
begin
  Result := THeaderFooterItem(FItems[Index]);
end;

function THeaderFooter.GetAsString: string;
var
  I: Integer;
begin
  FixLines;
  Result := '';
  for I := 0 to FItems.Count - 1 do begin
    if Result <> '' then Result := Result + '/';
    Result := Result + EncodeString(THeaderFooterItem(FItems[I]).AsString);
  end; //for
end;

procedure THeaderFooter.SetAsString(const Value: string);
var
  item: THeaderFooterItem;
  s: string;
begin
  Clear;
  item := THeaderFooterItem.Create;
  try
    s := Value;
    while s <> '' do
    begin
      item.AsString := DecodeString(GetFirstEl(s, '/'));
      Add(item.Text, item.Font, item.Alignment, item.LineNumber);
    end; 
  finally
    item.Free;
  end;
end;

procedure THeaderFooter.LoadFromStream(AStream: TStream);
var
  Num, I: Integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  bufSize: Integer;
  buffer: PAnsiChar;
begin
  with AStream do begin
    // read header/footer properties first
    Read(FFrameTypes, SizeOf(FFrameTypes));
    Read(FShadedColor, SizeOf(FShadedColor));
    Read(FLineColor, SizeOf(FLineColor));
    Read(FRomanNumbers, SizeOf(FRomanNumbers));
    Read(FMirrorPosition, SizeOf(FMirrorPosition));
    // font
    Read(aCharset, SizeOf(aCharset));
    Read(aColor, SizeOf(aColor));
    Read(aHeight, SizeOf(aHeight));
    Read(bufSize, SizeOf(bufSize));
    GetMem(buffer, bufSize+1);
    try
      Read(buffer^, bufSize);
      buffer[bufSize] := #0;
      aName := string(buffer);
    finally
      FreeMem(buffer);
    end;
    Read(aPitch, SizeOf(aPitch));
    Read(aSize, SizeOf(aSize));
    Read(aStyle, SizeOf(aStyle));
    FDefaultFont.Charset := aCharset;
    FDefaultFont.Color   := aColor;
    FDefaultFont.Height  := aHeight;
    FDefaultFont.Name    := aName;
    FDefaultFont.Pitch   := aPitch;
    FDefaultFont.Size    := aSize;
    FDefaultFont.Style   := aStyle;
    // now read in the items
    Read(Num, SizeOf(Num));
    while Num > 0 do
    begin
      // load headerfooter items from stream
      I := Add('', nil, taLeftJustify, 1);
      Get(I).LoadFromStream(AStream);
      Dec(Num);
    end;
  end;
end;

procedure THeaderFooter.SaveToStream(AStream: TStream);
var
  I, Num: Integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  aName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen: Integer;
begin
  with AStream do begin
    // write the header/footer properties first
    Write(FFrameTypes, SizeOf(FFrameTypes));
    Write(FShadedColor, SizeOf(FShadedColor));
    Write(FLineColor, SizeOf(FLineColor));
    Write(FRomanNumbers, SizeOf(FRomanNumbers));
    Write(FMirrorPosition, SizeOf(FMirrorPosition));
    // font
    aCharset := FDefaultFont.Charset;
    aColor   := FDefaultFont.Color;
    aHeight  := FDefaultFont.Height;
    aName    := FDefaultFont.Name;
    aPitch   := FDefaultFont.Pitch;
    aSize    := FDefaultFont.Size;
    aStyle   := FDefaultFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(aName);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(AnsiString(aName))^, Length(aName));
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));

    // now write the items
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

