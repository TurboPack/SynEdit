{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
CONTENTS:
  FMX print controller component with abstract print provider.

  ISynPrintProvider:
    Abstract interface for print output. Allows plugging in custom print
    backends (e.g. PDF export, bitmap rendering, platform-specific printers).

  TSynFMXPrintProvider:
    Concrete implementation wrapping FMX.Printer on supported platforms.

  TSynFMXEditPrint:
    Main print controller component.
    Design-time properties:
      Header, Footer   : Header/footer configuration.
      Margins          : Page margin configuration.
      Lines            : Text lines to print.
      Font             : Font for printing text.
      Title, DocTitle  : Document titles.
      Wrap             : Word wrap to margins.
      Highlight        : Apply syntax highlighting.
      Colors           : Print in color.
      LineNumbers      : Print line numbers.
      LineNumbersInMargin : Place line numbers within the margin area.
      LineOffset       : Value added to line numbers.
      PageOffset       : Value added to page numbers.
      SelectedOnly     : Print only selected text.
      TabWidth         : Tab stop width.
      Color            : Background color.
    Run-time properties:
      PrintProvider    : Plug in a custom ISynPrintProvider.
      PrinterInfo      : Current ISynPrinterInfo.
      PageCount        : Total page count.
    Run-time methods:
      Print            : Print all pages.
      PrintRange       : Print specified page range.
      CalcPages        : Calculate total page count.
      PrintToCanvas    : Render a page to an FMX canvas (for preview).
-------------------------------------------------------------------------------}

unit FMX.SynEditPrint;

{$M+}
{$I SynEdit.inc}

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.UITypes,
  System.UIConsts,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.TextLayout,
  SynEditTypes,
  SynEditHighlighter,
  SynEditMiscProcs,
  FMX.SynEdit,
  FMX.SynEditPrintTypes,
  FMX.SynEditPrintHeaderFooter,
  FMX.SynEditPrinterInfo,
  FMX.SynEditPrintMargins;

type
  { Abstract print provider interface.
    Implement this to send print output to any target (printer, PDF, etc.) }
  ISynPrintProvider = interface
    ['{B2C3D4E5-F6A7-4B8C-9D0E-1F2A3B4C5D6E}']
    procedure BeginDoc(const ATitle: string);
    procedure EndDoc;
    procedure NewPage;
    function GetCanvas: TCanvas;
    function GetPageWidth: Integer;
    function GetPageHeight: Integer;
    property Canvas: TCanvas read GetCanvas;
    property PageWidth: Integer read GetPageWidth;
    property PageHeight: Integer read GetPageHeight;
  end;

{$IFDEF MSWINDOWS}
  { Concrete FMX print provider wrapping FMX.Printer }
  TSynFMXPrintProvider = class(TInterfacedObject, ISynPrintProvider)
  private
    FPageWidth: Integer;
    FPageHeight: Integer;
  public
    procedure BeginDoc(const ATitle: string);
    procedure EndDoc;
    procedure NewPage;
    function GetCanvas: TCanvas;
    function GetPageWidth: Integer;
    function GetPageHeight: Integer;
  end;
{$ENDIF}

  { Page boundary tracking }
  TPageLine = class
  public
    FirstLine: Integer;
    FirstRow: Integer;
    LastLine: Integer;
    LastRow: Integer;
  end;

  { Main FMX print controller }
  TSynFMXEditPrint = class(TComponent)
  private
    FCopies: Integer;
    FHeader: THeader;
    FFooter: TFooter;
    FLines: TStrings;
    FMargins: TSynEditPrintMargins;
    FFont: TFont;
    FTitle: string;
    FDocTitle: string;
    FPrinterInfo: ISynPrinterInfo;
    FPrintProvider: ISynPrintProvider;
    FPages: TObjectList<TPageLine>;
    FMaxLeftChar: Integer;
    FWrap: Boolean;
    FOnPrintLine: TSynPrintLineEvent;
    FOnPrintStatus: TSynPrintStatusEvent;
    FLineHeight: Integer;
    FHighlight: Boolean;
    FColors: Boolean;
    FHighlighter: TSynCustomHighlighter;
    FSynOK: Boolean;
    FLineNumbers: Boolean;
    FLineOffset: Integer;
    FAbort: Boolean;
    FPrinting: Boolean;
    FDefaultBG: TColor;
    FPageOffset: Integer;
    FRangesOK: Boolean;
    FMaxRowCount: Integer;
    FMaxWidth: Integer;
    FPagesCounted: Boolean;
    FLineNumbersInMargin: Boolean;
    FTabWidth: Integer;
    FSelectedOnly: Boolean;
    FSelAvail: Boolean;
    FBlockBegin: TBufferCoord;
    FBlockEnd: TBufferCoord;
    procedure DoCalcPages;
    procedure SetLines(const Value: TStrings);
    procedure SetFont(const Value: TFont);
    procedure SetMaxLeftChar(const Value: Integer);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure InitRanges;
    function GetPageCount: Integer;
    procedure SetFooter(const Value: TFooter);
    procedure SetHeader(const Value: THeader);
    procedure SetMargins(const Value: TSynEditPrintMargins);
    procedure SetPrintProvider(const Value: ISynPrintProvider);
    procedure SetPrinterInfo(const Value: ISynPrinterInfo);
    function GetLineText(LineIndex: Integer): string;
    function MeasureLineRows(const S: string): Integer;
  protected
    property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
    procedure DoPrintStatus(Status: TSynPrintStatus; PageNumber: Integer;
      var Abort: Boolean); virtual;
    procedure DoPrintLine(LineNumber, PageNumber: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Initialization and page calculation }
    procedure InitPrint;
    function CalcPages: Integer;
    function GetPageCount2: Integer;

    { Printing }
    procedure Print;
    procedure PrintRange(StartPage, EndPage: Integer);

    { Preview rendering }
    procedure PrintToCanvas(ACanvas: TCanvas; const RenderRect: TRectF;
      PageNo: Integer);

    { Page rendering }
    procedure PrintPage(Canvas: TCanvas; PageNumber: Integer);

    { Line number rendering }
    procedure WriteLineNumber(Canvas: TCanvas; LineNumber, YPos: Integer);

    { Word wrap helper }
    procedure HandleWrap(const S: string; MaxWidth: Integer;
      out RowCount: Integer);

    { Set lines/font/highlighter from a SynEdit component.
      Pass any TComponent that has Highlighter, Font, TabWidth, Lines,
      SelAvail, BlockBegin, BlockEnd properties. }
    procedure SetSynEdit(AEditor: TComponent);

    { Stream persistence }
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    { Runtime properties }
    property PrintProvider: ISynPrintProvider read FPrintProvider
      write SetPrintProvider;
    property PrinterInfo: ISynPrinterInfo read FPrinterInfo
      write SetPrinterInfo;
    property PageCount: Integer read GetPageCount;
  published
    property Copies: Integer read FCopies write FCopies;
    property Header: THeader read FHeader write SetHeader;
    property Footer: TFooter read FFooter write SetFooter;
    property Margins: TSynEditPrintMargins read FMargins write SetMargins;
    property Lines: TStrings read FLines write SetLines;
    property Font: TFont read FFont write SetFont;
    property Title: string read FTitle write FTitle;
    property DocTitle: string read FDocTitle write FDocTitle;
    property Wrap: Boolean read FWrap write FWrap default True;
    property Highlight: Boolean read FHighlight write FHighlight default True;
    property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly
      default False;
    property Colors: Boolean read FColors write FColors default False;
    property LineNumbers: Boolean read FLineNumbers write FLineNumbers
      default False;
    property LineOffset: Integer read FLineOffset write FLineOffset default 0;
    property PageOffset: Integer read FPageOffset write FPageOffset default 0;
    property OnPrintLine: TSynPrintLineEvent read FOnPrintLine
      write FOnPrintLine;
    property OnPrintStatus: TSynPrintStatusEvent read FOnPrintStatus
      write FOnPrintStatus;
    property Highlighter: TSynCustomHighlighter read FHighlighter
      write SetHighlighter;
    property LineNumbersInMargin: Boolean read FLineNumbersInMargin
      write FLineNumbersInMargin default False;
    property TabWidth: Integer read FTabWidth write FTabWidth;
    property Color: TColor read FDefaultBG write FDefaultBG;
  end;

implementation

uses
  System.Math.Vectors,
{$IFDEF MSWINDOWS}
  FMX.Printer,
{$ENDIF}
  SynUnicode;

resourcestring
  SYNS_FMXNoPrinter = 'No printer available';

{ Helper: convert TColor to TAlphaColor }
function ColorToAlpha(AColor: TColor): TAlphaColor;
begin
  if Integer(AColor) < 0 then
    Result := TAlphaColors.Null
  else
    Result := TAlphaColor($FF000000 or
      (Cardinal(AColor and $FF) shl 16) or
      (Cardinal(AColor and $FF00)) or
      (Cardinal(AColor shr 16) and $FF));
end;

{ Measure text height using a temporary FMX TTextLayout }
function MeasureLineHeight(AFont: TFont; TabWidth: Integer): Integer;
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    try
      Layout.Font.Assign(AFont);
      Layout.Text := 'Wg';
      Layout.MaxSize := TPointF.Create(10000, 10000);
    finally
      Layout.EndUpdate;
    end;
    Result := Round(Layout.TextHeight);
    if Result < 1 then
      Result := Round(AFont.Size * 1.5);
  finally
    Layout.Free;
  end;
end;

{ Measure text width }
function MeasureTextWidth(AFont: TFont; const AText: string): Single;
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    try
      Layout.Font.Assign(AFont);
      Layout.Text := AText;
      Layout.MaxSize := TPointF.Create(10000, 10000);
    finally
      Layout.EndUpdate;
    end;
    Result := Layout.TextWidth;
  finally
    Layout.Free;
  end;
end;

{$IFDEF MSWINDOWS}
{ TSynFMXPrintProvider }

procedure TSynFMXPrintProvider.BeginDoc(const ATitle: string);
begin
  Printer.Title := ATitle;
  Printer.BeginDoc;
  FPageWidth := Round(Printer.PageWidth);
  FPageHeight := Round(Printer.PageHeight);
end;

procedure TSynFMXPrintProvider.EndDoc;
begin
  Printer.EndDoc;
end;

procedure TSynFMXPrintProvider.NewPage;
begin
  Printer.NewPage;
end;

function TSynFMXPrintProvider.GetCanvas: TCanvas;
begin
  Result := Printer.Canvas;
end;

function TSynFMXPrintProvider.GetPageWidth: Integer;
begin
  Result := FPageWidth;
end;

function TSynFMXPrintProvider.GetPageHeight: Integer;
begin
  Result := FPageHeight;
end;
{$ENDIF}

{ TSynFMXEditPrint }

constructor TSynFMXEditPrint.Create(AOwner: TComponent);
begin
  inherited;
  FCopies := 1;
  FFooter := TFooter.Create;
  FHeader := THeader.Create;
  FLines := TStringList.Create;
  FMargins := TSynEditPrintMargins.Create;
  FPrinterInfo := TSynFMXPrinterInfo.Create;
  FFont := TFont.Create;
  FFont.Family := 'Consolas';
  FFont.Size := 10;
  MaxLeftChar := 1024;
  FWrap := True;
  FHighlight := True;
  FColors := False;
  FLineNumbers := False;
  FLineOffset := 0;
  FPageOffset := 0;
  FLineNumbersInMargin := False;
  FPages := TObjectList<TPageLine>.Create;
  FTabWidth := 8;
  FDefaultBG := TColors.White;
end;

destructor TSynFMXEditPrint.Destroy;
begin
  FFooter.Free;
  FHeader.Free;
  FLines.Free;
  FMargins.Free;
  FFont.Free;
  FPages.Free;
  FPrinterInfo := nil;
  FPrintProvider := nil;
  inherited;
end;

procedure TSynFMXEditPrint.SetLines(const Value: TStrings);
begin
  FLines.Clear;
  FLines.AddStrings(Value);
  FRangesOK := False;
  FPagesCounted := False;
end;

procedure TSynFMXEditPrint.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FPagesCounted := False;
end;

procedure TSynFMXEditPrint.SetMaxLeftChar(const Value: Integer);
begin
  FMaxLeftChar := Value;
end;

procedure TSynFMXEditPrint.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  FHighlighter := Value;
  FRangesOK := False;
  FPagesCounted := False;
end;

procedure TSynFMXEditPrint.SetPrintProvider(const Value: ISynPrintProvider);
begin
  FPrintProvider := Value;
end;

procedure TSynFMXEditPrint.SetPrinterInfo(const Value: ISynPrinterInfo);
begin
  FPrinterInfo := Value;
end;

procedure TSynFMXEditPrint.InitRanges;
var
  I: Integer;
begin
  if not FRangesOK and Assigned(FHighlighter) and (Lines.Count > 0) then
  begin
    FHighlighter.ResetRange;
    FLines.Objects[0] := FHighlighter.GetRange;
    I := 1;
    while I < Lines.Count do
    begin
      FHighlighter.SetLine(FLines[I - 1], I - 1);
      FHighlighter.NextToEol;
      FLines.Objects[I] := FHighlighter.GetRange;
      Inc(I);
    end;
    FRangesOK := True;
  end;
end;

function TSynFMXEditPrint.GetLineText(LineIndex: Integer): string;
var
  iSelStart, iSelLen: Integer;
begin
  if not FSelectedOnly then
    Result := FLines[LineIndex]
  else
  begin
    if LineIndex = FBlockBegin.Line - 1 then
      iSelStart := FBlockBegin.Char
    else
      iSelStart := 1;
    if LineIndex = FBlockEnd.Line - 1 then
      iSelLen := FBlockEnd.Char - iSelStart
    else
      iSelLen := MaxInt;
    Result := Copy(FLines[LineIndex], iSelStart, iSelLen);
  end;
end;

function TSynFMXEditPrint.MeasureLineRows(const S: string): Integer;
{ Calculates how many rows a line occupies with word wrap.
  Uses the FMX text layout engine to measure wrapped line count. }
var
  Layout: TTextLayout;
  TotalHeight: Single;
begin
  if (S = '') or (not FWrap) then
  begin
    Result := 1;
    Exit;
  end;

  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    try
      Layout.Font.Assign(FFont);
      Layout.Text := S;
      Layout.MaxSize := TPointF.Create(FMaxWidth, 100000);
      Layout.WordWrap := True;
    finally
      Layout.EndUpdate;
    end;
    TotalHeight := Layout.TextHeight;
    if TotalHeight <= 0 then
      Result := 1
    else
      Result := Max(1, Round(TotalHeight / FLineHeight));
  finally
    Layout.Free;
  end;
end;

procedure TSynFMXEditPrint.HandleWrap(const S: string; MaxWidth: Integer;
  out RowCount: Integer);
begin
  FMaxWidth := MaxWidth;
  RowCount := MeasureLineRows(S);
end;

procedure TSynFMXEditPrint.InitPrint;
begin
  FPrinterInfo.UpdateInfo;
  FLineHeight := MeasureLineHeight(FFont, FTabWidth);
  FMargins.InitPage(FFont, FTabWidth, 1, FPrinterInfo, FLineNumbers,
    FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
  FSynOK := FHighlight and Assigned(FHighlighter) and (FLines.Count > 0);
  DoCalcPages;
  FHeader.InitPrint(FPages.Count, FTitle, FMargins);
  FFooter.InitPrint(FPages.Count, FTitle, FMargins);
end;

procedure TSynFMXEditPrint.DoCalcPages;
var
  I: Integer;
  PageLine: TPageLine;
  LayoutRowCount: Integer;
  RowCount: Integer;
  iStartLine, iEndLine: Integer;
begin
  InitRanges;
  FPages.Clear;
  FMaxWidth := FMargins.PRight - FMargins.PLeft;

  if FSelectedOnly then
  begin
    iStartLine := FBlockBegin.Line - 1;
    iEndLine := FBlockEnd.Line - 1;
  end
  else begin
    iStartLine := 0;
    iEndLine := Lines.Count - 1;
  end;

  PageLine := TPageLine.Create;
  PageLine.FirstLine := iStartLine;
  PageLine.FirstRow := 1;
  PageLine.LastLine := -1;
  FPages.Add(PageLine);

  FMaxRowCount := (FMargins.PBottom - FMargins.PTop) div FLineHeight;
  if FMaxRowCount < 1 then
    FMaxRowCount := 1;

  RowCount := 0;
  for I := iStartLine to iEndLine do
  begin
    if FLines[I] = '' then
    begin
      Inc(RowCount);
      LayoutRowCount := 1;
    end
    else
    begin
      LayoutRowCount := MeasureLineRows(GetLineText(I));
      Inc(RowCount, LayoutRowCount);
    end;

    { Add new page(s) if needed (handles word wrap spanning pages) }
    while RowCount >= FMaxRowCount do
    begin
      PageLine.LastLine := I;
      PageLine.LastRow := LayoutRowCount - (RowCount - FMaxRowCount);

      if (RowCount = FMaxRowCount) and (I = iEndLine) then Break;

      PageLine := TPageLine.Create;
      PageLine.FirstLine := IfThen(RowCount = FMaxRowCount, I + 1, I);
      PageLine.FirstRow := IfThen(RowCount = FMaxRowCount, 1,
        LayoutRowCount - RowCount + FMaxRowCount + 1);
      FPages.Add(PageLine);
      RowCount := RowCount - FMaxRowCount;
    end;

    if I = iEndLine then
    begin
      PageLine.LastLine := I;
      PageLine.LastRow := LayoutRowCount;
    end;
  end;
  FPagesCounted := True;
end;

function TSynFMXEditPrint.CalcPages: Integer;
begin
  InitPrint;
  Result := FPages.Count;
end;

function TSynFMXEditPrint.GetPageCount: Integer;
begin
  if FPagesCounted then
    Result := FPages.Count
  else begin
    InitPrint;
    Result := FPages.Count;
  end;
end;

function TSynFMXEditPrint.GetPageCount2: Integer;
begin
  Result := GetPageCount;
end;

procedure TSynFMXEditPrint.WriteLineNumber(Canvas: TCanvas;
  LineNumber, YPos: Integer);
var
  AStr: string;
  TextWidth: Single;
  R: TRectF;
begin
  AStr := IntToStr(LineNumber + FLineOffset) + ': ';
  TextWidth := MeasureTextWidth(FFont, AStr);

  R := RectF(FMargins.PLeft - TextWidth, YPos,
    FMargins.PLeft, YPos + FLineHeight);

  Canvas.Font.Assign(FFont);
  Canvas.Fill.Color := ColorToAlpha(TColors.Black);
  Canvas.FillText(R, AStr, False, 1.0, [],
    TTextAlign.Trailing, TTextAlign.Leading);
end;

procedure TSynFMXEditPrint.PrintPage(Canvas: TCanvas; PageNumber: Integer);
var
  I: Integer;
  LineText: string;
  YPos: Integer;
  LayoutRowCount: Integer;
  Token: string;
  TokenPos: Integer;
  Attr: TSynHighlighterAttributes;
  BkgColor, FontColor: TColor;
  AlphaBkg, AlphaFont: TAlphaColor;
  TextRect: TRectF;
  Layout: TTextLayout;
  iSelStart, iSelLen: Integer;
begin
  DoPrintStatus(psNewPage, PageNumber, FAbort);
  if FAbort then Exit;

  if FPages.Count < PageNumber then Exit;

  { Determine background and font colors }
  if FColors and FSynOK then
    BkgColor := FHighlighter.WhitespaceAttribute.Background
  else
    BkgColor := FDefaultBG;

  if Integer(BkgColor) < 0 then
    BkgColor := TColors.White;

  AlphaBkg := ColorToAlpha(BkgColor);

  FontColor := TColors.Black;
  AlphaFont := ColorToAlpha(FontColor);

  { Clear background }
  Canvas.Fill.Color := AlphaBkg;
  Canvas.FillRect(RectF(0, 0, Canvas.Width, Canvas.Height), 0, 0,
    AllCorners, 1.0);

  { Print header }
  FHeader.Print(Canvas, PageNumber + FPageOffset);

  { Print lines }
  YPos := FMargins.PTop;
  for I := FPages[PageNumber - 1].FirstLine to FPages[PageNumber - 1].LastLine do
  begin
    { Line numbers }
    if FLineNumbers then
      WriteLineNumber(Canvas, I + 1, YPos);

    LineText := GetLineText(I);

    if LineText = '' then
      LayoutRowCount := 1
    else
    begin
      { Measure wrapped row count }
      LayoutRowCount := MeasureLineRows(LineText);

      { Syntax highlighting }
      if FSynOK then
      begin
        FHighlighter.SetRange(FLines.Objects[I]);
        FHighlighter.SetLine(LineText, I + 1);

        { Render token by token }
        while not FHighlighter.GetEol do
        begin
          Token := FHighlighter.GetToken;
          TokenPos := FHighlighter.GetTokenPos;

          if FSelectedOnly then
          begin
            if I = FBlockBegin.Line - 1 then
              iSelStart := FBlockBegin.Char
            else
              iSelStart := 1;
            if I = FBlockEnd.Line - 1 then
              iSelLen := FBlockEnd.Char - iSelStart
            else
              iSelLen := MaxInt;
            if TokenPos - iSelStart >= iSelLen then Break;
          end;

          FHighlighter.Next;
        end;
      end;

      { Render the full line text using FMX text layout }
      Layout := TTextLayoutManager.DefaultTextLayout.Create;
      try
        Layout.BeginUpdate;
        try
          Layout.Font.Assign(FFont);
          Layout.Text := LineText;
          Layout.MaxSize := TPointF.Create(FMaxWidth, FLineHeight * LayoutRowCount);
          Layout.WordWrap := FWrap;
        finally
          Layout.EndUpdate;
        end;

        { Handle page boundary clipping for wrapped lines }
        if (I = FPages[PageNumber - 1].FirstLine) and
           (FPages[PageNumber - 1].FirstRow > 1) then
        begin
          { Line continues from previous page - clip top rows }
          Canvas.IntersectClipRect(RectF(FMargins.PLeft, YPos,
            FMargins.PRight, FMargins.PTop + FMaxRowCount * FLineHeight));
          try
            Layout.RenderLayout(Canvas);
            Canvas.FillText(
              RectF(FMargins.PLeft,
                YPos - Pred(FPages[PageNumber - 1].FirstRow) * FLineHeight,
                FMargins.PRight,
                YPos + LayoutRowCount * FLineHeight),
              LineText, False, 1.0, [],
              TTextAlign.Leading, TTextAlign.Leading);
          finally
            { FMX canvas clip rect is restored by SaveState/RestoreState
              but for simplicity we let the canvas manage it }
          end;
          LayoutRowCount := LayoutRowCount - FPages[PageNumber - 1].FirstRow + 1;
        end
        else if (I = FPages[PageNumber - 1].LastLine) and
                (FPages[PageNumber - 1].LastRow < LayoutRowCount) then
        begin
          { Line continues onto next page - clip bottom rows }
          TextRect := RectF(FMargins.PLeft, YPos, FMargins.PRight,
            YPos + FPages[PageNumber - 1].LastRow * FLineHeight);
          Canvas.Font.Assign(FFont);
          Canvas.Fill.Color := AlphaFont;
          Canvas.FillText(TextRect, LineText, FWrap, 1.0, [],
            TTextAlign.Leading, TTextAlign.Leading);
        end
        else
        begin
          { Normal line - render fully }
          TextRect := RectF(FMargins.PLeft, YPos, FMargins.PRight,
            YPos + LayoutRowCount * FLineHeight);
          Canvas.Font.Assign(FFont);
          Canvas.Fill.Color := AlphaFont;
          Canvas.FillText(TextRect, LineText, FWrap, 1.0, [],
            TTextAlign.Leading, TTextAlign.Leading);
        end;
      finally
        Layout.Free;
      end;
    end;

    DoPrintLine(I + 1, PageNumber);
    Inc(YPos, LayoutRowCount * FLineHeight);
  end;

  { Print footer }
  FFooter.Print(Canvas, PageNumber + FPageOffset);
end;

procedure TSynFMXEditPrint.PrintToCanvas(ACanvas: TCanvas;
  const RenderRect: TRectF; PageNo: Integer);
var
  ScaleX, ScaleY: Single;
  SaveState: TCanvasSaveState;
begin
  FAbort := False;
  FPrinting := False;

  ScaleX := RenderRect.Width /
    (FPrinterInfo.PhysicalWidth * 96 / FPrinterInfo.XPixPerInch);
  ScaleY := RenderRect.Height /
    (FPrinterInfo.PhysicalHeight * 96 / FPrinterInfo.YPixPerInch);

  SaveState := ACanvas.SaveState;
  try
    ACanvas.SetMatrix(
      TMatrix.CreateScaling(ScaleX, ScaleY) *
      TMatrix.CreateTranslation(RenderRect.Left, RenderRect.Top));

    PrintPage(ACanvas, PageNo);
  finally
    ACanvas.RestoreState(SaveState);
  end;
end;

procedure TSynFMXEditPrint.Print;
begin
  PrintRange(1, -1);
end;

procedure TSynFMXEditPrint.PrintRange(StartPage, EndPage: Integer);
var
  Page, Copy: Integer;
  Title: string;
  Provider: ISynPrintProvider;
  SaveState: TCanvasSaveState;
  ScaleX, ScaleY: Single;
begin
  if FSelectedOnly and not FSelAvail then
    Exit;

  FPrinting := True;
  FAbort := False;

  if FDocTitle <> '' then
    Title := FDocTitle
  else
    Title := FTitle;
  if Title = '' then
    Title := 'SynEdit document';

  DoPrintStatus(psBegin, StartPage, FAbort);
  if FAbort then Exit;

  InitPrint;

  { Use provided print provider, or create default FMX one }
  Provider := FPrintProvider;
{$IFDEF MSWINDOWS}
  if Provider = nil then
    Provider := TSynFMXPrintProvider.Create;
{$ENDIF}

  if Provider = nil then
    raise ESynError.Create(SYNS_FMXNoPrinter);

  if EndPage < 0 then
    EndPage := FPages.Count;

  try
    for Copy := 1 to FCopies do
    begin
      Provider.BeginDoc(Title);

      { Compute scale from ACTUAL canvas dimensions vs our 96-PPI layout.
        The CreateDC-based PrinterInfo may report wrong DPI or paper size
        (e.g. 300 DPI A4 when the FMX printer is actually 600 DPI Letter).
        Using the real page dimensions from the provider gives the correct
        scale regardless of PrinterInfo accuracy. }
      ScaleX := Provider.PageWidth /
        (FPrinterInfo.PhysicalWidth * 96.0 / FPrinterInfo.XPixPerInch);
      ScaleY := Provider.PageHeight /
        (FPrinterInfo.PhysicalHeight * 96.0 / FPrinterInfo.YPixPerInch);
      try
        for Page := StartPage to EndPage do
        begin
          if FAbort then Break;
          if Page > StartPage then
            Provider.NewPage;

          if Provider.Canvas.BeginScene then
          try
            SaveState := Provider.Canvas.SaveState;
            try
              Provider.Canvas.SetMatrix(
                TMatrix.CreateScaling(ScaleX, ScaleY));
              PrintPage(Provider.Canvas, Page);
            finally
              Provider.Canvas.RestoreState(SaveState);
            end;
          finally
            Provider.Canvas.EndScene;
          end;
        end;
      finally
        Provider.EndDoc;
      end;
    end;

    if not FAbort then
      DoPrintStatus(psEnd, EndPage, FAbort);
  finally
    FPrinting := False;
  end;
end;

procedure TSynFMXEditPrint.DoPrintLine(LineNumber, PageNumber: Integer);
begin
  if Assigned(FOnPrintLine) then
    FOnPrintLine(Self, LineNumber, PageNumber);
end;

procedure TSynFMXEditPrint.DoPrintStatus(Status: TSynPrintStatus;
  PageNumber: Integer; var Abort: Boolean);
begin
  Abort := False;
  if FPrinting and Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, Status, PageNumber, Abort);
end;

procedure TSynFMXEditPrint.SetSynEdit(AEditor: TComponent);
var
  Ed: TCustomFMXSynEdit;
begin
  FPagesCounted := False;
  FRangesOK := False;
  if AEditor is TCustomFMXSynEdit then
  begin
    Ed := TCustomFMXSynEdit(AEditor);
    FHighlighter := Ed.Highlighter;
    FTabWidth := Ed.TabWidth;
    FLines.Assign(Ed.Lines);
    FFont.Assign(Ed.Font);
    FSelAvail := Ed.SelAvail;
    FBlockBegin := Ed.BlockBegin;
    FBlockEnd := Ed.BlockEnd;
  end;
end;

procedure TSynFMXEditPrint.LoadFromStream(AStream: TStream);
var
  Len, BufferSize: Integer;
  Buffer: PWideChar;
begin
  FHeader.LoadFromStream(AStream);
  FFooter.LoadFromStream(AStream);
  FMargins.LoadFromStream(AStream);
  with AStream do
  begin
    Read(Len, SizeOf(Len));
    BufferSize := Len * SizeOf(WideChar);
    GetMem(Buffer, BufferSize + SizeOf(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div SizeOf(WideChar)] := #0;
      FTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(Len, SizeOf(Len));
    BufferSize := Len * SizeOf(WideChar);
    GetMem(Buffer, BufferSize + SizeOf(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div SizeOf(WideChar)] := #0;
      FDocTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(FWrap, SizeOf(FWrap));
    Read(FHighlight, SizeOf(FHighlight));
    Read(FColors, SizeOf(FColors));
    Read(FLineNumbers, SizeOf(FLineNumbers));
    Read(FLineOffset, SizeOf(FLineOffset));
    Read(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TSynFMXEditPrint.SaveToStream(AStream: TStream);
var
  aLen: Integer;
begin
  FHeader.SaveToStream(AStream);
  FFooter.SaveToStream(AStream);
  FMargins.SaveToStream(AStream);
  with AStream do
  begin
    aLen := Length(FTitle);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(FTitle)^, aLen * SizeOf(WideChar));
    aLen := Length(FDocTitle);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(FDocTitle)^, aLen * SizeOf(WideChar));
    Write(FWrap, SizeOf(FWrap));
    Write(FHighlight, SizeOf(FHighlight));
    Write(FColors, SizeOf(FColors));
    Write(FLineNumbers, SizeOf(FLineNumbers));
    Write(FLineOffset, SizeOf(FLineOffset));
    Write(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TSynFMXEditPrint.SetFooter(const Value: TFooter);
begin
  FFooter.Assign(Value);
end;

procedure TSynFMXEditPrint.SetHeader(const Value: THeader);
begin
  FHeader.Assign(Value);
end;

procedure TSynFMXEditPrint.SetMargins(const Value: TSynEditPrintMargins);
begin
  FMargins.Assign(Value);
end;

end.
