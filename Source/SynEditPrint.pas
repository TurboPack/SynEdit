{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrint.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
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
  Print controller component.
    Allows setting margins, headers and footers.

  Design time properties:
    Header        : Class property to set properties for headers -
                    see CSynEditHeaderFooter.pas
    Footer        : Class property to set properties for footers -
                    see CSynEditHeaderFooter.pas
    Margins       : Class property to set properties for margins -
                    see CSynEditPrintMargins.pas
    Lines         : The lines that should be printed (see also SynEdit the
                    property below)
    Font          : The font the lines should be printed in (see also SynEdit
                    the property below)
    Title         : A title - can be referenced in headers/footers by using the
                    $TITLE$ macro
    Wrap          : Wrap text to margins
    Highlight     : Highlight text
    Colors        : Print in colors
    LineNumbers   : Print line numbers
    LineOffset    : Value added to linenumbers when printing
    PageOffset    : Value added to pagenumbers when printing
    OnPrintLine   : Fired when a line is printed
    OnPrintStatus : Fired at Beginning, End and when a new page is started
    Highlighter   : The highlighter used for highlighting the text (see also the
                    SynEdit property below)
    LineNumbersInMargin : If True line numbers are printed in the left margin,
                          else left margin is increased by width of line
                          number text.
    SelectedOnly  : Print only the selected Area
  Run-time properties:
    DocTitle    : Used to display the document name in the print queue monitor
    PrinterInfo : Read only. Returns info on printer (used internally)
    PageCount   : Returns the total number of pages;
    SynEdit     : By setting SynEdit to a specific TSynEdit component, the
                  properties Lines, Font and Highlighter are automatically
                  set to the corresponding values of the TSynEdit component
  Run-time methods:
    InitPrint     : Used internally by the TSynEditPrintPreview component
    PaintPreview  : Used internally by the TSynEditPrintPreview component
    Print         : Prints the contents of the Lines property
    PrintRange(StartPage,EndPage) : Prints the specified page-range (both inclusive)
-------------------------------------------------------------------------------}

unit SynEditPrint;

{$M+}
{$I SynEdit.inc}

interface

uses
  Winapi.Windows,
  Winapi.D2D1,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Printers,
  SynEdit,
  SynEditTypes,
  SynEditPrintTypes,
  SynEditPrintHeaderFooter,
  SynEditPrinterInfo,
  SynEditPrintMargins,
  SynEditMiscProcs,
  SynEditHighlighter,
  SynUnicode,
  SynDWrite;

type
  TPageLine = class
  public
    FirstLine: Integer;
    FirstRow: Integer;
    LastLine: Integer;
    LastRow: Integer;
  end;

  //The actual print controller object
  TSynEditPrint = class(TComponent)
  private
    FCopies: Integer;                                                           
    FFooter: TFooter;
    FHeader: THeader;
    FLines: TStrings;
    FMargins: TSynEditPrintMargins;
    FFont: TFont;
    FTitle: string;
    FDocTitle: string;
    FPrinterInfo: TSynEditPrinterInfo;
    FPages: TObjectList<TPageLine>;
    FMaxLeftChar: Integer;
    FWrap: Boolean;
    FOnPrintLine: TPrintLineEvent;
    FOnPrintStatus: TPrintStatusEvent;
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
    FSynTextFormat: TSynTextFormat;
    procedure CalcPages;
    procedure SetLines(const Value: TStrings);
    procedure SetFont(const Value: TFont);
    procedure SetMaxLeftChar(const Value: Integer);
    procedure PrintPage(RT: ID2D1RenderTarget; Num: Integer; const ClipR: TRect);
    procedure WriteLineNumber(RT: ID2D1RenderTarget; const LineNumber, YPos:
        Integer; FontColor: TColor);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetPixelsPrInch;
    procedure InitRanges;
    function GetPageCount: Integer;
    procedure SetSynEdit(const Value: TCustomSynEdit);
    procedure SetFooter(const Value: TFooter);
    procedure SetHeader(const Value: THeader);
    procedure SetMargins(const Value: TSynEditPrintMargins);
    function GetTextLayout(const Line: Integer): TSynTextLayout;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
    procedure DoPrintStatus(Status: TSynPrintStatus; PageNumber: Integer;
      var Abort: Boolean); virtual;
    procedure DoPrintLine(LineNumber, PageNumber: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitPrint;
    procedure PrintToCanvas(ACanvas: TCanvas; const RenderRect, ClipRect: TRect;
      PageNo: Integer);
    procedure Print;
    procedure PrintRange(StartPage, EndPage: Integer);
    property PrinterInfo: TSynEditPrinterInfo read FPrinterInfo;
    property PageCount: Integer read GetPageCount;
    property SynEdit: TCustomSynEdit write SetSynEdit;

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
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
    property OnPrintLine: TPrintLineEvent read FOnPrintLine write FOnPrintLine;
    property OnPrintStatus: TPrintStatusEvent read FOnPrintStatus
      write FOnPrintStatus;
    property Highlighter: TSynCustomHighlighter read FHighlighter
      write SetHighlighter;
    property LineNumbersInMargin: Boolean read FLineNumbersInMargin
      write FLineNumbersInMargin default False;
    property TabWidth: Integer read fTabWidth write fTabWidth;
    property Color: TColor read fDefaultBG write fDefaultBG;
  end;

implementation

uses
  Winapi.MultiMon,
  System.Math,
  System.UITypes;

resourcestring
  SYNS_NoPrinter = 'No printer available';

{ TSynEditPrint }

constructor TSynEditPrint.Create(AOwner: TComponent);
begin
  inherited;
  FCopies := 1;
  FFooter := TFooter.Create;
  FHeader := THeader.Create;
  FLines := TStringList.Create;
  FMargins := TSynEditPrintMargins.Create;
  FPrinterInfo := TSynEditPrinterInfo.Create;
  FFont := TFont.Create;
  FFont.Name := DefaultFontName;
  FFont.PixelsPerInch := 96;
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
  FDefaultBG := clWhite;
end;

destructor TSynEditPrint.Destroy;
begin
  FFooter.Free;
  FHeader.Free;
  FLines.Free;
  FMargins.Free;
  FPrinterInfo.Free;
  FFont.Free;
  FPages.Free;
  inherited;
end;

procedure TSynEditPrint.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

procedure TSynEditPrint.SetLines(const Value: TStrings);
begin
  FLines.Clear;
  FLines.AddStrings(Value);
  FRangesOK := False;
  FPagesCounted := False;
end;

procedure TSynEditPrint.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FPagesCounted := False;
end;

procedure TSynEditPrint.SetMaxLeftChar(const Value: Integer);
begin
  FMaxLeftChar := Value;
end;

procedure TSynEditPrint.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  FHighlighter := Value;
  FRangesOK := False;
  FPagesCounted := False;
end;

procedure TSynEditPrint.InitPrint;
{ Initialize Font.PixelsPerInch, Character widths, Margins, Total Page count,
  headers and footers}
begin
  FPrinterInfo.UpdatePrinter;
  SetPixelsPrInch;
  FSynTextFormat := TSynTextFormat.Create(FFont, FTabWidth, 0, 0);
  FLineHeight := FSynTextFormat.LineHeight;
  FMargins.InitPage(FSynTextFormat, 1, FPrinterInfo, FLineNumbers,
    FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
  FSynOK := Highlight and Assigned(FHighLighter) and (FLines.Count > 0);
  CalcPages;
  FHeader.InitPrint(FPages.Count, FTitle, FMargins);
  FFooter.InitPrint(FPages.Count, FTitle, FMargins);
end;

procedure TSynEditPrint.SetPixelsPrInch;
// Scale fonts for 96 PPI
var
  TmpSize: Integer;
begin
  FHeader.SetPixPrInch(96);
  FFooter.SetPixPrInch(96);

  TmpSize := FFont.Size;
  FFont.PixelsPerInch := 96;
  FFont.Size := TmpSize;
end;

procedure TSynEditPrint.InitRanges;
//Initialize ranges in Highlighter
var
  i: Integer;
begin
  if not FRangesOK and Assigned(FHighlighter) and (Lines.Count > 0) then
  begin
    FHighlighter.ResetRange;
    FLines.Objects[0] := fHighlighter.GetRange;
    i := 1;
    while i < Lines.Count do
    begin
      FHighlighter.SetLine(FLines[i - 1], i - 1);
      FHighlighter.NextToEol;
      FLines.Objects[i] := FHighlighter.GetRange;
      Inc(i);
    end;
    FRangesOK := True;
  end;
end;

// Calculates the total number of pages
procedure TSynEditPrint.CalcPages;
var
  I: Integer;
  PageLine: TPageLine;
  TextLayout: TSynTextLayout;
  LineMetrics: TDwriteLineMetrics;
  ActualLineCount: Cardinal;
  LayoutRowCount: Integer;
  RowCount: Integer;
  iStartLine, iEndLine: Integer;
begin
  InitRanges;
  FPages.Clear;
  FMaxWidth := FMargins.PRight - FMargins.PLeft;
  if SelectedOnly then
  begin
    iStartLine := fBlockBegin.Line -1;
    iEndLine := fBlockEnd.Line -1;
  end
  else begin
    iStartLine := 0;
    iEndLine := Lines.Count -1;
  end;
  PageLine := TPageLine.Create;
  PageLine.FirstLine := iStartLine;
  PageLine.FirstRow := 1;
  PageLine.LastLine := -1;
  FPages.Add(PageLine);
  FMaxRowCount := (FMargins.PBottom - FMargins.PTop) div FLineHeight;
  Assert(FMaxRowCount > 1);
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
      TextLayout := GetTextLayout(i);
      TextLayout.IDW.GetLineMetrics(@LineMetrics, 1, ActualLineCount);
      LayoutRowCount := ActualLineCount; // to avoid warnings
      Inc(RowCount, LayoutRowCount);
    end;

    // Add new page(s) if needed (word wrap)
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

{ Writes the line number. FMargins. PLeft is the position of the left margin
  (which is automatically incremented by the length of the linenumber text, if
  the linenumbers should not be placed in the margin) }
procedure TSynEditPrint.WriteLineNumber(RT: ID2D1RenderTarget; const LineNumber, YPos: Integer;
  FontColor: TColor);
var
  AStr: string;
  Layout: TSynTextLayout;
begin
  AStr := IntToStr(LineNumber + FLineOffset) + ': ';
  Layout := TSynTextLayout.Create(FSynTextFormat, PChar(AStr), AStr.Length);
  Layout.Draw(RT, FMargins.PLeft -
    Round(Layout.TextMetrics.widthIncludingTrailingWhitespace), YPos, FontColor);
end;

procedure TSynEditPrint.PrintPage(RT: ID2D1RenderTarget; Num: Integer; const ClipR: TRect);
// Prints a page to a RenderTarget
// ** The RenderTarget assumes a PPI of 96 **
// The ClipR(ect) serves the purpose of reducing painting when previewing
var
  I: Integer;
  LineText: string;
  YPos: Integer;
  iSelStart, iSelLen: Integer;
  TextLayout: TSynTextLayout;
  LineMetrics: TDwriteLineMetrics;
  ActualLineCount: Cardinal;
  LayoutRowCount: Integer;
  Token: string;
  TokenPos, TokenEnd: Integer;
  Attr: TSynHighlighterAttributes;
  BkgColor, FontColor, AColor: TColor;
  HitMetrics: TDwriteHitTestMetrics;
  X1, X2, Y1, Y2: Single;
begin
  DoPrintStatus(psNewPage, Num, FAbort);
  if not FAbort then
  begin
    if FPages.Count >= Num then
    begin
      RT.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE);
      RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);

      if FColors and FSynOK then
        BkgColor := FHighlighter.WhitespaceAttribute.Background
      else
        BkgColor := Color;

      if IsColorDark(BkgColor) then
        FontColor := clWhite
      else
        FontColor := Font.Color;

      RT.Clear(D2D1ColorF(BkgColor));

      if FMargins.PTop >= ClipR.Top then
        FHeader.Print(RT, Num + FPageOffset);

      // TODO multicaret
      YPos := FMargins.PTop;
      for i := FPages[Num - 1].FirstLine to  FPages[Num - 1].LastLine do
      begin
        if FLineNumbers and (YPos + FLineHeight >= ClipR.Top) then
          WriteLineNumber(RT, i + 1, YPos, FontColor);

        LineText := FLines[I];

        if LineText = '' then
          LayoutRowCount := 1
        else
        begin
          if not fSelectedOnly then
          begin
            iSelStart := 1;
            iSelLen := LineText.Length;
          end
          else
          begin
            if i = fBlockBegin.Line -1 then
              iSelStart := fBlockBegin.Char
            else
              iSelStart := 1;
            if i = fBlockEnd.Line -1 then
              iSelLen := fBlockEnd.Char  - iSelStart
            else
              iSelLen := MaxInt;
          end;

          TextLayout := GetTextLayout(i);

          if FSynOK then
          begin
            FHighlighter.SetRange(FLines.Objects[i]);
            FHighlighter.SetLine(LineText, i + 1);

            while not FHighLighter.GetEol do
            begin
              Token := FHighLighter.GetToken;
              TokenPos := FHighLighter.GetTokenPos;
              if TokenPos - iSelStart >= iSelLen then Break;

              // Adjust for iSelStart
              TokenPos := TokenPos - iSelStart + 2; // TokenPos is zero based
              TokenEnd := TokenPos + Token.Length;

              Attr := FHighLighter.GetTokenAttribute;
              if (TokenEnd > 1) and Assigned(Attr) then
              begin
                TokenPos := Max(TokenPos, 1);
                TextLayout.SetFontStyle(Attr.Style, TokenPos, TokenEnd - TokenPos);
                if FColors then
                begin
                  AColor := Attr.Foreground;
                  if AColor <> clNone then
                    TextLayout.SetFontColor(AColor, TokenPos, TokenEnd - TokenPos);
                  AColor := Attr.Background;
                  if (AColor <> clNone) and (AColor <> BkgColor) then
                  begin
                    TextLayout.IDW.HitTestTextPosition(TokenPos - 1, False, X1, Y1, HitMetrics);
                    TextLayout.IDW.HitTestTextPosition(TokenEnd - 2, True, X2, Y2, HitMetrics);

                    // Word wrap complications - line continues from previous page
                    if (I = FPages[Num - 1].FirstLine) and (FPages[Num - 1].FirstRow > 1) then
                    begin
                      Y1 := Y1 - (FPages[Num - 1].FirstRow - 1) * FLineHeight;
                      Y2 := Y2 - (FPages[Num - 1].FirstRow - 1) * FLineHeight;
                    end;

                    if Y2 >= 0 then
                    begin
                      if YPos + Round(Y1) >=  FMargins.PTop + FMaxRowCount * FLineHeight
                      then
                        Break;

                      RT.FillRectangle(Rect(Round(X1) + FMargins.PLeft, YPos + Round(Y1),
                        Round(X2) + FMargins.PLeft, YPos + Round(Y2) + FLineHeight),
                        TSynDWrite.SolidBrush(AColor));
                    end;
                  end;
                end;
              end;
              FHighLighter.Next;
            end;
          end;

          TextLayout.IDW.GetLineMetrics(@LineMetrics, 1, ActualLineCount);
          LayoutRowCount := ActualLineCount; // to avoid warnings

          if YPos + LayoutRowCount * FLineHeight >= ClipR.Top then
          begin
            if (I = FPages[Num - 1].FirstLine) and (FPages[Num - 1].FirstRow > 1) then
            begin
              TextLayout.DrawClipped(RT, FMargins.PLeft,
                YPos - Pred(FPages[Num - 1].FirstRow) * FLineHeight,
                Rect(FMargins.PLeft, YPos, FMargins.PRight,
                FMargins.PTop + FMaxRowCount * FLineHeight), FFont.Color);
              LayoutRowCount := LayoutRowCount - FPages[Num - 1].FirstRow + 1;
            end else if (I = FPages[Num - 1].LastLine) and
              (FPages[Num - 1].LastRow < LayoutRowCount)
            then
              TextLayout.DrawClipped(RT, FMargins.PLeft, YPos,
                Rect(FMargins.PLeft, YPos, FMargins.PRight,
                YPos + FPages[Num - 1].LastRow * FLineHeight), FFont.Color)
            else
              TextLayout.Draw(RT, FMargins.PLeft, YPos, FFont.Color);
          end;
        end;
        DoPrintLine(I + 1, Num);

        Inc(YPos, LayoutRowCount * FLineHeight);
        // Optimization do not print anything below ClipR
        if YPos > ClipR.Bottom then Exit;
      end;

      FFooter.Print(RT, Num + FPageOffset);
    end;
  end;
end;

procedure TSynEditPrint.PrintToCanvas(ACanvas: TCanvas; const RenderRect,
    ClipRect: TRect; PageNo: Integer);
// Used by preview component
var
  RT:  ID2D1DCRenderTarget;
  ScaleX, ScaleY: Single;
  ClipR: TRect;
begin
  FAbort := False;
  FPrinting := False;

  with PrinterInfo do
  begin
    // The RenderTarget expects a PPI of 96
    ScaleX := RenderRect.Width / (PhysicalWidth * 96 / XPixPrInch) ;
    ScaleY := RenderRect.Height / (PhysicalHeight * 96 / YPixPrInch);
  end;

  ClipR := ClipRect;
  // Transform ClipR to the Coordinate system of RenderTarget
  ClipR.Offset(-RenderRect.Left, -RenderRect.Top);
  ClipR := TRect.Create(
    ScalePoint(ClipR.TopLeft, 1 / ScaleX, 1 / ScaleY),
    ScalePoint(ClipR.BottomRight, 1 / ScaleX, 1 / ScaleY));

  // Reset so that rendering for printing is not mixed up with Synedit rendering
  TSynDWrite.ResetRenderTarget;
  RT := TSynDWrite.RenderTarget;
  try
    RT.BindDC(ACanvas.Handle, RenderRect);
    RT.BeginDraw;
    try
      RT.SetTransform(
        TD2DMatrix3X2F.Scale(ScaleX, ScaleY, Point(0, 0)));

      PrintPage(RT, PageNo, ClipR);
    finally
      RT.EndDraw;
    end;
  finally
    // Reset so that it does not mess up the SynEdit drawing
    TSynDWrite.ResetRenderTarget;
  end;
end;

procedure TSynEditPrint.Print;
begin
  PrintRange(1, -1);
end;

procedure TSynEditPrint.PrintRange(StartPage, EndPage: Integer);
// Prints the pages in the specified range.  It uses as guide the sample app
// https://github.com/microsoft/Windows-classic-samples/tree/main/Samples/D2DPrintingFromDesktopApps
var
  Page, Copy: Integer;
  Title, PrinterName: string;
  PrintDocumentPackageTarget: IPrintDocumentPackageTarget;
  Device: ID2D1Device;
  DeviceContext: ID2D1DeviceContext;
  PrintControl: ID2D1PrintControl;
  CommandList: ID2D1CommandList;
  ClipR: TRect;
begin
  if fSelectedOnly and not fSelAvail then
    Exit;

  FPrinting := True;
  FAbort := False;
  // Set the print job title
  if FDocTitle <> '' then
    Title := FDocTitle
  else
    Title := FTitle;
  if Title = '' then
    Title := 'SynEdit document';

  DoPrintStatus(psBegin, StartPage, FAbort);
  InitPrint;

  if Printer.PrinterIndex < 0 then
    raise ESynError.CreateRes(Pointer(@SYNS_NoPrinter));

  PrinterName := Printer.Printers[Printer.PrinterIndex];

  // ClipR corresponds to the whole paper area -> no clipping
  ClipR := Rect(0, 0,
    MulDiv(FPrinterInfo.PhysicalWidth, 96, FPrinterInfo.XPixPrInch),
    MulDiv(FPrinterInfo.PhysicalHeight, 96,  FPrinterInfo.YPixPrInch));

  CheckOSError(TSynDWrite.PrintDocumentPackageTargetFactory.CreateDocumentPackageTargetForPrintJob(
    PChar(PrinterName),               // printer name
    PChar(Title),                     // job name
    nil,                              // job output stream; when nullptr, send to printer
    nil,                              // job print ticket
    PrintDocumentPackageTarget        // result IPrintDocumentPackageTarget object
    ));

  // Reset so that rendering for printing is not mixed up with Synedit rendering
  TSynDWrite.ResetRenderTarget;
  try
    CheckOSError(TSynDWrite.RenderTarget.BindDC(GetDC(0), TRect.Empty));
    DeviceContext := TSynDWrite.RenderTarget as ID2D1DeviceContext;

    DeviceContext.GetDevice(Device);
    CheckOSError(Device.CreatePrintControl(
      TSynDWrite.ImagingFactory,
      PrintDocumentPackageTarget,
      nil,
      PrintControl));

    for Copy := 1 to Copies do
    begin
      Page := StartPage;
      if EndPage < 0 then
        EndPage := FPages.Count;
      while (Page <= EndPage) and (not FAbort) do begin
        CheckOSError(DeviceContext.CreateCommandList(CommandList));
        DeviceContext.SetTarget(CommandList);
        DeviceContext.BeginDraw;
        try
          PrintPage(DeviceContext, Page, ClipR);
        finally
          DeviceContext.EndDraw;
        end;
        CheckOSError(CommandList.Close);

        // We have rendered at 96 PPI.  The D2D printing system scales
        // appropriately for the selected printer
        CheckOSError(PrintControl.AddPage(commandList,
          D2D1SizeF(ClipR.Width, ClipR.Height), nil));
        Page := Page + 1;
      end;
    end;
    if not FAbort then
      DoPrintStatus(psEnd, EndPage, FAbort);
    PrintControl.Close;
    FPrinting := False;
  finally
    // Reset so that it does not mess up the SynEdit drawing
    TSynDWrite.ResetRenderTarget;
  end;
end;

procedure TSynEditPrint.DoPrintLine(LineNumber, PageNumber: Integer);
//Fires the OnPrintLine event
begin
  if Assigned(FOnPrintLine) then
    FOnPrintLine(Self, LineNumber, PageNumber);
end;

procedure TSynEditPrint.DoPrintStatus(Status: TSynPrintStatus;
  PageNumber: Integer; var Abort: Boolean);
//Fires the OnPrintStatus event
begin
  Abort := False;
  if FPrinting and Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, Status, PageNumber, Abort);
  // if Abort then printing will stop at the earliest opportunity
end;

function TSynEditPrint.GetPageCount: Integer;
{Returns total page count. If pages hasn't been counted before,
 then InitPrint is called which calculates pages}
begin
  if FPagesCounted then
    Result := FPages.Count
  else begin
    InitPrint; // Calls CalcPages;
    Result := FPages.Count;
  end;
end;

function TSynEditPrint.GetTextLayout(const Line: Integer): TSynTextLayout;
// TODO multicaret
var
  iSelStart, iSelLen: Integer;
  S: string;
begin
  if not fSelectedOnly then
    S := Lines[Line]
  else
  begin
    if Line = fBlockBegin.Line -1 then
      iSelStart := fBlockBegin.Char
    else
      iSelStart := 1;
    if Line = fBlockEnd.Line -1 then
      iSelLen := fBlockEnd.Char  - iSelStart
    else
      iSelLen := MaxInt;
    S := Copy(Lines[Line], iSelStart, iSelLen);
  end;
  Result.Create(FSynTextFormat, PChar(S), S.Length, FMaxWidth, MaxInt, Wrap, 1);
end;

procedure TSynEditPrint.SetSynEdit(const Value: TCustomSynEdit);
begin
  HighLighter := Value.Highlighter;
  Font := Value.Font;
  FTabWidth := Value.TabWidth;
  Lines := Value.Lines;
  fSelAvail := Value.SelAvail;
  fBlockBegin := Value.BlockBegin;
  fBlockEnd := Value.BlockEnd;
end;

procedure TSynEditPrint.LoadFromStream(AStream: TStream);
var
  Len, BufferSize: Integer;
  Buffer: PWideChar;
begin
  FHeader.LoadFromStream(AStream);
  FFooter.LoadFromStream(AStream);
  FMargins.LoadFromStream(AStream);
  with AStream do
  begin
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div sizeof(WideChar)] := #0;
      FTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div sizeof(WideChar)] := #0;
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

procedure TSynEditPrint.SaveToStream(AStream: TStream);
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
    Write(PWideChar(FTitle)^, aLen * sizeof(WideChar));
    aLen := Length(FDocTitle);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(FDocTitle)^, aLen * sizeof(WideChar));
    Write(FWrap, SizeOf(FWrap));
    Write(FHighlight, SizeOf(FHighlight));
    Write(FColors, SizeOf(FColors));
    Write(FLineNumbers, SizeOf(FLineNumbers));
    Write(FLineOffset, SizeOf(FLineOffset));
    Write(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TSynEditPrint.SetFooter(const Value: TFooter);
begin
  FFooter.Assign(Value);
end;

procedure TSynEditPrint.SetHeader(const Value: THeader);
begin
  FHeader.Assign(Value);
end;

procedure TSynEditPrint.SetMargins(const Value: TSynEditPrintMargins);
begin
  FMargins.Assign(Value);
end;

end.

