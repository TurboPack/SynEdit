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
    LineNumbersInMargin : If true line numbers are printed in the left margin,
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
    UpdatePages   : Used internally by the TSynEditPrintPreview component
    PrintToCanvas : Used internally by the TSynEditPrintPreview component
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
    FCanvas: TCanvas;
    FMaxLeftChar: Integer;
    FWrap: Boolean;
    FOnPrintLine: TPrintLineEvent;
    FOnPrintStatus: TPrintStatusEvent;
    FLineHeight: Integer;
    FHighlight: Boolean;
    FColors: Boolean;
    FHighlighter: TSynCustomHighlighter;
    FOldFont: TFont;
    FSynOK: Boolean;
    FLineNumbers: Boolean;
    FLineOffset: Integer;
    FAbort: Boolean;
    FPrinting: Boolean;
    FDefaultBG: TColor;
    FPageOffset: Integer;
    FRangesOK: Boolean;
    FMaxWidth: integer;
    FPagesCounted: Boolean;
    FLineNumbersInMargin: Boolean;
    FTabWidth: integer;
    FFontColor: TColor;
    FSelectedOnly: Boolean;
    FSelAvail: Boolean;
    FSelMode: TSynSelectionMode;
    FBlockBegin: TBufferCoord;
    FBlockEnd: TBufferCoord;
    FSynTextFormat: TSynTextFormat;
    procedure CalcPages;
    procedure SetLines(const Value: TStrings);
    procedure SetFont(const Value: TFont);
    procedure SetMaxLeftChar(const Value: Integer);
    procedure PrintPage(Num: Integer);
    procedure WriteLineNumber(const LineNumber, YPos: Integer);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetPixelsPrInch;
    procedure InitPrint;
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
    procedure PrintStatus(Status: TSynPrintStatus; PageNumber: integer;
      var Abort: boolean); virtual;
    procedure PrintLine(LineNumber, PageNumber: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdatePages(ACanvas: TCanvas);
    procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
    procedure Print;
    procedure PrintRange(StartPage, EndPage: Integer);
    property PrinterInfo: TSynEditPrinterInfo read FPrinterInfo;
    property PageCount: Integer read GetPageCount;
    property SynEdit: TCustomSynEdit write SetSynEdit;

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property Copies: integer read FCopies write FCopies;
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
    property TabWidth: integer read fTabWidth write fTabWidth;
    property Color: TColor read fDefaultBG write fDefaultBG;
  end;

implementation

uses
  Winapi.MultiMon,
  System.Math,
  System.UITypes;

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
  FOldFont := TFont.Create;
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
  FOldFont.Free;
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
var
  TmpSize: Integer;
begin
  FDefaultBG := FCanvas.Brush.Color;
  fFontColor := FFont.Color;
  FCanvas.Font.Assign(FFont);
  if not FPrinting then
  begin
    SetPixelsPrInch;
    TmpSize := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch := FFont.PixelsPerInch;
    FCanvas.Font.Size := TmpSize;
  end;
  FSynTextFormat.Create(FCanvas.Font, FTabWidth, 0, 0);
  FLineHeight := FSynTextFormat.LineHeight;
  FMargins.InitPage(FCanvas, 1, FPrinterInfo, FLineNumbers, FLineNumbersInMargin,
    FLines.Count - 1 + FLineOffset);
  FSynOK := Highlight and Assigned(FHighLighter) and (FLines.Count > 0);
  CalcPages;
  FHeader.InitPrint(FCanvas, FPages.Count, FTitle, FMargins);
  FFooter.InitPrint(FCanvas, FPages.Count, FTitle, FMargins);
end;

procedure TSynEditPrint.SetPixelsPrInch;
var
  TmpSize: Integer;
begin
  FHeader.SetPixPrInch(FPrinterInfo.YPixPrInch);
  FFooter.SetPixPrInch(FPrinterInfo.YPixPrInch);
  //This should be necessary - else size would be changed...
  TmpSize := FFont.Size;
  FFont.PixelsPerInch := FPrinterInfo.YPixPrInch;
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
  MaxRowCount: Integer;
  iStartLine, iEndLine: integer;
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
  MaxRowCount := (FMargins.PBottom - FMargins.PTop) div FLineHeight;
  Assert(MaxRowCount > 1);
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

    while RowCount >= MaxRowCount do
    begin
      PageLine.LastLine := I;
      PageLine.LastRow := LayoutRowCount - (RowCount - MaxRowCount);
      if (RowCount = MaxRowCount) and (I = iEndLine) then Break;
      PageLine := TPageLine.Create;
      PageLine.FirstLine := IfThen(RowCount = MaxRowCount, I + 1, I);
      PageLine.FirstRow := IfThen(RowCount = MaxRowCount, 1,
         LayoutRowCount - (RowCount - MaxRowCount) + 1);
      FPages.Add(PageLine);
      RowCount := RowCount - MaxRowCount;
    end;

    if I = iEndLine then
      PageLine.LastLine := I;
  end;
  FPagesCounted := True;
end;

{ Writes the line number. FMargins. PLeft is the position of the left margin
  (which is automatically incremented by the length of the linenumber text, if
  the linenumbers should not be placed in the margin) }
procedure TSynEditPrint.WriteLineNumber(const LineNumber, YPos: Integer);
var
  AStr: string;
begin
  SaveCurrentFont;
  AStr := IntToStr(LineNumber + FLineOffset) + ': ';
  FCanvas.Brush.Color := FDefaultBG;
  FCanvas.Font.Assign(Font);
  FCanvas.Font.Style := [];
  FCanvas.Font.Color := clBlack;
  FCanvas.TextOut(FMargins.PLeft - FCanvas.TextWidth(AStr), YPos, AStr);
  RestoreCurrentFont;
end;

procedure TSynEditPrint.SaveCurrentFont;
begin
  FOldFont.Assign(FCanvas.Font);
end;

procedure TSynEditPrint.RestoreCurrentFont;
begin
  FCanvas.Font.Assign(FOldFont);
end;

procedure TSynEditPrint.PrintPage(Num: Integer);
//Prints a page
var
  I: Integer;
  LineText: string;
  YPos: Integer;
  iSelStart, iSelLen: integer;
  TextLayout: TSynTextLayout;
  LineMetrics: TDwriteLineMetrics;
  ActualLineCount: Cardinal;
  LayoutRowCount: Integer;
  Token: string;
  TokenPos, TokenEnd: Integer;
  Attr: TSynHighlighterAttributes;
  AColor: TColor;
  HitMetrics: TDwriteHitTestMetrics;
  X1, X2, Y1, Y2: Single;
  TextRect: TRect;
  DevTextRect: TRect;
  ClipRect: TRect;
  WicRT: ISynWICRenderTarget;
  RT: ID2D1RenderTarget;
  GDIRT: ID2D1GdiInteropRenderTarget;
  SourceDC: HDC;
begin
  PrintStatus(psNewPage, Num, FAbort);
  if not FAbort then
  begin
    FMargins.InitPage(FCanvas, Num, FPrinterInfo, FLineNumbers,
      FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
    FHeader.Print(FCanvas, Num + FPageOffset);

    if FPages.Count >= Num then
    begin
      with FMargins do
        TextRect := Rect(PLeft, PTop, PRight, PBottom);

      ClipRect := FCanvas.ClipRect;
      DevTextRect := TextRect;
      if not FPrinting then
        LPToDP(FCanvas.Handle, DevTextRect, 2);

      WicRT := SynWICRenderTarget(DevTextRect.Width, DevTextRect.Height);
      RT := WicRt.IDW;
      if not FPrinting then
        RT.SetTransform(
          TD2DMatrix3X2F.Scale(DevTextRect.Width / TextRect.Width,
            DevTextRect.Height /TextRect.Height, Point(0, 0)));

      RT.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE);
      RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      RT.BeginDraw;
      RT.Clear(D2D1ColorF(Color));

      YPos := 0;
      for i := FPages[Num - 1].FirstLine to  FPages[Num - 1].LastLine do
      begin
        if FLineNumbers then
          WriteLineNumber(i + 1, YPos + FMargins.PTop);

        LineText := FLines[I];

        if LineText = '' then
          Inc(YPos, FLineHeight)
        else
        begin
          if not fSelectedOnly or (fSelMode = smLine) then
          begin
            iSelStart := 1;
            iSelLen := LineText.Length;
          end
          else
          begin
            if (fSelMode = smColumn) or (i = fBlockBegin.Line -1) then
              iSelStart := fBlockBegin.Char
            else
              iSelStart := 1;
            if (fSelMode = smColumn) or (i = fBlockEnd.Line -1) then
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
                  if AColor <> clNone then
                  begin
                    TextLayout.IDW.HitTestTextPosition(TokenPos - 1, False, X1, Y1, HitMetrics);
                    TextLayout.IDW.HitTestTextPosition(TokenEnd - 2, True, X2, Y2, HitMetrics);
                    RT.FillRectangle(Rect(Round(X1), YPos + Round(Y1), Round(X2),
                      YPos + Round(Y2) + FLineHeight), TSynDWrite.SolidBrush(AColor));
                  end;
                end;
              end;
              FHighLighter.Next;
            end;
          end;

          TextLayout.IDW.GetLineMetrics(@LineMetrics, 1, ActualLineCount);
          LayoutRowCount := ActualLineCount; // to avoid warnings

          if YPos + FMargins.PTop + LayoutRowCount * FLineHeight >= ClipRect.Top then
            if (I = FPages[Num - 1].FirstLine) and (FPages[Num - 1].FirstRow > 1) then
            begin
              TextLayout.DrawClipped(RT, 0,
                YPos - Pred(FPages[Num - 1].FirstRow) * FLineHeight,
                Rect(0, YPos, FMaxWidth,
                ((FMargins.PBottom - FMargins.PTop) div FLineHeight) * FLineHeight),
                FFont.Color);
              LayoutRowCount := LayoutRowCount - FPages[Num - 1].FirstRow + 1;
            end else if (I = FPages[Num - 1].LastLine) and
              (FPages[Num - 1].LastRow < LayoutRowCount)
            then
              TextLayout.DrawClipped(RT, 0, YPos,
                Rect(0, YPos, FMaxWidth, YPos + FPages[Num - 1].LastRow * FLineHeight),
                FFont.Color)
            else
              TextLayout.Draw(RT, 0, YPos, FFont.Color);

          Inc(YPos, LayoutRowCount * FLineHeight);
          if YPos + FMargins.PTop > ClipRect.Bottom then Break;
        end;
        PrintLine(i + 1, Num);
      end;

      GDIRT := RT as ID2D1GdiInteropRenderTarget;
      CheckOSError(GDIRT.GetDC(D2D1_DC_INITIALIZE_MODE_COPY, SourceDC));
      StretchBlt(FCanvas.Handle, TextRect.Left, TextRect.Top, TextRect.Width, TextRect.Height,
        SourceDC, 0, 0, DevTextRect.Width, DevTextRect.Height, SRCCOPY);
      GDIRT.ReleaseDC(nil);

      RT.EndDraw;
    end;
    FFooter.Print(FCanvas, Num + FPageOffset);
  end;
end;

procedure TSynEditPrint.UpdatePages(ACanvas: TCanvas);
//Update pages (called explicitly by preview component)
begin
  FCanvas := ACanvas;
  FPrinterInfo.UpdatePrinter;
  InitPrint;
end;

procedure TSynEditPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
//Print to specified canvas. Used by preview component
begin
  FAbort := False;
  FPrinting := False;
  FCanvas := ACanvas;
  PrintPage(PageNumber);
end;

procedure TSynEditPrint.Print;
begin
  PrintRange(1, -1);
end;

procedure TSynEditPrint.PrintRange(StartPage, EndPage: Integer);
//Prints the pages in the specified range
var
  i, ii: Integer;
begin
  if fSelectedOnly and not fSelAvail then
    exit;

  FPrinting := True;
  FAbort := False;
  // The next part sets the document title that is used by the printer queue.
  if FDocTitle <> '' then
    Printer.Title := FDocTitle
  else
    Printer.Title := FTitle;
  Printer.BeginDoc;
  PrintStatus(psBegin, StartPage, FAbort);
  UpdatePages(Printer.Canvas);

  for ii:=1 to Copies do
  begin
    i := StartPage;
    if EndPage < 0 then
      EndPage := FPages.Count;
    while (i <= EndPage) and (not FAbort) do begin
      PrintPage(i);
      if ((i < EndPage) or (ii<Copies)) and not FAbort then
        Printer.NewPage;
      i := i + 1;
    end;
  end;
  if not FAbort then
    PrintStatus(psEnd, EndPage, FAbort);
  Printer.EndDoc;
  FPrinting := False;
end;

procedure TSynEditPrint.PrintLine(LineNumber, PageNumber: Integer);
//Fires the OnPrintLine event
begin
  if Assigned(FOnPrintLine) then
    FOnPrintLine(Self, LineNumber, PageNumber);
end;

procedure TSynEditPrint.PrintStatus(Status: TSynPrintStatus;
  PageNumber: integer; var Abort: boolean);
//Fires the OnPrintStatus event
begin
  Abort := False;
  if Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, Status, PageNumber, Abort);
  if Abort then begin
    if FPrinting then
      Printer.Abort;
  end;
end;

function TSynEditPrint.GetPageCount: Integer;
{Returns total page count. If pages hasn't been counted before,
 then a UpdatePages is called with a temporary canvas}
var
  TmpCanvas: TCanvas;
  DC: HDC;
begin
  Result := 0;
  if FPagesCounted then
    Result := FPages.Count
  else begin
    TmpCanvas := TCanvas.Create;
    try
      DC := GetDC(0);
      try
        if DC <> 0 then
        begin
          TmpCanvas.Handle := DC;
          UpdatePages(TmpCanvas);
          TmpCanvas.Handle := 0;
          Result := FPages.Count;
          FPagesCounted := True;
        end;
      finally
        ReleaseDC(0, DC);
      end;
    finally
      TmpCanvas.Free;
    end;
  end;
end;

function TSynEditPrint.GetTextLayout(const Line: Integer): TSynTextLayout;
var
  iSelStart, iSelLen: Integer;
  S: string;
begin
  if not fSelectedOnly or (fSelMode = smLine) then
    S := Lines[Line]
  else
  begin
    if (fSelMode = smColumn) or (Line = fBlockBegin.Line -1) then
      iSelStart := fBlockBegin.Char
    else
      iSelStart := 1;
    if (fSelMode = smColumn) or (Line = fBlockEnd.Line -1) then
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
  fSelMode := Value.SelectionMode;
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

