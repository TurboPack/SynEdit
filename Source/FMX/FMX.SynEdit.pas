{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Phase 2: Minimal FMX editor with syntax highlighting, keyboard input,
selection, scrolling, clipboard, and undo/redo.
-------------------------------------------------------------------------------}

unit FMX.SynEdit;

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
  FMX.Controls,
  FMX.Graphics,
  FMX.TextLayout,
  FMX.Platform,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditTextBuffer,
  SynEditMiscProcs,
  SynEditCodeFolding,
  FMX.SynEditMiscClasses;

type
  TCustomFMXSynEdit = class;
  TSynFMXEditPlugin = class;

  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column: Integer; var Action: TSynReplaceAction) of object;

  TScanForFoldRangesEvent = procedure(Sender: TObject;
    FoldRanges: TSynFoldRanges; LinesToScan: TStrings;
    FromLine: Integer; ToLine: Integer) of object;

  TCustomFMXSynEdit = class(TControl)
  private
    FLines: TSynEditStringList;
    FHighlighter: TSynCustomHighlighter;
    FFont: TFont;
    FTabWidth: Integer;
    FReadOnly: Boolean;
    FInsertMode: Boolean;
    FCaretX: Integer;
    FCaretY: Integer;
    FBlockBegin: TBufferCoord;
    FBlockEnd: TBufferCoord;
    FTopLine: Integer;
    FLeftChar: Integer;
    FRightEdge: Integer;
    FRightEdgeColor: TColor;
    FActiveLineColor: TColor;
    FOptions: TSynEditorOptions;
    FScrollOptions: TSynEditorScrollOptions;
    FSelectedColor: TSynSelectedColor;
    FOnChange: TNotifyEvent;
    FOnStatusChange: TNotifyEvent;
    // Internal state
    FRenderer: TObject; // TSynFMXRenderer (forward ref avoidance)
    FScrollBars: IInterface; // ISynEditScrollBars
    FUndoRedo: ISynEditUndo;
    FCharWidth: Single;
    FLineHeight: Single;
    FLinesInWindow: Integer;
    FCharsInWindow: Integer;
    FGutterWidth: Single;
    FTextAreaLeft: Single;
    FCaretTimer: TTimer;
    FCaretVisible: Boolean;
    FCaretBlinkOn: Boolean;
    FLastPosX: Integer;
    FUpdateCount: Integer;
    // Code folding
    FUseCodeFolding: Boolean;
    FCodeFolding: TSynCodeFolding;
    FAllFoldRanges: TSynFoldRanges;
    FOnScanForFoldRanges: TScanForFoldRangesEvent;
    // Search/Replace
    FSearchEngine: TSynEditSearchCustom;
    FOnReplaceText: TReplaceTextEvent;
    FOnSearchNotFound: TNotifyEvent;
    // Plugins
    FPlugins: TList;
    // Private methods
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetTabWidth(Value: Integer);
    procedure SetReadOnly(Value: Boolean);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure SetTopLine(Value: Integer);
    procedure SetLeftChar(Value: Integer);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetScrollOptions(Value: TSynEditorScrollOptions);
    procedure SetActiveLineColor(Value: TColor);
    function GetLineCount: Integer;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetMaxScrollWidth: Integer;
    function GetCaretXY: TBufferCoord;
    procedure SetCaretXY(const Value: TBufferCoord);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSelText: string;
    function GetSelAvail: Boolean;
    // Internal helpers
    procedure FontChanged(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure CaretTimerHandler(Sender: TObject);
    procedure RecalcCharExtent;
    procedure RecalcSizes;
    procedure UpdateGutterWidth;
    procedure UpdateScrollBars;
    procedure EnsureCursorPosVisible;
    procedure ShowCaret;
    procedure HideCaret;
    // Paint helpers
    procedure PaintGutter(Canvas: TCanvas; FirstLine, LastLine: Integer);
    procedure PaintTextLines(Canvas: TCanvas; FirstLine, LastLine: Integer);
    procedure PaintCaret(Canvas: TCanvas);
    // Editing helpers
    procedure InsertCharAtCursor(AChar: WideChar);
    procedure DoDeleteChar;
    procedure DoDeleteLastChar;
    procedure DoInsertLine;
    procedure DoDeleteSelection;
    // Navigation helpers
    procedure MoveCaretHorz(DX: Integer; SelectionCmd: Boolean);
    procedure MoveCaretVert(DY: Integer; SelectionCmd: Boolean);
    procedure MoveCaretAndSelection(const NewCaret: TBufferCoord;
      SelectionCmd: Boolean);
    // Range scanning for multi-line highlighters
    procedure ScanRanges;
    function ScanFrom(Index: Integer): Integer;
    // Code folding private
    procedure SetUseCodeFolding(const Value: Boolean);
    procedure OnCodeFoldingChange(Sender: TObject);
    procedure ReScanForFoldRanges(FromLine, ToLine: Integer);
    procedure FullFoldScan;
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine, ToLine: Integer);
    procedure PaintFoldGutter(Canvas: TCanvas; FirstRow, LastRow: Integer);
    // Search/Replace private
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column: Integer): TSynReplaceAction;
    // Row/Line mapping
    function LineToRow(aLine: Integer): Integer;
    function RowToLine(aRow: Integer): Integer;
    function GetDisplayRowCount: Integer;
    // Plugin hooks
    procedure DoPluginAfterPaint(Canvas: TCanvas; const AClip: TRectF;
      FirstLine, LastLine: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DialogKey(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure ClearAll;
    procedure Undo;
    procedure Redo;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure ClearSelection;
    function GetTextRange(AStart, AEnd: TBufferCoord): string;
    procedure SetCaretAndSelection(const ACaretXY, ABlockBegin,
      ABlockEnd: TBufferCoord);
    procedure BeginUpdate; reintroduce;
    procedure EndUpdate; reintroduce;
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar);
    procedure SetSelectedTextPrimitive(const Value: string);
    function PixelToBufferCoord(X, Y: Single): TBufferCoord;
    function BufferCoordToPixel(const BC: TBufferCoord): TPointF;
    // Code folding public methods
    procedure CollapseAll;
    procedure UncollapseAll;
    procedure CollapseNearest;
    procedure UncollapseNearest;
    procedure Collapse(FoldRangeIndex: Integer; Invalidate: Boolean = True);
    procedure Uncollapse(FoldRangeIndex: Integer; Invalidate: Boolean = True);
    procedure CollapseLevel(Level: Integer);
    procedure UncollapseLevel(Level: Integer);
    // Search/Replace
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): Integer;
    // Plugin management
    procedure RegisterPlugin(APlugin: TSynFMXEditPlugin);
    procedure UnregisterPlugin(APlugin: TSynFMXEditPlugin);
    property CodeFolding: TSynCodeFolding read FCodeFolding write FCodeFolding;
    property UseCodeFolding: Boolean read FUseCodeFolding write SetUseCodeFolding;
    property AllFoldRanges: TSynFoldRanges read FAllFoldRanges;
    property Lines: TSynEditStringList read FLines;
    property LineCount: Integer read GetLineCount;
    property CaretX: Integer read FCaretX write SetCaretX;
    property CaretY: Integer read FCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property BlockBegin: TBufferCoord read FBlockBegin;
    property BlockEnd: TBufferCoord read FBlockEnd;
    property TopLine: Integer read FTopLine write SetTopLine;
    property LeftChar: Integer read FLeftChar write SetLeftChar;
    property Modified: Boolean read GetCanUndo; // simplified: modified = can undo
    property InsertMode: Boolean read FInsertMode write FInsertMode;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property Text: string read GetText write SetText;
    property SelText: string read GetSelText;
    property SelAvail: Boolean read GetSelAvail;
    property LinesInWindow: Integer read FLinesInWindow;
    property CharsInWindow: Integer read FCharsInWindow;
    property MaxScrollWidth: Integer read GetMaxScrollWidth;
    property SelectedColor: TSynSelectedColor read FSelectedColor;
    property UndoRedo: ISynEditUndo read FUndoRedo;
    property ScrollOptions: TSynEditorScrollOptions read FScrollOptions
      write SetScrollOptions default SYNEDIT_DEFAULT_SCROLLOPTIONS;
  published
    property Font: TFont read FFont write FFont;
    property Highlighter: TSynCustomHighlighter read FHighlighter
      write SetHighlighter;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 8;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RightEdge: Integer read FRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor read FRightEdgeColor write SetRightEdgeColor
      default clSilver;
    property ActiveLineColor: TColor read FActiveLineColor
      write SetActiveLineColor default clNone;
    property Options: TSynEditorOptions read FOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property SearchEngine: TSynEditSearchCustom read FSearchEngine
      write SetSearchEngine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStatusChange: TNotifyEvent read FOnStatusChange
      write FOnStatusChange;
    property OnReplaceText: TReplaceTextEvent read FOnReplaceText
      write FOnReplaceText;
    property OnSearchNotFound: TNotifyEvent read FOnSearchNotFound
      write FOnSearchNotFound;
    property OnScanForFoldRanges: TScanForFoldRangesEvent
      read FOnScanForFoldRanges write FOnScanForFoldRanges;
  end;

  TFMXSynEdit = class(TCustomFMXSynEdit)
  published
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property Enabled;
    property Height;
    property HitTest default True;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop default True;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Inherited published properties }
    property Font;
    property Highlighter;
    property TabWidth;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ActiveLineColor;
    property Options;
    property CodeFolding;
    property UseCodeFolding;
    property SearchEngine;
    property OnChange;
    property OnStatusChange;
    property OnReplaceText;
    property OnSearchNotFound;
    property OnScanForFoldRanges;
  end;

implementation

uses
  FMX.SynEditRenderer,
  FMX.SynEditScrollBars,
  FMX.SynEditTypes,
  FMX.SynEditUndo,
  FMX.SynUnicode,
  FMX.SynEditPlugins,
  SynEditKeyConst,
  SynEditStrConst;

{ Expand tabs in a string to spaces }
function ExpandTabs(const S: string; TabWidth: Integer): string;
var
  I, Col: Integer;
  SB: TStringBuilder;
begin
  if Pos(#9, S) = 0 then Exit(S);
  SB := TStringBuilder.Create(Length(S) + 16);
  try
    Col := 0;
    for I := 1 to Length(S) do
    begin
      if S[I] = #9 then
      begin
        repeat
          SB.Append(' ');
          Inc(Col);
        until (Col mod TabWidth) = 0;
      end
      else
      begin
        SB.Append(S[I]);
        Inc(Col);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TCustomFMXSynEdit }

constructor TCustomFMXSynEdit.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TSynEditStringList.Create(nil);
  FLines.OnChange := LinesChanged;
  FFont := TFont.Create;
  FFont.Family := 'Consolas';
  FFont.Size := 10;
  FFont.OnChanged := FontChanged;
  FTabWidth := 8;
  FInsertMode := True;
  FCaretX := 1;
  FCaretY := 1;
  FBlockBegin := BufferCoord(1, 1);
  FBlockEnd := BufferCoord(1, 1);
  FTopLine := 1;
  FLeftChar := 1;
  FRightEdge := 80;
  FRightEdgeColor := clSilver;
  FActiveLineColor := clNone;
  FOptions := SYNEDIT_DEFAULT_OPTIONS;
  FScrollOptions := SYNEDIT_DEFAULT_SCROLLOPTIONS;
  FSelectedColor := TSynSelectedColor.Create;
  FLastPosX := -1;

  CanFocus := True;
  TabStop := True;
  HitTest := True;
  SetAcceptsControls(False);

  // Renderer
  FRenderer := TSynFMXRenderer.Create;
  TSynFMXRenderer(FRenderer).SetFont(FFont);
  FCharWidth := TSynFMXRenderer(FRenderer).CharWidth;
  FLineHeight := TSynFMXRenderer(FRenderer).LineHeight;

  // Undo system (hooks Lines events)
  FUndoRedo := CreateSynEditUndo(Self);
  if eoGroupUndo in FOptions then
    FUndoRedo.GroupUndo := True;

  // Scrollbars
  FScrollBars := CreateSynEditScrollBars(Self);

  // Caret timer
  FCaretTimer := TTimer.Create(Self);
  FCaretTimer.Interval := 500;
  FCaretTimer.Enabled := False;
  FCaretTimer.OnTimer := CaretTimerHandler;

  // Code folding
  FCodeFolding := TSynCodeFolding.Create;
  FCodeFolding.OnChange := OnCodeFoldingChange;
  FAllFoldRanges := TSynFoldRanges.Create;

  // Plugins
  FPlugins := TList.Create;

  UpdateGutterWidth;
end;

destructor TCustomFMXSynEdit.Destroy;
begin
  FPlugins.Free;
  FCodeFolding.Free;
  FAllFoldRanges.Free;
  FCaretTimer.Free;
  FUndoRedo := nil;
  FScrollBars := nil;
  FSelectedColor.Free;
  TSynFMXRenderer(FRenderer).Free;
  FHighlighter := nil;
  FFont.Free;
  FLines.Free;
  inherited;
end;

{ --- Font and sizing --- }

procedure TCustomFMXSynEdit.FontChanged(Sender: TObject);
begin
  RecalcCharExtent;
end;

procedure TCustomFMXSynEdit.RecalcCharExtent;
begin
  TSynFMXRenderer(FRenderer).SetFont(FFont);
  FCharWidth := TSynFMXRenderer(FRenderer).CharWidth;
  FLineHeight := TSynFMXRenderer(FRenderer).LineHeight;
  RecalcSizes;
  Repaint;
end;

procedure TCustomFMXSynEdit.RecalcSizes;
begin
  if (Width <= 0) or (Height <= 0) then Exit;
  UpdateGutterWidth;
  FTextAreaLeft := FGutterWidth;
  if FLineHeight > 0 then
    FLinesInWindow := Max(1, Trunc((Height - 16) / FLineHeight)) // -16 for hscrollbar
  else
    FLinesInWindow := 1;
  if FCharWidth > 0 then
    FCharsInWindow := Max(1, Trunc((Width - FGutterWidth - 16) / FCharWidth)) // -16 for vscrollbar
  else
    FCharsInWindow := 1;
  UpdateScrollBars;
end;

procedure TCustomFMXSynEdit.UpdateGutterWidth;
var
  DigitCount: Integer;
  LineCount: Integer;
  FoldWidth: Single;
begin
  LineCount := Max(FLines.Count, 1);
  DigitCount := Max(2, Length(IntToStr(LineCount)));
  FGutterWidth := Round((DigitCount + 1) * FCharWidth) + 4;
  if FUseCodeFolding then
  begin
    FoldWidth := FCodeFolding.GutterShapeSize + 8;
    FGutterWidth := FGutterWidth + FoldWidth;
  end;
  FTextAreaLeft := FGutterWidth;
end;

procedure TCustomFMXSynEdit.Resize;
begin
  inherited;
  RecalcSizes;
end;

{ --- Scrolling --- }

procedure TCustomFMXSynEdit.UpdateScrollBars;
begin
  if FScrollBars <> nil then
    (FScrollBars as ISynEditScrollBars).UpdateScrollBars;
end;

procedure TCustomFMXSynEdit.SetTopLine(Value: Integer);
var
  MaxTop: Integer;
begin
  if Value < 1 then Value := 1;
  MaxTop := Max(1, GetDisplayRowCount - FLinesInWindow + 1);
  if not (eoScrollPastEof in FScrollOptions) then
    Value := Min(Value, MaxTop);
  if FTopLine <> Value then
  begin
    FTopLine := Value;
    UpdateScrollBars;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetLeftChar(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FLeftChar <> Value then
  begin
    FLeftChar := Value;
    UpdateScrollBars;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.EnsureCursorPosVisible;
var
  CaretRow: Integer;
begin
  // Vertical - use display rows when code folding is active
  CaretRow := LineToRow(FCaretY);
  if CaretRow < FTopLine then
    TopLine := CaretRow
  else if CaretRow >= FTopLine + FLinesInWindow then
    TopLine := CaretRow - FLinesInWindow + 1;
  // Horizontal
  if FCaretX < FLeftChar then
    LeftChar := FCaretX
  else if FCaretX >= FLeftChar + FCharsInWindow then
    LeftChar := FCaretX - FCharsInWindow + 1;
end;

function TCustomFMXSynEdit.GetMaxScrollWidth: Integer;
var
  I, Len: Integer;
begin
  Result := 1;
  for I := 0 to FLines.Count - 1 do
  begin
    Len := Length(FLines[I]);
    if Len > Result then
      Result := Len;
  end;
  Inc(Result); // +1 for caret past end
  Result := Max(Result, FCharsInWindow + 1);
end;

{ --- Caret --- }

procedure TCustomFMXSynEdit.ShowCaret;
begin
  FCaretVisible := True;
  FCaretBlinkOn := True;
  FCaretTimer.Enabled := True;
  Repaint;
end;

procedure TCustomFMXSynEdit.HideCaret;
begin
  FCaretVisible := False;
  FCaretTimer.Enabled := False;
  Repaint;
end;

procedure TCustomFMXSynEdit.CaretTimerHandler(Sender: TObject);
begin
  FCaretBlinkOn := not FCaretBlinkOn;
  Repaint;
end;

procedure TCustomFMXSynEdit.DoEnter;
begin
  inherited;
  ShowCaret;
end;

procedure TCustomFMXSynEdit.DoExit;
begin
  inherited;
  HideCaret;
end;

{ --- Coordinate conversion --- }

function TCustomFMXSynEdit.BufferCoordToPixel(const BC: TBufferCoord): TPointF;
begin
  Result.X := FTextAreaLeft + (BC.Char - FLeftChar) * FCharWidth;
  Result.Y := (LineToRow(BC.Line) - FTopLine) * FLineHeight;
end;

function TCustomFMXSynEdit.PixelToBufferCoord(X, Y: Single): TBufferCoord;
var
  Row: Integer;
begin
  Result.Char := Max(1, FLeftChar + Round((X - FTextAreaLeft) / FCharWidth));
  Row := Max(1, FTopLine + Trunc(Y / FLineHeight));
  Result.Line := Max(1, Min(RowToLine(Row), FLines.Count));
end;

{ --- Paint --- }

procedure TCustomFMXSynEdit.Paint;
var
  FirstLine, LastLine: Integer;
  Renderer: TSynFMXRenderer;
  BGColor: TAlphaColor;
  R: TRectF;
begin
  Renderer := TSynFMXRenderer(FRenderer);

  // Background
  BGColor := TAlphaColors.White;
  if (FHighlighter <> nil) and (FHighlighter.WhitespaceAttribute <> nil) and
    (FHighlighter.WhitespaceAttribute.Background <> clNone)
  then
    BGColor := TColorToAlphaColor(FHighlighter.WhitespaceAttribute.Background);

  R := LocalRect;
  Renderer.FillRect(Canvas, R, BGColor);

  if FLineHeight <= 0 then Exit;

  FirstLine := FTopLine;
  LastLine := Min(FTopLine + FLinesInWindow, GetDisplayRowCount);

  PaintGutter(Canvas, FirstLine, LastLine);
  PaintTextLines(Canvas, FirstLine, LastLine);

  // Right edge
  if FRightEdge > 0 then
  begin
    var EdgeX: Single := FTextAreaLeft +
      (FRightEdge - FLeftChar + 1) * FCharWidth;
    if (EdgeX >= FTextAreaLeft) and (EdgeX <= Width) then
      Renderer.DrawLine(Canvas, EdgeX, 0, EdgeX, Height,
        TColorToAlphaColor(FRightEdgeColor));
  end;

  // Plugin AfterPaint hooks
  DoPluginAfterPaint(Canvas, LocalRect, RowToLine(FirstLine), RowToLine(LastLine));

  if FCaretVisible and FCaretBlinkOn then
    PaintCaret(Canvas);
end;

procedure TCustomFMXSynEdit.PaintGutter(Canvas: TCanvas;
  FirstLine, LastLine: Integer);
var
  Renderer: TSynFMXRenderer;
  Row: Integer;
  Line: Integer;
  Y: Single;
  R: TRectF;
  NumStr: string;
  NumberWidth: Single;
begin
  Renderer := TSynFMXRenderer(FRenderer);

  // Gutter background
  R := RectF(0, 0, FGutterWidth, Height);
  Renderer.FillRect(Canvas, R, TAlphaColors.Whitesmoke);

  // Gutter border
  Renderer.DrawLine(Canvas, FGutterWidth - 1, 0, FGutterWidth - 1, Height,
    TAlphaColors.Lightgray);

  // Calculate number area width (excluding fold gutter)
  if FUseCodeFolding then
    NumberWidth := FGutterWidth - FCodeFolding.GutterShapeSize - 8
  else
    NumberWidth := FGutterWidth;

  // Line numbers (iterate display rows)
  for Row := FirstLine to LastLine do
  begin
    Line := RowToLine(Row);
    if Line > FLines.Count then Break;
    Y := (Row - FTopLine) * FLineHeight;
    NumStr := IntToStr(Line);
    R := RectF(2, Y, NumberWidth - 4, Y + FLineHeight);
    Renderer.PaintLineNumber(Canvas, R, NumStr, TAlphaColors.Gray);
  end;

  // Fold gutter shapes
  if FUseCodeFolding then
    PaintFoldGutter(Canvas, FirstLine, LastLine);
end;

procedure TCustomFMXSynEdit.PaintTextLines(Canvas: TCanvas;
  FirstLine, LastLine: Integer);
var
  Renderer: TSynFMXRenderer;
  Row, Line: Integer;
  Y, X: Single;
  SLine, SExpanded: string;
  TokenPos: Integer;
  Attr: TSynHighlighterAttributes;
  ForeColor, BackColor: TAlphaColor;
  Style: TFontStyles;
  SelStart, SelEnd: Integer;
  LineR: TRectF;
  Token: string;
  SelBC1, SelBC2: TBufferCoord;
begin
  Renderer := TSynFMXRenderer(FRenderer);

  // Normalize selection
  SelBC1 := FBlockBegin;
  SelBC2 := FBlockEnd;
  if SelBC1 > SelBC2 then
  begin
    var Tmp := SelBC1;
    SelBC1 := SelBC2;
    SelBC2 := Tmp;
  end;

  for Row := FirstLine to LastLine do
  begin
    Line := RowToLine(Row);
    Y := (Row - FTopLine) * FLineHeight;

    // Active line highlight
    if (FActiveLineColor <> clNone) and (Line = FCaretY) and
      (SelBC1 = SelBC2) then
    begin
      LineR := RectF(FTextAreaLeft, Y, Width, Y + FLineHeight);
      Renderer.FillRect(Canvas, LineR,
        TColorToAlphaColor(FActiveLineColor));
    end;

    if Line > FLines.Count then Continue;
    SLine := FLines[Line - 1];
    SExpanded := ExpandTabs(SLine, FTabWidth);

    // Calculate selection range for this line
    SelStart := 0;
    SelEnd := 0;
    if (SelBC1 <> SelBC2) then
    begin
      if (Line > SelBC1.Line) and (Line < SelBC2.Line) then
      begin
        // Entire line selected
        SelStart := 1;
        SelEnd := Length(SExpanded) + 1;
      end
      else if (Line = SelBC1.Line) and (Line = SelBC2.Line) then
      begin
        SelStart := SelBC1.Char;
        SelEnd := SelBC2.Char;
      end
      else if Line = SelBC1.Line then
      begin
        SelStart := SelBC1.Char;
        SelEnd := Length(SExpanded) + 1;
      end
      else if Line = SelBC2.Line then
      begin
        SelStart := 1;
        SelEnd := SelBC2.Char;
      end;
    end;

    // Paint selection background
    if SelStart <> SelEnd then
    begin
      var SelX1: Single := FTextAreaLeft +
        (Max(SelStart, FLeftChar) - FLeftChar) * FCharWidth;
      var SelX2: Single := FTextAreaLeft +
        (Min(SelEnd, FLeftChar + FCharsInWindow) - FLeftChar) * FCharWidth;
      if SelX2 > SelX1 then
      begin
        LineR := RectF(SelX1, Y, SelX2, Y + FLineHeight);
        Renderer.FillRect(Canvas, LineR,
          TColorToAlphaColor(FSelectedColor.Background));
      end;
    end;

    // Paint tokens with highlighter
    if (FHighlighter <> nil) and (SLine <> '') then
    begin
      if Line > 1 then
        FHighlighter.SetRange(TSynEditStringList(FLines).Ranges[Line - 2])
      else
        FHighlighter.ResetRange;
      FHighlighter.SetLine(SLine, Line);

      while not FHighlighter.GetEol do
      begin
        TokenPos := FHighlighter.GetTokenPos; // 0-based
        Token := FHighlighter.GetToken;
        Attr := FHighlighter.GetTokenAttribute;

        // Expand tabs in token
        if Pos(#9, Token) > 0 then
          Token := ExpandTabs(Token, FTabWidth);

        // Skip tokens entirely before visible area
        if TokenPos + Length(Token) < FLeftChar - 1 then
        begin
          FHighlighter.Next;
          Continue;
        end;
        // Stop if past visible area
        if TokenPos >= FLeftChar + FCharsInWindow - 1 then
          Break;

        // Determine colors
        if Assigned(Attr) then
        begin
          ForeColor := TColorToAlphaColor(Attr.Foreground);
          if ForeColor = TAlphaColors.Null then
            ForeColor := TAlphaColors.Black;
          BackColor := TColorToAlphaColor(Attr.Background);
          Style := Attr.Style;
        end
        else
        begin
          ForeColor := TAlphaColors.Black;
          BackColor := TAlphaColors.Null;
          Style := [];
        end;

        // Paint the token, splitting at selection boundaries
        X := FTextAreaLeft + (TokenPos + 1 - FLeftChar) * FCharWidth;
        // Clip to visible area
        if X < FTextAreaLeft then
        begin
          var Skip := Trunc((FTextAreaLeft - X) / FCharWidth);
          Token := Copy(Token, Skip + 1, MaxInt);
          TokenPos := TokenPos + Skip;
          X := FTextAreaLeft + (TokenPos + 1 - FLeftChar) * FCharWidth;
        end;

        if (Token <> '') and (X < Width) then
        begin
          var TokStart := TokenPos + 1; // 1-based
          var TokLen := Length(Token);

          if (SelStart > 0) and (SelEnd > SelStart) and
            (TokStart < SelEnd) and (TokStart + TokLen > SelStart) then
          begin
            // Token overlaps selection - render in up to 3 parts
            var SelFore := TColorToAlphaColor(FSelectedColor.Foreground);
            if SelFore = TAlphaColors.Null then
              SelFore := TAlphaColors.White;

            // Part before selection
            if TokStart < SelStart then
            begin
              var PreLen := SelStart - TokStart;
              Renderer.PaintToken(Canvas, X, Y,
                Copy(Token, 1, PreLen), ForeColor, BackColor, Style);
              X := X + PreLen * FCharWidth;
              Token := Copy(Token, PreLen + 1, MaxInt);
              TokStart := SelStart;
              TokLen := Length(Token);
            end;

            // Selected part
            var SelPartLen := Min(SelEnd, TokStart + TokLen) - TokStart;
            if SelPartLen > 0 then
            begin
              Renderer.PaintToken(Canvas, X, Y,
                Copy(Token, 1, SelPartLen), SelFore,
                TAlphaColors.Null, Style);
              X := X + SelPartLen * FCharWidth;
              Token := Copy(Token, SelPartLen + 1, MaxInt);
            end;

            // Part after selection
            if Token <> '' then
              Renderer.PaintToken(Canvas, X, Y,
                Token, ForeColor, BackColor, Style);
          end
          else
            // Token entirely outside selection
            Renderer.PaintToken(Canvas, X, Y, Token, ForeColor,
              BackColor, Style);
        end;

        FHighlighter.Next;
      end;
    end
    else if SLine <> '' then
    begin
      // No highlighter - paint plain text, splitting at selection
      SExpanded := ExpandTabs(SLine, FTabWidth);
      X := FTextAreaLeft;
      var VisText := Copy(SExpanded, FLeftChar, FCharsInWindow + 1);
      var VisStart := FLeftChar; // 1-based position of first visible char
      var VisLen := Length(VisText);

      if (SelStart > 0) and (SelEnd > SelStart) and
        (VisStart < SelEnd) and (VisStart + VisLen > SelStart) then
      begin
        var SelFore := TColorToAlphaColor(FSelectedColor.Foreground);
        if SelFore = TAlphaColors.Null then
          SelFore := TAlphaColors.White;

        // Part before selection
        if VisStart < SelStart then
        begin
          var PreLen := SelStart - VisStart;
          Renderer.PaintToken(Canvas, X, Y,
            Copy(VisText, 1, PreLen), TAlphaColors.Black,
            TAlphaColors.Null, []);
          X := X + PreLen * FCharWidth;
          VisText := Copy(VisText, PreLen + 1, MaxInt);
          VisStart := SelStart;
          VisLen := Length(VisText);
        end;

        // Selected part
        var SelPartLen := Min(SelEnd, VisStart + VisLen) - VisStart;
        if SelPartLen > 0 then
        begin
          Renderer.PaintToken(Canvas, X, Y,
            Copy(VisText, 1, SelPartLen), SelFore,
            TAlphaColors.Null, []);
          X := X + SelPartLen * FCharWidth;
          VisText := Copy(VisText, SelPartLen + 1, MaxInt);
        end;

        // Part after selection
        if VisText <> '' then
          Renderer.PaintToken(Canvas, X, Y,
            VisText, TAlphaColors.Black, TAlphaColors.Null, []);
      end
      else
        Renderer.PaintToken(Canvas, X, Y, VisText, TAlphaColors.Black,
          TAlphaColors.Null, []);
    end;
  end;
end;

procedure TCustomFMXSynEdit.PaintCaret(Canvas: TCanvas);
var
  Renderer: TSynFMXRenderer;
  Pt: TPointF;
  R: TRectF;
begin
  Renderer := TSynFMXRenderer(FRenderer);
  Pt := BufferCoordToPixel(BufferCoord(FCaretX, FCaretY));
  if (Pt.X >= FTextAreaLeft) and (Pt.X < Width) and
    (Pt.Y >= 0) and (Pt.Y < Height) then
  begin
    if FInsertMode then
    begin
      // Vertical line caret
      R := RectF(Pt.X, Pt.Y, Pt.X + 2, Pt.Y + FLineHeight);
    end
    else
    begin
      // Block caret
      R := RectF(Pt.X, Pt.Y, Pt.X + FCharWidth, Pt.Y + FLineHeight);
    end;
    Renderer.FillRect(Canvas, R, TAlphaColors.Black);
  end;
end;

{ --- Lines change notification --- }

procedure TCustomFMXSynEdit.LinesChanged(Sender: TObject);
begin
  if FUseCodeFolding and FAllFoldRanges.StopScanning(FLines) then
    UpdateGutterWidth;
  UpdateGutterWidth;
  UpdateScrollBars;
  if FUpdateCount = 0 then
    Repaint;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomFMXSynEdit.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomFMXSynEdit.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    RecalcSizes;
    Repaint;
  end;
end;

{ --- Key handling --- }

function IfThen(Cond: Boolean; TrueVal, FalseVal: TSynEditorCommand): TSynEditorCommand; inline;
begin
  if Cond then Result := TrueVal else Result := FalseVal;
end;

procedure TCustomFMXSynEdit.DialogKey(var Key: Word; Shift: TShiftState);
begin
  // Intercept Tab/Shift+Tab: execute as editor command instead of focus navigation
  if Key = vkTab then
  begin
    if IsFocused then
    begin
      if ssShift in Shift then
        ExecuteCommand(ecShiftTab, #0)
      else
        ExecuteCommand(ecTab, #0);
    end;
    Key := 0;
    Exit;
  end;
  inherited;
end;

procedure TCustomFMXSynEdit.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var
  Cmd: TSynEditorCommand;
begin
  inherited;

  // Map key to command
  Cmd := ecNone;

  // Character input
  if (KeyChar >= #32) and (Shift * [ssCtrl, ssAlt] = []) then
  begin
    ExecuteCommand(ecChar, KeyChar);
    KeyChar := #0;
    Exit;
  end;

  // Navigation and editing keys
  case Key of
    vkLeft:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelWordLeft, ecWordLeft)
      else
        Cmd := IfThen(ssShift in Shift, ecSelLeft, ecLeft);
    vkRight:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelWordRight, ecWordRight)
      else
        Cmd := IfThen(ssShift in Shift, ecSelRight, ecRight);
    vkUp:
      Cmd := IfThen(ssShift in Shift, ecSelUp, ecUp);
    vkDown:
      Cmd := IfThen(ssShift in Shift, ecSelDown, ecDown);
    vkHome:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelEditorTop, ecEditorTop)
      else
        Cmd := IfThen(ssShift in Shift, ecSelLineStart, ecLineStart);
    vkEnd:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelEditorBottom, ecEditorBottom)
      else
        Cmd := IfThen(ssShift in Shift, ecSelLineEnd, ecLineEnd);
    vkPrior:
      Cmd := IfThen(ssShift in Shift, ecSelPageUp, ecPageUp);
    vkNext:
      Cmd := IfThen(ssShift in Shift, ecSelPageDown, ecPageDown);
    vkBack:
      Cmd := ecDeleteLastChar;
    vkDelete:
      Cmd := ecDeleteChar;
    vkReturn:
      Cmd := ecLineBreak;
    vkTab:
      Cmd := ecTab;
    vkInsert:
      Cmd := ecToggleMode;
    Ord('A'):
      if ssCtrl in Shift then Cmd := ecSelectAll;
    Ord('C'):
      if ssCtrl in Shift then Cmd := ecCopy;
    Ord('V'):
      if ssCtrl in Shift then Cmd := ecPaste;
    Ord('X'):
      if ssCtrl in Shift then Cmd := ecCut;
    Ord('Z'):
      if ssCtrl in Shift then
      begin
        if ssShift in Shift then
          Cmd := ecRedo
        else
          Cmd := ecUndo;
      end;
    Ord('Y'):
      if ssCtrl in Shift then Cmd := ecRedo;
  end;

  if Cmd <> ecNone then
  begin
    ExecuteCommand(Cmd, #0);
    Key := 0;
    KeyChar := #0;
  end;
end;

function IsWordBreakChar(C: WideChar): Boolean; inline;
begin
  case C of
    'A'..'Z', 'a'..'z', '0'..'9', '_': Result := False;
  else
    Result := True;
  end;
end;

{ --- Range scanning --- }

procedure TCustomFMXSynEdit.ScanRanges;
var
  I: Integer;
begin
  if Assigned(FHighlighter) and (FLines.Count > 0) then
  begin
    FHighlighter.ResetRange;
    I := 0;
    repeat
      FHighlighter.SetLine(FLines[I], I);
      FHighlighter.NextToEol;
      TSynEditStringList(FLines).Ranges[I] := FHighlighter.GetRange;
      Inc(I);
    until I >= FLines.Count;
  end;
end;

function TCustomFMXSynEdit.ScanFrom(Index: Integer): Integer;
var
  iRange: TSynEditRange;
begin
  Result := Index;
  if Result >= FLines.Count then Exit;

  if Result = 0 then
    FHighlighter.ResetRange
  else
    FHighlighter.SetRange(TSynEditStringList(FLines).Ranges[Result - 1]);

  repeat
    FHighlighter.SetLine(FLines[Result], Result);
    FHighlighter.NextToEol;
    iRange := FHighlighter.GetRange;
    if TSynEditStringList(FLines).Ranges[Result] = iRange then
      Exit;
    TSynEditStringList(FLines).Ranges[Result] := iRange;
    Inc(Result);
  until Result = FLines.Count;
  Dec(Result);
end;

{ --- Command execution --- }

procedure TCustomFMXSynEdit.ExecuteCommand(Command: TSynEditorCommand;
  AChar: WideChar);
var
  FirstAffectedLine: Integer;
begin
  FirstAffectedLine := -1;

  if FUndoRedo <> nil then
    FUndoRedo.CommandProcessed := Command;

  case Command of
    // Navigation
    ecLeft:       MoveCaretHorz(-1, False);
    ecSelLeft:    MoveCaretHorz(-1, True);
    ecRight:      MoveCaretHorz(1, False);
    ecSelRight:   MoveCaretHorz(1, True);
    ecUp:         MoveCaretVert(-1, False);
    ecSelUp:      MoveCaretVert(-1, True);
    ecDown:       MoveCaretVert(1, False);
    ecSelDown:    MoveCaretVert(1, True);
    ecPageUp:
      begin
        TopLine := TopLine - FLinesInWindow;
        MoveCaretVert(-FLinesInWindow, False);
      end;
    ecSelPageUp:
      begin
        TopLine := TopLine - FLinesInWindow;
        MoveCaretVert(-FLinesInWindow, True);
      end;
    ecPageDown:
      begin
        TopLine := TopLine + FLinesInWindow;
        MoveCaretVert(FLinesInWindow, False);
      end;
    ecSelPageDown:
      begin
        TopLine := TopLine + FLinesInWindow;
        MoveCaretVert(FLinesInWindow, True);
      end;
    ecLineStart:
      MoveCaretAndSelection(BufferCoord(1, FCaretY), False);
    ecSelLineStart:
      MoveCaretAndSelection(BufferCoord(1, FCaretY), True);
    ecLineEnd:
      begin
        var LineLen := 0;
        if (FCaretY >= 1) and (FCaretY <= FLines.Count) then
          LineLen := Length(FLines[FCaretY - 1]);
        MoveCaretAndSelection(BufferCoord(LineLen + 1, FCaretY), False);
      end;
    ecSelLineEnd:
      begin
        var LineLen := 0;
        if (FCaretY >= 1) and (FCaretY <= FLines.Count) then
          LineLen := Length(FLines[FCaretY - 1]);
        MoveCaretAndSelection(BufferCoord(LineLen + 1, FCaretY), True);
      end;
    ecEditorTop:
      MoveCaretAndSelection(BufferCoord(1, 1), False);
    ecSelEditorTop:
      MoveCaretAndSelection(BufferCoord(1, 1), True);
    ecEditorBottom:
      MoveCaretAndSelection(
        BufferCoord(1, Max(1, FLines.Count)), False);
    ecSelEditorBottom:
      MoveCaretAndSelection(
        BufferCoord(1, Max(1, FLines.Count)), True);
    ecWordLeft, ecSelWordLeft:
      begin
        var BC := GetCaretXY;
        if BC.Char > 1 then
        begin
          var S := FLines[BC.Line - 1];
          var NewChar := BC.Char - 1;
          while (NewChar > 1) and (NewChar <= Length(S)) and
            not IsWordBreakChar(S[NewChar - 1]) do
            Dec(NewChar);
          MoveCaretAndSelection(BufferCoord(NewChar, BC.Line),
            Command = ecSelWordLeft);
        end
        else if BC.Line > 1 then
        begin
          var PrevLen := Length(FLines[BC.Line - 2]);
          MoveCaretAndSelection(BufferCoord(PrevLen + 1, BC.Line - 1),
            Command = ecSelWordLeft);
        end;
      end;
    ecWordRight, ecSelWordRight:
      begin
        var BC := GetCaretXY;
        if (BC.Line >= 1) and (BC.Line <= FLines.Count) then
        begin
          var S := FLines[BC.Line - 1];
          if BC.Char <= Length(S) then
          begin
            var NewChar := BC.Char;
            while (NewChar <= Length(S)) and
              not IsWordBreakChar(S[NewChar]) do
              Inc(NewChar);
            while (NewChar <= Length(S)) and IsWordBreakChar(S[NewChar]) do
              Inc(NewChar);
            MoveCaretAndSelection(BufferCoord(NewChar, BC.Line),
              Command = ecSelWordRight);
          end
          else if BC.Line < FLines.Count then
            MoveCaretAndSelection(BufferCoord(1, BC.Line + 1),
              Command = ecSelWordRight);
        end;
      end;

    // Editing
    ecChar:
      if not FReadOnly then
      begin
        FirstAffectedLine := FCaretY - 1;
        FUndoRedo.BeginBlock(Self);
        try
          if GetSelAvail then
            DoDeleteSelection;
          InsertCharAtCursor(AChar);
        finally
          FUndoRedo.EndBlock(Self);
        end;
      end;
    ecDeleteChar:
      if not FReadOnly then
      begin
        FirstAffectedLine := FCaretY - 1;
        FUndoRedo.BeginBlock(Self);
        try
          if GetSelAvail then
            DoDeleteSelection
          else
            DoDeleteChar;
        finally
          FUndoRedo.EndBlock(Self);
        end;
      end;
    ecDeleteLastChar:
      if not FReadOnly then
      begin
        FirstAffectedLine := FCaretY - 1;
        FUndoRedo.BeginBlock(Self);
        try
          if GetSelAvail then
            DoDeleteSelection
          else
            DoDeleteLastChar;
        finally
          FUndoRedo.EndBlock(Self);
        end;
      end;
    ecLineBreak:
      if not FReadOnly then
      begin
        FirstAffectedLine := FCaretY - 1;
        FUndoRedo.BeginBlock(Self);
        try
          if GetSelAvail then
            DoDeleteSelection;
          DoInsertLine;
        finally
          FUndoRedo.EndBlock(Self);
        end;
      end;
    ecTab:
      if not FReadOnly then
      begin
        FirstAffectedLine := FCaretY - 1;
        if eoTabsToSpaces in FOptions then
        begin
          var Spaces := FTabWidth - ((FCaretX - 1) mod FTabWidth);
          FUndoRedo.BeginBlock(Self);
          try
            if GetSelAvail then
              DoDeleteSelection;
            for var I := 1 to Spaces do
              InsertCharAtCursor(' ');
          finally
            FUndoRedo.EndBlock(Self);
          end;
        end
        else
        begin
          FUndoRedo.BeginBlock(Self);
          try
            if GetSelAvail then
              DoDeleteSelection;
            InsertCharAtCursor(#9);
          finally
            FUndoRedo.EndBlock(Self);
          end;
        end;
      end;
    ecShiftTab:
      if not FReadOnly then
      begin
        // Remove up to TabWidth spaces from the beginning of the current line
        FirstAffectedLine := FCaretY - 1;
        if (FCaretY >= 1) and (FCaretY <= FLines.Count) then
        begin
          var Line := FLines[FCaretY - 1];
          var SpacesToRemove := 0;
          var MaxRemove := FTabWidth;
          while (SpacesToRemove < MaxRemove) and (SpacesToRemove < Length(Line))
            and (Line[SpacesToRemove + 1] = ' ') do
            Inc(SpacesToRemove);
          // If no spaces found, try removing a single tab
          if (SpacesToRemove = 0) and (Length(Line) > 0) and (Line[1] = #9) then
            SpacesToRemove := 1;
          if SpacesToRemove > 0 then
          begin
            FUndoRedo.BeginBlock(Self);
            try
              FLines[FCaretY - 1] := Copy(Line, SpacesToRemove + 1);
              // Adjust caret
              SetCaretX(Max(1, FCaretX - SpacesToRemove));
            finally
              FUndoRedo.EndBlock(Self);
            end;
          end;
        end;
      end;
    ecToggleMode:
      FInsertMode := not FInsertMode;

    // Clipboard
    ecCopy:     CopyToClipboard;
    ecCut:
      begin
        FirstAffectedLine := FCaretY - 1;
        CutToClipboard;
      end;
    ecPaste:
      begin
        FirstAffectedLine := FCaretY - 1;
        PasteFromClipboard;
      end;

    // Undo/Redo
    ecUndo:
      begin
        Undo;
        // Undo can affect any lines, so do a full rescan
        ScanRanges;
      end;
    ecRedo:
      begin
        Redo;
        // Redo can affect any lines, so do a full rescan
        ScanRanges;
      end;

    // Selection
    ecSelectAll: SelectAll;

    // Code folding
    ecFoldAll: CollapseAll;
    ecUnfoldAll: UncollapseAll;
    ecFoldNearest: CollapseNearest;
    ecUnfoldNearest: UncollapseNearest;
    ecFoldLevel1: CollapseLevel(1);
    ecUnfoldLevel1: UncollapseLevel(1);
    ecFoldLevel2: CollapseLevel(2);
    ecUnfoldLevel2: UncollapseLevel(2);
    ecFoldLevel3: CollapseLevel(3);
    ecUnfoldLevel3: UncollapseLevel(3);
  end;

  // Incremental range scan after text mutations
  if (FirstAffectedLine >= 0) and Assigned(FHighlighter) and
    (FLines.Count > 0) then
    ScanFrom(Max(0, FirstAffectedLine));

  // Reset caret blink after any command
  FCaretBlinkOn := True;
  if FCaretTimer.Enabled then
  begin
    FCaretTimer.Enabled := False;
    FCaretTimer.Enabled := True;
  end;
end;

{ --- Navigation --- }

procedure TCustomFMXSynEdit.MoveCaretHorz(DX: Integer; SelectionCmd: Boolean);
var
  NewCaret: TBufferCoord;
  LineLen: Integer;
begin
  NewCaret := GetCaretXY;

  if DX < 0 then
  begin
    if NewCaret.Char > 1 then
      Dec(NewCaret.Char)
    else if NewCaret.Line > 1 then
    begin
      Dec(NewCaret.Line);
      NewCaret.Char := Length(FLines[NewCaret.Line - 1]) + 1;
    end;
  end
  else
  begin
    if (NewCaret.Line >= 1) and (NewCaret.Line <= FLines.Count) then
      LineLen := Length(FLines[NewCaret.Line - 1])
    else
      LineLen := 0;
    if NewCaret.Char <= LineLen then
      Inc(NewCaret.Char)
    else if NewCaret.Line < FLines.Count then
    begin
      Inc(NewCaret.Line);
      NewCaret.Char := 1;
    end;
  end;

  FLastPosX := -1;
  MoveCaretAndSelection(NewCaret, SelectionCmd);
end;

procedure TCustomFMXSynEdit.MoveCaretVert(DY: Integer; SelectionCmd: Boolean);
var
  NewCaret: TBufferCoord;
  LineLen: Integer;
begin
  NewCaret := GetCaretXY;
  Inc(NewCaret.Line, DY);
  NewCaret.Line := Max(1, Min(NewCaret.Line, Max(1, FLines.Count)));

  // Sticky column
  if (eoKeepCaretX in FOptions) and (FLastPosX >= 0) then
    NewCaret.Char := FLastPosX
  else
    FLastPosX := NewCaret.Char;

  // Clamp to line length
  if (NewCaret.Line >= 1) and (NewCaret.Line <= FLines.Count) then
  begin
    LineLen := Length(FLines[NewCaret.Line - 1]);
    if not (eoScrollPastEol in FScrollOptions) then
      NewCaret.Char := Min(NewCaret.Char, LineLen + 1);
  end;

  MoveCaretAndSelection(NewCaret, SelectionCmd);
end;

procedure TCustomFMXSynEdit.MoveCaretAndSelection(const NewCaret: TBufferCoord;
  SelectionCmd: Boolean);
begin
  if SelectionCmd then
  begin
    // If no selection exists yet, start selection from current caret
    if FBlockBegin = FBlockEnd then
      FBlockBegin := GetCaretXY;
    FBlockEnd := NewCaret;
  end
  else
  begin
    // Clear selection
    FBlockBegin := NewCaret;
    FBlockEnd := NewCaret;
  end;

  FCaretX := NewCaret.Char;
  FCaretY := NewCaret.Line;
  EnsureCursorPosVisible;
  Repaint;
end;

{ --- Editing operations --- }

procedure TCustomFMXSynEdit.InsertCharAtCursor(AChar: WideChar);
var
  SLine: string;
begin
  // Ensure we have enough lines
  while FLines.Count < FCaretY do
    FLines.Add('');

  SLine := FLines[FCaretY - 1];

  // Pad line if caret is past end
  while Length(SLine) < FCaretX - 1 do
    SLine := SLine + ' ';

  if FInsertMode then
    System.Insert(AChar, SLine, FCaretX)
  else
  begin
    if FCaretX <= Length(SLine) then
      SLine[FCaretX] := AChar
    else
      SLine := SLine + AChar;
  end;

  FLines[FCaretY - 1] := SLine;
  Inc(FCaretX);
  FLastPosX := -1;
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.DoDeleteChar;
var
  SLine: string;
begin
  if (FCaretY < 1) or (FCaretY > FLines.Count) then Exit;
  SLine := FLines[FCaretY - 1];

  if FCaretX <= Length(SLine) then
  begin
    System.Delete(SLine, FCaretX, 1);
    FLines[FCaretY - 1] := SLine;
  end
  else if FCaretY < FLines.Count then
  begin
    // Join with next line
    FLines[FCaretY - 1] := SLine + FLines[FCaretY];
    FLines.Delete(FCaretY);
  end;
end;

procedure TCustomFMXSynEdit.DoDeleteLastChar;
var
  SLine: string;
begin
  if (FCaretX > 1) then
  begin
    if (FCaretY >= 1) and (FCaretY <= FLines.Count) then
    begin
      SLine := FLines[FCaretY - 1];
      if FCaretX - 1 <= Length(SLine) then
      begin
        System.Delete(SLine, FCaretX - 1, 1);
        FLines[FCaretY - 1] := SLine;
      end;
      Dec(FCaretX);
    end;
  end
  else if FCaretY > 1 then
  begin
    // Join with previous line
    var PrevLen := Length(FLines[FCaretY - 2]);
    FLines[FCaretY - 2] := FLines[FCaretY - 2] + FLines[FCaretY - 1];
    FLines.Delete(FCaretY - 1);
    Dec(FCaretY);
    FCaretX := PrevLen + 1;
  end;
  FLastPosX := -1;
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.DoInsertLine;
var
  SLine, LeftPart, RightPart, Indent: string;
begin
  if (FCaretY < 1) then Exit;

  while FLines.Count < FCaretY do
    FLines.Add('');

  SLine := FLines[FCaretY - 1];
  LeftPart := Copy(SLine, 1, FCaretX - 1);
  RightPart := Copy(SLine, FCaretX, MaxInt);

  // Auto-indent
  Indent := '';
  if eoAutoIndent in FOptions then
  begin
    var I := 1;
    while (I <= Length(LeftPart)) and (LeftPart[I] = ' ') do
      Inc(I);
    Indent := StringOfChar(' ', I - 1);
  end;

  FLines[FCaretY - 1] := LeftPart;
  FLines.Insert(FCaretY, Indent + RightPart);
  Inc(FCaretY);
  FCaretX := Length(Indent) + 1;
  FLastPosX := -1;
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.DoDeleteSelection;
var
  SelBC1, SelBC2: TBufferCoord;
  FirstLine, LastLine: string;
begin
  if not GetSelAvail then Exit;

  SelBC1 := FBlockBegin;
  SelBC2 := FBlockEnd;
  if SelBC1 > SelBC2 then
  begin
    var Tmp := SelBC1;
    SelBC1 := SelBC2;
    SelBC2 := Tmp;
  end;

  if SelBC1.Line = SelBC2.Line then
  begin
    // Single line deletion
    var SLine := FLines[SelBC1.Line - 1];
    System.Delete(SLine, SelBC1.Char, SelBC2.Char - SelBC1.Char);
    FLines[SelBC1.Line - 1] := SLine;
  end
  else
  begin
    // Multi-line deletion
    FirstLine := Copy(FLines[SelBC1.Line - 1], 1, SelBC1.Char - 1);
    LastLine := Copy(FLines[SelBC2.Line - 1], SelBC2.Char, MaxInt);
    FLines[SelBC1.Line - 1] := FirstLine + LastLine;
    // Delete intermediate and last lines
    if SelBC2.Line > SelBC1.Line then
      TSynEditStringList(FLines).DeleteLines(SelBC1.Line,
        SelBC2.Line - SelBC1.Line);
  end;

  FCaretX := SelBC1.Char;
  FCaretY := SelBC1.Line;
  FBlockBegin := BufferCoord(FCaretX, FCaretY);
  FBlockEnd := FBlockBegin;
  FLastPosX := -1;
end;

procedure TCustomFMXSynEdit.SetSelectedTextPrimitive(const Value: string);
begin
  FUndoRedo.BeginBlock(Self);
  try
    if GetSelAvail then
      DoDeleteSelection;

    if Value = '' then Exit;

    // Insert the text
    var Lines := Value.Split([#13#10, #10, #13]);
    if Length(Lines) = 1 then
    begin
      // Single line insert
      while FLines.Count < FCaretY do
        FLines.Add('');
      var SLine := FLines[FCaretY - 1];
      System.Insert(Value, SLine, FCaretX);
      FLines[FCaretY - 1] := SLine;
      Inc(FCaretX, Length(Value));
    end
    else
    begin
      // Multi-line insert
      while FLines.Count < FCaretY do
        FLines.Add('');
      var SLine := FLines[FCaretY - 1];
      var LeftPart := Copy(SLine, 1, FCaretX - 1);
      var RightPart := Copy(SLine, FCaretX, MaxInt);

      FLines[FCaretY - 1] := LeftPart + Lines[0];
      for var I := 1 to Length(Lines) - 1 do
        FLines.Insert(FCaretY - 1 + I, Lines[I]);
      // Append right part to last line
      var LastIdx := FCaretY - 1 + Length(Lines) - 1;
      FLines[LastIdx] := FLines[LastIdx] + RightPart;
      FCaretY := LastIdx + 1;
      FCaretX := Length(Lines[Length(Lines) - 1]) + 1;
    end;

    FBlockBegin := BufferCoord(FCaretX, FCaretY);
    FBlockEnd := FBlockBegin;
    FLastPosX := -1;
    EnsureCursorPosVisible;
  finally
    FUndoRedo.EndBlock(Self);
  end;
end;

{ --- Selection --- }

function TCustomFMXSynEdit.GetSelAvail: Boolean;
begin
  Result := FBlockBegin <> FBlockEnd;
end;

function TCustomFMXSynEdit.GetSelText: string;
var
  SelBC1, SelBC2: TBufferCoord;
begin
  Result := '';
  if not GetSelAvail then Exit;

  SelBC1 := FBlockBegin;
  SelBC2 := FBlockEnd;
  if SelBC1 > SelBC2 then
  begin
    var Tmp := SelBC1;
    SelBC1 := SelBC2;
    SelBC2 := Tmp;
  end;

  Result := GetTextRange(SelBC1, SelBC2);
end;

function TCustomFMXSynEdit.GetTextRange(AStart, AEnd: TBufferCoord): string;
var
  I: Integer;
  SB: TStringBuilder;
begin
  if (AStart.Line < 1) or (AStart.Line > FLines.Count) then Exit('');
  if AEnd.Line > FLines.Count then
    AEnd := BufferCoord(Length(FLines[FLines.Count - 1]) + 1, FLines.Count);

  if AStart.Line = AEnd.Line then
    Result := Copy(FLines[AStart.Line - 1], AStart.Char,
      AEnd.Char - AStart.Char)
  else
  begin
    SB := TStringBuilder.Create;
    try
      SB.Append(Copy(FLines[AStart.Line - 1], AStart.Char, MaxInt));
      for I := AStart.Line to AEnd.Line - 2 do
      begin
        SB.AppendLine;
        SB.Append(FLines[I]);
      end;
      SB.AppendLine;
      SB.Append(Copy(FLines[AEnd.Line - 1], 1, AEnd.Char - 1));
      Result := SB.ToString;
    finally
      SB.Free;
    end;
  end;
end;

procedure TCustomFMXSynEdit.SelectAll;
begin
  if FLines.Count > 0 then
  begin
    FBlockBegin := BufferCoord(1, 1);
    var LastLine := FLines.Count;
    FBlockEnd := BufferCoord(Length(FLines[LastLine - 1]) + 1, LastLine);
    FCaretX := FBlockEnd.Char;
    FCaretY := FBlockEnd.Line;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.ClearSelection;
begin
  FBlockBegin := GetCaretXY;
  FBlockEnd := FBlockBegin;
  Repaint;
end;

procedure TCustomFMXSynEdit.SetCaretAndSelection(const ACaretXY, ABlockBegin,
  ABlockEnd: TBufferCoord);
begin
  FBlockBegin := ABlockBegin;
  FBlockEnd := ABlockEnd;
  FCaretX := ACaretXY.Char;
  FCaretY := ACaretXY.Line;
  EnsureCursorPosVisible;
  Repaint;
end;

{ --- Clipboard --- }

procedure TCustomFMXSynEdit.CopyToClipboard;
var
  S: string;
begin
  S := GetSelText;
  if S <> '' then
    SetClipboardText(S);
end;

procedure TCustomFMXSynEdit.CutToClipboard;
begin
  if not FReadOnly and GetSelAvail then
  begin
    CopyToClipboard;
    FUndoRedo.BeginBlock(Self);
    try
      DoDeleteSelection;
    finally
      FUndoRedo.EndBlock(Self);
    end;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.PasteFromClipboard;
var
  S: string;
begin
  if FReadOnly then Exit;
  S := GetClipboardText;
  if S <> '' then
    SetSelectedTextPrimitive(S);
end;

{ --- Undo/Redo --- }

procedure TCustomFMXSynEdit.Undo;
begin
  if FUndoRedo.CanUndo then
  begin
    FUndoRedo.Undo(Self);
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.Redo;
begin
  if FUndoRedo.CanRedo then
  begin
    FUndoRedo.Redo(Self);
    Repaint;
  end;
end;

{ --- File I/O --- }

procedure TCustomFMXSynEdit.LoadFromFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomFMXSynEdit.SaveToFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomFMXSynEdit.LoadFromStream(AStream: TStream);
begin
  BeginUpdate;
  try
    FUndoRedo.Lock;
    try
      FLines.LoadFromStream(AStream);
    finally
      FUndoRedo.Unlock;
    end;
    FUndoRedo.Clear;
    FUndoRedo.Modified := False;
    FCaretX := 1;
    FCaretY := 1;
    FBlockBegin := BufferCoord(1, 1);
    FBlockEnd := BufferCoord(1, 1);
    FTopLine := 1;
    FLeftChar := 1;
    ScanRanges;
    if FUseCodeFolding then
      FullFoldScan;
  finally
    EndUpdate;
  end;
end;

procedure TCustomFMXSynEdit.SaveToStream(AStream: TStream);
begin
  FLines.SaveToStream(AStream);
  FUndoRedo.Modified := False;
end;

procedure TCustomFMXSynEdit.ClearAll;
begin
  BeginUpdate;
  try
    FUndoRedo.Lock;
    try
      FLines.Clear;
    finally
      FUndoRedo.Unlock;
    end;
    FUndoRedo.Clear;
    FUndoRedo.Modified := False;
    FCaretX := 1;
    FCaretY := 1;
    FBlockBegin := BufferCoord(1, 1);
    FBlockEnd := BufferCoord(1, 1);
  finally
    EndUpdate;
  end;
end;

{ --- Mouse handling --- }

procedure TCustomFMXSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  BC: TBufferCoord;
  Row, Line, Index: Integer;
  FoldGutterLeft: Single;
begin
  inherited;
  if not IsFocused then
    SetFocus;

  if Button = TMouseButton.mbLeft then
  begin
    // Check for fold gutter click
    if FUseCodeFolding then
    begin
      FoldGutterLeft := FGutterWidth - FCodeFolding.GutterShapeSize - 8;
      if (X >= FoldGutterLeft) and (X < FGutterWidth) then
      begin
        Row := Max(1, FTopLine + Trunc(Y / FLineHeight));
        Line := RowToLine(Row);
        if FAllFoldRanges.FoldStartAtLine(Line, Index) then
        begin
          if FAllFoldRanges.Ranges[Index].Collapsed then
            Uncollapse(Index)
          else
            Collapse(Index);
        end;
        Exit;
      end;
    end;

    BC := PixelToBufferCoord(X, Y);
    if ssShift in Shift then
    begin
      // Extend selection
      FBlockEnd := BC;
    end
    else
    begin
      FBlockBegin := BC;
      FBlockEnd := BC;
    end;
    FCaretX := BC.Char;
    FCaretY := BC.Line;
    FLastPosX := -1;
    FCaretBlinkOn := True;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.MouseMove(Shift: TShiftState; X, Y: Single);
var
  BC: TBufferCoord;
begin
  inherited;
  if ssLeft in Shift then
  begin
    BC := PixelToBufferCoord(X, Y);
    FBlockEnd := BC;
    FCaretX := BC.Char;
    FCaretY := BC.Line;
    EnsureCursorPosVisible;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
end;

procedure TCustomFMXSynEdit.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;
  if FScrollBars <> nil then
  begin
    (FScrollBars as ISynEditScrollBars).DoMouseWheel(Shift, WheelDelta,
      TPointF.Zero);
    Handled := True;
  end;
end;

{ --- Property setters --- }

procedure TCustomFMXSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if FHighlighter <> Value then
  begin
    if FHighlighter <> nil then
      FHighlighter.RemoveFreeNotification(Self);
    FHighlighter := Value;
    if FHighlighter <> nil then
      FHighlighter.FreeNotification(Self);
    ScanRanges;
    if FUseCodeFolding then
      FullFoldScan;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetTabWidth(Value: Integer);
begin
  if (Value > 0) and (Value <> FTabWidth) then
  begin
    FTabWidth := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TCustomFMXSynEdit.SetCaretX(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FCaretX <> Value then
  begin
    FCaretX := Value;
    FLastPosX := -1;
    EnsureCursorPosVisible;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetCaretY(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FCaretY <> Value then
  begin
    FCaretY := Value;
    EnsureCursorPosVisible;
    Repaint;
  end;
end;

function TCustomFMXSynEdit.GetCaretXY: TBufferCoord;
begin
  Result := BufferCoord(FCaretX, FCaretY);
end;

procedure TCustomFMXSynEdit.SetCaretXY(const Value: TBufferCoord);
begin
  FCaretX := Max(1, Value.Char);
  FCaretY := Max(1, Value.Line);
  FBlockBegin := Value;
  FBlockEnd := Value;
  FLastPosX := -1;
  EnsureCursorPosVisible;
  Repaint;
end;

procedure TCustomFMXSynEdit.SetRightEdge(Value: Integer);
begin
  if FRightEdge <> Value then
  begin
    FRightEdge := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetRightEdgeColor(Value: TColor);
begin
  if FRightEdgeColor <> Value then
  begin
    FRightEdgeColor := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetActiveLineColor(Value: TColor);
begin
  if FActiveLineColor <> Value then
  begin
    FActiveLineColor := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetOptions(Value: TSynEditorOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if FUndoRedo <> nil then
      FUndoRedo.GroupUndo := eoGroupUndo in FOptions;
  end;
end;

procedure TCustomFMXSynEdit.SetScrollOptions(Value: TSynEditorScrollOptions);
begin
  FScrollOptions := Value;
end;

function TCustomFMXSynEdit.GetText: string;
begin
  Result := FLines.Text;
end;

procedure TCustomFMXSynEdit.SetText(const Value: string);
begin
  FLines.Text := Value;
  FCaretX := 1;
  FCaretY := 1;
  FBlockBegin := BufferCoord(1, 1);
  FBlockEnd := BufferCoord(1, 1);
  FTopLine := 1;
  FLeftChar := 1;
  ScanRanges;
  RecalcSizes;
  Repaint;
end;

function TCustomFMXSynEdit.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TCustomFMXSynEdit.GetCanUndo: Boolean;
begin
  Result := (FUndoRedo <> nil) and FUndoRedo.CanUndo;
end;

function TCustomFMXSynEdit.GetCanRedo: Boolean;
begin
  Result := (FUndoRedo <> nil) and FUndoRedo.CanRedo;
end;

{ --- Notification --- }

procedure TCustomFMXSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FHighlighter then
      FHighlighter := nil;
    if AComponent = FSearchEngine then
      FSearchEngine := nil;
  end;
end;

{ --- Row/Line mapping --- }

function TCustomFMXSynEdit.LineToRow(aLine: Integer): Integer;
begin
  if FUseCodeFolding then
    Result := FAllFoldRanges.FoldLineToRow(aLine)
  else
    Result := aLine;
end;

function TCustomFMXSynEdit.RowToLine(aRow: Integer): Integer;
begin
  if FUseCodeFolding then
    Result := FAllFoldRanges.FoldRowToLine(aRow)
  else
    Result := aRow;
end;

function TCustomFMXSynEdit.GetDisplayRowCount: Integer;
begin
  if FUseCodeFolding then
    Result := LineToRow(FLines.Count)
  else
    Result := FLines.Count;
end;

{ --- Code Folding --- }

procedure TCustomFMXSynEdit.SetUseCodeFolding(const Value: Boolean);
var
  ValidValue: Boolean;
begin
  if csLoading in ComponentState then
  begin
    FUseCodeFolding := Value;
    Exit;
  end;

  ValidValue := Value and ((Assigned(FHighlighter) and
    (FHighlighter is TSynCustomCodeFoldingHighlighter))
      or Assigned(FOnScanForFoldRanges));

  if FUseCodeFolding <> ValidValue then
  begin
    FAllFoldRanges.Reset;
    FUseCodeFolding := ValidValue;
    if ValidValue then
    begin
      if FHighlighter is TSynCustomCodeFoldingHighlighter then
      begin
        TSynCustomCodeFoldingHighlighter(FHighlighter).InitFoldRanges(FAllFoldRanges);
        FAllFoldRanges.AdjustRangesProc :=
          TSynCustomCodeFoldingHighlighter(FHighlighter).AdjustFoldRanges;
      end
      else
        FAllFoldRanges.AdjustRangesProc := nil;
      FullFoldScan;
    end
    else
      FAllFoldRanges.AdjustRangesProc := nil;

    UpdateGutterWidth;
    RecalcSizes;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.OnCodeFoldingChange(Sender: TObject);
begin
  UpdateGutterWidth;
  Repaint;
end;

procedure TCustomFMXSynEdit.FullFoldScan;
begin
  if FUseCodeFolding then
    ReScanForFoldRanges(0, FLines.Count - 1);
end;

procedure TCustomFMXSynEdit.ReScanForFoldRanges(FromLine, ToLine: Integer);
var
  AdjustedToLine: Integer;
begin
  AdjustedToLine := Max(Min(ToLine, FLines.Count - 1), FromLine);
  FAllFoldRanges.StartScanning;
  ScanForFoldRanges(FAllFoldRanges, FLines, FromLine, AdjustedToLine);
  if not FLines.Updating and FAllFoldRanges.StopScanning(FLines) then
  begin
    UpdateGutterWidth;
    UpdateScrollBars;
  end;
end;

procedure TCustomFMXSynEdit.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
begin
  if FHighlighter is TSynCustomCodeFoldingHighlighter then
    TSynCustomCodeFoldingHighlighter(FHighlighter).ScanForFoldRanges(
      FoldRanges, LinesToScan, FromLine, ToLine);

  if Assigned(FOnScanForFoldRanges) then
    FOnScanForFoldRanges(Self, FoldRanges, LinesToScan, FromLine, ToLine);
end;

procedure TCustomFMXSynEdit.PaintFoldGutter(Canvas: TCanvas;
  FirstRow, LastRow: Integer);
var
  Renderer: TSynFMXRenderer;
  Row, Line, Index: Integer;
  Y, X, FoldLeft: Single;
  ShapeSize: Single;
  rcFold: TRectF;
  FoldRange: TSynFoldRange;
  Margin: Single;
  LinesColor: TAlphaColor;
begin
  Renderer := TSynFMXRenderer(FRenderer);
  ShapeSize := FCodeFolding.GutterShapeSize;
  FoldLeft := FGutterWidth - ShapeSize - 4;
  Margin := 2;
  LinesColor := TColorToAlphaColor(FCodeFolding.FolderBarLinesColor);

  for Row := FirstRow to LastRow do
  begin
    Line := RowToLine(Row);
    if Line > FLines.Count then Break;

    Y := (Row - FTopLine) * FLineHeight;
    rcFold := RectF(
      FoldLeft,
      Y + (FLineHeight - ShapeSize) / 2,
      FoldLeft + ShapeSize,
      Y + (FLineHeight + ShapeSize) / 2);

    // Fold start at this line?
    if FAllFoldRanges.FoldStartAtLine(Line, Index) then
    begin
      FoldRange := FAllFoldRanges.Ranges[Index];

      // Draw square
      Canvas.Stroke.Color := LinesColor;
      Canvas.Stroke.Thickness := 1;
      Canvas.DrawRect(rcFold, 0, 0, AllCorners, 1.0);

      // Draw horizontal minus sign
      X := rcFold.Left + ShapeSize / 2;
      Renderer.DrawLine(Canvas,
        rcFold.Left + Margin, rcFold.Top + ShapeSize / 2,
        rcFold.Right - Margin, rcFold.Top + ShapeSize / 2,
        LinesColor);

      if FoldRange.Collapsed then
      begin
        // Draw vertical plus sign
        Renderer.DrawLine(Canvas,
          X, rcFold.Top + Margin,
          X, rcFold.Bottom - Margin,
          LinesColor);
      end
      else
      begin
        // Draw line from bottom of square to bottom of row
        Renderer.DrawLine(Canvas,
          X, rcFold.Bottom,
          X, Y + FLineHeight,
          LinesColor);
      end;
    end
    else
    begin
      X := rcFold.Left + ShapeSize / 2;

      // Fold end at this line?
      if FAllFoldRanges.FoldEndAtLine(Line, Index) then
      begin
        // L-connector: vertical line from top, then horizontal to right
        Renderer.DrawLine(Canvas,
          X, Y,
          X, Y + FLineHeight / 2,
          LinesColor);
        Renderer.DrawLine(Canvas,
          X, Y + FLineHeight / 2,
          rcFold.Right, Y + FLineHeight / 2,
          LinesColor);
      end;

      // Line through fold body?
      if FAllFoldRanges.FoldAroundLine(Line, Index) then
      begin
        Renderer.DrawLine(Canvas,
          X, Y,
          X, Y + FLineHeight,
          LinesColor);
      end;
    end;
  end;
end;

procedure TCustomFMXSynEdit.Collapse(FoldRangeIndex: Integer; Invalidate: Boolean);
var
  Range: TSynFoldRange;
begin
  if not FUseCodeFolding then Exit;

  if FAllFoldRanges.Collapse(FoldRangeIndex) then
  begin
    Range := FAllFoldRanges[FoldRangeIndex];
    // Extract caret from fold
    if (FCaretY > Range.FromLine) and (FCaretY <= Range.ToLine) then
      CaretXY := BufferCoord(Length(FLines[Range.FromLine - 1]) + 1, Range.FromLine);
    if Invalidate then
    begin
      UpdateScrollBars;
      Repaint;
    end;
  end;
end;

procedure TCustomFMXSynEdit.Uncollapse(FoldRangeIndex: Integer; Invalidate: Boolean);
begin
  if not FUseCodeFolding then Exit;

  FAllFoldRanges.UnCollapse(FoldRangeIndex);
  if Invalidate then
  begin
    UpdateScrollBars;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.CollapseAll;
begin
  if not FUseCodeFolding then Exit;
  FAllFoldRanges.CollapseAll;

  // Surface caret from hidden folds
  var Index: Integer;
  while FAllFoldRanges.FoldHidesLine(FCaretY, Index) do
  begin
    var Range := FAllFoldRanges[Index];
    CaretXY := BufferCoord(Length(FLines[Range.FromLine - 1]) + 1, Range.FromLine);
  end;

  EnsureCursorPosVisible;
  UpdateScrollBars;
  Repaint;
end;

procedure TCustomFMXSynEdit.UncollapseAll;
var
  I: Integer;
begin
  if not FUseCodeFolding then Exit;
  for I := FAllFoldRanges.Count - 1 downto 0 do
    FAllFoldRanges.UnCollapse(I);

  UpdateScrollBars;
  EnsureCursorPosVisible;
  Repaint;
end;

procedure TCustomFMXSynEdit.CollapseNearest;
var
  Index: Integer;
begin
  if not FUseCodeFolding then Exit;
  if FAllFoldRanges.FoldAroundLineEx(FCaretY, False, True, True, Index) then
    Collapse(Index);
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.UncollapseNearest;
var
  Index: Integer;
begin
  if not FUseCodeFolding then Exit;
  if FAllFoldRanges.CollapsedFoldStartAtLine(FCaretY, Index) then
    Uncollapse(Index);
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.CollapseLevel(Level: Integer);
begin
  if not FUseCodeFolding then Exit;
  FAllFoldRanges.CollapseLevel(Level);

  // Surface caret
  var Index: Integer;
  while FAllFoldRanges.FoldHidesLine(FCaretY, Index) do
  begin
    var Range := FAllFoldRanges[Index];
    CaretXY := BufferCoord(Length(FLines[Range.FromLine - 1]) + 1, Range.FromLine);
  end;

  EnsureCursorPosVisible;
  UpdateScrollBars;
  Repaint;
end;

procedure TCustomFMXSynEdit.UncollapseLevel(Level: Integer);
begin
  if not FUseCodeFolding then Exit;
  FAllFoldRanges.UnCollapseLevel(Level);

  EnsureCursorPosVisible;
  UpdateScrollBars;
  Repaint;
end;

{ --- Search/Replace --- }

procedure TCustomFMXSynEdit.SetSearchEngine(Value: TSynEditSearchCustom);
begin
  if FSearchEngine <> Value then
  begin
    if FSearchEngine <> nil then
      FSearchEngine.RemoveFreeNotification(Self);
    FSearchEngine := Value;
    if FSearchEngine <> nil then
      FSearchEngine.FreeNotification(Self);
  end;
end;

function TCustomFMXSynEdit.DoOnReplaceText(const ASearch, AReplace: string;
  Line, Column: Integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(FOnReplaceText) then
    FOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

function TCustomFMXSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions): Integer;
var
  ptStart, ptEnd: TBufferCoord;
  bBackward, bFromCursor, bPrompt, bReplace, bReplaceAll: Boolean;
  sReplace: string;
  nEOLCount, I: Integer;

  function ProcessTextRange(const AStart: TBufferCoord;
    var AEnd: TBufferCoord): Integer;
  var
    lnStart, lnEnd, nInLine, nFound, nSearchLen, nReplaceLen, n: Integer;
    iResultOffset: Integer;
    CurrentLine: Integer;
    Line: string;
    nAction: TSynReplaceAction;
  begin
    Result := 0;
    if bBackward then
      CurrentLine := AEnd.Line
    else
      CurrentLine := AStart.Line;

    while (CurrentLine >= AStart.Line) and (CurrentLine <= AEnd.Line) do
    begin
      Line := FLines[CurrentLine - 1];
      if CurrentLine = AStart.Line then
        lnStart := AStart.Char
      else
        lnStart := 1;

      if CurrentLine = AEnd.Line then
        lnEnd := AEnd.Char
      else
        lnEnd := Length(Line) + 1;

      if lnEnd <= lnStart then
      begin
        if bBackward then Dec(CurrentLine) else Inc(CurrentLine);
        Continue;
      end;

      nInLine := FSearchEngine.FindAll(Line, lnStart, lnEnd);
      iResultOffset := 0;
      if bBackward then
        n := FSearchEngine.ResultCount - 1
      else
        n := 0;

      while nInLine > 0 do
      begin
        nFound := FSearchEngine.Results[n] + iResultOffset;
        nSearchLen := FSearchEngine.Lengths[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);
        Inc(Result);

        // Select the found text
        if bBackward then
          SetCaretAndSelection(
            BufferCoord(nFound, CurrentLine),
            BufferCoord(nFound + nSearchLen, CurrentLine),
            BufferCoord(nFound, CurrentLine))
        else
          SetCaretAndSelection(
            BufferCoord(nFound + nSearchLen, CurrentLine),
            BufferCoord(nFound, CurrentLine),
            BufferCoord(nFound + nSearchLen, CurrentLine));

        // If search only, return after first find
        if not (bReplace or bReplaceAll) then Exit;

        // Prompt for replace
        if bPrompt and Assigned(FOnReplaceText) then
        begin
          nAction := DoOnReplaceText(ASearch, sReplace, CurrentLine, nFound);
          if nAction = raCancel then
          begin
            Dec(Result);
            Exit;
          end;
        end
        else
          nAction := raReplace;

        if nAction = raSkip then
          Dec(Result)
        else
        begin
          if nAction = raReplaceAll then
          begin
            bReplaceAll := True;
            bPrompt := False;
          end;

          // Perform replacement
          var SelText := FSearchEngine.Replace(
            Copy(FLines[CurrentLine - 1], nFound, nSearchLen), sReplace);

          var SLine := FLines[CurrentLine - 1];
          System.Delete(SLine, nFound, nSearchLen);
          System.Insert(SelText, SLine, nFound);
          FLines[CurrentLine - 1] := SLine;
          nReplaceLen := Length(SelText);

          if not bBackward then
          begin
            FCaretX := nFound + nReplaceLen;
            if nSearchLen <> nReplaceLen then
            begin
              Inc(iResultOffset, nReplaceLen - nSearchLen);
              if CurrentLine = AEnd.Line then
                Inc(AEnd.Char, nReplaceLen - nSearchLen);
            end;
          end;
        end;

        if not bReplaceAll then Exit;
      end;

      if bBackward then Dec(CurrentLine) else Inc(CurrentLine);
    end;
  end;

begin
  if not Assigned(FSearchEngine) then
    raise Exception.Create('No search engine has been assigned');

  Result := 0;
  if Length(ASearch) = 0 then Exit;

  bBackward := ssoBackwards in AOptions;
  bPrompt := ssoPrompt in AOptions;
  bReplace := ssoReplace in AOptions;
  bReplaceAll := ssoReplaceAll in AOptions;
  bFromCursor := not (ssoEntireScope in AOptions);
  sReplace := FSearchEngine.PreprocessReplaceExpression(AReplace);

  // Count line ends in replacement
  nEOLCount := 0;
  I := 1;
  repeat
    I := Pos(#13#10, sReplace, I);
    if I <> 0 then
    begin
      I := I + 2;
      Inc(nEOLCount);
    end;
  until I = 0;

  // Initialize search engine
  FSearchEngine.Options := AOptions;
  FSearchEngine.Pattern := ASearch;

  BeginUpdate;
  try
    if not (ssoSelectedOnly in AOptions) then
    begin
      ptStart.Char := 1;
      ptStart.Line := 1;
      ptEnd.Line := FLines.Count;
      if ptEnd.Line > 0 then
        ptEnd.Char := Length(FLines[ptEnd.Line - 1]) + 1
      else
        ptEnd.Char := 1;
      if bFromCursor then
      begin
        if bBackward then
          ptEnd := GetCaretXY
        else
          ptStart := GetCaretXY;
      end;
      Result := ProcessTextRange(ptStart, ptEnd);
    end
    else if GetSelAvail then
    begin
      ptStart := FBlockBegin;
      ptEnd := FBlockEnd;
      if ptStart > ptEnd then
      begin
        var Tmp := ptStart;
        ptStart := ptEnd;
        ptEnd := Tmp;
      end;
      Result := ProcessTextRange(ptStart, ptEnd);
    end;

    // Notify if not found
    if (Result = 0) and not (bReplace or bReplaceAll) and
      Assigned(FOnSearchNotFound) then
      FOnSearchNotFound(Self);
  finally
    EndUpdate;
    FSearchEngine.IsWordBreakFunction := nil;
  end;
end;

{ --- Plugin support --- }

procedure TCustomFMXSynEdit.RegisterPlugin(APlugin: TSynFMXEditPlugin);
begin
  if FPlugins.IndexOf(APlugin) < 0 then
    FPlugins.Add(APlugin);
end;

procedure TCustomFMXSynEdit.UnregisterPlugin(APlugin: TSynFMXEditPlugin);
begin
  FPlugins.Remove(APlugin);
end;

procedure TCustomFMXSynEdit.DoPluginAfterPaint(Canvas: TCanvas;
  const AClip: TRectF; FirstLine, LastLine: Integer);
var
  I: Integer;
  Plugin: TSynFMXEditPlugin;
begin
  for I := 0 to FPlugins.Count - 1 do
  begin
    Plugin := TSynFMXEditPlugin(FPlugins[I]);
    if phAfterPaint in Plugin.Handlers then
      Plugin.AfterPaint(Canvas, AClip, FirstLine, LastLine);
  end;
end;

end.
