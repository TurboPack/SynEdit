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
  FMX.SynEditMiscClasses;

type
  TCustomFMXSynEdit = class;

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
    procedure SetSelectedTextPrimitive(const Value: string);
    // Navigation helpers
    procedure MoveCaretHorz(DX: Integer; SelectionCmd: Boolean);
    procedure MoveCaretVert(DY: Integer; SelectionCmd: Boolean);
    procedure MoveCaretAndSelection(const NewCaret: TBufferCoord;
      SelectionCmd: Boolean);
    // Command processing
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
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
    function PixelToBufferCoord(X, Y: Single): TBufferCoord;
    function BufferCoordToPixel(const BC: TBufferCoord): TPointF;
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
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStatusChange: TNotifyEvent read FOnStatusChange
      write FOnStatusChange;
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
    property OnChange;
    property OnStatusChange;
  end;

implementation

uses
  FMX.SynEditRenderer,
  FMX.SynEditScrollBars,
  FMX.SynEditTypes,
  FMX.SynEditUndo,
  FMX.SynUnicode,
  SynEditKeyConst;

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

  UpdateGutterWidth;
end;

destructor TCustomFMXSynEdit.Destroy;
begin
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
begin
  LineCount := Max(FLines.Count, 1);
  DigitCount := Max(2, Length(IntToStr(LineCount)));
  FGutterWidth := Round((DigitCount + 1) * FCharWidth) + 4;
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
  MaxTop := Max(1, FLines.Count - FLinesInWindow + 1);
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
begin
  // Vertical
  if FCaretY < FTopLine then
    TopLine := FCaretY
  else if FCaretY >= FTopLine + FLinesInWindow then
    TopLine := FCaretY - FLinesInWindow + 1;
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
  Result.Y := (BC.Line - FTopLine) * FLineHeight;
end;

function TCustomFMXSynEdit.PixelToBufferCoord(X, Y: Single): TBufferCoord;
begin
  Result.Char := Max(1, FLeftChar + Round((X - FTextAreaLeft) / FCharWidth));
  Result.Line := Max(1, Min(FTopLine + Trunc(Y / FLineHeight), FLines.Count));
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
  LastLine := Min(FTopLine + FLinesInWindow, FLines.Count);

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

  if FCaretVisible and FCaretBlinkOn then
    PaintCaret(Canvas);
end;

procedure TCustomFMXSynEdit.PaintGutter(Canvas: TCanvas;
  FirstLine, LastLine: Integer);
var
  Renderer: TSynFMXRenderer;
  Line: Integer;
  Y: Single;
  R: TRectF;
  NumStr: string;
begin
  Renderer := TSynFMXRenderer(FRenderer);

  // Gutter background
  R := RectF(0, 0, FGutterWidth, Height);
  Renderer.FillRect(Canvas, R, TAlphaColors.Whitesmoke);

  // Gutter border
  Renderer.DrawLine(Canvas, FGutterWidth - 1, 0, FGutterWidth - 1, Height,
    TAlphaColors.Lightgray);

  // Line numbers
  for Line := FirstLine to LastLine do
  begin
    Y := (Line - FTopLine) * FLineHeight;
    NumStr := IntToStr(Line);
    R := RectF(2, Y, FGutterWidth - 4, Y + FLineHeight);
    Renderer.PaintLineNumber(Canvas, R, NumStr, TAlphaColors.Gray);
  end;
end;

procedure TCustomFMXSynEdit.PaintTextLines(Canvas: TCanvas;
  FirstLine, LastLine: Integer);
var
  Renderer: TSynFMXRenderer;
  Line: Integer;
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

  for Line := FirstLine to LastLine do
  begin
    Y := (Line - FTopLine) * FLineHeight;

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

        // Check if this token is within selection
        if (SelStart > 0) and (SelEnd > SelStart) then
        begin
          var TokStart := TokenPos + 1; // 1-based
          var TokEnd := TokenPos + Length(Token); // 1-based, inclusive
          if (TokStart < SelEnd) and (TokEnd >= SelStart) then
          begin
            ForeColor := TColorToAlphaColor(FSelectedColor.Foreground);
            if ForeColor = TAlphaColors.Null then
              ForeColor := TAlphaColors.White;
          end;
        end;

        // Paint the token
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
          Renderer.PaintToken(Canvas, X, Y, Token, ForeColor,
            BackColor, Style);

        FHighlighter.Next;
      end;
    end
    else if SLine <> '' then
    begin
      // No highlighter - paint plain text
      SExpanded := ExpandTabs(SLine, FTabWidth);
      X := FTextAreaLeft;
      var VisText := Copy(SExpanded, FLeftChar, FCharsInWindow + 1);
      ForeColor := TAlphaColors.Black;
      if (SelStart > 0) and (SelEnd > SelStart) then
      begin
        ForeColor := TColorToAlphaColor(FSelectedColor.Foreground);
        if ForeColor = TAlphaColors.Null then
          ForeColor := TAlphaColors.Black;
      end;
      Renderer.PaintToken(Canvas, X, Y, VisText, ForeColor,
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

{ --- Command execution --- }

procedure TCustomFMXSynEdit.ExecuteCommand(Command: TSynEditorCommand;
  AChar: WideChar);
begin
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
    ecToggleMode:
      FInsertMode := not FInsertMode;

    // Clipboard
    ecCopy:     CopyToClipboard;
    ecCut:      CutToClipboard;
    ecPaste:    PasteFromClipboard;

    // Undo/Redo
    ecUndo:     Undo;
    ecRedo:     Redo;

    // Selection
    ecSelectAll: SelectAll;
  end;

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
begin
  inherited;
  if not IsFocused then
    SetFocus;

  if Button = TMouseButton.mbLeft then
  begin
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
    FHighlighter := Value;
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

end.
