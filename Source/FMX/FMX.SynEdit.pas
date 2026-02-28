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
  SynEditSelections,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditTextBuffer,
  SynEditMiscProcs,
  SynEditCodeFolding,
  FMX.SynEditKbdHandler,
  FMX.SynEditMiscClasses,
  FMX.SynEditWordWrap;

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
    FSelection: TSynSelection;
    FSelections: TSynFMXSelections;
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
    FUpdateCount: Integer;
    // Word wrap
    FWordWrap: Boolean;
    FWordWrapHelper: TFMXWordWrapHelper;
    // Code folding
    FUseCodeFolding: Boolean;
    FCodeFolding: TSynCodeFolding;
    FAllFoldRanges: TSynFoldRanges;
    FOnScanForFoldRanges: TScanForFoldRangesEvent;
    // Search/Replace
    FSearchEngine: TSynEditSearchCustom;
    FOnReplaceText: TReplaceTextEvent;
    FOnSearchNotFound: TNotifyEvent;
    // Keyboard handler chain
    FKbdHandler: TSynEditKbdHandler;
    // Bookmarks
    FBookmarks: array[0..9] of TSynFMXEditMark;
    FMarkList: TSynFMXEditMarkList;
    FOnPlaceBookmark: TNotifyEvent;
    FOnClearBookmark: TNotifyEvent;
    // Gutter
    FGutter: TSynFMXGutter;
    // Plugins
    FPlugins: TList;
    // Cached max scroll width
    FMaxScrollWidth: Integer;
    FMaxScrollWidthValid: Boolean;
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
    function GetModified: Boolean;
    function GetMaxScrollWidth: Integer;
    function GetCaretX: Integer;
    function GetCaretY: Integer;
    function GetCaretXY: TBufferCoord;
    procedure SetCaretXY(const Value: TBufferCoord);
    function GetBlockBegin: TBufferCoord;
    function GetBlockEnd: TBufferCoord;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetSelText: string;
    function GetSelAvail: Boolean;
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
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
    // Gutter
    procedure GutterChanged(Sender: TObject);
    // Search/Replace private
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column: Integer): TSynReplaceAction;
    // Multi-caret private
    function ColumnSelectionStart: TBufferCoord;
    procedure ExecuteMultiCaretCommand(Command: TSynEditorCommand;
      AChar: WideChar);
    procedure SelectAllMatchingText;
    procedure CaretsAtLineEnds;
    // Word wrap private
    procedure SetWordWrap(Value: Boolean);
    function GetWrapAreaWidth: Integer;
    function GetDisplayRowCount: Integer;
  protected
    // Plugin hooks (protected for testability)
    procedure DoPluginAfterPaint(Canvas: TCanvas; const AClip: TRectF;
      FirstLine, LastLine: Integer);
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
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: WideChar);
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
    // Bookmarks
    procedure SetBookmark(ABookmark: Integer; X, Y: Integer);
    procedure ClearBookmark(ABookmark: Integer);
    procedure GotoBookmark(ABookmark: Integer);
    function GetBookmark(ABookmark: Integer; var X, Y: Integer): Boolean;
    function IsBookmarkSet(ABookmark: Integer): Boolean;
    // Plugin management
    procedure RegisterPlugin(APlugin: TSynFMXEditPlugin);
    procedure UnregisterPlugin(APlugin: TSynFMXEditPlugin);
    // Keyboard handler chain
    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    // Row/Line mapping and coordinate conversion
    function LineToRow(aLine: Integer): Integer;
    function RowToLine(aRow: Integer): Integer;
    function BufferToDisplayPos(const P: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const P: TDisplayCoord): TBufferCoord;
    function GetRowLength(ARow: Integer): Integer;
    property CodeFolding: TSynCodeFolding read FCodeFolding write FCodeFolding;
    property UseCodeFolding: Boolean read FUseCodeFolding write SetUseCodeFolding;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property AllFoldRanges: TSynFoldRanges read FAllFoldRanges;
    property Lines: TSynEditStringList read FLines;
    property LineCount: Integer read GetLineCount;
    property CaretX: Integer read GetCaretX write SetCaretX;
    property CaretY: Integer read GetCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property Selections: TSynFMXSelections read FSelections;
    property TopLine: Integer read FTopLine write SetTopLine;
    property LeftChar: Integer read FLeftChar write SetLeftChar;
    property Modified: Boolean read GetModified;
    property InsertMode: Boolean read FInsertMode write FInsertMode;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property Text: string read GetText write SetText;
    property SelText: string read GetSelText;
    property SelAvail: Boolean read GetSelAvail;
    property CharWidth: Single read FCharWidth;
    property LineHeight: Single read FLineHeight;
    property LinesInWindow: Integer read FLinesInWindow;
    property CharsInWindow: Integer read FCharsInWindow;
    property MaxScrollWidth: Integer read GetMaxScrollWidth;
    property DisplayRowCount: Integer read GetDisplayRowCount;
    property Marks: TSynFMXEditMarkList read FMarkList;
    property Gutter: TSynFMXGutter read FGutter;
    property GutterWidth: Single read FGutterWidth;
    property Renderer: TObject read FRenderer;
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
    property OnPlaceBookmark: TNotifyEvent read FOnPlaceBookmark
      write FOnPlaceBookmark;
    property OnClearBookmark: TNotifyEvent read FOnClearBookmark
      write FOnClearBookmark;
  end;

  TPlugInHandler = (phLinesInserted, phLinesDeleted, phLinePut, phAfterPaint);
  TPlugInHandlers = set of TPlugInHandler;

  TSynFMXEditPlugin = class(TObject)
  private
    FOwner: TCustomFMXSynEdit;
  protected
    FHandlers: TPlugInHandlers;
  public
    constructor Create(AOwner: TCustomFMXSynEdit;
      AHandlers: TPlugInHandlers = []); virtual;
    destructor Destroy; override;
    procedure AfterPaint(Canvas: TCanvas; const AClip: TRectF;
      FirstLine, LastLine: Integer); virtual;
    procedure LinesInserted(FirstLine, Count: Integer); virtual;
    procedure LinesDeleted(FirstLine, Count: Integer); virtual;
    procedure LinePut(aIndex: Integer; const OldLine: string); virtual;
    property Owner: TCustomFMXSynEdit read FOwner;
    property Handlers: TPlugInHandlers read FHandlers;
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
    property Gutter;
    property UseCodeFolding;
    property WordWrap;
    property SearchEngine;
    property OnChange;
    property OnStatusChange;
    property OnReplaceText;
    property OnSearchNotFound;
    property OnScanForFoldRanges;
    property OnPlaceBookmark;
    property OnClearBookmark;
  end;

implementation

uses
  FMX.SynEditRenderer,
  FMX.SynEditScrollBars,
  FMX.SynEditTypes,
  FMX.SynEditUndo,
  FMX.SynUnicode,
  SynEditKeyConst,
  SynEditStrConst,
  SynEditSearch;

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
  FSelection := TSynSelection.Create(BufferCoord(1, 1), BufferCoord(1, 1),
    BufferCoord(1, 1));
  FSelection.LastPosX := -1;
  FTopLine := 1;
  FLeftChar := 1;
  FRightEdge := 80;
  FRightEdgeColor := clSilver;
  FActiveLineColor := clNone;
  FOptions := SYNEDIT_DEFAULT_OPTIONS;
  FScrollOptions := SYNEDIT_DEFAULT_SCROLLOPTIONS;
  FSelectedColor := TSynSelectedColor.Create;

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

  // Keyboard handler chain
  FKbdHandler := TSynEditKbdHandler.Create;

  // Bookmarks
  FMarkList := TSynFMXEditMarkList.Create;

  // Multi-selection
  FSelections := TSynFMXSelections.Create(Self);
  FSelections.AddCaret(FSelection.Caret, True);

  // Gutter
  FGutter := TSynFMXGutter.Create(Self);
  FGutter.OnChange := GutterChanged;

  // Plugins
  FPlugins := TList.Create;

  UpdateGutterWidth;
end;

destructor TCustomFMXSynEdit.Destroy;
begin
  FWordWrapHelper.Free;
  FPlugins.Free;
  FGutter.Free;
  FSelections.Free;
  FMarkList.Free;
  FKbdHandler.Free;
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
var
  SB: ISynEditScrollBars;
  HScrollHeight, VScrollWidth: Single;
  NewHScrollHeight, NewVScrollWidth: Single;
begin
  if (Width <= 0) or (Height <= 0) then Exit;
  UpdateGutterWidth;
  FTextAreaLeft := FGutterWidth;
  // Use actual scrollbar sizes (0 when hidden)
  if (FScrollBars <> nil) and Supports(FScrollBars, ISynEditScrollBars, SB) then
  begin
    HScrollHeight := SB.GetVisibleHScrollBarHeight;
    VScrollWidth := SB.GetVisibleVScrollBarWidth;
  end
  else
  begin
    HScrollHeight := 0;
    VScrollWidth := 0;
  end;
  if FLineHeight > 0 then
    FLinesInWindow := Max(1, Trunc((Height - HScrollHeight) / FLineHeight))
  else
    FLinesInWindow := 1;
  if FCharWidth > 0 then
    FCharsInWindow := Max(1, Trunc((Width - FGutterWidth - VScrollWidth) / FCharWidth))
  else
    FCharsInWindow := 1;
  UpdateScrollBars;
  // Recalculate if scrollbar visibility changed after UpdateScrollBars
  if (FScrollBars <> nil) and Supports(FScrollBars, ISynEditScrollBars, SB) then
  begin
    NewHScrollHeight := SB.GetVisibleHScrollBarHeight;
    NewVScrollWidth := SB.GetVisibleVScrollBarWidth;
    if (NewHScrollHeight <> HScrollHeight) or (NewVScrollWidth <> VScrollWidth) then
    begin
      if FLineHeight > 0 then
        FLinesInWindow := Max(1, Trunc((Height - NewHScrollHeight) / FLineHeight));
      if FCharWidth > 0 then
        FCharsInWindow := Max(1, Trunc((Width - FGutterWidth - NewVScrollWidth) / FCharWidth));
    end;
  end;
  // Re-wrap if word wrap width changed
  if FWordWrap and Assigned(FWordWrapHelper) then
  begin
    var NewWrapWidth := GetWrapAreaWidth;
    if NewWrapWidth <> FWordWrapHelper.MaxCharsPerRow then
    begin
      FWordWrapHelper.SetWrapWidth(NewWrapWidth, FTabWidth);
      FWordWrapHelper.Reset(FLines);
    end;
  end;
end;

procedure TCustomFMXSynEdit.UpdateGutterWidth;
begin
  FGutterWidth := FGutter.RealGutterWidth;
  FTextAreaLeft := FGutterWidth;
end;

procedure TCustomFMXSynEdit.GutterChanged(Sender: TObject);
var
  OldWidth: Single;
begin
  OldWidth := FGutterWidth;
  UpdateGutterWidth;
  if FGutterWidth <> OldWidth then
    RecalcSizes;
  Repaint;
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
  if FWordWrap then Value := 1;
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
  DC: TDisplayCoord;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
  begin
    DC := FWordWrapHelper.BufferToDisplayPos(BufferCoord(FSelection.Caret.Char, FSelection.Caret.Line));
    CaretRow := DC.Row;
    if CaretRow < FTopLine then
      TopLine := CaretRow
    else if CaretRow >= FTopLine + FLinesInWindow then
      TopLine := CaretRow - FLinesInWindow + 1;
    // No horizontal scrolling in word wrap mode
  end
  else
  begin
    // Vertical - use display rows when code folding is active
    CaretRow := LineToRow(FSelection.Caret.Line);
    if CaretRow < FTopLine then
      TopLine := CaretRow
    else if CaretRow >= FTopLine + FLinesInWindow then
      TopLine := CaretRow - FLinesInWindow + 1;
    // Horizontal
    if FSelection.Caret.Char < FLeftChar then
      LeftChar := FSelection.Caret.Char
    else if FSelection.Caret.Char >= FLeftChar + FCharsInWindow then
      LeftChar := FSelection.Caret.Char - FCharsInWindow + 1;
  end;
end;

function TCustomFMXSynEdit.GetMaxScrollWidth: Integer;
var
  I, Len: Integer;
begin
  if not FMaxScrollWidthValid then
  begin
    FMaxScrollWidth := 1;
    for I := 0 to FLines.Count - 1 do
    begin
      Len := Length(ExpandTabs(FLines[I], FTabWidth));
      if Len > FMaxScrollWidth then
        FMaxScrollWidth := Len;
    end;
    Inc(FMaxScrollWidth); // +1 for caret past end
    FMaxScrollWidthValid := True;
  end;
  Result := Max(FMaxScrollWidth, FCharsInWindow + 1);
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
var
  DC: TDisplayCoord;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
  begin
    DC := FWordWrapHelper.BufferToDisplayPos(BC);
    Result.X := FTextAreaLeft + (DC.Column - 1) * FCharWidth;
    Result.Y := (DC.Row - FTopLine) * FLineHeight;
  end
  else
  begin
    Result.X := FTextAreaLeft + (BC.Char - FLeftChar) * FCharWidth;
    Result.Y := (LineToRow(BC.Line) - FTopLine) * FLineHeight;
  end;
end;

function TCustomFMXSynEdit.PixelToBufferCoord(X, Y: Single): TBufferCoord;
var
  Row, Col: Integer;
  DC: TDisplayCoord;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
  begin
    Col := Max(1, 1 + Trunc((X - FTextAreaLeft) / FCharWidth));
    Row := Max(1, FTopLine + Trunc(Y / FLineHeight));
    DC.Column := Col;
    DC.Row := Row;
    Result := FWordWrapHelper.DisplayToBufferPos(DC);
    Result.Line := Max(1, Min(Result.Line, FLines.Count));
    Result.Char := Max(1, Result.Char);
  end
  else
  begin
    Result.Char := Max(1, FLeftChar + Trunc((X - FTextAreaLeft) / FCharWidth));
    Row := Max(1, FTopLine + Trunc(Y / FLineHeight));
    Result.Line := Max(1, Min(RowToLine(Row), FLines.Count));
  end;
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
  I: Integer;
  Band: TSynFMXGutterBand;
  BandR: TRectF;
  BandLeft, BandW: Single;
  GutterColor: TAlphaColor;
  SaveState: TCanvasSaveState;
begin
  if not FGutter.Visible then Exit;
  Renderer := TSynFMXRenderer(FRenderer);
  GutterColor := TColorToAlphaColor(FGutter.Color);

  // Paint gutter background for bands with gbbGutter background
  BandLeft := 0;
  for I := 0 to FGutter.Bands.Count - 1 do
  begin
    Band := FGutter.Bands[I];
    BandW := Band.RealWidth;
    if (BandW > 0) and (Band.Background = gbbGutter) then
    begin
      BandR := RectF(BandLeft, 0, BandLeft + BandW, Height);
      Renderer.FillRect(Canvas, BandR, GutterColor);
    end;
    BandLeft := BandLeft + BandW;
  end;

  // Paint each band
  BandLeft := 0;
  for I := 0 to FGutter.Bands.Count - 1 do
  begin
    Band := FGutter.Bands[I];
    BandW := Band.RealWidth;
    if BandW > 0 then
    begin
      BandR := RectF(BandLeft, 0, BandLeft + BandW, Height);
      SaveState := Canvas.SaveState;
      try
        Canvas.IntersectClipRect(BandR);
        Band.PaintLines(Canvas, BandR, FirstLine, LastLine);
      finally
        Canvas.RestoreState(SaveState);
      end;
    end;
    BandLeft := BandLeft + BandW;
  end;
end;

procedure TCustomFMXSynEdit.PaintTextLines(Canvas: TCanvas;
  FirstLine, LastLine: Integer);
var
  Renderer: TSynFMXRenderer;
  Row, Line, PrevLine: Integer;
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
  HasTabs: Boolean;
  ColMap: TArray<Integer>; // maps raw char index (0-based) to expanded column
  J, ExpandedCol: Integer;
  RawTokenPos, RawTokenLen: Integer;
  ExpandedTokenPos, ExpandedTokenLen: Integer;
  // Word wrap per-row variables
  IsWrapping: Boolean;
  EffLeftChar: Integer;     // effective left char for this row (1-based expanded)
  EffCharsInWin: Integer;   // effective chars visible in this row
  RowExpandedStart: Integer; // 0-based expanded column where this row starts
  RowExpandedEnd: Integer;   // 0-based expanded column where this row ends
  RowBufferStart: Integer;   // 1-based raw char where this row starts
  RowBufferLen: Integer;     // raw char count for this row
  WrapBC: TBufferCoord;
begin
  Renderer := TSynFMXRenderer(FRenderer);
  IsWrapping := FWordWrap and Assigned(FWordWrapHelper);
  PrevLine := -1;

  // Normalize selection
  SelBC1 := FSelection.Start;
  SelBC2 := FSelection.Stop;
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
    if (FActiveLineColor <> clNone) and FSelections.RowHasCaret(Row, Line) and
      (SelBC1 = SelBC2) then
    begin
      LineR := RectF(FTextAreaLeft, Y, Width, Y + FLineHeight);
      Renderer.FillRect(Canvas, LineR,
        TColorToAlphaColor(FActiveLineColor));
    end;

    if Line > FLines.Count then Continue;
    SLine := FLines[Line - 1];
    HasTabs := Pos(#9, SLine) > 0;
    SExpanded := ExpandTabs(SLine, FTabWidth);

    // Build raw-to-expanded column map (once per buffer line)
    if Line <> PrevLine then
    begin
      if HasTabs then
      begin
        SetLength(ColMap, Length(SLine) + 1); // index 0..Length(SLine)
        ExpandedCol := 0;
        for J := 0 to Length(SLine) - 1 do
        begin
          ColMap[J] := ExpandedCol;
          if SLine[J + 1] = #9 then
          begin
            repeat Inc(ExpandedCol) until (ExpandedCol mod FTabWidth) = 0;
          end
          else
            Inc(ExpandedCol);
        end;
        ColMap[Length(SLine)] := ExpandedCol; // past-end sentinel
      end;
    end;

    // Compute effective viewport for this row
    if IsWrapping then
    begin
      // Get the buffer position where this wrapped row starts
      WrapBC := FWordWrapHelper.DisplayToBufferPos(DisplayCoord(1, Row));
      RowBufferStart := WrapBC.Char;
      RowBufferLen := FWordWrapHelper.GetRowLength(Row);
      // Compute expanded column range
      if HasTabs then
      begin
        RowExpandedStart := ColMap[Min(RowBufferStart - 1, Length(SLine))];
        RowExpandedEnd := ColMap[Min(RowBufferStart - 1 + RowBufferLen, Length(SLine))];
      end
      else
      begin
        RowExpandedStart := RowBufferStart - 1;
        RowExpandedEnd := RowBufferStart - 1 + RowBufferLen;
      end;
      EffLeftChar := RowExpandedStart + 1; // 1-based
      EffCharsInWin := RowExpandedEnd - RowExpandedStart;
    end
    else
    begin
      RowExpandedStart := 0;
      RowExpandedEnd := Length(SExpanded);
      RowBufferStart := 1;
      RowBufferLen := Length(SLine);
      EffLeftChar := FLeftChar;
      EffCharsInWin := FCharsInWindow;
    end;

    // Calculate primary selection range for this line (for token foreground)
    SelStart := 0;
    SelEnd := 0;
    if (SelBC1 <> SelBC2) then
    begin
      if (Line > SelBC1.Line) and (Line < SelBC2.Line) then
      begin
        SelStart := 1;
        SelEnd := Length(SExpanded) + 1;
      end
      else if (Line = SelBC1.Line) and (Line = SelBC2.Line) then
      begin
        if HasTabs then
        begin
          SelStart := ColMap[Min(SelBC1.Char - 1, Length(SLine))] + 1;
          SelEnd := ColMap[Min(SelBC2.Char - 1, Length(SLine))] + 1;
        end
        else
        begin
          SelStart := SelBC1.Char;
          SelEnd := SelBC2.Char;
        end;
      end
      else if Line = SelBC1.Line then
      begin
        if HasTabs then
          SelStart := ColMap[Min(SelBC1.Char - 1, Length(SLine))] + 1
        else
          SelStart := SelBC1.Char;
        SelEnd := Length(SExpanded) + 1;
      end
      else if Line = SelBC2.Line then
      begin
        SelStart := 1;
        if HasTabs then
          SelEnd := ColMap[Min(SelBC2.Char - 1, Length(SLine))] + 1
        else
          SelEnd := SelBC2.Char;
      end;
    end;

    // Paint selection backgrounds for all selections (multi-caret)
    begin
      var RowStart := BufferCoord(1, Line);
      var RowEnd := BufferCoord(Length(SLine) + 1, Line);
      var PartSels := FSelections.PartSelectionsForRow(RowStart, RowEnd);
      for var PS in PartSels do
      begin
        var PSNorm := PS.Normalized;
        // Clip selection to current line boundaries
        var ClipStart, ClipEnd: Integer;
        if PSNorm.Start.Line < Line then
          ClipStart := 1
        else
          ClipStart := PSNorm.Start.Char;
        if PSNorm.Stop.Line > Line then
          ClipEnd := Length(SLine) + 1
        else
          ClipEnd := PSNorm.Stop.Char;
        var PSStart, PSEnd: Integer;
        if HasTabs then
        begin
          PSStart := ColMap[Min(ClipStart - 1, Length(SLine))] + 1;
          PSEnd := ColMap[Min(ClipEnd - 1, Length(SLine))] + 1;
        end
        else
        begin
          PSStart := ClipStart;
          PSEnd := ClipEnd;
        end;
        if PSStart <> PSEnd then
        begin
          var SelX1: Single := FTextAreaLeft +
            (Max(PSStart, EffLeftChar) - EffLeftChar) * FCharWidth;
          var SelX2: Single := FTextAreaLeft +
            (Min(PSEnd, EffLeftChar + EffCharsInWin) - EffLeftChar) * FCharWidth;
          // Extend selection to right edge for fully-selected lines
          if FSelectedColor.FillWholeLines and
            (PSEnd > Length(SExpanded)) then
            SelX2 := Width;
          if SelX2 > SelX1 then
          begin
            LineR := RectF(SelX1, Y, SelX2, Y + FLineHeight);
            Renderer.FillRect(Canvas, LineR,
              TColorToAlphaColor(FSelectedColor.Background));
          end;
        end;
      end;
    end;

    // Paint tokens with highlighter
    if (FHighlighter <> nil) and (SLine <> '') then
    begin
      // Only re-initialize highlighter when line changes
      if Line <> PrevLine then
      begin
        if Line > 1 then
          FHighlighter.SetRange(TSynEditStringList(FLines).Ranges[Line - 2])
        else
          FHighlighter.ResetRange;
        FHighlighter.SetLine(SLine, Line);
      end;

      while not FHighlighter.GetEol do
      begin
        RawTokenPos := FHighlighter.GetTokenPos; // 0-based raw
        Token := FHighlighter.GetToken;
        RawTokenLen := Length(Token);
        Attr := FHighlighter.GetTokenAttribute;

        // Convert raw positions to expanded visual columns
        if HasTabs then
        begin
          ExpandedTokenPos := ColMap[RawTokenPos];
          ExpandedTokenLen := ColMap[Min(RawTokenPos + RawTokenLen, Length(SLine))]
            - ExpandedTokenPos;
          // Replace token text with expanded version for rendering
          Token := Copy(SExpanded, ExpandedTokenPos + 1, ExpandedTokenLen);
        end
        else
        begin
          ExpandedTokenPos := RawTokenPos;
          ExpandedTokenLen := RawTokenLen;
        end;
        TokenPos := ExpandedTokenPos; // now in visual columns (0-based)

        // Skip tokens entirely before visible area
        if TokenPos + ExpandedTokenLen < EffLeftChar - 1 then
        begin
          FHighlighter.Next;
          Continue;
        end;
        // Stop if past visible area
        if TokenPos >= EffLeftChar + EffCharsInWin - 1 then
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
        X := FTextAreaLeft + (TokenPos + 1 - EffLeftChar) * FCharWidth;
        // Clip to visible area
        if X < FTextAreaLeft then
        begin
          var Skip := Trunc((FTextAreaLeft - X) / FCharWidth);
          Token := Copy(Token, Skip + 1, MaxInt);
          TokenPos := TokenPos + Skip;
          X := FTextAreaLeft + (TokenPos + 1 - EffLeftChar) * FCharWidth;
        end;

        if (Token <> '') and (X < Width) then
        begin
          var TokStart := TokenPos + 1; // 1-based expanded
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
      var VisText := Copy(SExpanded, EffLeftChar, EffCharsInWin + 1);
      var VisStart := EffLeftChar; // 1-based position of first visible char
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

    PrevLine := Line;
  end;
end;

procedure TCustomFMXSynEdit.PaintCaret(Canvas: TCanvas);
var
  Renderer: TSynFMXRenderer;
  Pt: TPointF;
  R: TRectF;
  I: Integer;
begin
  Renderer := TSynFMXRenderer(FRenderer);
  for I := 0 to FSelections.Count - 1 do
  begin
    Pt := BufferCoordToPixel(FSelections[I].Caret);
    if (Pt.X >= FTextAreaLeft) and (Pt.X < Width) and
      (Pt.Y >= 0) and (Pt.Y < Height) then
    begin
      if FInsertMode then
        R := RectF(Pt.X, Pt.Y, Pt.X + 2, Pt.Y + FLineHeight)
      else
        R := RectF(Pt.X, Pt.Y, Pt.X + FCharWidth, Pt.Y + FLineHeight);
      Renderer.FillRect(Canvas, R, TAlphaColors.Black);
    end;
  end;
end;

{ --- Lines change notification --- }

procedure TCustomFMXSynEdit.LinesChanged(Sender: TObject);
begin
  FMaxScrollWidthValid := False;
  if FWordWrap and Assigned(FWordWrapHelper) then
    FWordWrapHelper.Reset(FLines);
  if FUseCodeFolding then
    FAllFoldRanges.StopScanning(FLines);
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

  // Dispatch to keyboard handler chain (plugins, completion proposal, etc.)
  FKbdHandler.ExecuteKeyDown(Self, Key, KeyChar, Shift);
  if (Key = 0) and (KeyChar = #0) then
    Exit;

  // Map key to command
  Cmd := ecNone;

  // Character input
  if (KeyChar >= #32) and (Shift * [ssCtrl, ssAlt] = []) then
  begin
    CommandProcessor(ecChar, KeyChar);
    KeyChar := #0;
    Exit;
  end;

  // Navigation and editing keys
  case Key of
    vkLeft:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelWordLeft, ecWordLeft)
      else if Shift * [ssAlt, ssShift] = [ssAlt, ssShift] then
        Cmd := ecSelColumnLeft
      else
        Cmd := IfThen(ssShift in Shift, ecSelLeft, ecLeft);
    vkRight:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelWordRight, ecWordRight)
      else if Shift * [ssAlt, ssShift] = [ssAlt, ssShift] then
        Cmd := ecSelColumnRight
      else
        Cmd := IfThen(ssShift in Shift, ecSelRight, ecRight);
    vkUp:
      if Shift * [ssAlt, ssShift] = [ssAlt, ssShift] then
        Cmd := ecSelColumnUp
      else
        Cmd := IfThen(ssShift in Shift, ecSelUp, ecUp);
    vkDown:
      if Shift * [ssAlt, ssShift] = [ssAlt, ssShift] then
        Cmd := ecSelColumnDown
      else
        Cmd := IfThen(ssShift in Shift, ecSelDown, ecDown);
    vkHome:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelEditorTop, ecEditorTop)
      else
        Cmd := IfThen(ssShift in Shift, ecSelLineStart, ecLineStart);
    vkEnd:
      if ssCtrl in Shift then
        Cmd := IfThen(ssShift in Shift, ecSelEditorBottom, ecEditorBottom)
      else if ssAlt in Shift then
        Cmd := ecCaretsAtLineEnds
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
    vkEscape:
      Cmd := ecCancelSelections;
    Ord('A'):
      if ssCtrl in Shift then Cmd := ecSelectAll;
    Ord('C'):
      if ssCtrl in Shift then Cmd := ecCopy;
    Ord('V'):
      if ssCtrl in Shift then Cmd := ecPaste;
    Ord('W'):
      if Shift * [ssCtrl, ssShift] = [ssCtrl, ssShift] then
        Cmd := ecSelMatchingText;
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
    Ord('0')..Ord('9'):
      // Bookmarks: Ctrl+N = goto, Ctrl+Shift+N = set/toggle
      if ssCtrl in Shift then
      begin
        if ssShift in Shift then
          Cmd := ecSetMarker0 + Key - Ord('0')
        else
          Cmd := ecGotoMarker0 + Key - Ord('0');
      end;
  end;

  if Cmd <> ecNone then
  begin
    CommandProcessor(Cmd, #0);
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

function TCustomFMXSynEdit.ColumnSelectionStart: TBufferCoord;
begin
  if FSelections.BaseSelection.IsEmpty then
    Result := FSelections.BaseSelection.Caret
  else
    Result := FSelections.BaseSelection.Start;
end;

procedure TCustomFMXSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: WideChar);
var
  CommandInfo: TSynCommandInfo;
begin
  if (Command <> ecNone) and (Command < ecUserFirst) then
  begin
    if not SynCommandsInfo.TryGetValue(Command, CommandInfo)
      or (CommandInfo.CommandKind in [ckStandard, ckSingleCaret])
      or (FSelections.Count = 1)
    then
    begin
      if SynCommandsInfo.TryGetValue(Command, CommandInfo)
        and (CommandInfo.CommandKind = ckSingleCaret) and (FSelections.Count > 1) then
        FSelections.Clear(TSynSelectionsBase.TKeepSelection.ksKeepBase);
      ExecuteCommand(Command, AChar);
    end
    else
      ExecuteMultiCaretCommand(Command, AChar);
  end;
end;

procedure TCustomFMXSynEdit.ExecuteMultiCaretCommand(
  Command: TSynEditorCommand; AChar: WideChar);
var
  OldActiveSelIndex: Integer;
  I: Integer;
  OldTopLine, OldLeftChar: Integer;
begin
  BeginUpdate;
  try
    FUndoRedo.BeginBlock(Self);
    try
      OldActiveSelIndex := FSelections.ActiveSelIndex;
      OldLeftChar := LeftChar;
      OldTopLine := TopLine;

      for I := 0 to FSelections.Count - 1 do
      begin
        FSelections.ActiveSelIndex := I;
        FSelection := FSelections.ActiveSelection;

        if not FSelection.IsValid then Continue;

        ExecuteCommand(Command, AChar);
        FSelections.ActiveSelection := FSelection;
      end;

      // Restore Active Selection
      if OldActiveSelIndex < FSelections.Count then
        FSelections.ActiveSelIndex := OldActiveSelIndex
      else
        FSelections.ActiveSelIndex := FSelections.Count - 1;
      FSelection := FSelections.ActiveSelection;

      // Merge overlapping selections
      FSelections.Merge;
      FSelection := FSelections.ActiveSelection;

      TopLine := OldTopLine;
      LeftChar := OldLeftChar;

      EnsureCursorPosVisible;
    finally
      FUndoRedo.EndBlock(Self);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomFMXSynEdit.SelectAllMatchingText;
var
  Engine: TSynEditSearchCustom;
  SearchOptions: TSynSearchOptions;
  Line: Integer;
  ResNo: Integer;
  SelList: TList<TSynSelection>;
  Sel: TSynSelection;
  SelStorage: TSynSelStorage;
  LineText: string;
begin
  if FSelection.IsEmpty then Exit;
  if FSelection.Start.Line <> FSelection.Stop.Line then Exit;

  Engine := TSynEditSearch.Create(Self);
  SelList := TList<TSynSelection>.Create;
  try
    SelStorage.BaseIndex := 0;
    SelStorage.ActiveIndex := 0;

    Engine.Pattern := GetTextRange(FSelection.Normalized.Start,
      FSelection.Normalized.Stop);
    SearchOptions := [ssoMatchCase];
    Engine.Options := SearchOptions;

    for Line := 0 to FLines.Count - 1 do
    begin
      LineText := FLines[Line];
      if LineText.Length < Engine.Pattern.Length then Continue;

      Engine.FindAll(LineText);
      for ResNo := 0 to Engine.ResultCount - 1 do
      begin
        Sel.Start := BufferCoord(Engine.Results[ResNo], Line + 1);
        Sel.Stop := BufferCoord(Sel.Start.Char + Engine.Lengths[ResNo],
          Line + 1);
        Sel.Caret := Sel.Stop;
        Sel.CaretAtEOL := False;
        Sel.LastPosX := 0;
        SelList.Add(Sel);

        // Track which match is the current selection
        if (Sel.Start = FSelection.Normalized.Start) and
          (Sel.Stop = FSelection.Normalized.Stop) then
        begin
          SelStorage.BaseIndex := SelList.Count - 1;
          SelStorage.ActiveIndex := SelList.Count - 1;
        end;
      end;
    end;

    if SelList.Count = 0 then Exit;

    SelStorage.Selections := SelList.ToArray;
    FSelections.Restore(SelStorage);
    FSelection := FSelections.ActiveSelection;
    Repaint;
  finally
    SelList.Free;
    Engine.Free;
  end;
end;

procedure TCustomFMXSynEdit.CaretsAtLineEnds;
var
  SelList: TList<TSynSelection>;
  Sel: TSynSelection;
  Line: Integer;
  LineText: string;
  SelStorage: TSynSelStorage;
begin
  FSelections.Clear;

  SelList := TList<TSynSelection>.Create;
  try
    for Line := BlockBegin.Line to BlockEnd.Line do
    begin
      if (Line < 1) or (Line > FLines.Count) then Continue;
      LineText := FLines[Line - 1];
      Sel.Caret := BufferCoord(LineText.Length + 1, Line);
      Sel.Start := Sel.Caret;
      Sel.Stop := Sel.Caret;
      Sel.CaretAtEOL := False;
      Sel.LastPosX := 0;
      SelList.Add(Sel);
    end;

    if SelList.Count = 0 then Exit;

    SelStorage.Selections := SelList.ToArray;
    SelStorage.BaseIndex := SelList.Count - 1;
    SelStorage.ActiveIndex := SelList.Count - 1;
    FSelections.Restore(SelStorage);
    FSelection := FSelections.ActiveSelection;
    Repaint;
  finally
    SelList.Free;
  end;
end;

procedure TCustomFMXSynEdit.ExecuteCommand(Command: TSynEditorCommand;
  AChar: WideChar);
var
  FirstAffectedLine: Integer;
begin
  FirstAffectedLine := -1;

  if FUndoRedo <> nil then
    FUndoRedo.CommandProcessed := Command;

  case Command of
    // Navigation (including column selection)
    ecLeft, ecSelLeft, ecSelColumnLeft:
      begin
        if not FSelection.IsEmpty and (Command = ecLeft) then
          CaretXY := FSelection.Normalized.Start
        else
        begin
          var Anchor := ColumnSelectionStart;
          MoveCaretHorz(-1, Command = ecSelLeft);
          if Command = ecSelColumnLeft then
            FSelections.ColumnSelection(Anchor, CaretXY, FSelection.LastPosX);
        end;
      end;
    ecRight, ecSelRight, ecSelColumnRight:
      begin
        if not FSelection.IsEmpty and (Command = ecRight) then
          CaretXY := FSelection.Normalized.Stop
        else
        begin
          var Anchor := ColumnSelectionStart;
          MoveCaretHorz(1, Command = ecSelRight);
          if Command = ecSelColumnRight then
            FSelections.ColumnSelection(Anchor, CaretXY, FSelection.LastPosX);
        end;
      end;
    ecUp, ecSelUp, ecSelColumnUp:
      begin
        var Anchor := ColumnSelectionStart;
        MoveCaretVert(-1, Command = ecSelUp);
        if Command = ecSelColumnUp then
          FSelections.ColumnSelection(Anchor, CaretXY, FSelection.LastPosX);
      end;
    ecDown, ecSelDown, ecSelColumnDown:
      begin
        var Anchor := ColumnSelectionStart;
        MoveCaretVert(1, Command = ecSelDown);
        if Command = ecSelColumnDown then
          FSelections.ColumnSelection(Anchor, CaretXY, FSelection.LastPosX);
      end;
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
      MoveCaretAndSelection(BufferCoord(1, FSelection.Caret.Line), False);
    ecSelLineStart:
      MoveCaretAndSelection(BufferCoord(1, FSelection.Caret.Line), True);
    ecLineEnd:
      begin
        var LineLen := 0;
        if (FSelection.Caret.Line >= 1) and (FSelection.Caret.Line <= FLines.Count) then
          LineLen := Length(FLines[FSelection.Caret.Line - 1]);
        MoveCaretAndSelection(BufferCoord(LineLen + 1, FSelection.Caret.Line), False);
      end;
    ecSelLineEnd:
      begin
        var LineLen := 0;
        if (FSelection.Caret.Line >= 1) and (FSelection.Caret.Line <= FLines.Count) then
          LineLen := Length(FLines[FSelection.Caret.Line - 1]);
        MoveCaretAndSelection(BufferCoord(LineLen + 1, FSelection.Caret.Line), True);
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
        FirstAffectedLine := FSelection.Caret.Line - 1;
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
        FirstAffectedLine := FSelection.Caret.Line - 1;
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
        FirstAffectedLine := FSelection.Caret.Line - 1;
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
        FirstAffectedLine := FSelection.Caret.Line - 1;
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
        FirstAffectedLine := FSelection.Caret.Line - 1;
        if eoTabsToSpaces in FOptions then
        begin
          var Spaces := FTabWidth - ((FSelection.Caret.Char - 1) mod FTabWidth);
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
        FirstAffectedLine := FSelection.Caret.Line - 1;
        if (FSelection.Caret.Line >= 1) and (FSelection.Caret.Line <= FLines.Count) then
        begin
          var Line := FLines[FSelection.Caret.Line - 1];
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
              FLines[FSelection.Caret.Line - 1] := Copy(Line, SpacesToRemove + 1);
              // Adjust caret
              SetCaretX(Max(1, FSelection.Caret.Char - SpacesToRemove));
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
        FirstAffectedLine := FSelection.Caret.Line - 1;
        CutToClipboard;
      end;
    ecPaste:
      begin
        FirstAffectedLine := FSelection.Caret.Line - 1;
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

    // Bookmarks
    ecGotoMarker0..ecGotoMarker9:
      GotoBookmark(Command - ecGotoMarker0);
    ecSetMarker0..ecSetMarker9:
      begin
        var BmIdx := Command - ecSetMarker0;
        if IsBookmarkSet(BmIdx) then
        begin
          var BX, BY: Integer;
          GetBookmark(BmIdx, BX, BY);
          if BY = FSelection.Caret.Line then
            ClearBookmark(BmIdx)
          else
            SetBookmark(BmIdx, FSelection.Caret.Char, FSelection.Caret.Line);
        end
        else
          SetBookmark(BmIdx, FSelection.Caret.Char, FSelection.Caret.Line);
      end;

    // Multi-caret commands
    ecCancelSelections:
      begin
        if FSelections.Count = 1 then
          CaretXY := CaretXY  // collapses selection
        else
          FSelections.Clear(TSynSelectionsBase.TKeepSelection.ksKeepBase);
      end;
    ecSelMatchingText:
      SelectAllMatchingText;
    ecCaretsAtLineEnds:
      CaretsAtLineEnds;
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

  FSelection.LastPosX := -1;
  MoveCaretAndSelection(NewCaret, SelectionCmd);
end;

procedure TCustomFMXSynEdit.MoveCaretVert(DY: Integer; SelectionCmd: Boolean);
var
  NewCaret: TBufferCoord;
  LineLen: Integer;
  DC: TDisplayCoord;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
  begin
    // In word wrap mode, move between display rows
    DC := FWordWrapHelper.BufferToDisplayPos(GetCaretXY);
    // Sticky column tracks display column
    if (eoKeepCaretX in FOptions) and (FSelection.LastPosX >= 0) then
      DC.Column := FSelection.LastPosX
    else
      FSelection.LastPosX := DC.Column;
    Inc(DC.Row, DY);
    DC.Row := Max(1, Min(DC.Row, FWordWrapHelper.RowCount));
    NewCaret := FWordWrapHelper.DisplayToBufferPos(DC);
    NewCaret.Line := Max(1, Min(NewCaret.Line, Max(1, FLines.Count)));
    NewCaret.Char := Max(1, NewCaret.Char);
    // Clamp to line length
    if (NewCaret.Line >= 1) and (NewCaret.Line <= FLines.Count) then
    begin
      LineLen := Length(FLines[NewCaret.Line - 1]);
      if not (eoScrollPastEol in FScrollOptions) then
        NewCaret.Char := Min(NewCaret.Char, LineLen + 1);
    end;
    MoveCaretAndSelection(NewCaret, SelectionCmd);
  end
  else
  begin
    NewCaret := GetCaretXY;
    Inc(NewCaret.Line, DY);
    NewCaret.Line := Max(1, Min(NewCaret.Line, Max(1, FLines.Count)));

    // Sticky column
    if (eoKeepCaretX in FOptions) and (FSelection.LastPosX >= 0) then
      NewCaret.Char := FSelection.LastPosX
    else
      FSelection.LastPosX := NewCaret.Char;

    // Clamp to line length
    if (NewCaret.Line >= 1) and (NewCaret.Line <= FLines.Count) then
    begin
      LineLen := Length(FLines[NewCaret.Line - 1]);
      if not (eoScrollPastEol in FScrollOptions) then
        NewCaret.Char := Min(NewCaret.Char, LineLen + 1);
    end;

    MoveCaretAndSelection(NewCaret, SelectionCmd);
  end;
end;

procedure TCustomFMXSynEdit.MoveCaretAndSelection(const NewCaret: TBufferCoord;
  SelectionCmd: Boolean);
begin
  if SelectionCmd then
  begin
    // If no selection exists yet, start selection from current caret
    if FSelection.Start = FSelection.Stop then
      FSelection.Start := GetCaretXY;
    FSelection.Stop := NewCaret;
  end
  else
  begin
    // Clear selection
    FSelection.Start := NewCaret;
    FSelection.Stop := NewCaret;
  end;

  FSelection.Caret.Char := NewCaret.Char;
  FSelection.Caret.Line := NewCaret.Line;
  EnsureCursorPosVisible;
  Repaint;
end;

{ --- Editing operations --- }

procedure TCustomFMXSynEdit.InsertCharAtCursor(AChar: WideChar);
var
  SLine: string;
begin
  // Ensure we have enough lines
  while FLines.Count < FSelection.Caret.Line do
    FLines.Add('');

  SLine := FLines[FSelection.Caret.Line - 1];

  // Pad line if caret is past end
  while Length(SLine) < FSelection.Caret.Char - 1 do
    SLine := SLine + ' ';

  if FInsertMode then
    System.Insert(AChar, SLine, FSelection.Caret.Char)
  else
  begin
    if FSelection.Caret.Char <= Length(SLine) then
      SLine[FSelection.Caret.Char] := AChar
    else
      SLine := SLine + AChar;
  end;

  FLines[FSelection.Caret.Line - 1] := SLine;
  Inc(FSelection.Caret.Char);
  FSelection.LastPosX := -1;
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.DoDeleteChar;
var
  SLine: string;
begin
  if (FSelection.Caret.Line < 1) or (FSelection.Caret.Line > FLines.Count) then Exit;
  SLine := FLines[FSelection.Caret.Line - 1];

  if FSelection.Caret.Char <= Length(SLine) then
  begin
    System.Delete(SLine, FSelection.Caret.Char, 1);
    FLines[FSelection.Caret.Line - 1] := SLine;
  end
  else if FSelection.Caret.Line < FLines.Count then
  begin
    // Join with next line
    FLines[FSelection.Caret.Line - 1] := SLine + FLines[FSelection.Caret.Line];
    FLines.Delete(FSelection.Caret.Line);
  end;
end;

procedure TCustomFMXSynEdit.DoDeleteLastChar;
var
  SLine: string;
begin
  if (FSelection.Caret.Char > 1) then
  begin
    if (FSelection.Caret.Line >= 1) and (FSelection.Caret.Line <= FLines.Count) then
    begin
      SLine := FLines[FSelection.Caret.Line - 1];
      if FSelection.Caret.Char - 1 <= Length(SLine) then
      begin
        System.Delete(SLine, FSelection.Caret.Char - 1, 1);
        FLines[FSelection.Caret.Line - 1] := SLine;
      end;
      Dec(FSelection.Caret.Char);
    end;
  end
  else if FSelection.Caret.Line > 1 then
  begin
    // Join with previous line
    var PrevLen := Length(FLines[FSelection.Caret.Line - 2]);
    FLines[FSelection.Caret.Line - 2] := FLines[FSelection.Caret.Line - 2] + FLines[FSelection.Caret.Line - 1];
    FLines.Delete(FSelection.Caret.Line - 1);
    Dec(FSelection.Caret.Line);
    FSelection.Caret.Char := PrevLen + 1;
  end;
  FSelection.LastPosX := -1;
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.DoInsertLine;
var
  SLine, LeftPart, RightPart, Indent: string;
begin
  if (FSelection.Caret.Line < 1) then Exit;

  while FLines.Count < FSelection.Caret.Line do
    FLines.Add('');

  SLine := FLines[FSelection.Caret.Line - 1];
  LeftPart := Copy(SLine, 1, FSelection.Caret.Char - 1);
  RightPart := Copy(SLine, FSelection.Caret.Char, MaxInt);

  // Auto-indent: copy leading whitespace (spaces and tabs)
  Indent := '';
  if eoAutoIndent in FOptions then
  begin
    var I := 1;
    while (I <= Length(SLine)) and CharInSet(SLine[I], [' ', #9]) do
      Inc(I);
    Indent := Copy(SLine, 1, I - 1);
  end;

  FLines[FSelection.Caret.Line - 1] := LeftPart;
  FLines.Insert(FSelection.Caret.Line, Indent + RightPart);
  Inc(FSelection.Caret.Line);
  FSelection.Caret.Char := Length(Indent) + 1;
  FSelection.LastPosX := -1;
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.DoDeleteSelection;
var
  SelBC1, SelBC2: TBufferCoord;
  FirstLine, LastLine: string;
begin
  if not GetSelAvail then Exit;

  SelBC1 := FSelection.Start;
  SelBC2 := FSelection.Stop;
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

  FSelection.Caret.Char := SelBC1.Char;
  FSelection.Caret.Line := SelBC1.Line;
  FSelection.Start := BufferCoord(FSelection.Caret.Char, FSelection.Caret.Line);
  FSelection.Stop := FSelection.Start;
  FSelection.LastPosX := -1;
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
      while FLines.Count < FSelection.Caret.Line do
        FLines.Add('');
      var SLine := FLines[FSelection.Caret.Line - 1];
      System.Insert(Value, SLine, FSelection.Caret.Char);
      FLines[FSelection.Caret.Line - 1] := SLine;
      Inc(FSelection.Caret.Char, Length(Value));
    end
    else
    begin
      // Multi-line insert
      while FLines.Count < FSelection.Caret.Line do
        FLines.Add('');
      var SLine := FLines[FSelection.Caret.Line - 1];
      var LeftPart := Copy(SLine, 1, FSelection.Caret.Char - 1);
      var RightPart := Copy(SLine, FSelection.Caret.Char, MaxInt);

      FLines[FSelection.Caret.Line - 1] := LeftPart + Lines[0];
      for var I := 1 to Length(Lines) - 1 do
        FLines.Insert(FSelection.Caret.Line - 1 + I, Lines[I]);
      // Append right part to last line
      var LastIdx := FSelection.Caret.Line - 1 + Length(Lines) - 1;
      FLines[LastIdx] := FLines[LastIdx] + RightPart;
      FSelection.Caret.Line := LastIdx + 1;
      FSelection.Caret.Char := Length(Lines[Length(Lines) - 1]) + 1;
    end;

    FSelection.Start := BufferCoord(FSelection.Caret.Char, FSelection.Caret.Line);
    FSelection.Stop := FSelection.Start;
    FSelection.LastPosX := -1;
    EnsureCursorPosVisible;
  finally
    FUndoRedo.EndBlock(Self);
  end;
end;

{ --- Selection --- }

function TCustomFMXSynEdit.GetSelAvail: Boolean;
begin
  Result := FSelection.Start <> FSelection.Stop;
end;

function TCustomFMXSynEdit.GetSelText: string;
var
  SelBC1, SelBC2: TBufferCoord;
begin
  Result := '';
  if not GetSelAvail then Exit;

  SelBC1 := FSelection.Start;
  SelBC2 := FSelection.Stop;
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
    FSelection.Start := BufferCoord(1, 1);
    var LastLine := FLines.Count;
    FSelection.Stop := BufferCoord(Length(FLines[LastLine - 1]) + 1, LastLine);
    FSelection.Caret.Char := FSelection.Stop.Char;
    FSelection.Caret.Line := FSelection.Stop.Line;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.ClearSelection;
begin
  FSelection.Start := GetCaretXY;
  FSelection.Stop := FSelection.Start;
  Repaint;
end;

procedure TCustomFMXSynEdit.SetBlockBegin(Value: TBufferCoord);
begin
  Value.Line := Max(Value.Line, 1);
  Value.Char := Max(Value.Char, 1);
  if (FSelection.Start.Char <> Value.Char) or (FSelection.Start.Line <> Value.Line) then
  begin
    FSelection.Start := Value;
    FSelection.Stop := Value;
    FSelections.ActiveSelection := FSelection;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetBlockEnd(Value: TBufferCoord);
begin
  Value.Line := Max(Value.Line, 1);
  Value.Char := Max(Value.Char, 1);
  if (FSelection.Stop.Char <> Value.Char) or (FSelection.Stop.Line <> Value.Line) then
  begin
    FSelection.Stop := Value;
    FSelections.ActiveSelection := FSelection;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetCaretAndSelection(const ACaretXY, ABlockBegin,
  ABlockEnd: TBufferCoord);
begin
  FSelection.Start := ABlockBegin;
  FSelection.Stop := ABlockEnd;
  FSelection.Caret.Char := ACaretXY.Char;
  FSelection.Caret.Line := ACaretXY.Line;
  FSelections.ActiveSelection := FSelection;
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
    FSelection.Caret.Char := 1;
    FSelection.Caret.Line := 1;
    FSelection.Start := BufferCoord(1, 1);
    FSelection.Stop := BufferCoord(1, 1);
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
    FSelection.Caret.Char := 1;
    FSelection.Caret.Line := 1;
    FSelection.Start := BufferCoord(1, 1);
    FSelection.Stop := BufferCoord(1, 1);
    // Clear all bookmarks
    for var I := 0 to 9 do
      FBookmarks[I] := nil;
    FMarkList.Clear;
  finally
    EndUpdate;
  end;
end;

{ --- Bookmarks --- }

procedure TCustomFMXSynEdit.SetBookmark(ABookmark: Integer; X, Y: Integer);
var
  Mark: TSynFMXEditMark;
begin
  if (ABookmark < 0) or (ABookmark > 9) then Exit;
  // Clamp line to valid range
  Y := Max(1, Min(Y, Max(1, FLines.Count)));
  X := Max(1, X);

  if FBookmarks[ABookmark] <> nil then
  begin
    // Update existing bookmark position
    FBookmarks[ABookmark].Line := Y;
    FBookmarks[ABookmark].Char := X;
  end
  else
  begin
    Mark := TSynFMXEditMark.Create;
    Mark.BookmarkNumber := ABookmark;
    Mark.Line := Y;
    Mark.Char := X;
    Mark.Visible := True;
    FMarkList.Add(Mark);
    FBookmarks[ABookmark] := Mark;
  end;

  if Assigned(FOnPlaceBookmark) then
    FOnPlaceBookmark(Self);
  Repaint;
end;

procedure TCustomFMXSynEdit.ClearBookmark(ABookmark: Integer);
var
  Idx: Integer;
begin
  if (ABookmark < 0) or (ABookmark > 9) then Exit;
  if FBookmarks[ABookmark] = nil then Exit;

  Idx := FMarkList.IndexOf(FBookmarks[ABookmark]);
  if Idx >= 0 then
    FMarkList.Delete(Idx);  // TObjectList frees the object
  FBookmarks[ABookmark] := nil;

  if Assigned(FOnClearBookmark) then
    FOnClearBookmark(Self);
  Repaint;
end;

procedure TCustomFMXSynEdit.GotoBookmark(ABookmark: Integer);
begin
  if (ABookmark < 0) or (ABookmark > 9) then Exit;
  if FBookmarks[ABookmark] = nil then Exit;

  SetCaretXY(BufferCoord(FBookmarks[ABookmark].Char,
    FBookmarks[ABookmark].Line));
  EnsureCursorPosVisible;
end;

function TCustomFMXSynEdit.GetBookmark(ABookmark: Integer;
  var X, Y: Integer): Boolean;
begin
  Result := False;
  if (ABookmark < 0) or (ABookmark > 9) then Exit;
  if FBookmarks[ABookmark] = nil then Exit;
  X := FBookmarks[ABookmark].Char;
  Y := FBookmarks[ABookmark].Line;
  Result := True;
end;

function TCustomFMXSynEdit.IsBookmarkSet(ABookmark: Integer): Boolean;
begin
  Result := (ABookmark >= 0) and (ABookmark <= 9) and
    (FBookmarks[ABookmark] <> nil);
end;

{ --- Mouse handling --- }

procedure TCustomFMXSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  BC: TBufferCoord;
  Row, Line: Integer;
  Band: TSynFMXGutterBand;
begin
  inherited;
  if not IsFocused then
    SetFocus;

  if Button = TMouseButton.mbLeft then
  begin
    // Check for gutter click
    if X < FGutterWidth then
    begin
      Row := Max(1, FTopLine + Trunc(Y / FLineHeight));
      Line := RowToLine(Row);
      Band := FGutter.BandAtX(X);
      if Assigned(Band) then
        Band.DoClick(Button, X, Y, Row, Line);
      Exit;
    end;

    BC := PixelToBufferCoord(X, Y);
    if Shift * [ssAlt, ssShift] = [ssAlt, ssShift] then
    begin
      // Alt+Shift+Click: column selection from anchor to click
      FSelection.Caret.Char := BC.Char;
      FSelection.Caret.Line := BC.Line;
      FSelections.ColumnSelection(ColumnSelectionStart, BC, FSelection.LastPosX);
      FSelection := FSelections.ActiveSelection;
    end
    else if ssAlt in Shift then
    begin
      // Alt+Click: add/toggle caret
      FSelections.AddCaret(BC);
      FSelection := FSelections.ActiveSelection;
    end
    else if ssShift in Shift then
    begin
      // Shift+Click: extend selection
      FSelection.Stop := BC;
      FSelection.Caret.Char := BC.Char;
      FSelection.Caret.Line := BC.Line;
      FSelections.ActiveSelection := FSelection;
    end
    else
    begin
      // Normal click: single caret
      FSelection.Start := BC;
      FSelection.Stop := BC;
      FSelection.Caret.Char := BC.Char;
      FSelection.Caret.Line := BC.Line;
      FSelection.LastPosX := -1;
      FSelections.ActiveSelection := FSelection;
      if FSelections.Count > 1 then
        FSelections.Clear(TSynSelectionsBase.TKeepSelection.ksKeepActive);
    end;
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
    if Shift * [ssAlt, ssShift] = [ssAlt, ssShift] then
    begin
      // Alt+Shift+Drag: column selection
      FSelection.Caret.Char := BC.Char;
      FSelection.Caret.Line := BC.Line;
      FSelections.ColumnSelection(ColumnSelectionStart, BC, FSelection.LastPosX);
      FSelection := FSelections.ActiveSelection;
    end
    else
    begin
      FSelection.Stop := BC;
      FSelection.Caret.Char := BC.Char;
      FSelection.Caret.Line := BC.Line;
      FSelections.ActiveSelection := FSelection;
      FSelections.MouseSelection(FSelection);
    end;
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
    FMaxScrollWidthValid := False;
    if FWordWrap and Assigned(FWordWrapHelper) then
    begin
      FWordWrapHelper.SetWrapWidth(GetWrapAreaWidth, FTabWidth);
      FWordWrapHelper.Reset(FLines);
    end;
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
  if FSelection.Caret.Char <> Value then
  begin
    FSelection.Caret.Char := Value;
    FSelection.LastPosX := -1;
    FSelections.ActiveSelection := FSelection;
    EnsureCursorPosVisible;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetCaretY(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FSelection.Caret.Line <> Value then
  begin
    FSelection.Caret.Line := Value;
    FSelections.ActiveSelection := FSelection;
    EnsureCursorPosVisible;
    Repaint;
  end;
end;

function TCustomFMXSynEdit.GetCaretX: Integer;
begin
  Result := FSelection.Caret.Char;
end;

function TCustomFMXSynEdit.GetCaretY: Integer;
begin
  Result := FSelection.Caret.Line;
end;

function TCustomFMXSynEdit.GetBlockBegin: TBufferCoord;
begin
  Result := FSelection.Start;
end;

function TCustomFMXSynEdit.GetBlockEnd: TBufferCoord;
begin
  Result := FSelection.Stop;
end;

function TCustomFMXSynEdit.GetCaretXY: TBufferCoord;
begin
  Result := FSelection.Caret;
end;

procedure TCustomFMXSynEdit.SetCaretXY(const Value: TBufferCoord);
begin
  FSelection.Caret.Char := Max(1, Value.Char);
  FSelection.Caret.Line := Max(1, Value.Line);
  FSelection.Start := Value;
  FSelection.Stop := Value;
  FSelection.LastPosX := -1;
  FSelections.ActiveSelection := FSelection;
  if FSelections.Count > 1 then
    FSelections.Clear(TSynSelectionsBase.TKeepSelection.ksKeepActive);
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
  FSelection.Caret.Char := 1;
  FSelection.Caret.Line := 1;
  FSelection.Start := BufferCoord(1, 1);
  FSelection.Stop := BufferCoord(1, 1);
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

function TCustomFMXSynEdit.GetModified: Boolean;
begin
  Result := (FUndoRedo <> nil) and FUndoRedo.Modified;
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
  if FWordWrap and Assigned(FWordWrapHelper) then
    Result := FWordWrapHelper.LineToRow(aLine)
  else if FUseCodeFolding then
    Result := FAllFoldRanges.FoldLineToRow(aLine)
  else
    Result := aLine;
end;

function TCustomFMXSynEdit.RowToLine(aRow: Integer): Integer;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
    Result := FWordWrapHelper.RowToLine(aRow)
  else if FUseCodeFolding then
    Result := FAllFoldRanges.FoldRowToLine(aRow)
  else
    Result := aRow;
end;

function TCustomFMXSynEdit.BufferToDisplayPos(
  const P: TBufferCoord): TDisplayCoord;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
    Result := FWordWrapHelper.BufferToDisplayPos(P)
  else
    Result := DisplayCoord(P.Char, LineToRow(P.Line));
end;

function TCustomFMXSynEdit.DisplayToBufferPos(
  const P: TDisplayCoord): TBufferCoord;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
    Result := FWordWrapHelper.DisplayToBufferPos(P)
  else
    Result := BufferCoord(P.Column, RowToLine(P.Row));
end;

function TCustomFMXSynEdit.GetRowLength(ARow: Integer): Integer;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
    Result := FWordWrapHelper.GetRowLength(ARow)
  else
  begin
    var Line := RowToLine(ARow);
    if (Line >= 1) and (Line <= FLines.Count) then
      Result := FLines[Line - 1].Length
    else
      Result := 0;
  end;
end;

function TCustomFMXSynEdit.GetDisplayRowCount: Integer;
begin
  if FWordWrap and Assigned(FWordWrapHelper) then
    Result := FWordWrapHelper.RowCount
  else if FUseCodeFolding then
    Result := LineToRow(FLines.Count)
  else
    Result := FLines.Count;
end;

{ --- Word Wrap --- }

function TCustomFMXSynEdit.GetWrapAreaWidth: Integer;
begin
  if FCharWidth > 0 then
    Result := Max(1, Trunc((Width - FGutterWidth) / FCharWidth))
  else
    Result := 80;
end;

procedure TCustomFMXSynEdit.SetWordWrap(Value: Boolean);
begin
  if FWordWrap = Value then Exit;
  // Mutually exclusive with code folding
  if Value and FUseCodeFolding then Exit;
  FWordWrap := Value;
  if FWordWrap then
  begin
    FWordWrapHelper := TFMXWordWrapHelper.Create;
    FWordWrapHelper.SetWrapWidth(GetWrapAreaWidth, FTabWidth);
    FWordWrapHelper.Reset(FLines);
    FLeftChar := 1;
  end
  else
    FreeAndNil(FWordWrapHelper);
  RecalcSizes;
  UpdateScrollBars;
  EnsureCursorPosVisible;
  Repaint;
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

  // Mutually exclusive with word wrap
  ValidValue := Value and not FWordWrap and
    ((Assigned(FHighlighter) and
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

    // Toggle fold band visibility
    var FoldBand := FGutter.Bands.BandByKind(gbkFold);
    if Assigned(FoldBand) then
      FoldBand.Visible := ValidValue;

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

procedure TCustomFMXSynEdit.Collapse(FoldRangeIndex: Integer; Invalidate: Boolean);
var
  Range: TSynFoldRange;
begin
  if not FUseCodeFolding then Exit;

  if FAllFoldRanges.Collapse(FoldRangeIndex) then
  begin
    Range := FAllFoldRanges[FoldRangeIndex];
    // Extract caret from fold
    if (FSelection.Caret.Line > Range.FromLine) and (FSelection.Caret.Line <= Range.ToLine) then
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
  while FAllFoldRanges.FoldHidesLine(FSelection.Caret.Line, Index) do
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
  if FAllFoldRanges.FoldAroundLineEx(FSelection.Caret.Line, False, True, True, Index) then
    Collapse(Index);
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.UncollapseNearest;
var
  Index: Integer;
begin
  if not FUseCodeFolding then Exit;
  if FAllFoldRanges.CollapsedFoldStartAtLine(FSelection.Caret.Line, Index) then
    Uncollapse(Index);
  EnsureCursorPosVisible;
end;

procedure TCustomFMXSynEdit.CollapseLevel(Level: Integer);
begin
  if not FUseCodeFolding then Exit;
  FAllFoldRanges.CollapseLevel(Level);

  // Surface caret
  var Index: Integer;
  while FAllFoldRanges.FoldHidesLine(FSelection.Caret.Line, Index) do
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
            FSelection.Caret.Char := nFound + nReplaceLen;
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
      ptStart := FSelection.Start;
      ptEnd := FSelection.Stop;
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

procedure TCustomFMXSynEdit.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  FKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomFMXSynEdit.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  FKbdHandler.RemoveKeyDownHandler(aHandler);
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

{ TSynFMXEditPlugin }

constructor TSynFMXEditPlugin.Create(AOwner: TCustomFMXSynEdit;
  AHandlers: TPlugInHandlers);
begin
  inherited Create;
  FOwner := AOwner;
  FHandlers := AHandlers;
  if Assigned(AOwner) then
    AOwner.RegisterPlugin(Self);
end;

destructor TSynFMXEditPlugin.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.UnregisterPlugin(Self);
  inherited;
end;

procedure TSynFMXEditPlugin.AfterPaint(Canvas: TCanvas; const AClip: TRectF;
  FirstLine, LastLine: Integer);
begin
end;

procedure TSynFMXEditPlugin.LinesInserted(FirstLine, Count: Integer);
begin
end;

procedure TSynFMXEditPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
end;

procedure TSynFMXEditPlugin.LinePut(aIndex: Integer; const OldLine: string);
begin
end;

end.
