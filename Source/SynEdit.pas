{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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
-------------------------------------------------------------------------------}
//todo: Remove checks for WordWrap. Must abstract the behaviour with the plugins instead.
//todo: Move WordWrap glyph to the WordWrap plugin.

unit SynEdit;

{$I SynEdit.inc}

interface

uses
  System.Math,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  System.Generics.Collections,
  System.Diagnostics,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  Winapi.D2D1,
  Winapi.Imm,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.StdActns,
  Vcl.Dialogs,
  Vcl.Themes,
  System.UITypes,
  SynUnicode,
  SynEditTypes,
  SynEditKeyConst,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer,
  SynDWrite,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditKbdHandler,
  SynEditCodeFolding;

const
  // Max number of book/gutter marks returned from GetEditMarksForLine - that
  // really should be enough.
  MAX_MARKS = 16;

type
  TBufferCoord = SynEditTypes.TBufferCoord;
  TDisplayCoord = SynEditTypes.TDisplayCoord;

  TSynBorderStyle = TBorderStyle;

  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  ESynEditError = class(ESynError);

  TDropFilesEvent = procedure(Sender: TObject; X, Y: Integer; AFiles: TStrings)
    of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column: Integer; var Action: TSynReplaceAction) of object;

  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: Integer;
    var Special: Boolean; var FG, BG: TColor) of object;

  TTransientType = (ttBefore, ttAfter);
  TPaintTransient = procedure(Sender: TObject; Canvas: TCanvas;
    TransientType: TTransientType) of object;

  TScrollEvent = procedure(Sender: TObject; ScrollBar: TScrollBarKind) of object;

  TGutterGetTextEvent = procedure(Sender: TObject; aLine: Integer;
    var aText: string) of object;

  TGetLineIndicatorsEvent = procedure(Sender: TObject; const Line: Integer;
    var LineIndicators: TArray<TSynIndicator>) of Object;

  TZoomEvent = procedure(Sender: TObject; const NewFontSize,
    OrigFontSize: Integer) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfPossibleGutterClick,
    sfOleDragSource, sfGutterDragging);

  TSynStateFlags = set of TSynStateFlag;

  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TSynEditorOption = (
    eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoDisableScrollArrows,     //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               //Allows the editor accept OLE file drops
    eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
    eoEnhanceEndKey,           //enhances End key positioning, similar to JDeveloper
    eoGroupUndo,               //When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          //When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      //if enabled, then the scrollbars will only show when necessary.
    eoKeepCaretX,              //When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret,                 //Makes it so the caret is never visible
    eoNoSelection,             //Disables selecting text
    eoRightMouseMovesCursor,   //When clicking with the right mouse for a popup menu, move the cursor to that location
    eoScrollByOneLess,         //Forces scrolling to be one less
    eoScrollHintFollows,       //The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof,           //Allows the cursor to go past the end of file marker
    eoScrollPastEol,           //Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint,          //Shows a hint of the visible line numbers when scrolling vertically
    eoSmartTabDelete,          //similar to Smart Tabs, but when you delete characters
    eoSmartTabs,               //When tabbing, the cursor will go to the next non-white space character of the previous line
    eoSpecialLineDefaultFg,    //disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent,               //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,            //Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,      //Spaces at the end of lines will be trimmed and not saved
    eoShowLigatures,           //Shows font ligatures, by default it is disabled
    eoCopyPlainText,           //Do not include additional clipboard formats when you copy to Clipboard or drag text
    eoNoHTMLBackground,        //Ignore SynEdit background color when copying in HTML format
    eoWrapWithRightEdge,       //WordWrap with RightEdge position instead of the whole text area
    eoBracketsHighlight,       //Enable bracket highlighting
    eoAccessibility            //Enable accessibility support
    );
  TSynEditorOptions = set of TSynEditorOption;

  TSynSpecialChars = (scWhitespace, scControlChars, scEOL);
  TSynVisibleSpecialChars = set of TSynSpecialChars;

const
  SYNEDIT_DEFAULT_OPTIONS = [eoAutoIndent, eoDragDropEditing, eoKeepCaretX,
    eoEnhanceHomeKey, eoEnhanceEndKey, eoHideShowScrollbars,
    eoDisableScrollArrows, eoShowScrollHint, eoTabIndent, eoTabsToSpaces,
    eoSmartTabDelete, eoGroupUndo, eoDropFiles, eoShowLigatures,
    eoBracketsHighlight, eoAccessibility];

type
// use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly);
  TSynStatusChanges = set of TSynStatusChange;

  TContextHelpEvent = procedure(Sender: TObject; word: string)
    of object;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TMouseCursorEvent = procedure(Sender: TObject; const aLineCharPos: TBufferCoord;
    var aCursor: TCursor) of object;

  // CodeFolding
  TScanForFoldRangesEvent = procedure(Sender: TObject;
    FoldRanges: TSynFoldRanges; LinesToScan: TStrings;
    FromLine : Integer; ToLine : Integer) of object;

  TCustomSynEdit = class;

  TSynEditMark = class
  protected
    fLine, fChar, fImage: Integer;
    fEdit: TCustomSynEdit;
    fVisible: Boolean;
    fInternalImage: Boolean;
    fBookmarkNum: Integer;
    function GetEdit: TCustomSynEdit; virtual;
    procedure SetChar(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: Boolean);
    procedure SetInternalImage(const Value: Boolean);
    function GetIsBookmark: Boolean;
  public
    constructor Create(AOwner: TCustomSynEdit);
    property Line: Integer read fLine write SetLine;
    property Char: Integer read fChar write SetChar;
    property Edit: TCustomSynEdit read fEdit;
    property ImageIndex: Integer read fImage write SetImage;
    property BookmarkNumber: Integer read fBookmarkNum write fBookmarkNum;
    property Visible: Boolean read fVisible write SetVisible;
    property InternalImage: Boolean read fInternalImage write SetInternalImage;
    property IsBookmark: Boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

  TSynEditMarks = array[1..MAX_MARKS] of TSynEditMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the gutter. }
  TSynEditMarkList = class(TObjectList<TSynEditMark>)
  protected
    fEdit: TCustomSynEdit;
  public
    constructor Create(AOwner: TCustomSynEdit);
    procedure ClearLine(line: Integer);
    procedure GetMarksForLine(line: Integer; var Marks: TSynEditMarks);
    procedure Place(Mark: TSynEditMark);
  public
    property Edit: TCustomSynEdit read fEdit;
  end;

  TGutterClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    X, Y, Line: Integer; Mark: TSynEditMark) of object;

  TPluginHandler = (phLinesInserted, phLinesBeforeDeleted, phLinesDeleted,
    phLinePut, phLinesChanged, phPaintTransient, phAfterPaint);
  TPlugInHandlers = set of TPluginHandler;

  TSynEditPlugin = class(TObject)
  private
    fOwner: TCustomSynEdit;
  protected
    FHandlers: TPlugInHandlers;
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); virtual;
    procedure PaintTransient(ACanvas: TCanvas; ATransientType: TTransientType); virtual;
    procedure LinesInserted(FirstLine, Count: Integer); virtual;
    procedure LinesBeforeDeleted(FirstLine, Count: Integer); virtual;
    procedure LinesDeleted(FirstLine, Count: Integer); virtual;
    procedure LinePut(aIndex: Integer; const OldLine: string); virtual;
    procedure LinesChanged; virtual;
  protected
    property Editor: TCustomSynEdit read fOwner;
  public
    constructor Create(AOwner: TCustomSynEdit); overload;
    constructor Create(AOwner: TCustomSynEdit; AHandlers: TPlugInHandlers); overload;
    destructor Destroy; override;
    property Handlers: TPlugInHandlers read FHandlers;
  end;

  TCustomSynEditSearchNotFoundEvent = procedure(Sender: TObject;
    FindText: string) of object;

  TCustomSynEdit = class(TCustomControl)
  private
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMClear(var Msg: TMessage); message WM_CLEAR;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetText(var Msg: TWMGetText); message WM_GETTEXT;
    procedure WMGetTextLength(var Msg: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMMouseHWheel(var Message: TWMMouseWheel); message WM_MOUSEHWHEEL;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMImeChar(var Msg: TMessage); message WM_IME_CHAR;
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
    procedure WMImeRequest(var Message: TMessage); message WM_IME_REQUEST;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure WMGetObject(var Message: TMessage); message WM_GETOBJECT;
  private
//++ CodeFolding
    fUseCodeFolding : Boolean;
    fCodeFolding: TSynCodeFolding;
    fAllFoldRanges: TSynFoldRanges;
//-- CodeFolding
    fAlwaysShowCaret: Boolean;
    FCarets: TSynCarets;
    FCaseSensitive: Boolean;
    FSelection: TSynSelection;
    FSelections: TSynSelections;
    fCharWidth: Integer;
    fFontQuality: TFontQuality;
    fInserting: Boolean;
    fLines: TStrings;
    fOrigLines: TStrings;
    fOrigUndoRedo: ISynEditUndo;
    fLinesInWindow: Integer;
    fLeftChar: Integer;
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    fScrollHintColor: TColor;
    fScrollHintFormat: TScrollHintFormat;
    FScrollBars: TScrollStyle;
    FSynEditScrollBars: ISynEditScrollBars;
    FTextAreaWidth: Integer;
    FTextHint: string;
    fTextHeight: Integer;
    fTextMargin: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    fHighlighter: TSynCustomHighlighter;
    fSelectedColor: TSynSelectedColor;
    FIndentGuides: TSynIndentGuides;
    fActiveLineColor: TColor;
    fUndoRedo: ISynEditUndo;
    fBookMarks: array[0..9] of TSynEditMark; // these are just references, fMarkList is the owner
    fMouseDownX: Integer;
    fMouseDownY: Integer;
    fBookMarkOpt: TSynBookMarkOpt;
    fBorderStyle: TSynBorderStyle;
    fHideSelection: Boolean;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fKeyStrokes: TSynEditKeyStrokes;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: Integer;
    fWantReturns: Boolean;
    fWantTabs: Boolean;
    fWordWrapPlugin: ISynEditBufferPlugin;
    fWordWrapGlyph: TSynGlyph;

    fGutter: TSynGutter;
    fTabWidth: Integer;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    fStatusChanges: TSynStatusChanges;
    fLastKey: word;
    fLastShiftState: TShiftState;
    fSearchEngine: TSynEditSearchCustom;
    fHookedCommandHandlers: TObjectList;
    fKbdHandler: TSynEditKbdHandler;
    fFocusList: TList;
    fPlugins: TObjectList;
    fScrollTimer: TTimer;
    fScrollDeltaX, fScrollDeltaY: Integer;
    fClickCountTimer: TStopWatch;
    fClickCount: Integer;
    FIndicators: TSynIndicators;
    FPaintTransientLock: Integer;
    FAdditionalWordBreakChars: TSysCharSet;
    FAdditionalIdentChars: TSysCharSet;
    FTextFormat: TSynTextFormat;
    FSelStorage: TSynSelStorage;
    FBracketsHighlight: TSynBracketsHighlight;
    FScrollbarAnnotations: TSynScrollbarAnnotations;
    FPasteArray: TArray<string>;
    FPaintTransientPlugins: Boolean;
    FOrigFontSize: Integer;
    FOrigGutterFontSize: Integer;
    FDisplayFlowControl: TSynDisplayFlowControl;

    // Accessibility
    FUIAutomationProvider: IInterface;  // IRawElementProviderSimple
    FAccessibleName: string;   // For screen readers.

    // event handlers
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TDropFilesEvent;
    fOnGutterClick: TGutterClickEvent;
    fOnMouseCursor: TMouseCursorEvent;
    fOnPaint: TPaintEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fOnContextHelp: TContextHelpEvent;
    fOnPaintTransient: TPaintTransient;
    fOnScroll: TScrollEvent;
    fOnShowHint: TShowHintEvent;
    fOnGutterGetText: TGutterGetTextEvent;
    FOnGetLineIndicators: TGetLineIndicatorsEvent;
    fOnStatusChange: TStatusChangeEvent;
    fOnTripleClick: TNotifyEvent;
    fOnQudrupleClick: TNotifyEvent;
    FOnZoom: TZoomEvent;

    fChainListCleared: TNotifyEvent;
    fChainListDeleted: TStringListChangeEvent;
    fChainListInserted: TStringListChangeEvent;
    fChainListPut: TStringListPutEvent;
    fChainLinesChanging: TNotifyEvent;
    fChainLinesChanged: TNotifyEvent;
    fChainedEditor: TCustomSynEdit;
    fChainModifiedChanged: TNotifyEvent;
    fSearchNotFound: TCustomSynEditSearchNotFoundEvent;
    OnFindBeforeSearch: TNotifyEvent;
    OnReplaceBeforeSearch: TNotifyEvent;
    OnCloseBeforeSearch: TNotifyEvent;
//++ CodeFolding
    fOnScanForFoldRanges : TScanForFoldRangesEvent;
    procedure ReScanForFoldRanges(FromLine : Integer; ToLine : Integer);
    procedure FullFoldScan;
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine : Integer; ToLine : Integer);
//-- CodeFolding
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure ComputeScroll(X, Y: Integer);
    procedure DoHomeKey(Selection:boolean);
    procedure DoEndKey(Selection: Boolean);
    procedure DoLinesChanged;
    procedure DoLinesBeforeDeleted(FirstLine, Count: Integer);
    procedure DoLinesDeleted(FirstLine, Count: Integer);
    procedure DoLinesInserted(FirstLine, Count: Integer);
    procedure DoLinePut(Index: Integer; const OldLine: string);
    procedure DoShiftTabKey;
    procedure DoTabKey;
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;
    procedure SynFontChanged(Sender: TObject);
    procedure ForceCaretX(aCaretX: integer);
    function GetBlockBegin: TBufferCoord;
    function GetBlockEnd: TBufferCoord;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetDisplayX: Integer;
    function GetDisplayY: Integer;
    function GetDisplayXY: TDisplayCoord;
    function GetDisplayRowCount: Integer;
    function GetHookedCommandHandlersCount: Integer;
    function GetLineText: string;
    function GetMaxUndo: Integer;
    function GetModified: Boolean;
    function GetRow(RowIndex: Integer): string;
    function GetRowLength(RowIndex: Integer): Integer;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    function SynGetText: string;
    function GetWordAtCursor: string;
    function GetWordAtMouse: string;
    function GetWordWrap: Boolean;
    procedure GutterChanged(Sender: TObject);
    procedure IndentGuidesChanged(Sender: TObject);
    procedure InternalSetCaretX(Value: Integer);
    procedure InternalSetCaretY(Value: Integer);
    procedure InsertCharAtCursor(const AChar: string);
    function GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): string;
    procedure LinesChanging(Sender: TObject);
    procedure MoveCaretAndSelection(const NewPos: TBufferCoord; SelectionCmd:
        Boolean);
    procedure MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
    procedure MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
    procedure PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer);
    procedure ReadAddedKeystrokes(Reader: TReader);
    procedure ReadRemovedKeystrokes(Reader: TReader);
    function ScanFrom(Index: Integer): Integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SelectedColorChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
    procedure SetBorderStyle(Value: TSynBorderStyle);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure SetCaretInRow(Value:TBufferCoord; Row: Integer);
    procedure SetDisplayXY(const aPos: TDisplayCoord);
    procedure SetActiveLineColor(Value: TColor);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetIndentGuides(const Value: TSynIndentGuides);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: Boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TStrings);
    procedure SetLineText(Value: string);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetVisibleSpecialChars(Value: TSynVisibleSpecialChars);
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    procedure SetSelectedColor(const Value: TSynSelectedColor);
    procedure SetTabWidth(Value: Integer);
    procedure SynSetText(const Value: string);
    procedure SetTopLine(Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetWordWrapGlyph(const Value: TSynGlyph);
    procedure WordWrapGlyphChange(Sender: TObject);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure ModifiedChanged(Sender: TObject);
    procedure UpdateLastPosX;
    procedure UpdateScrollBars;
    procedure WriteAddedKeystrokes(Writer: TWriter);
    procedure WriteRemovedKeystrokes(Writer: TWriter);
    procedure SetAdditionalIdentChars(const Value: TSysCharSet);
    procedure SetAdditionalWordBreakChars(const Value: TSysCharSet);
    function ValidBC(const Value: TBufferCoord): TBufferCoord;

    procedure DoSearchFindFirstExecute(Action: TSearchFindFirst);
    procedure DoSearchFindExecute(Action: TSearchFind);
    procedure DoSearchReplaceExecute(Action: TSearchReplace);
    procedure DoSearchFindNextExecute(Action: TSearchFindNext);
    procedure FindDialogFindFirst(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    function SearchByFindDialog(FindDialog: TFindDialog) : bool;
    procedure FindDialogClose(Sender: TObject);
    procedure DoMouseSelectLineRange(NewPos: TBufferCoord);
    procedure DoMouseSelectWordRange(NewPos: TBufferCoord);
    procedure InternalCommandHook(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: pointer; HandlerData: pointer);
//++ CodeFolding
    procedure SetUseCodeFolding(const Value: Boolean);
    procedure OnCodeFoldingChange(Sender: TObject);
    function GetCollapseMarkRect(Row: Integer; Line: Integer = -1): TRect;
    function GetWrapAreaWidth: Integer;
    function GetIsScrolling: Boolean;
    function GetCaseSensitive: Boolean;
//-- CodeFolding
  protected
    FIgnoreNextChar: Boolean;
    FCharCodeString: string;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure InvalidateRect(const aRect: TRect; aErase: Boolean); virtual;
    procedure DblClick; override;
    procedure TripleClick; virtual;
    procedure QuadrupleClick; virtual;
    procedure DecPaintLock;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoChange; virtual;
    procedure HighlightBrackets; virtual;
    //++ Ole Drag & Drop
    procedure OleDragEnter(Sender : TObject; DataObject : IDataObject;
      State : TShiftState; MousePt : TPoint; var Effect: LongInt;
      var Result: HResult); virtual;
    procedure OleDragOver(Sender : TObject; DataObject : IDataObject;
      State : TShiftState; MousePt : TPoint; var Effect: LongInt;
      var Result: HResult); virtual;
    procedure OleDrop(Sender : TObject; DataObject : IDataObject;
      State : TShiftState; MousePt : TPoint; var Effect: LongInt;
      var Result: HResult); virtual;
    procedure OleDragLeave(Sender : TObject; var Result : HResult); virtual;
    //-- Ole Drag & Drop
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure LinesChanged(Sender: TObject); virtual;
    procedure ListCleared(Sender: TObject);
    procedure ListBeforeDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ListInserted(Sender: TObject; Index: Integer; aCount: Integer);
    procedure ListPut(Sender: TObject; Index: Integer; const OldLine: string);
    //helper procs to chain list commands
    procedure ChainListCleared(Sender: TObject);
    procedure ChainListDeleted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListInserted(Sender: TObject; aIndex: Integer; aCount: Integer);
    procedure ChainListPut(Sender: TObject; aIndex: Integer; const OldLine: string);
    procedure ChainLinesChanging(Sender: TObject);
    procedure ChainLinesChanged(Sender: TObject);
    procedure ChainModifiedChanged(Sender: TObject);
    procedure ScanRanges;
    procedure Loaded; override;
    procedure MarkListNotify(Sender: TObject; const Mark: TSynEditMark;
      Action: TCollectionNotification);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure NotifyHookedCommandHandlers(AfterProcessing: Boolean;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintGutter(RT: ID2D1RenderTarget; const AClip: TRect; const
        aFirstRow, aLastRow: Integer); virtual;
    procedure PaintTextLines(RT: ID2D1RenderTarget; AClip: TRect; const aFirstRow,
        aLastRow: Integer); virtual;
    procedure InternalSetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXYEx(EnsureVisible: Boolean; Value: TBufferCoord); virtual;
    procedure SetFontQuality(AValue: TFontQuality);
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetWantReturns(Value: Boolean);
    procedure SetSelText(const Value: string);
    procedure SetSelTextPrimitiveEx(const Value: string; AddToUndoList: Boolean = True);
    procedure SetTextHint(const Value: string);
    procedure SetWantTabs(Value: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UpdateMouseCursor; virtual;
    property FLastPosX: integer read FSelection.LastPosX write FSelection.LastPosX;
    property CaretAtEOL: Boolean read FSelection.CaretAtEOL write FSelection.CaretAtEOL;
  protected
    fGutterWidth: Integer;
    procedure CalcTextAreaWidth;
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    procedure DoOnGutterClick(Button: TMouseButton; X, Y: Integer); virtual;
    procedure DoOnMouserCursor(const aLineCharPos: TBufferCoord;
      var aCursor: TCursor); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransient(TransientType: TTransientType); virtual;
    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: WideChar; Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column: Integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: Integer;
      var Foreground, Background: TColor): Boolean; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    function GetSelEnd: integer;
    function GetSelStart: integer;
    function GetSelLength: integer;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelLength(const Value: integer);
    procedure SetAlwaysShowCaret(const Value: Boolean);
    procedure LinesHookChanged;
    // Command implementations
    procedure ExecCmdDeleteLine;
    procedure ExecCmdCopyOrMoveLine(const Command: TSynEditorCommand);
    procedure ExecCmdCaseChange(const Cmd : TSynEditorCommand);

    property InternalCaretX: Integer write InternalSetCaretX;
    property InternalCaretY: Integer write InternalSetCaretY;
    property InternalCaretXY: TBufferCoord write InternalSetCaretXY;
    procedure ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$endif}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property AlwaysShowCaret: Boolean read FAlwaysShowCaret
      write SetAlwaysShowCaret;
    procedure UpdateCarets;
    procedure UpdateIME;
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word = 0; SS2: TShiftState = []);
    procedure BeginUndoBlock;
    procedure BeginUpdate;
    function CaretInView: Boolean;
    procedure CaretsAtLineEnds;
    function CharIndexToRowCol(Index: Integer; LineBreak: string = SLineBreak): TBufferCoord;
    procedure Clear;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure DeleteSelections;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    procedure ClearUndo;
    procedure ClearTrackChanges;
    procedure MarkSaved;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure EndUndoBlock;
    procedure EndUpdate;
    procedure EnsureCaretInView;
    procedure EnsureCursorPosVisible;
    procedure EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
      EvenIfVisible: Boolean = False);
    procedure FindMatchingBracket; virtual;
    function GetMatchingBracket: TBufferCoord; virtual;
    function GetMatchingBracketEx(const APoint: TBufferCoord;
      Brackets: string = '()[]{}<>'): TBufferCoord; virtual;
    function GetMatchingBracketEnhanced(var BracketPos: TBufferCoord; Brackets:
        string = '()[]{}<>'; AdjustMatchingPos: Boolean = True): TBufferCoord;
        virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    procedure ExecuteMultiCaretCommand(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer; CommandInfo: TSynCommandInfo); virtual;
    function GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: string;
      var Attri: TSynHighlighterAttributes): Boolean;
    function GetHighlighterAttriAtRowColEx(const XY: TBufferCoord; var Token: string;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
    function GetWordAtRowCol(XY: TBufferCoord): string;
    procedure GetWordBoundaries(XY: TBufferCoord; var BB, BE: TBufferCoord);
    procedure GotoBookMark(BookMark: Integer); virtual;
    procedure GotoLineAndCenter(ALine: Integer); virtual;
    function IsIdentChar(AChar: WideChar): Boolean; virtual;
    function IsWhiteChar(AChar: WideChar): Boolean; virtual;
    function IsWordBreakChar(AChar: WideChar): Boolean; virtual;
    // support procedure for ecDeletexxx commands
    function IsNonWhiteChar(AChar: WideChar): Boolean; virtual;
    procedure InvalidateGutter;
    procedure InvalidateGutterLine(aLine: integer);
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateGutterBand(Kind: TSynGutterBandKind);
    procedure InvalidateLine(Line: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure InvalidateRange(const BB, BE: TBufferCoord);
    procedure InvalidateSelection; overload;
    procedure InvalidateSelection(const Sel: TSynSelection); overload;
    function IsBookmark(BookMark: Integer): Boolean;
    function IsPointInSelection(const Value: TBufferCoord): Boolean;
    procedure LockUndo;
    procedure MoveDisplayPosAndSelection(const NewPos: TDisplayCoord;
      SelectionCmd: Boolean);
    function BufferToDisplayPos(const BC: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const DC: TDisplayCoord): TBufferCoord;
    function LineToRow(aLine: Integer): Integer;
    function RowToLine(aRow: Integer): Integer;
    function SelectionToDisplayCoord(var Sel: TSynSelection): TDisplayCoord;
    procedure PasteFromClipboard;
    function TextWidth(const S: string): Integer; overload;
    function TextWidth(P: PChar; Len: Integer): Integer; overload;

    // for use in PaintTransient
    procedure PaintText(S: string; P: TPoint; ClipR: TRect; FontStyle: TFontStyles;
        FontColor: TColor; BkgColor: TColor = clNone);

    function NextWordPos: TBufferCoord; virtual;
    function NextWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordStart: TBufferCoord; virtual;
    function WordStartEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordEnd: TBufferCoord; virtual;
    function WordEndEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function PrevWordPos: TBufferCoord; virtual;
    function PrevWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;

    function PixelsToColumn(P: PChar; Len: Integer; aX: Integer; CharBefore:
        Boolean = False): Integer;
    function PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
    function PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
    procedure Redo;
    procedure RegisterCommandHandler(const AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnInView(RowCol: TDisplayCoord): Boolean;
    function ColumnToPixels(const S: string; Col: Integer): Integer;
    function RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
    function RowColToCharIndex(RowCol: TBufferCoord; LineBreak: string = SLineBreak): Integer;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): Integer; overload;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions; const Start, Stop: TBufferCoord): Integer; overload;
    procedure SelectAll;
    function SelectionText(Sel: TSynSelection): string;
    procedure SelectMatchingText;
    function SelectSameWord(const AWord: string; Start: TBufferCoord;
      BackwardSearch: Boolean = False; AddSelection: Boolean = False): Boolean;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore,
      ptAfter: TBufferCoord; EnsureVisible: Boolean= True;
      ForceToMiddle: Boolean = False); overload;
    procedure SetCaretAndSelection(const Sel: TSynSelection;
      EnsureVisible: Boolean= True; ForceToMiddle: Boolean = False); overload;
    procedure SetDefaultKeystrokes; virtual;
    procedure SetSelWord;
    procedure SetWordBlock(Value: TBufferCoord);
    procedure Undo;
    procedure UnlockUndo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function ValidTextPos(const S: String; Index: Integer; Trailing: Boolean):
        Integer; overload;
    function ValidTextPos(BC: TBufferCoord; Trailing: Boolean): TBufferCoord; overload;
    procedure SetFocus; override;

    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressEvent);
    procedure AddFocusControl(aControl: TWinControl);
    procedure RemoveFocusControl(aControl: TWinControl);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);

    procedure WndProc(var Msg: TMessage); override;
    procedure SetLinesPointer(ASynEdit: TCustomSynEdit);
    procedure RemoveLinesPointer;
    function IsChained: Boolean;
    procedure HookTextBuffer(aBuffer: TSynEditStringList; aUndoRedo: ISynEditUndo);
    procedure UnHookTextBuffer;

    procedure Zoom(ExtraFontSize: Integer);
    procedure ZoomReset;
//++ CodeFolding
    procedure CollapseAll;
    procedure UncollapseAll;
    procedure Collapse(FoldRangeIndex: Integer; Invalidate:Boolean = True);
    procedure Uncollapse(FoldRangeIndex: Integer; Invalidate:Boolean = True);
    procedure UncollapseAroundLine(Line: Integer);
    procedure CollapseNearest;
    procedure UncollapseNearest;
    procedure CollapseLevel(Level : Integer);
    procedure UnCollapseLevel(Level : Integer);
    procedure CollapseFoldType(FoldType : Integer);
    procedure UnCollapseFoldType(FoldType : Integer);
    procedure SurfaceCaretFromHiddenFolds;
//-- CodeFolding
  public
    property AdditionalIdentChars: TSysCharSet read FAdditionalIdentChars write SetAdditionalIdentChars;
    property AdditionalWordBreakChars: TSysCharSet read FAdditionalWordBreakChars write SetAdditionalWordBreakChars;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read FSelection.Caret.Char write SetCaretX;
    property CaretY: Integer read FSelection.Caret.Line write SetCaretY;
    property CaretXY: TBufferCoord read FSelection.Caret write SetCaretXY;
    property CaseSensitive: Boolean read GetCaseSensitive write FCaseSensitive;
    property ActiveLineColor: TColor read fActiveLineColor
      write SetActiveLineColor default clNone;
    property DisplayX: Integer read GetDisplayX;
    property DisplayY: Integer read GetDisplayY;
    property DisplayXY: TDisplayCoord read GetDisplayXY write SetDisplayXY;
    property DisplayRowCount: Integer read GetDisplayRowCount;
    property CharWidth: Integer read fCharWidth;
    property Color;
    property Cursor default crIBeam;
    property Font;
    property Indicators: TSynIndicators read FIndicators;
    property BracketsHighlight: TSynBracketsHighlight read FBracketsHighlight;
    property ScrollbarAnnotations: TSynScrollbarAnnotations read FScrollbarAnnotations;
    property Highlighter: TSynCustomHighlighter read fHighlighter
      write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: Integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: string read GetLineText write SetLineText;
    property Lines: TStrings read fLines write SetLines;
    property Rows[RowIndex: integer]: string read GetRow;
    property RowLength[RowIndex: integer]: Integer read GetRowLength;
    property Marks: TSynEditMarkList read fMarkList;
    property Modified: Boolean read GetModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchEngine: TSynEditSearchCustom read fSearchEngine write SetSearchEngine;
    property SelAvail: Boolean read GetSelAvail;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    property Selection: TSynSelection read FSelection;
    property Selections: TSynSelections read FSelections;
    property StateFlags: TSynStateFlags read fStateFlags write fStateFlags;
    property Text: string read SynGetText write SynSetText;
    property TextHint: string read FTextHint write SetTextHint;
    property TopLine: Integer read fTopLine write SetTopLine;
    property TextAreaWidth: Integer read FTextAreaWidth;
    property WrapAreaWidth: Integer read GetWrapAreaWidth;
    property WordAtCursor: string read GetWordAtCursor;
    property WordAtMouse: string read GetWordAtMouse;
    property GutterWidth: Integer read FGutterWidth;
    property TextMargin: Integer read FTextMargin;
    property UndoRedo: ISynEditUndo read fUndoRedo;
    property TextFormat: TSynTextFormat read FTextFormat;
    property FontQuality: TFontQuality read fFontQuality write SetFontQuality;
  public
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;

//++ CodeFolding
    property CodeFolding: TSynCodeFolding read fCodeFolding write fCodeFolding;
    property UseCodeFolding: Boolean read fUseCodeFolding write SetUseCodeFolding;
    property AllFoldRanges: TSynFoldRanges read fAllFoldRanges;
//-- CodeFolding
    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write SetExtraLineSpacing default 2;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property HideSelection: Boolean read fHideSelection write SetHideSelection
      default False;
    property IndentGuides: TSynIndentGuides
      read FIndentGuides write SetIndentGuides;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property IsScrolling : Boolean read GetIsScrolling;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes stored False;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 0;
    property Options: TSynEditorOptions read fOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property VisibleSpecialChars: TSynVisibleSpecialChars
      read FVisibleSpecialChars write SetVisibleSpecialChars;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollHintColor: TColor read fScrollHintColor
      write fScrollHintColor default clInfoBk;
    property ScrollHintFormat: TScrollHintFormat read fScrollHintFormat
      write fScrollHintFormat default shfTopLineOnly;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write SetSelectedColor;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantReturns: boolean read fWantReturns write SetWantReturns default True;
    property WantTabs: boolean read fWantTabs write SetWantTabs default False;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default False;
    property WordWrapGlyph: TSynGlyph read fWordWrapGlyph write SetWordWrapGlyph;
    property AccessibleName: string read FAccessibleName write FAccessibleName;
    property DisplayFlowControl: TSynDisplayFlowControl read FDisplayFlowControl
      write FDisplayFlowControl;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read fOnClearMark
      write fOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnContextHelp: TContextHelpEvent
      read fOnContextHelp write fOnContextHelp;
    property OnDropFiles: TDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read fOnGutterClick write fOnGutterClick;
    property OnGutterGetText: TGutterGetTextEvent read fOnGutterGetText
      write fOnGutterGetText;
    property OnGetLineIndicators: TGetLineIndicatorsEvent
      read FOnGetLineIndicators write FOnGetLineIndicators;
    property OnMouseCursor: TMouseCursorEvent read fOnMouseCursor
      write fOnMouseCursor;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
    property OnPaintTransient: TPaintTransient
      read fOnPaintTransient write fOnPaintTransient;
    property OnShowHint: TShowHintEvent read fOnShowHint write fOnShowHint;
    property OnScroll: TScrollEvent read fOnScroll write fOnScroll;
    property OnTripleClick: TNotifyEvent
      read fOnTripleClick write fOnTripleClick;
    property OnQuadrupleClick: TNotifyEvent
      read fOnQudrupleClick write fOnQudrupleClick;
    property OnZoom: TZoomEvent read FOnZoom write FOnZoom;
//++ CodeFolding
    property OnScanForFoldRanges: TScanForFoldRangesEvent
      read fOnScanForFoldRanges write fOnScanForFoldRanges;
//-- CodeFolding
    property OnSearchNotFound: TCustomSynEditSearchNotFoundEvent
      read fSearchNotFound write fSearchNotFound;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    // inherited properties
    property AccessibleName;
    property Align;
    property Anchors;
    property DoubleBuffered;
    property CaseSensitive default False;
    property Constraints;
    property Color;
    property ActiveLineColor;
    property Ctl3D;
    property Cursor;
    property ParentCtl3D;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentDoubleBuffered;
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property TextHint;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    // TCustomSynEdit properties
//++ CodeFolding
    property CodeFolding;
    property UseCodeFolding;
//-- CodeFolding
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property DisplayFlowControl;
    property FontQuality default fqClearTypeNatural;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property IndentGuides;
    property ImeMode;
    property ImeName;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property Lines;
    property MaxUndo;
    property Options;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollHintColor;
    property ScrollHintFormat;
    property ScrollBars;
    property ScrollbarAnnotations;
    property SearchEngine;
    property SelectedColor;
    property TabWidth;
    property VisibleSpecialChars;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property WordWrapGlyph;
    // TCustomSynEdit events
    property OnChange;
    property OnClearBookmark;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnContextPopup;
    property OnDropFiles;
    property OnGutterClick;
    property OnGutterGetText;
    property OnMouseCursor;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnShowHint;
    property OnScroll;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;
    property OnTripleClick;
    property OnQuadrupleClick;
    property OnSearchNotFound;
    property OnZoom;
//++ CodeFolding
    property OnScanForFoldRanges;
//-- CodeFolding
  end;

implementation

{$R SynEdit.res}

uses
  System.Types,
  System.Character,
  Winapi.ShellAPI,
  Vcl.Consts,
  Vcl.Clipbrd,
  Vcl.IMouse,
  SynAccessibility,
  SynEditScrollBars,
  SynEditUndo,
  SynEditWordWrap,
  SynEditStrConst,
  SynEditDataObject,
  SynEditDragDrop,
  SynEditSearch;

{ THookedCommandHandlerEntry }

type
  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: pointer;
    constructor Create(AEvent: THookedCommandEvent; AData: pointer);
    function Equals(AEvent: THookedCommandEvent): Boolean; reintroduce;
  end;

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: pointer);
begin
  inherited Create;
  fEvent := AEvent;
  fData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): Boolean;
begin
  with TMethod(fEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

{ TCustomSynEdit }

function TCustomSynEdit.PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
// Same as PixelsToRowColumn but don't return a partially visible last line
begin
  aY := MinMax(aY, 0, fLinesInWindow * fTextHeight - 1);
  Result := PixelsToRowColumn(aX, aY);
end;

function TCustomSynEdit.PixelsToColumn(P: PChar; Len: Integer; aX: Integer;
  CharBefore: Boolean = False): Integer;
{ Returns the character index at given pixel position aX when the text is
  rendered with the SynEdit TextFormat.
  If CharBefore is true you always get the character at or before the pixel
  position, othwerwise you get the nearest character}
var
  Layout: TSynTextLayout;
  HTM: TDwriteHitTestMetrics;
  IsTrailing, IsInside: LongBool;
  P2, PStart, PEnd: PChar;
  W : Integer;
  CopyS: string;
begin
  if (Len = 0) or (aX <= 0) then
    Result := Max((ax div fCharWidth) + 1, 1)
  else
  begin
    if scControlChars in FVisibleSpecialChars then
    begin
      SetString(CopyS, P, Len);
      SubstituteControlChars(CopyS);
      P := PChar(CopyS);
    end;

    PStart := P;
    PEnd := P + Len;
    W := 0;

    while (P < PEnd) and (W < aX) do
    begin
      while (P < PEnd) and (W < aX) do
      begin
        case P^ of
           #9: Inc(W, fTabWidth * fCharWidth - W mod (fTabWidth * fCharWidth));
           #32..#126, #$00A0: Inc(W, FCharWidth);
         else
           break;
         end;
         Inc(P);
      end;
      if not CharBefore and ((P = PEnd) or (W >= aX)) and (W <= aX + fCharWidth div 2) then
        Inc(P);

      if (P >= PEnd) or (W >= aX) then Break;

      // Just in case P is followed by combining characters
      if (P > PStart) and not (Word((P-1)^) in [9, 32]) then
      begin
        Dec(P);
        Dec(W, FCharWidth);
      end;

      // Measure non-ascii text code points
      P2 := P;
      while P2 < PEnd do
      begin
        Inc(P2);
        if Word(P2^) in [9, 65..90, 97..122] then Break;
      end;

      Layout.Create(FTextFormat, P, P2-P, MaxInt, fTextHeight);
      CheckOSError(Layout.IDW.HitTestPoint(aX - W,
        fTextHeight div 2, IsTrailing, IsInside, HTM));

      Inc(W, Round(HTM.left + HTM.width));
      if IsInside then
      begin
        Inc(P, Integer(HTM.textPosition) +
          IfThen(not CharBefore and IsTrailing, HTM.length + 1, 1));
        Break;
      end
      else
        P := P2;
    end;
    Result := P - PStart;
    if (P >= PEnd) and (ax > W) then
      Inc(Result, Round((ax - W) / fCharWidth))
  end;
end;

function TCustomSynEdit.PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
var
  S: string;
begin
  Result.Row := MinMax(TopLine + (aY div fTextHeight), 1, DisplayRowCount);
  S := Rows[Result.Row];
  Result.Column := PixelsToColumn(PChar(S), S.Length, ax - fTextOffset);
end;

function TCustomSynEdit.ColumnToPixels(const S: string; Col: Integer): Integer;
var
  Layout: TSynTextLayout;
  HTM: TDwriteHitTestMetrics;
  P, P2, PStart, PEnd, PCol: PChar;
  X, Y: Single;
  CopyS: string;
begin
  if scControlChars in FVisibleSpecialChars then
  begin
    CopyS := S;
    SubstituteControlChars(CopyS);
    P := PChar(CopyS);
    PEnd := P + CopyS.Length;
  end
  else
  begin
    P := PChar(S);
    PEnd := P + S.Length;
  end;


  PStart := P;
  PCol := P + Col - 1;
  Result := 0;

  while P < PCol do
  begin
    while P < PCol do
    begin
      case P^ of
         #9: Inc(Result, fTabWidth * fCharWidth - Result mod (fTabWidth * fCharWidth));
         #32..#126, #$00A0: Inc(Result, FCharWidth);
     else
         break;
       end;
       Inc(P);
    end;

    if P >= PCol then Break;

    // Just in case P is followed by combining characters
    if (P > PStart) and not (Word((P-1)^) in [9, 32]) then
    begin
      Dec(P);
      Dec(Result, FCharWidth);
    end;
    // Measure non-ascii text code points
    P2 := P;
    while P2 < PEnd do
    begin
      Inc(P2);
      if Word(P2^) in [9, 65..90, 97..122] then Break;
    end;
    Layout.Create(FTextFormat, P, P2-P, MaxInt, fTextHeight);
    if P2 < PCol then
    begin
      P := P2;
      Inc(Result, Round(Layout.TextMetrics.widthIncludingTrailingWhitespace));
    end
    else
    begin
      CheckOSError(Layout.IDW.HitTestTextPosition(PCol - P, False, X, Y, HTM));
      Inc(Result, Round(X));
      Break;
    end;
  end;
end;

function TCustomSynEdit.RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
var
  S: string;
begin
  Result.Y := (RowCol.Row - fTopLine) * fTextHeight;

  S := Rows[RowCol.Row];
  if RowCol.Column = 1 then
    Result.X := 0
  else if S = '' then
    Result.X := (RowCol.Column - 1) * fCharWidth
  else if RowCol.Column > S.Length then
     Result.X := TextWidth(S) + (RowCol.Column - S.Length - 1) * fCharWidth
  else
    Result.X := ColumnToPixels(S, RowCol.Column);
  Inc(Result.X, fTextOffset);
end;

function TCustomSynEdit.ValidTextPos(const S: String; Index: Integer;
  Trailing: Boolean): Integer;
var
  Layout: TSynTextLayout;
  X, Y: single;
  P, PStart, PEnd: PChar;
  HTM: TDwriteHitTestMetrics;
begin
  if not InRange(Index, 2, S.Length) or (Word(S[Index]) in [9, 32..126]) then
    Exit(Index);

  PStart := PChar(S);
  PEnd := PStart + S.Length - 1;
  P := PStart + Index - 1;

  // Include at least one more character before
  Dec(P);
  while (P > PStart) and not (Word(P^) in [9, 32..126]) do
    Dec(P);
  PStart := P;
  P := PChar(S) + Index - 1;
  // Add characters after
  while (P < PEnd) and not (Word((P + 1)^) in [9, 32..126]) do
    Inc(P);

  Layout.Create(FTextFormat, PStart, P - PStart + 1, MaxInt, fTextHeight);
  CheckOSError(Layout.IDW.HitTestTextPosition(PChar(S) + Index - PStart - 1,
    False, X, Y, HTM));

  if Index + PChar(S) - PStart = Integer(HTM.textPosition) + 1 then
    Result := Index
  else
    Result := PStart - PChar(S) +  IfThen(Trailing and (HTM.length > 1),
      HTM.textPosition + HTM.length + 1, HTM.textPosition + 1);
end;

function TCustomSynEdit.ValidTextPos(BC: TBufferCoord;
  Trailing: Boolean): TBufferCoord;
{  Accounts for surrogate pairs and combining diacritics }
begin
  Result := BC;
  if not InRange(BC.Line, 1, Lines.Count) then Exit;
  Result.Char := ValidTextPos(Lines[BC.Line - 1], BC.Char, Trailing);
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
{ X,Y are pixel coordinates }
var
  CaretNearestPos : TDisplayCoord;
begin
  CaretNearestPos := PixelsToNearestRowColumn(X, Y);
  DisplayXY := CaretNearestPos;
end;

procedure TCustomSynEdit.ComputeScroll(X, Y: Integer);
{ X,Y are pixel coordinates }
var
  iScrollBounds: TRect; // relative to the client area
  ScrollAreaSize : integer;
const
  ScrollAreaDefaultSize = 4;
begin
  fScrollDeltaX := 0;
  fScrollDeltaY := 0;

  iScrollBounds := Bounds(fGutterWidth, 0, FTextAreaWidth,
    fLinesInWindow * fTextHeight);

  ScrollAreaSize := 0;
  if sfOleDragSource in fStateFlags then
    Inc(ScrollAreaSize, ScrollAreaDefaultSize);
  if BorderStyle = bsNone then
    Inc(ScrollAreaSize, 2);

  InflateRect(iScrollBounds,
    -MulDiv(ScrollAreaSize, FCurrentPPI, 96),
    -MulDiv(ScrollAreaSize, FCurrentPPI, 96));

  if (X < iScrollBounds.Left) and (LeftChar > 1) then
    fScrollDeltaX := (X - iScrollBounds.Left) div fCharWidth - 1
  else if X >= iScrollBounds.Right then
    fScrollDeltaX := (X - iScrollBounds.Right) div fCharWidth + 1;

  if (Y < iScrollBounds.Top) and (TopLine > 1) then
    fScrollDeltaY := (Y - iScrollBounds.Top) div fTextHeight - 1
  else if Y >= iScrollBounds.Bottom then
    fScrollDeltaY := (Y - iScrollBounds.Bottom) div fTextHeight + 1;

  fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
end;

procedure TCustomSynEdit.CopyToClipboard;
begin
  OleSetClipboard(TSynEditDataObject.Create(Self));
end;

procedure TCustomSynEdit.CutToClipboard;
begin
  if not ReadOnly then
  begin
    BeginUndoBlock;
    try
      CopyToClipboard;
      DeleteSelections;
    finally
      EndUndoBlock;
    end;
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
var
  fFontDummy: TFont;
begin
  inherited Create(AOwner);
  fLines := TSynEditStringList.Create(TextWidth);
  fOrigLines := fLines;
  with TSynEditStringList(fLines) do
  begin
    OnChange := LinesChanged;
    OnChanging := LinesChanging;
    OnCleared := ListCleared;
    OnBeforeDeleted := ListBeforeDeleted;
    OnDeleted := ListDeleted;
    OnInserted := ListInserted;
    OnPut := ListPut;
  end;
  fUndoRedo := CreateSynEditUndo(Self);
  fUndoRedo.OnModifiedChanged := ModifiedChanged;
  fOrigUndoRedo := fUndoRedo;

  DoubleBuffered := False;
  fActiveLineColor := clNone;
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorChanged;
  FIndentGuides := TSynIndentGuides.Create;
  FIndentGuides.OnChange := IndentGuidesChanged;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;
  fTextMargin := 3;
  // fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create(Self);
  fGutter.OnChange := GutterChanged;
  fWordWrapGlyph := TSynGlyph.Create(HINSTANCE, 'SynEditWrapped');
  fWordWrapGlyph.OnChange := WordWrapGlyphChange;
  FIndicators := TSynIndicators.Create(Self);
  // Brackets Highlight
  FBracketsHighlight := TSynBracketsHighlight.Create(Self);
  // Scrollbar Annotations
  FScrollbarAnnotations := TSynScrollbarAnnotations.Create(Self, TSynScrollbarAnnItem);
  FScrollbarAnnotations.SetDefaultAnnotations;

  ControlStyle := ControlStyle + [csOpaque, csSetCaption, csNeedsBorderPaint, csPannable];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clWindow;
  fExtraLineSpacing := 2;
  fFontQuality := fqClearTypeNatural;
  fFontDummy := TFont.Create;
  try
    fFontDummy.Name := DefaultFontName;
    fFontDummy.Size := 10;
    fFontDummy.CharSet := DEFAULT_CHARSET;
    fFontDummy.Quality := fFontQuality;
    Font.Assign(fFontDummy);
    Font.PixelsPerInch := Screen.DefaultPixelsPerInch;
    Font.Size := fFontDummy.Size;
  finally
    fFontDummy.Free;
  end;
  {$if CompilerVersion >= 36}
  Font.IsDPIRelated := True;
  {$ifend CompilerVersion >= 36}
  Font.OnChange := SynFontChanged;
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  fInserting := True;
  fScrollBars := ssBoth;
  fBorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  fFocusList := TList.Create;
  fKbdHandler := TSynEditKbdHandler.Create;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  fMarkList := TSynEditMarkList.Create(Self);
  fMarkList.OnNotify := MarkListNotify;
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
  fWantReturns := True;
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  FLastPosX := 0;
  FCarets := TSynCarets.Create(Canvas);
  var BC := BufferCoord(1, 1);
  FSelection := TSynSelection.Create(BC, BC, BC);
  FSelections := TSynSelections.Create(Self);
  FSelections.AddCaret(BC, True);

  fOptions := SYNEDIT_DEFAULT_OPTIONS;

  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := ScrollTimerHandler;

  fScrollHintColor := clInfoBk;
  fScrollHintFormat := shfTopLineOnly;

  FSynEditScrollBars := CreateSynEditScrollBars(Self);

  FDisplayFlowControl := TSynDisplayFlowControl.Create;

//++ CodeFolding
  fCodeFolding := TSynCodeFolding.Create;
  fCodeFolding.OnChange := OnCodeFoldingChange;
  fAllFoldRanges := TSynFoldRanges.Create;
//-- CodeFolding
  SynFontChanged(nil);
  GutterChanged(nil); // to caclulate fGutterWidth also updates fTextOffset
  RegisterCommandHandler(InternalCommandHook, nil);
end;

procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  // Clear WindowText to avoid it being used as Caption, or else window creation will
  // fail if it's bigger than 64KB. It's useless to set the Caption anyway.
  StrDispose(WindowText);
  WindowText := nil;
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or BorderStyles[fBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;

  end;
end;

procedure TCustomSynEdit.DecPaintLock;
begin
  Assert(fPaintLock > 0);
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then
  begin
    if (fStatusChanges * [scCaretX, scCaretY, scSelection] <> []) or
      (sfCaretChanged in fStateFlags)
    then
      FSelections.ActiveSelection := FSelection;
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    if sfCaretChanged in fStateFlags then
      UpdateCarets;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

destructor TCustomSynEdit.Destroy;
begin
//  if Assigned(FUIAutomationProvider) then
//    UiaDisconnectProvider(IRawElementProviderSimple(FUIAutomationProvider));

  Highlighter := nil;
  if (fChainedEditor <> nil) or (fLines <> fOrigLines) then
    RemoveLinesPointer;
  UnregisterCommandHandler(InternalCommandHook);

  inherited Destroy;
  // free listeners while other fields are still valid
  // do not use FreeAndNil, it first nils and then freey causing problems with
  // code accessing fHookedCommandHandlers while destruction
  fHookedCommandHandlers.Free;
  fHookedCommandHandlers := nil;
  // do not use FreeAndNil, it first nils and then frees causing problems with
  // code accessing fPlugins while destruction
  fPlugins.Free;
  fPlugins := nil;

  fMarkList.Free;
  fBookMarkOpt.Free;
  fKeyStrokes.Free;
  fKbdHandler.Free;
  fFocusList.Free;
  fSelectedColor.Free;
  FIndentGuides.Free;
  fUndoRedo := nil;
  fOrigUndoRedo := nil;
  fGutter.Free;
  fWordWrapGlyph.Free;
  FScrollbarAnnotations.Free;
  FBracketsHighlight.Free;
  FIndicators.Free;
  fOrigLines.Free;
  fCodeFolding.Free;
  fAllFoldRanges.Free;
  FSelections.Free;
  FCarets.Free;
  FDisplayFlowControl.Free;
  Mouse.PanningWindow := nil;
end;

function TCustomSynEdit.GetBlockBegin: TBufferCoord;
{ Normalizes BlockBegin/End }
begin
  Result := FSelection.Normalized.Start;
end;

function TCustomSynEdit.GetBlockEnd: TBufferCoord;
{ Normalizes BlockBegin/End }
begin
  Result := FSelection.Normalized.Stop;
end;

procedure TCustomSynEdit.SynFontChanged(Sender: TObject);
begin
  Font.OnChange := nil;  // avoid recursion
  Font.Quality := FontQuality;

  // revert to default font if not monospaced or invalid
  if not IsFontMonospacedAndValid(Font) then
    Font.Name := DefaultFontName;
  Font.OnChange := SynFontChanged;

  // Create DirectWrite text format
  FTextFormat.Create(Font, fTabWidth, 0, fExtraLineSpacing);
  fTextHeight := FTextFormat.LineHeight;
  fCharWidth := FTextFormat.CharWidth;

  // We need to recalculate line widths
  TSynEditStringList(fLines).ResetMaxWidth;
  if fGutter.ShowLineNumbers and not fGutter.UseFontStyle then
    GutterChanged(Self);

  // Invalidate and handle the changes
  SizeOrFontChanged(True);

  // Used in Zoom
  FOrigFontSize := Font.Size;
end;

function TCustomSynEdit.GetLineText: string;
begin
  if InRange(CaretY, 1, Lines.Count) then
    Result := Lines[CaretY - 1]
  else
    Result := '';
end;

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := not FSelection.IsEmpty;
end;

function TCustomSynEdit.GetSelText: string;
begin
  Result := '';
  if not FSelection.IsEmpty then
    Result := SelectionText(FSelection);
end;

function TCustomSynEdit.SelectionText(Sel: TSynSelection): string;

  procedure CopyAndForward(const S: string; Index, Count: Integer; var P:
    PWideChar);
  var
    pSrc: PWideChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then
    begin
      Dec(Index);
      pSrc := PWideChar(S) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen * sizeof(WideChar));
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
  P: PWideChar;
begin
  if Sel.IsEmpty then
    Result := ''
  else begin
    Sel.Normalize;
    ColFrom := Sel.Start.Char;
    First := Sel.Start.Line - 1;
    //
    ColTo := Sel.Stop.Char;
    Last := Sel.Stop.Line - 1;

    if (First = Last) then
      Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
    else begin
      // step1: calculate total length of result string
      TotalLen := Max(0, Length(Lines[First]) - ColFrom + 1);
      for i := First + 1 to Last - 1 do
        Inc(TotalLen, Length(Lines[i]));
      Inc(TotalLen, ColTo - 1);
      Inc(TotalLen, Length(SLineBreak) * (Last - First));
      // step2: build up result string
      SetLength(Result, TotalLen);
      P := PWideChar(Result);
      CopyAndForward(Lines[First], ColFrom, MaxInt, P);

      CopyAndForward(SLineBreak, 1, MaxInt, P);

      for i := First + 1 to Last - 1 do
      begin
        CopyAndForward(Lines[i], 1, MaxInt, P);
        CopyAndForward(SLineBreak, 1, MaxInt, P);
      end;
      CopyAndForward(Lines[Last], 1, ColTo - 1, P);
    end;
  end;
end;

function TCustomSynEdit.SynGetText: string;
begin
  Result := Lines.Text;
end;

procedure TCustomSynEdit.ForceCaretX(aCaretX: integer);
{ Can place CaretX beyond the end of line }
var
  vRestoreScroll: boolean;
begin
  vRestoreScroll := not (eoScrollPastEol in fOptions);
  Include(fOptions, eoScrollPastEol);
  try
    InternalCaretX := aCaretX;
  finally
    if vRestoreScroll then
      Exclude(fOptions, eoScrollPastEol);
  end;
end;

function TCustomSynEdit.GetWordAtCursor: string;
begin
   Result:=GetWordAtRowCol(CaretXY);
end;

procedure TCustomSynEdit.IndentGuidesChanged(Sender: TObject);
begin
  InvalidateLines(-1, -1);
end;

procedure TCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterBand(Kind: TSynGutterBandKind);
var
  Band: TSynGutterBand;
  Left: Integer;
begin
   if not Gutter.Visible then Exit;
   Band := Gutter.Band[Kind];
   if not Assigned(Band) or not Band.Visible then Exit;

   Left := Band.LeftX;
   InvalidateRect(Rect(Left, 0, Left + Band.RealWidth, ClientHeight) ,False)
end;

procedure TCustomSynEdit.InvalidateGutterLine(aLine: Integer);
begin
  if (aLine < 1) or (aLine > Lines.Count) then
    Exit;

  InvalidateGutterLines(aLine, aLine);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := Rect(0, 0, fGutterWidth, ClientHeight);
      InvalidateRect(rcInval, False);
    end
    else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);
      if UseCodeFolding or WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        if LastLine <= Lines.Count then
          LastLine := LineToRow(LastLine)
        else
          LastLine := MaxInt;
      end;
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);
      { any line visible? }
      if (LastLine >= FirstLine) then
      begin
        rcInval := Rect(0, fTextHeight * (FirstLine - TopLine),
          fGutterWidth, fTextHeight * (LastLine - TopLine + 1));
        InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
// note: FirstLine and LastLine don't need to be in correct order
var
  rcInval: TRect;
begin
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then
    begin
      rcInval := ClientRect;
      Inc(rcInval.Left, fGutterWidth);
      InvalidateRect(rcInval, False);
    end
    else begin
      FirstLine := Max(FirstLine, 1);
      LastLine := Max(LastLine, 1);
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);

      if LastLine > Lines.Count then
        LastLine := MaxInt; // paint empty space beyond last line

      if UseCodeFolding or WordWrap then
      begin
        FirstLine := LineToRow(FirstLine);
        if LastLine <= Lines.Count then begin
          if UseCodeFolding then
            LastLine := LineToRow(LastLine)
          else
            LastLine := LineToRow(LastLine + 1) - 1;
        end;
      end;

      // TopLine is in display coordinates, so FirstLine and LastLine must be
      // converted previously.
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine, TopLine + LinesInWindow);

      { any line visible? }
      if (LastLine >= FirstLine) then
      begin
        rcInval := Rect(fGutterWidth, fTextHeight * (FirstLine - TopLine),
          ClientWidth, fTextHeight * (LastLine - TopLine + 1));
        InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateSelection;
begin
  InvalidateRange(BlockBegin, BlockEnd);
end;

procedure TCustomSynEdit.InvalidateSelection(const Sel: TSynSelection);
begin
  if Sel.IsEmpty then
    InvalidateLine(Sel.Caret.Line)
  else
    InvalidateRange(Sel.Start, Sel.Stop);
end;

procedure TCustomSynEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  CharCode: Integer;
begin
  { The following allows the entering of Unicode character codes using the
    Alt + Numpad numbers combination.  When the charcode is less than 256
    this is handled by Windows.
  }
  if (ssAlt in Shift) and (Key >= VK_NUMPAD0) and (Key <= VK_NUMPAD9) then
    FCharCodeString := FCharCodeString + IntToStr(Key - VK_NUMPAD0);

  if Key = VK_MENU then
  begin
    if (FCharCodeString <> '') and TryStrToInt(FCharCodeString, CharCode) and
      (CharCode >= 256) and (CharCode <= 65535) then
    begin
      SendMessage(Handle, WM_CHAR, CharCode, 0);
      FIgnoreNextChar := True;
    end;
    FCharCodeString := '';
  end;

  inherited;
  fKbdHandler.ExecuteKeyUp(Self, Key, Shift);
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: WideChar;
  Cmd: TSynEditorCommand;
begin
  inherited;
  fKbdHandler.ExecuteKeyDown(Self, Key, Shift);

  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end
    else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
end;

procedure TCustomSynEdit.Loaded;
var
  OldUseCodeFolding : Boolean;
begin
  inherited Loaded;

  // See SetUseCodeFolding
  OldUseCodeFolding := fUseCodeFolding;
  UseCodeFolding := False;
  UseCodeFolding := OldUseCodeFolding;

  GutterChanged(Self);
  UpdateScrollBars;
end;

procedure TCustomSynEdit.KeyPress(var Key: Char);
begin
  if FIgnoreNextChar then
  begin
    FIgnoreNextChar := False;
    Exit;
  end;

  inherited;  // Calls the OnKeyPress event handler if present
  if Key = #0 then Exit;

  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then
  begin
    fKbdHandler.ExecuteKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
  end
  else
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
end;

function TCustomSynEdit.GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): string;
begin
  if WantTabs and not(eoTabsToSpaces in Options) and (CharCount >= TabWidth) then
    Result := StringofChar(#9, CharCount div TabWidth) +
      StringofChar(#32, CharCount mod TabWidth)
  else
    Result := StringofChar(#32, CharCount);
end;

procedure TCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
begin
  DoLinesChanged;

  if (sfLinesChanging in fStateFlags) and UseCodeFolding and
    fAllFoldRanges.StopScanning(fLines)
  then
  begin
    if FIndentGuides.Visible and FIndentGuides.StructureHighlight and
      Assigned(fHighlighter) and (hcStructureHighlight in fHighlighter.Capabilities)
    then
      InvalidateLines(-1, -1);
    InvalidateGutterBand(gbkFold);
    UpdateScrollbars;
  end;

  Exclude(fStateFlags, sfLinesChanging);
  if HandleAllocated then
  begin
//++ Flicker Reduction
//    UpdateScrollBars;
//-- Flicker Reduction
    //SetBlockBegin(CaretXY);
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
  HighlightBrackets;
  DoChange;

  if Assigned(FUIAutomationProvider) and UiaClientsAreListening
  then
    TThread.ForceQueue(nil, procedure
    begin
      UiaRaiseAutomationEvent(IRawElementProviderSimple(FUIAutomationProvider),
        UIA_Text_TextChangedEventId);
    end);
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  bWasSel: Boolean;
  P : TPoint;
  // Ole drag drop
  DragSource : IDropSource;
  DataObject : IDataObject;
  dwEffect : integer;
begin
  // If Button = mbLeft MouseCapture is set by TControl.WMLButtonDown
  inherited MouseDown(Button, Shift, X, Y);

  //remember selection state, as it will be cleared later
  bWasSel := SelAvail;

  if (Button = mbLeft) {and ((Shift + [ssDouble]) = [ssLeft, ssDouble])} then
  begin
    if (FClickCount > 0)
       and (Abs(fMouseDownX - X) < GetSystemMetrics(SM_CXDRAG))
       and (Abs(fMouseDownY - Y) < GetSystemMetrics(SM_CYDRAG))
       and (fClickCountTimer.ElapsedMilliseconds < GetDoubleClickTime )
    then
      Inc(fClickCount)
    else
      fClickCount:= 1;
    fMouseDownX := X;
    fMouseDownY := Y;
    if fClickCount = 3 then TripleClick;
    if fClickCount = 4 then QuadrupleClick;
    fClickCountTimer := TStopWatch.StartNew;
  end else
    fClickCount := 0;

  if (Button = mbLeft) and (fClickCount > 1) then
  begin
    // Deal with overlapping selections
    Selections.MouseSelection(FSelection);
    Exit;
  end;

  fKbdHandler.ExecuteMouseDown(Self, Button, Shift, X, Y);

  // Check for drag and drop
  if (Button = mbLeft) and (FSelections.Count = 1) then
  begin
    //if mousedown occurred in selected block begin drag operation
    if bWasSel and (eoDragDropEditing in fOptions)
      and (X >= fGutterWidth + fTextMargin)
      and ([ssAlt, ssLeft] * Shift = [ssLeft])
      and IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y))) then
    begin
      if DragDetect(Handle, Point(X,Y)) then begin
        DataObject := TSynEditDataObject.Create(Self);
        DragSource := TSynDragSource.Create;
        try
          Include(fStateFlags, sfOleDragSource);
          DoDragDrop(DataObject, DragSource, DROPEFFECT_COPY or DROPEFFECT_MOVE, dwEffect);
          DataObject := nil;
        finally
          Exclude(fStateFlags, sfOleDragSource);
          if dwEffect = DROPEFFECT_MOVE then
            SelText := '';
        end;
        Exit;
      end else begin
        if csLButtonDown in ControlState then
        begin
          GetCursorPos(P);
          PostMessage(Handle, WM_LBUTTONUP, 0, PointToLParam(ScreenToClient(P)));
        end;
      end;
    end;
  end;

  if (ssDouble in Shift) or (Button = mbMiddle) or ((Button = mbRight)
    and (not (eoRightMouseMovesCursor in Options) or (SelAvail and
    IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y))))))
  then
    Exit;

  IncPaintLock;
  try
    MoveDisplayPosAndSelection(PixelsToNearestRowColumn(X,Y),
      [ssAlt, ssShift] * Shift = [ssShift]);

    if [ssAlt, ssShift] * Shift = [ssAlt, ssShift] then
    begin
      InvalidateSelection(FSelection);
      FSelections.ColumnSelection(FSelections.BaseSelection.Start, CaretXY, FLastPosX)
    end
    else if ssAlt in Shift then
      FSelections.AddCaret(FSelection.Caret)
    else
    begin
      FSelections.ActiveSelection := FSelection;
      FSelections.Clear(ksKeepActive);
    end;
  finally
    DecPaintLock;
  end;

  if (X < fGutterWidth) then
    Include(fStateFlags, sfPossibleGutterClick);
  if (sfPossibleGutterClick in fStateFlags) and (Button = mbRight) then
  begin
    DoOnGutterClick(Button, X, Y)
  end;

  SetFocus;
  Winapi.Windows.SetFocus(Handle);
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TDisplayCoord;
  BC: TBufferCoord;
begin
  inherited MouseMove(Shift, x, y);
  if (ssLeft in Shift) and MouseCapture and not IsScrolling then
  begin
    // should we begin scrolling?
    ComputeScroll(X, Y);
    { compute new caret }
    P := PixelsToNearestRowColumn(X, Y);
    P.Row := MinMax(P.Row, 1, DisplayRowCount);
//  Not sure what was the purpose of these
//    if fScrollDeltaX <> 0 then
//      P.Column := DisplayX;
//    if fScrollDeltaY <> 0 then
//      P.Row := DisplayY;
    BC := DisplayToBufferPos(P);

    if BC = CaretXY then Exit;  // no movement

    if [ssAlt, ssShift] * Shift = [ssAlt, ssShift] then
    begin
      // Column selection
      IncPaintLock;
      try
        MoveDisplayPosAndSelection(P, True);
        InvalidateSelection(FSelection);
        FSelections.ColumnSelection(FSelections.BaseSelection.Start, CaretXY, FLastPosX);
      finally
        DecPaintLock;
      end;
    end
    else
    begin
      if fClickCount = 2 then
        DoMouseSelectWordRange(BC)
      else if fClickCount = 3 then
        DoMouseSelectLineRange(BC)
      else
        MoveDisplayPosAndSelection(P, True);
      // Deal with overlapping selections
      Selections.MouseSelection(FSelection);
    end;

    if (sfPossibleGutterClick in fStateFlags) and (FSelection.Start.Line <> CaretXY.Line) then
      Include(fStateFlags, sfGutterDragging);
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  iMousePos: TPoint;
  DC: TDisplayCoord;
  BC: TBufferCoord;
  X, Y: Integer;
begin
  GetCursorPos( iMousePos );
  iMousePos := ScreenToClient( iMousePos );
  DC := PixelsToRowColumn( iMousePos.X, iMousePos.Y );
  DC.Row := MinMax(DC.Row, 1, DisplayRowCount);
  DC.Column := MinMax(DC.Column, 1, RowLength[DC.Row] + 1);

  IncPaintLock;
  try
    if fScrollDeltaX <> 0 then
    begin
      LeftChar := LeftChar + fScrollDeltaX;
      X := LeftChar;
      if fScrollDeltaX > 0 then  // scrolling right?
        Inc(X, FTextAreaWidth div FCharWidth);
      DC.Column := X;
    end;
    if fScrollDeltaY <> 0 then
    begin
      if GetKeyState(SYNEDIT_SHIFT) < 0 then
        Y := TopLine + fScrollDeltaY * LinesInWindow
      else
        Y := TopLine + fScrollDeltaY;
      if fScrollDeltaY > 0 then  // scrolling down?
        Inc(Y, LinesInWindow - 1);
      DC.Row := MinMax(Y, 1, DisplayRowCount);
    end;

    BC := DisplayToBufferPos(DC);
    if CaretXY <> BC then
    begin
      if MouseCapture and (fClickCount = 2) then
        // Word selection
        DoMouseSelectWordRange(BC)
      else if MouseCapture and (fClickCount = 3) then
        // Line selection
        DoMouseSelectLineRange(BC)
      else if MouseCapture then
        // if MouseCapture is True we are selecting with the mouse
        MoveDisplayPosAndSelection(DC, True)
      else
        // Ole dragging
        InternalCaretXY := BC;

      // Deal with overlapping selections
      Selections.MouseSelection(FSelection);
    end;
  finally
    DecPaintLock;
  end;

  ComputeScroll(iMousePos.x, iMousePos.y);
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Var
  // CodeFolding
  ptLineCol: TBufferCoord;
  ptRowCol: TDisplayCoord;
  Index: Integer;
  Rect: TRect;
begin
  // If Button = mbLeft MouseCapture is stopped by TControl.WMLButtonUp
  inherited MouseUp(Button, Shift, X, Y);
  fKbdHandler.ExecuteMouseUp(Self, Button, Shift, X, Y);

  fScrollTimer.Enabled := False;
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  MouseCapture := False;
  if (fStateFlags * [sfPossibleGutterClick, sfGutterDragging] = [sfPossibleGutterClick])
    and (X < fGutterWidth) and (Button <> mbRight) then
  begin
    DoOnGutterClick(Button, X, Y)
  end;
  Exclude(fStateFlags, sfPossibleGutterClick);
  Exclude(fStateFlags, sfGutterDragging);
  // CodeFolding
  ptRowCol := PixelsToRowColumn(X, Y);
  ptLineCol := DisplayToBufferPos(ptRowCol);

  if UseCodeFolding and CodeFolding.ShowHintMark and
    fAllFoldRanges.CollapsedFoldStartAtLine(ptLineCol.Line, Index) then
  begin
    Rect := GetCollapseMarkRect(ptRowCol.Row, ptLineCol.Line);
    if PtInRect(Rect, Point(X,Y)) then
      Uncollapse(Index);
    end;
end;

procedure TCustomSynEdit.DoOnGutterClick(Button: TMouseButton; X, Y: Integer);
var
  I     : Integer;
  Offs  : Integer;
  Line  : Integer;
  Allmrk: TSynEditMarks;
  Mark  : TSynEditMark;
  RowColumn: TDisplayCoord;
  Band: TSynGutterBand;
begin
  RowColumn := PixelsToRowColumn(X, Y);
  Line := RowToLine(RowColumn.Row);

  if Line <= Lines.Count then
  begin
    Band := FGutter.BandAtX(X);
    if Assigned(Band) then
      Band.DoClick(Self, Button, X, Y, RowColumn.Row, Line);
    if Assigned(fOnGutterClick) then
    begin
      // Check gutter marks
      Marks.GetMarksForLine(Line, Allmrk);
      Offs := 0;
      Mark := nil;
      for I := 1 to MAX_MARKS do
      begin
        if assigned(Allmrk[I]) then
        begin
          Inc(Offs, BookMarkOptions.XOffset);
          if X < Offs then
          begin
            Mark := Allmrk[I];
            break;
          end;
        end;
      end; //for
      fOnGutterClick(Self, Button, X, Y, Line, Mark);
    end;
  end;
end;

procedure TCustomSynEdit.DoOnMouserCursor(const aLineCharPos: TBufferCoord;
  var aCursor: TCursor);
begin
  if Assigned(fOnMouseCursor) then
    fOnMouseCursor(Self, aLineCharPos, aCursor);
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2: Integer;
  RT: ID2D1DCRenderTarget;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;
  if rcClip.IsEmpty then Exit;

  // lines
  nL1 := Max(TopLine + rcClip.Top div fTextHeight, TopLine);
  nL2 := MinMax(TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight,
    1, DisplayRowCount);

  //Create the RenderTarget
  RT := TSynDWrite.RenderTarget;
  RT.BindDC(Canvas.Handle, rcClip);
  RT.BeginDraw;
  RT.SetTransform(TD2DMatrix3X2F.Translation(-rcClip.Left, -rcClip.Top));

  // First paint the gutter area if it was (partly) invalidated.
  if (rcClip.Left < fGutterWidth) then
  begin
    rcDraw := rcClip;
    rcDraw.Right := fGutterWidth;
    PaintGutter(RT, rcDraw, nL1, nL2);
  end;

  // Then paint the text area if it was (partly) invalidated.
  if (rcClip.Right > fGutterWidth) then
  begin
    rcDraw := rcClip;
    rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
    PaintTextLines(RT, rcDraw, nL1, nL2);
  end;

  // If there was a problem rectreate the RenderTarget
  if RT.EndDraw <> S_OK then TSynDWrite.ResetRenderTarget;

  PluginsAfterPaint(Canvas, rcClip, nL1, nL2);

  // If there is a custom paint handler call it.
  DoOnPaint;
end;

procedure TCustomSynEdit.PaintGutter(RT: ID2D1RenderTarget; const AClip: TRect;
  const aFirstRow, aLastRow: Integer);
var
  I, L, W: Integer;
  rcBackGround: TRect;
  Band: TSynGutterBand;
  rcBand: TRect;
  EdBkgrColor: TColor;
  Attri: TSynHighlighterAttributes;
  Brush: ID2D1Brush;
  GradientBrush: ID2D1LinearGradientBrush;
begin
  // First paint gutter background
  W := 0;
  for I := 0 to FGutter.Bands.Count - 1 do
  begin
    Band := FGutter.Bands[I];
    if not Band.Visible then Continue;
    if Band.Background = gbbGutter then
      Inc(W, FGutter.Bands[I].RealWidth)
    else
      Break;
  end;
  rcBackGround := Rect(AClip.Left, AClip.Top, W, AClip.Bottom);

  if fGutter.Gradient then
  begin
    GradientBrush := TSynDWrite.GradientGutterBrush(fGutter.GradientStartColor,
      fGutter.GradientEndColor);
    GradientBrush.SetEndPoint(Point(W, 0));
    Brush := GradientBrush;
  end
  else
    Brush := TSynDWrite.SolidBrush(fGutter.Color);
  RT.FillRectangle(rcBackGround, Brush);

  // Set Brush to Editor Background
  EdBkgrColor := Color;
  if Highlighter <> nil then
  begin
    Highlighter.ResetRange;
    Highlighter.SetLine('', 1);  // Workaround for SynHighlighterWeb
    Attri := Highlighter.WhitespaceAttribute;
    if (Attri <> nil) and (Attri.Background <> clNone) then
      EdBkgrColor := Attri.Background;
  end;
  Brush := TSynDWrite.SolidBrush(EdBkgrColor);

  L := 0;
  for I := 0 to FGutter.Bands.Count - 1 do
  begin
    Band := FGutter.Bands[I];
    if not Band.Visible then Continue;
    W := Band.RealWidth;
    if (L > AClip.Right) or (L + W < AClip.Left) then Continue;

    rcBand := Rect(L, AClip.Top, L + W, AClip.Bottom);
    if rcBand.IsEmpty then Continue;

    // Paint Bands with Editor Background
    if Band.Background = gbbEditor then
      RT.FillRectangle(rcBand, Brush);

    //And now paint the bands
    RT.PushAxisAlignedClip(rcBand, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
    Band.PaintLines(RT, rcBand, aFirstRow, aLastRow);
    RT.PopAxisAlignedClip;
    Inc(L, W);
  end;
end;

procedure TCustomSynEdit.PaintTextLines(RT: ID2D1RenderTarget; AClip: TRect;
  const aFirstRow, aLastRow: Integer);
var
  LinesRect: TRect;
  XRowOffset: Integer;

  function WhitespaceColor(Bkground: Boolean = True;
    ResetHighlighter: Boolean = False): TColor;
  var
    Attr: TSynHighlighterAttributes;
  begin
    if Bkground then
      Result := Color
    else
      Result := Font.Color;
    if fHighlighter <> nil then
    begin
      if ResetHighlighter then
      begin
        fHighlighter.ResetRange;
        fHighlighter.SetLine('', 1);  // workaround for SynHighlighterWeb
      end;
      Attr := Highlighter.WhitespaceAttribute;
      if Attr <> nil then
        if Bkground and (Attr.Background <> clNone) then
          Result := Attr.Background
        else if not Bkground and (Attr.Foreground <> clNone) then
          Result := Attr.Foreground;
    end;
  end;

  function IsRowFullySelected(Row: Integer; const RowStart: TBufferCoord): Boolean;
  var
    RowStop: TBufferCoord;
    Index: Integer;
    Sel: TSynSelection;
  begin
    if (eoNoSelection in FOptions) or (not Focused and FHideSelection) then
      Exit(False);

    if not FSelections.FindSelection(RowStart, Index) then
      Exit(False);

    RowStop := DisplayToBufferPos(DisplayCoord(1, Row + 1));
    Sel := FSelections[Index].Normalized;
    Result := Sel.Stop >= RowStop;
    if Result and WordWrap and (Sel.Stop = RowStop) and Sel.CaretAtEOL then
      Result := False;
  end;

  procedure FullRowColors(const RowStart: TBufferCoord; Row, Line: Integer;
    var FullRowBG, FullRowFG: TColor; var BGAlpha: TD2D1ColorF);
  { Return clNone to do normal processing of text foreground/background color }
  var
    IsLineSpecial: Boolean;
    IsFullySelected: Boolean;
    SpecialFG, SpecialBG: TColor;
  begin
    IsLineSpecial := DoOnSpecialLineColors(Line, SpecialFG, SpecialBG);
    IsFullySelected := IsRowFullySelected(Row, RowStart);

    BGAlpha := clNoneF;
    if IsFullySelected then
    begin
      if not fSelectedColor.FillWholeLines then
        IsFullySelected := False
      else if not SameValue(fSelectedColor.Alpha, 1) then
      begin
        BGAlpha := D2D1ColorF(fSelectedColor.Background, fSelectedColor.Alpha);
        IsFullySelected := False;
      end
    end;

    if IsFullySelected and IsLineSpecial then
    begin
      // Invert the colors as in Delphi
      FullRowFG := SpecialBG;
      FullRowBG := SpecialFG;
    end
    else if IsLineSpecial then
    begin
      FullRowBG := SpecialBG;
      if eoSpecialLineDefaultFg in FOptions then
        FullRowFG := clNone
      else
        FullRowFG := SpecialFG;
    end
    else if IsFullySelected then
    begin
      FullRowFG := fSelectedColor.Foreground;
      FullRowBG := fSelectedColor.Background;
    end
    else if (ActiveLineColor <> clNone) and  FSelections.RowHasCaret(Row, Line)
      and FSelections.IsEmpty
    then
    begin
      FullRowFG := clNone;
      FullRowBG := ActiveLineColor;
    end
    else
    begin
      FullRowFG := clNone;
      FullRowBG := clNone;
    end;
  end;

  function YRowOffset(const Row: Integer): Integer;
  begin
    Result := (Row - fTopLine) * fTextHeight;
  end;

  function GetTokenRect(Layout: TSynTextLayout;
    const Row , Start, Len: Integer): TRect;
  var
    X1, Y1, X2, Y2: Single;
    HitMetrics: TDwriteHitTestMetrics;
  begin
    Layout.IDW.HitTestTextPosition(Max(Start - 1, 0), False, X1, Y1, HitMetrics);
    Layout.IDW.HitTestTextPosition(Start + Len - 2, True, X2, Y2, HitMetrics);
    Result := Rect(fTextOffset + XRowOffset + Round(X1), YRowOffset(Row),
       fTextOffset + XRowOffset + Round(X2) + 1, YRowOffset(Row + 1));
  end;

  procedure PaintTokenBackground(Layout: TSynTextLayout;
    const Row , Start, Len: Integer; AColor: TD2D1ColorF);
  begin
    RT.FillRectangle(GetTokenRect(Layout, Row, Start, Len),
           TSynDWrite.SolidBrush(AColor));
  end;

  procedure DrawWhitespace(Layout: TSynTextLayout; const Row, Pos: Integer;
    Ch: Char; SpecialCharsColor: TColor);
  var
    WSLayout: TSynTextLayout;
    X1, Y1, X2, Y2: Single;
    HitMetrics: TDwriteHitTestMetrics;
    PrintGlyph: Char;
    Alignment: DWRITE_TEXT_ALIGNMENT;
  begin
    if (Ch = #9) then // Tab
      PrintGlyph := SynTabGlyph
    else if not Ch.IsControl then
      PrintGlyph := SynSpaceGlyph
    else
      Exit;
    Layout.IDW.HitTestTextPosition(Pos-1, False, X1, Y1, HitMetrics);
    Layout.IDW.HitTestTextPosition(Pos-1, True, X2, Y2, HitMetrics);
    WSLayout.Create(FTextFormat, @PrintGlyph, 1, Abs(Round(X2 - X1)), fTextHeight);

    Alignment := DWRITE_TEXT_ALIGNMENT_CENTER;
    if Ch = #9 then
    begin
      case SynTabAlignment of
        taLeftJustify: Alignment := DWRITE_TEXT_ALIGNMENT_LEADING;
        taRightJustify: Alignment := DWRITE_TEXT_ALIGNMENT_TRAILING;
      end;
    end
    else if Ch <> #32 then
      WSLayout.SetFontStyle([TFontStyle.fsUnderline], 1, 1);

    WSLayout.IDW.SetTextAlignment(Alignment);
    WSLayout.SetFontColor(SpecialCharsColor, 1, 1);
    WSLayout.Draw(RT, FTextOffset + XRowOffset + Round(Min(X1, X2)), YRowOffset(Row), SpecialCharsColor);
  end;

  procedure DrawStructureHighlight;
  var
    Row, Line: Integer;
    XPos: Integer;
    LColor: TColor;
    Range: TSynFoldRange;
    RangeArr: TArray<TSynFoldRange>;
    Start, Stop: TPoint;
    StrokeStyle: ID2D1StrokeStyle;
    Brush: ID2D1SolidColorBrush;
    StartRow, EndRow: Integer;
  begin
    if not Assigned(fHighlighter) then Exit;
    if not (fHighlighter.Capabilities >= [hcCodeFolding, hcStructureHighlight]) then Exit;
    if not UseCodeFolding or not Assigned(fAllFoldRanges) then Exit;

    if FIndentGuides.Style = igsDotted then
      StrokeStyle := TSynDWrite.DottedStrokeStyle
    else
      StrokeStyle := nil;

    RangeArr := fAllFoldRanges.FoldRangesForTextRange(RowToLine(aFirstRow), RowToLine(aLastRow));

    RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
    for Range in RangeArr do
    begin
      if Range.Indent <= 0 then Continue;

      XPos := Range.Indent * CharWidth + fTextOffset;
      if XPos <= 0 then Continue;

      // Choose LColor and brush
      if FIndentGuides.UseStructureColors and
        (FIndentGuides.StructureColors.Count > 0)
      then
        LColor := FIndentGuides.StructureColors[(Range.Indent div fTabWidth)
        mod FIndentGuides.StructureColors.Count].Color
      else
        LColor := FIndentGuides.Color;
      Brush := TSynDWrite.SolidBrush(LColor);

      StartRow := Max(aFirstRow, LineToRow(Range.FromLine));
      EndRow := Min(aLastRow,  LineToRow(Range.ToLine));
      for Row := StartRow to EndRow do
      begin
        Line := RowToLine(Row);

        Start := TPoint.Create(Xpos, YRowOffset(Row));
        Stop := Start;
        Stop.Offset(0, fTextHeight);

        // Paint vertical part
        if InRange(Line, Range.FromLine, Range.ToLine) then
          //RT.DrawLine(Start, Stop, Brush, FCurrentPPI / 96.0, StrokeStyle);
          RT.DrawLine(Start, Stop, Brush, Round(FCurrentPPI / 96), StrokeStyle);

        if Range.FromLine = Line then
        begin
          // Paint top
          Start.Offset(0, 1);
          Stop := Start;
          Stop.Offset(fCharWidth, 0);
          RT.DrawLine(Start, Stop, Brush, Round(FCurrentPPI / 96), StrokeStyle);
        end
        else if Range.ToLine = Line then
        begin
          // paint bottom
          Start.Offset(0, fTextHeight);
          Stop := Start;
          Stop.Offset(fCharWidth, 0);
          RT.DrawLine(Start, Stop, Brush, Round(FCurrentPPI / 96), StrokeStyle);
        end;
      end;
    end;
    RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
  end;

  procedure DrawIndentGuides;
  { Only used when WordWrap is False, so rows correspond to full lines }

    function StrIsBlank(S: string): Boolean;
    var
      I: Integer;
    begin
      for I := 1 to S.Length do
        if not (Word(S[I]) in [9, 32]) then Exit(False);
      Result := True;
    end;

  var
    TabSteps, LineIndent, NonBlankLine, X, Y, Row, Line: Integer;
    BMWidth: Integer;
    BitmapRT: ID2D1BitmapRenderTarget;
    BM: ID2D1Bitmap;
    RectF: TRectF;
    StrokeStyle: ID2D1StrokeStyle;
    BMSize: TD2D1SizeF;
  begin
    if UseCodeFolding and
      FIndentGuides.StructureHighlight and Assigned(fHighlighter) and
     (fHighlighter.Capabilities >= [hcCodeFolding, hcStructureHighlight]) then
    begin
      DrawStructureHighlight;
      Exit;
    end;

    BMWidth := Round(FCurrentPPI / 96) + 2;
    BMSize := D2D1SizeF(BMWidth, FTextHeight);

    if FIndentGuides.Style = igsDotted then
      StrokeStyle := TSynDWrite.DottedStrokeStyle
    else
      StrokeStyle := nil;

    CheckOSError(RT.CreateCompatibleRenderTarget(@BMSize, nil, nil,
      D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_GDI_COMPATIBLE,
      BitmapRT));
      BitmapRT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
      BitmapRT.BeginDraw;
      BitmapRT.DrawLine(Point(1, 0), Point(1, fTextHeight),
        TSynDWrite.SolidBrush(FIndentGuides.Color),
        Round(FCurrentPPI / 96), StrokeStyle);
      BitmapRT.EndDraw;
      CheckOSError(BitmapRT.GetBitmap(BM));


    for Row := aFirstRow to aLastRow do begin
      Line := RowToLine(Row);
      if (Line > Lines.Count) then Break;
      // If line is blank get next nonblank line
      NonBlankLine := Line;
      while (NonBlankLine <= fLines.Count) and
        StrIsBlank(fLines[NonBlankLine - 1])
      do
        Inc(NonBlankLine);
      LineIndent := LeftSpaces(fLines[NonBlankLine - 1], True, FTabWidth);
      // Step horizontally
      Y := YRowOffset(Row);
      TabSteps := TabWidth;
      while TabSteps < LineIndent do
      begin
        X := TabSteps * CharWidth + fTextOffset;
        if X >= 0 then
        begin
          RectF := Rect(X - 1, Y, X + BMWidth - 1, Y + fTextHeight);
          // avoid having two consequtive dots
          if (FIndentGuides.Style = igsDotted) and Odd(fTextHeight) and
            not Odd(Row)
          then
            RectF.Offset(0, 1);
          RT.DrawBitmap(BM, @RectF);
        end;
        Inc(TabSteps, TabWidth);
      end;
    end;
  end;

  procedure PaintFoldMarks;
  var
    Row, Line, Y: Integer;
    HintRect : TRect;
    Layout: TSynTextLayout;
  begin
    if not fCodeFolding.ShowCollapsedLine and not fCodeFolding.ShowHintMark then
      Exit;

    for Row := aFirstRow to aLastRow do begin
      Line := RowToLine(Row);
      if fAllFoldRanges.CollapsedFoldStartAtLine(Line) then
      begin
        if fCodeFolding.ShowCollapsedLine then
        begin
          // Get starting and end points
          Y := YRowOffset(Row + 1) - 1;
          RT.DrawLine(Point(FGutterWidth + TextMargin, Y),
            Point(ClientWidth, Y),
            TSynDWrite.SolidBrush(fCodeFolding.CollapsedLineColor),
            MulDiv(1, FCurrentPPI, 96));
        end;
        end;
        if fCodeFolding.ShowHintMark then
        begin
          HintRect := GetCollapseMarkRect(Row, Line);
          if HintRect.IntersectsWith(LinesRect) then
          begin
            RT.DrawRectangle(HintRect,
              TSynDWrite.SolidBrush(fCodeFolding.CollapsedLineColor),
              FCurrentPPI/96);
            Layout.Create(FTextFormat, PChar(StringOfChar(SynSpaceGlyph, 3)), 3,
              HintRect.Width, HintRect.Height);
            Layout.IDW.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
            Layout.Draw(RT, HintRect.Left, HintRect.Top,
              fCodeFolding.CollapsedLineColor);
          end;
        end;
    end;
  end;

  procedure TextRangeToDisplay(const S: string; out FirstChar, LastChar: Integer);
  {
    When LeftChar > 1, we try to rended only the text that is visible.  This
    is particularly important when the edited text contains super long lines.
    FirstChar is the first char that should be displayed, which could be
    partially visible.  The presence of tabs prevents the use of this
    optimization.

    This routine also adjust XRowOffset.

    Text is painted at fTextOffset + XRowOffset
    fTextOffset := fGutterWidth + fTextMargin - (LeftChar - 1) * fCharWidth;
    fTextOffset can be negative.
  }

  var
    HasTabs: Boolean;
    I: Integer;
  begin
    if S = '' then
    begin
      FirstChar := 1;
      LastChar := IfThen(FLeftChar = 1, 0, -1); // To display CR;
      XRowOffset := 0;
      Exit;
    end;
    FirstChar := PixelsToColumn(PChar(S), S.Length,
      (FLeftChar - 1) * FCharWidth + 1, True);
    if FirstChar > S.Length then
    begin
      // nothing to display
      FirstChar := 1;
      LastChar := -1;
      Exit;
    end;

    while (FirstChar > 1) and not (Word(S[FirstChar - 1]) in [9, 32..126]) do
      Dec(FirstChar);

    XRowOffset := ColumnToPixels(S, FirstChar);

    LastChar := Min(FirstChar + PixelsToColumn(PChar(S) + FirstChar - 1,
      S.Length - FirstChar + 1, ClientWidth - fTextOffset - XRowOffset),
      S.Length);
    while (LastChar < S.Length) and not (Word(S[LastChar + 1]) in [9, 32..126]) do
      Inc(LastChar);

    // If there are tabs *inside* the displayed range we need to make sure
    // they are rendered at the correct place
    HasTabs := False;
    for I := FirstChar to LastChar do
      if S[I] = #9 then
      begin
        HasTabs := True;
        Break;
      end;
    if HasTabs and (XRowOffset mod (fTabWidth * fCharWidth) <> 0) then
    begin
      // Unfortunately this case cannot be readily optimized
      FirstChar := 1;
      XRowOffset := 0;
    end
    else begin
      // Check for RTL characters which are not safe to optimize
      for I := FirstChar to LastChar do
        if (S[I] >= #$0590) and (S[I] <= #$08FF) then
        begin
          FirstChar := 1;
          XRowOffset := 0;
          Exit;
        end;
    end;
  end;

  function SelEndX(Left, Width: Single; SelLast, I, RangeCount: Integer): Integer;
  // Helper that returns the position of the end of partial selection
  begin
    if (SelLast = MaxInt) and (I + 1 = Integer(RangeCount)) then
    begin
      if fSelectedColor.FillWholeLines then
        Result := LinesRect.Right
      else if scEOL in FVisibleSpecialChars then
        Result := Round(Left + Width)
      else
        Result := Round(Left + Width) + fCharWidth;
    end
    else
      Result := Round(Left + Width);
  end;

  type
    TPartialSelection = record
      First, Last: Integer;
      SelBG, SelFG: TColor;
    end;

    TPartSelArray = TArray<TPartialSelection>;

  function PartialSelections(Row, Line: Integer; const RowStart: TBufferCoord): TPartSelArray;
  var
    Len: Integer;

    function HavePartialSelection(const Sel: TSynSelection; var PartSel: TPartialSelection): Boolean;
    var
      BB, BE: TBufferCoord;
      FG, BG: TColor;
    begin
      with Sel.Normalized do
      begin
        BB := Start;
        BE := Stop;
      end;
      PartSel.First := 0;
      PartSel.Last := 0;

      if BB = BE then Exit(False);

      Result :=
        ((Line = BB.Line) and
         ((InRange(BB.Char, RowStart.Char, RowStart.Char + Len)) and
         not (WordWrap and (BB.Char = RowStart.Char + Len) and
         (RowtoLine(Row + 1) = Line) and not Sel.CaretAtEOL))) or
        ((Line = BE.Line) and InRange(BE.Char, RowStart.Char + 1, RowStart.Char + Len));
      if not Result then Exit;

      if BB >= RowStart then
      begin
        PartSel.First := BB.Char - RowStart.Char + 1;
        PartSel.Last := IfThen((BE > BufferCoord(RowStart.Char + Len, RowStart.Line)) or
          (WordWrap and (BE = BufferCoord(RowStart.Char + Len, RowStart.Line)) and
          (RowtoLine(Row + 1) = Line) and not Sel.CaretAtEOL), MaxInt, BE.Char - RowStart.Char);
      end
      else
      begin
        PartSel.First := 1;
        PartSel.Last := BE.Char - RowStart.Char;
      end;

      if DoOnSpecialLineColors(Line, FG, BG) and
        SameValue(fSelectedColor.Alpha, 1)
      then
      begin
        // Invert special colors as in Delphi
        PartSel.SelBG := FG;
        PartSel.SelFG := BG;
      end
      else
      begin
        PartSel.SelBG := fSelectedColor.Background;
        PartSel.SelFG := fSelectedColor.Foreground;
      end
    end;

  var
    IsFullySelected: Boolean;
    PartSel: TPartialSelection;
    SelArray: TSynSelectionArray;
    Sel: TSynSelection;
  begin
    Result := [];
    if (eoNoSelection in FOptions) or (not Focused and FHideSelection) then
      Exit;

    IsFullySelected := IsRowFullySelected(Row, RowStart);
    if IsFullySelected then
    begin
      if not fSelectedColor.FillWholeLines then
      begin
        PartSel.First := 1;
        PartSel.Last := MaxInt;
        PartSel.SelBG := fSelectedColor.Background;
        PartSel.SelFG := fSelectedColor.Foreground;
        Result := [PartSel];
      end
      else
        Result := [];
      Exit;
    end;

    if WordWrap then
      Len := fWordWrapPlugin.RowLength[Row]
    else
      Len := Lines[Line-1].Length;

    SelArray := FSelections.PartSelectionsForRow(RowStart,
      BufferCoord(RowStart.Char + Len, RowStart.Line));

    for Sel in SelArray do
      if HavePartialSelection(Sel, PartSel) then
        Result := Result + [PartSel];
  end;

  procedure PaintPartialSelections(const Layout: TSynTextLayout;
    ARow, Aline: Integer; const RowStart: TBufferCoord;
    FirstChar, LastChar: Integer);
  {   Paint selection if ARow is partially selected - deals with bidi text
     The foreground needs to be set before we render the layout
     The background needs to be painted before we render the layout if we
     are not blending the selection otherwise after}
  var
    PartSel: TPartialSelection;
    AlphaBlended: Boolean;
    BGColor: TD2D1ColorF;
    RangeCount: Cardinal;
    HMArr: array of TDwriteHitTestMetrics;
    Index, I: Integer;
    PartSelArr: TPartSelArray;
  begin
    PartSelArr := PartialSelections(ARow, ALine, RowStart);
    if Length(PartSelArr) = 0 then Exit;

    for Index := 0 to High(PartSelArr) do
    begin
      PartSel := PartSelArr[Index];
      if (PartSel.Last < MaxInt) and (PartSel.First > LastChar) then Continue;

      AlphaBlended := not SameValue(fSelectedColor.Alpha, 1);
      if AlphaBlended then
        BGColor := D2D1ColorF(fSelectedColor.Background, fSelectedColor.Alpha)
      else
        BGColor := D2D1ColorF(PartSel.SelBG);

      if (LastChar <= 0) and (PartSel.Last = MaxInt) then
      begin
        // empty line special case
        if fSelectedColor.FillWholeLines then
          RT.FillRectangle(Rect(LinesRect.Left, YRowOffset(ARow),
            LinesRect.Right,
            YRowOffset(ARow + 1)), TSynDWrite.SolidBrush(BGColor))
        else if LeftChar = 1 then
          RT.FillRectangle(Rect(LinesRect.Left, YRowOffset(ARow),
            LinesRect.Left + fCharWidth,
            YRowOffset(ARow + 1)), TSynDWrite.SolidBrush(BGColor))
      end
      else if LastChar > 0 then
      begin
        // Adjust for First/LastChar
        PartSel.First := Max(PartSel.First - FirstChar + 1, 1);
        if PartSel.Last <> MaxInt then
          PartSel.Last := PartSel.Last - FirstChar + 1;
       // Skip if selection is not visible
       if PartSel.Last < 1 then Continue;

        Layout.IDW.HitTestTextRange(PartSel.First - 1, PartSel.Last - PartSel.First + 1,
          FTextOffset, YRowOffset(ARow), PDwriteHitTestMetrics(nil)^, 0, RangeCount);

        SetLength(HMArr, RangeCount);
        Layout.IDW.HitTestTextRange(PartSel.First - 1, PartSel.Last - PartSel.First + 1,
        FTextOffset + XRowOffset, YRowOffset(ARow), HMArr[0], RangeCount, RangeCount);
        for I := 0 to RangeCount -1  do
        begin
          if not AlphaBlended then
            Layout.SetFontColor(PartSel.SelFG, HMArr[I].textPosition + 1, HMArr[I].length);
          RT.FillRectangle(Rect(Round(HMArr[I].left), YRowOffset(ARow),
            SelEndX(HMArr[I].Left, HMArr[I].Width, PartSel.Last, I, RangeCount),
            YRowOffset(ARow + 1)), TSynDWrite.SolidBrush(BGColor));
        end;
      end;
    end;
  end;

var
  Line, Row, CharOffset, I: Integer;
  RowStart: TBufferCoord;
  LayoutWidth: Integer;
  SLine, SRow: string;
  FirstChar, LastChar: Integer;
  DoWhitespacePainting: Boolean;
  Layout: TSynTextLayout;
  BGColor, FGColor, SpecialCharsColor: TColor;
  TokenPos, TokenLen: Integer;
  Attr: TSynHighlighterAttributes;
  LineIndicators: TArray<TSynIndicator>;
  Indicator: TSynIndicator;
  IndicatorSpec: TSynIndicatorSpec;
  AColor: TColor;
  REdgePos: Integer;
  FullRowFG, FullRowBG: TColor;
  HintColor: TColor;
  BGAlpha: TD2D1ColorF;
  FlowControl: TSynFlowControl;
begin
  // Paint background
  LinesRect := Rect(FGutterWidth, AClip.Top, AClip.Right, AClip.Bottom);
  if LinesRect.IsEmpty then Exit;
  BGColor := WhitespaceColor(True, True);  // Resets highlighter
  RT.FillRectangle(LinesRect, TSynDWrite.SolidBrush(BGColor));

  if Lines.Count = 0 then
    Exit;

  Inc(LinesRect.Left, FTextMargin);

  RT.PushAxisAlignedClip(LinesRect, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);

  for Row:= aFirstRow to aLastRow do
  begin
    RowStart := DisplayToBufferPos(DisplayCoord(1, Row));
    Line := RowStart.Line;
    SLine := Lines[Line -  1];

    CharOffset := RowStart.Char;
    if WordWrap then
      SRow := Copy(SLine, CharOffset, fWordWrapPlugin.RowLength[Row])
    else
      SRow := SLine;

    // TextHint
    if (Lines.Count <= 1) and (SLine = '') then
      SRow := FTextHint;

    // Flow control symbols
    FlowControl := fcNone;
    if FDisplayFlowControl.Enabled and Assigned(fHighlighter) and
      not WordWrap and not (scEOL in FVisibleSpecialChars)
    then
    begin
      FlowControl := fHighlighter.FlowControlAtLine(Lines, Line);
      if FlowControl <> fcNone then
        SRow := SRow + FlowControlChars[FlowControl];
    end;

    // Restrict the text to what can/should be displayed
    TextRangeToDisplay(SRow, FirstChar, LastChar);

    // Deal with Special Chars
    if scControlChars in FVisibleSpecialChars then
      // Show Control graphics instead of control chars.
      SubstituteControlChars(SRow);

    DoWhitespacePainting := False;
    if (scWhitespace in FVisibleSpecialChars) and (LastChar >= 0) then
      for I := FirstChar to LastChar do
        if IsWhiteChar(SRow[I]) then
        begin
          DoWhitespacePainting := True;
          break;
        end;

    if (scEOL in FVisibleSpecialChars) and (LastChar >= 0) and
      (CharOffset + LastChar = SLine.Length + 1) and (Line < Lines.Count) then
    begin
      SRow := SRow + SynLineBreakGlyph;
      Inc(LastChar);
    end;

    // Create the text layout
    if LastChar > 0 then
    begin
      LayoutWidth := ClientWidth - FTextOffset - XRowOffset;
      Layout.Create(FTextFormat, PChar(SRow) + FirstChar - 1,
        LastChar - FirstChar + 1, LayoutWidth, fTextHeight);
      if not (eoShowLigatures in FOptions) or (Line = CaretY) then
        // No ligatures for current line
        Layout.SetTypography(typNoLigatures, 1, SRow.Length);
    end;

    // TextHint
    if (Lines.Count <= 1) and (SLine = '') and (FTextHint <> '') then
    begin
      if Assigned(fHighlighter) and
        Assigned(fHighlighter.WhitespaceAttribute) and
        (fHighlighter.WhitespaceAttribute.Foreground <> clNone)
      then
        HintColor := fHighlighter.WhitespaceAttribute.Foreground
      else
        HintColor := clGray;
      Layout.SetFontColor(HintColor, FirstChar, LastChar - FirstChar + 1);
    end;

    // Special colors, full line selection and ActiveLineColor
    FullRowColors(RowStart, Row, Line, FullRowBG, FullRowFG, BGAlpha);
    if FullRowBG <> clNone then
      RT.FillRectangle(Rect(LinesRect.Left, YRowOffset(Row), LinesRect.Right,
        YRowOffset(Row + 1)), TSynDWrite.SolidBrush(FullRowBG));

    // Highlighted tokens
    if fHighlighter <> nil then
    begin
      // Optimization.  In WordWrap mode we carry on where the previous row ended,
      // if they are parts of the same line.  So, each line is scanned once.
      if not WordWrap or (Row = aFirstRow) or (CharOffset = 1) then
      begin
        if Line > 1 then
          fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Line - 2])
        else
          fHighlighter.ResetRange;
        FHighlighter.SetLine(SLine, Line);
      end;

      //  Whitespace color (returned by GetDefaultAttribute) may differ per line
      //  Done here so that the highlighter range is set correctly
      if (FullRowBG = clNone) then begin
        AColor := WhitespaceColor(True);
        if (AColor <> clNone) and (AColor <> BGColor) then
        RT.FillRectangle(Rect(LinesRect.Left, YRowOffset(Row), LinesRect.Right,
          YRowOffset(Row + 1)), TSynDWrite.SolidBrush(AColor));
      end;

      // When you scroll to the right the whole row may be hidden
      if (LastChar > 0) then
      begin
        while not FHighLighter.GetEol do
        begin
          // CharOffset adjusts for wrapped lines and FirstChar for LeftChar > 1
          TokenPos := FHighLighter.GetTokenPos;  //TokenPos is zero based

          // Check whether the start of the token is beyond the end of the row
          // CharOffset points to the start of the row and LastChar is a row offset
          if TokenPos - CharOffset + 2 > LastChar then Break;

          TokenLen := FHighLighter.GetTokenLength;

          // Skip if the whole token is on the left of FirstChar
          if TokenPos + TokenLen - CharOffset  + 2 <= FirstChar then
          begin
            fHighlighter.Next;
            Continue;
          end;

          // We need to display at least part of the token
          // Adjust for CharOffset and FirstChar
          Dec(TokenLen, Max(0,  FirstChar  - (TokenPos - CharOffset + 2)));
          TokenPos := TokenPos - CharOffset - FirstChar + 3 +
            Max(0,  FirstChar  - (TokenPos - CharOffset + 2));

          Attr := FHighLighter.GetTokenAttribute;

          if (TokenLen > 0) and Assigned(Attr) then
          begin
            Layout.SetFontStyle(Attr.Style, TokenPos, TokenLen);
            AColor := Attr.Foreground;
            if (FullRowFG = clNone) and (AColor <> clNone) then
              Layout.SetFontColor(AColor, TokenPos, TokenLen);
            AColor := Attr.Background;
            if (FullRowBG = clNone) and (AColor <> clNone) then
              PaintTokenBackground(Layout, Row, TokenPos, TokenLen, D2D1ColorF(AColor));
          end;
          // check whether the whole row has been painted
          if TokenPos + TokenLen - 1 > LastChar - FirstChar + 1 then
            Break
          else
            FHighLighter.Next;
        end;
        if (scEOL in FVisibleSpecialChars) and (FullRowFG = clNone) and
          (CharOffset + LastChar = SLine.Length + 2) and
          Assigned(fHighlighter.WhitespaceAttribute) and
          (fHighlighter.WhitespaceAttribute.Foreground <> clNone)
        then
          Layout.SetFontColor(fHighlighter.WhitespaceAttribute.Foreground, LastChar - FirstChar + 1, 1);

        if (FlowControl <> fcNone) and (FullRowFG = clNone) and
          (CharOffset + LastChar = SLine.Length + 2)
        then
          Layout.SetFontColor(FDisplayFlowControl.Color, LastChar - FirstChar + 1, 1);
      end;
    end;

    // Paint partial selection if not alpha blending the selection color
    if SameValue(fSelectedColor.Alpha, 1) then
      PaintPartialSelections(Layout, Row, Line, RowStart, FirstChar, LastChar);

    // Indicators
    LineIndicators := FIndicators.LineIndicators(Line);
    if Assigned(FOnGetLineIndicators) then
      FOnGetLineIndicators(Self, Line, LineIndicators);

    for Indicator in LineIndicators do
    begin
      TokenPos := Indicator.CharStart - CharOffset - FirstChar + 2;
      TokenLen := Indicator.CharEnd - Indicator.CharStart;
      if (TokenPos > LastChar - FirstChar + 1) or (TokenPos + TokenLen <= 1)
      then
        Continue;

      IndicatorSpec := Indicators.GetSpec(Indicator.Id);
      if (IndicatorSpec.Style = sisTextDecoration) then
      begin
        Layout.SetFontStyle(IndicatorSpec.FontStyle, TokenPos, TokenLen);
        if TAlphaColorF(IndicatorSpec.Foreground) <> TAlphaColorF(clNoneF) then
          Layout.SetFontColor(IndicatorSpec.Foreground, TokenPos, TokenLen);

        // Paint Indicator background before Layout if it is not alpha blended
        if SameValue(IndicatorSpec.Background.a, 1) then
          PaintTokenBackground(Layout, Row, TokenPos, TokenLen,
          IndicatorSpec.Background);
      end;
    end;

    // Paint the layout
    if LastChar > 0 then
    begin
      if FullRowFG = clNone then
        FGColor := Font.Color
      else
        FGColor := FullRowFG;
      Layout.Draw(RT, FTextOffset + XRowOffset, YRowOffset(Row), FGColor);
    end;

    // Paint special characters (whitespace)
    if DoWhitespacePainting then
    begin
      if FullRowFG <> clNone then
        SpecialCharsColor := FullRowFG
      else
        SpecialCharsColor := WhitespaceColor(False);

      for I := FirstChar to LastChar do
        if IsWhiteChar(SRow[I]) then
        begin
          if IsPointInSelection(DisplayToBufferPos(DisplayCoord(I, Row))) and
            SameValue(fSelectedColor.Alpha, 1)
          then
            DrawWhitespace(Layout, Row, I - FirstChar + 1, SRow[I], fSelectedColor.Foreground)
          else
            DrawWhitespace(Layout, Row, I - FirstChar + 1, SRow[I], SpecialCharsColor);
        end;
    end;

    //Draw indentation guides and code folding marks (PaintFoldAttributes)
    if FIndentGuides.Visible and not WordWrap then
      DrawIndentGuides;
    if UseCodeFolding then
      PaintFoldMarks;

    // Indicators
    for Indicator in LineIndicators do
    begin
      TokenPos := Indicator.CharStart - CharOffset - FirstChar + 2;
      TokenLen := Indicator.CharEnd - Indicator.CharStart;
      if (TokenPos > LastChar - FirstChar + 1) or (TokenPos + TokenLen <= 1)
      then
        Continue;

      IndicatorSpec := Indicators.GetSpec(Indicator.Id);
      Indicators.Paint(RT, IndicatorSpec,
        GetTokenRect(Layout, Row, TokenPos, TokenLen),
        IfThen(TokenPos < 1,
          TextWidth(Copy(SLine, Indicator.CharStart, 1 - TokenPos)), 0));
    end;

    // Alpha blend - full row selection, or Active line color or Special line
    if TAlphaColorF(BGAlpha) <> TAlphaColorF(clNoneF) then
      // Full Row
      RT.FillRectangle(Rect(LinesRect.Left, YRowOffset(Row), LinesRect.Right,
        YRowOffset(Row + 1)), TSynDWrite.SolidBrush(BGAlpha));

    // Paint partial selection if not alpha blending the selection color
    if not SameValue(fSelectedColor.Alpha, 1) then
      PaintPartialSelections(Layout, Row, Line, RowStart, FirstChar, LastChar);

    // Draw right edge
    if (fRightEdge > 0) then
    begin
      REdgePos := fRightEdge * fCharWidth; // pixel value
      if InRange(REdgePos + fTextOffset, AClip.Left, AClip.Right) then
        RT.DrawLine(Point(FTextOffset + REdgePos, AClip.Top),
          Point(FTextOffset + REdgePos, AClip.Bottom),
          TSynDWrite.SolidBrush(fRightEdgeColor));
    end;
  end;

  RT.PopAxisAlignedClip;
end;

procedure TCustomSynEdit.PasteFromClipboard;
begin
  CommandProcessor(ecPaste, ' ', nil);
end;

procedure TCustomSynEdit.SelectAll;
var
  LastPt: TBufferCoord;
begin
  LastPt.Char := 1;
  LastPt.Line := Lines.Count;
  if LastPt.Line > 0 then
    Inc(LastPt.Char, Length(Lines[LastPt.Line - 1]))
  else
    LastPt.Line  := 1;
  SetCaretAndSelection(LastPt, BufferCoord(1, 1), LastPt);
end;

procedure TCustomSynEdit.SelectMatchingText;
var
  Engine: TSynEditSearchCustom;
  SearchOptions: TSynSearchOptions;
  Line: Integer;
  ResNo: Integer;
  SelList: TList<TSynSelection>;
  Sel: TSynSelection;
  SelStorage: TSynSelStorage;
  LineText: string;
  CaretAtStart: Boolean;
begin
  if FSelection.IsEmpty then
    SetSelWord;

  if (FSelection.IsEmpty) or (FSelection.Start.Line <> FSelection.Stop.Line) then
    Exit;  // Only match single line text


  Engine := TSynEditSearch.Create(Self);
  SelList := TList<TSynSelection>.Create;
  try
    SelStorage.BaseIndex := 0;    // to avoid compiler warnings
    SelStorage.ActiveIndex := 0;

    Engine.Pattern := SelectionText(FSelection);
    SearchOptions := [];
    if CaseSensitive then
      Include(SearchOptions, ssoMatchCase);
    if Engine.Pattern = WordAtCursor then
      Include(SearchOptions, ssoWholeWord);
    CaretAtStart := FSelection.Start > FSelection.Stop;

    Engine.Options := SearchOptions;

    for Line := 0 to Lines.Count - 1 do
    begin
      LineText := Lines[Line];
      if LineText.Length < Engine.Pattern.Length then Continue;

      Engine.FindAll(LineText);
      for ResNo := 0 to Engine.ResultCount - 1 do
      begin
        if CaretAtStart then
        begin
          Sel.Stop := BufferCoord(Engine.Results[ResNo], Line + 1);
          Sel.Start := BufferCoord(Sel.Stop.Char + Engine.Lengths[ResNo], Line + 1)
        end
        else
        begin
          Sel.Start := BufferCoord(Engine.Results[ResNo], Line + 1);
          Sel.Stop := BufferCoord(Sel.Start.Char + Engine.Lengths[ResNo], Line + 1)
        end;
        Sel := TSynSelection.Create(Sel.Stop, Sel.Start, Sel.Stop);

        SelList.Add(Sel);

        if Sel = FSelection then
        begin
          SelStorage.BaseIndex := SelList.Count - 1;
          SelStorage.ActiveIndex := SelList.Count - 1;
        end;
      end;
    end;

    Assert(SelList.Count > 0);

    SelStorage.Selections := SelList.ToArray;
    FSelections.Restore(SelStorage);
    UpdateScrollbars;
  finally
    SelList.Free;
    Engine.Free;
  end;
end;

function TCustomSynEdit.ValidBC(const Value: TBufferCoord): TBufferCoord;
begin
  Result := Value;
  Result.Char := Max(Result.Char, 1);
  Result.Line := MinMax(Result.Line, 1, Lines.Count);
  if Lines.Count > 0 then
    Result.Char := Min(Result.Char, Lines[Result.Line - 1].Length + 1)
  else
    Result.Char := 1;
end;

procedure TCustomSynEdit.SetBlockBegin(Value: TBufferCoord);
{ Also sets FSelection.Stop to Value! }
var
  SelChanged: Boolean;
begin
  Value := ValidBC(Value);

  if SelAvail then
  begin
    InvalidateSelection;
    SelChanged := True;
  end
  else
    SelChanged := Value <> FSelection.Start; //FSelection.Stop = FSelection.Start

  FSelection.Start := Value;
  FSelection.Stop := Value;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TBufferCoord);
begin
  if not (eoNoSelection in Options) then
  begin
    Value := ValidBC(Value);

    if Value <> FSelection.Stop then
    begin
      InvalidateRange(Value, FSelection.Stop);
      FSelection.Stop := Value;
      StatusChanged([scSelection]);
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretX(Value: Integer);
begin
  SetCaretXY(BufferCoord(Value, CaretY));
end;

procedure TCustomSynEdit.SetCaretY(Value: Integer);
begin
  SetCaretXY(BufferCoord(CaretX, Value));
end;

procedure TCustomSynEdit.InternalSetCaretX(Value: Integer);
begin
  InternalSetCaretXY(BufferCoord(Value, CaretY));
end;

procedure TCustomSynEdit.InternalSetCaretY(Value: Integer);
begin
  InternalSetCaretXY(BufferCoord(CaretX, Value));
end;

function TCustomSynEdit.GetDisplayX: Integer;
begin
  Result := DisplayXY.Column;
end;

function TCustomSynEdit.GetDisplayY: Integer;
begin
  Result := DisplayXY.Row;
end;

function TCustomSynEdit.SelectionToDisplayCoord(var Sel: TSynSelection): TDisplayCoord;
begin
  Result := BufferToDisplayPos(Sel.Caret);
  if WordWrap and Sel.CaretAtEOL then
  begin
    if Result.Column = 1 then
    begin
      Dec(Result.Row);
      Result.Column := fWordWrapPlugin.GetRowLength(Result.Row) + 1;
    end
    else begin
      // Work-around situations where CaretAtEOL should have been updated because of
      //text change (it's only valid when Column = 1).
      Sel.CaretAtEOL := False;
    end;
  end;
end;

function TCustomSynEdit.SelectSameWord(const AWord: string; Start: TBufferCoord;
  BackwardSearch: Boolean; AddSelection: Boolean): Boolean;
var
  Engine, OldEngine: TSynEditSearchCustom;
  SearchOptions: TSynSearchOptions;
  SelStorage: TSynSelStorage;
  NewSel: TSynSelection;
begin
  if AWord = '' then Exit(False);

  OldEngine := SearchEngine;
  Engine := TSynEditSearch.Create(Self);
  SearchEngine := Engine;
  try
    IncPaintLock;
    try
      SearchOptions := [ssoWholeWord];
      if CaseSensitive then
        Include(SearchOptions, ssoMatchCase);
      if BackwardSearch  then
        Include(SearchOptions, ssoBackwards);

      // SearchReplace will clear existing selections if successful
      // So we need to store and restore
      FSelections.Store(SelStorage);
      Result := SearchReplace(AWord, '', SearchOptions) > 0;

      if Result then
        FSelection.Normalize;

      if Result and AddSelection then
      begin
        NewSel := FSelection;
        FSelections.Restore(SelStorage);
        FSelection := NewSel;
        if not FSelections.AddCaret(FSelection.Start) then
          // Try again. This time it will not fail
          FSelections.AddCaret(FSelection.Start);
        // Deal with overlapping selections as if the range was mouse selected
        FSelections.MouseSelection(FSelection);
      end;
    finally
      DecPaintLock;
    end;
  finally
    Engine.Free;
    SearchEngine := OldEngine;
  end;
end;

procedure TCustomSynEdit.InternalCommandHook(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data,
  HandlerData: pointer);
var
  BB, BE: TBufferCoord;
  Spaces: string;
  I: Integer;
begin
  if not AfterProcessing and (Command = ecSelWord) then
  begin
    GetWordBoundaries(CaretXY, BB, BE);
    if not FSelection.IsEmpty and (BB = BlockBegin) and (BE = BlockEnd) then
    begin
      // A Word is alredy selected. Add to selections the next
      // occurance of the that Word
      Inc(BE.Char);
      SelectSameWord(WordAtCursor, BE, False, True);
      Handled := True;
      Command := ecNone;
    end
  end
  else if not AfterProcessing and (Command = ecPaste) then
  begin
    if not CanPaste then
    begin
      Handled := True;
      Command := ecNone;
      Exit;
    end;
    FPasteArray := [];
    if (FSelections.Count > 1) and Clipboard.HasFormat(SynEditClipboardFormat) then
    begin
      FPasteArray := GetInternalClipText;
      if Length(FPasteArray) <> FSelections.Count then
        FPasteArray := [];
    end;

    if Length(FPasteArray) = 0 then
      FPasteArray := [GetClipboardText];

    if eoTabsToSpaces in Options then
    begin
      Spaces := StringOfChar(#32, TabWidth);
      for I := 0 to Length(FPasteArray) - 1 do
        FPasteArray[I] := StringReplace(FPasteArray[I], #9, Spaces, [rfReplaceAll]);
    end;
  end
  else if AfterProcessing and (Command = ecPaste) then
  begin
    FPasteArray := [];
    EnsureCursorPosVisible;
  end;
end;

Function TCustomSynEdit.GetDisplayXY: TDisplayCoord;
begin
  Result := SelectionToDisplayCoord(FSelection);
end;

procedure TCustomSynEdit.SetCaretXY(const Value: TBufferCoord);
{ There are two setCaretXY methods.  One Internal, one External.
  Property CaretXY (re)sets the block as well }
begin
  SetCaretAndSelection(Value, Value, Value);
end;

procedure TCustomSynEdit.InternalSetCaretXY(const Value: TBufferCoord);
{ Unlike SetCaretXY it does not affect BlockBegin/End }
begin
  SetCaretXYEx(True, Value);
end;

procedure TCustomSynEdit.UpdateLastPosX;
begin
  FLastPosX := RowColumnToPixels(DisplayXY).X - fTextOffset;
end;

procedure TCustomSynEdit.SetCaretXYEx(EnsureVisible: Boolean; Value: TBufferCoord);
var
  nMaxX: Integer;
  S, TS : string;
begin
  CaretAtEOL := False;
  if HandleAllocated then
    DoOnPaintTransient(ttBefore);
  nMaxX := MaxInt;
  if Value.Line > Lines.Count then
    Value.Line := Lines.Count;
  if Value.Line < 1 then
  begin
    // this is just to make sure if Lines stringlist should be empty
    Value.Line := 1;
    if not (eoScrollPastEol in fOptions) then
      nMaxX := 1;
  end
  else
  begin
    if not (eoScrollPastEol in fOptions) then
      nMaxX := Length(Lines[Value.Line - 1]) + 1;
  end;
  if (Value.Char > nMaxX) and (not(eoScrollPastEol in Options)) then
    Value.Char := nMaxX;
  if Value.Char < 1 then
    Value.Char := 1;

  //Trim here
  if not FReadOnly and (Value.Line <> CaretY) and (eoTrimTrailingSpaces in fOptions) and
     (CaretY <= Lines.Count) and (CaretY >= 1) then
  begin
    S := Lines[CaretY-1];
    TS := S.TrimRight;
    if S <> TS then
      Lines[CaretY-1] := TS;
  end;

  if (Value.Char <> CaretX) or (Value.Line <> CaretY) then
  begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if CaretX <> Value.Char then
      begin
        FSelection.Caret.Char := Value.Char;
        Include(fStatusChanges, scCaretX);
      end;
      if CaretY <> Value.Line then
      begin
        if (ActiveLineColor <> clNone) or (eoShowLigatures in fOptions) then
        begin
          InvalidateLine(Value.Line);
          InvalidateLine(CaretY);
        end;
        FSelection.Caret.Line := Value.Line;
        Include(fStatusChanges, scCaretY);
        // CodeFolding
        UncollapseAroundLine(CaretY);
        // Annotated Scrollbars
        if FScrollbarAnnotations.Count > 0 then
          Include(fStateFlags, sfScrollbarChanged);
      end;
      // Call UpdateLastPosX before DecPaintLock because the event handler it
      // calls could raise an exception, and we don't want FLastPosX to be
      // left in an undefined state if that happens.
      UpdateLastPosX;
      if EnsureVisible then
        EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
    finally
      DecPaintLock;
    end;
  end
  else begin
    // Also call UpdateLastPosX if the caret didn't move. Apps don't know
    // anything about FLastPosX and they shouldn't need to. So, to avoid any
    // unwanted surprises, always update FLastPosX whenever CaretXY is
    // assigned to.
    // Note to SynEdit developers: If this is undesirable in some obscure
    // case, just save the value of FLastPosX before assigning to CaretXY and
    // restore it afterward as appropriate.
    UpdateLastPosX;
  end;
  if HandleAllocated then
    DoOnPaintTransient(ttAfter);
end;

function TCustomSynEdit.RowColumnInView(RowCol: TDisplayCoord): Boolean;
//  Returns true even if it is partially visible.  Used in CaretInView
begin
  Result := InRange(RowCol.Row, TopLine, TopLine + LinesInWindow) and
    InRange(RowColumnToPixels(RowCol).X, FGutterWidth + TextMargin,
      ClientWidth - TextMargin);
end;

procedure TCustomSynEdit.CalcTextAreaWidth;
begin
  if WordWrap and (eoWrapWithRightEdge in FOptions) and (fRightEdge > 0) then
    FTextAreaWidth := Min(WrapAreaWidth,
       Max(ClientWidth - fGutterWidth - 2 * TextMargin, 0))
  else
    FTextAreaWidth := Max(ClientWidth - fGutterWidth - 2 * TextMargin, 0);
end;

function TCustomSynEdit.CaretInView: Boolean;
begin
  Result := RowColumnInView(DisplayXY);
end;

procedure TCustomSynEdit.CaretsAtLineEnds;
var
  SelList: TList<TSynSelection>;
  Sel: TSynSelection;
  Line: Integer;
  LineText: string;
  SelStorage: TSynSelStorage;
begin
  FSelections.Clear; // Operates on Active Selection only

  SelList := TList<TSynSelection>.Create;
  try
    for Line := BlockBegin.Line to BlockEnd.Line do
    begin
      LineText := Lines[Line - 1];
      Sel.Caret := BufferCoord(LineText.Length + 1, Line);
      Sel.Start := Sel.Caret;
      Sel.Stop := Sel.Caret;
      Sel.CaretAtEOL := False;
      Sel.LastPosX := RowColumnToPixels(BufferToDisplayPos(Sel.Caret)).X - fTextOffset;

      SelList.Add(Sel);

      SelStorage.Selections := SelList.ToArray;
      SelStorage.BaseIndex := SelList.Count - 1;
      SelStorage.ActiveIndex := SelList.Count - 1;
      FSelections.Restore(SelStorage);
    end;
  finally
    SelList.Free;
  end;
end;

procedure TCustomSynEdit.SetActiveLineColor(Value: TColor);
begin
  if (fActiveLineColor<>Value) then
  begin
    fActiveLineColor:=Value;
    InvalidateLine(CaretY);
  end;
end;

procedure TCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if fGutterWidth <> Value then
  begin
    fGutterWidth := Value;
    if HandleAllocated then
    begin
      CalcTextAreaWidth;
      if WordWrap then
        fWordWrapPlugin.DisplayChanged;
      UpdateScrollBars;
      Invalidate;
    end;
  end;
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
var
  MaxVal: Integer;
  iDelta: Integer;
  iTextArea: TRect;
begin
  if Value = FLeftChar then Exit;

	// when wrapping with right edge and right edge is behind the window width
  if WordWrap and not ((eoWrapWithRightEdge in FOptions) and
    (WrapAreaWidth > FTextAreaWidth))
  then
    Value := 1
  else
  begin
    if eoScrollPastEol in Options then
      MaxVal := MaxInt
    else if WordWrap and (eoWrapWithRightEdge in FOptions) and
      (WrapAreaWidth > FTextAreaWidth - FCharWidth)
    then
      MaxVal := CeilOfIntDiv(WrapAreaWidth - FTextAreaWidth, FCharWidth) + 2
    else
      // + 2 because we want to allow for an extra space at the end
      // and LeftChar 1 would mean that the char appears right at the edge
      MaxVal := Max(CeilOfIntDiv(Max(TSynEditStringList(Lines).MaxWidth +
                2 * FCharWidth - TextAreaWidth, 0), FCharWidth), 1);
    Value := MinMax(Value, 1, MaxVal);
  end;

  if Value <> fLeftChar then
  begin
    IncPaintLock;
    try
      FCarets.HideCarets;
      iDelta := fLeftChar - Value;
      fLeftChar := Value;
      fTextOffset := fGutterWidth + fTextMargin - (LeftChar - 1) * fCharWidth;
      if (Abs(iDelta) * FCharWidth < ClientWidth - GutterWidth - TextMargin) and
        (FSelections.Count = 1)
      then
      begin
        iTextArea := ClientRect;
        Inc(iTextArea.Left, fGutterWidth + fTextMargin);
        ScrollWindow(Handle, iDelta * CharWidth, 0, @iTextArea, @iTextArea);
      end
      else
        InvalidateLines(-1, -1);
      Include(fStateFlags, sfScrollbarChanged);
      StatusChanged([scLeftChar]);
    finally
      DecPaintLock;
    end;
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TStrings);
begin
  Lines.Assign(Value);
end;

procedure TCustomSynEdit.SetLineText(Value: string);
begin
  if (CaretY >= 1) and (CaretY <= Max(1, Lines.Count)) then
    Lines[CaretY - 1] := Value;
end;

procedure TCustomSynEdit.SetFontQuality(AValue: TFontQuality);
begin
  Font.Quality := AValue;
end;

procedure TCustomSynEdit.SetName(const Value: TComponentName);
var
  TextToName: Boolean;
begin
  TextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning])
    and (TrimRight(Text) = Name);
  inherited SetName(Value);
  if TextToName then
    Text := Value;
end;

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then
  begin
    FScrollBars := Value;
    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetSelText(const Value: string);
begin
  SetSelTextPrimitiveEx(Value, True);
end;

// This is really a last minute change and I hope I did it right.
// Reason for this modification: next two lines will loose the CaretX position
// if eoScrollPastEol is not set in Options. That is not really a good idea
// as we would typically want the cursor to stay where it is.
// To fix this (in the absence of a better idea), I changed the code in
// DeleteSelection not to trim the string if eoScrollPastEol is not set.
procedure TCustomSynEdit.SetSelTextPrimitiveEx(const Value: string;
  AddToUndoList: Boolean = True);
{
   Works in two stages:
     -  First deletes selection.
     -  Second inserts text taking into account PasteMode.
}
var
  BB, BE: TBufferCoord;
  TempString, SelectedText: string;

  procedure DeleteSelection;
  begin
    if Lines.Count > 0 then
    begin
        // Create a string that contains everything on the first line up
        // to the selection mark, and everything on the last line after
        // the selection mark.
      TempString := Copy(Lines[BB.Line - 1], 1, BB.Char - 1) +
        Copy(Lines[BE.Line - 1], BE.Char, MaxInt);
        // Delete all lines in the selection range.
      TSynEditStringList(Lines).DeleteLines(BB.Line, BE.Line - BB.Line);
        // Put the stuff that was outside of selection back in.
      if Options >= [eoScrollPastEol, eoTrimTrailingSpaces] then
        TempString := TempString.TrimRight;
      Lines[BB.Line - 1] := TempString;
    end;
    CaretXY := BB;
  end;

  procedure InsertText;
  var
    sLeftSide: string;
    sRightSide: string;
    NewLines: TArray<string>;
    LineCount: Integer;
    I: Integer;
  begin
    if Value = '' then
      Exit;

    NewLines := StringToLines(Value);
    LineCount := Length(NewLines);

    if LineCount = 0 then Exit;

    sLeftSide := Copy(LineText, 1, CaretX - 1);
    if CaretX - 1 > Length(sLeftSide) then
    begin
      sLeftSide := sLeftSide + StringofChar(#32,
        CaretX - 1 - Length(sLeftSide));
    end;
    sRightSide := Copy(LineText, CaretX, Length(LineText) - (CaretX - 1));
    if eoTrimTrailingSpaces in Options then
      sRightSide := sRightSide.TrimRight;

    // step1: insert the first line of Value into current line
    NewLines[0] := sLeftSide + NewLines[0];
    // step2: insert left lines of Value
    NewLines[LineCount - 1] := NewLines[LineCount - 1] + sRightSide;

    if eoTrimTrailingSpaces in Options then
      for I := 0 to LineCount - 1 do
        NewLines[I] := NewLines[I].TrimRight;

    Lines[CaretY -1] := NewLines[0];
    if LineCount > 1 then
    begin
      TSynEditStringList(Lines).InsertStrings(CaretY, NewLines, 1);
      Inc(FSelection.Caret.Line, LineCount - 1);
      Include(fStatusChanges, scCaretY);
    end;

    FSelection.Caret.Char := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide);
    StatusChanged([scCaretX]);

    // Force caret reset
    CaretXY := CaretXY;
  end;

begin
  BB := BlockBegin;
  BE := BlockEnd;

  if (Value.Length = 0) and (BB = BE) then Exit;  // nothing to do

  Lines.BeginUpdate;
  IncPaintLock;
  if AddToUndoList then BeginUndoBlock else fUndoRedo.Lock;
  try
    SelectedText := SelectionText(FSelection);

    if SelectedText <> '' then
      DeleteSelection;

    if Length(Value) > 0 then
      InsertText;

    if CaretY < 1 then
      CaretY := 1;
  finally
    if AddToUndoList then EndUndoBlock else fUndoRedo.UnLock;
    DecPaintLock;
    Lines.EndUpdate;
  end;
end;

procedure TCustomSynEdit.SetTextHint(const Value: string);
begin
  FTextHint := Value;
  if (FTextHint <> '') and (FLines.Count = 0) then
    // Add an empty line so that the hint will be shown
    FLines.Add('');
  if (FTextHint <> '') and (FLines.Count <= 1) and (FLines[0] = '') then
    Invalidate;
end;

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  FSelections.Clear();
  BeginUndoBlock;
  try
    IncPaintLock;
    try
      Lines.Text := Value;
    finally
      DecPaintLock;
    end;
  finally
    EndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
begin
  if (eoScrollPastEof in Options) then
    Value := Min(Value, DisplayRowCount)
  else
    Value := Min(Value, DisplayRowCount - fLinesInWindow + 1);
  Value := Max(Value, 1);
  if Value <> TopLine then
  begin
    IncPaintLock;
    try
      FCarets.HideCarets;
      Delta := TopLine - Value;
      fTopLine := Value;
      if (Abs(Delta) < fLinesInWindow) and (FSelections.Count = 1) then
        ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil)
      else
        Invalidate;

      Include(fStateFlags, sfScrollbarChanged);
      StatusChanged([scTopLine]);
    finally
      DecPaintLock;
    end;
  end;
end;

//++ CodeFolding
procedure TCustomSynEdit.SetUseCodeFolding(const Value: Boolean);
Var
  ValidValue : Boolean;
begin
  if csLoading in ComponentState then
  begin
    // when loading the highlighter may not have been assigned
    // this is taken care in Loaded.
    fUseCodeFolding := Value;
    Exit;
  end;

  ValidValue := Value and ((Assigned(fHighlighter) and
      (fHighlighter is TSynCustomCodeFoldingHighlighter))
        or Assigned(fOnScanForFoldRanges));

  if fUseCodeFolding <> ValidValue then
  begin
    AllFoldRanges.Reset;
    fUseCodeFolding := ValidValue;
    Invalidate; // better Invalidate before changing LeftChar and TopLine
    if ValidValue then
    begin
      // !!Mutually exclusive with WordWrap to reduce complexity
      WordWrap := False;
      if fHighlighter is TSynCustomCodeFoldingHighlighter then
      begin
        TSynCustomCodeFoldingHighlighter(fHighlighter).InitFoldRanges(fAllFoldRanges);
        AllFoldRanges.AdjustRangesProc :=
          TSynCustomCodeFoldingHighlighter(fHighlighter).AdjustFoldRanges;
      end
      else
        AllFoldRanges.AdjustRangesProc := nil;

      FullFoldScan;
    end
    else
      AllFoldRanges.AdjustRangesProc := nil;

    OnCodeFoldingChange(Self);
    InvalidateGutter;
  end;
end;

procedure TCustomSynEdit.OleDragEnter(Sender: TObject; DataObject: IDataObject;
  State: TShiftState; MousePt: TPoint; var Effect: LongInt;
  var Result: HResult);
begin
  if ReadOnly or not HasFormat(DataObject, CF_UNICODETEXT) then
    Effect := DROPEFFECT_NONE;
end;

procedure TCustomSynEdit.OleDragLeave(Sender: TObject; var Result: HResult);
begin
  fScrollTimer.Enabled := False;
  SetCaretAndSelection(FSelection.Stop, FSelection.Start, FSelection.Stop);
end;

procedure TCustomSynEdit.OleDragOver(Sender: TObject; DataObject: IDataObject;
  State: TShiftState; MousePt: TPoint; var Effect: LongInt;
  var Result: HResult);
var
  vNewPos: TDisplayCoord;
  Pt : TPoint;
begin
  Pt := ScreenToClient(MousePt);
  vNewPos := PixelsToNearestRowColumn(Pt.X, Pt.Y);
  InternalCaretXY := DisplayToBufferPos(vNewPos);
  ComputeScroll(Pt.X, Pt.Y);
end;

procedure TCustomSynEdit.OleDrop(Sender: TObject; DataObject: IDataObject;
  State: TShiftState; MousePt: TPoint; var Effect: LongInt;
  var Result: HResult);
var
  vNewCaret: TBufferCoord;
  DoDrop, DropAfter, DropMove: Boolean;
  vBB, vBE: TBufferCoord;
  DragDropText: string;
  ChangeScrollPastEOL: Boolean;
  FormatEtc : TFormatEtc;
  Medium : TStgMedium;
  Pt : TPoint;
begin
  Pt := ScreenToClient(MousePt);
  DropMove := Effect = DROPEFFECT_MOVE;

  IncPaintLock;
  try
    ComputeCaret(Pt.X, Pt.Y);
    vNewCaret := CaretXY;
    if not (sfOleDragSource in fStateFlags) then
    begin
      DoDrop := True;
      DropAfter := False;
    end
    else
    begin
      // Internal dragging
      vBB := BlockBegin;
      vBE := BlockEnd;
      DropAfter := (vNewCaret.Line > vBE.Line)
        or ((vNewCaret.Line = vBE.Line) and ((vNewCaret.Char > vBE.Char) or
        ((not DropMove) and (vNewCaret.Char = vBE.Char))));
      DoDrop := DropAfter or (vNewCaret.Line < vBB.Line)
        or ((vNewCaret.Line = vBB.Line) and ((vNewCaret.Char < vBB.Char) or
        ((not DropMove) and (vNewCaret.Char = vBB.Char))));
    end;

    if DoDrop then begin
      with FormatEtc do begin
        cfFormat := CF_UNICODETEXT;
        dwAspect := DVASPECT_CONTENT;
        ptd := nil;
        tymed := TYMED_HGLOBAL;
        lindex := -1;
      end;
      if DataObject.GetData(FormatEtc, Medium) = S_OK then begin
        if Medium.hGlobal <> 0 then begin
          DragDropText := PChar(GlobalLock(Medium.hGlobal));
          GlobalUnLock(Medium.hGlobal);
          DoDrop := DragDropText <> '';
        end else
          DoDrop := False;
        ReleaseStgMedium(Medium);
      end else
        DoDrop := False;
    end;

    if DoDrop then begin
      BeginUndoBlock;
      try
        // delete the selected text if necessary
        if DropMove then
        begin
          if sfOleDragSource in fStateFlags then begin
            // Internal dragging
            Effect := DROPEFFECT_NONE;  // do not clear selection after drop
            SelText := '';
            // adjust horizontal drop position
            if DropAfter and (vNewCaret.Line = vBE.Line) then
              Dec(vNewCaret.Char, vBE.Char - vBB.Char);
            // adjust vertical drop position
            if DropAfter and (vBE.Line > vBB.Line) then
              Dec(vNewCaret.Line, vBE.Line - vBB.Line);
          end;
        end;
        // insert the selected text
        ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
        try
          if ChangeScrollPastEOL then
            Include(fOptions, eoScrollPastEol);
          CaretXY := vNewCaret;
          SelText := DragDropText; // creates undo action
        finally
          if ChangeScrollPastEOL then
            Exclude(fOptions, eoScrollPastEol);
        end;
        SetCaretAndSelection(CaretXY, vNewCaret, CaretXY);
      finally
        EndUndoBlock;
      end;
    end else
      Effect := DROPEFFECT_NONE;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.OnCodeFoldingChange(Sender: TObject);
begin
  GutterChanged(Sender);
end;

function TCustomSynEdit.GetCollapseMarkRect(Row, Line: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);

  if not UseCodeFolding then
    Exit;

  if Line < 0 then
    Line := RowToLine(Row);

  if not AllFoldRanges.CollapsedFoldStartAtLine(Line) then
    Exit;

  { Prepare rect }
  with Result do
  begin
    Top := (Row - fTopLine) * fTextHeight + 1;
    Bottom := Top + fTextHeight - 2;
  end;

  Result.Left := fTextOffset +
    TSynEditStringList(fLines).TextWidth[Line-1] +  fCharWidth;

  { Fix rect }
  if scEOL in FVisibleSpecialChars then
    Inc(Result.Left, fCharWidth);

  Result.Right := Result.Left + fCharWidth * 3 +  4 * (fCharWidth div 7);
end;

procedure TCustomSynEdit.UpdateCarets;
var
  iClientRect: TRect;
  vCaretDisplay: TDisplayCoord;
  vCaretPix: TPoint;
  Sel: TSynSelection;
  Index: Integer;
begin
  if eoNoCaret in FOptions then
    Exclude(fStateFlags, sfCaretChanged)
  else if (PaintLock <> 0) or not (Focused or FAlwaysShowCaret) then
    Include(fStateFlags, sfCaretChanged)
  else
  begin
    Exclude(fStateFlags, sfCaretChanged);

    FCarets.HideCarets;
    FCarets.CaretRects.Clear;

    for Index := 0 to FSelections.Count - 1 do
    begin
      Sel  := FSelections[Index];
      if not Sel.Caret.IsValid then
        Continue;
      // The last space of a wrapped line may be out of view
      vCaretDisplay := SelectionToDisplayCoord(Sel);
      if WordWrap and not (eoWrapWithRightEdge in FOptions) and
        Sel.CaretAtEOL and not RowColumnInView(vCaretDisplay)
      then
        Dec(vCaretDisplay.Column);

      vCaretPix := RowColumnToPixels(vCaretDisplay);
      vCaretPix.Offset(FCarets.Shape.Offset);
      iClientRect := ClientRect;
      Inc(iClientRect.Left, fGutterWidth);
      if PtInRect(iClientRect, vCaretPix) then
        FCarets.CaretRects.Add(TRect.Create(vCaretPix, FCarets.Shape.Width,
          FCarets.Shape.Height));
    end;

    FCarets.ShowCarets;
    UpdateIME;
  end;
end;

procedure TCustomSynEdit.UpdateIME;
var
  BC: TBufferCoord;
  PosPix: TPoint;
begin
  if FSelection.IsEmpty then
    BC := CaretXY
  else
    BC := BlockBegin;

  PosPix := RowColumnToPixels(BufferToDisplayPos(BC));
  // Font is set in WMIMENotify
  SetImeCompositionWindow(nil, PosPix.X, PosPix.Y);
end;

procedure TCustomSynEdit.UpdateScrollBars;
begin
  if not HandleAllocated or (PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else begin
    // In case TopLine is not valid when not eoScollPastEOF in Options
    TopLine := TopLine;

    Exclude(fStateFlags, sfScrollbarChanged);

    if FSynEditScrollBars.UpdateScrollBars then
      Update
    else if FScrollbarAnnotations.Count > 0 then
      SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

function TCustomSynEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
    FSynEditScrollBars.DoMouseWheel(Shift, WheelDelta, MousePos);
  Result := True;
end;

procedure TCustomSynEdit.WMCaptureChanged(var Msg: TMessage);
begin
  fScrollTimer.Enabled := False;
  inherited;
end;

procedure TCustomSynEdit.WMClear(var Msg: TMessage);
begin
  if not ReadOnly then
    SelText := '';
end;

procedure TCustomSynEdit.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMCut(var Message: TMessage);
begin
  if not ReadOnly then
    CutToClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMDropFiles(var Msg: TMessage);
var
  i, iNumberDropped: Integer;
  FileNameW: array[0..MAX_PATH - 1] of WideChar;
  Point: TPoint;
  FilesList: TStringList;
begin
  try
    if Assigned(fOnDropFiles) then
    begin
      FilesList := TStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);

        for i := 0 to iNumberDropped - 1 do
        begin
          DragQueryFileW(THandle(Msg.wParam), i, FileNameW,
            sizeof(FileNameW) div 2);
          FilesList.Add(FileNameW)
        end;
        fOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
end;

procedure TCustomSynEdit.WMDestroy(var Message: TWMDestroy);
begin
  // See https://en.delphipraxis.net/topic/456-destroywnd-not-called-at-destruction-of-wincontrols/
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, False);

  //https://learn.microsoft.com/en-us/windows/win32/api/uiautomationcoreapi/nf-uiautomationcoreapi-uiareturnrawelementprovider
  if Assigned(FUIAutomationProvider) then
  begin
    UiaDisconnectProvider(IRawElementProviderSimple(FUIAutomationProvider));
    UiaReturnRawElementProvider(Handle, 0, 0, nil);
    FUIAutomationProvider := nil;
  end;

  RevokeDragDrop(Handle);

  inherited;
end;

procedure TCustomSynEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TCustomSynEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
  if fWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
  if fWantReturns then
    Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
end;

procedure TCustomSynEdit.WMGetObject(var Message: TMessage);
begin
  if (Message.LParam = UiaRootObjectId) and not (csDestroying in ComponentState)
   and (eoAccessibility in FOptions) then
  begin
    if FUIAutomationProvider = nil then
      FUIAutomationProvider := TSynUIAutomationProvider.Create(Self) as IRawElementProviderSimple;
    Message.Result :=  UiaReturnRawElementProvider(Handle, Message.WParam,
        Message.LParam, FUIAutomationProvider as IRawElementProviderSimple);
  end
  else
    inherited;
end;

procedure TCustomSynEdit.WMGetText(var Msg: TWMGetText);
begin
  Msg.Result := StrLen(StrLCopy(PChar(Msg.Text), PChar(Text), Msg.TextMax - 1));
end;

procedure TCustomSynEdit.WMGetTextLength(var Msg: TWMGetTextLength);
begin
  // Avoid (useless) temporary copy of WindowText while window is recreated
  // because of docking.
  if csDocking in ControlState then
    Msg.Result := 0
  else
    Msg.Result := Length(Text);
end;

procedure TCustomSynEdit.WMHScroll(var Msg: TWMScroll);
begin
  FSynEditScrollBars.WMHScroll(Msg);
end;

procedure TCustomSynEdit.WMImeChar(var Msg: TMessage);
begin
  // do nothing here, the IME string is retrieved in WMImeComposition

  // Handling the WM_IME_CHAR message stops Windows from sending WM_CHAR
  // messages while using the IME
end;

procedure TCustomSynEdit.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  PW: PWideChar;
  ImeCount: Integer;
begin
  if (Msg.LParam and GCS_RESULTSTR) <> 0 then
  begin
    imc := ImmGetContext(Handle);
    try
      ImeCount := ImmGetCompositionStringW(imc, GCS_RESULTSTR, nil, 0);
      // ImeCount is always the size in bytes, also for Unicode
      GetMem(PW, ImeCount + sizeof(WideChar));
      try
        ImmGetCompositionStringW(imc, GCS_RESULTSTR, PW, ImeCount);
        PW[ImeCount div sizeof(WideChar)] := #0;
        CommandProcessor(ecImeStr, #0, PW);
      finally
        FreeMem(PW);
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMImeNotify(var Msg: TMessage);
var
  imc: HIMC;
  LogFontW: TLogFontW;
begin
  with Msg do
  begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if imc <> 0 then
          begin
            GetObjectW(Font.Handle, SizeOf(TLogFontW), @LogFontW);
            ImmSetCompositionFontW(imc, @LogFontW);
            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMImeRequest(var Message: TMessage);
type
  // Reconversion string.
  PReconvertString = ^TReconvertString;
  TReconvertString = record
    dwSize: DWord;
    dwVersion: DWord;
    dwStrLen: DWord;
    dwStrOffset: DWord;
    dwCompStrLen: DWord;
    dwCompStrOffset: DWord;
    dwTargetStrLen: DWord;
    dwTargetStrOffset: DWord;
  end;

const
  IMR_RECONVERTSTRING           =  $0004;
  IMR_DOCUMENTFEED              =  $0007;
  SCS_QUERYRECONVERTSTRING      = $00020000;

var
  pReconvert: PReconvertString;
  TargetText: string;
  TargetByteLength: Integer;
  pTarget: PChar;
  H: HIMC;
begin
  case Message.WParam of
    IMR_RECONVERTSTRING:
      begin
        // Reconversion string
        if (Self.SelLength <> 0) then
        begin
          TargetText := Self.SelText;
        end
        else
        begin
          if (Self.Lines.Count >= Self.CaretY - 1) then
            TargetText := Self.Lines[Self.CaretY - 1]
          else
            TargetText := '';
        end;
        TargetByteLength := Length(TargetText) * sizeof(Char);
        if (Message.LParam = 0) then
        begin
          // 1st time (get buffer size (bytes))
          // Select only one row
          if (Self.BlockBegin.Line = Self.BlockEnd.Line) then
            Message.Result := Sizeof(TReconvertString) + TargetByteLength
          else
            Message.Result := 0;
        end
        else
        begin
          // 2nd time
          pReconvert := Pointer(Message.LParam);
          pReconvert.dwSize := Sizeof(TReconvertString);
          pReconvert.dwVersion := 0;
          pReconvert.dwStrLen := Length(TargetText);
          pReconvert.dwStrOffset := Sizeof(TReconvertString);
          pTarget := Pointer(Message.LParam + Sizeof(TReconvertString));
          move(TargetText[1], pTarget^, TargetByteLength);
          if (Self.SelLength <> 0) then
          begin
            pReconvert.dwTargetStrLen := 0;
            pReconvert.dwTargetStrOffset := 0;
            pReconvert.dwCompStrLen := Length(TargetText);
            pReconvert.dwCompStrOffset := 0;
          end
          else
          begin
            pReconvert.dwTargetStrLen := 0;
            pReconvert.dwTargetStrOffset := (Self.CaretX - 1) * sizeof(Char);
            H := Imm32GetContext(Handle);
            try
              ImmSetCompositionString(H, SCS_QUERYRECONVERTSTRING, pReconvert, Sizeof(TReconvertString) + TargetByteLength, nil, 0);
              if (pReconvert.dwCompStrLen <> 0) then
              begin
                Self.CaretX := pReconvert.dwCompStrOffset div sizeof(Char) + 1;
                Self.SelStart := RowColToCharIndex(Self.CaretXY);
                Self.SelLength := pReconvert.dwCompStrLen;
              end;
            finally
              Imm32ReleaseContext(Handle, H);
            end;
          end;
          Message.Result := Sizeof(TReconvertString) + TargetByteLength;
        end;
      end;
    IMR_DOCUMENTFEED:
      begin
        // Notifies an application when the selected IME needs the converted string from the application.
        if (Self.Lines.Count >= Self.CaretY) then
          TargetText := Self.Lines[Self.CaretY]
        else
          TargetText := '';
        if (Message.LParam = 0) then
        begin
          // 1st time (get line size (bytes))
          Message.Result := Sizeof(TReconvertString) + Length(TargetText) * sizeof(Char);
        end
        else
        begin
          // 2nd time
          pReconvert := Pointer(Message.LParam);
          pReconvert.dwSize := Sizeof(TReconvertString);
          pReconvert.dwVersion := 0;
          pReconvert.dwStrLen := Length(TargetText);
          pReconvert.dwStrOffset := Sizeof(TReconvertString);
          pReconvert.dwCompStrLen := 0;
          pReconvert.dwCompStrOffset := 0;
          pReconvert.dwTargetStrLen := 0;
          pReconvert.dwTargetStrOffset := (Self.CaretX - 1) * sizeof(Char);
          pTarget := Pointer(Message.LParam + Sizeof(TReconvertString));
          if TargetText <> '' then
            move(TargetText[1], pTarget^, Length(TargetText) * sizeof(Char));
          Message.Result := Sizeof(TReconvertString) + Length(TargetText) * sizeof(Char);
        end;
      end;
  end;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CommandProcessor(ecLostFocus, #0, nil);

  //Added check for focused to prevent caret disappearing problem
  if not (Focused or FAlwaysShowCaret) then
    FCarets.HideCarets;
  if FHideSelection and not FSelections.IsEmpty then
    FSelections.InvalidateAll;
end;

procedure TCustomSynEdit.WMMouseHWheel(var Message: TWMMouseWheel);
Var
  Shift: TShiftState;
  WheelDelta: SmallInt;
  MousePos: TSmallPoint;
begin
  // Message.Keys does not always contain correct shift state
  Shift := KeyboardStateToShiftState;
  Include(Shift, System.Classes.ssHorizontal);
  WheelDelta := - Message.WheelDelta; // HWheel directions are reversed from Wheel
  MousePos := Message.Pos;
  // Choosing not to call inherited DoMouseWheel as this would most likely
  // cause issues with handlers that don't support ssHorizontal
  FSynEditScrollBars.DoMouseWheel(Shift, WheelDelta, MousePos);
  Message.Result := 1;
end;

procedure TCustomSynEdit.WMPaint(var Message: TWMPaint);
begin
  DoOnPaintTransient(ttBefore);
  try
    // Paint everything while the caret is hidden.
    FCarets.HideCarets;
    inherited;
    UpdateCarets;
  finally
    DoOnPaintTransient(ttAfter);
  end;
end;

procedure TCustomSynEdit.WMPaste(var Message: TMessage);
begin
  if not ReadOnly then
    PasteFromClipboard;
  Message.Result := ord(True);
end;

procedure TCustomSynEdit.WMCancelMode(var Message:TMessage);
begin

end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  CommandProcessor(ecGotFocus, #0, nil);

  InitializeCaret;
  if FHideSelection and not FSelections.IsEmpty then
    FSelections.InvalidateAll;
end;

procedure TCustomSynEdit.WMSetText(var Msg: TWMSetText);
begin
  Msg.Result := 1;
  Text := PWideChar(Msg.Text)
end;

procedure TCustomSynEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  SizeOrFontChanged(False);

  if Assigned(FUIAutomationProvider) and UiaClientsAreListening then
    (FUIAutomationProvider as TSynUIAutomationProvider).NotifyBoundingRectangleChange;
end;

procedure TCustomSynEdit.WMUndo(var Msg: TMessage);
begin
  Undo;
end;

procedure TCustomSynEdit.WMVScroll(var Msg: TWMScroll);
begin
  FSynEditScrollBars.WMVScroll(Msg);
end;

function TCustomSynEdit.ScanFrom(Index: Integer): Integer;
var
  iRange: TSynEditRange;
begin
  Result := Index;
  if Result >= Lines.Count then Exit;

  if Result = 0 then
    fHighlighter.ResetRange
  else
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Result - 1]);

  repeat
    fHighlighter.SetLine(Lines[Result], Result);
    fHighlighter.NextToEol;
    iRange := fHighlighter.GetRange;
    if TSynEditStringList(Lines).Ranges[Result] = iRange then
      Exit; // avoid the final Decrement
    TSynEditStringList(Lines).Ranges[Result] := iRange;
    Inc(Result);
  until (Result = Lines.Count);
  Dec(Result);
end;

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  FSelections.Clear;

  if WordWrap then
    fWordWrapPlugin.Reset;

//++ CodeFolding
  if UseCodeFolding then
    AllFoldRanges.Reset;
//-- CodeFolding

  fMarkList.Clear; // fMarkList.Clear also frees all bookmarks,
  FillChar(fBookMarks, sizeof(fBookMarks), 0); // so fBookMarks should be cleared too

  ClearUndo;
  // invalidate the *whole* client area
  Invalidate;
  // set caret and selected block to start of text
  CaretXY := BufferCoord(1, 1);
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(fStatusChanges, scAll);
end;

procedure TCustomSynEdit.ListBeforeDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  DoLinesBeforeDeleted(aIndex, aCount);
end;

procedure TCustomSynEdit.ListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
Var
  vLastScan: Integer;
begin
  if WordWrap then
    fWordWrapPlugin.LinesDeleted(aIndex, aCount);

  vLastScan := aIndex;
  if Assigned(fHighlighter) and (Lines.Count > 0) then
    vLastScan := ScanFrom(aIndex);

  if UseCodeFolding then begin
    AllFoldRanges.LinesDeleted(aIndex, aCount);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(aIndex, vLastScan);
  end;

  DoLinesDeleted(aIndex, aCount);

  InvalidateLines(aIndex + 1, MaxInt);
  InvalidateGutterLines(aIndex + 1, MaxInt);
  Include(fStateFlags, sfScrollbarChanged);
end;

procedure TCustomSynEdit.ListInserted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  vLastScan: Integer;
  FoldIndex: Integer;
begin
  if WordWrap then
    fWordWrapPlugin.LinesInserted(Index, aCount);

  vLastScan := Index;
  if Assigned(fHighlighter) and (Lines.Count > 0) then
  begin
    repeat
      vLastScan := ScanFrom(vLastScan);
      Inc(vLastScan);
    until vLastScan >= Index + aCount;
  end;

  if UseCodeFolding then begin
    if fAllFoldRanges.CollapsedFoldStartAtLine(Index, FoldIndex) then
      // insertion starts at collapsed fold
      Uncollapse(FoldIndex);
    AllFoldRanges.LinesInserted(Index, aCount);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(Index, vLastScan-1);
  end;

  DoLinesInserted(Index, aCount);

  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);
  // Flicker Reduction
  Include(fStateFlags, sfScrollbarChanged);
end;

procedure TCustomSynEdit.ListPut(Sender: TObject; Index: Integer;
  const OldLine: string);
var
  vEndLine: Integer;
  vLastScan: Integer;
  FoldIndex: Integer;
begin
  vEndLine := Index +1;
  if WordWrap and (fWordWrapPlugin.LinePut(Index, OldLine) <> 0) then
    vEndLine := MaxInt;
  vLastScan := Index;
  if Assigned(fHighlighter) then
  begin
    vLastScan := ScanFrom(Index);
    vEndLine := Max(vEndLine, vLastScan + 1);
    // If this editor is chained then the real owner of text buffer will probably
    // have already parsed the changes, so ScanFrom will return immediately.
    if fLines <> fOrigLines then
      vEndLine := MaxInt;
  end;

  if fUseCodeFolding then begin
    if fAllFoldRanges.CollapsedFoldStartAtLine(Index + 1, FoldIndex) then
      // modification happens at collapsed fold
      Uncollapse(FoldIndex);
    AllFoldRanges.LinePut(Index, OldLine);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(Index, vLastScan);
  end;

  DoLinePut(Index, OldLine);

  InvalidateLines(Index + 1, vEndLine);
  InvalidateGutterLines(Index + 1, vEndLine);
  Include(fStateFlags, sfScrollbarChanged);
end;

procedure TCustomSynEdit.ScanRanges;
var
  i: Integer;
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    fHighlighter.ResetRange;
    i := 0;
    repeat
      fHighlighter.SetLine(Lines[i], i);
      fHighlighter.NextToEol;
      TSynEditStringList(Lines).Ranges[i] := fHighlighter.GetRange;
      Inc(i);
    until i >= Lines.Count;
  end;
end;

procedure TCustomSynEdit.SetWordBlock(Value: TBufferCoord);
var
  BB: TBufferCoord;
  BE: TBufferCoord;
begin
  GetWordBoundaries(Value, BB, BE);
  SetCaretAndSelection(BE, BB, BE);
end;

procedure TCustomSynEdit.DblClick;
var
  ptMouse: TPoint;
begin
  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + fTextMargin then
  begin
    if not (eoNoSelection in fOptions) then
      SetWordBlock(CaretXY);
    inherited;
  end
  else
    inherited;
end;

function TCustomSynEdit.GetCanUndo: Boolean;
begin
  Result := not ReadOnly and fUndoRedo.CanUndo;
end;

function TCustomSynEdit.GetCaseSensitive: Boolean;
begin
  if Assigned(fHighlighter) then
    Result := fHighlighter.CaseSensitive
  else
    Result := FCaseSensitive;
end;

function TCustomSynEdit.GetCanRedo: Boolean;
begin
  Result := not ReadOnly and fUndoRedo.CanRedo;
end;

function TCustomSynEdit.GetCanPaste;
begin
  Result := not ReadOnly and ClipboardProvidesText;
end;

procedure TCustomSynEdit.Redo;
begin
  if ReadOnly then
    exit;

  DoOnPaintTransient(ttBefore);
  IncPaintLock;
  Lines.BeginUpdate;
  try
    FUndoRedo.Redo(Self);
  finally
    Lines.EndUpdate;
    DecPaintLock;
    DoOnPaintTransient(ttAfter);
  end;
end;

procedure TCustomSynEdit.Collapse(FoldRangeIndex: Integer; Invalidate:Boolean);
var
  Range: TSynFoldRange;
begin
  if not fUseCodeFolding then Exit;

  if AllFoldRanges.Collapse(FoldRangeIndex) then
  begin
    Range := AllFoldRanges[FoldRangeIndex];

    // Extract caret from fold
    if (CaretY > Range.FromLine) and (CaretY <= Range.ToLine) then
      CaretXY := BufferCoord(Length(Lines[Range.FromLine - 1]) + 1, Range.FromLine);

    if Invalidate then begin
      // Redraw the collapsed line and below
      InvalidateLines(Range.FromLine, MaxInt);

      // Redraw fold mark
      InvalidateGutterLines(Range.FromLine, MaxInt);

      UpdateScrollBars;
    end else
      // Update Scrollbars
      Include(fStateFlags, sfScrollbarChanged);
  end;
end;

procedure TCustomSynEdit.CollapseAll;
begin
  if not fUseCodeFolding then Exit;

  fAllFoldRanges.CollapseAll;
  SurfaceCaretFromHiddenFolds;

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
  UpdateScrollBars;
end;


procedure TCustomSynEdit.CollapseLevel(Level: Integer);
begin
  if not fUseCodeFolding then Exit;

  AllFoldRanges.CollapseLevel(Level);
  SurfaceCaretFromHiddenFolds;

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.CollapseNearest;
var
  Index : integer;
begin
  if not fUseCodeFolding then Exit;

  if AllFoldRanges.FoldAroundLineEx(CaretY, False, True, True, Index) then
    Collapse(Index);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.CollapseFoldType(FoldType : Integer);
begin
  if not fUseCodeFolding then Exit;

  AllFoldRanges.CollapseFoldType(FoldType);
  SurfaceCaretFromHiddenFolds;

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.Uncollapse(FoldRangeIndex: Integer; Invalidate:Boolean);
begin
  AllFoldRanges.UnCollapse(FoldRangeIndex);

  if Invalidate then with AllFoldRanges.Ranges[FoldRangeIndex] do
  begin
    // Redraw the uncollapsed line and below
    InvalidateLines(FromLine, MaxInt);

    // Redraw fold marks
    InvalidateGutterLines(FromLine, MaxInt);

    // Make sure we can see the cursor
    // EnsureCursorPosVisible;

    UpdateScrollBars;
  end else
    // Update Scrollbars
    Include(fStateFlags, sfScrollbarChanged);
end;

procedure TCustomSynEdit.UncollapseAroundLine(Line: Integer);
var
  Index: Integer;
begin
  if not fUseCodeFolding then Exit;
  // Open up the closed folds around the focused line until we can see the line we're looking for
  while AllFoldRanges.FoldHidesLine(Line, Index) do
    Uncollapse(Index);
end;

procedure TCustomSynEdit.UnCollapseLevel(Level: Integer);
begin
  if not fUseCodeFolding then Exit;

  AllFoldRanges.UnCollapseLevel(Level);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.UncollapseNearest;
Var
  Index : integer;
begin
  if not fUseCodeFolding then Exit;
  if AllFoldRanges.CollapsedFoldStartAtLine(CaretY, Index) then
    Uncollapse(Index);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.UnCollapseFoldType(FoldType : Integer);
begin
  if not fUseCodeFolding then Exit;

  AllFoldRanges.UnCollapseFoldType(FoldType);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.DoMouseSelectLineRange(NewPos: TBufferCoord);
{ Select whole lines }
var
  BB, BE: TBufferCoord;
begin
  BB := BlockBegin;
  BE := BlockEnd;

  //  Set AnchorLine
  if CaretXY >= BE then
  begin
    if BB.Line < Lines.Count then
      BE := BufferCoord(1, BB.Line + 1)
    else
      BE := BufferCoord(Length(Lines[BB.Line - 1]), BB.Line);
  end
  else
  begin
    BB := BufferCoord(1, Max(1, IfThen(BE.Line < Lines.Count, BE.Line - 1, BE.Line)));
  end;

  if NewPos >= BB then
  begin
    BB := BufferCoord(1, BB.Line);
    if NewPos.Line < Lines.Count then
      BE := BufferCoord(1, NewPos.Line + 1)
    else
      BE := BufferCoord(Length(Lines[NewPos.Line - 1]), NewPos.Line);
  end
  else
  begin
    BB := BE;
    BE := BufferCoord(1, NewPos.Line);
  end;

  SetCaretAndSelection(BE, BB, BE);
end;

procedure TCustomSynEdit.DoMouseSelectWordRange(NewPos: TBufferCoord);
{ Select whole words }
var
  BB, BE: TBufferCoord;
begin
  //  Set Anchor Selection (Word)
  BB := BlockBegin;
  BE := BlockEnd;
  if CaretXY > BB then
    BE := WordEndEx(BB)
  else
    BB := WordStartEx(BE);

  NewPos.Char := Min(NewPos.Char, Lines[NewPos.Line-1].Length + 1);
  if NewPos > BE then begin
    BE := NewPos;
    if (BE.Char > 1) and IsIdentChar(Lines[BE.Line-1][BE.Char - 1]) then
      BE := WordEndEx(BE);
  end else if NewPos < BB then begin
    BB := BE;
    BE := NewPos;
    if (BE.Char < Lines[BE.Line-1].Length) and IsIdentChar(Lines[BE.Line-1][BE.Char]) then
      BE := WordStartEx(BE);
  end;

  SetCaretAndSelection(BE, BB, BE);
end;

procedure TCustomSynEdit.ExecCmdCopyOrMoveLine(const Command: TSynEditorCommand);
var
  vCaretRow, SelShift: Integer;
  Caret, BB, BE: TBufferCoord;
  StartBC, EndBC: TBufferCoord;
  Text: string;
begin
  if not ReadOnly and
    ((Command <> ecMoveLineUp) or (BlockBegin.Line > 1)) and
    ((Command <> ecMoveLineDown) or (BlockEnd.Line < Lines.Count)) then
  begin
    // Get Caret and selection
    Caret := CaretXY;
    StartBC := FSelection.Start;
    EndBC := FSelection.Stop;
    SelShift := Succ(Abs(EndBC.Line - StartBC.Line));

     //BB and BE define where insertion of Text will take place
    // SelShift is the number lines the Selection is shifted up or down
    case Command of
      ecCopyLineUp:
      begin
        BB := BufferCoord(1, BlockBegin.Line);
        BE := BB;
        SelShift := 0;
      end;
      ecMoveLineUp:
      begin
        BB := BufferCoord(1, Pred(BlockBegin.Line));
        if (StartBC.Line <> EndBC.Line) and (BlockEnd.Char = 1) then
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 2])), BlockEnd.Line - 1)
        else
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 1])), BlockEnd.Line);
        SelShift := -1;
      end;
      ecCopyLineDown:
      begin
        if (StartBC.Line <> EndBC.Line) and (BlockEnd.Char = 1) then begin
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 2])), BlockEnd.Line - 1);
          Dec(SelShift);
        end else
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 1])), BlockEnd.Line);
        BB := BE;
      end;
      ecMoveLineDown:
      begin
        BB := BufferCoord(1, BlockBegin.Line);
        if (StartBC.Line <> EndBC.Line) and (BlockEnd.Char = 1) then
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 1])), BlockEnd.Line)
        else
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line])), BlockEnd.Line + 1);
        SelShift := 1;
      end;
      else
        Exit;   //should not happen
    end;
    // store the lines into Text
    Text := '';
    for vCaretRow := BlockBegin.Line to BlockEnd.Line do
    begin
      if (vCaretRow = BlockEnd.Line) and
        (StartBC.Line <> EndBC.Line) and (BlockEnd.Char = 1)
      then
        break;
      if (Command = ecCopyLineDown) or (Command = ecMoveLineDown) then
        Text := Text + SLineBreak + Lines[vCaretRow - 1]
      else
        Text := Text + Lines[vCaretRow - 1] + SLineBreak;
    end;
    // Add the line over which we move
    if Command = ecMoveLineDown then
      Text := Lines[BE.Line - 1] + Text
    else if Command = ecMoveLineUp then
      Text := Text + Lines[BB.Line-1];

    // group undo redo actions and reduce transient painting
    BeginUndoBlock;
    try
      // Insert/replace text at selection BB-BE
      SetCaretAndSelection(BB, BB, BE);
      SetSelText(Text);

      // Set as new selection the shifted old one
      if SelShift <> 0 then
      begin
        Inc(Caret.Line, SelShift);
        Inc(StartBC.Line, SelShift);
        Inc(EndBC.Line, SelShift);
      end;
      SetCaretAndSelection(Caret, StartBC, EndBC);
    finally
      EndUndoBlock;
    end;
  end;
end;

procedure TCustomSynEdit.ExecCmdDeleteLine;
var
  BB, BE: TBufferCoord;
begin
  if not ReadOnly and (Lines.Count > 0) and not
    ((BlockBegin.Line = Lines.Count) and (Length(Lines[BlockBegin.Line - 1]) = 0)) then
  begin
    BeginUndoBlock;
    try
      // Normalize selection
      FSelection.Normalize;
      BB := FSelection.Start;
      BE := FSelection.Stop;
      BB.Char := 1;
      if (BB.Line = BE.Line) or (BE.Char > 1) then
      begin
        if BE.Line = Lines.Count then
          BE.Char := Length(Lines[BE.Line - 1]) + 1
        else
          BE := BufferCoord(1, Succ(BE.Line));
      end;
      SetCaretAndSelection(BB, BB, BE);
      SetSelText('');
    finally
      EndUndoBlock;
    end;
  end;
end;

procedure TCustomSynEdit.UncollapseAll;
var
  i: Integer;
begin
  if not fUseCodeFolding then Exit;
  for i := fAllFoldRanges.Count - 1 downto 0 do
      Uncollapse(i, False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  UpdateScrollBars;
  EnsureCursorPosVisible;
end;

//-- CodeFolding

procedure TCustomSynEdit.Undo;
begin
  if ReadOnly then
    exit;

  DoOnPaintTransient(ttBefore);
  IncPaintLock;
  Lines.BeginUpdate;
  try
    FUndoRedo.Undo(Self);
  finally
    Lines.EndUpdate;
    DecPaintLock;
    DoOnPaintTransient(ttAfter);
  end;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then
  begin
    DoOnClearBookmark(fBookMarks[BookMark]);
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark] := nil;
  end
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  iNewPos: TBufferCoord;
begin
  if (BookMark in [0..9]) and
     assigned(fBookMarks[BookMark]) and
     (fBookMarks[BookMark].Line <= fLines.Count)
  then
  begin
    iNewPos.Char := fBookMarks[BookMark].Char;
    iNewPos.Line := fBookMarks[BookMark].Line;
    //call it this way instead to make sure that the caret ends up in the middle
    //if it is off screen (like Delphi does with bookmarks)
    SetCaretAndSelection(iNewPos, iNewPos, iNewPos, True, True);
  end;
end;

procedure TCustomSynEdit.GotoLineAndCenter(ALine: Integer);
var
  iNewPos: TBufferCoord;
begin
  iNewPos := BufferCoord(1, ALine);
  SetCaretAndSelection(iNewPos, iNewPos, iNewPos, True, True);
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  mark: TSynEditMark;
begin
  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, fLines.Count)) then
  begin
    mark := TSynEditMark.Create(self);
    with mark do
    begin
      Line := Y;
      Char := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := True;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    DoOnPlaceMark(Mark);
    if (mark <> nil) then
    begin
      if assigned(fBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      fBookMarks[BookMark] := mark;
      FMarkList.Add(fBookMarks[BookMark]);
    end;
  end;
end;

procedure TCustomSynEdit.WndProc(var Msg: TMessage);
const
  ALT_KEY_DOWN = $20000000;
begin
  // Prevent Alt+ from beeping
  if (Msg.Msg = WM_SYSCHAR) and (Msg.lParam and ALT_KEY_DOWN <> 0) and
    (Msg.wParam in [VK_BACK, Ord('+'), Ord('-')])
  then
    Msg.Msg := 0;

  inherited;
end;

procedure TCustomSynEdit.ChainListCleared(Sender: TObject);
begin
  if Assigned(fChainListCleared) then
    fChainListCleared(Sender);
  TSynEditStringList(fOrigLines).OnCleared(Sender);
end;

procedure TCustomSynEdit.ChainListDeleted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListDeleted) then
    fChainListDeleted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnDeleted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListInserted(Sender: TObject; aIndex: Integer;
  aCount: Integer);
begin
  if Assigned(fChainListInserted) then
    fChainListInserted(Sender, aIndex, aCount);
  TSynEditStringList(fOrigLines).OnInserted(Sender, aIndex, aCount);
end;

procedure TCustomSynEdit.ChainListPut(Sender: TObject; aIndex: Integer;
  const OldLine: string);
begin
  if Assigned(fChainListPut) then
    fChainListPut(Sender, aIndex, OldLine);
  TSynEditStringList(fOrigLines).OnPut(Sender, aIndex, OldLine);
end;

procedure TCustomSynEdit.ChainLinesChanging(Sender: TObject);
begin
  if Assigned(fChainLinesChanging) then
    fChainLinesChanging(Sender);
  TSynEditStringList(fOrigLines).OnChanging(Sender);
end;

procedure TCustomSynEdit.ChainLinesChanged(Sender: TObject);
begin
  if Assigned(fChainLinesChanged) then
    fChainLinesChanged(Sender);
  TSynEditStringList(fOrigLines).OnChange(Sender);
end;

procedure TCustomSynEdit.ChainModifiedChanged(Sender: TObject);
begin
  if Assigned(fChainModifiedChanged) then
     fChainModifiedChanged(Sender);
  FOrigUndoRedo.OnModifiedChanged(Sender);
end;

procedure TCustomSynEdit.ChangeScale(M, D: Integer{$if CompilerVersion >= 31}; isDpiChange: Boolean{$endif});
begin
  {$if CompilerVersion >= 31}if isDpiChange then begin{$endif}
  IncPaintLock;
  try
    fExtraLineSpacing := MulDiv(fExtraLineSpacing, M, D);
    fTextMargin := MulDiv(fTextMargin, M, D);
    FCarets.CaretSize := MulDiv(FCarets.CaretSize, M, D);
    fGutter.ChangeScale(M,D);
    fBookMarkOpt.ChangeScale(M, D);
    fWordWrapGlyph.ChangeScale(M, D);
    // Adjust Font.PixelsPerInch so that Font.Size is correct
    // Delphi should be doing that but it doesn't
    {$if CompilerVersion < 36}
    Font.PixelsPerInch := M;
    {$endif}
  finally
    DecPaintLock;
  end;
  {$if CompilerVersion >= 31}end;{$endif}
  inherited ChangeScale(M, D{$if CompilerVersion >= 31}, isDpiChange{$endif});
 end;

procedure TCustomSynEdit.UnHookTextBuffer;
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  if fLines = fOrigLines then
    Exit;

  vOldWrap := WordWrap;
  WordWrap := False;

  //first put back the real methods
  with TSynEditStringList(fLines) do
  begin
    OnCleared := fChainListCleared;
    OnDeleted := fChainListDeleted;
    OnInserted := fChainListInserted;
    OnPut := fChainListPut;
    OnChanging := fChainLinesChanging;
    OnChange := fChainLinesChanged;
  end;
  fUndoRedo.OnModifiedChanged := fChainModifiedChanged;

  fChainListCleared := nil;
  fChainListDeleted := nil;
  fChainListInserted := nil;
  fChainListPut := nil;
  fChainLinesChanging := nil;
  fChainLinesChanged := nil;
  fChainModifiedChanged := nil;

  //make the switch
  fLines := fOrigLines;
  fUndoRedo := fOrigUndoRedo;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.HookTextBuffer(aBuffer: TSynEditStringList; aUndoRedo:
    ISynEditUndo);
var
  vOldWrap: Boolean;
begin
  Assert(fChainedEditor = nil);
  Assert(fLines = fOrigLines);

  vOldWrap := WordWrap;
  WordWrap := False;

  if fChainedEditor <> nil then
    RemoveLinesPointer
  else if fLines <> fOrigLines then
    UnHookTextBuffer;

  //store the current values and put in the chained methods
  fChainListCleared := aBuffer.OnCleared;
    aBuffer.OnCleared := ChainListCleared;
  fChainListDeleted := aBuffer.OnDeleted;
    aBuffer.OnDeleted := ChainListDeleted;
  fChainListInserted := aBuffer.OnInserted;
    aBuffer.OnInserted := ChainListInserted;
  fChainListPut := aBuffer.OnPut;
    aBuffer.OnPut := ChainListPut;
  fChainLinesChanging := aBuffer.OnChanging;
    aBuffer.OnChanging := ChainLinesChanging;
  fChainLinesChanged := aBuffer.OnChange;
    aBuffer.OnChange := ChainLinesChanged;

  fChainModifiedChanged := aUndoRedo.OnModifiedChanged;
    aUndoRedo.OnModifiedChanged := ChainModifiedChanged;

  //make the switch
  fLines := aBuffer;
  fUndoRedo := aUndoRedo;
  LinesHookChanged;

  WordWrap := vOldWrap;
end;

procedure TCustomSynEdit.LinesHookChanged;
begin
  Invalidate;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.SetLinesPointer(ASynEdit: TCustomSynEdit);
begin
  HookTextBuffer(TSynEditStringList(ASynEdit.Lines), ASynEdit.UndoRedo);

  fChainedEditor := ASynEdit;
  ASynEdit.FreeNotification(Self);
end;

procedure TCustomSynEdit.RemoveLinesPointer;
begin
  if Assigned(fChainedEditor) then
    RemoveFreeNotification(fChainedEditor);
  fChainedEditor := nil;

  UnHookTextBuffer;
end;

procedure TCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then
  begin
    fRightEdge := Value;
    // when wrapping with right edge, we must rewrap when edge is changed
    if WordWrap and (eoWrapWithRightEdge in fOptions) then
    begin
      CalcTextAreaWidth;
      fWordWrapPlugin.DisplayChanged;
      EnsureCursorPosVisible;
    end;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: Integer;
  rcInval: TRect;
begin
  if fRightEdgeColor <> Value then
  begin
    fRightEdgeColor := Value;
    if HandleAllocated then
    begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, Height);
      InvalidateRect(rcInval, False);
    end;
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  result := fUndoRedo.MaxUndoActions;
end;

function TCustomSynEdit.GetModified: Boolean;
begin
  Result := fUndoRedo.Modified;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  fUndoRedo.MaxUndoActions := Value;
end;

procedure TCustomSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = fSearchEngine then
    begin
      SearchEngine := nil;
    end;

    if AComponent = fHighlighter then
    begin
      Highlighter := nil;
    end;

    if AComponent = fChainedEditor then
    begin
      RemoveLinesPointer;
    end;

    if (fBookmarkOpt <> nil) then
      if (AComponent = fBookmarkOpt.BookmarkImages) then
      begin
        fBookmarkOpt.BookmarkImages := nil;
        InvalidateGutterLines(-1, -1);
      end;
  end;
end;

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
var
  OldUseCodeFolding : Boolean;
begin
  if Value <> fHighlighter then
  begin
    if Assigned(fHighlighter) then
    begin
      fHighlighter.UnhookAttrChangeEvent(HighlighterAttrChanged);
      fHighlighter.RemoveFreeNotification(Self);
    end;
    if Assigned(Value) then
    begin
      Value.HookAttrChangeEvent(HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighlighter := Value;
    if not(csDestroying in ComponentState) then
      HighlighterAttrChanged(fHighlighter);

    //  Disable Code Folding if not supported by highlighter
    //  If Loading then this is taken care of in Loaded
    if not (csLoading  in ComponentState) then
    begin
      OldUseCodeFolding := fUseCodeFolding;
      UseCodeFolding := False;
      UseCodeFolding := OldUseCodeFolding;
    end;
  end;
end;

procedure TCustomSynEdit.SetBorderStyle(Value: TSynBorderStyle);
begin
  if fBorderStyle <> Value then
  begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomSynEdit.SetHideSelection(const Value: Boolean);
begin
  if fHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateSelection;
  end;
end;

procedure TCustomSynEdit.SetInsertMode(const Value: Boolean);
begin
  if fInserting <> Value then
  begin
    fInserting := Value;
    if not (csDesigning in ComponentState) then
      // Reset the caret.
      InitializeCaret;
    StatusChanged([scInsertMode]);
  end;
end;

procedure TCustomSynEdit.InitializeCaret;
var
  ct: TSynEditCaretType;
begin
  if InsertMode then
    ct := FInsertCaret
  else
    ct := FOverwriteCaret;
  case ct of
    ctHorizontalLine:
      FCarets.Shape.Create(fCharWidth, FCarets.CaretSize, Point(0, fTextHeight - FCarets.CaretSize));
    ctHalfBlock:
      FCarets.Shape.Create(fCharWidth, (fTextHeight - FCarets.CaretSize) div 2,
        Point(0, (fTextHeight - FCarets.CaretSize) div 2));
    ctBlock:
      FCarets.Shape.Create(fCharWidth, fTextHeight - FCarets.CaretSize, Point(0, 0));
    else // ctVerticalLine
      FCarets.Shape.Create(FCarets.CaretSize, fTextHeight - FCarets.CaretSize, Point(-1, 0));
  end;
  fCarets.HideCarets;

  if Focused or FAlwaysShowCaret then
    UpdateCarets;
end;

procedure TCustomSynEdit.InsertCharAtCursor(const AChar: string);
{ AChar can be a multi-codepoint character }

  function DeleteGrapheme(var S: string; Index: Integer): Boolean;
  var
    After: string;
    GraphemeEnd: Integer;
  begin
    After := Copy(S, Index);
    if After.Length = 0 then Exit(False);

    GraphemeEnd := ValidTextPos(After, 2, True);
    Delete(After, 1, GraphemeEnd - 1);

    S := Copy(S, 1, Index - 1) + After;
    Result := True;
  end;

var
  SLine, Grapheme: string;
  SpaceBuffer: string;
  Len, CaretXNew: Integer;
  OldRow: Integer;
begin
  if ReadOnly or ((AChar.Length = 1)
  and ((AChar[1] < #32) or (AChar[1] = #127))) // #127 is Ctrl+Backspace
  then
    Exit;

  if SelAvail then
    SetSelText(AChar)
  else
  begin
    // This is to set fCaretXY correctly
    OldRow := BufferToDisplayPos(BufferCoord(Max(CaretX - 1, 1), CaretY)).Row;
    SLine := LineText;
    Len := SLine.Length;
    if Len < CaretX then
    begin
      if (Len > 0) then
        SpaceBuffer := StringofChar(#32, CaretX - Len - Ord(fInserting))
      else
        SpaceBuffer := GetLeftSpacing(CaretX - Len - Ord(fInserting), True);

      SLine := SLine + SpaceBuffer;
    end;

    CaretXNew := IfThen(Len = 0, Length(SLine) + Ord(fInserting), CaretX);
    if fInserting then
    begin
      Insert(AChar, SLine, CaretXNew);
      Lines[CaretY - 1] := SLine;
    end
    else begin
      // Deal with multi-codepoint graphemes like emojis
      // Delete as many graphemes as in AChar
      for Grapheme in Graphemes(AChar) do
        if not DeleteGrapheme(SLine, CaretXNew) then Break;

      Insert(AChar, SLine, CaretXNew);
      Lines[CaretY - 1] := SLine;
    end;
    SetCaretInRow(BufferCoord(CaretXNew + AChar.Length, CaretY), OldRow);

    if not CaretInView then
      LeftChar := LeftChar + Min(25, FTextAreaWidth div FCharWidth);
  end;
end;

procedure TCustomSynEdit.SetIndentGuides(const Value: TSynIndentGuides);
begin
  FIndentGuides.Assign(Value);
end;

procedure TCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if FInsertCaret <> Value then
  begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if FOverwriteCaret <> Value then
  begin
    FOverwriteCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.EnsureCaretInView;
{ Ensure cursor is visible by moving it to the edge of the text area if needed }
var
  DC, OldDC: TDisplayCoord;
  MinX, MaxX: Integer;
  P: TPoint;
begin
  if CaretInView then Exit;

  DC := DisplayXY;
  OldDC := DC;

  P := RowColumnToPixels(DC);
  MinX := FGutterWidth + TextMargin;
  MaxX := ClientWidth  - TextMargin - CharWidth;
  if not InRange(P.X, MinX, MaxX) then
  begin
    P.X := MinMax(P.X, MinX, MaxX);
    DC := PixelsToRowColumn(P.X, P.Y);
  end;

  if (DC <> OldDC) or
    not InRange(TopLine, DC.Row - (LinesInWindow - 1), DC.Row)
  then
    DisplayXY := DC;
end;

procedure TCustomSynEdit.EnsureCursorPosVisible;
begin
  EnsureCursorPosVisibleEx(False);
end;

procedure TCustomSynEdit.EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
  EvenIfVisible: Boolean = False);
{ Ensure cursor is visible by changing LeftChar and TopLine }
var
  DC: TDisplayCoord;
  WidthToX: Integer;
  TmpMiddle: Integer;
  SRow: String;
begin
  if not HandleAllocated then Exit;
  IncPaintLock;
  try
    DC := DisplayXY;
    SRow := Rows[DC.Row];

    // Make sure X is visible
    if (eoScrollPastEol in FOptions) and (DC.Column > SRow.Length) then
      WidthToX := TextWidth(SRow) + (DC.Column - SRow.Length) * FCharWidth
    else
      WidthToX := TextWidth(Copy(SRow, 1, DC.Column - 1));
    if WidthToX < (FLeftChar - 1) * FCharWidth then
      LeftChar := Max(WidthToX div fCharWidth, 1)
    else if WidthToX >= FTextAreaWidth + (LeftChar - 1) * FCharWidth then
      LeftChar := CeilofIntDiv(WidthToX - FTextAreaWidth, FCharWidth) + 2
    else
      LeftChar := LeftChar;

    // Make sure Y is visible
    if ForceToMiddle then
    begin
      if DC.Row < (TopLine - 1) then
      begin
        TmpMiddle := LinesInWindow div 2;
        if DC.Row - TmpMiddle < 0 then
          TopLine := 1
        else
          TopLine := DC.Row - TmpMiddle + 1;
      end
      else if DC.Row > (TopLine + (LinesInWindow - 2)) then
      begin
        TmpMiddle := LinesInWindow div 2;
        TopLine := DC.Row - (LinesInWindow - 1) + TmpMiddle;
      end
     { Forces to middle even if visible in viewport }
      else if EvenIfVisible then
      begin
        TmpMiddle := fLinesInWindow div 2;
        TopLine := DC.Row - TmpMiddle + 1;
      end;
    end
    else
      TopLine := MinMax(TopLine, DC.Row - (LinesInWindow - 1), DC.Row);
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

function TCustomSynEdit.TextWidth(const S: string): Integer;
begin
  Result := TextWidth(PChar(S), S.Length);
end;

function TCustomSynEdit.TextWidth(P: PChar; Len: Integer): Integer;
{ Ascii caracters are assumed to be fixed width.  Remaining text sequences
  are measured using TSynTextLayout }
var
  Layout: TSynTextLayout;
  P2, PStart, PEnd: PChar;
  CopyS: string;
begin
  if P^ = #0 then Exit(0);

  if scControlChars in FVisibleSpecialChars then
  begin
    SetString(CopyS, P, Len);
    SubstituteControlChars(CopyS);
    P := PChar(CopyS);
  end;

  PStart := P;
  PEnd:= P + Len;
  Result := 0;

  while P < PEnd do
  begin
    while P < PEnd do
    begin
      case P^ of
         #9: Inc(Result, fTabWidth * fCharWidth - Result mod (fTabWidth * fCharWidth));
         #32..#126, #$00A0: Inc(Result, FCharWidth);
       else
         break;
       end;
       Inc(P);
    end;

    if P >= PEnd then Break;

    // Just in case P is followed by combining characters
    if (P > PStart) and not (Word((P-1)^) in [9, 32]) then
    begin
      Dec(P);
      Dec(Result, FCharWidth);
    end;
    // Measure non-ascii text code points
    P2 := P;
    while P2 < PEnd do
    begin
      Inc(P2);
      if Word(P2^) in [9, 65..90, 97..122] then Break;
    end;
    Layout.Create(FTextFormat, P, P2-P, MaxInt, fTextHeight);
    Inc(Result, Round(Layout.TextMetrics.widthIncludingTrailingWhitespace));
    P := P2;
  end;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.
function TCustomSynEdit.TranslateKeyCode(Code: word; Shift: TShiftState;
  var Data: pointer): TSynEditorCommand;
var
  i: Integer;
begin
  i := KeyStrokes.FindKeycode2(fLastKey, fLastShiftState, Code, Shift);
  if i >= 0 then
    Result := KeyStrokes[i].Command
  else begin
    i := Keystrokes.FindKeycode(Code, Shift);
    if i >= 0 then
      Result := Keystrokes[i].Command
    else
      Result := ecNone;
  end;
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then
  begin
    fLastKey := Code;
    fLastShiftState := Shift;
  end
  else
  begin
    fLastKey := 0;
    fLastShiftState := [];
  end;
end;

procedure TCustomSynEdit.TripleClick;
Var
  BB, BE : TBufferCoord;
begin
  if not (eoNoSelection in fOptions) then
  begin
    BB := BufferCoord(1, CaretY);
    if CaretY < Lines.Count then
      BE := BufferCoord(1, CaretY + 1)
    else
      BE := BufferCoord(Length(Lines[CaretY-1]) + 1, CaretY);
    SetCaretAndSelection(BE, BB, BE);
  end;
  if Assigned(fOnTripleClick) then
    fOnTripleClick(Self);
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
var
  CommandInfo: TSynCommandInfo;
begin
  if csPanning in ControlState then
  begin
    // As in Word, Notepad stop panning and ignore the command
    Assert(Assigned(Mouse.PanningWindow));
    Mouse.PanningWindow.StopPanning;
    Exit;
  end;

  fUndoRedo.CommandProcessed := Command;
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then
  begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(False, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserFirst) then
    begin
      CommandInfo := SynCommandsInfo[Command];
      if (CommandInfo.CommandKind in [ckStandard, ckSingleCaret])
        or (FSelections.Count = 1)
      then
      begin
        if (CommandInfo.CommandKind = ckSingleCaret) and (FSelections.Count > 1) then
          FSelections.Clear(TSynSelections.TKeepSelection.ksKeepBase);
        ExecuteCommand(Command, AChar, Data);
      end
      else
        ExecuteMultiCaretCommand(Command, Achar, Data, CommandInfo);
    end;
    // notify hooked command handlers after the command was executed inside of
    // the class
    if Command <> ecNone then
      NotifyHookedCommandHandlers(True, Command, AChar, Data);
  end;
  DoOnCommandProcessed(Command, AChar, Data);
  fUndoRedo.CommandProcessed := ecNone;
end;

procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
  Data: pointer);
var
  CX: Integer;
  Len: Integer;
  Temp: string;
  Temp2: string;
  Helper: string;
  SpaceBuffer: string;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  BackCounter: Integer;
  moveBkm: Boolean;
  WP: TBufferCoord;
  Caret: TBufferCoord;
  CaretNew: TBufferCoord;
  CaretXNew: Integer;
  counter: Integer;
  vCaretRow: Integer;
  SaveLastPosX: Integer;

begin
  DoOnPaintTransient(ttBefore);
  IncPaintLock;
  try
    case Command of
      ecCancelSelections:
        begin
          if FSelections.Count = 1 then
            CaretXY := CaretXY // removes selection
          else
            FSelections.Clear(ksKeepBase);
        end;
      ecDeleteSelections:
        begin
          SelText := '';
        end;
      // horizontal caret movement or selection
      ecLeft, ecSelLeft, ecSelColumnLeft:
        if not FSelection.IsEmpty and (Command = ecLeft) then
          CaretXY := FSelection.Normalized.Start
        else
        begin
          Caret := FSelections.BaseSelection.Start;
          MoveCaretHorz(-1, Command = ecSelLeft);
          if Command = ecSelColumnLeft then
            FSelections.ColumnSelection(Caret, CaretXY, FLastPosX);
        end;
      ecRight, ecSelRight, ecSelColumnRight:
        if not FSelection.IsEmpty and (Command = ecRight) then
          CaretXY := FSelection.Normalized.Stop
        else
        begin
          Caret := FSelections.BaseSelection.Start;
          MoveCaretHorz(1, Command = ecSelRight);
          if Command = ecSelColumnRight then
            FSelections.ColumnSelection(Caret, CaretXY, FLastPosX);
        end;
      ecPageLeft, ecSelPageLeft:
        MoveCaretHorz(-(FTextAreaWidth div FCharWidth), Command = ecSelPageLeft);
      ecPageRight, ecSelPageRight:
        MoveCaretHorz(FTextAreaWidth div FCharWidth, Command = ecSelPageRight);
      ecLineStart, ecSelLineStart:
        begin
          DoHomeKey(Command = ecSelLineStart);
        end;
      ecLineEnd, ecSelLineEnd:
        DoEndKey(Command = ecSelLineEnd);
// vertical caret movement or selection
      ecUp, ecSelUp, ecSelColumnUp:
        begin
          Caret := FSelections.BaseSelection.Start;
          { on the first line we select first line too }
          if DisplayY = 1 then
          begin
            SaveLastPosX := FLastPosX;
            DoHomeKey(Command = ecSelUp);
            FLastPosX := SaveLastPosX;
          end
          else
            MoveCaretVert(-1, Command = ecSelUp);

          if Command = ecSelColumnUp then
            FSelections.ColumnSelection(Caret, CaretXY, FLastPosX);
        end;
      ecDown, ecSelDown, ecSelColumnDown:
        begin
          Caret := FSelections.BaseSelection.Start;
          { on the last line we will select last line too }
          if ((not Wordwrap and (CaretY = Lines.Count)) or
              (WordWrap and (DisplayY = fWordWrapPlugin.RowCount))) then
          begin
            SaveLastPosX := FLastPosX;
            DoEndKey(Command = ecSelDown);
            FLastPosX := SaveLastPosX;
          end
          else
            MoveCaretVert(1, Command = ecSelDown);

          if Command = ecSelColumnDown then
            FSelections.ColumnSelection(Caret, CaretXY, FLastPosX);
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown, ecSelColumnPageUp, ecSelColumnPageDown:
        begin
          Caret := FSelections.BaseSelection.Start;
          counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          if (Command in [ecPageUp, ecSelPageUp, ecSelColumnPageUp]) then
            counter := -counter;
          TopLine := TopLine + counter;
          { on the first line we will select first line too }
          if (Command in [ecPageUp, ecSelPageUp, ecSelColumnPageUp]) and (DisplayY = 1) then
          begin
            SaveLastPosX := FLastPosX;
            DoHomeKey(Command = ecSelPageUp);
            FLastPosX := SaveLastPosX;
          end
          else
          { on the last line we will select last line too }
          if (Command in [ecPageDown, ecSelPageDown, ecSelColumnPageDown]) and
             ((not Wordwrap and (CaretY = Lines.Count)) or
              (WordWrap and (DisplayY = fWordWrapPlugin.RowCount))) then
          begin
            SaveLastPosX := FLastPosX;
            DoEndKey(Command = ecSelPageDown);
            FLastPosX := SaveLastPosX;
          end
          else
            MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown]);

          if Command in [ecSelColumnPageUp, ecSelColumnPageDown] then
            FSelections.ColumnSelection(Caret, CaretXY, FLastPosX);
        end;
      ecPageTop, ecSelPageTop:
        begin
          MoveDisplayPosAndSelection(DisplayCoord(DisplayX, TopLine),
            Command = ecSelPageTop);
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          MoveDisplayPosAndSelection(
            DisplayCoord(DisplayX, TopLine + LinesInWindow -1),
            Command = ecSelPageBottom);
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          MoveDisplayPosAndSelection(DisplayCoord(1, 1),
            Command = ecSelEditorTop);
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew := BufferCoord(1, Max(1, Lines.Count));
          if (Lines.Count > 0) then
            CaretNew.Char := Lines[CaretNew.Line - 1].Length + 1;
          MoveCaretAndSelection(CaretNew, Command = ecSelEditorBottom);
        end;
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then
          MoveCaretAndSelection(TBufferCoord(Data^), Command = ecSelGotoXY);
      ecWordLeft, ecSelWordLeft:
        begin
          CaretNew := PrevWordPos;
          MoveCaretAndSelection(CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight:
        begin
          CaretNew := NextWordPos;
          MoveCaretAndSelection(CaretNew, Command = ecSelWordRight);
        end;
      ecSelWord:
        begin
          SetSelWord;
        end;
      ecSelectAll:
        begin
          SelectAll;
        end;
      ecDeleteLastChar:
        if not ReadOnly then begin
          if SelAvail then
            SetSelText('')
          else begin
            Temp := LineText;
            Len := Length(Temp);
            Caret := CaretXY;
            if CaretX > Len + 1 then
            begin
              if eoSmartTabDelete in fOptions then
              begin
                //It's at the end of the line, move it to the length
                if Len > 0 then
                  CaretX := Len + 1
                else begin
                  //move it as if there were normal spaces there
                  SpaceCount1 := CaretX - 1;
                  SpaceCount2 := 0;
                  // unindent
                  if SpaceCount1 > 0 then
                  begin
                    BackCounter := CaretY - 2;
                    while BackCounter >= 0 do
                    begin
                      SpaceCount2 := LeftSpaces(Lines[BackCounter], False);
                      if (SpaceCount2 > 0) and (SpaceCount2 < SpaceCount1) then
                        break;
                      Dec(BackCounter);
                    end;
                    if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                      SpaceCount2 := 0;
                  end;
                  if SpaceCount2 = SpaceCount1 then
                    SpaceCount2 := 0;
                  CaretX := CaretX - (SpaceCount1 - SpaceCount2);
                end;
              end
              else begin
                // only move caret one column
                CaretX := CaretX - 1;
              end;
            end // CaretX > Len + 1
            else if CaretX = 1 then
            begin
              // join this line with the last line if possible
              if CaretY > 1 then
              begin
                Lines.BeginUpdate;
                BeginUndoBlock;
                try
                  CaretXNew := Lines[CaretY - 2].Length + 1;
                  Lines[CaretY - 1] := Lines[CaretY - 2] + Temp;
                  Lines.Delete(CaretY - 2);
                  CaretXY := BufferCoord(CaretXNew, CaretY - 1);
                finally
                  EndUndoBlock;
                  Lines.EndUpdate;
                end;
              end;
            end
            else begin
              // delete text before the caret
              if ((Temp[CaretX - 1] <= #32) or (Temp[CaretX - 1] = #$00A0))
                 and (LeftSpaces(Temp, False) = CaretX - 1) then
              begin
                SpaceCount1 := LeftSpaces(Temp, True, FTabWidth);
                Assert(SpaceCount1 > 0);
                // only spaces - special treatment
                if eoSmartTabDelete in fOptions then
                begin
                  // unindent
                  SpaceCount2 := 0;
                  BackCounter := CaretY - 2;
                  while BackCounter >= 0 do
                  begin
                    Temp2 := Lines[BackCounter];
                    SpaceCount2 := LeftSpaces(Temp2, True, FTabWidth);
                    if (Temp2.Length > 0) and (SpaceCount2 < SpaceCount1) then
                      break;
                    Dec(BackCounter);
                  end;
                  if (BackCounter = -1) and (SpaceCount2 >= SpaceCount1) then
                    SpaceCount2 := 0;
                end
                else
                  SpaceCount2 := SpaceCount1 - (SpaceCount1 - 1) mod TabWidth - 1;
                Delete(Temp, 1, LeftSpaces(Temp, False));
                Temp2 := GetLeftSpacing(SpaceCount2, True);
                Temp := Temp2 + Temp;
                CaretXNew := Temp2.Length + 1;
                Lines[CaretY - 1] :=  Temp;
                CaretX := CaretXNew;
              end
              else begin
                // delete char accounting for surrogate pairs
                CaretXNew := CaretX - 1;
                if (CaretXNew > 1) and Temp[CaretXNew].IsLowSurrogate then
                  Dec(CaretXNew);
                Delete(Temp, CaretXNew, CaretX - CaretXNew);
                CaretNew := BufferCoord(CaretXNew, CaretY);
                vCaretRow := BufferToDisplayPos(CaretNew).Row;
                Lines[CaretY - 1] := Temp;
                SetCaretInRow(CaretNew, vCaretRow); // to deal with FCaretEOL
              end;
            end;
          end;
          EnsureCursorPosVisible;
        end;
      ecDeleteChar:
        if not ReadOnly then begin

          if SelAvail then
            SetSelText('')
          else begin
            // Call UpdateLastPosX. Even though the caret doesn't move, the
            // current caret position should "stick" whenever text is modified.
            UpdateLastPosX;
            Temp := LineText;
            Len := Length(Temp);
            if CaretX <= Len then
            begin
              // delete char
              vCaretRow := DisplayY;
              CaretXNew := ValidTextPos(BufferCoord(CaretX + 1, CaretY), True).Char;
              Delete(Temp, CaretX, CaretXNew - CaretX);
              Lines[CaretY - 1] := Temp;
              SetCaretInRow(CaretXY, vCaretRow); // to deal with FCaretEOL
            end
            else begin
              // join line with the line after
              if CaretY < Lines.Count then
              begin
                Lines.BeginUpdate;
                BeginUndoBlock;
                try
                  Helper := StringofChar(#32, CaretX - 1 - Len);
                  Lines[CaretY] := Temp + Helper + Lines[CaretY];
                  Lines.Delete(CaretY - 1);
                finally
                  EndUndoBlock;
                  Lines.EndUpdate;
                end;
              end;
            end;
          end;
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then
        begin
          Len := Length(LineText);
          if Command = ecDeleteWord then
          begin
            // in case of ident char, we delete word else char
            WP := CaretXY;
            // as first we skip all white chars behind cursor
            if (Len > WP.Char) and IsWhiteChar(LineText[WP.Char]) then
            begin
              cx := StrScanForCharInCategory(LineText, WP.Char, IsNonWhiteChar);
              // if not found, StrScanForCharInCategory() returns zero
              WP.Char := Max(WP.Char, cx);
            end;
            // in case of not ident char we move one char right
            if (Len > WP.Char) and not IsIdentChar(LineText[WP.Char]) then
              WP.Char := WP.Char + 1
            // in case of ident char, we move to word end
            else
              WP := WordEndEx(WP);
            // now we skip whitespaces behind
            if (Len > WP.Char) and IsWhiteChar(LineText[WP.Char]) then
            begin
              cx := StrScanForCharInCategory(LineText, WP.Char, IsNonWhiteChar);
              // if not found, StrScanForCharInCategory() returns 0
              WP.Char := Max(WP.Char, cx);
            end;
          end
          else begin
            WP.Char := Len + 1;
            WP.Line := CaretY;
          end;

          if (WP > CaretXY) and (WP.Line = CaretY) then
          begin
            Temp := Lines[CaretY - 1];
            Delete(Temp, CaretX, WP.Char - CaretX);
            Lines[CaretY - 1] := Temp;
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then begin
          if Command = ecDeleteLastWord then
          begin
            // we must find word end first
            WP := CaretXY;
            // in case scroll past EOL cursor can be behind the line end
            WP.Char := Min(Length(LineText), WP.Char);
            if (WP.Char > 1) and IsWordBreakChar(LineText[WP.Char - 1]) then
              WP.Char := StrRScanForCharInCategory(LineText, WP.Char - 1, IsIdentChar) + 1;
            // now we move to word start
            WP := WordStartEx(WP);
          end
          else begin
            WP.Char := 1;
            WP.Line := CaretY;
          end;
          if (WP < CaretXY) and (WP.Line = CaretY) then
          begin
            Temp := Lines[CaretY - 1];
            Delete(Temp, WP.Char, CaretX - WP.Char);
            Lines[CaretY - 1] := Temp;
            CaretXY := WP;
          end;
        end;
      ecDeleteLine:
        ExecCmdDeleteLine;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        if not ReadOnly then begin
          Lines.BeginUpdate;
          BeginUndoBlock;
          try
            if SelAvail then
              SetSelText('');
            Temp := LineText;
            Temp2 := Temp;
            Len := Length(Temp);
            if (Len > 0) and (CaretX <= Len) then
            begin
              if CaretX > 1 then
              begin
                Temp := Copy(LineText, 1, CaretX - 1);
                if eoAutoIndent in Options then
                  SpaceCount1 := LeftSpaces(Temp, True, FTabWidth)
                else
                  SpaceCount1 := 0;
                Delete(Temp2, 1, CaretX - 1);
                SpaceBuffer := GetLeftSpacing(SpaceCount1, True);
                Lines.Insert(CaretY - 1, Temp);
                Lines[CaretY] := SpaceBuffer + Temp2;
                if Command = ecLineBreak then
                  CaretXY := BufferCoord(SpaceBuffer.Length + 1, CaretY + 1);
              end
              else begin
                Lines.Insert(CaretY - 1, '');
                if Command = ecLineBreak then
                  CaretY := CaretY + 1;
              end;
            end // (Len > 0) and (CaretX < Len)
            else begin
              // either empty or at the end of line: insert new line below
              if fLines.Count = 0 then
                fLines.Add('');
              SpaceCount2 := 0;
              // Autoindent only in case we are not at the start of the line
              if (eoAutoIndent in Options) and (CaretX > 1)  then
              begin
                BackCounter := CaretY;
                repeat
                  Dec(BackCounter);
                  Temp := Lines[BackCounter];
                  SpaceCount2 := LeftSpaces(Temp, True, FTabWidth);
                until (BackCounter = 0) or (Temp <> '');
              end;
              SpaceBuffer := GetLeftSpacing(SpaceCount2, True);
              Lines.Insert(CaretY, SpaceBuffer);
              if Command = ecLineBreak then
                CaretXY := BufferCoord(SpaceBuffer.Length + 1, CaretY + 1);
            end;
            UpdateLastPosX;
          finally
            EndUndoBlock;
            Lines.EndUpdate;
          end;
        end;
      ecTab:
        if not ReadOnly then DoTabKey;
      ecShiftTab:
        if not ReadOnly then DoShiftTabKey;
      ecMatchBracket,
      ecSelMatchBracket:
        begin
          CaretNew := GetMatchingBracket;
          if CaretNew.IsValid then
            MoveCaretAndSelection(CaretNew, Command = ecSelMatchBracket);
        end;
      ecChar:
        InsertCharAtCursor(AChar);
      ecUpperCase,
      ecLowerCase,
      ecToggleCase,
      ecTitleCase:
        ExecCmdCaseChange(Command);
      ecUndo:
        begin
          if not ReadOnly then Undo;
        end;
      ecRedo:
        begin
          if not ReadOnly then Redo;
        end;
      ecGotoMarker0..ecGotoMarker9:
        begin
          if BookMarkOptions.EnableKeys then
            GotoBookMark(Command - ecGotoMarker0);
        end;
      ecSetMarker0..ecSetMarker9:
        begin
          if BookMarkOptions.EnableKeys then
          begin
            CX := Command - ecSetMarker0;
            if Assigned(Data) then
              Caret := TBufferCoord(Data^)
            else
              Caret := CaretXY;
            if assigned(fBookMarks[CX]) then
            begin
              moveBkm := (fBookMarks[CX].Line <> Caret.Line);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, Caret.Char, Caret.Line);
            end
            else
              SetBookMark(CX, Caret.Char, Caret.Line);
          end; // if BookMarkOptions.EnableKeys
        end;
      ecCut:
        begin
          if not ReadOnly then
            CutToClipboard;
        end;
      ecCopy:
        begin
          CopyToClipboard;
        end;
      ecPaste:
        begin
          if Length(FPasteArray) > 1 then
            SelText := FPasteArray[FSelections.ActiveSelIndex]
          else
            SelText := FPasteArray[0];
        end;
      ecScrollUp, ecScrollDown:
        begin
          vCaretRow := DisplayY;
          if (vCaretRow < TopLine) or (vCaretRow >= TopLine + LinesInWindow) then
            // If the caret is not in view then, like the Delphi editor, move
            // it in view and do nothing else
            EnsureCursorPosVisible
          else begin
            if Command = ecScrollUp then
            begin
              TopLine := TopLine - 1;
              if vCaretRow > TopLine + LinesInWindow - 1 then
                MoveCaretVert((TopLine + LinesInWindow - 1) - vCaretRow, False);
            end
            else begin
              TopLine := TopLine + 1;
              if vCaretRow < TopLine then
                MoveCaretVert(TopLine - vCaretRow, False);
            end;
            EnsureCursorPosVisible;
          end;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          EnsureCaretInView;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          EnsureCaretInView;
        end;
      ecInsertMode:
        begin
          InsertMode := True;
        end;
      ecOverwriteMode:
        begin
          InsertMode := False;
        end;
      ecToggleMode:
        begin
          InsertMode := not InsertMode;
        end;
      ecBlockIndent:
        if not ReadOnly then DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then DoBlockUnindent;
      ecContextHelp:
        begin
          if Assigned (fOnContextHelp) then
            fOnContextHelp (self,WordAtCursor);
        end;
      ecImeStr:
        InsertCharAtCursor(PWideChar(Data));
      ecCopyLineUp, ecCopyLineDown, ecMoveLineUp, ecMoveLineDown:
        ExecCmdCopyOrMoveLine(Command);
      ecFoldAll: begin CollapseAll; end;
      ecUnfoldAll: begin UncollapseAll; end;
      ecFoldNearest: begin CollapseNearest; end;
      ecUnfoldNearest: begin UncollapseNearest; end;
      ecFoldLevel1: begin CollapseLevel(1); end;
      ecFoldLevel2: begin CollapseLevel(2); end;
      ecFoldLevel3: begin CollapseLevel(3); end;
      ecUnfoldLevel1: begin UncollapseLevel(1); end;
      ecUnfoldLevel2: begin UncollapseLevel(2); end;
      ecUnfoldLevel3: begin UncollapseLevel(3); end;
      ecFoldRegions: begin CollapseFoldType(FoldRegionType) end;
      ecUnfoldRegions: begin UnCollapseFoldType(FoldRegionType) end;
      ecSelMatchingText:
        begin
          SelectMatchingText;
        end;
      ecCaretsAtLineEnds:
        begin
          CaretsAtLineEnds;
        end;
      ecZoomIn: Zoom(1);
      ecZoomOut: Zoom(-1);
      ecZoomReset: ZoomReset;
    end;
  finally
    DecPaintLock;
    DoOnPaintTransient(ttBefore);
  end;
end;

procedure TCustomSynEdit.ExecuteMultiCaretCommand(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer; CommandInfo: TSynCommandInfo);
var
  OldActiveSelIndex: Integer;
  I: Integer;
  OldTopLine, OldLeftChar: Integer;
begin
  DoOnPaintTransient(ttBefore);
  IncPaintLock;
  try
    if CommandInfo.StoreMultiCaret then
    begin
      Lines.BeginUpdate;
      BeginUndoBlock;
    end;

    OldActiveSelIndex := Selections.ActiveSelIndex;
    OldLeftChar := LeftChar;
    OldTopLine := TopLine;

    for I := 0 to FSelections.Count -1 do
    begin
      // Make the current selection active
      Selections.ActiveSelIndex := I;

      if not FSelection.IsValid then Continue;

      ExecuteCommand(Command, AChar, Data);
      Selections.ActiveSelection := FSelection;
    end;

    // Restore Active Selection
    Selections.ActiveSelIndex := OldActiveSelIndex;

    // Merge Selections
    FSelections.Merge;

    TopLine := OldTopLine;
    LeftChar := OldLeftChar;

    EnsureCursorPosVisible;

    if CommandInfo.StoreMultiCaret then
    begin
      EndUndoBlock;
      Lines.EndUpdate;
    end;
  finally
    DecPaintLock;
    DoOnPaintTransient(ttAfter);
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: WideChar; Data: pointer);
begin
  if Command < ecUserFirst then
  begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end
  else begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TCustomSynEdit.ClearAll;
begin
  Lines.Clear; // triggers ListCleared
  fUndoRedo.Clear;
  Modified := False;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.DeleteSelections;
begin
  if not FSelections.IsEmpty then
    CommandProcessor(ecDeleteSelections, ' ', nil);
end;

procedure TCustomSynEdit.ClearTrackChanges;
begin
  fUndoRedo.ClearTrackChanges(Lines);
  InvalidateGutter;
end;

function TCustomSynEdit.NextWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY, LineLen: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    LineLen := Length(Line);
    if CX > LineLen then
    begin
      // invalid char
      // find first non-whitespace char in the next line
      if CY < Lines.Count then
      begin
        Line := Lines[CY];
        LineLen := Length(Line);
        Inc(CY);
        CX := 1;
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end;
    end
    else
    begin
      if CX = 0 then
        CX := 1;
      // valid char
      if IsIdentChar(Line[CX]) then begin
        while (CX <= LineLen) and IsIdentChar(Line[CX]) do
          Inc(CX);
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end else if IsWhiteChar(Line[CX]) then begin
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end else begin
        // breakchar and not whitechar
        while (CX <= LineLen) and (IsWordBreakChar(Line[CX]) and not IsWhiteChar(Line[CX])) do
          Inc(CX);
        while (CX <= LineLen) and IsWhiteChar(Line[CX]) do
          Inc(CX);
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.WordStartEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: string;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    if CX > 1 then
    begin
      // only find previous char, if not already on start of line
      // if previous char isn't a word-break-char search for the last IdentChar
      if not IsWordBreakChar(Line[CX - 1]) then
        CX := StrRScanForCharInCategory(Line, CX - 1, IsWordBreakChar) + 1;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.WordEndEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: string;
begin
  CX := XY.Char;
  CY := XY.Line;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    CX := StrScanForCharInCategory(Line, CX, IsWordBreakChar);
    // if no word-break-char is found just position at the end of the line
    if CX = 0 then
      CX := Length(Line) + 1;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

function TCustomSynEdit.PrevWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY: Integer;
  Line: UnicodeString;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    if CX <= 1 then
    begin
      // skip whitespace at the end of the previous line
      if CY > 1 then
      begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
        if CX > 1 then
          Exit(PrevWordPosEx(BufferCoord(CX, CY)));  // recursive call
      end;
    end
    else
    begin
      // CX > 1 and <= LineLenght + 1
      if IsIdentChar(Line[CX-1]) then begin
        while (CX > 1) and IsIdentChar(Line[CX-1]) do
          Dec(CX);
      end else if IsWhiteChar(Line[CX-1]) then begin
        while (CX > 1) and IsWhiteChar(Line[CX-1]) do
          Dec(CX);
        if (CX > 1) then
        begin
          if IsIdentChar(Line[CX-1]) then
            while (CX > 1) and IsIdentChar(Line[CX-1]) do
              Dec(CX)
          else
            // breakchar and not whitechar
            while (CX > 1) and (IsWordBreakChar(Line[CX-1]) and not IsWhiteChar(Line[CX-1])) do
              Dec(CX);
        end;
      end else begin
        // breakchar and not whitechar
        while (CX > 1) and (IsWordBreakChar(Line[CX-1]) and not IsWhiteChar(Line[CX-1])) do
          Dec(CX);
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

procedure TCustomSynEdit.SetSelectedColor(const Value: TSynSelectedColor);
begin
  FSelectedColor.Assign(Value);
end;

procedure TCustomSynEdit.SetAdditionalIdentChars(const Value: TSysCharSet);
begin
  FAdditionalIdentChars := Value;
end;

procedure TCustomSynEdit.SetAdditionalWordBreakChars(const Value: TSysCharSet);
begin
  FAdditionalWordBreakChars := Value;
end;

procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoRedo.BeginBlock(Self);
end;

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TCustomSynEdit.EndUndoBlock;
begin
  fUndoRedo.EndBlock(Self);
end;

procedure TCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TCustomSynEdit.AddKey(Command: TSynEditorCommand;
  Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
var
  Key: TSynEditKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

{ Called by FMarkList if change }
procedure TCustomSynEdit.MarkListNotify(Sender: TObject; const Mark: TSynEditMark;
  Action: TCollectionNotification);
begin
  if ([csDestroying, csLoading] * ComponentState = []) and
    (Action in [cnAdded, cnRemoved]) and Assigned(Mark)
  then
    InvalidateGutterLines(Mark.fLine, Mark.fLine);
end;

procedure TCustomSynEdit.MarkSaved;
begin
  fUndoRedo.BufferSaved(Lines);
  InvalidateGutter;
end;

function TCustomSynEdit.GetSelStart: integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(BlockBegin)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.SetAlwaysShowCaret(const Value: Boolean);
begin
  if FAlwaysShowCaret <> Value then
  begin
    FAlwaysShowCaret := Value;
    if not(csDestroying in ComponentState) and not Focused then
    begin
      if Value then
        UpdateCarets
      else
        FCarets.HideCarets;
    end;
  end;
end;

procedure TCustomSynEdit.SetSelStart(const Value: Integer);
begin
  { if we don't call HandleNeeded, TextAreaWidth may be 0 and LeftChar will
  be set to CaretX }
  HandleNeeded;
  CaretXY := CharIndexToRowCol(Value);
end;

function TCustomSynEdit.GetSelEnd: Integer;
begin
  if GetSelAvail then
    Result := RowColToCharIndex(Blockend)
  else
    Result := RowColToCharIndex(CaretXY);
end;

procedure TCustomSynEdit.SetSelEnd(const Value: Integer);
begin
  HandleNeeded;
  BlockEnd := CharIndexToRowCol( Value );
end;

procedure TCustomSynEdit.SetSelWord;
begin
  SetWordBlock(CaretXY);
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: Integer);
begin
  fExtraLineSpacing := Value;
  SynFontChanged(self);
end;

function TCustomSynEdit.GetBookMark(BookMark: Integer; var X, Y: Integer):
  Boolean;
var
  i: Integer;
begin
  Result := False;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then
      begin
        X := Marks[i].Char;
        Y := Marks[i].Line;
        Result := True;
        Exit;
      end;
end;

function TCustomSynEdit.IsBookmark(BookMark: Integer): Boolean;
var
  x, y: Integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

function TCustomSynEdit.IsChained: Boolean;
begin
  Result := Assigned(fChainedEditor);
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoRedo.Clear;
  ClearTrackChanges;
end;

procedure TCustomSynEdit.CMHintShow(var Message: TCMHintShow);
var
  CanShow: Boolean;
  HintStr: string;
begin
  if Assigned(fOnShowHint) then
  begin
    HintStr := Message.HintInfo.HintStr;
    CanShow := HintStr <> '';
    fOnShowHint(HintStr, CanShow, Message.HintInfo^);
    if CanShow then
    begin
      Message.Result := 0;
      Message.HintInfo.HintStr := HintStr;
    end
    else
      Message.Result := 1;
  end
  else
    inherited;;
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
var
  nW: Integer;
begin
  if ComponentState * [csLoading, csDestroying]  = [] then
  begin
    nW := fGutter.RealGutterWidth;
    fTextOffset := nw + fTextMargin - (LeftChar - 1) * fCharWidth;
    if nW = fGutterWidth then
      InvalidateGutter
    else
      SetGutterWidth(nW);
  end;

  // Used in Zoom
  FOrigGutterFontSize := Font.Size;
end;

procedure TCustomSynEdit.LockUndo;
begin
  fUndoRedo.Lock;
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  fUndoRedo.Unlock;
end;

procedure TCustomSynEdit.WMSetCursor(var Msg: TWMSetCursor);
begin
  if (Msg.HitTest = HTCLIENT) and (Msg.CursorWnd = Handle) and
    not(csDesigning in ComponentState) then
  begin
    UpdateMouseCursor;
  end
  else
    inherited;
end;

procedure TCustomSynEdit.SetTabWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, 256);
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    TSynEditStringList(Lines).TabWidth := Value;
    FTextFormat.Create(Font, fTabWidth, 0, fExtraLineSpacing);
    Invalidate; // to redraw text containing tab chars
    if WordWrap then
    begin
      fWordWrapPlugin.Reset;
      InvalidateGutter;
    end;
  end;
end;

procedure TCustomSynEdit.SelectedColorChanged(Sender: TObject);
begin
  InvalidateSelection;
end;

// find / replace

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions): Integer;
begin
  Result := SearchReplace(ASearch, AReplace, AOptions,
    TBufferCoord.Invalid, TBufferCoord.Invalid);
end;

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions; const Start, Stop: TBufferCoord): Integer;
var
  ptStart, ptEnd: TBufferCoord; // start and end of the search range
  nEOLCount, i: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  bEndUndoBlock: boolean;
  sReplace: string;

  function ProcessTextRange(const ptStart: TBufferCoord;
    var ptEnd: TBufferCoord): Integer;
  var
    lnStart, lnEnd: Integer;  // the part of the line that is searched
    nSearchLen, nReplaceLen, n, nFound: integer;
    nInLine: integer;
    nAction: TSynReplaceAction;
    iResultOffset: Integer;
    CurrentLine: Integer;
    Line: string;
  begin
    Result := 0;
    if bBackward then
      CurrentLine := ptEnd.Line
    else
      CurrentLine := ptStart.Line;

    while (CurrentLine >= ptStart.Line) and (CurrentLine <= ptEnd.Line) do
    begin
      Line := Lines[CurrentLine - 1];
      if CurrentLine = ptStart.Line then
        lnStart := ptStart.Char
      else
        lnStart := 1;

      if CurrentLine = ptEnd.Line then
        lnEnd := ptEnd.Char
      else
        lnEnd := Length(Line) + 1;

      if lnEnd <= lnStart then
      begin
        CurrentLine := CurrentLine + IfThen(bBackward, -1, 1);
        Continue;
      end;
      nInLine := fSearchEngine.FindAll(Line, lnStart, lnEnd);
      iResultOffset := 0;
      if bBackward then
        n := Pred(fSearchEngine.ResultCount)
      else
        n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nFound := fSearchEngine.Results[n] + iResultOffset;
        nSearchLen := fSearchEngine.Lengths[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);

        // We have a match
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.
        // It is not necessary to see changes if replacing without confirmation.
        // It signicatntly slow down replace
        FSelections.Clear;
        if bBackward then
          SetCaretAndSelection(
            BufferCoord(nFound, CurrentLine),
            BufferCoord(nFound + nSearchLen, CurrentLine),
            BufferCoord(nFound, CurrentLine),
            not bReplaceAll or bPrompt, not bReplaceAll or bPrompt)
        else
          SetCaretAndSelection(
            BufferCoord(nFound + nSearchLen, CurrentLine),
            BufferCoord(nFound , CurrentLine),
            BufferCoord(nFound + nSearchLen, CurrentLine),
            not bReplaceAll or bPrompt, not bReplaceAll or bPrompt);
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then Exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then
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
        else begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            if not bReplaceAll or bPrompt then
            begin
              bReplaceAll := True;
              IncPaintLock;
            end;
            bPrompt := False;
            if not bEndUndoBlock then
              BeginUndoBlock;
            bEndUndoBlock:= true;
          end;
          // Allow advanced substitution in the search engine
          SelText := fSearchEngine.Replace(SelText, sReplace);
          nReplaceLen := CaretX - nFound;
          // fix the caret position and the remaining results
          if not bBackward then begin
            InternalCaretX := nFound + nReplaceLen;
            if nSearchLen <> nReplaceLen then
            begin
              Inc(iResultOffset, nReplaceLen - nSearchLen);
              if CaretY = ptEnd.Line then
              begin
                Inc(ptEnd.Char, nReplaceLen - nSearchLen);
                //BlockEnd := ptEnd;  // not sure what was the purpose of this
              end;
            end;
            //Fix new line ends
            if nEOLCount > 0 then
            begin
              Inc(ptEnd.Line, nEOLCount);
              if not bBackward then
                // New lines have been entered, so the remaining matches will not
                // be vaild and will be matched on the following line
                Break;
            end;
          end;
        end;
        if not bReplaceAll then
          Exit;
      end;
      // search next / previous line
      CurrentLine := CurrentLine + IfThen(bBackward, -1, 1);
    end;
  end;

var
  Sel: TSynSelection;
  Index, J, LineAdjustment: Integer;
  SelStorage: TSynSelStorage;
begin
  if not Assigned(fSearchEngine) then
    raise ESynEditError.Create('No search engine has been assigned');

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then exit;
  // get the text range to search in, ignore the "Search in selection only"
  // option if nothing is selected
  bBackward := (ssoBackwards in AOptions);
  bPrompt := (ssoPrompt in AOptions);
  bReplace := (ssoReplace in AOptions);
  bReplaceAll := (ssoReplaceAll in AOptions);
  bFromCursor := not (ssoEntireScope in AOptions);
  if FSelections.IsEmpty then Exclude(AOptions, ssoSelectedOnly);
  //  translate \n and \t to real chars for regular expressions
  sReplace := fSearchEngine.PreprocessReplaceExpression(AReplace);

  //count line ends
  nEOLCount := 0;
  I := 1;
  repeat
    I := Pos(WideCrLf, sReplace, I);
    if I <> 0 then
    begin
      I := I + Length(WideCrLf);
      Inc(nEolCount);
    end;
  until I = 0;
  // initialize the search engine
  fSearchEngine.Options := AOptions;
  fSearchEngine.Pattern := ASearch;
  fSearchEngine.IsWordBreakFunction := IsWordBreakChar;
  // search while the current search position is inside of the search range
  DoOnPaintTransient(ttBefore);
  if bReplaceAll and not bPrompt then
  begin
    IncPaintLock;
    BeginUndoBlock;
    bEndUndoBlock := True;
  end
  else
    bEndUndoBlock := False;
  try
    if Start.IsValid and Stop.IsValid then
    begin
      if Start > Stop then
      begin
        ptStart := Stop;
        ptEnd :=  Start;
      end
      else
      begin
        ptStart := Start;
        ptEnd :=  Stop;
      end;
      Result := ProcessTextRange(ptStart, ptEnd);
    end
    else if not (ssoSelectedOnly in AOptions) then
    begin
      ptStart.Char := 1;
      ptStart.Line := 1;
      ptEnd.Line := Lines.Count;
      ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
      if bFromCursor then
        if bBackward then
          ptEnd := CaretXY
        else
          ptStart := CaretXY;
      Result := ProcessTextRange(ptStart, ptEnd);
    end
    else
    begin
      FSelections.Store(SelStorage);
      for I := 0 to Length(SelStorage.Selections) - 1 do
      begin
        if bBackward then
          Index := Length(SelStorage.Selections) - 1 - I
        else
          Index := I;
        Sel := SelStorage.Selections[Index].Normalized;

        ptStart := Sel.Start;
        ptEnd := Sel.Stop;
        // Ignore the cursor position when searching in the selection
        // but take account of a valid Start
        if Start.IsValid then
        begin
          // if you pass a valid start search from there
          if bBackward then
          begin
            if Start <= Sel.Start then Continue;
            ptEnd := TBufferCoord.Min(Start, ptEnd);
          end
          else
          begin
            if Start >= ptEnd then Continue;
            ptStart := TBufferCoord.Max(Start, ptStart);
          end;
        end;

        if ptStart >= ptEnd then Continue;

        Inc(Result, ProcessTextRange(ptStart, ptEnd));

        if (Result > 0) and not bReplaceAll then
          Break;

        LineAdjustment := ptEnd.Line - Sel.Stop.Line;
        for J := Index + 1 to Length(SelStorage.Selections) - 1 do
        begin
          // Adjust lines
          Inc(SelStorage.Selections[J].Start.Line, LineAdjustment);
          Inc(SelStorage.Selections[J].Stop.Line, LineAdjustment);
        end;

        if SelStorage.Selections[Index] = Sel then
          SelStorage.Selections[Index].Stop := ptEnd
        else
          SelStorage.Selections[Index].Start := ptEnd
      end;
    end;
  finally
    if bReplaceAll and not bPrompt then DecPaintLock;
    if bEndUndoBlock then EndUndoBlock;
    DoOnPaintTransient(ttAfter);
  end;
end;

function TCustomSynEdit.IsPointInSelection(const Value: TBufferCoord): boolean;
var
  Index: Integer;
begin
  Result := FSelections.FindSelection(Value, Index) and
    not FSelections[Index].IsEmpty;
end;

procedure TCustomSynEdit.SetFocus;
begin
  if (fFocusList.Count > 0) then
  begin
    if TWinControl (fFocusList.Last).CanFocus then
      TWinControl (fFocusList.Last).SetFocus;
    exit;
  end;
  inherited;
end;

procedure TCustomSynEdit.UpdateMouseCursor;
var
  ptCursor: TPoint;
  ptLineCol: TBufferCoord;
  iNewCursor: TCursor;
  ptRowCol: TDisplayCoord;
  Rect: TRect;
  Band: TSynGutterBand;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  ptRowCol := PixelsToRowColumn(ptCursor.X, ptCursor.Y);
  ptLineCol := DisplayToBufferPos(ptRowCol);
  if (ptCursor.X < fGutterWidth) then
  begin
    iNewCursor := Gutter.Cursor;
    Band := FGutter.BandAtX(ptCursor.X);
    if Assigned(Band) and Assigned(Band) then
      Band.DoMouseCursor(Self, ptCursor.X, ptCursor.Y, ptRowCol.Row,
        ptLineCol.Line, iNewCursor);
  end
  else
  begin
    if (eoDragDropEditing in fOptions) and (not MouseCapture) and
      (FSelections.Count = 1) and IsPointInSelection(ptLineCol)
    then
      iNewCursor := crArrow
    else if UseCodeFolding and CodeFolding.ShowHintMark and
      fAllFoldRanges.CollapsedFoldStartAtLine(ptLineCol.Line) then
    begin
      Rect := GetCollapseMarkRect(ptRowCol.Row, ptLineCol.Line);
      if PtInRect(Rect, ptCursor) and (ptCursor.X > FGutterWidth + fTextMargin) then
        iNewCursor := crHandPoint;
    end else
      iNewCursor := Cursor;

    DoOnMouserCursor(ptLineCol, iNewCursor);
    fKbdHandler.ExecuteMouseCursor(Self, ptLineCol, iNewCursor);
  end;
  SetCursor(Screen.Cursors[iNewCursor]);
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
const
  ScrollOptions = [eoDisableScrollArrows, eoHideShowScrollbars,
    eoScrollPastEof, eoScrollPastEol];
var
  bSetDrag: Boolean;
  bUpdateScroll: Boolean;
begin
  if (Value <> fOptions) then
  begin
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);
    bUpdateScroll := (Options * ScrollOptions) <> (Value * ScrollOptions);
    fOptions := Value;

    FUndoRedo.GroupUndo := eoGroupUndo in Options;

    if HandleAllocated then
    begin
      CalcTextAreaWidth;  // in case eoWrapWithRightEdge changed

      // (un)register HWND as drop target
      if bSetDrag and not (csDesigning in ComponentState) then
        DragAcceptFiles(Handle, (eoDropFiles in fOptions));

      if bUpdateScroll then
      begin
        if not (eoScrollPastEol in Options) then
          LeftChar := LeftChar;
        if not (eoScrollPastEof in Options) then
          TopLine := TopLine;

        UpdateScrollBars;
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetVisibleSpecialChars(Value: TSynVisibleSpecialChars);
begin
  if Value <> FVisibleSpecialChars then
  begin
    FVisibleSpecialChars := Value;
    TSynEditStringList(fLines).ResetMaxWidth;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated and (fCharWidth <> 0) then
  begin
    fLinesInWindow := ClientHeight div fTextHeight;
    CalcTextAreaWidth;
    if WordWrap then
    begin
      fWordWrapPlugin.DisplayChanged;
      Invalidate;
    end;
    if bFont then
    begin
      if Gutter.ShowLineNumbers and not Gutter.UseFontStyle then
        GutterChanged(Self)
      else
        UpdateScrollbars;
      InitializeCaret;
      Exclude(fStateFlags, sfCaretChanged);
      Invalidate;
    end
    else
      UpdateScrollbars;
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
var
  Dst: TDisplayCoord;
  SRow: string;
  RowLen: Integer;
  ChangeY: Boolean;
begin
  Dst := DisplayXY;
  SRow := Rows[Dst.Row];
  RowLen := SRow.Length;
  // only moving or selecting one char can change the line
  ChangeY := WordWrap or not (eoScrollPastEol in fOptions);
  if ChangeY and (DX = -1) and (Dst.Column = 1) and (Dst.Row > 1) then
  begin
    // end of previous line
    Dec(Dst.Row);
    Dst.Column := Length(Rows[Dst.Row]) + 1;
  end
  else if ChangeY and (DX = 1) and (Dst.Column > RowLen) and (Dst.Row < DisplayRowCount) then
  begin
    // start of next row
    Inc(Dst.Row);
    Dst.Column := 1;
  end
  else begin
    Dst.Column := Max(1, Dst.Column + DX);
    // don't go past last char when ScrollPastEol option not set
    if (DX > 0) and ChangeY then
      Dst.Column := Min(Dst.Column, RowLen + 1);
    Dst.Column := ValidTextPos(SRow, Dst.Column, DX > 0);
  end;
  // set display pos and selection
  MoveDisplayPosAndSelection(Dst, SelectionCommand);
end;

procedure TCustomSynEdit.MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
var
  Org, Dst: TDisplayCoord;
  SaveLastPosX: Integer;
  SDestRow: string;
begin
  Org := DisplayXY;
  Dst := Org;

  Inc(Dst.Row, DY);
  if DY >= 0 then
  begin
    if RowToLine(Dst.Row) > Lines.Count then
      Dst.Row := Max(1, DisplayRowCount);
  end
  else
    Dst.Row := Max(Dst.Row, 1);

  if (Org.Row <> Dst.Row) then
  begin
    if eoKeepCaretX in Options then
    begin
      SDestRow := Rows[Dst.Row];
      Dst.Column := PixelsToColumn(PChar(SDestRow), SDestRow.Length, FLastPosX);
    end;
  end;
  Dst.Column := ValidTextPos(Rows[Dst.Row], Dst.Column, False);

  // set caret and block begin/end
  SaveLastPosX := FLastPosX;
  MoveDisplayPosAndSelection(Dst, SelectionCommand);

  // Restore FLastPosX after moving caret, since UpdateLastPosX, called by
  // SetCaretXYEx, changes them. This is the one case where we don't want that.
  FLastPosX := SaveLastPosX;
end;

procedure TCustomSynEdit.MoveCaretAndSelection(const NewPos: TBufferCoord;
  SelectionCmd: Boolean);
{ Moves the cursor to ptAfter (new cursor)
  If SelectionCmd is True sets selection from the old cursor to the new cursor
  If SelectionCmd is False it clears the selection }
var
  DC: TDisplayCoord;
begin
  DC := BufferToDisplayPos(NewPos);
  MoveDisplayPosAndSelection(DC, SelectionCmd);
end;

procedure TCustomSynEdit.MoveDisplayPosAndSelection(const NewPos: TDisplayCoord;
  SelectionCmd: Boolean);
{ Similar to MoveCaretAndSelection, but with display coordinates.
  It correctly sets fCaretEOL }
var
  BC: TBufferCoord;
begin
  if (eoGroupUndo in FOptions) and fUndoRedo.CanUndo then
    fUndoRedo.AddGroupBreak;

  BC := DisplayToBufferPos(NewPos);

  if ((CaretY = BC.Line) and (not FSelection.IsEmpty or SelectionCmd)) or
    ((ActiveLineColor <> clNone) and WordWrap and (DisplayY <> NewPos.Row))
  then
    InvalidateLine(BC.Line);

  IncPaintLock;
  try
    if SelectionCmd then
      SetBlockEnd(BC)
    else
      SetBlockBegin(BC); // Also sets BlockEnd = NewPos
    DisplayXY := NewPos; // Correctly sets CaretAtEOL when WordWrap is True
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetCaretAndSelection(const Sel: TSynSelection;
  EnsureVisible, ForceToMiddle: Boolean);
begin
  SetCaretAndSelection(Sel.Caret, Sel.Start, Sel.Stop,
    EnsureVisible, ForceToMiddle);
  CaretAtEOL := Sel.CaretAtEOL;
  FLastPosX := Sel.LastPosX;
end;

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TBufferCoord; EnsureVisible: Boolean; ForceToMiddle: Boolean);
{ Sets the caret and the selection in one step
  The caret may be different than BlockBegin/End }
var
  ValidBB, ValidBE: TBufferCoord;
begin
  if (ActiveLineColor <> clNone) and (CaretY = ptCaret.Line) and
  (FSelection.IsEmpty xor (ptBefore = ptAfter))
  then
    InvalidateLine(ptCaret.Line);

  IncPaintLock;
  try
    InvalidateSelection;
    SetCaretXYEx(False, ptCaret);
    if EnsureVisible then
      EnsureCursorPosVisibleEx(ForceToMiddle);
    if eoNoSelection in fOptions then
    begin
      FSelection.Start := ptCaret;
      FSelection.Stop := ptCaret;
    end
    else
    begin
      ValidBB := ValidBC(ptBefore);
      ValidBE := ValidBC(ptAfter);
      if (FSelection.Start <> ValidBB) or (FSelection.Stop <> ValidBE) then
        Include(fStatusChanges, scSelection);
      FSelection.Start := ValidBB;
      FSelection.Stop := ValidBE;
      InvalidateSelection;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetCaretInRow(Value: TBufferCoord; Row: Integer);
{ Set the caret taking care of fCaretEOL }
var
  DC: TDisplayCoord;
begin
  if WordWrap then
  begin
    DC := BufferToDisplayPos(Value);
    if DC = DisplayCoord(1, Row + 1) then
    begin
      DC := DisplayCoord(fWordWrapPlugin.RowLength[Row] + 1, Row);
      DisplayXY := DC;
    end
    else
      CaretXY := Value;
  end
  else
    CaretXY := Value;
end;

procedure TCustomSynEdit.HighlightBrackets;
var
  BracketPos,
  MatchingBracketPos: TBufferCoord;
  Indicator: TSynIndicator;
begin
  if HandleAllocated and (eoBracketsHighlight in FOptions) and Assigned(fHighlighter) then
  begin
    Indicators.Clear(BracketsHighlight.MatchingBracketsIndicatorID);
    Indicators.Clear(BracketsHighlight.UnbalancedBracketIndicatorID);

    BracketPos := CaretXY;
    MatchingBracketPos := GetMatchingBracketEnhanced(BracketPos, fHighlighter.Brackets, False);

    if MatchingBracketPos.Char >= 0 then
    begin
      // We have a bracket at BracketPos
      if MatchingBracketPos.Char = 0 then
      begin
        // The bracket at BracketPos is unbalanced
        Indicator := TSynIndicator.Create(fBracketsHighlight.UnbalancedBracketIndicatorID,
          BracketPos.Char, BracketPos.Char + 1);
        Indicators.Add(BracketPos.Line, Indicator);
      end
      else
      begin
        // Matching pair of brackets
        Indicator := TSynIndicator.Create(fBracketsHighlight.MatchingBracketsIndicatorID,
          BracketPos.Char, BracketPos.Char + 1);
        Indicators.Add(BracketPos.Line, Indicator);
        Indicator := TSynIndicator.Create(fBracketsHighlight.MatchingBracketsIndicatorID,
          MatchingBracketPos.Char, MatchingBracketPos.Char + 1);
        Indicators.Add(MatchingBracketPos.Line, Indicator);
      end;
    end;
  end;
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  if Sender is TSynCustomHighlighter then
  begin
    IncPaintLock;
    try
      ScanRanges;
    finally
      DecPaintLock;
    end;
  end
  else
    Invalidate;
  SizeOrFontChanged(True);
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.SurfaceCaretFromHiddenFolds;
var
  Index: Integer;
  BC: TBufferCoord;
  Range: TSynFoldRange;
begin
  if not fUseCodeFolding then Exit;

  BC := CaretXY;
  while AllFoldRanges.FoldHidesLine(BC.Line, Index) do
  begin
    Range := AllFoldRanges[Index];
    BC := BufferCoord(Length(Lines[Range.FromLine - 1]) + 1, Range.FromLine);
  end;
  if BC <> CaretXY then
    CaretXY := BC;
end;

procedure TCustomSynEdit.ExecCmdCaseChange(const Cmd: TSynEditorCommand);
  function ToggleCase(const aStr: string): string;
  var
    i: Integer;
    sLower: string;
  begin
    Result := aStr.ToUpper;
    sLower := aStr.ToLower;
    for i := 1 to Length(aStr) do
    begin
      if Result[i] = aStr[i] then
        Result[i] := sLower[i];
    end;
  end;

  Function TitleCase(S:string) : string;
  Var
    i : Integer;
  Begin
    S[1] := S[1].ToUpper;
    For i := 1 to Length(S)-1 Do
      If IsWordBreakChar(S[i]) then
        S[i+1] := S[i+1].ToUpper
      else
        S[i+1] := S[i+1].ToLower;
    Result := S;
  End;

var
  S: string;
  oldCaret, oldBlockBegin, oldBlockEnd: TBufferCoord;
begin
  Assert((Cmd >= ecUpperCase) and (Cmd <= ecTitleCase));
  if ReadOnly then Exit;

  oldBlockBegin := BlockBegin;
  oldBlockEnd := BlockEnd;
  oldCaret := CaretXY;
  try
    if not SelAvail then
      SetSelWord;

    S := SelText;
    if S <> '' then
    begin
      case Cmd of
        ecUpperCase:
          S := S.ToUpper;
        ecLowerCase:
          S := S.ToLower;
        ecToggleCase:
          S := ToggleCase(S);
        ecTitleCase:
          S := TitleCase(S);
      end;
      SelText := S;
    end;
  finally
    SetCaretAndSelection(oldCaret, oldBlockBegin, oldBlockEnd);
  end;
end;

procedure TCustomSynEdit.DoTabKey;
var
  StartOfBlock: TBufferCoord;
  i, MinLen, iLine: integer;
  PrevLine, Spaces: string;
  p: PWideChar;
  NewCaretX: integer;
  nPhysX, nDistanceToTab, nSpacesToNextTabStop : Integer;
  vIgnoreSmartTabs, TrimTrailingActive: Boolean;
begin
  // Provide Visual Studio like block indenting
  if (eoTabIndent in Options) and not FSelection.IsEmpty then
  begin
    DoBlockIndent;
    Exit;
  end;
  i := 0;
  iLine := 0;
  MinLen := 0;
  vIgnoreSmartTabs := False;
  if eoSmartTabs in fOptions then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then
    begin
      Dec(iLine);
      repeat
        //todo: rethink it
        MinLen := DisplayToBufferPos(DisplayCoord(
          BufferToDisplayPos(CaretXY).Column, LineToRow(iLine + 1))).Char;
        PrevLine := Lines[iLine];
        if (Length(PrevLine) >= MinLen) then begin
          p := @PrevLine[MinLen];
          // scan over non-whitespaces
          repeat
            if (p^ = #9) or (p^ = #32) or (p^ = #$00A0) then break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if (p^ <> #9) and (p^ <> #32) and (p^ <> #$00A0) then break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end
    else
      vIgnoreSmartTabs := True;
  end;
  BeginUndoBlock;
  try
    if SelAvail then
      SetSelText('');
    StartOfBlock := CaretXY;

    if i = 0 then
    begin
      if (eoTabsToSpaces in fOptions) then
      begin
        i := TabWidth - (StartOfBlock.Char - 1) mod TabWidth;
        if i = 0 then
          i := TabWidth;
      end
      else
        i := TabWidth;
    end;

    if eoTabsToSpaces in fOptions then
    begin
      Spaces := StringOfChar(#32, i);
      NewCaretX := StartOfBlock.Char + i;
    end
    else begin
      if (eoSmartTabs in fOptions) and not vIgnoreSmartTabs and (iLine > -1) then
      begin
        Spaces := Copy(fLines[CaretXY.Line - 1], 1, CaretXY.Char - 1);
        while Pos(#9, Spaces) > 0 do
          Delete(Spaces, Pos(#9, Spaces), 1);
        Spaces := Trim(Spaces);

        //smart tabs are only in the front of the line *NOT IN THE MIDDLE*
        if Spaces = '' then
        begin
          i := BufferToDisplayPos( BufferCoord(MinLen+i, iLine+1) ).Column;

          nPhysX := DisplayX;
          nDistanceToTab := i - nPhysX;
          nSpacesToNextTabStop := TabWidth - ((nPhysX - 1) mod TabWidth);
          if nSpacesToNextTabStop <= nDistanceToTab then begin
            Spaces := #9;
            Dec(nDistanceToTab, nSpacesToNextTabStop);
          end;
          while nDistanceToTab >= TabWidth do begin
            Spaces := Spaces + #9;
            Dec(nDistanceToTab, TabWidth);
          end;
          if nDistanceToTab > 0 then
            Spaces := Spaces + StringofChar(#32, nDistanceToTab);
        end else
          Spaces := #9;
      end
      else begin
        Spaces := #9;
      end;
      NewCaretX := StartOfBlock.Char + Length(Spaces);
    end;
    // Do not Trim
    TrimTrailingActive := eoTrimTrailingSpaces in Options;
    if TrimTrailingActive then Exclude(fOptions, eoTrimTrailingSpaces);
    SetSelText(Spaces);
    if TrimTrailingActive then Include(fOptions, eoTrimTrailingSpaces);
  finally
    EndUndoBlock;
  end;
  ForceCaretX(NewCaretX);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.PaintText(S: string; P: TPoint; ClipR: TRect;
  FontStyle: TFontStyles; FontColor: TColor; BkgColor: TColor = clNone);
{ Support routine that can be used in plugins, handlers of AfterPaint etc.
  P is relative to ClipRect }
var
  RT: ID2D1DCRenderTarget;
  Layout: TSynTextLayout;
begin
  Layout.Create(FTextFormat, PChar(S), S.Length, MaxInt, fTextHeight);
  Layout.SetFontStyle(FontStyle, 1, S.Length);
  RT := TSynDWrite.RenderTarget;
  RT.SetTransform(TD2DMatrix3X2F.Identity);
  RT.BindDC(Canvas.Handle, ClipR);
  RT.BeginDraw;
  if BkgColor <> clNone then
    RT.Clear(D2D1ColorF(BkgColor));
  Layout.Draw(RT, P.X, P.Y, FontColor);
  if RT.EndDraw <> S_OK then TSynDWrite.ResetRenderTarget;
end;

procedure TCustomSynEdit.DoShiftTabKey;
// shift-tab key handling
var
  MaxLen, iLine: Integer;
  PrevLine: string;
  p: PWideChar;
  SpaceCount1, SpaceCount2: Integer;
  TempS, TempS2: string;
begin
  if (eoSmartTabs in Options) and not SelAvail then
  begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then
    begin
      TempS := Lines[CaretY - 1];
      SpaceCount1 := LeftSpaces(TempS, True, FTabWidth);
      if SpaceCount1 = 0 then Exit;

      Dec(iLine);
      MaxLen := SpaceCount1;
      SpaceCount2 := 0;
      repeat
        PrevLine := ExpandTabs(Lines[iLine], fTabWidth);
        if (PrevLine.Length > 0) and (Length(PrevLine) >= MaxLen) then
        begin
          p := @PrevLine[MaxLen];
          // scan over whitespaces
          repeat
            if (p^ <> #32) and (p^ <> #$00A0) then break;
            Inc(SpaceCount2);
            Dec(p);
          until SpaceCount2 = SpaceCount1;
          // scan over non-whitespaces
          if SpaceCount2 < SpaceCount1 then
            repeat
              if (p^ = #32) or (p^ = #$00A0) then break;
              Inc(SpaceCount2);
              Dec(p);
            until SpaceCount2 = SpaceCount1;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
      if SpaceCount2 = 0 then // UnIndent at least
        SpaceCount2 := ((SpaceCount1 -1)  mod TabWidth) + 1;
      TempS2 := GetLeftSpacing(SpaceCount1 - SpaceCount2, True);
      SpaceCount1 := LeftSpaces(TempS, False);
      Delete(TempS, 1, SpaceCount1);
      TempS := TempS2 + TempS;
      Lines[CaretY - 1] :=  TempS;
      CaretX := CaretX + TempS2.Length - SpaceCount1;
    end;
  end
  else
    DoBlockUnIndent;
end;

procedure TCustomSynEdit.DoHomeKey(Selection: Boolean);
var
  FirstNonBlank: Integer;
  S: string;
  NewPos: TDisplayCoord;
  MaxX: Integer;
begin
  // home key enhancement
  NewPos := DisplayXY;
  if eoEnhanceHomeKey in fOptions then
  begin
    S := Rows[NewPos.Row];
    FirstNonBlank := 1;
    MaxX := Length(S);
    while (FirstNonBlank <= MaxX) and IsWhiteChar(S[FirstNonBlank]) do
      Inc(FirstNonBlank);
    if NewPos.Column = FirstNonBlank then
      NewPos.Column := 1
    else
      NewPos.Column := FirstNonBlank;
  end
  else
    NewPos.Column := 1;

  MoveDisplayPosAndSelection(NewPos, Selection);
end;

procedure TCustomSynEdit.DoEndKey(Selection: Boolean);
var
  S: string;
  LastNonBlank: Integer;
  NewPos: TDisplayCoord;
begin
  NewPos := DisplayXY;
  S := Rows[NewPos.Row];
  if eoEnhanceEndKey in fOptions then
  begin
    LastNonBlank := Length(S);
    while (LastNonBlank > 0) and IsWhiteChar(S[LastNonBlank]) do
      Dec(LastNonBlank);

    if NewPos.Column = LastNonBlank + 1 then
      NewPos.Column := Length(S) + 1
    else
      NewPos.Column := LastNonBlank + 1;
  end
  else
    NewPos.Column := Length(S) + 1;

  MoveDisplayPosAndSelection(NewPos, Selection);
end;

procedure TCustomSynEdit.CreateWnd;
Var
  DropTarget : TSynDropTarget;
begin
  inherited;
  //  This is to avoid getting the text of the control while recreating
  WindowText := StrNew('SynEdit');  // dummy caption

  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    DragAcceptFiles(Handle, True);

  if not (csDesigning in ComponentState) then begin
    DropTarget := TSynDropTarget.Create;
    with DropTarget do begin
      OnDragEnter := OleDragEnter;
      OnDragOver := OleDragOver;
      OnDragLeave := OleDragLeave;
      OnDrop := OleDrop;
    end;
    RegisterDragDrop (Handle, DropTarget);
  end;

  UpdateScrollBars;
end;

procedure TCustomSynEdit.InvalidateRange(const BB, BE: TBufferCoord);
var
  DB, DE: TDisplayCoord;
  P1, P2: TPoint;
  R : TRect;
begin
  if BB.Line <> BE.Line then
    InvalidateLines(BB.Line, BE.Line)
  else if BB.Char <> BE.Char then
  begin
    DB := BufferToDisplayPos(BB);
    DE := BufferToDisplayPos(BE);
    if DB.Row <> DE.Row then
      InvalidateLine(BB.Line)
    else
    begin
      // part of a row
      if not InRange(DB.Row, TopLine, TopLine + LinesInWindow) or
        (DB.Column = DE.Column)
      then
        Exit;
      P1 := RowColumnToPixels(DB);
      P2 := RowColumnToPixels(DE);
      R := Rect(P1.X, fTextHeight * (DB.Row - TopLine), P2.X,
        fTextHeight * (DB.Row - TopLine + 1));
      R.NormalizeRect;
      InvalidateRect(R, False);
    end;
  end;
end;

procedure TCustomSynEdit.InvalidateRect(const aRect: TRect; aErase: Boolean);
begin
  Winapi.Windows.InvalidateRect(Handle, @aRect, aErase);
end;

procedure TCustomSynEdit.DoBlockIndent;
var
  OrgCaretPos: TBufferCoord;
  BB, BE: TBufferCoord;
  Line: string;
  EndLine, I: Integer;
  Spaces: string;
begin
  OrgCaretPos := CaretXY;

  // keep current selection detail
  BB := BlockBegin;
  BE := BlockEnd;

  // build text to insert
  if (BE.Char = 1) and (BE.Line > BB.Line) then
    EndLine := BE.Line - 1
  else
    EndLine := BE.Line;
  if (eoTabsToSpaces in Options) then
    Spaces := StringofChar(#32, FTabWidth)
  else
    Spaces := #9;

  Lines.BeginUpdate;
  BeginUndoBlock;
  try
    for I := BB.Line to EndLine do
    begin
      Line := Lines[I - 1];
      if (Line <> '') or (I = EndLine) then
        Lines[I - 1] := Spaces + Line;
    end;

    if (OrgCaretPos.Char > 1) or (BB.Line = BE.Line) then
      Inc(OrgCaretPos.Char, Spaces.Length);
    if (BB.Char > 1) or (BB.Line = BE.Line) then
      Inc(BB.Char, Spaces.Length);
    if (BE.Char > 1) or (BB.Line = BE.Line) then
      Inc(BE.Char, Spaces.Length);

    SetCaretAndSelection(OrgCaretPos, BB, BE);
  finally
    EndUndoBlock;
    Lines.EndUpdate;
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;

  function GetDelLen(Line: PChar): Integer;
  var
    Run: PChar;
  begin
    Result := 0;
    Run := Line;
    //Take care of tab character
    if Run[0] = #9 then Exit(1);
    //Deal with compound tabwidths  Sometimes they have TabChars after a few
    //spaces, yet we need to delete the whole tab width even though the char
    //count might not be FTabWidth because of the TabChar
    while ((Run[0] = #32) or (Run[0] = #$00A0)) and (Result < FTabWidth) do
    begin
      Inc(Result);
      Inc(Run);
    end;
    if (Run[0] = #9) and (Result < FTabWidth) then
      Inc(Result);
  end;

var
  OrgCaretPos, BB, BE: TBufferCoord;
  Line: string;
  PLine: PChar;
  EndLine, I, TmpDelLen, DelPos: Integer;

begin
  BB := BlockBegin;
  BE := BlockEnd;
  OrgCaretPos := CaretXY;

  // convert selection to complete lines
  if BE.Char = 1 then
    EndLine := BE.Line - 1
  else
    EndLine := BE.Line;

  Lines.BeginUpdate;
  BeginUndoBlock;
  try
    for I := BB.Line to EndLine do
    begin
      Line := Lines[I - 1];
      //Instead of doing a StringofChar, we need to get *exactly* what was
      //being deleted incase there is a TabChar
      PLine := PChar(Line);
      TmpDelLen := GetDelLen(PChar(PLine));
      if TmpDelLen > 0 then
      begin
        DelPos := 1;
        Delete(Line, DelPos, TmpDelLen);
        Lines[I - 1] := Line;
        if I = BB.Line then
          BB.Char := Max(BB.Char - TmpDelLen, 1);
        if I = BE.Line then
          BE.Char := Max(BE.Char - TmpDelLen, 1);
        if I = OrgCaretPos.Line then
          OrgCaretPos.Char := Max(OrgCaretPos.Char - TmpDelLen, 1);
      end;
    end;
    SetCaretAndSelection(OrgCaretPos, BB, BE);
  finally
    EndUndoBlock;
    Lines.EndUpdate;
  end;
end;

function TCustomSynEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TEditCut then
        CommandProcessor(ecCut, ' ', nil)
      else if Action is TEditCopy then
        CommandProcessor(ecCopy, ' ', nil)
      else if Action is TEditPaste then
        CommandProcessor(ecPaste, ' ', nil)
      else if Action is TEditDelete then
        DeleteSelections
      else if Action is TEditUndo then
        CommandProcessor(ecUndo, ' ', nil)
      else if Action is TEditSelectAll then
        CommandProcessor(ecSelectAll, ' ', nil);
    end
  end
  else if Action is TSearchAction then
  begin
    Result := Focused;
    if Action is TSearchFindFirst then
      DoSearchFindFirstExecute(TSearchFindFirst(Action))
    else if Action is TSearchFind then
      DoSearchFindExecute(TSearchFind(Action))
    else if Action is TSearchReplace then
      DoSearchReplaceExecute(TSearchReplace(Action));
  end
  else if Action is TSearchFindNext then
  begin
    Result := Focused;
    DoSearchFindNextExecute(TSearchFindNext(Action))
  end
  else
    Result := inherited ExecuteAction(Action);
end;

function TCustomSynEdit.UpdateAction(Action: TBasicAction): Boolean;
var
  IsEmpty: Boolean;
begin
  IsEmpty := (Lines.Count = 0) or (Lines.Count = 1) and (Lines[0] = '');
  if Action is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TEditCut then
        TEditAction(Action).Enabled := not (IsEmpty or ReadOnly)
      else if Action is TEditCopy then
        TEditAction(Action).Enabled := not IsEmpty
      else if Action is TEditPaste then
        TEditAction(Action).Enabled := CanPaste
      else if Action is TEditDelete then
        TEditAction(Action).Enabled := not (FSelections.IsEmpty or ReadOnly)
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
      else if Action is TEditSelectAll then
        TEditAction(Action).Enabled := True;
    end;
  end else if (Action is TSearchAction) or (Action is TSearchFindNext) then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TSearchFindFirst then
        TSearchAction(Action).Enabled := not IsEmpty and Assigned(fSearchEngine)
      else if Action is TSearchFind then
        TSearchAction(Action).Enabled := not IsEmpty and Assigned(fSearchEngine)
      else if Action is TSearchReplace then
        TSearchAction(Action).Enabled := not IsEmpty and Assigned(fSearchEngine)
      else if Action is TSearchFindNext then
        TSearchAction(Action).Enabled := not IsEmpty and Assigned(fSearchEngine)
          and (TSearchFindNext(Action).SearchFind <> nil)
          and (TSearchFindNext(Action).SearchFind.Dialog.FindText <> '');
    end;
  end
  else
    Result := inherited UpdateAction(Action);
end;

procedure TCustomSynEdit.SetModified(Value: Boolean);
begin
  if Value <> FUndoRedo.Modified then begin
    fUndoRedo.Modified := Value;
    if (eoGroupUndo in Options) and (not Value) and fUndoRedo.CanUndo then
      fUndoRedo.AddGroupBreak;
    StatusChanged([scModified]);
  end;
end;

function TCustomSynEdit.DoOnSpecialLineColors(Line: Integer; var Foreground,
  Background: TColor): Boolean;
begin
  Result := False;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TCustomSynEdit.InvalidateLine(Line: Integer);
var
  rcInval: TRect;
begin
  if (not HandleAllocated) or (Line < 1) or (Line > Lines.Count) or (not Visible) then
    Exit;

  if UseCodeFolding or WordWrap then
  begin
    InvalidateLines(Line, Line);
    Exit;
  end;

  if (Line >= TopLine) and (Line <= TopLine + LinesInWindow) then
  begin
    // invalidate text area of this line
    rcInval := Rect(fGutterWidth, fTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
    InvalidateRect(rcInval, False);
  end;
end;

function TCustomSynEdit.GetReadOnly: Boolean;
begin
  Result := fReadOnly;
end;

function TCustomSynEdit.GetRow(RowIndex: Integer): string;
var
  BC: TBufferCoord;
  Len: Integer;
begin
  BC := DisplayToBufferPos(DisplayCoord(1, RowIndex));
  if InRange(BC.Line, 1, Lines.Count) then
  begin
    if WordWrap then
    begin
      Len := fWordWrapPlugin.RowLength[RowIndex];
      Result := Copy(Lines[BC.Line - 1], BC.Char, Len);
    end
    else
      Result := Lines[BC.Line - 1];
  end
  else
    Result := '';
end;

function TCustomSynEdit.GetRowLength(RowIndex: Integer): Integer;
var
  BC: TBufferCoord;
begin
  BC := DisplayToBufferPos(DisplayCoord(1, RowIndex));
  if InRange(BC.Line, 1, Lines.Count) then
  begin
    if WordWrap then
      Result := fWordWrapPlugin.RowLength[RowIndex]
    else
      Result := Lines[BC.Line - 1].Length;
  end
  else
    Result := 0;
end;

procedure TCustomSynEdit.SetReadOnly(Value: Boolean);
begin
  if fReadOnly <> Value then
  begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TCustomSynEdit.FindMatchingBracket;
begin
  InternalCaretXY := GetMatchingBracket;
end;

function TCustomSynEdit.GetMatchingBracket: TBufferCoord;
var
  Pos: TBufferCoord;
begin
  Pos := CaretXY;
  if fHighlighter <> nil then
    Result := GetMatchingBracketEnhanced(Pos, fHighlighter.Brackets)
  else
    Result := GetMatchingBracketEnhanced(Pos);
end;

function TCustomSynEdit.GetMatchingBracketEx(const APoint: TBufferCoord;
  Brackets: string): TBufferCoord;
var
  Line: string;
  Index, PosX, PosY, Len: Integer;
  Test, BracketInc, BracketDec: WideChar;
  NumBrackets: Integer;
  SDummy: string;
  Attr: TSynHighlighterAttributes;
  P: TBufferCoord;
  IsCommentOrString: Boolean;
begin
  Result.Char := 0;
  Result.Line := 0;
  // get char at caret
  PosX := APoint.Char;
  PosY := APoint.Line;
  Line := Lines[APoint.Line - 1];
  if Length(Line) >= PosX then
  begin
    Test := Line[PosX];
    // is it one of the recognized brackets?
    Index := Brackets.IndexOf(Test);  // zero based
    if Index >= 0 then
    begin
      // this is the bracket, get the matching one and the direction
      BracketInc := Brackets.Chars[Index];
      BracketDec := Brackets.Chars[Index xor 1]; // 0 -> 1, 1 -> 0, ...
      // search for the matching bracket (that is until NumBrackets = 0)
      NumBrackets := 1;
      if Odd(Index) then
      begin
        repeat
          // search until start of line
          while PosX > 1 do
          begin
            Dec(PosX);
            Test := Line[PosX];
            P.Char := PosX;
            P.Line := PosY;
            if (Test = BracketInc) or (Test = BracketDec) then
            begin
              IsCommentOrString := GetHighlighterAttriAtRowCol(P, SDummy, Attr) and
                ((Attr = Highlighter.StringAttribute) or (Attr = Highlighter.CommentAttribute));
              if (Test = BracketInc) and (not IsCommentOrString) then
                Inc(NumBrackets)
              else if (Test = BracketDec) and (not IsCommentOrString) then
              begin
                Dec(NumBrackets);
                if NumBrackets = 0 then
                begin
                  // matching bracket found, set caret and bail out
                  Result := P;
                  Exit;
                end;
              end;
            end;
          end;
          // get previous line if possible
          if PosY = 1 then break;
          Dec(PosY);
          Line := Lines[PosY - 1];
          PosX := Length(Line) + 1;
        until False;
      end
      else begin
        repeat
          // search until end of line
          Len := Length(Line);
          while PosX < Len do
          begin
            Inc(PosX);
            Test := Line[PosX];
            P.Char := PosX;
            P.Line := PosY;
            if (Test = BracketInc) or (Test = BracketDec) then
            begin
              IsCommentOrString := GetHighlighterAttriAtRowCol(P, SDummy, Attr) and
                ((Attr = Highlighter.StringAttribute) or (Attr = Highlighter.CommentAttribute));
              if (Test = BracketInc) and (not IsCommentOrString) then
                Inc(NumBrackets)
              else if (Test = BracketDec)and (not IsCommentOrString) then
              begin
                Dec(NumBrackets);
                if NumBrackets = 0 then
                begin
                  // matching bracket found, set caret and bail out
                  Result := P;
                  Exit;
                end;
              end;
            end;
          end;
          // get next line if possible
          if PosY = Lines.Count then
            Break;
          Inc(PosY);
          Line := Lines[PosY - 1];
          PosX := 0;
        until False;
      end;
    end;
  end;
end;

function TCustomSynEdit.GetMatchingBracketEnhanced(var BracketPos: TBufferCoord;
  Brackets: string = '()[]{}<>'; AdjustMatchingPos: Boolean = True): TBufferCoord;
{
   If there is a bracket on the left of BracketPos.Char it is used instead.
   On Exit BracketPos points at the position of the bracket used for matching.
   Returns BufferCoord(-1, -1) if BracketPos is not a bracket.
   Returns BufferCoord(0, 0) if BracketPos contains an unbalanced bracket.
   If AdjustMatchingPos and there is a match, the matching pair of positions
   will be both either inside or outside the brackets.
}

  function PosHasBracket(Pos: TBufferCoord; const Line: string): Boolean;
  var
    Token: string;
    Attri: TSynHighlighterAttributes;
  begin
    Result :=
     InRange(Pos.Char, 1, Line.Length) and
     (Brackets.IndexOf(Line[Pos.Char]) >= 0) and
     (not Assigned(fHighlighter) or
     (GetHighlighterAttriAtRowCol(Pos, Token, Attri) and
     (Attri <> fHighlighter.CommentAttribute) and
     (Attri <> fHighlighter.StringAttribute)));
  end;

var
  Line: string;
  HasBracket, IsPreviousChar, IsOpenChar, IsOutside: Boolean;
begin
  Result := BufferCoord(-1, -1);
  if BracketPos.Line > Lines.Count then Exit;
  Line := Lines[BracketPos.Line - 1];

  // First Look at the previous character like Site
  IsPreviousChar := BracketPos.Char > 1;
  if IsPreviousChar  then
    Dec(BracketPos.Char);
  HasBracket := PosHasBracket(BracketPos, Line);
  //if it is not a bracket then look at the next character;
  if not HasBracket and IsPreviousChar then
  begin
    Inc(BracketPos.Char);
    IsPreviousChar := False;
    HasBracket := PosHasBracket(BracketPos, Line);
  end;

  if HasBracket then
  begin
    Result := GetMatchingBracketEx(BracketPos, Brackets);
    if (Result.Char > 0) and AdjustMatchingPos then
    begin
      IsOpenChar := not Odd(Brackets.IndexOf(Line[BracketPos.Char]));
      IsOutside := IsOpenChar xor IsPreviousChar;
      if IsOutside xor not IsOpenChar then
        Inc(Result.Char);
    end;
  end;
end;

function TCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Token: string; var Attri: TSynHighlighterAttributes): Boolean;
begin
  Attri := nil;
  if Assigned(fHighlighter) then
    Attri := fHighlighter.GetHighlighterAttriAtRowCol(Lines, XY.Line - 1, XY.Char);
  Result := Attri <> nil;
end;

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(const XY: TBufferCoord;
  var Token: string; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;
begin
  if Assigned(fHighlighter) then
    Result := fHighlighter.GetHighlighterAttriAtRowColEx(Lines, XY.Line - 1,
      XY.Char, Token, TokenType, Start, Attri)
  else
    Result := False;
end;

function TCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): Integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    Dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: Integer;
begin
  if Assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

function TCustomSynEdit.GetIsScrolling: Boolean;
begin
  Result := FSynEditScrollBars.IsScrolling;
end;

procedure TCustomSynEdit.RegisterCommandHandler(
  const AHandlerProc: THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then
  begin
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TObjectList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
      AHandlerProc, AHandlerData));
end;

procedure TCustomSynEdit.UnregisterCommandHandler(AHandlerProc:
  THookedCommandEvent);
var
  i: Integer;
begin
  if not Assigned(AHandlerProc) then
    exit;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then
    fHookedCommandHandlers.Delete(i);
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer);
var
  Handled: Boolean;
  i: Integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := False;
  for i := 0 to GetHookedCommandHandlersCount - 1 do
  begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[i]);
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.fEvent(Self, AfterProcessing, Handled, Command, AChar, Data,
      Entry.fData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TCustomSynEdit.DoOnClearBookmark(var Mark: TSynEditMark);
begin
  if Assigned(fOnClearMark) then
    fOnClearMark(Self, Mark);
end;

procedure TCustomSynEdit.DoOnPaintTransient(TransientType: TTransientType);
var
  DoTransient: Boolean;
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  if (TransientType=ttBefore) then
    Inc(FPaintTransientLock)
  else
    Dec(FPaintTransientLock);

  DoTransient := (FPaintTransientLock = 0) or (fPaintLock = 0) and
    (FPaintTransientPlugins or Assigned(fOnPaintTransient));

  if DoTransient then
  begin
    FCarets.HideCarets;
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    try
      // plugins
      if fPlugins <> nil then
        for i := 0 to fPlugins.Count - 1 do
        begin
          PlugIn := TSynEditPlugin(fPlugins[i]);
          if phPaintTransient in Plugin.Handlers then
            PlugIn.PaintTransient(Canvas, TransientType);
        end;
      // event
      if Assigned(fOnPaintTransient) then
      begin
        fOnPaintTransient(Self, Canvas, TransientType);
      end;
    finally
      UpdateCarets;
    end;
  end;
end;

procedure TCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    fOnPaint(Self, Canvas);
  end;
end;

procedure TCustomSynEdit.DoOnPlaceMark(var Mark: TSynEditMark);
begin
  if Assigned(fOnPlaceMark) then
    fOnPlaceMark(Self, Mark);
end;

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: string;
  Line, Column: Integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(fOnReplaceText) then
    fOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Changes * [scCaretX, scCaretY] <> [] then
    HighlightBrackets;

  if (Changes * [scCaretX, scCaretY, scSelection] <> [])
    and Assigned(FUIAutomationProvider) and UiaClientsAreListening
  then
    TThread.ForceQueue(nil, procedure
    begin
      UiaRaiseAutomationEvent(IRawElementProviderSimple(FUIAutomationProvider),
        UIA_Text_TextSelectionChangedEventId);
    end);

  if Assigned(fOnStatusChange) then
    fOnStatusChange(Self, fStatusChanges);
  fStatusChanges := [];
end;

procedure TCustomSynEdit.ModifiedChanged(Sender: TObject);
begin
  StatusChanged([scModified]);
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TBufferCoord): string;
var
  Line: string;
  Len, Start, Stop: Integer;
begin
  Result := '';
  if (XY.Line >= 1) and (XY.Line <= Lines.Count) then
  begin
    Line := Lines[XY.Line - 1];
    Len := Length(Line);
    if (Len > 0) and InRange(XY.Char, 1, Len + 1) then
    begin
      Start := XY.Char;
      while (Start > 1) and IsIdentChar(Line[Start - 1]) do
        Dec(Start);

      Stop := XY.Char;
      while (Stop <= Len) and IsIdentChar(Line[Stop]) do
        Inc(Stop);

      Result := Copy(Line, Start, Stop - Start);
    end;
  end;
end;

procedure TCustomSynEdit.GetWordBoundaries(XY: TBufferCoord; var BB,
  BE: TBufferCoord);
var
  TempString: string;

  procedure CharScan;
  var
    cRun: Integer;
  begin
    { search BlockEnd }
    BE.Char := Length(TempString);
    for cRun := XY.Char to Length(TempString) do
      if not IsIdentChar(TempString[cRun]) then
      begin
        BE.Char := cRun;
        Break;
      end;
    { search BlockBegin }
    BB.Char := 1;
    for cRun := XY.Char - 1 downto 1 do
      if not IsIdentChar(TempString[cRun]) then
      begin
        BB.Char := cRun + 1;
        Break;
      end;
  end;

begin
  XY.Char := Max(XY.Char, 1);
  XY.Line := MinMax(XY.Line, 1, Lines.Count);
  TempString := Lines[XY.Line - 1] + #0; //needed for CaretX = LineLength + 1
  if XY.Char > Length(TempString) then
  begin
    BB.Char := TempString.Length;
    BE.Char := BB.Char;
  end
  else
    CharScan;

  BB.Line := XY.Line;
  BE.Line := XY.Line;
end;

function TCustomSynEdit.BufferToDisplayPos(const BC: TBufferCoord): TDisplayCoord;
// BufferToDisplayPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
begin
  Result := TDisplayCoord(BC);
  if WordWrap then
    Result := fWordWrapPlugin.BufferToDisplayPos(TBufferCoord(Result));
  if UseCodeFolding then
    Result.Row := fAllFoldRanges.FoldLineToRow(Result.Row)
end;

function TCustomSynEdit.DisplayToBufferPos(const DC: TDisplayCoord): TBufferCoord;
// DisplayToBufferPos takes a position on screen and transfrom it
// into a position of text
begin
  if WordWrap then
    Result := fWordWrapPlugin.DisplayToBufferPos(DC)
  else
    Result := TBufferCoord(DC);
  if UseCodeFolding then
    Result.Line := fAllFoldRanges.FoldRowToLine(DC.Row);
end;

procedure TCustomSynEdit.DoLinesBeforeDeleted(FirstLine, Count: Integer);
var
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phLinesBeforeDeleted in Plugin.Handlers then
        PlugIn.LinesBeforeDeleted(FirstLine, Count);
    end;
end;

procedure TCustomSynEdit.DoLinesChanged;
var
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phLinesChanged in Plugin.Handlers then
        PlugIn.LinesChanged;
    end;
end;

procedure TCustomSynEdit.DoLinesDeleted(FirstLine, Count: Integer);
var
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  fGutter.AutoSizeDigitCount;

  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine + Count then
      Marks[i].Line := Marks[i].Line - Count
    else if Marks[i].Line > FirstLine then
      Marks[i].Line := FirstLine;

  // SynIndicators
  FIndicators.LinesDeleted(FirstLine, Count);

  // Selections
  if not fUndoRedo.InsideUndoRedo then
    FSelections.LinesDeleted(FirstLine, Count);

  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phLinesDeleted in Plugin.Handlers then
        Plugin.LinesDeleted(FirstLine, Count);
    end;
end;

procedure TCustomSynEdit.DoLinesInserted(FirstLine, Count: Integer);
var
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  fGutter.AutoSizeDigitCount;
  // Gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;

  // SynIndicators
  FIndicators.LinesInserted(FirstLine, Count);

  // Selections
  if not fUndoRedo.InsideUndoRedo then
    FSelections.LinesInserted(FirstLine, Count);

  // Plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phLinesInserted in Plugin.Handlers then
        Plugin.LinesInserted(FirstLine, Count);
    end;
end;

procedure TCustomSynEdit.DoLinePut(Index: Integer; const OldLine: string);
var
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  // SynIndicators
  FIndicators.LinePut(Index);

  // Selections
  if not fUndoRedo.InsideUndoRedo then
    FSelections.LinePut(Index, OldLine);

  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phLinePut in Plugin.Handlers then
        Plugin.LinePut(Index, OldLine);
    end;
end;

procedure TCustomSynEdit.PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: Integer);
var
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phAfterPaint in Plugin.Handlers then
        Plugin.AfterPaint(ACanvas, AClip, FirstLine, LastLine);
    end;
end;

procedure TCustomSynEdit.QuadrupleClick;
begin
  if not (eoNoSelection in fOptions) then
    SelectAll;
  if Assigned(fOnQudrupleClick) then
    fOnQudrupleClick(Self);
end;

procedure TCustomSynEdit.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyUpHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  fKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveKeyPressHandler(aHandler: TKeyPressEvent);
begin
  fKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TCustomSynEdit.AddFocusControl(aControl: TWinControl);
begin
  fFocusList.Add(aControl);
end;

procedure TCustomSynEdit.RemoveFocusControl(aControl: TWinControl);
begin
  fFocusList.Remove(aControl);
end;

function TCustomSynEdit.IsIdentChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsIdentChar(AChar)
  else
    Result := (AChar = '_') or AChar.IsLetterOrDigit or
      CharInSet(AChar, FAdditionalIdentChars);
end;

function TCustomSynEdit.IsNonWhiteChar(AChar: WideChar): Boolean;
begin
  Result := not IsWhiteChar(AChar);
end;

function TCustomSynEdit.IsWhiteChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWhiteChar(AChar)
  else
    Result := AChar.IsWhiteSpace and not IsIdentChar(AChar);
end;

function TCustomSynEdit.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWordBreakChar(AChar)
  else
  begin
    case AChar of
      #0..#32, '.', ',', ';', ':', '"', '''', #$00B4, '`',
      #$00B0, '^', '!', '?', '&', '$', '@', #$00A7, '%',
      '#', '~', '[', ']', '(', ')', '{', '}', '<', '>', '-', '=', '+', '*',
      '/', '\', '|':
        Result := True;
      else
        Result := False;
    end;

    Result := Result or CharInSet(AChar, FAdditionalWordBreakChars);
    Result := Result and not IsIdentChar(AChar);
  end;
end;

procedure TCustomSynEdit.SetSearchEngine(Value: TSynEditSearchCustom);
begin
  if (fSearchEngine <> Value) then
  begin
    fSearchEngine := Value;
    if Assigned(fSearchEngine) then
      fSearchEngine.FreeNotification(Self);
  end;
end;

function TCustomSynEdit.NextWordPos: TBufferCoord;
begin
  Result := NextWordPosEx(CaretXY);
end;

function TCustomSynEdit.WordStart: TBufferCoord;
begin
  Result := WordStartEx(CaretXY);
end;

function TCustomSynEdit.WordEnd: TBufferCoord;
begin
  Result := WordEndEx(CaretXY);
end;

function TCustomSynEdit.PrevWordPos: TBufferCoord;
begin
  Result := PrevWordPosEx(CaretXY);
end;

function TCustomSynEdit.GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
// Get XY caret position of mouse. Returns False if point is outside the
// region of the SynEdit control.
var
  Point: TPoint;
begin
  GetCursorPos(Point);                    // mouse position (on screen)
  Point := Self.ScreenToClient(Point);    // convert to SynEdit coordinates
  { Make sure it fits within the SynEdit bounds }
  if (Point.X < 0) or (Point.Y < 0) or (Point.X > Self.Width) or (Point.Y> Self.Height) then
  begin
    Result := False;
    Exit;
  end;

  { inside the editor, get the word under the mouse pointer }
  aPos := DisplayToBufferPos(PixelsToRowColumn(Point.X, Point.Y));
  Result := True;
end;

function TCustomSynEdit.GetWordAtMouse: string;
var
  Point: TBufferCoord;
begin
  { Return the word under the mouse }
  if GetPositionOfMouse(Point) then        // if point is valid
    Result := Self.GetWordAtRowCol(Point); // return the point at the mouse position
end;

function TCustomSynEdit.CharIndexToRowCol(Index: Integer;
  LineBreak: string): TBufferCoord;
{ Index is 0-based; Result.x and Result.y are 1-based }
var
  x, y, LBLength, Chars: Integer;
begin
  x := 0;
  y := 0;
  Chars := 0;
  LBLength := LineBreak.Length;
  while y < Lines.Count do
  begin
    x := Length(Lines[y]);
    if Chars + x + LBLength > Index then
    begin
      x := Index - Chars;
      Break;
    end
    else if (y = Lines.Count - 1) and (Index >= Chars + x) then
      Break;
    Inc(Chars, x + LBLength);
    x := 0;
    Inc(y);
  end;
  Result.Char := x + 1;
  Result.Line := y + 1;
end;

function TCustomSynEdit.RowColToCharIndex(RowCol: TBufferCoord;
  LineBreak: string): Integer;
{ Row and Col are 1-based; Result is 0-based }
var
  synEditStringList : TSynEditStringList;
begin
  RowCol.Line := Max(0, Min(Lines.Count, RowCol.Line) - 1);
  synEditStringList := (FLines as TSynEditStringList);
  Result :=  synEditStringList.LineCharIndex(RowCol.Line)
           + RowCol.Line * LineBreak.Length + (RowCol.Char -  1);
end;

procedure TCustomSynEdit.Clear;
{ just to attain interface compatibility with TMemo }
begin
  ClearAll;
end;

function TCustomSynEdit.GetSelLength: Integer;
begin
  if SelAvail then
    Result := RowColToCharIndex(BlockEnd) - RowColToCharIndex(BlockBegin)
  else
    Result := 0;
end;

procedure TCustomSynEdit.SetSelLength(const Value: Integer);
var
  iNewCharIndex: Integer;
  iNewBegin: TBufferCoord;
  iNewEnd: TBufferCoord;
begin
  iNewCharIndex := RowColToCharIndex(BlockBegin) + Value;
  if (Value >= 0) or (iNewCharIndex < 0) then
  begin
    if iNewCharIndex < 0 then
    begin
      iNewEnd.Char := Length(Lines[Lines.Count - 1]) + 1;
      iNewEnd.Line := Lines.Count;
    end
    else
      iNewEnd := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewEnd, BlockBegin, iNewEnd);
  end
  else begin
    iNewBegin := CharIndexToRowCol(iNewCharIndex);
    SetCaretAndSelection(iNewBegin, iNewBegin, BlockBegin);
  end;
end;

procedure TCustomSynEdit.DefineProperties(Filer: TFiler);

  function CollectionsEqual(C1, C2: TCollection): Boolean;
  begin
    Result := System.Classes.CollectionsEqual(C1, C2, nil, nil);
  end;

  function HasKeyData: Boolean;
  var
    iDefKeys: TSynEditKeyStrokes;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := not CollectionsEqual(Keystrokes,
        TCustomSynEdit(Filer.Ancestor).Keystrokes);
    end
    else begin
      iDefKeys := TSynEditKeyStrokes.Create(nil);
      try
        iDefKeys.ResetDefaults;
        Result := not CollectionsEqual(Keystrokes, iDefKeys);
      finally
        iDefKeys.Free;
      end;
    end;
  end;

var
  iSaveKeyData: Boolean;
begin
  inherited;
  iSaveKeyData := HasKeyData;
  Filer.DefineProperty('RemovedKeystrokes', ReadRemovedKeystrokes,
    WriteRemovedKeystrokes, iSaveKeyData);
  Filer.DefineProperty('AddedKeystrokes', ReadAddedKeystrokes, WriteAddedKeystrokes,
    iSaveKeyData);
end;

procedure TCustomSynEdit.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCustomSynEdit.ReadAddedKeystrokes(Reader: TReader);
var
  iAddKeys: TSynEditKeyStrokes;
  cKey: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iAddKeys := TSynEditKeyStrokes.Create(Self);
  try
    Reader.ReadCollection(iAddKeys);
    for cKey := 0 to iAddKeys.Count -1 do
      Keystrokes.Add.Assign(iAddKeys[cKey]);
  finally
    iAddKeys.Free;
  end;
end;

procedure TCustomSynEdit.ReadRemovedKeystrokes(Reader: TReader);
var
  iDelKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iToDelete: Integer;
begin
  if Reader.NextValue = vaCollection then
    Reader.ReadValue
  else
    Exit;
  iDelKeys := TSynEditKeyStrokes.Create(nil);
  try
    Reader.ReadCollection(iDelKeys);
    for cKey := 0 to iDelKeys.Count -1 do
    begin
      iKey := iDelKeys[cKey];
      iToDelete := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iToDelete >= 0) and (Keystrokes[iToDelete].Command = iKey.Command) then
        Keystrokes[iToDelete].Free;
    end;
  finally
    iDelKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteAddedKeystrokes(Writer: TWriter);
var
  iDefaultKeys: TSynEditKeyStrokes;
  iAddedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iDelIndex: Integer;
begin
  iDefaultKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iDefaultKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iDefaultKeys.ResetDefaults;
    iAddedKeys := TSynEditKeyStrokes.Create(nil);
    try
      for cKey := 0 to Keystrokes.Count -1 do
      begin
        iKey := Keystrokes[cKey];
        iDelIndex := iDefaultKeys.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
        //if it's not a default keystroke, add it
        if (iDelIndex < 0) or (iDefaultKeys[iDelIndex].Command <> iKey.Command) then
          iAddedKeys.Add.Assign(iKey);
      end;
      Writer.WriteCollection(iAddedKeys);
    finally
      iAddedKeys.Free;
    end;
  finally
    iDefaultKeys.Free;
  end;
end;

procedure TCustomSynEdit.WriteRemovedKeystrokes(Writer: TWriter);
var
  iRemovedKeys: TSynEditKeyStrokes;
  cKey: Integer;
  iKey: TSynEditKeyStroke;
  iFoundAt: Integer;
begin
  iRemovedKeys := TSynEditKeyStrokes.Create(nil);
  try
    if Writer.Ancestor <> nil then
      iRemovedKeys.Assign(TSynEdit(Writer.Ancestor).Keystrokes)
    else
      iRemovedKeys.ResetDefaults;
    cKey := 0;
    while cKey < iRemovedKeys.Count do
    begin
      iKey := iRemovedKeys[cKey];
      iFoundAt := Keystrokes.FindShortcut2(iKey.ShortCut, iKey.ShortCut2);
      if (iFoundAt >= 0) and (Keystrokes[iFoundAt].Command = iKey.Command) then
        iKey.Free //if exists in Keystrokes, then shouldn't be in "removed" list
      else
        Inc(cKey);
    end;
    Writer.WriteCollection(iRemovedKeys);
  finally
    iRemovedKeys.Free;
  end;
end;

procedure TCustomSynEdit.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseDownHandler(aHandler);
end;

procedure TCustomSynEdit.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.AddMouseUpHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  fKbdHandler.RemoveMouseUpHandler(aHandler);
end;

//++ CodeFolding

procedure TCustomSynEdit.FullFoldScan;
begin
  if UseCodeFolding then
    ReScanForFoldRanges(0, fLines.Count -1);
end;

procedure TCustomSynEdit.ReScanForFoldRanges(FromLine : Integer; ToLine : Integer);
Var
  AdjustedToLine: Integer;
begin
  AdjustedToLine := Max(Min(ToLine, Lines.Count - 1), FromLine);
  fAllFoldRanges.StartScanning;
  ScanForFoldRanges(fAllFoldRanges, fLines, FromLine, AdjustedToLine);
  {  StopScanning recreates AllFoldRanges.
     Normally at this point (sfLinesChanging in fStateFlags) = True
     and StopScanning will be called when LinesChanged is executed }
  if not (sfLinesChanging in fStateFlags) and fAllFoldRanges.StopScanning(fLines) then
  begin
    InvalidateGutterBand(gbkFold);
    if FIndentGuides.Visible and FIndentGuides.StructureHighlight and
      Assigned(fHighlighter) and (hcStructureHighlight in fHighlighter.Capabilities)
    then
      InvalidateLines(-1, -1);
    UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine : Integer; ToLine : Integer);
begin
  if fHighlighter is TSynCustomCodeFoldingHighlighter then
    TSynCustomCodeFoldingHighlighter(fHighlighter).ScanForFoldRanges(FoldRanges,
      LinesToScan, FromLine, ToLine);

  if Assigned(fOnScanForFoldRanges) then
     fOnScanForFoldRanges(Self, FoldRanges, LinesToScan, FromLine, ToLine);
end;
//-- CodeFolding

procedure TCustomSynEdit.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.AddMouseCursorHandler(aHandler);
end;

procedure TCustomSynEdit.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  fKbdHandler.RemoveMouseCursorHandler(aHandler);
end;

procedure TCustomSynEdit.DoSearchFindFirstExecute(Action: TSearchFindFirst);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnCloseBeforeSearch := Action.Dialog.OnClose;
  FSelections.Store(FSelStorage);

  Action.Dialog.OnFind := FindDialogFindFirst;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchFindExecute(Action: TSearchFind);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnCloseBeforeSearch := Action.Dialog.OnClose;

  Action.Dialog.OnFind := FindDialogFind;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchReplaceExecute(Action: TSearchReplace);
begin
  OnFindBeforeSearch := Action.Dialog.OnFind;
  OnReplaceBeforeSearch := Action.Dialog.OnReplace;
  OnCloseBeforeSearch := Action.Dialog.OnClose;

  Action.Dialog.OnFind := FindDialogFind;
  Action.Dialog.OnReplace := FindDialogFind;
  Action.Dialog.OnClose := FindDialogClose;
  Action.Dialog.Execute();
end;

procedure TCustomSynEdit.DoSearchFindNextExecute(Action: TSearchFindNext);
begin
  SearchByFindDialog(Action.SearchFind.Dialog);
end;

procedure TCustomSynEdit.FindDialogFindFirst(Sender: TObject);
begin
  TFindDialog(Sender).CloseDialog;

  CaretXY := BufferCoord(1, 1);

  if Sender is TFindDialog then
    if not SearchByFindDialog(TFindDialog(Sender)) then
    begin
      FSelections.Restore(FSelStorage);
      FSelStorage.Clear;
    end;
end;

procedure TCustomSynEdit.FindDialogFind(Sender: TObject);
begin
  if Sender is TFindDialog then
    SearchByFindDialog(TFindDialog(Sender));
end;

function TCustomSynEdit.SearchByFindDialog(FindDialog: TFindDialog) : bool;
var
  Options :TSynSearchOptions;
  ReplaceText, MessageText :String;
begin
  // If there is a selection apply to the selection only
  Options := [ssoSelectedOnly];
  if (frReplaceAll in FindDialog.Options) then Options := Options + [ssoReplaceAll]
  else if (frReplace in FindDialog.Options) then Options := Options + [ssoReplace];
  if (frMatchCase in FindDialog.Options) then Options := Options + [ssoMatchCase];
  if (frWholeWord in FindDialog.Options) then Options := Options + [ssoWholeWord];
  if (not (frDown in FindDialog.Options)) then Options := Options + [ssoBackwards];

  ReplaceText := TReplaceDialog(FindDialog).ReplaceText;

  if (UpperCase(SelText) = UpperCase(FindDialog.FindText)) and not (frReplace in FindDialog.Options)
    or not (ssoSelectedOnly in Options)
  then
  begin
    FSelections.Clear;
    CaretXY := CaretXY;
  end;

  Result := SearchReplace(FindDialog.FindText, ReplaceText, Options) > 0;
  if not Result then
  begin
    if Assigned(OnSearchNotFound) then
      OnSearchNotFound(self, FindDialog.FindText)
    else
    begin
      MessageText := Format(STextNotFound, [FindDialog.FindText]);
      ShowMessage(MessageText);
    end;
  end;
end;

procedure TCustomSynEdit.FindDialogClose(Sender: TObject);
begin
  TFindDialog(Sender).OnFind := OnFindBeforeSearch;
  if Sender is TReplaceDialog then
    TReplaceDialog(Sender).OnReplace := OnReplaceBeforeSearch;
  TFindDialog(Sender).OnClose := OnCloseBeforeSearch;
end;

function TCustomSynEdit.GetWordWrap: Boolean;
begin
  Result := fWordWrapPlugin <> nil;
end;

function TCustomSynEdit.GetWrapAreaWidth: Integer;
begin
  if (eoWrapWithRightEdge in FOptions) and (fRightEdge > 0) then
    Result := Max(fRightEdge * CharWidth - TextMargin, 0)
  else if HandleAllocated then
    Result := Max(ClientWidth - fGutterWidth - 2 * TextMargin, 0)
  else
    Result := 80 * fCharWidth; // will be set correctly when the Handle is created
end;

procedure TCustomSynEdit.SetWordWrap(const Value: Boolean);
var
  vOldTopLine: Integer;
  vShowCaret: Boolean;
begin
  if WordWrap <> Value then
  begin
    // !!Mutually exclusive with CodeFolding to reduce complexity
    if Value and UseCodeFolding then Exit;

    if HandleAllocated then
    begin
      Invalidate; // better Invalidate before changing LeftChar and TopLine
      vShowCaret := CaretInView;
      vOldTopLine := RowToLine(TopLine);
    end
    else
    begin
      // to keep compiler happy
      vShowCaret := False;
      vOldTopLine := 1;
    end;

    if Value then
    begin
      fWordWrapPlugin := TSynWordWrapPlugin.Create(Self);
      LeftChar := 1;
    end
    else
      fWordWrapPlugin := nil;

    if HandleAllocated then
    begin
      CalcTextAreaWidth;
      TopLine := LineToRow(vOldTopLine);
      UpdateScrollBars;

      if vShowCaret then
        EnsureCursorPosVisible;
    end;
  end;
end;

function TCustomSynEdit.GetDisplayRowCount: Integer;
begin
  if fWordWrapPlugin = nil then begin
    if fUseCodeFolding then
      Result := LineToRow(Lines.Count)
     else
      Result := Lines.Count
  end else if Lines.Count = 0 then
    Result := 0
  else begin
    Result := fWordWrapPlugin.RowCount;
  end;
end;

function TCustomSynEdit.LineToRow(aLine: Integer): Integer;
begin
  if not UseCodeFolding and not WordWrap then
    Result := aLine
  else
    Result := BufferToDisplayPos(BufferCoord(1, aLine)).Row;
end;

function TCustomSynEdit.RowToLine(aRow: Integer): Integer;
begin
  if not UseCodeFolding and not WordWrap then
    Result := aRow
  else begin
    Result := DisplayToBufferPos(DisplayCoord(1, aRow)).Line;
  end;
end;

procedure TCustomSynEdit.SetDisplayXY(const aPos: TDisplayCoord);
var
  OldCaretAtEOL: Boolean;
begin
  OldCaretAtEOL := CaretAtEOL;
  IncPaintLock;
  try
    SetCaretXYEx(False, DisplayToBufferPos(aPos));

    // fCaretEOL is set if we are at the end of wrapped row
    CaretAtEOL := WordWrap and (aPos.Row <= fWordWrapPlugin.RowCount) and
      (aPos.Column > fWordWrapPlugin.GetRowLength(aPos.Row)) and
      (DisplayY <> aPos.Row);
    if CaretAtEOL <> OldCaretAtEOL then
    begin
      InvalidateLine(CaretY);
      Include(fStateFlags, sfCaretChanged);
      UpdateLastPosX;
    end;

    EnsureCursorPosVisible;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetWantReturns(Value: Boolean);
begin
  fWantReturns := Value;
end;

procedure TCustomSynEdit.SetWantTabs(Value: Boolean);
begin
  fWantTabs := Value;
end;

procedure TCustomSynEdit.SetWordWrapGlyph(const Value: TSynGlyph);
begin
  fWordWrapGlyph.Assign(Value);
end;

procedure TCustomSynEdit.WordWrapGlyphChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    InvalidateGutter;
end;

procedure TCustomSynEdit.Zoom(ExtraFontSize: Integer);
var
  OldFontSize: Integer;
  OldGutterFontSize: Integer;
begin
  OldFontSize := FOrigFontSize;
  OldGutterFontSize := FOrigGutterFontSize;
  Font.Size := EnsureRange(Font.Size + ExtraFontSize, 3, 50);
  Gutter.Font.Size := EnsureRange(Gutter.Font.Size + ExtraFontSize, 2, 49);
  FOrigFontSize := OldFontSize;
  FOrigGutterFontSize := OldGutterFontSize;
  if Assigned(FOnZoom) then
    FOnZoom(Self, Font.Size, FOrigFontSize);
end;


procedure TCustomSynEdit.ZoomReset;
begin
  Font.Size := FOrigFontSize;
  Gutter.Font.Size := FOrigGutterFontSize;
  if Assigned(FOnZoom) then
    FOnZoom(Self, Font.Size, FOrigFontSize);
end;

{ TSynEditMark }

function TSynEditMark.GetEdit: TCustomSynEdit;
begin
  if FEdit <> nil then try
    if FEdit.Marks.IndexOf(self) = -1 then
      FEdit := nil;
  except
    FEdit := nil;
  end;
  Result := FEdit;
end;

function TSynEditMark.GetIsBookmark: Boolean;
begin
  Result := (fBookmarkNum >= 0);
end;

procedure TSynEditMark.SetChar(const Value: Integer);
begin
  FChar := Value;
end;

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetInternalImage(const Value: Boolean);
begin
  fInternalImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if fVisible and Assigned(fEdit) then
  begin
    if fLine > 0 then
      fEdit.InvalidateGutterLines(fLine, fLine);
    fLine := Value;
    fEdit.InvalidateGutterLines(fLine, fLine);
  end
  else
    fLine := Value;
end;

procedure TSynEditMark.SetVisible(const Value: Boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;
    if Assigned(fEdit) then
      fEdit.InvalidateGutterLines(fLine, fLine);
  end;
end;

constructor TSynEditMark.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fBookmarkNum := -1;
  fEdit := AOwner;
end;

{ TSynEditMarkList }

constructor TSynEditMarkList.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fEdit := AOwner;
end;

procedure TSynEditMarkList.ClearLine(Line: Integer);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if not Items[i].IsBookmark and (Items[i].Line = Line) then
      Delete(i);
end;

procedure TSynEditMarkList.GetMarksForLine(line: Integer; var marks: TSynEditMarks);
//Returns up to maxMarks book/gutter marks for a chosen line.
var
  cnt: Integer;
  i: Integer;
begin
  FillChar(marks, SizeOf(marks), 0);
  cnt := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Line = line then
    begin
      Inc(cnt);
      marks[cnt] := Items[i];
      if cnt = MAX_MARKS then break;
    end;
  end;
end;

procedure TSynEditMarkList.Place(Mark: TSynEditMark);
begin
  if assigned(fEdit) then
    if Assigned(fEdit.OnPlaceBookmark) then
      fEdit.OnPlaceBookmark(fEdit, Mark);
  if assigned(Mark) then
    Add(Mark);
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
const
  AllPlugInHandlers = [phLinesInserted, phLinesBeforeDeleted, phLinesDeleted,
    phLinePut, phLinesChanged, phPaintTransient, phAfterPaint];

begin
  Create(AOwner, AllPlugInHandlers); // for backward compatibility
end;

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit;
  AHandlers: TPlugInHandlers);
begin
  inherited Create;
  if AOwner <> nil then
  begin
    fOwner := AOwner;
    if fOwner.fPlugins = nil then
      fOwner.fPlugins := TObjectList.Create;
    fOwner.fPlugins.Add(Self);
  end;
  FHandlers := AHandlers;

  if phPaintTransient in Handlers then
    FOwner.FPaintTransientPlugins := True;
end;

destructor TSynEditPlugin.Destroy;
begin
  if fOwner <> nil then
    fOwner.fPlugins.Extract(Self); // we are being destroyed, fOwner should not free us
  inherited Destroy;
end;

procedure TSynEditPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer);
begin
  // nothing
end;

procedure TSynEditPlugin.PaintTransient(ACanvas: TCanvas; ATransientType: TTransientType);
begin
  // nothing
end;

procedure TSynEditPlugin.LinesChanged;
begin
  // nothing
end;

procedure TSynEditPlugin.LinesInserted(FirstLine, Count: Integer);
begin
  // nothing
end;

procedure TSynEditPlugin.LinePut(aIndex: Integer; const OldLine: string);
begin
  // nothing
end;

procedure TSynEditPlugin.LinesBeforeDeleted(FirstLine, Count: Integer);
begin
  // nothing
end;

procedure TSynEditPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
  // nothing
end;


initialization
 TCustomStyleEngine.RegisterStyleHook(TCustomSynEdit, TSynScrollingStyleHook);

finalization
 TCustomStyleEngine.UnRegisterStyleHook(TCustomSynEdit, TSynScrollingStyleHook);

end.
