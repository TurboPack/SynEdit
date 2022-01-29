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
//todo: in WordWrap mode, parse lines only once in PaintLines()
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
   // maximum scroll range
  MAX_SCROLL = 32767;

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

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfPossibleGutterClick,
    sfOleDragSource, sfGutterDragging);

  TSynStateFlags = set of TSynStateFlag;

  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TSynEditorOption = (
    eoAltSetsColumnMode,       //Holding down the Alt Key will put the selection mode into columnar format
    eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoDisableScrollArrows,     //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               //Allows the editor accept OLE file drops
    eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
    eoEnhanceEndKey,           //enhances End key positioning, similar to JDeveloper
    eoGroupUndo,               //When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          //When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      //if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoKeepCaretX,              //When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret,                 //Makes it so the caret is never visible
    eoNoSelection,             //Disables selecting text
    eoRightMouseMovesCursor,   //When clicking with the right mouse for a popup menu, move the cursor to that location
    eoScrollByOneLess,         //Forces scrolling to be one less
    eoScrollHintFollows,       //The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof,           //Allows the cursor to go past the end of file marker
    eoScrollPastEol,           //Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint,          //Shows a hint of the visible line numbers when scrolling vertically
    eoShowSpecialChars,        //Shows the special Characters
    eoSmartTabDelete,          //similar to Smart Tabs, but when you delete characters
    eoSmartTabs,               //When tabbing, the cursor will go to the next non-white space character of the previous line
    eoSpecialLineDefaultFg,    //disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent,               //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,            //Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,      //Spaces at the end of lines will be trimmed and not saved
    eoShowLigatures,           //Shows font ligatures, by default it is disabled
    eoCopyPlainText,           //Do not include additional clipboard formats when you copy to Clipboard or drag text
    eoWrapWithRightEdge        //WordWrap with RightEdge position instead CharsInWindow
    );

  TSynEditorOptions = set of TSynEditorOption;

const
  SYNEDIT_DEFAULT_OPTIONS = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabIndent, eoTabsToSpaces,
    eoSmartTabDelete, eoGroupUndo, eoShowLigatures];

type
  TCreateParamsW = record
    Caption: PWideChar;
    Style: DWORD;
    ExStyle: DWORD;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClassW;
    WinClassName: array[0..63] of WideChar;
    InternalCaption: string;
  end;

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

//++ CodeFolding
  TScanForFoldRangesEvent = procedure(Sender: TObject;
    FoldRanges: TSynFoldRanges; LinesToScan: TStrings;
    FromLine : Integer; ToLine : Integer) of object;
//-- CodeFolding


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
  TSynEditMarkList = class(TObjectList)            // It makes more sence to derive from TObjectList,
  protected                                        // as it automatically frees its members
    fEdit: TCustomSynEdit;
    fOnChange: TNotifyEvent;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(Index: Integer): TSynEditMark;
    procedure SetItem(Index: Integer; Item: TSynEditMark);
    property OwnsObjects;                          // This is to hide the inherited property,
  public                                           // because TSynEditMarkList always owns the marks
    constructor Create(AOwner: TCustomSynEdit);
    function First: TSynEditMark;
    function Last: TSynEditMark;
    function Extract(Item: TSynEditMark): TSynEditMark;
    procedure ClearLine(line: Integer);
    procedure GetMarksForLine(line: Integer; var Marks: TSynEditMarks);
    procedure Place(mark: TSynEditMark);
  public
    property Items[Index: Integer]: TSynEditMark read GetItem write SetItem; default;
    property Edit: TCustomSynEdit read fEdit;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure WMImeChar(var Msg: TMessage); message WM_IME_CHAR;
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMUndo(var Msg: TMessage); message WM_UNDO;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
  private
//++ CodeFolding
    fUseCodeFolding : Boolean;
    fCodeFolding: TSynCodeFolding;
    fAllFoldRanges: TSynFoldRanges;
//-- CodeFolding
    fAlwaysShowCaret: Boolean;
    fBlockBegin: TBufferCoord;
    fBlockEnd: TBufferCoord;
    fCaretX: Integer;
    FLastPosX: integer;
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
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
    fTextHeight: Integer;
    fTextMargin: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    fHighlighter: TSynCustomHighlighter;
    fSelectedColor: TSynSelectedColor;
    fActiveLineColor: TColor;
    fUndoRedo: ISynEditUndo;
    fBookMarks: array[0..9] of TSynEditMark; // these are just references, fMarkList is the owner
    fMouseDownX: Integer;
    fMouseDownY: Integer;
    fBookMarkOpt: TSynBookMarkOpt;
    fBorderStyle: TSynBorderStyle;
    fHideSelection: Boolean;
    fMouseWheelAccumulator: Integer;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fCaretOffset: TPoint;
    fKeyStrokes: TSynEditKeyStrokes;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: Integer;
    fSelectionMode: TSynSelectionMode;
    fActiveSelectionMode: TSynSelectionMode; //mode of the active selection
    fWantReturns: Boolean;
    fWantTabs: Boolean;
    fWordWrapPlugin: ISynEditBufferPlugin;
    fWordWrapGlyph: TSynGlyph;
    fCaretAtEOL: Boolean; // used by wordwrap

    fGutter: TSynGutter;
    fTabWidth: Integer;
    fInvalidateRect: TRect;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
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
    FPaintTransientLock: Integer;
    FIsScrolling: Boolean;
    FAdditionalWordBreakChars: TSysCharSet;
    FAdditionalIdentChars: TSysCharSet;
    FTextFormat: TSynTextFormat;
    SelStartBeforeSearch: integer;
    SelLengthBeforeSearch: integer;

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
    fOnGutterGetText: TGutterGetTextEvent;
    fOnStatusChange: TStatusChangeEvent;
    fOnTripleClick: TNotifyEvent;
    fOnQudrupleClick: TNotifyEvent;

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
    procedure DoLinePut(FirstLine: Integer; const OldLine: string);
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
    function GetCaretXY: TBufferCoord;
    function GetDisplayX: Integer;
    function GetDisplayY: Integer;
    function GetDisplayXY: TDisplayCoord;
    function GetDisplayLineCount: Integer;
    function GetHookedCommandHandlersCount: Integer;
    function GetLineText: string;
    function GetMaxUndo: Integer;
    function GetModified: Boolean;
    function GetOptions: TSynEditorOptions;
    function GetRow(RowIndex: Integer): string;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    function SynGetText: string;
    function GetWordAtCursor: string;
    function GetWordAtMouse: string;
    function GetWordWrap: Boolean;
    procedure GutterChanged(Sender: TObject);
    function LeftSpaces(const Line: string; ExpandTabs: Boolean = False): Integer;
    function GetLeftSpacing(CharCount: Integer; WantTabs: Boolean): string;
    procedure LinesChanging(Sender: TObject);
    procedure MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
      SelectionCommand: Boolean);
    procedure MoveCaretHorz(DX: Integer; SelectionCommand: Boolean);
    procedure MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
    procedure PluginsAfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer);
    procedure ReadAddedKeystrokes(Reader: TReader);
    procedure ReadRemovedKeystrokes(Reader: TReader);
    function ScanFrom(Index: Integer): Integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SelectedColorsChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TBufferCoord);
    procedure SetBlockEnd(Value: TBufferCoord);
    procedure SetBorderStyle(Value: TSynBorderStyle);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure InternalSetCaretX(Value: Integer);
    procedure InternalSetCaretY(Value: Integer);
    procedure SetInternalDisplayXY(const aPos: TDisplayCoord);
    procedure SetActiveLineColor(Value: TColor);
    procedure SetExtraLineSpacing(const Value: Integer);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: Boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TStrings);
    procedure SetLineText(Value: string);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: Boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSearchEngine(Value: TSynEditSearchCustom);
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetTabWidth(Value: Integer);
    procedure SynSetText(const Value: string);
    procedure SetTopLine(Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetWordWrapGlyph(const Value: TSynGlyph);
    procedure WordWrapGlyphChange(Sender: TObject);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure ProperSetLine(ALine: Integer; const ALineText: string);
    procedure ModifiedChanged(Sender: TObject);
    procedure UpdateLastPosX;
    procedure UpdateScrollBars;
    procedure WriteAddedKeystrokes(Writer: TWriter);
    procedure WriteRemovedKeystrokes(Writer: TWriter);
    procedure SetAdditionalIdentChars(const Value: TSysCharSet);
    procedure SetAdditionalWordBreakChars(const Value: TSysCharSet);

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
//++ CodeFolding
    procedure SetUseCodeFolding(const Value: Boolean);
    procedure OnCodeFoldingChange(Sender: TObject);
    function GetCollapseMarkRect(Row: Integer; Line: Integer = -1): TRect;
    function GetWrapAreaWidth: Integer;
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
    procedure MarkListChange(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure NotifyHookedCommandHandlers(AfterProcessing: Boolean;
      var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintGutter(const AClip: TRect; const aFirstRow,
      aLastRow: Integer); virtual;
    procedure PaintTextLines(AClip: TRect; const aFirstRow, aLastRow:
        Integer); virtual;
    procedure InternalSetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXY(const Value: TBufferCoord); virtual;
    procedure SetCaretXYEx(EnsureVisible: Boolean; Value: TBufferCoord); virtual;
    procedure SetFontQuality(AValue: TFontQuality);
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetWantReturns(Value: Boolean);
    procedure SetSelText(const Value: string);
    procedure SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode; Value: string;
        AddToUndoList: Boolean = True; SilentDelete: Boolean = False);
    procedure SetWantTabs(Value: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UpdateMouseCursor; virtual;
  protected
    fGutterWidth: Integer;
    procedure CalcTextAreaWidth;
    procedure HideCaret;
    procedure ShowCaret;
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    procedure DoOnGutterClick(Button: TMouseButton; X, Y: Integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean); virtual;
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
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelLength(const Value: integer);
    procedure SetAlwaysShowCaret(const Value: Boolean);
    procedure LinesHookChanged;
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
    procedure UpdateCaret;
    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word = 0; SS2: TShiftState = []);
    procedure BeginUndoBlock;
    procedure BeginUpdate;
    function CaretInView: Boolean;
    function CharIndexToRowCol(Index: Integer): TBufferCoord;
    procedure Clear;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    procedure ClearUndo;
    procedure ClearTrackChanges;
    procedure MarkSaved;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure DoCopyToClipboard(const SText: string);
    procedure EndUndoBlock;
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
    procedure EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
      EvenIfVisible: Boolean = False);
    procedure FindMatchingBracket; virtual;
    function GetMatchingBracket: TBufferCoord; virtual;
    function GetMatchingBracketEx(const APoint: TBufferCoord;
      Brackets: string = '()[]{}<>'): TBufferCoord; virtual;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); virtual;
    function GetBookMark(BookMark: Integer; var X, Y: Integer): Boolean;
    function GetHighlighterAttriAtRowCol(const XY: TBufferCoord; var Token: string;
      var Attri: TSynHighlighterAttributes): Boolean;
    function GetHighlighterAttriAtRowColEx(const XY: TBufferCoord; var Token: string;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetPositionOfMouse(out aPos: TBufferCoord): Boolean;
    function GetWordAtRowCol(XY: TBufferCoord): string;
    procedure GotoBookMark(BookMark: Integer); virtual;
    procedure GotoLineAndCenter(ALine: Integer); virtual;
    function RowColumnInView(RowCol: TDisplayCoord): Boolean;
    function TextWidth(const S: string): Integer;
    function IsIdentChar(AChar: WideChar): Boolean; virtual;
    function IsWhiteChar(AChar: WideChar): Boolean; virtual;
    function IsWordBreakChar(AChar: WideChar): Boolean; virtual;
    // support procedure for ecDeletexxx commands
    function IsNonWhiteChar(AChar: WideChar): Boolean; virtual;

    procedure DoBlockIndent;
    procedure DoBlockUnindent;

    procedure InvalidateGutter;
    procedure InvalidateGutterLine(aLine: integer);
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLine(Line: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure InvalidateSelection;
    function IsBookmark(BookMark: Integer): Boolean;
    function IsPointInSelection(const Value: TBufferCoord): Boolean;
    procedure LockUndo;
    function BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
    function LineToRow(aLine: Integer): Integer;
    function RowToLine(aRow: Integer): Integer;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;

    function NextWordPos: TBufferCoord; virtual;
    function NextWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordStart: TBufferCoord; virtual;
    function WordStartEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function WordEnd: TBufferCoord; virtual;
    function WordEndEx(const XY: TBufferCoord): TBufferCoord; virtual;
    function PrevWordPos: TBufferCoord; virtual;
    function PrevWordPosEx(const XY: TBufferCoord): TBufferCoord; virtual;

    function PixelsToColumn(S: string; aX: Integer): Integer;
    function PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
    function PixelsToNearestRowColumn(aX, aY: Integer): TDisplayCoord;
    procedure Redo;
    procedure RegisterCommandHandler(const AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
    function RowColToCharIndex(RowCol: TBufferCoord): Integer;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): Integer;
    procedure SelectAll;
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TBufferCoord);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetSelWord;
    procedure SetWordBlock(Value: TBufferCoord);
    procedure Undo;
    procedure UnlockUndo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function ValidTextPos(BC: TBufferCoord; Trailing: Boolean): TBufferCoord;
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
    {Command implementations}
    procedure ExecCmdDeleteLine;
    procedure ExecCmdCopyOrMoveLine(const Command: TSynEditorCommand);
    procedure ExecCmdCaseChange(const Cmd : TSynEditorCommand);
//++ CodeFolding
    procedure CollapseAll;
    procedure UncollapseAll;
    procedure Collapse(FoldRangeIndex: Integer; Invalidate:Boolean = True);
    procedure Uncollapse(FoldRangeIndex: Integer; Invalidate:Boolean = True);
    procedure UncollapseAroundLine(Line: Integer);
    procedure CollapseNearest;
    procedure UncollapseNearest;
    procedure CollapseLevel(Level : integer);
    procedure UnCollapseLevel(Level : integer);
    procedure CollapseFoldType(FoldType : Integer);
    procedure UnCollapseFoldType(FoldType : Integer);
//-- CodeFolding
  public
    property AdditionalIdentChars: TSysCharSet read FAdditionalIdentChars write SetAdditionalIdentChars;
    property AdditionalWordBreakChars: TSysCharSet read FAdditionalWordBreakChars write SetAdditionalWordBreakChars;
    property BlockBegin: TBufferCoord read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TBufferCoord read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    property CaretXY: TBufferCoord read GetCaretXY write SetCaretXY;
    property ActiveLineColor: TColor read fActiveLineColor
      write SetActiveLineColor default clNone;
    property DisplayX: Integer read GetDisplayX;
    property DisplayY: Integer read GetDisplayY;
    property DisplayXY: TDisplayCoord read GetDisplayXY;
    property DisplayLineCount: Integer read GetDisplayLineCount;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: Integer read fCharWidth;
    property Color;
    property Cursor default crIBeam;
    property Font;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: Integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow;
    property LineText: string read GetLineText write SetLineText;
    property Lines: TStrings read fLines write SetLines;
    property Rows[RowIndex: integer]: string read GetRow;
    property Marks: TSynEditMarkList read fMarkList;
    property Modified: Boolean read GetModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchEngine: TSynEditSearchCustom read fSearchEngine write SetSearchEngine;
    property SelAvail: Boolean read GetSelAvail;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    property StateFlags: TSynStateFlags read fStateFlags;
    property Text: string read SynGetText write SynSetText;
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
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property IsScrolling : Boolean read FIsScrolling;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes stored False;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 0;
    property Options: TSynEditorOptions read GetOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
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
      read FSelectedColor write FSelectedColor;
    property SelectionMode: TSynSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    property ActiveSelectionMode: TSynSelectionMode read fActiveSelectionMode
      write SetActiveSelectionMode stored False;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantReturns: boolean read fWantReturns write SetWantReturns default True;
    property WantTabs: boolean read fWantTabs write SetWantTabs default False;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default False;
    property WordWrapGlyph: TSynGlyph read fWordWrapGlyph write SetWordWrapGlyph;
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
    property OnScroll: TScrollEvent read fOnScroll write fOnScroll;
    property OnTripleClick: TNotifyEvent
      read fOnTripleClick write fOnTripleClick;
    property OnQuadrupleClick: TNotifyEvent
      read fOnQudrupleClick write fOnQudrupleClick;
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
    property Align;
    property Anchors;
    property DoubleBuffered;
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
    property FontQuality default fqClearTypeNatural;
    property Gutter;
    property HideSelection;
    property Highlighter;
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
    property SearchEngine;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
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
    property OnScroll;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;
    property OnTripleClick;
    property OnQuadrupleClick;
    property OnSearchNotFound;
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
  SynEditScrollBars,
  SynEditUndo,
  SynEditWordWrap,
  SynEditStrConst,
  SynEditDataObject,
  SynEditDragDrop;

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

function TCustomSynEdit.PixelsToColumn(S: string; aX: Integer): Integer;
var
  Layout: TSynTextLayout;
  HTM: TDwriteHitTestMetrics;
  IsTrailing, IsInside: LongBool;
begin
  if S = '' then
    Result := Max(ax div fCharWidth, 1)
  else
  begin
    Layout.Create(FTextFormat, PChar(S), S.Length, MaxInt, fTextHeight);
    CheckOSError(Layout.IDW.HitTestPoint(aX,
      fTextHeight div 2, IsTrailing, IsInside, HTM));
    Result := Integer(HTM.textPosition) + IfThen(IsTrailing, HTM.length + 1, 1);
    if (Result = S.Length + 1) and (ax > HTM.left + HTM.width) then
      Inc(Result, Round((ax - HTM.left - HTM.width) / fCharWidth));
  end;
end;

function TCustomSynEdit.PixelsToRowColumn(aX, aY: Integer): TDisplayCoord;
var
  S: string;
begin
  Result.Row := Max(1, TopLine + (aY div fTextHeight));
  S := Rows[Result.Row];
  if S = '' then
    Result.Column := Max(1, (aX - fTextOffset) div fCharWidth)
  else
    Result.Column := PixelsToColumn(S, ax - fTextOffset);
end;

function TCustomSynEdit.RowColumnToPixels(const RowCol: TDisplayCoord): TPoint;
var
  Layout: TSynTextLayout;
  S: string;
  X, Y: single;
  HTM: TDwriteHitTestMetrics;
begin
  Result.Y := (RowCol.Row - fTopLine) * fTextHeight;

  S := Rows[RowCol.Row];
  if S = '' then
    Result.X := (RowCol.Column - 1) * fCharWidth
  else
  begin
    Layout.Create(FTextFormat, PChar(S), S.Length, MaxInt, fTextHeight);
    if RowCol.Column > S.Length then
      Result.X := Round(Layout.TextMetrics.widthIncludingTrailingWhitespace) +
      (RowCol.Column - S.Length - 1) * fCharWidth
    else
    begin
      CheckOSError(Layout.IDW.HitTestTextPosition(RowCol.Column - 1, False, X, Y, HTM));
      Result.X := Round(X);
    end;
  end;
  Inc(Result.X, fTextOffset);
end;

function TCustomSynEdit.ValidTextPos(BC: TBufferCoord; Trailing: Boolean): TBufferCoord;
//  Accounts of surrogate pairs and combining diacritics
var
  Layout: TSynTextLayout;
  S: string;
  X, Y: single;
  HTM: TDwriteHitTestMetrics;
begin
  Result := BC;
  if not InRange(BC.Line, 1, Lines.Count) then Exit;

  S:= Lines[BC.Line - 1];
  if InRange(BC.Char, 2, S.Length)  then
  begin
    Layout.Create(FTextFormat, PChar(S), S.Length, MaxInt, fTextHeight);
    CheckOSError(Layout.IDW.HitTestTextPosition(BC.Char - 1, False, X, Y, HTM));
    if BC.Char <> Integer(HTM.textPosition) + 1 then
      Result.Char := IfThen(Trailing and (HTM.length > 1),
        HTM.textPosition + HTM.length + 1, HTM.textPosition + 1);
  end;
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
//X,Y are pixel coordinates
var
  vCaretNearestPos : TDisplayCoord;
begin
  vCaretNearestPos := PixelsToNearestRowColumn(X, Y);
  vCaretNearestPos.Row := MinMax(vCaretNearestPos.Row, 1, DisplayLineCount);
  SetInternalDisplayXY(vCaretNearestPos);
end;

procedure TCustomSynEdit.ComputeScroll(X, Y: Integer);
//X,Y are pixel coordinates
var
  iScrollBounds: TRect; { relative to the client area }
  ScrollAreaSize : integer;
const
  ScrollAreaDefaultSize = 4;
begin
  iScrollBounds := Bounds(fGutterWidth, 0, fCharsInWindow * fCharWidth,
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
    fScrollDeltaX := (X - iScrollBounds.Right) div fCharWidth + 1
  else
    fScrollDeltaX := 0;

  if (Y < iScrollBounds.Top) and (TopLine > 1) then
    fScrollDeltaY := (Y - iScrollBounds.Top) div fTextHeight - 1
  else if Y >= iScrollBounds.Bottom then
    fScrollDeltaY := (Y - iScrollBounds.Bottom) div fTextHeight + 1
  else
    fScrollDeltaY := 0;

  fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
end;

procedure TCustomSynEdit.DoCopyToClipboard(const SText: string);
begin
  OleSetClipboard(TSynEditDataObject.Create(Self));
end;

procedure TCustomSynEdit.CopyToClipboard;
var
  SText: string;
  ChangeTrim: Boolean;
begin
  if SelAvail then
  begin
    ChangeTrim := (fActiveSelectionMode = smColumn) and (eoTrimTrailingSpaces in Options);
    try
      if ChangeTrim then
        Exclude(fOptions, eoTrimTrailingSpaces);
      SText := SelText;
    finally
      if ChangeTrim then
        Include(fOptions, eoTrimTrailingSpaces);
    end;
    DoCopyToClipboard(SText);
  end;
end;

procedure TCustomSynEdit.CutToClipboard;
begin
  if not ReadOnly and SelAvail then
  begin
    BeginUndoBlock;
    try
      DoCopyToClipboard(SelText);
      SelText := '';
    finally
      EndUndoBlock;
    end;
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
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
  fFontDummy := TFont.Create;
  fUndoRedo := CreateSynEditUndo(Self);
  fUndoRedo.OnModifiedChanged := ModifiedChanged;
  fOrigUndoRedo := fUndoRedo;

  DoubleBuffered := False;
  fActiveLineColor := clNone;
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := SelectedColorsChanged;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := BookMarkOptionsChanged;
  fTextMargin := 3;
  // fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create(Self);
  fGutter.OnChange := GutterChanged;
  fWordWrapGlyph := TSynGlyph.Create(HINSTANCE, 'SynEditWrapped');
  fWordWrapGlyph.OnChange := WordWrapGlyphChange;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption];
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  Color := clWindow;
  fExtraLineSpacing := 2;
  fFontQuality := fqClearTypeNatural;
  fFontDummy.Name := DefaultFontName;
  fFontDummy.Size := 10;
  fFontDummy.CharSet := DEFAULT_CHARSET;
  fFontDummy.Quality := fFontQuality;
  Font.Assign(fFontDummy);
  Font.OnChange := SynFontChanged;
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  fInserting := True;
  fScrollBars := ssBoth;
  fBorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  fActiveSelectionMode := smNormal;
  fFocusList := TList.Create;
  fKbdHandler := TSynEditKbdHandler.Create;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  fMarkList := TSynEditMarkList.Create(self);
  fMarkList.OnChange := MarkListChange;
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
  fWantReturns := True;
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  fCaretX := 1;
  FLastPosX := 0;
  fCaretY := 1;
  fBlockBegin.Char := 1;
  fBlockBegin.Line := 1;
  fBlockEnd := fBlockBegin;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := ScrollTimerHandler;

  fScrollHintColor := clInfoBk;
  fScrollHintFormat := shfTopLineOnly;

  FSynEditScrollBars := CreateSynEditScrollBars(Self);

//++ CodeFolding
  fCodeFolding := TSynCodeFolding.Create;
  fCodeFolding.OnChange := OnCodeFoldingChange;
  fAllFoldRanges := TSynFoldRanges.Create;
//-- CodeFolding
  SynFontChanged(nil);
  GutterChanged(nil); // to caclulate fGutterWidth also updates fTextOffset
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
var
  vAuxPos: TDisplayCoord;
begin
  Assert(fPaintLock > 0);
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then
  begin
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    //TODO
    // Locks the caret inside the visible area
//    if WordWrap and ([scCaretX,scCaretY] * fStatusChanges <> []) then
//    begin
//      vAuxPos := DisplayXY;
//      // This may happen in the last row of a line or in rows which length is
//      // greater than fCharsInWindow (Tabs and Spaces are allowed beyond
//      // fCharsInWindow while wrapping the lines)
//      if (vAuxPos.Column > fCharsInWindow + 1) and (fCharsInWindow > 0) then
//      begin
//        Assert(False);
//        if fCaretAtEOL then
//          fCaretAtEOL := False
//        else
//        begin
//          if scCaretY in fStatusChanges then
//          begin
//            vAuxPos.Column := fCharsInWindow + 1;
//            fCaretX := DisplayToBufferPos(vAuxPos).Char;
//            Include(fStatusChanges,scCaretX);
//            UpdateLastPosX;
//          end;
//        end;
//        Include(fStateFlags, sfCaretChanged);
//      end;
//    end;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

destructor TCustomSynEdit.Destroy;
begin
  Highlighter := nil;
  if (fChainedEditor <> nil) or (fLines <> fOrigLines) then
    RemoveLinesPointer;

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
  fUndoRedo := nil;
  fOrigUndoRedo := nil;
  fGutter.Free;
  fWordWrapGlyph.Free;
  fFontDummy.Free;
  fOrigLines.Free;
//++ CodeFolding
  fCodeFolding.Free;
  fAllFoldRanges.Free;
//-- CodeFolding
end;

function TCustomSynEdit.GetBlockBegin: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
  then
    Result := fBlockEnd
  else
    Result := fBlockBegin;
end;

function TCustomSynEdit.GetBlockEnd: TBufferCoord;
begin
  if (fBlockEnd.Line < fBlockBegin.Line)
    or ((fBlockEnd.Line = fBlockBegin.Line) and (fBlockEnd.Char < fBlockBegin.Char))
  then
    Result := fBlockBegin
  else
    Result := fBlockEnd;
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
  TSynEditStringList(fLines).FontChanged;
  if fGutter.ShowLineNumbers then
    GutterChanged(Self);
  // Invalidate and handle the changes
  SizeOrFontChanged(True);
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
  Result := (fBlockBegin.Char <> fBlockEnd.Char) or
    ((fBlockBegin.Line <> fBlockEnd.Line) and (fActiveSelectionMode <> smColumn));
end;

function TCustomSynEdit.GetSelText: string;

  function CopyPadded(const S: string; Index, Count: Integer): string;
  var
    SrcLen: Integer;
    DstLen: Integer;
    i: Integer;
    P: PWideChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PWideChar(Result);
      StrCopy(P, PWideChar(Copy(S, Index, Count)));
      Inc(P, Length(S));
      for i := 0 to DstLen - Srclen - 1 do
        P[i] := #32;
    end;
  end;

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

  function CopyPaddedAndForward(const S: string; Index, Count: Integer;
    var P: PWideChar): Integer;
  var
    OldP: PWideChar;
    Len, i: Integer;
  begin
    Result := 0;
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    if not (eoTrimTrailingSpaces in Options) then
    begin
      for i := 0 to Len - 1 do
        P[i] := #32;
      Inc(P, Len);
    end
    else
      Result:= Len;
  end;

var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
  l, r: Integer;
  s: string;
  P: PWideChar;
  cRow: Integer;
  vAuxLineChar: TBufferCoord;
  vAuxRowCol: TDisplayCoord;
  vTrimCount: Integer;
begin
  if not SelAvail then
    Result := ''
  else begin
    ColFrom := BlockBegin.Char;
    First := BlockBegin.Line - 1;
    //
    ColTo := BlockEnd.Char;
    Last := BlockEnd.Line - 1;
    //
    TotalLen := 0;
    case fActiveSelectionMode of
      smNormal:
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
      smColumn:
        begin
          with BufferToDisplayPos(BlockBegin) do
          begin
            First := Row;
            ColFrom := Column;
          end;
          with BufferToDisplayPos(BlockEnd) do
          begin
            Last := Row;
            ColTo := Column;
          end;
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: pre-allocate string large enough for worst case
          TotalLen := ((ColTo - ColFrom) + Length(sLineBreak)) *
            (Last - First +1);
          SetLength(Result, TotalLen);
          P := PWideChar(Result);

          // step2: copy chunks to the pre-allocated string
          TotalLen := 0;
          for cRow := First to Last do
          begin
            vAuxRowCol.Row := cRow;
            vAuxRowCol.Column := ColFrom;
            vAuxLineChar := DisplayToBufferPos(vAuxRowCol);
            l := vAuxLineChar.Char;
            s := Lines[vAuxLineChar.Line - 1];
            vAuxRowCol.Column := ColTo;
            r := DisplayToBufferPos(vAuxRowCol).Char;

            vTrimCount := CopyPaddedAndForward(s, l, r - l, P);
            TotalLen := TotalLen + (r - l) - vTrimCount + Length(sLineBreak);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          SetLength(Result, TotalLen - Length(sLineBreak));
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calculate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(Lines[i]) + Length(SLineBreak));
          if Last = Lines.Count then
            Dec(TotalLen, Length(SLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PWideChar(Result);
          for i := First to Last - 1 do
          begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(SLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if (Last + 1) < Lines.Count then
            CopyAndForward(SLineBreak, 1, MaxInt, P);
        end;
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

procedure TCustomSynEdit.HideCaret;
begin
  if sfCaretVisible in fStateFlags then
    if Winapi.Windows.HideCaret(Handle) then
      Exclude(fStateFlags, sfCaretVisible);
end;

procedure TCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
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
      if sfLinesChanging in fStateFlags then
//++ Flicker Reduction
          UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
//-- Flicker Reduction
      else
        InvalidateRect(rcInval, False);
    end
    else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then
        SwapInt(LastLine, FirstLine);
//++ CodeFolding
      if UseCodeFolding or WordWrap then
//-- CodeFolding
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
        if sfLinesChanging in fStateFlags then
//++ Flicker Reduction
          UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
//-- Flicker Reduction
        else
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
      if sfLinesChanging in fStateFlags then
//++ Flicker Reduction
        UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
//-- Flicker Reduction
      else
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
        if sfLinesChanging in fStateFlags then
//++ Flicker Reduction
          UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
//++ Flicker Reduction
        else
          InvalidateRect(rcInval, False);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateSelection;
begin
  InvalidateLines(BlockBegin.Line, BlockEnd.Line);
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
begin
  inherited Loaded;
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

function TCustomSynEdit.LeftSpaces(const Line: string; ExpandTabs: Boolean): Integer;
var
  P: PChar;
begin
  Result := 0;
  P := PChar(Line);
  if Assigned(P) then
  begin
    while (P^ >= #1) and (P^ <= #32) do
    begin
      if (P^ = #9) and ExpandTabs then
        Inc(Result, TabWidth - (Result mod FTabWidth))
      else
        Inc(Result);
      Inc(P);
    end;
  end;
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
var
  vOldMode: TSynSelectionMode;
begin
  DoLinesChanged;

//++ CodeFolding
  if (sfLinesChanging in fStateFlags) and fAllFoldRanges.StopScanning(fLines) then
  begin
    if Assigned(fHighlighter) and (fHighlighter is TSynCustomCodeFoldingHighlighter) then
      TSynCustomCodeFoldingHighlighter(fHighlighter).AdjustFoldRanges(AllFoldRanges,
      fLines);
    InvalidateGutter;
    Include(fStateFlags, sfScrollbarChanged);
  end;
//-- CodeFolding

  Exclude(fStateFlags, sfLinesChanging);
  if HandleAllocated then
  begin
//++ Flicker Reduction
//    UpdateScrollBars;
//-- Flicker Reduction
    vOldMode := fActiveSelectionMode;
    //SetBlockBegin(CaretXY);
    fActiveSelectionMode := vOldMode;
    InvalidateRect(fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TRect), 0);
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
  DoChange;
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  bWasSel: Boolean;
  TmpBegin, TmpEnd: TBufferCoord;
  P : TPoint;
  // Ole drag drop
  DragSource : IDropSource;
  DataObject : IDataObject;
  dwEffect : integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  TmpBegin := FBlockBegin;
  TmpEnd := FBlockEnd;

  //remember selection state, as it will be cleared later
  bWasSel := SelAvail;

  if (Button = mbLeft) and ((Shift + [ssDouble]) = [ssLeft, ssDouble]) then
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

  if (Button = mbLeft) and (fClickCount > 1) then Exit;

  fKbdHandler.ExecuteMouseDown(Self, Button, Shift, X, Y);

  if (Button in [mbLeft, mbRight]) then
  begin
    if Button = mbRight then
    begin
      if (eoRightMouseMovesCursor in Options) and
         (SelAvail and not IsPointInSelection(DisplayToBufferPos(PixelsToRowColumn(X, Y)))
         or not SelAvail) then
      begin
        InvalidateSelection;
        FBlockEnd := FBlockBegin;
        ComputeCaret(X, Y);
      end
      else
        Exit;
    end
    else
      ComputeCaret(X, Y);
  end;

  if Button = mbLeft then
  begin
    //I couldn't track down why, but sometimes (and definately not all the time)
    //the block positioning is lost.  This makes sure that the block is
    //maintained in case they started a drag operation on the block
    FBlockBegin := TmpBegin;
    FBlockEnd := TmpEnd;

    MouseCapture := True;
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
        finally
          Exclude(fStateFlags, sfOleDragSource);
          if dwEffect = DROPEFFECT_MOVE then
            ClearSelection;
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

  if not (ssDouble in Shift) then begin
    if ssShift in Shift then
      //BlockBegin and BlockEnd are restored to their original position in the
      //code from above and SetBlockEnd will take care of proper invalidation
      SetBlockEnd(CaretXY)
    else
    begin
      if (eoAltSetsColumnMode in Options) and (fActiveSelectionMode <> smLine) then
      begin
        if ssAlt in Shift then
          SelectionMode := smColumn
        else
          SelectionMode := smNormal;
      end;
      //Selection mode must be set before calling SetBlockBegin
      SetBlockBegin(CaretXY);
    end;
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
    P.Row := MinMax(P.Row, 1, DisplayLineCount);
//  Not sure what was the purpose of these
//    if fScrollDeltaX <> 0 then
//      P.Column := DisplayX;
//    if fScrollDeltaY <> 0 then
//      P.Row := DisplayY;
    BC := DisplayToBufferPos(P);

    if BC = CaretXY then Exit;  // no movement

    if (ActiveSelectionMode = smNormal) and (fClickCount = 2) then
      DoMouseSelectWordRange(BC)
    else if (ActiveSelectionMode = smNormal) and (fClickCount = 3) then
      DoMouseSelectLineRange(BC)
    else begin
      InternalCaretXY := BC;
      BlockEnd := BC;
    end;

    if (sfPossibleGutterClick in fStateFlags) and (FBlockBegin.Line <> CaretXY.Line) then
      Include(fStateFlags, sfGutterDragging);
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  iMousePos: TPoint;
  C: TDisplayCoord;
  X, Y: Integer;
  vCaret: TBufferCoord;
begin
  GetCursorPos( iMousePos );
  iMousePos := ScreenToClient( iMousePos );
  C := PixelsToRowColumn( iMousePos.X, iMousePos.Y );
  C.Row := MinMax(C.Row, 1, DisplayLineCount);

  if fScrollDeltaX <> 0 then
  begin
    LeftChar := LeftChar + fScrollDeltaX;
    X := LeftChar;
    if fScrollDeltaX > 0 then  // scrolling right?
      Inc(X, fCharsInWindow);
    C.Column := X;
  end;
  if fScrollDeltaY <> 0 then
  begin
    if GetKeyState(SYNEDIT_SHIFT) < 0 then
      TopLine := TopLine + fScrollDeltaY * LinesInWindow
    else
      TopLine := TopLine + fScrollDeltaY;
    Y := TopLine;
    if fScrollDeltaY > 0 then  // scrolling down?
      Inc(Y, LinesInWindow - 1);
    C.Row := MinMax(Y, 1, DisplayLineCount);
  end;

  vCaret := DisplayToBufferPos(C);
  if ((CaretX <> vCaret.Char) or (CaretY <> vCaret.Line)) then
  begin
    // changes to line / column in one go
    IncPaintLock;
    try
      if MouseCapture and (fClickCount = 2) and (ActiveSelectionMode = smNormal) then
        // Word selection
        DoMouseSelectWordRange(vCaret)
      else if MouseCapture and (fClickCount = 3) and (ActiveSelectionMode = smNormal) then
        // Line selection
        DoMouseSelectLineRange(vCaret)
      else begin
        InternalCaretXY := vCaret;
        // if MouseCapture is True we're changing selection. otherwise we're dragging
        if MouseCapture then
          SetBlockEnd(CaretXY);
      end;
    finally
      DecPaintLock;
    end;
  end;
  ComputeScroll(iMousePos.x, iMousePos.y);
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Var
//++ Code Folding
  ptLineCol: TBufferCoord;
  ptRowCol: TDisplayCoord;
  Index: Integer;
  Rect: TRect;
//-- Code Folding
begin
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
//++ Code Folding
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

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2: Integer;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;
  // lines
  nL1 := Max(TopLine + rcClip.Top div fTextHeight, TopLine);
  nL2 := MinMax(TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight,
    1, DisplayLineCount);
  // Now paint everything while the caret is hidden.
  HideCaret;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < fGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      PaintGutter(rcDraw, nL1, nL2);
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > fGutterWidth) then
    begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
      PaintTextLines(rcDraw, nL1, nL2);
    end;
    PluginsAfterPaint(Canvas, rcClip, nL1, nL2);
    // If there is a custom paint handler call it.
    DoOnPaint;
    DoOnPaintTransient(ttAfter);
  finally
    UpdateCaret;
  end;
end;

procedure TCustomSynEdit.PaintGutter(const AClip: TRect;
  const aFirstRow, aLastRow: Integer);
var
  I, L, W: Integer;
  rcBackGround: TRect;
  Band: TSynGutterBand;
  SaveIndex: Integer;
  rcBand: TRect;
  EdBkgrColor: TColor;
  Attri: TSynHighlighterAttributes;
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
    SynDrawGradient(Canvas, fGutter.GradientStartColor, fGutter.GradientEndColor,
      fGutter.GradientSteps, rcBackGround, True)
  else
  begin
    Canvas.Brush.Color := fGutter.Color;
    Canvas.FillRect(rcBackGround);
  end;

  // Set Brush to Editor Background
  EdBkgrColor := Color;
  if Highlighter <> nil then
  begin
    Highlighter.ResetRange;
    Attri := Highlighter.WhitespaceAttribute;
    if (Attri <> nil) and (Attri.Background <> clNone) then
      EdBkgrColor := Attri.Background;
  end;

  L := 0;
  for I := 0 to FGutter.Bands.Count - 1 do
  begin
    Band := FGutter.Bands[I];
    if not Band.Visible then Continue;
    W := Band.RealWidth;
    if (L > AClip.Right) or (L + W < AClip.Left) then Continue;

    rcBand := Rect(L, AClip.Top, L + W, AClip.Bottom);
    // Paint Bands with Editor Background
    if Band.Background = gbbEditor then
    begin
      Canvas.Brush.Color := EdBkgrColor;
      Canvas.FillRect(rcBand);
    end;
    //And now paint the bands
    SaveIndex := SaveDC(Canvas.Handle);
    try
      IntersectClipRect(Canvas.Handle,
        rcBand.Left, rcBand.Top, rcBand.Right, rcBand.Bottom);
      Band.PaintLines(Canvas, rcBand, aFirstRow, aLastRow);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
    Inc(L, W);
  end;
end;

procedure TCustomSynEdit.PaintTextLines(AClip: TRect;
  const aFirstRow, aLastRow: Integer);
var
  FRT: ID2D1DCRenderTarget;
  XLineOffset: Integer;
  LinesRect: TRect;

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
        fHighlighter.ResetRange;
      Attr := Highlighter.WhitespaceAttribute;
      if Attr <> nil then
        if Bkground and (Attr.Background <> clNone) then
          Result := Attr.Background
        else if not Bkground and (Attr.Foreground <> clNone) then
          Result := Attr.Foreground;
    end;
  end;

  function IsRowFullySelected(const Row, Line: Integer): Boolean;
  var
    BB, BE, BC: TBufferCoord;
    Len: Integer;
  begin
    BB := BlockBegin;
    BE := BlockEnd;
    Result :=
      (BB <> BE) and (not HideSelection or Self.Focused) and
      (fActiveSelectionMode <> smColumn);
    if not Result  then Exit;

    if WordWrap then
    begin
      BC := DisplayToBufferPos(DisplayCoord(1, Row));
      Len := fWordWrapPlugin.RowLength[Row];
      Result := ((BB <= BC) and
        ((BE > BufferCoord(BC.Char + Len, BC.Line)) or
         ((BufferCoord(BC.Char + Len, BC.Line) = BE) and
          (BC.Char > 1) and not fCaretAtEOL)));
    end
    else
      Result := (BB.Line < BE.Line) and
        ((InRange(Line, BB.Line + 1, BE.Line - 1)) or
        ((Line = BB.Line) and (BB.Char = 1)));
    Result := Result or
      ((fActiveSelectionMode = smLine) and InRange(Line, BB.Line, BE.Line));
  end;

  procedure FullRowColors(const Row, Line: Integer; var FullRowFG, FullRowBG: TColor);
  { clNone indicates normal processing of text foreground/background color }
  var
    IsLineSpecial: Boolean;
    IsFullySelected: Boolean;
    FG, BG: TColor;
  begin
    IsLineSpecial := DoOnSpecialLineColors(Line, FG, BG);
    IsFullySelected := IsRowFullySelected(Row, Line);
    if IsFullySelected and IsLineSpecial then
    begin
      // Invert the colors as in Delphi
      FullRowFG := BG;
      FullRowBG := FG;
    end
    else if IsLineSpecial then
    begin
      FullRowBG := BG;
      if eoSpecialLineDefaultFg in FOptions then
        FullRowFG := clNone
      else
        FullRowFG := FG;
    end
    else if IsFullySelected then
    begin
      FullRowFG := fSelectedColor.Foreground;
      FullRowBG := fSelectedColor.Background;
    end
    else if (CaretY = Line) and (ActiveLineColor <> clNone) then
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

  function PartialSelection(const Row, Line: Integer; var First, Last: Integer;
    var SelBG, SelFG: TColor): Boolean;
  var
    BC, BB, BE: TBufferCoord;
    Len: Integer;
    FG, BG: TColor;
  begin
    BB := BlockBegin;
    BE := BlockEnd;
    First := 0;
    Last := 0;
    Result :=
      (BB <> BE) and  (not HideSelection or Self.Focused) and
      (fActiveSelectionMode <> smLine) and
      not IsRowFullySelected(Row, Line);
    if not Result then Exit;

    if WordWrap then
    begin
      BC := DisplayToBufferPos(DisplayCoord(1, Row));
      Len := fWordWrapPlugin.RowLength[Row]
    end
    else
    begin
      BC := BufferCoord(1, Line);
      Len := Lines[Line-1].Length;
    end;

    if fActiveSelectionMode = smColumn then
      Result := (BB.Char <> BE.Char) and
        InRange(Line, BB.Line, BE.Line) and (BE >= BC) and
        (BB <= BufferCoord(BC.Char + Len, BC.Line))
    else
    begin
      Result :=
        (((Line = BB.Line) and InRange(BB.Char, BC.Char, BC.Char + Len))
        or ((Line = BE.Line) and InRange(BE.Char, BC.Char + 1, BC.Char + Len)));
    end;
    if not Result then Exit;

    if fActiveSelectionMode = smColumn then
    begin
      First := Min(BufferToDisplayPos(BB).Column, BufferToDisplayPos(BE).Column);
      Last := Max(BufferToDisplayPos(BB).Column, BufferToDisplayPos(BE).Column) - 1;
    end
    else if BB >= BC then
    begin
      First := BB.Char - BC.Char + 1;
      Last := IfThen((BE > BufferCoord(BC.Char + Len, BC.Line)) or
        (WordWrap and (BE = BufferCoord(BC.Char + Len, BC.Line)) and
        (RowtoLine(Row + 1) = Line) and not fCaretAtEOL), MaxInt, BE.Char - BC.Char);
    end
    else
    begin
      First := 1;
      Last := BE.Char - BC.Char;
    end;
    if DoOnSpecialLineColors(Line, FG, BG) then
    begin
      // Invert special colors as in Delphi
      SelBG := FG;
      SelFG := BG;
    end
    else
    begin
      SelBG := fSelectedColor.Background;
      SelFG := fSelectedColor.Foreground;
    end
  end;

  function YRowOffset(const Row: Integer): Integer;
  begin
    Result := (Row - fTopLine) * fTextHeight - AClip.Top;
  end;

  procedure PaintTokenBackground(Layout: TSynTextLayout;
    const Row , Start, Len: Integer; AColor: TColor);
  var
    X1, Y1, X2, Y2: Single;
    HitMetrics: TDwriteHitTestMetrics;
  begin
    Layout.IDW.HitTestTextPosition(Start - 1, False, X1, Y1, HitMetrics);
    Layout.IDW.HitTestTextPosition(Start + Len - 2, True, X2, Y2, HitMetrics);
    FRT.FillRectangle(
      Rect(XLineOffset + Round(X1), YRowOffset(Row),
           XLineOffset + Round(X2) + 1, YRowOffset(Row + 1)),
           TSynDWrite.SolidBrush(AColor));
  end;

  procedure DrawTab(Layout: TSynTextLayout; const Row, TabPos: Integer;
    TabColor: TColor);
  var
    TabLayout: TSynTextLayout;
    X1, Y1, X2, Y2: Single;
    HitMetrics: TDwriteHitTestMetrics;
  begin
    Layout.IDW.HitTestTextPosition(TabPos-1, False, X1, Y1, HitMetrics);
    Layout.IDW.HitTestTextPosition(TabPos-1, True, X2, Y2, HitMetrics);
    TabLayout.Create(FTextFormat, @SynTabGlyph, 1, Round(X2 - X1), fTextHeight);
    TabLayout.IDW.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
    TabLayout.SetFontColor(TabColor, 1, 1);
    TabLayout.Draw(FRT, XLineOffset + Round(X1), YRowOffset(Row), TabColor);
  end;

  procedure DrawIndentGuides;
  var
    TabSteps, LineIndent, NonBlankLine, X, Y, Row, Line: Integer;
    BMWidth: Integer;
    BitmapRT: ID2D1BitmapRenderTarget;
    BM: ID2D1Bitmap;
    RectF: TD2D1RectF;
  begin
    BMWidth := Round(1.5 * FCurrentPPI / 96) + 2;
    var Size := D2D1SizeF(BMWidth, FTextHeight);
    CheckOSError(FRT.CreateCompatibleRenderTarget(@Size, nil, nil,
      D2D1_COMPATIBLE_RENDER_TARGET_OPTIONS_GDI_COMPATIBLE,
      BitmapRT));
      BitmapRT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      BitmapRT.BeginDraw;
      BitmapRT.DrawLine(Point(1, 0), Point(1, fTextHeight),
        TSynDWrite.SolidBrush(fCodeFolding.IndentGuidesColor),
        1.5 * FCurrentPPI / 96, TSynDWrite.DottedStrokeStyle);
      BitmapRT.EndDraw;
      CheckOSError(BitmapRT.GetBitmap(BM));

//    FRT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
    for Row := aFirstRow to aLastRow do begin
      Line := RowToLine(Row);
      if (Line > Lines.Count) then Break;

      // If line is blank get next nonblank line
      NonBlankLine := Line;
      while (NonBlankLine <= fLines.Count) and
        (fLines[NonBlankLine - 1].TrimLeft = '')
      do
        Inc(NonBlankLine);
      LineIndent := LeftSpaces(fLines[NonBlankLine - 1], True);

      // Step horizontally
      Y := YRowOffset(Row);
      TabSteps := TabWidth;
      while TabSteps < LineIndent do
      begin
        X := TabSteps * CharWidth + XLineOffset;
        if X >= 0 then
        begin
          RectF := Rect(X-1, Y, X + BMWidth - 1, Y + fTextHeight);
          FRT.DrawBitmap(BM, @RectF);
//          FRT.DrawLine(Point(X, Y), Point(X, Y + fTextHeight),
//            TSynDWrite.SolidBrush(fCodeFolding.IndentGuidesColor),
//            FCurrentPPI / 96, nil);
//          FRT.DrawLine(Point(X, Y), Point(X, Y + fTextHeight),
//            TSynDWrite.SolidBrush(fCodeFolding.IndentGuidesColor),
//            1 * FCurrentPPI / 96, TSynDWrite.DottedStrokeStyle);
//          FRT.DrawLine(Point(X, Y), Point(X, Y + fTextHeight),
//            TSynDWrite.SolidBrush(fCodeFolding.IndentGuidesColor),
//            1 * FCurrentPPI / 96, nil);
        end;
        Inc(TabSteps, TabWidth);
      end;
    end;
//    FRT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
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
          FRT.DrawLine(Point(0, Y), Point(LinesRect.Width, Y),
            TSynDWrite.SolidBrush(fCodeFolding.CollapsedLineColor),
            MulDiv(1, FCurrentPPI, 96));
        end;
        end;
        if fCodeFolding.ShowHintMark then
        begin
          HintRect := GetCollapseMarkRect(Row, Line);
          HintRect.Offset(-(FGutterWidth + TextMargin), -AClip.Top);
          if HintRect.IntersectsWith(LinesRect) then
          begin
            FRT.DrawRectangle(HintRect,
              TSynDWrite.SolidBrush(fCodeFolding.CollapsedLineColor),
              FCurrentPPI/96);
            Layout.Create(FTextFormat, PChar(StringOfChar(SynSpaceGlyph, 3)), 3,
              HintRect.Width, HintRect.Height);
            Layout.IDW.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
            Layout.Draw(FRT, HintRect.Left, HintRect.Top,
              fCodeFolding.CollapsedLineColor);
          end;
        end;
    end;
  end;

var
  Line, Row, CharOffset, I: Integer;
  LayoutWidth: Integer;
  SLine, SRow: string;
  DoTabPainting: Boolean;
  MarginRect: TRect;
  Layout: TSynTextLayout;
  BGColor, FGColor, TabColor: TColor;
  Token: string;
  TokenPos: Integer;
  Attr: TSynHighlighterAttributes;
  AColor: TColor;
  REdgePos: Integer;
  FullRowFG, FullRowBG: TColor;
  SelFirst, SelLast: Integer;
  SelBG, SelFG: TColor;
  RangeCount: Cardinal;
  HMArr: array of TDwriteHitTestMetrics;
begin
  XLineOffset := - (FLeftChar - 1) * FCharWidth;

  // Fill TextMargin with backgournd color using GDI
  BGColor := WhitespaceColor(True, True);  // Resets highlighter
  Canvas.Brush.Color := BGColor;
  MarginRect := Rect(Max(FGutterWidth, AClip.Left), AClip.Top,
    Min(FGutterWidth + fTextMargin, AClip.Right), AClip.Bottom);
  if not MarginRect.IsEmpty then
    Canvas.FillRect(MarginRect);

  // Switch to DirectWrite - only paint within the clip region
  LinesRect := Rect(FGutterWidth + FTextMargin, AClip.Top, AClip.Right, AClip.Bottom);
  if LinesRect.IsEmpty then Exit;

  FRT := TSynDWrite.RenderTarget;
  FRT.BindDC(Canvas.Handle, LinesRect);
  FRT.BeginDraw;
  // Paint background
  // TODO: FLineHeight, FCharWidht
  LinesRect.Offset(-FGutterWidth - FTextMargin, -AClip.Top);
  FRT.FillRectangle(LinesRect, TSynDWrite.SolidBrush(BGColor));
  LayoutWidth := ClientWidth - fGutterWidth - fTextMargin - XLineOffset;

  for Row:= aFirstRow to aLastRow do
  begin
    Line := RowToLine(Row);
    SLine := Lines[Line -  1];

    // Get the row including control characters
    SRow := Rows[Row];
    CharOffset := DisplayToBufferPos(DisplayCoord(1, Row)).Char;

    DoTabPainting := False;
    if (eoShowSpecialChars in fOptions) then
    begin
      for I := 1 to SRow.Length do
        if SRow[I] = #32 then
          SRow[I] := SynSpaceGlyph
        else if SRow[I] = #9 then
          DoTabPainting := True;
      if (CharOffset + SRow.Length = SLine.Length + 1) and (Line < Lines.Count) then
        SRow := SRow + SynLineBreakGlyph;
    end;

    // Create the text layout
    Layout.Create(FTextFormat, PChar(SRow), SRow.Length, LayoutWidth, fTextHeight);
    if not (eoShowLigatures in FOptions) or (Line = CaretY) then
      // No ligatures for current line
      Layout.SetTypography(typNoLigatures, 1, SRow.Length);

    // Special colors, full line selection and ActiveLineColor
    FullRowColors(Row, Line, FullRowFG, FullRowBG);
    AColor := FullRowBG;
    if (AColor = clNone) and (WhitespaceColor <> BGColor) then
      AColor := WhiteSpaceColor; // Whitespace color may differ per line
    if AColor <> clNone then
      FRT.FillRectangle(Rect(0, YRowOffset(Row), LinesRect.Right,
        YRowOffset(Row + 1)), TSynDWrite.SolidBrush(FullRowBG));

    // Highlighted tokens
    if fHighlighter <> nil then
    begin
      if Line > 1 then
        fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Line - 2]);
      FHighlighter.SetLine(SLine, Line);

      while not FHighLighter.GetEol do
      begin
        TokenPos := FHighLighter.GetTokenPos - CharOffset + 2; //TokenPos is zero based
        if TokenPos > SRow.Length then Break;  // for WordWrap
        Token := FHighLighter.GetToken;
        if TokenPos + Token.Length <= 1 then
        begin
          fHighlighter.Next;
          Continue;  //for WordWrap
        end;
        Attr := FHighLighter.GetTokenAttribute;

        if (Token <> '') and Assigned(Attr) then
        begin
          Layout.SetFontStyle(Attr.Style, TokenPos, Token.Length);
          AColor := Attr.Foreground;
          if (FullRowFG = clNone) and (AColor <> clNone) then
            Layout.SetFontColor(AColor, TokenPos, Token.Length);
          AColor := Attr.Background;
          if (FullRowBG = clNone) and (AColor <> clNone) then
            PaintTokenBackground(Layout, Row, TokenPos, Token.Length, AColor);
        end;
        FHighLighter.Next;
      end;
      if (eoShowSpecialChars in fOptions) and (FullRowFG = clNone) and
        (CharOffset + SRow.Length = SLine.Length + 2) and
        Assigned(fHighlighter.WhitespaceAttribute) and
        (fHighlighter.WhitespaceAttribute.Foreground <> clNone)
      then
        Layout.SetFontColor(fHighlighter.WhitespaceAttribute.Foreground, SRow.Length, 1);
    end;

    // Paint selection if Line is partially selected - deals with bidi text
    if PartialSelection(Row, Line, SelFirst, SelLast, SelBG, SelFG) then
    begin
      Layout.IDW.HitTestTextRange(SelFirst - 1, SelLast -SelFirst + 1,
        XLineOffset, YRowOffset(Row), PDwriteHitTestMetrics(nil)^, 0, RangeCount);
      SetLength(HMArr, RangeCount);
      Layout.IDW.HitTestTextRange(SelFirst - 1, SelLast- SelFirst + 1,
        XLineOffset, YRowOffset(Row), HMArr[0], RangeCount, RangeCount);
      for I := 0 to RangeCount -1  do
      begin
        Layout.SetFontColor(SelFG, HMArr[I].textPosition + 1, HMArr[I].length);
        FRT.FillRectangle(Rect(Round(HMArr[I].left), YRowOffset(Row),
          IfThen(SelLast = MaxInt, LinesRect.Right, Round(HMArr[I].left + HMArr[I].width)),
          YRowOffset(Row + 1)), TSynDWrite.SolidBrush(SelBG));
      end;
    end;

    // Paint the layout
    if FullRowFG = clNone then
      FGColor := Font.Color
    else
      FGColor := FullRowFG;
    Layout.Draw(FRT, XLineOffset, YRowOffset(Row), FGColor);

    // Paint tab control characters
    if DoTabPainting then
    begin
      if FullRowFG <> clNone then
        TabColor := FullRowFG
      else
        TabColor := WhitespaceColor(False);
      for I := 1 to SRow.Length do
        if SRow[I] = #9 then
        begin
          if InRange(I, SelFirst, SelLast) then
            DrawTab(Layout, Row, I, SelFG)
          else
            DrawTab(Layout, Row, I, TabColor);
        end;
    end;

    //Draw indentation guides and code folding marks (PaintFoldAttributes)
    if CodeFolding.IndentGuides and not WordWrap then
      DrawIndentGuides;
    if UseCodeFolding then
      PaintFoldMarks;
  end;

  // Draw right edge
  if (fRightEdge > 0) then
  begin
    REdgePos := fRightEdge * fCharWidth; // pixel value
    if InRange(REdgePos + fTextOffset, AClip.Left, AClip.Right) then
      FRT.DrawLine(Point(XLineOffset + REdgePos, 0),
        Point(XLineOffset + REdgePos, AClip.Height),
        TSynDWrite.SolidBrush(fRightEdgeColor));
  end;
  if FRT.EndDraw <> S_OK then TSynDWrite.ResetRenderTarget;
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  PasteMode: TSynSelectionMode;
  Mem: HGLOBAL;
  P: PByte;
  ClipText, Spaces: string;
begin
  if not CanPaste then
    exit;
  DoOnPaintTransient(ttBefore);
  PasteMode := SelectionMode;
  // Check for our special format and read PasteMode.
  // The text is ignored as it is ANSI-only to stay compatible with programs
  // using the ANSI version of SynEdit.
  //
  // Instead we take the text stored in CF_UNICODETEXT or CF_TEXT.
  if Clipboard.HasFormat(SynEditClipboardFormat) then
  begin
    Clipboard.Open;
    try
      Mem := Clipboard.GetAsHandle(SynEditClipboardFormat);
      P := GlobalLock(Mem);
      try
        if P <> nil then
          PasteMode := PSynSelectionMode(P)^;
      finally
        GlobalUnlock(Mem);
      end
    finally
      Clipboard.Close;
    end;
  end;
  // SetSelTextPrimitiveEx Encloses the undo actions in Begin/EndUndoBlock
  ClipText := GetClipboardText;
  if eoTabsToSpaces in Options then
  begin
    Spaces := StringOfChar(#32, TabWidth);
    ClipText := StringReplace(ClipText, #9, Spaces, [rfReplaceAll]);
  end;
  SetSelTextPrimitiveEx(PasteMode, ClipText, True);

  EnsureCursorPosVisible;
  DoOnPaintTransient(ttAfter);
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

procedure TCustomSynEdit.SetBlockBegin(Value: TBufferCoord);
var
  nInval1, nInval2: Integer;
  SelChanged: Boolean;
begin
  ActiveSelectionMode := SelectionMode;
  Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  if (fActiveSelectionMode = smNormal) then
    if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
      Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
    else
      Value.Char := 1;
  if SelAvail then
  begin
    if fBlockBegin.Line < fBlockEnd.Line then
    begin
      nInval1 := Min(Value.Line, fBlockBegin.Line);
      nInval2 := Max(Value.Line, fBlockEnd.Line);
    end
    else
    begin
      nInval1 := Min(Value.Line, fBlockEnd.Line);
      nInval2 := Max(Value.Line, fBlockBegin.Line);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
    SelChanged := True;
  end
  else
  begin
    SelChanged :=
      (fBlockBegin.Char <> Value.Char) or (fBlockBegin.Line <> Value.Line) or
      (fBlockEnd.Char <> Value.Char) or (fBlockEnd.Line <> Value.Line);
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TBufferCoord);
var
  nLine: Integer;
begin
  ActiveSelectionMode := SelectionMode;
  if not (eoNoSelection in Options) then
  begin
    Value.Char := Max(Value.Char, 1);
    Value.Line := MinMax(Value.Line, 1, Lines.Count);
    if (fActiveSelectionMode = smNormal) then
      if (Value.Line >= 1) and (Value.Line <= Lines.Count) then
        Value.Char := Min(Value.Char, Length(Lines[Value.Line - 1]) + 1)
      else
        Value.Char := 1;
    if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
    begin
      if (Value.Char <> fBlockEnd.Char) or (Value.Line <> fBlockEnd.Line) then
      begin
        if (fActiveSelectionMode = smColumn) and (Value.Char <> fBlockEnd.Char) then
        begin
          InvalidateLines(
            Min(fBlockBegin.Line, Min(fBlockEnd.Line, Value.Line)),
            Max(fBlockBegin.Line, Max(fBlockEnd.Line, Value.Line)));
          fBlockEnd := Value;
        end
        else begin
          nLine := fBlockEnd.Line;
          fBlockEnd := Value;
          if (fActiveSelectionMode <> smColumn) or (fBlockBegin.Char <> fBlockEnd.Char) then
            InvalidateLines(nLine, fBlockEnd.Line);
        end;
        StatusChanged([scSelection]);
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretX(Value: Integer);
begin
  SetCaretXY(BufferCoord(Value, fCaretY));
end;

procedure TCustomSynEdit.SetCaretY(Value: Integer);
begin
  SetCaretXY(BufferCoord(fCaretX, Value));
end;

procedure TCustomSynEdit.InternalSetCaretX(Value: Integer);
begin
  InternalSetCaretXY(BufferCoord(Value, fCaretY));
end;

procedure TCustomSynEdit.InternalSetCaretY(Value: Integer);
begin
  InternalSetCaretXY(BufferCoord(fCaretX, Value));
end;

function TCustomSynEdit.GetCaretXY: TBufferCoord;
begin
  Result.Char := CaretX;
  Result.Line := CaretY;
end;

function TCustomSynEdit.GetDisplayX: Integer;
begin
  Result := DisplayXY.Column;
end;

function TCustomSynEdit.GetDisplayY: Integer;
begin
//++ CodeFolding
  if not WordWrap and not UseCodeFolding then
//-- CodeFolding
    Result := CaretY
  else
    Result := DisplayXY.Row;
end;

Function TCustomSynEdit.GetDisplayXY: TDisplayCoord;
begin
  Result := BufferToDisplayPos(CaretXY);
  if WordWrap and fCaretAtEOL then
  begin
    if Result.Column = 1 then
    begin
      Dec(Result.Row);
      Result.Column := fWordWrapPlugin.GetRowLength(Result.Row) +1;
    end
    else begin
      // Work-around situations where fCaretAtEOL should have been updated because of
      //text change (it's only valid when Column = 1). Updating it in ProperSetLine()
      //would probably be the right thing, but...
      fCaretAtEOL := False;
    end;
  end;
end;

procedure TCustomSynEdit.SetCaretXY(const Value: TBufferCoord);
//there are two setCaretXY methods.  One Internal, one External.  The published
//property CaretXY (re)sets the block as well
begin
  IncPaintLock;
  try
    Include(fStatusChanges, scSelection);
    SetCaretXYEx(True, Value);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.InternalSetCaretXY(const Value: TBufferCoord);
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
  vTriggerPaint: boolean;
  S, TS : string;
begin
  fCaretAtEOL := False;
  vTriggerPaint := HandleAllocated;
  if vTriggerPaint then
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
  if (Value.Line <> fCaretY) and (eoTrimTrailingSpaces in fOptions) and
     (fCaretY <= Lines.Count) and (fCaretY >= 1) then
  begin
    S := Lines[fCaretY-1];
    TS := S.TrimRight;
    if S <> TS then
      Lines[fCaretY-1] := TS;
  end;

  if (Value.Char <> fCaretX) or (Value.Line <> fCaretY) then
  begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if fCaretX <> Value.Char then
      begin
        fCaretX := Value.Char;
        Include(fStatusChanges, scCaretX);
      end;
      if fCaretY <> Value.Line then
      begin
        if (ActiveLineColor <> clNone) or (eoShowLigatures in fOptions) then
        begin
          InvalidateLine(Value.Line);
          InvalidateLine(fCaretY);
        end;
        fCaretY := Value.Line;
        Include(fStatusChanges, scCaretY);
//++ CodeFolding
        UncollapseAroundLine(fCaretY);
//-- CodeFolding
      end;
      // Call UpdateLastPosX before DecPaintLock because the event handler it
      // calls could raise an exception, and we don't want FLastPosX to be
      // left in an undefined state if that happens.
      UpdateLastPosX;
      if EnsureVisible then
        EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
//++ Flicker Reduction
//      Include(fStateFlags, sfScrollbarChanged);
//-- Flicker Reduction
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
  if vTriggerPaint then
    DoOnPaintTransient(ttAfter);
end;

function TCustomSynEdit.RowColumnInView(RowCol: TDisplayCoord): Boolean;
begin
  Result := InRange(RowCol.Row, TopLine, TopLine + LinesInWindow) and
    InRange(RowColumnToPixels(RowCol).X, FGutterWidth + TextMargin,
      ClientWidth - TextMargin -FCharWidth);
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
    fTextOffset := fGutterWidth + fTextMargin - (LeftChar - 1) * fCharWidth;
    if HandleAllocated then
    begin
      CalcTextAreaWidth;
      fCharsInWindow := Max(ClientWidth - fGutterWidth - fTextMargin, 0) div fCharWidth;
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
	// when wrapping with right edge and right edge is behind the window width
  if WordWrap and not ((eoWrapWithRightEdge in FOptions) and
    (WrapAreaWidth > FTextAreaWidth))
  then
    Value := 1;

  if eoScrollPastEol in Options then
      MaxVal := MaxInt - fCharsInWindow
  else
    // + 2 because we want to allow for an extra space at the end
    // and LeftChar 1 would mean that the char appears right at the edge
    MaxVal := Max(CeilOfIntDiv(Max(TSynEditStringList(Lines).MaxWidth -
              TextAreaWidth, 0),  FCharWidth)  + 2, 1);
  Value := MinMax(Value, 1, MaxVal);
  if Value <> fLeftChar then
  begin
    iDelta := fLeftChar - Value;
    fLeftChar := Value;
    fTextOffset := fGutterWidth + fTextMargin - (LeftChar - 1) * fCharWidth;
    if Abs(iDelta) * FCharWidth < ClientWidth - GutterWidth - TextMargin then
    begin
      iTextArea := ClientRect;
      Inc(iTextArea.Left, fGutterWidth + fTextMargin);
      ScrollWindow(Handle, iDelta * CharWidth, 0, @iTextArea, @iTextArea);
    end
    else
      InvalidateLines(-1, -1);
    UpdateScrollBars;
    StatusChanged([scLeftChar]);
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
  SetSelTextPrimitiveEx(fActiveSelectionMode, Value, True);
end;

// This is really a last minute change and I hope I did it right.
// Reason for this modification: next two lines will loose the CaretX position
// if eoScrollPastEol is not set in Options. That is not really a good idea
// as we would typically want the cursor to stay where it is.
// To fix this (in the absence of a better idea), I changed the code in
// DeleteSelection not to trim the string if eoScrollPastEol is not set.
procedure TCustomSynEdit.SetSelTextPrimitiveEx(PasteMode: TSynSelectionMode;
  Value: string; AddToUndoList: Boolean = True; SilentDelete: Boolean = False);
{
   Works in two stages:
     -  First deletes selection.
     -  Second inserts text taking into account PasteMode.
     -  SilentDelete does not restore selection on undo
     -  The routine takes full care of undo/redo
}
var
  BB, BE: TBufferCoord;
  TempString, SelectedText: string;

  procedure DeleteSelection;
  var
    I: Integer;
  begin
    case fActiveSelectionMode of
      smNormal:
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
      smColumn:
        begin
          // swap X if needed
          if BB.Char > BE.Char then
            SwapInt(Integer(BB.Char), Integer(BE.Char));
          // swap Y if needed
          if BB.Line > BE.Line then
            SwapInt(Integer(BB.Line), Integer(BE.Line));

          for I := BB.Line - 1 to BE.Line - 1 do
          begin
            TempString := Lines[I];
            Delete(TempString, BB.Char, BE.Char - BB.Char);
            ProperSetLine(I, TempString);
          end;
          // Lines never get deleted completely, so keep caret on the begin.
          CaretXY := BB;
          // Column deletion never removes a line entirely, so no mark
          // updating is needed here.
        end;
      smLine:
        begin
          if BE.Line = Lines.Count then
          begin
            Lines[BE.Line - 1] := '';
            TSynEditStringList(Lines).DeleteLines(BB.Line, BE.Line - BB.Line);
          end
          else
            TSynEditStringList(Lines).DeleteLines(BB.Line - 1, BE.Line - BB.Line + 1);
          // smLine deletion always resets to first column.
          CaretXY := BufferCoord(1, BB.Line);
        end;
    end;
  end;

  procedure InsertText;

    procedure InsertNormal;
    var
      sLeftSide: string;
      sRightSide: string;
      NewLines: TArray<string>;
      LineCount: Integer;
      I: Integer;
    begin
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
        Inc(fCaretY, LineCount - 1);
        Include(fStatusChanges, scCaretY);
      end;

      if eoTrimTrailingSpaces in Options then
        fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide.TrimRight)
      else
        fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide);
      StatusChanged([scCaretX]);
    end;

    procedure InsertColumn;
    var
      Str: string;
      Start: PWideChar;
      P: PWideChar;
      Len: Integer;
      InsertPos: Integer;
    begin
      // Insert string at current position
      InsertPos := CaretX;
      Start := PWideChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then
        begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], (P - Start) * sizeof(WideChar));
          if CaretY > Lines.Count then
          begin
            TempString := StringofChar(#32, InsertPos - 1) + Str;
            Lines.Add('');
          end
          else begin
            TempString := Lines[CaretY - 1];
            Len := Length(TempString);
            if Len < InsertPos then
            begin
              TempString :=
                TempString + StringofChar(#32, InsertPos - Len - 1) + Str
            end
            else
                Insert(Str, TempString, InsertPos);
          end;
          ProperSetLine(CaretY - 1, TempString);
        end;
        if P^ = #13 then
        begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
          Inc(fCaretY);
          Include(fStatusChanges, scCaretY);
        end;
        Start := P;
      until P^ = #0;
      Inc(fCaretX, Length(Str));
      Include(fStatusChanges, scCaretX);
    end;

    procedure InsertLine;
    var
      Str: string;
      NewLines: TArray<string>;
      LineCount: Integer;
      I: Integer;
      Index: Integer;
    begin
      NewLines := StringToLines(Value);
      LineCount := Length(NewLines);

      if LineCount = 0 then Exit;

      if LineCount = 1 then
      { When triggered from copy/past in smLineMode Value contains at least
        one LB and theLineCount would be at least 2.  This is the case
        that occurs when text is selected in line mode and say you type
        a single character}
      begin
          Str := NewLines[0];
         if eoTrimTrailingSpaces in Options then
           Str := Str.TrimRight;
         if (CaretY <= Lines.Count) then
         begin
           Str := NewLines[0] + Lines[CaretY - 1];
           Lines[CaretY - 1] := Str;
         end
         else
           Lines.Add(Str);
         fCaretX := 1 + Length(Str);
      end
      else
      begin
        fCaretX := 1;
        // Remove last empty line
        if NewLines[LineCount - 1]  = '' then
        begin
          Delete(NewLines, LineCount - 1, 1);
          Dec(LineCount);
        end;
        if eoTrimTrailingSpaces in Options then
          for I := 0 to LineCount - 1 do
            NewLines[I] := NewLines[I].TrimRight;
        Index := fCaretY - 1;  // zero based
        if not InsertMode then
        begin
          // Delete Lines that will be overwritten
          TSynEditStringList(Lines).DeleteLines(Index,
            Min(LineCount, Lines.Count - Index));
        end;
        // Now add the lines
        TSynEditStringList(Lines).InsertStrings(Index, NewLines);
        Inc(fCaretY, LineCount);
        Include(fStatusChanges, scCaretY);
      end;
      StatusChanged([scCaretX]);
    end;

  begin
    if Value = '' then
      Exit;

    case PasteMode of
      smNormal:
        InsertNormal;
      smColumn:
        InsertColumn;
      smLine:
        InsertLine;
    end;
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
    SelectedText := SelText;

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

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  Lines.Text := Value;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
begin
  if (eoScrollPastEof in Options) then
    Value := Min(Value, DisplayLineCount)
  else
    Value := Min(Value, DisplayLineCount - fLinesInWindow + 1);
  Value := Max(Value, 1);
  if Value <> TopLine then
  begin
    Delta := TopLine - Value;
    fTopLine := Value;
    if Abs(Delta) < fLinesInWindow then
      ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil)
    else
      Invalidate;

    UpdateScrollBars;
    StatusChanged([scTopLine]);
  end;
end;

//++ CodeFolding
procedure TCustomSynEdit.SetUseCodeFolding(const Value: Boolean);
Var
  ValidValue : Boolean;
begin
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
      FullFoldScan;
    end;
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
  if sfOleDragSource in fStateFlags then //internal dragging
    ComputeCaret(FMouseDownX, FMouseDownY);
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
          InternalCaretXY := vNewCaret;
          BlockBegin := vNewCaret;
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
// Todo: fix for DWrite

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
  if eoShowSpecialChars in fOptions then
    Inc(Result.Left, fCharWidth);

  // Deal wwth horizontal Scroll
  //Result.Left := Max(Result.Left, fGutterWidth + fCharWidth);

  Result.Right := Result.Left + fCharWidth * 3 +  4 * (fCharWidth div 7);
end;
//-- CodeFolding

procedure TCustomSynEdit.ShowCaret;
begin
  if not (eoNoCaret in Options) and not (sfCaretVisible in fStateFlags) then
  begin
    if Winapi.Windows.ShowCaret(Handle) then
      Include(fStateFlags, sfCaretVisible);
  end;
end;

procedure TCustomSynEdit.UpdateCaret;
var
  CX, CY: Integer;
  iClientRect: TRect;
  vCaretDisplay: TDisplayCoord;
  vCaretPix: TPoint;
  cf: TCompositionForm;
begin
  if (PaintLock <> 0) or not (Focused or FAlwaysShowCaret) then
    Include(fStateFlags, sfCaretChanged)
  else
  begin
    Exclude(fStateFlags, sfCaretChanged);
    vCaretDisplay := DisplayXY;
    if WordWrap and (vCaretDisplay.Column > fCharsInWindow + 1) then
      vCaretDisplay.Column := fCharsInWindow + 1;
    vCaretPix := RowColumnToPixels(vCaretDisplay);
    CX := vCaretPix.X + FCaretOffset.X;
    CY := vCaretPix.Y + FCaretOffset.Y;
    iClientRect := GetClientRect;
    Inc(iClientRect.Left, fGutterWidth);
    if (CX >= iClientRect.Left) and (CX < iClientRect.Right)
      and (CY >= iClientRect.Top) and (CY < iClientRect.Bottom) then
    begin
      SetCaretPos(CX, CY);
      ShowCaret;
    end
    else
    begin
      SetCaretPos(CX, CY);
      HideCaret;
    end;
    cf.dwStyle := CFS_POINT;
    cf.ptCurrentPos := Point(CX, CY);
    ImmSetCompositionWindow(ImmGetContext(Handle), @cf);
  end;
end;

procedure TCustomSynEdit.UpdateScrollBars;
begin
  if not HandleAllocated or (PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else begin
    Exclude(fStateFlags, sfScrollbarChanged);
    if FSynEditScrollBars.UpdateScrollBars then
      Update;
  end;
end;

function TCustomSynEdit.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  WHEEL_DIVISOR = 120; // Mouse Wheel standard
var
  iWheelClicks: Integer;
  iLinesToScroll: Integer;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then
    Exit;
  if GetKeyState(SYNEDIT_CONTROL) < 0 then
    iLinesToScroll := LinesInWindow shr Ord(eoHalfPageScroll in fOptions)
  else
    iLinesToScroll := 3;
  Inc(fMouseWheelAccumulator, WheelDelta);
  iWheelClicks := fMouseWheelAccumulator div WHEEL_DIVISOR;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DIVISOR;
  TopLine := TopLine - iWheelClicks * iLinesToScroll;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
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
var
  ScrollInfo: TScrollInfo;
begin
  Msg.Result := 0;
  case Msg.ScrollCode of
      // Scrolls to start / end of the line
    SB_LEFT: LeftChar := 1;
    SB_RIGHT:
      // Simply set LeftChar property to MaxWidth/FCharWidth
      // it would do the range checking and constrain the value if necessary
      LeftChar := CeilOfIntDiv(TSynEditStringList(Lines).MaxWidth, FCharWidth);
      // Scrolls one char left / right
    SB_LINERIGHT: LeftChar := LeftChar + 1;
    SB_LINELEFT: LeftChar := LeftChar - 1;
      // Scrolls one page of chars left / right
    SB_PAGERIGHT: LeftChar := LeftChar
      + (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGELEFT: LeftChar := LeftChar
      - (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
    begin
      FIsScrolling := True;
      ScrollInfo := FSynEditScrollBars.GetHorzScrollInfo;
      LeftChar := CeilOfIntDiv(ScrollInfo.nTrackPos, FCharWidth);
    end;
    SB_ENDSCROLL: FIsScrolling := False;
  end;
  if Assigned(OnScroll) then OnScroll(Self,sbHorizontal);
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

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  CommandProcessor(ecLostFocus, #0, nil);
  //Added check for focused to prevent caret disappearing problem
  if Focused or FAlwaysShowCaret then
    exit;
  HideCaret;
  Winapi.Windows.DestroyCaret;
  if FHideSelection and SelAvail then
    InvalidateSelection;
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
  if FHideSelection and SelAvail then
    InvalidateSelection;
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
end;

procedure TCustomSynEdit.WMUndo(var Msg: TMessage);
begin
  Undo;
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then
    ScrollHintWnd := HintWindowClass.Create(Application);
  Result := ScrollHintWnd;
end;

procedure TCustomSynEdit.WMVScroll(var Msg: TWMScroll);
var
  s: string;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
  ButtonH: Integer;
  ScrollInfo: TScrollInfo;
begin
  Msg.Result := 0;
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := DisplayLineCount;
      // Scrolls one line up / down
    SB_LINEDOWN: TopLine := TopLine + 1;
    SB_LINEUP: TopLine := TopLine - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopLine := TopLine
      + (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: TopLine := TopLine
      - (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FIsScrolling := True;
        ScrollInfo := FSynEditScrollBars.GetVertScrollInfo;
        TopLine := ScrollInfo.nTrackPos;
        if eoShowScrollHint in fOptions then
        begin
          ScrollHint := GetScrollHint;
          ScrollHint.Color := fScrollHintColor;
          case FScrollHintFormat of
            shfTopLineOnly:
              s := Format(SYNS_ScrollInfoFmtTop, [RowToLine(TopLine)]);
            else
              s := Format(SYNS_ScrollInfoFmt, [RowToLine(TopLine),
                RowToLine(TopLine + Min(LinesInWindow, DisplayLineCount-TopLine))]);
          end;

          rc := ScrollHint.CalcHintRect(200, s, nil);
          if eoScrollHintFollows in fOptions then
          begin
            ButtonH := GetSystemMetrics(SM_CYVSCROLL);
            pt := ClientToScreen(Point(ClientWidth - rc.Right - 4,
              ((rc.Bottom - rc.Top) shr 1) +                                    //half the size of the hint window
              Round((ScrollInfo.nTrackPos / ScrollInfo.nMax) *                  //The percentage of the page that has been scrolled
                    (ClientHeight - (ButtonH * 2)))                             //The height minus the arrow buttons
                   + ButtonH));                                                 //The height of the top button
          end
          else
            pt := ClientToScreen(Point(ClientWidth - rc.Right - 4, 10));

          OffsetRect(rc, pt.x, pt.y);
          ScrollHint.ActivateHint(rc, s);
          ScrollHint.Update;
        end;
      end;
      // Ends scrolling
    SB_ENDSCROLL:
      begin
        FIsScrolling := False;
      if eoShowScrollHint in fOptions then
        ShowWindow(GetScrollHint.Handle, SW_HIDE);
  end;
  end;
  Update;
  if Assigned(OnScroll) then OnScroll(Self,sbVertical);
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
  if WordWrap then
    fWordWrapPlugin.Reset;

//++ CodeFolding
  if UseCodeFolding then
    AllFoldRanges.Reset;
//-- CodeFolding

  ClearUndo;
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TRect), 0);
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
//++ CodeFolding
Var
  vLastScan: Integer;
begin
  if WordWrap then
    fWordWrapPlugin.LinesDeleted(aIndex, aCount);

  DoLinesDeleted(aIndex, aCount);

  vLastScan := aIndex;
  if Assigned(fHighlighter) and (Lines.Count > 0) then
    vLastScan := ScanFrom(aIndex);

  if UseCodeFolding then begin
    AllFoldRanges.LinesDeleted(aIndex, aCount);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(aIndex, vLastScan);
    InvalidateGutter;
  end;
//-- CodeFolding

  InvalidateLines(aIndex + 1, MaxInt);
  InvalidateGutterLines(aIndex + 1, MaxInt);
//++ Flicker Reduction
  Include(fStateFlags, sfScrollbarChanged);
//-- Flicker Reduction
end;

procedure TCustomSynEdit.ListInserted(Sender: TObject; Index: Integer;
  aCount: Integer);
var
  vLastScan: Integer;
//++ CodeFolding
  FoldIndex: Integer;
begin
  if WordWrap then
    fWordWrapPlugin.LinesInserted(Index, aCount);

  DoLinesInserted(Index, aCount);

  vLastScan := Index;
//-- CodeFolding
  if Assigned(fHighlighter) and (Lines.Count > 0) then
  begin
    repeat
      vLastScan := ScanFrom(vLastScan);
      Inc(vLastScan);
    until vLastScan >= Index + aCount;
  end;

//++ CodeFolding
  if UseCodeFolding then begin
    if fAllFoldRanges.CollapsedFoldStartAtLine(Index, FoldIndex) then
      // insertion starts at collapsed fold
      Uncollapse(FoldIndex);
    AllFoldRanges.LinesInserted(Index, aCount);
    // Scan the same lines the highlighter has scanned
    ReScanForFoldRanges(Index, vLastScan-1);
  end;
//-- CodeFolding

  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);
//++ Flicker Reduction
  Include(fStateFlags, sfScrollbarChanged);
//-- Flicker Reduction
end;

procedure TCustomSynEdit.ListPut(Sender: TObject; Index: Integer;
  const OldLine: string);
var
  vEndLine: Integer;
//++ CodeFolding
  vLastScan: Integer;
  FoldIndex: Integer;
//-- CodeFolding
begin
  DoLinePut(Index, OldLine);

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
  vBlockBegin: TBufferCoord;
  vBlockEnd: TBufferCoord;
  TempString: string;

  procedure CharScan;
  var
    cRun: Integer;
  begin
    { search BlockEnd }
    vBlockEnd.Char := Length(TempString);
    for cRun := Value.Char to Length(TempString) do
      if not IsIdentChar(TempString[cRun]) then
      begin
        vBlockEnd.Char := cRun;
        Break;
      end;
    { search BlockBegin }
    vBlockBegin.Char := 1;
    for cRun := Value.Char - 1 downto 1 do
      if not IsIdentChar(TempString[cRun]) then
      begin
        vBlockBegin.Char := cRun + 1;
        Break;
      end;
  end;

begin
  Value.Char := Max(Value.Char, 1);
  Value.Line := MinMax(Value.Line, 1, Lines.Count);
  TempString := Lines[Value.Line - 1] + #0; //needed for CaretX = LineLength + 1
  if Value.Char > Length(TempString) then
  begin
    InternalCaretXY := BufferCoord(Length(TempString), Value.Line);
    exit;
  end;

  CharScan;

  vBlockBegin.Line := Value.Line;
  vBlockEnd.Line := Value.Line;
  SetCaretAndSelection(vBlockEnd, vBlockBegin, vBlockEnd);
  InvalidateLine(Value.Line);
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

  DoOnPaintTransientEx(ttBefore,true);
  IncPaintLock;
  Lines.BeginUpdate;
  try
    FUndoRedo.Redo(Self);
  finally
    Lines.EndUpdate;
    DecPaintLock;
    DoOnPaintTransientEx(ttAfter,true);
  end;
end;

//++ CodeFolding
procedure TCustomSynEdit.Collapse(FoldRangeIndex: Integer; Invalidate:Boolean);
begin
  AllFoldRanges.Ranges.List[FoldRangeIndex].Collapsed := True;

  with AllFoldRanges.Ranges[FoldRangeIndex] do
  begin
    // Extract caret from fold
    if (fCaretY > FromLine) and (fCaretY <= ToLine) then
      CaretXY := BufferCoord(Length(Lines[FromLine - 1]) + 1, FromLine);

    if Invalidate then begin
      // Redraw the collapsed line and below
      InvalidateLines(FromLine, MaxInt);

      // Redraw fold mark
      InvalidateGutterLines(FromLine, MaxInt);

      UpdateScrollBars;
    end else
      // Update Scrollbars
      Include(fStateFlags, sfScrollbarChanged);
  end;
end;

procedure TCustomSynEdit.CollapseAll;
var
  i: Integer;
begin
  if not fUseCodeFolding then Exit;
    for i := fAllFoldRanges.Count - 1 downto 0 do
      Collapse(i, False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;


procedure TCustomSynEdit.CollapseLevel(Level: integer);
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsAtLevel(Level);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Collapse(RangeIndices[i], False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.CollapseNearest;
Var
  Index : integer;
begin
  if not fUseCodeFolding then Exit;
  if AllFoldRanges.FoldAroundLineEx(CaretY, False, True, True, Index) then
    Collapse(Index);

    EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.CollapseFoldType(FoldType : Integer);
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsOfType(FoldType);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Collapse(RangeIndices[i],False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.Uncollapse(FoldRangeIndex: Integer; Invalidate:Boolean);
begin
    AllFoldRanges.Ranges.List[FoldRangeIndex].Collapsed := False;

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
  while AllFoldRanges.FoldHidesLine(line, Index) do
    Uncollapse(Index);
end;

procedure TCustomSynEdit.UnCollapseLevel(Level: integer);
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsAtLevel(Level);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Uncollapse(RangeIndices[i], False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
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
Var
  i : integer;
  RangeIndices : TArray<Integer>;
begin
  if not fUseCodeFolding then Exit;
  RangeIndices := AllFoldRanges.FoldsOfType(FoldType);
  for i := Low(RangeIndices) to High(RangeIndices) do
    Uncollapse(RangeIndices[i], False);

  InvalidateLines(-1, -1);
  InvalidateGutterLines(-1, -1);

  EnsureCursorPosVisible;
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
  if NewPos.Line < BB.Line then
  begin
    BB := BufferCoord(1, NewPos.Line);
    NewPos := BB;
  end
  else if NewPos.Line >= BE.Line then
  begin
    if NewPos.Line < Lines.Count then
      BE := BufferCoord(1, NewPos.Line + 1)
    else
      BE := BufferCoord(Length(Lines[NewPos.Line - 1]), NewPos.Line);
    NewPos := BE;
  end
  else
    NewPos := BE;
  InternalCaretXY := NewPos;
  if BB <> fBlockBegin then
    BlockBegin := BB;
  if BE <> fBlockEnd then
    BlockEnd := BE;
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

  if BB <> fBlockBegin then
    BlockBegin := BB;
  if BE <> fBlockEnd then
    BlockEnd := BE;
  InternalCaretXY := fBlockEnd;
end;

procedure TCustomSynEdit.ExecCmdCopyOrMoveLine(const Command: TSynEditorCommand);
var
  vCaretRow, SelShift: Integer;
  Caret, BB, BE: TBufferCoord;
  StartOfBlock, EndOfBlock: TBufferCoord;
  Text: string;
  OldSelectionMode: TSynSelectionMode;
begin
  if not ReadOnly and
    ((Command <> ecMoveLineUp) or (BlockBegin.Line > 1)) and
    ((Command <> ecMoveLineDown) or (BlockEnd.Line < Lines.Count)) then
  begin
    // Get Caret and selection
    Caret := CaretXY;
    StartOfBlock := fBlockBegin;
    EndOfBlock := fBlockEnd;
    SelShift := Succ(Abs(fBlockEnd.Line - fBlockBegin.Line));

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
        if (fBlockBegin.Line <> fBlockEnd.Line) and (BlockEnd.Char = 1) then
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 2])), BlockEnd.Line - 1)
        else
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 1])), BlockEnd.Line);
        SelShift := -1;
      end;
      ecCopyLineDown:
      begin
        if (fBlockBegin.Line <> fBlockEnd.Line) and (BlockEnd.Char = 1) then begin
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 2])), BlockEnd.Line - 1);
          Dec(SelShift);
        end else
          BE := BufferCoord(Succ(Length(Lines[BlockEnd.Line - 1])), BlockEnd.Line);
        BB := BE;
      end;
      ecMoveLineDown:
      begin
        BB := BufferCoord(1, BlockBegin.Line);
        if (fBlockBegin.Line <> fBlockEnd.Line) and (BlockEnd.Char = 1) then
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
        (fBlockBegin.Line <> fBlockEnd.Line) and (BlockEnd.Char = 1)
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

    // Deal with Selection modes
    OldSelectionMode := ActiveSelectionMode;
    ActiveSelectionMode := smNormal;
    // group undo redo actions and reduce transient painting
    DoOnPaintTransientEx(ttBefore, true);
    BeginUndoBlock;
    try
      // Insert/replace text at selection BB-BE
      SetCaretAndSelection(BB, BB, BE);
      SetSelText(Text);

      // Set as new selection the shifted old one
      if SelShift <> 0 then
      begin
        Inc(Caret.Line, SelShift);
        Inc(StartOfBlock.Line, SelShift);
        Inc(EndOfBlock.Line, SelShift);
      end;
      SetCaretAndSelection(Caret, StartOfBlock, EndOfBlock);
    finally
      EndUndoBlock;
      DoOnPaintTransientEx(ttAfter, true);
    end;
    // Restore Selection mode
    ActiveSelectionMode := OldSelectionMode;
  end;
end;

procedure TCustomSynEdit.ExecCmdDeleteLine;
var
  OldSelectionMode: TSynSelectionMode;
begin
  if not ReadOnly and (Lines.Count > 0) and not
    ((BlockBegin.Line = Lines.Count) and (Length(Lines[BlockBegin.Line - 1]) = 0)) then
  begin
    DoOnPaintTransient(ttBefore);
    // Deal with Selection modes
    OldSelectionMode := ActiveSelectionMode;
    ActiveSelectionMode := smNormal;
    BeginUndoBlock;
    try
      // Nomalize selection
      if fBlockBegin > fBlockEnd then
        SetCaretAndSelection(BlockBegin, BlockBegin, BlockEnd);
      fBlockBegin.Char := 1;
      if (fBlockBegin.Line = fBlockEnd.Line) or (fBlockEnd.Char > 1) then
      begin
        if fBlockEnd.Line = Lines.Count then
          fBlockEnd.Char := Length(Lines[fBlockEnd.Line - 1]) + 1
        else
          fBlockEnd := BufferCoord(1, Succ(fBlockEnd.Line));
      end;
      SetSelText('');
      SetCaretAndSelection(fBlockBegin, fBlockBegin, fBlockBegin);
    finally
      EndUndoBlock;
      // Restore Selection mode
      ActiveSelectionMode := OldSelectionMode;
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

  EnsureCursorPosVisible;
end;

//-- CodeFolding

procedure TCustomSynEdit.Undo;
begin
  if ReadOnly then
    exit;

  DoOnPaintTransientEx(ttBefore,true);
  IncPaintLock;
  Lines.BeginUpdate;
  try
    FUndoRedo.Undo(Self);
  finally
    Lines.EndUpdate;
    DecPaintLock;
    DoOnPaintTransientEx(ttAfter,true);
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
    SetCaretXYEx(False, iNewPos);
    EnsureCursorPosVisibleEx(True);
    if SelAvail then
      InvalidateSelection;
    fBlockBegin.Char := fCaretX;
    fBlockBegin.Line := fCaretY;
    fBlockEnd := fBlockBegin;
  end;
end;

procedure TCustomSynEdit.GotoLineAndCenter(ALine: Integer);
begin
  SetCaretXYEx( False, BufferCoord(1, ALine) );
  if SelAvail then
    InvalidateSelection;
  fBlockBegin.Char := fCaretX;
  fBlockBegin.Line := fCaretY;
  fBlockEnd := fBlockBegin;
  EnsureCursorPosVisibleEx(True);
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
  // Prevent Alt-Backspace from beeping
  if (Msg.Msg = WM_SYSCHAR) and (Msg.wParam = VK_BACK) and
    (Msg.lParam and ALT_KEY_DOWN <> 0)
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
  fExtraLineSpacing := MulDiv(fExtraLineSpacing, M, D);
  fTextMargin := MulDiv(fTextMargin, M, D);
  fGutter.ChangeScale(M,D);
  fBookMarkOpt.ChangeScale(M, D);
  fWordWrapGlyph.ChangeScale(M, D);
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
//++ CodeFolding
Var
  OldUseCodeFolding : Boolean;
//-- CodeFolding
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

//++ CodeFolding
    //  Disable Code Folding if not supported by highlighter
    OldUseCodeFolding := fUseCodeFolding;
    UseCodeFolding := False;
    UseCodeFolding := OldUseCodeFolding;
    if fHighlighter is TSynCustomCodeFoldingHighlighter then
      TSynCustomCodeFoldingHighlighter(fHighlighter).InitFoldRanges(fAllFoldRanges);
//-- CodeFolding
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
  cw, ch: Integer;
begin
  // CreateCaret automatically destroys the previous one, so we don't have to
  // worry about cleaning up the old one here with DestroyCaret.
  // Ideally, we will have properties that control what these two carets look like.
  if InsertMode then
    ct := FInsertCaret
  else
    ct := FOverwriteCaret;
  case ct of
    ctHorizontalLine:
      begin
        cw := fCharWidth;
        ch := 2;
        FCaretOffset := Point(0, fTextHeight - 2);
      end;
    ctHalfBlock:
      begin
        cw := fCharWidth;
        ch := (fTextHeight - 2) div 2;
        FCaretOffset := Point(0, ch);
      end;
    ctBlock:
      begin
        cw := fCharWidth;
        ch := fTextHeight - 2;
        FCaretOffset := Point(0, 0);
      end;
    else
    begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := Point(-1, 0);
    end;
  end;
  Exclude(fStateFlags, sfCaretVisible);

  if Focused or FAlwaysShowCaret then
  begin
    CreateCaret(Handle, 0, cw, ch);
    UpdateCaret;
  end;
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

procedure TCustomSynEdit.EnsureCursorPosVisible;
begin
  EnsureCursorPosVisibleEx(False);
end;

procedure TCustomSynEdit.EnsureCursorPosVisibleEx(ForceToMiddle: Boolean;
  EvenIfVisible: Boolean = False);
var
  RowCol: TDisplayCoord;
  WidthToX: Integer;
  TmpMiddle: Integer;
  VisibleX: Integer;
  vCaretRow: Integer;
begin
  HandleNeeded;
  IncPaintLock;
  try
    RowCol := DisplayXY;
    vCaretRow := RowCol.Row;
    VisibleX := RowCol.Column;

    // Make sure X is visible
    WidthToX := TextWidth(Copy(Rows[vCaretRow], 1, VisibleX - 1));
    if WidthToX < (FLeftChar - 1) * FCharWidth then
      LeftChar := Max(WidthToX div fCharWidth, 1)
    else if WidthToX >= FTextAreaWidth + (LeftChar - 1) * FCharWidth then
      LeftChar := CeilofIntDiv(WidthToX - FTextAreaWidth, FCharWidth) + 2
    else
      LeftChar := LeftChar;

    // Make sure Y is visible
    if ForceToMiddle then
    begin
      if vCaretRow < (TopLine - 1) then
      begin
        TmpMiddle := LinesInWindow div 2;
        if vCaretRow - TmpMiddle < 0 then
          TopLine := 1
        else
          TopLine := vCaretRow - TmpMiddle + 1;
      end
      else if vCaretRow > (TopLine + (LinesInWindow - 2)) then
      begin
        TmpMiddle := LinesInWindow div 2;
        TopLine := vCaretRow - (LinesInWindow - 1) + TmpMiddle;
      end
     { Forces to middle even if visible in viewport }
      else if EvenIfVisible then
      begin
        TmpMiddle := fLinesInWindow div 2;
        TopLine := vCaretRow - TmpMiddle + 1;
      end;
    end
    else begin
      if vCaretRow < TopLine then
        TopLine := vCaretRow
      else if vCaretRow > TopLine + Max(1, LinesInWindow) - 1 then
        TopLine := vCaretRow - (LinesInWindow - 1)
      else
        TopLine := TopLine;
    end;
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
var
  Layout: TSynTextLayout;
begin
  if S = '' then Exit(0);

  Layout.Create(FTextFormat, PChar(S), S.Length, MaxInt, Maxint);
  Result := Round(Layout.TextMetrics.widthIncludingTrailingWhitespace);
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
begin
  // first the program event handler gets a chance to process the command
  fUndoRedo.CommandProcessed := Command;
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then
  begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(False, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserFirst) then
      ExecuteCommand(Command, AChar, Data);
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
  StartOfBlock: TBufferCoord;
  bChangeScroll: Boolean;
  moveBkm: Boolean;
  WP: TBufferCoord;
  Caret: TBufferCoord;
  CaretNew: TBufferCoord;
  CaretXNew: Integer;
  counter: Integer;
  vCaretRow: Integer;
  s: string;
  SaveLastPosX: Integer;

begin
  IncPaintLock;
  try
    case Command of
// horizontal caret movement or selection
      ecLeft, ecSelLeft:
        MoveCaretHorz(-1, Command = ecSelLeft);
      ecRight, ecSelRight:
        MoveCaretHorz(1, Command = ecSelRight);
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
      ecUp, ecSelUp:
        begin
          { on the first line we select first line too }
          if ((not Wordwrap and (CaretY = 1)) or
              (WordWrap and (DisplayY = 1))) then
          begin
            SaveLastPosX := FLastPosX;
            DoHomeKey(Command = ecSelUp);
            FLastPosX := SaveLastPosX;
          end
          else
            MoveCaretVert(-1, Command = ecSelUp);
          Update;
        end;
      ecDown, ecSelDown:
        begin
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
          Update;
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown:
        begin
          counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          if (Command in [ecPageUp, ecSelPageUp]) then
            counter := -counter;
          TopLine := TopLine + counter;
          { on the first line we will select first line too }
          if (Command in [ecPageUp, ecSelPageUp]) and
             ((not Wordwrap and (CaretY = 1)) or
              (WordWrap and (DisplayY = 1))) then
          begin
            SaveLastPosX := FLastPosX;
            DoHomeKey(Command = ecSelPageUp);
            FLastPosX := SaveLastPosX;
          end
          else
          { on the last line we will select last line too }
          if (Command in [ecPageDown, ecSelPageDown]) and
             ((not Wordwrap and (CaretY = Lines.Count)) or
              (WordWrap and (DisplayY = fWordWrapPlugin.RowCount))) then
          begin
            SaveLastPosX := FLastPosX;
            DoEndKey(Command = ecSelPageDown);
            FLastPosX := SaveLastPosX;
          end
          else
            MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown]);
          Update;
        end;
      ecPageTop, ecSelPageTop:
        begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine) );
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageTop);
          Update;
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          CaretNew := DisplayToBufferPos(
            DisplayCoord(DisplayX, TopLine + LinesInWindow -1) );
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelPageBottom);
          Update;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          CaretNew.Char := 1;
          CaretNew.Line := 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorTop);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew.Char := 1;
          CaretNew.Line := Lines.Count;
          if (CaretNew.Line > 0) then
            CaretNew.Char := Length(Lines[CaretNew.Line - 1]) + 1;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelEditorBottom);
          Update;
        end;
// goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then
        begin
          MoveCaretAndSelection(CaretXY, TBufferCoord(Data^), Command = ecSelGotoXY);
          Update;
        end;
// word selection
      ecWordLeft, ecSelWordLeft:
        begin
          CaretNew := PrevWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordLeft);
        end;
      ecWordRight, ecSelWordRight:
        begin
          CaretNew := NextWordPos;
          MoveCaretAndSelection(CaretXY, CaretNew, Command = ecSelWordRight);
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
          DoOnPaintTransientEx(ttBefore,true);
          try
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
                        SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                        if (SpaceCount2 > 0) and (SpaceCount2 < SpaceCount1) then
                          break;
                        Dec(BackCounter);
                      end;
                      if (BackCounter = -1) and (SpaceCount2 > SpaceCount1) then
                        SpaceCount2 := 0;
                    end;
                    if SpaceCount2 = SpaceCount1 then
                      SpaceCount2 := 0;
                    CaretX := fCaretX - (SpaceCount1 - SpaceCount2);
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
                  BeginUndoBlock;
                  try
                    CaretXY := BufferCoord(Lines[CaretY - 2].Length + 1, CaretY - 1);
                    Lines.Delete(CaretY);

                    LineText := LineText + Temp;
                  finally
                    EndUndoBlock;
                  end;
                end;
              end
              else begin
                // delete text before the caret
                if (Temp[CaretX - 1] <= #32) and (LeftSpaces(Temp) = CaretX - 1) then
                begin
                  SpaceCount1 := LeftSpaces(Temp, True);
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
                      SpaceCount2 := LeftSpaces(Temp2, True);
                      if (Temp2.Length > 0) and (SpaceCount2 < SpaceCount1) then
                        break;
                      Dec(BackCounter);
                    end;
                    if (BackCounter = -1) and (SpaceCount2 >= SpaceCount1) then
                      SpaceCount2 := 0;
                  end
                  else
                    SpaceCount2 := SpaceCount1 - (SpaceCount1 - 1) mod TabWidth - 1;
                  Delete(Temp, 1, LeftSpaces(Temp));
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
                  CaretX := CaretXNew;
                  Lines[CaretY - 1] := Temp;
                end;
              end;
            end;
            EnsureCursorPosVisible;
          finally
            DoOnPaintTransientEx(ttAfter,true);
          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          DoOnPaintTransient(ttBefore);

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
              CaretXNew := ValidTextPos(BufferCoord(CaretX + 1, CaretY), True).Char;
              Delete(Temp, CaretX, CaretXNew - CaretX);
              Lines[CaretY - 1] := Temp;
            end
            else begin
              // join line with the line after
              if CaretY < Lines.Count then
              begin
                BeginUndoBlock;
                try
                  Helper := StringofChar(#32, CaretX - 1 - Len);
                  Lines[CaretY - 1] := Temp + Helper + Lines[CaretY];
                  Lines.Delete(CaretY);
                finally
                  EndUndoBlock;
                end;
              end;
            end;
          end;
          DoOnPaintTransient(ttAfter);
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
            DoOnPaintTransient(ttBefore);
            Temp := Lines[CaretY - 1];
            Delete(Temp, CaretX, WP.Char - CaretX);
            Lines[CaretY - 1] := Temp;
            DoOnPaintTransient(ttAfter);
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
            DoOnPaintTransient(ttBefore);
            Temp := Lines[CaretY - 1];
            Delete(Temp, WP.Char, CaretX - WP.Char);
            Lines[CaretY - 1] := Temp;
            CaretXY := WP;
            DoOnPaintTransient(ttAfter);
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
                  SpaceCount1 := LeftSpaces(Temp, True)
                else
                  SpaceCount1 := 0;
                Delete(Temp2, 1, CaretX - 1);
                SpaceBuffer := GetLeftSpacing(SpaceCount1, True);
                Lines.Insert(CaretY, SpaceBuffer + Temp2);
                Lines[CaretY - 1] := Temp;
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
              // either emtpy or at the end of line: insert new line below
              if fLines.Count = 0 then
                fLines.Add('');
              SpaceCount2 := 0;
              { Autoindent only in case we are not on the line begin }
              if (eoAutoIndent in Options) and (CaretX > 1)  then
              begin
                BackCounter := CaretY;
                repeat
                  Dec(BackCounter);
                  Temp := Lines[BackCounter];
                  SpaceCount2 := LeftSpaces(Temp, True);
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
          end;
        end;
      ecTab:
        if not ReadOnly then DoTabKey;
      ecShiftTab:
        if not ReadOnly then DoShiftTabKey;
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
      // #127 is Ctrl + Backspace, #32 is space
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then
        begin
          DoOnPaintTransient(ttBefore);
          if SelAvail then
            SetSelText(AChar)
          else
          begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
            begin
              if (Len > 0) then
                SpaceBuffer := StringofChar(#32, CaretX - Len - Ord(fInserting))
              else
                SpaceBuffer := GetLeftSpacing(CaretX - Len - Ord(fInserting), True);

              Temp := Temp + SpaceBuffer;
            end;
            // Added the check for whether or not we're in insert mode.
            // If we are, we append one less space than we would in overwrite mode.
            // This is because in overwrite mode we have to put in a final space
            // character which will be overwritten with the typed character.  If we put the
            // extra space in in insert mode, it would be left at the end of the line and
            // cause problems unless eoTrimTrailingSpaces is set.
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;

              if fInserting then
              begin
                Insert(AChar, Temp, CaretX);
                Lines[CaretY - 1] := Temp;
                if Len = 0 then
                  CaretX := Length(Temp) + 1
                else
                  CaretX := CaretX + 1;
              end
              else begin
              // Processing of case character covers on LeadByte.
                Temp[CaretX] := AChar;
                Lines[CaretY - 1] := Temp;
                CaretX := CaretX + 1;
              end;
              // TODO: consider dropping
              if not CaretInView then
                LeftChar := LeftChar + Min(25, FTextAreaWidth div FCharWidth);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
          DoOnPaintTransient(ttAfter);
        end;
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
          if (not ReadOnly) and SelAvail then
            CutToClipboard;
        end;
      ecCopy:
        begin
          CopyToClipboard;
        end;
      ecPaste:
        begin
          if not ReadOnly then PasteFromClipboard;
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
            Update;
          end;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX > LeftChar + fCharsInWindow then
          //  InternalCaretX := LeftChar + fCharsInWindow;
          Update;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          // todo: The following code was commented out because it is not MBCS or hard-tab safe.
          //if CaretX < LeftChar then
          //  InternalCaretX := LeftChar;
          Update;
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
      ecNormalSelect:
        SelectionMode := smNormal;
      ecColumnSelect:
        SelectionMode := smColumn;
      ecLineSelect:
        SelectionMode := smLine;
      ecContextHelp:
        begin
          if Assigned (fOnContextHelp) then
            fOnContextHelp (self,WordAtCursor);
        end;
      ecImeStr:
        if not ReadOnly then
        begin
          S := PWideChar(Data);
          if SelAvail then
          begin
            SetSelText(s);
            InvalidateGutterLines(-1, -1);
          end
          else
          begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + StringofChar(#32, CaretX - Len - 1);
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
              Len := Length(s);
              if not fInserting then
                Delete(Temp, CaretX, Len);
              Insert(s, Temp, CaretX);
              CaretX := (CaretX + Len);
              Lines[CaretY - 1] := Temp;
              // TODO: consider dropping
              if not CaretInView then
                LeftChar := LeftChar + Min(25, FTextAreaWidth div FCharWidth);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
        end;
      ecCopyLineUp, ecCopyLineDown, ecMoveLineUp, ecMoveLineDown:
        ExecCmdCopyOrMoveLine(Command);
//++ CodeFolding
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
//-- CodeFolding
    end;
  finally
    DecPaintLock;
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
  Lines.Clear;
  fMarkList.Clear; // fMarkList.Clear also frees all bookmarks,
  FillChar(fBookMarks, sizeof(fBookMarks), 0); // so fBookMarks should be cleared too
  fUndoRedo.Clear;
  Modified := False;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

procedure TCustomSynEdit.ClearTrackChanges;
begin
  fUndoRedo.ClearTrackChanges(Lines);
  InvalidateGutter;
end;

function TCustomSynEdit.NextWordPosEx(const XY: TBufferCoord): TBufferCoord;
var
  CX, CY, LineLen: Integer;
  Line: string;
begin
  CX := XY.Char;
  CY := XY.Line;

  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    LineLen := Length(Line);
    if CX >= LineLen then
    begin
      // find first IdentChar or multibyte char in the next line
      if CY < Lines.Count then
      begin
        Line := Lines[CY];
        Inc(CY);
        CX := StrScanForCharInCategory(Line, 1, IsIdentChar);
        if CX = 0 then
          Inc(CX);
      end;
    end
    else
    begin
      // find next word-break-char if current char is an IdentChar
      if IsIdentChar(Line[CX]) then
        CX := StrScanForCharInCategory(Line, CX, IsWordBreakChar);
      // if word-break-char found, find the next IdentChar
      if CX > 0 then
        CX := StrScanForCharInCategory(Line, CX, IsIdentChar);
      // if one of those failed just position at the end of the line
      if CX = 0 then
        CX := LineLen + 1;
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
  Line: string;
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
      // find last IdentChar in the previous line
      if CY > 1 then
      begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
      end;
    end
    else
    begin
      // if previous char is a word-break-char search for the last IdentChar
      if IsWordBreakChar(Line[CX - 1]) then
        CX := StrRScanForCharInCategory(Line, CX - 1, IsIdentChar);
      if CX > 0 then
        // search for the first IdentChar of this "word"
        CX := StrRScanForCharInCategory(Line, CX - 1, IsWordBreakChar) + 1;
      if CX = 0 then
      begin
        // else just position at the end of the previous line
        if CY > 1 then
        begin
          Dec(CY);
          Line := Lines[CY - 1];
          CX := Length(Line) + 1;
        end
        else
          CX := 1;
      end;
    end;
  end;
  Result.Char := CX;
  Result.Line := CY;
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  if FSelectionMode <> Value then
  begin
    fSelectionMode := Value;
    ActiveSelectionMode := Value;
  end;
end;

procedure TCustomSynEdit.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if fActiveSelectionMode <> Value then
  begin
    if SelAvail then
      InvalidateSelection;
    fActiveSelectionMode := Value;
    if SelAvail then
      InvalidateSelection;
    StatusChanged([scSelection]);
  end;
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
procedure TCustomSynEdit.MarkListChange(Sender: TObject);
begin
  InvalidateGutter;
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
    if not(csDestroying in ComponentState) and  not(focused) then
    begin
      if Value then
      begin
        InitializeCaret;
      end
      else
      begin
        HideCaret;
        Winapi.Windows.DestroyCaret;
      end;
    end;
  end;
end;

procedure TCustomSynEdit.SetSelStart(const Value: Integer);
begin
  { if we don't call HandleNeeded, fCharsInWindow may be 0 and LeftChar will
  be set to CaretX }
  HandleNeeded;
  InternalCaretXY := CharIndexToRowCol(Value);
  BlockBegin := CaretXY;
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
    if nW = fGutterWidth then
      InvalidateGutter
    else
      SetGutterWidth(nW);
  end;
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

procedure TCustomSynEdit.SelectedColorsChanged(Sender: TObject);
begin
  InvalidateSelection;
end;

// find / replace

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions): Integer;
var
  ptStart, ptEnd: TBufferCoord; // start and end of the search range
  ptCurrent: TBufferCoord; // current search position
  nSearchLen, nReplaceLen, n, nFound: integer;
  nInLine, nEOLCount, i: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  bEndUndoBlock: boolean;
  nAction: TSynReplaceAction;
  iResultOffset: Integer;
  sReplace: string;

  function InValidSearchRange(First, Last: Integer): Boolean;
  begin
    Result := True;
    if (fActiveSelectionMode = smNormal) or not (ssoSelectedOnly in AOptions) then
    begin
      if ((ptCurrent.Line = ptStart.Line) and (First < ptStart.Char)) or
        ((ptCurrent.Line = ptEnd.Line) and (Last > ptEnd.Char))
      then
        Result := False;
    end
    else
    if (fActiveSelectionMode = smColumn) then
      // solves bug in search/replace when smColumn mode active and no selection
      Result := (First >= ptStart.Char) and (Last <= ptEnd.Char) or (ptEnd.Char - ptStart.Char < 1);
  end;

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
  if not SelAvail then Exclude(AOptions, ssoSelectedOnly);
  if (ssoSelectedOnly in AOptions) then begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;
    // search the whole line in the line selection mode
    if (fActiveSelectionMode = smLine) then
    begin
      ptStart.Char := 1;
      ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    end
    else if (fActiveSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.Char > ptEnd.Char) then
        SwapInt(Integer(ptStart.Char), Integer(ptEnd.Char));
    // ignore the cursor position when searching in the selection
    if bBackward then
      ptCurrent := ptEnd
    else
      ptCurrent := ptStart;
  end
  else
  begin
    ptStart.Char := 1;
    ptStart.Line := 1;
    ptEnd.Line := Lines.Count;
    ptEnd.Char := Length(Lines[ptEnd.Line - 1]) + 1;
    if bFromCursor then
      if bBackward then ptEnd := CaretXY else ptStart := CaretXY;
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;
  //  translate \n and \t to real chars for regular expressions
  sReplace := fSearchEngine.PreprocessReplaceExpression(AReplace);

  //count line ends
  nEOLCount := 0;
  i := 1;
  repeat
    i := Pos(WideCrLf, sReplace, i);
    if i <> 0 then
    begin
      i := i + Length(WideCrLf);
      Inc(nEolCount);
    end;
  until i = 0;
  // initialize the search engine
  fSearchEngine.Options := AOptions;
  fSearchEngine.Pattern := ASearch;
  // search while the current search position is inside of the search range
  nReplaceLen := 0;
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
    while (ptCurrent.Line >= ptStart.Line) and (ptCurrent.Line <= ptEnd.Line) do
    begin
      nInLine := fSearchEngine.FindAll(Lines[ptCurrent.Line - 1]);
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
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.

        ptCurrent.Char := nFound;
        BlockBegin := ptCurrent;
        // Be sure to use the Ex version of CursorPos so that it appears in the middle if necessary
        SetCaretXYEx(False, BufferCoord(1, ptCurrent.Line));
        // there is no necessary to see changes without confirmation. It signicatntly slow down replace
        if not (bReplaceAll) then
            EnsureCursorPosVisibleEx(True);
        Inc(ptCurrent.Char, nSearchLen);
        BlockEnd := ptCurrent;
        InternalCaretXY := ptCurrent;
        if bBackward then InternalCaretXY := BlockBegin else InternalCaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then
        begin
          nAction := DoOnReplaceText(ASearch, sReplace, ptCurrent.Line, nFound);
          if nAction = raCancel then
            exit;
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
            if bEndUndoBlock = false then
              BeginUndoBlock;
            bEndUndoBlock:= true;
          end;
          // Allow advanced substition in the search engine
          SelText := fSearchEngine.Replace(SelText, sReplace);
          nReplaceLen := CaretX - nFound;
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          InternalCaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then
          begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
            if (fActiveSelectionMode <> smColumn) and (CaretY = ptEnd.Line) then
            begin
              Inc(ptEnd.Char, nReplaceLen - nSearchLen);
              BlockEnd := ptEnd;
            end;
          end;
          //Fix new line ends
          if nEOLCount > 0 then
            Inc(ptEnd.Line, nEOLCount);
        end;
        if not bReplaceAll then
          exit;
      end;
      // search next / previous line
      if bBackward then
        Dec(ptCurrent.Line)
      else
        Inc(ptCurrent.Line);
    end;
  finally
    if bReplaceAll and not bPrompt then DecPaintLock;
    if bEndUndoBlock then EndUndoBlock;
    DoOnPaintTransient( ttAfter );
  end;
end;

function TCustomSynEdit.IsPointInSelection(const Value: TBufferCoord): boolean;
var
  ptBegin, ptEnd: TBufferCoord;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Line >= ptBegin.Line) and (Value.Line <= ptEnd.Line) and
    ((ptBegin.Line <> ptEnd.Line) or (ptBegin.Char <> ptEnd.Char)) then
  begin
    if fActiveSelectionMode = smLine then
      Result := True
    else if (fActiveSelectionMode = smColumn) then
    begin
      if (ptBegin.Char > ptEnd.Char) then
        Result := (Value.Char >= ptEnd.Char) and (Value.Char < ptBegin.Char)
      else if (ptBegin.Char < ptEnd.Char) then
        Result := (Value.Char >= ptBegin.Char) and (Value.Char < ptEnd.Char)
      else
        Result := False;
    end
    else
      Result := ((Value.Line > ptBegin.Line) or (Value.Char >= ptBegin.Char)) and
        ((Value.Line < ptEnd.Line) or (Value.Char < ptEnd.Char));
  end
  else
    Result := False;
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
    if (eoDragDropEditing in fOptions) and (not MouseCapture) and IsPointInSelection(ptLineCol) then
      iNewCursor := crArrow
    else if UseCodeFolding and CodeFolding.ShowHintMark and
      fAllFoldRanges.CollapsedFoldStartAtLine(ptLineCol.Line) then
    begin
      Rect := GetCollapseMarkRect(ptRowCol.Row, ptLineCol.Line);
      if PtInRect(Rect, ptCursor) and (ptCursor.X > FGutterWidth + fTextMargin) then
        iNewCursor := crHandPoint;
    end else
      iNewCursor := Cursor;
    if Assigned(OnMouseCursor) then
      OnMouseCursor(Self, ptLineCol, iNewCursor);
    fKbdHandler.ExecuteMouseCursor(Self, ptLineCol, iNewCursor);
  end;
  SetCursor(Screen.Cursors[iNewCursor]);
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

function TCustomSynEdit.GetOptions: TSynEditorOptions;
begin
  Result := fOptions;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
const
  ScrollOptions = [eoDisableScrollArrows,eoHideShowScrollbars,
    eoScrollPastEof,eoScrollPastEol];
var
  bSetDrag: Boolean;
  bUpdateScroll: Boolean;
  bInvalidate: Boolean;
begin
  if (Value <> fOptions) then
  begin
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);
    bInvalidate := (eoShowSpecialChars in fOptions) <> (eoShowSpecialChars in Value);
    bUpdateScroll := (Options * ScrollOptions) <> (Value * ScrollOptions);
    CalcTextAreaWidth;  // in case eoWrapWithRightEdge changed

    FUndoRedo.GroupUndo := eoGroupUndo in Options;

    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
    fOptions := Value;

    // (un)register HWND as drop target
    if bSetDrag and not (csDesigning in ComponentState) and HandleAllocated then
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
    if bInvalidate then
      Invalidate;
    if bUpdateScroll then
      UpdateScrollBars;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated and (fCharWidth <> 0) then
  begin
    fCharsInWindow := Max(ClientWidth - fGutterWidth - fTextMargin, 0) div fCharWidth;
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
  ptO, ptDst: TBufferCoord;
  s: string;
  nLineLen: Integer;
  bChangeY: Boolean;
  vCaretRowCol: TDisplayCoord;
begin
  if WordWrap then
  begin
    if DX > 0 then
    begin
      if fCaretAtEOL then
      begin
        fCaretAtEOL := False;
        UpdateLastPosX;
        InvalidateLine(fCaretY);
        IncPaintLock;
        Include(fStateFlags, sfCaretChanged);
        DecPaintLock;
        Exit;
      end;
    end
    else
    begin // DX < 0. Handle ecLeft/ecPageLeft at BOL.
      if (not fCaretAtEOL) and (CaretX > 1) and (DisplayX = 1) then
      begin
        fCaretAtEOL := True;
        UpdateLastPosX;
        InvalidateLine(fCaretY);
        if DisplayX > fCharsInWindow +1 then
          SetInternalDisplayXY( DisplayCoord(fCharsInWindow +1, DisplayY) )
        else begin
          IncPaintLock;
          Include(fStateFlags, sfCaretChanged);
          DecPaintLock;
        end;
        Exit;
      end;
    end;
  end;
  ptO := CaretXY;
  ptDst := ptO;
  s := LineText;
  nLineLen := Length(s);
  // only moving or selecting one char can change the line
  bChangeY := not (eoScrollPastEol in fOptions);
  if bChangeY and (DX = -1) and (ptO.Char = 1) and (ptO.Line > 1) then
  begin
    // end of previous line
    Dec(ptDst.Line);
    ptDst.Char := Length(Lines[ptDst.Line - 1]) + 1;
  end
  else if bChangeY and (DX = 1) and (ptO.Char > nLineLen) and (ptO.Line < Lines.Count) then
  begin
    // start of next line
    Inc(ptDst.Line);
    ptDst.Char := 1;
  end
  else begin
    ptDst.Char := Max(1, ptDst.Char + DX);
    // don't go past last char when ScrollPastEol option not set
    if (DX > 0) and bChangeY then
      ptDst.Char := Min(ptDst.Char, nLineLen + 1);
    ptDst := ValidTextPos(ptDst, DX > 0);
  end;
  // set caret and block begin / end
  MoveCaretAndSelection(fBlockBegin, ptDst, SelectionCommand);
  // TODO: Decide what to do with this
  // if caret is beyond fCharsInWindow move to next row (this means there are
  // spaces/tabs at the end of the row)
//  if WordWrap and (DX > 0) and (CaretX < Length(LineText)) then
//  begin
//    vCaretRowCol := DisplayXY;
//    if (vCaretRowCol.Column = 1) and (LineToRow(CaretY) <> vCaretRowCol.Row) then
//    begin
//      fCaretAtEOL := True;
//      UpdateLastPosX;
//    end
//    else if vCaretRowCol.Column > fCharsInWindow + 1 then
//    begin
//      Inc(vCaretRowCol.Row);
//      vCaretRowCol.Column := 1;
//      InternalCaretXY := DisplayToBufferPos(vCaretRowCol);
//    end;
//  end;
end;

procedure TCustomSynEdit.MoveCaretVert(DY: Integer; SelectionCommand: Boolean);
var
  ptO, ptDst, vEOLTestPos: TDisplayCoord;
  vDstLineChar: TBufferCoord;
  SaveLastPosX: Integer;
begin
  ptO := DisplayXY;
  ptDst := ptO;

  Inc(ptDst.Row, DY);
  if DY >= 0 then
  begin
    if RowToLine(ptDst.Row) > Lines.Count then
      ptDst.Row := Max(1, DisplayLineCount);
  end
  else begin
    if ptDst.Row < 1 then
      ptDst.Row := 1;
  end;

  if (ptO.Row <> ptDst.Row) then
  begin
    if eoKeepCaretX in Options then
      ptDst.Column := PixelsToColumn(Rows[ptDst.Row], FLastPosX);
  end;
  vDstLineChar := ValidTextPos(DisplayToBufferPos(ptDst), False);

  // set caret and block begin / end
  IncPaintLock;
  SaveLastPosX := FLastPosX;
  MoveCaretAndSelection(fBlockBegin, vDstLineChar, SelectionCommand);
  if WordWrap then
  begin
    vEOLTestPos := BufferToDisplayPos(vDstLineChar);
    fCaretAtEOL := (vEOLTestPos.Column = 1) and (vEOLTestPos.Row <> ptDst.Row);
  end;
  DecPaintLock;
  // Restore FLastPosX after moving caret, since UpdateLastPosX, called by
  // SetCaretXYEx, changes them. This is the one case where we don't want that.
  FLastPosX := SaveLastPosX;
end;

procedure TCustomSynEdit.MoveCaretAndSelection(const ptBefore, ptAfter: TBufferCoord;
  SelectionCommand: Boolean);
begin
  if (eoGroupUndo in FOptions) and fUndoRedo.CanUndo then
    fUndoRedo.AddGroupBreak;

  IncPaintLock;
  if SelectionCommand then
  begin
    if not SelAvail then
      SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  end
  else
    SetBlockBegin(ptAfter);
  InternalCaretXY := ptAfter;
  DecPaintLock;
end;

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TBufferCoord);
var
  vOldMode: TSynSelectionMode;
begin
  vOldMode := fActiveSelectionMode;
  IncPaintLock;
  try
    InternalCaretXY := ptCaret;
    SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
  finally
    ActiveSelectionMode := vOldMode;
    DecPaintLock;
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

  BeginUndoBlock;
  try
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
  finally
    EndUndoBlock;
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
  if (eoTabIndent in Options) and (FBlockBegin.Line <> fBlockEnd.Line) then
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
            if (p^ = #9) or (p^ = #32) then break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if (p^ <> #9) and (p^ <> #32) then break;
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
      SpaceCount1 := LeftSpaces(TempS, True);
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
            if p^ <> #32 then break;
            Inc(SpaceCount2);
            Dec(p);
          until SpaceCount2 = SpaceCount1;
          // scan over non-whitespaces
          if SpaceCount2 < SpaceCount1 then
            repeat
              if p^ = #32 then break;
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
      SpaceCount1 := LeftSpaces(TempS);
      Delete(TempS, 1, SpaceCount1);
      TempS := TempS2 + TempS;
      Lines[CaretY - 1] :=  TempS;
      CaretX := fCaretX + TempS2.Length - SpaceCount1;
    end;
  end
  else
    DoBlockUnIndent;
end;

procedure TCustomSynEdit.DoHomeKey(Selection: Boolean);
var
  FirstNonblank: Integer;
  S: string;
  NewPos: TDisplayCoord;
  MaxX: Integer;
begin
  // home key enhancement
  NewPos := DisplayXY;
  if eoEnhanceHomeKey in fOptions then
  begin
    S := Rows[NewPos.Row];
    FirstNonblank := 1;
    MaxX := Length(S);
    while (FirstNonblank <= MaxX) and IsWhiteChar(S[FirstNonblank]) do
      Inc(FirstNonblank);
    if NewPos.Column > FirstNonblank then
      NewPos.Column := FirstNonblank
    else
      NewPos.Column := 1;
  end
  else
    NewPos.Column := 1;

  MoveCaretAndSelection(CaretXY, DisplayToBufferPos(NewPos), Selection);
end;

procedure TCustomSynEdit.DoEndKey(Selection: Boolean);

  function CaretInLastRow: Boolean;
  var
    vLastRow: Integer;
  begin
    if not WordWrap then
      Result := True
    else
    begin
      vLastRow := LineToRow(CaretY + 1) - 1;
      // This check allows good behaviour with empty rows (this can be useful in a diff app ;-)
      while (vLastRow > 1)
        and (fWordWrapPlugin.GetRowLength(vLastRow) = 0)
        and (RowToLine(vLastRow) = CaretY) do
      begin
        Dec(vLastRow);
      end;
      Result := DisplayY = vLastRow;
    end;
  end;

  function FirstCharInRow: Integer;
  var
    vPos: TDisplayCoord;
  begin
    vPos.Row := DisplayY;
    vPos.Column := 1;
    Result := DisplayToBufferPos(vPos).Char;
  end;

var
  vText: string;
  vLastNonBlank: Integer;
  vNewX: Integer;
  vNewCaret: TDisplayCoord;
  vMinX: Integer;
  vEnhance: Boolean;
begin
  if (eoEnhanceEndKey in fOptions) and CaretInLastRow then
  begin
    vEnhance := True;
    vText := LineText;
    vLastNonBlank := Length(vText);
    if WordWrap then
      vMinX := FirstCharInRow() - 1
    else
      vMinX := 0;
    while (vLastNonBlank > vMinX) and CharInSet(vText[vLastNonBlank], [#32, #9]) do
      Dec(vLastNonBlank);

    vNewX := CaretX - 1;
    if vNewX = vLastNonBlank then
      vNewX := Length(LineText) + 1
    else
      vNewX := vLastNonBlank + 1;
  end
  else
  begin
    vNewX := Length(LineText) + 1;
    vEnhance := False;
  end;

  if WordWrap then
  begin
    vNewCaret.Row := DisplayY;
    if vEnhance then
      vNewCaret.Column := BufferToDisplayPos(BufferCoord(vNewX, CaretY)).Column
    else
      vNewCaret.Column := fWordWrapPlugin.GetRowLength(vNewCaret.Row) + 1;
    //TODO vNewCaret.Column := Min(fCharsInWindow + 1, vNewCaret.Column);
    MoveCaretAndSelection(CaretXY, DisplayToBufferPos(vNewCaret), Selection);
    // Updates fCaretAtEOL flag.
    SetInternalDisplayXY(vNewCaret);
  end
  else
    MoveCaretAndSelection(CaretXY,
      BufferCoord(vNewX, CaretY), Selection);
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
  InsertPos: Integer;
begin
  OrgCaretPos := CaretXY;

  // keep current selection detail
  BB := BlockBegin;
  BE := BlockEnd;

  // build text to insert
  if (BE.Char = 1) then
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
      if fActiveSelectionMode = smColumn then
        InsertPos := Min(BB.Char, BE.Char)
      else
        InsertPos := 1;
      if Line.Length >= InsertPos then
      begin
        Insert(Spaces, Line, InsertPos);
        Lines[I - 1] := Line;
      end;
    end;

    if OrgCaretPos.Char > 1 then
      Inc(OrgCaretPos.Char, Spaces.Length);
    if BB.Char > 1 then
      Inc(BB.Char, Spaces.Length);
    if BE.Char > 1 then
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
    while (Run[0] = #32) and (Result < FTabWidth) do
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
      if FActiveSelectionMode = smColumn then
        Inc(PLine, MinIntValue([BB.Char - 1, BE.Char - 1, Line.Length]));
      TmpDelLen := GetDelLen(PChar(PLine));
      if TmpDelLen > 0 then
      begin
        if FActiveSelectionMode = smColumn then
          DelPos := Min(BB.Char, BE.Char)
        else
          DelPos := 1;
        Delete(Line, DelPos, TmpDelLen);
        Lines[I - 1] := Line;
        if FActiveSelectionMode <> smColumn then
        begin
          if I = BB.Line then
            BB.Char := Max(BB.Char - TmpDelLen, 1);
          if I = BE.Line then
            BE.Char := Max(BE.Char - TmpDelLen, 1);
          if I = OrgCaretPos.Line then
            OrgCaretPos.Char := Max(OrgCaretPos.Char - TmpDelLen, 1);
        end;
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
      begin
        if SelAvail then
          ClearSelection
        else
          CommandProcessor(ecDeleteChar, ' ', nil)
      end
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
begin
  if Action is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TEditCut then
        TEditAction(Action).Enabled := SelAvail and not ReadOnly
      else if Action is TEditCopy then
        TEditAction(Action).Enabled := SelAvail
      else if Action is TEditPaste then
        TEditAction(Action).Enabled := CanPaste
      else if Action is TEditDelete then
        TEditAction(Action).Enabled := not ReadOnly
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
      else if Action is TEditSelectAll then
        TEditAction(Action).Enabled := True;
    end;
  end else if Action is TSearchAction then
  begin
    Result := Focused;
    if Result then
    begin
      if Action is TSearchFindFirst then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine)
      else if Action is TSearchFind then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine)
      else if Action is TSearchReplace then
        TSearchAction(Action).Enabled := (Text<>'') and assigned(fSearchEngine);
    end;
  end else if Action is TSearchFindNext then
  begin
    Result := Focused;
    if Result then
      TSearchAction(Action).Enabled := (Text<>'')
        and (TSearchFindNext(Action).SearchFind <> nil)
        and (TSearchFindNext(Action).SearchFind.Dialog.FindText <> '');
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

//++ CodeFolding
  if UseCodeFolding or WordWrap then
//-- CodeFolding
  begin
    InvalidateLines(Line, Line);
    Exit;
  end;

  if (Line >= TopLine) and (Line <= TopLine + LinesInWindow) then
  begin
    // invalidate text area of this line
    rcInval := Rect(fGutterWidth, fTextHeight * (Line - TopLine), ClientWidth, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
    if sfLinesChanging in fStateFlags then
//++ Flicker Reduction
      UnionRect(fInvalidateRect, rcInval, fInvalidateRect)
//-- Flicker Reduction
    else
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
  if Wordwrap then
  begin
    BC := fWordWrapPlugin.DisplayToBufferPos(DisplayCoord(1, RowIndex));
    if InRange(BC.Line, 1, Lines.Count) then
    begin
      Len := fWordWrapPlugin.RowLength[RowIndex];
      Result := Copy(Lines[BC.Line - 1], BC.Char, Len);
    end
    else
      Result := '';
  end
  else
  begin
    BC := DisplayToBufferPos(DisplayCoord(1, RowIndex));
    Result := Lines[BC.Line - 1];
  end;
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
begin
  Result := GetMatchingBracketEx(CaretXY);
end;

function TCustomSynEdit.GetMatchingBracketEx(const APoint: TBufferCoord;
  Brackets: string): TBufferCoord;
var
  Line: string;
  Index, PosX, PosY, Len: Integer;
  Test, BracketInc, BracketDec: WideChar;
  NumBrackets: Integer;
  vDummy: string;
  attr: TSynHighlighterAttributes;
  p: TBufferCoord;
  isCommentOrString: Boolean;
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
            p.Char := PosX;
            p.Line := PosY;
            if (Test = BracketInc) or (Test = BracketDec) then
            begin
              if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                isCommentOrString := (attr = Highlighter.StringAttribute) or
                  (attr = Highlighter.CommentAttribute)
              else
                isCommentOrString := False;
              if (Test = BracketInc) and (not isCommentOrString) then
                Inc(NumBrackets)
              else if (Test = BracketDec) and (not isCommentOrString) then
              begin
                Dec(NumBrackets);
                if NumBrackets = 0 then
                begin
                  // matching bracket found, set caret and bail out
                  Result := P;
                  exit;
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
            p.Char := PosX;
            p.Line := PosY;
            if (Test = BracketInc) or (Test = BracketDec) then
            begin
              if GetHighlighterAttriAtRowCol(p, vDummy, attr) then
                isCommentOrString := (attr = Highlighter.StringAttribute) or
                  (attr = Highlighter.CommentAttribute)
              else
                isCommentOrString := False;
              if (Test = BracketInc) and (not isCommentOrString) then
                Inc(NumBrackets)
              else if (Test = BracketDec)and (not isCommentOrString) then
              begin
                Dec(NumBrackets);
                if NumBrackets = 0 then
                begin
                  // matching bracket found, set caret and bail out
                  Result := P;
                  exit;
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

function TCustomSynEdit.GetHighlighterAttriAtRowCol(const XY: TBufferCoord;
  var Token: string; var Attri: TSynHighlighterAttributes): Boolean;
var
  TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpStart, Attri);
end;

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(const XY: TBufferCoord;
  var Token: string; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: Integer;
  Line: string;
begin
  PosY := XY.Line - 1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
  begin
    Line := Lines[PosY];
    if PosY = 0 then
      Highlighter.ResetRange
    else
      Highlighter.SetRange(TSynEditStringList(Lines).Ranges[PosY - 1]);
    Highlighter.SetLine(Line, PosY);
    PosX := XY.Char;
    if (PosX > 0) and (PosX <= Length(Line)) then
      while not Highlighter.GetEol do
      begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + Length(Token)) then
        begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := True;
          exit;
        end;
        Highlighter.Next;
      end;
  end;
  Token := '';
  Attri := nil;
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

procedure TCustomSynEdit.RegisterCommandHandler(
  const AHandlerProc: THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TObjectList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
      AHandlerProc, AHandlerData))
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) already registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.UnregisterCommandHandler(AHandlerProc:
  THookedCommandEvent);
var
  i: Integer;
begin
  if not Assigned(AHandlerProc) then
  begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then
    fHookedCommandHandlers.Delete(i)
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
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

procedure TCustomSynEdit.DoOnPaintTransientEx(TransientType: TTransientType; Lock: Boolean);
var
  DoTransient: Boolean;
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  DoTransient:=(FPaintTransientLock=0);
  if Lock then
  begin
    if (TransientType=ttBefore) then Inc(FPaintTransientLock)
    else
    begin
      Dec(FPaintTransientLock);
      DoTransient:= FPaintTransientLock = 0;
    end;
  end;

  if DoTransient then
  begin
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
      Canvas.Font.Assign(Font);
      Canvas.Brush.Color := Color;
      HideCaret;
      try
        fOnPaintTransient(Self, Canvas, TransientType);
      finally
        ShowCaret;
      end;
    end;
  end;
end;

procedure TCustomSynEdit.DoOnPaintTransient(TransientType: TTransientType);
begin
  DoOnPaintTransientEx(TransientType, False);
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
  if Assigned(fOnStatusChange) then
  begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
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
    Start := XY.Char;
    if (Len > 0) and InRange(XY.Char, 1, Len + 1) and IsIdentChar(Line[Start]) then
    begin
       while (Start > 1) and IsIdentChar(Line[Start - 1]) do
          Dec(Start);

       Stop := XY.Char;
       while (Stop <= Len) and IsIdentChar(Line[Stop]) do
          Inc(Stop);

       Result := Copy(Line, Start, Stop - Start);
    end;
  end;
end;

function TCustomSynEdit.BufferToDisplayPos(const p: TBufferCoord): TDisplayCoord;
// BufferToDisplayPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
begin
  Result := TDisplayCoord(p);
  if WordWrap then
    Result := fWordWrapPlugin.BufferToDisplayPos(TBufferCoord(Result));
  if UseCodeFolding then
    Result.Row := fAllFoldRanges.FoldLineToRow(Result.Row)
end;

function TCustomSynEdit.DisplayToBufferPos(const p: TDisplayCoord): TBufferCoord;
// DisplayToBufferPos takes a position on screen and transfrom it
// into position of text
begin
  if WordWrap then
    Result := fWordWrapPlugin.DisplayToBufferPos(p)
  else
    Result := TBufferCoord(p);
  if UseCodeFolding then
    Result.Line := fAllFoldRanges.FoldRowToLine(p.Row);
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
  // gutter marks
  for i := 0 to Marks.Count - 1 do
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;

  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phLinesInserted in Plugin.Handlers then
        Plugin.LinesInserted(FirstLine, Count);
    end;
end;

procedure TCustomSynEdit.DoLinePut(FirstLine: Integer; const OldLine: string);
var
  i: Integer;
  Plugin: TSynEditPlugin;
begin
  // plugins
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do
    begin
      PlugIn := TSynEditPlugin(fPlugins[i]);
      if phLinePut in Plugin.Handlers then
        Plugin.LinePut(FirstLine, OldLine);
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

procedure TCustomSynEdit.ProperSetLine(ALine: Integer; const ALineText: string);
begin
  if eoTrimTrailingSpaces in Options then
    Lines[ALine] := ALineText.TrimRight
  else
    Lines[ALine] := ALineText;
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
  begin
    case AChar of
      '_', '0'..'9', 'A'..'Z', 'a'..'z':
        Result := True;
      else
        Result := False;
    end;
    Result := Result or CharInSet(AChar, FAdditionalIdentChars);
    Result := Result and not IsWordBreakChar(AChar);
  end;
end;

function TCustomSynEdit.IsNonWhiteChar(AChar: WideChar): Boolean;
begin
  Result := not IsWhiteChar(AChar);
end;

function TCustomSynEdit.IsWhiteChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWhiteChar(AChar)
  else if AChar.IsWhiteSpace then
    Result := True
  else
    Result := not (IsIdentChar(AChar) or IsWordBreakChar(AChar));
end;

function TCustomSynEdit.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  if Assigned(Highlighter) then
    Result := Highlighter.IsWordBreakChar(AChar)
  else
  begin
    case AChar of
      #0..#32, '.', ',', ';', ':', '"', '''', WideChar(#$00B4), '`',
      WideChar(#$00B0), '^', '!', '?', '&', '$', '@', WideChar(#$00A7), '%',
      '#', '~', '[', ']', '(', ')', '{', '}', '<', '>', '-', '=', '+', '*',
      '/', '\', '|':
        Result := True;
      else
        Result := False;
    end;

    Result := Result or CharInSet(AChar, FAdditionalWordBreakChars);
    Result := Result and not CharInSet(AChar, FAdditionalIdentChars);
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

function TCustomSynEdit.CharIndexToRowCol(Index: Integer): TBufferCoord;
{ Index is 0-based; Result.x and Result.y are 1-based }
var
  x, y, Chars: Integer;
begin
  x := 0;
  y := 0;
  Chars := 0;
  while y < Lines.Count do
  begin
    x := Length(Lines[y]);
    if Chars + x + 2 > Index then
    begin
      x := Index - Chars;
      break;
    end;
    Inc(Chars, x + 2);
    x := 0;
    Inc(y);
  end;
  Result.Char := x + 1;
  Result.Line := y + 1;
end;

function TCustomSynEdit.RowColToCharIndex(RowCol: TBufferCoord): Integer;
{ Row and Col are 1-based; Result is 0-based }
var
  synEditStringList : TSynEditStringList;
begin
  RowCol.Line := Max(0, Min(Lines.Count, RowCol.Line) - 1);
  synEditStringList := (FLines as TSynEditStringList);
  // CharIndexToRowCol assumes a line break size of two
  Result :=  synEditStringList.LineCharIndex(RowCol.Line)
           + RowCol.Line * 2 + (RowCol.Char -1);
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
  begin
    ReScanForFoldRanges(0, fLines.Count -1);
  end;
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
    if fHighlighter is TSynCustomCodeFoldingHighlighter then
      TSynCustomCodeFoldingHighlighter(fHighlighter).AdjustFoldRanges(AllFoldRanges,
        fLines);
    InvalidateGutter;
    Include(fStateFlags, sfScrollbarChanged);
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
  SelStartBeforeSearch := SelStart; SelLengthBeforeSearch := SelLength;

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

  if (SelStart = SelStartBeforeSearch) and (SelLength = SelLengthBeforeSearch) then
  begin
    SelStart := 0;
    SelLength := 0;
  end;

  if Sender is TFindDialog then
    if not SearchByFindDialog(TFindDialog(Sender)) and (SelStart = 0) and (SelLength = 0) then
    begin
      SelStart := SelStartBeforeSearch;
      SelLength := SelLengthBeforeSearch;
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
  OldSelStart, OldSelLength: integer;
begin
  if (frReplaceAll in FindDialog.Options) then Options := [ssoReplaceAll]
  else if (frReplace in FindDialog.Options) then Options := [ssoReplace]
  else Options := [ssoSelectedOnly];

  if (frMatchCase in FindDialog.Options) then Options := Options + [ssoMatchCase];
  if (frWholeWord in FindDialog.Options) then Options := Options + [ssoWholeWord];
  if (not (frDown in FindDialog.Options)) then Options := Options + [ssoBackwards];

  if (ssoSelectedOnly in Options)
    then ReplaceText := ''
    else ReplaceText := TReplaceDialog(FindDialog).ReplaceText;

  OldSelStart := SelStart; OldSelLength := SelLength;
  if (UpperCase(SelText) = UpperCase(FindDialog.FindText)) and not (frReplace in FindDialog.Options) then
    SelStart := SelStart + SelLength
  else
    SelLength := 0;

  Result := SearchReplace(FindDialog.FindText, ReplaceText, Options) > 0;
  if not Result then
  begin
    SelStart := OldSelStart; SelLength := OldSelLength;
    if Assigned(OnSearchNotFound) then
      OnSearchNotFound(self, FindDialog.FindText)
    else
    begin
      MessageText := Format(STextNotFound, [FindDialog.FindText]);
      ShowMessage(MessageText);
    end;
  end
  else if (frReplace in FindDialog.Options) then
  begin
    SelStart := SelStart - Length(FindDialog.FindText) - 1;
    SelLength := Length(FindDialog.FindText) + 1;
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
  else
    Result := Max(ClientWidth - fGutterWidth - 2 * TextMargin, 0);;
end;

procedure TCustomSynEdit.SetWordWrap(const Value: Boolean);
var
  vOldTopLine: Integer;
  vShowCaret: Boolean;
begin
  if WordWrap <> Value then
  begin
    Invalidate; // better Invalidate before changing LeftChar and TopLine
    vShowCaret := CaretInView;
    vOldTopLine := RowToLine(TopLine);
     // !!Mutually exclusive with CodeFolding to reduce complexity
    if Value and not UseCodeFolding then
    begin
      fWordWrapPlugin := TSynWordWrapPlugin.Create(Self);
      LeftChar := 1;
    end
    else
      fWordWrapPlugin := nil;
    CalcTextAreaWidth;
    TopLine := LineToRow(vOldTopLine);
    UpdateScrollBars;

    if vShowCaret then
      EnsureCursorPosVisible;
  end;
end;

function TCustomSynEdit.GetDisplayLineCount: Integer;
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

procedure TCustomSynEdit.SetInternalDisplayXY(const aPos: TDisplayCoord);
begin
  IncPaintLock;
  InternalCaretXY := DisplayToBufferPos(aPos);
  fCaretAtEOL := WordWrap and (aPos.Row <= fWordWrapPlugin.RowCount) and
    (aPos.Column > fWordWrapPlugin.GetRowLength(aPos.Row)) and
    (DisplayY <> aPos.Row);
  DecPaintLock;
  UpdateLastPosX;
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

procedure TSynEditMarkList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynEditMarkList.GetItem(Index: Integer): TSynEditMark;
begin
  Result := TSynEditMark(inherited GetItem(Index));
end;

procedure TSynEditMarkList.SetItem(Index: Integer; Item: TSynEditMark);
begin
  inherited SetItem(Index, Item);
end;

constructor TSynEditMarkList.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fEdit := AOwner;
end;

function TSynEditMarkList.First: TSynEditMark;
begin
  Result := TSynEditMark(inherited First);
end;

function TSynEditMarkList.Last: TSynEditMark;
begin
  result := TSynEditMark(inherited Last);
end;

function TSynEditMarkList.Extract(Item: TSynEditMark): TSynEditMark;
begin
  Result := TSynEditMark(inherited Extract(Item));
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

procedure TSynEditMarkList.Place(mark: TSynEditMark);
begin
  if assigned(fEdit) then
    if Assigned(fEdit.OnPlaceBookmark) then
      fEdit.OnPlaceBookmark(fEdit, mark);
  if assigned(mark) then
    Add(mark);
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
const
  AllPlugInHandlers = [phLinesInserted, phLinesBeforeDeleted, phLinesDeleted,
    phLinePut, phLinesChanged, phPaintTransient, phAfterPaint];

begin
  inherited Create;
  if AOwner <> nil then
  begin
    fOwner := AOwner;
    if fOwner.fPlugins = nil then
      fOwner.fPlugins := TObjectList.Create;
    fOwner.fPlugins.Add(Self);
  end;
  FHandlers := AllPlugInHandlers;  // for backward compatibility
end;

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit;
  AHandlers: TPlugInHandlers);
begin
  Create(AOwner);
  FHandlers := AHandlers;
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

end.
