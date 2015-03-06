// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEdit.pas' rev: 29.00 (Windows)

#ifndef SyneditHPP
#define SyneditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <System.Contnrs.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.StdActns.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Themes.hpp>
#include <System.UITypes.hpp>
#include <SynUnicode.hpp>
#include <Winapi.Imm.hpp>
#include <SynTextDrawer.hpp>
#include <SynEditTypes.hpp>
#include <SynEditKeyConst.hpp>
#include <SynEditMiscProcs.hpp>
#include <SynEditMiscClasses.hpp>
#include <SynEditTextBuffer.hpp>
#include <SynEditKeyCmds.hpp>
#include <SynEditHighlighter.hpp>
#include <SynEditKbdHandler.hpp>
#include <System.WideStrUtils.hpp>
#include <System.Math.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ESynEditError;
struct TCreateParamsW;
class DELPHICLASS TSynEditMark;
class DELPHICLASS TSynEditMarkList;
__interface ISynEditBufferPlugin;
typedef System::DelphiInterface<ISynEditBufferPlugin> _di_ISynEditBufferPlugin;
class DELPHICLASS TSynEditPlugin;
class DELPHICLASS TCustomSynEdit;
class DELPHICLASS TSynEdit;
//-- type declarations -------------------------------------------------------
typedef Synedittypes::TBufferCoord TBufferCoord;

typedef Synedittypes::TDisplayCoord TDisplayCoord;

typedef Vcl::Forms::TFormBorderStyle TSynBorderStyle;

enum DECLSPEC_DENUM TSynReplaceAction : unsigned char { raCancel, raSkip, raReplace, raReplaceAll };

#pragma pack(push,4)
class PASCALIMPLEMENTATION ESynEditError : public Synedittypes::ESynError
{
	typedef Synedittypes::ESynError inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESynEditError(const System::UnicodeString Msg) : Synedittypes::ESynError(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESynEditError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : Synedittypes::ESynError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESynEditError(NativeUInt Ident)/* overload */ : Synedittypes::ESynError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESynEditError(System::PResStringRec ResStringRec)/* overload */ : Synedittypes::ESynError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynEditError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : Synedittypes::ESynError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynEditError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : Synedittypes::ESynError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESynEditError(const System::UnicodeString Msg, int AHelpContext) : Synedittypes::ESynError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESynEditError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : Synedittypes::ESynError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynEditError(NativeUInt Ident, int AHelpContext)/* overload */ : Synedittypes::ESynError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynEditError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Synedittypes::ESynError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynEditError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : Synedittypes::ESynError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynEditError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : Synedittypes::ESynError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESynEditError(void) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TDropFilesEvent)(System::TObject* Sender, int X, int Y, System::Classes::TStrings* AFiles);

typedef void __fastcall (__closure *THookedCommandEvent)(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData);

typedef void __fastcall (__closure *TPaintEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* ACanvas);

typedef void __fastcall (__closure *TProcessCommandEvent)(System::TObject* Sender, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data);

typedef void __fastcall (__closure *TReplaceTextEvent)(System::TObject* Sender, const System::UnicodeString ASearch, const System::UnicodeString AReplace, int Line, int Column, TSynReplaceAction &Action);

typedef void __fastcall (__closure *TSpecialLineColorsEvent)(System::TObject* Sender, int Line, bool &Special, System::Uitypes::TColor &FG, System::Uitypes::TColor &BG);

enum DECLSPEC_DENUM TTransientType : unsigned char { ttBefore, ttAfter };

typedef void __fastcall (__closure *TPaintTransient)(System::TObject* Sender, Vcl::Graphics::TCanvas* Canvas, TTransientType TransientType);

typedef void __fastcall (__closure *TScrollEvent)(System::TObject* Sender, Vcl::Forms::TScrollBarKind ScrollBar);

typedef void __fastcall (__closure *TGutterGetTextEvent)(System::TObject* Sender, int aLine, System::UnicodeString &aText);

typedef void __fastcall (__closure *TGutterPaintEvent)(System::TObject* Sender, int aLine, int X, int Y);

enum DECLSPEC_DENUM TSynEditCaretType : unsigned char { ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock };

enum DECLSPEC_DENUM TSynStateFlag : unsigned char { sfCaretChanged, sfScrollbarChanged, sfLinesChanging, sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfPossibleGutterClick, sfWaitForDragging, sfInsideRedo, sfGutterDragging };

typedef System::Set<TSynStateFlag, TSynStateFlag::sfCaretChanged, TSynStateFlag::sfGutterDragging> TSynStateFlags;

enum DECLSPEC_DENUM TScrollHintFormat : unsigned char { shfTopLineOnly, shfTopToBottom };

enum DECLSPEC_DENUM TSynEditorOption : unsigned char { eoAltSetsColumnMode, eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDisableScrollArrows, eoDragDropEditing, eoDropFiles, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoHalfPageScroll, eoHideShowScrollbars, eoKeepCaretX, eoNoCaret, eoNoSelection, eoRightMouseMovesCursor, eoScrollByOneLess, eoScrollHintFollows, eoScrollPastEof, eoScrollPastEol, eoShowScrollHint, eoShowSpecialChars, eoSmartTabDelete, eoSmartTabs, eoSpecialLineDefaultFg, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces };

typedef System::Set<TSynEditorOption, TSynEditorOption::eoAltSetsColumnMode, TSynEditorOption::eoTrimTrailingSpaces> TSynEditorOptions;

enum DECLSPEC_DENUM TSynFontSmoothMethod : unsigned char { fsmNone, fsmAntiAlias, fsmClearType };

struct DECLSPEC_DRECORD TCreateParamsW
{
public:
	System::WideChar *Caption;
	unsigned Style;
	unsigned ExStyle;
	int X;
	int Y;
	int Width;
	int Height;
	HWND WndParent;
	void *Param;
	tagWNDCLASSW WindowClass;
	System::StaticArray<System::WideChar, 64> WinClassName;
	System::UnicodeString InternalCaption;
};


enum DECLSPEC_DENUM TSynStatusChange : unsigned char { scAll, scCaretX, scCaretY, scLeftChar, scTopLine, scInsertMode, scModified, scSelection, scReadOnly };

typedef System::Set<TSynStatusChange, TSynStatusChange::scAll, TSynStatusChange::scReadOnly> TSynStatusChanges;

typedef void __fastcall (__closure *TContextHelpEvent)(System::TObject* Sender, System::UnicodeString word);

typedef void __fastcall (__closure *TStatusChangeEvent)(System::TObject* Sender, TSynStatusChanges Changes);

typedef void __fastcall (__closure *TMouseCursorEvent)(System::TObject* Sender, const Synedittypes::TBufferCoord &aLineCharPos, System::Uitypes::TCursor &aCursor);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditMark : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	int fLine;
	int fChar;
	int fImage;
	TCustomSynEdit* fEdit;
	bool fVisible;
	bool fInternalImage;
	int fBookmarkNum;
	virtual TCustomSynEdit* __fastcall GetEdit(void);
	virtual void __fastcall SetChar(const int Value);
	virtual void __fastcall SetImage(const int Value);
	virtual void __fastcall SetLine(const int Value);
	void __fastcall SetVisible(const bool Value);
	void __fastcall SetInternalImage(const bool Value);
	bool __fastcall GetIsBookmark(void);
	
public:
	__fastcall TSynEditMark(TCustomSynEdit* AOwner);
	__property int Line = {read=fLine, write=SetLine, nodefault};
	__property int Char = {read=fChar, write=SetChar, nodefault};
	__property TCustomSynEdit* Edit = {read=fEdit};
	__property int ImageIndex = {read=fImage, write=SetImage, nodefault};
	__property int BookmarkNumber = {read=fBookmarkNum, write=fBookmarkNum, nodefault};
	__property bool Visible = {read=fVisible, write=SetVisible, nodefault};
	__property bool InternalImage = {read=fInternalImage, write=SetInternalImage, nodefault};
	__property bool IsBookmark = {read=GetIsBookmark, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSynEditMark(void) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TPlaceMarkEvent)(System::TObject* Sender, TSynEditMark* &Mark);

typedef System::StaticArray<TSynEditMark*, 16> TSynEditMarks;

class PASCALIMPLEMENTATION TSynEditMarkList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	TSynEditMark* operator[](int Index) { return Items[Index]; }
	
protected:
	TCustomSynEdit* fEdit;
	System::Classes::TNotifyEvent fOnChange;
	virtual void __fastcall Notify(void * Ptr, System::Classes::TListNotification Action);
	HIDESBASE TSynEditMark* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TSynEditMark* Item);
	__property OwnsObjects;
	
public:
	__fastcall TSynEditMarkList(TCustomSynEdit* AOwner);
	HIDESBASE TSynEditMark* __fastcall First(void);
	HIDESBASE TSynEditMark* __fastcall Last(void);
	HIDESBASE TSynEditMark* __fastcall Extract(TSynEditMark* Item);
	void __fastcall ClearLine(int line);
	void __fastcall GetMarksForLine(int line, TSynEditMarks &Marks);
	void __fastcall Place(TSynEditMark* mark);
	__property TSynEditMark* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property TCustomSynEdit* Edit = {read=fEdit};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
public:
	/* TList.Destroy */ inline __fastcall virtual ~TSynEditMarkList(void) { }
	
};


typedef void __fastcall (__closure *TGutterClickEvent)(System::TObject* Sender, System::Uitypes::TMouseButton Button, int X, int Y, int Line, TSynEditMark* Mark);

__interface ISynEditBufferPlugin  : public System::IInterface 
{
	virtual Synedittypes::TDisplayCoord __fastcall BufferToDisplayPos(const Synedittypes::TBufferCoord &aPos) = 0 ;
	virtual Synedittypes::TBufferCoord __fastcall DisplayToBufferPos(const Synedittypes::TDisplayCoord &aPos) = 0 ;
	virtual int __fastcall RowCount(void) = 0 ;
	virtual int __fastcall GetRowLength(int aRow) = 0 ;
	virtual int __fastcall LinesInserted(int aIndex, int aCount) = 0 ;
	virtual int __fastcall LinesDeleted(int aIndex, int aCount) = 0 ;
	virtual int __fastcall LinesPutted(int aIndex, int aCount) = 0 ;
	virtual void __fastcall DisplayChanged(void) = 0 ;
	virtual void __fastcall Reset(void) = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditPlugin : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCustomSynEdit* fOwner;
	
protected:
	virtual void __fastcall AfterPaint(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &AClip, int FirstLine, int LastLine);
	virtual void __fastcall PaintTransient(Vcl::Graphics::TCanvas* ACanvas, TTransientType ATransientType);
	virtual void __fastcall LinesInserted(int FirstLine, int Count);
	virtual void __fastcall LinesDeleted(int FirstLine, int Count);
	__property TCustomSynEdit* Editor = {read=fOwner};
	
public:
	__fastcall TSynEditPlugin(TCustomSynEdit* AOwner);
	__fastcall virtual ~TSynEditPlugin(void);
};

#pragma pack(pop)

typedef void __fastcall (__closure *TCustomSynEditSearchNotFoundEvent)(System::TObject* Sender, System::UnicodeString FindText);

class PASCALIMPLEMENTATION TCustomSynEdit : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMCaptureChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Msg);
	MESSAGE void __fastcall WMClear(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMCopy(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMCut(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMDropFiles(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMDestroy(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Msg);
	MESSAGE void __fastcall WMGetText(Winapi::Messages::TWMGetText &Msg);
	MESSAGE void __fastcall WMGetTextLength(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMScroll &Msg);
	MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMSetText(Winapi::Messages::TWMSetText &Msg);
	MESSAGE void __fastcall WMImeChar(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMImeComposition(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMImeNotify(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	MESSAGE void __fastcall WMUndo(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMScroll &Msg);
	bool fAlwaysShowCaret;
	Synedittypes::TBufferCoord fBlockBegin;
	Synedittypes::TBufferCoord fBlockEnd;
	int fCaretX;
	int fLastCaretX;
	int fCaretY;
	int fCharsInWindow;
	int fCharWidth;
	Vcl::Graphics::TFont* fFontDummy;
	TSynFontSmoothMethod fFontSmoothing;
	bool fInserting;
	System::Classes::TStrings* fLines;
	System::Classes::TStrings* fOrigLines;
	Synedittextbuffer::TSynEditUndoList* fOrigUndoList;
	Synedittextbuffer::TSynEditUndoList* fOrigRedoList;
	int fLinesInWindow;
	int fLeftChar;
	int fMaxScrollWidth;
	int fPaintLock;
	bool fReadOnly;
	int fRightEdge;
	System::Uitypes::TColor fRightEdgeColor;
	System::Uitypes::TColor fScrollHintColor;
	TScrollHintFormat fScrollHintFormat;
	System::Uitypes::TScrollStyle FScrollBars;
	int fTextHeight;
	int fTextOffset;
	int fTopLine;
	Synedithighlighter::TSynCustomHighlighter* fHighlighter;
	Syneditmiscclasses::TSynSelectedColor* fSelectedColor;
	System::Uitypes::TColor fActiveLineColor;
	Synedittextbuffer::TSynEditUndoList* fUndoList;
	Synedittextbuffer::TSynEditUndoList* fRedoList;
	System::StaticArray<TSynEditMark*, 10> fBookMarks;
	int fMouseDownX;
	int fMouseDownY;
	Syneditmiscclasses::TSynBookMarkOpt* fBookMarkOpt;
	Vcl::Forms::TFormBorderStyle fBorderStyle;
	bool fHideSelection;
	int fMouseWheelAccumulator;
	TSynEditCaretType fOverwriteCaret;
	TSynEditCaretType fInsertCaret;
	System::Types::TPoint fCaretOffset;
	Syneditkeycmds::TSynEditKeyStrokes* fKeyStrokes;
	bool fModified;
	TSynEditMarkList* fMarkList;
	int fExtraLineSpacing;
	Synedittypes::TSynSelectionMode fSelectionMode;
	Synedittypes::TSynSelectionMode fActiveSelectionMode;
	bool fWantReturns;
	bool fWantTabs;
	_di_ISynEditBufferPlugin fWordWrapPlugin;
	Syneditmiscclasses::TSynGlyph* fWordWrapGlyph;
	bool fCaretAtEOL;
	Syneditmiscclasses::TSynGutter* fGutter;
	int fTabWidth;
	Syntextdrawer::TheTextDrawer* fTextDrawer;
	System::Types::TRect fInvalidateRect;
	TSynStateFlags fStateFlags;
	TSynEditorOptions fOptions;
	TSynStatusChanges fStatusChanges;
	System::Word fLastKey;
	System::Classes::TShiftState fLastShiftState;
	Syneditmiscclasses::TSynEditSearchCustom* fSearchEngine;
	System::Contnrs::TObjectList* fHookedCommandHandlers;
	Syneditkbdhandler::TSynEditKbdHandler* fKbdHandler;
	System::Classes::TList* fFocusList;
	System::Contnrs::TObjectList* fPlugins;
	Vcl::Extctrls::TTimer* fScrollTimer;
	int fScrollDeltaX;
	int fScrollDeltaY;
	System::Classes::TNotifyEvent fOnChange;
	TPlaceMarkEvent fOnClearMark;
	TProcessCommandEvent fOnCommandProcessed;
	TDropFilesEvent fOnDropFiles;
	TGutterClickEvent fOnGutterClick;
	Synedittypes::TKeyPressWEvent FOnKeyPressW;
	TMouseCursorEvent fOnMouseCursor;
	TPaintEvent fOnPaint;
	TPlaceMarkEvent fOnPlaceMark;
	TProcessCommandEvent fOnProcessCommand;
	TProcessCommandEvent fOnProcessUserCommand;
	TReplaceTextEvent fOnReplaceText;
	TSpecialLineColorsEvent fOnSpecialLineColors;
	TContextHelpEvent fOnContextHelp;
	TPaintTransient fOnPaintTransient;
	TScrollEvent fOnScroll;
	TGutterGetTextEvent fOnGutterGetText;
	TGutterPaintEvent fOnGutterPaint;
	TStatusChangeEvent fOnStatusChange;
	bool fShowSpecChar;
	int FPaintTransientLock;
	bool FIsScrolling;
	System::Classes::TNotifyEvent fChainListCleared;
	Synedittextbuffer::TStringListChangeEvent fChainListDeleted;
	Synedittextbuffer::TStringListChangeEvent fChainListInserted;
	Synedittextbuffer::TStringListChangeEvent fChainListPutted;
	System::Classes::TNotifyEvent fChainLinesChanging;
	System::Classes::TNotifyEvent fChainLinesChanged;
	TCustomSynEdit* fChainedEditor;
	System::Classes::TNotifyEvent fChainUndoAdded;
	System::Classes::TNotifyEvent fChainRedoAdded;
	System::Sysutils::TSysCharSet FAdditionalWordBreakChars;
	System::Sysutils::TSysCharSet FAdditionalIdentChars;
	TCustomSynEditSearchNotFoundEvent fSearchNotFound;
	System::Classes::TNotifyEvent OnFindBeforeSearch;
	System::Classes::TNotifyEvent OnReplaceBeforeSearch;
	System::Classes::TNotifyEvent OnCloseBeforeSearch;
	int SelStartBeforeSearch;
	int SelLengthBeforeSearch;
	bool FWindowProducedMessage;
	void __fastcall BookMarkOptionsChanged(System::TObject* Sender);
	void __fastcall ComputeCaret(int X, int Y);
	void __fastcall ComputeScroll(int X, int Y);
	void __fastcall DoHomeKey(bool Selection);
	void __fastcall DoEndKey(bool Selection);
	void __fastcall DoLinesDeleted(int FirstLine, int Count);
	void __fastcall DoLinesInserted(int FirstLine, int Count);
	void __fastcall DoShiftTabKey(void);
	void __fastcall DoTabKey(void);
	void __fastcall DoCaseChange(const Syneditkeycmds::TSynEditorCommand Cmd);
	int __fastcall FindHookedCmdEvent(THookedCommandEvent AHandlerProc);
	void __fastcall SynFontChanged(System::TObject* Sender);
	Synedittypes::TBufferCoord __fastcall GetBlockBegin(void);
	Synedittypes::TBufferCoord __fastcall GetBlockEnd(void);
	bool __fastcall GetCanPaste(void);
	bool __fastcall GetCanRedo(void);
	bool __fastcall GetCanUndo(void);
	Synedittypes::TBufferCoord __fastcall GetCaretXY(void);
	int __fastcall GetDisplayX(void);
	int __fastcall GetDisplayY(void);
	Synedittypes::TDisplayCoord __fastcall GetDisplayXY(void);
	int __fastcall GetDisplayLineCount(void);
	Vcl::Graphics::TFont* __fastcall GetFont(void);
	int __fastcall GetHookedCommandHandlersCount(void);
	System::UnicodeString __fastcall GetLineText(void);
	int __fastcall GetMaxUndo(void);
	TSynEditorOptions __fastcall GetOptions(void);
	bool __fastcall GetSelAvail(void);
	bool __fastcall GetSelTabBlock(void);
	bool __fastcall GetSelTabLine(void);
	System::UnicodeString __fastcall GetSelText(void);
	System::UnicodeString __fastcall SynGetText(void);
	System::UnicodeString __fastcall GetWordAtCursor(void);
	System::UnicodeString __fastcall GetWordAtMouse(void);
	bool __fastcall GetWordWrap(void);
	void __fastcall GutterChanged(System::TObject* Sender);
	int __fastcall LeftSpaces(const System::UnicodeString Line);
	int __fastcall LeftSpacesEx(const System::UnicodeString Line, bool WantTabs);
	System::UnicodeString __fastcall GetLeftSpacing(int CharCount, bool WantTabs);
	void __fastcall LinesChanging(System::TObject* Sender);
	void __fastcall MoveCaretAndSelection(const Synedittypes::TBufferCoord &ptBefore, const Synedittypes::TBufferCoord &ptAfter, bool SelectionCommand);
	void __fastcall MoveCaretHorz(int DX, bool SelectionCommand);
	void __fastcall MoveCaretVert(int DY, bool SelectionCommand);
	void __fastcall PluginsAfterPaint(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &AClip, int FirstLine, int LastLine);
	void __fastcall ReadAddedKeystrokes(System::Classes::TReader* Reader);
	void __fastcall ReadRemovedKeystrokes(System::Classes::TReader* Reader);
	int __fastcall ScanFrom(int Index);
	void __fastcall ScrollTimerHandler(System::TObject* Sender);
	void __fastcall SelectedColorsChanged(System::TObject* Sender);
	void __fastcall SetBlockBegin(const Synedittypes::TBufferCoord &Value);
	void __fastcall SetBlockEnd(const Synedittypes::TBufferCoord &Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetCaretX(int Value);
	void __fastcall SetCaretY(int Value);
	void __fastcall InternalSetCaretX(int Value);
	void __fastcall InternalSetCaretY(int Value);
	void __fastcall SetInternalDisplayXY(const Synedittypes::TDisplayCoord &aPos);
	void __fastcall SetActiveLineColor(System::Uitypes::TColor Value);
	void __fastcall SetExtraLineSpacing(const int Value);
	HIDESBASE void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetGutter(Syneditmiscclasses::TSynGutter* const Value);
	void __fastcall SetGutterWidth(int Value);
	void __fastcall SetHideSelection(const bool Value);
	void __fastcall SetHighlighter(Synedithighlighter::TSynCustomHighlighter* const Value);
	void __fastcall SetInsertCaret(const TSynEditCaretType Value);
	void __fastcall SetInsertMode(const bool Value);
	void __fastcall SetKeystrokes(Syneditkeycmds::TSynEditKeyStrokes* const Value);
	void __fastcall SetLeftChar(int Value);
	void __fastcall SetLines(System::Classes::TStrings* Value);
	void __fastcall SetLineText(System::UnicodeString Value);
	void __fastcall SetMaxScrollWidth(int Value);
	void __fastcall SetMaxUndo(const int Value);
	void __fastcall SetModified(bool Value);
	void __fastcall SetOptions(TSynEditorOptions Value);
	void __fastcall SetOverwriteCaret(const TSynEditCaretType Value);
	void __fastcall SetRightEdge(int Value);
	void __fastcall SetRightEdgeColor(System::Uitypes::TColor Value);
	void __fastcall SetScrollBars(const System::Uitypes::TScrollStyle Value);
	void __fastcall SetSearchEngine(Syneditmiscclasses::TSynEditSearchCustom* Value);
	void __fastcall SetSelectionMode(const Synedittypes::TSynSelectionMode Value);
	void __fastcall SetActiveSelectionMode(const Synedittypes::TSynSelectionMode Value);
	void __fastcall SetSelTextExternal(const System::UnicodeString Value);
	void __fastcall SetTabWidth(int Value);
	void __fastcall SynSetText(const System::UnicodeString Value);
	void __fastcall SetTopLine(int Value);
	void __fastcall SetWordWrap(const bool Value);
	void __fastcall SetWordWrapGlyph(Syneditmiscclasses::TSynGlyph* const Value);
	void __fastcall WordWrapGlyphChange(System::TObject* Sender);
	void __fastcall SizeOrFontChanged(bool bFont);
	void __fastcall ProperSetLine(int ALine, const System::UnicodeString ALineText);
	void __fastcall UpdateModifiedStatus(void);
	void __fastcall UndoRedoAdded(System::TObject* Sender);
	void __fastcall UpdateLastCaretX(void);
	void __fastcall UpdateScrollBars(void);
	void __fastcall WriteAddedKeystrokes(System::Classes::TWriter* Writer);
	void __fastcall WriteRemovedKeystrokes(System::Classes::TWriter* Writer);
	void __fastcall SetAdditionalIdentChars(const System::Sysutils::TSysCharSet &Value);
	void __fastcall SetAdditionalWordBreakChars(const System::Sysutils::TSysCharSet &Value);
	void __fastcall DoSearchFindFirstExecute(Vcl::Stdactns::TSearchFindFirst* Action);
	void __fastcall DoSearchFindExecute(Vcl::Stdactns::TSearchFind* Action);
	void __fastcall DoSearchReplaceExecute(Vcl::Stdactns::TSearchReplace* Action);
	void __fastcall DoSearchFindNextExecute(Vcl::Stdactns::TSearchFindNext* Action);
	void __fastcall FindDialogFindFirst(System::TObject* Sender);
	void __fastcall FindDialogFind(System::TObject* Sender);
	System::LongBool __fastcall SearchByFindDialog(Vcl::Dialogs::TFindDialog* FindDialog);
	void __fastcall FindDialogClose(System::TObject* Sender);
	
protected:
	bool FIgnoreNextChar;
	System::UnicodeString FCharCodeString;
	DYNAMIC bool __fastcall DoMouseWheel(System::Classes::TShiftState Shift, int WheelDelta, const System::Types::TPoint &MousePos);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual void __fastcall DestroyWnd(void);
	virtual void __fastcall InvalidateRect(const System::Types::TRect &aRect, bool aErase);
	DYNAMIC void __fastcall DblClick(void);
	void __fastcall DecPaintLock(void);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall DoChange(void);
	void __fastcall DoKeyPressW(Winapi::Messages::TWMKey &Message);
	DYNAMIC void __fastcall DragCanceled(void);
	DYNAMIC void __fastcall DragOver(System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	virtual bool __fastcall GetReadOnly(void);
	void __fastcall HighlighterAttrChanged(System::TObject* Sender);
	void __fastcall IncPaintLock(void);
	void __fastcall InitializeCaret(void);
	DYNAMIC void __fastcall KeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall KeyPressW(System::WideChar &Key);
	virtual void __fastcall LinesChanged(System::TObject* Sender);
	void __fastcall ListCleared(System::TObject* Sender);
	void __fastcall ListDeleted(System::TObject* Sender, int aIndex, int aCount);
	void __fastcall ListInserted(System::TObject* Sender, int Index, int aCount);
	void __fastcall ListPutted(System::TObject* Sender, int Index, int aCount);
	void __fastcall ChainListCleared(System::TObject* Sender);
	void __fastcall ChainListDeleted(System::TObject* Sender, int aIndex, int aCount);
	void __fastcall ChainListInserted(System::TObject* Sender, int aIndex, int aCount);
	void __fastcall ChainListPutted(System::TObject* Sender, int aIndex, int aCount);
	void __fastcall ChainLinesChanging(System::TObject* Sender);
	void __fastcall ChainLinesChanged(System::TObject* Sender);
	void __fastcall ChainUndoRedoAdded(System::TObject* Sender);
	void __fastcall ScanRanges(void);
	virtual void __fastcall Loaded(void);
	void __fastcall MarkListChange(System::TObject* Sender);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall NotifyHookedCommandHandlers(bool AfterProcessing, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data);
	virtual void __fastcall Paint(void);
	virtual void __fastcall PaintGutter(const System::Types::TRect &AClip, const int aFirstRow, const int aLastRow);
	virtual void __fastcall PaintTextLines(const System::Types::TRect &AClip, const int aFirstRow, const int aLastRow, const int FirstCol, const int LastCol);
	void __fastcall RecalcCharExtent(void);
	void __fastcall RedoItem(void);
	virtual void __fastcall InternalSetCaretXY(const Synedittypes::TBufferCoord &Value);
	virtual void __fastcall SetCaretXY(const Synedittypes::TBufferCoord &Value);
	virtual void __fastcall SetCaretXYEx(bool CallEnsureCursorPos, const Synedittypes::TBufferCoord &Value);
	void __fastcall SetFontSmoothing(TSynFontSmoothMethod AValue);
	virtual void __fastcall SetName(const System::Classes::TComponentName Value);
	virtual void __fastcall SetReadOnly(bool Value);
	void __fastcall SetWantReturns(bool Value);
	void __fastcall SetSelTextPrimitive(const System::UnicodeString Value);
	void __fastcall SetSelTextPrimitiveEx(Synedittypes::TSynSelectionMode PasteMode, System::WideChar * Value, bool AddToUndoList);
	void __fastcall SetWantTabs(bool Value);
	void __fastcall StatusChanged(TSynStatusChanges AChanges);
	Syneditkeycmds::TSynEditorCommand __fastcall TranslateKeyCode(System::Word Code, System::Classes::TShiftState Shift, void * &Data);
	void __fastcall UndoItem(void);
	virtual void __fastcall UpdateMouseCursor(void);
	int fGutterWidth;
	Syneditmiscclasses::TSynInternalImage* fInternalImage;
	void __fastcall HideCaret(void);
	void __fastcall ShowCaret(void);
	virtual void __fastcall DoOnClearBookmark(TSynEditMark* &Mark);
	virtual void __fastcall DoOnCommandProcessed(Syneditkeycmds::TSynEditorCommand Command, System::WideChar AChar, void * Data);
	virtual void __fastcall DoOnGutterClick(System::Uitypes::TMouseButton Button, int X, int Y);
	virtual void __fastcall DoOnPaint(void);
	virtual void __fastcall DoOnPaintTransientEx(TTransientType TransientType, bool Lock);
	virtual void __fastcall DoOnPaintTransient(TTransientType TransientType);
	virtual void __fastcall DoOnPlaceMark(TSynEditMark* &Mark);
	virtual void __fastcall DoOnProcessCommand(Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data);
	virtual TSynReplaceAction __fastcall DoOnReplaceText(const System::UnicodeString ASearch, const System::UnicodeString AReplace, int Line, int Column);
	virtual bool __fastcall DoOnSpecialLineColors(int Line, System::Uitypes::TColor &Foreground, System::Uitypes::TColor &Background);
	virtual void __fastcall DoOnStatusChange(TSynStatusChanges Changes);
	int __fastcall GetSelEnd(void);
	int __fastcall GetSelStart(void);
	int __fastcall GetSelLength(void);
	void __fastcall SetSelEnd(const int Value);
	void __fastcall SetSelStart(const int Value);
	void __fastcall SetSelLength(const int Value);
	void __fastcall SetAlwaysShowCaret(const bool Value);
	System::UnicodeString __fastcall ShrinkAtWideGlyphs(const System::UnicodeString S, int First, int &CharCount);
	void __fastcall LinesHookChanged(void);
	__property int InternalCaretX = {write=InternalSetCaretX, nodefault};
	__property int InternalCaretY = {write=InternalSetCaretY, nodefault};
	__property Synedittypes::TBufferCoord InternalCaretXY = {write=InternalSetCaretXY};
	__property TSynFontSmoothMethod FontSmoothing = {read=fFontSmoothing, write=SetFontSmoothing, nodefault};
	
public:
	__fastcall virtual TCustomSynEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomSynEdit(void);
	__property Canvas;
	__property int SelStart = {read=GetSelStart, write=SetSelStart, nodefault};
	__property int SelEnd = {read=GetSelEnd, write=SetSelEnd, nodefault};
	__property bool AlwaysShowCaret = {read=fAlwaysShowCaret, write=SetAlwaysShowCaret, nodefault};
	void __fastcall UpdateCaret(void);
	void __fastcall AddKey(Syneditkeycmds::TSynEditorCommand Command, System::Word Key1, System::Classes::TShiftState SS1, System::Word Key2 = (System::Word)(0x0), System::Classes::TShiftState SS2 = System::Classes::TShiftState() );
	void __fastcall BeginUndoBlock(void);
	void __fastcall BeginUpdate(void);
	bool __fastcall CaretInView(void);
	Synedittypes::TBufferCoord __fastcall CharIndexToRowCol(int Index);
	void __fastcall Clear(void);
	void __fastcall ClearAll(void);
	void __fastcall ClearBookMark(int BookMark);
	void __fastcall ClearSelection(void);
	virtual void __fastcall CommandProcessor(Syneditkeycmds::TSynEditorCommand Command, System::WideChar AChar, void * Data);
	void __fastcall ClearUndo(void);
	void __fastcall CopyToClipboard(void);
	void __fastcall CutToClipboard(void);
	void __fastcall DoCopyToClipboard(const System::UnicodeString SText);
	DYNAMIC void __fastcall DragDrop(System::TObject* Source, int X, int Y);
	void __fastcall EndUndoBlock(void);
	void __fastcall EndUpdate(void);
	void __fastcall EnsureCursorPosVisible(void);
	void __fastcall EnsureCursorPosVisibleEx(bool ForceToMiddle, bool EvenIfVisible = false);
	virtual void __fastcall FindMatchingBracket(void);
	virtual Synedittypes::TBufferCoord __fastcall GetMatchingBracket(void);
	virtual Synedittypes::TBufferCoord __fastcall GetMatchingBracketEx(const Synedittypes::TBufferCoord &APoint);
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual void __fastcall ExecuteCommand(Syneditkeycmds::TSynEditorCommand Command, System::WideChar AChar, void * Data);
	System::UnicodeString __fastcall ExpandAtWideGlyphs(const System::UnicodeString S);
	bool __fastcall GetBookMark(int BookMark, int &X, int &Y);
	bool __fastcall GetHighlighterAttriAtRowCol(const Synedittypes::TBufferCoord &XY, System::UnicodeString &Token, Synedithighlighter::TSynHighlighterAttributes* &Attri);
	bool __fastcall GetHighlighterAttriAtRowColEx(const Synedittypes::TBufferCoord &XY, System::UnicodeString &Token, int &TokenType, int &Start, Synedithighlighter::TSynHighlighterAttributes* &Attri);
	bool __fastcall GetPositionOfMouse(/* out */ Synedittypes::TBufferCoord &aPos);
	System::UnicodeString __fastcall GetWordAtRowCol(const Synedittypes::TBufferCoord &XY);
	virtual void __fastcall GotoBookMark(int BookMark);
	virtual void __fastcall GotoLineAndCenter(int ALine);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual bool __fastcall IsWhiteChar(System::WideChar AChar);
	virtual bool __fastcall IsWordBreakChar(System::WideChar AChar);
	void __fastcall InsertBlock(const Synedittypes::TBufferCoord &BB, const Synedittypes::TBufferCoord &BE, System::WideChar * ChangeStr, bool AddToUndoList);
	Synedittypes::TBufferBlock __fastcall UnifiedSelection(void);
	void __fastcall DoBlockIndent(void);
	void __fastcall DoBlockUnindent(void);
	void __fastcall InvalidateGutter(void);
	void __fastcall InvalidateGutterLine(int aLine);
	void __fastcall InvalidateGutterLines(int FirstLine, int LastLine);
	void __fastcall InvalidateLine(int Line);
	void __fastcall InvalidateLines(int FirstLine, int LastLine);
	void __fastcall InvalidateSelection(void);
	bool __fastcall IsBookmark(int BookMark);
	bool __fastcall IsPointInSelection(const Synedittypes::TBufferCoord &Value);
	void __fastcall LockUndo(void);
	Synedittypes::TDisplayCoord __fastcall BufferToDisplayPos(const Synedittypes::TBufferCoord &p);
	Synedittypes::TBufferCoord __fastcall DisplayToBufferPos(const Synedittypes::TDisplayCoord &p);
	int __fastcall LineToRow(int aLine);
	int __fastcall RowToLine(int aRow);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall PasteFromClipboard(void);
	virtual Synedittypes::TBufferCoord __fastcall NextWordPos(void);
	virtual Synedittypes::TBufferCoord __fastcall NextWordPosEx(const Synedittypes::TBufferCoord &XY);
	virtual Synedittypes::TBufferCoord __fastcall WordStart(void);
	virtual Synedittypes::TBufferCoord __fastcall WordStartEx(const Synedittypes::TBufferCoord &XY);
	virtual Synedittypes::TBufferCoord __fastcall WordEnd(void);
	virtual Synedittypes::TBufferCoord __fastcall WordEndEx(const Synedittypes::TBufferCoord &XY);
	virtual Synedittypes::TBufferCoord __fastcall PrevWordPos(void);
	virtual Synedittypes::TBufferCoord __fastcall PrevWordPosEx(const Synedittypes::TBufferCoord &XY);
	Synedittypes::TDisplayCoord __fastcall PixelsToRowColumn(int aX, int aY);
	Synedittypes::TDisplayCoord __fastcall PixelsToNearestRowColumn(int aX, int aY);
	void __fastcall Redo(void);
	void __fastcall RegisterCommandHandler(const THookedCommandEvent AHandlerProc, void * AHandlerData);
	System::Types::TPoint __fastcall RowColumnToPixels(const Synedittypes::TDisplayCoord &RowCol);
	int __fastcall RowColToCharIndex(const Synedittypes::TBufferCoord &RowCol);
	int __fastcall SearchReplace(const System::UnicodeString ASearch, const System::UnicodeString AReplace, Synedittypes::TSynSearchOptions AOptions);
	void __fastcall SelectAll(void);
	void __fastcall SetBookMark(int BookMark, int X, int Y);
	void __fastcall SetCaretAndSelection(const Synedittypes::TBufferCoord &ptCaret, const Synedittypes::TBufferCoord &ptBefore, const Synedittypes::TBufferCoord &ptAfter);
	virtual void __fastcall SetDefaultKeystrokes(void);
	void __fastcall SetSelWord(void);
	void __fastcall SetWordBlock(const Synedittypes::TBufferCoord &Value);
	void __fastcall Undo(void);
	void __fastcall UnlockUndo(void);
	void __fastcall UnregisterCommandHandler(THookedCommandEvent AHandlerProc);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	virtual void __fastcall SetFocus(void);
	void __fastcall AddKeyUpHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall RemoveKeyUpHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall AddKeyDownHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall RemoveKeyDownHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall AddKeyPressHandler(Synedittypes::TKeyPressWEvent aHandler);
	void __fastcall RemoveKeyPressHandler(Synedittypes::TKeyPressWEvent aHandler);
	void __fastcall AddFocusControl(Vcl::Controls::TWinControl* aControl);
	void __fastcall RemoveFocusControl(Vcl::Controls::TWinControl* aControl);
	void __fastcall AddMouseDownHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall RemoveMouseDownHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall AddMouseUpHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall RemoveMouseUpHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall AddMouseCursorHandler(TMouseCursorEvent aHandler);
	void __fastcall RemoveMouseCursorHandler(TMouseCursorEvent aHandler);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Msg);
	void __fastcall SetLinesPointer(TCustomSynEdit* ASynEdit);
	void __fastcall RemoveLinesPointer(void);
	void __fastcall HookTextBuffer(Synedittextbuffer::TSynEditStringList* aBuffer, Synedittextbuffer::TSynEditUndoList* aUndo, Synedittextbuffer::TSynEditUndoList* aRedo);
	void __fastcall UnHookTextBuffer(void);
	__property System::Sysutils::TSysCharSet AdditionalIdentChars = {read=FAdditionalIdentChars, write=SetAdditionalIdentChars};
	__property System::Sysutils::TSysCharSet AdditionalWordBreakChars = {read=FAdditionalWordBreakChars, write=SetAdditionalWordBreakChars};
	__property Synedittypes::TBufferCoord BlockBegin = {read=GetBlockBegin, write=SetBlockBegin};
	__property Synedittypes::TBufferCoord BlockEnd = {read=GetBlockEnd, write=SetBlockEnd};
	__property bool CanPaste = {read=GetCanPaste, nodefault};
	__property bool CanRedo = {read=GetCanRedo, nodefault};
	__property bool CanUndo = {read=GetCanUndo, nodefault};
	__property int CaretX = {read=fCaretX, write=SetCaretX, nodefault};
	__property int CaretY = {read=fCaretY, write=SetCaretY, nodefault};
	__property Synedittypes::TBufferCoord CaretXY = {read=GetCaretXY, write=SetCaretXY};
	__property System::Uitypes::TColor ActiveLineColor = {read=fActiveLineColor, write=SetActiveLineColor, default=536870911};
	__property int DisplayX = {read=GetDisplayX, nodefault};
	__property int DisplayY = {read=GetDisplayY, nodefault};
	__property Synedittypes::TDisplayCoord DisplayXY = {read=GetDisplayXY};
	__property int DisplayLineCount = {read=GetDisplayLineCount, nodefault};
	__property int CharsInWindow = {read=fCharsInWindow, nodefault};
	__property int CharWidth = {read=fCharWidth, nodefault};
	__property Color = {default=-16777211};
	__property Vcl::Graphics::TFont* Font = {read=GetFont, write=SetFont};
	__property Synedithighlighter::TSynCustomHighlighter* Highlighter = {read=fHighlighter, write=SetHighlighter};
	__property int LeftChar = {read=fLeftChar, write=SetLeftChar, nodefault};
	__property int LineHeight = {read=fTextHeight, nodefault};
	__property int LinesInWindow = {read=fLinesInWindow, nodefault};
	__property System::UnicodeString LineText = {read=GetLineText, write=SetLineText};
	__property System::Classes::TStrings* Lines = {read=fLines, write=SetLines};
	__property TSynEditMarkList* Marks = {read=fMarkList};
	__property int MaxScrollWidth = {read=fMaxScrollWidth, write=SetMaxScrollWidth, default=1024};
	__property bool Modified = {read=fModified, write=SetModified, nodefault};
	__property int PaintLock = {read=fPaintLock, nodefault};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property Syneditmiscclasses::TSynEditSearchCustom* SearchEngine = {read=fSearchEngine, write=SetSearchEngine};
	__property bool SelAvail = {read=GetSelAvail, nodefault};
	__property int SelLength = {read=GetSelLength, write=SetSelLength, nodefault};
	__property bool SelTabBlock = {read=GetSelTabBlock, nodefault};
	__property bool SelTabLine = {read=GetSelTabLine, nodefault};
	__property System::UnicodeString SelText = {read=GetSelText, write=SetSelTextExternal};
	__property TSynStateFlags StateFlags = {read=fStateFlags, nodefault};
	__property System::UnicodeString Text = {read=SynGetText, write=SynSetText};
	__property int TopLine = {read=fTopLine, write=SetTopLine, nodefault};
	__property System::UnicodeString WordAtCursor = {read=GetWordAtCursor};
	__property System::UnicodeString WordAtMouse = {read=GetWordAtMouse};
	__property Synedittextbuffer::TSynEditUndoList* UndoList = {read=fUndoList};
	__property Synedittextbuffer::TSynEditUndoList* RedoList = {read=fRedoList};
	__property TProcessCommandEvent OnProcessCommand = {read=fOnProcessCommand, write=fOnProcessCommand};
	__property Syneditmiscclasses::TSynBookMarkOpt* BookMarkOptions = {read=fBookMarkOpt, write=fBookMarkOpt};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=fBorderStyle, write=SetBorderStyle, default=1};
	__property int ExtraLineSpacing = {read=fExtraLineSpacing, write=SetExtraLineSpacing, default=0};
	__property Syneditmiscclasses::TSynGutter* Gutter = {read=fGutter, write=SetGutter};
	__property bool HideSelection = {read=fHideSelection, write=SetHideSelection, default=0};
	__property TSynEditCaretType InsertCaret = {read=fInsertCaret, write=SetInsertCaret, default=0};
	__property bool InsertMode = {read=fInserting, write=SetInsertMode, default=1};
	__property bool IsScrolling = {read=FIsScrolling, nodefault};
	__property Syneditkeycmds::TSynEditKeyStrokes* Keystrokes = {read=fKeyStrokes, write=SetKeystrokes, stored=false};
	__property int MaxUndo = {read=GetMaxUndo, write=SetMaxUndo, default=1024};
	__property TSynEditorOptions Options = {read=GetOptions, write=SetOptions, default=40632722};
	__property TSynEditCaretType OverwriteCaret = {read=fOverwriteCaret, write=SetOverwriteCaret, default=3};
	__property int RightEdge = {read=fRightEdge, write=SetRightEdge, default=80};
	__property System::Uitypes::TColor RightEdgeColor = {read=fRightEdgeColor, write=SetRightEdgeColor, default=12632256};
	__property System::Uitypes::TColor ScrollHintColor = {read=fScrollHintColor, write=fScrollHintColor, default=-16777192};
	__property TScrollHintFormat ScrollHintFormat = {read=fScrollHintFormat, write=fScrollHintFormat, default=0};
	__property System::Uitypes::TScrollStyle ScrollBars = {read=FScrollBars, write=SetScrollBars, default=3};
	__property Syneditmiscclasses::TSynSelectedColor* SelectedColor = {read=fSelectedColor, write=fSelectedColor};
	__property Synedittypes::TSynSelectionMode SelectionMode = {read=fSelectionMode, write=SetSelectionMode, default=0};
	__property Synedittypes::TSynSelectionMode ActiveSelectionMode = {read=fActiveSelectionMode, write=SetActiveSelectionMode, stored=false, nodefault};
	__property int TabWidth = {read=fTabWidth, write=SetTabWidth, default=8};
	__property bool WantReturns = {read=fWantReturns, write=SetWantReturns, default=1};
	__property bool WantTabs = {read=fWantTabs, write=SetWantTabs, default=0};
	__property bool WordWrap = {read=GetWordWrap, write=SetWordWrap, default=0};
	__property Syneditmiscclasses::TSynGlyph* WordWrapGlyph = {read=fWordWrapGlyph, write=SetWordWrapGlyph};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
	__property TPlaceMarkEvent OnClearBookmark = {read=fOnClearMark, write=fOnClearMark};
	__property TProcessCommandEvent OnCommandProcessed = {read=fOnCommandProcessed, write=fOnCommandProcessed};
	__property TContextHelpEvent OnContextHelp = {read=fOnContextHelp, write=fOnContextHelp};
	__property TDropFilesEvent OnDropFiles = {read=fOnDropFiles, write=fOnDropFiles};
	__property TGutterClickEvent OnGutterClick = {read=fOnGutterClick, write=fOnGutterClick};
	__property TGutterGetTextEvent OnGutterGetText = {read=fOnGutterGetText, write=fOnGutterGetText};
	__property TGutterPaintEvent OnGutterPaint = {read=fOnGutterPaint, write=fOnGutterPaint};
	__property TMouseCursorEvent OnMouseCursor = {read=fOnMouseCursor, write=fOnMouseCursor};
	__property Synedittypes::TKeyPressWEvent OnKeyPress = {read=FOnKeyPressW, write=FOnKeyPressW};
	__property TPaintEvent OnPaint = {read=fOnPaint, write=fOnPaint};
	__property TPlaceMarkEvent OnPlaceBookmark = {read=fOnPlaceMark, write=fOnPlaceMark};
	__property TProcessCommandEvent OnProcessUserCommand = {read=fOnProcessUserCommand, write=fOnProcessUserCommand};
	__property TReplaceTextEvent OnReplaceText = {read=fOnReplaceText, write=fOnReplaceText};
	__property TSpecialLineColorsEvent OnSpecialLineColors = {read=fOnSpecialLineColors, write=fOnSpecialLineColors};
	__property TStatusChangeEvent OnStatusChange = {read=fOnStatusChange, write=fOnStatusChange};
	__property TPaintTransient OnPaintTransient = {read=fOnPaintTransient, write=fOnPaintTransient};
	__property TScrollEvent OnScroll = {read=fOnScroll, write=fOnScroll};
	
__published:
	__property Cursor = {default=-4};
	__property TCustomSynEditSearchNotFoundEvent OnSearchNotFound = {read=fSearchNotFound, write=fSearchNotFound};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomSynEdit(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TSynEdit : public TCustomSynEdit
{
	typedef TCustomSynEdit inherited;
	
__published:
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property Constraints;
	__property Color = {default=-16777211};
	__property ActiveLineColor = {default=536870911};
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property Enabled = {default=1};
	__property Font;
	__property Height;
	__property Name = {default=0};
	__property ParentColor = {default=0};
	__property ParentFont = {default=0};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property Width;
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDock;
	__property OnStartDock;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnMouseWheel;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnStartDrag;
	__property BookMarkOptions;
	__property BorderStyle = {default=1};
	__property ExtraLineSpacing = {default=0};
	__property Gutter;
	__property HideSelection = {default=0};
	__property Highlighter;
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property InsertCaret = {default=0};
	__property InsertMode = {default=1};
	__property Keystrokes;
	__property Lines;
	__property MaxScrollWidth = {default=1024};
	__property MaxUndo = {default=1024};
	__property Options = {default=40632722};
	__property OverwriteCaret = {default=3};
	__property ReadOnly = {default=0};
	__property RightEdge = {default=80};
	__property RightEdgeColor = {default=12632256};
	__property ScrollHintColor = {default=-16777192};
	__property ScrollHintFormat = {default=0};
	__property ScrollBars = {default=3};
	__property SearchEngine;
	__property SelectedColor;
	__property SelectionMode = {default=0};
	__property TabWidth = {default=8};
	__property WantReturns = {default=1};
	__property WantTabs = {default=0};
	__property WordWrap = {default=0};
	__property WordWrapGlyph;
	__property OnChange;
	__property OnClearBookmark;
	__property OnCommandProcessed;
	__property OnContextHelp;
	__property OnDropFiles;
	__property OnGutterClick;
	__property OnGutterGetText;
	__property OnGutterPaint;
	__property OnMouseCursor;
	__property OnPaint;
	__property OnPlaceBookmark;
	__property OnProcessCommand;
	__property OnProcessUserCommand;
	__property OnReplaceText;
	__property OnScroll;
	__property OnSpecialLineColors;
	__property OnStatusChange;
	__property OnPaintTransient;
	__property FontSmoothing;
public:
	/* TCustomSynEdit.Create */ inline __fastcall virtual TSynEdit(System::Classes::TComponent* AOwner) : TCustomSynEdit(AOwner) { }
	/* TCustomSynEdit.Destroy */ inline __fastcall virtual ~TSynEdit(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSynEdit(HWND ParentWindow) : TCustomSynEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word MAX_SCROLL = System::Word(0x7fff);
static const System::Int8 MAX_MARKS = System::Int8(0x10);
#define SYNEDIT_CLIPBOARD_FORMAT L"SynEdit Control Block Type"
extern DELPHI_PACKAGE unsigned SynEditClipboardFormat;
#define SYNEDIT_DEFAULT_OPTIONS (System::Set<TSynEditorOption, TSynEditorOption::eoAltSetsColumnMode, TSynEditorOption::eoTrimTrailingSpaces>() << TSynEditorOption::eoAutoIndent << TSynEditorOption::eoDragDropEditing << TSynEditorOption::eoEnhanceEndKey << TSynEditorOption::eoGroupUndo << TSynEditorOption::eoScrollPastEol << TSynEditorOption::eoShowScrollHint << TSynEditorOption::eoSmartTabDelete << TSynEditorOption::eoSmartTabs << TSynEditorOption::eoTabsToSpaces )
}	/* namespace Synedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDIT)
using namespace Synedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditHPP
