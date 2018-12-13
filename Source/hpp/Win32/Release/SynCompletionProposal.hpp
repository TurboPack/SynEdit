// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynCompletionProposal.pas' rev: 33.00 (Windows)

#ifndef SyncompletionproposalHPP
#define SyncompletionproposalHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Dialogs.hpp>
#include <SynEditTypes.hpp>
#include <SynEditKeyCmds.hpp>
#include <SynEditHighlighter.hpp>
#include <SynEditKbdHandler.hpp>
#include <SynEdit.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syncompletionproposal
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynBaseCompletionProposalForm;
class DELPHICLASS TSynBaseCompletionProposal;
class DELPHICLASS TSynCompletionProposal;
class DELPHICLASS TSynAutoComplete;
class DELPHICLASS TProposalColumn;
class DELPHICLASS TProposalColumns;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM SynCompletionType : unsigned char { ctCode, ctHint, ctParams };

typedef Vcl::Forms::TCustomForm TSynForm;

typedef void __fastcall (__closure *TSynBaseCompletionProposalPaintItem)(System::TObject* Sender, int Index, Vcl::Graphics::TCanvas* TargetCanvas, const System::Types::TRect &ItemRect, bool &CustomDraw);

typedef void __fastcall (__closure *TSynBaseCompletionProposalMeasureItem)(System::TObject* Sender, int Index, Vcl::Graphics::TCanvas* TargetCanvas, int &ItemWidth);

typedef void __fastcall (__closure *TCodeCompletionEvent)(System::TObject* Sender, System::UnicodeString &Value, System::Classes::TShiftState Shift, int Index, System::WideChar EndToken);

typedef void __fastcall (__closure *TAfterCodeCompletionEvent)(System::TObject* Sender, const System::UnicodeString Value, System::Classes::TShiftState Shift, int Index, System::WideChar EndToken);

typedef void __fastcall (__closure *TValidateEvent)(System::TObject* Sender, System::Classes::TShiftState Shift, System::WideChar EndToken);

typedef void __fastcall (__closure *TCompletionParameter)(System::TObject* Sender, int CurrentIndex, int &Level, int &IndexToDisplay, System::WideChar &Key, System::UnicodeString &DisplayString);

typedef void __fastcall (__closure *TCompletionExecute)(SynCompletionType Kind, System::TObject* Sender, System::UnicodeString &CurrentInput, int &x, int &y, bool &CanExecute);

typedef void __fastcall (__closure *TCompletionChange)(System::TObject* Sender, int AIndex);

enum DECLSPEC_DENUM TSynCompletionOption : unsigned char { scoCaseSensitive, scoLimitToMatchedText, scoTitleIsCentered, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoConsiderWordBreakChars, scoCompleteWithTab, scoCompleteWithEnter };

typedef System::Set<TSynCompletionOption, TSynCompletionOption::scoCaseSensitive, TSynCompletionOption::scoCompleteWithEnter> TSynCompletionOptions;

class PASCALIMPLEMENTATION TSynBaseCompletionProposalForm : public Vcl::Forms::TCustomForm
{
	typedef Vcl::Forms::TCustomForm inherited;
	
private:
	System::UnicodeString FCurrentString;
	Synedittypes::TKeyPressWEvent FOnKeyPress;
	TSynBaseCompletionProposalPaintItem FOnPaintItem;
	TSynBaseCompletionProposalMeasureItem FOnMeasureItem;
	TCompletionChange FOnChangePosition;
	System::Classes::TStrings* FItemList;
	System::Classes::TStrings* FInsertList;
	System::Classes::TStrings* FAssignedList;
	int FPosition;
	int FLinesInWindow;
	int FTitleFontHeight;
	int FFontHeight;
	Vcl::Stdctrls::TScrollBar* FScrollbar;
	TValidateEvent FOnValidate;
	System::Classes::TNotifyEvent FOnCancel;
	System::Uitypes::TColor FClSelect;
	System::Uitypes::TColor fClSelectText;
	System::Uitypes::TColor FClTitleBackground;
	System::Uitypes::TColor fClBackGround;
	Vcl::Graphics::TBitmap* Bitmap;
	Vcl::Graphics::TBitmap* TitleBitmap;
	Synedit::TCustomSynEdit* FCurrentEditor;
	System::UnicodeString FTitle;
	Vcl::Graphics::TFont* FTitleFont;
	Vcl::Graphics::TFont* FFont;
	bool FResizeable;
	int FItemHeight;
	int FMargin;
	int FEffectiveItemHeight;
	Vcl::Controls::TImageList* FImages;
	bool FCase;
	bool FMatchText;
	bool FFormattedText;
	bool FCenterTitle;
	bool FUseInsertList;
	bool FCompleteWithTab;
	bool FCompleteWithEnter;
	int FMouseWheelAccumulator;
	SynCompletionType FDisplayKind;
	TCompletionParameter FParameterToken;
	int FCurrentIndex;
	int FCurrentLevel;
	SynCompletionType FDefaultKind;
	System::UnicodeString FEndOfTokenChr;
	System::UnicodeString FTriggerChars;
	bool OldShowCaret;
	int FHeightBuffer;
	TProposalColumns* FColumns;
	void __fastcall SetCurrentString(const System::UnicodeString Value);
	void __fastcall MoveLine(int cnt);
	void __fastcall ScrollbarOnChange(System::TObject* Sender);
	void __fastcall ScrollbarOnScroll(System::TObject* Sender, System::Uitypes::TScrollCode ScrollCode, int &ScrollPos);
	void __fastcall ScrollbarOnEnter(System::TObject* Sender);
	void __fastcall SetItemList(System::Classes::TStrings* const Value);
	void __fastcall SetInsertList(System::Classes::TStrings* const Value);
	HIDESBASE void __fastcall SetPosition(const int Value);
	void __fastcall SetResizeable(const bool Value);
	void __fastcall SetItemHeight(const int Value);
	void __fastcall SetImages(Vcl::Controls::TImageList* const Value);
	void __fastcall StringListChange(System::TObject* Sender);
	void __fastcall DoDoubleClick(System::TObject* Sender);
	void __fastcall DoFormShow(System::TObject* Sender);
	void __fastcall DoFormHide(System::TObject* Sender);
	void __fastcall AdjustScrollBarPosition();
	void __fastcall AdjustMetrics();
	void __fastcall SetTitle(const System::UnicodeString Value);
	HIDESBASE void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetTitleFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetColumns(TProposalColumns* Value);
	void __fastcall TitleFontChange(System::TObject* Sender);
	void __fastcall FontChange(System::TObject* Sender);
	void __fastcall RecalcItemHeight();
	bool __fastcall IsWordBreakChar(System::WideChar AChar);
	
protected:
	void __fastcall DoKeyPressW(System::WideChar Key);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall KeyPressW(System::WideChar &Key);
	DYNAMIC void __fastcall Paint();
	DYNAMIC void __fastcall Activate();
	DYNAMIC void __fastcall Deactivate();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall Resize();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMActivate(Winapi::Messages::TWMActivate &Message);
	MESSAGE void __fastcall WMEraseBackgrnd(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Message);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	virtual bool __fastcall CanResize(int &NewWidth, int &NewHeight);
	
public:
	__fastcall virtual TSynBaseCompletionProposalForm(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynBaseCompletionProposalForm();
	int __fastcall LogicalToPhysicalIndex(int Index);
	int __fastcall PhysicalToLogicalIndex(int Index);
	__property SynCompletionType DisplayType = {read=FDisplayKind, write=FDisplayKind, nodefault};
	__property SynCompletionType DefaultType = {read=FDefaultKind, write=FDefaultKind, default=0};
	__property System::UnicodeString CurrentString = {read=FCurrentString, write=SetCurrentString};
	__property int CurrentIndex = {read=FCurrentIndex, write=FCurrentIndex, nodefault};
	__property int CurrentLevel = {read=FCurrentLevel, write=FCurrentLevel, nodefault};
	__property TCompletionParameter OnParameterToken = {read=FParameterToken, write=FParameterToken};
	__property Synedittypes::TKeyPressWEvent OnKeyPress = {read=FOnKeyPress, write=FOnKeyPress};
	__property TSynBaseCompletionProposalPaintItem OnPaintItem = {read=FOnPaintItem, write=FOnPaintItem};
	__property TSynBaseCompletionProposalMeasureItem OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
	__property TValidateEvent OnValidate = {read=FOnValidate, write=FOnValidate};
	__property System::Classes::TNotifyEvent OnCancel = {read=FOnCancel, write=FOnCancel};
	__property System::Classes::TStrings* ItemList = {read=FItemList, write=SetItemList};
	__property System::Classes::TStrings* InsertList = {read=FInsertList, write=SetInsertList};
	__property System::Classes::TStrings* AssignedList = {read=FAssignedList, write=FAssignedList};
	__property int Position = {read=FPosition, write=SetPosition, nodefault};
	__property System::UnicodeString Title = {read=FTitle, write=SetTitle};
	__property System::Uitypes::TColor ClSelect = {read=FClSelect, write=FClSelect, default=-16777203};
	__property System::Uitypes::TColor ClSelectedText = {read=fClSelectText, write=fClSelectText, default=-16777202};
	__property System::Uitypes::TColor ClBackground = {read=fClBackGround, write=fClBackGround, default=-16777211};
	__property System::Uitypes::TColor ClTitleBackground = {read=FClTitleBackground, write=FClTitleBackground, default=-16777201};
	__property int ItemHeight = {read=FItemHeight, write=SetItemHeight, default=0};
	__property int Margin = {read=FMargin, write=FMargin, default=2};
	__property bool UsePrettyText = {read=FFormattedText, write=FFormattedText, default=0};
	__property bool UseInsertList = {read=FUseInsertList, write=FUseInsertList, default=0};
	__property bool CenterTitle = {read=FCenterTitle, write=FCenterTitle, default=1};
	__property bool CaseSensitive = {read=FCase, write=FCase, default=0};
	__property Synedit::TCustomSynEdit* CurrentEditor = {read=FCurrentEditor, write=FCurrentEditor};
	__property bool MatchText = {read=FMatchText, write=FMatchText, nodefault};
	__property System::UnicodeString EndOfTokenChr = {read=FEndOfTokenChr, write=FEndOfTokenChr};
	__property System::UnicodeString TriggerChars = {read=FTriggerChars, write=FTriggerChars};
	__property bool CompleteWithTab = {read=FCompleteWithTab, write=FCompleteWithTab, nodefault};
	__property bool CompleteWithEnter = {read=FCompleteWithEnter, write=FCompleteWithEnter, nodefault};
	__property Vcl::Graphics::TFont* TitleFont = {read=FTitleFont, write=SetTitleFont};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property TProposalColumns* Columns = {read=FColumns, write=SetColumns};
	__property bool Resizeable = {read=FResizeable, write=SetResizeable, default=1};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TSynBaseCompletionProposalForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TCustomForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSynBaseCompletionProposalForm(HWND ParentWindow) : Vcl::Forms::TCustomForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TSynBaseCompletionProposal : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TSynBaseCompletionProposalForm* FForm;
	TCompletionExecute FOnExecute;
	System::Classes::TNotifyEvent FOnClose;
	System::Classes::TNotifyEvent FOnShow;
	int FWidth;
	System::UnicodeString FPreviousToken;
	int FDotOffset;
	TSynCompletionOptions FOptions;
	int FNbLinesInWindow;
	bool FCanExecute;
	System::Uitypes::TColor __fastcall GetClSelect();
	void __fastcall SetClSelect(const System::Uitypes::TColor Value);
	System::UnicodeString __fastcall GetCurrentString();
	System::Classes::TStrings* __fastcall GetItemList();
	System::Classes::TStrings* __fastcall GetInsertList();
	System::Classes::TNotifyEvent __fastcall GetOnCancel();
	Synedittypes::TKeyPressWEvent __fastcall GetOnKeyPress();
	TSynBaseCompletionProposalPaintItem __fastcall GetOnPaintItem();
	TSynBaseCompletionProposalMeasureItem __fastcall GetOnMeasureItem();
	TValidateEvent __fastcall GetOnValidate();
	int __fastcall GetPosition();
	void __fastcall SetCurrentString(const System::UnicodeString Value);
	void __fastcall SetItemList(System::Classes::TStrings* const Value);
	void __fastcall SetInsertList(System::Classes::TStrings* const Value);
	void __fastcall SetNbLinesInWindow(const int Value);
	void __fastcall SetOnCancel(const System::Classes::TNotifyEvent Value);
	void __fastcall SetOnKeyPress(const Synedittypes::TKeyPressWEvent Value);
	void __fastcall SetOnPaintItem(const TSynBaseCompletionProposalPaintItem Value);
	void __fastcall SetOnMeasureItem(const TSynBaseCompletionProposalMeasureItem Value);
	void __fastcall SetPosition(const int Value);
	void __fastcall SetOnValidate(const TValidateEvent Value);
	void __fastcall SetWidth(int Value);
	void __fastcall SetImages(Vcl::Controls::TImageList* const Value);
	SynCompletionType __fastcall GetDisplayKind();
	void __fastcall SetDisplayKind(const SynCompletionType Value);
	TCompletionParameter __fastcall GetParameterToken();
	void __fastcall SetParameterToken(const TCompletionParameter Value);
	SynCompletionType __fastcall GetDefaultKind();
	void __fastcall SetDefaultKind(const SynCompletionType Value);
	System::Uitypes::TColor __fastcall GetClBack();
	void __fastcall SetClBack(const System::Uitypes::TColor Value);
	System::Uitypes::TColor __fastcall GetClSelectedText();
	void __fastcall SetClSelectedText(const System::Uitypes::TColor Value);
	System::UnicodeString __fastcall GetEndOfTokenChar();
	void __fastcall SetEndOfTokenChar(const System::UnicodeString Value);
	System::Uitypes::TColor __fastcall GetClTitleBackground();
	void __fastcall SetClTitleBackground(const System::Uitypes::TColor Value);
	void __fastcall SetTitle(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetTitle();
	Vcl::Graphics::TFont* __fastcall GetFont();
	Vcl::Graphics::TFont* __fastcall GetTitleFont();
	void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetTitleFont(Vcl::Graphics::TFont* const Value);
	TSynCompletionOptions __fastcall GetOptions();
	System::UnicodeString __fastcall GetTriggerChars();
	void __fastcall SetTriggerChars(const System::UnicodeString Value);
	TCompletionChange __fastcall GetOnChange();
	void __fastcall SetOnChange(const TCompletionChange Value);
	void __fastcall SetColumns(TProposalColumns* const Value);
	TProposalColumns* __fastcall GetColumns();
	bool __fastcall GetResizeable();
	void __fastcall SetResizeable(const bool Value);
	int __fastcall GetItemHeight();
	void __fastcall SetItemHeight(const int Value);
	int __fastcall GetMargin();
	void __fastcall SetMargin(const int Value);
	Vcl::Controls::TImageList* __fastcall GetImages();
	bool __fastcall IsWordBreakChar(System::WideChar AChar);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall SetOptions(const TSynCompletionOptions Value);
	virtual void __fastcall EditorCancelMode(System::TObject* Sender);
	virtual void __fastcall HookedEditorCommand(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData);
	
public:
	__fastcall virtual TSynBaseCompletionProposal(System::Classes::TComponent* Aowner);
	void __fastcall Execute(System::UnicodeString s, int x, int y);
	virtual void __fastcall ExecuteEx(System::UnicodeString s, int x, int y, SynCompletionType Kind = (SynCompletionType)(0x0));
	void __fastcall Activate();
	void __fastcall Deactivate();
	void __fastcall ClearList();
	System::UnicodeString __fastcall DisplayItem(int AIndex);
	System::UnicodeString __fastcall InsertItem(int AIndex);
	void __fastcall AddItemAt(int Where, System::UnicodeString ADisplayText, System::UnicodeString AInsertText);
	void __fastcall AddItem(System::UnicodeString ADisplayText, System::UnicodeString AInsertText);
	void __fastcall ResetAssignedList();
	__property Synedittypes::TKeyPressWEvent OnKeyPress = {read=GetOnKeyPress, write=SetOnKeyPress};
	__property TValidateEvent OnValidate = {read=GetOnValidate, write=SetOnValidate};
	__property System::Classes::TNotifyEvent OnCancel = {read=GetOnCancel, write=SetOnCancel};
	__property System::UnicodeString CurrentString = {read=GetCurrentString, write=SetCurrentString};
	__property int DotOffset = {read=FDotOffset, write=FDotOffset, nodefault};
	__property SynCompletionType DisplayType = {read=GetDisplayKind, write=SetDisplayKind, nodefault};
	__property TSynBaseCompletionProposalForm* Form = {read=FForm};
	__property System::UnicodeString PreviousToken = {read=FPreviousToken};
	__property int Position = {read=GetPosition, write=SetPosition, nodefault};
	
__published:
	__property SynCompletionType DefaultType = {read=GetDefaultKind, write=SetDefaultKind, default=0};
	__property TSynCompletionOptions Options = {read=GetOptions, write=SetOptions, default=834};
	__property System::Classes::TStrings* ItemList = {read=GetItemList, write=SetItemList};
	__property System::Classes::TStrings* InsertList = {read=GetInsertList, write=SetInsertList};
	__property int NbLinesInWindow = {read=FNbLinesInWindow, write=SetNbLinesInWindow, default=8};
	__property System::Uitypes::TColor ClSelect = {read=GetClSelect, write=SetClSelect, default=-16777203};
	__property System::Uitypes::TColor ClSelectedText = {read=GetClSelectedText, write=SetClSelectedText, default=-16777202};
	__property System::Uitypes::TColor ClBackground = {read=GetClBack, write=SetClBack, default=-16777211};
	__property System::Uitypes::TColor ClTitleBackground = {read=GetClTitleBackground, write=SetClTitleBackground, default=-16777201};
	__property int Width = {read=FWidth, write=SetWidth, default=260};
	__property System::UnicodeString EndOfTokenChr = {read=GetEndOfTokenChar, write=SetEndOfTokenChar};
	__property System::UnicodeString TriggerChars = {read=GetTriggerChars, write=SetTriggerChars};
	__property System::UnicodeString Title = {read=GetTitle, write=SetTitle};
	__property Vcl::Graphics::TFont* Font = {read=GetFont, write=SetFont};
	__property Vcl::Graphics::TFont* TitleFont = {read=GetTitleFont, write=SetTitleFont};
	__property TProposalColumns* Columns = {read=GetColumns, write=SetColumns};
	__property bool Resizeable = {read=GetResizeable, write=SetResizeable, default=1};
	__property int ItemHeight = {read=GetItemHeight, write=SetItemHeight, default=0};
	__property Vcl::Controls::TImageList* Images = {read=GetImages, write=SetImages, default=0};
	__property int Margin = {read=GetMargin, write=SetMargin, default=2};
	__property TCompletionChange OnChange = {read=GetOnChange, write=SetOnChange};
	__property System::Classes::TNotifyEvent OnClose = {read=FOnClose, write=FOnClose};
	__property TCompletionExecute OnExecute = {read=FOnExecute, write=FOnExecute};
	__property TSynBaseCompletionProposalMeasureItem OnMeasureItem = {read=GetOnMeasureItem, write=SetOnMeasureItem};
	__property TSynBaseCompletionProposalPaintItem OnPaintItem = {read=GetOnPaintItem, write=SetOnPaintItem};
	__property TCompletionParameter OnParameterToken = {read=GetParameterToken, write=SetParameterToken};
	__property System::Classes::TNotifyEvent OnShow = {read=FOnShow, write=FOnShow};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TSynBaseCompletionProposal() { }
	
};


class PASCALIMPLEMENTATION TSynCompletionProposal : public TSynBaseCompletionProposal
{
	typedef TSynBaseCompletionProposal inherited;
	
private:
	System::Classes::TList* fEditors;
	System::Classes::TShortCut FShortCut;
	bool FNoNextKey;
	int FCompletionStart;
	bool FAdjustCompletionStart;
	TCodeCompletionEvent FOnCodeCompletion;
	Vcl::Extctrls::TTimer* FTimer;
	int FTimerInterval;
	Synedit::TCustomSynEdit* FEditor;
	TAfterCodeCompletionEvent FOnAfterCodeCompletion;
	System::Classes::TNotifyEvent FOnCancelled;
	void __fastcall SetEditor(Synedit::TCustomSynEdit* const Value);
	void __fastcall HandleOnCancel(System::TObject* Sender);
	void __fastcall HandleOnValidate(System::TObject* Sender, System::Classes::TShiftState Shift, System::WideChar EndToken);
	void __fastcall HandleOnKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall HandleDblClick(System::TObject* Sender);
	void __fastcall EditorKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall EditorKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall TimerExecute(System::TObject* Sender);
	System::UnicodeString __fastcall GetPreviousToken(Synedit::TCustomSynEdit* AEditor);
	System::UnicodeString __fastcall GetCurrentInput(Synedit::TCustomSynEdit* AEditor);
	int __fastcall GetTimerInterval();
	void __fastcall SetTimerInterval(const int Value);
	Synedit::TCustomSynEdit* __fastcall GetEditor(int i);
	void __fastcall InternalCancelCompletion();
	
protected:
	virtual void __fastcall DoExecute(Synedit::TCustomSynEdit* AEditor);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetShortCut(System::Classes::TShortCut Value);
	virtual void __fastcall SetOptions(const TSynCompletionOptions Value);
	virtual void __fastcall EditorCancelMode(System::TObject* Sender);
	virtual void __fastcall HookedEditorCommand(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData);
	
public:
	__fastcall virtual TSynCompletionProposal(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynCompletionProposal();
	void __fastcall AddEditor(Synedit::TCustomSynEdit* AEditor);
	bool __fastcall RemoveEditor(Synedit::TCustomSynEdit* AEditor);
	int __fastcall EditorsCount();
	virtual void __fastcall ExecuteEx(System::UnicodeString s, int x, int y, SynCompletionType Kind = (SynCompletionType)(0x0));
	void __fastcall ActivateCompletion();
	void __fastcall CancelCompletion();
	void __fastcall ActivateTimer(Synedit::TCustomSynEdit* ACurrentEditor);
	void __fastcall DeactivateTimer();
	__property Synedit::TCustomSynEdit* Editors[int i] = {read=GetEditor};
	__property int CompletionStart = {read=FCompletionStart, write=FCompletionStart, nodefault};
	
__published:
	__property System::Classes::TShortCut ShortCut = {read=FShortCut, write=SetShortCut, nodefault};
	__property Synedit::TCustomSynEdit* Editor = {read=FEditor, write=SetEditor};
	__property int TimerInterval = {read=GetTimerInterval, write=SetTimerInterval, default=1000};
	__property TAfterCodeCompletionEvent OnAfterCodeCompletion = {read=FOnAfterCodeCompletion, write=FOnAfterCodeCompletion};
	__property System::Classes::TNotifyEvent OnCancelled = {read=FOnCancelled, write=FOnCancelled};
	__property TCodeCompletionEvent OnCodeCompletion = {read=FOnCodeCompletion, write=FOnCodeCompletion};
};


class PASCALIMPLEMENTATION TSynAutoComplete : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TShortCut FShortCut;
	Synedit::TCustomSynEdit* fEditor;
	System::Classes::TStrings* fAutoCompleteList;
	bool fNoNextKey;
	System::UnicodeString FEndOfTokenChr;
	System::Classes::TNotifyEvent FOnBeforeExecute;
	System::Classes::TNotifyEvent FOnAfterExecute;
	TSynCompletionProposal* FInternalCompletion;
	bool FDoLookup;
	TSynCompletionOptions FOptions;
	void __fastcall SetAutoCompleteList(System::Classes::TStrings* List);
	void __fastcall SetEditor(Synedit::TCustomSynEdit* const Value);
	void __fastcall SetDoLookup(const bool Value);
	void __fastcall CreateInternalCompletion();
	TSynCompletionOptions __fastcall GetOptions();
	void __fastcall SetOptions(const TSynCompletionOptions Value);
	void __fastcall DoInternalAutoCompletion(System::TObject* Sender, const System::UnicodeString Value, System::Classes::TShiftState Shift, int Index, System::WideChar EndToken);
	bool __fastcall GetExecuting();
	
protected:
	void __fastcall SetShortCut(System::Classes::TShortCut Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall EditorKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall EditorKeyPress(System::TObject* Sender, System::WideChar &Key);
	System::UnicodeString __fastcall GetPreviousToken(Synedit::TCustomSynEdit* Editor);
	
public:
	__fastcall virtual TSynAutoComplete(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynAutoComplete();
	void __fastcall Execute(System::UnicodeString Token, Synedit::TCustomSynEdit* Editor);
	void __fastcall ExecuteEx(System::UnicodeString Token, Synedit::TCustomSynEdit* Editor, bool LookupIfNotExact);
	System::UnicodeString __fastcall GetTokenList();
	System::UnicodeString __fastcall GetTokenValue(System::UnicodeString Token);
	void __fastcall CancelCompletion();
	__property bool Executing = {read=GetExecuting, nodefault};
	
__published:
	__property System::Classes::TStrings* AutoCompleteList = {read=fAutoCompleteList, write=SetAutoCompleteList};
	__property System::UnicodeString EndOfTokenChr = {read=FEndOfTokenChr, write=FEndOfTokenChr};
	__property Synedit::TCustomSynEdit* Editor = {read=fEditor, write=SetEditor};
	__property System::Classes::TShortCut ShortCut = {read=FShortCut, write=SetShortCut, nodefault};
	__property System::Classes::TNotifyEvent OnBeforeExecute = {read=FOnBeforeExecute, write=FOnBeforeExecute};
	__property System::Classes::TNotifyEvent OnAfterExecute = {read=FOnAfterExecute, write=FOnAfterExecute};
	__property bool DoLookupWhenNotExact = {read=FDoLookup, write=SetDoLookup, default=1};
	__property TSynCompletionOptions Options = {read=GetOptions, write=SetOptions, default=834};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TProposalColumn : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	int FColumnWidth;
	int FInternalWidth;
	System::Uitypes::TFontStyles FFontStyle;
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	
public:
	__fastcall virtual TProposalColumn(System::Classes::TCollection* Collection);
	__fastcall virtual ~TProposalColumn();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property int ColumnWidth = {read=FColumnWidth, write=FColumnWidth, nodefault};
	__property System::Uitypes::TFontStyles DefaultFontStyle = {read=FFontStyle, write=FFontStyle, default=0};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TProposalColumns : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TProposalColumn* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	HIDESBASE TProposalColumn* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TProposalColumn* Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TProposalColumns(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass);
	HIDESBASE TProposalColumn* __fastcall Add();
	HIDESBASE TProposalColumn* __fastcall FindItemID(int ID);
	HIDESBASE TProposalColumn* __fastcall Insert(int Index);
	__property TProposalColumn* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TProposalColumns() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define DefaultProposalOptions (System::Set<TSynCompletionOption, TSynCompletionOption::scoCaseSensitive, TSynCompletionOption::scoCompleteWithEnter>() << TSynCompletionOption::scoLimitToMatchedText << TSynCompletionOption::scoEndCharCompletion << TSynCompletionOption::scoCompleteWithTab << TSynCompletionOption::scoCompleteWithEnter )
#define DefaultEndOfTokenChr L"()[]. "
extern DELPHI_PACKAGE void __fastcall FormattedTextOut(Vcl::Graphics::TCanvas* TargetCanvas, const System::Types::TRect &Rect, const System::UnicodeString Text, bool Selected, TProposalColumns* Columns, Vcl::Controls::TImageList* Images);
extern DELPHI_PACKAGE int __fastcall FormattedTextWidth(Vcl::Graphics::TCanvas* TargetCanvas, const System::UnicodeString Text, TProposalColumns* Columns, Vcl::Controls::TImageList* Images);
extern DELPHI_PACKAGE System::UnicodeString __fastcall PrettyTextToFormattedString(const System::UnicodeString APrettyText, bool AlternateBoldStyle = false);
}	/* namespace Syncompletionproposal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNCOMPLETIONPROPOSAL)
using namespace Syncompletionproposal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyncompletionproposalHPP
