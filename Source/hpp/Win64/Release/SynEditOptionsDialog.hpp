// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditOptionsDialog.pas' rev: 32.00 (Windows)

#ifndef SyneditoptionsdialogHPP
#define SyneditoptionsdialogHPP

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
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Winapi.CommCtrl.hpp>
#include <System.Win.Registry.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Menus.hpp>
#include <SynEdit.hpp>
#include <SynEditHighlighter.hpp>
#include <SynEditMiscClasses.hpp>
#include <SynEditKeyCmds.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditoptionsdialog
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TfmEditorOptionsDialog;
class DELPHICLASS TSynEditOptionsDialog;
class DELPHICLASS TSynEditorOptionsContainer;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TColorPopup : unsigned char { cpGutter, cpRightEdge };

typedef void __fastcall (__closure *TSynEditorOptionsUserCommand)(int AUserCommand, System::UnicodeString &ADescription);

typedef void __fastcall (__closure *TSynEditorOptionsAllUserCommands)(System::Classes::TStrings* ACommands);

class PASCALIMPLEMENTATION TfmEditorOptionsDialog : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Comctrls::TPageControl* PageControl1;
	Vcl::Stdctrls::TButton* btnOk;
	Vcl::Stdctrls::TButton* btnCancel;
	Vcl::Comctrls::TTabSheet* Display;
	Vcl::Dialogs::TColorDialog* ColorDialog;
	Vcl::Menus::TPopupMenu* ColorPopup;
	Vcl::Menus::TMenuItem* None1;
	Vcl::Menus::TMenuItem* Scrollbar1;
	Vcl::Menus::TMenuItem* ActiveCaption1;
	Vcl::Menus::TMenuItem* Background1;
	Vcl::Menus::TMenuItem* InactiveCaption1;
	Vcl::Menus::TMenuItem* Menu1;
	Vcl::Menus::TMenuItem* Window1;
	Vcl::Menus::TMenuItem* WindowFrame1;
	Vcl::Menus::TMenuItem* MEnu2;
	Vcl::Menus::TMenuItem* WindowText1;
	Vcl::Menus::TMenuItem* CaptionText1;
	Vcl::Menus::TMenuItem* ActiveBorder1;
	Vcl::Menus::TMenuItem* InactiveBorder1;
	Vcl::Menus::TMenuItem* ApplicationWorkspace1;
	Vcl::Menus::TMenuItem* Highlight1;
	Vcl::Menus::TMenuItem* HighlightText1;
	Vcl::Menus::TMenuItem* ButtonFace1;
	Vcl::Menus::TMenuItem* ButtonShadow1;
	Vcl::Menus::TMenuItem* GrayText1;
	Vcl::Menus::TMenuItem* ButtonText1;
	Vcl::Menus::TMenuItem* InactiveCaptionText1;
	Vcl::Menus::TMenuItem* Highlight2;
	Vcl::Menus::TMenuItem* N3dDarkShadow1;
	Vcl::Menus::TMenuItem* N3DLight1;
	Vcl::Menus::TMenuItem* InfoTipText1;
	Vcl::Menus::TMenuItem* InfoTipBackground1;
	Vcl::Controls::TImageList* ImageList1;
	Vcl::Comctrls::TTabSheet* Options;
	Vcl::Comctrls::TTabSheet* Keystrokes;
	Vcl::Stdctrls::TGroupBox* gbBookmarks;
	Vcl::Stdctrls::TCheckBox* ckBookmarkKeys;
	Vcl::Stdctrls::TCheckBox* ckBookmarkVisible;
	Vcl::Stdctrls::TGroupBox* gbLineSpacing;
	Vcl::Stdctrls::TEdit* eLineSpacing;
	Vcl::Stdctrls::TGroupBox* gbGutter;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TCheckBox* ckGutterAutosize;
	Vcl::Stdctrls::TCheckBox* ckGutterShowLineNumbers;
	Vcl::Stdctrls::TCheckBox* ckGutterShowLeaderZeros;
	Vcl::Stdctrls::TCheckBox* ckGutterStartAtZero;
	Vcl::Stdctrls::TCheckBox* ckGutterVisible;
	Vcl::Stdctrls::TGroupBox* gbRightEdge;
	Vcl::Stdctrls::TLabel* Label3;
	Vcl::Extctrls::TPanel* pRightEdgeBack;
	Vcl::Stdctrls::TEdit* eRightEdge;
	Vcl::Stdctrls::TGroupBox* gbEditorFont;
	Vcl::Stdctrls::TButton* btnFont;
	Vcl::Stdctrls::TGroupBox* gbOptions;
	Vcl::Stdctrls::TCheckBox* ckAutoIndent;
	Vcl::Stdctrls::TCheckBox* ckDragAndDropEditing;
	Vcl::Stdctrls::TCheckBox* ckAutoSizeMaxWidth;
	Vcl::Stdctrls::TCheckBox* ckHalfPageScroll;
	Vcl::Stdctrls::TCheckBox* ckEnhanceEndKey;
	Vcl::Stdctrls::TCheckBox* ckScrollByOneLess;
	Vcl::Stdctrls::TCheckBox* ckScrollPastEOF;
	Vcl::Stdctrls::TCheckBox* ckScrollPastEOL;
	Vcl::Stdctrls::TCheckBox* ckShowScrollHint;
	Vcl::Stdctrls::TCheckBox* ckSmartTabs;
	Vcl::Stdctrls::TCheckBox* ckTabsToSpaces;
	Vcl::Stdctrls::TCheckBox* ckTrimTrailingSpaces;
	Vcl::Stdctrls::TCheckBox* ckWantTabs;
	Vcl::Stdctrls::TGroupBox* gbCaret;
	Vcl::Stdctrls::TComboBox* cInsertCaret;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TComboBox* cOverwriteCaret;
	Vcl::Extctrls::TPanel* Panel3;
	Vcl::Stdctrls::TLabel* labFont;
	Vcl::Dialogs::TFontDialog* FontDialog;
	Vcl::Stdctrls::TButton* btnAddKey;
	Vcl::Stdctrls::TButton* btnRemKey;
	Vcl::Stdctrls::TGroupBox* gbKeyStrokes;
	Vcl::Stdctrls::TLabel* Label5;
	Vcl::Stdctrls::TLabel* Label6;
	Vcl::Stdctrls::TLabel* Label7;
	Vcl::Stdctrls::TComboBox* cKeyCommand;
	Vcl::Stdctrls::TButton* btnUpdateKey;
	Vcl::Stdctrls::TCheckBox* ckAltSetsColumnMode;
	Vcl::Stdctrls::TCheckBox* ckKeepCaretX;
	Vcl::Stdctrls::TEdit* eTabWidth;
	Vcl::Extctrls::TPanel* pRightEdgeColor;
	Vcl::Stdctrls::TLabel* Label8;
	Vcl::Stdctrls::TLabel* Label9;
	Vcl::Stdctrls::TLabel* Label10;
	Vcl::Stdctrls::TCheckBox* cbGutterFont;
	Vcl::Stdctrls::TButton* btnGutterFont;
	Vcl::Extctrls::TPanel* btnRightEdge;
	Vcl::Extctrls::TImage* Image1;
	Vcl::Extctrls::TPanel* pGutterBack;
	Vcl::Extctrls::TPanel* pGutterColor;
	Vcl::Extctrls::TPanel* btnGutterColor;
	Vcl::Extctrls::TImage* Image2;
	Vcl::Stdctrls::TCheckBox* ckScrollHintFollows;
	Vcl::Stdctrls::TCheckBox* ckGroupUndo;
	Vcl::Stdctrls::TCheckBox* ckSmartTabDelete;
	Vcl::Stdctrls::TCheckBox* ckRightMouseMoves;
	Vcl::Extctrls::TPanel* pnlGutterFontDisplay;
	Vcl::Stdctrls::TLabel* lblGutterFont;
	Vcl::Stdctrls::TCheckBox* ckEnhanceHomeKey;
	Vcl::Extctrls::TPanel* pnlCommands;
	Vcl::Comctrls::TListView* KeyList;
	Vcl::Stdctrls::TCheckBox* ckHideShowScrollbars;
	Vcl::Stdctrls::TCheckBox* ckDisableScrollArrows;
	Vcl::Stdctrls::TCheckBox* ckShowSpecialChars;
	void __fastcall PopupMenuClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall pGutterColorClick(System::TObject* Sender);
	void __fastcall pRightEdgeColorClick(System::TObject* Sender);
	void __fastcall btnFontClick(System::TObject* Sender);
	void __fastcall KeyListSelectItem(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, bool Selected);
	void __fastcall btnUpdateKeyClick(System::TObject* Sender);
	void __fastcall btnAddKeyClick(System::TObject* Sender);
	void __fastcall btnRemKeyClick(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall KeyListEditing(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, bool &AllowEdit);
	void __fastcall btnOkClick(System::TObject* Sender);
	void __fastcall btnGutterFontClick(System::TObject* Sender);
	void __fastcall cbGutterFontClick(System::TObject* Sender);
	void __fastcall btnRightEdgeMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall btnGutterColorMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall cKeyCommandExit(System::TObject* Sender);
	void __fastcall cKeyCommandKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall cKeyCommandKeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall KeyListChanging(System::TObject* Sender, Vcl::Comctrls::TListItem* Item, Vcl::Comctrls::TItemChange Change, bool &AllowChange);
	
private:
	TSynEditorOptionsContainer* FSynEdit;
	TColorPopup FPoppedFrom;
	TSynEditorOptionsUserCommand FUserCommand;
	TSynEditorOptionsAllUserCommands FAllUserCommands;
	Vcl::Comctrls::TListItem* OldSelected;
	bool InChanging;
	bool FExtended;
	System::Uitypes::TColor __fastcall GetColor(Vcl::Menus::TMenuItem* Item);
	void __fastcall GetData(void);
	void __fastcall PutData(void);
	void __fastcall EditStrCallback(const System::UnicodeString S);
	void __fastcall FillInKeystrokeInfo(Syneditkeycmds::TSynEditKeyStroke* AKey, Vcl::Comctrls::TListItem* AItem);
	
public:
	Syneditmiscclasses::TSynHotKey* eKeyShort2;
	Syneditmiscclasses::TSynHotKey* eKeyShort1;
	bool __fastcall Execute(TSynEditorOptionsContainer* EditOptions);
	__property TSynEditorOptionsUserCommand GetUserCommandNames = {read=FUserCommand, write=FUserCommand};
	__property TSynEditorOptionsAllUserCommands GetAllUserCommands = {read=FAllUserCommands, write=FAllUserCommands};
	__property bool UseExtendedStrings = {read=FExtended, write=FExtended, nodefault};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TfmEditorOptionsDialog(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TfmEditorOptionsDialog(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TfmEditorOptionsDialog(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TfmEditorOptionsDialog(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TSynEditOptionsDialog : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TfmEditorOptionsDialog* FForm;
	TSynEditorOptionsUserCommand __fastcall GetUserCommandNames(void);
	void __fastcall SetUserCommandNames(const TSynEditorOptionsUserCommand Value);
	TSynEditorOptionsAllUserCommands __fastcall GetUserCommands(void);
	void __fastcall SetUserCommands(const TSynEditorOptionsAllUserCommands Value);
	bool __fastcall GetExtended(void);
	void __fastcall SetExtended(const bool Value);
	
public:
	__fastcall virtual TSynEditOptionsDialog(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynEditOptionsDialog(void);
	bool __fastcall Execute(TSynEditorOptionsContainer* EditOptions);
	__property TfmEditorOptionsDialog* Form = {read=FForm};
	
__published:
	__property TSynEditorOptionsUserCommand GetUserCommand = {read=GetUserCommandNames, write=SetUserCommandNames};
	__property TSynEditorOptionsAllUserCommands GetAllUserCommands = {read=GetUserCommands, write=SetUserCommands};
	__property bool UseExtendedStrings = {read=GetExtended, write=SetExtended, nodefault};
};


class PASCALIMPLEMENTATION TSynEditorOptionsContainer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FHideSelection;
	bool FWantTabs;
	int FMaxUndo;
	int FExtraLineSpacing;
	int FTabWidth;
	int FMaxScrollWidth;
	int FRightEdge;
	Syneditmiscclasses::TSynSelectedColor* FSelectedColor;
	System::Uitypes::TColor FRightEdgeColor;
	Vcl::Graphics::TFont* FFont;
	Syneditmiscclasses::TSynBookMarkOpt* FBookmarks;
	Synedit::TSynEditCaretType FOverwriteCaret;
	Synedit::TSynEditCaretType FInsertCaret;
	Syneditkeycmds::TSynEditKeyStrokes* FKeystrokes;
	Synedit::TSynEditorOptions FOptions;
	Syneditmiscclasses::TSynGutter* FSynGutter;
	System::Uitypes::TColor FColor;
	void __fastcall SetBookMarks(Syneditmiscclasses::TSynBookMarkOpt* const Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetKeystrokes(Syneditkeycmds::TSynEditKeyStrokes* const Value);
	void __fastcall SetOptions(const Synedit::TSynEditorOptions Value);
	void __fastcall SetSynGutter(Syneditmiscclasses::TSynGutter* const Value);
	
public:
	__fastcall virtual TSynEditorOptionsContainer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynEditorOptionsContainer(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property Synedit::TSynEditorOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property Syneditmiscclasses::TSynBookMarkOpt* BookMarkOptions = {read=FBookmarks, write=SetBookMarks};
	__property System::Uitypes::TColor Color = {read=FColor, write=FColor, nodefault};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property int ExtraLineSpacing = {read=FExtraLineSpacing, write=FExtraLineSpacing, nodefault};
	__property Syneditmiscclasses::TSynGutter* Gutter = {read=FSynGutter, write=SetSynGutter};
	__property int RightEdge = {read=FRightEdge, write=FRightEdge, nodefault};
	__property System::Uitypes::TColor RightEdgeColor = {read=FRightEdgeColor, write=FRightEdgeColor, nodefault};
	__property bool WantTabs = {read=FWantTabs, write=FWantTabs, nodefault};
	__property Synedit::TSynEditCaretType InsertCaret = {read=FInsertCaret, write=FInsertCaret, nodefault};
	__property Synedit::TSynEditCaretType OverwriteCaret = {read=FOverwriteCaret, write=FOverwriteCaret, nodefault};
	__property bool HideSelection = {read=FHideSelection, write=FHideSelection, nodefault};
	__property int MaxScrollWidth = {read=FMaxScrollWidth, write=FMaxScrollWidth, nodefault};
	__property int MaxUndo = {read=FMaxUndo, write=FMaxUndo, nodefault};
	__property Syneditmiscclasses::TSynSelectedColor* SelectedColor = {read=FSelectedColor, write=FSelectedColor};
	__property int TabWidth = {read=FTabWidth, write=FTabWidth, nodefault};
	__property Syneditkeycmds::TSynEditKeyStrokes* Keystrokes = {read=FKeystrokes, write=SetKeystrokes};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditoptionsdialog */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITOPTIONSDIALOG)
using namespace Syneditoptionsdialog;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditoptionsdialogHPP
