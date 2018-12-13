// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditKeyCmdsEditor.pas' rev: 32.00 (Windows)

#ifndef SyneditkeycmdseditorHPP
#define SyneditkeycmdseditorHPP

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
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <SynEditKeyCmds.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditkeycmdseditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynEditKeystrokesEditorForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSynEditKeystrokesEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnlBottom;
	Vcl::Stdctrls::TLabel* lnlInfo;
	Vcl::Stdctrls::TLabel* lnlInfo2;
	Vcl::Stdctrls::TButton* btnAdd;
	Vcl::Stdctrls::TButton* btnEdit;
	Vcl::Stdctrls::TButton* btnDelete;
	Vcl::Stdctrls::TButton* btnClear;
	Vcl::Stdctrls::TButton* btnReset;
	Vcl::Stdctrls::TButton* btnOK;
	Vcl::Stdctrls::TButton* btnCancel;
	Vcl::Extctrls::TPanel* pnlCommands;
	Vcl::Comctrls::TListView* KeyCmdList;
	void __fastcall FormResize(System::TObject* Sender);
	void __fastcall btnAddClick(System::TObject* Sender);
	void __fastcall btnEditClick(System::TObject* Sender);
	void __fastcall btnDeleteClick(System::TObject* Sender);
	void __fastcall btnResetClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall btnClearClick(System::TObject* Sender);
	void __fastcall btnOKClick(System::TObject* Sender);
	void __fastcall btnCancelClick(System::TObject* Sender);
	void __fastcall KeyCmdListClick(System::TObject* Sender);
	
private:
	Syneditkeycmds::TSynEditKeyStrokes* FKeystrokes;
	bool FExtended;
	void __fastcall SetKeystrokes(Syneditkeycmds::TSynEditKeyStrokes* const Value);
	void __fastcall UpdateKeystrokesList(void);
	HIDESBASE MESSAGE void __fastcall WMGetMinMaxInfo(Winapi::Messages::TWMGetMinMaxInfo &Msg);
	
public:
	__fastcall virtual TSynEditKeystrokesEditorForm(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynEditKeystrokesEditorForm(void);
	__property Syneditkeycmds::TSynEditKeyStrokes* Keystrokes = {read=FKeystrokes, write=SetKeystrokes};
	__property bool ExtendedString = {read=FExtended, write=FExtended, nodefault};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TSynEditKeystrokesEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSynEditKeystrokesEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditkeycmdseditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITKEYCMDSEDITOR)
using namespace Syneditkeycmdseditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditkeycmdseditorHPP
