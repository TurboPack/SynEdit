// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditKeyCmdEditor.pas' rev: 28.00 (Windows)

#ifndef SyneditkeycmdeditorHPP
#define SyneditkeycmdeditorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Menus.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.ComCtrls.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <SynEditKeyCmds.hpp>	// Pascal unit
#include <SynEditMiscClasses.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Syneditkeycmdeditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSynEditKeystrokeEditorForm;
class PASCALIMPLEMENTATION TSynEditKeystrokeEditorForm : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Extctrls::TPanel* pnlAlign;
	Vcl::Stdctrls::TLabel* Label1;
	Vcl::Stdctrls::TLabel* Label2;
	Vcl::Stdctrls::TLabel* Label4;
	Vcl::Stdctrls::TButton* bntClearKey;
	Vcl::Stdctrls::TButton* btnOK;
	Vcl::Stdctrls::TComboBox* cmbCommand;
	Vcl::Stdctrls::TButton* btnCancel;
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall bntClearKeyClick(System::TObject* Sender);
	void __fastcall cmbCommandKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall cmbCommandExit(System::TObject* Sender);
	void __fastcall btnOKClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	
private:
	bool FExtended;
	void __fastcall SetCommand(const Syneditkeycmds::TSynEditorCommand Value);
	void __fastcall SetKeystroke(const System::Classes::TShortCut Value);
	void __fastcall AddEditorCommand(const System::UnicodeString S);
	Syneditkeycmds::TSynEditorCommand __fastcall GetCommand(void);
	System::Classes::TShortCut __fastcall GetKeystroke(void);
	System::Classes::TShortCut __fastcall GetKeystroke2(void);
	void __fastcall SetKeystroke2(const System::Classes::TShortCut Value);
	
public:
	Syneditmiscclasses::TSynHotKey* hkKeystroke2;
	Syneditmiscclasses::TSynHotKey* hkKeystroke;
	__property Syneditkeycmds::TSynEditorCommand Command = {read=GetCommand, write=SetCommand, nodefault};
	__property System::Classes::TShortCut Keystroke = {read=GetKeystroke, write=SetKeystroke, nodefault};
	__property System::Classes::TShortCut Keystroke2 = {read=GetKeystroke2, write=SetKeystroke2, nodefault};
	__property bool ExtendedString = {read=FExtended, write=FExtended, default=1};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TSynEditKeystrokeEditorForm(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TSynEditKeystrokeEditorForm(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TSynEditKeystrokeEditorForm(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSynEditKeystrokeEditorForm(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TSynEditKeystrokeEditorForm* SynEditKeystrokeEditorForm;
}	/* namespace Syneditkeycmdeditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITKEYCMDEDITOR)
using namespace Syneditkeycmdeditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditkeycmdeditorHPP
