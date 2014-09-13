// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynAutoCorrectEditor.pas' rev: 28.00 (Windows)

#ifndef SynautocorrecteditorHPP
#define SynautocorrecteditorHPP

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
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <Vcl.ExtCtrls.hpp>	// Pascal unit
#include <Vcl.StdCtrls.hpp>	// Pascal unit
#include <Vcl.Buttons.hpp>	// Pascal unit
#include <System.Win.Registry.hpp>	// Pascal unit
#include <SynAutoCorrect.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synautocorrecteditor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TfrmAutoCorrectEditor;
class PASCALIMPLEMENTATION TfrmAutoCorrectEditor : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TLabel* lblLabel1;
	Vcl::Stdctrls::TLabel* lblLabel2;
	Vcl::Stdctrls::TListBox* lbxItems;
	Vcl::Buttons::TSpeedButton* btnAdd;
	Vcl::Buttons::TSpeedButton* btnDelete;
	Vcl::Buttons::TSpeedButton* btnClear;
	Vcl::Buttons::TSpeedButton* btnEdit;
	Vcl::Buttons::TSpeedButton* btnDone;
	Vcl::Extctrls::TBevel* bvlSeparator;
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall btnAddClick(System::TObject* Sender);
	void __fastcall btnDeleteClick(System::TObject* Sender);
	void __fastcall btnEditClick(System::TObject* Sender);
	void __fastcall btnDoneClick(System::TObject* Sender);
	void __fastcall btnClearClick(System::TObject* Sender);
	void __fastcall lbxItemsClick(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormPaint(System::TObject* Sender);
	
private:
	void __fastcall lbxItemsDrawItemCLX(System::TObject* Sender, int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State, bool &Handled);
	void __fastcall lbxItemsDrawItem(Vcl::Controls::TWinControl* Control, int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	
public:
	Synautocorrect::TSynAutoCorrect* SynAutoCorrect;
public:
	/* TCustomForm.Create */ inline __fastcall virtual TfrmAutoCorrectEditor(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TfrmAutoCorrectEditor(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TfrmAutoCorrectEditor(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TfrmAutoCorrectEditor(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _SConfirmation;
#define Synautocorrecteditor_SConfirmation System::LoadResourceString(&Synautocorrecteditor::_SConfirmation)
extern DELPHI_PACKAGE System::ResourceString _SError;
#define Synautocorrecteditor_SError System::LoadResourceString(&Synautocorrecteditor::_SError)
extern DELPHI_PACKAGE System::ResourceString _SOriginal;
#define Synautocorrecteditor_SOriginal System::LoadResourceString(&Synautocorrecteditor::_SOriginal)
extern DELPHI_PACKAGE System::ResourceString _SCorrection;
#define Synautocorrecteditor_SCorrection System::LoadResourceString(&Synautocorrecteditor::_SCorrection)
extern DELPHI_PACKAGE System::ResourceString _SAdd;
#define Synautocorrecteditor_SAdd System::LoadResourceString(&Synautocorrecteditor::_SAdd)
extern DELPHI_PACKAGE System::ResourceString _SEdit;
#define Synautocorrecteditor_SEdit System::LoadResourceString(&Synautocorrecteditor::_SEdit)
extern DELPHI_PACKAGE System::ResourceString _SPleaseSelectItem;
#define Synautocorrecteditor_SPleaseSelectItem System::LoadResourceString(&Synautocorrecteditor::_SPleaseSelectItem)
extern DELPHI_PACKAGE System::ResourceString _SClearListConfirmation;
#define Synautocorrecteditor_SClearListConfirmation System::LoadResourceString(&Synautocorrecteditor::_SClearListConfirmation)
}	/* namespace Synautocorrecteditor */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNAUTOCORRECTEDITOR)
using namespace Synautocorrecteditor;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynautocorrecteditorHPP
