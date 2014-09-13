// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynMemo.pas' rev: 28.00 (Windows)

#ifndef SynmemoHPP
#define SynmemoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.RichEdit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <SynEdit.hpp>	// Pascal unit
#include <SynEditTextBuffer.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synmemo
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSynMemo;
class PASCALIMPLEMENTATION TSynMemo : public Synedit::TSynEdit
{
	typedef Synedit::TSynEdit inherited;
	
private:
	MESSAGE void __fastcall EMGetSel(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMSetSel(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetModify(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMSetModify(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetLineCount(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetSelText(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMReplaceSel(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetLine(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMCanUndo(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMUndo(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetFirstVisibleLine(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMCharFromPos(Winapi::Messages::TMessage &Message);
public:
	/* TCustomSynEdit.Create */ inline __fastcall virtual TSynMemo(System::Classes::TComponent* AOwner) : Synedit::TSynEdit(AOwner) { }
	/* TCustomSynEdit.Destroy */ inline __fastcall virtual ~TSynMemo(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSynMemo(HWND ParentWindow) : Synedit::TSynEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synmemo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNMEMO)
using namespace Synmemo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynmemoHPP
