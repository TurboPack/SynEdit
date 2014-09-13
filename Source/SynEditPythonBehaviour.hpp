// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPythonBehaviour.pas' rev: 28.00 (Windows)

#ifndef SyneditpythonbehaviourHPP
#define SyneditpythonbehaviourHPP

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
#include <SynEdit.hpp>	// Pascal unit
#include <SynEditKeyCmds.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Syneditpythonbehaviour
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSynEditPythonBehaviour;
class PASCALIMPLEMENTATION TSynEditPythonBehaviour : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Synedit::TSynEdit* FEditor;
	int fIndent;
	
protected:
	virtual void __fastcall SetEditor(Synedit::TSynEdit* Value);
	virtual void __fastcall doProcessUserCommand(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData);
	
public:
	__fastcall virtual TSynEditPythonBehaviour(System::Classes::TComponent* aOwner);
	
__published:
	__property Synedit::TSynEdit* Editor = {read=FEditor, write=SetEditor};
	__property int Indent = {read=fIndent, write=fIndent, default=4};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TSynEditPythonBehaviour(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditpythonbehaviour */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPYTHONBEHAVIOUR)
using namespace Syneditpythonbehaviour;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditpythonbehaviourHPP
