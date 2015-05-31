// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynURIOpener.pas' rev: 29.00 (Windows)

#ifndef SynuriopenerHPP
#define SynuriopenerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <SynEditTypes.hpp>
#include <SynEdit.hpp>
#include <SynHighlighterURI.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synuriopener
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynURIOpener;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSynURIOpener : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FControlDown;
	bool FCtrlActivatesLinks;
	Synedit::TCustomSynEdit* FEditor;
	int FMouseDownX;
	int FMouseDownY;
	Synhighlighteruri::TSynURISyn* FURIHighlighter;
	System::Classes::TStringList* FVisitedURIs;
	void __fastcall OpenLink(System::UnicodeString URI, int LinkType);
	bool __fastcall MouseInSynEdit(void);
	
protected:
	void __fastcall NewKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall NewKeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall NewMouseCursor(System::TObject* Sender, const Synedittypes::TBufferCoord &aLineCharPos, System::Uitypes::TCursor &aCursor);
	void __fastcall NewMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall NewMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetEditor(Synedit::TCustomSynEdit* const Value);
	void __fastcall SetURIHighlighter(Synhighlighteruri::TSynURISyn* const Value);
	
public:
	__fastcall virtual TSynURIOpener(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynURIOpener(void);
	bool __fastcall VisitedURI(System::UnicodeString URI);
	
__published:
	__property bool CtrlActivatesLinks = {read=FCtrlActivatesLinks, write=FCtrlActivatesLinks, default=1};
	__property Synedit::TCustomSynEdit* Editor = {read=FEditor, write=SetEditor};
	__property Synhighlighteruri::TSynURISyn* URIHighlighter = {read=FURIHighlighter, write=SetURIHighlighter};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synuriopener */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNURIOPENER)
using namespace Synuriopener;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynuriopenerHPP
