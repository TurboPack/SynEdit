// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditAutoComplete.pas' rev: 33.00 (Windows)

#ifndef SyneditautocompleteHPP
#define SyneditautocompleteHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Menus.hpp>
#include <SynEdit.hpp>
#include <SynEditKeyCmds.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditautocomplete
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomSynAutoComplete;
class DELPHICLASS TSynAutoComplete;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomSynAutoComplete : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	System::Classes::TStrings* fAutoCompleteList;
	System::Classes::TStrings* fCompletions;
	System::Classes::TStrings* fCompletionComments;
	System::Classes::TStrings* fCompletionValues;
	Synedit::TCustomSynEdit* fEditor;
	System::Classes::TList* fEditors;
	System::UnicodeString fEOTokenChars;
	bool fCaseSensitive;
	bool fParsed;
	void __fastcall CompletionListChanged(System::TObject* Sender);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	System::Classes::TStrings* __fastcall GetCompletions();
	System::Classes::TStrings* __fastcall GetCompletionComments();
	System::Classes::TStrings* __fastcall GetCompletionValues();
	int __fastcall GetEditorCount();
	Synedit::TCustomSynEdit* __fastcall GetNthEditor(int Index);
	virtual void __fastcall SetAutoCompleteList(System::Classes::TStrings* Value);
	void __fastcall SetEditor(Synedit::TCustomSynEdit* Value);
	void __fastcall SynEditCommandHandler(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TCustomSynAutoComplete(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomSynAutoComplete();
	bool __fastcall AddEditor(Synedit::TCustomSynEdit* AEditor);
	bool __fastcall RemoveEditor(Synedit::TCustomSynEdit* AEditor);
	void __fastcall AddCompletion(const System::UnicodeString AToken, const System::UnicodeString AValue, const System::UnicodeString AComment);
	virtual void __fastcall Execute(Synedit::TCustomSynEdit* AEditor);
	virtual void __fastcall ExecuteCompletion(const System::UnicodeString AToken, Synedit::TCustomSynEdit* AEditor);
	virtual void __fastcall ParseCompletionList();
	__property System::Classes::TStrings* AutoCompleteList = {read=fAutoCompleteList, write=SetAutoCompleteList};
	__property bool CaseSensitive = {read=fCaseSensitive, write=fCaseSensitive, nodefault};
	__property System::Classes::TStrings* Completions = {read=GetCompletions};
	__property System::Classes::TStrings* CompletionComments = {read=GetCompletionComments};
	__property System::Classes::TStrings* CompletionValues = {read=GetCompletionValues};
	__property Synedit::TCustomSynEdit* Editor = {read=fEditor, write=SetEditor};
	__property int EditorCount = {read=GetEditorCount, nodefault};
	__property Synedit::TCustomSynEdit* Editors[int Index] = {read=GetNthEditor};
	__property System::UnicodeString EndOfTokenChr = {read=fEOTokenChars, write=fEOTokenChars};
};


class PASCALIMPLEMENTATION TSynAutoComplete : public TCustomSynAutoComplete
{
	typedef TCustomSynAutoComplete inherited;
	
__published:
	__property AutoCompleteList;
	__property CaseSensitive;
	__property Editor;
	__property EndOfTokenChr = {default=0};
public:
	/* TCustomSynAutoComplete.Create */ inline __fastcall virtual TSynAutoComplete(System::Classes::TComponent* AOwner) : TCustomSynAutoComplete(AOwner) { }
	/* TCustomSynAutoComplete.Destroy */ inline __fastcall virtual ~TSynAutoComplete() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditautocomplete */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITAUTOCOMPLETE)
using namespace Syneditautocomplete;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditautocompleteHPP
