// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynAutoCorrect.pas' rev: 31.00 (Windows)

#ifndef SynautocorrectHPP
#define SynautocorrectHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <SynEditMiscProcs.hpp>
#include <SynEditTypes.hpp>
#include <SynEditKeyCmds.hpp>
#include <SynEdit.hpp>
#include <SynEditMiscClasses.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.IniFiles.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synautocorrect
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomSynAutoCorrect;
class DELPHICLASS TSynAutoCorrect;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAsSynAutoCorrectOption : unsigned char { ascoCorrectOnMouseDown, ascoIgnoreCase, ascoMaintainCase };

typedef System::Set<TAsSynAutoCorrectOption, TAsSynAutoCorrectOption::ascoCorrectOnMouseDown, TAsSynAutoCorrectOption::ascoMaintainCase> TAsSynAutoCorrectOptions;

enum DECLSPEC_DENUM TAutoCorrectAction : unsigned char { aaCorrect, aaAbort };

typedef void __fastcall (__closure *TAutoCorrectEvent)(System::TObject* Sender, const System::UnicodeString AOriginal, const System::UnicodeString ACorrection, int Line, int Column, TAutoCorrectAction &Action);

class PASCALIMPLEMENTATION TCustomSynAutoCorrect : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Synedit::TCustomSynEdit* FEditor;
	bool FEnabled;
	System::Classes::TStrings* FItems;
	System::WideChar FItemSepChar;
	TAsSynAutoCorrectOptions FOptions;
	TAutoCorrectEvent FOnAutoCorrect;
	System::Classes::TNotifyEvent FOnCorrected;
	int FPrevLine;
	int __fastcall CorrectItemStart(System::UnicodeString EditLine, System::UnicodeString SearchString, int StartPos, bool MatchCase, bool WholeWord);
	bool __fastcall FindAndCorrect(System::UnicodeString &EditLine, System::UnicodeString Original, System::UnicodeString Correction, int &CurrentX);
	System::UnicodeString __fastcall PreviousToken(void);
	System::Classes::TStrings* __fastcall GetItems(void);
	void __fastcall SetItems(System::Classes::TStrings* const Value);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall KeyboardHandler(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData);
	virtual void __fastcall MouseDownHandler(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetEditor(Synedit::TCustomSynEdit* Value);
	
public:
	__fastcall virtual TCustomSynAutoCorrect(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomSynAutoCorrect(void);
	void __fastcall Add(System::UnicodeString AOriginal, System::UnicodeString ACorrection);
	bool __fastcall AutoCorrectAll(void);
	void __fastcall Delete(int AIndex);
	void __fastcall Edit(int AIndex, System::UnicodeString ANewOriginal, System::UnicodeString ANewCorrection);
	void __fastcall LoadFromINI(System::UnicodeString AFileName, System::UnicodeString ASection);
	void __fastcall SaveToINI(System::UnicodeString AFileName, System::UnicodeString ASection);
	void __fastcall LoadFromRegistry(unsigned ARoot, System::UnicodeString AKey);
	void __fastcall SaveToRegistry(unsigned ARoot, System::UnicodeString AKey);
	bool __fastcall LoadFromList(System::UnicodeString AFileName);
	void __fastcall SaveToList(System::UnicodeString AFileName);
	System::UnicodeString __fastcall HalfString(System::UnicodeString Str, bool GetFirstHalf);
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property Synedit::TCustomSynEdit* Editor = {read=FEditor, write=SetEditor};
	__property System::Classes::TStrings* Items = {read=GetItems, write=SetItems};
	__property System::WideChar ItemSepChar = {read=FItemSepChar, write=FItemSepChar, default=9};
	__property TAsSynAutoCorrectOptions Options = {read=FOptions, write=FOptions, default=6};
	__property TAutoCorrectEvent OnAutoCorrect = {read=FOnAutoCorrect, write=FOnAutoCorrect};
	__property System::Classes::TNotifyEvent OnCorrected = {read=FOnCorrected, write=FOnCorrected};
};


class PASCALIMPLEMENTATION TSynAutoCorrect : public TCustomSynAutoCorrect
{
	typedef TCustomSynAutoCorrect inherited;
	
__published:
	__property Enabled = {default=1};
	__property Editor;
	__property Items;
	__property ItemSepChar = {default=9};
	__property Options = {default=6};
	__property OnAutoCorrect;
	__property OnCorrected;
public:
	/* TCustomSynAutoCorrect.Create */ inline __fastcall virtual TSynAutoCorrect(System::Classes::TComponent* AOwner) : TCustomSynAutoCorrect(AOwner) { }
	/* TCustomSynAutoCorrect.Destroy */ inline __fastcall virtual ~TSynAutoCorrect(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synautocorrect */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNAUTOCORRECT)
using namespace Synautocorrect;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynautocorrectHPP
