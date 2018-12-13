// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterCSS.pas' rev: 33.00 (Windows)

#ifndef SynhighlightercssHPP
#define SynhighlightercssHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynHighlighterHashEntries.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlightercss
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynCssSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkProperty, tkSelector, tkSelectorAttrib, tkNull, tkSpace, tkString, tkSymbol, tkText, tkUndefProperty, tkValue, tkColor, tkNumber, tkImportant };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsComment, rsSelector, rsDeclaration, rsUnknown, rsProperty, rsValue, rsAttrib, rsParameter };

class PASCALIMPLEMENTATION TSynCssSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TRangeState fCommentRange;
	TRangeState fParameterRange;
	TtkTokenKind fTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPropertyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fAttributeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSelectorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fColorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fTextAttri;
	Synedithighlighter::TSynHighlighterAttributes* fValueAttri;
	Synedithighlighter::TSynHighlighterAttributes* fUndefPropertyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fImportantPropertyAttri;
	Synhighlighterhashentries::TSynHashEntryList* fKeywords;
	void __fastcall DoAddKeyword(System::UnicodeString AKeyword, int AKind);
	int __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall SelectorProc();
	void __fastcall AttributeProc();
	void __fastcall CommentProc();
	void __fastcall BraceCloseProc();
	void __fastcall BraceOpenProc();
	void __fastcall ParenOpenProc();
	void __fastcall ParenCloseProc();
	void __fastcall BracketOpenProc();
	void __fastcall BracketCloseProc();
	void __fastcall CRProc();
	void __fastcall SemiProc();
	void __fastcall StartValProc();
	void __fastcall NumberProc();
	void __fastcall IdentProc();
	void __fastcall LFProc();
	void __fastcall NullProc();
	void __fastcall SpaceProc();
	void __fastcall StringProc();
	void __fastcall HashProc();
	void __fastcall SlashProc();
	void __fastcall GreaterProc();
	void __fastcall PlusProc();
	void __fastcall TildeProc();
	void __fastcall PipeProc();
	void __fastcall EqualProc();
	void __fastcall ExclamProc();
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource();
	virtual bool __fastcall IsFilterStored();
	void __fastcall NextDeclaration();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynCssSyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynCssSyn();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	virtual void * __fastcall GetRange();
	TtkTokenKind __fastcall GetTokenID();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	virtual int __fastcall GetTokenKind();
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next();
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange();
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PropertyAttri = {read=fPropertyAttri, write=fPropertyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ColorAttri = {read=fColorAttri, write=fColorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SelectorAttri = {read=fSelectorAttri, write=fSelectorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* AttributeAttri = {read=fAttributeAttri, write=fAttributeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* TextAttri = {read=fTextAttri, write=fTextAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ValueAttri = {read=fValueAttri, write=fValueAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* UndefPropertyAttri = {read=fUndefPropertyAttri, write=fUndefPropertyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ImportantPropertyAttri = {read=fImportantPropertyAttri, write=fImportantPropertyAttri};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightercss */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERCSS)
using namespace Synhighlightercss;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightercssHPP
