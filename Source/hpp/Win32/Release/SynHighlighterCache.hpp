// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterCache.pas' rev: 33.00 (Windows)

#ifndef SynhighlightercacheHPP
#define SynhighlightercacheHPP

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
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlightercache
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynCacheSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkClass, tkComment, tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkDirective, tkSpace, tkString, tkSymbol, tkIndirect, tkLabel, tkMacro, tkUserFunction, tkEmbedSQL, tkEmbedText, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown, rsSQL, rsHTML, rsCommand };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynCacheSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	int fBrace;
	bool fFirstBrace;
	TRangeState fRange;
	TtkTokenKind FTokenID;
	System::StaticArray<TIdentFuncTableFunc, 1997> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fClassAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fFunctionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDirectiveAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIndirectAttri;
	Synedithighlighter::TSynHighlighterAttributes* fLabelAttri;
	Synedithighlighter::TSynHighlighterAttributes* fMacroAttri;
	Synedithighlighter::TSynHighlighterAttributes* fUserFunctionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fEmbedSQLAttri;
	Synedithighlighter::TSynHighlighterAttributes* fEmbedTextAttri;
	bool FCanKey;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall KeyWordFunc(int Index);
	TtkTokenKind __fastcall Func38html(int Index);
	TtkTokenKind __fastcall Func38sql(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent();
	void __fastcall CRProc();
	void __fastcall CommentProc();
	void __fastcall IdentProc();
	void __fastcall LFProc();
	void __fastcall NullProc();
	void __fastcall NumberProc();
	void __fastcall SpaceProc();
	void __fastcall StringProc();
	void __fastcall UnknownProc();
	void __fastcall IndirectProc();
	void __fastcall SymbolProc();
	void __fastcall FuncProc();
	void __fastcall DirectiveProc();
	void __fastcall EmbeddedProc();
	
protected:
	virtual bool __fastcall IsFilterStored();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynCacheSyn(System::Classes::TComponent* AOwner);
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
	__property Synedithighlighter::TSynHighlighterAttributes* ClassAttri = {read=fClassAttri, write=fClassAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* FunctionAttri = {read=fFunctionAttri, write=fFunctionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PreprocesorAttri = {read=fDirectiveAttri, write=fDirectiveAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IndirectAttri = {read=fIndirectAttri, write=fIndirectAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* LabelAttri = {read=fLabelAttri, write=fLabelAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* MacroAttri = {read=fMacroAttri, write=fMacroAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* UserFunctionAttri = {read=fUserFunctionAttri, write=fUserFunctionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* EmbededSQLandHTMLAttri = {read=fEmbedSQLAttri, write=fEmbedSQLAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* EmbededTextAttri = {read=fEmbedTextAttri, write=fEmbedTextAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynCacheSyn() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightercache */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERCACHE)
using namespace Synhighlightercache;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightercacheHPP
