// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterSml.pas' rev: 30.00 (Windows)

#ifndef SynhighlightersmlHPP
#define SynhighlightersmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Win.Registry.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlightersml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynSMLSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkCharacter, tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkOperator, tkSpace, tkString, tkSymbol, tkSyntaxError, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsComment, rsMultilineString };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynSMLSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	bool fBasis;
	TRangeState fRange;
	TtkTokenKind FTokenID;
	System::StaticArray<TIdentFuncTableFunc, 71> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCharacterAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fOperatorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSyntaxErrorAttri;
	bool __fastcall IsValidMLCharacter(void);
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall KeyWordFunc(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall CRProc(void);
	void __fastcall CharacterProc(void);
	void __fastcall ColonProc(void);
	void __fastcall CommentProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall OperatorProc(void);
	void __fastcall RoundBracketOpenProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall BasisOpProc(void);
	void __fastcall StringEndProc(void);
	void __fastcall PoundProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	virtual void * __fastcall GetRange(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	__fastcall virtual TSynSMLSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CharacterAttri = {read=fCharacterAttri, write=fCharacterAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* OperatorAttri = {read=fOperatorAttri, write=fOperatorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SyntaxErrorAttri = {read=fSyntaxErrorAttri, write=fSyntaxErrorAttri};
	__property bool Basis = {read=fBasis, write=fBasis, default=1};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynSMLSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightersml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERSML)
using namespace Synhighlightersml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightersmlHPP
