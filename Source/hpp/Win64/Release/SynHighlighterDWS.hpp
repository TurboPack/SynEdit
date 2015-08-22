// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterDWS.pas' rev: 30.00 (Windows)

#ifndef SynhighlighterdwsHPP
#define SynhighlighterdwsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Character.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterdws
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAnsiStringList;
class DELPHICLASS TSynDWSSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty, rsExports, rsDirective, rsDirectiveAsm, rsHereDocSingle, rsHereDocDouble, rsUnKnown };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(void);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TAnsiStringList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
__published:
	virtual int __fastcall CompareStrings(const System::UnicodeString S1, const System::UnicodeString S2);
public:
	/* TStringList.Create */ inline __fastcall TAnsiStringList(void)/* overload */ : System::Classes::TStringList() { }
	/* TStringList.Create */ inline __fastcall TAnsiStringList(bool OwnsObjects)/* overload */ : System::Classes::TStringList(OwnsObjects) { }
	/* TStringList.Destroy */ inline __fastcall virtual ~TAnsiStringList(void) { }
	
};


class PASCALIMPLEMENTATION TSynDWSSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	bool fAsmStart;
	TRangeState fRange;
	System::WideChar fCommentClose;
	System::StaticArray<TIdentFuncTableFunc, 389> fIdentFuncTable;
	TAnsiStringList* fKeyWords;
	TAnsiStringList* fKeyWords_PropertyScoped;
	TtkTokenKind fTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCharAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fFloatAttri;
	Synedithighlighter::TSynHighlighterAttributes* fHexAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fAsmAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDirecAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	TtkTokenKind __fastcall AltFunc(void);
	TtkTokenKind __fastcall KeyWordFunc(void);
	TtkTokenKind __fastcall FuncAsm(void);
	TtkTokenKind __fastcall FuncEnd(void);
	TtkTokenKind __fastcall FuncPropertyScoped(void);
	TtkTokenKind __fastcall FuncProperty(void);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall AddressOpProc(void);
	void __fastcall AsciiCharProc(void);
	void __fastcall AnsiProc(void);
	void __fastcall BorProc(void);
	void __fastcall BraceOpenProc(void);
	void __fastcall ColonOrGreaterProc(void);
	void __fastcall CRProc(void);
	void __fastcall IdentProc(void);
	void __fastcall IntegerProc(void);
	void __fastcall LFProc(void);
	void __fastcall LowerProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall PointProc(void);
	void __fastcall RoundOpenProc(void);
	void __fastcall SemicolonProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringAposProc(void);
	void __fastcall StringAposMultiProc(void);
	void __fastcall StringQuoteProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall UnknownProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	virtual bool __fastcall IsCurrentToken(const System::UnicodeString Token);
	
public:
	__classmethod virtual Synedithighlighter::TSynHighlighterCapabilities __fastcall GetCapabilities();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynDWSSyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynDWSSyn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall LoadDelphiStyle(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* AsmAttri = {read=fAsmAttri, write=fAsmAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DirectiveAttri = {read=fDirecAttri, write=fDirecAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* FloatAttri = {read=fFloatAttri, write=fFloatAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* HexAttri = {read=fHexAttri, write=fHexAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CharAttri = {read=fCharAttri, write=fCharAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterdws */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERDWS)
using namespace Synhighlighterdws;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterdwsHPP
