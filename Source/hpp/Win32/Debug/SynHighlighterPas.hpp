// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterPas.pas' rev: 33.00 (Windows)

#ifndef SynhighlighterpasHPP
#define SynhighlighterpasHPP

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
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterpas
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynPasSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty, rsExports, rsDirective, rsDirectiveAsm, rsUnKnown };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

enum DECLSPEC_DENUM TDelphiVersion : unsigned char { dvDelphi1, dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5, dvDelphi6, dvDelphi7, dvDelphi8, dvDelphi2005 };

class PASCALIMPLEMENTATION TSynPasSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	bool fAsmStart;
	TRangeState fRange;
	System::StaticArray<TIdentFuncTableFunc, 389> fIdentFuncTable;
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
	TDelphiVersion fDelphiVersion;
	bool fPackageSource;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall KeyWordFunc(int Index);
	TtkTokenKind __fastcall FuncAsm(int Index);
	TtkTokenKind __fastcall FuncAutomated(int Index);
	TtkTokenKind __fastcall FuncCdecl(int Index);
	TtkTokenKind __fastcall FuncContains(int Index);
	TtkTokenKind __fastcall FuncDeprecated(int Index);
	TtkTokenKind __fastcall FuncDispid(int Index);
	TtkTokenKind __fastcall FuncDispinterface(int Index);
	TtkTokenKind __fastcall FuncEnd(int Index);
	TtkTokenKind __fastcall FuncExports(int Index);
	TtkTokenKind __fastcall FuncFinal(int Index);
	TtkTokenKind __fastcall FuncFinalization(int Index);
	TtkTokenKind __fastcall FuncHelper(int Index);
	TtkTokenKind __fastcall FuncImplements(int Index);
	TtkTokenKind __fastcall FuncIndex(int Index);
	TtkTokenKind __fastcall FuncName(int Index);
	TtkTokenKind __fastcall FuncNodefault(int Index);
	TtkTokenKind __fastcall FuncOperator(int Index);
	TtkTokenKind __fastcall FuncOverload(int Index);
	TtkTokenKind __fastcall FuncPackage(int Index);
	TtkTokenKind __fastcall FuncPlatform(int Index);
	TtkTokenKind __fastcall FuncProperty(int Index);
	TtkTokenKind __fastcall FuncRead(int Index);
	TtkTokenKind __fastcall FuncReadonly(int Index);
	TtkTokenKind __fastcall FuncReintroduce(int Index);
	TtkTokenKind __fastcall FuncRequires(int Index);
	TtkTokenKind __fastcall FuncResourcestring(int Index);
	TtkTokenKind __fastcall FuncSafecall(int Index);
	TtkTokenKind __fastcall FuncSealed(int Index);
	TtkTokenKind __fastcall FuncStdcall(int Index);
	TtkTokenKind __fastcall FuncStored(int Index);
	TtkTokenKind __fastcall FuncStringresource(int Index);
	TtkTokenKind __fastcall FuncThreadvar(int Index);
	TtkTokenKind __fastcall FuncWrite(int Index);
	TtkTokenKind __fastcall FuncWriteonly(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent();
	void __fastcall AddressOpProc();
	void __fastcall AsciiCharProc();
	void __fastcall AnsiProc();
	void __fastcall BorProc();
	void __fastcall BraceOpenProc();
	void __fastcall ColonOrGreaterProc();
	void __fastcall CRProc();
	void __fastcall IdentProc();
	void __fastcall IntegerProc();
	void __fastcall LFProc();
	void __fastcall LowerProc();
	void __fastcall NullProc();
	void __fastcall NumberProc();
	void __fastcall PointProc();
	void __fastcall RoundOpenProc();
	void __fastcall SemicolonProc();
	void __fastcall SlashProc();
	void __fastcall SpaceProc();
	void __fastcall StringProc();
	void __fastcall SymbolProc();
	void __fastcall UnknownProc();
	void __fastcall SetDelphiVersion(const TDelphiVersion Value);
	void __fastcall SetPackageSource(const bool Value);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource();
	virtual bool __fastcall IsFilterStored();
	
public:
	__classmethod virtual Synedithighlighter::TSynHighlighterCapabilities __fastcall GetCapabilities();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynPasSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	virtual void * __fastcall GetRange();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	TtkTokenKind __fastcall GetTokenID();
	virtual int __fastcall GetTokenKind();
	virtual void __fastcall Next();
	virtual void __fastcall ResetRange();
	virtual void __fastcall SetRange(void * Value);
	virtual bool __fastcall UseUserSettings(int VersionIndex);
	virtual void __fastcall EnumUserSettings(System::Classes::TStrings* DelphiVersions);
	
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
	__property TDelphiVersion DelphiVersion = {read=fDelphiVersion, write=SetDelphiVersion, default=8};
	__property bool PackageSource = {read=fPackageSource, write=SetPackageSource, default=1};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynPasSyn() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const TDelphiVersion LastDelphiVersion = (TDelphiVersion)(8);
#define BDSVersionPrefix L"BDS"
}	/* namespace Synhighlighterpas */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERPAS)
using namespace Synhighlighterpas;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterpasHPP
