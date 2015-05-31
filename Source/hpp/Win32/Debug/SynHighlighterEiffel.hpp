// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterEiffel.pas' rev: 29.00 (Windows)

#ifndef SynhighlightereiffelHPP
#define SynhighlightereiffelHPP

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

namespace Synhighlightereiffel
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynEiffelSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkBasicTypes, tkComment, tkIdentifier, tkKey, tkLace, tkNull, tkOperatorAndSymbols, tkPredefined, tkResultValue, tkSpace, tkString, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown, rsEiffelComment, rsString, rsOperatorAndSymbolProc };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynEiffelSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 503> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fBasicTypesAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fLaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fOperatorAndSymbolsAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPredefinedAttri;
	Synedithighlighter::TSynHighlighterAttributes* fResultValueAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall OperatorFunc(int Index);
	TtkTokenKind __fastcall Func37u(int Index);
	TtkTokenKind __fastcall FuncAdapt(int Index);
	TtkTokenKind __fastcall FuncAlias(int Index);
	TtkTokenKind __fastcall FuncAll(int Index);
	TtkTokenKind __fastcall FuncAnd(int Index);
	TtkTokenKind __fastcall FuncArray(int Index);
	TtkTokenKind __fastcall FuncAs(int Index);
	TtkTokenKind __fastcall FuncAssertion(int Index);
	TtkTokenKind __fastcall FuncBit(int Index);
	TtkTokenKind __fastcall FuncBoolean(int Index);
	TtkTokenKind __fastcall FuncCharacter(int Index);
	TtkTokenKind __fastcall FuncCheck(int Index);
	TtkTokenKind __fastcall FuncClass(int Index);
	TtkTokenKind __fastcall FuncCluster(int Index);
	TtkTokenKind __fastcall FuncColon(int Index);
	TtkTokenKind __fastcall FuncComma(int Index);
	TtkTokenKind __fastcall FuncCreation(int Index);
	TtkTokenKind __fastcall FuncCurrent(int Index);
	TtkTokenKind __fastcall FuncDebug(int Index);
	TtkTokenKind __fastcall FuncDefault(int Index);
	TtkTokenKind __fastcall FuncDeferred(int Index);
	TtkTokenKind __fastcall FuncDo(int Index);
	TtkTokenKind __fastcall FuncDouble(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncElseif(int Index);
	TtkTokenKind __fastcall FuncEnd(int Index);
	TtkTokenKind __fastcall FuncEnsure(int Index);
	TtkTokenKind __fastcall FuncExclude(int Index);
	TtkTokenKind __fastcall FuncExecutable(int Index);
	TtkTokenKind __fastcall FuncExpanded(int Index);
	TtkTokenKind __fastcall FuncExport(int Index);
	TtkTokenKind __fastcall FuncExternal(int Index);
	TtkTokenKind __fastcall FuncFalse(int Index);
	TtkTokenKind __fastcall FuncFeature(int Index);
	TtkTokenKind __fastcall FuncFrom(int Index);
	TtkTokenKind __fastcall FuncFrozen(int Index);
	TtkTokenKind __fastcall FuncGenerate(int Index);
	TtkTokenKind __fastcall FuncIdentifier(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncIgnore(int Index);
	TtkTokenKind __fastcall FuncImplies(int Index);
	TtkTokenKind __fastcall FuncInclude(int Index);
	TtkTokenKind __fastcall FuncInclude95path(int Index);
	TtkTokenKind __fastcall FuncIndexing(int Index);
	TtkTokenKind __fastcall FuncInfix(int Index);
	TtkTokenKind __fastcall FuncInherit(int Index);
	TtkTokenKind __fastcall FuncInspect(int Index);
	TtkTokenKind __fastcall FuncInteger(int Index);
	TtkTokenKind __fastcall FuncInvariant(int Index);
	TtkTokenKind __fastcall FuncIs(int Index);
	TtkTokenKind __fastcall FuncLike(int Index);
	TtkTokenKind __fastcall FuncLocal(int Index);
	TtkTokenKind __fastcall FuncLoop(int Index);
	TtkTokenKind __fastcall FuncMake(int Index);
	TtkTokenKind __fastcall FuncNo(int Index);
	TtkTokenKind __fastcall FuncNot(int Index);
	TtkTokenKind __fastcall FuncObject(int Index);
	TtkTokenKind __fastcall FuncObsolete(int Index);
	TtkTokenKind __fastcall FuncOld(int Index);
	TtkTokenKind __fastcall FuncOnce(int Index);
	TtkTokenKind __fastcall FuncOptimize(int Index);
	TtkTokenKind __fastcall FuncOption(int Index);
	TtkTokenKind __fastcall FuncOr(int Index);
	TtkTokenKind __fastcall FuncPointer(int Index);
	TtkTokenKind __fastcall FuncPrecompiled(int Index);
	TtkTokenKind __fastcall FuncPrecursor(int Index);
	TtkTokenKind __fastcall FuncPrefix(int Index);
	TtkTokenKind __fastcall FuncReal(int Index);
	TtkTokenKind __fastcall FuncRedefine(int Index);
	TtkTokenKind __fastcall FuncRename(int Index);
	TtkTokenKind __fastcall FuncRequire(int Index);
	TtkTokenKind __fastcall FuncRescue(int Index);
	TtkTokenKind __fastcall FuncResult(int Index);
	TtkTokenKind __fastcall FuncRetry(int Index);
	TtkTokenKind __fastcall FuncRoot(int Index);
	TtkTokenKind __fastcall FuncSelect(int Index);
	TtkTokenKind __fastcall FuncSeparate(int Index);
	TtkTokenKind __fastcall FuncString(int Index);
	TtkTokenKind __fastcall FuncStrip(int Index);
	TtkTokenKind __fastcall FuncSystem(int Index);
	TtkTokenKind __fastcall FuncThen(int Index);
	TtkTokenKind __fastcall FuncTrace(int Index);
	TtkTokenKind __fastcall FuncTrue(int Index);
	TtkTokenKind __fastcall FuncUndefine(int Index);
	TtkTokenKind __fastcall FuncUnique(int Index);
	TtkTokenKind __fastcall FuncUntil(int Index);
	TtkTokenKind __fastcall FuncUse(int Index);
	TtkTokenKind __fastcall FuncVariant(int Index);
	TtkTokenKind __fastcall FuncVisible(int Index);
	TtkTokenKind __fastcall FuncVoid(int Index);
	TtkTokenKind __fastcall FuncWhen(int Index);
	TtkTokenKind __fastcall FuncXor(int Index);
	TtkTokenKind __fastcall FuncYes(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall IdentProc(void);
	void __fastcall InitIdent(void);
	void __fastcall OperatorAndSymbolProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall NullProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	void __fastcall EiffelCommentOpenProc(void);
	void __fastcall EiffelCommentProc(void);
	void __fastcall StringOpenProc(void);
	void __fastcall StringProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__fastcall virtual TSynEiffelSyn(System::Classes::TComponent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	virtual void * __fastcall GetRange(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual System::UnicodeString __fastcall GetKeyWords(int TokenKind);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	bool __fastcall IsOperatorChar(System::WideChar AChar);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* BasicTypesAttri = {read=fBasicTypesAttri, write=fBasicTypesAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* LaceAttri = {read=fLaceAttri, write=fLaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* OperatorAndSymbolsAttri = {read=fOperatorAndSymbolsAttri, write=fOperatorAndSymbolsAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PredefinedAttri = {read=fPredefinedAttri, write=fPredefinedAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ResultValueAttri = {read=fResultValueAttri, write=fResultValueAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynEiffelSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightereiffel */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTEREIFFEL)
using namespace Synhighlightereiffel;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightereiffelHPP
