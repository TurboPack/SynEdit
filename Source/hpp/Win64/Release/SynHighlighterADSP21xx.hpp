// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterADSP21xx.pas' rev: 32.00 (Windows)

#ifndef Synhighlighteradsp21xxHPP
#define Synhighlighteradsp21xxHPP

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

namespace Synhighlighteradsp21xx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynADSP21xxSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkCondition, tkIdentifier, tkKey, tkNull, tkNumber, tkRegister, tkSpace, tkString, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown, rsPascalComment, rsCComment, rsHexNumber, rsBinaryNumber, rsInclude };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynADSP21xxSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	System::StaticArray<TIdentFuncTableFunc, 821> fIdentFuncTable;
	TtkTokenKind FTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fRegisterAttri;
	Synedithighlighter::TSynHighlighterAttributes* fConditionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNullAttri;
	Synedithighlighter::TSynHighlighterAttributes* fUnknownAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAbs(int Index);
	TtkTokenKind __fastcall FuncAbstract(int Index);
	TtkTokenKind __fastcall FuncAc(int Index);
	TtkTokenKind __fastcall FuncAf(int Index);
	TtkTokenKind __fastcall FuncAlt95reg(int Index);
	TtkTokenKind __fastcall FuncAnd(int Index);
	TtkTokenKind __fastcall FuncAr(int Index);
	TtkTokenKind __fastcall FuncAr95sat(int Index);
	TtkTokenKind __fastcall FuncAshift(int Index);
	TtkTokenKind __fastcall FuncAstat(int Index);
	TtkTokenKind __fastcall FuncAux(int Index);
	TtkTokenKind __fastcall FuncAv(int Index);
	TtkTokenKind __fastcall FuncAv95latch(int Index);
	TtkTokenKind __fastcall FuncAx0(int Index);
	TtkTokenKind __fastcall FuncAx1(int Index);
	TtkTokenKind __fastcall FuncAy0(int Index);
	TtkTokenKind __fastcall FuncAy1(int Index);
	TtkTokenKind __fastcall FuncB(int Index);
	TtkTokenKind __fastcall FuncBit95rev(int Index);
	TtkTokenKind __fastcall FuncBm(int Index);
	TtkTokenKind __fastcall FuncBoot(int Index);
	TtkTokenKind __fastcall FuncBy(int Index);
	TtkTokenKind __fastcall FuncCache(int Index);
	TtkTokenKind __fastcall FuncCall(int Index);
	TtkTokenKind __fastcall FuncCe(int Index);
	TtkTokenKind __fastcall FuncCirc(int Index);
	TtkTokenKind __fastcall FuncClear(int Index);
	TtkTokenKind __fastcall FuncClr(int Index);
	TtkTokenKind __fastcall FuncClrbit(int Index);
	TtkTokenKind __fastcall FuncCntl(int Index);
	TtkTokenKind __fastcall FuncCntr(int Index);
	TtkTokenKind __fastcall FuncConst(int Index);
	TtkTokenKind __fastcall FuncDefine(int Index);
	TtkTokenKind __fastcall FuncDis(int Index);
	TtkTokenKind __fastcall FuncDivq(int Index);
	TtkTokenKind __fastcall FuncDivs(int Index);
	TtkTokenKind __fastcall FuncDm(int Index);
	TtkTokenKind __fastcall FuncDmovlay(int Index);
	TtkTokenKind __fastcall FuncDo(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncEmode(int Index);
	TtkTokenKind __fastcall FuncEna(int Index);
	TtkTokenKind __fastcall FuncEndif(int Index);
	TtkTokenKind __fastcall FuncEndmacro(int Index);
	TtkTokenKind __fastcall FuncEndmod(int Index);
	TtkTokenKind __fastcall FuncEntry(int Index);
	TtkTokenKind __fastcall FuncEq(int Index);
	TtkTokenKind __fastcall FuncExp(int Index);
	TtkTokenKind __fastcall FuncExpadj(int Index);
	TtkTokenKind __fastcall FuncExternal(int Index);
	TtkTokenKind __fastcall FuncFl0(int Index);
	TtkTokenKind __fastcall FuncFl1(int Index);
	TtkTokenKind __fastcall FuncFl2(int Index);
	TtkTokenKind __fastcall FuncFlag95in(int Index);
	TtkTokenKind __fastcall FuncFlag95out(int Index);
	TtkTokenKind __fastcall FuncFor(int Index);
	TtkTokenKind __fastcall FuncForever(int Index);
	TtkTokenKind __fastcall FuncGe(int Index);
	TtkTokenKind __fastcall FuncGlobal(int Index);
	TtkTokenKind __fastcall FuncGo95mode(int Index);
	TtkTokenKind __fastcall FuncGt(int Index);
	TtkTokenKind __fastcall FuncH(int Index);
	TtkTokenKind __fastcall FuncHi(int Index);
	TtkTokenKind __fastcall FuncI0(int Index);
	TtkTokenKind __fastcall FuncI1(int Index);
	TtkTokenKind __fastcall FuncI2(int Index);
	TtkTokenKind __fastcall FuncI3(int Index);
	TtkTokenKind __fastcall FuncI4(int Index);
	TtkTokenKind __fastcall FuncI5(int Index);
	TtkTokenKind __fastcall FuncI6(int Index);
	TtkTokenKind __fastcall FuncI7(int Index);
	TtkTokenKind __fastcall FuncIcntl(int Index);
	TtkTokenKind __fastcall FuncIdle(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncIfc(int Index);
	TtkTokenKind __fastcall FuncIfdef(int Index);
	TtkTokenKind __fastcall FuncIfndef(int Index);
	TtkTokenKind __fastcall FuncImask(int Index);
	TtkTokenKind __fastcall FuncIn(int Index);
	TtkTokenKind __fastcall FuncInclude(int Index);
	TtkTokenKind __fastcall FuncInit(int Index);
	TtkTokenKind __fastcall FuncIo(int Index);
	TtkTokenKind __fastcall FuncJump(int Index);
	TtkTokenKind __fastcall FuncL0(int Index);
	TtkTokenKind __fastcall FuncL1(int Index);
	TtkTokenKind __fastcall FuncL2(int Index);
	TtkTokenKind __fastcall FuncL3(int Index);
	TtkTokenKind __fastcall FuncL4(int Index);
	TtkTokenKind __fastcall FuncL5(int Index);
	TtkTokenKind __fastcall FuncL6(int Index);
	TtkTokenKind __fastcall FuncL7(int Index);
	TtkTokenKind __fastcall FuncLe(int Index);
	TtkTokenKind __fastcall FuncLo(int Index);
	TtkTokenKind __fastcall FuncLocal(int Index);
	TtkTokenKind __fastcall FuncLoop(int Index);
	TtkTokenKind __fastcall FuncLshift(int Index);
	TtkTokenKind __fastcall FuncLt(int Index);
	TtkTokenKind __fastcall FuncM95mode(int Index);
	TtkTokenKind __fastcall FuncM0(int Index);
	TtkTokenKind __fastcall FuncM1(int Index);
	TtkTokenKind __fastcall FuncM2(int Index);
	TtkTokenKind __fastcall FuncM3(int Index);
	TtkTokenKind __fastcall FuncM4(int Index);
	TtkTokenKind __fastcall FuncM5(int Index);
	TtkTokenKind __fastcall FuncM6(int Index);
	TtkTokenKind __fastcall FuncM7(int Index);
	TtkTokenKind __fastcall FuncMacro(int Index);
	TtkTokenKind __fastcall FuncMf(int Index);
	TtkTokenKind __fastcall FuncModify(int Index);
	TtkTokenKind __fastcall FuncModule(int Index);
	TtkTokenKind __fastcall FuncMr(int Index);
	TtkTokenKind __fastcall FuncMr0(int Index);
	TtkTokenKind __fastcall FuncMr1(int Index);
	TtkTokenKind __fastcall FuncMr2(int Index);
	TtkTokenKind __fastcall FuncMstat(int Index);
	TtkTokenKind __fastcall FuncMv(int Index);
	TtkTokenKind __fastcall FuncMx0(int Index);
	TtkTokenKind __fastcall FuncMx1(int Index);
	TtkTokenKind __fastcall FuncMy0(int Index);
	TtkTokenKind __fastcall FuncMy1(int Index);
	TtkTokenKind __fastcall FuncName(int Index);
	TtkTokenKind __fastcall FuncNe(int Index);
	TtkTokenKind __fastcall FuncNeg(int Index);
	TtkTokenKind __fastcall FuncNewpage(int Index);
	TtkTokenKind __fastcall FuncNop(int Index);
	TtkTokenKind __fastcall FuncNorm(int Index);
	TtkTokenKind __fastcall FuncNot(int Index);
	TtkTokenKind __fastcall FuncOf(int Index);
	TtkTokenKind __fastcall FuncOr(int Index);
	TtkTokenKind __fastcall FuncPass(int Index);
	TtkTokenKind __fastcall FuncPc(int Index);
	TtkTokenKind __fastcall FuncPm(int Index);
	TtkTokenKind __fastcall FuncPop(int Index);
	TtkTokenKind __fastcall FuncPort(int Index);
	TtkTokenKind __fastcall FuncPush(int Index);
	TtkTokenKind __fastcall FuncRam(int Index);
	TtkTokenKind __fastcall FuncRegbank(int Index);
	TtkTokenKind __fastcall FuncReset(int Index);
	TtkTokenKind __fastcall FuncRnd(int Index);
	TtkTokenKind __fastcall FuncRom(int Index);
	TtkTokenKind __fastcall FuncRti(int Index);
	TtkTokenKind __fastcall FuncRts(int Index);
	TtkTokenKind __fastcall FuncRx0(int Index);
	TtkTokenKind __fastcall FuncRx1(int Index);
	TtkTokenKind __fastcall FuncSat(int Index);
	TtkTokenKind __fastcall FuncSb(int Index);
	TtkTokenKind __fastcall FuncSec95reg(int Index);
	TtkTokenKind __fastcall FuncSeg(int Index);
	TtkTokenKind __fastcall FuncSegment(int Index);
	TtkTokenKind __fastcall FuncSet(int Index);
	TtkTokenKind __fastcall FuncSetbit(int Index);
	TtkTokenKind __fastcall FuncShift(int Index);
	TtkTokenKind __fastcall FuncShl(int Index);
	TtkTokenKind __fastcall FuncShr(int Index);
	TtkTokenKind __fastcall FuncSi(int Index);
	TtkTokenKind __fastcall FuncSr(int Index);
	TtkTokenKind __fastcall FuncSr0(int Index);
	TtkTokenKind __fastcall FuncSr1(int Index);
	TtkTokenKind __fastcall FuncSs(int Index);
	TtkTokenKind __fastcall FuncSstat(int Index);
	TtkTokenKind __fastcall FuncStatic(int Index);
	TtkTokenKind __fastcall FuncSts(int Index);
	TtkTokenKind __fastcall FuncSu(int Index);
	TtkTokenKind __fastcall FuncTest(int Index);
	TtkTokenKind __fastcall FuncTestbit(int Index);
	TtkTokenKind __fastcall FuncTglbit(int Index);
	TtkTokenKind __fastcall FuncTimer(int Index);
	TtkTokenKind __fastcall FuncToggle(int Index);
	TtkTokenKind __fastcall FuncTopofpcstack(int Index);
	TtkTokenKind __fastcall FuncTrap(int Index);
	TtkTokenKind __fastcall FuncTrue(int Index);
	TtkTokenKind __fastcall FuncTx0(int Index);
	TtkTokenKind __fastcall FuncTx1(int Index);
	TtkTokenKind __fastcall FuncUndef(int Index);
	TtkTokenKind __fastcall FuncUntil(int Index);
	TtkTokenKind __fastcall FuncUs(int Index);
	TtkTokenKind __fastcall FuncUu(int Index);
	TtkTokenKind __fastcall FuncVar(int Index);
	TtkTokenKind __fastcall FuncXor(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall PascalCommentProc(void);
	void __fastcall BraceCloseProc(void);
	void __fastcall BraceOpenProc(void);
	void __fastcall CCommentProc(void);
	void __fastcall CRProc(void);
	void __fastcall ExclamationProc(void);
	void __fastcall IdentProc(void);
	void __fastcall IntegerProc(void);
	void __fastcall IncludeCloseProc(void);
	void __fastcall LFProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall BinaryNumber(void);
	void __fastcall HexNumber(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringProc(void);
	void __fastcall UnknownProc(void);
	
protected:
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual Synedithighlighter::TSynHighlighterCapabilities __fastcall GetCapabilities();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynADSP21xxSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	virtual bool __fastcall UseUserSettings(int settingIndex);
	virtual void __fastcall EnumUserSettings(System::Classes::TStrings* settings);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ConditionAttri = {read=fConditionAttri, write=fConditionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* RegisterAttri = {read=fRegisterAttri, write=fRegisterAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynADSP21xxSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighteradsp21xx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERADSP21XX)
using namespace Synhighlighteradsp21xx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Synhighlighteradsp21xxHPP
