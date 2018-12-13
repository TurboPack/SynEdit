// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterRexx.pas' rev: 32.00 (Windows)

#ifndef SynhighlighterrexxHPP
#define SynhighlighterrexxHPP

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

namespace Synhighlighterrexx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynRexxSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkSpecial, tkStdFunction, tkString, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown, rsMultilineComment, rsSinglelineComment, rsStringDouble, rsStringSingle };

typedef void __fastcall (__closure *TProcTableProc)(void);

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynRexxSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 349> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpecialAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStdFunctionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall FuncAbbrev(int Index);
	TtkTokenKind __fastcall FuncAbs(int Index);
	TtkTokenKind __fastcall FuncAddress(int Index);
	TtkTokenKind __fastcall FuncArg(int Index);
	TtkTokenKind __fastcall FuncB2x(int Index);
	TtkTokenKind __fastcall FuncBitand(int Index);
	TtkTokenKind __fastcall FuncBitor(int Index);
	TtkTokenKind __fastcall FuncBitxor(int Index);
	TtkTokenKind __fastcall FuncC2d(int Index);
	TtkTokenKind __fastcall FuncC2x(int Index);
	TtkTokenKind __fastcall FuncCall(int Index);
	TtkTokenKind __fastcall FuncCenter(int Index);
	TtkTokenKind __fastcall FuncCentre(int Index);
	TtkTokenKind __fastcall FuncChangestr(int Index);
	TtkTokenKind __fastcall FuncCharin(int Index);
	TtkTokenKind __fastcall FuncCharout(int Index);
	TtkTokenKind __fastcall FuncChars(int Index);
	TtkTokenKind __fastcall FuncCompare(int Index);
	TtkTokenKind __fastcall FuncCondition(int Index);
	TtkTokenKind __fastcall FuncCopies(int Index);
	TtkTokenKind __fastcall FuncD2c(int Index);
	TtkTokenKind __fastcall FuncD2x(int Index);
	TtkTokenKind __fastcall FuncDatatype(int Index);
	TtkTokenKind __fastcall FuncDate(int Index);
	TtkTokenKind __fastcall FuncDelstr(int Index);
	TtkTokenKind __fastcall FuncDelword(int Index);
	TtkTokenKind __fastcall FuncDigits(int Index);
	TtkTokenKind __fastcall FuncDo(int Index);
	TtkTokenKind __fastcall FuncDrop(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncEnd(int Index);
	TtkTokenKind __fastcall FuncErrortext(int Index);
	TtkTokenKind __fastcall FuncExit(int Index);
	TtkTokenKind __fastcall FuncForm(int Index);
	TtkTokenKind __fastcall FuncFormat(int Index);
	TtkTokenKind __fastcall FuncFuzz(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncInsert(int Index);
	TtkTokenKind __fastcall FuncInterpret(int Index);
	TtkTokenKind __fastcall FuncIterate(int Index);
	TtkTokenKind __fastcall FuncJustify(int Index);
	TtkTokenKind __fastcall FuncLastpos(int Index);
	TtkTokenKind __fastcall FuncLeave(int Index);
	TtkTokenKind __fastcall FuncLeft(int Index);
	TtkTokenKind __fastcall FuncLength(int Index);
	TtkTokenKind __fastcall FuncLinein(int Index);
	TtkTokenKind __fastcall FuncLineout(int Index);
	TtkTokenKind __fastcall FuncLines(int Index);
	TtkTokenKind __fastcall FuncLinesize(int Index);
	TtkTokenKind __fastcall FuncMax(int Index);
	TtkTokenKind __fastcall FuncMin(int Index);
	TtkTokenKind __fastcall FuncNop(int Index);
	TtkTokenKind __fastcall FuncNumeric(int Index);
	TtkTokenKind __fastcall FuncOptions(int Index);
	TtkTokenKind __fastcall FuncOtherwise(int Index);
	TtkTokenKind __fastcall FuncOverlay(int Index);
	TtkTokenKind __fastcall FuncParse(int Index);
	TtkTokenKind __fastcall FuncPos(int Index);
	TtkTokenKind __fastcall FuncProcedure(int Index);
	TtkTokenKind __fastcall FuncPull(int Index);
	TtkTokenKind __fastcall FuncPush(int Index);
	TtkTokenKind __fastcall FuncQueue(int Index);
	TtkTokenKind __fastcall FuncQueued(int Index);
	TtkTokenKind __fastcall FuncRandom(int Index);
	TtkTokenKind __fastcall FuncReturn(int Index);
	TtkTokenKind __fastcall FuncReverse(int Index);
	TtkTokenKind __fastcall FuncRight(int Index);
	TtkTokenKind __fastcall FuncRxfuncadd(int Index);
	TtkTokenKind __fastcall FuncRxfuncdrop(int Index);
	TtkTokenKind __fastcall FuncRxfuncquery(int Index);
	TtkTokenKind __fastcall FuncSay(int Index);
	TtkTokenKind __fastcall FuncSelect(int Index);
	TtkTokenKind __fastcall FuncSignal(int Index);
	TtkTokenKind __fastcall FuncSourceline(int Index);
	TtkTokenKind __fastcall FuncSpace(int Index);
	TtkTokenKind __fastcall FuncStream(int Index);
	TtkTokenKind __fastcall FuncStrip(int Index);
	TtkTokenKind __fastcall FuncSubstr(int Index);
	TtkTokenKind __fastcall FuncSubword(int Index);
	TtkTokenKind __fastcall FuncSymbol(int Index);
	TtkTokenKind __fastcall FuncThen(int Index);
	TtkTokenKind __fastcall FuncTime(int Index);
	TtkTokenKind __fastcall FuncTrace(int Index);
	TtkTokenKind __fastcall FuncTranslate(int Index);
	TtkTokenKind __fastcall FuncTrunc(int Index);
	TtkTokenKind __fastcall FuncUpper(int Index);
	TtkTokenKind __fastcall FuncValue(int Index);
	TtkTokenKind __fastcall FuncVar(int Index);
	TtkTokenKind __fastcall FuncVerify(int Index);
	TtkTokenKind __fastcall FuncWhen(int Index);
	TtkTokenKind __fastcall FuncWord(int Index);
	TtkTokenKind __fastcall FuncWordindex(int Index);
	TtkTokenKind __fastcall FuncWordlength(int Index);
	TtkTokenKind __fastcall FuncWordpos(int Index);
	TtkTokenKind __fastcall FuncWords(int Index);
	TtkTokenKind __fastcall FuncX2b(int Index);
	TtkTokenKind __fastcall FuncX2c(int Index);
	TtkTokenKind __fastcall FuncX2d(int Index);
	TtkTokenKind __fastcall FuncXrange(int Index);
	void __fastcall IdentProc(void);
	void __fastcall UnknownProc(void);
	TtkTokenKind __fastcall AltFunc(int Index);
	void __fastcall InitIdent(void);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall NullProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	void __fastcall MultilineCommentOpenProc(void);
	void __fastcall MultilineCommentProc(void);
	void __fastcall SinglelineCommentOpenProc(void);
	void __fastcall SinglelineCommentProc(void);
	void __fastcall StringDoubleOpenProc(void);
	void __fastcall StringDoubleProc(void);
	void __fastcall StringSingleOpenProc(void);
	void __fastcall StringSingleProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__fastcall virtual TSynRexxSyn(System::Classes::TComponent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	virtual void * __fastcall GetRange(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual System::UnicodeString __fastcall GetKeyWords(int TokenKind);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpecialAttri = {read=fSpecialAttri, write=fSpecialAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StdFunctionAttri = {read=fStdFunctionAttri, write=fStdFunctionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynRexxSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterrexx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERREXX)
using namespace Synhighlighterrexx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterrexxHPP
