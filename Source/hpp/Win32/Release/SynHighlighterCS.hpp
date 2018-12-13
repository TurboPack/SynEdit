// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterCS.pas' rev: 33.00 (Windows)

#ifndef SynhighlightercsHPP
#define SynhighlightercsHPP

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
#include <SynEditMiscClasses.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlightercs
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynCSSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkAsm, tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TxtkTokenKind : unsigned char { xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign, xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma, xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan, xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan, xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr, xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion, xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft, xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose, xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor, xtkXorAssign };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm, rsAsmBlock, rsDirective, rsDirectiveComment, rsString34, rsString39, rsMultiLineString };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynCSSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	bool fAsmStart;
	TRangeState fRange;
	TtkTokenKind FTokenID;
	TxtkTokenKind FExtTokenID;
	System::StaticArray<TIdentFuncTableFunc, 211> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fAsmAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDirecAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fInvalidAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAbstract(int Index);
	TtkTokenKind __fastcall FuncAs(int Index);
	TtkTokenKind __fastcall FuncBase(int Index);
	TtkTokenKind __fastcall FuncBool(int Index);
	TtkTokenKind __fastcall FuncBreak(int Index);
	TtkTokenKind __fastcall FuncByte(int Index);
	TtkTokenKind __fastcall FuncCase(int Index);
	TtkTokenKind __fastcall FuncCatch(int Index);
	TtkTokenKind __fastcall FuncChar(int Index);
	TtkTokenKind __fastcall FuncChecked(int Index);
	TtkTokenKind __fastcall FuncClass(int Index);
	TtkTokenKind __fastcall FuncConst(int Index);
	TtkTokenKind __fastcall FuncContinue(int Index);
	TtkTokenKind __fastcall FuncDecimal(int Index);
	TtkTokenKind __fastcall FuncDefault(int Index);
	TtkTokenKind __fastcall FuncDelegate(int Index);
	TtkTokenKind __fastcall FuncDo(int Index);
	TtkTokenKind __fastcall FuncDouble(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncEnum(int Index);
	TtkTokenKind __fastcall FuncEvent(int Index);
	TtkTokenKind __fastcall FuncExplicit(int Index);
	TtkTokenKind __fastcall FuncExtern(int Index);
	TtkTokenKind __fastcall FuncFalse(int Index);
	TtkTokenKind __fastcall FuncFinally(int Index);
	TtkTokenKind __fastcall FuncFixed(int Index);
	TtkTokenKind __fastcall FuncFloat(int Index);
	TtkTokenKind __fastcall FuncFor(int Index);
	TtkTokenKind __fastcall FuncForeach(int Index);
	TtkTokenKind __fastcall FuncGoto(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncImplicit(int Index);
	TtkTokenKind __fastcall FuncIn(int Index);
	TtkTokenKind __fastcall FuncInt(int Index);
	TtkTokenKind __fastcall FuncInterface(int Index);
	TtkTokenKind __fastcall FuncInternal(int Index);
	TtkTokenKind __fastcall FuncIs(int Index);
	TtkTokenKind __fastcall FuncLock(int Index);
	TtkTokenKind __fastcall FuncLong(int Index);
	TtkTokenKind __fastcall FuncNamespace(int Index);
	TtkTokenKind __fastcall FuncNew(int Index);
	TtkTokenKind __fastcall FuncNull(int Index);
	TtkTokenKind __fastcall FuncObject(int Index);
	TtkTokenKind __fastcall FuncOperator(int Index);
	TtkTokenKind __fastcall FuncOut(int Index);
	TtkTokenKind __fastcall FuncOverride(int Index);
	TtkTokenKind __fastcall FuncParams(int Index);
	TtkTokenKind __fastcall FuncPrivate(int Index);
	TtkTokenKind __fastcall FuncProtected(int Index);
	TtkTokenKind __fastcall FuncPublic(int Index);
	TtkTokenKind __fastcall FuncReadonly(int Index);
	TtkTokenKind __fastcall FuncRef(int Index);
	TtkTokenKind __fastcall FuncReturn(int Index);
	TtkTokenKind __fastcall FuncSbyte(int Index);
	TtkTokenKind __fastcall FuncSealed(int Index);
	TtkTokenKind __fastcall FuncSizeof(int Index);
	TtkTokenKind __fastcall FuncStackalloc(int Index);
	TtkTokenKind __fastcall FuncStatic(int Index);
	TtkTokenKind __fastcall FuncString(int Index);
	TtkTokenKind __fastcall FuncStruct(int Index);
	TtkTokenKind __fastcall FuncSwitch(int Index);
	TtkTokenKind __fastcall FuncThis(int Index);
	TtkTokenKind __fastcall FuncThrow(int Index);
	TtkTokenKind __fastcall FuncTrue(int Index);
	TtkTokenKind __fastcall FuncTry(int Index);
	TtkTokenKind __fastcall FuncTypeof(int Index);
	TtkTokenKind __fastcall FuncUint(int Index);
	TtkTokenKind __fastcall FuncUlong(int Index);
	TtkTokenKind __fastcall FuncUnchecked(int Index);
	TtkTokenKind __fastcall FuncUnsafe(int Index);
	TtkTokenKind __fastcall FuncUshort(int Index);
	TtkTokenKind __fastcall FuncUsing(int Index);
	TtkTokenKind __fastcall FuncVirtual(int Index);
	TtkTokenKind __fastcall FuncVoid(int Index);
	TtkTokenKind __fastcall FuncWhile(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent();
	void __fastcall AnsiCProc();
	void __fastcall AndSymbolProc();
	void __fastcall AsciiCharProc();
	void __fastcall AtSymbolProc();
	void __fastcall BraceCloseProc();
	void __fastcall BraceOpenProc();
	void __fastcall CRProc();
	void __fastcall ColonProc();
	void __fastcall CommaProc();
	void __fastcall DirectiveProc();
	void __fastcall EqualProc();
	void __fastcall GreaterProc();
	void __fastcall IdentProc();
	void __fastcall LFProc();
	void __fastcall LowerProc();
	void __fastcall MinusProc();
	void __fastcall ModSymbolProc();
	void __fastcall NotSymbolProc();
	void __fastcall NullProc();
	void __fastcall NumberProc();
	void __fastcall OrSymbolProc();
	void __fastcall PlusProc();
	void __fastcall PointProc();
	void __fastcall QuestionProc();
	void __fastcall RoundCloseProc();
	void __fastcall RoundOpenProc();
	void __fastcall SemiColonProc();
	void __fastcall SlashProc();
	void __fastcall SpaceProc();
	void __fastcall SquareCloseProc();
	void __fastcall SquareOpenProc();
	void __fastcall StarProc();
	void __fastcall StringProc();
	void __fastcall TildeProc();
	void __fastcall XOrSymbolProc();
	void __fastcall UnknownProc();
	void __fastcall StringEndProc();
	
protected:
	TxtkTokenKind __fastcall GetExtTokenID();
	virtual bool __fastcall IsFilterStored();
	virtual System::UnicodeString __fastcall GetSampleSource();
	void __fastcall NextProcedure();
	
public:
	__classmethod virtual Synedithighlighter::TSynHighlighterCapabilities __fastcall GetCapabilities();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynCSSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	virtual void * __fastcall GetRange();
	TtkTokenKind __fastcall GetTokenID();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	virtual int __fastcall GetTokenKind();
	virtual void __fastcall Next();
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange();
	virtual bool __fastcall UseUserSettings(int settingIndex);
	virtual void __fastcall EnumUserSettings(System::Classes::TStrings* settings);
	__property TxtkTokenKind ExtTokenID = {read=GetExtTokenID, nodefault};
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* AsmAttri = {read=fAsmAttri, write=fAsmAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DirecAttri = {read=fDirecAttri, write=fDirecAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* InvalidAttri = {read=fInvalidAttri, write=fInvalidAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynCSSyn() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightercs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERCS)
using namespace Synhighlightercs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightercsHPP
