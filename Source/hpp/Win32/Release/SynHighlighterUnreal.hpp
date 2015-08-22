// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterUnreal.pas' rev: 30.00 (Windows)

#ifndef SynhighlighterunrealHPP
#define SynhighlighterunrealHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Win.Registry.hpp>
#include <Winapi.Windows.hpp>
#include <SynEditHighlighter.hpp>
#include <SynEditTypes.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterunreal
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynUnrealSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkDirective, tkIdentifier, tkKey, tkKey2, tkNull, tkNumber, tkSpace, tkString, tkString2, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TxtkTokenKind : unsigned char { xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign, xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma, xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan, xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan, xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr, xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion, xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft, xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose, xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor, xtkXorAssign };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsANil, rsAnsiC, rsDirective, rsDirectiveComment, rsUnKnown };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynUnrealSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	int FRoundCount;
	int FSquareCount;
	TtkTokenKind FTokenID;
	TxtkTokenKind FExtTokenID;
	System::StaticArray<TIdentFuncTableFunc, 733> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDirecAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fInvalidAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKey2Attri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fString2Attri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAbstract(int Index);
	TtkTokenKind __fastcall FuncAlways(int Index);
	TtkTokenKind __fastcall FuncArray(int Index);
	TtkTokenKind __fastcall FuncArraycount(int Index);
	TtkTokenKind __fastcall FuncAssert(int Index);
	TtkTokenKind __fastcall FuncAuto(int Index);
	TtkTokenKind __fastcall FuncAutomated(int Index);
	TtkTokenKind __fastcall FuncBool(int Index);
	TtkTokenKind __fastcall FuncBoundingbox(int Index);
	TtkTokenKind __fastcall FuncBoundingvolume(int Index);
	TtkTokenKind __fastcall FuncBreak(int Index);
	TtkTokenKind __fastcall FuncButton(int Index);
	TtkTokenKind __fastcall FuncByte(int Index);
	TtkTokenKind __fastcall FuncCache(int Index);
	TtkTokenKind __fastcall FuncCacheexempt(int Index);
	TtkTokenKind __fastcall FuncCase(int Index);
	TtkTokenKind __fastcall FuncCatch(int Index);
	TtkTokenKind __fastcall FuncClass(int Index);
	TtkTokenKind __fastcall FuncCoerce(int Index);
	TtkTokenKind __fastcall FuncCollapsecategories(int Index);
	TtkTokenKind __fastcall FuncColor(int Index);
	TtkTokenKind __fastcall FuncConfig(int Index);
	TtkTokenKind __fastcall FuncConst(int Index);
	TtkTokenKind __fastcall FuncContinue(int Index);
	TtkTokenKind __fastcall FuncCoords(int Index);
	TtkTokenKind __fastcall FuncCpptext(int Index);
	TtkTokenKind __fastcall FuncCross(int Index);
	TtkTokenKind __fastcall FuncDefault(int Index);
	TtkTokenKind __fastcall FuncDefaultproperties(int Index);
	TtkTokenKind __fastcall FuncDelegate(int Index);
	TtkTokenKind __fastcall FuncDelete(int Index);
	TtkTokenKind __fastcall FuncDependson(int Index);
	TtkTokenKind __fastcall FuncDeprecated(int Index);
	TtkTokenKind __fastcall FuncDo(int Index);
	TtkTokenKind __fastcall FuncDontcollapsecategories(int Index);
	TtkTokenKind __fastcall FuncDot(int Index);
	TtkTokenKind __fastcall FuncEach(int Index);
	TtkTokenKind __fastcall FuncEdfindable(int Index);
	TtkTokenKind __fastcall FuncEditconst(int Index);
	TtkTokenKind __fastcall FuncEditconstarray(int Index);
	TtkTokenKind __fastcall FuncEditinline(int Index);
	TtkTokenKind __fastcall FuncEditinlinenew(int Index);
	TtkTokenKind __fastcall FuncEditinlinenotify(int Index);
	TtkTokenKind __fastcall FuncEditinlineuse(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncEnum(int Index);
	TtkTokenKind __fastcall FuncEnumcount(int Index);
	TtkTokenKind __fastcall FuncEvent(int Index);
	TtkTokenKind __fastcall FuncExec(int Index);
	TtkTokenKind __fastcall FuncExpands(int Index);
	TtkTokenKind __fastcall FuncExplicit(int Index);
	TtkTokenKind __fastcall FuncExport(int Index);
	TtkTokenKind __fastcall FuncExportstructs(int Index);
	TtkTokenKind __fastcall FuncExtends(int Index);
	TtkTokenKind __fastcall FuncFalse(int Index);
	TtkTokenKind __fastcall FuncFinal(int Index);
	TtkTokenKind __fastcall FuncFloat(int Index);
	TtkTokenKind __fastcall FuncFor(int Index);
	TtkTokenKind __fastcall FuncForeach(int Index);
	TtkTokenKind __fastcall FuncFunction(int Index);
	TtkTokenKind __fastcall FuncGlobal(int Index);
	TtkTokenKind __fastcall FuncGlobalconfig(int Index);
	TtkTokenKind __fastcall FuncGoto(int Index);
	TtkTokenKind __fastcall FuncGuid(int Index);
	TtkTokenKind __fastcall FuncHidecategories(int Index);
	TtkTokenKind __fastcall FuncHidedropdown(int Index);
	TtkTokenKind __fastcall FuncHideparent(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncIgnores(int Index);
	TtkTokenKind __fastcall FuncImport(int Index);
	TtkTokenKind __fastcall FuncInit(int Index);
	TtkTokenKind __fastcall FuncInput(int Index);
	TtkTokenKind __fastcall FuncInsert(int Index);
	TtkTokenKind __fastcall FuncInstanced(int Index);
	TtkTokenKind __fastcall FuncInt(int Index);
	TtkTokenKind __fastcall FuncIntrinsic(int Index);
	TtkTokenKind __fastcall FuncInvariant(int Index);
	TtkTokenKind __fastcall FuncIterator(int Index);
	TtkTokenKind __fastcall FuncLatent(int Index);
	TtkTokenKind __fastcall FuncLength(int Index);
	TtkTokenKind __fastcall FuncLocal(int Index);
	TtkTokenKind __fastcall FuncLocalized(int Index);
	TtkTokenKind __fastcall FuncLong(int Index);
	TtkTokenKind __fastcall FuncMesh(int Index);
	TtkTokenKind __fastcall FuncModel(int Index);
	TtkTokenKind __fastcall FuncMutable(int Index);
	TtkTokenKind __fastcall FuncName(int Index);
	TtkTokenKind __fastcall FuncNative(int Index);
	TtkTokenKind __fastcall FuncNativereplication(int Index);
	TtkTokenKind __fastcall FuncNew(int Index);
	TtkTokenKind __fastcall FuncNoexport(int Index);
	TtkTokenKind __fastcall FuncNone(int Index);
	TtkTokenKind __fastcall FuncNoteditinlinenew(int Index);
	TtkTokenKind __fastcall FuncNotplaceable(int Index);
	TtkTokenKind __fastcall FuncNousercreate(int Index);
	TtkTokenKind __fastcall FuncOperator(int Index);
	TtkTokenKind __fastcall FuncOptional(int Index);
	TtkTokenKind __fastcall FuncOut(int Index);
	TtkTokenKind __fastcall FuncParseconfig(int Index);
	TtkTokenKind __fastcall FuncPerobjectconfig(int Index);
	TtkTokenKind __fastcall FuncPlaceable(int Index);
	TtkTokenKind __fastcall FuncPlane(int Index);
	TtkTokenKind __fastcall FuncPointer(int Index);
	TtkTokenKind __fastcall FuncPostoperator(int Index);
	TtkTokenKind __fastcall FuncPreoperator(int Index);
	TtkTokenKind __fastcall FuncPrivate(int Index);
	TtkTokenKind __fastcall FuncProtected(int Index);
	TtkTokenKind __fastcall FuncRegister(int Index);
	TtkTokenKind __fastcall FuncReliable(int Index);
	TtkTokenKind __fastcall FuncRemove(int Index);
	TtkTokenKind __fastcall FuncReplication(int Index);
	TtkTokenKind __fastcall FuncReturn(int Index);
	TtkTokenKind __fastcall FuncRng(int Index);
	TtkTokenKind __fastcall FuncRot(int Index);
	TtkTokenKind __fastcall FuncRotator(int Index);
	TtkTokenKind __fastcall FuncSafereplace(int Index);
	TtkTokenKind __fastcall FuncScale(int Index);
	TtkTokenKind __fastcall FuncScriptconst(int Index);
	TtkTokenKind __fastcall FuncSelf(int Index);
	TtkTokenKind __fastcall FuncShowcategories(int Index);
	TtkTokenKind __fastcall FuncSimulated(int Index);
	TtkTokenKind __fastcall FuncSingular(int Index);
	TtkTokenKind __fastcall FuncSkip(int Index);
	TtkTokenKind __fastcall FuncSound(int Index);
	TtkTokenKind __fastcall FuncState(int Index);
	TtkTokenKind __fastcall FuncStatic(int Index);
	TtkTokenKind __fastcall FuncStop(int Index);
	TtkTokenKind __fastcall FuncString(int Index);
	TtkTokenKind __fastcall FuncStruct(int Index);
	TtkTokenKind __fastcall FuncSuper(int Index);
	TtkTokenKind __fastcall FuncSwitch(int Index);
	TtkTokenKind __fastcall FuncTexture(int Index);
	TtkTokenKind __fastcall FuncTransient(int Index);
	TtkTokenKind __fastcall FuncTravel(int Index);
	TtkTokenKind __fastcall FuncTrue(int Index);
	TtkTokenKind __fastcall FuncUnreliable(int Index);
	TtkTokenKind __fastcall FuncUntil(int Index);
	TtkTokenKind __fastcall FuncVar(int Index);
	TtkTokenKind __fastcall FuncVect(int Index);
	TtkTokenKind __fastcall FuncVector(int Index);
	TtkTokenKind __fastcall FuncVoid(int Index);
	TtkTokenKind __fastcall FuncWhile(int Index);
	TtkTokenKind __fastcall FuncWithin(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall AnsiCProc(void);
	void __fastcall AndSymbolProc(void);
	void __fastcall AsciiCharProc(void);
	void __fastcall BraceCloseProc(void);
	void __fastcall BraceOpenProc(void);
	void __fastcall CRProc(void);
	void __fastcall ColonProc(void);
	void __fastcall CommaProc(void);
	void __fastcall DirectiveProc(void);
	void __fastcall EqualProc(void);
	void __fastcall GreaterProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall LowerProc(void);
	void __fastcall MinusProc(void);
	void __fastcall ModSymbolProc(void);
	void __fastcall NotSymbolProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall OrSymbolProc(void);
	void __fastcall PlusProc(void);
	void __fastcall PointProc(void);
	void __fastcall QuestionProc(void);
	void __fastcall RoundCloseProc(void);
	void __fastcall RoundOpenProc(void);
	void __fastcall SemiColonProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall SquareCloseProc(void);
	void __fastcall SquareOpenProc(void);
	void __fastcall StarProc(void);
	void __fastcall StringProc(void);
	void __fastcall DollarSignProc(void);
	void __fastcall TildeProc(void);
	void __fastcall XOrSymbolProc(void);
	void __fastcall UnknownProc(void);
	
protected:
	TxtkTokenKind __fastcall GetExtTokenID(void);
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	void __fastcall NextProcedure(void);
	
public:
	__classmethod virtual Synedithighlighter::TSynHighlighterCapabilities __fastcall GetCapabilities();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynUnrealSyn(System::Classes::TComponent* AOwner);
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
	__property TxtkTokenKind ExtTokenID = {read=GetExtTokenID, nodefault};
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DirecAttri = {read=fDirecAttri, write=fDirecAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* InvalidAttri = {read=fInvalidAttri, write=fInvalidAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* Key2Attri = {read=fKey2Attri, write=fKey2Attri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SingleStringAttri = {read=fString2Attri, write=fString2Attri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynUnrealSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterunreal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERUNREAL)
using namespace Synhighlighterunreal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterunrealHPP
