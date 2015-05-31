// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterPerl.pas' rev: 29.00 (Windows)

#ifndef SynhighlighterperlHPP
#define SynhighlighterperlHPP

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

namespace Synhighlighterperl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynPerlSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkOperator, tkPragma, tkSpace, tkString, tkSymbol, tkUnknown, tkVariable };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynPerlSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TtkTokenKind FTokenID;
	System::StaticArray<TIdentFuncTableFunc, 2423> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fInvalidAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fOperatorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPragmaAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVariableAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall Func36accumulator(int Index);
	TtkTokenKind __fastcall Func36arg(int Index);
	TtkTokenKind __fastcall Func36argv(int Index);
	TtkTokenKind __fastcall Func36basetime(int Index);
	TtkTokenKind __fastcall Func36child95error(int Index);
	TtkTokenKind __fastcall Func36debugging(int Index);
	TtkTokenKind __fastcall Func36effective95group95id(int Index);
	TtkTokenKind __fastcall Func36effective95user95id(int Index);
	TtkTokenKind __fastcall Func36egid(int Index);
	TtkTokenKind __fastcall Func36env(int Index);
	TtkTokenKind __fastcall Func36errno(int Index);
	TtkTokenKind __fastcall Func36euid(int Index);
	TtkTokenKind __fastcall Func36eval95error(int Index);
	TtkTokenKind __fastcall Func36executable95name(int Index);
	TtkTokenKind __fastcall Func36format95formfeed(int Index);
	TtkTokenKind __fastcall Func36format95line95break95characters(int Index);
	TtkTokenKind __fastcall Func36format95lines95left(int Index);
	TtkTokenKind __fastcall Func36format95lines95per95page(int Index);
	TtkTokenKind __fastcall Func36format95name(int Index);
	TtkTokenKind __fastcall Func36format95page95number(int Index);
	TtkTokenKind __fastcall Func36format95top95name(int Index);
	TtkTokenKind __fastcall Func36gid(int Index);
	TtkTokenKind __fastcall Func36inplace95edit(int Index);
	TtkTokenKind __fastcall Func36input95line95number(int Index);
	TtkTokenKind __fastcall Func36input95record95separator(int Index);
	TtkTokenKind __fastcall Func36last95paren95match(int Index);
	TtkTokenKind __fastcall Func36list95separator(int Index);
	TtkTokenKind __fastcall Func36match(int Index);
	TtkTokenKind __fastcall Func36multiline95matching(int Index);
	TtkTokenKind __fastcall Func36nr(int Index);
	TtkTokenKind __fastcall Func36ofmt(int Index);
	TtkTokenKind __fastcall Func36ors(int Index);
	TtkTokenKind __fastcall Func36os95error(int Index);
	TtkTokenKind __fastcall Func36output95autoflush(int Index);
	TtkTokenKind __fastcall Func36output95field95separator(int Index);
	TtkTokenKind __fastcall Func36perl95version(int Index);
	TtkTokenKind __fastcall Func36perldb(int Index);
	TtkTokenKind __fastcall Func36pid(int Index);
	TtkTokenKind __fastcall Func36postmatch(int Index);
	TtkTokenKind __fastcall Func36prematch(int Index);
	TtkTokenKind __fastcall Func36process95id(int Index);
	TtkTokenKind __fastcall Func36program95name(int Index);
	TtkTokenKind __fastcall Func36real95group95id(int Index);
	TtkTokenKind __fastcall Func36real95user95id(int Index);
	TtkTokenKind __fastcall Func36rs(int Index);
	TtkTokenKind __fastcall Func36sig(int Index);
	TtkTokenKind __fastcall Func36subscript95separator(int Index);
	TtkTokenKind __fastcall Func36subsep(int Index);
	TtkTokenKind __fastcall Func36system95fd95max(int Index);
	TtkTokenKind __fastcall Func36uid(int Index);
	TtkTokenKind __fastcall Func36warning(int Index);
	TtkTokenKind __fastcall Func37inc(int Index);
	TtkTokenKind __fastcall Func64argv(int Index);
	TtkTokenKind __fastcall Func64inc(int Index);
	TtkTokenKind __fastcall FuncAbs(int Index);
	TtkTokenKind __fastcall FuncAccept(int Index);
	TtkTokenKind __fastcall FuncAlarm(int Index);
	TtkTokenKind __fastcall FuncAnd(int Index);
	TtkTokenKind __fastcall FuncAtan2(int Index);
	TtkTokenKind __fastcall FuncBind(int Index);
	TtkTokenKind __fastcall FuncBinmode(int Index);
	TtkTokenKind __fastcall FuncBless(int Index);
	TtkTokenKind __fastcall FuncCaller(int Index);
	TtkTokenKind __fastcall FuncChdir(int Index);
	TtkTokenKind __fastcall FuncChmod(int Index);
	TtkTokenKind __fastcall FuncChomp(int Index);
	TtkTokenKind __fastcall FuncChop(int Index);
	TtkTokenKind __fastcall FuncChown(int Index);
	TtkTokenKind __fastcall FuncChr(int Index);
	TtkTokenKind __fastcall FuncChroot(int Index);
	TtkTokenKind __fastcall FuncClose(int Index);
	TtkTokenKind __fastcall FuncClosedir(int Index);
	TtkTokenKind __fastcall FuncCmp(int Index);
	TtkTokenKind __fastcall FuncConnect(int Index);
	TtkTokenKind __fastcall FuncConstant(int Index);
	TtkTokenKind __fastcall FuncCos(int Index);
	TtkTokenKind __fastcall FuncCrypt(int Index);
	TtkTokenKind __fastcall FuncDbmclose(int Index);
	TtkTokenKind __fastcall FuncDbmopen(int Index);
	TtkTokenKind __fastcall FuncDefined(int Index);
	TtkTokenKind __fastcall FuncDelete(int Index);
	TtkTokenKind __fastcall FuncDiagnostics(int Index);
	TtkTokenKind __fastcall FuncDie(int Index);
	TtkTokenKind __fastcall FuncDo(int Index);
	TtkTokenKind __fastcall FuncDump(int Index);
	TtkTokenKind __fastcall FuncEach(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncElsif(int Index);
	TtkTokenKind __fastcall FuncEndgrent(int Index);
	TtkTokenKind __fastcall FuncEndhostent(int Index);
	TtkTokenKind __fastcall FuncEndnetent(int Index);
	TtkTokenKind __fastcall FuncEndprotoent(int Index);
	TtkTokenKind __fastcall FuncEndpwent(int Index);
	TtkTokenKind __fastcall FuncEndservent(int Index);
	TtkTokenKind __fastcall FuncEof(int Index);
	TtkTokenKind __fastcall FuncEq(int Index);
	TtkTokenKind __fastcall FuncEval(int Index);
	TtkTokenKind __fastcall FuncExec(int Index);
	TtkTokenKind __fastcall FuncExists(int Index);
	TtkTokenKind __fastcall FuncExit(int Index);
	TtkTokenKind __fastcall FuncExp(int Index);
	TtkTokenKind __fastcall FuncFcntl(int Index);
	TtkTokenKind __fastcall FuncFileno(int Index);
	TtkTokenKind __fastcall FuncFlock(int Index);
	TtkTokenKind __fastcall FuncFor(int Index);
	TtkTokenKind __fastcall FuncForeach(int Index);
	TtkTokenKind __fastcall FuncFork(int Index);
	TtkTokenKind __fastcall FuncFormat(int Index);
	TtkTokenKind __fastcall FuncFormline(int Index);
	TtkTokenKind __fastcall FuncGe(int Index);
	TtkTokenKind __fastcall FuncGetc(int Index);
	TtkTokenKind __fastcall FuncGetgrent(int Index);
	TtkTokenKind __fastcall FuncGetgrgid(int Index);
	TtkTokenKind __fastcall FuncGetgrnam(int Index);
	TtkTokenKind __fastcall FuncGethostbyaddr(int Index);
	TtkTokenKind __fastcall FuncGethostbyname(int Index);
	TtkTokenKind __fastcall FuncGethostent(int Index);
	TtkTokenKind __fastcall FuncGetlogin(int Index);
	TtkTokenKind __fastcall FuncGetnetbyaddr(int Index);
	TtkTokenKind __fastcall FuncGetnetbyname(int Index);
	TtkTokenKind __fastcall FuncGetnetent(int Index);
	TtkTokenKind __fastcall FuncGetpeername(int Index);
	TtkTokenKind __fastcall FuncGetpgrp(int Index);
	TtkTokenKind __fastcall FuncGetppid(int Index);
	TtkTokenKind __fastcall FuncGetpriority(int Index);
	TtkTokenKind __fastcall FuncGetprotobyname(int Index);
	TtkTokenKind __fastcall FuncGetprotobynumber(int Index);
	TtkTokenKind __fastcall FuncGetprotoent(int Index);
	TtkTokenKind __fastcall FuncGetpwent(int Index);
	TtkTokenKind __fastcall FuncGetpwnam(int Index);
	TtkTokenKind __fastcall FuncGetpwuid(int Index);
	TtkTokenKind __fastcall FuncGetservbyname(int Index);
	TtkTokenKind __fastcall FuncGetservbyport(int Index);
	TtkTokenKind __fastcall FuncGetservent(int Index);
	TtkTokenKind __fastcall FuncGetsockname(int Index);
	TtkTokenKind __fastcall FuncGetsockopt(int Index);
	TtkTokenKind __fastcall FuncGlob(int Index);
	TtkTokenKind __fastcall FuncGmtime(int Index);
	TtkTokenKind __fastcall FuncGoto(int Index);
	TtkTokenKind __fastcall FuncGrep(int Index);
	TtkTokenKind __fastcall FuncGt(int Index);
	TtkTokenKind __fastcall FuncHex(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncImport(int Index);
	TtkTokenKind __fastcall FuncIndex(int Index);
	TtkTokenKind __fastcall FuncInt(int Index);
	TtkTokenKind __fastcall FuncInteger(int Index);
	TtkTokenKind __fastcall FuncIoctl(int Index);
	TtkTokenKind __fastcall FuncJoin(int Index);
	TtkTokenKind __fastcall FuncKeys(int Index);
	TtkTokenKind __fastcall FuncKill(int Index);
	TtkTokenKind __fastcall FuncLast(int Index);
	TtkTokenKind __fastcall FuncLc(int Index);
	TtkTokenKind __fastcall FuncLcfirst(int Index);
	TtkTokenKind __fastcall FuncLe(int Index);
	TtkTokenKind __fastcall FuncLength(int Index);
	TtkTokenKind __fastcall FuncLess(int Index);
	TtkTokenKind __fastcall FuncLink(int Index);
	TtkTokenKind __fastcall FuncListen(int Index);
	TtkTokenKind __fastcall FuncLocal(int Index);
	TtkTokenKind __fastcall FuncLocale(int Index);
	TtkTokenKind __fastcall FuncLocaltime(int Index);
	TtkTokenKind __fastcall FuncLog(int Index);
	TtkTokenKind __fastcall FuncLstat(int Index);
	TtkTokenKind __fastcall FuncLt(int Index);
	TtkTokenKind __fastcall FuncM(int Index);
	TtkTokenKind __fastcall FuncMap(int Index);
	TtkTokenKind __fastcall FuncMkdir(int Index);
	TtkTokenKind __fastcall FuncMsgctl(int Index);
	TtkTokenKind __fastcall FuncMsgget(int Index);
	TtkTokenKind __fastcall FuncMsgrcv(int Index);
	TtkTokenKind __fastcall FuncMsgsnd(int Index);
	TtkTokenKind __fastcall FuncMy(int Index);
	TtkTokenKind __fastcall FuncNe(int Index);
	TtkTokenKind __fastcall FuncNext(int Index);
	TtkTokenKind __fastcall FuncNo(int Index);
	TtkTokenKind __fastcall FuncNot(int Index);
	TtkTokenKind __fastcall FuncOct(int Index);
	TtkTokenKind __fastcall FuncOpen(int Index);
	TtkTokenKind __fastcall FuncOpendir(int Index);
	TtkTokenKind __fastcall FuncOr(int Index);
	TtkTokenKind __fastcall FuncOrd(int Index);
	TtkTokenKind __fastcall FuncPack(int Index);
	TtkTokenKind __fastcall FuncPackage(int Index);
	TtkTokenKind __fastcall FuncPipe(int Index);
	TtkTokenKind __fastcall FuncPop(int Index);
	TtkTokenKind __fastcall FuncPos(int Index);
	TtkTokenKind __fastcall FuncPrint(int Index);
	TtkTokenKind __fastcall FuncPush(int Index);
	TtkTokenKind __fastcall FuncQ(int Index);
	TtkTokenKind __fastcall FuncQq(int Index);
	TtkTokenKind __fastcall FuncQuotemeta(int Index);
	TtkTokenKind __fastcall FuncQw(int Index);
	TtkTokenKind __fastcall FuncQx(int Index);
	TtkTokenKind __fastcall FuncRand(int Index);
	TtkTokenKind __fastcall FuncRead(int Index);
	TtkTokenKind __fastcall FuncReaddir(int Index);
	TtkTokenKind __fastcall FuncReadlink(int Index);
	TtkTokenKind __fastcall FuncRecv(int Index);
	TtkTokenKind __fastcall FuncRedo(int Index);
	TtkTokenKind __fastcall FuncRef(int Index);
	TtkTokenKind __fastcall FuncRename(int Index);
	TtkTokenKind __fastcall FuncRequire(int Index);
	TtkTokenKind __fastcall FuncReset(int Index);
	TtkTokenKind __fastcall FuncReturn(int Index);
	TtkTokenKind __fastcall FuncReverse(int Index);
	TtkTokenKind __fastcall FuncRewinddir(int Index);
	TtkTokenKind __fastcall FuncRindex(int Index);
	TtkTokenKind __fastcall FuncRmdir(int Index);
	TtkTokenKind __fastcall FuncScalar(int Index);
	TtkTokenKind __fastcall FuncSeek(int Index);
	TtkTokenKind __fastcall FuncSeekdir(int Index);
	TtkTokenKind __fastcall FuncSelect(int Index);
	TtkTokenKind __fastcall FuncSemctl(int Index);
	TtkTokenKind __fastcall FuncSemget(int Index);
	TtkTokenKind __fastcall FuncSemop(int Index);
	TtkTokenKind __fastcall FuncSend(int Index);
	TtkTokenKind __fastcall FuncSetgrent(int Index);
	TtkTokenKind __fastcall FuncSethostent(int Index);
	TtkTokenKind __fastcall FuncSetnetent(int Index);
	TtkTokenKind __fastcall FuncSetpgrp(int Index);
	TtkTokenKind __fastcall FuncSetpriority(int Index);
	TtkTokenKind __fastcall FuncSetprotoent(int Index);
	TtkTokenKind __fastcall FuncSetpwent(int Index);
	TtkTokenKind __fastcall FuncSetservent(int Index);
	TtkTokenKind __fastcall FuncSetsockopt(int Index);
	TtkTokenKind __fastcall FuncShift(int Index);
	TtkTokenKind __fastcall FuncShmctl(int Index);
	TtkTokenKind __fastcall FuncShmget(int Index);
	TtkTokenKind __fastcall FuncShmread(int Index);
	TtkTokenKind __fastcall FuncShmwrite(int Index);
	TtkTokenKind __fastcall FuncShutdown(int Index);
	TtkTokenKind __fastcall FuncSigtrap(int Index);
	TtkTokenKind __fastcall FuncSin(int Index);
	TtkTokenKind __fastcall FuncSleep(int Index);
	TtkTokenKind __fastcall FuncSocket(int Index);
	TtkTokenKind __fastcall FuncSocketpair(int Index);
	TtkTokenKind __fastcall FuncSort(int Index);
	TtkTokenKind __fastcall FuncSplice(int Index);
	TtkTokenKind __fastcall FuncSplit(int Index);
	TtkTokenKind __fastcall FuncSprintf(int Index);
	TtkTokenKind __fastcall FuncSqrt(int Index);
	TtkTokenKind __fastcall FuncSrand(int Index);
	TtkTokenKind __fastcall FuncStat(int Index);
	TtkTokenKind __fastcall FuncStrict(int Index);
	TtkTokenKind __fastcall FuncStudy(int Index);
	TtkTokenKind __fastcall FuncSub(int Index);
	TtkTokenKind __fastcall FuncSubs(int Index);
	TtkTokenKind __fastcall FuncSubstr(int Index);
	TtkTokenKind __fastcall FuncSymlink(int Index);
	TtkTokenKind __fastcall FuncSyscall(int Index);
	TtkTokenKind __fastcall FuncSysread(int Index);
	TtkTokenKind __fastcall FuncSystem(int Index);
	TtkTokenKind __fastcall FuncSyswrite(int Index);
	TtkTokenKind __fastcall FuncTell(int Index);
	TtkTokenKind __fastcall FuncTelldir(int Index);
	TtkTokenKind __fastcall FuncTie(int Index);
	TtkTokenKind __fastcall FuncTime(int Index);
	TtkTokenKind __fastcall FuncTimes(int Index);
	TtkTokenKind __fastcall FuncTr(int Index);
	TtkTokenKind __fastcall FuncTruncate(int Index);
	TtkTokenKind __fastcall FuncUc(int Index);
	TtkTokenKind __fastcall FuncUcfirst(int Index);
	TtkTokenKind __fastcall FuncUmask(int Index);
	TtkTokenKind __fastcall FuncUndef(int Index);
	TtkTokenKind __fastcall FuncUnless(int Index);
	TtkTokenKind __fastcall FuncUnlink(int Index);
	TtkTokenKind __fastcall FuncUnpack(int Index);
	TtkTokenKind __fastcall FuncUnshift(int Index);
	TtkTokenKind __fastcall FuncUntie(int Index);
	TtkTokenKind __fastcall FuncUse(int Index);
	TtkTokenKind __fastcall FuncUtime(int Index);
	TtkTokenKind __fastcall FuncValues(int Index);
	TtkTokenKind __fastcall FuncVars(int Index);
	TtkTokenKind __fastcall FuncVec(int Index);
	TtkTokenKind __fastcall FuncWait(int Index);
	TtkTokenKind __fastcall FuncWaitpid(int Index);
	TtkTokenKind __fastcall FuncWantarray(int Index);
	TtkTokenKind __fastcall FuncWarn(int Index);
	TtkTokenKind __fastcall FuncWhile(int Index);
	TtkTokenKind __fastcall FuncWrite(int Index);
	TtkTokenKind __fastcall FuncXor(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall AndSymbolProc(void);
	void __fastcall CRProc(void);
	void __fastcall ColonProc(void);
	void __fastcall CommentProc(void);
	void __fastcall EqualProc(void);
	void __fastcall GreaterProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall LowerProc(void);
	void __fastcall MinusProc(void);
	void __fastcall NotSymbolProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall OrSymbolProc(void);
	void __fastcall PlusProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StarProc(void);
	void __fastcall StringInterpProc(void);
	void __fastcall StringLiteralProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall XOrSymbolProc(void);
	void __fastcall UnknownProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynPerlSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* InvalidAttri = {read=fInvalidAttri, write=fInvalidAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* OperatorAttri = {read=fOperatorAttri, write=fOperatorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PragmaAttri = {read=fPragmaAttri, write=fPragmaAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VariableAttri = {read=fVariableAttri, write=fVariableAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynPerlSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterperl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERPERL)
using namespace Synhighlighterperl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterperlHPP
