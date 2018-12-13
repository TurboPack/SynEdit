// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterCPM.pas' rev: 33.00 (Windows)

#ifndef SynhighlightercpmHPP
#define SynhighlightercpmHPP

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

namespace Synhighlightercpm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynCPMSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkSQLKey, tkString, tkSymbol, tkSpecialVar, tkSystem, tkVariable, tkNumber, tkUnknown };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

enum DECLSPEC_DENUM TRangeState : unsigned char { rsBraceComment, rsUnKnown };

class PASCALIMPLEMENTATION TSynCPMSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	int fCommentLevel;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 797> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSQLKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpecialVarAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSystemAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVariableAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAllentities(int Index);
	TtkTokenKind __fastcall FuncAllproducts(int Index);
	TtkTokenKind __fastcall FuncAllproperties(int Index);
	TtkTokenKind __fastcall FuncAllqualityproperties(int Index);
	TtkTokenKind __fastcall FuncAllsuppliers(int Index);
	TtkTokenKind __fastcall FuncAssign(int Index);
	TtkTokenKind __fastcall FuncBegin(int Index);
	TtkTokenKind __fastcall FuncBlock(int Index);
	TtkTokenKind __fastcall FuncCase(int Index);
	TtkTokenKind __fastcall FuncCategory(int Index);
	TtkTokenKind __fastcall FuncCenterstr(int Index);
	TtkTokenKind __fastcall FuncCharreplacestr(int Index);
	TtkTokenKind __fastcall FuncCharrlenstr(int Index);
	TtkTokenKind __fastcall FuncCharrllenstr(int Index);
	TtkTokenKind __fastcall FuncChr(int Index);
	TtkTokenKind __fastcall FuncClient(int Index);
	TtkTokenKind __fastcall FuncConstants(int Index);
	TtkTokenKind __fastcall FuncContinue(int Index);
	TtkTokenKind __fastcall FuncCopyfile(int Index);
	TtkTokenKind __fastcall FuncCountry(int Index);
	TtkTokenKind __fastcall FuncDecr(int Index);
	TtkTokenKind __fastcall FuncDefinition(int Index);
	TtkTokenKind __fastcall FuncDistinct_execute(int Index);
	TtkTokenKind __fastcall FuncDivide(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncEmptysheet(int Index);
	TtkTokenKind __fastcall FuncEnd(int Index);
	TtkTokenKind __fastcall FuncEntitycode(int Index);
	TtkTokenKind __fastcall FuncEqualstring(int Index);
	TtkTokenKind __fastcall FuncEqualvalue(int Index);
	TtkTokenKind __fastcall FuncExecute(int Index);
	TtkTokenKind __fastcall FuncFileappend(int Index);
	TtkTokenKind __fastcall FuncFileassign(int Index);
	TtkTokenKind __fastcall FuncFileclose(int Index);
	TtkTokenKind __fastcall FuncFilecopy(int Index);
	TtkTokenKind __fastcall FuncFiledate(int Index);
	TtkTokenKind __fastcall FuncFiledelete(int Index);
	TtkTokenKind __fastcall FuncFileend(int Index);
	TtkTokenKind __fastcall FuncFileexists(int Index);
	TtkTokenKind __fastcall FuncFilereadln(int Index);
	TtkTokenKind __fastcall FuncFilereset(int Index);
	TtkTokenKind __fastcall FuncFilerewrite(int Index);
	TtkTokenKind __fastcall FuncFilesize(int Index);
	TtkTokenKind __fastcall FuncFilesort(int Index);
	TtkTokenKind __fastcall FuncFiletime(int Index);
	TtkTokenKind __fastcall FuncFilewriteln(int Index);
	TtkTokenKind __fastcall FuncFilterstr(int Index);
	TtkTokenKind __fastcall FuncFirstinstance(int Index);
	TtkTokenKind __fastcall FuncFlow(int Index);
	TtkTokenKind __fastcall FuncFold(int Index);
	TtkTokenKind __fastcall FuncForeign(int Index);
	TtkTokenKind __fastcall FuncGlobalconstants(int Index);
	TtkTokenKind __fastcall FuncGlobals(int Index);
	TtkTokenKind __fastcall FuncGlobalvariables(int Index);
	TtkTokenKind __fastcall FuncGroupdown(int Index);
	TtkTokenKind __fastcall FuncGroupfooter(int Index);
	TtkTokenKind __fastcall FuncGroupheader(int Index);
	TtkTokenKind __fastcall FuncGroupkey(int Index);
	TtkTokenKind __fastcall FuncGroupup(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncInclude(int Index);
	TtkTokenKind __fastcall FuncIncr(int Index);
	TtkTokenKind __fastcall FuncLanguage(int Index);
	TtkTokenKind __fastcall FuncLastinstance(int Index);
	TtkTokenKind __fastcall FuncLeftstr(int Index);
	TtkTokenKind __fastcall FuncLength(int Index);
	TtkTokenKind __fastcall FuncLlenstr(int Index);
	TtkTokenKind __fastcall FuncLocal(int Index);
	TtkTokenKind __fastcall FuncLocasestr(int Index);
	TtkTokenKind __fastcall FuncLoop(int Index);
	TtkTokenKind __fastcall FuncLowerlevelstoo(int Index);
	TtkTokenKind __fastcall FuncLtrunc(int Index);
	TtkTokenKind __fastcall FuncMatching(int Index);
	TtkTokenKind __fastcall FuncMember(int Index);
	TtkTokenKind __fastcall FuncMerge(int Index);
	TtkTokenKind __fastcall FuncMessagedlg(int Index);
	TtkTokenKind __fastcall FuncMetaflow(int Index);
	TtkTokenKind __fastcall FuncMidstr(int Index);
	TtkTokenKind __fastcall FuncMultiply(int Index);
	TtkTokenKind __fastcall FuncNextinstance(int Index);
	TtkTokenKind __fastcall FuncNextrepeatinstance(int Index);
	TtkTokenKind __fastcall FuncOf(int Index);
	TtkTokenKind __fastcall FuncOptions(int Index);
	TtkTokenKind __fastcall FuncOrganisation(int Index);
	TtkTokenKind __fastcall FuncOutput(int Index);
	TtkTokenKind __fastcall FuncParam(int Index);
	TtkTokenKind __fastcall FuncParent(int Index);
	TtkTokenKind __fastcall FuncParseinc(int Index);
	TtkTokenKind __fastcall FuncPdriver(int Index);
	TtkTokenKind __fastcall FuncPrevinstance(int Index);
	TtkTokenKind __fastcall FuncPrevrepeatinstance(int Index);
	TtkTokenKind __fastcall FuncPrinter(int Index);
	TtkTokenKind __fastcall FuncPrintfile(int Index);
	TtkTokenKind __fastcall FuncPropertygroup(int Index);
	TtkTokenKind __fastcall FuncRastr(int Index);
	TtkTokenKind __fastcall FuncRaval(int Index);
	TtkTokenKind __fastcall FuncReadinstance(int Index);
	TtkTokenKind __fastcall FuncReadrepeatinstance(int Index);
	TtkTokenKind __fastcall FuncRepeat(int Index);
	TtkTokenKind __fastcall FuncRepeatcount(int Index);
	TtkTokenKind __fastcall FuncReportlevel(int Index);
	TtkTokenKind __fastcall FuncRightstr(int Index);
	TtkTokenKind __fastcall FuncRlenstr(int Index);
	TtkTokenKind __fastcall FuncRoot(int Index);
	TtkTokenKind __fastcall FuncRound(int Index);
	TtkTokenKind __fastcall FuncShowmessage(int Index);
	TtkTokenKind __fastcall FuncSkipemtpty(int Index);
	TtkTokenKind __fastcall FuncSortdown(int Index);
	TtkTokenKind __fastcall FuncSortkey(int Index);
	TtkTokenKind __fastcall FuncSortup(int Index);
	TtkTokenKind __fastcall FuncSql_add(int Index);
	TtkTokenKind __fastcall FuncSql_asfloat(int Index);
	TtkTokenKind __fastcall FuncSql_asstring(int Index);
	TtkTokenKind __fastcall FuncSql_create(int Index);
	TtkTokenKind __fastcall FuncSql_dump(int Index);
	TtkTokenKind __fastcall FuncSql_eof(int Index);
	TtkTokenKind __fastcall FuncSql_execute(int Index);
	TtkTokenKind __fastcall FuncSql_free(int Index);
	TtkTokenKind __fastcall FuncSql_mladd(int Index);
	TtkTokenKind __fastcall FuncSql_mlmultiadd(int Index);
	TtkTokenKind __fastcall FuncSql_next(int Index);
	TtkTokenKind __fastcall FuncSql_setvar(int Index);
	TtkTokenKind __fastcall FuncSqr(int Index);
	TtkTokenKind __fastcall FuncStripstr(int Index);
	TtkTokenKind __fastcall FuncStroptions(int Index);
	TtkTokenKind __fastcall FuncStrpos(int Index);
	TtkTokenKind __fastcall FuncSubtract(int Index);
	TtkTokenKind __fastcall FuncSum(int Index);
	TtkTokenKind __fastcall FuncSupplier(int Index);
	TtkTokenKind __fastcall FuncSuppliesofmembers(int Index);
	TtkTokenKind __fastcall FuncThen(int Index);
	TtkTokenKind __fastcall FuncTrunc(int Index);
	TtkTokenKind __fastcall FuncUpcasestr(int Index);
	TtkTokenKind __fastcall FuncUsedby(int Index);
	TtkTokenKind __fastcall FuncV_date(int Index);
	TtkTokenKind __fastcall FuncV_false(int Index);
	TtkTokenKind __fastcall FuncV_nonereal(int Index);
	TtkTokenKind __fastcall FuncV_par_language(int Index);
	TtkTokenKind __fastcall FuncV_par_language_count(int Index);
	TtkTokenKind __fastcall FuncV_par_language_fields(int Index);
	TtkTokenKind __fastcall FuncV_time(int Index);
	TtkTokenKind __fastcall FuncV_true(int Index);
	TtkTokenKind __fastcall FuncVariables(int Index);
	TtkTokenKind __fastcall FuncVaroptions(int Index);
	TtkTokenKind __fastcall FuncWhile(int Index);
	TtkTokenKind __fastcall FuncZerorlenstr(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent();
	void __fastcall CRProc();
	void __fastcall LFProc();
	void __fastcall SemiColonProc();
	void __fastcall SymbolProc();
	void __fastcall NumberProc();
	void __fastcall BraceOpenProc();
	void __fastcall IdentProc();
	void __fastcall VariableProc();
	void __fastcall NullProc();
	void __fastcall SpaceProc();
	void __fastcall StringProc();
	void __fastcall UnknownProc();
	void __fastcall BraceCommentProc();
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource();
	virtual bool __fastcall IsFilterStored();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynCPMSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	TtkTokenKind __fastcall GetTokenID();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	virtual int __fastcall GetTokenKind();
	virtual void __fastcall Next();
	virtual void * __fastcall GetRange();
	virtual void __fastcall ResetRange();
	virtual void __fastcall SetRange(void * Value);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SQLKeyAttri = {read=fSQLKeyAttri, write=fSQLKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpecialVarAttri = {read=fSpecialVarAttri, write=fSpecialVarAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SystemAttri = {read=fSystemAttri, write=fSystemAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VariableAttri = {read=fVariableAttri, write=fVariableAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynCPMSyn() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightercpm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERCPM)
using namespace Synhighlightercpm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightercpmHPP
