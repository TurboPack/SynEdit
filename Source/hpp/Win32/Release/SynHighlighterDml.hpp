// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterDml.pas' rev: 32.00 (Windows)

#ifndef SynhighlighterdmlHPP
#define SynhighlighterdmlHPP

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

namespace Synhighlighterdml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynDmlSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkBlock, tkComment, tkForm, tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkQualifier, tkSpace, tkSpecial, tkString, tkSymbol, tkUnknown, tkVariable };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsANil, rsAdd, rsFind, rsUnKnown };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynDmlSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	System::StaticArray<TIdentFuncTableFunc, 2439> fIdentFuncTable;
	TtkTokenKind FTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fFormAttri;
	Synedithighlighter::TSynHighlighterAttributes* fBlockAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fQualiAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fFunctionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVariableAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpecialAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAbs(int Index);
	TtkTokenKind __fastcall FuncAbsolute_position(int Index);
	TtkTokenKind __fastcall FuncAccount(int Index);
	TtkTokenKind __fastcall FuncAcos(int Index);
	TtkTokenKind __fastcall FuncActual_break(int Index);
	TtkTokenKind __fastcall FuncAdd(int Index);
	TtkTokenKind __fastcall FuncAdd_form(int Index);
	TtkTokenKind __fastcall FuncAlternate_form(int Index);
	TtkTokenKind __fastcall FuncAscii(int Index);
	TtkTokenKind __fastcall FuncAsin(int Index);
	TtkTokenKind __fastcall FuncAtan(int Index);
	TtkTokenKind __fastcall FuncAtan2(int Index);
	TtkTokenKind __fastcall FuncAttributes(int Index);
	TtkTokenKind __fastcall FuncBack(int Index);
	TtkTokenKind __fastcall FuncBase(int Index);
	TtkTokenKind __fastcall FuncBatch(int Index);
	TtkTokenKind __fastcall FuncBegin_block(int Index);
	TtkTokenKind __fastcall FuncBegin_case(int Index);
	TtkTokenKind __fastcall FuncBegin_disable_trigger(int Index);
	TtkTokenKind __fastcall FuncBegin_row(int Index);
	TtkTokenKind __fastcall FuncBegin_signal_to_status(int Index);
	TtkTokenKind __fastcall FuncBell(int Index);
	TtkTokenKind __fastcall FuncBinary_to_poly(int Index);
	TtkTokenKind __fastcall FuncBottom_line(int Index);
	TtkTokenKind __fastcall FuncBreak(int Index);
	TtkTokenKind __fastcall FuncBreak0(int Index);
	TtkTokenKind __fastcall FuncCall(int Index);
	TtkTokenKind __fastcall FuncCase(int Index);
	TtkTokenKind __fastcall FuncCeil(int Index);
	TtkTokenKind __fastcall FuncCheck(int Index);
	TtkTokenKind __fastcall FuncCheck_domain(int Index);
	TtkTokenKind __fastcall FuncChr(int Index);
	TtkTokenKind __fastcall FuncClear_buffer(int Index);
	TtkTokenKind __fastcall FuncCli(int Index);
	TtkTokenKind __fastcall FuncClose(int Index);
	TtkTokenKind __fastcall FuncClose_text(int Index);
	TtkTokenKind __fastcall FuncCol(int Index);
	TtkTokenKind __fastcall FuncColumn_heading_row(int Index);
	TtkTokenKind __fastcall FuncColumn_headings(int Index);
	TtkTokenKind __fastcall FuncColumn_spacing(int Index);
	TtkTokenKind __fastcall FuncCommit(int Index);
	TtkTokenKind __fastcall FuncCommit_rate(int Index);
	TtkTokenKind __fastcall FuncCompile(int Index);
	TtkTokenKind __fastcall FuncCompress(int Index);
	TtkTokenKind __fastcall FuncCompress_all(int Index);
	TtkTokenKind __fastcall FuncConfirm(int Index);
	TtkTokenKind __fastcall FuncConnect(int Index);
	TtkTokenKind __fastcall FuncContinue(int Index);
	TtkTokenKind __fastcall FuncCos(int Index);
	TtkTokenKind __fastcall FuncCosh(int Index);
	TtkTokenKind __fastcall FuncCross_reference(int Index);
	TtkTokenKind __fastcall FuncDate(int Index);
	TtkTokenKind __fastcall FuncDate_seconds(int Index);
	TtkTokenKind __fastcall FuncDay_of_week(int Index);
	TtkTokenKind __fastcall FuncDays(int Index);
	TtkTokenKind __fastcall FuncDcl(int Index);
	TtkTokenKind __fastcall FuncDefault_tag(int Index);
	TtkTokenKind __fastcall FuncDelete(int Index);
	TtkTokenKind __fastcall FuncDelete_form(int Index);
	TtkTokenKind __fastcall FuncDescription(int Index);
	TtkTokenKind __fastcall FuncDir(int Index);
	TtkTokenKind __fastcall FuncDisconnect(int Index);
	TtkTokenKind __fastcall FuncDisplay(int Index);
	TtkTokenKind __fastcall FuncDisplay_length(int Index);
	TtkTokenKind __fastcall FuncDocumentation(int Index);
	TtkTokenKind __fastcall FuncDomain(int Index);
	TtkTokenKind __fastcall FuncEdit(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncElse_if(int Index);
	TtkTokenKind __fastcall FuncEnd_block(int Index);
	TtkTokenKind __fastcall FuncEnd_case(int Index);
	TtkTokenKind __fastcall FuncEnd_disable_trigger(int Index);
	TtkTokenKind __fastcall FuncEnd_execute(int Index);
	TtkTokenKind __fastcall FuncEnd_form(int Index);
	TtkTokenKind __fastcall FuncEnd_if(int Index);
	TtkTokenKind __fastcall FuncEnd_row(int Index);
	TtkTokenKind __fastcall FuncEnd_signal_to_status(int Index);
	TtkTokenKind __fastcall FuncEnd_while(int Index);
	TtkTokenKind __fastcall FuncErase(int Index);
	TtkTokenKind __fastcall FuncError(int Index);
	TtkTokenKind __fastcall FuncExecute(int Index);
	TtkTokenKind __fastcall FuncExit(int Index);
	TtkTokenKind __fastcall FuncExit_forward(int Index);
	TtkTokenKind __fastcall FuncExpand(int Index);
	TtkTokenKind __fastcall FuncExternal(int Index);
	TtkTokenKind __fastcall FuncFacility(int Index);
	TtkTokenKind __fastcall FuncFailure(int Index);
	TtkTokenKind __fastcall FuncFetch(int Index);
	TtkTokenKind __fastcall FuncFiles(int Index);
	TtkTokenKind __fastcall FuncFind(int Index);
	TtkTokenKind __fastcall FuncFind_form(int Index);
	TtkTokenKind __fastcall FuncFinish(int Index);
	TtkTokenKind __fastcall FuncFirst(int Index);
	TtkTokenKind __fastcall FuncFloor(int Index);
	TtkTokenKind __fastcall FuncFooting(int Index);
	TtkTokenKind __fastcall FuncFooting_form(int Index);
	TtkTokenKind __fastcall FuncForm(int Index);
	TtkTokenKind __fastcall FuncGenerate(int Index);
	TtkTokenKind __fastcall FuncGoto(int Index);
	TtkTokenKind __fastcall FuncGrouped_by(int Index);
	TtkTokenKind __fastcall FuncHeading(int Index);
	TtkTokenKind __fastcall FuncHeading_form(int Index);
	TtkTokenKind __fastcall FuncHeight(int Index);
	TtkTokenKind __fastcall FuncIdentifier(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncIn(int Index);
	TtkTokenKind __fastcall FuncInput_block(int Index);
	TtkTokenKind __fastcall FuncInput_mask(int Index);
	TtkTokenKind __fastcall FuncInput_row_height(int Index);
	TtkTokenKind __fastcall FuncInt(int Index);
	TtkTokenKind __fastcall FuncInvoke(int Index);
	TtkTokenKind __fastcall FuncItem(int Index);
	TtkTokenKind __fastcall FuncItem_block(int Index);
	TtkTokenKind __fastcall FuncItem_if(int Index);
	TtkTokenKind __fastcall FuncJoined_to(int Index);
	TtkTokenKind __fastcall FuncLeft(int Index);
	TtkTokenKind __fastcall FuncLen(int Index);
	TtkTokenKind __fastcall FuncLfooting(int Index);
	TtkTokenKind __fastcall FuncLheading(int Index);
	TtkTokenKind __fastcall FuncLine(int Index);
	TtkTokenKind __fastcall FuncLines_after(int Index);
	TtkTokenKind __fastcall FuncLines_before(int Index);
	TtkTokenKind __fastcall FuncList(int Index);
	TtkTokenKind __fastcall FuncLoad(int Index);
	TtkTokenKind __fastcall FuncLock(int Index);
	TtkTokenKind __fastcall FuncLog(int Index);
	TtkTokenKind __fastcall FuncLog10(int Index);
	TtkTokenKind __fastcall FuncLov(int Index);
	TtkTokenKind __fastcall FuncLov_auto_select(int Index);
	TtkTokenKind __fastcall FuncLov_col(int Index);
	TtkTokenKind __fastcall FuncLov_data(int Index);
	TtkTokenKind __fastcall FuncLov_first(int Index);
	TtkTokenKind __fastcall FuncLov_height(int Index);
	TtkTokenKind __fastcall FuncLov_noheading(int Index);
	TtkTokenKind __fastcall FuncLov_nosearch(int Index);
	TtkTokenKind __fastcall FuncLov_reduced_to(int Index);
	TtkTokenKind __fastcall FuncLov_row(int Index);
	TtkTokenKind __fastcall FuncLov_secondary(int Index);
	TtkTokenKind __fastcall FuncLov_selection(int Index);
	TtkTokenKind __fastcall FuncLov_sorted_by(int Index);
	TtkTokenKind __fastcall FuncLov_width(int Index);
	TtkTokenKind __fastcall FuncLov_with(int Index);
	TtkTokenKind __fastcall FuncLowercase(int Index);
	TtkTokenKind __fastcall FuncLtrim(int Index);
	TtkTokenKind __fastcall FuncMail(int Index);
	TtkTokenKind __fastcall FuncMenu(int Index);
	TtkTokenKind __fastcall FuncMenu_block(int Index);
	TtkTokenKind __fastcall FuncMenu_form(int Index);
	TtkTokenKind __fastcall FuncMessage(int Index);
	TtkTokenKind __fastcall FuncMid(int Index);
	TtkTokenKind __fastcall FuncMod(int Index);
	TtkTokenKind __fastcall FuncModify_form(int Index);
	TtkTokenKind __fastcall FuncNew(int Index);
	TtkTokenKind __fastcall FuncNo_domain(int Index);
	TtkTokenKind __fastcall FuncNobell(int Index);
	TtkTokenKind __fastcall FuncNoclear_buffer(int Index);
	TtkTokenKind __fastcall FuncNodeadlock_exit(int Index);
	TtkTokenKind __fastcall FuncNoerase(int Index);
	TtkTokenKind __fastcall FuncNoerror(int Index);
	TtkTokenKind __fastcall FuncNoexit_forward(int Index);
	TtkTokenKind __fastcall FuncNoheading(int Index);
	TtkTokenKind __fastcall FuncNolov_data(int Index);
	TtkTokenKind __fastcall FuncNorepeat(int Index);
	TtkTokenKind __fastcall FuncNostatus(int Index);
	TtkTokenKind __fastcall FuncNototals(int Index);
	TtkTokenKind __fastcall FuncNounderlines(int Index);
	TtkTokenKind __fastcall FuncNowait(int Index);
	TtkTokenKind __fastcall FuncOpen(int Index);
	TtkTokenKind __fastcall FuncOpen_text(int Index);
	TtkTokenKind __fastcall FuncOpt(int Index);
	TtkTokenKind __fastcall FuncOptions(int Index);
	TtkTokenKind __fastcall FuncOutput(int Index);
	TtkTokenKind __fastcall FuncOutput_block(int Index);
	TtkTokenKind __fastcall FuncOutput_mask(int Index);
	TtkTokenKind __fastcall FuncPause(int Index);
	TtkTokenKind __fastcall FuncPause_block(int Index);
	TtkTokenKind __fastcall FuncPerform(int Index);
	TtkTokenKind __fastcall FuncPoly_to_binary(int Index);
	TtkTokenKind __fastcall FuncPos(int Index);
	TtkTokenKind __fastcall FuncPrint(int Index);
	TtkTokenKind __fastcall FuncProcedure_form(int Index);
	TtkTokenKind __fastcall FuncPrompt(int Index);
	TtkTokenKind __fastcall FuncProtect(int Index);
	TtkTokenKind __fastcall FuncQuery(int Index);
	TtkTokenKind __fastcall FuncQuery_form(int Index);
	TtkTokenKind __fastcall FuncRandom(int Index);
	TtkTokenKind __fastcall FuncRead_line(int Index);
	TtkTokenKind __fastcall FuncRead_only(int Index);
	TtkTokenKind __fastcall FuncReceive(int Index);
	TtkTokenKind __fastcall FuncReceive_arguments(int Index);
	TtkTokenKind __fastcall FuncReceive_data(int Index);
	TtkTokenKind __fastcall FuncReceive_table(int Index);
	TtkTokenKind __fastcall FuncReduced_to(int Index);
	TtkTokenKind __fastcall FuncRelease(int Index);
	TtkTokenKind __fastcall FuncRemain(int Index);
	TtkTokenKind __fastcall FuncRepeat(int Index);
	TtkTokenKind __fastcall FuncReport(int Index);
	TtkTokenKind __fastcall FuncReport_form(int Index);
	TtkTokenKind __fastcall FuncReposition(int Index);
	TtkTokenKind __fastcall FuncRewind_text(int Index);
	TtkTokenKind __fastcall FuncRfooting(int Index);
	TtkTokenKind __fastcall FuncRheading(int Index);
	TtkTokenKind __fastcall FuncRight(int Index);
	TtkTokenKind __fastcall FuncRollback(int Index);
	TtkTokenKind __fastcall FuncRound(int Index);
	TtkTokenKind __fastcall FuncRow(int Index);
	TtkTokenKind __fastcall FuncRow_height(int Index);
	TtkTokenKind __fastcall FuncSearch(int Index);
	TtkTokenKind __fastcall FuncSecondary(int Index);
	TtkTokenKind __fastcall FuncSeconds(int Index);
	TtkTokenKind __fastcall FuncSelection(int Index);
	TtkTokenKind __fastcall FuncSend(int Index);
	TtkTokenKind __fastcall FuncSend_data(int Index);
	TtkTokenKind __fastcall FuncSend_message(int Index);
	TtkTokenKind __fastcall FuncSend_table(int Index);
	TtkTokenKind __fastcall FuncSequence(int Index);
	TtkTokenKind __fastcall FuncSeverity(int Index);
	TtkTokenKind __fastcall FuncSin(int Index);
	TtkTokenKind __fastcall FuncSinh(int Index);
	TtkTokenKind __fastcall FuncSorted_by(int Index);
	TtkTokenKind __fastcall FuncSource(int Index);
	TtkTokenKind __fastcall FuncSource_if(int Index);
	TtkTokenKind __fastcall FuncSqrt(int Index);
	TtkTokenKind __fastcall FuncStart_stream(int Index);
	TtkTokenKind __fastcall FuncStart_transaction(int Index);
	TtkTokenKind __fastcall FuncStatistic(int Index);
	TtkTokenKind __fastcall FuncStatus(int Index);
	TtkTokenKind __fastcall FuncStream_name(int Index);
	TtkTokenKind __fastcall FuncString(int Index);
	TtkTokenKind __fastcall FuncSuccess(int Index);
	TtkTokenKind __fastcall FuncSwitch(int Index);
	TtkTokenKind __fastcall FuncSwitch_base(int Index);
	TtkTokenKind __fastcall FuncSystem(int Index);
	TtkTokenKind __fastcall FuncTable(int Index);
	TtkTokenKind __fastcall FuncTable_form(int Index);
	TtkTokenKind __fastcall FuncTable_search(int Index);
	TtkTokenKind __fastcall FuncTag(int Index);
	TtkTokenKind __fastcall FuncTag_length(int Index);
	TtkTokenKind __fastcall FuncTan(int Index);
	TtkTokenKind __fastcall FuncTanh(int Index);
	TtkTokenKind __fastcall FuncTarget(int Index);
	TtkTokenKind __fastcall FuncText(int Index);
	TtkTokenKind __fastcall FuncText_only(int Index);
	TtkTokenKind __fastcall FuncTitle(int Index);
	TtkTokenKind __fastcall FuncTo(int Index);
	TtkTokenKind __fastcall FuncTop_line(int Index);
	TtkTokenKind __fastcall FuncTotal(int Index);
	TtkTokenKind __fastcall FuncTransfer(int Index);
	TtkTokenKind __fastcall FuncTrigger(int Index);
	TtkTokenKind __fastcall FuncTrim(int Index);
	TtkTokenKind __fastcall FuncTsuppress(int Index);
	TtkTokenKind __fastcall FuncUnload(int Index);
	TtkTokenKind __fastcall FuncUppercase(int Index);
	TtkTokenKind __fastcall FuncUse_if(int Index);
	TtkTokenKind __fastcall FuncUser_key(int Index);
	TtkTokenKind __fastcall FuncUsing(int Index);
	TtkTokenKind __fastcall FuncUtilities(int Index);
	TtkTokenKind __fastcall FuncWait(int Index);
	TtkTokenKind __fastcall FuncWhile(int Index);
	TtkTokenKind __fastcall FuncWidth(int Index);
	TtkTokenKind __fastcall FuncWith(int Index);
	TtkTokenKind __fastcall FuncWrite(int Index);
	TtkTokenKind __fastcall FuncWrite_line(int Index);
	TtkTokenKind __fastcall FuncYesno_block(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall SymbolProc(void);
	void __fastcall AddressOpProc(void);
	void __fastcall AsciiCharProc(void);
	void __fastcall CRProc(void);
	void __fastcall GreaterProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall LowerProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall PointProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall RemProc(void);
	bool __fastcall IsQuali(void);
	bool __fastcall IsSpecial(void);
	
protected:
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynDmlSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* BlockAttri = {read=fBlockAttri, write=fBlockAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* FormAttri = {read=fFormAttri, write=fFormAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* FunctionAttri = {read=fFunctionAttri, write=fFunctionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* QualiAttri = {read=fQualiAttri, write=fQualiAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpecialAttri = {read=fSpecialAttri, write=fSpecialAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VariableAttri = {read=fVariableAttri, write=fVariableAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynDmlSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterdml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERDML)
using namespace Synhighlighterdml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterdmlHPP
