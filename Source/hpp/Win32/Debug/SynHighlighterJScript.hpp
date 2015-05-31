// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterJScript.pas' rev: 29.00 (Windows)

#ifndef SynhighlighterjscriptHPP
#define SynhighlighterjscriptHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Win.Registry.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterjscript
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynJScriptSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkNonReservedKey, tkEvent };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsANSI };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynJScriptSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind FTokenID;
	System::StaticArray<TIdentFuncTableFunc, 5153> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNonReservedKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fEventAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAbs(int Index);
	TtkTokenKind __fastcall FuncAbstract(int Index);
	TtkTokenKind __fastcall FuncAcos(int Index);
	TtkTokenKind __fastcall FuncAction(int Index);
	TtkTokenKind __fastcall FuncAlert(int Index);
	TtkTokenKind __fastcall FuncAlign(int Index);
	TtkTokenKind __fastcall FuncAlinkcolor(int Index);
	TtkTokenKind __fastcall FuncAll(int Index);
	TtkTokenKind __fastcall FuncAnchor(int Index);
	TtkTokenKind __fastcall FuncAnchors(int Index);
	TtkTokenKind __fastcall FuncAppcodename(int Index);
	TtkTokenKind __fastcall FuncApplet(int Index);
	TtkTokenKind __fastcall FuncApplets(int Index);
	TtkTokenKind __fastcall FuncAppname(int Index);
	TtkTokenKind __fastcall FuncAppversion(int Index);
	TtkTokenKind __fastcall FuncArea(int Index);
	TtkTokenKind __fastcall FuncArguments(int Index);
	TtkTokenKind __fastcall FuncArray(int Index);
	TtkTokenKind __fastcall FuncAsin(int Index);
	TtkTokenKind __fastcall FuncAtan(int Index);
	TtkTokenKind __fastcall FuncAtan2(int Index);
	TtkTokenKind __fastcall FuncBack(int Index);
	TtkTokenKind __fastcall FuncBackground(int Index);
	TtkTokenKind __fastcall FuncBgcolor(int Index);
	TtkTokenKind __fastcall FuncBig(int Index);
	TtkTokenKind __fastcall FuncBlink(int Index);
	TtkTokenKind __fastcall FuncBlur(int Index);
	TtkTokenKind __fastcall FuncBody(int Index);
	TtkTokenKind __fastcall FuncBold(int Index);
	TtkTokenKind __fastcall FuncBoolean(int Index);
	TtkTokenKind __fastcall FuncBoolean2(int Index);
	TtkTokenKind __fastcall FuncBorder(int Index);
	TtkTokenKind __fastcall FuncBottom(int Index);
	TtkTokenKind __fastcall FuncBreak(int Index);
	TtkTokenKind __fastcall FuncButton(int Index);
	TtkTokenKind __fastcall FuncByte(int Index);
	TtkTokenKind __fastcall FuncCall(int Index);
	TtkTokenKind __fastcall FuncCallee(int Index);
	TtkTokenKind __fastcall FuncCaller(int Index);
	TtkTokenKind __fastcall FuncCaptureevents(int Index);
	TtkTokenKind __fastcall FuncCase(int Index);
	TtkTokenKind __fastcall FuncCatch(int Index);
	TtkTokenKind __fastcall FuncCeil(int Index);
	TtkTokenKind __fastcall FuncChar(int Index);
	TtkTokenKind __fastcall FuncCharat(int Index);
	TtkTokenKind __fastcall FuncCharcodeat(int Index);
	TtkTokenKind __fastcall FuncCheckbox(int Index);
	TtkTokenKind __fastcall FuncChecked(int Index);
	TtkTokenKind __fastcall FuncClass(int Index);
	TtkTokenKind __fastcall FuncClear(int Index);
	TtkTokenKind __fastcall FuncClearinterval(int Index);
	TtkTokenKind __fastcall FuncCleartimeout(int Index);
	TtkTokenKind __fastcall FuncClick(int Index);
	TtkTokenKind __fastcall FuncClose(int Index);
	TtkTokenKind __fastcall FuncClosed(int Index);
	TtkTokenKind __fastcall FuncColor(int Index);
	TtkTokenKind __fastcall FuncComplete(int Index);
	TtkTokenKind __fastcall FuncConcat(int Index);
	TtkTokenKind __fastcall FuncConfirm(int Index);
	TtkTokenKind __fastcall FuncConst(int Index);
	TtkTokenKind __fastcall FuncConstructor(int Index);
	TtkTokenKind __fastcall FuncContinue(int Index);
	TtkTokenKind __fastcall FuncCookie(int Index);
	TtkTokenKind __fastcall FuncCos(int Index);
	TtkTokenKind __fastcall FuncCurrent(int Index);
	TtkTokenKind __fastcall FuncDate(int Index);
	TtkTokenKind __fastcall FuncDebugger(int Index);
	TtkTokenKind __fastcall FuncDefault(int Index);
	TtkTokenKind __fastcall FuncDefaultchecked(int Index);
	TtkTokenKind __fastcall FuncDefaultselected(int Index);
	TtkTokenKind __fastcall FuncDefaultstatus(int Index);
	TtkTokenKind __fastcall FuncDefaultvalue(int Index);
	TtkTokenKind __fastcall FuncDelete(int Index);
	TtkTokenKind __fastcall FuncDescription(int Index);
	TtkTokenKind __fastcall FuncDisplay(int Index);
	TtkTokenKind __fastcall FuncDo(int Index);
	TtkTokenKind __fastcall FuncDocument(int Index);
	TtkTokenKind __fastcall FuncDomain(int Index);
	TtkTokenKind __fastcall FuncDouble(int Index);
	TtkTokenKind __fastcall FuncE(int Index);
	TtkTokenKind __fastcall FuncElements(int Index);
	TtkTokenKind __fastcall FuncElse(int Index);
	TtkTokenKind __fastcall FuncEmbed(int Index);
	TtkTokenKind __fastcall FuncEmbeds(int Index);
	TtkTokenKind __fastcall FuncEnabledplugin(int Index);
	TtkTokenKind __fastcall FuncEncoding(int Index);
	TtkTokenKind __fastcall FuncEnum(int Index);
	TtkTokenKind __fastcall FuncEscape(int Index);
	TtkTokenKind __fastcall FuncEval(int Index);
	TtkTokenKind __fastcall FuncEvent(int Index);
	TtkTokenKind __fastcall FuncExp(int Index);
	TtkTokenKind __fastcall FuncExport(int Index);
	TtkTokenKind __fastcall FuncExtends(int Index);
	TtkTokenKind __fastcall FuncFalse(int Index);
	TtkTokenKind __fastcall FuncFgcolor(int Index);
	TtkTokenKind __fastcall FuncFilename(int Index);
	TtkTokenKind __fastcall FuncFileupload(int Index);
	TtkTokenKind __fastcall FuncFinal(int Index);
	TtkTokenKind __fastcall FuncFinally(int Index);
	TtkTokenKind __fastcall FuncFind(int Index);
	TtkTokenKind __fastcall FuncFixed(int Index);
	TtkTokenKind __fastcall FuncFloat(int Index);
	TtkTokenKind __fastcall FuncFloat2(int Index);
	TtkTokenKind __fastcall FuncFloor(int Index);
	TtkTokenKind __fastcall FuncFocus(int Index);
	TtkTokenKind __fastcall FuncFontcolor(int Index);
	TtkTokenKind __fastcall FuncFontsize(int Index);
	TtkTokenKind __fastcall FuncFor(int Index);
	TtkTokenKind __fastcall FuncForm(int Index);
	TtkTokenKind __fastcall FuncForms(int Index);
	TtkTokenKind __fastcall FuncForward(int Index);
	TtkTokenKind __fastcall FuncFrame(int Index);
	TtkTokenKind __fastcall FuncFrames(int Index);
	TtkTokenKind __fastcall FuncFromcharcode(int Index);
	TtkTokenKind __fastcall FuncFunction(int Index);
	TtkTokenKind __fastcall FuncFunction2(int Index);
	TtkTokenKind __fastcall FuncGetdate(int Index);
	TtkTokenKind __fastcall FuncGetday(int Index);
	TtkTokenKind __fastcall FuncGetelementbyid(int Index);
	TtkTokenKind __fastcall FuncGetfullyear(int Index);
	TtkTokenKind __fastcall FuncGethours(int Index);
	TtkTokenKind __fastcall FuncGetmilliseconds(int Index);
	TtkTokenKind __fastcall FuncGetminutes(int Index);
	TtkTokenKind __fastcall FuncGetmonth(int Index);
	TtkTokenKind __fastcall FuncGetseconds(int Index);
	TtkTokenKind __fastcall FuncGettime(int Index);
	TtkTokenKind __fastcall FuncGettimezoneoffset(int Index);
	TtkTokenKind __fastcall FuncGetutcdate(int Index);
	TtkTokenKind __fastcall FuncGetutcday(int Index);
	TtkTokenKind __fastcall FuncGetutcfullyear(int Index);
	TtkTokenKind __fastcall FuncGetutchours(int Index);
	TtkTokenKind __fastcall FuncGetutcmilliseconds(int Index);
	TtkTokenKind __fastcall FuncGetutcminutes(int Index);
	TtkTokenKind __fastcall FuncGetutcmonth(int Index);
	TtkTokenKind __fastcall FuncGetutcseconds(int Index);
	TtkTokenKind __fastcall FuncGetyear(int Index);
	TtkTokenKind __fastcall FuncGlobal(int Index);
	TtkTokenKind __fastcall FuncGo(int Index);
	TtkTokenKind __fastcall FuncGoto(int Index);
	TtkTokenKind __fastcall FuncHandleevent(int Index);
	TtkTokenKind __fastcall FuncHash(int Index);
	TtkTokenKind __fastcall FuncHeight(int Index);
	TtkTokenKind __fastcall FuncHidden(int Index);
	TtkTokenKind __fastcall FuncHistory(int Index);
	TtkTokenKind __fastcall FuncHome(int Index);
	TtkTokenKind __fastcall FuncHost(int Index);
	TtkTokenKind __fastcall FuncHostname(int Index);
	TtkTokenKind __fastcall FuncHref(int Index);
	TtkTokenKind __fastcall FuncHspace(int Index);
	TtkTokenKind __fastcall FuncIf(int Index);
	TtkTokenKind __fastcall FuncImage(int Index);
	TtkTokenKind __fastcall FuncImages(int Index);
	TtkTokenKind __fastcall FuncImplements(int Index);
	TtkTokenKind __fastcall FuncImport(int Index);
	TtkTokenKind __fastcall FuncIn(int Index);
	TtkTokenKind __fastcall FuncIndex(int Index);
	TtkTokenKind __fastcall FuncIndexof(int Index);
	TtkTokenKind __fastcall FuncInfinity(int Index);
	TtkTokenKind __fastcall FuncInnerheight(int Index);
	TtkTokenKind __fastcall FuncInnerwidth(int Index);
	TtkTokenKind __fastcall FuncInput(int Index);
	TtkTokenKind __fastcall FuncInstanceof(int Index);
	TtkTokenKind __fastcall FuncInt(int Index);
	TtkTokenKind __fastcall FuncInterface(int Index);
	TtkTokenKind __fastcall FuncIsfinite(int Index);
	TtkTokenKind __fastcall FuncIsnan(int Index);
	TtkTokenKind __fastcall FuncItalics(int Index);
	TtkTokenKind __fastcall FuncJava(int Index);
	TtkTokenKind __fastcall FuncJavaenabled(int Index);
	TtkTokenKind __fastcall FuncJoin(int Index);
	TtkTokenKind __fastcall FuncLastindexof(int Index);
	TtkTokenKind __fastcall FuncLastmodified(int Index);
	TtkTokenKind __fastcall FuncLayer(int Index);
	TtkTokenKind __fastcall FuncLayers(int Index);
	TtkTokenKind __fastcall FuncLeft(int Index);
	TtkTokenKind __fastcall FuncLength(int Index);
	TtkTokenKind __fastcall FuncLink(int Index);
	TtkTokenKind __fastcall FuncLinkcolor(int Index);
	TtkTokenKind __fastcall FuncLinks(int Index);
	TtkTokenKind __fastcall FuncLn10(int Index);
	TtkTokenKind __fastcall FuncLn2(int Index);
	TtkTokenKind __fastcall FuncLocation(int Index);
	TtkTokenKind __fastcall FuncLocationbar(int Index);
	TtkTokenKind __fastcall FuncLog(int Index);
	TtkTokenKind __fastcall FuncLog10e(int Index);
	TtkTokenKind __fastcall FuncLog2e(int Index);
	TtkTokenKind __fastcall FuncLogon(int Index);
	TtkTokenKind __fastcall FuncLong(int Index);
	TtkTokenKind __fastcall FuncLowsrc(int Index);
	TtkTokenKind __fastcall FuncMatch(int Index);
	TtkTokenKind __fastcall FuncMath(int Index);
	TtkTokenKind __fastcall FuncMax(int Index);
	TtkTokenKind __fastcall FuncMax_value(int Index);
	TtkTokenKind __fastcall FuncMenubar(int Index);
	TtkTokenKind __fastcall FuncMethod(int Index);
	TtkTokenKind __fastcall FuncMimetype(int Index);
	TtkTokenKind __fastcall FuncMimetypes(int Index);
	TtkTokenKind __fastcall FuncMin(int Index);
	TtkTokenKind __fastcall FuncMin_value(int Index);
	TtkTokenKind __fastcall FuncMoveby(int Index);
	TtkTokenKind __fastcall FuncMoveto(int Index);
	TtkTokenKind __fastcall FuncName(int Index);
	TtkTokenKind __fastcall FuncNan(int Index);
	TtkTokenKind __fastcall FuncNative(int Index);
	TtkTokenKind __fastcall FuncNavigator(int Index);
	TtkTokenKind __fastcall FuncNegative_infinity(int Index);
	TtkTokenKind __fastcall FuncNetscape(int Index);
	TtkTokenKind __fastcall FuncNew(int Index);
	TtkTokenKind __fastcall FuncNext(int Index);
	TtkTokenKind __fastcall FuncNull(int Index);
	TtkTokenKind __fastcall FuncNull2(int Index);
	TtkTokenKind __fastcall FuncNumber(int Index);
	TtkTokenKind __fastcall FuncObject(int Index);
	TtkTokenKind __fastcall FuncOnabort(int Index);
	TtkTokenKind __fastcall FuncOnblur(int Index);
	TtkTokenKind __fastcall FuncOnchange(int Index);
	TtkTokenKind __fastcall FuncOnclick(int Index);
	TtkTokenKind __fastcall FuncOndblclick(int Index);
	TtkTokenKind __fastcall FuncOnerror(int Index);
	TtkTokenKind __fastcall FuncOnfocus(int Index);
	TtkTokenKind __fastcall FuncOnkeydown(int Index);
	TtkTokenKind __fastcall FuncOnkeypress(int Index);
	TtkTokenKind __fastcall FuncOnkeyup(int Index);
	TtkTokenKind __fastcall FuncOnload(int Index);
	TtkTokenKind __fastcall FuncOnmousedown(int Index);
	TtkTokenKind __fastcall FuncOnmousemove(int Index);
	TtkTokenKind __fastcall FuncOnmouseout(int Index);
	TtkTokenKind __fastcall FuncOnmouseover(int Index);
	TtkTokenKind __fastcall FuncOnmouseup(int Index);
	TtkTokenKind __fastcall FuncOnreset(int Index);
	TtkTokenKind __fastcall FuncOnselect(int Index);
	TtkTokenKind __fastcall FuncOnsubmit(int Index);
	TtkTokenKind __fastcall FuncOnunload(int Index);
	TtkTokenKind __fastcall FuncOpen(int Index);
	TtkTokenKind __fastcall FuncOpener(int Index);
	TtkTokenKind __fastcall FuncOption(int Index);
	TtkTokenKind __fastcall FuncOptions(int Index);
	TtkTokenKind __fastcall FuncOuterheight(int Index);
	TtkTokenKind __fastcall FuncOuterwidth(int Index);
	TtkTokenKind __fastcall FuncPackage(int Index);
	TtkTokenKind __fastcall FuncPackages(int Index);
	TtkTokenKind __fastcall FuncPagex(int Index);
	TtkTokenKind __fastcall FuncPagexoffset(int Index);
	TtkTokenKind __fastcall FuncPagey(int Index);
	TtkTokenKind __fastcall FuncPageyoffset(int Index);
	TtkTokenKind __fastcall FuncParent(int Index);
	TtkTokenKind __fastcall FuncParse(int Index);
	TtkTokenKind __fastcall FuncParsefloat(int Index);
	TtkTokenKind __fastcall FuncParseint(int Index);
	TtkTokenKind __fastcall FuncPassword(int Index);
	TtkTokenKind __fastcall FuncPathname(int Index);
	TtkTokenKind __fastcall FuncPersonalbar(int Index);
	TtkTokenKind __fastcall FuncPi(int Index);
	TtkTokenKind __fastcall FuncPlatform(int Index);
	TtkTokenKind __fastcall FuncPlugin(int Index);
	TtkTokenKind __fastcall FuncPlugins(int Index);
	TtkTokenKind __fastcall FuncPort(int Index);
	TtkTokenKind __fastcall FuncPositive_infinity(int Index);
	TtkTokenKind __fastcall FuncPow(int Index);
	TtkTokenKind __fastcall FuncPrevious(int Index);
	TtkTokenKind __fastcall FuncPrint(int Index);
	TtkTokenKind __fastcall FuncPrivate(int Index);
	TtkTokenKind __fastcall FuncPrompt(int Index);
	TtkTokenKind __fastcall FuncProtected(int Index);
	TtkTokenKind __fastcall FuncProtocol(int Index);
	TtkTokenKind __fastcall FuncPrototype(int Index);
	TtkTokenKind __fastcall FuncPublic(int Index);
	TtkTokenKind __fastcall FuncRadio(int Index);
	TtkTokenKind __fastcall FuncRandom(int Index);
	TtkTokenKind __fastcall FuncReferrer(int Index);
	TtkTokenKind __fastcall FuncRefresh(int Index);
	TtkTokenKind __fastcall FuncRegexp(int Index);
	TtkTokenKind __fastcall FuncReleaseevents(int Index);
	TtkTokenKind __fastcall FuncReload(int Index);
	TtkTokenKind __fastcall FuncReplace(int Index);
	TtkTokenKind __fastcall FuncReset(int Index);
	TtkTokenKind __fastcall FuncResizeby(int Index);
	TtkTokenKind __fastcall FuncResizeto(int Index);
	TtkTokenKind __fastcall FuncReturn(int Index);
	TtkTokenKind __fastcall FuncReverse(int Index);
	TtkTokenKind __fastcall FuncRight(int Index);
	TtkTokenKind __fastcall FuncRound(int Index);
	TtkTokenKind __fastcall FuncRouteevent(int Index);
	TtkTokenKind __fastcall FuncScreen(int Index);
	TtkTokenKind __fastcall FuncScroll(int Index);
	TtkTokenKind __fastcall FuncScrollbars(int Index);
	TtkTokenKind __fastcall FuncScrollby(int Index);
	TtkTokenKind __fastcall FuncScrollto(int Index);
	TtkTokenKind __fastcall FuncSearch(int Index);
	TtkTokenKind __fastcall FuncSelect(int Index);
	TtkTokenKind __fastcall FuncSelected(int Index);
	TtkTokenKind __fastcall FuncSelectedindex(int Index);
	TtkTokenKind __fastcall FuncSelf(int Index);
	TtkTokenKind __fastcall FuncSetdate(int Index);
	TtkTokenKind __fastcall FuncSetfullyear(int Index);
	TtkTokenKind __fastcall FuncSethours(int Index);
	TtkTokenKind __fastcall FuncSetinterval(int Index);
	TtkTokenKind __fastcall FuncSetmilliseconds(int Index);
	TtkTokenKind __fastcall FuncSetminutes(int Index);
	TtkTokenKind __fastcall FuncSetmonth(int Index);
	TtkTokenKind __fastcall FuncSetseconds(int Index);
	TtkTokenKind __fastcall FuncSettime(int Index);
	TtkTokenKind __fastcall FuncSettimeout(int Index);
	TtkTokenKind __fastcall FuncSetutcdate(int Index);
	TtkTokenKind __fastcall FuncSetutcfullyear(int Index);
	TtkTokenKind __fastcall FuncSetutchours(int Index);
	TtkTokenKind __fastcall FuncSetutcmilliseconds(int Index);
	TtkTokenKind __fastcall FuncSetutcminutes(int Index);
	TtkTokenKind __fastcall FuncSetutcmonth(int Index);
	TtkTokenKind __fastcall FuncSetutcseconds(int Index);
	TtkTokenKind __fastcall FuncSetyear(int Index);
	TtkTokenKind __fastcall FuncShort(int Index);
	TtkTokenKind __fastcall FuncSin(int Index);
	TtkTokenKind __fastcall FuncSlice(int Index);
	TtkTokenKind __fastcall FuncSmall(int Index);
	TtkTokenKind __fastcall FuncSort(int Index);
	TtkTokenKind __fastcall FuncSplit(int Index);
	TtkTokenKind __fastcall FuncSqrt(int Index);
	TtkTokenKind __fastcall FuncSqrt1_2(int Index);
	TtkTokenKind __fastcall FuncSqrt2(int Index);
	TtkTokenKind __fastcall FuncSrc(int Index);
	TtkTokenKind __fastcall FuncStart(int Index);
	TtkTokenKind __fastcall FuncStatic(int Index);
	TtkTokenKind __fastcall FuncStatus(int Index);
	TtkTokenKind __fastcall FuncStatusbar(int Index);
	TtkTokenKind __fastcall FuncStop(int Index);
	TtkTokenKind __fastcall FuncStrike(int Index);
	TtkTokenKind __fastcall FuncString(int Index);
	TtkTokenKind __fastcall FuncStyle(int Index);
	TtkTokenKind __fastcall FuncSub(int Index);
	TtkTokenKind __fastcall FuncSubmit(int Index);
	TtkTokenKind __fastcall FuncSubstr(int Index);
	TtkTokenKind __fastcall FuncSubstring(int Index);
	TtkTokenKind __fastcall FuncSuffixes(int Index);
	TtkTokenKind __fastcall FuncSup(int Index);
	TtkTokenKind __fastcall FuncSuper(int Index);
	TtkTokenKind __fastcall FuncSwitch(int Index);
	TtkTokenKind __fastcall FuncSynchronized(int Index);
	TtkTokenKind __fastcall FuncTags(int Index);
	TtkTokenKind __fastcall FuncTaint(int Index);
	TtkTokenKind __fastcall FuncTaintenabled(int Index);
	TtkTokenKind __fastcall FuncTan(int Index);
	TtkTokenKind __fastcall FuncTarget(int Index);
	TtkTokenKind __fastcall FuncText(int Index);
	TtkTokenKind __fastcall FuncTextarea(int Index);
	TtkTokenKind __fastcall FuncThis(int Index);
	TtkTokenKind __fastcall FuncThrow(int Index);
	TtkTokenKind __fastcall FuncThrows(int Index);
	TtkTokenKind __fastcall FuncTitle(int Index);
	TtkTokenKind __fastcall FuncTogmtstring(int Index);
	TtkTokenKind __fastcall FuncTolocalestring(int Index);
	TtkTokenKind __fastcall FuncTolowercase(int Index);
	TtkTokenKind __fastcall FuncToolbar(int Index);
	TtkTokenKind __fastcall FuncTop(int Index);
	TtkTokenKind __fastcall FuncTosource(int Index);
	TtkTokenKind __fastcall FuncTostring(int Index);
	TtkTokenKind __fastcall FuncTouppercase(int Index);
	TtkTokenKind __fastcall FuncToutcstring(int Index);
	TtkTokenKind __fastcall FuncTransient(int Index);
	TtkTokenKind __fastcall FuncTrue(int Index);
	TtkTokenKind __fastcall FuncTry(int Index);
	TtkTokenKind __fastcall FuncType(int Index);
	TtkTokenKind __fastcall FuncTypeof(int Index);
	TtkTokenKind __fastcall FuncUndefined(int Index);
	TtkTokenKind __fastcall FuncUnescape(int Index);
	TtkTokenKind __fastcall FuncUntaint(int Index);
	TtkTokenKind __fastcall FuncUnwatch(int Index);
	TtkTokenKind __fastcall FuncUrl(int Index);
	TtkTokenKind __fastcall FuncUseragent(int Index);
	TtkTokenKind __fastcall FuncUtc(int Index);
	TtkTokenKind __fastcall FuncValue(int Index);
	TtkTokenKind __fastcall FuncValueof(int Index);
	TtkTokenKind __fastcall FuncVar(int Index);
	TtkTokenKind __fastcall FuncVisibility(int Index);
	TtkTokenKind __fastcall FuncVlinkcolor(int Index);
	TtkTokenKind __fastcall FuncVoid(int Index);
	TtkTokenKind __fastcall FuncVspace(int Index);
	TtkTokenKind __fastcall FuncWatch(int Index);
	TtkTokenKind __fastcall FuncWhile(int Index);
	TtkTokenKind __fastcall FuncWidth(int Index);
	TtkTokenKind __fastcall FuncWindow(int Index);
	TtkTokenKind __fastcall FuncWith(int Index);
	TtkTokenKind __fastcall FuncWrite(int Index);
	TtkTokenKind __fastcall FuncWriteln(int Index);
	TtkTokenKind __fastcall FuncZindex(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall AndSymbolProc(void);
	void __fastcall CommentProc(void);
	void __fastcall CRProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall MinusProc(void);
	void __fastcall ModSymbolProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall OrSymbolProc(void);
	void __fastcall PlusProc(void);
	void __fastcall PointProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StarProc(void);
	void __fastcall StringProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall UnknownProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynJScriptSyn(System::Classes::TComponent* AOwner);
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
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NonReservedKeyAttri = {read=fNonReservedKeyAttri, write=fNonReservedKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* EventAttri = {read=fEventAttri, write=fEventAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynJScriptSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterjscript */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERJSCRIPT)
using namespace Synhighlighterjscript;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterjscriptHPP
