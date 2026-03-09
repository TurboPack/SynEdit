{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterDml.pas, released 2000-04-17.
The Original Code is based on the mwDmlSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Peter Adam.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

Known Issues:
  - There are no metadata qualifiers.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Dml highlighter for SynEdit)
@author(Peter Adam)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterDml unit provides SynEdit with a Dml highlighter.
}

unit SynHighlighterDml;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynFunc,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkBlock, tkComment, tkForm, tkFunction, tkIdentifier, tkKey,
    tkNull, tkNumber, tkQualifier, tkSpace, tkSpecial, tkString, tkSymbol,
    tkUnknown, tkVariable);

  TRangeState = (rsANil, rsAdd, rsFind, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: TSynNativeInt): TtkTokenKind of object;

  TSynDmlSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fIdentFuncTable: array[0..2438] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    fFormAttri: TSynHighlighterAttributes;
    fBlockAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fQualiAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fSpecialAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    function AltFunc(Index: TSynNativeInt): TtkTokenKind;
    function FuncAbs(Index: TSynNativeInt): TtkTokenKind;
    function FuncAbsolute_position(Index: TSynNativeInt): TtkTokenKind;
    function FuncAccount(Index: TSynNativeInt): TtkTokenKind;
    function FuncAcos(Index: TSynNativeInt): TtkTokenKind;
    function FuncActual_break(Index: TSynNativeInt): TtkTokenKind;
    function FuncAdd(Index: TSynNativeInt): TtkTokenKind;
    function FuncAdd_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncAlternate_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncAscii(Index: TSynNativeInt): TtkTokenKind;
    function FuncAsin(Index: TSynNativeInt): TtkTokenKind;
    function FuncAtan(Index: TSynNativeInt): TtkTokenKind;
    function FuncAtan2(Index: TSynNativeInt): TtkTokenKind;
    function FuncAttributes(Index: TSynNativeInt): TtkTokenKind;
    function FuncBack(Index: TSynNativeInt): TtkTokenKind;
    function FuncBase(Index: TSynNativeInt): TtkTokenKind;
    function FuncBatch(Index: TSynNativeInt): TtkTokenKind;
    function FuncBegin_block(Index: TSynNativeInt): TtkTokenKind;
    function FuncBegin_case(Index: TSynNativeInt): TtkTokenKind;
    function FuncBegin_disable_trigger(Index: TSynNativeInt): TtkTokenKind;
    function FuncBegin_row(Index: TSynNativeInt): TtkTokenKind;
    function FuncBegin_signal_to_status(Index: TSynNativeInt): TtkTokenKind;
    function FuncBell(Index: TSynNativeInt): TtkTokenKind;
    function FuncBinary_to_poly(Index: TSynNativeInt): TtkTokenKind;
    function FuncBottom_line(Index: TSynNativeInt): TtkTokenKind;
    function FuncBreak(Index: TSynNativeInt): TtkTokenKind;
    function FuncBreak0(Index: TSynNativeInt): TtkTokenKind;
    function FuncCall(Index: TSynNativeInt): TtkTokenKind;
    function FuncCase(Index: TSynNativeInt): TtkTokenKind;
    function FuncCeil(Index: TSynNativeInt): TtkTokenKind;
    function FuncCheck(Index: TSynNativeInt): TtkTokenKind;
    function FuncCheck_domain(Index: TSynNativeInt): TtkTokenKind;
    function FuncChr(Index: TSynNativeInt): TtkTokenKind;
    function FuncClear_buffer(Index: TSynNativeInt): TtkTokenKind;
    function FuncCli(Index: TSynNativeInt): TtkTokenKind;
    function FuncClose(Index: TSynNativeInt): TtkTokenKind;
    function FuncClose_text(Index: TSynNativeInt): TtkTokenKind;
    function FuncCol(Index: TSynNativeInt): TtkTokenKind;
    function FuncColumn_heading_row(Index: TSynNativeInt): TtkTokenKind;
    function FuncColumn_headings(Index: TSynNativeInt): TtkTokenKind;
    function FuncColumn_spacing(Index: TSynNativeInt): TtkTokenKind;
    function FuncCommit(Index: TSynNativeInt): TtkTokenKind;
    function FuncCommit_rate(Index: TSynNativeInt): TtkTokenKind;
    function FuncCompile(Index: TSynNativeInt): TtkTokenKind;
    function FuncCompress(Index: TSynNativeInt): TtkTokenKind;
    function FuncCompress_all(Index: TSynNativeInt): TtkTokenKind;
    function FuncConfirm(Index: TSynNativeInt): TtkTokenKind;
    function FuncConnect(Index: TSynNativeInt): TtkTokenKind;
    function FuncContinue(Index: TSynNativeInt): TtkTokenKind;
    function FuncCos(Index: TSynNativeInt): TtkTokenKind;
    function FuncCosh(Index: TSynNativeInt): TtkTokenKind;
    function FuncCross_reference(Index: TSynNativeInt): TtkTokenKind;
    function FuncDate(Index: TSynNativeInt): TtkTokenKind;
    function FuncDate_seconds(Index: TSynNativeInt): TtkTokenKind;
    function FuncDay_of_week(Index: TSynNativeInt): TtkTokenKind;
    function FuncDays(Index: TSynNativeInt): TtkTokenKind;
    function FuncDcl(Index: TSynNativeInt): TtkTokenKind;
    function FuncDefault_tag(Index: TSynNativeInt): TtkTokenKind;
    function FuncDelete(Index: TSynNativeInt): TtkTokenKind;
    function FuncDelete_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncDescription(Index: TSynNativeInt): TtkTokenKind;
    function FuncDir(Index: TSynNativeInt): TtkTokenKind;
    function FuncDisconnect(Index: TSynNativeInt): TtkTokenKind;
    function FuncDisplay(Index: TSynNativeInt): TtkTokenKind;
    function FuncDisplay_length(Index: TSynNativeInt): TtkTokenKind;
    function FuncDocumentation(Index: TSynNativeInt): TtkTokenKind;
    function FuncDomain(Index: TSynNativeInt): TtkTokenKind;
    function FuncEdit(Index: TSynNativeInt): TtkTokenKind;
    function FuncElse(Index: TSynNativeInt): TtkTokenKind;
    function FuncElse_if(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_block(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_case(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_disable_trigger(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_execute(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_if(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_row(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_signal_to_status(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd_while(Index: TSynNativeInt): TtkTokenKind;
    function FuncErase(Index: TSynNativeInt): TtkTokenKind;
    function FuncError(Index: TSynNativeInt): TtkTokenKind;
    function FuncExecute(Index: TSynNativeInt): TtkTokenKind;
    function FuncExit(Index: TSynNativeInt): TtkTokenKind;
    function FuncExit_forward(Index: TSynNativeInt): TtkTokenKind;
    function FuncExpand(Index: TSynNativeInt): TtkTokenKind;
    function FuncExternal(Index: TSynNativeInt): TtkTokenKind;
    function FuncFacility(Index: TSynNativeInt): TtkTokenKind;
    function FuncFailure(Index: TSynNativeInt): TtkTokenKind;
    function FuncFetch(Index: TSynNativeInt): TtkTokenKind;
    function FuncFiles(Index: TSynNativeInt): TtkTokenKind;
    function FuncFind(Index: TSynNativeInt): TtkTokenKind;
    function FuncFind_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncFinish(Index: TSynNativeInt): TtkTokenKind;
    function FuncFirst(Index: TSynNativeInt): TtkTokenKind;
    function FuncFloor(Index: TSynNativeInt): TtkTokenKind;
    function FuncFooting(Index: TSynNativeInt): TtkTokenKind;
    function FuncFooting_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncForm(Index: TSynNativeInt): TtkTokenKind;
    function FuncGenerate(Index: TSynNativeInt): TtkTokenKind;
    function FuncGoto(Index: TSynNativeInt): TtkTokenKind;
    function FuncGrouped_by(Index: TSynNativeInt): TtkTokenKind;
    function FuncHeading(Index: TSynNativeInt): TtkTokenKind;
    function FuncHeading_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncHeight(Index: TSynNativeInt): TtkTokenKind;
    function FuncIdentifier(Index: TSynNativeInt): TtkTokenKind;
    function FuncIf(Index: TSynNativeInt): TtkTokenKind;
    function FuncIn(Index: TSynNativeInt): TtkTokenKind;
    function FuncInput_block(Index: TSynNativeInt): TtkTokenKind;
    function FuncInput_mask(Index: TSynNativeInt): TtkTokenKind;
    function FuncInput_row_height(Index: TSynNativeInt): TtkTokenKind;
    function FuncInt(Index: TSynNativeInt): TtkTokenKind;
    function FuncInvoke(Index: TSynNativeInt): TtkTokenKind;
    function FuncItem(Index: TSynNativeInt): TtkTokenKind;
    function FuncItem_block(Index: TSynNativeInt): TtkTokenKind;
    function FuncItem_if(Index: TSynNativeInt): TtkTokenKind;
    function FuncJoined_to(Index: TSynNativeInt): TtkTokenKind;
    function FuncLeft(Index: TSynNativeInt): TtkTokenKind;
    function FuncLen(Index: TSynNativeInt): TtkTokenKind;
    function FuncLfooting(Index: TSynNativeInt): TtkTokenKind;
    function FuncLheading(Index: TSynNativeInt): TtkTokenKind;
    function FuncLine(Index: TSynNativeInt): TtkTokenKind;
    function FuncLines_after(Index: TSynNativeInt): TtkTokenKind;
    function FuncLines_before(Index: TSynNativeInt): TtkTokenKind;
    function FuncList(Index: TSynNativeInt): TtkTokenKind;
    function FuncLoad(Index: TSynNativeInt): TtkTokenKind;
    function FuncLock(Index: TSynNativeInt): TtkTokenKind;
    function FuncLog(Index: TSynNativeInt): TtkTokenKind;
    function FuncLog10(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_auto_select(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_col(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_data(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_first(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_height(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_noheading(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_nosearch(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_reduced_to(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_row(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_secondary(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_selection(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_sorted_by(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_width(Index: TSynNativeInt): TtkTokenKind;
    function FuncLov_with(Index: TSynNativeInt): TtkTokenKind;
    function FuncLowercase(Index: TSynNativeInt): TtkTokenKind;
    function FuncLtrim(Index: TSynNativeInt): TtkTokenKind;
    function FuncMail(Index: TSynNativeInt): TtkTokenKind;
    function FuncMenu(Index: TSynNativeInt): TtkTokenKind;
    function FuncMenu_block(Index: TSynNativeInt): TtkTokenKind;
    function FuncMenu_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncMessage(Index: TSynNativeInt): TtkTokenKind;
    function FuncMid(Index: TSynNativeInt): TtkTokenKind;
    function FuncMod(Index: TSynNativeInt): TtkTokenKind;
    function FuncModify_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncNew(Index: TSynNativeInt): TtkTokenKind;
    function FuncNo_domain(Index: TSynNativeInt): TtkTokenKind;
    function FuncNobell(Index: TSynNativeInt): TtkTokenKind;
    function FuncNoclear_buffer(Index: TSynNativeInt): TtkTokenKind;
    function FuncNodeadlock_exit(Index: TSynNativeInt): TtkTokenKind;
    function FuncNoerase(Index: TSynNativeInt): TtkTokenKind;
    function FuncNoerror(Index: TSynNativeInt): TtkTokenKind;
    function FuncNoexit_forward(Index: TSynNativeInt): TtkTokenKind;
    function FuncNoheading(Index: TSynNativeInt): TtkTokenKind;
    function FuncNolov_data(Index: TSynNativeInt): TtkTokenKind;
    function FuncNorepeat(Index: TSynNativeInt): TtkTokenKind;
    function FuncNostatus(Index: TSynNativeInt): TtkTokenKind;
    function FuncNototals(Index: TSynNativeInt): TtkTokenKind;
    function FuncNounderlines(Index: TSynNativeInt): TtkTokenKind;
    function FuncNowait(Index: TSynNativeInt): TtkTokenKind;
    function FuncOpen(Index: TSynNativeInt): TtkTokenKind;
    function FuncOpen_text(Index: TSynNativeInt): TtkTokenKind;
    function FuncOpt(Index: TSynNativeInt): TtkTokenKind;
    function FuncOptions(Index: TSynNativeInt): TtkTokenKind;
    function FuncOutput(Index: TSynNativeInt): TtkTokenKind;
    function FuncOutput_block(Index: TSynNativeInt): TtkTokenKind;
    function FuncOutput_mask(Index: TSynNativeInt): TtkTokenKind;
    function FuncPause(Index: TSynNativeInt): TtkTokenKind;
    function FuncPause_block(Index: TSynNativeInt): TtkTokenKind;
    function FuncPerform(Index: TSynNativeInt): TtkTokenKind;
    function FuncPoly_to_binary(Index: TSynNativeInt): TtkTokenKind;
    function FuncPos(Index: TSynNativeInt): TtkTokenKind;
    function FuncPrint(Index: TSynNativeInt): TtkTokenKind;
    function FuncProcedure_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncPrompt(Index: TSynNativeInt): TtkTokenKind;
    function FuncProtect(Index: TSynNativeInt): TtkTokenKind;
    function FuncQuery(Index: TSynNativeInt): TtkTokenKind;
    function FuncQuery_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncRandom(Index: TSynNativeInt): TtkTokenKind;
    function FuncRead_line(Index: TSynNativeInt): TtkTokenKind;
    function FuncRead_only(Index: TSynNativeInt): TtkTokenKind;
    function FuncReceive(Index: TSynNativeInt): TtkTokenKind;
    function FuncReceive_arguments(Index: TSynNativeInt): TtkTokenKind;
    function FuncReceive_data(Index: TSynNativeInt): TtkTokenKind;
    function FuncReceive_table(Index: TSynNativeInt): TtkTokenKind;
    function FuncReduced_to(Index: TSynNativeInt): TtkTokenKind;
    function FuncRelease(Index: TSynNativeInt): TtkTokenKind;
    function FuncRemain(Index: TSynNativeInt): TtkTokenKind;
    function FuncRepeat(Index: TSynNativeInt): TtkTokenKind;
    function FuncReport(Index: TSynNativeInt): TtkTokenKind;
    function FuncReport_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncReposition(Index: TSynNativeInt): TtkTokenKind;
    function FuncRewind_text(Index: TSynNativeInt): TtkTokenKind;
    function FuncRfooting(Index: TSynNativeInt): TtkTokenKind;
    function FuncRheading(Index: TSynNativeInt): TtkTokenKind;
    function FuncRight(Index: TSynNativeInt): TtkTokenKind;
    function FuncRollback(Index: TSynNativeInt): TtkTokenKind;
    function FuncRound(Index: TSynNativeInt): TtkTokenKind;
    function FuncRow(Index: TSynNativeInt): TtkTokenKind;
    function FuncRow_height(Index: TSynNativeInt): TtkTokenKind;
    function FuncSearch(Index: TSynNativeInt): TtkTokenKind;
    function FuncSecondary(Index: TSynNativeInt): TtkTokenKind;
    function FuncSeconds(Index: TSynNativeInt): TtkTokenKind;
    function FuncSelection(Index: TSynNativeInt): TtkTokenKind;
    function FuncSend(Index: TSynNativeInt): TtkTokenKind;
    function FuncSend_data(Index: TSynNativeInt): TtkTokenKind;
    function FuncSend_message(Index: TSynNativeInt): TtkTokenKind;
    function FuncSend_table(Index: TSynNativeInt): TtkTokenKind;
    function FuncSequence(Index: TSynNativeInt): TtkTokenKind;
    function FuncSeverity(Index: TSynNativeInt): TtkTokenKind;
    function FuncSin(Index: TSynNativeInt): TtkTokenKind;
    function FuncSinh(Index: TSynNativeInt): TtkTokenKind;
    function FuncSorted_by(Index: TSynNativeInt): TtkTokenKind;
    function FuncSource(Index: TSynNativeInt): TtkTokenKind;
    function FuncSource_if(Index: TSynNativeInt): TtkTokenKind;
    function FuncSqrt(Index: TSynNativeInt): TtkTokenKind;
    function FuncStart_stream(Index: TSynNativeInt): TtkTokenKind;
    function FuncStart_transaction(Index: TSynNativeInt): TtkTokenKind;
    function FuncStatistic(Index: TSynNativeInt): TtkTokenKind;
    function FuncStatus(Index: TSynNativeInt): TtkTokenKind;
    function FuncStream_name(Index: TSynNativeInt): TtkTokenKind;
    function FuncString(Index: TSynNativeInt): TtkTokenKind;
    function FuncSuccess(Index: TSynNativeInt): TtkTokenKind;
    function FuncSwitch(Index: TSynNativeInt): TtkTokenKind;
    function FuncSwitch_base(Index: TSynNativeInt): TtkTokenKind;
    function FuncSystem(Index: TSynNativeInt): TtkTokenKind;
    function FuncTable(Index: TSynNativeInt): TtkTokenKind;
    function FuncTable_form(Index: TSynNativeInt): TtkTokenKind;
    function FuncTable_search(Index: TSynNativeInt): TtkTokenKind;
    function FuncTag(Index: TSynNativeInt): TtkTokenKind;
    function FuncTag_length(Index: TSynNativeInt): TtkTokenKind;
    function FuncTan(Index: TSynNativeInt): TtkTokenKind;
    function FuncTanh(Index: TSynNativeInt): TtkTokenKind;
    function FuncTarget(Index: TSynNativeInt): TtkTokenKind;
    function FuncText(Index: TSynNativeInt): TtkTokenKind;
    function FuncText_only(Index: TSynNativeInt): TtkTokenKind;
    function FuncTitle(Index: TSynNativeInt): TtkTokenKind;
    function FuncTo(Index: TSynNativeInt): TtkTokenKind;
    function FuncTop_line(Index: TSynNativeInt): TtkTokenKind;
    function FuncTotal(Index: TSynNativeInt): TtkTokenKind;
    function FuncTransfer(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrigger(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrim(Index: TSynNativeInt): TtkTokenKind;
    function FuncTsuppress(Index: TSynNativeInt): TtkTokenKind;
    function FuncUnload(Index: TSynNativeInt): TtkTokenKind;
    function FuncUppercase(Index: TSynNativeInt): TtkTokenKind;
    function FuncUse_if(Index: TSynNativeInt): TtkTokenKind;
    function FuncUser_key(Index: TSynNativeInt): TtkTokenKind;
    function FuncUsing(Index: TSynNativeInt): TtkTokenKind;
    function FuncUtilities(Index: TSynNativeInt): TtkTokenKind;
    function FuncWait(Index: TSynNativeInt): TtkTokenKind;
    function FuncWhile(Index: TSynNativeInt): TtkTokenKind;
    function FuncWidth(Index: TSynNativeInt): TtkTokenKind;
    function FuncWith(Index: TSynNativeInt): TtkTokenKind;
    function FuncWrite(Index: TSynNativeInt): TtkTokenKind;
    function FuncWrite_line(Index: TSynNativeInt): TtkTokenKind;
    function FuncYesno_block(Index: TSynNativeInt): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure SymbolProc;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure RemProc;
    function IsQuali: Boolean;
    function IsSpecial: Boolean;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property BlockAttri: TSynHighlighterAttributes read fBlockAttri
      write fBlockAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property FormAttri: TSynHighlighterAttributes read fFormAttri
      write fFormAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property QualiAttri: TSynHighlighterAttributes read fQualiAttri
      write fQualiAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SpecialAttri: TSynHighlighterAttributes read fSpecialAttri
      write fSpecialAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..263] of string = (
    'abs', 'absolute_position', 'account', 'acos', 'actual_break', 'add', 
    'add_form', 'alternate_form', 'ascii', 'asin', 'atan', 'atan2', 
    'attributes', 'back', 'base', 'batch', 'begin_block', 'begin_case', 
    'begin_disable_trigger', 'begin_row', 'begin_signal_to_status', 'bell', 
    'binary_to_poly', 'bottom_line', 'break', 'break0', 'call', 'case', 'ceil', 
    'check', 'check_domain', 'chr', 'clear_buffer', 'cli', 'close', 
    'close_text', 'col', 'column_heading_row', 'column_headings', 
    'column_spacing', 'commit', 'commit_rate', 'compile', 'compress', 
    'compress_all', 'confirm', 'connect', 'continue', 'cos', 'cosh', 
    'cross_reference', 'date', 'date_seconds', 'day_of_week', 'days', 'dcl', 
    'default_tag', 'delete', 'delete_form', 'description', 'dir', 'disconnect', 
    'display', 'display_length', 'documentation', 'domain', 'edit', 'else', 
    'else_if', 'end_block', 'end_case', 'end_disable_trigger', 'end_execute', 
    'end_form', 'end_if', 'end_row', 'end_signal_to_status', 'end_while', 
    'erase', 'error', 'execute', 'exit', 'exit_forward', 'expand', 'external', 
    'facility', 'failure', 'fetch', 'files', 'find', 'find_form', 'finish', 
    'first', 'floor', 'footing', 'footing_form', 'form', 'generate', 'goto', 
    'grouped_by', 'heading', 'heading_form', 'height', 'identifier', 'if', 'in', 
    'input_block', 'input_mask', 'input_row_height', 'int', 'invoke', 'item', 
    'item_block', 'item_if', 'joined_to', 'left', 'len', 'lfooting', 'lheading', 
    'line', 'lines_after', 'lines_before', 'list', 'load', 'lock', 'log', 
    'log10', 'lov', 'lov_auto_select', 'lov_col', 'lov_data', 'lov_first', 
    'lov_height', 'lov_noheading', 'lov_nosearch', 'lov_reduced_to', 'lov_row', 
    'lov_secondary', 'lov_selection', 'lov_sorted_by', 'lov_width', 'lov_with', 
    'lowercase', 'ltrim', 'mail', 'menu', 'menu_block', 'menu_form', 'message', 
    'mid', 'mod', 'modify_form', 'new', 'no_domain', 'nobell', 'noclear_buffer', 
    'nodeadlock_exit', 'noerase', 'noerror', 'noexit_forward', 'noheading', 
    'nolov_data', 'norepeat', 'nostatus', 'nototals', 'nounderlines', 'nowait', 
    'open', 'open_text', 'opt', 'options', 'output', 'output_block', 
    'output_mask', 'pause', 'pause_block', 'perform', 'poly_to_binary', 'pos', 
    'print', 'procedure_form', 'prompt', 'protect', 'query', 'query_form', 
    'random', 'read_line', 'read_only', 'receive', 'receive_arguments', 
    'receive_data', 'receive_table', 'reduced_to', 'release', 'remain', 
    'repeat', 'report', 'report_form', 'reposition', 'rewind_text', 'rfooting', 
    'rheading', 'right', 'rollback', 'round', 'row', 'row_height', 'search', 
    'secondary', 'seconds', 'selection', 'send', 'send_data', 'send_message', 
    'send_table', 'sequence', 'severity', 'sin', 'sinh', 'sorted_by', 'source', 
    'source_if', 'sqrt', 'start_stream', 'start_transaction', 'statistic', 
    'status', 'stream_name', 'string', 'success', 'switch', 'switch_base', 
    'system', 'table', 'table_form', 'table_search', 'tag', 'tag_length', 'tan', 
    'tanh', 'target', 'text', 'text_only', 'title', 'to', 'top_line', 'total', 
    'transfer', 'trigger', 'trim', 'tsuppress', 'unload', 'uppercase', 'use_if', 
    'user_key', 'using', 'utilities', 'wait', 'while', 'width', 'with', 'write', 
    'write_line', 'yesno_block' 
  );

  KeyIndices: array[0..2438] of TSynNativeInt = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 261, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 230, -1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 217, -1, -1, -1, -1, -1, 183, -1, 246, -1, 134, -1, -1, -1, -1, 
    -1, 65, -1, -1, 223, -1, -1, -1, -1, -1, 213, -1, -1, -1, 46, -1, -1, 262, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 111, 157, -1, -1, -1, -1, -1, -1, 118, -1, -1, -1, -1, -1, -1, 
    -1, -1, 208, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 22, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 86, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 123, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 84, -1, 154, -1, 96, -1, -1, -1, 176, -1, -1, -1, 120, 178, -1, -1, -1, 
    -1, 74, -1, -1, -1, -1, 241, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 81, -1, 147, -1, -1, -1, 122, 
    -1, 58, -1, 87, 191, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 170, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 5, -1, -1, 194, -1, -1, -1, 243, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 52, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 248, -1, -1, 
    -1, 28, 77, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 255, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 239, -1, -1, -1, -1, 20, -1, -1, -1, -1, 79, 
    116, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 0, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 196, -1, -1, 85, -1, -1, -1, 104, -1, 103, -1, -1, 14, -1, -1, 
    131, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 105, -1, 
    -1, 6, -1, 182, -1, -1, 171, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 80, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 97, -1, -1, -1, -1, 41, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 204, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 185, -1, -1, -1, -1, 
    -1, -1, -1, 115, -1, -1, 108, -1, 150, -1, -1, 42, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 224, -1, -1, -1, 59, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 143, 166, -1, -1, 
    -1, -1, -1, -1, 225, -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, 92, -1, -1, 
    226, -1, 161, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, -1, -1, 237, -1, -1, -1, -1, 
    -1, 100, -1, -1, -1, -1, -1, -1, -1, -1, 214, -1, -1, -1, -1, -1, -1, -1, 
    -1, 151, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1, -1, 146, 210, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 23, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 245, -1, -1, -1, -1, -1, 68, -1, 
    231, -1, -1, -1, 126, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 57, 
    -1, -1, -1, 112, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, 
    -1, -1, 75, -1, 252, 212, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    149, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 187, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 
    -1, -1, 47, -1, -1, -1, -1, -1, -1, -1, -1, 164, 35, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    234, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107, -1, -1, -1, -1, 
    -1, -1, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 240, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 39, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 72, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 21, -1, -1, 229, 11, 
    -1, -1, 43, -1, -1, -1, -1, -1, 236, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 159, -1, -1, 238, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 181, -1, 139, -1, -1, -1, -1, -1, -1, 37, -1, -1, -1, 15, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 83, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 69, 
    258, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, -1, 153, 
    -1, -1, 36, -1, -1, 175, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, -1, -1, -1, 
    -1, -1, -1, -1, 48, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, 232, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 199, -1, -1, -1, -1, -1, 9, -1, -1, 140, 193, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 82, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 177, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 163, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 188, -1, -1, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1, 
    -1, -1, 228, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    19, -1, 66, -1, -1, -1, 24, -1, -1, -1, -1, 186, -1, -1, -1, -1, 99, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 253, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 54, -1, -1, -1, -1, 259, -1, 32, -1, -1, -1, -1, -1, 
    121, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 221, 209, 260, -1, 
    -1, -1, -1, -1, -1, -1, 76, 257, -1, -1, -1, -1, 211, -1, 90, -1, -1, -1, 
    -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, 70, 
    -1, -1, -1, -1, -1, 63, -1, -1, -1, 25, 207, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 174, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 167, -1, -1, -1, -1, -1, -1, -1, -1, 179, -1, 
    189, -1, -1, -1, 113, -1, -1, -1, 110, -1, 205, -1, 56, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 51, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 132, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1, -1, -1, 
    -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 180, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, -1, 254, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, -1, -1, 155, -1, -1, 
    235, -1, 34, -1, 218, -1, -1, -1, -1, -1, -1, 152, -1, -1, -1, -1, 220, -1, 
    -1, -1, 141, -1, -1, -1, -1, 195, -1, -1, -1, 137, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, 160, -1, 
    -1, -1, -1, 227, -1, -1, -1, -1, -1, -1, -1, 148, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 203, 
    -1, -1, -1, -1, -1, -1, -1, -1, 156, -1, -1, -1, -1, -1, -1, -1, -1, -1, 91, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 62, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 219, -1, -1, -1, -1, -1, 61, -1, -1, 30, -1, -1, 130, -1, -1, -1, 
    -1, -1, -1, -1, -1, 12, -1, 202, -1, -1, -1, -1, -1, 200, -1, -1, 169, -1, 
    -1, -1, -1, -1, -1, 16, -1, -1, -1, 172, -1, -1, -1, -1, -1, -1, 162, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 114, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 247, -1, -1, -1, -1, -1, -1, 242, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 198, -1, -1, -1, 251, -1, -1, -1, -1, -1, 
    -1, -1, -1, 216, -1, -1, -1, -1, 128, 27, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 18, -1, -1, -1, 
    -1, 158, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, -1, 173, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, 17, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 135, -1, -1, 190, -1, -1, -1, 222, 60, -1, -1, -1, -1, 
    -1, -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 38, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 98, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 136, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 168, -1, -1, -1, -1, -1, -1, 144, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 94, -1, 95, -1, -1, -1, -1, -1, 215, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 145, -1, 
    -1, -1, 10, 250, -1, -1, -1, 256, -1, -1, -1, -1, -1, -1, -1, -1, -1, 197, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 201, -1, -1, -1, 
    233, -1, -1, -1, -1, -1, 249, -1, -1, 184, -1, -1, -1, -1, -1, 263, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 125, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 26, 138, -1, -1, -1, -1, -1, -1, -1, -1, 64, 
    -1, -1, -1, 55, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, 127, 206, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 165, -1, 
    -1, 244, -1, -1, -1, -1, -1, -1, -1, -1 
  );

{$Q-}
function TSynDmlSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 798 + Ord(Str^) * 3;
    Inc(Str);
  end;
  Result := Result mod 2439;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynDmlSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynDmlSyn.InitIdent;
var
  i: TSynNativeInt;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[435] := FuncAbs;
  fIdentFuncTable[41] := FuncAbsolute_position;
  fIdentFuncTable[13] := FuncAccount;
  fIdentFuncTable[2405] := FuncAcos;
  fIdentFuncTable[1707] := FuncActual_break;
  fIdentFuncTable[300] := FuncAdd;
  fIdentFuncTable[486] := FuncAdd_form;
  fIdentFuncTable[839] := FuncAlternate_form;
  fIdentFuncTable[735] := FuncAscii;
  fIdentFuncTable[1265] := FuncAsin;
  fIdentFuncTable[2300] := FuncAtan;
  fIdentFuncTable[1065] := FuncAtan2;
  fIdentFuncTable[1930] := FuncAttributes;
  fIdentFuncTable[1202] := FuncBack;
  fIdentFuncTable[464] := FuncBase;
  fIdentFuncTable[1118] := FuncBatch;
  fIdentFuncTable[1948] := FuncBegin_block;
  fIdentFuncTable[2124] := FuncBegin_case;
  fIdentFuncTable[2068] := FuncBegin_disable_trigger;
  fIdentFuncTable[1385] := FuncBegin_row;
  fIdentFuncTable[387] := FuncBegin_signal_to_status;
  fIdentFuncTable[1061] := FuncBell;
  fIdentFuncTable[166] := FuncBinary_to_poly;
  fIdentFuncTable[776] := FuncBottom_line;
  fIdentFuncTable[1391] := FuncBreak;
  fIdentFuncTable[1524] := FuncBreak0;
  fIdentFuncTable[2380] := FuncCall;
  fIdentFuncTable[2044] := FuncCase;
  fIdentFuncTable[337] := FuncCeil;
  fIdentFuncTable[644] := FuncCheck;
  fIdentFuncTable[1918] := FuncCheck_domain;
  fIdentFuncTable[1512] := FuncChr;
  fIdentFuncTable[1454] := FuncClear_buffer;
  fIdentFuncTable[1305] := FuncCli;
  fIdentFuncTable[1761] := FuncClose;
  fIdentFuncTable[908] := FuncClose_text;
  fIdentFuncTable[1179] := FuncCol;
  fIdentFuncTable[1114] := FuncColumn_heading_row;
  fIdentFuncTable[2183] := FuncColumn_headings;
  fIdentFuncTable[1007] := FuncColumn_spacing;
  fIdentFuncTable[697] := FuncCommit;
  fIdentFuncTable[521] := FuncCommit_rate;
  fIdentFuncTable[591] := FuncCompile;
  fIdentFuncTable[1068] := FuncCompress;
  fIdentFuncTable[1359] := FuncCompress_all;
  fIdentFuncTable[1637] := FuncConfirm;
  fIdentFuncTable[89] := FuncConnect;
  fIdentFuncTable[898] := FuncContinue;
  fIdentFuncTable[1200] := FuncCos;
  fIdentFuncTable[1747] := FuncCosh;
  fIdentFuncTable[954] := FuncCross_reference;
  fIdentFuncTable[1630] := FuncDate;
  fIdentFuncTable[320] := FuncDate_seconds;
  fIdentFuncTable[368] := FuncDay_of_week;
  fIdentFuncTable[1447] := FuncDays;
  fIdentFuncTable[2394] := FuncDcl;
  fIdentFuncTable[1583] := FuncDefault_tag;
  fIdentFuncTable[820] := FuncDelete;
  fIdentFuncTable[261] := FuncDelete_form;
  fIdentFuncTable[608] := FuncDescription;
  fIdentFuncTable[2142] := FuncDir;
  fIdentFuncTable[1915] := FuncDisconnect;
  fIdentFuncTable[1889] := FuncDisplay;
  fIdentFuncTable[1520] := FuncDisplay_length;
  fIdentFuncTable[2390] := FuncDocumentation;
  fIdentFuncTable[76] := FuncDomain;
  fIdentFuncTable[1387] := FuncEdit;
  fIdentFuncTable[1414] := FuncElse;
  fIdentFuncTable[801] := FuncElse_if;
  fIdentFuncTable[1158] := FuncEnd_block;
  fIdentFuncTable[1514] := FuncEnd_case;
  fIdentFuncTable[1734] := FuncEnd_disable_trigger;
  fIdentFuncTable[1043] := FuncEnd_execute;
  fIdentFuncTable[2119] := FuncEnd_form;
  fIdentFuncTable[224] := FuncEnd_if;
  fIdentFuncTable[842] := FuncEnd_row;
  fIdentFuncTable[1484] := FuncEnd_signal_to_status;
  fIdentFuncTable[338] := FuncEnd_while;
  fIdentFuncTable[893] := FuncErase;
  fIdentFuncTable[392] := FuncError;
  fIdentFuncTable[503] := FuncExecute;
  fIdentFuncTable[253] := FuncExit;
  fIdentFuncTable[1280] := FuncExit_forward;
  fIdentFuncTable[1146] := FuncExpand;
  fIdentFuncTable[206] := FuncExternal;
  fIdentFuncTable[455] := FuncFacility;
  fIdentFuncTable[176] := FuncFailure;
  fIdentFuncTable[263] := FuncFetch;
  fIdentFuncTable[2106] := FuncFiles;
  fIdentFuncTable[1191] := FuncFind;
  fIdentFuncTable[1492] := FuncFind_form;
  fIdentFuncTable[1868] := FuncFinish;
  fIdentFuncTable[651] := FuncFirst;
  fIdentFuncTable[2081] := FuncFloor;
  fIdentFuncTable[2267] := FuncFooting;
  fIdentFuncTable[2269] := FuncFooting_form;
  fIdentFuncTable[210] := FuncForm;
  fIdentFuncTable[516] := FuncGenerate;
  fIdentFuncTable[2196] := FuncGoto;
  fIdentFuncTable[1401] := FuncGrouped_by;
  fIdentFuncTable[711] := FuncHeading;
  fIdentFuncTable[1173] := FuncHeading_form;
  fIdentFuncTable[194] := FuncHeight;
  fIdentFuncTable[461] := FuncIdentifier;
  fIdentFuncTable[459] := FuncIf;
  fIdentFuncTable[483] := FuncIn;
  fIdentFuncTable[2151] := FuncInput_block;
  fIdentFuncTable[947] := FuncInput_mask;
  fIdentFuncTable[586] := FuncInput_row_height;
  fIdentFuncTable[420] := FuncInt;
  fIdentFuncTable[1579] := FuncInvoke;
  fIdentFuncTable[134] := FuncItem;
  fIdentFuncTable[824] := FuncItem_block;
  fIdentFuncTable[1575] := FuncItem_if;
  fIdentFuncTable[1988] := FuncJoined_to;
  fIdentFuncTable[583] := FuncLeft;
  fIdentFuncTable[393] := FuncLen;
  fIdentFuncTable[1698] := FuncLfooting;
  fIdentFuncTable[142] := FuncLheading;
  fIdentFuncTable[439] := FuncLine;
  fIdentFuncTable[218] := FuncLines_after;
  fIdentFuncTable[1460] := FuncLines_before;
  fIdentFuncTable[259] := FuncList;
  fIdentFuncTable[193] := FuncLoad;
  fIdentFuncTable[124] := FuncLock;
  fIdentFuncTable[2361] := FuncLog;
  fIdentFuncTable[807] := FuncLog10;
  fIdentFuncTable[2406] := FuncLov;
  fIdentFuncTable[2043] := FuncLov_auto_select;
  fIdentFuncTable[1806] := FuncLov_col;
  fIdentFuncTable[1921] := FuncLov_data;
  fIdentFuncTable[467] := FuncLov_first;
  fIdentFuncTable[1673] := FuncLov_height;
  fIdentFuncTable[1499] := FuncLov_noheading;
  fIdentFuncTable[70] := FuncLov_nosearch;
  fIdentFuncTable[2134] := FuncLov_reduced_to;
  fIdentFuncTable[2208] := FuncLov_row;
  fIdentFuncTable[1788] := FuncLov_secondary;
  fIdentFuncTable[2381] := FuncLov_selection;
  fIdentFuncTable[1107] := FuncLov_sorted_by;
  fIdentFuncTable[1268] := FuncLov_width;
  fIdentFuncTable[1779] := FuncLov_with;
  fIdentFuncTable[538] := FuncLowercase;
  fIdentFuncTable[631] := FuncLtrim;
  fIdentFuncTable[2233] := FuncMail;
  fIdentFuncTable[2296] := FuncMenu;
  fIdentFuncTable[743] := FuncMenu_block;
  fIdentFuncTable[255] := FuncMenu_form;
  fIdentFuncTable[1824] := FuncMessage;
  fIdentFuncTable[858] := FuncMid;
  fIdentFuncTable[588] := FuncMod;
  fIdentFuncTable[729] := FuncModify_form;
  fIdentFuncTable[1770] := FuncNew;
  fIdentFuncTable[1176] := FuncNo_domain;
  fIdentFuncTable[208] := FuncNobell;
  fIdentFuncTable[1756] := FuncNoclear_buffer;
  fIdentFuncTable[1858] := FuncNodeadlock_exit;
  fIdentFuncTable[135] := FuncNoerase;
  fIdentFuncTable[2073] := FuncNoerror;
  fIdentFuncTable[1092] := FuncNoexit_forward;
  fIdentFuncTable[1811] := FuncNoheading;
  fIdentFuncTable[656] := FuncNolov_data;
  fIdentFuncTable[1959] := FuncNorepeat;
  fIdentFuncTable[1319] := FuncNostatus;
  fIdentFuncTable[907] := FuncNototals;
  fIdentFuncTable[2427] := FuncNounderlines;
  fIdentFuncTable[632] := FuncNowait;
  fIdentFuncTable[1560] := FuncOpen;
  fIdentFuncTable[2226] := FuncOpen_text;
  fIdentFuncTable[1941] := FuncOpt;
  fIdentFuncTable[290] := FuncOptions;
  fIdentFuncTable[491] := FuncOutput;
  fIdentFuncTable[1952] := FuncOutput_block;
  fIdentFuncTable[2108] := FuncOutput_mask;
  fIdentFuncTable[1539] := FuncPause;
  fIdentFuncTable[1182] := FuncPause_block;
  fIdentFuncTable[214] := FuncPerform;
  fIdentFuncTable[1294] := FuncPoly_to_binary;
  fIdentFuncTable[219] := FuncPos;
  fIdentFuncTable[1569] := FuncPrint;
  fIdentFuncTable[1719] := FuncProcedure_form;
  fIdentFuncTable[1105] := FuncPrompt;
  fIdentFuncTable[488] := FuncProtect;
  fIdentFuncTable[66] := FuncQuery;
  fIdentFuncTable[2344] := FuncQuery_form;
  fIdentFuncTable[575] := FuncRandom;
  fIdentFuncTable[1396] := FuncRead_line;
  fIdentFuncTable[885] := FuncRead_only;
  fIdentFuncTable[1353] := FuncReceive;
  fIdentFuncTable[1571] := FuncReceive_arguments;
  fIdentFuncTable[2137] := FuncReceive_data;
  fIdentFuncTable[264] := FuncReceive_table;
  fIdentFuncTable[410] := FuncReduced_to;
  fIdentFuncTable[1269] := FuncRelease;
  fIdentFuncTable[303] := FuncRemain;
  fIdentFuncTable[1784] := FuncRepeat;
  fIdentFuncTable[452] := FuncReport;
  fIdentFuncTable[2315] := FuncReport_form;
  fIdentFuncTable[2025] := FuncReposition;
  fIdentFuncTable[1259] := FuncRewind_text;
  fIdentFuncTable[1938] := FuncRfooting;
  fIdentFuncTable[2331] := FuncRheading;
  fIdentFuncTable[1932] := FuncRight;
  fIdentFuncTable[1849] := FuncRollback;
  fIdentFuncTable[553] := FuncRound;
  fIdentFuncTable[1581] := FuncRow;
  fIdentFuncTable[2407] := FuncRow_height;
  fIdentFuncTable[1525] := FuncSearch;
  fIdentFuncTable[151] := FuncSecondary;
  fIdentFuncTable[1475] := FuncSeconds;
  fIdentFuncTable[744] := FuncSelection;
  fIdentFuncTable[1490] := FuncSend;
  fIdentFuncTable[845] := FuncSend_data;
  fIdentFuncTable[85] := FuncSend_message;
  fIdentFuncTable[720] := FuncSend_table;
  fIdentFuncTable[2275] := FuncSequence;
  fIdentFuncTable[2038] := FuncSeverity;
  fIdentFuncTable[60] := FuncSin;
  fIdentFuncTable[1763] := FuncSinh;
  fIdentFuncTable[1909] := FuncSorted_by;
  fIdentFuncTable[1775] := FuncSource;
  fIdentFuncTable[1474] := FuncSource_if;
  fIdentFuncTable[2141] := FuncSqrt;
  fIdentFuncTable[79] := FuncStart_stream;
  fIdentFuncTable[604] := FuncStart_transaction;
  fIdentFuncTable[639] := FuncStatistic;
  fIdentFuncTable[654] := FuncStatus;
  fIdentFuncTable[1816] := FuncStream_name;
  fIdentFuncTable[1368] := FuncString;
  fIdentFuncTable[1064] := FuncSuccess;
  fIdentFuncTable[39] := FuncSwitch;
  fIdentFuncTable[803] := FuncSwitch_base;
  fIdentFuncTable[1212] := FuncSystem;
  fIdentFuncTable[2335] := FuncTable;
  fIdentFuncTable[934] := FuncTable_form;
  fIdentFuncTable[1759] := FuncTable_search;
  fIdentFuncTable[1074] := FuncTag;
  fIdentFuncTable[705] := FuncTag_length;
  fIdentFuncTable[1095] := FuncTan;
  fIdentFuncTable[382] := FuncTanh;
  fIdentFuncTable[975] := FuncTarget;
  fIdentFuncTable[229] := FuncText;
  fIdentFuncTable[2007] := FuncText_only;
  fIdentFuncTable[307] := FuncTitle;
  fIdentFuncTable[2430] := FuncTo;
  fIdentFuncTable[795] := FuncTop_line;
  fIdentFuncTable[68] := FuncTotal;
  fIdentFuncTable[2000] := FuncTransfer;
  fIdentFuncTable[333] := FuncTrigger;
  fIdentFuncTable[2341] := FuncTrim;
  fIdentFuncTable[2301] := FuncTsuppress;
  fIdentFuncTable[2029] := FuncUnload;
  fIdentFuncTable[844] := FuncUppercase;
  fIdentFuncTable[1437] := FuncUse_if;
  fIdentFuncTable[1736] := FuncUser_key;
  fIdentFuncTable[353] := FuncUsing;
  fIdentFuncTable[2305] := FuncUtilities;
  fIdentFuncTable[1485] := FuncWait;
  fIdentFuncTable[1159] := FuncWhile;
  fIdentFuncTable[1452] := FuncWidth;
  fIdentFuncTable[1476] := FuncWith;
  fIdentFuncTable[27] := FuncWrite;
  fIdentFuncTable[92] := FuncWrite_line;
  fIdentFuncTable[2350] := FuncYesno_block;
end;

function TSynDmlSyn.IsQuali: boolean;
begin
  Result:= False;
  if Run > 0 then
    if fLine[Run - 1] = '/' then Result:= True;
end;

function TSynDmlSyn.IsSpecial: Boolean;
begin
  Result:= False;
  if Run > 0 then
    if fLine[Run - 1] = '%' then Result:= True;
end;

function TSynDmlSyn.FuncAbs(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAbsolute_position(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAccount(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAcos(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncActual_break(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAdd(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsSpecial then
      Result := tkSpecial
    else
    begin
      Result := tkKey;
      fRange := rsAdd;
    end;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAdd_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAlternate_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAscii(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAsin(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAtan(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAtan2(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncAttributes(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBack(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBatch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBegin_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBegin_case(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBegin_disable_trigger(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBegin_row(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBegin_signal_to_status(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBell(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBinary_to_poly(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBottom_line(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBreak(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncBreak0(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCall(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCeil(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCheck(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCheck_domain(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncChr(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncClear_buffer(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCli(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncClose(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncClose_text(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCol(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncColumn_heading_row(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncColumn_headings(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncColumn_spacing(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCommit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCommit_rate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCompile(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCompress(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCompress_all(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncConfirm(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncConnect(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncContinue(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCos(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCosh(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncCross_reference(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDate_seconds(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDay_of_week(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDays(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDcl(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDefault_tag(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDelete(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDelete_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDescription(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDir(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDisconnect(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDisplay(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDisplay_length(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDocumentation(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncDomain(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEdit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncElse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncElse_if(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_case(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_disable_trigger(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_execute(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_if(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_row(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_signal_to_status(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncEnd_while(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncErase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncError(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncExecute(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncExit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncExit_forward(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncExpand(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncExternal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFacility(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFailure(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFetch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFiles(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFind(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    fRange := rsFind;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFind_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFinish(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFirst(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFloor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFooting(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncFooting_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncForm(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsSpecial then
      Result := tkSpecial
    else
      Result := tkForm;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncGenerate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncGoto(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncGrouped_by(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncHeading(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncHeading_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncHeight(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncIdentifier(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncIf(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncIn(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and (fRange = rsFind) then
  begin
    Result := tkKey;
    fRange := rsUnKnown;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncInput_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncInput_mask(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncInput_row_height(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncInt(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncInvoke(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncItem(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncItem_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncItem_if(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncJoined_to(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLeft(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLen(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkFunction;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLfooting(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLheading(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLine(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLines_after(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLines_before(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncList(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLoad(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLock(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLog(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkFunction;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLog10(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_auto_select(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_col(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_data(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_first(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_height(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_noheading(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_nosearch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_reduced_to(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_row(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_secondary(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_selection(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_sorted_by(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_width(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLov_with(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLowercase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncLtrim(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncMail(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncMenu(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncMenu_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncMenu_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncMessage(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncMid(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncMod(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncModify_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNew(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNo_domain(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNobell(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNoclear_buffer(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNodeadlock_exit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNoerase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNoerror(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNoexit_forward(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNoheading(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNolov_data(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNorepeat(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNostatus(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNototals(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNounderlines(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncNowait(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncOpen(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncOpen_text(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncOpt(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncOptions(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncOutput(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncOutput_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncOutput_mask(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncPause(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncPause_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncPerform(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncPoly_to_binary(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncPos(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncPrint(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncProcedure_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncPrompt(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncProtect(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncQuery(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncQuery_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRandom(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRead_line(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRead_only(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReceive(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReceive_arguments(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReceive_data(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReceive_table(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReduced_to(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRelease(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRemain(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRepeat(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReport(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReport_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncReposition(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRewind_text(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRfooting(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRheading(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRight(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRollback(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRound(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRow(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncRow_height(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSearch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSecondary(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSeconds(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSelection(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSend(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSend_data(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSend_message(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSend_table(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSequence(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSeverity(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSin(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSinh(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSorted_by(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSource(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSource_if(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSqrt(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncStart_stream(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncStart_transaction(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncStatistic(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncStatus(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncStream_name(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncString(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSuccess(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else if IsSpecial then
      Result := tkSpecial
    else
      Result := tkIdentifier;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSwitch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSwitch_base(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncSystem(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTable_form(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkForm
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTable_search(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTag(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTag_length(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTan(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTanh(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTarget(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncText(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsSpecial then
      Result := tkSpecial
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncText_only(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTitle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if IsQuali then
      Result := tkQualifier
    else
      Result := tkKey;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTo(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and (fRange = rsAdd) then
  begin
    Result := tkKey;
    fRange := rsUnKnown;
  end
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTop_line(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTotal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTransfer(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTrigger(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTrim(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncTsuppress(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsSpecial then
    Result := tkSpecial
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncUnload(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncUppercase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncUse_if(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncUser_key(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncUsing(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncUtilities(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncWait(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncWhile(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncWidth(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncWith(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) and IsQuali then
    Result := tkQualifier
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncWrite(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncWrite_line(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.FuncYesno_block(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkBlock
  else
    Result := tkIdentifier;
end;

function TSynDmlSyn.AltFunc(Index: TSynNativeInt): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

constructor TSynDmlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fFormAttri:= TSynHighlighterAttributes.Create(SYNS_AttrForm, SYNS_FriendlyAttrForm);
  fFormAttri.Style:= [fsBold];
  fFormAttri.Foreground:= clBlue;
  AddAttribute(fFormAttri);
  fBlockAttri:= TSynHighlighterAttributes.Create(SYNS_AttrBlock, SYNS_FriendlyAttrBlock);
  fBlockAttri.Style:= [fsBold];
  fBlockAttri.Foreground:= clGreen;
  AddAttribute(fBlockAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsBold];
  fCommentAttri.Foreground:= clRed;
  AddAttribute(fCommentAttri);
  fQualiAttri:= TSynHighlighterAttributes.Create(SYNS_AttrQualifier, SYNS_FriendlyAttrQualifier);
  fQualiAttri.Style:= [fsItalic];
  fQualiAttri.Foreground:= clGreen;
  AddAttribute(fQualiAttri);
  fFunctionAttri:= TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  fFunctionAttri.Style:= [fsItalic];
  fFunctionAttri.Foreground:= clBlack;
  AddAttribute(fFunctionAttri);
  fVariableAttri:= TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  fVariableAttri.Style:= [fsBold, fsItalic];
  fVariableAttri.Foreground:= clBlack;
  AddAttribute(fVariableAttri);
  fSpecialAttri:= TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable, SYNS_FriendlyAttrSpecialVariable);
  fSpecialAttri.Style:= [fsItalic];
  fSpecialAttri.Foreground:= clBlack;
  AddAttribute(fSpecialAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  fRange := rsUnknown;

  fDefaultFilter := SYNS_FilterGembase;
end;

procedure TSynDmlSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '@' then Inc(Run);
end;

procedure TSynDmlSyn.AsciiCharProc;

  function IsAsciiChar: Boolean;
  begin
     case FLine[Run] of
       '_', '0'..'9', 'A'..'Z', 'a'..'z':
         Result := True;
       else
         Result := False;
     end;
  end;

begin
  // variables...
  fTokenID := tkVariable;
  repeat
    Inc(Run);
  until not IsAsciiChar;
end;

procedure TSynDmlSyn.SymbolProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynDmlSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynDmlSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynDmlSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do Inc(Run);
end;

procedure TSynDmlSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynDmlSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run]= '=') or (fLine[Run]= '>') then Inc(Run);
end;

procedure TSynDmlSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynDmlSyn.NumberProc;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while CharInSet(FLine[Run], ['0'..'9', '.']) do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynDmlSyn.PointProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run]='.') or (fLine[Run]=')') then Inc(Run);
end;

procedure TSynDmlSyn.RemProc;
var
  p: PWideChar;
begin
  p := PWideChar(@fLine[Run - 1]);
  while p >= fLine do
  begin
    if not CharInSet(p^, [#9, #32]) then
    begin
      Inc(Run);
      fTokenID := tkSymbol;
      Exit;
    end;
    Dec(p);
  end;
  // it is a comment...
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynDmlSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  while (fLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynDmlSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then Inc(Run, 2);
  repeat
    Inc(Run);
  until (FLine[Run] = '"') or IsLineEnd(Run);

  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynDmlSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDmlSyn.Next;
begin
  fTokenPos := Run;
   case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32:
      SpaceProc;
    '#': AsciiCharProc;
    '"': StringProc;
    '0'..'9': NumberProc;
    'A'..'Z', 'a'..'z', '_':
      IdentProc;
    '{': SymbolProc;
    '}': SymbolProc;
    '!': RemProc;
    '.': PointProc;
    '<': LowerProc;
    '>': GreaterProc;
    '@': AddressOpProc;
    #39, '&', '('..'-', '/', ':', ';', '=', '?', '['..'^', '`', '~':
      SymbolProc;
  else
    UnknownProc;
  end;
  inherited;
end;

function TSynDmlSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDmlSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynDmlSyn.GetTokenID: TtkTokenKind;
begin
  Result:= fTokenId;
end;

function TSynDmlSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkForm: Result := fFormAttri;
    tkBlock: Result := fBlockAttri;
    tkKey: Result := fKeyAttri;
    tkComment: Result := fCommentAttri;
    tkQualifier: Result := fQualiAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkNumber: Result := fNumberAttri;
    tkSpecial: Result := fSpecialAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkVariable: Result := fVariableAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
    else Result := nil;
  end;
end;

function TSynDmlSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(GetTokenID);
end;

function TSynDmlSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynDmlSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynDmlSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

function TSynDmlSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterGembase;
end;

class function TSynDmlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGembase;
end;

class function TSynDmlSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangGembase;
end;

initialization
  RegisterPlaceableHighlighter(TSynDmlSyn);
end.
