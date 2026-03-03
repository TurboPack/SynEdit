{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterADSP21xx.pas, released 2000-04-17.
The Original Code is based on the wbADSP21xxSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Wynand Breytenbach.
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
-------------------------------------------------------------------------------}
{
@abstract(Provides a ADSP21xx highlighter for SynEdit)
@author(Wynand Breytenbach, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterADSP21xx unit provides a ADSP21xx DSP assembler highlighter for SynEdit.
}

unit SynHighlighterADSP21xx;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkCondition, tkIdentifier, tkKey, tkNull, tkNumber,
    tkRegister, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnKnown, rsPascalComment, rsCComment, rsHexNumber,
    rsBinaryNumber, rsInclude);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: NativeInt): TtkTokenKind of object;

  TSynADSP21xxSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fIdentFuncTable: array[0..820] of TIdentFuncTableFunc;
    FTokenID: TtkTokenKind;
    fNumberAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fRegisterAttri: TSynHighlighterAttributes;
    fConditionAttri: TSynHighlighterAttributes;
    fNullAttri: TSynHighlighterAttributes;
    fUnknownAttri: TSynHighlighterAttributes;
    function AltFunc(Index: NativeInt): TtkTokenKind;
    function FuncAbs(Index: NativeInt): TtkTokenKind;
    function FuncAbstract(Index: NativeInt): TtkTokenKind;
    function FuncAc(Index: NativeInt): TtkTokenKind;
    function FuncAf(Index: NativeInt): TtkTokenKind;
    function FuncAlt95reg(Index: NativeInt): TtkTokenKind;
    function FuncAnd(Index: NativeInt): TtkTokenKind;
    function FuncAr(Index: NativeInt): TtkTokenKind;
    function FuncAr95sat(Index: NativeInt): TtkTokenKind;
    function FuncAshift(Index: NativeInt): TtkTokenKind;
    function FuncAstat(Index: NativeInt): TtkTokenKind;
    function FuncAux(Index: NativeInt): TtkTokenKind;
    function FuncAv(Index: NativeInt): TtkTokenKind;
    function FuncAv95latch(Index: NativeInt): TtkTokenKind;
    function FuncAx0(Index: NativeInt): TtkTokenKind;
    function FuncAx1(Index: NativeInt): TtkTokenKind;
    function FuncAy0(Index: NativeInt): TtkTokenKind;
    function FuncAy1(Index: NativeInt): TtkTokenKind;
    function FuncB(Index: NativeInt): TtkTokenKind;
    function FuncBit95rev(Index: NativeInt): TtkTokenKind;
    function FuncBm(Index: NativeInt): TtkTokenKind;
    function FuncBoot(Index: NativeInt): TtkTokenKind;
    function FuncBy(Index: NativeInt): TtkTokenKind;
    function FuncCache(Index: NativeInt): TtkTokenKind;
    function FuncCall(Index: NativeInt): TtkTokenKind;
    function FuncCe(Index: NativeInt): TtkTokenKind;
    function FuncCirc(Index: NativeInt): TtkTokenKind;
    function FuncClear(Index: NativeInt): TtkTokenKind;
    function FuncClr(Index: NativeInt): TtkTokenKind;
    function FuncClrbit(Index: NativeInt): TtkTokenKind;
    function FuncCntl(Index: NativeInt): TtkTokenKind;
    function FuncCntr(Index: NativeInt): TtkTokenKind;
    function FuncConst(Index: NativeInt): TtkTokenKind;
    function FuncDefine(Index: NativeInt): TtkTokenKind;
    function FuncDis(Index: NativeInt): TtkTokenKind;
    function FuncDivq(Index: NativeInt): TtkTokenKind;
    function FuncDivs(Index: NativeInt): TtkTokenKind;
    function FuncDm(Index: NativeInt): TtkTokenKind;
    function FuncDmovlay(Index: NativeInt): TtkTokenKind;
    function FuncDo(Index: NativeInt): TtkTokenKind;
    function FuncElse(Index: NativeInt): TtkTokenKind;
    function FuncEmode(Index: NativeInt): TtkTokenKind;
    function FuncEna(Index: NativeInt): TtkTokenKind;
    function FuncEndif(Index: NativeInt): TtkTokenKind;
    function FuncEndmacro(Index: NativeInt): TtkTokenKind;
    function FuncEndmod(Index: NativeInt): TtkTokenKind;
    function FuncEntry(Index: NativeInt): TtkTokenKind;
    function FuncEq(Index: NativeInt): TtkTokenKind;
    function FuncExp(Index: NativeInt): TtkTokenKind;
    function FuncExpadj(Index: NativeInt): TtkTokenKind;
    function FuncExternal(Index: NativeInt): TtkTokenKind;
    function FuncFl0(Index: NativeInt): TtkTokenKind;
    function FuncFl1(Index: NativeInt): TtkTokenKind;
    function FuncFl2(Index: NativeInt): TtkTokenKind;
    function FuncFlag95in(Index: NativeInt): TtkTokenKind;
    function FuncFlag95out(Index: NativeInt): TtkTokenKind;
    function FuncFor(Index: NativeInt): TtkTokenKind;
    function FuncForever(Index: NativeInt): TtkTokenKind;
    function FuncGe(Index: NativeInt): TtkTokenKind;
    function FuncGlobal(Index: NativeInt): TtkTokenKind;
    function FuncGo95mode(Index: NativeInt): TtkTokenKind;
    function FuncGt(Index: NativeInt): TtkTokenKind;
    function FuncH(Index: NativeInt): TtkTokenKind;
    function FuncHi(Index: NativeInt): TtkTokenKind;
    function FuncI0(Index: NativeInt): TtkTokenKind;
    function FuncI1(Index: NativeInt): TtkTokenKind;
    function FuncI2(Index: NativeInt): TtkTokenKind;
    function FuncI3(Index: NativeInt): TtkTokenKind;
    function FuncI4(Index: NativeInt): TtkTokenKind;
    function FuncI5(Index: NativeInt): TtkTokenKind;
    function FuncI6(Index: NativeInt): TtkTokenKind;
    function FuncI7(Index: NativeInt): TtkTokenKind;
    function FuncIcntl(Index: NativeInt): TtkTokenKind;
    function FuncIdle(Index: NativeInt): TtkTokenKind;
    function FuncIf(Index: NativeInt): TtkTokenKind;
    function FuncIfc(Index: NativeInt): TtkTokenKind;
    function FuncIfdef(Index: NativeInt): TtkTokenKind;
    function FuncIfndef(Index: NativeInt): TtkTokenKind;
    function FuncImask(Index: NativeInt): TtkTokenKind;
    function FuncIn(Index: NativeInt): TtkTokenKind;
    function FuncInclude(Index: NativeInt): TtkTokenKind;
    function FuncInit(Index: NativeInt): TtkTokenKind;
    function FuncIo(Index: NativeInt): TtkTokenKind;
    function FuncJump(Index: NativeInt): TtkTokenKind;
    function FuncL0(Index: NativeInt): TtkTokenKind;
    function FuncL1(Index: NativeInt): TtkTokenKind;
    function FuncL2(Index: NativeInt): TtkTokenKind;
    function FuncL3(Index: NativeInt): TtkTokenKind;
    function FuncL4(Index: NativeInt): TtkTokenKind;
    function FuncL5(Index: NativeInt): TtkTokenKind;
    function FuncL6(Index: NativeInt): TtkTokenKind;
    function FuncL7(Index: NativeInt): TtkTokenKind;
    function FuncLe(Index: NativeInt): TtkTokenKind;
    function FuncLo(Index: NativeInt): TtkTokenKind;
    function FuncLocal(Index: NativeInt): TtkTokenKind;
    function FuncLoop(Index: NativeInt): TtkTokenKind;
    function FuncLshift(Index: NativeInt): TtkTokenKind;
    function FuncLt(Index: NativeInt): TtkTokenKind;
    function FuncM95mode(Index: NativeInt): TtkTokenKind;
    function FuncM0(Index: NativeInt): TtkTokenKind;
    function FuncM1(Index: NativeInt): TtkTokenKind;
    function FuncM2(Index: NativeInt): TtkTokenKind;
    function FuncM3(Index: NativeInt): TtkTokenKind;
    function FuncM4(Index: NativeInt): TtkTokenKind;
    function FuncM5(Index: NativeInt): TtkTokenKind;
    function FuncM6(Index: NativeInt): TtkTokenKind;
    function FuncM7(Index: NativeInt): TtkTokenKind;
    function FuncMacro(Index: NativeInt): TtkTokenKind;
    function FuncMf(Index: NativeInt): TtkTokenKind;
    function FuncModify(Index: NativeInt): TtkTokenKind;
    function FuncModule(Index: NativeInt): TtkTokenKind;
    function FuncMr(Index: NativeInt): TtkTokenKind;
    function FuncMr0(Index: NativeInt): TtkTokenKind;
    function FuncMr1(Index: NativeInt): TtkTokenKind;
    function FuncMr2(Index: NativeInt): TtkTokenKind;
    function FuncMstat(Index: NativeInt): TtkTokenKind;
    function FuncMv(Index: NativeInt): TtkTokenKind;
    function FuncMx0(Index: NativeInt): TtkTokenKind;
    function FuncMx1(Index: NativeInt): TtkTokenKind;
    function FuncMy0(Index: NativeInt): TtkTokenKind;
    function FuncMy1(Index: NativeInt): TtkTokenKind;
    function FuncName(Index: NativeInt): TtkTokenKind;
    function FuncNe(Index: NativeInt): TtkTokenKind;
    function FuncNeg(Index: NativeInt): TtkTokenKind;
    function FuncNewpage(Index: NativeInt): TtkTokenKind;
    function FuncNop(Index: NativeInt): TtkTokenKind;
    function FuncNorm(Index: NativeInt): TtkTokenKind;
    function FuncNot(Index: NativeInt): TtkTokenKind;
    function FuncOf(Index: NativeInt): TtkTokenKind;
    function FuncOr(Index: NativeInt): TtkTokenKind;
    function FuncPass(Index: NativeInt): TtkTokenKind;
    function FuncPc(Index: NativeInt): TtkTokenKind;
    function FuncPm(Index: NativeInt): TtkTokenKind;
    function FuncPop(Index: NativeInt): TtkTokenKind;
    function FuncPort(Index: NativeInt): TtkTokenKind;
    function FuncPush(Index: NativeInt): TtkTokenKind;
    function FuncRam(Index: NativeInt): TtkTokenKind;
    function FuncRegbank(Index: NativeInt): TtkTokenKind;
    function FuncReset(Index: NativeInt): TtkTokenKind;
    function FuncRnd(Index: NativeInt): TtkTokenKind;
    function FuncRom(Index: NativeInt): TtkTokenKind;
    function FuncRti(Index: NativeInt): TtkTokenKind;
    function FuncRts(Index: NativeInt): TtkTokenKind;
    function FuncRx0(Index: NativeInt): TtkTokenKind;
    function FuncRx1(Index: NativeInt): TtkTokenKind;
    function FuncSat(Index: NativeInt): TtkTokenKind;
    function FuncSb(Index: NativeInt): TtkTokenKind;
    function FuncSec95reg(Index: NativeInt): TtkTokenKind;
    function FuncSeg(Index: NativeInt): TtkTokenKind;
    function FuncSegment(Index: NativeInt): TtkTokenKind;
    function FuncSet(Index: NativeInt): TtkTokenKind;
    function FuncSetbit(Index: NativeInt): TtkTokenKind;
    function FuncShift(Index: NativeInt): TtkTokenKind;
    function FuncShl(Index: NativeInt): TtkTokenKind;
    function FuncShr(Index: NativeInt): TtkTokenKind;
    function FuncSi(Index: NativeInt): TtkTokenKind;
    function FuncSr(Index: NativeInt): TtkTokenKind;
    function FuncSr0(Index: NativeInt): TtkTokenKind;
    function FuncSr1(Index: NativeInt): TtkTokenKind;
    function FuncSs(Index: NativeInt): TtkTokenKind;
    function FuncSstat(Index: NativeInt): TtkTokenKind;
    function FuncStatic(Index: NativeInt): TtkTokenKind;
    function FuncSts(Index: NativeInt): TtkTokenKind;
    function FuncSu(Index: NativeInt): TtkTokenKind;
    function FuncTest(Index: NativeInt): TtkTokenKind;
    function FuncTestbit(Index: NativeInt): TtkTokenKind;
    function FuncTglbit(Index: NativeInt): TtkTokenKind;
    function FuncTimer(Index: NativeInt): TtkTokenKind;
    function FuncToggle(Index: NativeInt): TtkTokenKind;
    function FuncTopofpcstack(Index: NativeInt): TtkTokenKind;
    function FuncTrap(Index: NativeInt): TtkTokenKind;
    function FuncTrue(Index: NativeInt): TtkTokenKind;
    function FuncTx0(Index: NativeInt): TtkTokenKind;
    function FuncTx1(Index: NativeInt): TtkTokenKind;
    function FuncUndef(Index: NativeInt): TtkTokenKind;
    function FuncUntil(Index: NativeInt): TtkTokenKind;
    function FuncUs(Index: NativeInt): TtkTokenKind;
    function FuncUu(Index: NativeInt): TtkTokenKind;
    function FuncVar(Index: NativeInt): TtkTokenKind;
    function FuncXor(Index: NativeInt): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure PascalCommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CCommentProc;
    procedure CRProc;
    procedure ExclamationProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure IncludeCloseProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure BinaryNumber;
    procedure HexNumber;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
  protected
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
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
    function GetTokenKind: NativeInt; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: NativeInt): Boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property ConditionAttri: TSynHighlighterAttributes read fConditionAttri
      write fConditionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property RegisterAttri: TSynHighlighterAttributes read fRegisterAttri
      write fRegisterAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  Windows,
  Registry,
  SynEditStrConst,
  SynFunc;

const
  KeyWords: array[0..178] of string = (
    'abs', 'abstract', 'ac', 'af', 'alt_reg', 'and', 'ar', 'ar_sat', 'ashift',
    'astat', 'aux', 'av', 'av_latch', 'ax0', 'ax1', 'ay0', 'ay1', 'b',
    'bit_rev', 'bm', 'boot', 'by', 'cache', 'call', 'ce', 'circ', 'clear',
    'clr', 'clrbit', 'cntl', 'cntr', 'const', 'define', 'dis', 'divq', 'divs',
    'dm', 'dmovlay', 'do', 'else', 'emode', 'ena', 'endif', 'endmacro',
    'endmod', 'entry', 'eq', 'exp', 'expadj', 'external', 'fl0', 'fl1', 'fl2',
    'flag_in', 'flag_out', 'for', 'forever', 'ge', 'global', 'go_mode', 'gt',
    'h', 'hi', 'i0', 'i1', 'i2', 'i3', 'i4', 'i5', 'i6', 'i7', 'icntl', 'idle',
    'if', 'ifc', 'ifdef', 'ifndef', 'imask', 'in', 'include', 'init', 'io',
    'jump', 'l0', 'l1', 'l2', 'l3', 'l4', 'l5', 'l6', 'l7', 'le', 'lo', 'local',
    'loop', 'lshift', 'lt', 'm_mode', 'm0', 'm1', 'm2', 'm3', 'm4', 'm5', 'm6',
    'm7', 'macro', 'mf', 'modify', 'module', 'mr', 'mr0', 'mr1', 'mr2', 'mstat',
    'mv', 'mx0', 'mx1', 'my0', 'my1', 'name', 'ne', 'neg', 'newpage', 'nop',
    'norm', 'not', 'of', 'or', 'pass', 'pc', 'pm', 'pop', 'port', 'push', 'ram',
    'regbank', 'reset', 'rnd', 'rom', 'rti', 'rts', 'rx0', 'rx1', 'sat', 'sb',
    'sec_reg', 'seg', 'segment', 'set', 'setbit', 'shift', 'shl', 'shr', 'si', 
    'sr', 'sr0', 'sr1', 'ss', 'sstat', 'static', 'sts', 'su', 'test', 'testbit', 
    'tglbit', 'timer', 'toggle', 'topofpcstack', 'trap', 'true', 'tx0', 'tx1', 
    'undef', 'until', 'us', 'uu', 'var', 'xor' 
  );

  KeyIndices: array[0..820] of NativeInt = (
    -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, -1, 67, 
    15, -1, 48, 100, 132, -1, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, 
    152, 93, 155, -1, -1, -1, 70, 62, -1, -1, 103, 0, -1, -1, 10, -1, -1, -1, 
    -1, -1, -1, 171, -1, -1, -1, -1, 120, 162, -1, -1, -1, -1, -1, 82, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 153, -1, -1, -1, 50, 
    -1, -1, -1, -1, -1, -1, 72, 12, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 20, -1, -1, -1, 25, -1, -1, -1, 8, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 156, 83, -1, -1, -1, -1, -1, 77, 106, -1, 45, 27, 
    -1, -1, -1, -1, -1, 7, -1, -1, 43, -1, 74, 14, 174, 73, 86, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 56, -1, -1, -1, -1, 111, -1, -1, 140, -1, 
    -1, -1, 89, -1, -1, -1, -1, 127, -1, -1, -1, 28, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 116, -1, 49, -1, -1, 164, 23, -1, -1, 9, -1, -1, 
    -1, -1, 149, -1, -1, -1, 40, -1, -1, 46, -1, 94, -1, 81, -1, 134, -1, -1, 
    -1, -1, -1, -1, -1, 55, -1, 47, -1, -1, -1, -1, 11, -1, 135, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, -1, -1, -1, 65, 142, -1, 
    -1, 98, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 128, -1, -1, -1, -1, 
    -1, 18, -1, 68, 16, -1, -1, 101, 91, -1, -1, -1, 130, -1, 167, -1, -1, -1, 
    115, -1, -1, -1, -1, 19, 158, -1, 163, -1, -1, -1, -1, -1, 104, -1, -1, -1, 
    -1, -1, -1, -1, 39, -1, 79, 172, -1, -1, -1, -1, 41, -1, 38, 176, 80, -1, 
    -1, -1, 118, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, 
    75, -1, -1, 51, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 138, -1, -1, -1, -1, 
    -1, -1, 42, -1, -1, -1, -1, -1, -1, 58, -1, -1, 136, -1, -1, -1, -1, -1, -1, 
    177, -1, -1, -1, -1, -1, -1, -1, 57, -1, 157, 84, 21, -1, -1, -1, -1, -1, 1, 
    -1, -1, -1, 96, 161, -1, -1, 123, -1, -1, -1, -1, -1, -1, -1, -1, -1, 87, 
    -1, -1, -1, 54, 137, -1, -1, 124, 145, -1, -1, -1, -1, -1, -1, -1, -1, 112, 
    -1, -1, 173, -1, -1, -1, 90, -1, 125, -1, 166, -1, -1, -1, -1, 144, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1, 170, -1, -1, 
    35, -1, -1, -1, -1, -1, -1, -1, 148, -1, 44, -1, -1, -1, -1, 159, -1, -1, 
    -1, -1, -1, 150, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, 
    178, -1, -1, -1, 141, 60, -1, 17, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 66, 143, -1, -1, 99, -1, -1, 97, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 37, -1, -1, 26, -1, -1, 69, -1, -1, -1, 102, -1, -1, 121, -1, 
    -1, -1, 61, 129, 95, -1, -1, -1, 122, -1, 139, -1, -1, 36, 175, -1, -1, -1, 
    -1, -1, 105, -1, -1, -1, -1, -1, 108, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 32, -1, -1, -1, -1, -1, 119, -1, -1, -1, -1, -1, -1, 2, -1, -1, 165, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 52, -1, -1, -1, -1, -1, -1, 92, -1, 147, 
    -1, 131, 3, -1, 24, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 168, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, 13, -1, -1, 85, 59, 
    -1, -1, 146, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 88, -1, -1, 107, -1, -1, -1, -1, -1, -1, 160, -1, -1, -1, 
    -1, -1, -1, -1, 113, 151, -1, -1, -1, -1, 53, -1, -1, -1, -1, -1, 34, 29, 
    169, 126, 114, -1, -1, 22, -1, -1, -1, 6, -1, -1, -1, -1, -1, -1, -1, 78, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 154, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 76, -1, -1, -1, -1, -1, 5, 30, -1, -1, -1, -1, -1, -1, 
    64, -1, -1, -1, -1, -1, -1 
  );

{$Q-}
function TSynADSP21xxSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 641 + Ord(Str^) * 282;
    Inc(Str);
  end;
  Result := Result mod 821;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynADSP21xxSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynADSP21xxSyn.InitIdent;
var
  i: NativeInt;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[48] := FuncAbs;
  fIdentFuncTable[426] := FuncAbstract;
  fIdentFuncTable[642] := FuncAc;
  fIdentFuncTable[667] := FuncAf;
  fIdentFuncTable[693] := FuncAlt95reg;
  fIdentFuncTable[806] := FuncAnd;
  fIdentFuncTable[767] := FuncAr;
  fIdentFuncTable[153] := FuncAr95sat;
  fIdentFuncTable[126] := FuncAshift;
  fIdentFuncTable[220] := FuncAstat;
  fIdentFuncTable[51] := FuncAux;
  fIdentFuncTable[253] := FuncAv;
  fIdentFuncTable[99] := FuncAv95latch;
  fIdentFuncTable[698] := FuncAx0;
  fIdentFuncTable[159] := FuncAx1;
  fIdentFuncTable[19] := FuncAy0;
  fIdentFuncTable[301] := FuncAy1;
  fIdentFuncTable[543] := FuncB;
  fIdentFuncTable[298] := FuncBit95rev;
  fIdentFuncTable[320] := FuncBm;
  fIdentFuncTable[118] := FuncBoot;
  fIdentFuncTable[420] := FuncBy;
  fIdentFuncTable[763] := FuncCache;
  fIdentFuncTable[217] := FuncCall;
  fIdentFuncTable[669] := FuncCe;
  fIdentFuncTable[122] := FuncCirc;
  fIdentFuncTable[579] := FuncClear;
  fIdentFuncTable[147] := FuncClr;
  fIdentFuncTable[196] := FuncClrbit;
  fIdentFuncTable[757] := FuncCntl;
  fIdentFuncTable[807] := FuncCntr;
  fIdentFuncTable[525] := FuncConst;
  fIdentFuncTable[629] := FuncDefine;
  fIdentFuncTable[715] := FuncDis;
  fIdentFuncTable[756] := FuncDivq;
  fIdentFuncTable[499] := FuncDivs;
  fIdentFuncTable[604] := FuncDm;
  fIdentFuncTable[576] := FuncDmovlay;
  fIdentFuncTable[347] := FuncDo;
  fIdentFuncTable[337] := FuncElse;
  fIdentFuncTable[229] := FuncEmode;
  fIdentFuncTable[345] := FuncEna;
  fIdentFuncTable[391] := FuncEndif;
  fIdentFuncTable[156] := FuncEndmacro;
  fIdentFuncTable[509] := FuncEndmod;
  fIdentFuncTable[146] := FuncEntry;
  fIdentFuncTable[232] := FuncEq;
  fIdentFuncTable[248] := FuncExp;
  fIdentFuncTable[21] := FuncExpadj;
  fIdentFuncTable[213] := FuncExternal;
  fIdentFuncTable[91] := FuncFl0;
  fIdentFuncTable[373] := FuncFl1;
  fIdentFuncTable[655] := FuncFl2;
  fIdentFuncTable[750] := FuncFlag95in;
  fIdentFuncTable[448] := FuncFlag95out;
  fIdentFuncTable[246] := FuncFor;
  fIdentFuncTable[175] := FuncForever;
  fIdentFuncTable[416] := FuncGe;
  fIdentFuncTable[398] := FuncGlobal;
  fIdentFuncTable[702] := FuncGo95mode;
  fIdentFuncTable[541] := FuncGt;
  fIdentFuncTable[593] := FuncH;
  fIdentFuncTable[44] := FuncHi;
  fIdentFuncTable[532] := FuncI0;
  fIdentFuncTable[814] := FuncI1;
  fIdentFuncTable[275] := FuncI2;
  fIdentFuncTable[557] := FuncI3;
  fIdentFuncTable[18] := FuncI4;
  fIdentFuncTable[300] := FuncI5;
  fIdentFuncTable[582] := FuncI6;
  fIdentFuncTable[43] := FuncI7;
  fIdentFuncTable[369] := FuncIcntl;
  fIdentFuncTable[98] := FuncIdle;
  fIdentFuncTable[161] := FuncIf;
  fIdentFuncTable[158] := FuncIfc;
  fIdentFuncTable[370] := FuncIfdef;
  fIdentFuncTable[800] := FuncIfndef;
  fIdentFuncTable[143] := FuncImask;
  fIdentFuncTable[775] := FuncIn;
  fIdentFuncTable[339] := FuncInclude;
  fIdentFuncTable[349] := FuncInit;
  fIdentFuncTable[236] := FuncIo;
  fIdentFuncTable[70] := FuncJump;
  fIdentFuncTable[137] := FuncL0;
  fIdentFuncTable[419] := FuncL1;
  fIdentFuncTable[701] := FuncL2;
  fIdentFuncTable[162] := FuncL3;
  fIdentFuncTable[444] := FuncL4;
  fIdentFuncTable[726] := FuncL5;
  fIdentFuncTable[187] := FuncL6;
  fIdentFuncTable[469] := FuncL7;
  fIdentFuncTable[305] := FuncLe;
  fIdentFuncTable[662] := FuncLo;
  fIdentFuncTable[38] := FuncLocal;
  fIdentFuncTable[234] := FuncLoop;
  fIdentFuncTable[595] := FuncLshift;
  fIdentFuncTable[430] := FuncLt;
  fIdentFuncTable[564] := FuncM95mode;
  fIdentFuncTable[279] := FuncM0;
  fIdentFuncTable[561] := FuncM1;
  fIdentFuncTable[22] := FuncM2;
  fIdentFuncTable[304] := FuncM3;
  fIdentFuncTable[586] := FuncM4;
  fIdentFuncTable[47] := FuncM5;
  fIdentFuncTable[329] := FuncM6;
  fIdentFuncTable[611] := FuncM7;
  fIdentFuncTable[144] := FuncMacro;
  fIdentFuncTable[729] := FuncMf;
  fIdentFuncTable[617] := FuncModify;
  fIdentFuncTable[268] := FuncModule;
  fIdentFuncTable[8] := FuncMr;
  fIdentFuncTable[180] := FuncMr0;
  fIdentFuncTable[462] := FuncMr1;
  fIdentFuncTable[744] := FuncMr2;
  fIdentFuncTable[760] := FuncMstat;
  fIdentFuncTable[315] := FuncMv;
  fIdentFuncTable[211] := FuncMx0;
  fIdentFuncTable[493] := FuncMx1;
  fIdentFuncTable[353] := FuncMy0;
  fIdentFuncTable[635] := FuncMy1;
  fIdentFuncTable[63] := FuncName;
  fIdentFuncTable[589] := FuncNe;
  fIdentFuncTable[599] := FuncNeg;
  fIdentFuncTable[434] := FuncNewpage;
  fIdentFuncTable[452] := FuncNop;
  fIdentFuncTable[471] := FuncNorm;
  fIdentFuncTable[759] := FuncNot;
  fIdentFuncTable[192] := FuncOf;
  fIdentFuncTable[292] := FuncOr;
  fIdentFuncTable[594] := FuncPass;
  fIdentFuncTable[309] := FuncPc;
  fIdentFuncTable[666] := FuncPm;
  fIdentFuncTable[23] := FuncPop;
  fIdentFuncTable[29] := FuncPort;
  fIdentFuncTable[238] := FuncPush;
  fIdentFuncTable[255] := FuncRam;
  fIdentFuncTable[401] := FuncRegbank;
  fIdentFuncTable[449] := FuncReset;
  fIdentFuncTable[384] := FuncRnd;
  fIdentFuncTable[601] := FuncRom;
  fIdentFuncTable[183] := FuncRti;
  fIdentFuncTable[540] := FuncRts;
  fIdentFuncTable[276] := FuncRx0;
  fIdentFuncTable[558] := FuncRx1;
  fIdentFuncTable[478] := FuncSat;
  fIdentFuncTable[453] := FuncSb;
  fIdentFuncTable[705] := FuncSec95reg;
  fIdentFuncTable[664] := FuncSeg;
  fIdentFuncTable[507] := FuncSegment;
  fIdentFuncTable[225] := FuncSet;
  fIdentFuncTable[520] := FuncSetbit;
  fIdentFuncTable[745] := FuncShift;
  fIdentFuncTable[37] := FuncShl;
  fIdentFuncTable[87] := FuncShr;
  fIdentFuncTable[785] := FuncSi;
  fIdentFuncTable[39] := FuncSr;
  fIdentFuncTable[136] := FuncSr0;
  fIdentFuncTable[418] := FuncSr1;
  fIdentFuncTable[321] := FuncSs;
  fIdentFuncTable[514] := FuncSstat;
  fIdentFuncTable[736] := FuncStatic;
  fIdentFuncTable[431] := FuncSts;
  fIdentFuncTable[64] := FuncSu;
  fIdentFuncTable[323] := FuncTest;
  fIdentFuncTable[216] := FuncTestbit;
  fIdentFuncTable[645] := FuncTglbit;
  fIdentFuncTable[473] := FuncTimer;
  fIdentFuncTable[311] := FuncToggle;
  fIdentFuncTable[683] := FuncTopofpcstack;
  fIdentFuncTable[758] := FuncTrap;
  fIdentFuncTable[496] := FuncTrue;
  fIdentFuncTable[58] := FuncTx0;
  fIdentFuncTable[340] := FuncTx1;
  fIdentFuncTable[465] := FuncUndef;
  fIdentFuncTable[160] := FuncUntil;
  fIdentFuncTable[605] := FuncUs;
  fIdentFuncTable[348] := FuncUu;
  fIdentFuncTable[408] := FuncVar;
  fIdentFuncTable[536] := FuncXor;
end;

function TSynADSP21xxSyn.AltFunc(Index: NativeInt): TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynADSP21xxSyn.FuncAbs(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAbstract(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAc(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAf(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAlt95reg(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAnd(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAr(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAr95sat(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAshift(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAstat(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAux(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAv(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAv95latch(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAx0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAx1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAy0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncAy1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncB(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if FLine[Run + 1] = '#' then
    begin
      Result := tkNumber;
      fRange := rsBinaryNumber;
    end
    else
    begin
      Result := tkIdentifier;
    end
  end
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncBit95rev(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncBm(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncBoot(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncBy(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncCache(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncCall(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncCe(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncCirc(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncClear(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncClr(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncClrbit(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncCntl(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncCntr(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncConst(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncDefine(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncDis(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncDivq(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncDivs(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncDm(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncDmovlay(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncDo(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncElse(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncEmode(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncEna(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncEndif(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncEndmacro(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncEndmod(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncEntry(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncEq(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncExp(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncExpadj(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncExternal(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncFl0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncFl1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncFl2(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncFlag95in(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncFlag95out(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncFor(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncForever(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncGe(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncGlobal(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncGo95mode(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncGt(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncH(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    if FLine[Run + 1] = '#' then
    begin
      Result := tkNumber;
      fRange := rsHexNumber;
    end
    else
    begin
      Result := tkIdentifier;
    end
  end
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncHi(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI2(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI3(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI4(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI5(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI6(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncI7(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIcntl(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIdle(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIf(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIfc(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIfdef(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIfndef(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncImask(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIn(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncInclude(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncInit(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncIo(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncJump(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL2(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL3(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL4(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL5(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL6(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncL7(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncLe(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncLo(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncLocal(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncLoop(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncLshift(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncLt(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM95mode(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM2(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM3(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM4(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM5(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM6(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncM7(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMacro(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMf(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncModify(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncModule(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMr(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMr0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMr1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMr2(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMstat(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMv(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMx0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMx1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMy0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncMy1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncName(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncNe(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncNeg(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncNewpage(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncNop(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncNorm(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncNot(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncOf(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncOr(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncPass(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncPc(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncPm(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncPop(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncPort(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncPush(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRam(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRegbank(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncReset(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRnd(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRom(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRti(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRts(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRx0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncRx1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSat(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSb(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSec95reg(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSeg(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSegment(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSet(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSetbit(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncShift(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncShl(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncShr(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSi(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSr(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSr0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSr1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSs(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSstat(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncStatic(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSts(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncSu(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTest(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTestbit(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTglbit(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTimer(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncToggle(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTopofpcstack(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTrap(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTrue(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTx0(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncTx1(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkRegister
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncUndef(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncUntil(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncUs(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncUu(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkCondition
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncVar(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynADSP21xxSyn.FuncXor(Index: NativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynADSP21xxSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.ForeGround := clTeal;
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.ForeGround := clOlive;
  AddAttribute(fNumberAttri);

  fRegisterAttri := TSynHighlighterAttributes.Create(SYNS_AttrRegister, SYNS_FriendlyAttrRegister);
  fRegisterAttri.ForeGround := clBlue;
  AddAttribute(fRegisterAttri);

  fConditionAttri := TSynHighlighterAttributes.Create(SYNS_AttrCondition, SYNS_FriendlyAttrCondition);
  fConditionAttri.ForeGround := clFuchsia;
  AddAttribute(fConditionAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);

  fNullAttri := TSynHighlighterAttributes.Create(SYNS_AttrNull, SYNS_FriendlyAttrNull);
  AddAttribute(fNullAttri);

  fUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknownWord, SYNS_FriendlyAttrUnknownWord);
  AddAttribute(fUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterADSP21xx;
end;

procedure TSynADSP21xxSyn.BraceCloseProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynADSP21xxSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynADSP21xxSyn.PascalCommentProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '}':
        begin
          fRange := rsUnKnown;
          Inc(Run);
          Break;
        end;
      #10: Break;
      #13: Break;
      else Inc(Run);
    end;
end;

procedure TSynADSP21xxSyn.CCommentProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0: begin
          NullProc;
          Exit;
        end;
    #10:begin
         LFProc;
         Exit;
        end;
    #13:begin
          CRProc;
          Exit;
        end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        begin
          if FLine[Run+1] = '/' then
          begin
            fRange := rsUnknown;
            Inc(Run, 2);
            Break;
          end
          else
            Inc(Run);
        end;
      #10: Break;
      #13: Break;
      else Inc(Run);
    end;
end;

procedure TSynADSP21xxSyn.BraceOpenProc;
begin
  fTokenID := tkComment;
  fRange := rsPascalComment;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '}':
        begin
          fRange := rsUnKnown;
          Inc(Run);
          Break;
        end;
      #10: Break;
      #13: Break;
    else Inc(Run);
    end;
end;


procedure TSynADSP21xxSyn.IncludeCloseProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynADSP21xxSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
end;

procedure TSynADSP21xxSyn.ExclamationProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynADSP21xxSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynADSP21xxSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsIntegerChar do Inc(Run);
end;

procedure TSynADSP21xxSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynADSP21xxSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynADSP21xxSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', 'x', 'X', '.':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynADSP21xxSyn.HexNumber;

  function IsHexChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  fRange := rsUnKnown;
  while IsHexChar do
  begin
    Inc(Run);
  end;
end;

procedure TSynADSP21xxSyn.BinaryNumber;
begin
  Inc(Run);
  fRange := rsUnKnown;
  while CharInSet(FLine[Run], ['0'..'1']) do
  begin
    Inc(Run);
  end;
  if CharInSet(FLine[Run], ['2'..'9', 'A'..'F', 'a'..'f']) then
  begin
    fTokenID := tkIdentifier
  end
  else
    fTokenID := tkNumber;
end;

procedure TSynADSP21xxSyn.SlashProc;
begin
  if FLine[Run + 1] = '*' then
  begin
    fTokenID := tkComment;
    fRange := rsCComment;
    Inc(Run, 2);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        '*':  begin
                if FLine[Run+1] = '/' then
                begin
                  Inc(Run, 2);
                  fRange := rsUnknown;
                  Break;
                end
                else Inc(Run);
              end;
        #10: Break;
        #13: Break;
        else Inc(Run);
      end;
    end
  else
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynADSP21xxSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynADSP21xxSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynADSP21xxSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsPascalComment: PascalCommentProc;
    rsCComment: CCommentProc;
    rsHexNumber: HexNumber;
    rsBinaryNumber: BinaryNumber;
  else
    fRange := rsUnknown;
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '$': IntegerProc;
      #39: StringProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '{': BraceOpenProc;
      '}': BraceCloseProc;
      '/': SlashProc;
      '>': IncludeCloseProc;
      '!': ExclamationProc;
      else UnknownProc;
    end;
  end;
  inherited;
end;

function TSynADSP21xxSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynADSP21xxSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynADSP21xxSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynADSP21xxSyn.GetTokenKind: NativeInt;
begin
  Result := Ord(GetTokenID);
end;

function TSynADSP21xxSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkRegister: Result := fRegisterAttri;
    tkCondition: Result := fConditionAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynADSP21xxSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynADSP21xxSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynADSP21xxSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynADSP21xxSyn.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  with TRegistry.Create do
  begin
    try
      RootKey := HKEY_CURRENT_USER;
      // we need some method to make the following statement more universal!
      if OpenKeyReadOnly('\SOFTWARE\Wynand\DSPIDE\1.0') then
      begin
        try
          GetKeyNames(settings);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TSynADSP21xxSyn.UseUserSettings(settingIndex: NativeInt): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

    function ReadDspIDESetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
    begin
      try
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Wynand\DspIDE\1.0\Editor\Highlight',key, False);
      except
        Result := False;
      end;
    end;
var
  tmpNumberAttri    : TSynHighlighterAttributes;
  tmpKeyAttri       : TSynHighlighterAttributes;
  tmpSymbolAttri    : TSynHighlighterAttributes;
  tmpCommentAttri   : TSynHighlighterAttributes;
  tmpConditionAttri : TSynHighlighterAttributes;
  tmpIdentifierAttri: TSynHighlighterAttributes;
  tmpSpaceAttri     : TSynHighlighterAttributes;
  tmpRegisterAttri  : TSynHighlighterAttributes;
  StrLst            : TStringList;

begin  // UseUserSettings
  StrLst := TStringList.Create;
  try
    EnumUserSettings(StrLst);
    if settingIndex >= StrLst.Count then
      Result := False
    else
    begin
      tmpNumberAttri    := TSynHighlighterAttributes.Create('', '');
      tmpKeyAttri       := TSynHighlighterAttributes.Create('', '');
      tmpSymbolAttri    := TSynHighlighterAttributes.Create('', '');
      tmpCommentAttri   := TSynHighlighterAttributes.Create('', '');
      tmpConditionAttri := TSynHighlighterAttributes.Create('', '');
      tmpIdentifierAttri:= TSynHighlighterAttributes.Create('', '');
      tmpSpaceAttri     := TSynHighlighterAttributes.Create('', '');
      tmpRegisterAttri  := TSynHighlighterAttributes.Create('', '');

      tmpNumberAttri    .Assign(fNumberAttri);
      tmpKeyAttri       .Assign(fKeyAttri);
      tmpSymbolAttri    .Assign(fSymbolAttri);
      tmpCommentAttri   .Assign(fCommentAttri);
      tmpConditionAttri .Assign(fConditionAttri);
      tmpIdentifierAttri.Assign(fIdentifierAttri);
      tmpSpaceAttri     .Assign(fSpaceAttri);
      tmpRegisterAttri  .Assign(fRegisterAttri);
      Result := ReadDspIDESetting(StrLst.GetItem(settingIndex),fCommentAttri,'Comment')       and
                ReadDspIDESetting(StrLst.GetItem(settingIndex),fIdentifierAttri,'Identifier') and
                ReadDspIDESetting(StrLst.GetItem(settingIndex),fKeyAttri,'Reserved word')     and
                ReadDspIDESetting(StrLst.GetItem(settingIndex),fNumberAttri,'BinaryNumber')   and
                ReadDspIDESetting(StrLst.GetItem(settingIndex),fSpaceAttri,'Whitespace')      and
                ReadDspIDESetting(StrLst.GetItem(settingIndex),fSymbolAttri,'Symbol')         and
                ReadDspIDESetting(StrLst.GetItem(settingIndex),fConditionAttri,'Condition')   and
                ReadDspIDESetting(StrLst.GetItem(settingIndex),fRegisterAttri,'Symbol');
      if not Result then
      begin
        fNumberAttri     .Assign(tmpNumberAttri);
        fKeyAttri        .Assign(tmpKeyAttri);
        fSymbolAttri     .Assign(tmpSymbolAttri);
        fCommentAttri    .Assign(tmpCommentAttri);
        fConditionAttri  .Assign(tmpConditionAttri);
        fIdentifierAttri .Assign(tmpIdentifierAttri);
        fSpaceAttri      .Assign(tmpSpaceAttri);
        fConditionAttri  .Assign(tmpConditionAttri);
        fRegisterAttri   .Assign(tmpRegisterAttri);
      end;
      tmpNumberAttri    .Free;
      tmpKeyAttri       .Free;
      tmpSymbolAttri    .Free;
      tmpCommentAttri   .Free;
      tmpConditionAttri .Free;
      tmpIdentifierAttri.Free;
      tmpSpaceAttri     .Free;
      tmpRegisterAttri  .Free;
    end;
  finally StrLst.Free; end;
end;

function TSynADSP21xxSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterADSP21xx;
end;

class function TSynADSP21xxSyn.GetLanguageName: string;
begin
  Result := SYNS_LangADSP21xx;
end;

class function TSynADSP21xxSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

class function TSynADSP21xxSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangADSP21xx;
end;

initialization
  RegisterPlaceableHighlighter(TSynADSP21xxSyn);
end.
