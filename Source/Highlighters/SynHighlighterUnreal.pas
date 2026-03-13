{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
                                          PP - 2001/10/24:
The Original Code is based on the UnrealSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Dean Harmon.
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
@abstract(Provides a Unreal syntax highlighter for SynEdit)
@author(Dean Harmon)
@created(2000)
@lastmod(2001-06-29)
}

unit SynHighlighterUnreal;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Registry,
  Windows, // registry constants
  SynEditHighlighter,
  SynEditTypes,
  SynFunc,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkDirective,
    tkIdentifier,
    tkKey,
    tkKey2,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkString2,
    tkSymbol,
    tkUnknown);

  TxtkTokenKind = (
    xtkAdd, xtkAddAssign, xtkAnd, xtkAndAssign, xtkArrow, xtkAssign,
    xtkBitComplement, xtkBraceClose, xtkBraceOpen, xtkColon, xtkComma,
    xtkDecrement, xtkDivide, xtkDivideAssign, xtkEllipse, xtkGreaterThan,
    xtkGreaterThanEqual, xtkIncOr, xtkIncOrAssign, xtkIncrement, xtkLessThan,
    xtkLessThanEqual, xtkLogAnd, xtkLogComplement, xtkLogEqual, xtkLogOr,
    xtkMod, xtkModAssign, xtkMultiplyAssign, xtkNotEqual, xtkPoint, xtkQuestion,
    xtkRoundClose, xtkRoundOpen, xtkScopeResolution, xtkSemiColon, xtkShiftLeft,
    xtkShiftLeftAssign, xtkShiftRight, xtkShiftRightAssign, xtkSquareClose,
    xtkSquareOpen, xtkStar, xtkSubtract, xtkSubtractAssign, xtkXor,
    xtkXorAssign);

  TRangeState = (rsANil, rsAnsiC, rsDirective, rsDirectiveComment, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: TSynNativeInt): TtkTokenKind of object;

  TSynUnrealSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    FRoundCount: TSynNativeInt;
    FSquareCount: TSynNativeInt;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fIdentFuncTable: array[0..732] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fKey2Attri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fString2Attri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: TSynNativeInt): TtkTokenKind;
    function FuncAbstract(Index: TSynNativeInt): TtkTokenKind;
    function FuncAlways(Index: TSynNativeInt): TtkTokenKind;
    function FuncArray(Index: TSynNativeInt): TtkTokenKind;
    function FuncArraycount(Index: TSynNativeInt): TtkTokenKind;
    function FuncAssert(Index: TSynNativeInt): TtkTokenKind;
    function FuncAuto(Index: TSynNativeInt): TtkTokenKind;
    function FuncAutomated(Index: TSynNativeInt): TtkTokenKind;
    function FuncBool(Index: TSynNativeInt): TtkTokenKind;
    function FuncBoundingbox(Index: TSynNativeInt): TtkTokenKind;
    function FuncBoundingvolume(Index: TSynNativeInt): TtkTokenKind;
    function FuncBreak(Index: TSynNativeInt): TtkTokenKind;
    function FuncButton(Index: TSynNativeInt): TtkTokenKind;
    function FuncByte(Index: TSynNativeInt): TtkTokenKind;
    function FuncCache(Index: TSynNativeInt): TtkTokenKind;
    function FuncCacheexempt(Index: TSynNativeInt): TtkTokenKind;
    function FuncCase(Index: TSynNativeInt): TtkTokenKind;
    function FuncCatch(Index: TSynNativeInt): TtkTokenKind;
    function FuncClass(Index: TSynNativeInt): TtkTokenKind;
    function FuncCoerce(Index: TSynNativeInt): TtkTokenKind;
    function FuncCollapsecategories(Index: TSynNativeInt): TtkTokenKind;
    function FuncColor(Index: TSynNativeInt): TtkTokenKind;
    function FuncConfig(Index: TSynNativeInt): TtkTokenKind;
    function FuncConst(Index: TSynNativeInt): TtkTokenKind;
    function FuncContinue(Index: TSynNativeInt): TtkTokenKind;
    function FuncCoords(Index: TSynNativeInt): TtkTokenKind;
    function FuncCpptext(Index: TSynNativeInt): TtkTokenKind;
    function FuncCross(Index: TSynNativeInt): TtkTokenKind;
    function FuncDefault(Index: TSynNativeInt): TtkTokenKind;
    function FuncDefaultproperties(Index: TSynNativeInt): TtkTokenKind;
    function FuncDelegate(Index: TSynNativeInt): TtkTokenKind;
    function FuncDelete(Index: TSynNativeInt): TtkTokenKind;
    function FuncDependson(Index: TSynNativeInt): TtkTokenKind;
    function FuncDeprecated(Index: TSynNativeInt): TtkTokenKind;
    function FuncDo(Index: TSynNativeInt): TtkTokenKind;
    function FuncDontcollapsecategories(Index: TSynNativeInt): TtkTokenKind;
    function FuncDot(Index: TSynNativeInt): TtkTokenKind;
    function FuncEach(Index: TSynNativeInt): TtkTokenKind;
    function FuncEdfindable(Index: TSynNativeInt): TtkTokenKind;
    function FuncEditconst(Index: TSynNativeInt): TtkTokenKind;
    function FuncEditconstarray(Index: TSynNativeInt): TtkTokenKind;
    function FuncEditinline(Index: TSynNativeInt): TtkTokenKind;
    function FuncEditinlinenew(Index: TSynNativeInt): TtkTokenKind;
    function FuncEditinlinenotify(Index: TSynNativeInt): TtkTokenKind;
    function FuncEditinlineuse(Index: TSynNativeInt): TtkTokenKind;
    function FuncElse(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnum(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnumcount(Index: TSynNativeInt): TtkTokenKind;
    function FuncEvent(Index: TSynNativeInt): TtkTokenKind;
    function FuncExec(Index: TSynNativeInt): TtkTokenKind;
    function FuncExpands(Index: TSynNativeInt): TtkTokenKind;
    function FuncExplicit(Index: TSynNativeInt): TtkTokenKind;
    function FuncExport(Index: TSynNativeInt): TtkTokenKind;
    function FuncExportstructs(Index: TSynNativeInt): TtkTokenKind;
    function FuncExtends(Index: TSynNativeInt): TtkTokenKind;
    function FuncFalse(Index: TSynNativeInt): TtkTokenKind;
    function FuncFinal(Index: TSynNativeInt): TtkTokenKind;
    function FuncFloat(Index: TSynNativeInt): TtkTokenKind;
    function FuncFor(Index: TSynNativeInt): TtkTokenKind;
    function FuncForeach(Index: TSynNativeInt): TtkTokenKind;
    function FuncFunction(Index: TSynNativeInt): TtkTokenKind;
    function FuncGlobal(Index: TSynNativeInt): TtkTokenKind;
    function FuncGlobalconfig(Index: TSynNativeInt): TtkTokenKind;
    function FuncGoto(Index: TSynNativeInt): TtkTokenKind;
    function FuncGuid(Index: TSynNativeInt): TtkTokenKind;
    function FuncHidecategories(Index: TSynNativeInt): TtkTokenKind;
    function FuncHidedropdown(Index: TSynNativeInt): TtkTokenKind;
    function FuncHideparent(Index: TSynNativeInt): TtkTokenKind;
    function FuncIf(Index: TSynNativeInt): TtkTokenKind;
    function FuncIgnores(Index: TSynNativeInt): TtkTokenKind;
    function FuncImport(Index: TSynNativeInt): TtkTokenKind;
    function FuncInit(Index: TSynNativeInt): TtkTokenKind;
    function FuncInput(Index: TSynNativeInt): TtkTokenKind;
    function FuncInsert(Index: TSynNativeInt): TtkTokenKind;
    function FuncInstanced(Index: TSynNativeInt): TtkTokenKind;
    function FuncInt(Index: TSynNativeInt): TtkTokenKind;
    function FuncIntrinsic(Index: TSynNativeInt): TtkTokenKind;
    function FuncInvariant(Index: TSynNativeInt): TtkTokenKind;
    function FuncIterator(Index: TSynNativeInt): TtkTokenKind;
    function FuncLatent(Index: TSynNativeInt): TtkTokenKind;
    function FuncLength(Index: TSynNativeInt): TtkTokenKind;
    function FuncLocal(Index: TSynNativeInt): TtkTokenKind;
    function FuncLocalized(Index: TSynNativeInt): TtkTokenKind;
    function FuncLong(Index: TSynNativeInt): TtkTokenKind;
    function FuncMesh(Index: TSynNativeInt): TtkTokenKind;
    function FuncModel(Index: TSynNativeInt): TtkTokenKind;
    function FuncMutable(Index: TSynNativeInt): TtkTokenKind;
    function FuncName(Index: TSynNativeInt): TtkTokenKind;
    function FuncNative(Index: TSynNativeInt): TtkTokenKind;
    function FuncNativereplication(Index: TSynNativeInt): TtkTokenKind;
    function FuncNew(Index: TSynNativeInt): TtkTokenKind;
    function FuncNoexport(Index: TSynNativeInt): TtkTokenKind;
    function FuncNone(Index: TSynNativeInt): TtkTokenKind;
    function FuncNoteditinlinenew(Index: TSynNativeInt): TtkTokenKind;
    function FuncNotplaceable(Index: TSynNativeInt): TtkTokenKind;
    function FuncNousercreate(Index: TSynNativeInt): TtkTokenKind;
    function FuncOperator(Index: TSynNativeInt): TtkTokenKind;
    function FuncOptional(Index: TSynNativeInt): TtkTokenKind;
    function FuncOut(Index: TSynNativeInt): TtkTokenKind;
    function FuncParseconfig(Index: TSynNativeInt): TtkTokenKind;
    function FuncPerobjectconfig(Index: TSynNativeInt): TtkTokenKind;
    function FuncPlaceable(Index: TSynNativeInt): TtkTokenKind;
    function FuncPlane(Index: TSynNativeInt): TtkTokenKind;
    function FuncPointer(Index: TSynNativeInt): TtkTokenKind;
    function FuncPostoperator(Index: TSynNativeInt): TtkTokenKind;
    function FuncPreoperator(Index: TSynNativeInt): TtkTokenKind;
    function FuncPrivate(Index: TSynNativeInt): TtkTokenKind;
    function FuncProtected(Index: TSynNativeInt): TtkTokenKind;
    function FuncRegister(Index: TSynNativeInt): TtkTokenKind;
    function FuncReliable(Index: TSynNativeInt): TtkTokenKind;
    function FuncRemove(Index: TSynNativeInt): TtkTokenKind;
    function FuncReplication(Index: TSynNativeInt): TtkTokenKind;
    function FuncReturn(Index: TSynNativeInt): TtkTokenKind;
    function FuncRng(Index: TSynNativeInt): TtkTokenKind;
    function FuncRot(Index: TSynNativeInt): TtkTokenKind;
    function FuncRotator(Index: TSynNativeInt): TtkTokenKind;
    function FuncSafereplace(Index: TSynNativeInt): TtkTokenKind;
    function FuncScale(Index: TSynNativeInt): TtkTokenKind;
    function FuncScriptconst(Index: TSynNativeInt): TtkTokenKind;
    function FuncSelf(Index: TSynNativeInt): TtkTokenKind;
    function FuncShowcategories(Index: TSynNativeInt): TtkTokenKind;
    function FuncSimulated(Index: TSynNativeInt): TtkTokenKind;
    function FuncSingular(Index: TSynNativeInt): TtkTokenKind;
    function FuncSkip(Index: TSynNativeInt): TtkTokenKind;
    function FuncSound(Index: TSynNativeInt): TtkTokenKind;
    function FuncState(Index: TSynNativeInt): TtkTokenKind;
    function FuncStatic(Index: TSynNativeInt): TtkTokenKind;
    function FuncStop(Index: TSynNativeInt): TtkTokenKind;
    function FuncString(Index: TSynNativeInt): TtkTokenKind;
    function FuncStruct(Index: TSynNativeInt): TtkTokenKind;
    function FuncSuper(Index: TSynNativeInt): TtkTokenKind;
    function FuncSwitch(Index: TSynNativeInt): TtkTokenKind;
    function FuncTexture(Index: TSynNativeInt): TtkTokenKind;
    function FuncTransient(Index: TSynNativeInt): TtkTokenKind;
    function FuncTravel(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrue(Index: TSynNativeInt): TtkTokenKind;
    function FuncUnreliable(Index: TSynNativeInt): TtkTokenKind;
    function FuncUntil(Index: TSynNativeInt): TtkTokenKind;
    function FuncVar(Index: TSynNativeInt): TtkTokenKind;
    function FuncVect(Index: TSynNativeInt): TtkTokenKind;
    function FuncVector(Index: TSynNativeInt): TtkTokenKind;
    function FuncVoid(Index: TSynNativeInt): TtkTokenKind;
    function FuncWhile(Index: TSynNativeInt): TtkTokenKind;
    function FuncWithin(Index: TSynNativeInt): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure DollarSignProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
  protected
    function GetExtTokenID: TxtkTokenKind;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
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
    function GetTokenKind: TSynNativeInt; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: TSynNativeInt): Boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property Key2Attri: TSynHighlighterAttributes read fKey2Attri write fKey2Attri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SingleStringAttri: TSynHighlighterAttributes read fString2Attri
      write fString2Attri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..142] of string = (
    'abstract', 'always', 'array', 'arraycount', 'assert', 'auto', 'automated', 
    'bool', 'boundingbox', 'boundingvolume', 'break', 'button', 'byte', 'cache', 
    'cacheexempt', 'case', 'catch', 'class', 'coerce', 'collapsecategories', 
    'color', 'config', 'const', 'continue', 'coords', 'cpptext', 'cross', 
    'default', 'defaultproperties', 'delegate', 'delete', 'dependson', 
    'deprecated', 'do', 'dontcollapsecategories', 'dot', 'each', 'edfindable', 
    'editconst', 'editconstarray', 'editinline', 'editinlinenew', 
    'editinlinenotify', 'editinlineuse', 'else', 'enum', 'enumcount', 'event', 
    'exec', 'expands', 'explicit', 'export', 'exportstructs', 'extends', 
    'false', 'final', 'float', 'for', 'foreach', 'function', 'global', 
    'globalconfig', 'goto', 'guid', 'hidecategories', 'hidedropdown', 
    'hideparent', 'if', 'ignores', 'import', 'init', 'input', 'insert', 
    'instanced', 'int', 'intrinsic', 'invariant', 'iterator', 'latent', 
    'length', 'local', 'localized', 'long', 'mesh', 'model', 'mutable', 'name', 
    'native', 'nativereplication', 'new', 'noexport', 'none', 
    'noteditinlinenew', 'notplaceable', 'nousercreate', 'operator', 'optional', 
    'out', 'parseconfig', 'perobjectconfig', 'placeable', 'plane', 'pointer', 
    'postoperator', 'preoperator', 'private', 'protected', 'register', 
    'reliable', 'remove', 'replication', 'return', 'rng', 'rot', 'rotator', 
    'safereplace', 'scale', 'scriptconst', 'self', 'showcategories', 
    'simulated', 'singular', 'skip', 'sound', 'state', 'static', 'stop', 
    'string', 'struct', 'super', 'switch', 'texture', 'transient', 'travel', 
    'true', 'unreliable', 'until', 'var', 'vect', 'vector', 'void', 'while', 
    'within' 
  );

  KeyIndices: array[0..732] of TSynNativeInt = (
    -1, -1, -1, -1, -1, -1, 78, -1, -1, -1, -1, 25, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 79, -1, -1, -1, -1, -1, -1, -1, -1, 104, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 34, -1, -1, -1, 18, -1, -1, -1, -1, -1, 30, 1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 63, -1, -1, -1, -1, 114, 
    -1, -1, 121, -1, -1, -1, -1, -1, 105, -1, -1, 108, -1, 135, 9, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 117, 33, 109, -1, -1, -1, -1, -1, -1, 90, -1, -1, 
    -1, -1, -1, 106, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, 19, -1, 
    -1, -1, -1, 81, -1, 82, -1, -1, -1, -1, 40, 15, -1, -1, -1, 52, -1, 80, -1, 
    -1, -1, -1, -1, -1, 136, -1, -1, 61, -1, 113, -1, -1, -1, 83, -1, -1, -1, 
    -1, -1, -1, 27, -1, -1, 133, -1, -1, -1, -1, 62, -1, -1, -1, -1, -1, -1, -1, 
    76, -1, -1, -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, 
    51, -1, -1, -1, -1, 44, -1, 22, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 20, -1, -1, -1, 8, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 96, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, -1, 
    -1, -1, -1, -1, -1, 39, 24, -1, -1, -1, -1, 54, -1, 4, 123, -1, -1, -1, -1, 
    -1, -1, 50, 141, -1, -1, -1, -1, -1, -1, -1, 87, -1, -1, 21, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, 85, -1, 
    -1, -1, -1, -1, 70, -1, 68, 131, -1, -1, 69, -1, -1, -1, -1, -1, 128, 26, 
    -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, 142, -1, -1, 122, -1, 74, -1, -1, 
    -1, -1, -1, -1, -1, 13, -1, -1, -1, -1, 101, 119, -1, -1, 94, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, 89, -1, -1, 0, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 29, -1, -1, 92, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 112, -1, -1, -1, -1, 67, -1, -1, 45, -1, 
    116, -1, -1, 132, 28, -1, -1, -1, 31, -1, -1, -1, 77, -1, -1, -1, -1, -1, 
    91, -1, 37, -1, -1, -1, -1, 35, -1, 6, -1, -1, -1, -1, -1, -1, -1, 97, -1, 
    -1, -1, -1, -1, 53, -1, 84, -1, -1, -1, -1, 56, 14, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 23, -1, 107, -1, -1, -1, -1, 98, -1, -1, 75, -1, -1, -1, -1, 
    -1, 88, -1, -1, 103, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, 59, 
    139, 11, 42, -1, -1, 95, -1, -1, -1, -1, -1, 3, -1, -1, -1, 38, -1, -1, -1, 
    -1, -1, -1, -1, -1, 16, -1, 46, -1, -1, -1, -1, -1, 102, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, 41, -1, -1, -1, 
    -1, -1, -1, -1, -1, 48, 64, -1, -1, -1, -1, 86, -1, 58, 43, 72, -1, -1, 66, 
    137, 71, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, -1, -1, -1, -1, 17, 130, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 120, -1, 73, -1, -1, 118, -1, -1, -1, 
    -1, -1, -1, 138, -1, -1, -1, 55, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 10, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, 115, -1, 
    -1, -1, -1, 32, 47, 49, -1, -1, -1, -1, -1, -1, -1, 57, -1, -1, -1, -1, -1, 
    -1, 125, 134, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 99, 12, -1, 127, 
    140, -1, -1 
  );

{$Q-}
function TSynUnrealSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 41 + Ord(Str^) * 701;
    Inc(Str);
  end;
  Result := Result mod 733;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynUnrealSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynUnrealSyn.InitIdent;
var
  i: TSynNativeInt;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[410] := FuncAbstract;
  fIdentFuncTable[71] := FuncAlways;
  fIdentFuncTable[219] := FuncArray;
  fIdentFuncTable[554] := FuncArraycount;
  fIdentFuncTable[294] := FuncAssert;
  fIdentFuncTable[681] := FuncAuto;
  fIdentFuncTable[477] := FuncAutomated;
  fIdentFuncTable[364] := FuncBool;
  fIdentFuncTable[249] := FuncBoundingbox;
  fIdentFuncTable[109] := FuncBoundingvolume;
  fIdentFuncTable[675] := FuncBreak;
  fIdentFuncTable[544] := FuncButton;
  fIdentFuncTable[727] := FuncByte;
  fIdentFuncTable[380] := FuncCache;
  fIdentFuncTable[499] := FuncCacheexempt;
  fIdentFuncTable[160] := FuncCase;
  fIdentFuncTable[567] := FuncCatch;
  fIdentFuncTable[635] := FuncClass;
  fIdentFuncTable[64] := FuncCoerce;
  fIdentFuncTable[147] := FuncCollapsecategories;
  fIdentFuncTable[245] := FuncColor;
  fIdentFuncTable[314] := FuncConfig;
  fIdentFuncTable[231] := FuncConst;
  fIdentFuncTable[510] := FuncContinue;
  fIdentFuncTable[287] := FuncCoords;
  fIdentFuncTable[11] := FuncCpptext;
  fIdentFuncTable[355] := FuncCross;
  fIdentFuncTable[189] := FuncDefault;
  fIdentFuncTable[454] := FuncDefaultproperties;
  fIdentFuncTable[425] := FuncDelegate;
  fIdentFuncTable[70] := FuncDelete;
  fIdentFuncTable[458] := FuncDependson;
  fIdentFuncTable[696] := FuncDeprecated;
  fIdentFuncTable[120] := FuncDo;
  fIdentFuncTable[60] := FuncDontcollapsecategories;
  fIdentFuncTable[475] := FuncDot;
  fIdentFuncTable[49] := FuncEach;
  fIdentFuncTable[470] := FuncEdfindable;
  fIdentFuncTable[558] := FuncEditconst;
  fIdentFuncTable[286] := FuncEditconstarray;
  fIdentFuncTable[159] := FuncEditinline;
  fIdentFuncTable[596] := FuncEditinlinenew;
  fIdentFuncTable[545] := FuncEditinlinenotify;
  fIdentFuncTable[614] := FuncEditinlineuse;
  fIdentFuncTable[229] := FuncElse;
  fIdentFuncTable[448] := FuncEnum;
  fIdentFuncTable[569] := FuncEnumcount;
  fIdentFuncTable[697] := FuncEvent;
  fIdentFuncTable[605] := FuncExec;
  fIdentFuncTable[698] := FuncExpands;
  fIdentFuncTable[302] := FuncExplicit;
  fIdentFuncTable[224] := FuncExport;
  fIdentFuncTable[164] := FuncExportstructs;
  fIdentFuncTable[491] := FuncExtends;
  fIdentFuncTable[292] := FuncFalse;
  fIdentFuncTable[662] := FuncFinal;
  fIdentFuncTable[498] := FuncFloat;
  fIdentFuncTable[706] := FuncFor;
  fIdentFuncTable[613] := FuncForeach;
  fIdentFuncTable[542] := FuncFunction;
  fIdentFuncTable[330] := FuncGlobal;
  fIdentFuncTable[176] := FuncGlobalconfig;
  fIdentFuncTable[197] := FuncGoto;
  fIdentFuncTable[89] := FuncGuid;
  fIdentFuncTable[606] := FuncHidecategories;
  fIdentFuncTable[278] := FuncHidedropdown;
  fIdentFuncTable[618] := FuncHideparent;
  fIdentFuncTable[445] := FuncIf;
  fIdentFuncTable[344] := FuncIgnores;
  fIdentFuncTable[348] := FuncImport;
  fIdentFuncTable[342] := FuncInit;
  fIdentFuncTable[620] := FuncInput;
  fIdentFuncTable[615] := FuncInsert;
  fIdentFuncTable[648] := FuncInstanced;
  fIdentFuncTable[372] := FuncInt;
  fIdentFuncTable[520] := FuncIntrinsic;
  fIdentFuncTable[205] := FuncInvariant;
  fIdentFuncTable[462] := FuncIterator;
  fIdentFuncTable[6] := FuncLatent;
  fIdentFuncTable[24] := FuncLength;
  fIdentFuncTable[166] := FuncLocal;
  fIdentFuncTable[152] := FuncLocalized;
  fIdentFuncTable[154] := FuncLong;
  fIdentFuncTable[182] := FuncMesh;
  fIdentFuncTable[493] := FuncModel;
  fIdentFuncTable[336] := FuncMutable;
  fIdentFuncTable[611] := FuncName;
  fIdentFuncTable[311] := FuncNative;
  fIdentFuncTable[526] := FuncNativereplication;
  fIdentFuncTable[407] := FuncNew;
  fIdentFuncTable[128] := FuncNoexport;
  fIdentFuncTable[468] := FuncNone;
  fIdentFuncTable[428] := FuncNoteditinlinenew;
  fIdentFuncTable[532] := FuncNotplaceable;
  fIdentFuncTable[389] := FuncNousercreate;
  fIdentFuncTable[548] := FuncOperator;
  fIdentFuncTable[265] := FuncOptional;
  fIdentFuncTable[485] := FuncOut;
  fIdentFuncTable[517] := FuncParseconfig;
  fIdentFuncTable[726] := FuncPerobjectconfig;
  fIdentFuncTable[401] := FuncPlaceable;
  fIdentFuncTable[385] := FuncPlane;
  fIdentFuncTable[575] := FuncPointer;
  fIdentFuncTable[529] := FuncPostoperator;
  fIdentFuncTable[33] := FuncPreoperator;
  fIdentFuncTable[103] := FuncPrivate;
  fIdentFuncTable[134] := FuncProtected;
  fIdentFuncTable[512] := FuncRegister;
  fIdentFuncTable[106] := FuncReliable;
  fIdentFuncTable[121] := FuncRemove;
  fIdentFuncTable[253] := FuncReplication;
  fIdentFuncTable[593] := FuncReturn;
  fIdentFuncTable[440] := FuncRng;
  fIdentFuncTable[178] := FuncRot;
  fIdentFuncTable[94] := FuncRotator;
  fIdentFuncTable[691] := FuncSafereplace;
  fIdentFuncTable[450] := FuncScale;
  fIdentFuncTable[119] := FuncScriptconst;
  fIdentFuncTable[651] := FuncSelf;
  fIdentFuncTable[386] := FuncShowcategories;
  fIdentFuncTable[646] := FuncSimulated;
  fIdentFuncTable[97] := FuncSingular;
  fIdentFuncTable[370] := FuncSkip;
  fIdentFuncTable[295] := FuncSound;
  fIdentFuncTable[142] := FuncState;
  fIdentFuncTable[713] := FuncStatic;
  fIdentFuncTable[213] := FuncStop;
  fIdentFuncTable[729] := FuncString;
  fIdentFuncTable[354] := FuncStruct;
  fIdentFuncTable[626] := FuncSuper;
  fIdentFuncTable[636] := FuncSwitch;
  fIdentFuncTable[345] := FuncTexture;
  fIdentFuncTable[453] := FuncTransient;
  fIdentFuncTable[192] := FuncTravel;
  fIdentFuncTable[714] := FuncTrue;
  fIdentFuncTable[108] := FuncUnreliable;
  fIdentFuncTable[173] := FuncUntil;
  fIdentFuncTable[619] := FuncVar;
  fIdentFuncTable[658] := FuncVect;
  fIdentFuncTable[543] := FuncVector;
  fIdentFuncTable[730] := FuncVoid;
  fIdentFuncTable[303] := FuncWhile;
  fIdentFuncTable[367] := FuncWithin;
end;

function TSynUnrealSyn.AltFunc(Index: TSynNativeInt): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAbstract(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAlways(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncArray(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncArraycount(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAssert(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAuto(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncAutomated(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBool(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBoundingbox(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBoundingvolume(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncBreak(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncButton(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncByte(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCache(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCacheexempt(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCatch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncClass(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCoerce(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCollapsecategories(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncColor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncConfig(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncConst(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncContinue(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCoords(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncCpptext(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;

end;

function TSynUnrealSyn.FuncCross(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSymbol
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDefault(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDefaultproperties(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDelegate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDelete(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDependson(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDeprecated(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDo(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDontcollapsecategories(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncDot(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkSymbol
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEach(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEdfindable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditconst(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditconstarray(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinline(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinlinenew(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinlinenotify(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEditinlineuse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncElse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEnum(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEnumcount(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncEvent(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExec(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExpands(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExplicit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExport(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExportstructs(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncExtends(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFalse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFinal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFloat(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncForeach(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncFunction(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGlobal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGlobalconfig(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGoto(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncGuid(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncHidecategories(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncHidedropdown(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncHideparent(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIf(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIgnores(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncImport(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInput(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInsert(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInstanced(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInt(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIntrinsic(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncInvariant(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncIterator(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLatent(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLength(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLocal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLocalized(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncLong(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncMesh(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncModel(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncMutable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncName(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNative(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNativereplication(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNew(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNoexport(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNone(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNoteditinlinenew(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNotplaceable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncNousercreate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncOperator(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncOptional(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncOut(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncParseconfig(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPerobjectconfig(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPlaceable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPlane(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPointer(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPostoperator(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPreoperator(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncPrivate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncProtected(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRegister(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncReliable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRemove(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncReplication(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncReturn(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRng(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRot(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncRotator(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSafereplace(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncScale(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncScriptconst(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSelf(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncShowcategories(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSimulated(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;

end;

function TSynUnrealSyn.FuncSingular(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSkip(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSound(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncState(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncStatic(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncStop(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncString(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncStruct(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSuper(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncSwitch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTexture(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTransient(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTravel(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncTrue(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncUnreliable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey2
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncUntil(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVar(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVect(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVector(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncVoid(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncWhile(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynUnrealSyn.FuncWithin(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

constructor TSynUnrealSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fKey2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  fKey2Attri.Style:= [fsBold];
  AddAttribute(fKey2Attri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fString2Attri := TSynHighlighterAttributes.Create(SYNS_AttrSingleString, SYNS_FriendlyAttrSingleString);
  AddAttribute(fString2Attri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterCPP;
end; { Create }

procedure TSynUnrealSyn.AnsiCProc;
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

  while not IsLineEnd(Run) do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          Inc(Run, 2);
          if fRange = rsDirectiveComment then                              
            fRange := rsDirective
          else
            fRange := rsUnKnown;
          Break;
        end else
          Inc(Run);
      #10: Break;
      #13: Break;
    else Inc(Run);
    end;
end;

procedure TSynUnrealSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkAndAssign;
      end;
    '&':                               {logical and}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkLogAnd;
      end;
  else                                 {and}
    begin
      Inc(Run);
      FExtTokenID := xtkAnd;
    end;
  end;
end;

procedure TSynUnrealSyn.AsciiCharProc;
begin
  fTokenID := tkString2;
  repeat
    if IsLineEnd(Run) then Break;
    if FLine[Run] = #92 then                             {backslash}
        {if we have an escaped single quote it doesn't count}
      if FLine[Run + 1] = #39 then Inc(Run);
    Inc(Run);
  until FLine[Run] = #39;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynUnrealSyn.BraceCloseProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceClose;
end;

procedure TSynUnrealSyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
  FExtTokenID := xtkBraceOpen;
end;

procedure TSynUnrealSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynUnrealSyn.ColonProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
    ':':                               {scope resolution operator}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkScopeResolution;
      end;
  else                                 {colon}
    begin
      Inc(Run);
      FExtTokenID := xtkColon;
    end;
  end;
end;

procedure TSynUnrealSyn.CommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynUnrealSyn.DirectiveProc;
begin
  if IsLineEnd(Run) then
  begin
    if (Run <= 0) then
      fRange := rsUnknown;
    NextProcedure;
  end
  else
  begin
    fTokenID := tkDirective;
    while True do
      case fLine[Run] of
        '/': // comment?
          begin
            if fLine[Run + 1] = '/' then // is end of directive as well
              Break
            else if fLine[Run + 1] = '*' then
            begin // might be embedded only
              fRange := rsDirectiveComment;
              Break;
            end else
              Inc(Run);
          end;
        #0, #10, #13:
          begin
            fRange := rsUnknown;
            Break;
          end;
        else Inc(Run);
      end;
  end;
end;

procedure TSynUnrealSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkLogEqual;
      end;
  else                                 {assign}
    begin
      Inc(Run);
      FExtTokenID := xtkAssign;
    end;
  end;
end;

procedure TSynUnrealSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkGreaterThanEqual;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
        begin
          Inc(Run, 3);
          FExtTokenID := xtkShiftRightAssign;
        end
        else                           {shift right}
        begin
          Inc(Run, 2);
          FExtTokenID := xtkShiftRight;
        end;
      end;
  else                                 {greater than}
    begin
      Inc(Run);
      FExtTokenID := xtkGreaterThan;
    end;
  end;
end;

procedure TSynUnrealSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

procedure TSynUnrealSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do Inc(Run);
end;

procedure TSynUnrealSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynUnrealSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkLessThanEqual;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          Inc(Run, 3);
          FExtTokenID := xtkShiftLeftAssign;
        end
        else                           {shift left}
        begin
          Inc(Run, 2);
          FExtTokenID := xtkShiftLeft;
        end;
      end;
  else                                 {less than}
    begin
      Inc(Run);
      FExtTokenID := xtkLessThan;
    end;
  end;
end;

procedure TSynUnrealSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkSubtractAssign;
      end;
    '-':                               {decrement}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkDecrement;
      end;
    '>':                               {arrow}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkArrow;
      end;
  else                                 {subtract}
    begin
      Inc(Run);
      FExtTokenID := xtkSubtract;
    end;
  end;
end;

procedure TSynUnrealSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {mod assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkModAssign;
      end;
  else                                 {mod}
    begin
      Inc(Run);
      FExtTokenID := xtkMod;
    end;
  end;
end;

procedure TSynUnrealSyn.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkNotEqual;
      end;
  else                                 {not}
    begin
      Inc(Run);
      FExtTokenID := xtkLogComplement;
    end;
  end;
end;

procedure TSynUnrealSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynUnrealSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X':
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

procedure TSynUnrealSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {or assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkIncOrAssign;
      end;
    '|':                               {logical or}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkLogOr;
      end;
  else                                 {or}
    begin
      Inc(Run);
      FExtTokenID := xtkIncOr;
    end;
  end;
end;

procedure TSynUnrealSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkAddAssign;
      end;
    '+':                               {increment}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkIncrement;
      end;
  else                                 {add}
    begin
      Inc(Run);
      FExtTokenID := xtkAdd;
    end;
  end;
end;

procedure TSynUnrealSyn.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      Inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else                                 {point}
    begin
      Inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynUnrealSyn.RoundCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkRoundClose;
  Dec(FRoundCount);
end;

procedure TSynUnrealSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  FExtTokenID := xtkRoundOpen;
  Inc(FRoundCount);
end;

procedure TSynUnrealSyn.SemiColonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
end;

procedure TSynUnrealSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        fTokenID := tkComment;
        Inc(Run, 2);
       while not IsLineEnd(Run) do Inc(Run);
      end;
    '*':                               {c style comments}
      begin
        fTokenID := tkComment;
        if fRange <> rsDirectiveComment then                               
          fRange := rsAnsiC;
        Inc(Run, 2);
        while not IsLineEnd(Run) do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                Inc(Run, 2);
                if fRange = rsDirectiveComment then
                  fRange := rsDirective
                else
                  fRange := rsUnKnown;
                Break;
              end else Inc(Run);
            #10, #13:
              begin
                if fRange = rsDirectiveComment then
                  fRange := rsAnsiC;
                Break;
              end;
          else Inc(Run);
          end;
      end;
    '=':                               {divide assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
        FExtTokenID := xtkDivideAssign;
      end;
  else                                 {divide}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
      FExtTokenID := xtkDivide;
    end;
  end;
end;

procedure TSynUnrealSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynUnrealSyn.SquareCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareClose;
  Dec(FSquareCount);
end;

procedure TSynUnrealSyn.SquareOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSquareOpen;
  Inc(FSquareCount);
end;

procedure TSynUnrealSyn.StarProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkMultiplyAssign;
      end;
  else                                 {star}
    begin
      Inc(Run);
      FExtTokenID := xtkStar;
    end;
  end;
end;

procedure TSynUnrealSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then Break;
    if FLine[Run] = #92 then                             {backslash}
        case FLine[Run + 1] of
          #10: Inc(Run);               {line continuation character}
          #34: Inc(Run);               {escaped quote doesn't count}
          #92: Inc(Run);
        end;
    Inc(Run);
  until FLine[Run] = #34;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynUnrealSyn.DollarSignProc;
begin
  fTokenID := tkSymbol;
  Inc(run);
end;


procedure TSynUnrealSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynUnrealSyn.XOrSymbolProc;
begin
  fTokenID := tkSymbol;
  Case FLine[Run + 1] of
  	'=':                               {xor assign}
      begin
        Inc(Run, 2);
        FExtTokenID := xtkXorAssign;
      end;
  else                                 {xor}
    begin
      Inc(Run);
      FExtTokenID := xtkXor;
    end;
  end;
end;

procedure TSynUnrealSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynUnrealSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsDirectiveComment: AnsiCProc;
    rsDirective: DirectiveProc;
  else
    begin
      fRange := rsUnknown;
      NextProcedure
    end;
  end;
  inherited;
end;

procedure TSynUnrealSyn.NextProcedure;
begin
  case fLine[Run] of
    '&': AndSymbolProc;
    #39: AsciiCharProc;
    '}': BraceCloseProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '#': DirectiveProc;
    '=': EqualProc;
    '>': GreaterProc;
    '?': QuestionProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '%': ModSymbolProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '.': PointProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    '*': StarProc;
    #34: StringProc;
    '$', '@': DollarSignProc;
    '~': TildeProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
end;

function TSynUnrealSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynUnrealSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynUnrealSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynUnrealSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynUnrealSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;


function TSynUnrealSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPP;
end; { IsFilterStored }


function TSynUnrealSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkKey2: Result := fKey2Attri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkString2: Result := fString2Attri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    else Result := nil;
  end;
end;

function TSynUnrealSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynUnrealSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynUnrealSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynUnrealSyn.EnumUserSettings(settings: TStrings);
begin
  { returns the user settings that exist in the registry }
  with TRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Borland\C++Builder') then
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

function TSynUnrealSyn.UseUserSettings(settingIndex: TSynNativeInt): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

  function ReadCPPBSettings(settingIndex: TSynNativeInt): Boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;

      function ReadCPPB1(settingTag: string; attri: TSynHighlighterAttributes; name: string): Boolean;
      var
        I: TSynNativeInt;
      begin
        for I := 1 to Length(name) do
          if name[I] = ' ' then name[I] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
             '\SOFTWARE\Borland\C++Builder\'+settingTag+'\Highlight',name, True);
      end; { ReadCPPB1 }

      function ReadCPPB3OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                 '\Software\Borland\C++Builder\'+settingTag+'\Editor\Highlight',
                 key, False);
      end; { ReadCPPB3OrMore }

    begin { ReadCPPBSetting }
      try
        if (settingTag[1] = '1')
          then Result := ReadCPPB1(settingTag,attri,key)
          else Result := ReadCPPB3OrMore(settingTag,attri,key);
      except
        Result := False;
      end;
    end; { ReadCPPBSetting }

  var
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpInvalidAttri   : TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    tmpDirecAttri     : TSynHighlighterAttributes;
    sl                 : TStringList;

  begin { ReadCPPBSettings }
    sl := TStringList.Create;
    try
      EnumUserSettings(sl);
      if settingIndex >= sl.Count then
        Result := False
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('', '');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('', '');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('', '');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('', '');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('', '');
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('', '');
        tmpInvalidAttri   := TSynHighlighterAttributes.Create('', '');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('', '');
        tmpDirecAttri     := TSynHighlighterAttributes.Create('', '');
        tmpStringAttri    .Assign(fStringAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpInvalidAttri   .Assign(fInvalidAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        tmpDirecAttri     .Assign(fDirecAttri);
        Result := ReadCPPBSetting(sl.ItemsNative[settingIndex],fCommentAttri,'Comment')       and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fNumberAttri,'Integer')        and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fStringAttri,'String')         and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(sl.ItemsNative[settingIndex],fDirecAttri,'Preprocessor');
        if not Result then begin
          fStringAttri    .Assign(tmpStringAttri);
          fString2Attri   .Assign(tmpStringAttri);
          fNumberAttri    .Assign(tmpNumberAttri);
          fKeyAttri       .Assign(tmpKeyAttri);
          fKey2Attri      .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
          fCommentAttri   .Assign(tmpCommentAttri);
          fIdentifierAttri.Assign(tmpIdentifierAttri);
          fInvalidAttri.Assign(tmpInvalidAttri);
          fSpaceAttri     .Assign(tmpSpaceAttri);
          fDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpNumberAttri    .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally
      sl.Free;
    end;
  end; { ReadCPPBSettings }

begin
  Result := ReadCPPBSettings(settingIndex);
end; { TSynUnrealSyn.UseUserSettings }

class function TSynUnrealSyn.GetLanguageName: string;
begin
  Result := SYNS_LangUnreal;
end;

class function TSynUnrealSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynUnrealSyn.GetSampleSource: string;
begin
  Result := '//----Comment-----------------------------------------------------------'#13#10+
            'class TestObject expands Object native;'#13#10+
            #13#10+
            '#exec MESH    IMPORT     MESH=Something ANIVFILE=MODELS\Something.3D DATAFILE=MODELS\Something.3D X=0 Y=0 Z=0 MLOD=0'#13#10+
            #13#10+
            'var() Sound HitSound;'#13#10+
            'function Cast()'#13#10+
            '{'#13#10+
            '  Super.Cast();'#13#10+
            '  CastTime = 50;'#13#10+
            '  GatherEffect = Spawn( class''SomethingCorona'',,, GetStartLoc(), Pawn(Owner).ViewRotation );'#13#10+
            '  GatherEffect.SetFollowPawn( Pawn(Owner) );'#13#10+
            '}'#13#10+
            #13#10+
            'defaultproperties'#13#10+
            '{'#13#10+
            '  PickupMessage="You have picked up a thing."'#13#10+
            '}';
end;

class function TSynUnrealSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangUnreal;
end;

initialization
  RegisterPlaceableHighlighter(TSynUnrealSyn);
end.
