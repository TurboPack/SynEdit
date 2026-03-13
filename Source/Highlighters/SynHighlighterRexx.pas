{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: D:\GDS\rexx\synedit\SynHighlighterRexx.pas, released 2016-03-23.
Description: Syntax Parser/Highlighter
The initial author of this file is gds.
Copyright (c) 2016, all rights reserved.

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

unit SynHighlighterRexx;

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
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkSpace,
    tkSpecial,
    tkStdFunction,
    tkString,
    tkUnknown);

  TRangeState = (rsUnKnown, rsMultilineComment, rsSinglelineComment, rsStringDouble, rsStringSingle);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: TSynNativeInt): TtkTokenKind of object;

type
  TSynRexxSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..348] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSpecialAttri: TSynHighlighterAttributes;
    fStdFunctionAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function FuncAbbrev(Index: TSynNativeInt): TtkTokenKind;
    function FuncAbs(Index: TSynNativeInt): TtkTokenKind;
    function FuncAddress(Index: TSynNativeInt): TtkTokenKind;
    function FuncArg(Index: TSynNativeInt): TtkTokenKind;
    function FuncB2x(Index: TSynNativeInt): TtkTokenKind;
    function FuncBitand(Index: TSynNativeInt): TtkTokenKind;
    function FuncBitor(Index: TSynNativeInt): TtkTokenKind;
    function FuncBitxor(Index: TSynNativeInt): TtkTokenKind;
    function FuncC2d(Index: TSynNativeInt): TtkTokenKind;
    function FuncC2x(Index: TSynNativeInt): TtkTokenKind;
    function FuncCall(Index: TSynNativeInt): TtkTokenKind;
    function FuncCenter(Index: TSynNativeInt): TtkTokenKind;
    function FuncCentre(Index: TSynNativeInt): TtkTokenKind;
    function FuncChangestr(Index: TSynNativeInt): TtkTokenKind;
    function FuncCharin(Index: TSynNativeInt): TtkTokenKind;
    function FuncCharout(Index: TSynNativeInt): TtkTokenKind;
    function FuncChars(Index: TSynNativeInt): TtkTokenKind;
    function FuncCompare(Index: TSynNativeInt): TtkTokenKind;
    function FuncCondition(Index: TSynNativeInt): TtkTokenKind;
    function FuncCopies(Index: TSynNativeInt): TtkTokenKind;
    function FuncD2c(Index: TSynNativeInt): TtkTokenKind;
    function FuncD2x(Index: TSynNativeInt): TtkTokenKind;
    function FuncDatatype(Index: TSynNativeInt): TtkTokenKind;
    function FuncDate(Index: TSynNativeInt): TtkTokenKind;
    function FuncDelstr(Index: TSynNativeInt): TtkTokenKind;
    function FuncDelword(Index: TSynNativeInt): TtkTokenKind;
    function FuncDigits(Index: TSynNativeInt): TtkTokenKind;
    function FuncDo(Index: TSynNativeInt): TtkTokenKind;
    function FuncDrop(Index: TSynNativeInt): TtkTokenKind;
    function FuncElse(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnd(Index: TSynNativeInt): TtkTokenKind;
    function FuncErrortext(Index: TSynNativeInt): TtkTokenKind;
    function FuncExit(Index: TSynNativeInt): TtkTokenKind;
    function FuncForm(Index: TSynNativeInt): TtkTokenKind;
    function FuncFormat(Index: TSynNativeInt): TtkTokenKind;
    function FuncFuzz(Index: TSynNativeInt): TtkTokenKind;
    function FuncIf(Index: TSynNativeInt): TtkTokenKind;
    function FuncInsert(Index: TSynNativeInt): TtkTokenKind;
    function FuncInterpret(Index: TSynNativeInt): TtkTokenKind;
    function FuncIterate(Index: TSynNativeInt): TtkTokenKind;
    function FuncJustify(Index: TSynNativeInt): TtkTokenKind;
    function FuncLastpos(Index: TSynNativeInt): TtkTokenKind;
    function FuncLeave(Index: TSynNativeInt): TtkTokenKind;
    function FuncLeft(Index: TSynNativeInt): TtkTokenKind;
    function FuncLength(Index: TSynNativeInt): TtkTokenKind;
    function FuncLinein(Index: TSynNativeInt): TtkTokenKind;
    function FuncLineout(Index: TSynNativeInt): TtkTokenKind;
    function FuncLines(Index: TSynNativeInt): TtkTokenKind;
    function FuncLinesize(Index: TSynNativeInt): TtkTokenKind;
    function FuncMax(Index: TSynNativeInt): TtkTokenKind;
    function FuncMin(Index: TSynNativeInt): TtkTokenKind;
    function FuncNop(Index: TSynNativeInt): TtkTokenKind;
    function FuncNumeric(Index: TSynNativeInt): TtkTokenKind;
    function FuncOptions(Index: TSynNativeInt): TtkTokenKind;
    function FuncOtherwise(Index: TSynNativeInt): TtkTokenKind;
    function FuncOverlay(Index: TSynNativeInt): TtkTokenKind;
    function FuncParse(Index: TSynNativeInt): TtkTokenKind;
    function FuncPos(Index: TSynNativeInt): TtkTokenKind;
    function FuncProcedure(Index: TSynNativeInt): TtkTokenKind;
    function FuncPull(Index: TSynNativeInt): TtkTokenKind;
    function FuncPush(Index: TSynNativeInt): TtkTokenKind;
    function FuncQueue(Index: TSynNativeInt): TtkTokenKind;
    function FuncQueued(Index: TSynNativeInt): TtkTokenKind;
    function FuncRandom(Index: TSynNativeInt): TtkTokenKind;
    function FuncReturn(Index: TSynNativeInt): TtkTokenKind;
    function FuncReverse(Index: TSynNativeInt): TtkTokenKind;
    function FuncRight(Index: TSynNativeInt): TtkTokenKind;
    function FuncRxfuncadd(Index: TSynNativeInt): TtkTokenKind;
    function FuncRxfuncdrop(Index: TSynNativeInt): TtkTokenKind;
    function FuncRxfuncquery(Index: TSynNativeInt): TtkTokenKind;
    function FuncSay(Index: TSynNativeInt): TtkTokenKind;
    function FuncSelect(Index: TSynNativeInt): TtkTokenKind;
    function FuncSignal(Index: TSynNativeInt): TtkTokenKind;
    function FuncSourceline(Index: TSynNativeInt): TtkTokenKind;
    function FuncSpace(Index: TSynNativeInt): TtkTokenKind;
    function FuncStream(Index: TSynNativeInt): TtkTokenKind;
    function FuncStrip(Index: TSynNativeInt): TtkTokenKind;
    function FuncSubstr(Index: TSynNativeInt): TtkTokenKind;
    function FuncSubword(Index: TSynNativeInt): TtkTokenKind;
    function FuncSymbol(Index: TSynNativeInt): TtkTokenKind;
    function FuncThen(Index: TSynNativeInt): TtkTokenKind;
    function FuncTime(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrace(Index: TSynNativeInt): TtkTokenKind;
    function FuncTranslate(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrunc(Index: TSynNativeInt): TtkTokenKind;
    function FuncUpper(Index: TSynNativeInt): TtkTokenKind;
    function FuncValue(Index: TSynNativeInt): TtkTokenKind;
    function FuncVar(Index: TSynNativeInt): TtkTokenKind;
    function FuncVerify(Index: TSynNativeInt): TtkTokenKind;
    function FuncWhen(Index: TSynNativeInt): TtkTokenKind;
    function FuncWord(Index: TSynNativeInt): TtkTokenKind;
    function FuncWordindex(Index: TSynNativeInt): TtkTokenKind;
    function FuncWordlength(Index: TSynNativeInt): TtkTokenKind;
    function FuncWordpos(Index: TSynNativeInt): TtkTokenKind;
    function FuncWords(Index: TSynNativeInt): TtkTokenKind;
    function FuncX2b(Index: TSynNativeInt): TtkTokenKind;
    function FuncX2c(Index: TSynNativeInt): TtkTokenKind;
    function FuncX2d(Index: TSynNativeInt): TtkTokenKind;
    function FuncXrange(Index: TSynNativeInt): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc(Index: TSynNativeInt): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure MultilineCommentOpenProc;
    procedure MultilineCommentProc;
    procedure SinglelineCommentOpenProc;
    procedure SinglelineCommentProc;
    procedure StringDoubleOpenProc;
    procedure StringDoubleProc;
    procedure StringSingleOpenProc;
    procedure StringSingleProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetFriendlyLanguageName: UnicodeString; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: TSynNativeInt): UnicodeString; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property SpecialAttri: TSynHighlighterAttributes read fSpecialAttri write fSpecialAttri;
    property StdFunctionAttri: TSynHighlighterAttributes read fStdFunctionAttri write fStdFunctionAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
  end;

implementation

uses
  SynEditStrConst;

resourcestring
  SYNS_FilterREXX = 'Rexx sources|*.REX|All files (*.*)|*.*';
  SYNS_LangREXX = 'REXX';
  SYNS_FriendlyLangREXX = 'REXX';
  SYNS_AttrSpecial = 'Special';
  SYNS_FriendlyAttrSpecial = 'Special';
  SYNS_AttrStdFunction = 'StdFunction';
  SYNS_FriendlyAttrStdFunction = 'StdFunction';

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..98] of UnicodeString = (
    'abbrev', 'abs', 'address', 'arg', 'b2x', 'bitand', 'bitor', 'bitxor', 
    'c2d', 'c2x', 'call', 'center', 'centre', 'changestr', 'charin', 'charout', 
    'chars', 'compare', 'condition', 'copies', 'd2c', 'd2x', 'datatype', 'date', 
    'delstr', 'delword', 'digits', 'do', 'drop', 'else', 'end', 'errortext', 
    'exit', 'form', 'format', 'fuzz', 'if', 'insert', 'interpret', 'iterate', 
    'justify', 'lastpos', 'leave', 'left', 'length', 'linein', 'lineout', 
    'lines', 'linesize', 'max', 'min', 'nop', 'numeric', 'options', 'otherwise', 
    'overlay', 'parse', 'pos', 'procedure', 'pull', 'push', 'queue', 'queued', 
    'random', 'return', 'reverse', 'right', 'rxfuncadd', 'rxfuncdrop', 
    'rxfuncquery', 'say', 'select', 'signal', 'sourceline', 'space', 'stream', 
    'strip', 'substr', 'subword', 'symbol', 'then', 'time', 'trace', 
    'translate', 'trunc', 'upper', 'value', 'var', 'verify', 'when', 'word', 
    'wordindex', 'wordlength', 'wordpos', 'words', 'x2b', 'x2c', 'x2d', 'xrange' 
  );

  KeyIndices: array[0..348] of TSynNativeInt = (
    -1, 26, -1, -1, -1, -1, -1, -1, -1, 6, -1, -1, 20, -1, 75, 0, -1, 98, -1, 
    -1, 50, -1, 10, -1, -1, -1, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1, 66, -1, 
    -1, -1, 84, -1, -1, -1, -1, 83, 13, 42, 39, -1, -1, 5, -1, -1, 49, -1, -1, 
    -1, 87, -1, 1, -1, -1, -1, -1, -1, 65, -1, 12, 74, -1, 38, 29, -1, 82, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 80, 45, 68, 19, -1, -1, 63, -1, 67, -1, -1, 
    -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, 72, -1, -1, 60, -1, -1, 18, -1, -1, 
    -1, -1, -1, -1, -1, -1, 3, 23, -1, -1, 93, -1, -1, -1, -1, 47, 61, 30, -1, 
    -1, -1, -1, -1, 14, -1, -1, -1, -1, -1, 9, -1, -1, -1, -1, -1, 59, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 58, -1, 90, -1, -1, -1, 92, -1, -1, 37, -1, -1, 
    -1, 8, -1, 41, -1, -1, 51, -1, -1, -1, -1, -1, -1, -1, -1, 79, -1, -1, -1, 
    -1, -1, 86, 88, 78, -1, -1, -1, -1, 56, -1, -1, -1, -1, 31, -1, -1, 7, -1, 
    46, 27, 22, -1, -1, -1, 73, -1, -1, -1, -1, 16, -1, 94, -1, -1, -1, -1, 43, 
    -1, -1, -1, -1, 36, -1, -1, -1, -1, 48, -1, -1, 53, -1, -1, -1, 34, -1, -1, 
    -1, -1, -1, -1, -1, -1, 70, -1, -1, -1, 95, 24, -1, -1, -1, -1, -1, -1, 69, 
    -1, 35, -1, -1, -1, 81, -1, 96, -1, -1, -1, -1, -1, 85, 62, -1, -1, 28, -1, 
    -1, 57, 4, 15, 97, 77, -1, -1, -1, -1, -1, 17, -1, -1, -1, -1, -1, 91, -1, 
    -1, -1, 52, 71, 76, -1, 25, 55, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 64, -1, -1, 11, -1, -1, -1, -1, -1, 89, 54, -1, 
    -1, -1, 40, -1, 32, 2, 21 
  );

procedure TSynRexxSyn.InitIdent;
var
  i: TSynNativeInt;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[15] := FuncAbbrev;
  fIdentFuncTable[60] := FuncAbs;
  fIdentFuncTable[347] := FuncAddress;
  fIdentFuncTable[120] := FuncArg;
  fIdentFuncTable[287] := FuncB2x;
  fIdentFuncTable[51] := FuncBitand;
  fIdentFuncTable[9] := FuncBitor;
  fIdentFuncTable[207] := FuncBitxor;
  fIdentFuncTable[172] := FuncC2d;
  fIdentFuncTable[143] := FuncC2x;
  fIdentFuncTable[22] := FuncCall;
  fIdentFuncTable[333] := FuncCenter;
  fIdentFuncTable[68] := FuncCentre;
  fIdentFuncTable[46] := FuncChangestr;
  fIdentFuncTable[137] := FuncCharin;
  fIdentFuncTable[288] := FuncCharout;
  fIdentFuncTable[220] := FuncChars;
  fIdentFuncTable[296] := FuncCompare;
  fIdentFuncTable[111] := FuncCondition;
  fIdentFuncTable[87] := FuncCopies;
  fIdentFuncTable[12] := FuncD2c;
  fIdentFuncTable[348] := FuncD2x;
  fIdentFuncTable[211] := FuncDatatype;
  fIdentFuncTable[121] := FuncDate;
  fIdentFuncTable[258] := FuncDelstr;
  fIdentFuncTable[310] := FuncDelword;
  fIdentFuncTable[1] := FuncDigits;
  fIdentFuncTable[210] := FuncDo;
  fIdentFuncTable[283] := FuncDrop;
  fIdentFuncTable[72] := FuncElse;
  fIdentFuncTable[131] := FuncEnd;
  fIdentFuncTable[204] := FuncErrortext;
  fIdentFuncTable[346] := FuncExit;
  fIdentFuncTable[99] := FuncForm;
  fIdentFuncTable[244] := FuncFormat;
  fIdentFuncTable[267] := FuncFuzz;
  fIdentFuncTable[232] := FuncIf;
  fIdentFuncTable[168] := FuncInsert;
  fIdentFuncTable[71] := FuncInterpret;
  fIdentFuncTable[48] := FuncIterate;
  fIdentFuncTable[344] := FuncJustify;
  fIdentFuncTable[174] := FuncLastpos;
  fIdentFuncTable[47] := FuncLeave;
  fIdentFuncTable[227] := FuncLeft;
  fIdentFuncTable[28] := FuncLength;
  fIdentFuncTable[85] := FuncLinein;
  fIdentFuncTable[209] := FuncLineout;
  fIdentFuncTable[129] := FuncLines;
  fIdentFuncTable[237] := FuncLinesize;
  fIdentFuncTable[54] := FuncMax;
  fIdentFuncTable[20] := FuncMin;
  fIdentFuncTable[177] := FuncNop;
  fIdentFuncTable[306] := FuncNumeric;
  fIdentFuncTable[240] := FuncOptions;
  fIdentFuncTable[340] := FuncOtherwise;
  fIdentFuncTable[311] := FuncOverlay;
  fIdentFuncTable[199] := FuncParse;
  fIdentFuncTable[286] := FuncPos;
  fIdentFuncTable[159] := FuncProcedure;
  fIdentFuncTable[149] := FuncPull;
  fIdentFuncTable[108] := FuncPush;
  fIdentFuncTable[130] := FuncQueue;
  fIdentFuncTable[280] := FuncQueued;
  fIdentFuncTable[90] := FuncRandom;
  fIdentFuncTable[330] := FuncReturn;
  fIdentFuncTable[66] := FuncReverse;
  fIdentFuncTable[36] := FuncRight;
  fIdentFuncTable[92] := FuncRxfuncadd;
  fIdentFuncTable[86] := FuncRxfuncdrop;
  fIdentFuncTable[265] := FuncRxfuncquery;
  fIdentFuncTable[253] := FuncSay;
  fIdentFuncTable[307] := FuncSelect;
  fIdentFuncTable[105] := FuncSignal;
  fIdentFuncTable[215] := FuncSourceline;
  fIdentFuncTable[69] := FuncSpace;
  fIdentFuncTable[14] := FuncStream;
  fIdentFuncTable[308] := FuncStrip;
  fIdentFuncTable[290] := FuncSubstr;
  fIdentFuncTable[194] := FuncSubword;
  fIdentFuncTable[186] := FuncSymbol;
  fIdentFuncTable[84] := FuncThen;
  fIdentFuncTable[271] := FuncTime;
  fIdentFuncTable[74] := FuncTrace;
  fIdentFuncTable[45] := FuncTranslate;
  fIdentFuncTable[40] := FuncTrunc;
  fIdentFuncTable[279] := FuncUpper;
  fIdentFuncTable[192] := FuncValue;
  fIdentFuncTable[58] := FuncVar;
  fIdentFuncTable[193] := FuncVerify;
  fIdentFuncTable[339] := FuncWhen;
  fIdentFuncTable[161] := FuncWord;
  fIdentFuncTable[302] := FuncWordindex;
  fIdentFuncTable[165] := FuncWordlength;
  fIdentFuncTable[124] := FuncWordpos;
  fIdentFuncTable[222] := FuncWords;
  fIdentFuncTable[257] := FuncX2b;
  fIdentFuncTable[273] := FuncX2c;
  fIdentFuncTable[289] := FuncX2d;
  fIdentFuncTable[17] := FuncXrange;
end;

{$Q-}
function TSynRexxSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 290 + Ord(Str^) * 365;
    Inc(Str);
  end;
  Result := Result mod 349;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynRexxSyn.FuncAbbrev(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncAbs(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncAddress(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncArg(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncB2x(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncBitand(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncBitor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncBitxor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncC2d(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncC2x(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCall(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCenter(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCentre(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncChangestr(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCharin(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCharout(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncChars(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCompare(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCondition(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncCopies(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncD2c(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncD2x(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncDatatype(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncDate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncDelstr(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncDelword(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncDigits(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncDo(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncDrop(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncElse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncEnd(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncErrortext(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncExit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncForm(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncFormat(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncFuzz(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncIf(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncInsert(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncInterpret(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncIterate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncJustify(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLastpos(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLeave(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLeft(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLength(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLinein(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLineout(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLines(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncLinesize(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncMax(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncMin(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncNop(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncNumeric(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncOptions(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncOtherwise(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncOverlay(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncParse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncPos(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncProcedure(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncPull(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncPush(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncQueue(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncQueued(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncRandom(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncReturn(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncReverse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncRight(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncRxfuncadd(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncRxfuncdrop(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncRxfuncquery(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSay(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSelect(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSignal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSourceline(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSpace(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncStream(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncStrip(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSubstr(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSubword(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncSymbol(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncThen(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncTime(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncTrace(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncTranslate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncTrunc(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncUpper(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncValue(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncVar(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncVerify(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncWhen(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncWord(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncWordindex(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncWordlength(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncWordpos(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncWords(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncX2b(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncX2c(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncX2d(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.FuncXrange(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkStdFunction
  else
    Result := tkIdentifier;
end;

function TSynRexxSyn.AltFunc(Index: TSynNativeInt): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynRexxSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynRexxSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynRexxSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynRexxSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynRexxSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynRexxSyn.MultilineCommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '*') then
  begin
    Inc(Run, 1);
    fRange := rsMultilineComment;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynRexxSyn.MultilineCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and
           (fLine[Run + 1] = '/') then
        begin
          Inc(Run, 2);
          fRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(Run) then
          Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynRexxSyn.SinglelineCommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '-') then
  begin
    Inc(Run, 1);
    fRange := rsSinglelineComment;
    SinglelineCommentProc;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynRexxSyn.SinglelineCommentProc;
begin
  fTokenID := tkComment;
  repeat
    if (fLine[Run] = '-') and
       (fLine[Run + 1] = '-') then
    begin
      Inc(Run, 2);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynRexxSyn.StringDoubleOpenProc;
begin
  Inc(Run);
  fRange := rsStringDouble;
  StringDoubleProc;
  fTokenID := tkString;
end;

procedure TSynRexxSyn.StringDoubleProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '"') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynRexxSyn.StringSingleOpenProc;
begin
  Inc(Run);
  fRange := rsStringSingle;
  StringSingleProc;
  fTokenID := tkString;
end;

procedure TSynRexxSyn.StringSingleProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '''') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

constructor TSynRexxSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := $00EFBC89;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  fIdentifierAttri.Foreground := clLime;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Foreground := $009797FF;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fSpecialAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpecial, SYNS_FriendlyAttrSpecial);
  fSpecialAttri.Foreground := clAqua    ;
  AddAttribute(fSpecialAttri);

  fStdFunctionAttri := TSynHighLighterAttributes.Create(SYNS_AttrStdFunction, SYNS_FriendlyAttrStdFunction);
  fStdFunctionAttri.Foreground := clAqua  { new token type here };
  AddAttribute(fStdFunctionAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := $008CD959;
  fStringAttri.Background := $00606060;
  AddAttribute(fStringAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterREXX;
  fRange := rsUnknown;
end;

procedure TSynRexxSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
   Inc(Run, fStringLen);
   while IsIdentChar(fLine[Run]) do
          Inc(Run);
end;

procedure TSynRexxSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynRexxSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsMultilineComment: MultilineCommentProc;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      '/': MultilineCommentOpenProc;
      '-': SinglelineCommentOpenProc;
      '"': StringDoubleOpenProc;
      '''': StringSingleOpenProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynRexxSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynRexxSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynRexxSyn.GetKeyWords(TokenKind: TSynNativeInt): UnicodeString;
begin
  Result := 
    'ABBREV,ABS,ADDRESS,ARG,B2X,BITAND,BITOR,BITXOR,C2D,C2X,CALL,CENTER,CE' +
    'NTRE,CHANGESTR,CHARIN,CHAROUT,CHARS,COMPARE,CONDITION,COPIES,D2C,D2X,D' +
    'ATATYPE,DATE,DELSTR,DELWORD,DIGITS,DO,DROP,ELSE,END,ERRORTEXT,EXIT,FOR' +
    'M,FORMAT,FUZZ,IF,INSERT,INTERPRET,ITERATE,JUSTIFY,LASTPOS,LEAVE,LEFT,L' +
    'ENGTH,LINEIN,LINEOUT,LINES,LINESIZE,MAX,MIN,NOP,NUMERIC,OPTIONS,OTHERW' +
    'ISE,OVERLAY,PARSE,POS,PROCEDURE,PULL,PUSH,QUEUE,QUEUED,RANDOM,RETURN,R' +
    'EVERSE,RIGHT,RXFUNCADD,RXFUNCDROP,RXFUNCQUERY,SAY,SELECT,SIGNAL,SOURCE' +
    'LINE,SPACE,STREAM,STRIP,SUBSTR,SUBWORD,SYMBOL,THEN,TIME,TRACE,TRANSLAT' +
    'E,TRUNC,UPPER,VALUE,VAR,VERIFY,WHEN,WORD,WORDINDEX,WORDLENGTH,WORDPOS,' +
    'WORDS,X2B,X2C,X2D,XRANGE';
end;

function TSynRexxSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynRexxSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkSpecial: Result := fSpecialAttri;
    tkStdFunction: Result := fStdFunctionAttri;
    tkString: Result := fStringAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynRexxSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(fTokenId);
end;

function TSynRexxSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynRexxSyn.GetSampleSource: UnicodeString;
begin
  Result := 
    #13#10 +
    '/* multiline comment */'#13#10 +
    '-- single line comment'#13#10 +
    'identifier = 12'#13#10 +
    'say identifier'#13#10 +
    'return(identifier)';
end;

function TSynRexxSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterREXX;
end;

class function TSynRexxSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangREXX;
end;

class function TSynRexxSyn.GetLanguageName: string;
begin
  Result := SYNS_LangREXX;
end;

procedure TSynRexxSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynRexxSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynRexxSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  RegisterPlaceableHighlighter(TSynRexxSyn);
end.
