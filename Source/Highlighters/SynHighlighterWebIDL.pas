{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterWebIDL.pas, released 2013-02-14.
Description: Syntax Parser/Highlighter
The initial author of this file is Christian-W. Budde.
Copyright (c) 2013, all rights reserved.

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

unit SynHighlighterWebIDL;

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
  TtkTokenKind = (
    tkArguments,
    tkComment,
    tkExtendedAttributes,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkTypes,
    tkSymbol,
    tkUnknown);

  TstkSymbolTokenKind = (
    stkBraceOpen,
    stkBraceClose,
    stkSquareOpen,
    stkSquareClose,
    stkQuestionMark,
    stkColon,
    stkGreater,
    stkLess
  );

  TRangeState = (rsUnknown, rsSingleComment, rsCStyleComment, rsString,
    rsExtendedAttributes);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: TSynNativeInt): TtkTokenKind of object;

type
  TSynWebIDLSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fSymbolTokenID: TstkSymbolTokenKind;
    fIdentFuncTable: array [0..58] of TIdentFuncTableFunc;
    fArgumentsAttri: TSynHighlighterAttributes;
    fExtendedAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTypesAttri: TSynHighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function FuncAny(Index: TSynNativeInt): TtkTokenKind;
    function FuncAttribute(Index: TSynNativeInt): TtkTokenKind;
    function FuncBoolean(Index: TSynNativeInt): TtkTokenKind;
    function FuncByte(Index: TSynNativeInt): TtkTokenKind;
    function FuncBytestring(Index: TSynNativeInt): TtkTokenKind;
    function FuncCallback(Index: TSynNativeInt): TtkTokenKind;
    function FuncConst(Index: TSynNativeInt): TtkTokenKind;
    function FuncCreator(Index: TSynNativeInt): TtkTokenKind;
    function FuncDate(Index: TSynNativeInt): TtkTokenKind;
    function FuncDeleter(Index: TSynNativeInt): TtkTokenKind;
    function FuncDictionary(Index: TSynNativeInt): TtkTokenKind;
    function FuncDomstring(Index: TSynNativeInt): TtkTokenKind;
    function FuncDouble(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnum(Index: TSynNativeInt): TtkTokenKind;
    function FuncException(Index: TSynNativeInt): TtkTokenKind;
    function FuncFloat(Index: TSynNativeInt): TtkTokenKind;
    function FuncGetter(Index: TSynNativeInt): TtkTokenKind;
    function FuncImplements(Index: TSynNativeInt): TtkTokenKind;
    function FuncInherit(Index: TSynNativeInt): TtkTokenKind;
    function FuncInterface(Index: TSynNativeInt): TtkTokenKind;
    function FuncLegacycaller(Index: TSynNativeInt): TtkTokenKind;
    function FuncLong(Index: TSynNativeInt): TtkTokenKind;
    function FuncObject(Index: TSynNativeInt): TtkTokenKind;
    function FuncOctet(Index: TSynNativeInt): TtkTokenKind;
    function FuncOptional(Index: TSynNativeInt): TtkTokenKind;
    function FuncPartial(Index: TSynNativeInt): TtkTokenKind;
    function FuncReadonly(Index: TSynNativeInt): TtkTokenKind;
    function FuncRegexp(Index: TSynNativeInt): TtkTokenKind;
    function FuncSequence(Index: TSynNativeInt): TtkTokenKind;
    function FuncSetter(Index: TSynNativeInt): TtkTokenKind;
    function FuncShort(Index: TSynNativeInt): TtkTokenKind;
    function FuncStatic(Index: TSynNativeInt): TtkTokenKind;
    function FuncStringifier(Index: TSynNativeInt): TtkTokenKind;
    function FuncTypedef(Index: TSynNativeInt): TtkTokenKind;
    function FuncUnresticted(Index: TSynNativeInt): TtkTokenKind;
    function FuncUnrestricted(Index: TSynNativeInt): TtkTokenKind;
    function FuncUnsigned(Index: TSynNativeInt): TtkTokenKind;
    function FuncVoid(Index: TSynNativeInt): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc(Index: TSynNativeInt): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure CRProc;
    procedure CStyleCommentProc;
    procedure GreaterProc;
    procedure LessProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure QuestionMarkProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StringOpenProc;
    procedure StringProc;
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
    property ArgumentsAttri: TSynHighlighterAttributes read fArgumentsAttri write fArgumentsAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property ExtendedAttri: TSynHighlighterAttributes read fExtendedAttri write fExtendedAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property TypesAttri: TSynHighlighterAttributes read fTypesAttri write fTypesAttri;
  end;

implementation

uses
  SynEditStrConst;

resourcestring
  SYNS_FilterWebIDL = 'Web IDL (*.idl)|*.idl';
  SYNS_LangWebIDL = 'Web IDL';
  SYNS_FriendlyLangWebIDL = 'Web IDL';
  SYNS_AttrArguments = 'Arguments';
  SYNS_FriendlyAttrArguments = 'Arguments';
  SYNS_AttrExtended = 'Extended';
  SYNS_FriendlyAttrExtended = 'Extended';

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..37] of UnicodeString = (
    'any', 'attribute', 'boolean', 'byte', 'bytestring', 'callback', 'const',
    'creator', 'date', 'deleter', 'dictionary', 'domstring', 'double', 'enum',
    'exception', 'float', 'getter', 'implements', 'inherit', 'interface',
    'legacycaller', 'long', 'object', 'octet', 'optional', 'partial',
    'readonly', 'regexp', 'sequence', 'setter', 'short', 'static',
    'stringifier', 'typedef', 'unresticted', 'unrestricted', 'unsigned', 'void'
  );

  KeyIndices: array[0..58] of TSynNativeInt = (
    14, 28, 4, 37, 21, -1, -1, 12, 17, -1, -1, 22, -1, 3, -1, -1, 29, -1, 27,
    31, -1, 1, 20, -1, 24, 15, 2, -1, -1, -1, -1, 23, -1, 19, 0, 13, 11, 16, 34,
    10, 36, 25, -1, 30, -1, 33, 32, 6, -1, 9, 7, -1, 8, -1, 26, 18, -1, 5, 35
  );

constructor TSynWebIDLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fArgumentsAttri := TSynHighLighterAttributes.Create(SYNS_AttrArguments,
    SYNS_FriendlyAttrArguments);
  fArgumentsAttri.Style := [fsBold];
  fArgumentsAttri.Foreground := clNavy;
  AddAttribute(fArgumentsAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment,
    SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fExtendedAttri := TSynHighlighterAttributes.Create(SYNS_AttrExtended,
    SYNS_FriendlyAttrExtended);
  fExtendedAttri.Style := [fsBold, fsItalic];
  fExtendedAttri.Foreground := clMaroon;
  AddAttribute(fExtendedAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier,
    SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord,
    SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clPurple;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsBold];
  fSymbolAttri.Foreground := clMaroon;
  AddAttribute(fSymbolAttri);

  fTypesAttri := TSynHighLighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  fTypesAttri.Foreground := clNavy;
  AddAttribute(fTypesAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterWebIDL;
  fRange := rsUnknown;
end;

procedure TSynWebIDLSyn.InitIdent;
var
  i: TSynNativeInt;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[34] := FuncAny;
  fIdentFuncTable[21] := FuncAttribute;
  fIdentFuncTable[26] := FuncBoolean;
  fIdentFuncTable[13] := FuncByte;
  fIdentFuncTable[2] := FuncBytestring;
  fIdentFuncTable[57] := FuncCallback;
  fIdentFuncTable[47] := FuncConst;
  fIdentFuncTable[50] := FuncCreator;
  fIdentFuncTable[52] := FuncDate;
  fIdentFuncTable[49] := FuncDeleter;
  fIdentFuncTable[39] := FuncDictionary;
  fIdentFuncTable[36] := FuncDomstring;
  fIdentFuncTable[7] := FuncDouble;
  fIdentFuncTable[35] := FuncEnum;
  fIdentFuncTable[0] := FuncException;
  fIdentFuncTable[25] := FuncFloat;
  fIdentFuncTable[37] := FuncGetter;
  fIdentFuncTable[8] := FuncImplements;
  fIdentFuncTable[55] := FuncInherit;
  fIdentFuncTable[33] := FuncInterface;
  fIdentFuncTable[22] := FuncLegacycaller;
  fIdentFuncTable[4] := FuncLong;
  fIdentFuncTable[11] := FuncObject;
  fIdentFuncTable[31] := FuncOctet;
  fIdentFuncTable[24] := FuncOptional;
  fIdentFuncTable[41] := FuncPartial;
  fIdentFuncTable[54] := FuncReadonly;
  fIdentFuncTable[18] := FuncRegexp;
  fIdentFuncTable[1] := FuncSequence;
  fIdentFuncTable[16] := FuncSetter;
  fIdentFuncTable[43] := FuncShort;
  fIdentFuncTable[19] := FuncStatic;
  fIdentFuncTable[46] := FuncStringifier;
  fIdentFuncTable[45] := FuncTypedef;
  fIdentFuncTable[38] := FuncUnresticted;
  fIdentFuncTable[58] := FuncUnrestricted;
  fIdentFuncTable[40] := FuncUnsigned;
  fIdentFuncTable[3] := FuncVoid;
end;

procedure TSynWebIDLSyn.ColonProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
  fSymbolTokenID := stkColon;
end;

{$Q-}
function TSynWebIDLSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 622 + Ord(Str^) * 657;
    Inc(Str);
  end;
  Result := Result mod 59;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynWebIDLSyn.FuncAny(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncAttribute(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncBoolean(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncByte(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncBytestring(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncCallback(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncConst(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncCreator(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncDate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncDeleter(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncDictionary(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncDomstring(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncDouble(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncEnum(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncException(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncFloat(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncGetter(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncImplements(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncInherit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncInterface(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncLegacycaller(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncLong(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncObject(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncOctet(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncOptional(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncPartial(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncReadonly(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncRegexp(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncSequence(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncSetter(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncShort(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncStatic(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncStringifier(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncTypedef(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncUnresticted(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncUnrestricted(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArguments
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncUnsigned(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.FuncVoid(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypes
  else
    Result := tkIdentifier;
end;

function TSynWebIDLSyn.AltFunc(Index: TSynNativeInt): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynWebIDLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynWebIDLSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynWebIDLSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynWebIDLSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexChar(Run: TSynNativeInt): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: TSynNativeInt; // token[1]
  isHex: Boolean;
begin
  fTokenID := tkNumber;
  isHex := False;
  idx1 := Run;
  Inc(Run);
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break;
      'a'..'f', 'A'..'F':
        if not isHex then
          Break;
      'x', 'X':
        begin
          if (FLine[idx1] <> '0') or (Run > Succ(idx1)) then
            Break;
          if not IsHexChar(Succ(Run)) then
            Break;
          isHex := True;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynWebIDLSyn.QuestionMarkProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  fSymbolTokenID := stkQuestionMark;
end;

procedure TSynWebIDLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynWebIDLSyn.GreaterProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
  fSymbolTokenID := stkGreater;
end;

procedure TSynWebIDLSyn.LessProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
  fSymbolTokenID := stkLess;
end;

procedure TSynWebIDLSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynWebIDLSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/':
      begin
        repeat
          Inc(Run);
        until IsLineEnd(Run);
        fRange := rsSingleComment;
        fTokenID := tkComment;
      end;
    '*':
      begin
        Inc(Run, 1);
        fRange := rsCStyleComment;
        fTokenID := tkComment;
      end
    else
      fTokenID := tkIdentifier;
  end;
end;

procedure TSynWebIDLSyn.CStyleCommentProc;
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

procedure TSynWebIDLSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  fTokenID := tkString;
end;

procedure TSynWebIDLSyn.StringProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
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
  end;
end;

procedure TSynWebIDLSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynWebIDLSyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
  fSymbolTokenID := stkBraceOpen;
end;

procedure TSynWebIDLSyn.BraceCloseProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
  fSymbolTokenID := stkBraceClose;
end;

procedure TSynWebIDLSyn.SquareOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  fSymbolTokenID := stkSquareOpen;
  fRange := rsExtendedAttributes;
end;

procedure TSynWebIDLSyn.SquareCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  fSymbolTokenID := stkSquareClose;
  fRange := rsUnknown
end;

procedure TSynWebIDLSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynWebIDLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsCStyleComment: CStyleCommentProc;
    rsString: StringProc;
  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      '/': SlashProc;
      '"': StringOpenProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '{': BraceOpenProc;
      '}': BraceCloseProc;
      ']': SquareCloseProc;
      '[': SquareOpenProc;
      '?': QuestionMarkProc;
      ':': ColonProc;
      '>': GreaterProc;
      '<': LessProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynWebIDLSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
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

function TSynWebIDLSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynWebIDLSyn.GetKeyWords(TokenKind: TSynNativeInt): UnicodeString;
begin
  Result :=
    'any,attribute,boolean,byte,ByteString,callback,const,creator,Date,del' +
    'eter,dictionary,DOMString,double,enum,exception,float,getter,implement' +
    's,inherit,interface,legacycaller,long,object,octet,optional,partial,re' +
    'adonly,RegExp,sequence,setter,short,static,stringifier,typedef,unresti' +
    'cted,unrestricted,unsigned,void';
end;

function TSynWebIDLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
  if (fRange = rsExtendedAttributes) and not ((fTokenID = tkSymbol) and
    (fSymbolTokenID = stkSquareOpen)) then
    Result := tkExtendedAttributes;
end;

function TSynWebIDLSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkArguments: Result := fArgumentsAttri;
    tkComment: Result := fCommentAttri;
    tkExtendedAttributes: Result := fExtendedAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTypes: Result := fTypesAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynWebIDLSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(fTokenId);
end;

function TSynWebIDLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynWebIDLSyn.GetSampleSource: UnicodeString;
begin
  Result :=
    '/* WEB IDL sample source */'#13#10 +
    '[Constructor]'#13#10 +
    'interface GraphicalWindow {'#13#10 +
    '  readonly attribute unsigned long width;'#13#10 +
    '  readonly attribute unsigned long height;'#13#10 +
    #13#10 +
    '  attribute Paint currentPaint;'#13#10 +
    #13#10 +
    '  void drawRectangle(float x, float y, float width, float height);' +
    #13#10#13#10 +
    '  void drawText(float x, float y, DOMString text);'#13#10 +
    '};';
end;

function TSynWebIDLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterWebIDL;
end;

class function TSynWebIDLSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangWebIDL;
end;

class function TSynWebIDLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangWebIDL;
end;

procedure TSynWebIDLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynWebIDLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynWebIDLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  RegisterPlaceableHighlighter(TSynWebIDLSyn);
end.
