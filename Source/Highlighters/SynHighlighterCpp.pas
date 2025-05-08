{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterCpp.pas, released 2000-04-10.
The Original Code is based on the dcjCppSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
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
@abstract(Provides a C++ syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1998)
@lastmod(2001-11-21)
The SynHighlighterCpp unit provides SynEdit with a C++ syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterCpp;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  SynUnicode,
  Classes,
  SynEditCodeFolding;

type
  TtkTokenKind = (tkBracket, tkKey, tkSymbol, tkAsm, tkComment, tkDirective, tkIdentifier, tkNull,
    tkNumber, tkSpace, tkString, tkUnknown,
    tkChar, tkFloat, tkHex, tkOctal);

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

  TRangeState = (rsUnknown, rsAnsiC, rsAnsiCAsm, rsAnsiCAsmBlock, rsAsm,
    rsAsmBlock, rsDirective, rsDirectiveComment, rsString34, rsString39,
    rsMultiLineString, rsMultiLineDirective);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

//  TSynCppSyn = class(TSynCustomHighlighter)
  TSynCppSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    FExtTokenID: TxtkTokenKind;
    fIdentFuncTable: array[0..640] of TIdentFuncTableFunc;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fOctalAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fCharAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fBracketAttri: TSynHighlighterAttributes;
    FNewPreprocesorStyle: Boolean;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncAsm(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AnsiCProc;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure DirectiveProc;
    procedure DirectiveEndProc;
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
    procedure TildeProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure StringEndProc;
    procedure SetNewPreprocesorStyle(const Value: Boolean);
  protected
    function GetExtTokenID: TxtkTokenKind;
    function GetSampleSource: string; override;
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
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UseUserSettings(settingIndex: Integer): Boolean; override;
    procedure EnumUserSettings(settings: TStrings); override;
    property ExtTokenID: TxtkTokenKind read GetExtTokenID;
    property NewPreprocesorStyle: Boolean read FNewPreprocesorStyle write SetNewPreprocesorStyle;
//++ CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
//-- CodeFolding
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property BracketAttri: TSynHighlighterAttributes read fBracketAttri write fBracketAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri write fFloatAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri write fHexAttri;
    property OctalAttri: TSynHighlighterAttributes read fOctalAttri write fOctalAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property CharAttri: TSynHighlighterAttributes read fCharAttri write fCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
  Windows,
  Registry,
  SynEditStrConst,
  SynEditMiscProcs;

const
  KeyWords: array[0..129] of string = (
    '__asm', '__automated', '__cdecl', '__classid', '__closure', '__declspec',
    '__dispid', '__except', '__export', '__fastcall', '__finally', '__import',
    '__int16', '__int32', '__int64', '__int8', '__pascal', '__property',
    '__published', '__rtti', '__stdcall', '__thread', '__try', '_Alignas',
    '_Alignof', '_asm', '_Atomic', '_Bool', '_cdecl', '_Complex', '_export',
    '_fastcall', '_Generic', '_Imaginary', '_import', '_Noreturn', '_pascal',
    '_Pragma', '_Static_assert', '_stdcall', '_Thread_local', 'alignas',
    'alignof', 'and', 'and_eq', 'asm', 'auto', 'bitand', 'bitor', 'bool',
    'break', 'case', 'catch', 'cdecl', 'class', 'compl', 'const', 'const_cast',
    'constexpr', 'continue', 'decltype', 'default', 'delete', 'do', 'double',
    'dynamic_cast', 'else', 'enum', 'explicit', 'extern', 'false', 'final',
    'float', 'for', 'friend', 'goto', 'char', 'char16_t', 'char32_t', 'if',
    'inline', 'int', 'interface', 'long', 'mutable', 'namespace', 'new',
    'noexcept', 'not', 'not_eq', 'nullptr', 'operator', 'or', 'or_eq',
    'override', 'pascal', 'private', 'protected', 'public', 'register',
    'reinterpret_cast', 'restrict', 'return', 'short', 'signed', 'sizeof',
    'static', 'static_assert', 'static_cast', 'struct', 'switch', 'template',
    'this', 'thread_local', 'throw', 'true', 'try', 'typedef', 'typeid',
    'typename', 'union', 'unsigned', 'using', 'virtual', 'void', 'volatile',
    'while', 'wchar_t', 'xor', 'xor_eq'
  );

  KeyIndices: array[0..640] of Integer = (
    68, -1, -1, -1, -1, -1, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 1, -1, -1, 17, -1, -1, -1, -1, 97, -1, -1, -1, -1,
    -1, 62, -1, 28, -1, -1, -1, -1, -1, -1, 64, 21, 90, -1, -1, -1, -1, 108, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 15, 81, -1, 3, -1, 69, -1,
    -1, -1, -1, -1, 127, -1, 98, 54, -1, -1, -1, -1, -1, -1, 101, -1, -1, -1,
    65, -1, -1, -1, 58, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, -1, -1,
    -1, -1, -1, -1, 75, -1, -1, 93, 121, 99, -1, -1, 12, -1, -1, -1, -1, -1, -1,
    -1, 100, -1, -1, 57, 120, 4, -1, 86, -1, -1, -1, -1, 44, -1, -1, -1, -1, 56,
    -1, -1, -1, -1, -1, -1, -1, -1, 124, -1, -1, -1, -1, 50, -1, -1, -1, 112,
    -1, -1, -1, -1, 40, -1, -1, -1, -1, -1, -1, -1, -1, 73, -1, -1, -1, -1, -1,
    -1, -1, -1, 41, -1, -1, -1, 55, 109, 89, -1, 9, 114, 128, -1, 123, -1, 8,
    -1, -1, -1, -1, -1, -1, 36, -1, -1, -1, 34, -1, 103, -1, -1, 22, -1, 20, -1,
    -1, -1, -1, -1, 94, -1, 83, -1, 51, -1, 33, -1, -1, -1, -1, -1, 47, -1, -1,
    -1, -1, 74, -1, -1, -1, -1, -1, -1, -1, -1, 116, 91, -1, 6, -1, -1, -1, -1,
    -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 129, -1, -1, 35, -1,
    -1, -1, -1, -1, -1, -1, 92, -1, -1, -1, 7, -1, 23, -1, -1, -1, -1, -1, -1,
    59, -1, -1, -1, 48, -1, -1, -1, -1, -1, -1, -1, -1, -1, 119, 67, -1, -1, 60,
    -1, -1, -1, 38, -1, -1, -1, 111, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88,
    -1, -1, -1, -1, -1, 80, -1, 42, -1, -1, -1, -1, -1, 25, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 95, 26, -1, -1, -1, -1, -1, -1, -1, 61, -1, -1, -1,
    32, 46, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 122, -1, -1, -1,
    -1, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1, -1, 66, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 71, 72, -1, -1, -1, -1, -1, -1, -1, 0,
    -1, -1, -1, -1, -1, -1, 39, -1, -1, -1, -1, -1, -1, -1, 43, 24, 105, -1, -1,
    -1, -1, -1, -1, -1, 118, -1, -1, -1, -1, -1, -1, -1, -1, 45, 49, -1, -1, -1,
    14, -1, -1, -1, -1, -1, -1, -1, -1, -1, 77, 82, -1, -1, 125, 70, -1, -1, -1,
    -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, -1, 113, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 16, -1, -1, -1, 11, -1, -1, 104, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, 117, -1, -1, -1, -1, 30, -1, -1,
    -1, -1, -1, -1, -1, 27, -1, -1, 84, -1, -1, -1, -1, -1, 106, -1, 126, -1,
    -1, -1, -1, 13, -1, -1, -1, -1, -1, -1, 52, -1, -1, 107, -1, 37, -1, -1, -1,
    -1, -1, -1, -1, 115, 87, -1, -1, 85, -1, -1, -1, -1, -1, -1, -1, 31, 19,
    102, 79, -1, -1, -1, 18, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 10, -1, -1, -1, -1, -1, -1, 63, -1, 96, -1, -1, -1, 76, -1, -1, -1
  );

{$Q-}
function TSynCppSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 61 + Ord(Str^) * 145;
    Inc(Str);
  end;
  Result := Result mod 641;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynCppSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynCppSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[352] := FuncAsm;
  fIdentFuncTable[433] := FuncAsm;
  fIdentFuncTable[467] := FuncAsm;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
end;

function TSynCppSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCppSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynCppSyn.FuncAsm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    fRange := rsAsm;
    fAsmStart := True;
  end
  else
    Result := tkIdentifier
end;

constructor TSynCppSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;
  FNewPreprocesorStyle := False;

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(fAsmAttri);
  fBracketAttri := TSynHighLighterAttributes.Create(SYNS_AttrBrackets, SYNS_AttrBrackets);
  AddAttribute(fBracketAttri);
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
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(fCharAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(fFloatAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(fHexAttri);
  fOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  AddAttribute(fOctalAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterCPP;
end;

procedure TSynCppSyn.AnsiCProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;
    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '*':
        if fLine[Run + 1] = '/' then
        begin
          Inc(Run, 2);
          if fRange = rsAnsiCAsm then
            fRange := rsAsm
          else if fRange = rsAnsiCAsmBlock then
            fRange := rsAsmBlock
          else if (fRange = rsDirectiveComment) and
            not IsLineEnd(Run) then
              fRange := rsMultiLineDirective
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

procedure TSynCppSyn.AndSymbolProc;
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

procedure TSynCppSyn.AsciiCharProc;
begin
  fTokenID := tkChar;
  repeat
    if fLine[Run] = '\' then begin
      if CharInSet(fLine[Run + 1], [#39, '\']) then
        Inc(Run);
    end;
    Inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = #39);
  if fLine[Run] = #39 then
    Inc(Run);
end;

procedure TSynCppSyn.AtSymbolProc;
begin
  fTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynCppSyn.BraceCloseProc;
begin
  Inc(Run);
  fTokenID := tkBracket;
  FExtTokenID := xtkBraceClose;
  if fRange = rsAsmBlock then fRange := rsUnknown;
end;

procedure TSynCppSyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenID := tkBracket;
  FExtTokenID := xtkBraceOpen;
  if fRange = rsAsm then
  begin
    fRange := rsAsmBlock;
    fAsmStart := True;
  end;
end;

procedure TSynCppSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynCppSyn.ColonProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
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

procedure TSynCppSyn.CommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkComma;
end;

procedure TSynCppSyn.DirectiveProc;
var
  WasWord: Boolean;
begin
  if FNewPreprocesorStyle then
  begin
    if Trim(fLine)[1] <> '#' then
    begin
      fTokenID := tkUnknown;
      Inc(Run);
      Exit;
    end;
    WasWord := False;
    fTokenID := tkDirective;
    fRange := rsUnknown;
    repeat
      if not WasWord and IsIdentChar(FLine[Run]) then WasWord := True;
      Inc(Run);
    until IsLineEnd(Run) or (fLine[Run] = ' ') and WasWord;
  end
  else
  begin
    if Trim(fLine)[1] <> '#' then // '#' is not first char on the line, treat it as an invalid char
    begin
      fTokenID := tkUnknown;
      Inc(Run);
      Exit;
    end;
    fTokenID := tkDirective;
    repeat
      if fLine[Run] = '/' then // comment?
      begin
        if fLine[Run + 1] = '/' then // is end of directive as well
        begin
          fRange := rsUnknown;
          Exit;
        end
        else
          if fLine[Run + 1] = '*' then // might be embedded only
          begin
            fRange := rsDirectiveComment;
            Exit;
          end;
      end;
      if (fLine[Run] = '\') and (fLine[Run +1 ] = #0) then // a multiline directive
      begin
        Inc(Run);
        fRange := rsMultiLineDirective;
        Exit;
      end;
      Inc(Run);
    until IsLineEnd(Run)
  end;
end;

procedure TSynCppSyn.DirectiveEndProc;
begin
  fTokenID := tkDirective;
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
  fRange := rsUnknown;
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '/': // comment?
        begin
          case fLine[Run + 1] of
            '/': // is end of directive as well
              begin
                fRange := rsUnknown;
                Exit;
              end;
            '*': // might be embedded only
              begin
                fRange := rsDirectiveComment;
                Exit;
              end;
          end;
        end;
      '\': // yet another line?
        begin
          if fLine[Run + 1] = #0 then
          begin
            Inc(Run);
            fRange := rsMultiLineDirective;
            Exit;
          end;
        end;
    end;
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynCppSyn.EqualProc;
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

procedure TSynCppSyn.GreaterProc;
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

procedure TSynCppSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {conditional}
  FExtTokenID := xtkQuestion;
  Inc(Run);
end;

procedure TSynCppSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do Inc(Run);
end;

procedure TSynCppSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCppSyn.LowerProc;
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

procedure TSynCppSyn.MinusProc;
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

procedure TSynCppSyn.ModSymbolProc;
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

procedure TSynCppSyn.NotSymbolProc;
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

procedure TSynCppSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynCppSyn.NumberProc;

  function IsNumberChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsDigitPlusMinusChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '+', '-':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexDigit(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsAlphaUncerscore(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      'A'..'Z', 'a'..'z', '_':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  i: Integer;
begin
  idx1 := Run;
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar(Run) do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break
        else
          if (fTokenID <> tkHex) then
            fTokenID := tkFloat
          else // invalid
          begin
            fTokenID := tkUnknown;
            Exit;
          end;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // number <> float. an arithmetic operator
            Exit;
          if not CharInSet(FLine[Pred(Run)], ['e', 'E']) then
            Exit; // number = float, but no exponent. an arithmetic operator
          if not IsDigitPlusMinusChar(Succ(Run)) then // invalid
          begin
            Inc(Run);
            fTokenID := tkUnknown;
            Exit;
          end
        end;
      '0'..'7':
        if (Run = Succ(idx1)) and (FLine[idx1] = '0') then // octal number
          fTokenID := tkOctal;
      '8', '9':
        if (FLine[idx1] = '0') and
           ((fTokenID <> tkHex) and (fTokenID <> tkFloat)) then // invalid octal char
             fTokenID := tkUnknown;
      'a'..'d', 'A'..'D':
        if fTokenID <> tkHex then // invalid char
          Break;
      'e', 'E':
        if (fTokenID <> tkHex) then
          if CharInSet(FLine[Pred(Run)], ['0'..'9']) then // exponent
          begin
            for i := idx1 to Pred(Run) do
              if CharInSet(FLine[i], ['e', 'E']) then // too many exponents
              begin
                fTokenID := tkUnknown;
                Exit;
              end;
            if not IsDigitPlusMinusChar(Succ(Run)) then
              Break
            else
              fTokenID := tkFloat
          end
          else // invalid char
            Break;
      'f', 'F':
        if fTokenID <> tkHex then
        begin
          for i := idx1 to Pred(Run) do
            if CharInSet(FLine[i], ['f', 'F']) then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
          begin
            if CharInSet(fLine[Pred(Run)], ['l', 'L']) then // can't mix
              Break;
          end
          else
            fTokenID := tkFloat;
        end;
      'l', 'L':
        begin
          for i := idx1 to Run - 2 do
            if CharInSet(FLine[i], ['l', 'L']) then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
          if fTokenID = tkFloat then
            if CharInSet(fLine[Pred(Run)], ['f', 'F']) then // can't mix
              Break;
        end;
      'u', 'U':
        if fTokenID = tkFloat then // not allowed
          Break
        else
          for i := idx1 to Pred(Run) do
            if CharInSet(FLine[i], ['u', 'U']) then // declaration syntax error
            begin
              fTokenID := tkUnknown;
              Exit;
            end;
      'x', 'X':
        if (Run = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLine[idx1] = '0') and  // 0x...
           IsHexDigit(Succ(Run)) then // 0x... must be continued with a number
             fTokenID := tkHex
           else // invalid char
           begin
             if not IsIdentChar(fLine[Succ(Run)]) and
                CharInSet(FLine[Succ(idx1)], ['x', 'X']) then
             begin
               Inc(Run); // highlight 'x' too
               fTokenID := tkUnknown;
             end;
             Break;
           end;
    end; // case
    Inc(Run);
  end; // while
  if IsAlphaUncerscore(Run) then
    fTokenID := tkUnknown;
end;

procedure TSynCppSyn.OrSymbolProc;
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

procedure TSynCppSyn.PlusProc;
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

procedure TSynCppSyn.PointProc;
begin
  fTokenID := tkSymbol;
  if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
    begin                              {ellipse}
      Inc(Run, 3);
      FExtTokenID := xtkEllipse;
    end
  else
    if CharInSet(FLine[Run + 1], ['0'..'9']) then // float
    begin
      Dec(Run); // numberproc must see the point
      NumberProc;
    end
  else                                 {point}
    begin
      Inc(Run);
      FExtTokenID := xtkPoint;
    end;
end;

procedure TSynCppSyn.RoundCloseProc;
begin
  Inc(Run);
  fTokenID := tkBracket;
  FExtTokenID := xtkRoundClose;
end;

procedure TSynCppSyn.RoundOpenProc;
begin
  Inc(Run);
  fTokenID := tkBracket;
  FExtTokenID := xtkRoundOpen;
end;

procedure TSynCppSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: string;
  Line: Integer;

  function FindBraces(Line: Integer): Boolean;
  // Covers the following line patterns: {, }, {}, }{, {}{, }{}

    function LineHasChar(AChar: Char; StartCol: Integer; out Col: Integer): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      Col := 0;
      for I := StartCol to Length(CurLine) do begin
        if CurLine[I] = AChar then begin
          // Char must have proper highlighting (ignore stuff inside comments...)
          if GetHighlighterAttriAtRowCol(LinesToScan, Line, I) = fBracketAttri then
          begin
            Col := I;
            Exit(True);
          end;
        end;
      end;
    end;

    function Indent: Integer;
    begin
      Result := LeftSpaces(CurLine, True, TabWidth(LinesToScan));
    end;

  var
    OpenIdx: Integer;
    CloseIdx: Integer;
    Idx: Integer;
  begin
    LineHasChar('{', 1, OpenIdx);
    LineHasChar('}', 1, CloseIdx);

    Result := True;
    if (OpenIdx <= 0) and (CloseIdx <= 0) then
      Result := False
    else if (OpenIdx > 0) and (CloseIdx <= 0) then
      FoldRanges.StartFoldRange(Line + 1, 1, Indent)
    else if (OpenIdx <= 0) and (CloseIdx > 0) then
      FoldRanges.StopFoldRange(Line + 1, 1)
    else if CloseIdx >= OpenIdx then // {}
    begin
      if LineHasChar('{', CloseIdx, Idx) then
        FoldRanges.StartFoldRange(Line + 1, 1, Indent)
      else
        Result := False;
    end
    else // }{
    begin
      if LineHasChar('}', OpenIdx, Idx) then
        FoldRanges.StopFoldRange(Line + 1, 1)
      else
        FoldRanges.StopStartFoldRange(Line + 1, 1, Indent);
    end;
  end;

  function FoldRegion(Line: Integer): Boolean;
  var
    S: string;
  begin
    Result := False;
    S := TrimLeft(CurLine);
    if Uppercase(Copy(S, 1, 14)) = '#PRAGMA REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 17)) = '#PRAGMA ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline comments (Fold Type 2)
    if TRangeState(GetLineRange(LinesToScan, Line)) = rsANSIc then
    begin
      if TRangeState(GetLineRange(LinesToScan, Line - 1)) <> rsANSIc then
        FoldRanges.StartFoldRange(Line + 1, 2)
      else
        FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end
    else if TRangeState(GetLineRange(LinesToScan, Line - 1)) = rsANSIc then
    begin
      FoldRanges.StopFoldRange(Line + 1, 2);
      Continue;
    end;

    // Find Fold regions
    CurLine := LinesToScan[Line];
    if FoldRegion(Line) then
      Continue;

    // Find an braces on this line  (Fold Type 1)
    CurLine := LinesToScan[Line];
    if not FindBraces(Line) then
      FoldRanges.NoFoldInfo(Line + 1);
  end; // while Line
end;

procedure TSynCppSyn.SemiColonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  FExtTokenID := xtkSemiColon;
  if fRange = rsAsm then fRange := rsUnknown;
end;

procedure TSynCppSyn.SlashProc;
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
        if fRange = rsAsm then
          fRange := rsAnsiCAsm
        else if fRange = rsAsmBlock then
          fRange := rsAnsiCAsmBlock
        else if fRange <> rsDirectiveComment then
          fRange := rsAnsiC;
        Inc(Run, 2);
        while fLine[Run] <> #0 do
          case fLine[Run] of
            '*':
              if fLine[Run + 1] = '/' then
              begin
                Inc(Run, 2);
                if fRange = rsDirectiveComment then
                  fRange := rsMultiLineDirective
                else if fRange = rsAnsiCAsm then
                  fRange := rsAsm
                else
                  begin
                  if fRange = rsAnsiCAsmBlock then
                    fRange := rsAsmBlock
                  else
                    fRange := rsUnKnown;
                  end;
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

procedure TSynCppSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynCppSyn.SquareCloseProc;
begin
  Inc(Run);
  fTokenID := tkBracket;
  FExtTokenID := xtkSquareClose;
end;

procedure TSynCppSyn.SquareOpenProc;
begin
  Inc(Run);
  fTokenID := tkBracket;
  FExtTokenID := xtkSquareOpen;
end;

procedure TSynCppSyn.StarProc;
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

procedure TSynCppSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '\' then begin
      case fLine[Run + 1] of
        #34, '\':
          Inc(Run);
        #00:
          begin
            Inc(Run);
            fRange := rsMultilineString;
            Exit;
          end;
      end;
    end;
    Inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = #34);
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynCppSyn.StringEndProc;
begin
  fTokenID := tkString;

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

  fRange := rsUnknown;

  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      '\':
        begin
          case fLine[Run + 1] of
            #34, '\':
              Inc(Run);
            #00:
              begin
                Inc(Run);
                fRange := rsMultilineString;
                Exit;
              end;
          end;
        end;
      #34: Break;
    end;
    Inc(Run);
  until IsLineEnd(Run) or (fLine[Run] = #34);
  if FLine[Run] = #34 then
    Inc(Run);
end;

procedure TSynCppSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
  FExtTokenID := xtkBitComplement;
end;

procedure TSynCppSyn.XOrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
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

procedure TSynCppSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCppSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsiC, rsAnsiCAsm,
    rsAnsiCAsmBlock, rsDirectiveComment: AnsiCProc;
    rsMultiLineDirective: DirectiveEndProc;
    rsMultilineString: StringEndProc;
  else
    begin
//      fRange := rsUnknown;
      case fLine[Run] of
        '&': AndSymbolProc;
        #39: AsciiCharProc;
        '@': AtSymbolProc;
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
        '~': TildeProc;
        '^': XOrSymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynCppSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynCppSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynCppSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynCppSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
  if ((fRange = rsAsm) or (fRange = rsAsmBlock)) and not fAsmStart
    and not (fTokenId in [tkComment, tkSpace, tkNull])
  then
    Result := tkAsm;
end;

function TSynCppSyn.GetExtTokenID: TxtkTokenKind;
begin
  Result := FExtTokenID;
end;

function TSynCppSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  fTokenID := GetTokenID;
  case fTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkFloat: Result := fFloatAttri;
    tkHex: Result := fHexAttri;
    tkOctal: Result := fOctalAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkChar: Result := fCharAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    tkBracket: Result := fBracketAttri;
    else Result := nil;
  end;
end;

function TSynCppSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

procedure TSynCppSyn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynCppSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynCppSyn.EnumUserSettings(settings: TStrings);
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

function TSynCppSyn.UseUserSettings(settingIndex: Integer): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

  function ReadCPPBSettings(settingIndex: Integer): Boolean;

    function ReadCPPBSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;

      function ReadCPPB1(settingTag: string; attri: TSynHighlighterAttributes; name: string): Boolean;
      var
        I: Integer;
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
    tmpCharAttri      : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpFloatAttri     : TSynHighlighterAttributes;
    tmpHexAttri       : TSynHighlighterAttributes;
    tmpOctalAttri     : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    tmpAsmAttri       : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    tmpIdentifierAttri: TSynHighlighterAttributes;
    tmpInvalidAttri   : TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    tmpDirecAttri     : TSynHighlighterAttributes;
    s                 : TStringList;

  begin { ReadCPPBSettings }
    s := TStringList.Create;
    try
      EnumUserSettings(s);
      if settingIndex >= s.Count then
        Result := False
      else begin
        tmpStringAttri    := TSynHighlighterAttributes.Create('', '');
        tmpCharAttri      := TSynHighlighterAttributes.Create('', '');
        tmpNumberAttri    := TSynHighlighterAttributes.Create('', '');
        tmpFloatAttri     := TSynHighlighterAttributes.Create('', '');
        tmpHexAttri       := TSynHighlighterAttributes.Create('', '');
        tmpOctalAttri     := TSynHighlighterAttributes.Create('', '');
        tmpKeyAttri       := TSynHighlighterAttributes.Create('', '');
        tmpSymbolAttri    := TSynHighlighterAttributes.Create('', '');
        tmpAsmAttri       := TSynHighlighterAttributes.Create('', '');
        tmpCommentAttri   := TSynHighlighterAttributes.Create('', '');
        tmpIdentifierAttri:= TSynHighlighterAttributes.Create('', '');
        tmpInvalidAttri   := TSynHighlighterAttributes.Create('', '');
        tmpSpaceAttri     := TSynHighlighterAttributes.Create('', '');
        tmpDirecAttri     := TSynHighlighterAttributes.Create('', '');
        tmpStringAttri    .Assign(fStringAttri);
        tmpCharAttri      .Assign(fCharAttri);
        tmpNumberAttri    .Assign(fNumberAttri);
        tmpFloatAttri     .Assign(fFloatAttri);
        tmpHexAttri       .Assign(fHexAttri);
        tmpOctalAttri     .Assign(fOctalAttri);
        tmpKeyAttri       .Assign(fKeyAttri);
        tmpSymbolAttri    .Assign(fSymbolAttri);
        tmpAsmAttri       .Assign(fAsmAttri);
        tmpCommentAttri   .Assign(fCommentAttri);
        tmpIdentifierAttri.Assign(fIdentifierAttri);
        tmpInvalidAttri   .Assign(fInvalidAttri);
        tmpSpaceAttri     .Assign(fSpaceAttri);
        tmpDirecAttri     .Assign(fDirecAttri);
        if s[settingIndex][1] = '1'
          then Result := ReadCPPBSetting(s[settingIndex],fAsmAttri,'Plain text')
          else Result := ReadCPPBSetting(s[settingIndex],fAsmAttri,'Assembler');
        Result := Result                                                         and
                  ReadCPPBSetting(s[settingIndex],fCommentAttri,'Comment')       and
                  ReadCPPBSetting(s[settingIndex],fIdentifierAttri,'Identifier') and
                  ReadCPPBSetting(s[settingIndex],fInvalidAttri,'Illegal Char')  and
                  ReadCPPBSetting(s[settingIndex],fKeyAttri,'Reserved word')     and
                  ReadCPPBSetting(s[settingIndex],fNumberAttri,'Integer')        and
                  ReadCPPBSetting(s[settingIndex],fFloatAttri,'Float')           and
                  ReadCPPBSetting(s[settingIndex],fHexAttri,'Hex')               and
                  ReadCPPBSetting(s[settingIndex],fOctalAttri,'Octal')           and
                  ReadCPPBSetting(s[settingIndex],fSpaceAttri,'Whitespace')      and
                  ReadCPPBSetting(s[settingIndex],fStringAttri,'String')         and
                  ReadCPPBSetting(s[settingIndex],fCharAttri,'Character')             and
                  ReadCPPBSetting(s[settingIndex],fSymbolAttri,'Symbol')         and
                  ReadCPPBSetting(s[settingIndex],fDirecAttri,'Preprocessor');
        if not Result then begin
          fStringAttri    .Assign(tmpStringAttri);
          fCharAttri      .Assign(tmpCharAttri);
          fNumberAttri    .Assign(tmpNumberAttri);
          fFloatAttri     .Assign(tmpFloatAttri);
          fHexAttri       .Assign(tmpHexAttri);
          fOctalAttri     .Assign(tmpOctalAttri);
          fKeyAttri       .Assign(tmpKeyAttri);
          fSymbolAttri    .Assign(tmpSymbolAttri);
          fAsmAttri       .Assign(tmpAsmAttri);
          fCommentAttri   .Assign(tmpCommentAttri);
          fIdentifierAttri.Assign(tmpIdentifierAttri);
          fInvalidAttri   .Assign(tmpInvalidAttri);
          fSpaceAttri     .Assign(tmpSpaceAttri);
          fDirecAttri     .Assign(tmpDirecAttri);
        end;
        tmpStringAttri    .Free;
        tmpCharAttri      .Free;
        tmpNumberAttri    .Free;
        tmpFloatAttri     .Free;
        tmpHexAttri       .Free;
        tmpOctalAttri     .Free;
        tmpKeyAttri       .Free;
        tmpSymbolAttri    .Free;
        tmpAsmAttri       .Free;
        tmpCommentAttri   .Free;
        tmpIdentifierAttri.Free;
        tmpInvalidAttri   .Free;
        tmpSpaceAttri     .Free;
        tmpDirecAttri     .Free;
      end;
    finally s.Free; end;
  end; { ReadCPPBSettings }

begin
  Result := ReadCPPBSettings(settingIndex);
end; { TSynCppSyn.UseUserSettings }

function TSynCppSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCPP;
end;

class function TSynCppSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCPP;
end;

class function TSynCppSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings, hcStructureHighlight];
end;

function TSynCppSyn.GetSampleSource: string;
begin
  Result := '// Syntax Highlighting'#13#10+
            'void __fastcall TForm1::Button1Click(TObject *Sender)'#13#10+
            '{'#13#10+
            '  int number = 123456;'#13#10+
            '  char c = ''a'';'#13#10+
            '  Caption = "The number is " + IntToStr(i);'#13#10+
            '  for (int i = 0; i <= number; i++)'#13#10+
            '  {'#13#10+
            '    x -= 0xff;'#13#10+
            '    x -= 023;'#13#10+
            '    x += 1.0;'#13#10+
            '    x += @; /* illegal character */'#13#10+
            '  }'#13#10+
            '  #ifdef USE_ASM'#13#10+
            '    asm'#13#10+
            '    {'#13#10+
            '      ASM MOV AX, 0x1234'#13#10+
            '      ASM MOV i, AX'#13#10+
            '    }'#13#10+
            '  #endif'#13#10+
            '}';

end;

class function TSynCppSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangCPP;
end;

procedure TSynCppSyn.SetNewPreprocesorStyle(const Value: Boolean);
begin
  if FNewPreprocesorStyle <> Value then
  begin
    FNewPreprocesorStyle := Value;
    ResetRange;
  end;
end;

initialization
  RegisterPlaceableHighlighter(TSynCppSyn);
end.
