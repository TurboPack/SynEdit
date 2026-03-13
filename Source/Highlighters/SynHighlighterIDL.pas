{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterIDL.pas, released 2001-10-15.
Description: CORBA IDL Parser/Highlighter
The initial author of this file is P.L. Polak.
Unicode translation by Maël Hörz.
Copyright (c) 2001, all rights reserved.

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

unit SynHighlighterIDL;

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

Type
  TtkTokenKind = (
    tkComment,
    tkDatatype,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkPreprocessor,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsString, rsChar);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: TSynNativeInt): TtkTokenKind of object;

type
  TSynIdlSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..100] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fDatatypeAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPreprocessorAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: TSynNativeInt): TtkTokenKind;
    procedure IdentProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function FuncAbstract(Index: TSynNativeInt): TtkTokenKind;
    function FuncAny(Index: TSynNativeInt): TtkTokenKind;
    function FuncAttribute(Index: TSynNativeInt): TtkTokenKind;
    function FuncBoolean(Index: TSynNativeInt): TtkTokenKind;
    function FuncCase(Index: TSynNativeInt): TtkTokenKind;
    function FuncChar(Index: TSynNativeInt): TtkTokenKind;
    function FuncConst(Index: TSynNativeInt): TtkTokenKind;
    function FuncContext(Index: TSynNativeInt): TtkTokenKind;
    function FuncCustom(Index: TSynNativeInt): TtkTokenKind;
    function FuncDefault(Index: TSynNativeInt): TtkTokenKind;
    function FuncDouble(Index: TSynNativeInt): TtkTokenKind;
    function FuncEnum(Index: TSynNativeInt): TtkTokenKind;
    function FuncException(Index: TSynNativeInt): TtkTokenKind;
    function FuncFactory(Index: TSynNativeInt): TtkTokenKind;
    function FuncFalse(Index: TSynNativeInt): TtkTokenKind;
    function FuncFixed(Index: TSynNativeInt): TtkTokenKind;
    function FuncFloat(Index: TSynNativeInt): TtkTokenKind;
    function FuncIn(Index: TSynNativeInt): TtkTokenKind;
    function FuncInout(Index: TSynNativeInt): TtkTokenKind;
    function FuncInterface(Index: TSynNativeInt): TtkTokenKind;
    function FuncLocal(Index: TSynNativeInt): TtkTokenKind;
    function FuncLong(Index: TSynNativeInt): TtkTokenKind;
    function FuncModule(Index: TSynNativeInt): TtkTokenKind;
    function FuncNative(Index: TSynNativeInt): TtkTokenKind;
    function FuncObject(Index: TSynNativeInt): TtkTokenKind;
    function FuncOctet(Index: TSynNativeInt): TtkTokenKind;
    function FuncOneway(Index: TSynNativeInt): TtkTokenKind;
    function FuncOut(Index: TSynNativeInt): TtkTokenKind;
    function FuncPrivate(Index: TSynNativeInt): TtkTokenKind;
    function FuncPublic(Index: TSynNativeInt): TtkTokenKind;
    function FuncRaises(Index: TSynNativeInt): TtkTokenKind;
    function FuncReadonly(Index: TSynNativeInt): TtkTokenKind;
    function FuncSequence(Index: TSynNativeInt): TtkTokenKind;
    function FuncShort(Index: TSynNativeInt): TtkTokenKind;
    function FuncString(Index: TSynNativeInt): TtkTokenKind;
    function FuncStruct(Index: TSynNativeInt): TtkTokenKind;
    function FuncSupports(Index: TSynNativeInt): TtkTokenKind;
    function FuncSwitch(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrue(Index: TSynNativeInt): TtkTokenKind;
    function FuncTruncatable(Index: TSynNativeInt): TtkTokenKind;
    function FuncTypedef(Index: TSynNativeInt): TtkTokenKind;
    function FuncUnion(Index: TSynNativeInt): TtkTokenKind;
    function FuncUnsigned(Index: TSynNativeInt): TtkTokenKind;
    function FuncValuebase(Index: TSynNativeInt): TtkTokenKind;
    function FuncValuetype(Index: TSynNativeInt): TtkTokenKind;
    function FuncVoid(Index: TSynNativeInt): TtkTokenKind;
    function FuncWchar(Index: TSynNativeInt): TtkTokenKind;
    function FuncWstring(Index: TSynNativeInt): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CommentOpenProc;
    procedure CommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure CharOpenProc;
    procedure CharProc;
    procedure PreProcessorProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DatatypeAttri: TSynHighlighterAttributes read fDatatypeAttri write fDatatypeAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read fPreprocessorAttri write fPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..47] of string = (
    'abstract', 'any', 'attribute', 'boolean', 'case', 'char', 'const', 
    'context', 'custom', 'default', 'double', 'enum', 'exception', 'factory', 
    'FALSE', 'fixed', 'float', 'in', 'inout', 'interface', 'local', 'long', 
    'module', 'native', 'Object', 'octet', 'oneway', 'out', 'private', 'public', 
    'raises', 'readonly', 'sequence', 'short', 'string', 'struct', 'supports', 
    'switch', 'TRUE', 'truncatable', 'typedef', 'union', 'unsigned', 
    'ValueBase', 'valuetype', 'void', 'wchar', 'wstring' 
  );

  KeyIndices: array[0..100] of TSynNativeInt = (
    5, 19, 17, 7, -1, -1, -1, -1, -1, 15, 18, -1, 37, -1, 24, -1, -1, -1, 44, 
    -1, 11, 31, -1, 25, 33, -1, -1, 42, 39, -1, -1, 36, 46, -1, 27, -1, 43, 28, 
    26, 20, -1, 1, 32, 6, -1, 14, 8, -1, -1, -1, -1, 0, 35, -1, -1, -1, -1, -1, 
    -1, -1, -1, 45, 22, 47, -1, -1, 12, 4, -1, -1, -1, 10, -1, -1, 3, -1, 9, -1, 
    34, 30, 13, -1, 2, 21, 16, -1, 29, 40, -1, -1, -1, -1, -1, -1, -1, 23, -1, 
    38, -1, -1, 41 
  );

{$Q-}
function TSynIdlSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 612 + Ord(Str^) * 199;
    Inc(Str);
  end;
  Result := Result mod 101;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynIdlSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynIdlSyn.InitIdent;
var
  i: TSynNativeInt;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[51] := FuncAbstract;
  fIdentFuncTable[41] := FuncAny;
  fIdentFuncTable[82] := FuncAttribute;
  fIdentFuncTable[74] := FuncBoolean;
  fIdentFuncTable[67] := FuncCase;
  fIdentFuncTable[0] := FuncChar;
  fIdentFuncTable[43] := FuncConst;
  fIdentFuncTable[3] := FuncContext;
  fIdentFuncTable[46] := FuncCustom;
  fIdentFuncTable[76] := FuncDefault;
  fIdentFuncTable[71] := FuncDouble;
  fIdentFuncTable[20] := FuncEnum;
  fIdentFuncTable[66] := FuncException;
  fIdentFuncTable[80] := FuncFactory;
  fIdentFuncTable[45] := FuncFalse;
  fIdentFuncTable[9] := FuncFixed;
  fIdentFuncTable[84] := FuncFloat;
  fIdentFuncTable[2] := FuncIn;
  fIdentFuncTable[10] := FuncInout;
  fIdentFuncTable[1] := FuncInterface;
  fIdentFuncTable[39] := FuncLocal;
  fIdentFuncTable[83] := FuncLong;
  fIdentFuncTable[62] := FuncModule;
  fIdentFuncTable[95] := FuncNative;
  fIdentFuncTable[14] := FuncObject;
  fIdentFuncTable[23] := FuncOctet;
  fIdentFuncTable[38] := FuncOneway;
  fIdentFuncTable[34] := FuncOut;
  fIdentFuncTable[37] := FuncPrivate;
  fIdentFuncTable[86] := FuncPublic;
  fIdentFuncTable[79] := FuncRaises;
  fIdentFuncTable[21] := FuncReadonly;
  fIdentFuncTable[42] := FuncSequence;
  fIdentFuncTable[24] := FuncShort;
  fIdentFuncTable[78] := FuncString;
  fIdentFuncTable[52] := FuncStruct;
  fIdentFuncTable[31] := FuncSupports;
  fIdentFuncTable[12] := FuncSwitch;
  fIdentFuncTable[97] := FuncTrue;
  fIdentFuncTable[28] := FuncTruncatable;
  fIdentFuncTable[87] := FuncTypedef;
  fIdentFuncTable[100] := FuncUnion;
  fIdentFuncTable[27] := FuncUnsigned;
  fIdentFuncTable[36] := FuncValuebase;
  fIdentFuncTable[18] := FuncValuetype;
  fIdentFuncTable[61] := FuncVoid;
  fIdentFuncTable[32] := FuncWchar;
  fIdentFuncTable[63] := FuncWstring;
end;

function TSynIdlSyn.AltFunc(Index: TSynNativeInt): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynIdlSyn.FuncAbstract(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncAny(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncAttribute(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncBoolean(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncCase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncChar(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncConst(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncContext(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncCustom(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncDefault(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncDouble(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncEnum(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncException(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFactory(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFalse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFixed(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncFloat(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncIn(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncInout(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncInterface(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncLocal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncLong(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncModule(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncNative(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncObject(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncOctet(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncOneway(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncOut(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncPrivate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncPublic(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncRaises(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncReadonly(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncSequence(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncShort(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncString(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncStruct(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncSupports(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncSwitch(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncTrue(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncTruncatable(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncTypedef(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncUnion(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncUnsigned(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncValuebase(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncValuetype(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncVoid(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncWchar(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

function TSynIdlSyn.FuncWstring(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkDatatype
  else
    Result := tkIdentifier;
end;

procedure TSynIdlSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynIdlSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynIdlSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
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
      '.': if FLine[Run + 1] = '.' then
             Break;
    end;
    Inc(Run);
  end;
end; { NumberProc }


procedure TSynIdlSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynIdlSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynIdlSyn.CommentOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '*') then
  begin
    fRange := rsComment;
    CommentProc;
    fTokenID := tkComment;
  end
  else if (fLine[Run] = '/') then
  begin
    while not IsLineEnd(Run) do
      Inc(Run);
    fTokenID := tkComment;
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynIdlSyn.CommentProc;
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

procedure TSynIdlSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynIdlSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '"') then
    begin
      Inc(Run);
      fRange := rsUnKnown;
      Break;
    end
    else if (fLine[Run] = '\') then
      Inc(Run);
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynIdlSyn.CharOpenProc;
begin
  Inc(Run);
  fRange := rsChar;
  CharProc;
  fTokenID := tkString;
end;

procedure TSynIdlSyn.CharProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '''') then
    begin
      Inc(Run);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynIdlSyn.PreProcessorProc;

  function IsWhiteChar: Boolean;
  begin
    case fLine[Run] of
      #0, #9, #10, #13, #32:
        Result := True;
      else
        Result := False;
    end;
  end;

var
  Directive: string;
begin
  Directive := '';
  while not IsWhiteChar do
  begin
    Directive := Directive + fLine[Run];
    Inc(Run);
  end;
  if (CompareStr(Directive, '#include') = 0) then
    fTokenID := tkPreprocessor
  else if (CompareStr(Directive, '#pragma') = 0) then
    fTokenID := tkPreprocessor
  else
    fTokenID := tkIdentifier;
end; { PreProcessorProc }


constructor TSynIdlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fDatatypeAttri := TSynHighLighterAttributes.Create(SYNS_AttrDatatype, SYNS_FriendlyAttrDatatype);
  fDatatypeAttri.Style := [fsBold];
  fDatatypeAttri.Foreground := clTeal;
  AddAttribute(fDatatypeAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);

  fPreprocessorAttri := TSynHighLighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fPreprocessorAttri.Foreground := clRed;
  AddAttribute(fPreprocessorAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterCORBAIDL;
  fRange := rsUnknown;
end;

procedure TSynIdlSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynIdlSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynIdlSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynIdlSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment: CommentProc;
  else
    begin
      fRange := rsUnknown;
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '/': CommentOpenProc;
        '"': StringOpenProc;
        '''': CharOpenProc;
        '#': PreProcessorProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '0'..'9': NumberProc;
        '-', '+', '*', '\', ',', '.', '[', ']', '{', '}', '<', '>', '(', ')',
        '=', '?', ':', ';' : SymbolProc;
      else
        UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynIdlSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
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

function TSynIdlSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynIdlSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynIdlSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDatatype: Result := fDatatypeAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPreprocessor: Result := fPreprocessorAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynIdlSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(fTokenId);
end;

function TSynIdlSyn.GetSampleSource: string;
begin
  Result := '/* CORBA IDL sample source */'#13#10 +
            '#include <sample.idl>'#13#10 +
            #13#10 +
            'const string TestString = "Hello World";'#13#10 +
            'const long TestLong = 10;'#13#10 +
            #13#10 +
            'module TestModule {'#13#10 +
            '  interface DemoInterface {'#13#10 +
            '    boolean HelloWorld(in string Message);'#13#10 +
            '  }'#13#10 +
            '}';
end;

function TSynIdlSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCORBAIDL;
end;

function TSynIdlSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynIdlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCORBAIDL;
end;

procedure TSynIdlSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynIdlSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynIdlSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynIdlSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangCORBAIDL;
end;

initialization
  RegisterPlaceableHighlighter(TSynIdlSyn);
end.
