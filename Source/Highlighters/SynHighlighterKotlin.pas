{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Kotlin syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterKotlin;

{$I SynEdit.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynFunc,
  SynUnicode;

type
  TtkTokenKind = (tkComment, tkKeyword, tkType, tkAnnotation, tkIdentifier,
    tkString, tkChar, tkNumber, tkSymbol, tkSpace, tkNull, tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment, rsRawString);

  TSynKotlinSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FRangeLevel: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FTypes: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FTypeAttri: TSynHighlighterAttributes;
    FAnnotationAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FCharAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AnnotationProc;
    procedure BacktickProc;
    procedure BlockCommentProc;
    procedure CharProc;
    procedure CRProc;
    procedure DoubleQuoteProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RawStringProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property TypeAttri: TSynHighlighterAttributes read FTypeAttri write FTypeAttri;
    property AnnotationAttri: TSynHighlighterAttributes read FAnnotationAttri write FAnnotationAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri write FCharAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterKotlin = 'Kotlin Files (*.kt;*.kts)|*.kt;*.kts';
  SYNS_LangKotlin = 'Kotlin';
  SYNS_FriendlyLangKotlin = 'Kotlin';

  Keywords: string =
    'abstract,actual,annotation,as,break,by,catch,class,companion,const,' +
    'constructor,continue,crossinline,data,do,else,enum,expect,external,false,' +
    'final,finally,for,fun,get,if,import,in,infix,init,inline,inner,interface,' +
    'internal,is,it,lateinit,noinline,null,object,open,operator,out,override,' +
    'package,private,protected,public,reified,return,sealed,set,super,suspend,' +
    'tailrec,this,throw,true,try,typealias,typeof,val,var,vararg,when,where,' +
    'while';

  Types: string =
    'Any,Array,Boolean,BooleanArray,Byte,ByteArray,Char,CharArray,CharSequence,' +
    'Collection,Comparable,Double,DoubleArray,Enum,Exception,Float,FloatArray,' +
    'Function,Int,IntArray,Iterable,Iterator,List,Long,LongArray,Map,' +
    'MutableCollection,MutableIterable,MutableList,MutableMap,MutableSet,' +
    'Nothing,Number,Pair,Result,RuntimeException,Sequence,Set,Short,ShortArray,' +
    'String,StringBuilder,Throwable,Triple,UByte,UInt,ULong,UShort,Unit';

constructor TSynKotlinSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText := Keywords;

  FTypes := TStringList.Create;
  FTypes.CaseSensitive := True;
  FTypes.Sorted := True;
  FTypes.Duplicates := dupIgnore;
  FTypes.CommaText := Types;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  FTypeAttri.Foreground := clTeal;
  FTypeAttri.Style := [fsBold];
  AddAttribute(FTypeAttri);

  FAnnotationAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro, SYNS_FriendlyAttrMacro);
  FAnnotationAttri.Foreground := clOlive;
  AddAttribute(FAnnotationAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  FCharAttri.Foreground := clBlue;
  AddAttribute(FCharAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FIdentifierAttri.Foreground := clWindowText;

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterKotlin;
  FRange := rsUnknown;
  FRangeLevel := 0;
end;

destructor TSynKotlinSyn.Destroy;
begin
  FTypes.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynKotlinSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

function TSynKotlinSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKeyword
  else if FTypes.IndexOf(S) >= 0 then
    Result := tkType
  else
    Result := tkIdentifier;
end;

procedure TSynKotlinSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynKotlinSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynKotlinSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynKotlinSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynKotlinSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynKotlinSyn.AnnotationProc;
begin
  FTokenID := tkAnnotation;
  Inc(Run); // '@'
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynKotlinSyn.BacktickProc;
begin
  // `escaped name` identifier
  FTokenID := tkIdentifier;
  Inc(Run); // opening `
  while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> '`') do
    Inc(Run);
  if FLine[Run] = '`' then
    Inc(Run);
end;

procedure TSynKotlinSyn.NumberProc;
begin
  FTokenID := tkNumber;
  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
      Inc(Run);
  end
  else if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['b', 'B']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0', '1', '_']) do
      Inc(Run);
  end
  else
  begin
    while CharInSet(FLine[Run], ['0'..'9', '_']) do
      Inc(Run);
    if (FLine[Run] = '.') and CharInSet(FLine[Run + 1], ['0'..'9']) then
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9', '_']) do
        Inc(Run);
    end;
    if CharInSet(FLine[Run], ['e', 'E']) then
    begin
      Inc(Run);
      if CharInSet(FLine[Run], ['+', '-']) then
        Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9', '_']) do
        Inc(Run);
    end;
  end;
  // suffixes: u/U (unsigned), then L/l (long) or f/F (float)
  if CharInSet(FLine[Run], ['u', 'U']) then
    Inc(Run);
  if CharInSet(FLine[Run], ['l', 'L', 'f', 'F']) then
    Inc(Run);
end;

procedure TSynKotlinSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        FTokenID := tkComment;
        Inc(Run, 2);
        while not CharInSet(FLine[Run], [#0, #10, #13]) do
          Inc(Run);
      end;
    '*':
      begin
        FRange := rsBlockComment;
        FRangeLevel := 1;
        Inc(Run, 2);
        BlockCommentProc;
      end;
  else
    SymbolProc;
  end;
end;

procedure TSynKotlinSyn.BlockCommentProc;
begin
  if (Run = FTokenPos) and CharInSet(FLine[Run], [#0, #10, #13]) then
  begin
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    end;
    Exit;
  end;

  FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if (FLine[Run] = '/') and (FLine[Run + 1] = '*') then
    begin
      Inc(FRangeLevel);
      Inc(Run, 2);
    end
    else if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
    begin
      if FRangeLevel > 0 then
        Dec(FRangeLevel);
      Inc(Run, 2);
      if FRangeLevel <= 0 then
      begin
        FRange := rsUnknown;
        FRangeLevel := 0;
        Exit;
      end;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynKotlinSyn.DoubleQuoteProc;
begin
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then
  begin
    // triple-quoted raw string (may span lines, no escapes)
    FRange := rsRawString;
    Inc(Run, 3);
    RawStringProc;
  end
  else
    StringProc;
end;

procedure TSynKotlinSyn.StringProc;
begin
  // "..." string; \ escapes; $name / ${expr} templates stay part of the string.
  FTokenID := tkString;
  Inc(Run); // opening "
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '"' then
    begin
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynKotlinSyn.RawStringProc;
begin
  if (Run = FTokenPos) and CharInSet(FLine[Run], [#0, #10, #13]) then
  begin
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    end;
    Exit;
  end;

  FTokenID := tkString;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if (FLine[Run] = '"') and (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then
    begin
      Inc(Run, 3);
      FRange := rsUnknown;
      Exit;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynKotlinSyn.CharProc;
begin
  // 'c' character literal with \ escapes (\n, \u0041, ...)
  FTokenID := tkChar;
  Inc(Run); // opening '
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '''' then
    begin
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynKotlinSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '-': if CharInSet(FLine[Run + 1], ['>', '=']) then Inc(Run);
    '+', '*', '/', '%': if FLine[Run + 1] = '=' then Inc(Run);
    '=':
      if FLine[Run + 1] = '=' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end;
    '!':
      if FLine[Run + 1] = '=' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end
      else if FLine[Run + 1] = '!' then
        Inc(Run);
    '<': if FLine[Run + 1] = '=' then Inc(Run);
    '>': if FLine[Run + 1] = '=' then Inc(Run);
    '&': if FLine[Run + 1] = '&' then Inc(Run);
    '|': if FLine[Run + 1] = '|' then Inc(Run);
    '?': if CharInSet(FLine[Run + 1], ['.', ':']) then Inc(Run);
    ':': if FLine[Run + 1] = ':' then Inc(Run);
    '.':
      if FLine[Run + 1] = '.' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '<' then Inc(Run); // ..<
      end;
  end;
  Inc(Run);
end;

procedure TSynKotlinSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynKotlinSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsBlockComment: BlockCommentProc;
    rsRawString: RawStringProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      '/': SlashProc;
      '"': DoubleQuoteProc;
      '''': CharProc;
      '@': AnnotationProc;
      '`': BacktickProc;
      '+', '-', '*', '%', '=', '<', '>', '!', '&', '|', '^', '~', '?', ':',
      ';', ',', '.', '(', ')', '[', ']', '{', '}': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynKotlinSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynKotlinSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynKotlinSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangKotlin;
end;

class function TSynKotlinSyn.GetLanguageName: string;
begin
  Result := SYNS_LangKotlin;
end;

function TSynKotlinSyn.GetRange: Pointer;
begin
  Result := Pointer(NativeInt(Ord(FRange)) or (NativeInt(FRangeLevel) shl 8));
end;

function TSynKotlinSyn.GetSampleSource: string;
begin
  Result :=
    '// sample.kt'#13#10 +
    'package com.example'#13#10 +
    ''#13#10 +
    'import kotlin.math.sqrt'#13#10 +
    ''#13#10 +
    '/* a /* nested */ comment */'#13#10 +
    '@JvmStatic'#13#10 +
    'data class Point(val x: Int, val y: Double = 0.0) {'#13#10 +
    '    fun distance(): Double {'#13#10 +
    '        val sum = x.toDouble() * x + y * y'#13#10 +
    '        return sqrt(sum)'#13#10 +
    '    }'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'fun main() {'#13#10 +
    '    val p = Point(3, 4.0)'#13#10 +
    '    val ch = ''\n'''#13#10 +
    '    val hex = 0xFFL'#13#10 +
    '    val msg = "distance = ${p.distance()}"'#13#10 +
    '    val raw = """'#13#10 +
    '        multi-line'#13#10 +
    '        $msg'#13#10 +
    '    """'#13#10 +
    '    println(msg)'#13#10 +
    '}';
end;

function TSynKotlinSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkKeyword: Result := FKeyAttri;
    tkType: Result := FTypeAttri;
    tkAnnotation: Result := FAnnotationAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkNumber: Result := FNumberAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynKotlinSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynKotlinSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynKotlinSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterKotlin;
end;

procedure TSynKotlinSyn.ResetRange;
begin
  FRange := rsUnknown;
  FRangeLevel := 0;
end;

procedure TSynKotlinSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(NativeInt(Value) and $FF);
  FRangeLevel := NativeInt(Value) shr 8;
end;

initialization
  RegisterPlaceableHighlighter(TSynKotlinSyn);

end.
