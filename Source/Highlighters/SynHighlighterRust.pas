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
@abstract(Provides a Rust language syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterRust;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkBuiltin, tkNull, tkNumber,
    tkSpace, tkString, tkChar, tkLifetime, tkAttribute, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment, rsRawString);

  TSynRustSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FRangeLevel: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FBuiltins: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FBuiltinAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FCharAttri: TSynHighlighterAttributes;
    FLifetimeAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function IsIdentStart(AChar: WideChar): Boolean;
    function RawStringEnd(StartPos, Hashes: Integer): Boolean;
    function TryStartRawString: Boolean;
    procedure BlockCommentProc;
    procedure RawStringProc;
    procedure ByteOrIdentProc;
    procedure CharProc;
    procedure CRProc;
    procedure HashProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RawOrIdentProc;
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
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property BuiltinAttri: TSynHighlighterAttributes read FBuiltinAttri write FBuiltinAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri write FCharAttri;
    property LifetimeAttri: TSynHighlighterAttributes read FLifetimeAttri write FLifetimeAttri;
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri write FAttributeAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterRust = 'Rust Files (*.rs)|*.rs';
  SYNS_LangRust = 'Rust';
  SYNS_FriendlyLangRust = 'Rust';

constructor TSynRustSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText :=
    'as,async,await,break,const,continue,crate,dyn,else,enum,extern,false,fn,' +
    'for,if,impl,in,let,loop,match,mod,move,mut,pub,ref,return,self,Self,static,' +
    'struct,super,trait,true,type,union,unsafe,use,where,while,' +
    // reserved for future use
    'abstract,become,box,do,final,macro,override,priv,try,typeof,unsized,' +
    'virtual,yield';

  FBuiltins := TStringList.Create;
  FBuiltins.CaseSensitive := True;
  FBuiltins.Sorted := True;
  FBuiltins.Duplicates := dupIgnore;
  FBuiltins.CommaText :=
    // primitives
    'bool,char,str,u8,u16,u32,u64,u128,usize,i8,i16,i32,i64,i128,isize,f32,f64,' +
    // common std types
    'String,Vec,VecDeque,Box,Rc,Arc,Cell,RefCell,Mutex,RwLock,Cow,Pin,' +
    'Option,Result,HashMap,HashSet,BTreeMap,BTreeSet,Some,None,Ok,Err,' +
    // common traits
    'Copy,Clone,Debug,Default,PartialEq,Eq,PartialOrd,Ord,Hash,Drop,Sized,' +
    'Send,Sync,Iterator,IntoIterator,DoubleEndedIterator,ExactSizeIterator,' +
    'From,Into,TryFrom,TryInto,AsRef,AsMut,Borrow,BorrowMut,Deref,DerefMut,' +
    'Display,ToString,Fn,FnMut,FnOnce,Add,Sub,Mul,Div,Index,IndexMut';

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clBlue;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FBuiltinAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  FBuiltinAttri.Foreground := clTeal;
  AddAttribute(FBuiltinAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clPurple;
  AddAttribute(FStringAttri);

  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  FCharAttri.Foreground := clPurple;
  AddAttribute(FCharAttri);

  FLifetimeAttri := TSynHighlighterAttributes.Create(SYNS_AttrLabel, SYNS_FriendlyAttrLabel);
  FLifetimeAttri.Foreground := clMaroon;
  FLifetimeAttri.Style := [fsItalic];
  AddAttribute(FLifetimeAttri);

  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FAttributeAttri.Foreground := clOlive;
  AddAttribute(FAttributeAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  AddAttribute(FSymbolAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  FUnknownAttri.Foreground := clRed;
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterRust;
  FRange := rsUnknown;
  FRangeLevel := 0;
end;

destructor TSynRustSyn.Destroy;
begin
  FBuiltins.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynRustSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKey
  else if FBuiltins.IndexOf(S) >= 0 then
    Result := tkBuiltin
  else
    Result := tkIdentifier;
end;

function TSynRustSyn.IsIdentStart(AChar: WideChar): Boolean;
begin
  Result := CharInSet(AChar, ['A'..'Z', 'a'..'z', '_']);
end;

function TSynRustSyn.RawStringEnd(StartPos, Hashes: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Hashes - 1 do
    if FLine[StartPos + I] <> '#' then
    begin
      Result := False;
      Exit;
    end;
end;

procedure TSynRustSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynRustSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynRustSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynRustSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynRustSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  if (FLine[Run] = '!') and CharInSet(FLine[Run + 1], ['(', '[', '{']) then
    FTokenID := tkAttribute
  else
    FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynRustSyn.NumberProc;
begin
  FTokenID := tkNumber;

  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
      Inc(Run);
  end
  else if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['o', 'O']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'7', '_']) do
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

  if CharInSet(FLine[Run], ['a'..'z', 'A'..'Z']) then
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
end;

procedure TSynRustSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        // Line comment (covers //, /// and //!).
        FTokenID := tkComment;
        Inc(Run, 2);
        while not CharInSet(FLine[Run], [#0, #10, #13]) do
          Inc(Run);
      end;
    '*':
      begin
        // Block comment (nested). Covers /*, /** and /*!.
        FRange := rsBlockComment;
        FRangeLevel := 1;
        Inc(Run, 2);
        BlockCommentProc;
      end;
  else
    SymbolProc;
  end;
end;

procedure TSynRustSyn.BlockCommentProc;
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
      Inc(Run, 2);
      Dec(FRangeLevel);
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

function TSynRustSyn.TryStartRawString: Boolean;
var
  P, Hashes: Integer;
begin
  Result := False;
  P := Run + 1;
  Hashes := 0;
  while FLine[P] = '#' do
  begin
    Inc(Hashes);
    Inc(P);
  end;
  if FLine[P] = '"' then
  begin
    FRangeLevel := Hashes;
    FRange := rsRawString;
    Run := P + 1;
    RawStringProc;
    Result := True;
  end;
end;

procedure TSynRustSyn.RawStringProc;
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
    // Raw strings have no escapes; close on '"' followed by FRangeLevel '#'.
    if (FLine[Run] = '"') and RawStringEnd(Run + 1, FRangeLevel) then
    begin
      Inc(Run, 1 + FRangeLevel);
      FRange := rsUnknown;
      FRangeLevel := 0;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynRustSyn.RawOrIdentProc;
begin
  if not TryStartRawString then
    IdentProc;
end;

procedure TSynRustSyn.ByteOrIdentProc;
begin
  case FLine[Run + 1] of
    '"':
      begin
        Inc(Run);
        StringProc;
      end;
    '''':
      begin
        Inc(Run);
        CharProc;
      end;
    'r':
      begin
        Inc(Run);
        if not TryStartRawString then
          IdentProc;
      end;
  else
    IdentProc;
  end;
end;

procedure TSynRustSyn.StringProc;
begin
  FTokenID := tkString;
  Inc(Run);
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

procedure TSynRustSyn.CharProc;
begin
  // Distinguish a char literal ('a', '\n', '\u{1F}') from a lifetime ('a,
  // 'static, '_). A lifetime is a quote followed by an identifier start that
  // is not immediately closed by another quote.
  if (FLine[Run + 1] = '\') then
  begin
    // Escaped char literal.
    FTokenID := tkChar;
    Inc(Run, 2);
    if not CharInSet(FLine[Run], [#0, #10, #13]) then
      Inc(Run);
    while not CharInSet(FLine[Run], [#0, #10, #13, '''']) do
      Inc(Run);
    if FLine[Run] = '''' then
      Inc(Run);
  end
  else if IsIdentStart(FLine[Run + 1]) and (FLine[Run + 2] <> '''') then
  begin
    // Lifetime: 'name
    FTokenID := tkLifetime;
    Inc(Run);
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
  end
  else
  begin
    // Plain char literal: 'x'
    FTokenID := tkChar;
    Inc(Run);
    if not CharInSet(FLine[Run], [#0, #10, #13]) then
      Inc(Run);
    if FLine[Run] = '''' then
      Inc(Run);
  end;
end;

procedure TSynRustSyn.HashProc;
begin
  // Attribute: #[...] or inner attribute #![...]. Coloured to the first ']'.
  if (FLine[Run + 1] = '[') or
     ((FLine[Run + 1] = '!') and (FLine[Run + 2] = '[')) then
  begin
    FTokenID := tkAttribute;
    Inc(Run);
    if FLine[Run] = '!' then
      Inc(Run);
    while not CharInSet(FLine[Run], [#0, #10, #13]) do
    begin
      if FLine[Run] = ']' then
      begin
        Inc(Run);
        Break;
      end;
      Inc(Run);
    end;
  end
  else
    SymbolProc;
end;

procedure TSynRustSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    ':': if FLine[Run + 1] = ':' then Inc(Run);
    '-': if CharInSet(FLine[Run + 1], ['>', '=']) then Inc(Run);
    '=': if CharInSet(FLine[Run + 1], ['=', '>']) then Inc(Run);
    '!': if FLine[Run + 1] = '=' then Inc(Run);
    '+', '*', '%', '^': if FLine[Run + 1] = '=' then Inc(Run);
    '&': if CharInSet(FLine[Run + 1], ['&', '=']) then Inc(Run);
    '|': if CharInSet(FLine[Run + 1], ['|', '=']) then Inc(Run);
    '<':
      if CharInSet(FLine[Run + 1], ['=', '<']) then
      begin
        Inc(Run);
        if (FLine[Run] = '<') and (FLine[Run + 1] = '=') then Inc(Run);
      end;
    '>':
      if CharInSet(FLine[Run + 1], ['=', '>']) then
      begin
        Inc(Run);
        if (FLine[Run] = '>') and (FLine[Run + 1] = '=') then Inc(Run);
      end;
    '.':
      if FLine[Run + 1] = '.' then
      begin
        Inc(Run);
        if CharInSet(FLine[Run + 1], ['=', '.']) then Inc(Run);
      end;
  end;
  Inc(Run);
end;

procedure TSynRustSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

function TSynRustSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynRustSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynRustSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangRust;
end;

class function TSynRustSyn.GetLanguageName: string;
begin
  Result := SYNS_LangRust;
end;

function TSynRustSyn.GetRange: Pointer;
begin
  Result := Pointer(NativeInt(Ord(FRange)) or (NativeInt(FRangeLevel) shl 8));
end;

function TSynRustSyn.GetSampleSource: string;
begin
  Result :=
    '//! Rust syntax highlighting'#13#10 +
    'use std::collections::HashMap;'#13#10 +
    ''#13#10 +
    '/* a /* nested */ block comment */'#13#10 +
    '#[derive(Debug, Clone)]'#13#10 +
    'struct Point<''a> {'#13#10 +
    '    name: &''a str,'#13#10 +
    '    x: f64,'#13#10 +
    '    y: f64,'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'impl<''a> Point<''a> {'#13#10 +
    '    fn dist(&self) -> f64 {'#13#10 +
    '        (self.x * self.x + self.y * self.y).sqrt()'#13#10 +
    '    }'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'fn main() {'#13#10 +
    '    let p = Point { name: "origin", x: 0x1F as f64, y: 3.14_f64 };'#13#10 +
    '    let raw = r#"a "raw" string"#;'#13#10 +
    '    let c = ''\n'';'#13#10 +
    '    println!("{:?} {} {}", p, p.dist(), raw);'#13#10 +
    '}';
end;

function TSynRustSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkBuiltin: Result := FBuiltinAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkLifetime: Result := FLifetimeAttri;
    tkAttribute: Result := FAttributeAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynRustSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynRustSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynRustSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterRust;
end;

function TSynRustSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

procedure TSynRustSyn.Next;
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
      'r': RawOrIdentProc;
      'b': ByteOrIdentProc;
      'A'..'Z', 'a', 'c'..'q', 's'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      '.':
        if CharInSet(FLine[Run + 1], ['0'..'9']) then
          NumberProc
        else
          SymbolProc;
      '/': SlashProc;
      '"': StringProc;
      '''': CharProc;
      '#': HashProc;
      '+', '-', '*', '%', '^', '!', '&', '|', '<', '>', '=', ':', ';', ',',
      '(', ')', '[', ']', '{', '}', '@', '?', '$': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

procedure TSynRustSyn.ResetRange;
begin
  FRange := rsUnknown;
  FRangeLevel := 0;
end;

procedure TSynRustSyn.SetRange(Value: Pointer);
var
  V: NativeInt;
begin
  V := NativeInt(Value);
  FRange := TRangeState(V and $FF);
  FRangeLevel := V shr 8;
end;

initialization
  RegisterPlaceableHighlighter(TSynRustSyn);

end.
