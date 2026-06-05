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
@abstract(Provides a Go language syntax highlighter for SynEdit)
@created(2026-06-03)
The SynHighlighterGo unit provides SynEdit with a Go syntax highlighter.
}

unit SynHighlighterGo;

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
    tkSpace, tkString, tkRune, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment, rsRawString);

  TSynGoSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
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
    FRuneAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure BlockCommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RawStringProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure RuneProc;
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
    property RuneAttri: TSynHighlighterAttributes read FRuneAttri write FRuneAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterGo = 'Go Files (*.go)|*.go';
  SYNS_LangGo = 'Go';
  SYNS_FriendlyLangGo = 'Go';

constructor TSynGoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText := 'break,default,func,interface,select,case,defer,go,map,' +
    'struct,chan,else,goto,package,switch,const,fallthrough,if,range,type,' +
    'continue,for,import,return,var';

  FBuiltins := TStringList.Create;
  FBuiltins.CaseSensitive := True;
  FBuiltins.Sorted := True;
  FBuiltins.Duplicates := dupIgnore;
  FBuiltins.CommaText := 'any,bool,byte,comparable,complex64,complex128,error,float32,' +
    'float64,int,int8,int16,int32,int64,rune,string,uint,uint8,uint16,uint32,' +
    'uint64,uintptr,true,false,iota,nil,append,cap,clear,close,complex,copy,' +
    'delete,imag,len,make,max,min,new,panic,print,println,real,recover';

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

  FRuneAttri := TSynHighlighterAttributes.Create('Rune', 'Rune');
  FRuneAttri.Foreground := clPurple;
  AddAttribute(FRuneAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  AddAttribute(FSymbolAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  FUnknownAttri.Foreground := clRed;
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterGo;
  FRange := rsUnknown;
end;

destructor TSynGoSyn.Destroy;
begin
  FBuiltins.Free;
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynGoSyn.BlockCommentProc;
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
    if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
    begin
      Inc(Run, 2);
      FRange := rsUnknown;
      Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynGoSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

function TSynGoSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynGoSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynGoSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynGoSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynGoSyn.NumberProc;
begin
  FTokenID := tkNumber;

  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X', 'o', 'O', 'b', 'B']) then
    Inc(Run, 2);

  while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
    Inc(Run);

  if FLine[Run] = '.' then
  begin
    Inc(Run);
    while CharInSet(FLine[Run], ['0'..'9', '_']) do
      Inc(Run);
  end;

  if CharInSet(FLine[Run], ['e', 'E', 'p', 'P']) then
  begin
    Inc(Run);
    if CharInSet(FLine[Run], ['+', '-']) then
      Inc(Run);
    while CharInSet(FLine[Run], ['0'..'9', '_']) do
      Inc(Run);
  end;

  if FLine[Run] = 'i' then
    Inc(Run);
end;

procedure TSynGoSyn.RawStringProc;
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
    if FLine[Run] = '`' then
    begin
      Inc(Run);
      FRange := rsUnknown;
      Exit;
    end;
    Inc(Run);
  end;
  FRange := rsRawString;
end;

procedure TSynGoSyn.SlashProc;
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
        Inc(Run, 2);
        BlockCommentProc;
      end;
  else
    SymbolProc;
  end;
end;

procedure TSynGoSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynGoSyn.StringProc;
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

procedure TSynGoSyn.RuneProc;
begin
  FTokenID := tkRune;
  Inc(Run);
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

procedure TSynGoSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    ':': if FLine[Run + 1] = '=' then Inc(Run);
    '+': if CharInSet(FLine[Run + 1], ['+', '=']) then Inc(Run);
    '-': if CharInSet(FLine[Run + 1], ['-', '=', '>']) then Inc(Run);
    '*', '/', '%', '^', '!': if FLine[Run + 1] = '=' then Inc(Run);
    '=': if FLine[Run + 1] = '=' then Inc(Run);
    '<': if CharInSet(FLine[Run + 1], ['<', '=', '-']) then
         begin
           Inc(Run);
           if (FLine[Run] = '<') and (FLine[Run + 1] = '=') then Inc(Run);
         end;
    '>': if CharInSet(FLine[Run + 1], ['>', '=']) then
         begin
           Inc(Run);
           if (FLine[Run] = '>') and (FLine[Run + 1] = '=') then Inc(Run);
         end;
    '&': if CharInSet(FLine[Run + 1], ['&', '^', '=']) then
         begin
           Inc(Run);
           if (FLine[Run] = '^') and (FLine[Run + 1] = '=') then Inc(Run);
         end;
    '|': if CharInSet(FLine[Run + 1], ['|', '=']) then Inc(Run);
    '.': if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then Inc(Run, 2);
  end;
  Inc(Run);
end;

procedure TSynGoSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

function TSynGoSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynGoSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynGoSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangGo;
end;

class function TSynGoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGo;
end;

function TSynGoSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynGoSyn.GetSampleSource: string;
begin
  Result := 'package main'#13#10 +
    ''#13#10 +
    'import "fmt"'#13#10 +
    ''#13#10 +
    'type Person struct {'#13#10 +
    '    Name string'#13#10 +
    '    Age  int'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'func main() {'#13#10 +
    '    // Go syntax highlighting'#13#10 +
    '    raw := `raw string`'#13#10 +
    '    r := ''g'''#13#10 +
    '    fmt.Println(raw, r, 0x2A, 3.14i)'#13#10 +
    '}';
end;

function TSynGoSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkBuiltin: Result := FBuiltinAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkRune: Result := FRuneAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynGoSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynGoSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynGoSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterGo;
end;

function TSynGoSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

procedure TSynGoSyn.Next;
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
      '"': StringProc;
      '''': RuneProc;
      '`':
        begin
          FRange := rsRawString;
          Inc(Run);
          RawStringProc;
        end;
      '+', '-', '*', '%', '&', '|', '^', '<', '>', '=', '!', ':', ';', ',', '.',
      '(', ')', '[', ']', '{', '}': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

procedure TSynGoSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynGoSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynGoSyn);

end.
