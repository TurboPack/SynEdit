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
@abstract(Provides a TypeScript syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterTypeScript;

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
  TtkTokenKind = (tkComment, tkBuiltin, tkDecorator, tkIdentifier, tkKey,
    tkNull, tkNumber, tkRegex, tkSpace, tkString, tkSymbol, tkTemplate, tkType,
    tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment, rsTemplate);

  TSynTypeScriptSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FMaybeRegex: Boolean;
    FKeywords: TStringList;
    FTypes: TStringList;
    FBuiltins: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FBuiltinAttri: TSynHighlighterAttributes;
    FDecoratorAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FRegexAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FTemplateAttri: TSynHighlighterAttributes;
    FTypeAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function CurrentTokenIsValueKeyword: Boolean;
    procedure UpdateRegexState;
    procedure BlockCommentProc;
    procedure CRProc;
    procedure DecoratorProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RegexProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure TemplateProc;
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
    property BuiltinAttri: TSynHighlighterAttributes read FBuiltinAttri write FBuiltinAttri;
    property DecoratorAttri: TSynHighlighterAttributes read FDecoratorAttri write FDecoratorAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property RegexAttri: TSynHighlighterAttributes read FRegexAttri write FRegexAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property TemplateAttri: TSynHighlighterAttributes read FTemplateAttri write FTemplateAttri;
    property TypeAttri: TSynHighlighterAttributes read FTypeAttri write FTypeAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterTypeScript = 'TypeScript Files (*.ts;*.tsx;*.mts;*.cts)|*.ts;*.tsx;*.mts;*.cts';
  SYNS_LangTypeScript = 'TypeScript';
  SYNS_FriendlyLangTypeScript = 'TypeScript';

  // Reserved words and contextual keywords (case-sensitive).
  Keywords: string =
    'abstract,accessor,as,asserts,async,await,break,case,catch,class,const,' +
    'continue,debugger,declare,default,delete,do,else,enum,export,extends,' +
    'false,finally,for,from,function,get,global,if,implements,import,in,infer,' +
    'instanceof,interface,is,keyof,let,module,namespace,new,null,of,out,' +
    'override,package,private,protected,public,readonly,require,return,' +
    'satisfies,set,static,super,switch,this,throw,true,try,type,typeof,' +
    'undefined,unique,using,var,void,while,with,yield';

  // TypeScript primitive / special type names.
  Types: string =
    'any,bigint,boolean,never,number,object,string,symbol,unknown';

  // Commonly used global objects/values and TS utility types.
  Builtins: string =
    'Array,ArrayBuffer,BigInt,Boolean,Buffer,DataView,Date,Error,EvalError,' +
    'Function,Infinity,Intl,JSON,Map,Math,NaN,Number,Object,Promise,Proxy,' +
    'RangeError,ReferenceError,Reflect,RegExp,Set,String,Symbol,SyntaxError,' +
    'TypeError,URIError,WeakMap,WeakSet,console,document,exports,globalThis,' +
    'module,process,require,window,__dirname,__filename,clearInterval,' +
    'clearTimeout,decodeURIComponent,encodeURIComponent,isFinite,isNaN,' +
    'parseFloat,parseInt,setInterval,setTimeout,' +
    // common TS utility types
    'Awaited,Exclude,Extract,InstanceType,NonNullable,Omit,Parameters,Partial,' +
    'Pick,Readonly,Record,Required,ReturnType,Uppercase,Lowercase,Capitalize';

constructor TSynTypeScriptSyn.Create(AOwner: TComponent);
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

  FBuiltins := TStringList.Create;
  FBuiltins.CaseSensitive := True;
  FBuiltins.Sorted := True;
  FBuiltins.Duplicates := dupIgnore;
  FBuiltins.CommaText := Builtins;

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

  FBuiltinAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  FBuiltinAttri.Foreground := clOlive;
  AddAttribute(FBuiltinAttri);

  FDecoratorAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro, SYNS_FriendlyAttrMacro);
  FDecoratorAttri.Foreground := clOlive;
  FDecoratorAttri.Style := [fsItalic];
  AddAttribute(FDecoratorAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FTemplateAttri := TSynHighlighterAttributes.Create(SYNS_AttrTemplate, SYNS_FriendlyAttrTemplate);
  FTemplateAttri.Foreground := clTeal;
  AddAttribute(FTemplateAttri);

  FRegexAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable, SYNS_FriendlyAttrSpecialVariable);
  FRegexAttri.Foreground := clMaroon;
  AddAttribute(FRegexAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterTypeScript;
  FRange := rsUnknown;
  FMaybeRegex := True;
end;

destructor TSynTypeScriptSyn.Destroy;
begin
  FBuiltins.Free;
  FTypes.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynTypeScriptSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_') or (AChar = '$');
end;

function TSynTypeScriptSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKey
  else if FTypes.IndexOf(S) >= 0 then
    Result := tkType
  else if FBuiltins.IndexOf(S) >= 0 then
    Result := tkBuiltin
  else
    Result := tkIdentifier;
end;

function TSynTypeScriptSyn.CurrentTokenIsValueKeyword: Boolean;
var
  S: string;
begin
  SetString(S, FLine + FTokenPos, Run - FTokenPos);
  Result := (S = 'this') or (S = 'super') or (S = 'true') or (S = 'false') or
    (S = 'null') or (S = 'undefined');
end;

procedure TSynTypeScriptSyn.UpdateRegexState;
begin
  case FTokenID of
    tkSpace, tkComment, tkNull: ;
    tkIdentifier, tkNumber, tkString, tkTemplate, tkRegex, tkType, tkBuiltin,
    tkDecorator:
      FMaybeRegex := False;
    tkKey:
      FMaybeRegex := not CurrentTokenIsValueKeyword;
    tkSymbol:
      FMaybeRegex := not ((Run > 0) and
        CharInSet(FLine[Run - 1], [')', ']', '}']));
  else
    FMaybeRegex := True;
  end;
end;

procedure TSynTypeScriptSyn.BlockCommentProc;
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
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynTypeScriptSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynTypeScriptSyn.DecoratorProc;
begin
  FTokenID := tkDecorator;
  Inc(Run); // '@'
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynTypeScriptSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynTypeScriptSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynTypeScriptSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynTypeScriptSyn.NumberProc;
begin
  FTokenID := tkNumber;
  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X', 'o', 'O', 'b', 'B']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
      Inc(Run);
  end
  else
  begin
    while CharInSet(FLine[Run], ['0'..'9', '_']) do
      Inc(Run);
    if FLine[Run] = '.' then
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
  if FLine[Run] = 'n' then // BigInt literal
    Inc(Run);
end;

procedure TSynTypeScriptSyn.RegexProc;
var
  InClass: Boolean;
begin
  FTokenID := tkRegex;
  Inc(Run); // opening '/'
  InClass := False;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '[' then
    begin
      InClass := True;
      Inc(Run);
    end
    else if FLine[Run] = ']' then
    begin
      InClass := False;
      Inc(Run);
    end
    else if (FLine[Run] = '/') and not InClass then
    begin
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
  end;
  // flags (g i m s u y d ...)
  while IsCharAlpha(FLine[Run]) do
    Inc(Run);
end;

procedure TSynTypeScriptSyn.SlashProc;
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
    if FMaybeRegex then
      RegexProc
    else
      SymbolProc; // division ( / or /= )
  end;
end;

procedure TSynTypeScriptSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynTypeScriptSyn.StringProc;
var
  Quote: WideChar;
begin
  FTokenID := tkString;
  Quote := FLine[Run];
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = Quote then
    begin
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynTypeScriptSyn.TemplateProc;
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

  FTokenID := tkTemplate;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '`' then
    begin
      Inc(Run);
      FRange := rsUnknown;
      Exit;
    end
    else
      Inc(Run);
  end;
  FRange := rsTemplate;
end;

procedure TSynTypeScriptSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '=':
      if FLine[Run + 1] = '=' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end
      else if FLine[Run + 1] = '>' then
        Inc(Run);
    '!':
      if FLine[Run + 1] = '=' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end;
    '<':
      if FLine[Run + 1] = '=' then Inc(Run)
      else if FLine[Run + 1] = '<' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end;
    '>':
      if FLine[Run + 1] = '=' then Inc(Run)
      else if FLine[Run + 1] = '>' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '>' then Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end;
    '&': if CharInSet(FLine[Run + 1], ['&', '=']) then Inc(Run);
    '|': if CharInSet(FLine[Run + 1], ['|', '=']) then Inc(Run);
    '+': if CharInSet(FLine[Run + 1], ['+', '=']) then Inc(Run);
    '-': if CharInSet(FLine[Run + 1], ['-', '=']) then Inc(Run);
    '*':
      if FLine[Run + 1] = '*' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end
      else if FLine[Run + 1] = '=' then
        Inc(Run);
    '%', '^', '/': if FLine[Run + 1] = '=' then Inc(Run);
    '?':
      if FLine[Run + 1] = '?' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end
      else if FLine[Run + 1] = '.' then
        Inc(Run);
    '.': if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then Inc(Run, 2);
  end;
  Inc(Run);
end;

procedure TSynTypeScriptSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynTypeScriptSyn.Next;
begin
  FTokenPos := Run;
  if Run = 0 then
    FMaybeRegex := True;
  case FRange of
    rsBlockComment: BlockCommentProc;
    rsTemplate: TemplateProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_', '$': IdentProc;
      '0'..'9': NumberProc;
      '/': SlashProc;
      '"', '''': StringProc;
      '`':
        begin
          FRange := rsTemplate;
          Inc(Run);
          TemplateProc;
        end;
      '@': DecoratorProc;
      '+', '-', '*', '%', '=', '<', '>', '!', '&', '|', '^', '~', '?', ':',
      ';', ',', '.', '(', ')', '[', ']', '{', '}': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  UpdateRegexState;
  inherited;
end;

function TSynTypeScriptSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynTypeScriptSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynTypeScriptSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangTypeScript;
end;

class function TSynTypeScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTypeScript;
end;

function TSynTypeScriptSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynTypeScriptSyn.GetSampleSource: string;
begin
  Result :=
    '// TypeScript sample'#13#10 +
    'import { Component } from ''@angular/core'';'#13#10 +
    ''#13#10 +
    'interface Point<T = number> {'#13#10 +
    '  readonly x: T;'#13#10 +
    '  y: T;'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    '@Component({ selector: ''app'' })'#13#10 +
    'export class Greeter {'#13#10 +
    '  private count: number = 0xFF;'#13#10 +
    '  greet(name: string): string {'#13#10 +
    '    const re = /^[a-z]+$/i;'#13#10 +
    '    const big = 10n / 2n;'#13#10 +
    '    return `Hello, ${name}!`; /* done */'#13#10 +
    '  }'#13#10 +
    '}';
end;

function TSynTypeScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkBuiltin: Result := FBuiltinAttri;
    tkDecorator: Result := FDecoratorAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkRegex: Result := FRegexAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkTemplate: Result := FTemplateAttri;
    tkType: Result := FTypeAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynTypeScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynTypeScriptSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynTypeScriptSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterTypeScript;
end;

procedure TSynTypeScriptSyn.ResetRange;
begin
  FRange := rsUnknown;
  FMaybeRegex := True;
end;

procedure TSynTypeScriptSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynTypeScriptSyn);

end.
