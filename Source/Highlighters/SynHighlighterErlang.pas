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
@abstract(Provides an Erlang syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterErlang;

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
  TtkTokenKind = (tkComment, tkKeyword, tkVariable, tkBif, tkIdentifier,
    tkString, tkChar, tkNumber, tkMacro, tkDirective, tkSymbol, tkSpace,
    tkNull, tkUnknown);

  TRangeState = (rsUnknown, rsString, rsQuotedAtom);

  TSynErlangSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FBifs: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FBifAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FCharAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FMacroAttri: TSynHighlighterAttributes;
    FDirectiveAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function IsAtLineStart: Boolean;
    procedure CharProc;
    procedure CommentProc;
    procedure CRProc;
    procedure DirectiveProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MacroProc;
    procedure NullProc;
    procedure NumberProc;
    procedure QuotedAtomProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure VariableProc;
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
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property BifAttri: TSynHighlighterAttributes read FBifAttri write FBifAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri write FCharAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property MacroAttri: TSynHighlighterAttributes read FMacroAttri write FMacroAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirectiveAttri write FDirectiveAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterErlang = 'Erlang Files (*.erl;*.hrl)|*.erl;*.hrl';
  SYNS_LangErlang = 'Erlang';
  SYNS_FriendlyLangErlang = 'Erlang';

  Keywords: string =
    'after,and,andalso,band,begin,bnot,bor,bsl,bsr,bxor,case,catch,cond,div,' +
    'else,end,fun,if,let,maybe,not,of,or,orelse,query,receive,rem,try,when,xor';

  Bifs: string =
    'abs,alive,apply,atom_to_binary,atom_to_list,binary_to_atom,' +
    'binary_to_existing_atom,binary_to_float,binary_to_integer,' +
    'binary_to_list,binary_to_term,bit_size,byte_size,ceil,check_old_code,' +
    'date,demonitor,element,erase,error,exit,float,float_to_binary,' +
    'float_to_list,floor,garbage_collect,get,get_keys,group_leader,halt,hd,' +
    'integer_to_binary,integer_to_list,iolist_size,iolist_to_binary,is_alive,' +
    'is_atom,is_binary,is_bitstring,is_boolean,is_float,is_function,' +
    'is_integer,is_list,is_map,is_map_key,is_number,is_pid,is_port,' +
    'is_process_alive,is_record,is_reference,is_tuple,length,link,' +
    'list_to_atom,list_to_binary,list_to_existing_atom,list_to_float,' +
    'list_to_integer,list_to_pid,list_to_tuple,make_ref,map_get,map_size,max,' +
    'min,monitor,node,nodes,now,open_port,pid_to_list,port_close,port_command,' +
    'port_connect,process_flag,process_info,processes,put,register,registered,' +
    'round,self,setelement,size,spawn,spawn_link,spawn_monitor,spawn_opt,' +
    'split_binary,statistics,term_to_binary,throw,time,tl,trunc,tuple_size,' +
    'tuple_to_list,unlink,unregister,whereis';

constructor TSynErlangSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText := Keywords;

  FBifs := TStringList.Create;
  FBifs.CaseSensitive := True;
  FBifs.Sorted := True;
  FBifs.Duplicates := dupIgnore;
  FBifs.CommaText := Bifs;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clTeal;
  AddAttribute(FVariableAttri);

  FBifAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  FBifAttri.Foreground := clOlive;
  FBifAttri.Style := [fsBold];
  AddAttribute(FBifAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  FCharAttri.Foreground := clBlue;
  AddAttribute(FCharAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FMacroAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro, SYNS_FriendlyAttrMacro);
  FMacroAttri.Foreground := clPurple;
  FMacroAttri.Style := [fsBold];
  AddAttribute(FMacroAttri);

  FDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FDirectiveAttri.Foreground := clTeal;
  FDirectiveAttri.Style := [fsBold];
  AddAttribute(FDirectiveAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterErlang;
  FRange := rsUnknown;
end;

destructor TSynErlangSyn.Destroy;
begin
  FBifs.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynErlangSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_') or (AChar = '@');
end;

function TSynErlangSyn.IsAtLineStart: Boolean;
var
  I: Integer;
begin
  for I := 0 to Run - 1 do
    if FLine[I] > ' ' then
      Exit(False);
  Result := True;
end;

function TSynErlangSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKeyword
  else if FBifs.IndexOf(S) >= 0 then
    Result := tkBif
  else
    Result := tkIdentifier; // atom
end;

procedure TSynErlangSyn.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until CharInSet(FLine[Run], [#0, #10, #13]);
end;

procedure TSynErlangSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynErlangSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynErlangSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynErlangSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynErlangSyn.VariableProc;
begin
  FTokenID := tkVariable;
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynErlangSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynErlangSyn.NumberProc;
begin
  FTokenID := tkNumber;
  while CharInSet(FLine[Run], ['0'..'9', '_']) do
    Inc(Run);
  if (FLine[Run] = '#') and
    CharInSet(FLine[Run + 1], ['0'..'9', 'a'..'z', 'A'..'Z']) then
  begin
    // Base#Value, e.g. 16#1F or 2#1010
    Inc(Run);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'z', 'A'..'Z', '_']) do
      Inc(Run);
  end
  else
  begin
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
end;

procedure TSynErlangSyn.CharProc;
begin
  // $c character literal, e.g. $a, $\n, $\123 (octal), $\x1F, $\x{1F600}
  FTokenID := tkChar;
  Inc(Run); // '$'
  if FLine[Run] = '\' then
  begin
    Inc(Run);
    if FLine[Run] = 'x' then
    begin
      Inc(Run);
      if FLine[Run] = '{' then
      begin
        while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> '}') do
          Inc(Run);
        if FLine[Run] = '}' then
          Inc(Run);
      end
      else
        while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
          Inc(Run);
    end
    else if CharInSet(FLine[Run], ['0'..'7']) then
      while CharInSet(FLine[Run], ['0'..'7']) do
        Inc(Run)
    else if not CharInSet(FLine[Run], [#0, #10, #13]) then
      Inc(Run);
  end
  else if not CharInSet(FLine[Run], [#0, #10, #13]) then
    Inc(Run);
end;

procedure TSynErlangSyn.MacroProc;
begin
  // ?MACRO / ?MODULE / ??Arg
  FTokenID := tkMacro;
  Inc(Run); // '?'
  if FLine[Run] = '?' then
    Inc(Run);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynErlangSyn.DirectiveProc;
begin
  FTokenID := tkDirective;
  Inc(Run); // '-'
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynErlangSyn.StringProc;
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
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '"' then
    begin
      Inc(Run);
      FRange := rsUnknown;
      Exit;
    end
    else
      Inc(Run);
  end;
  FRange := rsString;
end;

procedure TSynErlangSyn.QuotedAtomProc;
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

  FTokenID := tkIdentifier;
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
      FRange := rsUnknown;
      Exit;
    end
    else
      Inc(Run);
  end;
  FRange := rsQuotedAtom;
end;

procedure TSynErlangSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '-': if CharInSet(FLine[Run + 1], ['>', '-']) then Inc(Run);
    '<': if CharInSet(FLine[Run + 1], ['-', '=', '<']) then Inc(Run);
    '>': if CharInSet(FLine[Run + 1], ['=', '>']) then Inc(Run);
    '/': if FLine[Run + 1] = '=' then Inc(Run);
    '+': if FLine[Run + 1] = '+' then Inc(Run);
    '|': if FLine[Run + 1] = '|' then Inc(Run);
    ':': if CharInSet(FLine[Run + 1], ['=', ':']) then Inc(Run);
    '.': if FLine[Run + 1] = '.' then Inc(Run);
    '=':
      case FLine[Run + 1] of
        '>', '<', '=': Inc(Run);
        ':': if FLine[Run + 2] = '=' then Inc(Run, 2);
        '/': if FLine[Run + 2] = '=' then Inc(Run, 2);
      end;
  end;
  Inc(Run);
end;

procedure TSynErlangSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynErlangSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsString: StringProc;
    rsQuotedAtom: QuotedAtomProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '%': CommentProc;
      'A'..'Z', '_': VariableProc;
      'a'..'z': IdentProc;
      '0'..'9': NumberProc;
      '"':
        begin
          FRange := rsString;
          Inc(Run);
          StringProc;
        end;
      '''':
        begin
          FRange := rsQuotedAtom;
          Inc(Run);
          QuotedAtomProc;
        end;
      '$': CharProc;
      '?': MacroProc;
      '-':
        if IsAtLineStart and CharInSet(FLine[Run + 1], ['a'..'z']) then
          DirectiveProc
        else
          SymbolProc;
      '+', '*', '/', '=', '<', '>', ':', ';', ',', '.', '(', ')', '[', ']',
      '{', '}', '|', '!', '#', '@', '\', '^', '~', '&': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynErlangSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynErlangSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynErlangSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangErlang;
end;

class function TSynErlangSyn.GetLanguageName: string;
begin
  Result := SYNS_LangErlang;
end;

function TSynErlangSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynErlangSyn.GetSampleSource: string;
begin
  Result :=
    '%% sample.erl'#13#10 +
    '-module(fib).'#13#10 +
    '-export([fib/1, start/0]).'#13#10 +
    '-define(MAX, 100).'#13#10 +
    ''#13#10 +
    '-spec fib(non_neg_integer()) -> non_neg_integer().'#13#10 +
    'fib(0) -> 0;'#13#10 +
    'fib(1) -> 1;'#13#10 +
    'fib(N) when N > 1 ->'#13#10 +
    '    fib(N - 1) + fib(N - 2).'#13#10 +
    ''#13#10 +
    'start() ->'#13#10 +
    '    Pid = spawn(fun loop/0),'#13#10 +
    '    Pid ! {self(), $A, 16#1F},'#13#10 +
    '    receive'#13#10 +
    '        {From, Char} -> io:format("got ~p~n", [Char])'#13#10 +
    '    after ?MAX ->'#13#10 +
    '        ok'#13#10 +
    '    end.';
end;

function TSynErlangSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkKeyword: Result := FKeyAttri;
    tkVariable: Result := FVariableAttri;
    tkBif: Result := FBifAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkNumber: Result := FNumberAttri;
    tkMacro: Result := FMacroAttri;
    tkDirective: Result := FDirectiveAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynErlangSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynErlangSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynErlangSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterErlang;
end;

procedure TSynErlangSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynErlangSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynErlangSyn);

end.
