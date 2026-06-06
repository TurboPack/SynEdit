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
@abstract(Provides a Lua language syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterLua;

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
    tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsLongString, rsLongComment);

  TSynLuaSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FLongLevel: Integer;
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
    FSymbolAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function LongBracketLevel(StartPos: Integer): Integer;
    function IsLongBracketEnd(StartPos, Level: Integer): Boolean;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LongCommentProc;
    procedure LongStringProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure BracketProc;
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
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterLua = 'Lua Files (*.lua)|*.lua';
  SYNS_LangLua = 'Lua';
  SYNS_FriendlyLangLua = 'Lua';

constructor TSynLuaSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  // Lua reserved words (5.1 - 5.4). 'goto' is 5.2+.
  FKeywords.CommaText := 'and,break,do,else,elseif,end,false,for,function,goto,' +
    'if,in,local,nil,not,or,repeat,return,then,true,until,while';

  FBuiltins := TStringList.Create;
  FBuiltins.CaseSensitive := True;
  FBuiltins.Sorted := True;
  FBuiltins.Duplicates := dupIgnore;
  FBuiltins.CommaText :=
    // special globals
    '_G,_ENV,_VERSION,' +
    // basic functions
    'assert,collectgarbage,dofile,error,getmetatable,ipairs,load,loadfile,' +
    'loadstring,next,pairs,pcall,print,rawequal,rawget,rawlen,rawset,require,' +
    'select,setmetatable,tonumber,tostring,type,unpack,xpcall,gcinfo,' +
    'newproxy,module,setfenv,getfenv,' +
    // library tables
    'coroutine,debug,io,math,os,package,string,table,utf8,bit32,' +
    // common LuaJIT globals
    'bit,ffi,jit';

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

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  AddAttribute(FSymbolAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  FUnknownAttri.Foreground := clRed;
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterLua;
  FRange := rsUnknown;
  FLongLevel := 0;
end;

destructor TSynLuaSyn.Destroy;
begin
  FBuiltins.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynLuaSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

function TSynLuaSyn.LongBracketLevel(StartPos: Integer): Integer;
var
  P: Integer;
begin
  Result := -1;
  if FLine[StartPos] <> '[' then
    Exit;
  P := StartPos + 1;
  while FLine[P] = '=' do
    Inc(P);
  if FLine[P] = '[' then
    Result := P - StartPos - 1;
end;

function TSynLuaSyn.IsLongBracketEnd(StartPos, Level: Integer): Boolean;
var
  I, P: Integer;
begin
  Result := False;
  if FLine[StartPos] <> ']' then
    Exit;
  P := StartPos + 1;
  for I := 1 to Level do
  begin
    if FLine[P] <> '=' then
      Exit;
    Inc(P);
  end;
  Result := FLine[P] = ']';
end;

procedure TSynLuaSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynLuaSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynLuaSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynLuaSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynLuaSyn.NumberProc;
begin
  FTokenID := tkNumber;

  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X']) then
  begin
    // Hexadecimal integer or hex float (0x1A, 0xA.8p-2).
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
      Inc(Run);
    if FLine[Run] = '.' then
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
        Inc(Run);
    end;
    if CharInSet(FLine[Run], ['p', 'P']) then
    begin
      Inc(Run);
      if CharInSet(FLine[Run], ['+', '-']) then
        Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9']) do
        Inc(Run);
    end;
  end
  else
  begin
    // Decimal integer or float (123, 3.14, .5, 2., 1e10, 1.5e-3).
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
    if FLine[Run] = '.' then
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9']) do
        Inc(Run);
    end;
    if CharInSet(FLine[Run], ['e', 'E']) then
    begin
      Inc(Run);
      if CharInSet(FLine[Run], ['+', '-']) then
        Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9']) do
        Inc(Run);
    end;
  end;
end;

procedure TSynLuaSyn.MinusProc;
var
  Level: Integer;
begin
  if FLine[Run + 1] = '-' then
  begin
    // A comment. Consume the leading '--'.
    Inc(Run, 2);
    Level := LongBracketLevel(Run);
    if Level >= 0 then
    begin
      // Long comment: --[[ ... ]] / --[==[ ... ]==]
      FLongLevel := Level;
      FRange := rsLongComment;
      Inc(Run, Level + 2);
      LongCommentProc;
    end
    else
    begin
      // Line comment: -- ...
      FTokenID := tkComment;
      while not CharInSet(FLine[Run], [#0, #10, #13]) do
        Inc(Run);
    end;
  end
  else
    SymbolProc;
end;

procedure TSynLuaSyn.LongCommentProc;
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
    if (FLine[Run] = ']') and IsLongBracketEnd(Run, FLongLevel) then
    begin
      Inc(Run, FLongLevel + 2);
      FRange := rsUnknown;
      FLongLevel := 0;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynLuaSyn.LongStringProc;
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
    if (FLine[Run] = ']') and IsLongBracketEnd(Run, FLongLevel) then
    begin
      Inc(Run, FLongLevel + 2);
      FRange := rsUnknown;
      FLongLevel := 0;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynLuaSyn.BracketProc;
var
  Level: Integer;
begin
  Level := LongBracketLevel(Run);
  if Level >= 0 then
  begin
    // Long string: [[ ... ]] / [==[ ... ]==]
    FLongLevel := Level;
    FRange := rsLongString;
    Inc(Run, Level + 2);
    LongStringProc;
  end
  else
    SymbolProc;
end;

procedure TSynLuaSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynLuaSyn.StringProc;
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

procedure TSynLuaSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '.':
      if FLine[Run + 1] = '.' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '.' then
          Inc(Run);
      end;
    '=': if FLine[Run + 1] = '=' then Inc(Run);
    '~': if FLine[Run + 1] = '=' then Inc(Run);
    '<': if CharInSet(FLine[Run + 1], ['=', '<']) then Inc(Run);
    '>': if CharInSet(FLine[Run + 1], ['=', '>']) then Inc(Run);
    '/': if FLine[Run + 1] = '/' then Inc(Run);
    ':': if FLine[Run + 1] = ':' then Inc(Run);
  end;
  Inc(Run);
end;

procedure TSynLuaSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

function TSynLuaSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynLuaSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynLuaSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangLua;
end;

class function TSynLuaSyn.GetLanguageName: string;
begin
  Result := SYNS_LangLua;
end;

function TSynLuaSyn.GetRange: Pointer;
begin
  Result := Pointer(NativeInt(Ord(FRange)) or (NativeInt(FLongLevel) shl 8));
end;

function TSynLuaSyn.GetSampleSource: string;
begin
  Result :=
    '-- Lua syntax highlighting'#13#10 +
    '--[[ a long'#13#10 +
    '     comment ]]'#13#10 +
    'local Account = {}'#13#10 +
    'Account.__index = Account'#13#10 +
    ''#13#10 +
    'function Account.new(balance)'#13#10 +
    '  local self = setmetatable({}, Account)'#13#10 +
    '  self.balance = balance or 0'#13#10 +
    '  return self'#13#10 +
    'end'#13#10 +
    ''#13#10 +
    'function Account:deposit(v)'#13#10 +
    '  self.balance = self.balance + v'#13#10 +
    'end'#13#10 +
    ''#13#10 +
    'local a = Account.new(100)'#13#10 +
    'a:deposit(0xFF)'#13#10 +
    'local msg = [==[multi-line'#13#10 +
    'long string]==]'#13#10 +
    'print(string.format("%.2f %d %g", a.balance, 42, 3.14e2), msg)';
end;

function TSynLuaSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkBuiltin: Result := FBuiltinAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynLuaSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynLuaSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynLuaSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterLua;
end;

function TSynLuaSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

procedure TSynLuaSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsLongString: LongStringProc;
    rsLongComment: LongCommentProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      '.':
        if CharInSet(FLine[Run + 1], ['0'..'9']) then
          NumberProc
        else
          SymbolProc;
      '-': MinusProc;
      '"', '''': StringProc;
      '[': BracketProc;
      '+', '*', '/', '%', '^', '#', '&', '~', '|', '<', '>', '=',
      '(', ')', '{', '}', ']', ';', ':', ',': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

procedure TSynLuaSyn.ResetRange;
begin
  FRange := rsUnknown;
  FLongLevel := 0;
end;

procedure TSynLuaSyn.SetRange(Value: Pointer);
var
  V: NativeInt;
begin
  V := NativeInt(Value);
  FRange := TRangeState(V and $FF);
  FLongLevel := V shr 8;
end;

initialization
  RegisterPlaceableHighlighter(TSynLuaSyn);

end.
