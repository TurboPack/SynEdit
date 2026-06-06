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
@abstract(Provides a dotenv (.env) syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterENV;

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
  TtkTokenKind = (tkComment, tkKeyword, tkKey, tkValue, tkString, tkVariable,
    tkSymbol, tkSpace, tkNull, tkUnknown);

  // rsDQ = inside a (possibly multi-line) "..." value,
  // rsSQ = inside a (possibly multi-line) '...' value.
  TRangeState = (rsUnknown, rsDQ, rsSQ);

  TSynENVSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FInValue: Boolean;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IsAtLineStart: Boolean;
    procedure CommentProc;
    procedure CRProc;
    procedure DoubleQuoteProc;
    procedure KeyProc;
    procedure LFProc;
    procedure MLStringProc;
    procedure NullProc;
    procedure SingleQuoteProc;
    procedure SpaceProc;
    procedure ValueProc;
    procedure VariableProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
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
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write FKeywordAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterENV = 'Environment Files (*.env;*.env.*)|*.env;*.env.*';
  SYNS_LangENV = 'ENV';
  SYNS_FriendlyLangENV = 'ENV (dotenv)';

constructor TSynENVSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FKeywordAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeywordAttri.Foreground := clPurple;
  FKeywordAttri.Style := [fsBold];
  AddAttribute(FKeywordAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clOlive;
  FVariableAttri.Style := [fsBold];
  AddAttribute(FVariableAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterENV;
  FRange := rsUnknown;
  FInValue := False;
end;

function TSynENVSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

function TSynENVSyn.IsAtLineStart: Boolean;
var
  I: Integer;
begin
  for I := 0 to Run - 1 do
    if FLine[I] > ' ' then
      Exit(False);
  Result := True;
end;

procedure TSynENVSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynENVSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynENVSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynENVSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynENVSyn.CommentProc;
begin
  FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynENVSyn.KeyProc;
var
  S: string;
  AtStart: Boolean;
begin
  AtStart := IsAtLineStart;
  while CharInSet(FLine[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '-']) do
    Inc(Run);
  SetString(S, FLine + FTokenPos, Run - FTokenPos);
  if (S = 'export') and AtStart and CharInSet(FLine[Run], [#9, ' ']) then
    FTokenID := tkKeyword
  else
    FTokenID := tkKey;
end;

procedure TSynENVSyn.ValueProc;
begin
  // an unquoted value run: stops at whitespace, '$' (so a variable reference
  // becomes its own token) or end of line. '#' is NOT a boundary here, so a
  // '#' glued to the value (e.g. pa#ss) stays part of the value.
  FTokenID := tkValue;
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) and
    not IsWhiteChar(FLine[Run]) and (FLine[Run] <> '$') do
    Inc(Run);
end;

procedure TSynENVSyn.VariableProc;
begin
  FTokenID := tkVariable;
  Inc(Run); // '$'
  if FLine[Run] = '{' then
  begin
    Inc(Run);
    while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> '}') do
      Inc(Run);
    if FLine[Run] = '}' then
      Inc(Run);
  end
  else
    while CharInSet(FLine[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(Run);
end;

procedure TSynENVSyn.DoubleQuoteProc;
begin
  FRange := rsDQ;
  Inc(Run); // opening "
  MLStringProc;
end;

procedure TSynENVSyn.SingleQuoteProc;
begin
  FRange := rsSQ;
  Inc(Run); // opening '
  MLStringProc;
end;

procedure TSynENVSyn.MLStringProc;
var
  Quote: WideChar;
  Escapes: Boolean;
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
  if FRange = rsDQ then
  begin
    Quote := '"';
    Escapes := True;
  end
  else
  begin
    Quote := '''';
    Escapes := False;
  end;

  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if Escapes and (FLine[Run] = '\') then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = Quote then
    begin
      Inc(Run);
      FRange := rsUnknown;
      Exit;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynENVSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynENVSyn.Next;
begin
  FTokenPos := Run;
  if (Run = 0) and (FRange = rsUnknown) then
    FInValue := False;
  case FRange of
    rsDQ, rsSQ: MLStringProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '#':
        if (not FInValue) or (Run = 0) or CharInSet(FLine[Run - 1], [#9, ' ']) then
          CommentProc
        else
          ValueProc;
      '"': DoubleQuoteProc;
      '''': SingleQuoteProc;
      '$':
        if CharInSet(FLine[Run + 1], ['{', 'A'..'Z', 'a'..'z', '_']) then
          VariableProc
        else if FInValue then
          ValueProc
        else
          UnknownProc;
      '=':
        if FInValue then
          ValueProc
        else
        begin
          FInValue := True;
          FTokenID := tkSymbol;
          Inc(Run);
        end;
      '0'..'9', 'A'..'Z', 'a'..'z', '_', '.', '-':
        if FInValue then ValueProc else KeyProc;
    else
      if FInValue then ValueProc else UnknownProc;
    end;
  end;
  inherited;
end;

function TSynENVSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FKeyAttri;
    SYN_ATTR_KEYWORD: Result := FKeywordAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynENVSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynENVSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangENV;
end;

class function TSynENVSyn.GetLanguageName: string;
begin
  Result := SYNS_LangENV;
end;

function TSynENVSyn.GetRange: Pointer;
var
  V: NativeInt;
begin
  V := Ord(FRange);
  if FInValue then
    V := V or $100;
  Result := Pointer(V);
end;

function TSynENVSyn.GetSampleSource: string;
begin
  Result :=
    '# .env -- application configuration'#13#10 +
    'NODE_ENV=production'#13#10 +
    'PORT=3000            # inline comment'#13#10 +
    'export DEBUG=false'#13#10 +
    ''#13#10 +
    'APP_NAME="My App"'#13#10 +
    'GREETING="Hello, ${USER}!"'#13#10 +
    'RAW_VALUE=''no $interpolation here'''#13#10 +
    'PASSWORD=p@ss#w0rd   # the # above is literal'#13#10 +
    'DATABASE_URL=postgres://user:pass@localhost:5432/db'#13#10 +
    'BASE_URL=http://${HOST}:${PORT}/api'#13#10 +
    ''#13#10 +
    'PRIVATE_KEY="-----BEGIN KEY-----'#13#10 +
    'multi-line value continues'#13#10 +
    '-----END KEY-----"';
end;

function TSynENVSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkKeyword: Result := FKeywordAttri;
    tkKey: Result := FKeyAttri;
    tkValue: Result := FStringAttri;
    tkString: Result := FStringAttri;
    tkVariable: Result := FVariableAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynENVSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynENVSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynENVSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterENV;
end;

procedure TSynENVSyn.ResetRange;
begin
  FRange := rsUnknown;
  FInValue := False;
end;

procedure TSynENVSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(NativeInt(Value) and $FF);
  FInValue := (NativeInt(Value) and $100) <> 0;
end;

initialization
  RegisterPlaceableHighlighter(TSynENVSyn);

end.
