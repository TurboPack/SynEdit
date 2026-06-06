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
@abstract(Provides a TOML syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterTOML;

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
  TtkTokenKind = (tkComment, tkSection, tkKey, tkKeyword, tkIdentifier,
    tkString, tkNumber, tkSymbol, tkSpace, tkNull, tkUnknown);

  // rsMLBasic = inside """ ... """, rsMLLiteral = inside ''' ... '''
  TRangeState = (rsUnknown, rsMLBasic, rsMLLiteral);

  TSynTOMLSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FArrayDepth: Integer;
    FInValue: Boolean;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FSectionAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IsAtLineStart: Boolean;
    procedure CommentProc;
    procedure CRProc;
    procedure KeyProc;
    procedure LFProc;
    procedure MLStringProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OpenBracketProc;
    procedure DoubleQuoteProc;
    procedure SingleQuoteProc;
    procedure SpaceProc;
    procedure StringProc(AKind: TtkTokenKind; AEscapes: Boolean);
    procedure SymbolProc;
    procedure UnknownProc;
    procedure ValueIdentProc;
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
    property SectionAttri: TSynHighlighterAttributes read FSectionAttri write FSectionAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write FKeywordAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterTOML = 'TOML Files (*.toml)|*.toml';
  SYNS_LangTOML = 'TOML';
  SYNS_FriendlyLangTOML = 'TOML';

constructor TSynTOMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  FSectionAttri.Foreground := clRed;
  FSectionAttri.Style := [fsBold];
  AddAttribute(FSectionAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FKeyAttri.Foreground := clTeal;
  AddAttribute(FKeyAttri);

  FKeywordAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeywordAttri.Foreground := clNavy;
  FKeywordAttri.Style := [fsBold];
  AddAttribute(FKeywordAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterTOML;
  FRange := rsUnknown;
  FArrayDepth := 0;
  FInValue := False;
end;

destructor TSynTOMLSyn.Destroy;
begin
  inherited Destroy;
end;

function TSynTOMLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_') or (AChar = '-');
end;

function TSynTOMLSyn.IsAtLineStart: Boolean;
var
  I: Integer;
begin
  for I := 0 to Run - 1 do
    if FLine[I] > ' ' then
      Exit(False);
  Result := True;
end;

procedure TSynTOMLSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynTOMLSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynTOMLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynTOMLSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynTOMLSyn.CommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until CharInSet(FLine[Run], [#0, #10, #13]);
end;

procedure TSynTOMLSyn.KeyProc;
begin
  FTokenID := tkKey;
  while CharInSet(FLine[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) do
    Inc(Run);
end;

procedure TSynTOMLSyn.ValueIdentProc;
var
  S: string;
begin
  while CharInSet(FLine[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) do
    Inc(Run);
  SetString(S, FLine + FTokenPos, Run - FTokenPos);
  if (S = 'true') or (S = 'false') or (S = 'inf') or (S = 'nan') then
    FTokenID := tkKeyword
  else
    FTokenID := tkIdentifier;
end;

procedure TSynTOMLSyn.NumberProc;
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
    if (FLine[Run] = '-') and CharInSet(FLine[Run + 1], ['0'..'9']) then
      // RFC 3339 date (and optional T<time>)
      while CharInSet(FLine[Run],
        ['0'..'9', '-', ':', '.', 'T', 't', 'Z', 'z', '+']) do
        Inc(Run)
    else if (FLine[Run] = ':') and CharInSet(FLine[Run + 1], ['0'..'9']) then
      // local time
      while CharInSet(FLine[Run], ['0'..'9', ':', '.', 'Z', 'z', '+', '-']) do
        Inc(Run)
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
end;

procedure TSynTOMLSyn.StringProc(AKind: TtkTokenKind; AEscapes: Boolean);
var
  Quote: WideChar;
begin
  FTokenID := AKind;
  Quote := FLine[Run];
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if AEscapes and (FLine[Run] = '\') then
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

procedure TSynTOMLSyn.MLStringProc;
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
  if FRange = rsMLBasic then
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
    else if (FLine[Run] = Quote) and (FLine[Run + 1] = Quote) and
      (FLine[Run + 2] = Quote) then
    begin
      Inc(Run, 3);
      FRange := rsUnknown;
      Exit;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynTOMLSyn.DoubleQuoteProc;
begin
  if FInValue and (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then
  begin
    FRange := rsMLBasic;
    Inc(Run, 3);
    MLStringProc;
  end
  else if FInValue then
    StringProc(tkString, True)
  else
    StringProc(tkKey, True); // quoted key
end;

procedure TSynTOMLSyn.SingleQuoteProc;
begin
  if FInValue and (FLine[Run + 1] = '''') and (FLine[Run + 2] = '''') then
  begin
    FRange := rsMLLiteral;
    Inc(Run, 3);
    MLStringProc;
  end
  else if FInValue then
    StringProc(tkString, False) // literal string: no escapes
  else
    StringProc(tkKey, False); // quoted (literal) key
end;

procedure TSynTOMLSyn.OpenBracketProc;
begin
  if (FArrayDepth = 0) and (not FInValue) and IsAtLineStart then
  begin
    // table header [name] or array-of-tables header [[name]]
    FTokenID := tkSection;
    Inc(Run);
    if FLine[Run] = '[' then
      Inc(Run);
    while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> ']') do
      Inc(Run);
    if FLine[Run] = ']' then
      Inc(Run);
    if FLine[Run] = ']' then
      Inc(Run);
  end
  else
  begin
    // array literal
    Inc(FArrayDepth);
    FInValue := True;
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynTOMLSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynTOMLSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynTOMLSyn.Next;
begin
  FTokenPos := Run;
  if (Run = 0) and (FRange = rsUnknown) then
    FInValue := FArrayDepth > 0;
  case FRange of
    rsMLBasic, rsMLLiteral: MLStringProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '#': CommentProc;
      '[': OpenBracketProc;
      ']':
        begin
          if FArrayDepth > 0 then
            Dec(FArrayDepth);
          SymbolProc;
        end;
      '{':
        begin
          FInValue := False;
          SymbolProc;
        end;
      '}':
        begin
          FInValue := True;
          SymbolProc;
        end;
      ',':
        begin
          FInValue := FArrayDepth > 0;
          SymbolProc;
        end;
      '=':
        begin
          FInValue := True;
          SymbolProc;
        end;
      '"': DoubleQuoteProc;
      '''': SingleQuoteProc;
      '0'..'9':
        if FInValue then NumberProc else KeyProc;
      'A'..'Z', 'a'..'z', '_':
        if FInValue then ValueIdentProc else KeyProc;
      '.', ';', '(', ')', '+', '-', '*', '/', ':', '@', '~', '<', '>', '?',
      '!', '&', '|', '^', '\': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynTOMLSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeywordAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynTOMLSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynTOMLSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangTOML;
end;

class function TSynTOMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTOML;
end;

function TSynTOMLSyn.GetRange: Pointer;
begin
  Result := Pointer(NativeInt(Ord(FRange)) or (NativeInt(FArrayDepth) shl 8));
end;

function TSynTOMLSyn.GetSampleSource: string;
begin
  Result :=
    '# sample.toml'#13#10 +
    'title = "TOML Example"'#13#10 +
    ''#13#10 +
    '[owner]'#13#10 +
    'name = "Tom"'#13#10 +
    'dob = 1979-05-27T07:32:00Z'#13#10 +
    ''#13#10 +
    '[database]'#13#10 +
    'enabled = true'#13#10 +
    'ports = [ 8000, 8001, 8002 ]'#13#10 +
    'data = [ ["delta", "phi"], [3.14] ]'#13#10 +
    'limits = { cpu = 79.5, mem = 0xFF }'#13#10 +
    ''#13#10 +
    '[servers.alpha]'#13#10 +
    'ip = "10.0.0.1"'#13#10 +
    'note = """'#13#10 +
    'multi-line'#13#10 +
    'basic string"""'#13#10 +
    'path = ''C:\Users\nodejs''';
end;

function TSynTOMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkSection: Result := FSectionAttri;
    tkKey: Result := FKeyAttri;
    tkKeyword: Result := FKeywordAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkString: Result := FStringAttri;
    tkNumber: Result := FNumberAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynTOMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynTOMLSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynTOMLSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterTOML;
end;

procedure TSynTOMLSyn.ResetRange;
begin
  FRange := rsUnknown;
  FArrayDepth := 0;
  FInValue := False;
end;

procedure TSynTOMLSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(NativeInt(Value) and $FF);
  FArrayDepth := NativeInt(Value) shr 8;
end;

initialization
  RegisterPlaceableHighlighter(TSynTOMLSyn);

end.
