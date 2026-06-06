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
@abstract(Provides a Windows Registry (.reg) syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterRegistry;

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
  TtkTokenKind = (tkComment, tkHeader, tkSection, tkRoot, tkValueName,
    tkKeyword, tkString, tkNumber, tkSymbol, tkSpace, tkNull, tkUnknown);

  // rsHexValue = continuing a hex byte list from a previous line's trailing '\'
  TRangeState = (rsUnknown, rsHexValue);

  TSynRegistrySyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FInValue: Boolean;
    FInSection: Boolean;
    FSectionFirst: Boolean;
    FHexCont: Boolean;
    FTokenID: TtkTokenKind;
    FHives: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FHeaderAttri: TSynHighlighterAttributes;
    FSectionAttri: TSynHighlighterAttributes;
    FRootAttri: TSynHighlighterAttributes;
    FValueNameAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IsAtLineStart: Boolean;
    function LineStartsWith(const S: string): Boolean;
    procedure BackslashProc;
    procedure CommentProc;
    procedure CRProc;
    procedure HeaderProc;
    procedure LFProc;
    procedure NullProc;
    procedure SegmentProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure ValueWordProc;
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
    property HeaderAttri: TSynHighlighterAttributes read FHeaderAttri write FHeaderAttri;
    property SectionAttri: TSynHighlighterAttributes read FSectionAttri write FSectionAttri;
    property RootAttri: TSynHighlighterAttributes read FRootAttri write FRootAttri;
    property ValueNameAttri: TSynHighlighterAttributes read FValueNameAttri write FValueNameAttri;
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write FKeywordAttri;
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
  SYNS_FilterRegistry = 'Registry Files (*.reg)|*.reg';
  SYNS_LangRegistry = 'Registry';
  SYNS_FriendlyLangRegistry = 'Windows Registry';

  Hives: string =
    'HKEY_CLASSES_ROOT,HKEY_CURRENT_CONFIG,HKEY_CURRENT_USER,' +
    'HKEY_CURRENT_USER_LOCAL_SETTINGS,HKEY_DYN_DATA,HKEY_LOCAL_MACHINE,' +
    'HKEY_PERFORMANCE_DATA,HKEY_USERS,HKCC,HKCR,HKCU,HKLM,HKU';

constructor TSynRegistrySyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHives := TStringList.Create;
  FHives.CaseSensitive := False;
  FHives.Sorted := True;
  FHives.Duplicates := dupIgnore;
  FHives.CommaText := Hives;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FHeaderAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FHeaderAttri.Foreground := clTeal;
  FHeaderAttri.Style := [fsBold];
  AddAttribute(FHeaderAttri);

  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  FSectionAttri.Foreground := clPurple;
  FSectionAttri.Style := [fsBold];
  AddAttribute(FSectionAttri);

  FRootAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FRootAttri.Foreground := clNavy;
  FRootAttri.Style := [fsBold];
  AddAttribute(FRootAttri);

  FValueNameAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FValueNameAttri.Foreground := clMaroon;
  FValueNameAttri.Style := [fsBold];
  AddAttribute(FValueNameAttri);

  FKeywordAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  FKeywordAttri.Foreground := clOlive;
  FKeywordAttri.Style := [fsBold];
  AddAttribute(FKeywordAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clRed;
  AddAttribute(FNumberAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterRegistry;
  FRange := rsUnknown;
  FInValue := False;
  FInSection := False;
  FSectionFirst := False;
  FHexCont := False;
end;

destructor TSynRegistrySyn.Destroy;
begin
  FHives.Free;
  inherited Destroy;
end;

function TSynRegistrySyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

function TSynRegistrySyn.IsAtLineStart: Boolean;
var
  I: Integer;
begin
  for I := 0 to Run - 1 do
    if FLine[I] > ' ' then
      Exit(False);
  Result := True;
end;

function TSynRegistrySyn.LineStartsWith(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if FLine[Run + I - 1] <> S[I] then
      Exit(False);
  Result := True;
end;

procedure TSynRegistrySyn.CRProc;
begin
  if (FRange = rsHexValue) and not FHexCont then
    FRange := rsUnknown;
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynRegistrySyn.LFProc;
begin
  if (FRange = rsHexValue) and not FHexCont then
    FRange := rsUnknown;
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynRegistrySyn.NullProc;
begin
  if (FRange = rsHexValue) and not FHexCont then
    FRange := rsUnknown;
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynRegistrySyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynRegistrySyn.CommentProc;
begin
  FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynRegistrySyn.HeaderProc;
begin
  FTokenID := tkHeader;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynRegistrySyn.SegmentProc;
var
  S: string;
begin
  while not CharInSet(FLine[Run], [#0, #10, #13, '\', ']']) do
    Inc(Run);
  if FSectionFirst then
  begin
    SetString(S, FLine + FTokenPos, Run - FTokenPos);
    if FHives.IndexOf(S) >= 0 then
      FTokenID := tkRoot
    else
      FTokenID := tkSection;
    FSectionFirst := False;
  end
  else
    FTokenID := tkSection;
end;

procedure TSynRegistrySyn.StringProc;
begin
  // a quoted value name (key position) or a quoted string value (value position)
  if FInValue then
    FTokenID := tkString
  else
    FTokenID := tkValueName;
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

procedure TSynRegistrySyn.ValueWordProc;
var
  S: string;
  I: Integer;
  AllHex: Boolean;
begin
  while CharInSet(FLine[Run], ['0'..'9', 'a'..'z', 'A'..'Z']) do
    Inc(Run);
  SetString(S, FLine + FTokenPos, Run - FTokenPos);
  if SameText(S, 'dword') or SameText(S, 'hex') then
    FTokenID := tkKeyword
  else
  begin
    AllHex := Length(S) > 0;
    for I := 1 to Length(S) do
      if not CharInSet(S[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
      begin
        AllHex := False;
        Break;
      end;
    if AllHex then
      FTokenID := tkNumber
    else
      FTokenID := tkUnknown;
  end;
end;

procedure TSynRegistrySyn.BackslashProc;
var
  I: Integer;
begin
  // a '\' at the end of a value line continues the hex byte list
  FTokenID := tkSymbol;
  I := Run + 1;
  while CharInSet(FLine[I], [#9, ' ']) do
    Inc(I);
  if CharInSet(FLine[I], [#0, #10, #13]) then
  begin
    FRange := rsHexValue;
    FHexCont := True;
  end;
  Inc(Run);
end;

procedure TSynRegistrySyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynRegistrySyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynRegistrySyn.Next;
begin
  FTokenPos := Run;
  if Run = 0 then
  begin
    FHexCont := False;
    if FRange = rsHexValue then
      FInValue := True
    else
    begin
      FInValue := False;
      FInSection := False;
      FSectionFirst := False;
    end;
  end;

  if FInSection then
  begin
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      ']':
        begin
          FTokenID := tkSymbol;
          FInSection := False;
          Inc(Run);
        end;
      '\': SymbolProc;
      '-':
        if FSectionFirst then
          SymbolProc
        else
          SegmentProc;
    else
      SegmentProc;
    end;
  end
  else if (Run = 0) and (LineStartsWith('Windows Registry Editor') or
    LineStartsWith('REGEDIT4')) then
    HeaderProc
  else
  begin
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      ';': CommentProc;
      '[':
        if (not FInValue) and IsAtLineStart then
        begin
          FTokenID := tkSymbol;
          FInSection := True;
          FSectionFirst := True;
          Inc(Run);
        end
        else
          UnknownProc;
      '"': StringProc;
      '@':
        if not FInValue then
        begin
          FTokenID := tkValueName;
          Inc(Run);
        end
        else
          UnknownProc;
      '=':
        begin
          FInValue := True;
          FTokenID := tkSymbol;
          Inc(Run);
        end;
      '0'..'9', 'A'..'Z', 'a'..'z', '_':
        if FInValue then ValueWordProc else UnknownProc;
      ':', '(', ')', ',': SymbolProc;
      '\':
        if FInValue then BackslashProc else UnknownProc;
      '-':
        if FInValue then SymbolProc else UnknownProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynRegistrySyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FValueNameAttri;
    SYN_ATTR_KEYWORD: Result := FKeywordAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynRegistrySyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynRegistrySyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangRegistry;
end;

class function TSynRegistrySyn.GetLanguageName: string;
begin
  Result := SYNS_LangRegistry;
end;

function TSynRegistrySyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynRegistrySyn.GetSampleSource: string;
begin
  Result :=
    'Windows Registry Editor Version 5.00'#13#10 +
    ''#13#10 +
    '; Example registry export'#13#10 +
    ''#13#10 +
    '[HKEY_CURRENT_USER\Software\MyApp]'#13#10 +
    '"DisplayName"="My Application"'#13#10 +
    '@="(default value)"'#13#10 +
    '"Enabled"=dword:00000001'#13#10 +
    '"Timeout"=dword:0000001f'#13#10 +
    '"Data"=hex:00,01,02,03,04,05,06,07,\'#13#10 +
    '  08,09,0a,0b,0c,0d,0e,0f'#13#10 +
    '"Path"=hex(2):25,00,50,00,41,00,54,00,48,00,00,00'#13#10 +
    '"Big"=hex(b):ff,ee,dd,cc,00,00,00,00'#13#10 +
    ''#13#10 +
    '[HKEY_LOCAL_MACHINE\SOFTWARE\Obsolete]'#13#10 +
    '"OldValue"=-'#13#10 +
    ''#13#10 +
    '[-HKEY_CURRENT_USER\Software\ToRemove]';
end;

function TSynRegistrySyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkHeader: Result := FHeaderAttri;
    tkSection: Result := FSectionAttri;
    tkRoot: Result := FRootAttri;
    tkValueName: Result := FValueNameAttri;
    tkKeyword: Result := FKeywordAttri;
    tkString: Result := FStringAttri;
    tkNumber: Result := FNumberAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynRegistrySyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynRegistrySyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynRegistrySyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterRegistry;
end;

procedure TSynRegistrySyn.ResetRange;
begin
  FRange := rsUnknown;
  FInValue := False;
  FInSection := False;
  FSectionFirst := False;
  FHexCont := False;
end;

procedure TSynRegistrySyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynRegistrySyn);

end.
