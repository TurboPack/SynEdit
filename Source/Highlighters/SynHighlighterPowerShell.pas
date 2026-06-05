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
@abstract(Provides a Windows PowerShell language syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterPowerShell;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkCmdlet, tkVariable,
    tkParameter, tkNumber, tkNull, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment, rsHereStringDouble,
    rsHereStringSingle);

  TSynPowerShellSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FOperators: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FCmdletAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FParameterAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function IsIdentStart(AChar: WideChar): Boolean;
    procedure AtProc;
    procedure BackQuoteProc;
    procedure BlockCommentProc;
    procedure CommentProc;
    procedure CRProc;
    procedure DashProc;
    procedure DollarProc;
    procedure HereStringDoubleProc;
    procedure HereStringSingleProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure NumberSuffixProc;
    procedure SingleQuoteProc;
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
    property CmdletAttri: TSynHighlighterAttributes read FCmdletAttri write FCmdletAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property ParameterAttri: TSynHighlighterAttributes read FParameterAttri write FParameterAttri;
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
  SYNS_FilterPowerShell = 'PowerShell Files (*.ps1;*.psm1;*.psd1)|*.ps1;*.psm1;*.psd1';
  SYNS_LangPowerShell = 'PowerShell';
  SYNS_FriendlyLangPowerShell = 'PowerShell';

constructor TSynPowerShellSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := False;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText :=
    'begin,break,catch,class,configuration,continue,data,define,do,' +
    'dynamicparam,else,elseif,end,enum,exit,filter,finally,for,foreach,from,' +
    'function,hidden,if,in,inlinescript,parallel,param,process,return,' +
    'sequence,static,switch,throw,trap,try,until,using,var,while,workflow';

  FOperators := TStringList.Create;
  FOperators.CaseSensitive := False;
  FOperators.Sorted := True;
  FOperators.Duplicates := dupIgnore;
  FOperators.CommaText :=
    'eq,ne,gt,ge,lt,le,like,notlike,match,notmatch,contains,notcontains,in,' +
    'notin,replace,split,join,is,isnot,as,and,or,not,xor,band,bor,bnot,bxor,' +
    'shl,shr,f,' +
    'ceq,cne,cgt,cge,clt,cle,clike,cnotlike,cmatch,cnotmatch,ccontains,' +
    'cnotcontains,cin,cnotin,creplace,csplit,cjoin,cis,cisnot,cas,' +
    'ieq,ine,igt,ige,ilt,ile,ilike,inotlike,imatch,inotmatch,icontains,' +
    'inotcontains,iin,inotin,ireplace,isplit';

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

  FCmdletAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  FCmdletAttri.Foreground := clTeal;
  AddAttribute(FCmdletAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clMaroon;
  AddAttribute(FVariableAttri);

  FParameterAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable, SYNS_FriendlyAttrSpecialVariable);
  FParameterAttri.Foreground := clOlive;
  AddAttribute(FParameterAttri);

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
  FDefaultFilter := SYNS_FilterPowerShell;
  FRange := rsUnknown;
end;

destructor TSynPowerShellSyn.Destroy;
begin
  FOperators.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynPowerShellSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPowerShellSyn.IsIdentStart(AChar: WideChar): Boolean;
begin
  Result := IsCharAlpha(AChar) or (AChar = '_');
end;

function TSynPowerShellSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

procedure TSynPowerShellSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynPowerShellSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynPowerShellSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynPowerShellSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynPowerShellSyn.CommentProc;
begin
  FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynPowerShellSyn.BlockCommentProc;
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
    if (FLine[Run] = '#') and (FLine[Run + 1] = '>') then
    begin
      Inc(Run, 2);
      FRange := rsUnknown;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynPowerShellSyn.HereStringDoubleProc;
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

  if (Run = 0) and (FLine[0] = '"') and (FLine[1] = '@') then
  begin
    Inc(Run, 2);
    FRange := rsUnknown;
    Exit;
  end;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynPowerShellSyn.HereStringSingleProc;
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

  if (Run = 0) and (FLine[0] = '''') and (FLine[1] = '@') then
  begin
    Inc(Run, 2);
    FRange := rsUnknown;
    Exit;
  end;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynPowerShellSyn.LowerProc;
begin
  if FLine[Run + 1] = '#' then
  begin
    FRange := rsBlockComment;
    Inc(Run, 2);
    BlockCommentProc;
  end
  else
    SymbolProc;
end;

procedure TSynPowerShellSyn.AtProc;
begin
  case FLine[Run + 1] of
    '"':
      begin
        FTokenID := tkString;
        FRange := rsHereStringDouble;
        Inc(Run, 2);
        while not CharInSet(FLine[Run], [#0, #10, #13]) do
          Inc(Run);
      end;
    '''':
      begin
        FTokenID := tkString;
        FRange := rsHereStringSingle;
        Inc(Run, 2);
        while not CharInSet(FLine[Run], [#0, #10, #13]) do
          Inc(Run);
      end;
  else
    if IsIdentStart(FLine[Run + 1]) then
    begin
      FTokenID := tkVariable;
      Inc(Run);
      while IsIdentChar(FLine[Run]) do
        Inc(Run);
    end
    else
    begin
      FTokenID := tkSymbol;
      Inc(Run);
    end;
  end;
end;

procedure TSynPowerShellSyn.DollarProc;
begin
  FTokenID := tkVariable;
  Inc(Run);
  case FLine[Run] of
    '{':
      begin
        Inc(Run);
        while not CharInSet(FLine[Run], [#0, #10, #13, '}']) do
          Inc(Run);
        if FLine[Run] = '}' then
          Inc(Run);
      end;
    '(':
      // $( subexpression ) : the '$' is the variable sigil; the parenthesised
      // expression is tokenised as ordinary code.
      ;
    '$', '?', '^':
      Inc(Run);
  else
    begin
      while IsIdentChar(FLine[Run]) do
        Inc(Run);
      if FLine[Run] = ':' then
      begin
        Inc(Run);
        while IsIdentChar(FLine[Run]) do
          Inc(Run);
      end;
    end;
  end;
end;

procedure TSynPowerShellSyn.DashProc;
var
  WStart: Integer;
  W: string;
begin
  if IsIdentStart(FLine[Run + 1]) then
  begin
    Inc(Run);
    WStart := Run;
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
    SetString(W, FLine + WStart, Run - WStart);
    if FOperators.IndexOf(W) >= 0 then
      FTokenID := tkKey
    else
      FTokenID := tkParameter;
  end
  else
  begin
    FTokenID := tkSymbol;
    if CharInSet(FLine[Run + 1], ['-', '=']) then
      Inc(Run);
    Inc(Run);
  end;
end;

procedure TSynPowerShellSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  if (FLine[Run] = '-') and IsIdentStart(FLine[Run + 1]) then
  begin
    repeat
      Inc(Run); // '-'
      while IsIdentChar(FLine[Run]) do
        Inc(Run);
    until not ((FLine[Run] = '-') and IsIdentStart(FLine[Run + 1]));
    FTokenID := tkCmdlet;
  end
  else
    FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynPowerShellSyn.NumberSuffixProc;
begin
  if CharInSet(FLine[Run],
    ['k', 'K', 'm', 'M', 'g', 'G', 't', 'T', 'p', 'P', 'l', 'L', 'd', 'D',
     'u', 'U']) then
    while CharInSet(FLine[Run], ['a'..'z', 'A'..'Z']) do
      Inc(Run);
end;

procedure TSynPowerShellSyn.NumberProc;
begin
  FTokenID := tkNumber;

  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
      Inc(Run);
  end
  else if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['b', 'B']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0', '1']) do
      Inc(Run);
  end
  else
  begin
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
    if (FLine[Run] = '.') and CharInSet(FLine[Run + 1], ['0'..'9']) then
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

  NumberSuffixProc;
end;

procedure TSynPowerShellSyn.SingleQuoteProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '''' then
    begin
      if FLine[Run + 1] = '''' then
        Inc(Run, 2)
      else
      begin
        Inc(Run);
        Break;
      end;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynPowerShellSyn.StringProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '`' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '"' then
    begin
      if FLine[Run + 1] = '"' then
        Inc(Run, 2)
      else
      begin
        Inc(Run);
        Break;
      end;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynPowerShellSyn.BackQuoteProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if not CharInSet(FLine[Run], [#0, #10, #13]) then
    Inc(Run);
end;

procedure TSynPowerShellSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '+': if CharInSet(FLine[Run + 1], ['+', '=']) then Inc(Run);
    '*', '/', '%': if FLine[Run + 1] = '=' then Inc(Run);
    '|': if FLine[Run + 1] = '|' then Inc(Run);
    '&': if FLine[Run + 1] = '&' then Inc(Run);
    '.': if FLine[Run + 1] = '.' then Inc(Run);
    ':': if FLine[Run + 1] = ':' then Inc(Run);
    '>': if FLine[Run + 1] = '>' then Inc(Run);
    '?': if CharInSet(FLine[Run + 1], ['?', '.']) then Inc(Run);
  end;
  Inc(Run);
end;

procedure TSynPowerShellSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

function TSynPowerShellSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynPowerShellSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynPowerShellSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangPowerShell;
end;

class function TSynPowerShellSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPowerShell;
end;

function TSynPowerShellSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynPowerShellSyn.GetSampleSource: string;
begin
  Result :=
    '<#'#13#10 +
    '.SYNOPSIS'#13#10 +
    '  Demo PowerShell script.'#13#10 +
    '#>'#13#10 +
    'function Get-Greeting {'#13#10 +
    '    param('#13#10 +
    '        [string]$Name = "World",'#13#10 +
    '        [int]$Count = 0x2A'#13#10 +
    '    )'#13#10 +
    '    # Build the message'#13#10 +
    '    $msg = "Hello, $Name!`n"'#13#10 +
    '    if ($Name -eq '''' -or $Count -gt 1kb) {'#13#10 +
    '        $msg = @"'#13#10 +
    'a multi-line'#13#10 +
    'here-string'#13#10 +
    '"@'#13#10 +
    '    }'#13#10 +
    '    Write-Output $msg | Out-Host'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'Get-Greeting -Name ''PowerShell'' -Count 5';
end;

function TSynPowerShellSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkCmdlet: Result := FCmdletAttri;
    tkVariable: Result := FVariableAttri;
    tkParameter: Result := FParameterAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynPowerShellSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynPowerShellSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynPowerShellSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterPowerShell;
end;

procedure TSynPowerShellSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsBlockComment: BlockCommentProc;
    rsHereStringDouble: HereStringDoubleProc;
    rsHereStringSingle: HereStringSingleProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '#': CommentProc;
      '<': LowerProc;
      '$': DollarProc;
      '@': AtProc;
      '-': DashProc;
      '`': BackQuoteProc;
      '''': SingleQuoteProc;
      '"': StringProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      '.':
        if CharInSet(FLine[Run + 1], ['0'..'9']) then
          NumberProc
        else
          SymbolProc;
      '+', '*', '/', '%', '|', '&', '=', ':', ';', ',', '(', ')', '{', '}',
      '[', ']', '>', '?', '~', '^', '!': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

procedure TSynPowerShellSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynPowerShellSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynPowerShellSyn);

end.
