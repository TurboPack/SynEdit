//
// Based on SynHighlighterPas.pas
//
// Using the same license
//
unit SynHighlighterDelphi;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes, System.UITypes, SynEditTypes, SynEditHighlighter,
  SynEditCodeFolding, SynEditStrConst, System.Math, System.RegularExpressions;

type
  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsMultilineString, rsUnKnown);

  TtkTokenKind = (tkSymbol, tkKey, tkAsm, tkComment, tkIdentifier, tkNull, tkNumber,
    tkSpace, tkString, tkUnknown, tkFloat, tkHex, tkDirec, tkChar, tkType);

  TSynDelphiSyn = class(TSynCustomCodeFoldingHighlighter)
  strict private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fAsmStart: Boolean;
    fStringAttri: TSynHighlighterAttributes;
    fCharAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fTypeAttri: TSynHighlighterAttributes;
  strict private class var
    // Regex for Code Folding
    fRE_BlockBegin: TRegEx;
    fRE_BlockEnd: TRegEx;
    fRE_Code: TRegEx;
    fRE_Implementation: TRegEx;

    // Parsers
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;

    function IdentKind(MayBe: PWideChar): TtkTokenKind;
  strict protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
    function IsKeyword(const AKeyword: string): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class constructor Create;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;

    // Code Folding Support
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirecAttri write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri write fFloatAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri write fHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property CharAttri: TSynHighlighterAttributes read fCharAttri write fCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property TypeAttri: TSynHighlighterAttributes read fTypeAttri write fTypeAttri;
  end;

implementation

const
  // SORTED list of keywords for Binary Search (Delphi 13 + Standard Pascal)
  DelphiKeywords: array[0..138] of string = (
    'absolute', 'abstract', 'add', 'and', 'ansistring', 'array', 'as', 'asm',
    'assembler', 'automated', 'begin', 'boolean', 'byte', 'bytebool',
    'cardinal', 'case', 'cdecl', 'class', 'const', 'constructor', 'contains',
    'currency', 'default', 'delayed', 'deprecated', 'destructor', 'dispid',
    'dispinterface', 'div', 'do', 'double', 'downto', 'dynamic', 'else', 'end',
    'except', 'experimental', 'export', 'exports', 'extended', 'external',
    'far', 'file', 'final', 'finalization', 'finally', 'for', 'forward',
    'function', 'goto', 'helper', 'if', 'implementation', 'implements', 'in',
    'index', 'inherited', 'initialization', 'inline', 'int64', 'integer',
    'interface', 'is', 'label', 'library', 'longbool', 'longint', 'longword',
    'message', 'mod', 'name', 'near', 'nil', 'nodefault', 'not', 'object',
    'of', 'on', 'operator', 'or', 'out', 'overload', 'override', 'package',
    'packed', 'pascal', 'platform', 'private', 'procedure', 'program',
    'property', 'protected', 'public', 'published', 'raise', 'read',
    'readonly', 'real48', 'record', 'reference', 'register', 'reintroduce',
    'remove', 'repeat', 'requires', 'resourcestring', 'safecall', 'sealed',
    'set', 'shl', 'shortint', 'shortstring', 'shr', 'single', 'smallint',
    'static', 'stdcall', 'stored', 'strict', 'string', 'stringresource',
    'then', 'threadvar', 'to', 'try', 'type', 'unit', 'unsafe', 'until',
    'uses', 'var', 'virtual', 'while', 'with', 'word', 'wordbool', 'write',
    'writeonly', 'xor'
  );

{ TSynDelphiSyn }

class constructor TSynDelphiSyn.Create;
begin
  // These are now initialized ONCE for the entire application lifetime
  // and are scoped specifically to TSynDelphiSyn.
  FRE_BlockBegin := TRegEx.Create('\b(begin|record|class|case|try)\b', [roIgnoreCase]);
  FRE_BlockEnd := TRegEx.Create('\bend\b', [roIgnoreCase]);
  FRE_Code := TRegEx.Create('^\s*(function|procedure|constructor|destructor)\b', [roIgnoreCase]);
  FRE_Implementation := TRegEx.Create('^implementation\b', [roIgnoreCase]);
end;

constructor TSynDelphiSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fDirecAttri.Style:= [fsItalic];
  AddAttribute(fDirecAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(fFloatAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(fHexAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(fCharAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  AddAttribute(fTypeAttri);

  SetAttributesOnChange(DefHighlightChange);
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterPascal;
end;

class function TSynDelphiSyn.GetLanguageName: string;
begin
  Result := 'Delphi 13';
end;

class function TSynDelphiSyn.GetFriendlyLanguageName: string;
begin
  Result := 'Delphi 13';
end;

class function TSynDelphiSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcCodeFolding];
end;

function TSynDelphiSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

function TSynDelphiSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDelphiSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynDelphiSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynDelphiSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynDelphiSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

function TSynDelphiSyn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkDirec, tkSpace]) then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynDelphiSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirec: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkFloat: Result := fFloatAttri;
    tkHex: Result := fHexAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkChar: Result := fCharAttri;
    tkSymbol: Result := fSymbolAttri;
    tkType: Result := fTypeAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDelphiSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

// =============================================================================
//   Keyword Handling (Binary Search)
// =============================================================================

function TSynDelphiSyn.IsKeyword(const AKeyword: string): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := inherited IsKeyword(AKeyword);
  L := Low(DelphiKeywords);
  H := High(DelphiKeywords);
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareText(DelphiKeywords[I], AKeyword);
    if C = 0 then Exit(True)
    else if C < 0 then L := I + 1
    else H := I - 1;
  end;
end;

function TSynDelphiSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  // Extract identifier from current run position
  // Note: fStringLen is usually calculated in IdentProc
  SetString(S, MayBe, fStringLen);

  if IsKeyword(S) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

// =============================================================================
//   Parsers
// =============================================================================

procedure TSynDelphiSyn.IdentProc;
begin
  fTokenID := tkIdentifier;
  // Calculate length for IdentKind
  fStringLen := 0;
  while IsIdentChar(fLine[Run + fStringLen]) do
    Inc(fStringLen);

  fTokenID := IdentKind(fLine + Run);
  Inc(Run, fStringLen);
end;

procedure TSynDelphiSyn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '@' then Inc(Run);
end;

procedure TSynDelphiSyn.AsciiCharProc;
  function IsAsciiChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '$', 'A'..'F', 'a'..'f': Result := True;
      else Result := False;
    end;
  end;
begin
  fTokenID := tkChar;
  Inc(Run);
  while IsAsciiChar do Inc(Run);
end;

procedure TSynDelphiSyn.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      if fRange in [rsDirective, rsDirectiveAsm] then
        fTokenID := tkDirec
      else
        fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then
        begin
          Inc(Run);
          if fRange in [rsBorAsm, rsDirectiveAsm] then
            fRange := rsAsm
          else
            fRange := rsUnKnown;
          Break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynDelphiSyn.BraceOpenProc;
begin
  if (fLine[Run + 1] = '$') then
  begin
    if fRange = rsAsm then fRange := rsDirectiveAsm else fRange := rsDirective;
  end
  else
  begin
    if fRange = rsAsm then fRange := rsBorAsm else fRange := rsBor;
  end;
  BorProc;
end;

procedure TSynDelphiSyn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynDelphiSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynDelphiSyn.IntegerProc;
  function IsIntegerChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f': Result := True;
      else Result := False;
    end;
  end;
begin
  Inc(Run);
  fTokenID := tkHex;
  while IsIntegerChar do Inc(Run);
end;

procedure TSynDelphiSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynDelphiSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] = '=') or (fLine[Run] = '>') then Inc(Run);
end;

procedure TSynDelphiSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynDelphiSyn.NumberProc;
  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+': Result := True;
      else Result := False;
    end;
  end;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case fLine[Run] of
      '.':
        if fLine[Run + 1] = '.' then Break else fTokenID := tkFloat;
      'e', 'E': fTokenID := tkFloat;
      '-', '+':
        begin
          if fTokenID <> tkFloat then Break;
          if (FLine[Run - 1] <> 'e') and (FLine[Run - 1] <> 'E') then Break;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynDelphiSyn.PointProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if (fLine[Run] = '.') or (fLine[Run - 1] = ')') then Inc(Run);
end;

procedure TSynDelphiSyn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if fRange = rsAnsiAsm then fRange := rsAsm else fRange := rsUnKnown;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynDelphiSyn.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        Inc(Run);
        if fRange = rsAsm then fRange := rsAnsiAsm else fRange := rsAnsi;
        fTokenID := tkComment;
        if not IsLineEnd(Run) then AnsiProc;
      end;
    '.':
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynDelphiSyn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange in [rsProperty, rsExports] then fRange := rsUnknown;
end;

procedure TSynDelphiSyn.SlashProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynDelphiSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynDelphiSyn.StringProc;
begin
  fTokenID := tkString;

  // Check for Multiline String Start (Triple Quote)
  if (fLine[Run] = #39) and (fLine[Run+1] = #39) and (fLine[Run+2] = #39) then
  begin
    fRange := rsMultilineString;
    Inc(Run, 3);
  end
  else if fRange = rsMultilineString then
  begin
    // Continuing a multiline string from previous line
  end
  else
  begin
    // Standard Single Quote String
    Inc(Run);
    while not IsLineEnd(Run) do
    begin
      if fLine[Run] = #39 then begin
        Inc(Run);
        if fLine[Run] <> #39 then Exit; // End of string
      end;
      Inc(Run);
    end;
    Exit;
  end;

  // Handle Multiline Body
  while not IsLineEnd(Run) do
  begin
    if (fLine[Run] = #39) and (fLine[Run+1] = #39) and (fLine[Run+2] = #39) then
    begin
      Inc(Run, 3);
      fRange := rsUnknown; // Reset range
      Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynDelphiSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynDelphiSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDelphiSyn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;

  // Handle Multiline String State
  if fRange = rsMultilineString then
  begin
    StringProc;
    Exit;
  end;

  case fRange of
    rsAnsi, rsAnsiAsm: AnsiProc;
    rsBor, rsBorAsm, rsDirective, rsDirectiveAsm: BorProc;
    else
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '#': AsciiCharProc;
        '$': IntegerProc;
        #39: StringProc;
        '0'..'9': NumberProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '{': BraceOpenProc;
        '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
          begin
            case fLine[Run] of
              '(': RoundOpenProc;
              '.': PointProc;
              ';': SemicolonProc;
              '/': SlashProc;
              ':', '>': ColonOrGreaterProc;
              '<': LowerProc;
              '@': AddressOpProc;
              else SymbolProc;
            end;
          end;
        else
          UnknownProc;
      end;
  end;
  inherited;
end;

// =============================================================================
//   Sample Source
// =============================================================================

function TSynDelphiSyn.GetSampleSource: string;
begin
  Result :=
    'unit ModernDelphi;'#13#10 +
    'interface'#13#10 +
    'type'#13#10 +
    '  TMyRecord = record'#13#10 +
    '    class var Count: Integer;'#13#10 +
    '  end;'#13#10 +
    '  TMyHelper = record helper for TMyRecord'#13#10 +
    '    procedure Save;'#13#10 +
    '  end;'#13#10 +
    'implementation'#13#10 +
    'procedure Test;'#13#10 +
    'var'#13#10 +
    '  JSON: string;'#13#10 +
    'begin'#13#10 +
    '  // Delphi 13 Multiline String'#13#10 +
    '  JSON := '''#13#10 +
    '    {'#13#10 +
    '      "name": "Delphi",'#13#10 +
    '      "version": 13'#13#10 +
    '    }'#13#10 +
    '  '''';'#13#10 +
    'end;'#13#10 +
    'end.';
end;

// =============================================================================
//   Code Folding Support (Simplified for Modern Syntax)
// =============================================================================

procedure TSynDelphiSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: string;
  Line: Integer;

  function IsStartKeyword(const S: string): Boolean;
  begin
    // Simple check for folding start blocks
    Result := fRE_BlockBegin.IsMatch(S);
  end;

  function IsEndKeyword(const S: string): Boolean;
  begin
    Result := fRE_BlockEnd.IsMatch(S);
  end;

begin
  for Line := FromLine to ToLine do
  begin
    CurLine := Trim(LinesToScan[Line]);
    if CurLine = '' then
    begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // Region Folding
    if CurLine.ToUpper.StartsWith('{$REGION') then
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType)
    else if CurLine.ToUpper.StartsWith('{$ENDREGION') then
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType)
    // Implementation section
    else if fRE_Implementation.IsMatch(CurLine) then
      FoldRanges.StartFoldRange(Line + 1, 18) // FT_Implementation
    // Procedure/Function headers
    else if fRE_Code.IsMatch(CurLine) then
      FoldRanges.StartFoldRange(Line + 1, 16) // FT_CodeDeclaration
    // Standard Blocks (begin..end)
    else
    begin
      if IsStartKeyword(CurLine) then
        FoldRanges.StartFoldRange(Line + 1, 1)
      else if IsEndKeyword(CurLine) then
        FoldRanges.StopFoldRange(Line + 1, 1);
    end;
  end;
end;

initialization

finalization

end.
