unit SynHighlighterWebStencils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Generics.Collections,
  SynEditTypes, SynEditHighlighter, SynUnicode;

type
  TtkTokenKind = (
    tkComment,
    tkDirective, // @
    tkIdentifier,
    tkKey, // @something
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown,
    tkVariable,
    tkBrace, // { and }
    tkOperator,
    tkProperty, // @Entity.Property
    tkDeclaredVar, // @ForEach(var DeclaredVar in ...
    tkStencilParen, // Parentheses that are part of WebStencils syntax
    tkFilename, // Filename in @import statements
    tkImportAlias, // Alias name in @import { alias = value }
    tkImportVar // Variable reference in @import { alias = value }
  );

  TRangeState = (
    rsUnknown,
    rsComment,
    rsForEach, rsForEachVar, rsForEachIn,  // Track ForEach parsing states
    rsIfCondition,  // Track if we're in an if condition
    rsDirectiveFunction, // Track when we're in a directive function call
    rsSwitchExpression, // Track when we're in a switch expression
    rsCaseValue, // Track when we're expecting a case value
    rsImportFilename, // Track when we're expecting a filename after @import
    rsImportAliases // Track when we're in the alias mapping section
  );

  TSynWebStencilsSyn = class(TSynCustomHighlighter)
  private
    FEscapeChar: Char;
    FEscapeDoubling: Boolean;
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FDeclaredVariables: TDictionary<string, Integer>;
    FCurrentScopeLevel: Integer;
    FBraceLevel: Integer;
    FLastIdentifier: string;
    FExpectingCollection: Boolean;  // Track when we expect a collection after "in"
    FParenLevel: Integer; // Track parenthesis nesting level for stencils
    FInStencilExpression: Boolean; // Track if we're in a stencil expression
    FExpectingAssignment: Boolean; // Track if we expect = in import aliases
    FJustParsedImportFilename: Boolean; // Track if we just parsed an import filename

    FCommentAttri: TSynHighlighterAttributes;
    FDirectiveAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FBraceAttri: TSynHighlighterAttributes;
    FOperatorAttri: TSynHighlighterAttributes;
    FPropertyAttri: TSynHighlighterAttributes;
    FDeclaredVarAttri: TSynHighlighterAttributes;
    FStencilParenAttri: TSynHighlighterAttributes;
    FFilenameAttri: TSynHighlighterAttributes;
    FImportAliasAttri: TSynHighlighterAttributes;
    FImportVarAttri: TSynHighlighterAttributes;

    function IsKeyword(const AKeyword: string): Boolean;
    function IsDeclaredVariable(const AName: string): Boolean;
    procedure InitKeywords;
    procedure CleanupVariablesForScope(AScope: Integer);

    procedure CommentProc;
    procedure HashCommentProc;
    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure IdentProc;
    procedure NumberProc;
    procedure DirectiveProc;
    procedure BraceOpenProc;
    procedure BraceCloseProc;
    procedure SymbolProc;
    procedure DotProc;
    procedure ParenOpenProc;
    procedure ParenCloseProc;

    function IsFilterStored: Boolean; override;
  protected
    function GetSampleSource: UnicodeString; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;

    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;

    procedure Next; override;

  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirectiveAttri write FDirectiveAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property BraceAttri: TSynHighlighterAttributes read FBraceAttri write FBraceAttri;
    property OperatorAttri: TSynHighlighterAttributes read FOperatorAttri write FOperatorAttri;
    property PropertyAttri: TSynHighlighterAttributes read FPropertyAttri write FPropertyAttri;
    property DeclaredVarAttri: TSynHighlighterAttributes read FDeclaredVarAttri write FDeclaredVarAttri;
    property StencilParenAttri: TSynHighlighterAttributes read FStencilParenAttri write FStencilParenAttri;
    property FilenameAttri: TSynHighlighterAttributes read FFilenameAttri write FFilenameAttri;
    property ImportAliasAttri: TSynHighlighterAttributes read FImportAliasAttri write FImportAliasAttri;
    property ImportVarAttri: TSynHighlighterAttributes read FImportVarAttri write FImportVarAttri;

    property EscapeChar: Char read FEscapeChar write FEscapeChar;
    property EscapeDoubling: Boolean read FEscapeDoubling write FEscapeDoubling;
  end;

implementation

uses
  SynEditStrConst;

{ TSynWebStencilsSyn }

constructor TSynWebStencilsSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEscapeChar := '\';
  FEscapeDoubling := True;
  FCaseSensitive := False;
  FBraceLevel := 0;
  FCurrentScopeLevel := 0;
  FExpectingCollection := False;
  FExpectingAssignment := False;
  FJustParsedImportFilename := False;
  FParenLevel := 0;
  FInStencilExpression := False;
  FLastIdentifier := '';
  FRange := rsUnknown;
  FTokenID := tkUnknown;

  FKeywords := TStringList.Create;
  FKeywords.Sorted := True;
  FKeywords.CaseSensitive := False;
  InitKeywords;

  FDeclaredVariables := TDictionary<string, Integer>.Create;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);

  FDirectiveAttri := TSynHighlighterAttributes.Create('Directive', 'Directive');
  FDirectiveAttri.Style := [fsBold];
  FDirectiveAttri.Foreground := clBlue;
  AddAttribute(FDirectiveAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clNavy;
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clRed;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := clGray;
  AddAttribute(FSymbolAttri);

  FVariableAttri := TSynHighlighterAttributes.Create('Variable', 'Variable');
  FVariableAttri.Foreground := clPurple;
  FVariableAttri.Style := [];
  AddAttribute(FVariableAttri);

  FBraceAttri := TSynHighlighterAttributes.Create('Brace', 'Template Brace');
  FBraceAttri.Foreground := clMaroon;
  FBraceAttri.Style := [fsBold];
  AddAttribute(FBraceAttri);

  FOperatorAttri := TSynHighlighterAttributes.Create('Operator', 'Operator');
  FOperatorAttri.Foreground := clMaroon;
  AddAttribute(FOperatorAttri);

  FPropertyAttri := TSynHighlighterAttributes.Create('Property', 'Property');
  FPropertyAttri.Foreground := clTeal;
  AddAttribute(FPropertyAttri);

  FDeclaredVarAttri := TSynHighlighterAttributes.Create('DeclaredVar', 'Declared Variable');
  FDeclaredVarAttri.Foreground := clOlive;
  FDeclaredVarAttri.Style := [fsBold];
  AddAttribute(FDeclaredVarAttri);

  FStencilParenAttri := TSynHighlighterAttributes.Create('StencilParen', 'Stencil Parenthesis');
  FStencilParenAttri.Foreground := clBlue;
  FStencilParenAttri.Style := [fsBold];
  AddAttribute(FStencilParenAttri);

  FFilenameAttri := TSynHighlighterAttributes.Create('Filename', 'Filename');
  FFilenameAttri.Foreground := clOlive;
  FFilenameAttri.Style := [fsUnderline];
  AddAttribute(FFilenameAttri);

  FImportAliasAttri := TSynHighlighterAttributes.Create('ImportAlias', 'Import Alias Name');
  FImportAliasAttri.Foreground := clFuchsia;
  FImportAliasAttri.Style := [fsBold];
  AddAttribute(FImportAliasAttri);

  FImportVarAttri := TSynHighlighterAttributes.Create('ImportVar', 'Import Variable Reference');
  FImportVarAttri.Foreground := clPurple;
  FImportVarAttri.Style := [fsItalic];
  AddAttribute(FImportVarAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := '*.stencil;*.tpl;*.ini';
end;

destructor TSynWebStencilsSyn.Destroy;
begin
  FKeywords.Free;
  FDeclaredVariables.Free;
  inherited;
end;

procedure TSynWebStencilsSyn.InitKeywords;
begin
  FKeywords.Clear;
  // WebStencils keywords (only recognized in stencil context)
  FKeywords.Add('if');
  FKeywords.Add('else');
  FKeywords.Add('foreach');
  FKeywords.Add('for');
  FKeywords.Add('in');
  FKeywords.Add('var');
  FKeywords.Add('not');
  FKeywords.Add('and');
  FKeywords.Add('or');
  FKeywords.Add('true');
  FKeywords.Add('false');
  FKeywords.Add('null');
  FKeywords.Add('split');
  FKeywords.Add('trim');
  FKeywords.Add('range');
  FKeywords.Add('eof');
  FKeywords.Add('switch');
  FKeywords.Add('case');
  FKeywords.Add('default');
  FKeywords.Add('import');
end;

function TSynWebStencilsSyn.IsKeyword(const AKeyword: string): Boolean;
begin
  Result := FKeywords.IndexOf(LowerCase(AKeyword)) >= 0;
end;

function TSynWebStencilsSyn.IsDeclaredVariable(const AName: string): Boolean;
begin
  Result := FDeclaredVariables.ContainsKey(LowerCase(AName));
end;

procedure TSynWebStencilsSyn.CleanupVariablesForScope(AScope: Integer);
var
  Pair: TPair<string, Integer>;
  ToRemove: TList<string>;
begin
  ToRemove := TList<string>.Create;
  try
    for Pair in FDeclaredVariables do
    begin
      if Pair.Value >= AScope then
        ToRemove.Add(Pair.Key);
    end;

    for var Key in ToRemove do
      FDeclaredVariables.Remove(Key);
  finally
    ToRemove.Free;
  end;
end;

procedure TSynWebStencilsSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  // Reset state for new line BEFORE calling inherited
  FLastIdentifier := '';
  FExpectingCollection := False;
  FExpectingAssignment := False;
  FJustParsedImportFilename := False;
  FInStencilExpression := False;

  // Preserve ForEach, If, and Switch state if we're in the middle of parsing one
  if not (FRange in [rsForEach, rsForEachVar, rsForEachIn, rsIfCondition,
                      rsDirectiveFunction, rsSwitchExpression, rsCaseValue,
                      rsImportFilename, rsImportAliases]) then
  begin
    FRange := rsUnknown;
    FParenLevel := 0;
  end;

  // Now call inherited which will call DoSetLine and then Next
  inherited;
end;

procedure TSynWebStencilsSyn.SpaceProc;
begin
  FTokenID := tkSpace;

  // Clear the import filename flag when we hit spaces after other tokens
  if FJustParsedImportFilename and (FRange <> rsImportAliases) then
    FJustParsedImportFilename := False;

  repeat
    Inc(Run);
  until (Run > FLineLen) or (FLine[Run] > #32);
end;

procedure TSynWebStencilsSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynWebStencilsSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if (Run <= FLineLen) and (FLine[Run] = #10) then
    Inc(Run);
end;

procedure TSynWebStencilsSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynWebStencilsSyn.CommentProc;
begin
  // WebStencils block comment @* ... *@
  if (Run <= FLineLen) and (FLine[Run] = '@') and
     (Run + 1 <= FLineLen) and (FLine[Run + 1] = '*') then
  begin
    FTokenID := tkComment;
    FRange := rsComment;
    Inc(Run, 2);

    while Run <= FLineLen do
    begin
      if (FLine[Run] = '*') and (Run + 1 <= FLineLen) and (FLine[Run + 1] = '@') then
      begin
        Inc(Run, 2);
        FRange := rsUnknown;
        Break;
      end;
      Inc(Run);
    end;
  end
  else
    DirectiveProc;
end;

procedure TSynWebStencilsSyn.HashCommentProc;
begin
  FTokenID := tkComment;
  Inc(Run); // Skip #

  // Read rest of line as comment
  while (Run <= FLineLen) and not CharInSet(FLine[Run], [#10, #13]) do
    Inc(Run);
end;

procedure TSynWebStencilsSyn.StringProc;
var
  QuoteChar: WideChar;
begin
  // Check if this is a quoted filename in an import statement
  if FRange = rsImportFilename then
  begin
    FTokenID := tkFilename;
    QuoteChar := FLine[Run];
    Inc(Run);

    while Run <= FLineLen do
    begin
      // Check for escape character
      if (FEscapeChar <> #0) and (FLine[Run] = FEscapeChar) then
      begin
        Inc(Run);
        if Run <= FLineLen then
          Inc(Run);
      end
      // Check for closing quote
      else if FLine[Run] = QuoteChar then
      begin
        Inc(Run);
        Break;
      end
      else
        Inc(Run);
    end;

    // Mark that we just parsed an import filename
    FJustParsedImportFilename := True;
    FRange := rsImportAliases;
    Exit;
  end;

  // Regular string processing
  FTokenID := tkString;
  QuoteChar := FLine[Run];

  // If we're in a case value context, this is the case value
  if FRange = rsCaseValue then
  begin
    FRange := rsUnknown;
    FInStencilExpression := False;
  end;

  Inc(Run);
  while Run <= FLineLen do
  begin
    // Check for escape character
    if (FEscapeChar <> #0) and (FLine[Run] = FEscapeChar) then
    begin
      Inc(Run);
      if Run <= FLineLen then
        Inc(Run);
    end
    // Check for closing quote
    else if FLine[Run] = QuoteChar then
    begin
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynWebStencilsSyn.DirectiveProc;
var
  StartPos: Integer;
  DirectiveName: string;
  IsFunction: Boolean;
begin
  FTokenID := tkDirective;
  Inc(Run); // Skip @

  // Check for @@ escape
  if (Run <= FLineLen) and FEscapeDoubling and (FLine[Run] = '@') then
  begin
    FTokenID := tkSymbol;
    Inc(Run);
    Exit;
  end;

  // Capture the directive/variable name
  StartPos := Run;
  while (Run <= FLineLen) and IsIdentChar(FLine[Run]) do
    Inc(Run);

  if StartPos < Run then
  begin
    SetString(DirectiveName, PChar(@FLine[StartPos]), Run - StartPos);
    FLastIdentifier := DirectiveName;  // Store for later reference

    // Check if followed by parenthesis (function call)
    IsFunction := (Run <= FLineLen) and (FLine[Run] = '(');

    // Check for special directives/keywords that introduce constructs
    if SameText(DirectiveName, 'ForEach') or SameText(DirectiveName, 'For') then
    begin
      FTokenID := tkDirective;
      FInStencilExpression := True;
      if IsFunction then
        FRange := rsDirectiveFunction
      else
      begin
        // ForEach without parentheses - next token should be the variable name
        FRange := rsForEachIn;  // Skip directly to expecting variable name
        FExpectingCollection := False;
      end;
    end
    else if SameText(DirectiveName, 'import') then
    begin
      FTokenID := tkDirective;
      FRange := rsImportFilename;  // Expecting filename next
      FInStencilExpression := False;
    end
    else if SameText(DirectiveName, 'if') then
    begin
      FTokenID := tkDirective;
      FInStencilExpression := True;
      if IsFunction then
        FRange := rsDirectiveFunction
      else
        FRange := rsIfCondition;
    end
    else if SameText(DirectiveName, 'else') then
    begin
      FTokenID := tkDirective;
      FInStencilExpression := False;
    end
    else if SameText(DirectiveName, 'switch') then
    begin
      FTokenID := tkDirective;
      FInStencilExpression := True;
      if IsFunction then
      begin
        FRange := rsDirectiveFunction;
        // We'll transition to rsSwitchExpression after the opening paren
      end
      else
      begin
        // Switch without parentheses - next should be the expression
        FRange := rsSwitchExpression;
      end;
    end
    else if SameText(DirectiveName, 'case') then
    begin
      FTokenID := tkDirective;
      FInStencilExpression := True;
      FRange := rsCaseValue;  // Expecting a value after case
    end
    else if SameText(DirectiveName, 'default') then
    begin
      FTokenID := tkDirective;
      FInStencilExpression := False;
      FRange := rsUnknown;
    end
    else if IsFunction then
    begin
      // Function calls like @Split(), @Trim(), etc.
      FTokenID := tkDirective;
      FRange := rsDirectiveFunction;
      FInStencilExpression := True;
    end
    else if IsKeyword(DirectiveName) then
    begin
      // Other keywords used with @ prefix
      FTokenID := tkKey;
      FInStencilExpression := False;
    end
    else
    begin
      // Everything else after @ is a variable or entity
      // Check if it's a declared variable from ForEach
      if IsDeclaredVariable(DirectiveName) and
         not SameText('current', DirectiveName) and
         not SameText('index', DirectiveName) then
        FTokenID := tkDeclaredVar
      else
        FTokenID := tkVariable;

      // Variables might have property access
      FInStencilExpression := True;
    end;
  end;
end;

procedure TSynWebStencilsSyn.DotProc;
begin
  // Dot in a stencil expression is part of property access syntax
  if FInStencilExpression or (FRange = rsImportAliases) then
  begin
    FTokenID := tkSymbol;  // Or could be tkOperator for different highlighting
    Inc(Run);
    // Keep FInStencilExpression = True for the following property name
  end
  else
  begin
    // Regular dot in template text
    FTokenID := tkIdentifier;
    Inc(Run);
    FInStencilExpression := False;
  end;
end;

procedure TSynWebStencilsSyn.ParenOpenProc;
begin
  // Check if we're right after a directive that expects parentheses
  if (FRange = rsDirectiveFunction) or
     (FInStencilExpression and (FRange = rsForEach)) then
  begin
    FTokenID := tkStencilParen;
    Inc(FParenLevel);

    // Handle specific directive transitions
    if FRange = rsForEach then
      FRange := rsForEachVar
    else if FRange = rsDirectiveFunction then
    begin
      // Check what directive we're processing
      if SameText(FLastIdentifier, 'foreach') or SameText(FLastIdentifier, 'for') then
        FRange := rsForEachVar
      else if SameText(FLastIdentifier, 'switch') then
        FRange := rsSwitchExpression
      else if SameText(FLastIdentifier, 'if') then
        FRange := rsIfCondition;
      // Otherwise keep rsDirectiveFunction for other functions
    end;
  end
  else if (FParenLevel > 0) then
  begin
    // We're inside stencil parentheses already
    FTokenID := tkStencilParen;
    Inc(FParenLevel);
  end
  else
  begin
    // Regular parenthesis in template text - just plain text
    FTokenID := tkIdentifier;
    FInStencilExpression := False;  // Parenthesis breaks stencil expression
  end;

  Inc(Run);
end;

procedure TSynWebStencilsSyn.ParenCloseProc;
begin
  // Check if this is a stencil parenthesis
  if (FParenLevel > 0) or FInStencilExpression then
  begin
    FTokenID := tkStencilParen;
    if FParenLevel > 0 then
      Dec(FParenLevel);

    // Closing ForEach or directive function parameters
    if FParenLevel = 0 then
    begin
      if FRange in [rsForEachVar, rsForEachIn, rsDirectiveFunction] then
      begin
        FRange := rsUnknown;
        FInStencilExpression := False;
      end;
    end;
  end
  else
  begin
    // Regular parenthesis in template text
    FTokenID := tkSymbol;
  end;

  Inc(Run);
end;

procedure TSynWebStencilsSyn.BraceOpenProc;
begin
  // Check if this is the opening brace for import aliases
  // Only if we just parsed an import filename
  if FJustParsedImportFilename then
  begin
    FTokenID := tkBrace;
    FRange := rsImportAliases;
    FExpectingAssignment := False;
    FJustParsedImportFilename := False; // Reset flag
    Inc(Run);
    Exit;
  end;

  FTokenID := tkBrace;
  Inc(FBraceLevel);
  Inc(FCurrentScopeLevel);

  // Reset ForEach, If, Switch, and Case state when entering code block
  if FRange in [rsForEach, rsForEachVar, rsForEachIn, rsIfCondition,
                rsSwitchExpression, rsCaseValue, rsImportAliases] then
    FRange := rsUnknown;

  FInStencilExpression := False;
  FParenLevel := 0;
  FJustParsedImportFilename := False; // Clear flag

  Inc(Run);
end;

procedure TSynWebStencilsSyn.BraceCloseProc;
begin
  // Check if this is the closing brace for import aliases
  if FRange = rsImportAliases then
  begin
    FTokenID := tkBrace;
    FRange := rsUnknown;
    FExpectingAssignment := False;
    FInStencilExpression := False;  // Reset stencil expression state
    Inc(Run);
    Exit;
  end;

  FTokenID := tkBrace;
  if FBraceLevel > 0 then
  begin
    Dec(FBraceLevel);
    CleanupVariablesForScope(FCurrentScopeLevel);
    if FCurrentScopeLevel > 0 then
      Dec(FCurrentScopeLevel);
  end;
  Inc(Run);
end;

procedure TSynWebStencilsSyn.SymbolProc;
begin
  // Handle parentheses specially
  case FLine[Run] of
    '(': ParenOpenProc;
    ')': ParenCloseProc;
    '=':
      begin
        // In import aliases, = is an assignment operator
        if FRange = rsImportAliases then
        begin
          FTokenID := tkOperator;
          FExpectingAssignment := True;  // Next identifier is the value
          Inc(Run);
        end
        else if (FParenLevel > 0) or FInStencilExpression then
        begin
          FTokenID := tkOperator;
          Inc(Run);
          // Check for ==
          if (Run <= FLineLen) and (FLine[Run] = '=') then
            Inc(Run);
        end
        else
        begin
          FTokenID := tkSymbol;
          Inc(Run);
        end;
      end;
    ',':
      begin
        // Comma is special in import aliases - separates alias pairs
        if FRange = rsImportAliases then
        begin
          FTokenID := tkSymbol;
          FExpectingAssignment := False;  // Reset for next alias pair
          FInStencilExpression := False;  // Reset stencil expression state
          Inc(Run);
        end
        else if (FParenLevel > 0) then
        begin
          FTokenID := tkSymbol;
          Inc(Run);
        end
        else
        begin
          FTokenID := tkIdentifier;
          FInStencilExpression := False;
          Inc(Run);
        end;
      end;
    '<', '>', '!', '+', '-', '*', '/', '%', '|', '&':
      begin
        // Only treat as operator if we're in stencil context
        if (FParenLevel > 0) or FInStencilExpression then
          FTokenID := tkOperator
        else
          FTokenID := tkSymbol;
        Inc(Run);
        // Check for two-character operators
        if (FTokenID = tkOperator) and (Run <= FLineLen) then
        begin
          case FLine[Run - 1] of
            '<': if FLine[Run] = '=' then Inc(Run); // <=
            '>': if FLine[Run] = '=' then Inc(Run); // >=
            '!': if FLine[Run] = '=' then Inc(Run); // !=
            '|': if FLine[Run] = '|' then Inc(Run); // ||
            '&': if FLine[Run] = '&' then Inc(Run); // &&
          end;
        end;
      end;
    ';', ':', '?', '[', ']', '^':
      begin
        // Only treat as symbol in stencil context
        if (FParenLevel > 0) then
          FTokenID := tkSymbol
        else
        begin
          // Regular character in template text
          FTokenID := tkIdentifier;
          FInStencilExpression := False;
        end;
        Inc(Run);
      end;
  else
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynWebStencilsSyn.IdentProc;
var
  StartPos: Integer;
  TempStr: string;
  InStencilContext: Boolean;
  IsPrecedingDot: Boolean;
begin
  StartPos := Run;

  // Check if preceded by a dot (property access)
  IsPrecedingDot := (StartPos > 1) and (FLine[StartPos - 1] = '.') and FInStencilExpression;

  // ONLY handle filenames if we're in the rsImportFilename state
  // This state is set immediately after seeing @import directive
  if FRange = rsImportFilename then
  begin
    // Read filename including dots, slashes, backslashes
    while (Run <= FLineLen) and
          (IsIdentChar(FLine[Run]) or CharInSet(FLine[Run], ['.', '/', '\', '-', '_'])) do
      Inc(Run);

    SetString(TempStr, PChar(@FLine[StartPos]), Run - StartPos);
    FTokenID := tkFilename;

    // Mark that we just parsed an import filename
    FJustParsedImportFilename := True;
    FRange := rsImportAliases;
    Exit;
  end;

  // For import aliases, handle variable assignments with specific token types
  if FRange = rsImportAliases then
  begin
    while (Run <= FLineLen) and IsIdentChar(FLine[Run]) do
      Inc(Run);

    SetString(TempStr, PChar(@FLine[StartPos]), Run - StartPos);

    if FExpectingAssignment then
    begin
      // This is the value after =, use special import variable token
      FTokenID := tkImportVar;
      FExpectingAssignment := False;
      // Enable stencil expression for property access on imported variables
      FInStencilExpression := True;
    end
    else if not FExpectingAssignment and IsPrecedingDot then
    begin
      // This is a property access on an import variable
      FTokenID := tkProperty;
      // Keep FInStencilExpression = True for potential chained properties
    end
    else
    begin
      // This is an alias name before =, use special import alias token
      FTokenID := tkImportAlias;
      // Next we expect an = operator
      FExpectingAssignment := True;
    end;
    Exit;
  end;

  while (Run <= FLineLen) and IsIdentChar(FLine[Run]) do
    Inc(Run);

  SetString(TempStr, PChar(@FLine[StartPos]), Run - StartPos);
  FLastIdentifier := TempStr;

  // If preceded by a dot in stencil context, it's a property
  if IsPrecedingDot then
  begin
    FTokenID := tkProperty;
    // Stay in stencil context for potential chained properties
    Exit;
  end;

  // Determine if we're in a stencil context
  InStencilContext := (FParenLevel > 0) or FInStencilExpression or
                       (FRange in [rsDirectiveFunction, rsForEachIn, rsIfCondition]);

  // Handle ForEach variable declaration flow
  if (FRange = rsDirectiveFunction) and (FParenLevel > 0) then
  begin
    // Inside directive function parameters
    if SameText(TempStr, 'var') then
    begin
      FTokenID := tkKey;
      FRange := rsForEachIn;  // Expecting variable name next
    end
    else if SameText(TempStr, 'in') and (FRange = rsForEachIn) then
    begin
      FTokenID := tkKey;
      FExpectingCollection := True;
    end
    else if FRange = rsForEachIn then
    begin
      // This is the variable being declared in ForEach
      FDeclaredVariables.AddOrSetValue(LowerCase(TempStr), FCurrentScopeLevel);
      FTokenID := tkDeclaredVar;
    end
    else if IsKeyword(TempStr) then
      FTokenID := tkKey
    else if IsDeclaredVariable(TempStr) then
      FTokenID := tkDeclaredVar
    else
      FTokenID := tkVariable;
  end
  else if (FRange = rsIfCondition) and (FParenLevel = 0) then
  begin
    // In an if condition without parentheses (like @if table.PrimaryKey {)
    if IsDeclaredVariable(TempStr) then
    begin
      FTokenID := tkDeclaredVar;
      FInStencilExpression := True; // This variable might have properties
    end
    else if IsKeyword(TempStr) then
      FTokenID := tkKey
    else
    begin
      FTokenID := tkVariable;
      FInStencilExpression := True; // This variable might have properties
    end
  end
  else if InStencilContext then
  begin
    // We're in a stencil context - check for keywords and variables
    if IsKeyword(TempStr) then
      FTokenID := tkKey
    else if IsDeclaredVariable(TempStr) then
      FTokenID := tkDeclaredVar
    else if FExpectingCollection then
    begin
      FExpectingCollection := False;
      FTokenID := tkVariable;
    end
    else
      FTokenID := tkVariable;  // Treat as variable in stencil context
  end
  else if FExpectingCollection then
  begin
    // This is the collection after "in" - it's a variable reference without @
    FExpectingCollection := False;
    if IsDeclaredVariable(TempStr) then
    begin
      FTokenID := tkDeclaredVar;
      FInStencilExpression := True;  // Enable property access
    end
    else
    begin
      FTokenID := tkVariable;
      FInStencilExpression := True;  // Enable property access
    end
  end
  else
  begin
    // Regular identifiers in template text - NOT keywords
    // Words like CREATE, TABLE, SELECT, PRIMARY, KEY etc. are just plain text
    FTokenID := tkIdentifier;
    FInStencilExpression := False;
  end;

  // Don't clear stencil expression if we might have property access coming
  if (FTokenID = tkIdentifier) and not IsPrecedingDot then
    FInStencilExpression := False;
end;

procedure TSynWebStencilsSyn.NumberProc;
begin
  FTokenID := tkNumber;
  while (Run <= FLineLen) and CharInSet(FLine[Run], ['0'..'9', '.']) do
  begin
    if (FLine[Run] = '.') and (Run + 1 <= FLineLen) and (FLine[Run + 1] = '.') then
      Break; // Range operator
    Inc(Run);
  end;
end;

procedure TSynWebStencilsSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynWebStencilsSyn.Next;
begin
  FTokenPos := Run;

  // Handle continuation states from previous line
  case FRange of
    rsComment:
      begin
        FTokenID := tkComment;
        while Run <= FLineLen do
        begin
          if (FLine[Run] = '*') and (Run + 1 <= FLineLen) and (FLine[Run + 1] = '@') then
          begin
            Inc(Run, 2);
            FRange := rsUnknown;
            Break;
          end;
          Inc(Run);
        end;
        inherited;
        Exit;
      end;
  end;

  // Check for end of line
  if Run > FLineLen then
  begin
    NullProc;
    inherited;
    Exit;
  end;

  // Normal token processing
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32:
      begin
        SpaceProc;
        // Space only breaks stencil expressions if we're actually in one
        // and the next character isn't part of the stencil expression
        if FInStencilExpression and (FParenLevel = 0) and
           (Run <= FLineLen) and not (FLine[Run] in ['@', '.']) then
          FInStencilExpression := False;
      end;
    '"', '''':
      begin
        // Check if this is a quoted filename in import statement
        if FRange = rsImportFilename then
          StringProc  // Will handle as filename
        else
        begin
          StringProc;
          FInStencilExpression := False;
        end;
      end;
    '{': BraceOpenProc;
    '}': BraceCloseProc;
    '.':
      begin
        // Process dot specially if we're in a stencil expression or import aliases
        if FInStencilExpression or (FRange = rsImportAliases) then
          DotProc
        else
        begin
          // Regular dot in template - treat as normal text
          FTokenID := tkIdentifier;
          Inc(Run);
        end;
      end;
    '(':
      begin
        // Only process parenthesis specially in specific contexts
        if (FRange = rsDirectiveFunction) or (FParenLevel > 0) then
          ParenOpenProc
        else
        begin
          // Regular parenthesis in template text
          FTokenID := tkIdentifier;
          FInStencilExpression := False;
          Inc(Run);
        end;
      end;
    ')':
      begin
        // Only process as stencil parenthesis if we're in stencil parentheses
        if FParenLevel > 0 then
          ParenCloseProc
        else
        begin
          // Regular parenthesis in template text
          FTokenID := tkIdentifier;
          Inc(Run);
        end;
      end;
    '#':
      begin
        HashCommentProc;
        FInStencilExpression := False;
      end;
    '@':
      begin
        if (Run + 1 <= FLineLen) and (FLine[Run + 1] = '*') then
          CommentProc
        else if FEscapeDoubling and (Run + 1 <= FLineLen) and (FLine[Run + 1] = '@') then
        begin
          FTokenID := tkSymbol;
          FInStencilExpression := False;
          Inc(Run, 2);
        end
        else if (FEscapeChar <> #0) and (Run > 1) and (FLine[Run - 1] = FEscapeChar) then
        begin
          FTokenID := tkSymbol;
          FInStencilExpression := False;
          Inc(Run);
        end
        else
          DirectiveProc;  // This sets FInStencilExpression = True for variables
      end;
    '0'..'9':
      begin
        NumberProc;
        if (FParenLevel = 0) and not (Run <= FLineLen) and (FLine[Run] = '.') then
          FInStencilExpression := False;
      end;
    'A'..'Z', 'a'..'z', '_':
      begin
        IdentProc;
        // IdentProc handles FInStencilExpression internally
      end;
    '<', '>', '=', '!', '+', '-', '*', '/', '%', '|', '&':
      begin
        // Only treat as operator in stencil context
        if (FParenLevel > 0) or (FInStencilExpression and (FRange = rsIfCondition)) then
          SymbolProc  // This will set tkOperator
        else
        begin
          // Regular character in template text
          FTokenID := tkIdentifier;
          Inc(Run);
        end;
      end;
    ',', ';', ':', '?', '[', ']', '^':
      begin
        // Only treat as symbol in stencil context
        if (FParenLevel > 0) or (FRange = rsImportAliases) then
          SymbolProc
        else
        begin
          // Regular character in template text
          FTokenID := tkIdentifier;
          FInStencilExpression := False;
          Inc(Run);
        end;
      end;
  else
    if (FEscapeChar <> #0) and (FEscapeChar <> '@') and
       (FLine[Run] = FEscapeChar) and (Run + 1 <= FLineLen) and (FLine[Run + 1] = '@') then
    begin
      FTokenID := tkSymbol;
      FInStencilExpression := False;
      Inc(Run, 2);
    end
    else
    begin
      UnknownProc;
      FInStencilExpression := False;
    end;
  end;

  inherited;
end;

function TSynWebStencilsSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynWebStencilsSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynWebStencilsSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynWebStencilsSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirectiveAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkBrace: Result := FBraceAttri;
    tkOperator: Result := FOperatorAttri;
    tkProperty: Result := FPropertyAttri;
    tkDeclaredVar: Result := FDeclaredVarAttri;
    tkStencilParen: Result := FStencilParenAttri;
    tkFilename: Result := FFilenameAttri;
    tkImportAlias: Result := FImportAliasAttri;
    tkImportVar: Result := FImportVarAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynWebStencilsSyn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

function TSynWebStencilsSyn.GetRange: Pointer;
begin
  // Pack state into pointer
  // Bits 0-7: FRange
  // Bits 8-15: FBraceLevel
  // Bits 16-23: FCurrentScopeLevel
  // Bit 24: FInStencilExpression
  // Bits 25-31: FParenLevel
  Result := Pointer(
    Ord(FRange) or
    (FBraceLevel shl 8) or
    (FCurrentScopeLevel shl 16) or
    (Ord(FInStencilExpression) shl 24) or
    (FParenLevel shl 25)
  );
end;

procedure TSynWebStencilsSyn.SetRange(Value: Pointer);
var
  PackedValue: Integer;
begin
  PackedValue := Integer(Value);
  FRange := TRangeState(PackedValue and $FF);
  FBraceLevel := (PackedValue shr 8) and $FF;
  FCurrentScopeLevel := (PackedValue shr 16) and $FF;
  FInStencilExpression := Boolean((PackedValue shr 24) and $01);
  FParenLevel := (PackedValue shr 25) and $7F;
end;

procedure TSynWebStencilsSyn.ResetRange;
begin
  FRange := rsUnknown;
  FTokenID := tkUnknown;
  FBraceLevel := 0;
  FCurrentScopeLevel := 0;
  FDeclaredVariables.Clear;
  FInStencilExpression := False;
  FParenLevel := 0;
  FExpectingCollection := False;
  FExpectingAssignment := False;
  FJustParsedImportFilename := False;
  FLastIdentifier := '';
end;

function TSynWebStencilsSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> '*.stencil;*.tpl;*.ini';
end;

function TSynWebStencilsSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
  else
    Result := False;
  end;
end;

function TSynWebStencilsSyn.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result := CharInSet(AChar, [
    #0..#32, '.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(', ')',
    '{', '}', '^', '%', '*', '+', '-', '=', '\', '|', '/', '<', '>']);
end;

class function TSynWebStencilsSyn.GetLanguageName: string;
begin
  Result := 'WebStencils';
end;

class function TSynWebStencilsSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := 'Web Stencils Template';
end;

function TSynWebStencilsSyn.GetSampleSource: UnicodeString;
begin
  Result := '# WebStencils Template Example'#13#10 +
            '@project.Db.Name'#13#10 +
            ''#13#10 +
            '# Import external templates with various filename formats'#13#10 +
            '@import templates/header.stencil { title = project.Name, author = project.Author }'#13#10 +
            '@import "templates/my file with spaces.tpl"'#13#10 +
            '@import ''C:\Program Files\Templates\utils.stencil'''#13#10 +
            '@import common/utils.tpl'#13#10 +
            ''#13#10 +
            '# ForEach with parentheses and var'#13#10 +
            '@ForEach(var table in project.Tables) {'#13#10 +
            '  CREATE TABLE @table.Name ('#13#10 +
            '    @if table.PrimaryKey {'#13#10 +
            '      PRIMARY KEY (@table.PrimaryKey.Name)'#13#10 +
            '    }'#13#10 +
            '  );'#13#10 +
            '}'#13#10 +
            ''#13#10 +
            '# ForEach without parentheses (simplified syntax)'#13#10 +
            '@foreach user in users {'#13#10 +
            '  <div>@user.Name - @user.Email</div>'#13#10 +
            '}'#13#10 +
            ''#13#10 +
            '# Switch statement (both syntaxes supported)'#13#10 +
            '@switch(user.role) {'#13#10 +
            '  @case "admin" {'#13#10 +
            '    <div class="admin-panel">Admin Dashboard</div>'#13#10 +
            '  }'#13#10 +
            '  @default {'#13#10 +
            '    <div>Default content</div>'#13#10 +
            '  }'#13#10 +
            '}'#13#10 +
            ''#13#10 +
            '@switch status {'#13#10 +
            '  @case active {'#13#10 +
            '    Status: Active'#13#10 +
            '  }'#13#10 +
            '}'#13#10 +
            ''#13#10 +
            '# If statements (both syntaxes)'#13#10 +
            '@if(index < count) { Less than }'#13#10 +
            '@if isActive { Active! }'#13#10 +
            ''#13#10 +
            '@* Block comment example *@'#13#10 +
            '@@escaped_at_symbol = ''test''';
end;

initialization
  RegisterPlaceableHighlighter(TSynWebStencilsSyn);

end.
