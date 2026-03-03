{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{ Pure-Delphi Hunspell dictionary provider implementing ISynSpellCheckProvider.
  Framework-neutral — works with both VCL and FMX. }

unit SynSpellCheckHunspellProvider;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  SynSpellCheckTypes;

{$REGION 'Hunspell Types'}

type
  TSynFlagType = (ftShort, ftLong, ftNum, ftUTF8);

  TSynAffixRule = record
    Flag: string;
    CrossProduct: Boolean;
    Strip: string;
    Add: string;
    Condition: string;
  end;

  TSynRepEntry = record
    Pattern: string;
    Replacement: string;
  end;

{$ENDREGION 'Hunspell Types'}

{$REGION 'Hunspell Provider'}

  TSynHunspellProvider = class(TInterfacedObject, ISynSpellCheckProvider)
  private
    FLanguage: string;
    FDictionaryPath: string;
    FLoaded: Boolean;
    FWordList: TDictionary<string, Boolean>;
    FWordFlags: TDictionary<string, string>;
    FPrefixRules: TList<TSynAffixRule>;
    FSuffixRules: TList<TSynAffixRule>;
    FRepTable: TList<TSynRepEntry>;
    FTryChars: string;
    FFlagType: TSynFlagType;
    procedure LoadDictionary;
    procedure ParseAffFile(const AFileName: string);
    function MatchCondition(const AWord, ACondition: string;
      AFromEnd: Boolean): Boolean;
    function CheckSuffix(const AWord: string): Boolean;
    function CheckPrefix(const AWord: string): Boolean;
    function CheckCrossProduct(const AWord: string): Boolean;
    function LookupWord(const AWord: string): Boolean;
    function WordHasFlag(const AWord, AFlag: string): Boolean;
  public
    constructor Create; overload;
    constructor Create(const ADictionaryPath: string;
      const ALanguage: string = ''); overload;
    destructor Destroy; override;
    { ISynSpellCheckProvider }
    function CheckWord(const AWord: string): Boolean;
    function Suggest(const AWord: string): TArray<string>;
    function IsAvailable: Boolean;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    { Additional }
    property DictionaryPath: string read FDictionaryPath write FDictionaryPath;
  end;

{$ENDREGION 'Hunspell Provider'}

{$REGION 'Hunspell Native Provider'}

  { Stub for future native Hunspell DLL binding.
    All methods are no-ops; IsAvailable returns False. }
  TSynHunspellNativeProvider = class(TInterfacedObject, ISynSpellCheckProvider)
  // External functions to bind (hunspell.dll / libhunspell.so / libhunspell.dylib):
  //   function Hunspell_create(affpath, dpath: PAnsiChar): Pointer; cdecl;
  //   procedure Hunspell_destroy(handle: Pointer); cdecl;
  //   function Hunspell_spell(handle: Pointer; word: PAnsiChar): Integer; cdecl;
  //   function Hunspell_suggest(handle: Pointer; out slst: PPAnsiChar;
  //     word: PAnsiChar): Integer; cdecl;
  //   procedure Hunspell_free_list(handle: Pointer; slst: PPAnsiChar;
  //     count: Integer); cdecl;
  private
    FHandle: Pointer;
    FLanguage: string;
    FDictionaryPath: string;
    FLoaded: Boolean;
  public
    constructor Create(const ADictionaryPath: string;
      const ALanguage: string = '');
    destructor Destroy; override;
    function CheckWord(const AWord: string): Boolean;
    function Suggest(const AWord: string): TArray<string>;
    function IsAvailable: Boolean;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    property DictionaryPath: string read FDictionaryPath;
  end;

{$ENDREGION 'Hunspell Native Provider'}

implementation

uses
  System.IOUtils,
  System.Character;

{ ============================================================================ }
{ TSynHunspellProvider                                                         }
{ ============================================================================ }

constructor TSynHunspellProvider.Create;
begin
  inherited Create;
  FLanguage := '';
  FDictionaryPath := '';
  FLoaded := False;
  FFlagType := ftShort;
  FWordList := TDictionary<string, Boolean>.Create;
  FWordFlags := TDictionary<string, string>.Create;
  FPrefixRules := TList<TSynAffixRule>.Create;
  FSuffixRules := TList<TSynAffixRule>.Create;
  FRepTable := TList<TSynRepEntry>.Create;
end;

constructor TSynHunspellProvider.Create(const ADictionaryPath: string;
  const ALanguage: string);
begin
  inherited Create;
  FDictionaryPath := ADictionaryPath;
  FLanguage := ALanguage;
  FLoaded := False;
  FFlagType := ftShort;
  FWordList := TDictionary<string, Boolean>.Create;
  FWordFlags := TDictionary<string, string>.Create;
  FPrefixRules := TList<TSynAffixRule>.Create;
  FSuffixRules := TList<TSynAffixRule>.Create;
  FRepTable := TList<TSynRepEntry>.Create;
end;

destructor TSynHunspellProvider.Destroy;
begin
  FRepTable.Free;
  FSuffixRules.Free;
  FPrefixRules.Free;
  FWordFlags.Free;
  FWordList.Free;
  inherited;
end;

{ ---------------------------------------------------------------------------- }
{ .aff file parser                                                             }
{ ---------------------------------------------------------------------------- }

procedure TSynHunspellProvider.ParseAffFile(const AFileName: string);
var
  Lines: TStringList;
  I: Integer;
  Line, Cmd, HeaderKey: string;
  Parts: TArray<string>;
  Rule: TSynAffixRule;
  Rep: TSynRepEntry;
  SeenHeaders: TDictionary<string, Boolean>;
  CrossProducts: TDictionary<string, Boolean>;
  CrossProd: Boolean;
  SlashPos: Integer;
begin
  FPrefixRules.Clear;
  FSuffixRules.Clear;
  FRepTable.Clear;
  FTryChars := '';
  FFlagType := ftShort;

  if not TFile.Exists(AFileName) then
    Exit;

  Lines := TStringList.Create;
  SeenHeaders := TDictionary<string, Boolean>.Create;
  CrossProducts := TDictionary<string, Boolean>.Create;
  try
    Lines.LoadFromFile(AFileName, TEncoding.UTF8);
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if (Line = '') or (Line[1] = '#') then
        Continue;

      Parts := Line.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
      if Length(Parts) < 2 then
        Continue;

      Cmd := Parts[0];

      if Cmd = 'SET' then
      begin
        // Encoding directive — we load as UTF-8 by default
      end
      else if Cmd = 'FLAG' then
      begin
        if SameText(Parts[1], 'long') then
          FFlagType := ftLong
        else if SameText(Parts[1], 'num') then
          FFlagType := ftNum
        else if SameText(Parts[1], 'UTF-8') then
          FFlagType := ftUTF8;
      end
      else if Cmd = 'TRY' then
      begin
        FTryChars := Parts[1];
      end
      else if Cmd = 'REP' then
      begin
        // REP pattern replacement  (or REP count — skip count lines)
        if Length(Parts) >= 3 then
        begin
          Rep.Pattern := Parts[1];
          Rep.Replacement := Parts[2];
          FRepTable.Add(Rep);
        end;
      end
      else if (Cmd = 'PFX') or (Cmd = 'SFX') then
      begin
        if Length(Parts) < 4 then
          Continue;

        HeaderKey := Cmd + ':' + Parts[1];

        if not SeenHeaders.ContainsKey(HeaderKey) then
        begin
          // Header line: CMD FLAG Y/N COUNT
          SeenHeaders.Add(HeaderKey, True);
          CrossProducts.AddOrSetValue(HeaderKey, Parts[2] = 'Y');
        end
        else
        begin
          // Rule line: CMD FLAG STRIP ADD [CONDITION]
          Rule.Flag := Parts[1];
          CrossProd := False;
          CrossProducts.TryGetValue(HeaderKey, CrossProd);
          Rule.CrossProduct := CrossProd;

          if Parts[2] = '0' then
            Rule.Strip := ''
          else
            Rule.Strip := Parts[2];

          Rule.Add := Parts[3];
          // Strip continuation flags after '/'
          SlashPos := Pos('/', Rule.Add);
          if SlashPos > 0 then
            Rule.Add := Copy(Rule.Add, 1, SlashPos - 1);
          if Rule.Add = '0' then
            Rule.Add := '';

          if Length(Parts) >= 5 then
            Rule.Condition := Parts[4]
          else
            Rule.Condition := '.';

          if Cmd = 'PFX' then
            FPrefixRules.Add(Rule)
          else
            FSuffixRules.Add(Rule);
        end;
      end;
      // Skip: COMPOUNDFLAG, COMPOUNDMIN, COMPOUNDRULE, KEY, MAP, PHONE, etc.
    end;
  finally
    CrossProducts.Free;
    SeenHeaders.Free;
    Lines.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ .dic file loader (preserves affix flags)                                     }
{ ---------------------------------------------------------------------------- }

procedure TSynHunspellProvider.LoadDictionary;
var
  DicFile, AffFile: string;
  Lines: TStringList;
  I: Integer;
  Entry, Word, Flags: string;
  SlashPos, TabPos, SpPos: Integer;
begin
  FWordList.Clear;
  FWordFlags.Clear;
  FPrefixRules.Clear;
  FSuffixRules.Clear;
  FRepTable.Clear;
  FLoaded := False;

  if FDictionaryPath = '' then
    Exit;

  if FLanguage <> '' then
  begin
    DicFile := TPath.Combine(FDictionaryPath, FLanguage + '.dic');
    AffFile := TPath.Combine(FDictionaryPath, FLanguage + '.aff');
  end
  else
  begin
    DicFile := FDictionaryPath;
    AffFile := ChangeFileExt(FDictionaryPath, '.aff');
  end;

  if not TFile.Exists(DicFile) then
    Exit;

  // Parse .aff first (sets FFlagType, rules, TRY, REP, etc.)
  ParseAffFile(AffFile);

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(DicFile, TEncoding.UTF8);
    // Hunspell .dic format: first line is word count, then one word per line.
    // Words may have flags after a slash, e.g. "word/ABC"
    for I := 1 to Lines.Count - 1 do
    begin
      Entry := Trim(Lines[I]);
      if Entry = '' then
        Continue;

      SlashPos := Pos('/', Entry);
      if SlashPos > 0 then
      begin
        Word := Copy(Entry, 1, SlashPos - 1);
        Flags := Copy(Entry, SlashPos + 1, MaxInt);
        // Strip morphological data after tab or space
        TabPos := Pos(#9, Flags);
        if TabPos > 0 then
          Flags := Copy(Flags, 1, TabPos - 1);
        SpPos := Pos(' ', Flags);
        if SpPos > 0 then
          Flags := Copy(Flags, 1, SpPos - 1);
      end
      else
      begin
        Word := Entry;
        // Strip morphological data
        TabPos := Pos(#9, Word);
        if TabPos > 0 then
          Word := Copy(Word, 1, TabPos - 1);
        Flags := '';
      end;

      FWordList.AddOrSetValue(LowerCase(Word), True);
      if Flags <> '' then
        FWordFlags.AddOrSetValue(LowerCase(Word), Flags);
    end;
    FLoaded := True;
  finally
    Lines.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ Condition matching for affix rules                                           }
{ Hunspell conditions: [abc] = char class, [^abc] = negated, . = any char,    }
{ literal chars. Anchored to word end (SFX) or start (PFX).                   }
{ ---------------------------------------------------------------------------- }

function TSynHunspellProvider.MatchCondition(const AWord, ACondition: string;
  AFromEnd: Boolean): Boolean;
var
  CondLen, WordLen, I, J, StartPos: Integer;
  Negated, Matched: Boolean;
  C: Char;
begin
  if (ACondition = '') or (ACondition = '.') then
    Exit(True);

  // Count character positions in the condition pattern
  CondLen := 0;
  I := 1;
  while I <= Length(ACondition) do
  begin
    Inc(CondLen);
    if ACondition[I] = '[' then
    begin
      while (I <= Length(ACondition)) and (ACondition[I] <> ']') do
        Inc(I);
    end;
    Inc(I);
  end;

  WordLen := Length(AWord);
  if WordLen < CondLen then
    Exit(False);

  // Determine start position in the word
  if AFromEnd then
    StartPos := WordLen - CondLen + 1
  else
    StartPos := 1;

  // Match each condition element against the word
  I := 1;
  J := StartPos;
  while (I <= Length(ACondition)) and (J >= 1) and (J <= WordLen) do
  begin
    if ACondition[I] = '[' then
    begin
      Inc(I); // skip '['
      Negated := False;
      if (I <= Length(ACondition)) and (ACondition[I] = '^') then
      begin
        Negated := True;
        Inc(I);
      end;
      Matched := False;
      C := AWord[J];
      while (I <= Length(ACondition)) and (ACondition[I] <> ']') do
      begin
        if ACondition[I] = C then
          Matched := True;
        Inc(I);
      end;
      if I <= Length(ACondition) then
        Inc(I); // skip ']'
      if Negated then
        Matched := not Matched;
      if not Matched then
        Exit(False);
      Inc(J);
    end
    else if ACondition[I] = '.' then
    begin
      Inc(I);
      Inc(J);
    end
    else
    begin
      if AWord[J] <> ACondition[I] then
        Exit(False);
      Inc(I);
      Inc(J);
    end;
  end;

  Result := (I > Length(ACondition));
end;

{ ---------------------------------------------------------------------------- }
{ Word lookup helpers                                                          }
{ ---------------------------------------------------------------------------- }

function TSynHunspellProvider.LookupWord(const AWord: string): Boolean;
begin
  Result := FWordList.ContainsKey(LowerCase(AWord));
end;

function TSynHunspellProvider.WordHasFlag(const AWord, AFlag: string): Boolean;
var
  Flags: string;
  FlagPos: Integer;
  FlagParts: TArray<string>;
  FP: string;
begin
  if not FWordFlags.TryGetValue(LowerCase(AWord), Flags) then
    Exit(False);

  case FFlagType of
    ftShort, ftUTF8:
      Result := Pos(AFlag, Flags) > 0;
    ftLong:
      begin
        FlagPos := 1;
        Result := False;
        while FlagPos + 1 <= Length(Flags) do
        begin
          if Copy(Flags, FlagPos, 2) = AFlag then
            Exit(True);
          Inc(FlagPos, 2);
        end;
      end;
    ftNum:
      begin
        FlagParts := Flags.Split([',']);
        Result := False;
        for FP in FlagParts do
          if Trim(FP) = AFlag then
            Exit(True);
      end;
  else
    Result := False;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ Affix rule checking                                                          }
{ ---------------------------------------------------------------------------- }

function TSynHunspellProvider.CheckSuffix(const AWord: string): Boolean;
var
  Rule: TSynAffixRule;
  Stem, WordLower: string;
begin
  WordLower := LowerCase(AWord);
  for Rule in FSuffixRules do
  begin
    if Rule.Add = '' then
    begin
      // Rule adds nothing — word itself is the stem with Strip re-added
      Stem := WordLower + Rule.Strip;
      if MatchCondition(Stem, Rule.Condition, True) and
         WordHasFlag(Stem, Rule.Flag) then
        Exit(True);
    end
    else if (Length(WordLower) > Length(Rule.Add)) and
            WordLower.EndsWith(Rule.Add) then
    begin
      // Strip the added suffix and re-add the stripped part
      Stem := Copy(WordLower, 1, Length(WordLower) - Length(Rule.Add))
              + Rule.Strip;
      if (Stem <> '') and MatchCondition(Stem, Rule.Condition, True) and
         WordHasFlag(Stem, Rule.Flag) then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TSynHunspellProvider.CheckPrefix(const AWord: string): Boolean;
var
  Rule: TSynAffixRule;
  Stem, WordLower: string;
begin
  WordLower := LowerCase(AWord);
  for Rule in FPrefixRules do
  begin
    if Rule.Add = '' then
    begin
      Stem := Rule.Strip + WordLower;
      if MatchCondition(Stem, Rule.Condition, False) and
         WordHasFlag(Stem, Rule.Flag) then
        Exit(True);
    end
    else if (Length(WordLower) > Length(Rule.Add)) and
            WordLower.StartsWith(Rule.Add) then
    begin
      Stem := Rule.Strip + Copy(WordLower, Length(Rule.Add) + 1, MaxInt);
      if (Stem <> '') and MatchCondition(Stem, Rule.Condition, False) and
         WordHasFlag(Stem, Rule.Flag) then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TSynHunspellProvider.CheckCrossProduct(const AWord: string): Boolean;
var
  PfxRule, SfxRule: TSynAffixRule;
  AfterPrefix, Stem, WordLower: string;
begin
  WordLower := LowerCase(AWord);

  for PfxRule in FPrefixRules do
  begin
    if not PfxRule.CrossProduct then
      Continue;

    // Strip prefix
    if PfxRule.Add = '' then
      AfterPrefix := PfxRule.Strip + WordLower
    else if WordLower.StartsWith(PfxRule.Add) then
      AfterPrefix := PfxRule.Strip +
                     Copy(WordLower, Length(PfxRule.Add) + 1, MaxInt)
    else
      Continue;

    if AfterPrefix = '' then
      Continue;
    if not MatchCondition(AfterPrefix, PfxRule.Condition, False) then
      Continue;

    // Now try each suffix rule on the result
    for SfxRule in FSuffixRules do
    begin
      if not SfxRule.CrossProduct then
        Continue;

      if SfxRule.Add = '' then
      begin
        Stem := AfterPrefix + SfxRule.Strip;
        if MatchCondition(Stem, SfxRule.Condition, True) and
           WordHasFlag(Stem, PfxRule.Flag) and
           WordHasFlag(Stem, SfxRule.Flag) then
          Exit(True);
      end
      else if AfterPrefix.EndsWith(SfxRule.Add) then
      begin
        Stem := Copy(AfterPrefix, 1,
                  Length(AfterPrefix) - Length(SfxRule.Add)) + SfxRule.Strip;
        if (Stem <> '') and
           MatchCondition(Stem, SfxRule.Condition, True) and
           WordHasFlag(Stem, PfxRule.Flag) and
           WordHasFlag(Stem, SfxRule.Flag) then
          Exit(True);
      end;
    end;
  end;
  Result := False;
end;

{ ---------------------------------------------------------------------------- }
{ ISynSpellCheckProvider                                                        }
{ ---------------------------------------------------------------------------- }

function TSynHunspellProvider.CheckWord(const AWord: string): Boolean;
begin
  if not FLoaded and (FDictionaryPath <> '') then
    LoadDictionary;

  // If not loaded or empty dictionary, accept all words
  if not FLoaded or (FWordList.Count = 0) then
    Exit(True);

  // Stage 1: Direct lookup
  if LookupWord(AWord) then
    Exit(True);

  // Stage 2: Suffix stripping
  if CheckSuffix(AWord) then
    Exit(True);

  // Stage 3: Prefix stripping
  if CheckPrefix(AWord) then
    Exit(True);

  // Stage 4: Cross-product (prefix + suffix)
  if CheckCrossProduct(AWord) then
    Exit(True);

  Result := False;
end;

function TSynHunspellProvider.Suggest(const AWord: string): TArray<string>;
const
  MaxSuggestions = 10;

  function IsValidWord(Provider: TSynHunspellProvider;
    const S: string): Boolean;
  begin
    Result := Provider.CheckWord(S);
  end;

var
  Suggestions: TList<string>;
  WordLower, S: string;
  Rep: TSynRepEntry;
  I, J, FoundPos: Integer;
  C: Char;
begin
  if not FLoaded and (FDictionaryPath <> '') then
    LoadDictionary;

  Suggestions := TList<string>.Create;
  try
    WordLower := LowerCase(AWord);

    // Stage 1: REP table replacements
    for Rep in FRepTable do
    begin
      if Suggestions.Count >= MaxSuggestions then
        Break;
      FoundPos := Pos(Rep.Pattern, WordLower);
      while (FoundPos > 0) and (Suggestions.Count < MaxSuggestions) do
      begin
        S := Copy(WordLower, 1, FoundPos - 1) + Rep.Replacement +
             Copy(WordLower, FoundPos + Length(Rep.Pattern), MaxInt);
        if (S <> '') and not Suggestions.Contains(S) and
           IsValidWord(Self, S) then
          Suggestions.Add(S);
        FoundPos := Pos(Rep.Pattern, WordLower, FoundPos + 1);
      end;
    end;

    // Stage 2: Single-char edits using TRY characters
    if (Suggestions.Count < MaxSuggestions) and (FTryChars <> '') then
    begin
      // Deletions
      for I := 1 to Length(WordLower) do
      begin
        if Suggestions.Count >= MaxSuggestions then Break;
        S := Copy(WordLower, 1, I - 1) + Copy(WordLower, I + 1, MaxInt);
        if (S <> '') and not Suggestions.Contains(S) and
           IsValidWord(Self, S) then
          Suggestions.Add(S);
      end;

      // Transpositions
      for I := 1 to Length(WordLower) - 1 do
      begin
        if Suggestions.Count >= MaxSuggestions then Break;
        S := WordLower;
        C := S[I];
        S[I] := S[I + 1];
        S[I + 1] := C;
        if not Suggestions.Contains(S) and IsValidWord(Self, S) then
          Suggestions.Add(S);
      end;

      // Substitutions
      for I := 1 to Length(WordLower) do
      begin
        if Suggestions.Count >= MaxSuggestions then Break;
        for J := 1 to Length(FTryChars) do
        begin
          if Suggestions.Count >= MaxSuggestions then Break;
          if FTryChars[J] = WordLower[I] then
            Continue;
          S := WordLower;
          S[I] := FTryChars[J];
          if not Suggestions.Contains(S) and IsValidWord(Self, S) then
            Suggestions.Add(S);
        end;
      end;

      // Insertions
      for I := 1 to Length(WordLower) + 1 do
      begin
        if Suggestions.Count >= MaxSuggestions then Break;
        for J := 1 to Length(FTryChars) do
        begin
          if Suggestions.Count >= MaxSuggestions then Break;
          S := Copy(WordLower, 1, I - 1) + FTryChars[J] +
               Copy(WordLower, I, MaxInt);
          if not Suggestions.Contains(S) and IsValidWord(Self, S) then
            Suggestions.Add(S);
        end;
      end;
    end;

    Result := Suggestions.ToArray;
  finally
    Suggestions.Free;
  end;
end;

function TSynHunspellProvider.IsAvailable: Boolean;
begin
  if not FLoaded and (FDictionaryPath <> '') then
    LoadDictionary;
  Result := FLoaded;
end;

function TSynHunspellProvider.GetLanguage: string;
begin
  Result := FLanguage;
end;

procedure TSynHunspellProvider.SetLanguage(const Value: string);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    FLoaded := False;
    FWordList.Clear;
    FWordFlags.Clear;
    FPrefixRules.Clear;
    FSuffixRules.Clear;
    FRepTable.Clear;
  end;
end;

{ ============================================================================ }
{ TSynHunspellNativeProvider (stub)                                            }
{ ============================================================================ }

constructor TSynHunspellNativeProvider.Create(const ADictionaryPath: string;
  const ALanguage: string);
begin
  inherited Create;
  FDictionaryPath := ADictionaryPath;
  FLanguage := ALanguage;
  FHandle := nil;
  FLoaded := False;
end;

destructor TSynHunspellNativeProvider.Destroy;
begin
  FHandle := nil;
  inherited;
end;

function TSynHunspellNativeProvider.CheckWord(const AWord: string): Boolean;
begin
  Result := True; // No-op: accept all words when native DLL not loaded
end;

function TSynHunspellNativeProvider.Suggest(
  const AWord: string): TArray<string>;
begin
  SetLength(Result, 0);
end;

function TSynHunspellNativeProvider.IsAvailable: Boolean;
begin
  Result := False; // Native DLL not yet implemented
end;

function TSynHunspellNativeProvider.GetLanguage: string;
begin
  Result := FLanguage;
end;

procedure TSynHunspellNativeProvider.SetLanguage(const Value: string);
begin
  FLanguage := Value;
end;

end.
