{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynSpellCheck;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ActiveX,
  {$ENDIF}
  FMX.Graphics,
  SynEditTypes,
  SynEditMiscProcs,
  FMX.SynEdit;

{$REGION 'Spell Check Token Types'}

type
  TSynSpellCheckToken = (sctComment, sctString, sctIdentifier);
  TSynSpellCheckTokens = set of TSynSpellCheckToken;

{$ENDREGION 'Spell Check Token Types'}

{$REGION 'Spell Error Record'}

  TSynSpellError = record
    Line: Integer;       // 1-based line
    StartChar: Integer;  // 1-based char
    EndChar: Integer;    // 1-based char (exclusive)
    Word: string;
  end;

{$ENDREGION 'Spell Error Record'}

{$REGION 'Provider Interface'}

  ISynSpellCheckProvider = interface
    ['{A1F5B2C3-D4E6-4789-AB01-23456789ABCD}']
    function CheckWord(const AWord: string): Boolean;
    function Suggest(const AWord: string): TArray<string>;
    function IsAvailable: Boolean;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
  end;

{$ENDREGION 'Provider Interface'}

{$IFDEF MSWINDOWS}
{$REGION 'Windows Spell Checking COM Interfaces'}

const
  IID_ISpellCheckerFactory: TGUID = '{8E018A9D-2415-4677-BF08-794EA61F94BB}';
  IID_IUserDictionariesRegistrar: TGUID = '{AA176B85-0E12-4844-8E1A-EEF1DA77F586}';
  IID_ISpellChecker: TGUID = '{B6FD0B71-E2BC-4653-8D05-F197E412770B}';
  IID_IEnumSpellingError: TGUID = '{803E3BD4-2828-4410-8290-418D1D73C762}';
  IID_ISpellingError: TGUID = '{B7C82D61-FBE8-4B47-9B27-6C0D2E0DE0A3}';
  IID_ISpellCheckerChangedEventHandler: TGUID = '{0B83A5B0-792F-4EAB-9799-ACF52C5ED08A}';
  IID_IOptionDescription: TGUID = '{432E5F85-35CF-4606-A801-6F70277E1D7A}';
  CLASS_SpellCheckerFactory: TGUID = '{7AB36653-1796-484B-BDFA-E74F1DB7C1DC}';

// Constants for enum CORRECTIVE_ACTION
type
  CORRECTIVE_ACTION = TOleEnum;

const
  CORRECTIVE_ACTION_NONE = $00000000;
  CORRECTIVE_ACTION_GET_SUGGESTIONS = $00000001;
  CORRECTIVE_ACTION_REPLACE = $00000002;
  CORRECTIVE_ACTION_DELETE = $00000003;

type
  // Forward declarations
  ISpellCheckerFactory = interface;
  IUserDictionariesRegistrar = interface;
  ISpellChecker = interface;
  IEnumSpellingError = interface;
  ISpellingError = interface;
  ISpellCheckerChangedEventHandler = interface;
  IOptionDescription = interface;

  SpellCheckerFactory = ISpellCheckerFactory;

  ISpellCheckerFactory = interface(IUnknown)
    ['{8E018A9D-2415-4677-BF08-794EA61F94BB}']
    function Get_SupportedLanguages(out value: IEnumString): HResult; stdcall;
    function IsSupported(languageTag: PWideChar; out value: Integer): HResult; stdcall;
    function CreateSpellChecker(languageTag: PWideChar; out value: ISpellChecker): HResult; stdcall;
  end;

  IUserDictionariesRegistrar = interface(IUnknown)
    ['{AA176B85-0E12-4844-8E1A-EEF1DA77F586}']
    function RegisterUserDictionary(dictionaryPath: PWideChar; languageTag: PWideChar): HResult; stdcall;
    function UnregisterUserDictionary(dictionaryPath: PWideChar; languageTag: PWideChar): HResult; stdcall;
  end;

  ISpellChecker = interface(IUnknown)
    ['{B6FD0B71-E2BC-4653-8D05-F197E412770B}']
    function Get_languageTag(out value: PWideChar): HResult; stdcall;
    function Check(text: PWideChar; out value: IEnumSpellingError): HResult; stdcall;
    function Suggest(word: PWideChar; out value: IEnumString): HResult; stdcall;
    function Add(word: PWideChar): HResult; stdcall;
    function Ignore(word: PWideChar): HResult; stdcall;
    function AutoCorrect(from: PWideChar; to_: PWideChar): HResult; stdcall;
    function GetOptionValue(optionId: PWideChar; out value: Byte): HResult; stdcall;
    function Get_OptionIds(out value: IEnumString): HResult; stdcall;
    function Get_Id(out value: PWideChar): HResult; stdcall;
    function Get_LocalizedName(out value: PWideChar): HResult; stdcall;
    function add_SpellCheckerChanged(const handler: ISpellCheckerChangedEventHandler;
                                     out eventCookie: LongWord): HResult; stdcall;
    function remove_SpellCheckerChanged(eventCookie: LongWord): HResult; stdcall;
    function GetOptionDescription(optionId: PWideChar; out value: IOptionDescription): HResult; stdcall;
    function ComprehensiveCheck(text: PWideChar; out value: IEnumSpellingError): HResult; stdcall;
  end;

  ISpellChecker2 = interface(ISpellChecker)
    ['{E7ED1C71-87F7-4378-A840-C9200DACEE47}']
    function Remove(word: PWideChar): HResult; stdcall;
  end;

  IEnumSpellingError = interface(IUnknown)
    ['{803E3BD4-2828-4410-8290-418D1D73C762}']
    function Next(out value: ISpellingError): HResult; stdcall;
  end;

  ISpellingError = interface(IUnknown)
    ['{B7C82D61-FBE8-4B47-9B27-6C0D2E0DE0A3}']
    function Get_StartIndex(out value: LongWord): HResult; stdcall;
    function Get_Length(out value: LongWord): HResult; stdcall;
    function Get_CorrectiveAction(out value: CORRECTIVE_ACTION): HResult; stdcall;
    function Get_Replacement(out value: PWideChar): HResult; stdcall;
  end;

  ISpellCheckerChangedEventHandler = interface(IUnknown)
    ['{0B83A5B0-792F-4EAB-9799-ACF52C5ED08A}']
    function Invoke(const sender: ISpellChecker): HResult; stdcall;
  end;

  IOptionDescription = interface(IUnknown)
    ['{432E5F85-35CF-4606-A801-6F70277E1D7A}']
    function Get_Id(out value: PWideChar): HResult; stdcall;
    function Get_Heading(out value: PWideChar): HResult; stdcall;
    function Get_Description(out value: PWideChar): HResult; stdcall;
    function Get_Labels(out value: IEnumString): HResult; stdcall;
  end;

{$ENDREGION 'Windows Spell Checking COM Interfaces'}
{$ENDIF MSWINDOWS}

{$REGION 'Windows Spell Provider'}
{$IFDEF MSWINDOWS}

  TSynWindowsSpellProvider = class(TInterfacedObject, ISynSpellCheckProvider)
  private
    FLanguage: string;
    FFactory: ISpellCheckerFactory;
    FSpellChecker: ISpellChecker;
    FAvailable: Boolean;
    procedure CreateFactory;
    procedure CreateSpellChecker;
  public
    constructor Create; overload;
    constructor Create(const ALanguage: string); overload;
    destructor Destroy; override;
    { ISynSpellCheckProvider }
    function CheckWord(const AWord: string): Boolean;
    function Suggest(const AWord: string): TArray<string>;
    function IsAvailable: Boolean;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    { Additional }
    class function SupportedLanguages: TArray<string>;
  end;

{$ENDIF MSWINDOWS}
{$ENDREGION 'Windows Spell Provider'}

{$REGION 'Hunspell Types'}

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

{$REGION 'TSynFMXSpellCheck Component'}

  TSynFMXSpellCheck = class;

  TSynFMXSpellPaintPlugin = class(TSynFMXEditPlugin)
  private
    FSpellCheck: TSynFMXSpellCheck;
  public
    constructor Create(AOwner: TCustomFMXSynEdit;
      ASpellCheck: TSynFMXSpellCheck); reintroduce;
    destructor Destroy; override;
    procedure AfterPaint(Canvas: TCanvas; const AClip: TRectF;
      FirstLine, LastLine: Integer); override;
  end;

  TSynFMXSpellCheck = class(TComponent)
  private
    FEditor: TComponent;
    FProvider: ISynSpellCheckProvider;
    FLanguage: string;
    FErrors: TList<TSynSpellError>;
    FEnabled: Boolean;
    FUnderlineColor: TAlphaColor;
    FPaintPlugin: TSynFMXSpellPaintPlugin;
    FOnCheckComplete: TNotifyEvent;
    FCheckTokens: TSynSpellCheckTokens;
    procedure SetEditor(Value: TComponent);
    procedure SetLanguage(const Value: string);
    procedure SetEnabled(Value: Boolean);
    function GetLineText(ALine: Integer): string;
    function GetLineCount: Integer;
    function GetEditorLines: TStrings;
    function GetEditorBlockBegin: TBufferCoord;
    function GetEditorBlockEnd: TBufferCoord;
    procedure DoCheckText(const AText: string; ALine: Integer;
      AStartOffset: Integer = 0);
    procedure DoCheckLine(ALine: Integer);
    procedure InvalidateEditor;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckLine(ALine: Integer);
    procedure CheckFile;
    procedure CheckSelection;
    procedure ClearErrors;
    function ErrorAtPos(ALine, AChar: Integer): Integer;
    property Errors: TList<TSynSpellError> read FErrors;
    property Provider: ISynSpellCheckProvider read FProvider write FProvider;
    property UnderlineColor: TAlphaColor read FUnderlineColor
      write FUnderlineColor;
  published
    property Editor: TComponent read FEditor write SetEditor;
    property Language: string read FLanguage write SetLanguage;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property CheckTokens: TSynSpellCheckTokens read FCheckTokens
      write FCheckTokens default [sctComment, sctString, sctIdentifier];
    property OnCheckComplete: TNotifyEvent read FOnCheckComplete
      write FOnCheckComplete;
  end;

{$ENDREGION 'TSynFMXSpellCheck Component'}

implementation

uses
  {$IFDEF MSWINDOWS}
  System.Win.ComObj,
  {$ENDIF}
  System.Rtti,
  System.Character,
  System.IOUtils,
  SynEditHighlighter,
  SynEditTextBuffer;

type
  TSynHighlighterAccess = class(TSynCustomHighlighter);


{ ---------------------------------------------------------------------------- }
{ Local helper: identify word break characters (matching FMX.SynEdit logic)    }
{ ---------------------------------------------------------------------------- }

function IsSpellWordBreakChar(C: WideChar): Boolean; inline;
begin
  case C of
    'A'..'Z', 'a'..'z', '0'..'9', '_', '''': Result := False;
  else
    Result := True;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ Extract words from a line, returning (Word, StartChar-1based, EndChar-1based)}
{ ---------------------------------------------------------------------------- }

type
  TWordInfo = record
    Word: string;
    StartChar: Integer; // 1-based
    EndChar: Integer;   // 1-based, exclusive
  end;

function ExtractWords(const ALine: string): TArray<TWordInfo>;
var
  I, Len, WordStart: Integer;
  List: TList<TWordInfo>;
  Info: TWordInfo;
begin
  Len := Length(ALine);
  List := TList<TWordInfo>.Create;
  try
    I := 1;
    while I <= Len do
    begin
      // Skip non-word characters
      while (I <= Len) and IsSpellWordBreakChar(ALine[I]) do
        Inc(I);
      if I > Len then Break;
      // Start of a word
      WordStart := I;
      while (I <= Len) and not IsSpellWordBreakChar(ALine[I]) do
        Inc(I);
      Info.StartChar := WordStart;
      Info.EndChar := I;
      Info.Word := Copy(ALine, WordStart, I - WordStart);
      if Info.Word.Length > 0 then
        List.Add(Info);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function ContainsLetter(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I].IsLetter then
      Exit(True);
  Result := False;
end;

{ ---------------------------------------------------------------------------- }
{ RTTI helper: read a TStrings property from a component by name               }
{ ---------------------------------------------------------------------------- }

function GetObjectPropertyAsStrings(AObj: TObject;
  const APropName: string): TStrings;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Val: TValue;
begin
  Result := nil;
  if not Assigned(AObj) then
    Exit;

  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(AObj.ClassType);
    if not Assigned(RttiType) then
      Exit;
    Prop := RttiType.GetProperty(APropName);
    if not Assigned(Prop) then
      Exit;
    Val := Prop.GetValue(AObj);
    if Val.IsObject and (Val.AsObject is TStrings) then
      Result := TStrings(Val.AsObject);
  finally
    Ctx.Free;
  end;
end;

function GetRecordProperty(AObj: TObject;
  const APropName: string): TValue;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
begin
  Result := TValue.Empty;
  if not Assigned(AObj) then
    Exit;

  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(AObj.ClassType);
    if not Assigned(RttiType) then
      Exit;
    Prop := RttiType.GetProperty(APropName);
    if Assigned(Prop) then
      Result := Prop.GetValue(AObj);
  finally
    Ctx.Free;
  end;
end;

{$IFDEF MSWINDOWS}
{ ============================================================================ }
{ TSynWindowsSpellProvider                                                     }
{ ============================================================================ }

constructor TSynWindowsSpellProvider.Create;
begin
  inherited Create;
  FLanguage := 'en-US';
  FAvailable := False;
  CreateFactory;
  if Assigned(FFactory) then
    CreateSpellChecker;
end;

constructor TSynWindowsSpellProvider.Create(const ALanguage: string);
begin
  inherited Create;
  FLanguage := ALanguage;
  FAvailable := False;
  CreateFactory;
  if Assigned(FFactory) then
    CreateSpellChecker;
end;

destructor TSynWindowsSpellProvider.Destroy;
begin
  FSpellChecker := nil;
  FFactory := nil;
  inherited;
end;

procedure TSynWindowsSpellProvider.CreateFactory;
var
  HR: HResult;
  Unk: IUnknown;
begin
  FFactory := nil;
  // Windows 8+ required for spell checking API
  if not TOSVersion.Check(6, 2) then
    Exit;

  HR := CoCreateInstance(CLASS_SpellCheckerFactory, nil, CLSCTX_INPROC_SERVER,
    IID_ISpellCheckerFactory, Unk);
  if Succeeded(HR) and Assigned(Unk) then
    FFactory := Unk as ISpellCheckerFactory;
end;

procedure TSynWindowsSpellProvider.CreateSpellChecker;
var
  HR: HResult;
  Supported: Integer;
begin
  FSpellChecker := nil;
  FAvailable := False;

  if not Assigned(FFactory) then
    Exit;

  HR := FFactory.IsSupported(PChar(FLanguage), Supported);
  if Failed(HR) or (Supported = 0) then
    Exit;

  HR := FFactory.CreateSpellChecker(PChar(FLanguage), FSpellChecker);
  FAvailable := Succeeded(HR) and Assigned(FSpellChecker);
end;

function TSynWindowsSpellProvider.CheckWord(const AWord: string): Boolean;
var
  SpellingErrors: IEnumSpellingError;
  SpellingError: ISpellingError;
  HR: HResult;
begin
  Result := True;
  if not FAvailable or not Assigned(FSpellChecker) then
    Exit;

  HR := FSpellChecker.Check(PChar(AWord), SpellingErrors);
  if Failed(HR) or not Assigned(SpellingErrors) then
    Exit;

  // If Next returns S_OK, there is at least one error => word is misspelled
  Result := SpellingErrors.Next(SpellingError) <> S_OK;
end;

function TSynWindowsSpellProvider.Suggest(const AWord: string): TArray<string>;
var
  Suggestions: IEnumString;
  HR: HResult;
  Fetched: LongInt;
  Str: PWideChar;
  List: TList<string>;
begin
  SetLength(Result, 0);
  if not FAvailable or not Assigned(FSpellChecker) then
    Exit;

  HR := FSpellChecker.Suggest(PChar(AWord), Suggestions);
  if Failed(HR) or not Assigned(Suggestions) then
    Exit;

  List := TList<string>.Create;
  try
    while Suggestions.Next(1, Str, @Fetched) = S_OK do
    begin
      List.Add(string(Str));
      CoTaskMemFree(Str);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TSynWindowsSpellProvider.IsAvailable: Boolean;
begin
  Result := FAvailable;
end;

function TSynWindowsSpellProvider.GetLanguage: string;
begin
  Result := FLanguage;
end;

procedure TSynWindowsSpellProvider.SetLanguage(const Value: string);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    if Assigned(FFactory) then
      CreateSpellChecker;
  end;
end;

class function TSynWindowsSpellProvider.SupportedLanguages: TArray<string>;
var
  Factory: ISpellCheckerFactory;
  Languages: IEnumString;
  Lang: PWideChar;
  Fetched: LongInt;
  HR: HResult;
  Unk: IUnknown;
  List: TList<string>;
begin
  SetLength(Result, 0);
  if not TOSVersion.Check(6, 2) then
    Exit;

  HR := CoCreateInstance(CLASS_SpellCheckerFactory, nil, CLSCTX_INPROC_SERVER,
    IID_ISpellCheckerFactory, Unk);
  if Failed(HR) or not Assigned(Unk) then
    Exit;

  Factory := Unk as ISpellCheckerFactory;
  HR := Factory.Get_SupportedLanguages(Languages);
  if Failed(HR) or not Assigned(Languages) then
    Exit;

  List := TList<string>.Create;
  try
    while Languages.Next(1, Lang, @Fetched) = S_OK do
    begin
      List.Add(string(Lang));
      CoTaskMemFree(Lang);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

{$ENDIF MSWINDOWS}

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

{ ============================================================================ }
{ TSynFMXSpellPaintPlugin                                                      }
{ ============================================================================ }

constructor TSynFMXSpellPaintPlugin.Create(AOwner: TCustomFMXSynEdit;
  ASpellCheck: TSynFMXSpellCheck);
begin
  inherited Create(AOwner, [phAfterPaint]);
  FSpellCheck := ASpellCheck;
end;

destructor TSynFMXSpellPaintPlugin.Destroy;
begin
  inherited;
end;

procedure TSynFMXSpellPaintPlugin.AfterPaint(Canvas: TCanvas;
  const AClip: TRectF; FirstLine, LastLine: Integer);
var
  Error: TSynSpellError;
  Pt1, Pt2: TPointF;
  BaseY, X, Delta, NextX: Single;
  Up: Boolean;
begin
  if not FSpellCheck.Enabled then Exit;
  if FSpellCheck.Errors.Count = 0 then Exit;

  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := FSpellCheck.UnderlineColor;
  Canvas.Stroke.Thickness := 1.0;

  for Error in FSpellCheck.Errors do
  begin
    if (Error.Line < FirstLine) or (Error.Line > LastLine) then
      Continue;

    Pt1 := Owner.BufferCoordToPixel(BufferCoord(Error.StartChar, Error.Line));
    Pt2 := Owner.BufferCoordToPixel(BufferCoord(Error.EndChar, Error.Line));
    BaseY := Pt1.Y + Owner.LineHeight - 1;
    Delta := Owner.LineHeight / 6;

    // Draw zigzag wave
    X := Pt1.X;
    Up := True;
    while X < Pt2.X do
    begin
      NextX := Min(X + Delta, Pt2.X);
      if Up then
        Canvas.DrawLine(PointF(X, BaseY), PointF(NextX, BaseY - Delta), 1.0)
      else
        Canvas.DrawLine(PointF(X, BaseY - Delta), PointF(NextX, BaseY), 1.0);
      X := NextX;
      Up := not Up;
    end;
  end;
end;

{ ============================================================================ }
{ TSynFMXSpellCheck                                                            }
{ ============================================================================ }

constructor TSynFMXSpellCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FErrors := TList<TSynSpellError>.Create;
  FEnabled := True;
  FLanguage := 'en-US';
  FUnderlineColor := TAlphaColors.Red;
  FCheckTokens := [sctComment, sctString, sctIdentifier];
  FProvider := nil;
  FEditor := nil;
end;

destructor TSynFMXSpellCheck.Destroy;
begin
  FPaintPlugin.Free;
  if Assigned(FEditor) then
    FEditor.RemoveFreeNotification(Self);
  FErrors.Free;
  FProvider := nil;
  inherited;
end;

procedure TSynFMXSpellCheck.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FEditor) then
  begin
    FreeAndNil(FPaintPlugin);
    FEditor := nil;
  end;
end;

procedure TSynFMXSpellCheck.SetEditor(Value: TComponent);
begin
  if Value <> FEditor then
  begin
    FreeAndNil(FPaintPlugin);
    if Assigned(FEditor) then
      FEditor.RemoveFreeNotification(Self);
    FEditor := Value;
    if Assigned(FEditor) then
    begin
      FEditor.FreeNotification(Self);
      if FEditor is TCustomFMXSynEdit then
        FPaintPlugin := TSynFMXSpellPaintPlugin.Create(
          TCustomFMXSynEdit(FEditor), Self);
    end;
    ClearErrors;
  end;
end;

procedure TSynFMXSpellCheck.SetLanguage(const Value: string);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    if Assigned(FProvider) then
      FProvider.SetLanguage(Value);
    ClearErrors;
  end;
end;

procedure TSynFMXSpellCheck.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not FEnabled then
      ClearErrors
    else
      InvalidateEditor;
  end;
end;

function TSynFMXSpellCheck.GetEditorLines: TStrings;
begin
  Result := GetObjectPropertyAsStrings(FEditor, 'Lines');
end;

function TSynFMXSpellCheck.GetEditorBlockBegin: TBufferCoord;
var
  Val: TValue;
begin
  Result := BufferCoord(0, 0);
  Val := GetRecordProperty(FEditor, 'BlockBegin');
  if not Val.IsEmpty then
    try
      Result := Val.AsType<TBufferCoord>;
    except
      // If type mismatch, return default
    end;
end;

function TSynFMXSpellCheck.GetEditorBlockEnd: TBufferCoord;
var
  Val: TValue;
begin
  Result := BufferCoord(0, 0);
  Val := GetRecordProperty(FEditor, 'BlockEnd');
  if not Val.IsEmpty then
    try
      Result := Val.AsType<TBufferCoord>;
    except
      // If type mismatch, return default
    end;
end;

function TSynFMXSpellCheck.GetLineText(ALine: Integer): string;
var
  Lines: TStrings;
begin
  Result := '';
  Lines := GetEditorLines;
  if Assigned(Lines) and (ALine >= 1) and (ALine <= Lines.Count) then
    Result := Lines[ALine - 1];
end;

function TSynFMXSpellCheck.GetLineCount: Integer;
var
  Lines: TStrings;
begin
  Result := 0;
  Lines := GetEditorLines;
  if Assigned(Lines) then
    Result := Lines.Count;
end;

procedure TSynFMXSpellCheck.DoCheckText(const AText: string; ALine: Integer;
  AStartOffset: Integer);
var
  Words: TArray<TWordInfo>;
  Info: TWordInfo;
  Err: TSynSpellError;
begin
  if not Assigned(FProvider) or not FProvider.IsAvailable then
    Exit;
  if AText = '' then
    Exit;

  Words := ExtractWords(AText);
  for Info in Words do
  begin
    // Skip tokens that are all digits or have no letters
    if not ContainsLetter(Info.Word) then
      Continue;

    if not FProvider.CheckWord(Info.Word) then
    begin
      Err.Line := ALine;
      Err.StartChar := Info.StartChar + AStartOffset;
      Err.EndChar := Info.EndChar + AStartOffset;
      Err.Word := Info.Word;
      FErrors.Add(Err);
    end;
  end;
end;

procedure TSynFMXSpellCheck.DoCheckLine(ALine: Integer);
var
  Ed: TCustomFMXSynEdit;
  HL: TSynCustomHighlighter;
  Attr, DefComment, DefString, DefIdent: TSynHighlighterAttributes;
  LineText, Token: string;
  TokenPos: Integer;
  ShouldCheck: Boolean;
begin
  LineText := GetLineText(ALine);
  if LineText = '' then
    Exit;

  // If the editor has a highlighter and we have token filters, use them
  if (FEditor is TCustomFMXSynEdit) then
  begin
    Ed := TCustomFMXSynEdit(FEditor);
    HL := Ed.Highlighter;
    if Assigned(HL) and (FCheckTokens <> []) then
    begin
      // Cache default attributes for comparison
      DefComment := TSynHighlighterAccess(HL).GetDefaultAttribute(SYN_ATTR_COMMENT);
      DefString := TSynHighlighterAccess(HL).GetDefaultAttribute(SYN_ATTR_STRING);
      DefIdent := TSynHighlighterAccess(HL).GetDefaultAttribute(SYN_ATTR_IDENTIFIER);

      // Set up highlighter range from previous line
      if ALine <= 1 then
        HL.ResetRange
      else
        HL.SetRange(Ed.Lines.Ranges[ALine - 2]); // 0-based index

      HL.SetLine(LineText, ALine - 1);

      while not HL.GetEol do
      begin
        Attr := HL.GetTokenAttribute;
        ShouldCheck := False;

        if Assigned(Attr) then
        begin
          if (sctComment in FCheckTokens) and (Attr = DefComment) then
            ShouldCheck := True
          else if (sctString in FCheckTokens) and (Attr = DefString) then
            ShouldCheck := True
          else if (sctIdentifier in FCheckTokens) and (Attr = DefIdent) then
            ShouldCheck := True;
        end;

        if ShouldCheck then
        begin
          Token := HL.GetToken;
          TokenPos := HL.GetTokenPos; // 0-based
          DoCheckText(Token, ALine, TokenPos);
        end;

        HL.Next;
      end;
      Exit;
    end;
  end;

  // Fallback: no highlighter — check entire line
  DoCheckText(LineText, ALine);
end;

procedure TSynFMXSpellCheck.InvalidateEditor;
begin
  if FEditor is TCustomFMXSynEdit then
    TCustomFMXSynEdit(FEditor).Repaint;
end;

procedure TSynFMXSpellCheck.CheckLine(ALine: Integer);
var
  I: Integer;
begin
  if not FEnabled or not Assigned(FEditor) or not Assigned(FProvider) then
    Exit;

  // Remove existing errors for this line
  for I := FErrors.Count - 1 downto 0 do
    if FErrors[I].Line = ALine then
      FErrors.Delete(I);

  DoCheckLine(ALine);

  InvalidateEditor;

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.CheckFile;
var
  I, LC: Integer;
begin
  if not FEnabled or not Assigned(FEditor) or not Assigned(FProvider) then
    Exit;

  ClearErrors;

  LC := GetLineCount;
  for I := 1 to LC do
    DoCheckLine(I);

  InvalidateEditor;

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.CheckSelection;
var
  BB, BE: TBufferCoord;
  Line, I: Integer;
  Err: TSynSpellError;
begin
  if not FEnabled or not Assigned(FEditor) or not Assigned(FProvider) then
    Exit;

  BB := GetEditorBlockBegin;
  BE := GetEditorBlockEnd;

  // If no valid selection, check the whole file
  if (BB.Line = 0) or (BE.Line = 0) or
     ((BB.Line = BE.Line) and (BB.Char = BE.Char)) then
  begin
    CheckFile;
    Exit;
  end;

  // Check each line in the selection using highlighter-aware logic
  for Line := BB.Line to BE.Line do
    DoCheckLine(Line);

  // Remove errors that fall outside the selection bounds
  for I := FErrors.Count - 1 downto 0 do
  begin
    Err := FErrors[I];
    if (Err.Line = BB.Line) and (Err.StartChar < BB.Char) then
      FErrors.Delete(I)
    else if (Err.Line = BE.Line) and (Err.EndChar > BE.Char) then
      FErrors.Delete(I);
  end;

  InvalidateEditor;

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.ClearErrors;
begin
  FErrors.Clear;
  InvalidateEditor;
end;

function TSynFMXSpellCheck.ErrorAtPos(ALine, AChar: Integer): Integer;
var
  I: Integer;
  Err: TSynSpellError;
begin
  for I := 0 to FErrors.Count - 1 do
  begin
    Err := FErrors[I];
    if (Err.Line = ALine) and (AChar >= Err.StartChar) and
       (AChar < Err.EndChar) then
      Exit(I);
  end;
  Result := -1;
end;

end.
