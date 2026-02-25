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
  System.Generics.Collections,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ActiveX,
  {$ENDIF}
  SynEditTypes,
  SynEditMiscProcs;

{$REGION 'Spell Error Record'}

type
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

{$REGION 'Hunspell Provider'}

  TSynHunspellProvider = class(TInterfacedObject, ISynSpellCheckProvider)
  private
    FLanguage: string;
    FDictionaryPath: string;
    FLoaded: Boolean;
    // Placeholder: in a full implementation these would hold parsed
    // dictionary and affix data
    FWordList: TDictionary<string, Boolean>;
    procedure LoadDictionary;
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

{$REGION 'TSynFMXSpellCheck Component'}

  TSynFMXSpellCheck = class(TComponent)
  private
    FEditor: TComponent;
    FProvider: ISynSpellCheckProvider;
    FLanguage: string;
    FErrors: TList<TSynSpellError>;
    FEnabled: Boolean;
    FOnCheckComplete: TNotifyEvent;
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
  published
    property Editor: TComponent read FEditor write SetEditor;
    property Language: string read FLanguage write SetLanguage;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
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
  System.IOUtils;

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
  FWordList := TDictionary<string, Boolean>.Create;
end;

constructor TSynHunspellProvider.Create(const ADictionaryPath: string;
  const ALanguage: string);
begin
  inherited Create;
  FDictionaryPath := ADictionaryPath;
  FLanguage := ALanguage;
  FLoaded := False;
  FWordList := TDictionary<string, Boolean>.Create;
end;

destructor TSynHunspellProvider.Destroy;
begin
  FWordList.Free;
  inherited;
end;

procedure TSynHunspellProvider.LoadDictionary;
var
  DicFile: string;
  Lines: TStringList;
  I: Integer;
  Entry: string;
  SlashPos: Integer;
begin
  FWordList.Clear;
  FLoaded := False;

  if FDictionaryPath = '' then
    Exit;

  // Look for <path>/<language>.dic
  if FLanguage <> '' then
    DicFile := TPath.Combine(FDictionaryPath, FLanguage + '.dic')
  else
    // Treat DictionaryPath as the full path to the .dic file
    DicFile := FDictionaryPath;

  if not TFile.Exists(DicFile) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(DicFile);
    // Hunspell .dic format: first line is word count, then one word per line.
    // Words may have suffix flags after a slash, e.g. "word/ABC"
    for I := 1 to Lines.Count - 1 do // skip first line (count)
    begin
      Entry := Trim(Lines[I]);
      if Entry = '' then
        Continue;
      // Strip affix flags after '/'
      SlashPos := Pos('/', Entry);
      if SlashPos > 0 then
        Entry := Copy(Entry, 1, SlashPos - 1);
      // Store lowercase for case-insensitive lookup
      FWordList.AddOrSetValue(LowerCase(Entry), True);
    end;
    FLoaded := True;
  finally
    Lines.Free;
  end;
end;

function TSynHunspellProvider.CheckWord(const AWord: string): Boolean;
begin
  // If dictionary not loaded yet, try to load
  if not FLoaded and (FDictionaryPath <> '') then
    LoadDictionary;

  // If still not loaded or empty dictionary, accept all words
  // (stub behavior - a real implementation would parse .aff rules too)
  if not FLoaded or (FWordList.Count = 0) then
    Exit(True);

  Result := FWordList.ContainsKey(LowerCase(AWord));
end;

function TSynHunspellProvider.Suggest(const AWord: string): TArray<string>;
begin
  // Stub: a real Hunspell implementation would generate suggestions
  // based on edit distance and affix rules from the .aff file
  SetLength(Result, 0);
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
  FProvider := nil;
  FEditor := nil;
end;

destructor TSynFMXSpellCheck.Destroy;
begin
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
    FEditor := nil;
end;

procedure TSynFMXSpellCheck.SetEditor(Value: TComponent);
begin
  if Value <> FEditor then
  begin
    if Assigned(FEditor) then
      FEditor.RemoveFreeNotification(Self);
    FEditor := Value;
    if Assigned(FEditor) then
      FEditor.FreeNotification(Self);
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
      ClearErrors;
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

procedure TSynFMXSpellCheck.CheckLine(ALine: Integer);
var
  LineText: string;
  I: Integer;
begin
  if not FEnabled or not Assigned(FEditor) or not Assigned(FProvider) then
    Exit;

  // Remove existing errors for this line
  for I := FErrors.Count - 1 downto 0 do
    if FErrors[I].Line = ALine then
      FErrors.Delete(I);

  LineText := GetLineText(ALine);
  DoCheckText(LineText, ALine);

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
    DoCheckText(GetLineText(I), I);

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.CheckSelection;
var
  BB, BE: TBufferCoord;
  Line: Integer;
  LineText: string;
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

  if BB.Line = BE.Line then
  begin
    // Single-line selection
    LineText := GetLineText(BB.Line);
    if (BB.Char >= 1) and (BE.Char >= 1) and (BB.Char <= Length(LineText) + 1) then
    begin
      LineText := Copy(LineText, BB.Char, BE.Char - BB.Char);
      DoCheckText(LineText, BB.Line, BB.Char - 1);
    end;
  end
  else
  begin
    // Multi-line selection
    // First line: from BB.Char to end of line
    LineText := GetLineText(BB.Line);
    if BB.Char >= 1 then
    begin
      LineText := Copy(LineText, BB.Char, MaxInt);
      DoCheckText(LineText, BB.Line, BB.Char - 1);
    end;
    // Middle lines: full lines
    for Line := BB.Line + 1 to BE.Line - 1 do
      DoCheckText(GetLineText(Line), Line);
    // Last line: from start to BE.Char
    if BE.Char > 1 then
    begin
      LineText := GetLineText(BE.Line);
      LineText := Copy(LineText, 1, BE.Char - 1);
      DoCheckText(LineText, BE.Line);
    end;
  end;

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.ClearErrors;
begin
  FErrors.Clear;
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
