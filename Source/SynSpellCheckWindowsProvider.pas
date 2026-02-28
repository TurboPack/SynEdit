{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{ Windows Spell Checking API provider implementing ISynSpellCheckProvider.
  Framework-neutral — works with both VCL and FMX.
  Requires Windows 8+ at runtime. }

unit SynSpellCheckWindowsProvider;

{$I SynEdit.inc}

{$IFDEF MSWINDOWS}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  SynSpellCheckTypes,
  SynSpellCheckWinAPI;

type
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

implementation

uses
  System.Win.ComObj;

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

end.
