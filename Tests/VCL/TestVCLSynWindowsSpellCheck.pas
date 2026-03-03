unit TestVCLSynWindowsSpellCheck;

{$IFDEF MSWINDOWS}

interface

uses
  DUnitX.TestFramework,
  SynSpellCheckTypes,
  SynSpellCheckWindowsProvider;

type
  [TestFixture]
  TTestVCLSynWindowsSpellProvider = class
  private
    FProvider: ISynSpellCheckProvider;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestIsAvailable;
    [Test]
    procedure TestGetLanguage;
    [Test]
    procedure TestCreateDefaultLanguage;
    [Test]
    procedure TestSetLanguageChangesChecker;
    [Test]
    procedure TestUnsupportedLanguage;
    [Test]
    procedure TestCheckWordValid;
    [Test]
    procedure TestCheckWordInvalid;
    [Test]
    procedure TestSuggestReturnsResults;
    [Test]
    procedure TestSuggestContainsCorrection;
    [Test]
    procedure TestSupportedLanguagesNotEmpty;
    [Test]
    procedure TestSupportedLanguagesContainsEnUS;
  end;

implementation

uses
  System.SysUtils;

procedure TTestVCLSynWindowsSpellProvider.Setup;
begin
  FProvider := TSynWindowsSpellProvider.Create('en-US');
end;

procedure TTestVCLSynWindowsSpellProvider.TearDown;
begin
  FProvider := nil;
end;

procedure TTestVCLSynWindowsSpellProvider.TestIsAvailable;
begin
  Assert.IsTrue(FProvider.IsAvailable,
    'en-US provider should be available on Windows 8+');
end;

procedure TTestVCLSynWindowsSpellProvider.TestGetLanguage;
begin
  Assert.AreEqual('en-US', FProvider.GetLanguage);
end;

procedure TTestVCLSynWindowsSpellProvider.TestCreateDefaultLanguage;
var
  P: ISynSpellCheckProvider;
begin
  P := TSynWindowsSpellProvider.Create;
  Assert.AreEqual('en-US', P.GetLanguage,
    'Parameterless Create should default to en-US');
  Assert.IsTrue(P.IsAvailable,
    'Default provider should be available');
end;

procedure TTestVCLSynWindowsSpellProvider.TestSetLanguageChangesChecker;
begin
  // Confirm initial state
  Assert.IsTrue(FProvider.IsAvailable, 'Should start available');
  // Change to a bogus language
  FProvider.SetLanguage('xx-XX');
  Assert.IsFalse(FProvider.IsAvailable,
    'Bogus language should not be available');
  // Change back to en-US
  FProvider.SetLanguage('en-US');
  Assert.IsTrue(FProvider.IsAvailable,
    'Restoring en-US should be available again');
end;

procedure TTestVCLSynWindowsSpellProvider.TestUnsupportedLanguage;
var
  P: ISynSpellCheckProvider;
begin
  P := TSynWindowsSpellProvider.Create('xx-BOGUS');
  Assert.IsFalse(P.IsAvailable,
    'Bogus language tag should not be available');
end;

procedure TTestVCLSynWindowsSpellProvider.TestCheckWordValid;
begin
  Assert.IsTrue(FProvider.CheckWord('hello'), '"hello" should be valid');
  Assert.IsTrue(FProvider.CheckWord('world'), '"world" should be valid');
  Assert.IsTrue(FProvider.CheckWord('the'), '"the" should be valid');
end;

procedure TTestVCLSynWindowsSpellProvider.TestCheckWordInvalid;
begin
  Assert.IsFalse(FProvider.CheckWord('helo'), '"helo" should be invalid');
  Assert.IsFalse(FProvider.CheckWord('wrold'), '"wrold" should be invalid');
end;

procedure TTestVCLSynWindowsSpellProvider.TestSuggestReturnsResults;
var
  Suggestions: TArray<string>;
begin
  Suggestions := FProvider.Suggest('helo');
  Assert.IsTrue(Length(Suggestions) >= 1,
    '"helo" should produce at least 1 suggestion, got ' +
    IntToStr(Length(Suggestions)));
end;

procedure TTestVCLSynWindowsSpellProvider.TestSuggestContainsCorrection;
var
  Suggestions: TArray<string>;
  S: string;
  Found: Boolean;
begin
  Suggestions := FProvider.Suggest('helo');
  Found := False;
  for S in Suggestions do
    if SameText(S, 'hello') then
    begin
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, '"helo" suggestions should include "hello"');
end;

procedure TTestVCLSynWindowsSpellProvider.TestSupportedLanguagesNotEmpty;
var
  Langs: TArray<string>;
begin
  Langs := TSynWindowsSpellProvider.SupportedLanguages;
  Assert.IsTrue(Length(Langs) >= 1,
    'SupportedLanguages should return at least 1 language, got ' +
    IntToStr(Length(Langs)));
end;

procedure TTestVCLSynWindowsSpellProvider.TestSupportedLanguagesContainsEnUS;
var
  Langs: TArray<string>;
  S: string;
  Found: Boolean;
begin
  Langs := TSynWindowsSpellProvider.SupportedLanguages;
  Found := False;
  for S in Langs do
    if SameText(S, 'en-US') then
    begin
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, 'en-US should be in SupportedLanguages');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestVCLSynWindowsSpellProvider);

{$ENDIF MSWINDOWS}

end.
