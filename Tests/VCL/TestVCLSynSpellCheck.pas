unit TestVCLSynSpellCheck;

interface

uses
  DUnitX.TestFramework,
  SynSpellCheckTypes,
  SynSpellCheckHunspellProvider;

type
  [TestFixture]
  TTestVCLSynHunspellProvider = class
  private
    FProvider: ISynSpellCheckProvider;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    { Loading }
    [Test]
    procedure TestIsAvailable;
    [Test]
    procedure TestIsAvailableMissingPath;
    [Test]
    procedure TestGetLanguage;
    [Test]
    procedure TestSetLanguageReloads;
    { Direct word checks }
    [Test]
    procedure TestCommonWordsValid;
    [Test]
    procedure TestMisspelledWordsInvalid;
    [Test]
    procedure TestCaseInsensitive;
    { Suffix rules }
    [Test]
    procedure TestSuffixIng;
    [Test]
    procedure TestSuffixEd;
    [Test]
    procedure TestSuffixPlural;
    [Test]
    procedure TestSuffixIon;
    [Test]
    procedure TestSuffixIve;
    { Prefix rules }
    [Test]
    procedure TestPrefixUn;
    [Test]
    procedure TestPrefixRe;
    { Cross-product }
    [Test]
    procedure TestCrossProduct;
    { Suggest }
    [Test]
    procedure TestSuggestReturnsResults;
    [Test]
    procedure TestSuggestMaxCount;
    [Test]
    procedure TestSuggestContainsCorrection;
    { Native provider stub }
    [Test]
    procedure TestNativeNotAvailable;
    [Test]
    procedure TestNativeCheckWordTrue;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

const
  DictLang = 'en_US';

function GetDictPath: string;
begin
  // Resolve relative to exe location (Tests/bin/Win32/Debug/)
  Result := TPath.GetFullPath(
    TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\..\Dictionaries'));
end;

procedure TTestVCLSynHunspellProvider.Setup;
begin
  FProvider := TSynHunspellProvider.Create(GetDictPath, DictLang);
end;

procedure TTestVCLSynHunspellProvider.TearDown;
begin
  FProvider := nil;
end;

{ Loading }

procedure TTestVCLSynHunspellProvider.TestIsAvailable;
begin
  Assert.IsTrue(FProvider.IsAvailable, 'en_US dictionary should be available');
end;

procedure TTestVCLSynHunspellProvider.TestIsAvailableMissingPath;
var
  P: ISynSpellCheckProvider;
begin
  P := TSynHunspellProvider.Create('C:\nonexistent\path', 'en_US');
  Assert.IsFalse(P.IsAvailable, 'Missing path should not be available');
end;

procedure TTestVCLSynHunspellProvider.TestGetLanguage;
begin
  Assert.AreEqual('en_US', FProvider.GetLanguage);
end;

procedure TTestVCLSynHunspellProvider.TestSetLanguageReloads;
begin
  // Force load
  FProvider.IsAvailable;
  // Change to non-existent language — should clear and fail to reload
  FProvider.SetLanguage('xx_XX');
  Assert.IsFalse(FProvider.IsAvailable, 'Invalid language should not be available');
  // Change back — should reload successfully
  FProvider.SetLanguage('en_US');
  Assert.IsTrue(FProvider.IsAvailable, 'Restoring en_US should reload');
end;

{ Direct word checks }

procedure TTestVCLSynHunspellProvider.TestCommonWordsValid;
begin
  Assert.IsTrue(FProvider.CheckWord('hello'), '"hello" should be valid');
  Assert.IsTrue(FProvider.CheckWord('world'), '"world" should be valid');
  Assert.IsTrue(FProvider.CheckWord('the'), '"the" should be valid');
  Assert.IsTrue(FProvider.CheckWord('computer'), '"computer" should be valid');
end;

procedure TTestVCLSynHunspellProvider.TestMisspelledWordsInvalid;
begin
  Assert.IsFalse(FProvider.CheckWord('helo'), '"helo" should be invalid');
  Assert.IsFalse(FProvider.CheckWord('wrold'), '"wrold" should be invalid');
  Assert.IsFalse(FProvider.CheckWord('teh'), '"teh" should be invalid');
  Assert.IsFalse(FProvider.CheckWord('computr'), '"computr" should be invalid');
end;

procedure TTestVCLSynHunspellProvider.TestCaseInsensitive;
begin
  Assert.IsTrue(FProvider.CheckWord('Hello'), '"Hello" should be valid');
  Assert.IsTrue(FProvider.CheckWord('HELLO'), '"HELLO" should be valid');
  Assert.IsTrue(FProvider.CheckWord('hello'), '"hello" should be valid');
end;

{ Suffix rules }

procedure TTestVCLSynHunspellProvider.TestSuffixIng;
begin
  Assert.IsTrue(FProvider.CheckWord('walking'), '"walking" should be valid');
  Assert.IsTrue(FProvider.CheckWord('talking'), '"talking" should be valid');
end;

procedure TTestVCLSynHunspellProvider.TestSuffixEd;
begin
  Assert.IsTrue(FProvider.CheckWord('walked'), '"walked" should be valid');
  Assert.IsTrue(FProvider.CheckWord('talked'), '"talked" should be valid');
end;

procedure TTestVCLSynHunspellProvider.TestSuffixPlural;
begin
  Assert.IsTrue(FProvider.CheckWord('classes'), '"classes" should be valid');
  Assert.IsTrue(FProvider.CheckWord('boxes'), '"boxes" should be valid');
end;

procedure TTestVCLSynHunspellProvider.TestSuffixIon;
begin
  Assert.IsTrue(FProvider.CheckWord('creation'), '"creation" should be valid');
end;

procedure TTestVCLSynHunspellProvider.TestSuffixIve;
begin
  Assert.IsTrue(FProvider.CheckWord('creative'), '"creative" should be valid');
end;

{ Prefix rules }

procedure TTestVCLSynHunspellProvider.TestPrefixUn;
begin
  Assert.IsTrue(FProvider.CheckWord('unlikely'), '"unlikely" should be valid');
end;

procedure TTestVCLSynHunspellProvider.TestPrefixRe;
begin
  Assert.IsTrue(FProvider.CheckWord('reuse'), '"reuse" should be valid');
end;

{ Cross-product }

procedure TTestVCLSynHunspellProvider.TestCrossProduct;
begin
  Assert.IsTrue(FProvider.CheckWord('undoing'), '"undoing" should be valid');
end;

{ Suggest }

procedure TTestVCLSynHunspellProvider.TestSuggestReturnsResults;
var
  Suggestions: TArray<string>;
begin
  Suggestions := FProvider.Suggest('helo');
  Assert.IsTrue(Length(Suggestions) >= 3,
    '"helo" should produce at least 3 suggestions, got ' + IntToStr(Length(Suggestions)));
end;

procedure TTestVCLSynHunspellProvider.TestSuggestMaxCount;
var
  Suggestions: TArray<string>;
begin
  Suggestions := FProvider.Suggest('helo');
  Assert.IsTrue(Length(Suggestions) > 0,
    'Should return at least 1 suggestion');
  Assert.IsTrue(Length(Suggestions) <= 10,
    'Should return at most 10 suggestions, got ' + IntToStr(Length(Suggestions)));
end;

procedure TTestVCLSynHunspellProvider.TestSuggestContainsCorrection;
var
  Suggestions: TArray<string>;
  S: string;
  Found: Boolean;
begin
  Suggestions := FProvider.Suggest('walkin');
  Found := False;
  for S in Suggestions do
    if SameText(S, 'walking') then
    begin
      Found := True;
      Break;
    end;
  Assert.IsTrue(Found, '"walkin" suggestions should include "walking"');
end;

{ Native provider stub }

procedure TTestVCLSynHunspellProvider.TestNativeNotAvailable;
var
  P: ISynSpellCheckProvider;
begin
  P := TSynHunspellNativeProvider.Create(GetDictPath, DictLang);
  Assert.IsFalse(P.IsAvailable, 'Native provider should not be available');
end;

procedure TTestVCLSynHunspellProvider.TestNativeCheckWordTrue;
var
  P: ISynSpellCheckProvider;
begin
  P := TSynHunspellNativeProvider.Create(GetDictPath, DictLang);
  Assert.IsTrue(P.CheckWord('anything'), 'Native stub should accept all words');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestVCLSynHunspellProvider);

end.
