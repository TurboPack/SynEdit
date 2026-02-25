unit TestFMXSynEditSearch;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit,
  SynEditSearch,
  SynEditRegexSearch;

type
  [TestFixture]
  TTestFMXSynEditSearch = class
  private
    FEditor: TFMXSynEdit;
    FSearchEngine: TSynEditSearch;
    FRegexEngine: TSynEditRegexSearch;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestSearchFindsMatch;
    [Test]
    procedure TestSearchNoMatch;
    [Test]
    procedure TestSearchCaseSensitive;
    [Test]
    procedure TestSearchCaseInsensitive;
    [Test]
    procedure TestSearchWholeWord;
    [Test]
    procedure TestSearchReplace;
    [Test]
    procedure TestSearchReplaceAll;
    [Test]
    procedure TestRegexSearch;
    [Test]
    procedure TestRegexReplace;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes;

procedure TTestFMXSynEditSearch.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FSearchEngine := TSynEditSearch.Create(nil);
  FRegexEngine := TSynEditRegexSearch.Create(nil);
end;

procedure TTestFMXSynEditSearch.TearDown;
begin
  FEditor.SearchEngine := nil;
  FRegexEngine.Free;
  FSearchEngine.Free;
  FEditor.Free;
end;

procedure TTestFMXSynEditSearch.TestSearchFindsMatch;
var
  Count: Integer;
begin
  FEditor.Text := 'Hello World';
  FEditor.SearchEngine := FSearchEngine;
  Count := FEditor.SearchReplace('World', '', [ssoEntireScope]);
  Assert.IsTrue(Count > 0, 'Should find "World" in text');
end;

procedure TTestFMXSynEditSearch.TestSearchNoMatch;
var
  Count: Integer;
begin
  FEditor.Text := 'Hello World';
  FEditor.SearchEngine := FSearchEngine;
  Count := FEditor.SearchReplace('XYZ', '', [ssoEntireScope]);
  Assert.AreEqual(0, Count, 'Should not find "XYZ" in text');
end;

procedure TTestFMXSynEditSearch.TestSearchCaseSensitive;
var
  Count: Integer;
begin
  FEditor.Text := 'Hello World';
  FEditor.SearchEngine := FSearchEngine;
  // 'hello' with case-sensitive should NOT match 'Hello'
  Count := FEditor.SearchReplace('hello', '', [ssoEntireScope, ssoMatchCase]);
  Assert.AreEqual(0, Count, 'Case-sensitive search should not match different case');
end;

procedure TTestFMXSynEditSearch.TestSearchCaseInsensitive;
var
  Count: Integer;
begin
  FEditor.Text := 'Hello World';
  FEditor.SearchEngine := FSearchEngine;
  // 'hello' without case-sensitive should match 'Hello'
  Count := FEditor.SearchReplace('hello', '', [ssoEntireScope]);
  Assert.IsTrue(Count > 0, 'Case-insensitive search should match different case');
end;

procedure TTestFMXSynEditSearch.TestSearchWholeWord;
var
  Count: Integer;
begin
  FEditor.Text := 'cat concatenate';
  FEditor.SearchEngine := FSearchEngine;
  // Replace all 'cat' whole-word only: should replace 'cat' but not 'cat' in 'concatenate'
  Count := FEditor.SearchReplace('cat', 'dog',
    [ssoEntireScope, ssoReplace, ssoReplaceAll, ssoWholeWord]);
  Assert.AreEqual(1, Count, 'Whole-word should match only standalone "cat"');
  Assert.AreEqual('dog concatenate', FEditor.Text);
end;

procedure TTestFMXSynEditSearch.TestSearchReplace;
begin
  FEditor.Text := 'Hello World Hello';
  FEditor.SearchEngine := FSearchEngine;
  FEditor.SearchReplace('Hello', 'Hi',
    [ssoEntireScope, ssoReplace]);
  // Single replace: only the first occurrence should be replaced
  Assert.AreEqual('Hi World Hello', FEditor.Text);
end;

procedure TTestFMXSynEditSearch.TestSearchReplaceAll;
var
  Count: Integer;
begin
  FEditor.Text := 'aaa bbb aaa bbb aaa';
  FEditor.SearchEngine := FSearchEngine;
  Count := FEditor.SearchReplace('aaa', 'xxx',
    [ssoEntireScope, ssoReplace, ssoReplaceAll]);
  Assert.AreEqual(3, Count, 'Should replace all 3 occurrences');
  Assert.AreEqual('xxx bbb xxx bbb xxx', FEditor.Text);
end;

procedure TTestFMXSynEditSearch.TestRegexSearch;
var
  Count: Integer;
begin
  FEditor.Text := 'abc 123 def 456';
  FEditor.SearchEngine := FRegexEngine;
  Count := FEditor.SearchReplace('\d+', '', [ssoEntireScope]);
  Assert.IsTrue(Count > 0, 'Regex should find digit sequence');
end;

procedure TTestFMXSynEditSearch.TestRegexReplace;
var
  Count: Integer;
begin
  FEditor.Text := 'abc 123 def 456';
  FEditor.SearchEngine := FRegexEngine;
  Count := FEditor.SearchReplace('\d+', 'NUM',
    [ssoEntireScope, ssoReplace, ssoReplaceAll]);
  Assert.AreEqual(2, Count, 'Should replace both digit sequences');
  Assert.AreEqual('abc NUM def NUM', FEditor.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditSearch);

end.
