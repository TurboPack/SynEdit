unit TestFMXSynEditCodeFolding;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit,
  SynHighlighterJSON;

type
  [TestFixture]
  TTestFMXSynEditCodeFolding = class
  private
    FEditor: TFMXSynEdit;
    FHighlighter: TSynJSONSyn;
    procedure SetupFoldableContent;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestFoldRangesDetected;
    [Test]
    procedure TestCollapseAll;
    [Test]
    procedure TestUncollapseAll;
    [Test]
    procedure TestUseCodeFoldingRequiresHighlighter;
    [Test]
    procedure TestCollapseLevel;
    [Test]
    procedure TestUncollapseLevel;
  end;

implementation

uses
  System.SysUtils,
  SynEditCodeFolding;

const
  // JSON with nested braces and brackets for fold testing
  SampleJSON =
    '{' + sLineBreak +
    '  "name": "test",' + sLineBreak +
    '  "items": [' + sLineBreak +
    '    1,' + sLineBreak +
    '    2,' + sLineBreak +
    '    3' + sLineBreak +
    '  ]' + sLineBreak +
    '}';

procedure TTestFMXSynEditCodeFolding.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FHighlighter := TSynJSONSyn.Create(nil);
end;

procedure TTestFMXSynEditCodeFolding.TearDown;
begin
  FEditor.Free;
  FHighlighter.Free;
end;

procedure TTestFMXSynEditCodeFolding.SetupFoldableContent;
begin
  FEditor.Highlighter := FHighlighter;
  FEditor.Text := SampleJSON;
  // UseCodeFolding must be set AFTER Text so that SetUseCodeFolding
  // triggers FullFoldScan with text and highlighter both in place
  FEditor.UseCodeFolding := True;
end;

procedure TTestFMXSynEditCodeFolding.TestFoldRangesDetected;
begin
  SetupFoldableContent;
  Assert.IsTrue(FEditor.AllFoldRanges.Count > 0,
    'Fold ranges should be detected in JSON with braces/brackets');
end;

procedure TTestFMXSynEditCodeFolding.TestCollapseAll;
var
  I: Integer;
  AnyCollapsed: Boolean;
begin
  SetupFoldableContent;
  FEditor.CollapseAll;
  AnyCollapsed := False;
  for I := 0 to FEditor.AllFoldRanges.Count - 1 do
    if FEditor.AllFoldRanges[I].Collapsed then
    begin
      AnyCollapsed := True;
      Break;
    end;
  Assert.IsTrue(AnyCollapsed, 'At least one fold range should be collapsed');
end;

procedure TTestFMXSynEditCodeFolding.TestUncollapseAll;
var
  I: Integer;
  AnyCollapsed: Boolean;
begin
  SetupFoldableContent;
  FEditor.CollapseAll;
  FEditor.UncollapseAll;
  AnyCollapsed := False;
  for I := 0 to FEditor.AllFoldRanges.Count - 1 do
    if FEditor.AllFoldRanges[I].Collapsed then
    begin
      AnyCollapsed := True;
      Break;
    end;
  Assert.IsFalse(AnyCollapsed, 'No fold ranges should be collapsed after UncollapseAll');
end;

procedure TTestFMXSynEditCodeFolding.TestUseCodeFoldingRequiresHighlighter;
begin
  // Without a highlighter, UseCodeFolding should not produce fold ranges
  FEditor.UseCodeFolding := True;
  FEditor.Text := SampleJSON;
  Assert.AreEqual(0, FEditor.AllFoldRanges.Count,
    'No fold ranges without a highlighter');
end;

procedure TTestFMXSynEditCodeFolding.TestCollapseLevel;
var
  I: Integer;
  AnyCollapsed: Boolean;
begin
  SetupFoldableContent;
  // Level 1 = outermost folds
  FEditor.CollapseLevel(1);
  AnyCollapsed := False;
  for I := 0 to FEditor.AllFoldRanges.Count - 1 do
    if FEditor.AllFoldRanges[I].Collapsed then
    begin
      AnyCollapsed := True;
      Break;
    end;
  Assert.IsTrue(AnyCollapsed, 'CollapseLevel(1) should collapse top-level folds');
end;

procedure TTestFMXSynEditCodeFolding.TestUncollapseLevel;
var
  I: Integer;
  AnyCollapsed: Boolean;
begin
  SetupFoldableContent;
  FEditor.CollapseLevel(1);
  FEditor.UncollapseLevel(1);
  AnyCollapsed := False;
  for I := 0 to FEditor.AllFoldRanges.Count - 1 do
    if FEditor.AllFoldRanges[I].Collapsed then
    begin
      AnyCollapsed := True;
      Break;
    end;
  Assert.IsFalse(AnyCollapsed,
    'UncollapseLevel(1) should uncollapse top-level folds');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditCodeFolding);

end.
