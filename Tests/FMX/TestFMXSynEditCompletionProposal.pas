unit TestFMXSynEditCompletionProposal;

interface

uses
  DUnitX.TestFramework,
  FMX.SynCompletionProposal;

type
  [TestFixture]
  TTestFMXCompletionProposalForm = class
  private
    FForm: TSynFMXCompletionProposalForm;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAddItemIncreasesCount;
    [Test]
    procedure TestClearListEmptiesAll;
    [Test]
    procedure TestFilterByPrefixCaseInsensitive;
    [Test]
    procedure TestFilterByPrefixCaseSensitive;
    [Test]
    procedure TestEmptyStringShowsAll;
    [Test]
    procedure TestNoMatchShowsEmpty;
    [Test]
    procedure TestUseInsertListFiltering;
    [Test]
    procedure TestPositionClampedToValidRange;
    [Test]
    procedure TestMoveLineDown;
    [Test]
    procedure TestMoveLineUp;
    [Test]
    procedure TestMoveLineTopBoundary;
    [Test]
    procedure TestMoveLineBottomBoundary;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

procedure TTestFMXCompletionProposalForm.Setup;
begin
  FForm := TSynFMXCompletionProposalForm.Create(nil);
  // MatchText = True by default, CaseSensitive = False by default
end;

procedure TTestFMXCompletionProposalForm.TearDown;
begin
  FForm.Free;
end;

procedure TTestFMXCompletionProposalForm.TestAddItemIncreasesCount;
begin
  Assert.AreEqual(0, FForm.ItemList.Count, 'Should start empty');
  FForm.AddItem('Display1', 'Insert1');
  Assert.AreEqual(1, FForm.ItemList.Count, 'ItemList should have 1 item');
  Assert.AreEqual(1, FForm.InsertList.Count, 'InsertList should have 1 item');
  FForm.AddItem('Display2', 'Insert2');
  Assert.AreEqual(2, FForm.ItemList.Count, 'ItemList should have 2 items');
end;

procedure TTestFMXCompletionProposalForm.TestClearListEmptiesAll;
begin
  FForm.AddItem('A', 'a');
  FForm.AddItem('B', 'b');
  FForm.ClearList;
  Assert.AreEqual(0, FForm.ItemList.Count, 'ItemList should be empty');
  Assert.AreEqual(0, FForm.InsertList.Count, 'InsertList should be empty');
  Assert.AreEqual(0, FForm.AssignedList.Count, 'AssignedList should be empty');
  Assert.AreEqual(0, FForm.FilteredInsertList.Count,
    'FilteredInsertList should be empty');
end;

procedure TTestFMXCompletionProposalForm.TestFilterByPrefixCaseInsensitive;
begin
  FForm.CaseSensitive := False;
  FForm.MatchText := True;
  FForm.AddItem('ArrayList', 'ArrayList');
  FForm.AddItem('ArrayBuffer', 'ArrayBuffer');
  FForm.AddItem('Boolean', 'Boolean');
  // Filter with lowercase prefix
  FForm.CurrentString := 'arr';
  Assert.AreEqual(2, FForm.AssignedList.Count,
    'Case-insensitive filter should match 2 "Array" items');
  Assert.AreEqual(0, FForm.Position, 'Position should be 0');
end;

procedure TTestFMXCompletionProposalForm.TestFilterByPrefixCaseSensitive;
begin
  FForm.CaseSensitive := True;
  FForm.MatchText := True;
  FForm.AddItem('ArrayList', 'ArrayList');
  FForm.AddItem('ArrayBuffer', 'ArrayBuffer');
  FForm.AddItem('Boolean', 'Boolean');
  // Filter with lowercase — should NOT match uppercase items
  FForm.CurrentString := 'arr';
  Assert.AreEqual(0, FForm.AssignedList.Count,
    'Case-sensitive filter with "arr" should not match "Array" items');
  // Filter with correct case
  FForm.CurrentString := 'Array';
  Assert.AreEqual(2, FForm.AssignedList.Count,
    'Case-sensitive filter with "Array" should match 2 items');
end;

procedure TTestFMXCompletionProposalForm.TestEmptyStringShowsAll;
begin
  FForm.MatchText := True;
  FForm.AddItem('Alpha', 'Alpha');
  FForm.AddItem('Beta', 'Beta');
  FForm.AddItem('Gamma', 'Gamma');
  FForm.CurrentString := '';
  Assert.AreEqual(3, FForm.AssignedList.Count,
    'Empty filter string should show all items');
end;

procedure TTestFMXCompletionProposalForm.TestNoMatchShowsEmpty;
begin
  FForm.MatchText := True;
  FForm.AddItem('Alpha', 'Alpha');
  FForm.AddItem('Beta', 'Beta');
  FForm.CurrentString := 'xyz';
  Assert.AreEqual(0, FForm.AssignedList.Count,
    'Non-matching filter should show empty list');
end;

procedure TTestFMXCompletionProposalForm.TestUseInsertListFiltering;
begin
  FForm.CaseSensitive := False;
  FForm.MatchText := True;
  FForm.UseInsertList := True;
  // Display text differs from insert text
  FForm.AddItem('Array List (java.util)', 'ArrayList');
  FForm.AddItem('Boolean Value', 'BooleanValue');
  // Filter by insert list prefix
  FForm.CurrentString := 'Array';
  Assert.AreEqual(1, FForm.AssignedList.Count,
    'Should filter using InsertList, matching "ArrayList"');
  Assert.AreEqual('Array List (java.util)', FForm.AssignedList[0],
    'Display text should be shown in AssignedList');
end;

procedure TTestFMXCompletionProposalForm.TestPositionClampedToValidRange;
begin
  FForm.MatchText := False;
  FForm.AddItem('A', 'A');
  FForm.AddItem('B', 'B');
  FForm.AddItem('C', 'C');
  FForm.CurrentString := '';
  // Try to set beyond range
  FForm.Position := 100;
  Assert.AreEqual(2, FForm.Position,
    'Position should be clamped to last item index');
  // Try negative
  FForm.Position := -5;
  Assert.AreEqual(0, FForm.Position,
    'Position should be clamped to 0');
end;

procedure TTestFMXCompletionProposalForm.TestMoveLineDown;
begin
  FForm.MatchText := False;
  FForm.AddItem('A', 'A');
  FForm.AddItem('B', 'B');
  FForm.AddItem('C', 'C');
  FForm.CurrentString := '';
  FForm.Position := 0;
  FForm.MoveLine(1);
  Assert.AreEqual(1, FForm.Position,
    'MoveLine(1) should move to next item');
end;

procedure TTestFMXCompletionProposalForm.TestMoveLineUp;
begin
  FForm.MatchText := False;
  FForm.AddItem('A', 'A');
  FForm.AddItem('B', 'B');
  FForm.AddItem('C', 'C');
  FForm.CurrentString := '';
  FForm.Position := 2;
  FForm.MoveLine(-1);
  Assert.AreEqual(1, FForm.Position,
    'MoveLine(-1) should move to previous item');
end;

procedure TTestFMXCompletionProposalForm.TestMoveLineTopBoundary;
begin
  FForm.MatchText := False;
  FForm.AddItem('A', 'A');
  FForm.AddItem('B', 'B');
  FForm.CurrentString := '';
  FForm.Position := 0;
  FForm.MoveLine(-1);
  Assert.AreEqual(0, FForm.Position,
    'MoveLine up at top should stay at 0');
end;

procedure TTestFMXCompletionProposalForm.TestMoveLineBottomBoundary;
begin
  FForm.MatchText := False;
  FForm.AddItem('A', 'A');
  FForm.AddItem('B', 'B');
  FForm.CurrentString := '';
  FForm.Position := 1;
  FForm.MoveLine(1);
  Assert.AreEqual(1, FForm.Position,
    'MoveLine down at bottom should stay at last index');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXCompletionProposalForm);

end.
