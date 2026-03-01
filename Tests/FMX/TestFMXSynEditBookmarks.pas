unit TestFMXSynEditBookmarks;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditBookmarks = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestSetBookmark;
    [Test]
    procedure TestSetBookmarkUpdates;
    [Test]
    procedure TestClearBookmark;
    [Test]
    procedure TestGotoBookmark;
    [Test]
    procedure TestIsBookmarkSet;
    [Test]
    procedure TestGetBookmarkFalseWhenEmpty;
    [Test]
    procedure TestMultipleBookmarks;
    [Test]
    procedure TestToggleOnSameLine;
    [Test]
    procedure TestToggleDifferentLine;
    [Test]
    procedure TestAllTenBookmarks;
    [Test]
    procedure TestMarkListOwnership;
    [Test]
    procedure TestGotoBookmarkUnset;
    [Test]
    procedure TestBookmarkAfterClearAll;
    [Test]
    procedure TestMarksForLine;
    [Test]
    procedure TestBookmarkLineClamp;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  SynEditTypes,
  SynEditKeyCmds,
  FMX.SynEditMiscClasses;

const
  SampleText =
    'Line one' + sLineBreak +
    'Line two' + sLineBreak +
    'Line three' + sLineBreak +
    'Line four' + sLineBreak +
    'Line five';

procedure TTestFMXSynEditBookmarks.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := SampleText;
end;

procedure TTestFMXSynEditBookmarks.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditBookmarks.TestSetBookmark;
var
  X, Y: Integer;
begin
  FEditor.SetBookmark(0, 5, 2);
  Assert.IsTrue(FEditor.GetBookmark(0, X, Y));
  Assert.AreEqual(5, X);
  Assert.AreEqual(2, Y);
end;

procedure TTestFMXSynEditBookmarks.TestSetBookmarkUpdates;
var
  X, Y: Integer;
begin
  FEditor.SetBookmark(0, 1, 1);
  FEditor.SetBookmark(0, 3, 4);
  Assert.IsTrue(FEditor.GetBookmark(0, X, Y));
  Assert.AreEqual(3, X);
  Assert.AreEqual(4, Y);
  // Mark list should still have only one entry
  Assert.AreEqual(1, FEditor.Marks.Count);
end;

procedure TTestFMXSynEditBookmarks.TestClearBookmark;
begin
  FEditor.SetBookmark(1, 1, 1);
  Assert.IsTrue(FEditor.IsBookmarkSet(1));
  FEditor.ClearBookmark(1);
  Assert.IsFalse(FEditor.IsBookmarkSet(1));
end;

procedure TTestFMXSynEditBookmarks.TestGotoBookmark;
begin
  FEditor.SetBookmark(2, 5, 3);
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.GotoBookmark(2);
  Assert.AreEqual(5, FEditor.CaretX);
  Assert.AreEqual(3, FEditor.CaretY);
end;

procedure TTestFMXSynEditBookmarks.TestIsBookmarkSet;
begin
  Assert.IsFalse(FEditor.IsBookmarkSet(0));
  FEditor.SetBookmark(0, 1, 1);
  Assert.IsTrue(FEditor.IsBookmarkSet(0));
end;

procedure TTestFMXSynEditBookmarks.TestGetBookmarkFalseWhenEmpty;
var
  X, Y: Integer;
begin
  Assert.IsFalse(FEditor.GetBookmark(5, X, Y));
end;

procedure TTestFMXSynEditBookmarks.TestMultipleBookmarks;
var
  X, Y: Integer;
begin
  FEditor.SetBookmark(0, 1, 1);
  FEditor.SetBookmark(1, 2, 2);
  FEditor.SetBookmark(2, 3, 3);
  FEditor.SetBookmark(3, 4, 4);

  Assert.IsTrue(FEditor.GetBookmark(0, X, Y));
  Assert.AreEqual(1, Y);
  Assert.IsTrue(FEditor.GetBookmark(1, X, Y));
  Assert.AreEqual(2, Y);
  Assert.IsTrue(FEditor.GetBookmark(2, X, Y));
  Assert.AreEqual(3, Y);
  Assert.IsTrue(FEditor.GetBookmark(3, X, Y));
  Assert.AreEqual(4, Y);
end;

procedure TTestFMXSynEditBookmarks.TestToggleOnSameLine;
begin
  // Set bookmark on line 2
  FEditor.SetBookmark(0, 1, 2);
  Assert.IsTrue(FEditor.IsBookmarkSet(0));

  // Move caret to same line and execute ecSetMarker0 — should toggle off
  FEditor.CaretXY := BufferCoord(1, 2);
  FEditor.ExecuteCommand(ecSetMarker0, #0);
  Assert.IsFalse(FEditor.IsBookmarkSet(0));
end;

procedure TTestFMXSynEditBookmarks.TestToggleDifferentLine;
var
  X, Y: Integer;
begin
  // Set bookmark on line 1
  FEditor.SetBookmark(0, 1, 1);

  // Move caret to line 3 and execute ecSetMarker0 — should move bookmark
  FEditor.CaretXY := BufferCoord(4, 3);
  FEditor.ExecuteCommand(ecSetMarker0, #0);
  Assert.IsTrue(FEditor.IsBookmarkSet(0));
  Assert.IsTrue(FEditor.GetBookmark(0, X, Y));
  Assert.AreEqual(3, Y);
  Assert.AreEqual(4, X);
end;

procedure TTestFMXSynEditBookmarks.TestAllTenBookmarks;
begin
  for var I := 0 to 9 do
    FEditor.SetBookmark(I, 1, Min(I + 1, FEditor.LineCount));

  for var I := 0 to 9 do
    Assert.IsTrue(FEditor.IsBookmarkSet(I),
      Format('Bookmark %d should be set', [I]));

  Assert.AreEqual(10, FEditor.Marks.Count);
end;

procedure TTestFMXSynEditBookmarks.TestMarkListOwnership;
begin
  FEditor.SetBookmark(0, 1, 1);
  FEditor.SetBookmark(3, 2, 2);
  Assert.AreEqual(2, FEditor.Marks.Count);

  FEditor.ClearBookmark(0);
  Assert.AreEqual(1, FEditor.Marks.Count);
end;

procedure TTestFMXSynEditBookmarks.TestGotoBookmarkUnset;
begin
  // Should be a no-op — caret stays at (1,1)
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.GotoBookmark(5);
  Assert.AreEqual(1, FEditor.CaretX);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditBookmarks.TestBookmarkAfterClearAll;
begin
  FEditor.SetBookmark(0, 1, 1);
  FEditor.SetBookmark(1, 2, 2);
  FEditor.ClearAll;
  Assert.IsFalse(FEditor.IsBookmarkSet(0));
  Assert.IsFalse(FEditor.IsBookmarkSet(1));
  Assert.AreEqual(0, FEditor.Marks.Count);
end;

procedure TTestFMXSynEditBookmarks.TestMarksForLine;
var
  LineMarks: TArray<TSynFMXEditMark>;
begin
  FEditor.SetBookmark(0, 1, 2);
  FEditor.SetBookmark(3, 5, 2);
  FEditor.SetBookmark(5, 1, 4);

  LineMarks := FEditor.Marks.GetMarksForLine(2);
  Assert.AreEqual(2, Length(LineMarks));

  LineMarks := FEditor.Marks.GetMarksForLine(4);
  Assert.AreEqual(1, Length(LineMarks));

  LineMarks := FEditor.Marks.GetMarksForLine(1);
  Assert.AreEqual(0, Length(LineMarks));
end;

procedure TTestFMXSynEditBookmarks.TestBookmarkLineClamp;
var
  X, Y: Integer;
begin
  // Set bookmark beyond line count — should clamp
  FEditor.SetBookmark(0, 1, 999);
  Assert.IsTrue(FEditor.GetBookmark(0, X, Y));
  Assert.AreEqual(FEditor.LineCount, Y);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditBookmarks);

end.
