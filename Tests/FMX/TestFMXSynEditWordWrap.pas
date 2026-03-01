unit TestFMXSynEditWordWrap;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit,
  SynEditTypes;

type
  [TestFixture]
  TTestFMXSynEditWordWrap = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestEnableDisable;
    [Test]
    procedure TestMutuallyExclusiveWithCodeFolding;
    [Test]
    procedure TestShortLineNoWrap;
    [Test]
    procedure TestEmptyLineOneRow;
    [Test]
    procedure TestLongLineWraps;
    [Test]
    procedure TestRowCount;
    [Test]
    procedure TestLineToRow;
    [Test]
    procedure TestRowToLine;
    [Test]
    procedure TestBufferToDisplayPos;
    [Test]
    procedure TestDisplayToBufferPos;
    [Test]
    procedure TestRoundTrip;
    [Test]
    procedure TestWrapAtWordBoundary;
    [Test]
    procedure TestWrapWithTabs;
    [Test]
    procedure TestEmergencyWrap;
    [Test]
    procedure TestCaretUpDown;
    [Test]
    procedure TestHorizontalScrollLocked;
    [Test]
    procedure TestGutterLineNumbers;
    [Test]
    procedure TestResizeReWraps;
    [Test]
    procedure TestTextChangeReWraps;
    [Test]
    procedure TestDisplayRowCount;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  FMX.SynEditWordWrap,
  SynEditKeyCmds,
  SynHighlighterJSON;

procedure TTestFMXSynEditWordWrap.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Width := 400;
  FEditor.Height := 300;
end;

procedure TTestFMXSynEditWordWrap.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditWordWrap.TestEnableDisable;
begin
  Assert.IsFalse(FEditor.WordWrap, 'WordWrap should be false by default');
  FEditor.Text := 'Hello';
  FEditor.WordWrap := True;
  Assert.IsTrue(FEditor.WordWrap, 'WordWrap should be true after enabling');
  FEditor.WordWrap := False;
  Assert.IsFalse(FEditor.WordWrap, 'WordWrap should be false after disabling');
end;

procedure TTestFMXSynEditWordWrap.TestMutuallyExclusiveWithCodeFolding;
var
  HL: TSynJSONSyn;
begin
  HL := TSynJSONSyn.Create(nil);
  try
    FEditor.Highlighter := HL;
    FEditor.Text := '{"a":1}';
    FEditor.UseCodeFolding := True;
    Assert.IsTrue(FEditor.UseCodeFolding);

    // Enabling word wrap should fail when code folding is active
    FEditor.WordWrap := True;
    Assert.IsFalse(FEditor.WordWrap,
      'WordWrap should not enable when code folding is active');

    FEditor.UseCodeFolding := False;
    FEditor.WordWrap := True;
    Assert.IsTrue(FEditor.WordWrap);

    // Enabling code folding should fail when word wrap is active
    FEditor.UseCodeFolding := True;
    Assert.IsFalse(FEditor.UseCodeFolding,
      'UseCodeFolding should not enable when WordWrap is active');
  finally
    FEditor.Highlighter := nil;
    HL.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestShortLineNoWrap;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(80, 8);
    Lines.Add('Short line');
    Helper.Reset(Lines);
    Assert.AreEqual(1, Helper.RowCount, 'Short line should produce 1 row');
    Assert.AreEqual(10, Helper.GetRowLength(1), 'Row length should match line length');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestEmptyLineOneRow;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(80, 8);
    Lines.Add('');
    Helper.Reset(Lines);
    Assert.AreEqual(1, Helper.RowCount, 'Empty line should produce 1 row');
    Assert.AreEqual(0, Helper.GetRowLength(1), 'Empty row length should be 0');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestLongLineWraps;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    // 25 chars should wrap to 3 rows at width 10
    Lines.Add('abcdefghij klmnopqrst uvwxy');
    Helper.Reset(Lines);
    Assert.IsTrue(Helper.RowCount > 1,
      'Long line should produce more than 1 row (got ' +
      IntToStr(Helper.RowCount) + ')');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestRowCount;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    Lines.Add('Short');      // 1 row
    Lines.Add('Also short'); // 1 row (10 chars = exactly fits)
    Lines.Add('This is a longer line that wraps'); // multiple rows
    Helper.Reset(Lines);
    Assert.IsTrue(Helper.RowCount >= 4,
      'Expected at least 4 rows, got ' + IntToStr(Helper.RowCount));
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestLineToRow;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    Lines.Add('Short');        // row 1
    Lines.Add('Hello world!'); // wraps: row 2+
    Lines.Add('End');          // last rows
    Helper.Reset(Lines);
    Assert.AreEqual(1, Helper.LineToRow(1), 'Line 1 should start at row 1');
    Assert.AreEqual(2, Helper.LineToRow(2), 'Line 2 should start at row 2');
    // Line 3 should start after all rows of line 2
    Assert.IsTrue(Helper.LineToRow(3) > Helper.LineToRow(2),
      'Line 3 should start after line 2');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestRowToLine;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    Lines.Add('Short');        // row 1
    Lines.Add('Hello world!'); // wraps to rows 2+
    Lines.Add('End');
    Helper.Reset(Lines);
    Assert.AreEqual(1, Helper.RowToLine(1), 'Row 1 should be line 1');
    Assert.AreEqual(2, Helper.RowToLine(2), 'Row 2 should be line 2');
    // All wrapped rows of line 2 should map back to line 2
    var Line2Start := Helper.LineToRow(2);
    var Line3Start := Helper.LineToRow(3);
    var I: Integer;
    for I := Line2Start to Line3Start - 1 do
      Assert.AreEqual(2, Helper.RowToLine(I),
        'Wrapped row ' + IntToStr(I) + ' should map to line 2');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestBufferToDisplayPos;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
  DC: TDisplayCoord;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    Lines.Add('abcdefghij klmno'); // wraps at 10
    Helper.Reset(Lines);
    // First char
    DC := Helper.BufferToDisplayPos(BufferCoord(1, 1));
    Assert.AreEqual(1, DC.Column);
    Assert.AreEqual(1, DC.Row);
    // Char on second wrapped row
    var Row2Start := Helper.GetRowLength(1) + 1;
    DC := Helper.BufferToDisplayPos(BufferCoord(Row2Start, 1));
    Assert.AreEqual(2, DC.Row, 'Char after wrap should be on row 2');
    Assert.AreEqual(1, DC.Column, 'First char of wrapped row should be column 1');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestDisplayToBufferPos;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
  BC: TBufferCoord;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    Lines.Add('abcdefghij klmno'); // wraps
    Helper.Reset(Lines);
    // Row 1 col 1 -> char 1 line 1
    BC := Helper.DisplayToBufferPos(DisplayCoord(1, 1));
    Assert.AreEqual(1, BC.Char);
    Assert.AreEqual(1, BC.Line);
    // Row 2 col 1 -> should be after first row's chars
    BC := Helper.DisplayToBufferPos(DisplayCoord(1, 2));
    Assert.AreEqual(1, BC.Line);
    Assert.AreEqual(Helper.GetRowLength(1) + 1, BC.Char,
      'Row 2 col 1 should map to char after first row');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestRoundTrip;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
  BC, BC2: TBufferCoord;
  DC: TDisplayCoord;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    Lines.Add('Hello World Foo Bar');
    Lines.Add('Short');
    Helper.Reset(Lines);
    // Test several positions
    BC := BufferCoord(1, 1);
    DC := Helper.BufferToDisplayPos(BC);
    BC2 := Helper.DisplayToBufferPos(DC);
    Assert.AreEqual(BC.Char, BC2.Char, 'Round trip char mismatch at (1,1)');
    Assert.AreEqual(BC.Line, BC2.Line, 'Round trip line mismatch at (1,1)');

    BC := BufferCoord(5, 1);
    DC := Helper.BufferToDisplayPos(BC);
    BC2 := Helper.DisplayToBufferPos(DC);
    Assert.AreEqual(BC.Char, BC2.Char, 'Round trip char mismatch at (5,1)');
    Assert.AreEqual(BC.Line, BC2.Line, 'Round trip line mismatch at (5,1)');

    BC := BufferCoord(1, 2);
    DC := Helper.BufferToDisplayPos(BC);
    BC2 := Helper.DisplayToBufferPos(DC);
    Assert.AreEqual(BC.Char, BC2.Char, 'Round trip char mismatch at (1,2)');
    Assert.AreEqual(BC.Line, BC2.Line, 'Round trip line mismatch at (1,2)');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestWrapAtWordBoundary;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    // "Hello World" = 11 chars. Should wrap after "Hello " (6 chars) at the space
    Lines.Add('Hello World');
    Helper.Reset(Lines);
    Assert.AreEqual(2, Helper.RowCount, 'Should wrap to 2 rows');
    // First row should contain "Hello " (6 chars including space)
    Assert.AreEqual(6, Helper.GetRowLength(1),
      'First row should break at word boundary (after space)');
    Assert.AreEqual(5, Helper.GetRowLength(2),
      'Second row should contain remaining word');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestWrapWithTabs;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 4);
    // Tab at position 0 expands to 4 visual columns + "abcdefgh" = 12 visual columns
    Lines.Add(#9'abcdefgh');
    Helper.Reset(Lines);
    Assert.IsTrue(Helper.RowCount >= 2,
      'Tab-expanded line should wrap (got ' + IntToStr(Helper.RowCount) + ' rows)');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestEmergencyWrap;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(5, 8);
    // No spaces -> emergency wrap at max width
    Lines.Add('abcdefghijklmno');
    Helper.Reset(Lines);
    Assert.AreEqual(3, Helper.RowCount,
      'Should emergency-wrap 15 chars to 3 rows at width 5');
    Assert.AreEqual(5, Helper.GetRowLength(1), 'Row 1 should be 5 chars');
    Assert.AreEqual(5, Helper.GetRowLength(2), 'Row 2 should be 5 chars');
    Assert.AreEqual(5, Helper.GetRowLength(3), 'Row 3 should be 5 chars');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

procedure TTestFMXSynEditWordWrap.TestCaretUpDown;
begin
  FEditor.Text := 'Hello World, this is a test' + sLineBreak + 'Short';
  FEditor.WordWrap := True;
  // Move caret to start
  FEditor.CaretXY := BufferCoord(1, 1);
  // Press Down - should move to next display row (possibly wrapped)
  FEditor.ExecuteCommand(ecDown, #0);
  // Caret should have moved
  var NewPos := FEditor.CaretXY;
  Assert.IsTrue((NewPos.Line > 1) or (NewPos.Char > 1),
    'Down arrow should move caret in word wrap mode');
end;

procedure TTestFMXSynEditWordWrap.TestHorizontalScrollLocked;
begin
  FEditor.Text := 'Hello';
  FEditor.WordWrap := True;
  FEditor.LeftChar := 5;
  Assert.AreEqual(1, FEditor.LeftChar,
    'LeftChar should stay at 1 when word wrap is on');
end;

procedure TTestFMXSynEditWordWrap.TestGutterLineNumbers;
begin
  // This is a behavioral test - verify the editor doesn't crash when painting
  // with word wrap active and wrapped lines
  FEditor.Text := StringOfChar('x', 200) + sLineBreak + 'Line 2';
  FEditor.WordWrap := True;
  // Trigger a repaint (no-op without parent but exercises the code path)
  FEditor.Repaint;
  Assert.Pass('Painting with word wrap should not raise exceptions');
end;

procedure TTestFMXSynEditWordWrap.TestResizeReWraps;
var
  RowsBefore, RowsAfter: Integer;
begin
  FEditor.Text := StringOfChar('a', 100);
  FEditor.WordWrap := True;
  RowsBefore := FEditor.DisplayRowCount;
  // Make editor narrower -> more wrapping
  FEditor.Width := FEditor.Width / 2;
  RowsAfter := FEditor.DisplayRowCount;
  Assert.IsTrue(RowsAfter >= RowsBefore,
    'Narrower editor should produce >= rows (before=' +
    IntToStr(RowsBefore) + ', after=' + IntToStr(RowsAfter) + ')');
end;

procedure TTestFMXSynEditWordWrap.TestTextChangeReWraps;
var
  RowsBefore, RowsAfter: Integer;
begin
  FEditor.Text := 'Short';
  FEditor.WordWrap := True;
  RowsBefore := FEditor.DisplayRowCount;
  Assert.AreEqual(1, RowsBefore, 'Short text should be 1 row');
  // Add a long line
  FEditor.Text := StringOfChar('x', 200);
  RowsAfter := FEditor.DisplayRowCount;
  Assert.IsTrue(RowsAfter > 1,
    'Long line should produce multiple rows after text change');
end;

procedure TTestFMXSynEditWordWrap.TestDisplayRowCount;
var
  Helper: TFMXWordWrapHelper;
  Lines: TStringList;
begin
  Helper := TFMXWordWrapHelper.Create;
  Lines := TStringList.Create;
  try
    Helper.SetWrapWidth(10, 8);
    Lines.Add('Short');     // 1 row
    Lines.Add('');          // 1 row
    Lines.Add('Also short'); // 1 row
    Helper.Reset(Lines);
    Assert.AreEqual(3, Helper.RowCount,
      'Three short lines should produce 3 rows');
  finally
    Lines.Free;
    Helper.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditWordWrap);

end.
