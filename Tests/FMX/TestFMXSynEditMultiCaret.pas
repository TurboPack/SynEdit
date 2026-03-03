unit TestFMXSynEditMultiCaret;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditMultiCaret = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestSingleCaretBackwardCompat;
    [Test]
    procedure TestSelectionsDefaultCount;
    [Test]
    procedure TestAddCaret;
    [Test]
    procedure TestAddCaretToggle;
    [Test]
    procedure TestColumnSelectionDown;
    [Test]
    procedure TestColumnSelectionUp;
    [Test]
    procedure TestMultiCaretCharInsert;
    [Test]
    procedure TestMultiCaretDeleteChar;
    [Test]
    procedure TestMultiCaretBackspace;
    [Test]
    procedure TestCancelSelections;
    [Test]
    procedure TestCancelSingleSelection;
    [Test]
    procedure TestMergeOverlapping;
    [Test]
    procedure TestSelectAllMatchingText;
    [Test]
    procedure TestCaretsAtLineEnds;
    [Test]
    procedure TestMultiCaretUndo;
    [Test]
    procedure TestMultiCaretRedo;
    [Test]
    procedure TestPartSelectionsForRow;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes,
  SynEditSelections,
  SynEditKeyCmds;

procedure TTestFMXSynEditMultiCaret.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'Hello World' + sLineBreak +
                  'Second line' + sLineBreak +
                  'Third line' + sLineBreak +
                  'Hello Again' + sLineBreak +
                  'Fifth line';
end;

procedure TTestFMXSynEditMultiCaret.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditMultiCaret.TestSingleCaretBackwardCompat;
begin
  FEditor.CaretXY := BufferCoord(3, 2);
  Assert.AreEqual(3, FEditor.CaretX);
  Assert.AreEqual(2, FEditor.CaretY);
  FEditor.BlockBegin := BufferCoord(1, 2);
  FEditor.BlockEnd := BufferCoord(7, 2);
  Assert.AreEqual('Second', FEditor.SelText);
end;

procedure TTestFMXSynEditMultiCaret.TestSelectionsDefaultCount;
begin
  Assert.AreEqual(1, FEditor.Selections.Count,
    'Should start with 1 selection');
end;

procedure TTestFMXSynEditMultiCaret.TestAddCaret;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(1, 2));
  Assert.AreEqual(2, FEditor.Selections.Count,
    'Should have 2 selections after AddCaret');
end;

procedure TTestFMXSynEditMultiCaret.TestAddCaretToggle;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(1, 2));
  Assert.AreEqual(2, FEditor.Selections.Count);
  // Adding caret at same position removes it
  FEditor.Selections.AddCaret(BufferCoord(1, 2));
  Assert.AreEqual(1, FEditor.Selections.Count,
    'AddCaret at same position should toggle (remove)');
end;

procedure TTestFMXSynEditMultiCaret.TestColumnSelectionDown;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.CommandProcessor(ecSelColumnDown, #0);
  Assert.AreEqual(2, FEditor.Selections.Count,
    'Column down from line 1 should create 2 selections');
  FEditor.CommandProcessor(ecSelColumnDown, #0);
  Assert.AreEqual(3, FEditor.Selections.Count,
    'Column down again should create 3 selections');
end;

procedure TTestFMXSynEditMultiCaret.TestColumnSelectionUp;
begin
  FEditor.CaretXY := BufferCoord(1, 3);
  FEditor.CommandProcessor(ecSelColumnUp, #0);
  Assert.AreEqual(2, FEditor.Selections.Count,
    'Column up from line 3 should create 2 selections');
end;

procedure TTestFMXSynEditMultiCaret.TestMultiCaretCharInsert;
begin
  // Place carets at start of lines 1, 2, 3
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(1, 2));
  FEditor.Selections.AddCaret(BufferCoord(1, 3));
  Assert.AreEqual(3, FEditor.Selections.Count);

  // Insert 'X' at all carets
  FEditor.CommandProcessor(ecChar, 'X');

  Assert.AreEqual('XHello World', FEditor.Lines[0]);
  Assert.AreEqual('XSecond line', FEditor.Lines[1]);
  Assert.AreEqual('XThird line', FEditor.Lines[2]);
end;

procedure TTestFMXSynEditMultiCaret.TestMultiCaretDeleteChar;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(1, 2));
  FEditor.Selections.AddCaret(BufferCoord(1, 3));

  FEditor.CommandProcessor(ecDeleteChar, #0);

  Assert.AreEqual('ello World', FEditor.Lines[0]);
  Assert.AreEqual('econd line', FEditor.Lines[1]);
  Assert.AreEqual('hird line', FEditor.Lines[2]);
end;

procedure TTestFMXSynEditMultiCaret.TestMultiCaretBackspace;
begin
  FEditor.CaretXY := BufferCoord(2, 1);
  FEditor.Selections.AddCaret(BufferCoord(2, 2));
  FEditor.Selections.AddCaret(BufferCoord(2, 3));

  FEditor.CommandProcessor(ecDeleteLastChar, #0);

  Assert.AreEqual('ello World', FEditor.Lines[0]);
  Assert.AreEqual('econd line', FEditor.Lines[1]);
  Assert.AreEqual('hird line', FEditor.Lines[2]);
end;

procedure TTestFMXSynEditMultiCaret.TestCancelSelections;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(1, 2));
  FEditor.Selections.AddCaret(BufferCoord(1, 3));
  Assert.AreEqual(3, FEditor.Selections.Count);

  FEditor.CommandProcessor(ecCancelSelections, #0);
  Assert.AreEqual(1, FEditor.Selections.Count,
    'CancelSelections should reduce to 1');
end;

procedure TTestFMXSynEditMultiCaret.TestCancelSingleSelection;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.BlockBegin := BufferCoord(1, 1);
  FEditor.BlockEnd := BufferCoord(6, 1);
  Assert.IsTrue(FEditor.SelAvail);

  FEditor.CommandProcessor(ecCancelSelections, #0);
  Assert.IsFalse(FEditor.SelAvail,
    'CancelSelections on single should collapse selection');
end;

procedure TTestFMXSynEditMultiCaret.TestMergeOverlapping;
begin
  // Place two carets adjacent — after editing they should merge
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(2, 1));
  Assert.AreEqual(2, FEditor.Selections.Count);

  // Delete char at both positions — they converge
  FEditor.CommandProcessor(ecDeleteChar, #0);
  // After merge, should be 1 selection
  Assert.AreEqual(1, FEditor.Selections.Count,
    'Overlapping carets should merge');
end;

procedure TTestFMXSynEditMultiCaret.TestSelectAllMatchingText;
begin
  // Select "Hello" on line 1
  FEditor.SetCaretAndSelection(
    BufferCoord(6, 1), BufferCoord(1, 1), BufferCoord(6, 1));

  Assert.AreEqual('Hello', FEditor.SelText, 'Selection should be "Hello"');

  FEditor.CommandProcessor(ecSelMatchingText, #0);

  // "Hello" appears on lines 1 and 4
  Assert.AreEqual(2, FEditor.Selections.Count,
    'Should find 2 occurrences of "Hello"');
end;

procedure TTestFMXSynEditMultiCaret.TestCaretsAtLineEnds;
begin
  // Select lines 1-3
  FEditor.BlockBegin := BufferCoord(1, 1);
  FEditor.BlockEnd := BufferCoord(1, 3);

  FEditor.CommandProcessor(ecCaretsAtLineEnds, #0);

  Assert.AreEqual(3, FEditor.Selections.Count,
    'Should have 3 carets (one per line)');
  // Each caret should be at end of its line
  Assert.AreEqual(12, FEditor.Selections[0].Caret.Char,
    'Caret 1 should be at end of "Hello World"');
  Assert.AreEqual(1, FEditor.Selections[0].Caret.Line);
  Assert.AreEqual(12, FEditor.Selections[1].Caret.Char,
    'Caret 2 should be at end of "Second line"');
  Assert.AreEqual(2, FEditor.Selections[1].Caret.Line);
  Assert.AreEqual(11, FEditor.Selections[2].Caret.Char,
    'Caret 3 should be at end of "Third line"');
  Assert.AreEqual(3, FEditor.Selections[2].Caret.Line);
end;

procedure TTestFMXSynEditMultiCaret.TestMultiCaretUndo;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(1, 2));
  Assert.AreEqual(2, FEditor.Selections.Count);

  FEditor.CommandProcessor(ecChar, 'Z');
  Assert.AreEqual('ZHello World', FEditor.Lines[0]);
  Assert.AreEqual('ZSecond line', FEditor.Lines[1]);

  FEditor.Undo;
  Assert.AreEqual('Hello World', FEditor.Lines[0]);
  Assert.AreEqual('Second line', FEditor.Lines[1]);
end;

procedure TTestFMXSynEditMultiCaret.TestMultiCaretRedo;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.Selections.AddCaret(BufferCoord(1, 2));

  FEditor.CommandProcessor(ecChar, 'Z');
  FEditor.Undo;
  Assert.AreEqual('Hello World', FEditor.Lines[0]);

  FEditor.Redo;
  Assert.AreEqual('ZHello World', FEditor.Lines[0]);
  Assert.AreEqual('ZSecond line', FEditor.Lines[1]);
end;

procedure TTestFMXSynEditMultiCaret.TestPartSelectionsForRow;
begin
  // Create a column selection spanning 3 lines, chars 3-6
  FEditor.CaretXY := BufferCoord(3, 1);
  FEditor.Selections.ColumnSelection(
    BufferCoord(3, 1), BufferCoord(6, 3), 0);

  Assert.AreEqual(3, FEditor.Selections.Count);

  var Parts := FEditor.Selections.PartSelectionsForRow(
    BufferCoord(1, 2), BufferCoord(12, 2));
  Assert.AreEqual(1, Length(Parts),
    'Row 2 should have 1 partial selection');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditMultiCaret);

end.
