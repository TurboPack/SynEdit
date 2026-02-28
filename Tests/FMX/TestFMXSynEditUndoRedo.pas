unit TestFMXSynEditUndoRedo;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditUndoRedo = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestCanUndoInitiallyFalse;
    [Test]
    procedure TestCanRedoInitiallyFalse;
    [Test]
    procedure TestCanUndoAfterTextChange;
    [Test]
    procedure TestUndoRestoresText;
    [Test]
    procedure TestCanRedoAfterUndo;
    [Test]
    procedure TestRedoRestoresText;
    [Test]
    procedure TestClearAllResetsUndo;
    [Test]
    procedure TestMultipleUndoRedo;
    [Test]
    procedure TestRedoCaretPosition;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes,
  SynEditKeyCmds;

procedure TTestFMXSynEditUndoRedo.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestFMXSynEditUndoRedo.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditUndoRedo.TestCanUndoInitiallyFalse;
begin
  Assert.IsFalse(FEditor.CanUndo);
end;

procedure TTestFMXSynEditUndoRedo.TestCanRedoInitiallyFalse;
begin
  Assert.IsFalse(FEditor.CanRedo);
end;

procedure TTestFMXSynEditUndoRedo.TestCanUndoAfterTextChange;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.IsTrue(FEditor.CanUndo);
end;

procedure TTestFMXSynEditUndoRedo.TestUndoRestoresText;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.AreEqual('XHello', FEditor.Lines[0]);
  FEditor.Undo;
  Assert.AreEqual('Hello', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditUndoRedo.TestCanRedoAfterUndo;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  FEditor.Undo;
  Assert.IsTrue(FEditor.CanRedo);
end;

procedure TTestFMXSynEditUndoRedo.TestRedoRestoresText;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  FEditor.Undo;
  Assert.AreEqual('Hello', FEditor.Lines[0]);
  FEditor.Redo;
  Assert.AreEqual('XHello', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditUndoRedo.TestClearAllResetsUndo;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.IsTrue(FEditor.CanUndo);
  FEditor.ClearAll;
  Assert.IsFalse(FEditor.CanUndo);
end;

procedure TTestFMXSynEditUndoRedo.TestMultipleUndoRedo;
begin
  FEditor.Text := 'AB';
  // Type 'X' at position 1
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.AreEqual('XAB', FEditor.Lines[0]);
  // Type 'Y' at current position (2,1)
  FEditor.ExecuteCommand(ecChar, 'Y');
  Assert.AreEqual('XYAB', FEditor.Lines[0]);
  // Undo 'Y'
  FEditor.Undo;
  Assert.AreEqual('XAB', FEditor.Lines[0]);
  // Undo 'X'
  FEditor.Undo;
  Assert.AreEqual('AB', FEditor.Lines[0]);
  // Redo 'X'
  FEditor.Redo;
  Assert.AreEqual('XAB', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditUndoRedo.TestRedoCaretPosition;
begin
  // Type two characters on separate lines, undo both, redo both
  // Verify caret ends at the last redone item's position
  FEditor.Text := 'Line1' + #13#10 + 'Line2';
  // Type 'A' at start of line 1
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'A');
  Assert.AreEqual('ALine1', FEditor.Lines[0]);
  Assert.AreEqual(2, FEditor.CaretX, 'Caret should be at 2 after typing A');
  // Type 'B' at start of line 2
  FEditor.CaretXY := BufferCoord(1, 2);
  FEditor.ExecuteCommand(ecChar, 'B');
  Assert.AreEqual('BLine2', FEditor.Lines[1]);
  Assert.AreEqual(2, FEditor.CaretX, 'Caret should be at 2 after typing B');
  // Undo both
  FEditor.Undo;
  FEditor.Undo;
  Assert.AreEqual('Line1', FEditor.Lines[0]);
  Assert.AreEqual('Line2', FEditor.Lines[1]);
  // Redo first (A on line 1)
  FEditor.Redo;
  Assert.AreEqual('ALine1', FEditor.Lines[0]);
  Assert.AreEqual(1, FEditor.CaretY, 'Caret should be on line 1 after first redo');
  // Redo second (B on line 2)
  FEditor.Redo;
  Assert.AreEqual('BLine2', FEditor.Lines[1]);
  Assert.AreEqual(2, FEditor.CaretY, 'Caret should be on line 2 after second redo');
  Assert.AreEqual(2, FEditor.CaretX, 'Caret X should be 2 after redoing B');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditUndoRedo);

end.
