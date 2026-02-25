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

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditUndoRedo);

end.
