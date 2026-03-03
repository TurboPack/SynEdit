unit TestFMXSynEditEditing;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditEditing = class
  private
    FEditor: TFMXSynEdit;
    FChangeCount: Integer;
    procedure OnChangeHandler(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestDeleteAtEndOfLineJoinsLines;
    [Test]
    procedure TestBackspaceAtStartOfLineJoinsLines;
    [Test]
    procedure TestJoinedLineContentPreserved;
    [Test]
    procedure TestOverwriteModeReplacesChar;
    [Test]
    procedure TestInsertModeInsertsChar;
    [Test]
    procedure TestEcToggleMode;
    [Test]
    procedure TestOnChangeFiresOnEdit;
    [Test]
    procedure TestOnChangeFiresOnDelete;
    [Test]
    procedure TestOnChangeFiresDuringBeginUpdate;
    [Test]
    procedure TestBackspaceAtDocStartIsNoOp;
    [Test]
    procedure TestDeleteAtDocEndIsNoOp;
    [Test]
    procedure TestDeleteCharMiddleOfLine;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes,
  SynEditKeyCmds;

procedure TTestFMXSynEditEditing.OnChangeHandler(Sender: TObject);
begin
  Inc(FChangeCount);
end;

procedure TTestFMXSynEditEditing.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'Line one' + sLineBreak +
                  'Line two' + sLineBreak +
                  'Line three';
  FChangeCount := 0;
end;

procedure TTestFMXSynEditEditing.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditEditing.TestDeleteAtEndOfLineJoinsLines;
var
  OrigCount: Integer;
begin
  OrigCount := FEditor.LineCount;
  // Position at end of line 1 ('Line one' = 8 chars, caret at 9)
  FEditor.CaretXY := BufferCoord(9, 1);
  FEditor.ExecuteCommand(ecDeleteChar, #0);
  Assert.AreEqual(OrigCount - 1, FEditor.LineCount,
    'Delete at end-of-line should join with next line');
  Assert.AreEqual('Line oneLine two', FEditor.Lines[0],
    'Lines should be concatenated');
end;

procedure TTestFMXSynEditEditing.TestBackspaceAtStartOfLineJoinsLines;
var
  OrigCount: Integer;
begin
  OrigCount := FEditor.LineCount;
  // Position at start of line 2
  FEditor.CaretXY := BufferCoord(1, 2);
  FEditor.ExecuteCommand(ecDeleteLastChar, #0);
  Assert.AreEqual(OrigCount - 1, FEditor.LineCount,
    'Backspace at start-of-line should join with previous line');
  Assert.AreEqual('Line oneLine two', FEditor.Lines[0],
    'Lines should be concatenated');
end;

procedure TTestFMXSynEditEditing.TestJoinedLineContentPreserved;
begin
  // Join line 2 into line 1 via backspace
  FEditor.CaretXY := BufferCoord(1, 2);
  FEditor.ExecuteCommand(ecDeleteLastChar, #0);
  // Line 3 (now line 2) should be unchanged
  Assert.AreEqual('Line three', FEditor.Lines[1],
    'Remaining lines should be preserved');
end;

procedure TTestFMXSynEditEditing.TestOverwriteModeReplacesChar;
begin
  FEditor.InsertMode := False;
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.AreEqual('Xine one', FEditor.Lines[0],
    'Overwrite mode should replace the character at caret');
  Assert.AreEqual(2, FEditor.CaretX,
    'Caret should advance after overwrite');
end;

procedure TTestFMXSynEditEditing.TestInsertModeInsertsChar;
begin
  FEditor.InsertMode := True;
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.AreEqual('XLine one', FEditor.Lines[0],
    'Insert mode should insert without replacing');
end;

procedure TTestFMXSynEditEditing.TestEcToggleMode;
begin
  Assert.IsTrue(FEditor.InsertMode, 'Default should be insert mode');
  FEditor.ExecuteCommand(ecToggleMode, #0);
  Assert.IsFalse(FEditor.InsertMode, 'Should toggle to overwrite mode');
  FEditor.ExecuteCommand(ecToggleMode, #0);
  Assert.IsTrue(FEditor.InsertMode, 'Should toggle back to insert mode');
end;

procedure TTestFMXSynEditEditing.TestOnChangeFiresOnEdit;
begin
  FEditor.OnChange := OnChangeHandler;
  FChangeCount := 0;
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.IsTrue(FChangeCount > 0,
    'OnChange should fire when text is edited');
end;

procedure TTestFMXSynEditEditing.TestOnChangeFiresOnDelete;
begin
  FEditor.OnChange := OnChangeHandler;
  FChangeCount := 0;
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecDeleteChar, #0);
  Assert.IsTrue(FChangeCount > 0,
    'OnChange should fire when text is deleted');
end;

procedure TTestFMXSynEditEditing.TestOnChangeFiresDuringBeginUpdate;
begin
  // The current implementation fires OnChange even during BeginUpdate
  // (only Repaint is suppressed). Verify this behavior.
  FEditor.OnChange := OnChangeHandler;
  FChangeCount := 0;
  FEditor.BeginUpdate;
  try
    FEditor.CaretXY := BufferCoord(1, 1);
    FEditor.ExecuteCommand(ecChar, 'X');
  finally
    FEditor.EndUpdate;
  end;
  Assert.IsTrue(FChangeCount > 0,
    'OnChange should fire even during BeginUpdate');
end;

procedure TTestFMXSynEditEditing.TestBackspaceAtDocStartIsNoOp;
var
  OrigText: string;
begin
  OrigText := FEditor.Text;
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecDeleteLastChar, #0);
  Assert.AreEqual(OrigText, FEditor.Text,
    'Backspace at (1,1) should not modify text');
  Assert.AreEqual(1, FEditor.CaretX);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditEditing.TestDeleteAtDocEndIsNoOp;
var
  OrigText: string;
  LastLine: Integer;
begin
  OrigText := FEditor.Text;
  LastLine := FEditor.LineCount;
  // Position at end of last line
  FEditor.CaretXY := BufferCoord(Length(FEditor.Lines[LastLine - 1]) + 1, LastLine);
  FEditor.ExecuteCommand(ecDeleteChar, #0);
  Assert.AreEqual(OrigText, FEditor.Text,
    'Delete at end of document should not modify text');
end;

procedure TTestFMXSynEditEditing.TestDeleteCharMiddleOfLine;
begin
  FEditor.CaretXY := BufferCoord(5, 1);
  FEditor.ExecuteCommand(ecDeleteChar, #0);
  Assert.AreEqual('Lineone', FEditor.Lines[0],
    'Delete should remove character at caret position');
  Assert.AreEqual(5, FEditor.CaretX, 'Caret should not move');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditEditing);

end.
