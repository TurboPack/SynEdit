unit TestFMXSynEditCommands;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditCommands = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestEcCharInserts;
    [Test]
    procedure TestEcDeleteChar;
    [Test]
    procedure TestEcDeleteLastChar;
    [Test]
    procedure TestEcLineBreak;
    [Test]
    procedure TestEcLineBreakAutoIndent;
    [Test]
    procedure TestEcTabInsertsSpaces;
    [Test]
    procedure TestEcTabInsertsTabChar;
    [Test]
    procedure TestEcShiftTabRemovesIndent;
    [Test]
    procedure TestEcLeft;
    [Test]
    procedure TestEcRight;
    [Test]
    procedure TestEcUp;
    [Test]
    procedure TestEcDown;
    [Test]
    procedure TestEcLineStart;
    [Test]
    procedure TestEcLineEnd;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes,
  SynEditKeyCmds;

procedure TTestFMXSynEditCommands.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'Line one' + sLineBreak +
                  'Line two' + sLineBreak +
                  'Line three';
end;

procedure TTestFMXSynEditCommands.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditCommands.TestEcCharInserts;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.AreEqual('XLine one', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditCommands.TestEcDeleteChar;
begin
  // Delete first character of line 1
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecDeleteChar, #0);
  Assert.AreEqual('ine one', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditCommands.TestEcDeleteLastChar;
begin
  // Backspace at position 2 on line 1 should delete the 'L'
  FEditor.CaretXY := BufferCoord(2, 1);
  FEditor.ExecuteCommand(ecDeleteLastChar, #0);
  Assert.AreEqual('ine one', FEditor.Lines[0]);
  Assert.AreEqual(1, FEditor.CaretX);
end;

procedure TTestFMXSynEditCommands.TestEcLineBreak;
var
  OrigLineCount: Integer;
begin
  OrigLineCount := FEditor.LineCount;
  FEditor.CaretXY := BufferCoord(5, 1);
  FEditor.ExecuteCommand(ecLineBreak, #0);
  Assert.AreEqual(OrigLineCount + 1, FEditor.LineCount);
  Assert.AreEqual('Line', FEditor.Lines[0]);
  Assert.AreEqual(' one', FEditor.Lines[1]);
end;

procedure TTestFMXSynEditCommands.TestEcLineBreakAutoIndent;
begin
  Assert.IsTrue(eoAutoIndent in FEditor.Options);
  // Set line with leading spaces
  FEditor.Text := '  indented';
  FEditor.CaretXY := BufferCoord(11, 1); // end of 'indented' (2 spaces + 8 chars = pos 11)
  FEditor.ExecuteCommand(ecLineBreak, #0);
  Assert.AreEqual(2, FEditor.LineCount);
  // New line should have auto-indentation
  Assert.IsTrue(FEditor.Lines[1].StartsWith('  '),
    'Auto-indent should preserve leading whitespace');
end;

procedure TTestFMXSynEditCommands.TestEcTabInsertsSpaces;
begin
  // Default options include eoTabsToSpaces
  Assert.IsTrue(eoTabsToSpaces in FEditor.Options);
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecTab, #0);
  // Tab should insert spaces, not a tab character
  Assert.IsFalse(FEditor.Lines[0].Contains(#9),
    'With eoTabsToSpaces, tab should not insert tab character');
  Assert.IsTrue(FEditor.Lines[0].EndsWith('Hello'),
    'Original text should still be present after tab');
  Assert.IsTrue(Length(FEditor.Lines[0]) > 5,
    'Line should be longer after inserting spaces');
end;

procedure TTestFMXSynEditCommands.TestEcTabInsertsTabChar;
begin
  FEditor.Options := FEditor.Options - [eoTabsToSpaces];
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecTab, #0);
  Assert.IsTrue(FEditor.Lines[0].Contains(#9),
    'Without eoTabsToSpaces, tab should insert tab character');
end;

procedure TTestFMXSynEditCommands.TestEcShiftTabRemovesIndent;
begin
  FEditor.Text := '    Indented';
  FEditor.CaretXY := BufferCoord(5, 1);
  FEditor.ExecuteCommand(ecShiftTab, #0);
  Assert.IsTrue(Length(FEditor.Lines[0]) < Length('    Indented'),
    'Shift-Tab should remove indentation');
end;

procedure TTestFMXSynEditCommands.TestEcLeft;
begin
  FEditor.CaretXY := BufferCoord(5, 1);
  FEditor.ExecuteCommand(ecLeft, #0);
  Assert.AreEqual(4, FEditor.CaretX);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditCommands.TestEcRight;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecRight, #0);
  Assert.AreEqual(2, FEditor.CaretX);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditCommands.TestEcUp;
begin
  FEditor.CaretXY := BufferCoord(1, 2);
  FEditor.ExecuteCommand(ecUp, #0);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditCommands.TestEcDown;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecDown, #0);
  Assert.AreEqual(2, FEditor.CaretY);
end;

procedure TTestFMXSynEditCommands.TestEcLineStart;
begin
  FEditor.CaretXY := BufferCoord(5, 1);
  FEditor.ExecuteCommand(ecLineStart, #0);
  Assert.AreEqual(1, FEditor.CaretX);
end;

procedure TTestFMXSynEditCommands.TestEcLineEnd;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecLineEnd, #0);
  // 'Line one' = 8 chars, caret should be at 9 (past last char)
  Assert.AreEqual(9, FEditor.CaretX);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditCommands);

end.
