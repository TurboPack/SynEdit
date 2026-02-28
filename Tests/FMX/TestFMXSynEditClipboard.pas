unit TestFMXSynEditClipboard;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditClipboard = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestCopySetsClipboard;
    [Test]
    procedure TestCopyWithNoSelection;
    [Test]
    procedure TestCutRemovesTextAndSetsClipboard;
    [Test]
    procedure TestCutInReadOnly;
    [Test]
    procedure TestPasteInsertsText;
    [Test]
    procedure TestPasteReplacesSelection;
    [Test]
    procedure TestPasteMultiLine;
    [Test]
    procedure TestPasteInReadOnly;
    [Test]
    procedure TestCutIsUndoable;
    [Test]
    procedure TestPasteIsUndoable;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes,
  SynEditKeyCmds,
  FMX.SynUnicode;

procedure TTestFMXSynEditClipboard.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'Hello World' + sLineBreak +
                  'Second line';
end;

procedure TTestFMXSynEditClipboard.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditClipboard.TestCopySetsClipboard;
begin
  // Select "Hello" and copy
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  Assert.AreEqual('Hello', FEditor.SelText);
  FEditor.CopyToClipboard;
  Assert.AreEqual('Hello', GetClipboardText,
    'Clipboard should contain selected text');
  // Text should be unchanged
  Assert.AreEqual('Hello World', FEditor.Lines[0],
    'Copy should not modify text');
end;

procedure TTestFMXSynEditClipboard.TestCopyWithNoSelection;
var
  OrigClipboard: string;
begin
  // Set clipboard to known value
  SetClipboardText('before');
  OrigClipboard := GetClipboardText;
  // No selection — copy should be a no-op
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.CopyToClipboard;
  Assert.AreEqual(OrigClipboard, GetClipboardText,
    'Clipboard should not change when nothing is selected');
end;

procedure TTestFMXSynEditClipboard.TestCutRemovesTextAndSetsClipboard;
begin
  // Select "Hello" and cut
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.CutToClipboard;
  Assert.AreEqual('Hello', GetClipboardText,
    'Clipboard should contain cut text');
  Assert.AreEqual(' World', FEditor.Lines[0],
    'Cut text should be removed from editor');
end;

procedure TTestFMXSynEditClipboard.TestCutInReadOnly;
begin
  FEditor.ReadOnly := True;
  // Select "Hello"
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  SetClipboardText('before');
  FEditor.CutToClipboard;
  Assert.AreEqual('Hello World', FEditor.Lines[0],
    'Cut should not modify text in read-only mode');
  Assert.AreEqual('before', GetClipboardText,
    'Clipboard should not change in read-only mode');
end;

procedure TTestFMXSynEditClipboard.TestPasteInsertsText;
begin
  SetClipboardText('ABC');
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.PasteFromClipboard;
  Assert.AreEqual('ABCHello World', FEditor.Lines[0],
    'Paste should insert clipboard text at caret');
end;

procedure TTestFMXSynEditClipboard.TestPasteReplacesSelection;
begin
  SetClipboardText('Goodbye');
  // Select "Hello"
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.PasteFromClipboard;
  Assert.AreEqual('Goodbye World', FEditor.Lines[0],
    'Paste should replace selected text');
end;

procedure TTestFMXSynEditClipboard.TestPasteMultiLine;
begin
  SetClipboardText('ABC' + sLineBreak + 'DEF');
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.PasteFromClipboard;
  Assert.AreEqual('ABC', FEditor.Lines[0],
    'First pasted line');
  Assert.AreEqual('DEFHello World', FEditor.Lines[1],
    'Second pasted line should be joined with original');
end;

procedure TTestFMXSynEditClipboard.TestPasteInReadOnly;
begin
  FEditor.ReadOnly := True;
  SetClipboardText('ABC');
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.PasteFromClipboard;
  Assert.AreEqual('Hello World', FEditor.Lines[0],
    'Paste should not modify text in read-only mode');
end;

procedure TTestFMXSynEditClipboard.TestCutIsUndoable;
begin
  // Select "Hello" and cut
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.CutToClipboard;
  Assert.AreEqual(' World', FEditor.Lines[0]);
  // Undo should restore
  FEditor.Undo;
  Assert.AreEqual('Hello World', FEditor.Lines[0],
    'Undo should restore text after cut');
end;

procedure TTestFMXSynEditClipboard.TestPasteIsUndoable;
begin
  SetClipboardText('ABC');
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.PasteFromClipboard;
  Assert.AreEqual('ABCHello World', FEditor.Lines[0]);
  FEditor.Undo;
  Assert.AreEqual('Hello World', FEditor.Lines[0],
    'Undo should restore text after paste');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditClipboard);

end.
