unit TestFMXSynEditSelection;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditSelection = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestEcSelLeft;
    [Test]
    procedure TestEcSelRight;
    [Test]
    procedure TestEcSelUp;
    [Test]
    procedure TestEcSelDown;
    [Test]
    procedure TestEcSelWordLeft;
    [Test]
    procedure TestEcSelWordRight;
    [Test]
    procedure TestEcSelLineStart;
    [Test]
    procedure TestEcSelLineEnd;
    [Test]
    procedure TestEcSelEditorTop;
    [Test]
    procedure TestEcSelEditorBottom;
    [Test]
    procedure TestAccumulatedSelection;
    [Test]
    procedure TestSelectionCollapseOnArrow;
    [Test]
    procedure TestTypingReplacesSelection;
    [Test]
    procedure TestDeleteReplacesSelection;
    [Test]
    procedure TestSelWordLeftAcrossLineBoundary;
    [Test]
    procedure TestSelWordRightAcrossLineBoundary;
    [Test]
    procedure TestSelLeftAtLineStart;
    [Test]
    procedure TestSelRightAtLineEnd;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes,
  SynEditKeyCmds;

procedure TTestFMXSynEditSelection.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'Hello World' + sLineBreak +
                  'Second line' + sLineBreak +
                  'Third line';
end;

procedure TTestFMXSynEditSelection.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditSelection.TestEcSelLeft;
begin
  FEditor.CaretXY := BufferCoord(6, 1); // after 'Hello'
  FEditor.ExecuteCommand(ecSelLeft, #0);
  Assert.AreEqual(5, FEditor.CaretX, 'Caret should move left');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
  Assert.AreEqual('o', FEditor.SelText, 'Should select one char to the left');
end;

procedure TTestFMXSynEditSelection.TestEcSelRight;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  Assert.AreEqual(2, FEditor.CaretX, 'Caret should move right');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
  Assert.AreEqual('H', FEditor.SelText, 'Should select first char');
end;

procedure TTestFMXSynEditSelection.TestEcSelUp;
begin
  FEditor.CaretXY := BufferCoord(3, 2);
  FEditor.ExecuteCommand(ecSelUp, #0);
  Assert.AreEqual(1, FEditor.CaretY, 'Caret should move up');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

procedure TTestFMXSynEditSelection.TestEcSelDown;
begin
  FEditor.CaretXY := BufferCoord(3, 1);
  FEditor.ExecuteCommand(ecSelDown, #0);
  Assert.AreEqual(2, FEditor.CaretY, 'Caret should move down');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

procedure TTestFMXSynEditSelection.TestEcSelWordLeft;
begin
  FEditor.CaretXY := BufferCoord(12, 1); // end of 'Hello World'
  FEditor.ExecuteCommand(ecSelWordLeft, #0);
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
  Assert.AreEqual('World', FEditor.SelText, 'Should select word "World"');
end;

procedure TTestFMXSynEditSelection.TestEcSelWordRight;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelWordRight, #0);
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
  // Word-right typically selects to end of current word including trailing space
  Assert.IsTrue(Length(FEditor.SelText) >= 5,
    'Should select at least "Hello"');
end;

procedure TTestFMXSynEditSelection.TestEcSelLineStart;
begin
  FEditor.CaretXY := BufferCoord(6, 1);
  FEditor.ExecuteCommand(ecSelLineStart, #0);
  Assert.AreEqual(1, FEditor.CaretX, 'Caret should be at line start');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
  Assert.AreEqual('Hello', FEditor.SelText, 'Should select from start to original pos');
end;

procedure TTestFMXSynEditSelection.TestEcSelLineEnd;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelLineEnd, #0);
  Assert.AreEqual(12, FEditor.CaretX, 'Caret should be at line end');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
  Assert.AreEqual('Hello World', FEditor.SelText, 'Should select entire line');
end;

procedure TTestFMXSynEditSelection.TestEcSelEditorTop;
begin
  FEditor.CaretXY := BufferCoord(1, 3);
  FEditor.ExecuteCommand(ecSelEditorTop, #0);
  Assert.AreEqual(1, FEditor.CaretY, 'Caret should be at top');
  Assert.AreEqual(1, FEditor.CaretX, 'Caret should be at column 1');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

procedure TTestFMXSynEditSelection.TestEcSelEditorBottom;
begin
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelEditorBottom, #0);
  Assert.AreEqual(3, FEditor.CaretY, 'Caret should be at bottom');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

procedure TTestFMXSynEditSelection.TestAccumulatedSelection;
begin
  // Multiple Shift+Right should accumulate selection
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  Assert.AreEqual(4, FEditor.CaretX, 'Caret should be at pos 4');
  Assert.AreEqual('Hel', FEditor.SelText, 'Should select 3 chars');
end;

procedure TTestFMXSynEditSelection.TestSelectionCollapseOnArrow;
begin
  // Select some text, then press arrow without shift to collapse
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
  // Now press Right without shift
  FEditor.ExecuteCommand(ecRight, #0);
  Assert.IsFalse(FEditor.SelAvail, 'Selection should be collapsed');
end;

procedure TTestFMXSynEditSelection.TestTypingReplacesSelection;
begin
  // Select "Hello" then type "X"
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  Assert.AreEqual('Hello', FEditor.SelText);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.AreEqual('X World', FEditor.Lines[0],
    'Typing should replace selected text');
end;

procedure TTestFMXSynEditSelection.TestDeleteReplacesSelection;
begin
  // Select "Hello" then press Delete
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  FEditor.ExecuteCommand(ecSelRight, #0);
  Assert.AreEqual('Hello', FEditor.SelText);
  FEditor.ExecuteCommand(ecDeleteChar, #0);
  Assert.AreEqual(' World', FEditor.Lines[0],
    'Delete should remove selected text');
end;

procedure TTestFMXSynEditSelection.TestSelWordLeftAcrossLineBoundary;
begin
  // At beginning of line 2, ecSelWordLeft should select into line 1
  FEditor.CaretXY := BufferCoord(1, 2);
  FEditor.ExecuteCommand(ecSelWordLeft, #0);
  Assert.AreEqual(1, FEditor.CaretY,
    'Caret should move to previous line');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

procedure TTestFMXSynEditSelection.TestSelWordRightAcrossLineBoundary;
begin
  // At end of line 1, ecSelWordRight should select into line 2
  FEditor.CaretXY := BufferCoord(12, 1); // past 'Hello World'
  FEditor.ExecuteCommand(ecSelWordRight, #0);
  Assert.AreEqual(2, FEditor.CaretY,
    'Caret should move to next line');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

procedure TTestFMXSynEditSelection.TestSelLeftAtLineStart;
begin
  // ecSelLeft at the beginning of line 2 should move to end of line 1
  FEditor.CaretXY := BufferCoord(1, 2);
  FEditor.ExecuteCommand(ecSelLeft, #0);
  Assert.AreEqual(1, FEditor.CaretY,
    'Caret should move to previous line');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

procedure TTestFMXSynEditSelection.TestSelRightAtLineEnd;
begin
  // ecSelRight at the end of line 1 should move to start of line 2
  FEditor.CaretXY := BufferCoord(12, 1);
  FEditor.ExecuteCommand(ecSelRight, #0);
  Assert.AreEqual(2, FEditor.CaretY,
    'Caret should move to next line');
  Assert.AreEqual(1, FEditor.CaretX,
    'Caret should be at start of next line');
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be active');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditSelection);

end.
