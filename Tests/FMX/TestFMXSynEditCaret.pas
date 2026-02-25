unit TestFMXSynEditCaret;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditCaret = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestInitialPosition;
    [Test]
    procedure TestSetCaretXY;
    [Test]
    procedure TestCaretClampedToMin1;
    [Test]
    procedure TestSelectAll;
    [Test]
    procedure TestSelAvailAfterSelectAll;
    [Test]
    procedure TestSelTextAfterSelectAll;
    [Test]
    procedure TestSetCaretAndSelection;
    [Test]
    procedure TestBlockBeginBlockEnd;
    [Test]
    procedure TestClearSelection;
    [Test]
    procedure TestGetTextRange;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes;

procedure TTestFMXSynEditCaret.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestFMXSynEditCaret.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditCaret.TestInitialPosition;
begin
  Assert.AreEqual(1, FEditor.CaretX);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditCaret.TestSetCaretXY;
begin
  FEditor.Text := 'Hello World';
  FEditor.CaretXY := BufferCoord(6, 1);
  Assert.AreEqual(6, FEditor.CaretX);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditCaret.TestCaretClampedToMin1;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretX := 0;
  Assert.IsTrue(FEditor.CaretX >= 1);
  FEditor.CaretY := 0;
  Assert.IsTrue(FEditor.CaretY >= 1);
end;

procedure TTestFMXSynEditCaret.TestSelectAll;
begin
  FEditor.Text := 'Hello World';
  FEditor.SelectAll;
  Assert.IsTrue(FEditor.SelAvail);
end;

procedure TTestFMXSynEditCaret.TestSelAvailAfterSelectAll;
begin
  FEditor.Text := 'Line1' + sLineBreak + 'Line2';
  FEditor.SelectAll;
  Assert.IsTrue(FEditor.SelAvail);
end;

procedure TTestFMXSynEditCaret.TestSelTextAfterSelectAll;
begin
  FEditor.Text := 'Hello';
  FEditor.SelectAll;
  Assert.AreEqual('Hello', FEditor.SelText);
end;

procedure TTestFMXSynEditCaret.TestSetCaretAndSelection;
var
  BC1, BC2, BCCaret: TBufferCoord;
begin
  FEditor.Text := 'Hello World';
  BC1 := BufferCoord(1, 1);
  BC2 := BufferCoord(6, 1);
  BCCaret := BufferCoord(6, 1);
  FEditor.SetCaretAndSelection(BCCaret, BC1, BC2);
  Assert.AreEqual(6, FEditor.CaretX);
  Assert.IsTrue(FEditor.SelAvail);
  Assert.AreEqual('Hello', FEditor.SelText);
end;

procedure TTestFMXSynEditCaret.TestBlockBeginBlockEnd;
var
  BC1, BC2, BCCaret: TBufferCoord;
begin
  FEditor.Text := 'ABCDEF';
  BC1 := BufferCoord(2, 1);
  BC2 := BufferCoord(5, 1);
  BCCaret := BufferCoord(5, 1);
  FEditor.SetCaretAndSelection(BCCaret, BC1, BC2);
  Assert.AreEqual(2, FEditor.BlockBegin.Char);
  Assert.AreEqual(5, FEditor.BlockEnd.Char);
end;

procedure TTestFMXSynEditCaret.TestClearSelection;
begin
  FEditor.Text := 'Hello World';
  FEditor.SelectAll;
  Assert.IsTrue(FEditor.SelAvail);
  FEditor.ClearSelection;
  Assert.IsFalse(FEditor.SelAvail);
end;

procedure TTestFMXSynEditCaret.TestGetTextRange;
var
  BC1, BC2: TBufferCoord;
begin
  FEditor.Text := 'ABCDEFGH';
  BC1 := BufferCoord(3, 1);
  BC2 := BufferCoord(6, 1);
  Assert.AreEqual('CDE', FEditor.GetTextRange(BC1, BC2));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditCaret);

end.
