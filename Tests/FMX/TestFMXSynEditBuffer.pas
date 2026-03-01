unit TestFMXSynEditBuffer;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditBuffer = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestLinesInitiallyEmpty;
    [Test]
    procedure TestAddSingle;
    [Test]
    procedure TestAddMultiple;
    [Test]
    procedure TestInsert;
    [Test]
    procedure TestDelete;
    [Test]
    procedure TestCount;
    [Test]
    procedure TestClear;
    [Test]
    procedure TestTextProperty;
    [Test]
    procedure TestIndexAccess;
    [Test]
    procedure TestLineCountMatchesLines;
  end;

implementation

uses
  System.SysUtils;

procedure TTestFMXSynEditBuffer.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestFMXSynEditBuffer.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditBuffer.TestLinesInitiallyEmpty;
begin
  Assert.AreEqual(0, FEditor.Lines.Count);
end;

procedure TTestFMXSynEditBuffer.TestAddSingle;
begin
  FEditor.Lines.Add('Hello');
  Assert.AreEqual(1, FEditor.Lines.Count);
  Assert.AreEqual('Hello', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditBuffer.TestAddMultiple;
begin
  FEditor.Lines.Add('Line1');
  FEditor.Lines.Add('Line2');
  FEditor.Lines.Add('Line3');
  Assert.AreEqual(3, FEditor.Lines.Count);
end;

procedure TTestFMXSynEditBuffer.TestInsert;
begin
  FEditor.Lines.Add('First');
  FEditor.Lines.Add('Third');
  FEditor.Lines.Insert(1, 'Second');
  Assert.AreEqual(3, FEditor.Lines.Count);
  Assert.AreEqual('Second', FEditor.Lines[1]);
end;

procedure TTestFMXSynEditBuffer.TestDelete;
begin
  FEditor.Lines.Add('Keep');
  FEditor.Lines.Add('Remove');
  FEditor.Lines.Delete(1);
  Assert.AreEqual(1, FEditor.Lines.Count);
  Assert.AreEqual('Keep', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditBuffer.TestCount;
begin
  Assert.AreEqual(0, FEditor.Lines.Count);
  FEditor.Lines.Add('A');
  Assert.AreEqual(1, FEditor.Lines.Count);
  FEditor.Lines.Add('B');
  Assert.AreEqual(2, FEditor.Lines.Count);
end;

procedure TTestFMXSynEditBuffer.TestClear;
begin
  FEditor.Lines.Add('Something');
  FEditor.Lines.Add('Else');
  FEditor.Lines.Clear;
  Assert.AreEqual(0, FEditor.Lines.Count);
end;

procedure TTestFMXSynEditBuffer.TestTextProperty;
begin
  FEditor.Lines.Text := 'Line1' + sLineBreak + 'Line2';
  Assert.AreEqual(2, FEditor.Lines.Count);
  Assert.AreEqual('Line1', FEditor.Lines[0]);
  Assert.AreEqual('Line2', FEditor.Lines[1]);
end;

procedure TTestFMXSynEditBuffer.TestIndexAccess;
begin
  FEditor.Lines.Add('Zero');
  FEditor.Lines.Add('One');
  FEditor.Lines.Add('Two');
  Assert.AreEqual('Zero', FEditor.Lines[0]);
  Assert.AreEqual('One', FEditor.Lines[1]);
  Assert.AreEqual('Two', FEditor.Lines[2]);
end;

procedure TTestFMXSynEditBuffer.TestLineCountMatchesLines;
begin
  FEditor.Text := 'A' + sLineBreak + 'B' + sLineBreak + 'C';
  Assert.AreEqual(FEditor.Lines.Count, FEditor.LineCount);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditBuffer);

end.
