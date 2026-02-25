unit TestFMXSynEditContent;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditContent = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestGetTextEmpty;
    [Test]
    procedure TestSetTextSingleLine;
    [Test]
    procedure TestSetTextMultiLine;
    [Test]
    procedure TestSetTextClearsPrevious;
    [Test]
    procedure TestClearAll;
    [Test]
    procedure TestClearAllResetsCaret;
    [Test]
    procedure TestLoadSaveStreamRoundTrip;
    [Test]
    procedure TestSetTextResetsSelection;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  SynEditTypes;

procedure TTestFMXSynEditContent.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestFMXSynEditContent.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditContent.TestGetTextEmpty;
begin
  Assert.AreEqual('', FEditor.Text);
end;

procedure TTestFMXSynEditContent.TestSetTextSingleLine;
begin
  FEditor.Text := 'Hello';
  Assert.AreEqual(1, FEditor.Lines.Count);
  Assert.AreEqual('Hello', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditContent.TestSetTextMultiLine;
begin
  FEditor.Text := 'Line1' + sLineBreak + 'Line2' + sLineBreak + 'Line3';
  Assert.AreEqual(3, FEditor.Lines.Count);
  Assert.AreEqual('Line1', FEditor.Lines[0]);
  Assert.AreEqual('Line2', FEditor.Lines[1]);
  Assert.AreEqual('Line3', FEditor.Lines[2]);
end;

procedure TTestFMXSynEditContent.TestSetTextClearsPrevious;
begin
  FEditor.Text := 'Old content';
  FEditor.Text := 'New content';
  Assert.AreEqual(1, FEditor.Lines.Count);
  Assert.AreEqual('New content', FEditor.Lines[0]);
end;

procedure TTestFMXSynEditContent.TestClearAll;
begin
  FEditor.Text := 'Something' + sLineBreak + 'Here';
  FEditor.ClearAll;
  Assert.AreEqual(0, FEditor.Lines.Count);
end;

procedure TTestFMXSynEditContent.TestClearAllResetsCaret;
begin
  FEditor.Text := 'Line1' + sLineBreak + 'Line2';
  FEditor.CaretXY := BufferCoord(3, 2);
  FEditor.ClearAll;
  Assert.AreEqual(1, FEditor.CaretX);
  Assert.AreEqual(1, FEditor.CaretY);
end;

procedure TTestFMXSynEditContent.TestLoadSaveStreamRoundTrip;
var
  Stream: TMemoryStream;
  OrigText: string;
begin
  OrigText := 'Alpha' + sLineBreak + 'Beta' + sLineBreak + 'Gamma';
  FEditor.Text := OrigText;

  Stream := TMemoryStream.Create;
  try
    FEditor.SaveToStream(Stream);
    FEditor.ClearAll;
    Assert.AreEqual(0, FEditor.Lines.Count);

    Stream.Position := 0;
    FEditor.LoadFromStream(Stream);
    Assert.AreEqual(3, FEditor.Lines.Count);
    Assert.AreEqual('Alpha', FEditor.Lines[0]);
    Assert.AreEqual('Beta', FEditor.Lines[1]);
    Assert.AreEqual('Gamma', FEditor.Lines[2]);
  finally
    Stream.Free;
  end;
end;

procedure TTestFMXSynEditContent.TestSetTextResetsSelection;
begin
  FEditor.Text := 'Select me';
  FEditor.SelectAll;
  Assert.IsTrue(FEditor.SelAvail);
  FEditor.Text := 'New text';
  Assert.IsFalse(FEditor.SelAvail);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditContent);

end.
