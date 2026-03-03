unit TestFMXSynEditOptions;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditOptions = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestDefaultOptions;
    [Test]
    procedure TestSetOptionsInclude;
    [Test]
    procedure TestSetOptionsExclude;
    [Test]
    procedure TestReadOnlyDefault;
    [Test]
    procedure TestSetReadOnly;
    [Test]
    procedure TestReadOnlyBlocksTyping;
    [Test]
    procedure TestTabWidthDefault;
    [Test]
    procedure TestSetTabWidth;
    [Test]
    procedure TestInsertModeDefault;
    [Test]
    procedure TestRightEdgeDefault;
  end;

implementation

uses
  System.SysUtils,
  SynEditTypes,
  SynEditKeyCmds;

procedure TTestFMXSynEditOptions.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestFMXSynEditOptions.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditOptions.TestDefaultOptions;
begin
  Assert.IsTrue(eoAutoIndent in FEditor.Options, 'eoAutoIndent should be in defaults');
  Assert.IsTrue(eoTabsToSpaces in FEditor.Options, 'eoTabsToSpaces should be in defaults');
  Assert.IsTrue(eoGroupUndo in FEditor.Options, 'eoGroupUndo should be in defaults');
  Assert.IsTrue(eoKeepCaretX in FEditor.Options, 'eoKeepCaretX should be in defaults');
  Assert.IsTrue(eoTabIndent in FEditor.Options, 'eoTabIndent should be in defaults');
end;

procedure TTestFMXSynEditOptions.TestSetOptionsInclude;
begin
  FEditor.Options := FEditor.Options + [eoNoCaret];
  Assert.IsTrue(eoNoCaret in FEditor.Options);
end;

procedure TTestFMXSynEditOptions.TestSetOptionsExclude;
begin
  Assert.IsTrue(eoAutoIndent in FEditor.Options);
  FEditor.Options := FEditor.Options - [eoAutoIndent];
  Assert.IsFalse(eoAutoIndent in FEditor.Options);
end;

procedure TTestFMXSynEditOptions.TestReadOnlyDefault;
begin
  Assert.IsFalse(FEditor.ReadOnly);
end;

procedure TTestFMXSynEditOptions.TestSetReadOnly;
begin
  FEditor.ReadOnly := True;
  Assert.IsTrue(FEditor.ReadOnly);
end;

procedure TTestFMXSynEditOptions.TestReadOnlyBlocksTyping;
begin
  FEditor.Text := 'Hello';
  FEditor.ReadOnly := True;
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.AreEqual('Hello', FEditor.Lines[0],
    'ReadOnly should prevent character insertion');
end;

procedure TTestFMXSynEditOptions.TestTabWidthDefault;
begin
  Assert.AreEqual(8, FEditor.TabWidth);
end;

procedure TTestFMXSynEditOptions.TestSetTabWidth;
begin
  FEditor.TabWidth := 4;
  Assert.AreEqual(4, FEditor.TabWidth);
end;

procedure TTestFMXSynEditOptions.TestInsertModeDefault;
begin
  Assert.IsTrue(FEditor.InsertMode);
end;

procedure TTestFMXSynEditOptions.TestRightEdgeDefault;
begin
  Assert.AreEqual(80, FEditor.RightEdge);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditOptions);

end.
