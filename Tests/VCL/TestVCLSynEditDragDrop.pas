unit TestVCLSynEditDragDrop;

interface

uses
  DUnitX.TestFramework,
  Vcl.Forms,
  SynEditTypes,
  SynEdit;

type
  /// Test subclass to access the protected DropTextAtPos method
  TTestableVCLEditor = class(TSynEdit)
  public
    procedure TestDropTextAtPos(const DropText: string; DropPos: TBufferCoord;
      IsInternal, IsMove: Boolean);
  end;

  [TestFixture]
  TTestVCLDragDropIntegration = class
  private
    FForm: TForm;
    FEditor: TTestableVCLEditor;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // External drop tests
    [Test]
    procedure TestExternalDrop_SingleLine;
    [Test]
    procedure TestExternalDrop_MultiLine;
    [Test]
    procedure TestExternalDrop_PastEndOfLine;
    [Test]
    procedure TestExternalDrop_AtEndOfDocument;
    [Test]
    procedure TestExternalDrop_ReadOnlyRejects;
    [Test]
    procedure TestExternalDrop_EmptyTextRejects;
    // Internal move tests
    [Test]
    procedure TestInternalMove_SameLine_Forward;
    [Test]
    procedure TestInternalMove_SameLine_Backward;
    [Test]
    procedure TestInternalMove_DifferentLine;
    [Test]
    procedure TestInternalMove_MultiLineSelection;
    [Test]
    procedure TestInternalMove_SourceTextDeleted;
    [Test]
    procedure TestInternalMove_RejectsDropInSelection;
    // Internal copy tests
    [Test]
    procedure TestInternalCopy_DuplicatesText;
    [Test]
    procedure TestInternalCopy_SourceTextPreserved;
    // Selection state tests
    [Test]
    procedure TestDrop_SelectsInsertedText;
    [Test]
    procedure TestDrop_LineCountIncreasesForMultiLine;
    // Undo tests
    [Test]
    procedure TestInternalMove_UndoRestoresOriginal;
  end;

implementation

uses
  System.SysUtils,
  System.Classes;

{ TTestableVCLEditor }

procedure TTestableVCLEditor.TestDropTextAtPos(const DropText: string;
  DropPos: TBufferCoord; IsInternal, IsMove: Boolean);
begin
  DropTextAtPos(DropText, DropPos, IsInternal, IsMove);
end;

{ TTestVCLDragDropIntegration }

procedure TTestVCLDragDropIntegration.Setup;
begin
  FForm := TForm.CreateNew(nil);
  FEditor := TTestableVCLEditor.Create(FForm);
  FEditor.Parent := FForm;
end;

procedure TTestVCLDragDropIntegration.TearDown;
begin
  FForm.Free;
end;

// --- External drop tests ---

procedure TTestVCLDragDropIntegration.TestExternalDrop_SingleLine;
begin
  FEditor.Text := 'Hello World';
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(6, 1), False, True);
  Assert.AreEqual('HelloXYZ World', FEditor.Lines[0]);
end;

procedure TTestVCLDragDropIntegration.TestExternalDrop_MultiLine;
begin
  FEditor.Text := 'Hello World';
  FEditor.TestDropTextAtPos('AAA' + sLineBreak + 'BBB',
    BufferCoord(6, 1), False, True);
  Assert.AreEqual(2, FEditor.Lines.Count, 'Line count should increase');
  Assert.AreEqual('HelloAAA', FEditor.Lines[0]);
  Assert.AreEqual('BBB World', FEditor.Lines[1]);
end;

procedure TTestVCLDragDropIntegration.TestExternalDrop_PastEndOfLine;
begin
  FEditor.Text := 'Short';
  // Drop text at char 15, well past end of "Short" (5 chars)
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(15, 1), False, True);
  // VCL clamps drop position to end of line via ValidBC
  Assert.AreEqual('ShortXYZ', FEditor.Lines[0]);
end;

procedure TTestVCLDragDropIntegration.TestExternalDrop_AtEndOfDocument;
begin
  FEditor.Text := 'Line1' + sLineBreak + 'Line2';
  FEditor.TestDropTextAtPos('AAA' + sLineBreak + 'BBB' + sLineBreak + 'CCC',
    BufferCoord(6, 2), False, True);
  Assert.AreEqual(4, FEditor.Lines.Count, 'Should expand buffer for multi-line drop');
  Assert.AreEqual('Line1', FEditor.Lines[0]);
  Assert.AreEqual('Line2AAA', FEditor.Lines[1]);
  Assert.AreEqual('BBB', FEditor.Lines[2]);
  Assert.AreEqual('CCC', FEditor.Lines[3]);
end;

procedure TTestVCLDragDropIntegration.TestExternalDrop_ReadOnlyRejects;
begin
  FEditor.Text := 'Hello';
  FEditor.ReadOnly := True;
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(1, 1), False, True);
  Assert.AreEqual('Hello', FEditor.Lines[0], 'Read-only editor should reject drop');
end;

procedure TTestVCLDragDropIntegration.TestExternalDrop_EmptyTextRejects;
begin
  FEditor.Text := 'Hello';
  FEditor.TestDropTextAtPos('', BufferCoord(1, 1), False, True);
  Assert.AreEqual('Hello', FEditor.Lines[0], 'Empty text drop should be rejected');
end;

// --- Internal move tests ---

procedure TTestVCLDragDropIntegration.TestInternalMove_SameLine_Forward;
begin
  // "Hello World Goodbye" — select "World" (7-12), move to char 20
  FEditor.Text := 'Hello World Goodbye';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(20, 1), True, True);
  Assert.AreEqual('Hello  GoodbyeWorld', FEditor.Lines[0]);
end;

procedure TTestVCLDragDropIntegration.TestInternalMove_SameLine_Backward;
begin
  // "Hello World" — select "World" (7-12), move to char 1
  FEditor.Text := 'Hello World';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 1), True, True);
  Assert.AreEqual('WorldHello ', FEditor.Lines[0]);
end;

procedure TTestVCLDragDropIntegration.TestInternalMove_DifferentLine;
begin
  FEditor.Text := 'Hello World' + sLineBreak + 'Goodbye';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 2), True, True);
  Assert.AreEqual('Hello ', FEditor.Lines[0]);
  Assert.AreEqual('WorldGoodbye', FEditor.Lines[1]);
end;

procedure TTestVCLDragDropIntegration.TestInternalMove_MultiLineSelection;
begin
  FEditor.Text := 'AAABBB' + sLineBreak + 'CCCDDD' + sLineBreak + 'EEEFFF';
  // Select "BBB\r\nCCC" = from (4,1) to (4,2)
  FEditor.SetCaretAndSelection(BufferCoord(4, 2),
    BufferCoord(4, 1), BufferCoord(4, 2));
  FEditor.TestDropTextAtPos('BBB' + sLineBreak + 'CCC',
    BufferCoord(1, 3), True, True);
  Assert.AreEqual(3, FEditor.Lines.Count);
  Assert.AreEqual('AAADDD', FEditor.Lines[0]);
  Assert.AreEqual('BBB', FEditor.Lines[1]);
  Assert.AreEqual('CCCEEEFFF', FEditor.Lines[2]);
end;

procedure TTestVCLDragDropIntegration.TestInternalMove_SourceTextDeleted;
begin
  FEditor.Text := 'ABCDEFGH';
  // Select "CDE" (3-6)
  FEditor.SetCaretAndSelection(BufferCoord(6, 1),
    BufferCoord(3, 1), BufferCoord(6, 1));
  FEditor.TestDropTextAtPos('CDE', BufferCoord(9, 1), True, True);
  Assert.AreEqual('ABFGHCDE', FEditor.Lines[0]);
  Assert.AreEqual(0, Pos('CDEFGH', FEditor.Lines[0]),
    'Original text should be removed');
end;

procedure TTestVCLDragDropIntegration.TestInternalMove_RejectsDropInSelection;
begin
  FEditor.Text := 'Hello World';
  FEditor.SetCaretAndSelection(BufferCoord(8, 1),
    BufferCoord(3, 1), BufferCoord(8, 1));
  FEditor.TestDropTextAtPos('llo W', BufferCoord(5, 1), True, True);
  Assert.AreEqual('Hello World', FEditor.Lines[0],
    'Drop inside selection should be rejected');
end;

// --- Internal copy tests ---

procedure TTestVCLDragDropIntegration.TestInternalCopy_DuplicatesText;
begin
  FEditor.Text := 'Hello World';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 1), True, False);
  Assert.AreEqual('WorldHello World', FEditor.Lines[0]);
end;

procedure TTestVCLDragDropIntegration.TestInternalCopy_SourceTextPreserved;
begin
  FEditor.Text := 'ABCDEF';
  FEditor.SetCaretAndSelection(BufferCoord(5, 1),
    BufferCoord(2, 1), BufferCoord(5, 1));
  FEditor.TestDropTextAtPos('BCD', BufferCoord(7, 1), True, False);
  Assert.AreEqual('ABCDEFBCD', FEditor.Lines[0]);
end;

// --- Selection state tests ---

procedure TTestVCLDragDropIntegration.TestDrop_SelectsInsertedText;
begin
  FEditor.Text := 'Hello';
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(1, 1), False, True);
  Assert.IsTrue(FEditor.SelAvail, 'Inserted text should be selected');
  Assert.AreEqual('XYZ', FEditor.SelText, 'Selection should match dropped text');
end;

procedure TTestVCLDragDropIntegration.TestDrop_LineCountIncreasesForMultiLine;
begin
  FEditor.Text := 'OneLine';
  Assert.AreEqual(1, FEditor.Lines.Count);
  FEditor.TestDropTextAtPos('A' + sLineBreak + 'B' + sLineBreak + 'C',
    BufferCoord(8, 1), False, True);
  Assert.AreEqual(3, FEditor.Lines.Count,
    'Multi-line drop should increase line count');
end;

// --- Undo tests ---

procedure TTestVCLDragDropIntegration.TestInternalMove_UndoRestoresOriginal;
begin
  FEditor.Text := 'Hello World';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 1), True, True);
  Assert.AreEqual('WorldHello ', FEditor.Lines[0]);
  FEditor.Undo;
  Assert.AreEqual('Hello World', FEditor.Text,
    'Undo should restore original text');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestVCLDragDropIntegration);

end.
