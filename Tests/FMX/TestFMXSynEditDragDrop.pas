unit TestFMXSynEditDragDrop;

interface

uses
  DUnitX.TestFramework,
  SynEditTypes,
  SynEditDragDropShared,
  FMX.SynEdit;

type
  [TestFixture]
  TSynDragDropHelperTests = class
  public
    [Test]
    procedure TestIsDropCopy_NoCtrl;
    [Test]
    procedure TestIsDropCopy_Ctrl;
    [Test]
    procedure TestComputeDropInfo_External_AlwaysDrops;
    [Test]
    procedure TestComputeDropInfo_Internal_RejectsDropInSelection;
    [Test]
    procedure TestComputeDropInfo_Internal_AcceptsDropBefore;
    [Test]
    procedure TestComputeDropInfo_Internal_AcceptsDropAfter;
    [Test]
    procedure TestComputeDropInfo_Internal_CopyAllowsDropAtBoundary;
    [Test]
    procedure TestAdjustDropPos_SameLine;
    [Test]
    procedure TestAdjustDropPos_MultiLine;
    [Test]
    procedure TestAdjustDropPos_BeforeSelection;
  end;

  /// Test subclass to access the protected DropTextAtPos method
  TTestableEditor = class(TFMXSynEdit)
  public
    procedure TestDropTextAtPos(const DropText: string; DropPos: TBufferCoord;
      IsInternal, IsMove: Boolean);
  end;

  [TestFixture]
  TTestFMXDragDropIntegration = class
  private
    FEditor: TTestableEditor;
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

{ TTestableEditor }

procedure TTestableEditor.TestDropTextAtPos(const DropText: string;
  DropPos: TBufferCoord; IsInternal, IsMove: Boolean);
begin
  DropTextAtPos(DropText, DropPos, IsInternal, IsMove);
end;

{ TSynDragDropHelperTests }

procedure TSynDragDropHelperTests.TestIsDropCopy_NoCtrl;
begin
  Assert.IsFalse(TSynDragDropHelper.IsDropCopy([]));
  Assert.IsFalse(TSynDragDropHelper.IsDropCopy([ssShift]));
  Assert.IsFalse(TSynDragDropHelper.IsDropCopy([ssAlt]));
end;

procedure TSynDragDropHelperTests.TestIsDropCopy_Ctrl;
begin
  Assert.IsTrue(TSynDragDropHelper.IsDropCopy([ssCtrl]));
  Assert.IsTrue(TSynDragDropHelper.IsDropCopy([ssCtrl, ssShift]));
end;

procedure TSynDragDropHelperTests.TestComputeDropInfo_External_AlwaysDrops;
var
  Info: TSynDropInfo;
begin
  // External drop at any position should always be allowed
  Info := TSynDragDropHelper.ComputeDropInfo(
    BufferCoord(5, 3),   // DropPos
    BufferCoord(1, 1),   // SelStart
    BufferCoord(10, 1),  // SelEnd
    False,               // IsInternal
    True);               // IsMove
  Assert.IsTrue(Info.DoDrop);
  Assert.IsFalse(Info.DropAfter);
end;

procedure TSynDragDropHelperTests.TestComputeDropInfo_Internal_RejectsDropInSelection;
var
  Info: TSynDropInfo;
begin
  // Selection is from (3,2) to (8,2); drop at (5,2) — inside selection
  Info := TSynDragDropHelper.ComputeDropInfo(
    BufferCoord(5, 2),   // DropPos - inside selection
    BufferCoord(3, 2),   // SelStart
    BufferCoord(8, 2),   // SelEnd
    True,                // IsInternal
    True);               // IsMove
  Assert.IsFalse(Info.DoDrop);
end;

procedure TSynDragDropHelperTests.TestComputeDropInfo_Internal_AcceptsDropBefore;
var
  Info: TSynDropInfo;
begin
  // Selection is from (3,2) to (8,2); drop at (1,2) — before selection
  Info := TSynDragDropHelper.ComputeDropInfo(
    BufferCoord(1, 2),   // DropPos - before selection
    BufferCoord(3, 2),   // SelStart
    BufferCoord(8, 2),   // SelEnd
    True,                // IsInternal
    True);               // IsMove
  Assert.IsTrue(Info.DoDrop);
  Assert.IsFalse(Info.DropAfter);
end;

procedure TSynDragDropHelperTests.TestComputeDropInfo_Internal_AcceptsDropAfter;
var
  Info: TSynDropInfo;
begin
  // Selection is from (3,2) to (8,2); drop at (12,2) — after selection
  Info := TSynDragDropHelper.ComputeDropInfo(
    BufferCoord(12, 2),  // DropPos - after selection
    BufferCoord(3, 2),   // SelStart
    BufferCoord(8, 2),   // SelEnd
    True,                // IsInternal
    True);               // IsMove
  Assert.IsTrue(Info.DoDrop);
  Assert.IsTrue(Info.DropAfter);
end;

procedure TSynDragDropHelperTests.TestComputeDropInfo_Internal_CopyAllowsDropAtBoundary;
var
  Info: TSynDropInfo;
begin
  // Copy at selection end boundary should be allowed (not move)
  Info := TSynDragDropHelper.ComputeDropInfo(
    BufferCoord(8, 2),   // DropPos - at selection end
    BufferCoord(3, 2),   // SelStart
    BufferCoord(8, 2),   // SelEnd
    True,                // IsInternal
    False);              // IsMove = False (copy)
  Assert.IsTrue(Info.DoDrop);
  Assert.IsTrue(Info.DropAfter);

  // Copy at selection start boundary should also be allowed
  Info := TSynDragDropHelper.ComputeDropInfo(
    BufferCoord(3, 2),   // DropPos - at selection start
    BufferCoord(3, 2),   // SelStart
    BufferCoord(8, 2),   // SelEnd
    True,                // IsInternal
    False);              // IsMove = False (copy)
  Assert.IsTrue(Info.DoDrop);
  Assert.IsFalse(Info.DropAfter);
end;

procedure TSynDragDropHelperTests.TestAdjustDropPos_SameLine;
var
  Result: TBufferCoord;
begin
  // Selection was "World" (chars 7-12) on line 1, drop at char 20 on same line
  // After deletion, drop position shifts left by selection width (5 chars)
  Result := TSynDragDropHelper.AdjustDropPos(
    BufferCoord(20, 1),  // DropPos
    BufferCoord(7, 1),   // SelStart
    BufferCoord(12, 1),  // SelEnd
    True);               // DropAfter
  Assert.AreEqual(15, Result.Char);
  Assert.AreEqual(1, Result.Line);
end;

procedure TSynDragDropHelperTests.TestAdjustDropPos_MultiLine;
var
  Result: TBufferCoord;
begin
  // Selection from (3,2) to (5,4), drop at (10,6)
  // After deletion, 2 lines removed, drop line shifts up
  Result := TSynDragDropHelper.AdjustDropPos(
    BufferCoord(10, 6),  // DropPos
    BufferCoord(3, 2),   // SelStart
    BufferCoord(5, 4),   // SelEnd
    True);               // DropAfter
  Assert.AreEqual(10, Result.Char);  // Different line, no char adjustment
  Assert.AreEqual(4, Result.Line);   // 6 - (4-2) = 4
end;

procedure TSynDragDropHelperTests.TestAdjustDropPos_BeforeSelection;
var
  Result: TBufferCoord;
begin
  // Drop before selection — no adjustment needed
  Result := TSynDragDropHelper.AdjustDropPos(
    BufferCoord(1, 1),   // DropPos - before selection
    BufferCoord(3, 2),   // SelStart
    BufferCoord(8, 2),   // SelEnd
    False);              // DropAfter = False
  Assert.AreEqual(1, Result.Char);
  Assert.AreEqual(1, Result.Line);
end;

{ TTestFMXDragDropIntegration }

procedure TTestFMXDragDropIntegration.Setup;
begin
  FEditor := TTestableEditor.Create(nil);
end;

procedure TTestFMXDragDropIntegration.TearDown;
begin
  FEditor.Free;
end;

// --- External drop tests ---

procedure TTestFMXDragDropIntegration.TestExternalDrop_SingleLine;
begin
  FEditor.Text := 'Hello World';
  // Drop "XYZ" at position 6 (between "Hello" and " World")
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(6, 1), False, True);
  Assert.AreEqual('HelloXYZ World', FEditor.Lines[0]);
end;

procedure TTestFMXDragDropIntegration.TestExternalDrop_MultiLine;
begin
  FEditor.Text := 'Hello World';
  // Drop multi-line text at position 6
  FEditor.TestDropTextAtPos('AAA' + sLineBreak + 'BBB',
    BufferCoord(6, 1), False, True);
  Assert.AreEqual(2, FEditor.Lines.Count, 'Line count should increase');
  Assert.AreEqual('HelloAAA', FEditor.Lines[0]);
  Assert.AreEqual('BBB World', FEditor.Lines[1]);
end;

procedure TTestFMXDragDropIntegration.TestExternalDrop_PastEndOfLine;
begin
  FEditor.Text := 'Short';
  // Drop text at char 15, well past end of "Short" (5 chars)
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(15, 1), False, True);
  // Should pad with spaces: "Short" + 9 spaces + "XYZ"
  Assert.AreEqual('Short         XYZ', FEditor.Lines[0]);
end;

procedure TTestFMXDragDropIntegration.TestExternalDrop_AtEndOfDocument;
begin
  FEditor.Text := 'Line1' + sLineBreak + 'Line2';
  // Drop multi-line text at end of last line
  FEditor.TestDropTextAtPos('AAA' + sLineBreak + 'BBB' + sLineBreak + 'CCC',
    BufferCoord(6, 2), False, True);
  Assert.AreEqual(4, FEditor.Lines.Count, 'Should expand buffer for multi-line drop');
  Assert.AreEqual('Line1', FEditor.Lines[0]);
  Assert.AreEqual('Line2AAA', FEditor.Lines[1]);
  Assert.AreEqual('BBB', FEditor.Lines[2]);
  Assert.AreEqual('CCC', FEditor.Lines[3]);
end;

procedure TTestFMXDragDropIntegration.TestExternalDrop_ReadOnlyRejects;
begin
  FEditor.Text := 'Hello';
  FEditor.ReadOnly := True;
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(1, 1), False, True);
  Assert.AreEqual('Hello', FEditor.Lines[0], 'Read-only editor should reject drop');
end;

procedure TTestFMXDragDropIntegration.TestExternalDrop_EmptyTextRejects;
begin
  FEditor.Text := 'Hello';
  FEditor.TestDropTextAtPos('', BufferCoord(1, 1), False, True);
  Assert.AreEqual('Hello', FEditor.Lines[0], 'Empty text drop should be rejected');
end;

// --- Internal move tests ---

procedure TTestFMXDragDropIntegration.TestInternalMove_SameLine_Forward;
begin
  // "Hello World Goodbye"  — select "World" (7-12), move to char 19
  FEditor.Text := 'Hello World Goodbye';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(20, 1), True, True);
  // "World" removed from pos 7, then inserted after "Goodbye"
  // After deletion: "Hello  Goodbye" (14 chars)
  // AdjustDropPos: 20 - (12-7) = 15
  // Insert at 15: "Hello Goodbye World"  wait, that's wrong...
  // Actually "Hello  Goodbye" has a double space. Let me re-think.
  // Original: "Hello World Goodbye" (H=1..o=5, space=6, W=7..d=11, space=12, G=13..e=19)
  // Select chars 7-12 = "World " (includes trailing space)
  // Wait, selection is (7,1)-(12,1). Delete(SLine, 7, 12-7) = Delete(SLine, 7, 5) removes 5 chars = "World"
  // Result: "Hello  Goodbye" (note double space)
  // AdjustDropPos: char 20, same line as sel end (line 1), Dec(Char, 12-7=5) -> char 15
  // Insert "World" at char 15 of "Hello  Goodbye" (14 chars, so 15 = past end)
  // With space padding: "Hello  Goodbye" + " " + "World" wait, char 15 is 1 past end of 14 chars
  // Actually Length("Hello  Goodbye") = 14, char 15 = position just after 'e', no padding needed
  // Result: "Hello  GoodbyeWorld"
  // Hmm, that's not ideal but it's the correct behavior for these coordinates.
  // Let me use a cleaner example.
  Assert.AreEqual('Hello  GoodbyeWorld', FEditor.Lines[0]);
end;

procedure TTestFMXDragDropIntegration.TestInternalMove_SameLine_Backward;
begin
  // "Hello World" — select "World" (7-12), move to char 1
  FEditor.Text := 'Hello World';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 1), True, True);
  // Drop is before selection, so no adjustment
  // Delete "World" (7-12): "Hello " (note trailing space, only 6 chars remain... wait)
  // Original: "Hello World" = H(1)e(2)l(3)l(4)o(5) (6)W(7)o(8)r(9)l(10)d(11)
  // Delete(SLine, 7, 5): removes chars 7-11 = "World" -> "Hello " (6 chars)
  // Insert "World" at char 1: "WorldHello "
  Assert.AreEqual('WorldHello ', FEditor.Lines[0]);
end;

procedure TTestFMXDragDropIntegration.TestInternalMove_DifferentLine;
begin
  // Two lines: select "World" on line 1, move to start of line 2
  FEditor.Text := 'Hello World' + sLineBreak + 'Goodbye';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 2), True, True);
  // Delete "World" from line 1: "Hello " remains
  // Drop is on different line (after sel), AdjustDropPos: line stays 2, char stays 1
  // (sel is single line, no line adjustment needed)
  // Insert "World" at start of line 2
  Assert.AreEqual('Hello ', FEditor.Lines[0]);
  Assert.AreEqual('WorldGoodbye', FEditor.Lines[1]);
end;

procedure TTestFMXDragDropIntegration.TestInternalMove_MultiLineSelection;
begin
  // Three lines, select from middle of line 1 to middle of line 2
  FEditor.Text := 'AAABBB' + sLineBreak + 'CCCDDD' + sLineBreak + 'EEEFFF';
  // Select "BBB\r\nCCC" = from (4,1) to (4,2)
  FEditor.SetCaretAndSelection(BufferCoord(4, 2),
    BufferCoord(4, 1), BufferCoord(4, 2));
  // Move to start of line 3
  FEditor.TestDropTextAtPos('BBB' + sLineBreak + 'CCC',
    BufferCoord(1, 3), True, True);
  // Delete (4,1)-(4,2): line 1 becomes "AAA" + "DDD" = "AAADDD", line 2 deleted
  // Now 2 lines: "AAADDD", "EEEFFF"
  // AdjustDropPos: drop was line 3, after sel end line 2. Dec(Line, 2-1=1) -> line 2
  // Insert "BBB\r\nCCC" at (1, 2) of "EEEFFF"
  // Line 2 becomes "BBB", new line 3 = "CCCEEEFFF"
  Assert.AreEqual(3, FEditor.Lines.Count);
  Assert.AreEqual('AAADDD', FEditor.Lines[0]);
  Assert.AreEqual('BBB', FEditor.Lines[1]);
  Assert.AreEqual('CCCEEEFFF', FEditor.Lines[2]);
end;

procedure TTestFMXDragDropIntegration.TestInternalMove_SourceTextDeleted;
begin
  // Verify the source text is removed after an internal move
  FEditor.Text := 'ABCDEFGH';
  // Select "CDE" (3-6)
  FEditor.SetCaretAndSelection(BufferCoord(6, 1),
    BufferCoord(3, 1), BufferCoord(6, 1));
  FEditor.TestDropTextAtPos('CDE', BufferCoord(9, 1), True, True);
  // After delete "CDE": "ABFGH" (5 chars)
  // AdjustDropPos: 9 - (6-3) = 6
  // Insert at 6: "ABFGHCDE"  wait, 6 is past "ABFGH" (5 chars) by 1
  // char 6 = just past end, no padding needed
  // "ABFGH" + insert at 6 = "ABFGHCDE"
  Assert.AreEqual('ABFGHCDE', FEditor.Lines[0]);
  // Original "CDE" is gone from its original position
  Assert.AreEqual(0, Pos('CDEFGH', FEditor.Lines[0]),
    'Original text should be removed');
end;

procedure TTestFMXDragDropIntegration.TestInternalMove_RejectsDropInSelection;
begin
  // Drop inside selection should be rejected
  FEditor.Text := 'Hello World';
  FEditor.SetCaretAndSelection(BufferCoord(8, 1),
    BufferCoord(3, 1), BufferCoord(8, 1));
  FEditor.TestDropTextAtPos('llo W', BufferCoord(5, 1), True, True);
  // Drop at char 5 is inside selection (3-8), should be rejected
  Assert.AreEqual('Hello World', FEditor.Lines[0],
    'Drop inside selection should be rejected');
end;

// --- Internal copy tests ---

procedure TTestFMXDragDropIntegration.TestInternalCopy_DuplicatesText;
begin
  FEditor.Text := 'Hello World';
  // Select "World" (7-12)
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  // Copy (not move) to char 1
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 1), True, False);
  Assert.AreEqual('WorldHello World', FEditor.Lines[0]);
end;

procedure TTestFMXDragDropIntegration.TestInternalCopy_SourceTextPreserved;
begin
  FEditor.Text := 'ABCDEF';
  // Select "BCD" (2-5)
  FEditor.SetCaretAndSelection(BufferCoord(5, 1),
    BufferCoord(2, 1), BufferCoord(5, 1));
  // Copy to end of line
  FEditor.TestDropTextAtPos('BCD', BufferCoord(7, 1), True, False);
  // Source preserved, copy appended
  Assert.AreEqual('ABCDEFBCD', FEditor.Lines[0]);
end;

// --- Selection state tests ---

procedure TTestFMXDragDropIntegration.TestDrop_SelectsInsertedText;
begin
  FEditor.Text := 'Hello';
  FEditor.TestDropTextAtPos('XYZ', BufferCoord(1, 1), False, True);
  // After drop, selection should cover the inserted text
  Assert.IsTrue(FEditor.SelAvail, 'Inserted text should be selected');
  Assert.AreEqual('XYZ', FEditor.SelText, 'Selection should match dropped text');
end;

procedure TTestFMXDragDropIntegration.TestDrop_LineCountIncreasesForMultiLine;
begin
  FEditor.Text := 'OneLine';
  Assert.AreEqual(1, FEditor.Lines.Count);
  FEditor.TestDropTextAtPos('A' + sLineBreak + 'B' + sLineBreak + 'C',
    BufferCoord(8, 1), False, True);
  Assert.AreEqual(3, FEditor.Lines.Count,
    'Multi-line drop should increase line count');
end;

// --- Undo tests ---

procedure TTestFMXDragDropIntegration.TestInternalMove_UndoRestoresOriginal;
begin
  FEditor.Text := 'Hello World';
  FEditor.SetCaretAndSelection(BufferCoord(12, 1),
    BufferCoord(7, 1), BufferCoord(12, 1));
  FEditor.TestDropTextAtPos('World', BufferCoord(1, 1), True, True);
  // After move: "WorldHello "
  Assert.AreEqual('WorldHello ', FEditor.Lines[0]);
  // Undo should restore original
  FEditor.Undo;
  Assert.AreEqual('Hello World', FEditor.Text,
    'Undo should restore original text');
end;

initialization
  TDUnitX.RegisterTestFixture(TSynDragDropHelperTests);
  TDUnitX.RegisterTestFixture(TTestFMXDragDropIntegration);

end.
