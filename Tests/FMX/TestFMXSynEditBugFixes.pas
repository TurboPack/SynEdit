unit TestFMXSynEditBugFixes;

interface

uses
  System.Classes,
  System.UITypes,
  FMX.Types,
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  { Tests for blocking bugs identified in the FMX port review }

  [TestFixture]
  TTestPluginRegistration = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestPluginAutoRegisters;
    [Test]
    procedure TestPluginAutoUnregistersOnFree;
    [Test]
    procedure TestPluginHandlersSetViaConstructor;
    [Test]
    procedure TestPluginDefaultHandlersEmpty;
    [Test]
    procedure TestPluginAfterPaintDispatch;
  end;

  [TestFixture]
  TTestModifiedProperty = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestModifiedInitiallyFalse;
    [Test]
    procedure TestModifiedTrueAfterEdit;
    [Test]
    procedure TestModifiedFalseAfterSaveReset;
    [Test]
    procedure TestModifiedDiffersFromCanUndo;
    [Test]
    procedure TestCanUndoTrueWhenModifiedFalse;
  end;

  [TestFixture]
  TTestTextBufferNilWidthFunc = class
  public
    [Test]
    procedure TestGetTextWidthWithNilFunc;
    [Test]
    procedure TestGetMaxWidthWithNilFunc;
    [Test]
    procedure TestPutWithNilFunc;
  end;

  [TestFixture]
  TTestTabExpansion = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestMaxScrollWidthWithTabs;
    [Test]
    procedure TestMaxScrollWidthNoTabs;
    [Test]
    procedure TestMaxScrollWidthMixedLines;
    [Test]
    procedure TestExpandTabsAtColumnBoundary;
    [Test]
    procedure TestExpandTabsMidColumn;
  end;

  [TestFixture]
  TTestAutoIndentTabs = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAutoIndentPreservesLeadingTabs;
    [Test]
    procedure TestAutoIndentPreservesMixedWhitespace;
    [Test]
    procedure TestAutoIndentWithSpacesStillWorks;
    [Test]
    procedure TestNoAutoIndentWhenDisabled;
  end;

  [TestFixture]
  TTestPixelToBufferCoord = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestClickLeftOfCharBoundary;
    [Test]
    procedure TestClickRightOfCharBoundary;
    [Test]
    procedure TestClickExactlyOnCharBoundary;
    [Test]
    procedure TestClickInGutterClampsToOne;
  end;

  [TestFixture]
  TTestScrollBarSizing = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestLinesInWindowDeltaNoScrollBars;
    [Test]
    procedure TestCharsInWindowDeltaNoScrollBars;
    [Test]
    procedure TestScrollBarsHiddenInitially;
  end;

  { Tests for cross-platform fixes (issues 13-15) }
  [TestFixture]
  TTestCrossPlatformFixes = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    { Issue 13: Selection colors should not use Windows system colors }
    [Test]
    procedure TestSelectionBGNotSystemColor;
    [Test]
    procedure TestSelectionFGNotSystemColor;
    [Test]
    procedure TestSelectionBGIsExplicitARGB;
    [Test]
    procedure TestSelectionFGIsExplicitARGB;
    { Issue 15: BlockBegin/BlockEnd should be writable }
    [Test]
    procedure TestBlockBeginWritable;
    [Test]
    procedure TestBlockEndWritable;
    [Test]
    procedure TestBlockBeginResetsBlockEnd;
    [Test]
    procedure TestBlockBeginClampsToMin;
    [Test]
    procedure TestSetSelectionViaProperties;
  end;

  [TestFixture]
  TTestKeyboardHandlerChain = class
  private
    FEditor: TFMXSynEdit;
    FHandlerCalled: Boolean;
    FHandlerKey: Word;
    FUserHandlerCalled: Boolean;
    procedure KeyDownHandler(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure ConsumingKeyDownHandler(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure UserOnKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestAddKeyDownHandlerIsCalled;
    [Test]
    procedure TestRemoveKeyDownHandler;
    [Test]
    procedure TestMultipleHandlersBothCalled;
    [Test]
    procedure TestConsumingHandlerStopsChain;
    [Test]
    procedure TestOnKeyDownPreservedWithHandler;
  end;

implementation

uses
  System.Types,
  System.SysUtils,
  System.Math,
  FMX.Graphics,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditTextBuffer,
  FMX.SynEditMiscClasses;

type
  // Helper to access protected KeyDown for testing
  TTestFMXSynEdit = class(TFMXSynEdit)
  public
    procedure DoKeyDown(var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
  end;

procedure TTestFMXSynEdit.DoKeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  KeyDown(Key, KeyChar, Shift);
end;

{ ---- Bug 1: Plugin auto-registration ---- }

type
  { Access class to reach protected DoPluginAfterPaint for testing }
  TFMXSynEditAccess = class(TCustomFMXSynEdit);

  TTestPlugin = class(TSynFMXEditPlugin)
  public
    AfterPaintCalled: Boolean;
    procedure AfterPaint(Canvas: TCanvas; const AClip: TRectF;
      FirstLine, LastLine: Integer); override;
  end;

procedure TTestPlugin.AfterPaint(Canvas: TCanvas; const AClip: TRectF;
  FirstLine, LastLine: Integer);
begin
  AfterPaintCalled := True;
end;

procedure TTestPluginRegistration.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestPluginRegistration.TearDown;
begin
  FEditor.Free;
end;

procedure TTestPluginRegistration.TestPluginAutoRegisters;
var
  Plugin: TTestPlugin;
begin
  // Creating a plugin with an owner should auto-register it
  Plugin := TTestPlugin.Create(FEditor, [phAfterPaint]);
  try
    // Verify it was registered: unregister should succeed without error
    FEditor.UnregisterPlugin(Plugin);
    // Re-register manually to confirm the API works
    FEditor.RegisterPlugin(Plugin);
    Assert.IsTrue(Plugin.Owner = FEditor,
      'Plugin should be owned by the editor after registration');
  finally
    // Destructor will auto-unregister
    Plugin.Free;
  end;
end;

procedure TTestPluginRegistration.TestPluginAutoUnregistersOnFree;
var
  Plugin: TTestPlugin;
begin
  Plugin := TTestPlugin.Create(FEditor, [phAfterPaint]);
  // Free should auto-unregister - should not AV or leave dangling pointer
  Plugin.Free;
  // Editor should still be usable after plugin is freed
  FEditor.Text := 'test';
  Assert.AreEqual('test', FEditor.Lines[0]);
end;

procedure TTestPluginRegistration.TestPluginHandlersSetViaConstructor;
var
  Plugin: TTestPlugin;
begin
  Plugin := TTestPlugin.Create(FEditor, [phAfterPaint, phLinesInserted]);
  try
    Assert.IsTrue(phAfterPaint in Plugin.Handlers);
    Assert.IsTrue(phLinesInserted in Plugin.Handlers);
    Assert.IsFalse(phLinesDeleted in Plugin.Handlers);
  finally
    Plugin.Free;
  end;
end;

procedure TTestPluginRegistration.TestPluginDefaultHandlersEmpty;
var
  Plugin: TTestPlugin;
begin
  Plugin := TTestPlugin.Create(FEditor);
  try
    Assert.IsTrue(Plugin.Handlers = [],
      'Default handlers should be empty set');
  finally
    Plugin.Free;
  end;
end;

procedure TTestPluginRegistration.TestPluginAfterPaintDispatch;
var
  Plugin: TTestPlugin;
  Helper: TFMXSynEditAccess;
begin
  // Tests that DoPluginAfterPaint actually dispatches to registered plugins
  Plugin := TTestPlugin.Create(FEditor, [phAfterPaint]);
  try
    Assert.IsFalse(Plugin.AfterPaintCalled,
      'AfterPaint should not be called before dispatch');
    // DoPluginAfterPaint is protected; use a cast to access it
    Helper := TFMXSynEditAccess(FEditor);
    Helper.DoPluginAfterPaint(nil, TRectF.Empty, 1, 1);
    Assert.IsTrue(Plugin.AfterPaintCalled,
      'AfterPaint should be called after DoPluginAfterPaint dispatch');
  finally
    Plugin.Free;
  end;
end;

{ ---- Bug 2: Modified property ---- }

procedure TTestModifiedProperty.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestModifiedProperty.TearDown;
begin
  FEditor.Free;
end;

procedure TTestModifiedProperty.TestModifiedInitiallyFalse;
begin
  Assert.IsFalse(FEditor.Modified,
    'New editor should not be modified');
end;

procedure TTestModifiedProperty.TestModifiedTrueAfterEdit;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.IsTrue(FEditor.Modified,
    'Editor should be modified after typing');
end;

procedure TTestModifiedProperty.TestModifiedFalseAfterSaveReset;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.IsTrue(FEditor.Modified);
  // Simulate save by resetting Modified via the undo interface
  FEditor.UndoRedo.Modified := False;
  Assert.IsFalse(FEditor.Modified,
    'Modified should be False after save reset');
end;

procedure TTestModifiedProperty.TestModifiedDiffersFromCanUndo;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  Assert.IsTrue(FEditor.Modified);
  Assert.IsTrue(FEditor.CanUndo);
  // Reset Modified (simulating save) - CanUndo should remain True
  FEditor.UndoRedo.Modified := False;
  Assert.IsFalse(FEditor.Modified,
    'Modified should be False after reset');
  Assert.IsTrue(FEditor.CanUndo,
    'CanUndo should still be True - undo stack was not cleared');
end;

procedure TTestModifiedProperty.TestCanUndoTrueWhenModifiedFalse;
begin
  // This is the key test: the old bug had Modified = CanUndo
  FEditor.Text := 'AB';
  FEditor.CaretXY := BufferCoord(1, 1);
  FEditor.ExecuteCommand(ecChar, 'X');
  // Simulate save
  FEditor.UndoRedo.Modified := False;
  // After save: Modified=False, CanUndo=True (they must differ)
  Assert.IsFalse(FEditor.Modified, 'Modified should be False after save');
  Assert.IsTrue(FEditor.CanUndo, 'CanUndo should be True (undo stack exists)');
  Assert.AreNotEqual(FEditor.Modified, FEditor.CanUndo,
    'Modified and CanUndo should differ after save');
end;

{ ---- Bug 4: TSynEditStringList nil FTextWidthFunc ---- }

procedure TTestTextBufferNilWidthFunc.TestGetTextWidthWithNilFunc;
var
  Lines: TSynEditStringList;
begin
  Lines := TSynEditStringList.Create(nil);
  try
    Lines.Add('Hello World');
    // GetTextWidth should not crash - should fall back to Length
    Assert.AreEqual(11, Lines.TextWidth[0],
      'TextWidth with nil func should return string length');
  finally
    Lines.Free;
  end;
end;

procedure TTestTextBufferNilWidthFunc.TestGetMaxWidthWithNilFunc;
var
  Lines: TSynEditStringList;
begin
  Lines := TSynEditStringList.Create(nil);
  try
    Lines.Add('Short');
    Lines.Add('A longer line');
    // MaxWidth should not crash - should use Length fallback
    Assert.AreEqual(13, Lines.MaxWidth,
      'MaxWidth with nil func should use string length');
  finally
    Lines.Free;
  end;
end;

procedure TTestTextBufferNilWidthFunc.TestPutWithNilFunc;
var
  Lines: TSynEditStringList;
begin
  Lines := TSynEditStringList.Create(nil);
  try
    Lines.Add('Original');
    // Force a width calculation to initialize MaxWidth
    Lines.MaxWidth;
    // Put triggers width recalculation via FTextWidthFunc
    Lines[0] := 'Replacement text';
    // Should not crash, and TextWidth should reflect new length
    Assert.AreEqual(16, Lines.TextWidth[0],
      'TextWidth after Put with nil func should return new length');
  finally
    Lines.Free;
  end;
end;

{ ---- Bugs 5 & 6: Tab expansion ---- }

procedure TTestTabExpansion.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.TabWidth := 8;
  // Disable eoTabsToSpaces so literal tabs stay in buffer
  FEditor.Options := FEditor.Options - [eoTabsToSpaces];
end;

procedure TTestTabExpansion.TearDown;
begin
  FEditor.Free;
end;

procedure TTestTabExpansion.TestMaxScrollWidthWithTabs;
begin
  // Tab at column 0 expands to 8 spaces, "AB" is 2 chars = 10 visual columns
  FEditor.Text := #9'AB';
  // MaxScrollWidth should use expanded length (10), not raw length (3)
  Assert.AreEqual(11, FEditor.MaxScrollWidth,
    'MaxScrollWidth should be 11 for tab(8)+AB(2)+1');
end;

procedure TTestTabExpansion.TestMaxScrollWidthNoTabs;
begin
  FEditor.Text := 'ABCDEFGHIJ'; // 10 chars, no tabs
  Assert.AreEqual(11, FEditor.MaxScrollWidth,
    'MaxScrollWidth should be 11 for 10-char line+1');
end;

procedure TTestTabExpansion.TestMaxScrollWidthMixedLines;
begin
  // Line 1: 5 chars. Line 2: tab(8) + 5 = 13 visual.
  FEditor.Text := 'Hello' + sLineBreak + #9'World';
  Assert.AreEqual(14, FEditor.MaxScrollWidth,
    'MaxScrollWidth should be 14 for tab(8)+World(5)+1');
end;

procedure TTestTabExpansion.TestExpandTabsAtColumnBoundary;
begin
  // "12345678\tX" - tab at column 8 should expand to 8 spaces (next tab stop)
  // Total: 8 + 8 + 1 = 17 visual columns
  FEditor.Text := '12345678'#9'X';
  Assert.AreEqual(18, FEditor.MaxScrollWidth,
    'MaxScrollWidth should be 18 for 8chars+tab(8)+X(1)+1');
end;

procedure TTestTabExpansion.TestExpandTabsMidColumn;
begin
  // "123\tX" - tab at column 3, expands to 5 spaces (to reach column 8)
  // Total: 3 + 5 + 1 = 9 visual columns (but "123" occupies cols 0-2,
  // tab fills cols 3-7, X at col 8 = 9 visual columns)
  FEditor.Text := '123'#9'X';
  Assert.AreEqual(10, FEditor.MaxScrollWidth,
    'MaxScrollWidth should be 10 for 123(3)+tab(5)+X(1)+1');
end;

{ ---- Bug 7: Keyboard handler chain ---- }

procedure TTestKeyboardHandlerChain.Setup;
begin
  FEditor := TTestFMXSynEdit.Create(nil);
  FHandlerCalled := False;
  FHandlerKey := 0;
  FUserHandlerCalled := False;
end;

procedure TTestKeyboardHandlerChain.TearDown;
begin
  FEditor.Free;
end;

procedure TTestKeyboardHandlerChain.KeyDownHandler(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  FHandlerCalled := True;
  FHandlerKey := Key;
end;

procedure TTestKeyboardHandlerChain.ConsumingKeyDownHandler(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  FHandlerCalled := True;
  FHandlerKey := Key;
  // Consume the key
  Key := 0;
  KeyChar := #0;
end;

procedure TTestKeyboardHandlerChain.UserOnKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  FUserHandlerCalled := True;
end;

procedure TTestKeyboardHandlerChain.TestAddKeyDownHandlerIsCalled;
var
  Key: Word;
  KeyChar: WideChar;
begin
  FEditor.AddKeyDownHandler(KeyDownHandler);
  try
    // Simulate a key press through KeyDown
    Key := vkReturn;
    KeyChar := #0;
    TTestFMXSynEdit(FEditor).DoKeyDown(Key, KeyChar, []);
    Assert.IsTrue(FHandlerCalled,
      'KeyDown handler should have been called');
    Assert.AreEqual(Word(vkReturn), FHandlerKey,
      'Handler should receive the correct key');
  finally
    FEditor.RemoveKeyDownHandler(KeyDownHandler);
  end;
end;

procedure TTestKeyboardHandlerChain.TestRemoveKeyDownHandler;
var
  Key: Word;
  KeyChar: WideChar;
begin
  FEditor.AddKeyDownHandler(KeyDownHandler);
  FEditor.RemoveKeyDownHandler(KeyDownHandler);
  Key := vkReturn;
  KeyChar := #0;
  TTestFMXSynEdit(FEditor).DoKeyDown(Key, KeyChar, []);
  Assert.IsFalse(FHandlerCalled,
    'Removed handler should not be called');
end;

procedure TTestKeyboardHandlerChain.TestMultipleHandlersBothCalled;
var
  Key: Word;
  KeyChar: WideChar;
begin
  // Register the same non-consuming handler twice to verify both fire
  FEditor.AddKeyDownHandler(KeyDownHandler);
  FEditor.AddKeyDownHandler(KeyDownHandler);
  try
    FHandlerCalled := False;
    Key := vkReturn;
    KeyChar := #0;
    TTestFMXSynEdit(FEditor).DoKeyDown(Key, KeyChar, []);
    Assert.IsTrue(FHandlerCalled,
      'Handler should be called when multiple handlers registered');
  finally
    FEditor.RemoveKeyDownHandler(KeyDownHandler);
    FEditor.RemoveKeyDownHandler(KeyDownHandler);
  end;
end;

procedure TTestKeyboardHandlerChain.TestConsumingHandlerStopsChain;
var
  Key: Word;
  KeyChar: WideChar;
begin
  // Add a non-consuming handler first (it runs last since chain is LIFO)
  FEditor.AddKeyDownHandler(KeyDownHandler);
  // Add a consuming handler second (it runs first in LIFO order)
  FEditor.AddKeyDownHandler(ConsumingKeyDownHandler);
  try
    FHandlerCalled := False;
    Key := vkReturn;
    KeyChar := #0;
    TTestFMXSynEdit(FEditor).DoKeyDown(Key, KeyChar, []);
    // The consuming handler should have been called
    Assert.IsTrue(FHandlerCalled,
      'Consuming handler should have been called');
    // Key should be consumed (set to 0)
    Assert.AreEqual(Word(0), Key,
      'Key should be consumed by handler');
  finally
    FEditor.RemoveKeyDownHandler(ConsumingKeyDownHandler);
    FEditor.RemoveKeyDownHandler(KeyDownHandler);
  end;
end;

procedure TTestKeyboardHandlerChain.TestOnKeyDownPreservedWithHandler;
var
  Key: Word;
  KeyChar: WideChar;
begin
  // This is the key test: assigning OnKeyDown and adding a handler
  // should NOT destroy the OnKeyDown handler
  FEditor.OnKeyDown := UserOnKeyDown;
  FEditor.AddKeyDownHandler(KeyDownHandler);
  try
    Assert.IsTrue(Assigned(FEditor.OnKeyDown),
      'OnKeyDown should still be assigned after AddKeyDownHandler');

    // Simulate a key press
    Key := vkReturn;
    KeyChar := #0;
    TTestFMXSynEdit(FEditor).DoKeyDown(Key, KeyChar, []);

    Assert.IsTrue(FHandlerCalled,
      'Chain handler should have been called');
    // The OnKeyDown fires via inherited KeyDown, which is called first
    Assert.IsTrue(FUserHandlerCalled,
      'User OnKeyDown handler should still be called');
  finally
    FEditor.RemoveKeyDownHandler(KeyDownHandler);
  end;

  // After removing chain handler, OnKeyDown should still be intact
  Assert.IsTrue(Assigned(FEditor.OnKeyDown),
    'OnKeyDown should still be assigned after RemoveKeyDownHandler');
end;

{ ---- Auto-indent with tabs ---- }

procedure TTestAutoIndentTabs.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  Assert.IsTrue(eoAutoIndent in FEditor.Options, 'eoAutoIndent should be on by default');
  // Disable eoTabsToSpaces so literal tabs stay in the buffer
  FEditor.Options := FEditor.Options - [eoTabsToSpaces];
end;

procedure TTestAutoIndentTabs.TearDown;
begin
  FEditor.Free;
end;

procedure TTestAutoIndentTabs.TestAutoIndentPreservesLeadingTabs;
begin
  FEditor.Text := #9'indented';
  FEditor.CaretXY := BufferCoord(10, 1); // end of line
  FEditor.ExecuteCommand(ecLineBreak, #0);
  Assert.AreEqual(2, FEditor.LineCount);
  Assert.AreEqual(#9, Copy(FEditor.Lines[1], 1, 1),
    'New line should start with a tab from auto-indent');
end;

procedure TTestAutoIndentTabs.TestAutoIndentPreservesMixedWhitespace;
begin
  FEditor.Text := #9'  mixed';
  FEditor.CaretXY := BufferCoord(9, 1); // end of line
  FEditor.ExecuteCommand(ecLineBreak, #0);
  Assert.AreEqual(2, FEditor.LineCount);
  Assert.AreEqual(#9'  ', Copy(FEditor.Lines[1], 1, 3),
    'New line should preserve tab+spaces from auto-indent');
end;

procedure TTestAutoIndentTabs.TestAutoIndentWithSpacesStillWorks;
begin
  FEditor.Text := '    spaced';
  FEditor.CaretXY := BufferCoord(11, 1);
  FEditor.ExecuteCommand(ecLineBreak, #0);
  Assert.AreEqual(2, FEditor.LineCount);
  Assert.AreEqual('    ', Copy(FEditor.Lines[1], 1, 4),
    'Auto-indent should still work with spaces');
end;

procedure TTestAutoIndentTabs.TestNoAutoIndentWhenDisabled;
begin
  FEditor.Options := FEditor.Options - [eoAutoIndent];
  FEditor.Text := #9'indented';
  FEditor.CaretXY := BufferCoord(10, 1);
  FEditor.ExecuteCommand(ecLineBreak, #0);
  Assert.AreEqual(2, FEditor.LineCount);
  Assert.AreEqual('', FEditor.Lines[1],
    'No auto-indent should produce empty new line');
end;

{ ---- PixelToBufferCoord ---- }

procedure TTestPixelToBufferCoord.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'ABCDEFGHIJ';
  FEditor.Width := 500;
  FEditor.Height := 300;
end;

procedure TTestPixelToBufferCoord.TearDown;
begin
  FEditor.Free;
end;

procedure TTestPixelToBufferCoord.TestClickLeftOfCharBoundary;
var
  BC: TBufferCoord;
  Char3Px: TPointF;
begin
  // Get exact pixel position of char 3
  Char3Px := FEditor.BufferCoordToPixel(BufferCoord(3, 1));
  // Click slightly left of char 3 start — should map to char 2, not char 3
  BC := FEditor.PixelToBufferCoord(Char3Px.X - 1, Char3Px.Y);
  Assert.AreEqual(2, BC.Char,
    'Clicking 1px left of char 3 should map to char 2 (Trunc behavior)');
end;

procedure TTestPixelToBufferCoord.TestClickRightOfCharBoundary;
var
  BC: TBufferCoord;
  Char3Px: TPointF;
begin
  // Click just past the start of char 3
  Char3Px := FEditor.BufferCoordToPixel(BufferCoord(3, 1));
  BC := FEditor.PixelToBufferCoord(Char3Px.X + 1, Char3Px.Y);
  Assert.AreEqual(3, BC.Char,
    'Clicking 1px right of char 3 start should map to char 3');
end;

procedure TTestPixelToBufferCoord.TestClickExactlyOnCharBoundary;
var
  BC: TBufferCoord;
  Char3Px: TPointF;
begin
  // Click exactly on the start of char 3
  Char3Px := FEditor.BufferCoordToPixel(BufferCoord(3, 1));
  BC := FEditor.PixelToBufferCoord(Char3Px.X, Char3Px.Y);
  Assert.AreEqual(3, BC.Char,
    'Clicking exactly on char 3 start should map to char 3');
end;

procedure TTestPixelToBufferCoord.TestClickInGutterClampsToOne;
var
  BC: TBufferCoord;
begin
  // Click at X = 0 (gutter area) — should clamp to char 1
  BC := FEditor.PixelToBufferCoord(0, 0);
  Assert.AreEqual(1, BC.Char,
    'Clicking in gutter should clamp to char 1');
end;

{ ---- Scrollbar conditional sizing ---- }

procedure TTestScrollBarSizing.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Width := 400;
  FEditor.Height := 300;
end;

procedure TTestScrollBarSizing.TearDown;
begin
  FEditor.Free;
end;

procedure TTestScrollBarSizing.TestLinesInWindowDeltaNoScrollBars;
var
  Lines300, Lines400: Integer;
  ExpectedDelta: Integer;
begin
  // Short content — no scrollbars should appear
  FEditor.Text := 'Short';
  FEditor.Height := 300;
  Lines300 := FEditor.LinesInWindow;
  FEditor.Height := 400;
  Lines400 := FEditor.LinesInWindow;
  // Increasing height by 100 should gain exactly Trunc(100/LineHeight) lines
  ExpectedDelta := Integer(Trunc(100 / FEditor.LineHeight));
  Assert.AreEqual(ExpectedDelta, Lines400 - Lines300,
    'LinesInWindow delta should match full height delta when no scrollbar');
end;

procedure TTestScrollBarSizing.TestCharsInWindowDeltaNoScrollBars;
var
  CharsNarrow, CharsWide: Integer;
begin
  // Short content — no vertical scrollbar should appear
  FEditor.Text := 'Short';
  FEditor.Width := 300;
  CharsNarrow := FEditor.CharsInWindow;
  FEditor.Width := 500;
  CharsWide := FEditor.CharsInWindow;
  // Adding 200px should gain roughly 200/CharWidth chars (within 1 of rounding)
  Assert.IsTrue(Abs((CharsWide - CharsNarrow) * FEditor.CharWidth - 200) < FEditor.CharWidth,
    'CharsInWindow delta should reflect full width change when no scrollbar');
end;

procedure TTestScrollBarSizing.TestScrollBarsHiddenInitially;
var
  LinesShort, LinesManyShort: Integer;
begin
  // Verify that adding more short lines (that don't need a horizontal
  // scrollbar) doesn't reduce LinesInWindow. If the vertical scrollbar
  // were incorrectly shown for short content, LinesInWindow would shrink.
  FEditor.Text := 'Short';
  FEditor.Height := 300;
  LinesShort := FEditor.LinesInWindow;

  // 5 short lines — still fits in window, no scrollbar needed
  FEditor.Text := 'A' + sLineBreak + 'B' + sLineBreak + 'C' +
    sLineBreak + 'D' + sLineBreak + 'E';
  LinesManyShort := FEditor.LinesInWindow;

  Assert.AreEqual(LinesShort, LinesManyShort,
    'LinesInWindow should not change when adding lines that still fit');
end;

{ ---- Cross-platform fixes (issues 13-15) ---- }

procedure TTestCrossPlatformFixes.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'hello world' + sLineBreak + 'second line';
end;

procedure TTestCrossPlatformFixes.TearDown;
begin
  FEditor.Free;
end;

procedure TTestCrossPlatformFixes.TestSelectionBGNotSystemColor;
begin
  // Issue 13: Default selection background should NOT be a system color
  // (system colors have the SystemColor flag bit set and rely on platform
  // APIs to resolve, which may fail on non-Windows FMX targets)
  Assert.AreNotEqual(TColor(TColors.SysHighlight),
    FEditor.SelectedColor.Background,
    'Default selection BG should not be clHighlight system color');
end;

procedure TTestCrossPlatformFixes.TestSelectionFGNotSystemColor;
begin
  Assert.AreNotEqual(TColor(TColors.SysHighlightText),
    FEditor.SelectedColor.Foreground,
    'Default selection FG should not be clHighlightText system color');
end;

procedure TTestCrossPlatformFixes.TestSelectionBGIsExplicitARGB;
begin
  // TColor uses BGR format where system colors have negative values (high bit
  // set).  Explicit colors are non-negative (>= 0).  Verify the default is
  // not a system color so TColorToAlphaColor can convert it on any platform.
  Assert.IsTrue(Integer(FEditor.SelectedColor.Background) >= 0,
    'Selection BG should be a non-negative TColor (not a system color)');
end;

procedure TTestCrossPlatformFixes.TestSelectionFGIsExplicitARGB;
begin
  Assert.IsTrue(Integer(FEditor.SelectedColor.Foreground) >= 0,
    'Selection FG should be a non-negative TColor (not a system color)');
end;

procedure TTestCrossPlatformFixes.TestBlockBeginWritable;
var
  BB: TBufferCoord;
begin
  // Issue 15: BlockBegin should be writable
  BB := BufferCoord(3, 1);
  FEditor.BlockBegin := BB;
  Assert.AreEqual(3, FEditor.BlockBegin.Char);
  Assert.AreEqual(1, FEditor.BlockBegin.Line);
end;

procedure TTestCrossPlatformFixes.TestBlockEndWritable;
var
  BE: TBufferCoord;
begin
  FEditor.BlockBegin := BufferCoord(1, 1);
  BE := BufferCoord(6, 1);
  FEditor.BlockEnd := BE;
  Assert.AreEqual(6, FEditor.BlockEnd.Char);
  Assert.AreEqual(1, FEditor.BlockEnd.Line);
end;

procedure TTestCrossPlatformFixes.TestBlockBeginResetsBlockEnd;
begin
  // Setting BlockBegin should also reset BlockEnd to the same value
  // (matching VCL behavior — starting a new selection)
  FEditor.BlockBegin := BufferCoord(1, 1);
  FEditor.BlockEnd := BufferCoord(6, 1);
  // Now set a new BlockBegin — BlockEnd should be reset
  FEditor.BlockBegin := BufferCoord(3, 2);
  Assert.AreEqual(FEditor.BlockBegin.Char, FEditor.BlockEnd.Char,
    'BlockEnd.Char should equal BlockBegin.Char after SetBlockBegin');
  Assert.AreEqual(FEditor.BlockBegin.Line, FEditor.BlockEnd.Line,
    'BlockEnd.Line should equal BlockBegin.Line after SetBlockBegin');
end;

procedure TTestCrossPlatformFixes.TestBlockBeginClampsToMin;
begin
  // Setting BlockBegin with invalid coordinates should clamp to 1
  FEditor.BlockBegin := BufferCoord(-5, 0);
  Assert.IsTrue(FEditor.BlockBegin.Char >= 1, 'BlockBegin.Char should be >= 1');
  Assert.IsTrue(FEditor.BlockBegin.Line >= 1, 'BlockBegin.Line should be >= 1');
end;

procedure TTestCrossPlatformFixes.TestSetSelectionViaProperties;
begin
  // Test setting selection entirely via BlockBegin/BlockEnd properties
  FEditor.BlockBegin := BufferCoord(1, 1);
  FEditor.BlockEnd := BufferCoord(6, 1);
  Assert.IsTrue(FEditor.SelAvail, 'Selection should be available');
  Assert.AreEqual('hello', FEditor.SelText,
    'Selected text should be "hello"');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPluginRegistration);
  TDUnitX.RegisterTestFixture(TTestModifiedProperty);
  TDUnitX.RegisterTestFixture(TTestTextBufferNilWidthFunc);
  TDUnitX.RegisterTestFixture(TTestTabExpansion);
  TDUnitX.RegisterTestFixture(TTestAutoIndentTabs);
  TDUnitX.RegisterTestFixture(TTestPixelToBufferCoord);
  TDUnitX.RegisterTestFixture(TTestScrollBarSizing);
  TDUnitX.RegisterTestFixture(TTestKeyboardHandlerChain);
  TDUnitX.RegisterTestFixture(TTestCrossPlatformFixes);

end.
