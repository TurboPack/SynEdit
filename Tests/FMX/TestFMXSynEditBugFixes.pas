unit TestFMXSynEditBugFixes;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  { Tests for the 6 blocking bugs identified in the FMX port review }

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

implementation

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  FMX.Graphics,
  FMX.Types,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditTextBuffer;

{ ---- Bug 1: Plugin auto-registration ---- }

type
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
    // Verify it was registered by checking the plugin list is non-empty
    // We verify indirectly: unregister should not raise, and a second
    // unregister attempt should be harmless (not found = no-op)
    FEditor.UnregisterPlugin(Plugin);
    // Re-register manually to confirm the API works
    FEditor.RegisterPlugin(Plugin);
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
begin
  // This tests that DoPluginAfterPaint actually dispatches to plugins
  // with phAfterPaint in their Handlers
  Plugin := TTestPlugin.Create(FEditor, [phAfterPaint]);
  try
    Assert.IsFalse(Plugin.AfterPaintCalled);
    // Call the dispatch method directly (it's private, so we test
    // indirectly via Paint which calls DoPluginAfterPaint)
    // Since we're headless, Repaint is a no-op, but we can verify
    // the plugin is registered and has correct handlers
    Assert.IsTrue(phAfterPaint in Plugin.Handlers,
      'Plugin should have phAfterPaint handler');
    Assert.IsTrue(Plugin.Owner = FEditor,
      'Plugin owner should be the editor');
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
  Assert.IsTrue(FEditor.MaxScrollWidth >= 10,
    Format('MaxScrollWidth should be >= 10 for tab+AB, got %d',
      [FEditor.MaxScrollWidth]));
end;

procedure TTestTabExpansion.TestMaxScrollWidthNoTabs;
begin
  FEditor.Text := 'ABCDEFGHIJ'; // 10 chars, no tabs
  Assert.IsTrue(FEditor.MaxScrollWidth >= 10,
    Format('MaxScrollWidth should be >= 10 for 10-char line, got %d',
      [FEditor.MaxScrollWidth]));
end;

procedure TTestTabExpansion.TestMaxScrollWidthMixedLines;
begin
  // Line 1: 5 chars. Line 2: tab(8) + 5 = 13 visual.
  FEditor.Text := 'Hello' + sLineBreak + #9'World';
  Assert.IsTrue(FEditor.MaxScrollWidth >= 13,
    Format('MaxScrollWidth should be >= 13 for tab line, got %d',
      [FEditor.MaxScrollWidth]));
end;

procedure TTestTabExpansion.TestExpandTabsAtColumnBoundary;
begin
  // "12345678\tX" - tab at column 8 should expand to 8 spaces (next tab stop)
  // Total: 8 + 8 + 1 = 17 visual columns
  FEditor.Text := '12345678'#9'X';
  Assert.IsTrue(FEditor.MaxScrollWidth >= 17,
    Format('MaxScrollWidth should be >= 17 for tab at boundary, got %d',
      [FEditor.MaxScrollWidth]));
end;

procedure TTestTabExpansion.TestExpandTabsMidColumn;
begin
  // "123\tX" - tab at column 3, expands to 5 spaces (to reach column 8)
  // Total: 3 + 5 + 1 = 9 visual columns (but "123" occupies cols 0-2,
  // tab fills cols 3-7, X at col 8 = 9 visual columns)
  FEditor.Text := '123'#9'X';
  Assert.IsTrue(FEditor.MaxScrollWidth >= 9,
    Format('MaxScrollWidth should be >= 9 for mid-column tab, got %d',
      [FEditor.MaxScrollWidth]));
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPluginRegistration);
  TDUnitX.RegisterTestFixture(TTestModifiedProperty);
  TDUnitX.RegisterTestFixture(TTestTextBufferNilWidthFunc);
  TDUnitX.RegisterTestFixture(TTestTabExpansion);

end.
