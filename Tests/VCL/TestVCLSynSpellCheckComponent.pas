unit TestVCLSynSpellCheckComponent;

interface

uses
  DUnitX.TestFramework,
  SynEdit,
  SynEditTypes,
  SynEditMiscClasses,
  Vcl.SynSpellCheck,
  SynSpellCheckTypes;

type
  [TestFixture]
  TTestVCLSynSpellCheckComponent = class
  private
    FEditor: TSynEdit;
    FSpellCheck: TSynSpellCheck;
    FProvider: ISynSpellCheckProvider;
    FEventFired: Boolean;
    procedure OnChangeFired(Sender: TObject);
    function GetErrorCount: Integer;
    function HasErrorAt(Line, Char: Integer): Boolean;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    { Defaults }
    [Test]
    procedure TestDefaults;
    { Singleton }
    [Test]
    procedure TestSingleton;
    { Editor assignment }
    [Test]
    procedure TestSetEditor;
    [Test]
    procedure TestSetEditorNil;
    { CheckFile }
    [Test]
    procedure TestCheckFileFindsErrors;
    [Test]
    procedure TestCheckFileNoErrors;
    [Test]
    procedure TestCheckFileNoProvider;
    { CheckLine }
    [Test]
    procedure TestCheckLineSpecificLine;
    [Test]
    procedure TestCheckLineClearsOldErrors;
    { ClearErrors }
    [Test]
    procedure TestClearErrors;
    { IndicatorAtPos }
    [Test]
    procedure TestIndicatorAtPosFound;
    [Test]
    procedure TestIndicatorAtPosNotFound;
    [Test]
    procedure TestIndicatorAtPosWrongLine;
    { Provider }
    [Test]
    procedure TestProviderProperty;
    { Events }
    [Test]
    procedure TestOnChangeEvent;
    { Multi-editor }
    [Test]
    procedure TestMultiEditor;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  TMockSpellProvider = class(TInterfacedObject, ISynSpellCheckProvider)
  private
    FValidWords: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;
    function CheckWord(const AWord: string): Boolean;
    function Suggest(const AWord: string): TArray<string>;
    function IsAvailable: Boolean;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
  end;

{ TMockSpellProvider }

constructor TMockSpellProvider.Create;
begin
  inherited Create;
  FValidWords := TList<string>.Create;
  FValidWords.Add('hello');
  FValidWords.Add('world');
  FValidWords.Add('the');
  FValidWords.Add('quick');
  FValidWords.Add('brown');
  FValidWords.Add('fox');
end;

destructor TMockSpellProvider.Destroy;
begin
  FValidWords.Free;
  inherited;
end;

function TMockSpellProvider.CheckWord(const AWord: string): Boolean;
var
  S: string;
begin
  for S in FValidWords do
    if SameText(S, AWord) then
      Exit(True);
  Result := False;
end;

function TMockSpellProvider.Suggest(const AWord: string): TArray<string>;
begin
  Result := TArray<string>.Create('hello');
end;

function TMockSpellProvider.IsAvailable: Boolean;
begin
  Result := True;
end;

function TMockSpellProvider.GetLanguage: string;
begin
  Result := 'mock';
end;

procedure TMockSpellProvider.SetLanguage(const Value: string);
begin
  // no-op
end;

{ TTestVCLSynSpellCheckComponent }

function TTestVCLSynSpellCheckComponent.GetErrorCount: Integer;
begin
  Result := Length(FEditor.Indicators.GetById(TSynSpellCheck.SpellErrorIndicatorId));
end;

function TTestVCLSynSpellCheckComponent.HasErrorAt(Line, Char: Integer): Boolean;
var
  Ind: TSynIndicator;
begin
  Result := FEditor.Indicators.IndicatorAtPos(
    BufferCoord(Char, Line), TSynSpellCheck.SpellErrorIndicatorId, Ind);
end;

procedure TTestVCLSynSpellCheckComponent.OnChangeFired(Sender: TObject);
begin
  FEventFired := True;
end;

procedure TTestVCLSynSpellCheckComponent.Setup;
begin
  FSpellCheck := TSynSpellCheck.Create(nil);
  FEditor := TSynEdit.Create(nil);
  FProvider := TMockSpellProvider.Create;
  FSpellCheck.Provider := FProvider;
  FSpellCheck.Editor := FEditor;
end;

procedure TTestVCLSynSpellCheckComponent.TearDown;
begin
  FSpellCheck.Free;
  FEditor.Free;
  FProvider := nil;
end;

{ Defaults }

procedure TTestVCLSynSpellCheckComponent.TestDefaults;
begin
  Assert.AreEqual(Integer(clRed), Integer(FSpellCheck.PenColor),
    'PenColor should default to clRed');
  Assert.AreEqual(Integer(usMicrosoftWord), Integer(FSpellCheck.UnderlineStyle),
    'UnderlineStyle should default to usMicrosoftWord');
  Assert.IsTrue(FSpellCheck.AttributesChecked.IndexOf('Comment') >= 0,
    'Comment should be in AttributesChecked');
  Assert.IsTrue(FSpellCheck.AttributesChecked.IndexOf('Text') >= 0,
    'Text should be in AttributesChecked');
  Assert.IsTrue(FSpellCheck.AttributesChecked.IndexOf('String') >= 0,
    'String should be in AttributesChecked');
  Assert.IsTrue(FSpellCheck.AttributesChecked.IndexOf('Documentation') >= 0,
    'Documentation should be in AttributesChecked');
  Assert.IsTrue(TSynSpellCheck.GlobalInstance = FSpellCheck,
    'GlobalInstance should be assigned');
end;

{ Singleton }

procedure TTestVCLSynSpellCheckComponent.TestSingleton;
begin
  Assert.WillRaise(
    procedure
    var
      SC2: TSynSpellCheck;
    begin
      SC2 := TSynSpellCheck.Create(nil);
      SC2.Free;
    end,
    ESynError,
    'Creating a second TSynSpellCheck instance should raise ESynError');
end;

{ Editor assignment }

procedure TTestVCLSynSpellCheckComponent.TestSetEditor;
begin
  Assert.IsNotNull(FSpellCheck.Editor, 'Editor should be assigned');
end;

procedure TTestVCLSynSpellCheckComponent.TestSetEditorNil;
begin
  FSpellCheck.Editor := nil;
  Assert.IsNull(FSpellCheck.Editor, 'Editor should be nil after clearing');
end;

{ CheckFile }

procedure TTestVCLSynSpellCheckComponent.TestCheckFileFindsErrors;
begin
  // "hello" is valid, "wrold" is not, "the" is valid, "quik" is not
  FEditor.Text := 'hello wrold the quik';
  FSpellCheck.CheckFile;
  Assert.AreEqual(2, GetErrorCount,
    'Should find 2 misspelled words');
end;

procedure TTestVCLSynSpellCheckComponent.TestCheckFileNoErrors;
begin
  FEditor.Text := 'hello world the';
  FSpellCheck.CheckFile;
  Assert.AreEqual(0, GetErrorCount,
    'All valid words should produce no errors');
end;

procedure TTestVCLSynSpellCheckComponent.TestCheckFileNoProvider;
begin
  FSpellCheck.Provider := nil;
  FEditor.Text := 'hello wrold';
  // On Windows 10+, COM SpellChecker is available even without a provider,
  // so errors may be found via the Windows API. Just verify it doesn't crash.
  FSpellCheck.CheckFile;
  Assert.Pass('CheckFile with no provider should not crash');
end;

{ CheckLine }

procedure TTestVCLSynSpellCheckComponent.TestCheckLineSpecificLine;
begin
  FEditor.Lines.Clear;
  FEditor.Lines.Add('hello world');
  FEditor.Lines.Add('baaad wrold');
  FEditor.Lines.Add('the fox');
  // Set caret to line 2 — VCL CheckLine uses FEditor.CaretY
  FEditor.CaretY := 2;
  FSpellCheck.CheckLine;
  // Should find errors on line 2
  Assert.IsTrue(HasErrorAt(2, 1), 'Should find error at line 2 char 1 ("baaad")');
end;

procedure TTestVCLSynSpellCheckComponent.TestCheckLineClearsOldErrors;
begin
  FEditor.Lines.Clear;
  FEditor.Lines.Add('baaad');
  FEditor.Lines.Add('hello');
  // Check line 1
  FEditor.CaretY := 1;
  FSpellCheck.CheckLine;
  Assert.IsTrue(GetErrorCount >= 1, 'Should have error on line 1');
  // Fix line 1 and re-check
  FEditor.Lines[0] := 'hello';
  FEditor.CaretY := 1;
  FSpellCheck.CheckLine;
  Assert.AreEqual(0, GetErrorCount,
    'Re-checking line 1 with valid text should clear its errors');
end;

{ ClearErrors }

procedure TTestVCLSynSpellCheckComponent.TestClearErrors;
begin
  FEditor.Text := 'wrold baaad';
  FSpellCheck.CheckFile;
  Assert.IsTrue(GetErrorCount > 0, 'Should have errors');
  FSpellCheck.ClearErrors(False);
  Assert.AreEqual(0, GetErrorCount,
    'ClearErrors should remove all error indicators');
end;

{ IndicatorAtPos }

procedure TTestVCLSynSpellCheckComponent.TestIndicatorAtPosFound;
begin
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckFile;
  // "wrold" starts at char 7 on line 1
  Assert.IsTrue(HasErrorAt(1, 7),
    'Should find indicator at start of "wrold"');
  Assert.IsTrue(HasErrorAt(1, 10),
    'Should find indicator in middle of "wrold"');
end;

procedure TTestVCLSynSpellCheckComponent.TestIndicatorAtPosNotFound;
begin
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckFile;
  // "hello" is valid, positions 1..5 should have no error indicator
  Assert.IsFalse(HasErrorAt(1, 1),
    'Should not find indicator at valid word position');
end;

procedure TTestVCLSynSpellCheckComponent.TestIndicatorAtPosWrongLine;
begin
  FEditor.Lines.Clear;
  FEditor.Lines.Add('hello wrold');
  FEditor.Lines.Add('hello world');
  FSpellCheck.CheckFile;
  // Error is on line 1 at char 7; line 2 at same char should return false
  Assert.IsFalse(HasErrorAt(2, 7),
    'Should not find indicator on wrong line');
end;

{ Provider }

procedure TTestVCLSynSpellCheckComponent.TestProviderProperty;
var
  NewProvider: ISynSpellCheckProvider;
begin
  NewProvider := TMockSpellProvider.Create;
  FSpellCheck.Provider := NewProvider;
  Assert.IsTrue(FSpellCheck.Provider = NewProvider,
    'Provider should return the assigned instance');
  FSpellCheck.Provider := nil;
  Assert.IsNull(FSpellCheck.Provider,
    'Provider should be nil after clearing');
  // Restore for TearDown
  FSpellCheck.Provider := FProvider;
end;

{ Events }

procedure TTestVCLSynSpellCheckComponent.TestOnChangeEvent;
begin
  FEventFired := False;
  FSpellCheck.OnChange := OnChangeFired;
  FSpellCheck.PenColor := clBlue;
  Assert.IsTrue(FEventFired, 'OnChange should fire after changing PenColor');
end;

{ Multi-editor }

procedure TTestVCLSynSpellCheckComponent.TestMultiEditor;
var
  Editor2: TSynEdit;
begin
  Editor2 := TSynEdit.Create(nil);
  try
    FSpellCheck.AddEditor(Editor2);
    // Both editors should be tracked; removing one should not affect the other
    Assert.IsTrue(FSpellCheck.RemoveEditor(Editor2),
      'RemoveEditor should return True for tracked editor');
    Assert.IsFalse(FSpellCheck.RemoveEditor(Editor2),
      'RemoveEditor should return False for already-removed editor');
    // Original editor should still work
    FEditor.Text := 'hello wrold';
    FSpellCheck.CheckFile;
    Assert.IsTrue(GetErrorCount > 0, 'Original editor should still find errors');
  finally
    Editor2.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestVCLSynSpellCheckComponent);

end.
