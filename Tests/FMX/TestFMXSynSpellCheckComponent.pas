unit TestFMXSynSpellCheckComponent;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit,
  FMX.SynSpellCheck;

type
  [TestFixture]
  TTestFMXSynSpellCheckComponent = class
  private
    FEditor: TFMXSynEdit;
    FSpellCheck: TSynFMXSpellCheck;
    FProvider: ISynSpellCheckProvider;
    FEventFired: Boolean;
    procedure OnCheckCompleteFired(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    { Defaults }
    [Test]
    procedure TestDefaults;
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
    [Test]
    procedure TestCheckFileDisabled;
    { CheckLine }
    [Test]
    procedure TestCheckLineSpecificLine;
    [Test]
    procedure TestCheckLineClearsOldErrors;
    { ClearErrors }
    [Test]
    procedure TestClearErrors;
    { ErrorAtPos }
    [Test]
    procedure TestErrorAtPosFound;
    [Test]
    procedure TestErrorAtPosNotFound;
    [Test]
    procedure TestErrorAtPosWrongLine;
    { Enabled toggle }
    [Test]
    procedure TestEnabledToggleClearsErrors;
    { Events }
    [Test]
    procedure TestOnCheckCompleteEvent;
    { CheckSelection }
    [Test]
    procedure TestCheckSelectionFallback;
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

{ TTestFMXSynSpellCheckComponent }

procedure TTestFMXSynSpellCheckComponent.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FSpellCheck := TSynFMXSpellCheck.Create(nil);
  FProvider := TMockSpellProvider.Create;
  FSpellCheck.Provider := FProvider;
  FSpellCheck.Editor := FEditor;
end;

procedure TTestFMXSynSpellCheckComponent.TearDown;
begin
  FSpellCheck.Free;
  FEditor.Free;
  FProvider := nil;
end;

{ Defaults }

procedure TTestFMXSynSpellCheckComponent.TestDefaults;
var
  SC: TSynFMXSpellCheck;
begin
  SC := TSynFMXSpellCheck.Create(nil);
  try
    Assert.IsTrue(SC.Enabled, 'Enabled should default to True');
    Assert.AreEqual(0, SC.Errors.Count, 'Errors should be empty');
    Assert.IsTrue(sctComment in SC.CheckTokens, 'sctComment should be in CheckTokens');
    Assert.IsTrue(sctString in SC.CheckTokens, 'sctString should be in CheckTokens');
    Assert.IsTrue(sctIdentifier in SC.CheckTokens, 'sctIdentifier should be in CheckTokens');
  finally
    SC.Free;
  end;
end;

{ Editor assignment }

procedure TTestFMXSynSpellCheckComponent.TestSetEditor;
begin
  // Setup already assigns editor — just verify no crash and editor is assigned
  Assert.IsNotNull(FSpellCheck.Editor, 'Editor should be assigned');
end;

procedure TTestFMXSynSpellCheckComponent.TestSetEditorNil;
begin
  FSpellCheck.Editor := nil;
  Assert.IsNull(FSpellCheck.Editor, 'Editor should be nil after clearing');
end;

{ CheckFile }

procedure TTestFMXSynSpellCheckComponent.TestCheckFileFindsErrors;
begin
  // "hello" is valid, "wrold" is not, "the" is valid, "quik" is not
  FEditor.Text := 'hello wrold the quik';
  FSpellCheck.CheckFile;
  Assert.AreEqual(2, FSpellCheck.Errors.Count,
    'Should find 2 misspelled words');
  // First error: "wrold"
  Assert.AreEqual('wrold', FSpellCheck.Errors[0].Word);
  Assert.AreEqual(1, FSpellCheck.Errors[0].Line, 'Error should be on line 1');
  Assert.AreEqual(7, FSpellCheck.Errors[0].StartChar, 'wrold starts at char 7');
  Assert.AreEqual(12, FSpellCheck.Errors[0].EndChar, 'wrold ends at char 12');
  // Second error: "quik"
  Assert.AreEqual('quik', FSpellCheck.Errors[1].Word);
end;

procedure TTestFMXSynSpellCheckComponent.TestCheckFileNoErrors;
begin
  FEditor.Text := 'hello world the';
  FSpellCheck.CheckFile;
  Assert.AreEqual(0, FSpellCheck.Errors.Count,
    'All valid words should produce no errors');
end;

procedure TTestFMXSynSpellCheckComponent.TestCheckFileNoProvider;
begin
  FSpellCheck.Provider := nil;
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckFile;
  Assert.AreEqual(0, FSpellCheck.Errors.Count,
    'No provider should produce no errors');
end;

procedure TTestFMXSynSpellCheckComponent.TestCheckFileDisabled;
begin
  FSpellCheck.Enabled := False;
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckFile;
  Assert.AreEqual(0, FSpellCheck.Errors.Count,
    'Disabled spell check should produce no errors');
end;

{ CheckLine }

procedure TTestFMXSynSpellCheckComponent.TestCheckLineSpecificLine;
begin
  FEditor.Lines.Clear;
  FEditor.Lines.Add('hello world');
  FEditor.Lines.Add('baaad wrold');
  FEditor.Lines.Add('the fox');
  FSpellCheck.CheckLine(2);
  // Only errors from line 2 should appear
  Assert.IsTrue(FSpellCheck.Errors.Count >= 1,
    'Should find errors on line 2');
  Assert.AreEqual(2, FSpellCheck.Errors[0].Line,
    'Error should be on line 2');
end;

procedure TTestFMXSynSpellCheckComponent.TestCheckLineClearsOldErrors;
begin
  FEditor.Lines.Clear;
  FEditor.Lines.Add('baaad');
  FEditor.Lines.Add('wrold');
  // Check line 1 first
  FSpellCheck.CheckLine(1);
  Assert.AreEqual(1, FSpellCheck.Errors.Count, 'Should have 1 error on line 1');
  Assert.AreEqual('baaad', FSpellCheck.Errors[0].Word);
  // Now fix line 1 and re-check
  FEditor.Lines[0] := 'hello';
  FSpellCheck.CheckLine(1);
  // Line 1 errors replaced, line 2 error still from before (only if checked)
  // Since we only checked line 1, and it's now valid, only remaining errors are
  // from any previous line 2 checks. We never checked line 2, so 0 errors.
  Assert.AreEqual(0, FSpellCheck.Errors.Count,
    'Re-checking line 1 with valid text should clear its errors');
end;

{ ClearErrors }

procedure TTestFMXSynSpellCheckComponent.TestClearErrors;
begin
  FEditor.Text := 'wrold baaad';
  FSpellCheck.CheckFile;
  Assert.IsTrue(FSpellCheck.Errors.Count > 0, 'Should have errors');
  FSpellCheck.ClearErrors;
  Assert.AreEqual(0, FSpellCheck.Errors.Count,
    'ClearErrors should empty the list');
end;

{ ErrorAtPos }

procedure TTestFMXSynSpellCheckComponent.TestErrorAtPosFound;
begin
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckFile;
  // "wrold" is at chars 7..11 on line 1
  Assert.AreEqual(0, FSpellCheck.ErrorAtPos(1, 7),
    'ErrorAtPos should return index 0 for start of "wrold"');
  Assert.AreEqual(0, FSpellCheck.ErrorAtPos(1, 10),
    'ErrorAtPos should return index 0 for middle of "wrold"');
end;

procedure TTestFMXSynSpellCheckComponent.TestErrorAtPosNotFound;
begin
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckFile;
  // "hello" is valid, positions 1..5 should not have errors
  Assert.AreEqual(-1, FSpellCheck.ErrorAtPos(1, 1),
    'ErrorAtPos should return -1 for valid word position');
end;

procedure TTestFMXSynSpellCheckComponent.TestErrorAtPosWrongLine;
begin
  FEditor.Lines.Clear;
  FEditor.Lines.Add('hello wrold');
  FEditor.Lines.Add('hello world');
  FSpellCheck.CheckFile;
  // Error is on line 1 at char 7, line 2 should return -1 at same char
  Assert.AreEqual(-1, FSpellCheck.ErrorAtPos(2, 7),
    'ErrorAtPos should return -1 for wrong line');
end;

{ Enabled toggle }

procedure TTestFMXSynSpellCheckComponent.TestEnabledToggleClearsErrors;
begin
  FEditor.Text := 'wrold baaad';
  FSpellCheck.CheckFile;
  Assert.IsTrue(FSpellCheck.Errors.Count > 0, 'Should have errors');
  FSpellCheck.Enabled := False;
  Assert.AreEqual(0, FSpellCheck.Errors.Count,
    'Disabling should clear errors');
end;

{ Events }

procedure TTestFMXSynSpellCheckComponent.OnCheckCompleteFired(Sender: TObject);
begin
  FEventFired := True;
end;

procedure TTestFMXSynSpellCheckComponent.TestOnCheckCompleteEvent;
begin
  FEventFired := False;
  FSpellCheck.OnCheckComplete := OnCheckCompleteFired;
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckFile;
  Assert.IsTrue(FEventFired, 'OnCheckComplete should fire after CheckFile');
end;

{ CheckSelection }

procedure TTestFMXSynSpellCheckComponent.TestCheckSelectionFallback;
begin
  // No selection set — should fall back to full-file check
  FEditor.Text := 'hello wrold';
  FSpellCheck.CheckSelection;
  Assert.IsTrue(FSpellCheck.Errors.Count > 0,
    'CheckSelection with no selection should fall back to CheckFile');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynSpellCheckComponent);

end.
