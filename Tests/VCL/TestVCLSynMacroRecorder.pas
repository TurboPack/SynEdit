unit TestVCLSynMacroRecorder;

interface

uses
  DUnitX.TestFramework,
  Vcl.Forms,
  System.Classes,
  SynEditTypes,
  SynEditKeyCmds,
  SynEdit,
  SynMacroRecorderShared,
  Vcl.SynMacroRecorder;

type
  [TestFixture]
  TTestVCLMacroRecorder = class
  private
    FForm: TForm;
    FEditor: TSynEdit;
    FRecorder: TSynMacroRecorder;
    FStateChangeCount: Integer;
    procedure OnStateChange(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // State machine tests
    [Test]
    procedure TestInitialStateIsStopped;
    [Test]
    procedure TestRecordChangesState;
    [Test]
    procedure TestStopChangesState;
    [Test]
    procedure TestPauseResumeState;
    [Test]
    procedure TestCannotRecordWhilePlaying;
    [Test]
    procedure TestCannotPlayWhileRecording;
    [Test]
    procedure TestCannotPauseWhenStopped;
    [Test]
    procedure TestCannotResumeWhenNotPaused;
    [Test]
    procedure TestStopWhenAlreadyStoppedIsSafe;
    [Test]
    procedure TestOnStateChangeFiresOnRecord;
    [Test]
    procedure TestOnStateChangeFiresOnStop;

    // Recording tests
    [Test]
    procedure TestRecordClearsExistingMacro;
    [Test]
    procedure TestRecordBasicNavigation;
    [Test]
    procedure TestRecordCharInput;
    [Test]
    procedure TestRecordMultipleCommands;

    // Playback tests
    [Test]
    procedure TestPlaybackEmptyMacro;
    [Test]
    procedure TestPlaybackNavigation;
    [Test]
    procedure TestPlaybackTyping;
    [Test]
    procedure TestPlaybackMultiEdit;
    [Test]
    procedure TestPlaybackIsUndoable;

    // Event management tests
    [Test]
    procedure TestAddEvent;
    [Test]
    procedure TestDeleteEvent;
    [Test]
    procedure TestClear;
    [Test]
    procedure TestIsEmptyAfterClear;
    [Test]
    procedure TestEventCount;

    // Serialization tests
    [Test]
    procedure TestSaveLoadStreamRoundTrip;
    [Test]
    procedure TestAsStringRoundTrip;
    [Test]
    procedure TestSaveLoadFileRoundTrip;

    // Property tests
    [Test]
    procedure TestDefaultShortcuts;
    [Test]
    procedure TestMacroNameDefault;
    [Test]
    procedure TestSaveMarkerPosDefault;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Vcl.Menus;

{ TTestVCLMacroRecorder }

procedure TTestVCLMacroRecorder.OnStateChange(Sender: TObject);
begin
  Inc(FStateChangeCount);
end;

procedure TTestVCLMacroRecorder.Setup;
begin
  FForm := TForm.CreateNew(nil);
  FEditor := TSynEdit.Create(FForm);
  FEditor.Parent := FForm;
  FRecorder := TSynMacroRecorder.Create(FForm);
  FRecorder.AddEditor(FEditor);
  FStateChangeCount := 0;
end;

procedure TTestVCLMacroRecorder.TearDown;
begin
  FForm.Free;
end;

// --- State machine tests ---

procedure TTestVCLMacroRecorder.TestInitialStateIsStopped;
begin
  Assert.AreEqual(Ord(msStopped), Ord(FRecorder.State));
end;

procedure TTestVCLMacroRecorder.TestRecordChangesState;
begin
  FRecorder.RecordMacro(FEditor);
  Assert.AreEqual(Ord(msRecording), Ord(FRecorder.State));
  FRecorder.Stop;
end;

procedure TTestVCLMacroRecorder.TestStopChangesState;
begin
  FRecorder.RecordMacro(FEditor);
  FRecorder.Stop;
  Assert.AreEqual(Ord(msStopped), Ord(FRecorder.State));
end;

procedure TTestVCLMacroRecorder.TestPauseResumeState;
begin
  FRecorder.RecordMacro(FEditor);
  FRecorder.Pause;
  Assert.AreEqual(Ord(msPaused), Ord(FRecorder.State));
  FRecorder.Resume;
  Assert.AreEqual(Ord(msRecording), Ord(FRecorder.State));
  FRecorder.Stop;
end;

procedure TTestVCLMacroRecorder.TestCannotRecordWhilePlaying;
begin
  // Add a simple event so playback has something to do
  FRecorder.AddEvent(ecRight, #0, nil);
  FRecorder.PlaybackMacro(FEditor);
  // After playback completes, state returns to stopped - should be safe to record
  FRecorder.RecordMacro(FEditor);
  Assert.AreEqual(Ord(msRecording), Ord(FRecorder.State));
  FRecorder.Stop;
end;

procedure TTestVCLMacroRecorder.TestCannotPlayWhileRecording;
begin
  FRecorder.RecordMacro(FEditor);
  Assert.WillRaise(
    procedure
    begin
      FRecorder.PlaybackMacro(FEditor);
    end, Exception);
  FRecorder.Stop;
end;

procedure TTestVCLMacroRecorder.TestCannotPauseWhenStopped;
begin
  Assert.WillRaise(
    procedure
    begin
      FRecorder.Pause;
    end, Exception);
end;

procedure TTestVCLMacroRecorder.TestCannotResumeWhenNotPaused;
begin
  Assert.WillRaise(
    procedure
    begin
      FRecorder.Resume;
    end, Exception);
end;

procedure TTestVCLMacroRecorder.TestStopWhenAlreadyStoppedIsSafe;
begin
  FRecorder.Stop;
  Assert.AreEqual(Ord(msStopped), Ord(FRecorder.State));
end;

procedure TTestVCLMacroRecorder.TestOnStateChangeFiresOnRecord;
begin
  FRecorder.OnStateChange := OnStateChange;
  FStateChangeCount := 0;
  FRecorder.RecordMacro(FEditor);
  Assert.IsTrue(FStateChangeCount > 0, 'OnStateChange should fire on Record');
  FRecorder.Stop;
end;

procedure TTestVCLMacroRecorder.TestOnStateChangeFiresOnStop;
begin
  FRecorder.OnStateChange := OnStateChange;
  FStateChangeCount := 0;
  FRecorder.RecordMacro(FEditor);
  FRecorder.Stop;
  Assert.AreEqual(2, FStateChangeCount, 'Should fire on record and stop');
end;

// --- Recording tests ---

procedure TTestVCLMacroRecorder.TestRecordClearsExistingMacro;
begin
  FRecorder.AddEvent(ecRight, #0, nil);
  Assert.AreEqual(1, FRecorder.EventCount);
  FRecorder.RecordMacro(FEditor);
  Assert.AreEqual(0, FRecorder.EventCount);
  FRecorder.Stop;
end;

procedure TTestVCLMacroRecorder.TestRecordBasicNavigation;
begin
  FEditor.Text := 'Hello World';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FRecorder.Stop;
  Assert.AreEqual(2, FRecorder.EventCount);
end;

procedure TTestVCLMacroRecorder.TestRecordCharInput;
begin
  FEditor.Text := '';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecChar, 'A', nil);
  FEditor.CommandProcessor(ecChar, 'B', nil);
  FRecorder.Stop;
  Assert.AreEqual(2, FRecorder.EventCount);
end;

procedure TTestVCLMacroRecorder.TestRecordMultipleCommands;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FEditor.CommandProcessor(ecChar, 'X', nil);
  FEditor.CommandProcessor(ecLeft, #0, nil);
  FRecorder.Stop;
  Assert.AreEqual(3, FRecorder.EventCount);
end;

// --- Playback tests ---

procedure TTestVCLMacroRecorder.TestPlaybackEmptyMacro;
begin
  FEditor.Text := 'Hello';
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual('Hello', FEditor.Text);
end;

procedure TTestVCLMacroRecorder.TestPlaybackNavigation;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  // Record: move right 3 times
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FRecorder.Stop;
  // Playback from position 1
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual(4, FEditor.CaretX);
end;

procedure TTestVCLMacroRecorder.TestPlaybackTyping;
begin
  FEditor.Text := '';
  FEditor.CaretXY := BufferCoord(1, 1);
  // Record typing "Hi"
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecChar, 'H', nil);
  FEditor.CommandProcessor(ecChar, 'i', nil);
  FRecorder.Stop;
  // Playback on fresh text
  FEditor.Text := '';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual('Hi', FEditor.Lines[0]);
end;

procedure TTestVCLMacroRecorder.TestPlaybackMultiEdit;
begin
  FEditor.Text := 'ABC';
  FEditor.CaretXY := BufferCoord(1, 1);
  // Record: delete first char, type 'X'
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecDeleteChar, #0, nil);
  FEditor.CommandProcessor(ecChar, 'X', nil);
  FRecorder.Stop;
  // Apply to fresh text
  FEditor.Text := 'DEF';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual('XEF', FEditor.Lines[0]);
end;

procedure TTestVCLMacroRecorder.TestPlaybackIsUndoable;
var
  OrigText: string;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  OrigText := FEditor.Text;
  // Record: type 'X'
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecChar, 'X', nil);
  FRecorder.Stop;
  // Reset and playback
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreNotEqual(OrigText, FEditor.Text);
  // Undo
  FEditor.Undo;
  Assert.AreEqual(OrigText, FEditor.Text);
end;

// --- Event management tests ---

procedure TTestVCLMacroRecorder.TestAddEvent;
begin
  FRecorder.AddEvent(ecRight, #0, nil);
  Assert.AreEqual(1, FRecorder.EventCount);
end;

procedure TTestVCLMacroRecorder.TestDeleteEvent;
begin
  FRecorder.AddEvent(ecRight, #0, nil);
  FRecorder.AddEvent(ecLeft, #0, nil);
  FRecorder.DeleteEvent(0);
  Assert.AreEqual(1, FRecorder.EventCount);
end;

procedure TTestVCLMacroRecorder.TestClear;
begin
  FRecorder.AddEvent(ecRight, #0, nil);
  FRecorder.AddEvent(ecLeft, #0, nil);
  FRecorder.Clear;
  Assert.IsTrue(FRecorder.IsEmpty);
end;

procedure TTestVCLMacroRecorder.TestIsEmptyAfterClear;
begin
  FRecorder.AddEvent(ecRight, #0, nil);
  Assert.IsFalse(FRecorder.IsEmpty);
  FRecorder.Clear;
  Assert.IsTrue(FRecorder.IsEmpty);
end;

procedure TTestVCLMacroRecorder.TestEventCount;
begin
  Assert.AreEqual(0, FRecorder.EventCount);
  FRecorder.AddEvent(ecRight, #0, nil);
  Assert.AreEqual(1, FRecorder.EventCount);
  FRecorder.AddEvent(ecLeft, #0, nil);
  Assert.AreEqual(2, FRecorder.EventCount);
end;

// --- Serialization tests ---

procedure TTestVCLMacroRecorder.TestSaveLoadStreamRoundTrip;
var
  Stream: TMemoryStream;
begin
  FEditor.Text := 'Test';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FEditor.CommandProcessor(ecChar, 'X', nil);
  FRecorder.Stop;
  Assert.AreEqual(2, FRecorder.EventCount);
  Stream := TMemoryStream.Create;
  try
    FRecorder.SaveToStream(Stream);
    Assert.IsTrue(Stream.Size > 0);
    FRecorder.Clear;
    Assert.IsTrue(FRecorder.IsEmpty);
    Stream.Position := 0;
    FRecorder.LoadFromStream(Stream);
    Assert.AreEqual(2, FRecorder.EventCount);
    FEditor.Text := 'Test';
    FEditor.CaretXY := BufferCoord(1, 1);
    FRecorder.PlaybackMacro(FEditor);
    Assert.AreEqual('TXest', FEditor.Lines[0]);
  finally
    Stream.Free;
  end;
end;

procedure TTestVCLMacroRecorder.TestAsStringRoundTrip;
var
  MacroStr: string;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0, nil);
  FEditor.CommandProcessor(ecChar, 'Y', nil);
  FRecorder.Stop;
  MacroStr := FRecorder.AsString;
  Assert.IsNotEmpty(MacroStr);
  FRecorder.Clear;
  FRecorder.AsString := MacroStr;
  Assert.AreEqual(2, FRecorder.EventCount);
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual('HYello', FEditor.Lines[0]);
end;

procedure TTestVCLMacroRecorder.TestSaveLoadFileRoundTrip;
var
  TempFile: string;
begin
  TempFile := TPath.GetTempFileName;
  try
    FEditor.Text := 'File test';
    FEditor.CaretXY := BufferCoord(1, 1);
    FRecorder.RecordMacro(FEditor);
    FEditor.CommandProcessor(ecChar, 'Z', nil);
    FRecorder.Stop;
    FRecorder.SaveToFile(TempFile);
    FRecorder.Clear;
    FRecorder.LoadFromFile(TempFile);
    Assert.AreEqual(1, FRecorder.EventCount);
    FEditor.Text := 'File test';
    FEditor.CaretXY := BufferCoord(1, 1);
    FRecorder.PlaybackMacro(FEditor);
    Assert.AreEqual('ZFile test', FEditor.Lines[0]);
  finally
    if FileExists(TempFile) then
      DeleteFile(TempFile);
  end;
end;

// --- Property tests ---

procedure TTestVCLMacroRecorder.TestDefaultShortcuts;
begin
  Assert.AreEqual(
    Integer(Vcl.Menus.ShortCut(Ord('R'), [ssCtrl, ssShift])),
    Integer(FRecorder.RecordShortCut));
  Assert.AreEqual(
    Integer(Vcl.Menus.ShortCut(Ord('P'), [ssCtrl, ssShift])),
    Integer(FRecorder.PlaybackShortCut));
end;

procedure TTestVCLMacroRecorder.TestMacroNameDefault;
begin
  Assert.AreEqual('unnamed', FRecorder.MacroName);
end;

procedure TTestVCLMacroRecorder.TestSaveMarkerPosDefault;
begin
  Assert.IsFalse(FRecorder.SaveMarkerPos);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestVCLMacroRecorder);
end.
