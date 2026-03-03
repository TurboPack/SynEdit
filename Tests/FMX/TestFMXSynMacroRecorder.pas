unit TestFMXSynMacroRecorder;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  SynEditTypes,
  SynEditKeyCmds,
  SynMacroRecorderShared,
  FMX.SynEdit,
  FMX.SynMacroRecorder;

type
  // --- Shared macro event unit tests ---
  [TestFixture]
  TTestSharedMacroEvents = class
  private
    FPlaybackCalled: Boolean;
    FPlaybackCount: Integer;
    FPlaybackCommand: TSynEditorCommand;
    procedure PlaybackCallback(Command: TSynEditorCommand;
      AChar: WideChar; Data: Pointer);
    procedure PlaybackCountCallback(Command: TSynEditorCommand;
      AChar: WideChar; Data: Pointer);
  public
    [Test]
    procedure TestCreateMacroEventBasic;
    [Test]
    procedure TestCreateMacroEventChar;
    [Test]
    procedure TestCreateMacroEventString;
    [Test]
    procedure TestCreateMacroEventGotoXY;
    [Test]
    procedure TestBasicEventStreamRoundTrip;
    [Test]
    procedure TestCharEventStreamRoundTrip;
    [Test]
    procedure TestStringEventStreamRoundTrip;
    [Test]
    procedure TestPositionEventStreamRoundTrip;
    [Test]
    procedure TestBasicEventAsString;
    [Test]
    procedure TestCharEventAsString;
    [Test]
    procedure TestStringEventAsString;
    [Test]
    procedure TestPositionEventAsString;
    [Test]
    procedure TestPlaybackToCallsCallback;
    [Test]
    procedure TestRepeatCountPlaybackTo;
  end;

  // --- FMX editor hooked command handler tests ---
  [TestFixture]
  TTestFMXHookedCommandHandlers = class
  private
    FEditor: TFMXSynEdit;
    FHandlerCallCount: Integer;
    FHandler2CallCount: Integer;
    FLastAfterProcessing: Boolean;
    FLastCommand: TSynEditorCommand;
    FSuppressCommand: Boolean;
    procedure TestHandler(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: Pointer);
    procedure TestHandler2(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand;
      var AChar: WideChar; Data: Pointer; HandlerData: Pointer);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestRegisterAndCallHandler;
    [Test]
    procedure TestHandlerCalledBeforeAndAfter;
    [Test]
    procedure TestHandlerCanSuppressCommand;
    [Test]
    procedure TestUnregisterRemovesHandler;
    [Test]
    procedure TestMultipleHandlers;
  end;

  // --- FMX macro recorder integration tests ---
  [TestFixture]
  TTestFMXMacroRecorder = class
  private
    FEditor: TFMXSynEdit;
    FRecorder: TFMXSynMacroRecorder;
    FStateChangeCount: Integer;
    procedure OnStateChange(Sender: TObject);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestInitialState;
    [Test]
    procedure TestRecordAndPlaybackTyping;
    [Test]
    procedure TestRecordAndPlaybackNavigation;
    [Test]
    procedure TestRecordAndPlaybackDeleteAndType;
    [Test]
    procedure TestPlaybackIsUndoable;
    [Test]
    procedure TestStateTransitions;
    [Test]
    procedure TestPauseResume;
    [Test]
    procedure TestCannotRecordWhilePlaying;
    [Test]
    procedure TestCannotPlayWhileRecording;
    [Test]
    procedure TestClear;
    [Test]
    procedure TestSaveLoadStreamRoundTrip;
    [Test]
    procedure TestAsStringRoundTrip;
    [Test]
    procedure TestEditorFreeUnhooksRecorder;
    [Test]
    procedure TestEmptyMacroPlayback;
    [Test]
    procedure TestRecordGotoXY;
    [Test]
    procedure TestOnStateChangeFires;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

{ TTestSharedMacroEvents }

procedure TTestSharedMacroEvents.TestCreateMacroEventBasic;
var
  E: TSynMacroEvent;
begin
  E := CreateMacroEvent(ecRight);
  try
    Assert.IsTrue(E is TSynBasicEvent);
    Assert.AreEqual(Integer(ecRight), Integer(TSynBasicEvent(E).Command));
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestCreateMacroEventChar;
var
  E: TSynMacroEvent;
begin
  E := CreateMacroEvent(ecChar);
  try
    Assert.IsTrue(E is TSynCharEvent);
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestCreateMacroEventString;
var
  E: TSynMacroEvent;
begin
  E := CreateMacroEvent(ecString);
  try
    Assert.IsTrue(E is TSynStringEvent);
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestCreateMacroEventGotoXY;
var
  E: TSynMacroEvent;
begin
  E := CreateMacroEvent(ecGotoXY);
  try
    Assert.IsTrue(E is TSynPositionEvent);
    Assert.AreEqual(Integer(ecGotoXY), Integer(TSynPositionEvent(E).Command));
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestBasicEventStreamRoundTrip;
var
  E1, E2: TSynBasicEvent;
  Stream: TMemoryStream;
  Cmd: TSynEditorCommand;
begin
  E1 := TSynBasicEvent.Create;
  Stream := TMemoryStream.Create;
  try
    E1.Command := ecRight;
    E1.RepeatCount := 3;
    E1.SaveToStream(Stream);

    Stream.Position := 0;
    Stream.Read(Cmd, SizeOf(Cmd));
    E2 := TSynBasicEvent.Create;
    try
      E2.Command := Cmd;
      E2.LoadFromStream(Stream);
      Assert.AreEqual(Integer(ecRight), Integer(E2.Command));
      Assert.AreEqual(Integer(3), Integer(E2.RepeatCount));
    finally
      E2.Free;
    end;
  finally
    E1.Free;
    Stream.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestCharEventStreamRoundTrip;
var
  E1, E2: TSynCharEvent;
  Stream: TMemoryStream;
  Cmd: TSynEditorCommand;
begin
  E1 := TSynCharEvent.Create;
  Stream := TMemoryStream.Create;
  try
    E1.Initialize(ecChar, 'Z', nil);
    E1.SaveToStream(Stream);

    Stream.Position := 0;
    Stream.Read(Cmd, SizeOf(Cmd));
    E2 := TSynCharEvent.Create;
    try
      E2.LoadFromStream(Stream);
      Assert.AreEqual('Z', string(E2.Key));
    finally
      E2.Free;
    end;
  finally
    E1.Free;
    Stream.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestStringEventStreamRoundTrip;
var
  E1, E2: TSynStringEvent;
  Stream: TMemoryStream;
  Cmd: TSynEditorCommand;
  S: string;
begin
  E1 := TSynStringEvent.Create;
  Stream := TMemoryStream.Create;
  S := 'Hello';
  try
    E1.Initialize(ecString, #0, PChar(S));
    E1.SaveToStream(Stream);

    Stream.Position := 0;
    Stream.Read(Cmd, SizeOf(Cmd));
    E2 := TSynStringEvent.Create;
    try
      E2.LoadFromStream(Stream);
      Assert.AreEqual('Hello', E2.Value);
    finally
      E2.Free;
    end;
  finally
    E1.Free;
    Stream.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestPositionEventStreamRoundTrip;
var
  E1, E2: TSynPositionEvent;
  Stream: TMemoryStream;
  Cmd: TSynEditorCommand;
  RepeatCnt: Byte;
  Pos: TBufferCoord;
begin
  E1 := TSynPositionEvent.Create;
  Stream := TMemoryStream.Create;
  try
    E1.Command := ecGotoXY;
    Pos := BufferCoord(5, 10);
    E1.Initialize(ecGotoXY, #0, @Pos);
    E1.SaveToStream(Stream);

    // Stream format: Command(2) + RepeatCount(1) + Position(8)
    Stream.Position := 0;
    Stream.Read(Cmd, SizeOf(Cmd));
    Stream.Read(RepeatCnt, SizeOf(RepeatCnt));
    E2 := TSynPositionEvent.Create;
    try
      E2.Command := Cmd;
      E2.LoadFromStream(Stream);
      Assert.AreEqual(5, E2.Position.Char);
      Assert.AreEqual(10, E2.Position.Line);
    finally
      E2.Free;
    end;
  finally
    E1.Free;
    Stream.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestBasicEventAsString;
var
  E: TSynBasicEvent;
begin
  E := TSynBasicEvent.Create;
  try
    E.Command := ecRight;
    Assert.IsNotEmpty(E.AsString);
    Assert.IsTrue(Pos('ecRight', E.AsString) > 0);
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestCharEventAsString;
var
  E: TSynCharEvent;
begin
  E := TSynCharEvent.Create;
  try
    E.Initialize(ecChar, 'A', nil);
    Assert.IsNotEmpty(E.AsString);
    Assert.IsTrue(Pos('A', E.AsString) > 0);
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestStringEventAsString;
var
  E: TSynStringEvent;
  S: string;
begin
  E := TSynStringEvent.Create;
  S := 'test';
  try
    E.Initialize(ecString, #0, PChar(S));
    Assert.IsNotEmpty(E.AsString);
    Assert.IsTrue(Pos('test', E.AsString) > 0);
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestPositionEventAsString;
var
  E: TSynPositionEvent;
  Pos: TBufferCoord;
begin
  E := TSynPositionEvent.Create;
  try
    E.Command := ecGotoXY;
    Pos := BufferCoord(3, 7);
    E.Initialize(ecGotoXY, #0, @Pos);
    Assert.IsNotEmpty(E.AsString);
    Assert.IsTrue(Pos.Char > 0); // sanity
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.PlaybackCallback(
  Command: TSynEditorCommand; AChar: WideChar; Data: Pointer);
begin
  FPlaybackCalled := True;
  FPlaybackCommand := Command;
end;

procedure TTestSharedMacroEvents.PlaybackCountCallback(
  Command: TSynEditorCommand; AChar: WideChar; Data: Pointer);
begin
  Inc(FPlaybackCount);
end;

procedure TTestSharedMacroEvents.TestPlaybackToCallsCallback;
var
  E: TSynBasicEvent;
begin
  FPlaybackCalled := False;
  FPlaybackCommand := ecNone;
  E := TSynBasicEvent.Create;
  try
    E.Command := ecRight;
    E.PlaybackTo(PlaybackCallback);
    Assert.IsTrue(FPlaybackCalled);
    Assert.AreEqual(Integer(ecRight), Integer(FPlaybackCommand));
  finally
    E.Free;
  end;
end;

procedure TTestSharedMacroEvents.TestRepeatCountPlaybackTo;
var
  E: TSynBasicEvent;
begin
  FPlaybackCount := 0;
  E := TSynBasicEvent.Create;
  try
    E.Command := ecRight;
    E.RepeatCount := 5;
    E.PlaybackTo(PlaybackCountCallback);
    Assert.AreEqual(5, FPlaybackCount);
  finally
    E.Free;
  end;
end;

{ TTestFMXHookedCommandHandlers }

procedure TTestFMXHookedCommandHandlers.TestHandler(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar;
  Data: Pointer; HandlerData: Pointer);
begin
  Inc(FHandlerCallCount);
  FLastAfterProcessing := AfterProcessing;
  FLastCommand := Command;
  if FSuppressCommand and (not AfterProcessing) then
    Handled := True;
end;

procedure TTestFMXHookedCommandHandlers.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'Hello World';
  FEditor.CaretXY := BufferCoord(1, 1);
  FHandlerCallCount := 0;
  FLastAfterProcessing := False;
  FLastCommand := ecNone;
  FSuppressCommand := False;
end;

procedure TTestFMXHookedCommandHandlers.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXHookedCommandHandlers.TestRegisterAndCallHandler;
begin
  FEditor.RegisterCommandHandler(TestHandler, nil);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.UnregisterCommandHandler(TestHandler);
  Assert.IsTrue(FHandlerCallCount > 0);
end;

procedure TTestFMXHookedCommandHandlers.TestHandlerCalledBeforeAndAfter;
begin
  FEditor.RegisterCommandHandler(TestHandler, nil);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.UnregisterCommandHandler(TestHandler);
  // Should be called twice: before and after
  Assert.AreEqual(2, FHandlerCallCount);
end;

procedure TTestFMXHookedCommandHandlers.TestHandlerCanSuppressCommand;
begin
  FSuppressCommand := True;
  FEditor.RegisterCommandHandler(TestHandler, nil);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.UnregisterCommandHandler(TestHandler);
  // Command was suppressed, so caret should not have moved
  Assert.AreEqual(1, FEditor.CaretX);
  // Only the pre-hook should fire (command suppressed)
  Assert.AreEqual(1, FHandlerCallCount);
end;

procedure TTestFMXHookedCommandHandlers.TestUnregisterRemovesHandler;
begin
  FEditor.RegisterCommandHandler(TestHandler, nil);
  FEditor.UnregisterCommandHandler(TestHandler);
  FEditor.CommandProcessor(ecRight, #0);
  Assert.AreEqual(0, FHandlerCallCount);
end;

procedure TTestFMXHookedCommandHandlers.TestHandler2(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar;
  Data: Pointer; HandlerData: Pointer);
begin
  Inc(FHandler2CallCount);
end;

procedure TTestFMXHookedCommandHandlers.TestMultipleHandlers;
begin
  FHandler2CallCount := 0;
  FEditor.RegisterCommandHandler(TestHandler, nil);
  FEditor.RegisterCommandHandler(TestHandler2, nil);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.UnregisterCommandHandler(TestHandler);
  FEditor.UnregisterCommandHandler(TestHandler2);
  Assert.IsTrue(FHandlerCallCount > 0);
  Assert.IsTrue(FHandler2CallCount > 0);
end;

{ TTestFMXMacroRecorder }

procedure TTestFMXMacroRecorder.OnStateChange(Sender: TObject);
begin
  Inc(FStateChangeCount);
end;

procedure TTestFMXMacroRecorder.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := 'Hello World' + sLineBreak + 'Second Line';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder := TFMXSynMacroRecorder.Create(nil);
  FRecorder.Editor := FEditor;
  FStateChangeCount := 0;
end;

procedure TTestFMXMacroRecorder.TearDown;
begin
  FRecorder.Free;
  FEditor.Free;
end;

procedure TTestFMXMacroRecorder.TestInitialState;
begin
  Assert.AreEqual(Ord(msStopped), Ord(FRecorder.State));
  Assert.IsTrue(FRecorder.IsEmpty);
  Assert.AreEqual('unnamed', FRecorder.MacroName);
  Assert.IsFalse(FRecorder.SaveMarkerPos);
end;

procedure TTestFMXMacroRecorder.TestRecordAndPlaybackTyping;
begin
  FEditor.Text := '';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecChar, 'A');
  FEditor.CommandProcessor(ecChar, 'B');
  FEditor.CommandProcessor(ecChar, 'C');
  FRecorder.Stop;
  Assert.AreEqual(3, FRecorder.EventCount);
  // Playback on fresh text
  FEditor.Text := '';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual('ABC', FEditor.Lines[0]);
end;

procedure TTestFMXMacroRecorder.TestRecordAndPlaybackNavigation;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.CommandProcessor(ecRight, #0);
  FRecorder.Stop;
  Assert.AreEqual(3, FRecorder.EventCount);
  // Playback from position 1
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual(4, FEditor.CaretX);
end;

procedure TTestFMXMacroRecorder.TestRecordAndPlaybackDeleteAndType;
begin
  FEditor.Text := 'ABC';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecDeleteChar, #0);
  FEditor.CommandProcessor(ecChar, 'X');
  FRecorder.Stop;
  // Apply to fresh text
  FEditor.Text := 'DEF';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual('XEF', FEditor.Lines[0]);
end;

procedure TTestFMXMacroRecorder.TestPlaybackIsUndoable;
var
  OrigText: string;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  OrigText := FEditor.Text;
  // Record: type 'X'
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecChar, 'X');
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

procedure TTestFMXMacroRecorder.TestStateTransitions;
begin
  Assert.AreEqual(Ord(msStopped), Ord(FRecorder.State));
  FRecorder.RecordMacro(FEditor);
  Assert.AreEqual(Ord(msRecording), Ord(FRecorder.State));
  FRecorder.Stop;
  Assert.AreEqual(Ord(msStopped), Ord(FRecorder.State));
end;

procedure TTestFMXMacroRecorder.TestPauseResume;
begin
  FRecorder.RecordMacro(FEditor);
  FRecorder.Pause;
  Assert.AreEqual(Ord(msPaused), Ord(FRecorder.State));
  FRecorder.Resume;
  Assert.AreEqual(Ord(msRecording), Ord(FRecorder.State));
  FRecorder.Stop;
end;

procedure TTestFMXMacroRecorder.TestCannotRecordWhilePlaying;
begin
  FRecorder.AddEvent(ecRight, #0, nil);
  FRecorder.PlaybackMacro(FEditor);
  // After playback completes, should be able to record
  FRecorder.RecordMacro(FEditor);
  Assert.AreEqual(Ord(msRecording), Ord(FRecorder.State));
  FRecorder.Stop;
end;

procedure TTestFMXMacroRecorder.TestCannotPlayWhileRecording;
var
  Raised: Boolean;
begin
  Raised := False;
  FRecorder.RecordMacro(FEditor);
  try
    FRecorder.PlaybackMacro(FEditor);
  except
    on E: Exception do
      Raised := True;
  end;
  FRecorder.Stop;
  Assert.IsTrue(Raised, 'Should raise exception when playing while recording');
end;

procedure TTestFMXMacroRecorder.TestClear;
begin
  FRecorder.AddEvent(ecRight, #0, nil);
  FRecorder.AddEvent(ecLeft, #0, nil);
  Assert.AreEqual(2, FRecorder.EventCount);
  FRecorder.Clear;
  Assert.IsTrue(FRecorder.IsEmpty);
  Assert.AreEqual(0, FRecorder.EventCount);
end;

procedure TTestFMXMacroRecorder.TestSaveLoadStreamRoundTrip;
var
  Stream: TMemoryStream;
begin
  FEditor.Text := 'Test';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.CommandProcessor(ecChar, 'X');
  FRecorder.Stop;
  Assert.AreEqual(2, FRecorder.EventCount);

  Stream := TMemoryStream.Create;
  try
    FRecorder.SaveToStream(Stream);
    Assert.IsTrue(Stream.Size > 0);
    FRecorder.Clear;
    Stream.Position := 0;
    FRecorder.LoadFromStream(Stream);
    Assert.AreEqual(2, FRecorder.EventCount);
    // Verify playback
    FEditor.Text := 'Test';
    FEditor.CaretXY := BufferCoord(1, 1);
    FRecorder.PlaybackMacro(FEditor);
    Assert.AreEqual('TXest', FEditor.Lines[0]);
  finally
    Stream.Free;
  end;
end;

procedure TTestFMXMacroRecorder.TestAsStringRoundTrip;
var
  MacroStr: string;
begin
  FEditor.Text := 'Hello';
  FEditor.CaretXY := BufferCoord(1, 1);
  FRecorder.RecordMacro(FEditor);
  FEditor.CommandProcessor(ecRight, #0);
  FEditor.CommandProcessor(ecChar, 'Y');
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

procedure TTestFMXMacroRecorder.TestEditorFreeUnhooksRecorder;
var
  TempEditor: TFMXSynEdit;
begin
  TempEditor := TFMXSynEdit.Create(nil);
  try
    FRecorder.Editor := TempEditor;
    Assert.IsNotNull(TComponent(FRecorder.Editor));
  finally
    TempEditor.Free;
  end;
  // After editor is freed, recorder should have nil editor
  Assert.IsNull(TComponent(FRecorder.Editor));
  Assert.AreEqual(Ord(msStopped), Ord(FRecorder.State));
end;

procedure TTestFMXMacroRecorder.TestEmptyMacroPlayback;
begin
  FEditor.Text := 'Hello';
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual('Hello', FEditor.Text);
end;

procedure TTestFMXMacroRecorder.TestRecordGotoXY;
var
  Pos: TBufferCoord;
begin
  FEditor.Text := 'Hello' + sLineBreak + 'World';
  FEditor.CaretXY := BufferCoord(1, 1);
  // Manually add a GotoXY event
  Pos := BufferCoord(3, 2);
  FRecorder.AddEvent(ecGotoXY, #0, @Pos);
  Assert.AreEqual(1, FRecorder.EventCount);
  // Playback
  FRecorder.PlaybackMacro(FEditor);
  Assert.AreEqual(3, FEditor.CaretX);
  Assert.AreEqual(2, FEditor.CaretY);
end;

procedure TTestFMXMacroRecorder.TestOnStateChangeFires;
begin
  FRecorder.OnStateChange := OnStateChange;
  FStateChangeCount := 0;
  FRecorder.RecordMacro(FEditor);
  Assert.IsTrue(FStateChangeCount > 0, 'Should fire on record');
  FRecorder.Stop;
  Assert.AreEqual(2, FStateChangeCount, 'Should fire on record and stop');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSharedMacroEvents);
  TDUnitX.RegisterTestFixture(TTestFMXHookedCommandHandlers);
  TDUnitX.RegisterTestFixture(TTestFMXMacroRecorder);
end.
