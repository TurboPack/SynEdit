program FMXSynEditTests;

{$APPTYPE CONSOLE}
{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  FMX.Forms,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  TestFMXSynEditBuffer in 'TestFMXSynEditBuffer.pas',
  TestFMXSynEditContent in 'TestFMXSynEditContent.pas',
  TestFMXSynEditCaret in 'TestFMXSynEditCaret.pas',
  TestFMXSynEditUndoRedo in 'TestFMXSynEditUndoRedo.pas',
  TestFMXSynEditOptions in 'TestFMXSynEditOptions.pas',
  TestFMXSynEditSearch in 'TestFMXSynEditSearch.pas',
  TestFMXSynEditCodeFolding in 'TestFMXSynEditCodeFolding.pas',
  TestFMXSynEditHighlighter in 'TestFMXSynEditHighlighter.pas',
  TestFMXSynEditCommands in 'TestFMXSynEditCommands.pas',
  TestFMXSynSpellCheck in 'TestFMXSynSpellCheck.pas',
  TestFMXSynWindowsSpellCheck in 'TestFMXSynWindowsSpellCheck.pas',
  TestFMXSynSpellCheckComponent in 'TestFMXSynSpellCheckComponent.pas',
  TestFMXSynEditBugFixes in 'TestFMXSynEditBugFixes.pas',
  TestFMXSynEditSelection in 'TestFMXSynEditSelection.pas',
  TestFMXSynEditClipboard in 'TestFMXSynEditClipboard.pas',
  TestFMXSynEditEditing in 'TestFMXSynEditEditing.pas',
  TestFMXSynEditRenderer in 'TestFMXSynEditRenderer.pas',
  TestFMXSynEditCompletionProposal in 'TestFMXSynEditCompletionProposal.pas',
  TestSynHighlighterDelphiFolding in 'TestSynHighlighterDelphiFolding.pas',
  TestSynHighlighterHTMLFolding in 'TestSynHighlighterHTMLFolding.pas',
  TestSynHighlighterXMLFolding in 'TestSynHighlighterXMLFolding.pas',
  TestSynHighlighterCSSFolding in 'TestSynHighlighterCSSFolding.pas',
  TestFMXSynEditWordWrap in 'TestFMXSynEditWordWrap.pas',
  TestFMXSynEditBookmarks in 'TestFMXSynEditBookmarks.pas',
  TestFMXSynEditGutter in 'TestFMXSynEditGutter.pas',
  TestFMXSynEditMultiCaret in 'TestFMXSynEditMultiCaret.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
begin
  // FMX platform services must be initialized before creating TFMXSynEdit
  Application.Initialize;
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Logger := TDUnitXConsoleLogger.Create(True);
    Runner.AddLogger(Logger);
    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);
    Runner.FailsOnNoAsserts := True;
    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := 1;
    {$IFNDEF CI}
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
