program FMXHighlighterDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFMXHighlighterMain in 'uFMXHighlighterMain.pas' {FMXHighlighterForm};

begin
  Application.Initialize;
  Application.CreateForm(TFMXHighlighterForm, FMXHighlighterForm);
  Application.Run;
end.
