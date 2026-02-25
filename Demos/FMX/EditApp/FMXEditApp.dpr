program FMXEditApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFMXEditAppMain in 'uFMXEditAppMain.pas' {FMXEditAppForm},
  uHighlighterProcs in '..\..\uHighlighterProcs.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFMXEditAppForm, FMXEditAppForm);
  Application.Run;
end.
