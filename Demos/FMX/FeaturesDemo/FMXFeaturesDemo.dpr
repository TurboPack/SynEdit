program FMXFeaturesDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFMXFeaturesMain in 'uFMXFeaturesMain.pas' {FMXFeaturesForm},
  dlgFMXSearchText in 'dlgFMXSearchText.pas' {FMXSearchTextDialog},
  dlgFMXReplaceText in 'dlgFMXReplaceText.pas' {FMXReplaceTextDialog};

begin
  Application.Initialize;
  Application.CreateForm(TFMXFeaturesForm, FMXFeaturesForm);
  Application.Run;
end.
