program FMXFeaturesDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFMXFeaturesMain in 'uFMXFeaturesMain.pas' {FMXFeaturesForm};

begin
  Application.Initialize;
  Application.CreateForm(TFMXFeaturesForm, FMXFeaturesForm);
  Application.Run;
end.
