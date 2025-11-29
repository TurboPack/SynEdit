program FoldingDemo;

uses
  Vcl.Forms,
  uFoldingDemoForm in 'uFoldingDemoForm.pas' {FoldingDemoForm},
  Vcl.Themes,
  uHighlighterProcs in 'uHighlighterProcs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFoldingDemoForm, FoldingDemoForm);
  Application.Run;
end.
