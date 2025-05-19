program MarkdownViewerDemo;

uses
  Vcl.Forms,
  frmMain in 'frmMain.pas' {Form1},
  SynMarkdownViewer in 'SynMarkdownViewer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
