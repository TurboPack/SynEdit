// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program SpellCheck;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  dmCommands in 'dmCommands.pas' {CommandsDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TCommandsDataModule, CommandsDataModule);
  Application.Run;
end.
