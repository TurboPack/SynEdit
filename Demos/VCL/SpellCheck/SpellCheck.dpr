// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program SpellCheck;

uses
  Vcl.Forms,
  SpellCheckMain in 'SpellCheckMain.pas' {Form1},
  dmSpellCheckCommands in 'dmSpellCheckCommands.pas' {CommandsDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TCommandsDataModule, CommandsDataModule);
  Application.Run;
end.
