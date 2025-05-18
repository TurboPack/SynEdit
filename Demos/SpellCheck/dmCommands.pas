unit dmCommands;

interface

uses
  System.SysUtils, System.Classes, SynEditHighlighter, SynEditCodeFolding,
  SynHighlighterPas, SynSpellCheck, Vcl.StdActns, System.Actions, Vcl.ActnList;

type
  TCommandsDataModule = class(TDataModule)
    SynPasSyn1: TSynPasSyn;
    SynSpellCheck: TSynSpellCheck;
    ActionList1: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    actSynSpellCheckFile: TSynSpellCheckFile;
    actSynSpellCheckLine: TSynSpellCheckLine;
    actSynSpellCheckSelection: TSynSpellCheckSelection;
    actSynSpellCheckWord: TSynSpellCheckWord;
    actSynSpellClearErrors: TSynSpellClearErrors;
    actSynSpellCheckAsYouType: TSynSpellCheckAsYouType;
    actSynSpellErrorAdd: TSynSpellErrorAdd;
    actSynSpellErrorIgnoreOnce: TSynSpellErrorIgnoreOnce;
    actSynSpellErrorIgnore: TSynSpellErrorIgnore;
    actSynSpellErrorDelete: TSynSpellErrorDelete;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CommandsDataModule: TCommandsDataModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TCommandsDataModule.DataModuleCreate(Sender: TObject);
begin
  SynSpellCheck.LanguageCode := 'en-US';
end;

end.
