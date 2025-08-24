unit SynEditActionsResource;

interface

uses
  System.SysUtils, System.Classes, System.Actions, Vcl.ActnList, Vcl.StdActns,
  SynEditMiscClasses, SynSpellCheck;

type
  TSynEditActions = class(TDataModule)
    ActionList: TActionList;
    SynEditRedo: TSynEditRedo;
    SynSpellCheckFile: TSynSpellCheckFile;
    SynSpellCheckLine: TSynSpellCheckLine;
    SynSpellCheckSelection: TSynSpellCheckSelection;
    SynSpellCheckWord: TSynSpellCheckWord;
    SynSpellClearErrors: TSynSpellClearErrors;
    SynSpellCheckAsYouType: TSynSpellCheckAsYouType;
    SynSpellErrorAdd: TSynSpellErrorAdd;
    SynSpellErrorIgnoreOnce: TSynSpellErrorIgnoreOnce;
    SynSpellErrorIgnore: TSynSpellErrorIgnore;
    SynSpellErrorDelete: TSynSpellErrorDelete;
  private
  public
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
