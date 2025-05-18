unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dmCommands, SynEdit, Vcl.Menus;

type
  TForm1 = class(TForm)
    SynEdit: TSynEdit;
    pmnuEditor: TPopupMenu;
    pmnuSpelling: TMenuItem;
    pmnSeparator: TMenuItem;
    pmnDelete: TMenuItem;
    pmnAdd: TMenuItem;
    pmnIgnore: TMenuItem;
    pmnIgnoreOnce: TMenuItem;
    pmnSeparator2: TMenuItem;
    CheckFile1: TMenuItem;
    SpellClearErros11: TMenuItem;
    CheckSelection1: TMenuItem;
    CheckWord1: TMenuItem;
    N3: TMenuItem;
    ClearErrors1: TMenuItem;
    N4: TMenuItem;
    CheckAsYouType1: TMenuItem;
    lmiEditUndo: TMenuItem;
    N2: TMenuItem;
    lmiEditCut: TMenuItem;
    lmiEditCopy: TMenuItem;
    lmiEditPaste: TMenuItem;
    lmiEditDelete: TMenuItem;
    N1: TMenuItem;
    lmiEditSelectAll: TMenuItem;
    N5: TMenuItem;
    procedure pmnuEditorPopup(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

Uses
  Winapi.ActiveX,
  SynEditMiscClasses,
  SynSpellCheck;

{$R *.dfm}

procedure TForm1.pmnuEditorPopup(Sender: TObject);
var
  Error: ISpellingError;
  CorrectiveAction: CORRECTIVE_ACTION;
  Replacement: PChar;
  MenuItem: TMenuItem;
  Action: TSynSpellErrorReplace;
  Suggestions: IEnumString;
  Suggestion: PWideChar;
  Fetched: LongInt;
  Indicator: TSynIndicator;
  AWord: string;
  HaveError: Boolean;
begin
  // Remove replacement menu items and actions;
  repeat
    MenuItem := pmnuSpelling.Items[0];
    if MenuItem.Action is TSynSpellErrorReplace then
    begin
      pmnuSpelling.Remove(MenuItem);
      MenuItem.Action.Free;
      MenuItem.Free;
    end
    else
      Break;
  until (False);

  if not Assigned(CommandsDataModule.SynSpellCheck.SpellChecker()) then
  begin
    pmnuSpelling.Visible := False;
    Exit;
  end;

  if SynEdit.Indicators.IndicatorAtPos(SynEdit.CaretXY, Indicator) and
    (Indicator.Id = TSynSpellCheck.SpellErrorIndicatorId) then
     AWord := Copy(SynEdit.Lines[SynEdit.CaretY - 1], Indicator.CharStart,
       Indicator.CharEnd - Indicator.CharStart)
  else
    AWord := '';

  CommandsDataModule.SynSpellCheck.Editor := SynEdit;
  Error := CommandsDataModule.SynSpellCheck.ErrorAtPos(SynEdit.CaretXY);
  HaveError := Assigned(Error) and (AWord <> '');

  pmnSeparator.Visible := HaveError;
  pmnSeparator2.Visible := HaveError;
  pmnIgnore.Visible := HaveError;
  pmnAdd.Visible := HaveError;
  pmnIgnoreOnce.Visible := HaveError;
  pmnIgnore.Visible := HaveError;
  pmnDelete.Visible := HaveError;


  if HaveError then
  begin
    Error.Get_CorrectiveAction(CorrectiveAction);
    case CorrectiveAction of
      CORRECTIVE_ACTION_GET_SUGGESTIONS:
        begin
          CheckOSError(CommandsDataModule.SynSpellCheck.SpellChecker.Suggest(
            PChar(AWord), Suggestions));
          while Suggestions.Next(1, Suggestion, @Fetched) = S_OK do
          begin
            Action := TSynSpellErrorReplace.Create(Self);
            Action.Caption := Suggestion;
            MenuItem := TMenuItem.Create(Self);
            MenuItem.Action := Action;
            pmnuSpelling.Insert(pmnSeparator.MenuIndex, MenuItem);
            CoTaskMemFree(Suggestion);
          end;
        end;
      CORRECTIVE_ACTION_REPLACE:
        begin
          Error.Get_Replacement(Replacement);
          Action := TSynSpellErrorReplace.Create(Self);
          Action.Caption := Replacement;
          MenuItem := TMenuItem.Create(Self);
          MenuItem.Action := Action;
          pmnuSpelling.Insert(0, MenuItem);
        end;
    end;
  end;
end;

end.
