{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Search Dialog Demo
-------------------------------------------------------------------------------}
unit dlgFMXSearchText;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.Controls.Presentation,
  SynEditTypes;

type
  TFMXSearchTextDialog = class(TForm)
    lblSearch: TLabel;
    edtSearch: TEdit;
    cbMatchCase: TCheckBox;
    cbWholeWord: TCheckBox;
    cbRegex: TCheckBox;
    rbForward: TRadioButton;
    rbBackward: TRadioButton;
    btnFindNext: TButton;
    btnClose: TButton;
    grpOptions: TGroupBox;
    grpDirection: TGroupBox;
    procedure btnFindNextClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSearchOptions: TSynSearchOptions;
  public
    property SearchOptions: TSynSearchOptions read FSearchOptions;
    function GetSearchText: string;
    function GetOptions: TSynSearchOptions;
  end;

implementation

{$R *.fmx}

procedure TFMXSearchTextDialog.btnFindNextClick(Sender: TObject);
begin
  FSearchOptions := GetOptions;
  ModalResult := mrOk;
end;

procedure TFMXSearchTextDialog.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFMXSearchTextDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

function TFMXSearchTextDialog.GetSearchText: string;
begin
  Result := edtSearch.Text;
end;

function TFMXSearchTextDialog.GetOptions: TSynSearchOptions;
begin
  Result := [];
  if cbMatchCase.IsChecked then
    Include(Result, ssoMatchCase);
  if cbWholeWord.IsChecked then
    Include(Result, ssoWholeWord);
  if rbBackward.IsChecked then
    Include(Result, ssoBackwards);
end;

end.
