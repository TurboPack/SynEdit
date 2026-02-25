{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Replace Dialog Demo
-------------------------------------------------------------------------------}
unit dlgFMXReplaceText;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.Controls.Presentation,
  SynEditTypes;

type
  TFMXReplaceTextDialog = class(TForm)
    lblSearch: TLabel;
    edtSearch: TEdit;
    lblReplace: TLabel;
    edtReplace: TEdit;
    cbMatchCase: TCheckBox;
    cbWholeWord: TCheckBox;
    cbRegex: TCheckBox;
    rbForward: TRadioButton;
    rbBackward: TRadioButton;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    btnClose: TButton;
    grpOptions: TGroupBox;
    grpDirection: TGroupBox;
    procedure btnReplaceClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSearchOptions: TSynSearchOptions;
  public
    property SearchOptions: TSynSearchOptions read FSearchOptions;
    function GetSearchText: string;
    function GetReplaceText: string;
    function GetOptions: TSynSearchOptions;
  end;

implementation

{$R *.fmx}

procedure TFMXReplaceTextDialog.btnReplaceClick(Sender: TObject);
begin
  FSearchOptions := GetOptions;
  Include(FSearchOptions, ssoReplace);
  ModalResult := mrOk;
end;

procedure TFMXReplaceTextDialog.btnReplaceAllClick(Sender: TObject);
begin
  FSearchOptions := GetOptions;
  Include(FSearchOptions, ssoReplaceAll);
  ModalResult := mrAll;
end;

procedure TFMXReplaceTextDialog.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFMXReplaceTextDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

function TFMXReplaceTextDialog.GetSearchText: string;
begin
  Result := edtSearch.Text;
end;

function TFMXReplaceTextDialog.GetReplaceText: string;
begin
  Result := edtReplace.Text;
end;

function TFMXReplaceTextDialog.GetOptions: TSynSearchOptions;
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
