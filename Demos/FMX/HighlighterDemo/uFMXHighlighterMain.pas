{-------------------------------------------------------------------------------
SynEdit FMX - Highlighter Demo

Demonstrates the FMX SynEdit component with various syntax highlighters.
Select a highlighter from the dropdown to see its sample source rendered.
-------------------------------------------------------------------------------}

unit uFMXHighlighterMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation,
  SynEdit,
  SynEditHighlighter;

type
  TFMXHighlighterForm = class(TForm)
    PanelTop: TPanel;
    LabelHL: TLabel;
    ComboBoxHL: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxHLChange(Sender: TObject);
  private
    FEditor: TFMXSynEdit;
    FHighlighters: TList;
    procedure PopulateHighlighterList;
  end;

var
  FMXHighlighterForm: TFMXHighlighterForm;

implementation

{$R *.fmx}

uses
  SynHighlighterDelphi,
  SynHighlighterCpp,
  SynHighlighterJava,
  SynHighlighterPython,
  SynHighlighterJScript,
  SynHighlighterHtml,
  SynHighlighterXML,
  SynHighlighterCSS,
  SynHighlighterSQL,
  SynHighlighterJSON,
  SynHighlighterCS,
  SynHighlighterIni,
  SynHighlighterBat;

procedure TFMXHighlighterForm.FormCreate(Sender: TObject);
begin
  FHighlighters := TList.Create;

  // Create editor programmatically
  FEditor := TFMXSynEdit.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := TAlignLayout.Client;
  FEditor.Font.Family := 'Consolas';
  FEditor.Font.Size := 12;
  FEditor.TabWidth := 2;

  PopulateHighlighterList;

  // Select first highlighter
  if ComboBoxHL.Count > 0 then
  begin
    ComboBoxHL.ItemIndex := 0;
    ComboBoxHLChange(nil);
  end;
end;

procedure TFMXHighlighterForm.PopulateHighlighterList;

  procedure AddHL(HL: TSynCustomHighlighter);
  begin
    FHighlighters.Add(HL);
    ComboBoxHL.Items.Add(HL.FriendlyLanguageName);
  end;

begin
  AddHL(TSynDelphiSyn.Create(Self));
  AddHL(TSynCppSyn.Create(Self));
  AddHL(TSynJavaSyn.Create(Self));
  AddHL(TSynPythonSyn.Create(Self));
  AddHL(TSynJScriptSyn.Create(Self));
  AddHL(TSynHTMLSyn.Create(Self));
  AddHL(TSynXMLSyn.Create(Self));
  AddHL(TSynCssSyn.Create(Self));
  AddHL(TSynSQLSyn.Create(Self));
  AddHL(TSynJSONSyn.Create(Self));
  AddHL(TSynCSSyn.Create(Self));
  AddHL(TSynIniSyn.Create(Self));
  AddHL(TSynBatSyn.Create(Self));
end;

procedure TFMXHighlighterForm.ComboBoxHLChange(Sender: TObject);
var
  HL: TSynCustomHighlighter;
begin
  if (ComboBoxHL.ItemIndex < 0) or
    (ComboBoxHL.ItemIndex >= FHighlighters.Count)
  then
    Exit;

  HL := TSynCustomHighlighter(FHighlighters[ComboBoxHL.ItemIndex]);
  FEditor.Highlighter := HL;
  FEditor.Text := HL.SampleSource;
end;

end.
