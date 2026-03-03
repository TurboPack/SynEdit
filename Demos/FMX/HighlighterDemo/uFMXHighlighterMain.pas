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
  SynEditTypes,
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

var
  Delphi: TSynDelphiSyn;
  Cpp: TSynCppSyn;
  Java: TSynJavaSyn;
  Python: TSynPythonSyn;
  JS: TSynJScriptSyn;
  HTML: TSynHTMLSyn;
  XML: TSynXMLSyn;
  CSS: TSynCssSyn;
  SQL: TSynSQLSyn;
  JSON: TSynJSONSyn;
  CS: TSynCSSyn;
  Ini: TSynIniSyn;
  Bat: TSynBatSyn;
begin
  // Delphi
  Delphi := TSynDelphiSyn.Create(Self);
  Delphi.KeyAttri.Foreground := clNavy;
  Delphi.KeyAttri.Style := [fsBold];
  Delphi.CommentAttri.Foreground := clGreen;
  Delphi.CommentAttri.Style := [fsItalic];
  Delphi.StringAttri.Foreground := clBlue;
  Delphi.NumberAttri.Foreground := clBlue;
  Delphi.FloatAttri.Foreground := clBlue;
  Delphi.HexAttri.Foreground := clBlue;
  Delphi.CharAttri.Foreground := clBlue;
  Delphi.DirectiveAttri.Foreground := clTeal;
  Delphi.DirectiveAttri.Style := [fsItalic];
  AddHL(Delphi);

  // C++
  Cpp := TSynCppSyn.Create(Self);
  Cpp.KeyAttri.Foreground := clNavy;
  Cpp.KeyAttri.Style := [fsBold];
  Cpp.CommentAttri.Foreground := clGreen;
  Cpp.CommentAttri.Style := [fsItalic];
  Cpp.StringAttri.Foreground := clBlue;
  Cpp.NumberAttri.Foreground := clBlue;
  Cpp.FloatAttri.Foreground := clBlue;
  Cpp.HexAttri.Foreground := clBlue;
  Cpp.CharAttri.Foreground := clBlue;
  Cpp.DirecAttri.Foreground := clTeal;
  AddHL(Cpp);

  // Java
  Java := TSynJavaSyn.Create(Self);
  Java.KeyAttri.Foreground := clNavy;
  Java.KeyAttri.Style := [fsBold];
  Java.CommentAttri.Foreground := clGreen;
  Java.CommentAttri.Style := [fsItalic];
  Java.DocumentAttri.Foreground := clGreen;
  Java.DocumentAttri.Style := [fsItalic];
  Java.StringAttri.Foreground := clBlue;
  Java.NumberAttri.Foreground := clBlue;
  AddHL(Java);

  // Python
  Python := TSynPythonSyn.Create(Self);
  Python.KeyAttri.Foreground := clNavy;
  Python.KeyAttri.Style := [fsBold];
  Python.NonKeyAttri.Foreground := clTeal;
  Python.SystemAttri.Foreground := clTeal;
  Python.CommentAttri.Foreground := clGreen;
  Python.CommentAttri.Style := [fsItalic];
  Python.DocStringAttri.Foreground := clGreen;
  Python.DocStringAttri.Style := [fsItalic];
  Python.StringAttri.Foreground := clBlue;
  Python.NumberAttri.Foreground := clBlue;
  Python.FloatAttri.Foreground := clBlue;
  Python.HexAttri.Foreground := clBlue;
  Python.OctalAttri.Foreground := clBlue;
  AddHL(Python);

  // JavaScript
  JS := TSynJScriptSyn.Create(Self);
  JS.KeyAttri.Foreground := clNavy;
  JS.KeyAttri.Style := [fsBold];
  JS.NonReservedKeyAttri.Foreground := clTeal;
  JS.EventAttri.Foreground := clTeal;
  JS.CommentAttri.Foreground := clGreen;
  JS.CommentAttri.Style := [fsItalic];
  JS.StringAttri.Foreground := clBlue;
  JS.NumberAttri.Foreground := clBlue;
  AddHL(JS);

  // HTML
  HTML := TSynHTMLSyn.Create(Self);
  HTML.KeyAttri.Foreground := clMaroon;
  HTML.KeyAttri.Style := [fsBold];
  HTML.UndefKeyAttri.Foreground := clMaroon;
  HTML.CommentAttri.Foreground := clGreen;
  HTML.CommentAttri.Style := [fsItalic];
  HTML.IdentifierAttri.Foreground := clRed;
  HTML.ValueAttri.Foreground := clBlue;
  HTML.AndAttri.Foreground := clTeal;
  AddHL(HTML);

  // XML
  XML := TSynXMLSyn.Create(Self);
  XML.ElementAttri.Foreground := clMaroon;
  XML.AttributeAttri.Foreground := clRed;
  XML.NamespaceAttributeAttri.Foreground := clRed;
  XML.AttributeValueAttri.Foreground := clBlue;
  XML.NamespaceAttributeValueAttri.Foreground := clBlue;
  XML.CommentAttri.Foreground := clGreen;
  XML.CommentAttri.Style := [fsItalic];
  XML.CDATAAttri.Foreground := clTeal;
  XML.EntityRefAttri.Foreground := clTeal;
  XML.ProcessingInstructionAttri.Foreground := clTeal;
  XML.DocTypeAttri.Foreground := clNavy;
  AddHL(XML);

  // CSS
  CSS := TSynCssSyn.Create(Self);
  CSS.SelectorAttri.Foreground := clNavy;
  CSS.SelectorAttri.Style := [fsBold];
  CSS.PropertyAttri.Foreground := clMaroon;
  CSS.ValueAttri.Foreground := clBlue;
  CSS.ColorAttri.Foreground := clBlue;
  CSS.NumberAttri.Foreground := clBlue;
  CSS.StringAttri.Foreground := clBlue;
  CSS.CommentAttri.Foreground := clGreen;
  CSS.CommentAttri.Style := [fsItalic];
  CSS.AtRuleAttri.Foreground := clTeal;
  AddHL(CSS);

  // SQL
  SQL := TSynSQLSyn.Create(Self);
  SQL.KeyAttri.Foreground := clNavy;
  SQL.KeyAttri.Style := [fsBold];
  SQL.CommentAttri.Foreground := clGreen;
  SQL.CommentAttri.Style := [fsItalic];
  SQL.StringAttri.Foreground := clBlue;
  SQL.NumberAttri.Foreground := clBlue;
  SQL.DataTypeAttri.Foreground := clTeal;
  SQL.DataTypeAttri.Style := [fsBold];
  SQL.FunctionAttri.Foreground := clTeal;
  SQL.TableNameAttri.Foreground := clMaroon;
  SQL.VariableAttri.Foreground := clMaroon;
  AddHL(SQL);

  // JSON
  JSON := TSynJSONSyn.Create(Self);
  JSON.AttributeAttri.Foreground := clMaroon;
  JSON.ValueAttri.Foreground := clBlue;
  JSON.NumberAttri.Foreground := clBlue;
  JSON.ReservedAttri.Foreground := clNavy;
  JSON.ReservedAttri.Style := [fsBold];
  AddHL(JSON);

  // C#
  CS := TSynCSSyn.Create(Self);
  CS.KeyAttri.Foreground := clNavy;
  CS.KeyAttri.Style := [fsBold];
  CS.CommentAttri.Foreground := clGreen;
  CS.CommentAttri.Style := [fsItalic];
  CS.StringAttri.Foreground := clBlue;
  CS.NumberAttri.Foreground := clBlue;
  CS.DirecAttri.Foreground := clTeal;
  CS.TypeAttri.Foreground := clTeal;
  AddHL(CS);

  // INI
  Ini := TSynIniSyn.Create(Self);
  Ini.SectionAttri.Foreground := clNavy;
  Ini.SectionAttri.Style := [fsBold];
  Ini.KeyAttri.Foreground := clMaroon;
  Ini.CommentAttri.Foreground := clGreen;
  Ini.CommentAttri.Style := [fsItalic];
  Ini.StringAttri.Foreground := clBlue;
  Ini.NumberAttri.Foreground := clBlue;
  AddHL(Ini);

  // Batch
  Bat := TSynBatSyn.Create(Self);
  Bat.KeyAttri.Foreground := clNavy;
  Bat.KeyAttri.Style := [fsBold];
  Bat.CommentAttri.Foreground := clGreen;
  Bat.CommentAttri.Style := [fsItalic];
  Bat.NumberAttri.Foreground := clBlue;
  Bat.VariableAttri.Foreground := clMaroon;
  AddHL(Bat);
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
