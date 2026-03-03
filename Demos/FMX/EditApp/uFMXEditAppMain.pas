{-------------------------------------------------------------------------------
SynEdit FMX - EditApp Demo

A single-document text editor demonstrating FMX SynEdit with menus, file I/O,
clipboard operations, undo/redo, and automatic highlighter detection.
-------------------------------------------------------------------------------}

unit uFMXEditAppMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Menus,
  FMX.Dialogs, FMX.Controls.Presentation,
  SynEdit,
  SynEditHighlighter;

type
  TFMXEditAppForm = class(TForm)
    MenuBar1: TMenuBar;
    MenuFile: TMenuItem;
    MenuNew: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    MenuSep1: TMenuItem;
    MenuExit: TMenuItem;
    MenuEdit: TMenuItem;
    MenuUndo: TMenuItem;
    MenuRedo: TMenuItem;
    MenuSep2: TMenuItem;
    MenuCut: TMenuItem;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    MenuSep3: TMenuItem;
    MenuSelectAll: TMenuItem;
    PanelStatus: TPanel;
    LabelPos: TLabel;
    LabelInsert: TLabel;
    LabelModified: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuCutClick(Sender: TObject);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
  private
    FEditor: TFMXSynEdit;
    FFileName: string;
    FHighlighters: TStringList;
    procedure CreateHighlighters;
    procedure EditorChange(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject);
    procedure UpdateCaption;
    procedure UpdateStatusBar;
    procedure SetHighlighterForFile(const AFileName: string);
  end;

var
  FMXEditAppForm: TFMXEditAppForm;

implementation

{$R *.fmx}

uses
  SynEditTypes,
  uHighlighterProcs,
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

procedure TFMXEditAppForm.FormCreate(Sender: TObject);
begin
  FFileName := '';
  FHighlighters := TStringList.Create;

  // Create highlighters (owned by Self so GetHighlighters can find them)
  CreateHighlighters;

  // Build highlighter list for file extension mapping
  GetHighlighters(Self, FHighlighters, False);

  // Build Open/Save dialog filter from highlighters
  OpenDialog1.Filter := GetHighlightersFilter(FHighlighters) +
    'All Files (*.*)|*.*';
  SaveDialog1.Filter := OpenDialog1.Filter;

  // Create editor
  FEditor := TFMXSynEdit.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := TAlignLayout.Client;
  FEditor.Font.Family := 'Consolas';
  FEditor.Font.Size := 12;
  FEditor.TabWidth := 2;
  FEditor.OnChange := EditorChange;
  FEditor.OnStatusChange := EditorStatusChange;

  UpdateCaption;
  UpdateStatusBar;
end;

procedure TFMXEditAppForm.CreateHighlighters;
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

  // JSON
  JSON := TSynJSONSyn.Create(Self);
  JSON.AttributeAttri.Foreground := clMaroon;
  JSON.ValueAttri.Foreground := clBlue;
  JSON.NumberAttri.Foreground := clBlue;
  JSON.ReservedAttri.Foreground := clNavy;
  JSON.ReservedAttri.Style := [fsBold];

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

  // INI
  Ini := TSynIniSyn.Create(Self);
  Ini.SectionAttri.Foreground := clNavy;
  Ini.SectionAttri.Style := [fsBold];
  Ini.KeyAttri.Foreground := clMaroon;
  Ini.CommentAttri.Foreground := clGreen;
  Ini.CommentAttri.Style := [fsItalic];
  Ini.StringAttri.Foreground := clBlue;
  Ini.NumberAttri.Foreground := clBlue;

  // Batch
  Bat := TSynBatSyn.Create(Self);
  Bat.KeyAttri.Foreground := clNavy;
  Bat.KeyAttri.Style := [fsBold];
  Bat.CommentAttri.Foreground := clGreen;
  Bat.CommentAttri.Style := [fsItalic];
  Bat.NumberAttri.Foreground := clBlue;
  Bat.VariableAttri.Foreground := clMaroon;
end;

procedure TFMXEditAppForm.EditorChange(Sender: TObject);
begin
  UpdateCaption;
  UpdateStatusBar;
end;

procedure TFMXEditAppForm.EditorStatusChange(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TFMXEditAppForm.UpdateCaption;
var
  Name: string;
begin
  if FFileName <> '' then
    Name := ExtractFileName(FFileName)
  else
    Name := 'Untitled';

  if FEditor.Modified then
    Caption := Name + '* - SynEdit FMX'
  else
    Caption := Name + ' - SynEdit FMX';
end;

procedure TFMXEditAppForm.UpdateStatusBar;
begin
  LabelPos.Text := Format('Ln: %d  Col: %d', [FEditor.CaretY, FEditor.CaretX]);

  if FEditor.InsertMode then
    LabelInsert.Text := 'Insert'
  else
    LabelInsert.Text := 'Overwrite';

  if FEditor.Modified then
    LabelModified.Text := 'Modified'
  else
    LabelModified.Text := '';
end;

procedure TFMXEditAppForm.SetHighlighterForFile(const AFileName: string);
var
  Ext: string;
  HL: TSynCustomHighlighter;
begin
  Ext := ExtractFileExt(AFileName);
  HL := GetHighlighterFromFileExt(FHighlighters, Ext);
  FEditor.Highlighter := HL;
end;

// --- File menu ---

procedure TFMXEditAppForm.MenuNewClick(Sender: TObject);
begin
  FEditor.ClearAll;
  FEditor.Highlighter := nil;
  FFileName := '';
  UpdateCaption;
  UpdateStatusBar;
end;

procedure TFMXEditAppForm.MenuOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FFileName := OpenDialog1.FileName;
    FEditor.LoadFromFile(FFileName);
    SetHighlighterForFile(FFileName);
    UpdateCaption;
    UpdateStatusBar;
  end;
end;

procedure TFMXEditAppForm.MenuSaveClick(Sender: TObject);
begin
  if FFileName <> '' then
  begin
    FEditor.SaveToFile(FFileName);
    UpdateCaption;
  end
  else
    MenuSaveAsClick(Sender);
end;

procedure TFMXEditAppForm.MenuSaveAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FFileName := SaveDialog1.FileName;
    FEditor.SaveToFile(FFileName);
    SetHighlighterForFile(FFileName);
    UpdateCaption;
  end;
end;

procedure TFMXEditAppForm.MenuExitClick(Sender: TObject);
begin
  Close;
end;

// --- Edit menu ---

procedure TFMXEditAppForm.MenuUndoClick(Sender: TObject);
begin
  FEditor.Undo;
end;

procedure TFMXEditAppForm.MenuRedoClick(Sender: TObject);
begin
  FEditor.Redo;
end;

procedure TFMXEditAppForm.MenuCutClick(Sender: TObject);
begin
  FEditor.CutToClipboard;
end;

procedure TFMXEditAppForm.MenuCopyClick(Sender: TObject);
begin
  FEditor.CopyToClipboard;
end;

procedure TFMXEditAppForm.MenuPasteClick(Sender: TObject);
begin
  FEditor.PasteFromClipboard;
end;

procedure TFMXEditAppForm.MenuSelectAllClick(Sender: TObject);
begin
  FEditor.SelectAll;
end;

end.
