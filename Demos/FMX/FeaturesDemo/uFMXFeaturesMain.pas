{-------------------------------------------------------------------------------
SynEdit FMX - Features Demo

A comprehensive feature showcase demonstrating all configurable properties and
APIs of the FMX SynEdit editor: clipboard operations, editor options, appearance
settings, and status reporting.
-------------------------------------------------------------------------------}

unit uFMXFeaturesMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.Memo, FMX.Edit,
  FMX.SpinBox, FMX.Colors,
  SynEdit,
  SynEditHighlighter;

type
  TFMXFeaturesForm = class(TForm)
    PanelLeft: TPanel;
    ScrollBox1: TVertScrollBox;
    LabelHLCaption: TLabel;
    ComboHL: TComboBox;
    BtnLoadSample: TButton;
    LabelClipboard: TLabel;
    BtnCut: TButton;
    BtnCopy: TButton;
    BtnPaste: TButton;
    BtnUndo: TButton;
    BtnRedo: TButton;
    BtnSelectAll: TButton;
    BtnClear: TButton;
    LabelOptions: TLabel;
    ChkAutoIndent: TCheckBox;
    ChkInsertMode: TCheckBox;
    ChkReadOnly: TCheckBox;
    ChkTabsToSpaces: TCheckBox;
    ChkSmartTabs: TCheckBox;
    LabelTabWidth: TLabel;
    SpinTabWidth: TSpinBox;
    LabelAppearance: TLabel;
    LabelRightEdge: TLabel;
    SpinRightEdge: TSpinBox;
    LabelActiveColor: TLabel;
    ComboActiveColor: TComboColorBox;
    LabelStatus: TLabel;
    LabelPosInfo: TLabel;
    LabelLinesInfo: TLabel;
    LabelModInfo: TLabel;
    Splitter1: TSplitter;
    MemoLog: TMemo;
    SplitterBottom: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ComboHLChange(Sender: TObject);
    procedure BtnLoadSampleClick(Sender: TObject);
    procedure BtnCutClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnPasteClick(Sender: TObject);
    procedure BtnUndoClick(Sender: TObject);
    procedure BtnRedoClick(Sender: TObject);
    procedure BtnSelectAllClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure ChkOptionChange(Sender: TObject);
    procedure ChkInsertModeChange(Sender: TObject);
    procedure ChkReadOnlyChange(Sender: TObject);
    procedure SpinTabWidthChange(Sender: TObject);
    procedure SpinRightEdgeChange(Sender: TObject);
    procedure ComboActiveColorChange(Sender: TObject);
  private
    FEditor: TFMXSynEdit;
    FHighlighters: TList;
    FUpdatingControls: Boolean;
    procedure CreateHighlighters;
    procedure EditorChange(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject);
    procedure UpdateStatusLabels;
    procedure LogEvent(const Msg: string);
  end;

var
  FMXFeaturesForm: TFMXFeaturesForm;

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

procedure TFMXFeaturesForm.FormCreate(Sender: TObject);
begin
  FHighlighters := TList.Create;
  FUpdatingControls := False;

  // Create highlighters
  CreateHighlighters;

  // Create editor programmatically in the client area
  FEditor := TFMXSynEdit.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := TAlignLayout.Client;
  FEditor.Font.Family := 'Consolas';
  FEditor.Font.Size := 12;
  FEditor.TabWidth := 2;
  FEditor.OnChange := EditorChange;
  FEditor.OnStatusChange := EditorStatusChange;

  // Load first highlighter's sample
  if ComboHL.Count > 0 then
  begin
    ComboHL.ItemIndex := 0;
    ComboHLChange(nil);
    BtnLoadSampleClick(nil);
  end;

  UpdateStatusLabels;
end;

procedure TFMXFeaturesForm.CreateHighlighters;

  procedure AddHL(HL: TSynCustomHighlighter);
  begin
    FHighlighters.Add(HL);
    ComboHL.Items.Add(HL.FriendlyLanguageName);
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

// --- Highlighter ---

procedure TFMXFeaturesForm.ComboHLChange(Sender: TObject);
var
  HL: TSynCustomHighlighter;
begin
  if (ComboHL.ItemIndex < 0) or (ComboHL.ItemIndex >= FHighlighters.Count) then
    Exit;
  HL := TSynCustomHighlighter(FHighlighters[ComboHL.ItemIndex]);
  FEditor.Highlighter := HL;
  LogEvent('Highlighter: ' + HL.FriendlyLanguageName);
end;

procedure TFMXFeaturesForm.BtnLoadSampleClick(Sender: TObject);
var
  HL: TSynCustomHighlighter;
begin
  if (ComboHL.ItemIndex < 0) or (ComboHL.ItemIndex >= FHighlighters.Count) then
    Exit;
  HL := TSynCustomHighlighter(FHighlighters[ComboHL.ItemIndex]);
  FEditor.Highlighter := HL;
  FEditor.Text := HL.SampleSource;
  LogEvent('Loaded sample: ' + HL.FriendlyLanguageName);
end;

// --- Clipboard ---

procedure TFMXFeaturesForm.BtnCutClick(Sender: TObject);
begin
  FEditor.CutToClipboard;
  LogEvent('Cut');
end;

procedure TFMXFeaturesForm.BtnCopyClick(Sender: TObject);
begin
  FEditor.CopyToClipboard;
  LogEvent('Copy');
end;

procedure TFMXFeaturesForm.BtnPasteClick(Sender: TObject);
begin
  FEditor.PasteFromClipboard;
  LogEvent('Paste');
end;

procedure TFMXFeaturesForm.BtnUndoClick(Sender: TObject);
begin
  FEditor.Undo;
  LogEvent('Undo');
end;

procedure TFMXFeaturesForm.BtnRedoClick(Sender: TObject);
begin
  FEditor.Redo;
  LogEvent('Redo');
end;

procedure TFMXFeaturesForm.BtnSelectAllClick(Sender: TObject);
begin
  FEditor.SelectAll;
  LogEvent('Select All');
end;

procedure TFMXFeaturesForm.BtnClearClick(Sender: TObject);
begin
  FEditor.ClearAll;
  LogEvent('Clear All');
end;

// --- Options ---

procedure TFMXFeaturesForm.ChkOptionChange(Sender: TObject);
var
  Opts: TSynEditorOptions;
begin
  if FUpdatingControls then Exit;

  Opts := FEditor.Options;

  if ChkAutoIndent.IsChecked then
    Include(Opts, eoAutoIndent)
  else
    Exclude(Opts, eoAutoIndent);

  if ChkTabsToSpaces.IsChecked then
    Include(Opts, eoTabsToSpaces)
  else
    Exclude(Opts, eoTabsToSpaces);

  if ChkSmartTabs.IsChecked then
    Include(Opts, eoSmartTabs)
  else
    Exclude(Opts, eoSmartTabs);

  FEditor.Options := Opts;
  LogEvent('Options changed');
end;

procedure TFMXFeaturesForm.ChkInsertModeChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FEditor.InsertMode := ChkInsertMode.IsChecked;
  LogEvent('Insert mode: ' + BoolToStr(FEditor.InsertMode, True));
end;

procedure TFMXFeaturesForm.ChkReadOnlyChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FEditor.ReadOnly := ChkReadOnly.IsChecked;
  LogEvent('Read only: ' + BoolToStr(FEditor.ReadOnly, True));
end;

procedure TFMXFeaturesForm.SpinTabWidthChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FEditor.TabWidth := Round(SpinTabWidth.Value);
  LogEvent('Tab width: ' + IntToStr(FEditor.TabWidth));
end;

// --- Appearance ---

procedure TFMXFeaturesForm.SpinRightEdgeChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FEditor.RightEdge := Round(SpinRightEdge.Value);
  LogEvent('Right edge: ' + IntToStr(FEditor.RightEdge));
end;

procedure TFMXFeaturesForm.ComboActiveColorChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FEditor.ActiveLineColor := ComboActiveColor.Color;
  LogEvent('Active line color changed');
end;

// --- Status ---

procedure TFMXFeaturesForm.EditorChange(Sender: TObject);
begin
  UpdateStatusLabels;
  LogEvent('Changed');
end;

procedure TFMXFeaturesForm.EditorStatusChange(Sender: TObject);
begin
  UpdateStatusLabels;
end;

procedure TFMXFeaturesForm.UpdateStatusLabels;
begin
  LabelPosInfo.Text := Format('Line: %d  Col: %d', [FEditor.CaretY, FEditor.CaretX]);
  LabelLinesInfo.Text := Format('Lines: %d  Sel: %s',
    [FEditor.LineCount, BoolToStr(FEditor.SelAvail, True)]);

  if FEditor.Modified then
    LabelModInfo.Text := 'Modified: Yes'
  else
    LabelModInfo.Text := 'Modified: No';

  // Sync controls to editor state
  FUpdatingControls := True;
  try
    ChkInsertMode.IsChecked := FEditor.InsertMode;
    ChkReadOnly.IsChecked := FEditor.ReadOnly;
  finally
    FUpdatingControls := False;
  end;
end;

procedure TFMXFeaturesForm.LogEvent(const Msg: string);
begin
  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + '  ' + Msg);
  // Auto-scroll to bottom
  MemoLog.GoToTextEnd;
end;

end.
