unit SynDelphiIDEImporter;

interface

uses
  Classes, SysUtils, Vcl.Dialogs, Vcl.Graphics, System.Win.Registry,
  DesignIntf, DesignEditors, ToolsAPI,
  SynEdit, SynHighlighterDelphi, SynEditHighlighter, SynEditTypes;

type
  TSynIDEImportEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  Windows;

// =============================================================================
//   Helpers
// =============================================================================

function GetIDEBaseRegistryKey: string;
var
  Services: IOTAServices;
begin
  if Supports(BorlandIDEServices, IOTAServices, Services) then
    Result := Services.GetBaseRegistryKey
  else
    Result := '';
end;

function ReadStrBool(Reg: TRegistry; const Name: string; Default: Boolean): Boolean;
var
  S: string;
begin
  Result := Default;
  if Reg.ValueExists(Name) then
  begin
    S := Reg.ReadString(Name);
    if SameText(S, 'True') then Result := True
    else if SameText(S, 'False') then Result := False;
  end;
end;

function ReadStrColor(Reg: TRegistry; const Name: string; Default: TColor): TColor;
begin
  Result := Default;
  if Reg.ValueExists(Name) then
  begin
    try
      Result := StringToColor(Reg.ReadString(Name));
    except
      Result := Default;
    end;
  end;
end;

// =============================================================================
//   Importers
// =============================================================================

procedure ImportToSynEdit(SynEdit: TSynEdit);
var
  Reg: TRegistry;
  BaseKey: string;
  RightMarginVisible: Boolean;
  TempOptions: TSynEditorOptions;
  TempScrollOptions: TSynEditorScrollOptions;
  
  procedure UpdateOption(Enable: Boolean; Opt: TSynEditorOption);
  begin
    if Enable then
      TempOptions := TempOptions + [Opt]
    else
      TempOptions := TempOptions - [Opt];
  end;

  procedure UpdateScrollOption(Enable: Boolean; Opt: TSynEditorScrollOption);
  begin
    if Enable then
      TempScrollOptions := TempScrollOptions + [Opt]
    else
      TempScrollOptions := TempScrollOptions - [Opt];
  end;

begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    BaseKey := GetIDEBaseRegistryKey;
    if BaseKey = '' then Exit;

    // 1. Editor Options
    if Reg.OpenKeyReadOnly(BaseKey + '\Editor\Options') then
    begin
      // Font
      if Reg.ValueExists('Editor Font') then
        SynEdit.Font.Name := Reg.ReadString('Editor Font');

      if Reg.ValueExists('Font Size') then
        SynEdit.Font.Size := Reg.ReadInteger('Font Size');

      // Insert Mode
      if Reg.ValueExists('Insert') then
        SynEdit.InsertMode := ReadStrBool(Reg, 'Insert', True);

      // Gutter
      if Reg.ValueExists('Visible Gutter') then
        SynEdit.Gutter.Visible := ReadStrBool(Reg, 'Visible Gutter', True);
        
      if Reg.ValueExists('Show Line Numbers') then
        SynEdit.Gutter.ShowLineNumbers := ReadStrBool(Reg, 'Show Line Numbers', True);

      // Right Edge
      RightMarginVisible := True;
      if Reg.ValueExists('Visible Right Margin') then
        RightMarginVisible := ReadStrBool(Reg, 'Visible Right Margin', True);

      if RightMarginVisible then
      begin
        if Reg.ValueExists('Right Margin') then
          SynEdit.RightEdge := Reg.ReadInteger('Right Margin');
      end
      else
        SynEdit.RightEdge := 0;

      // -- TSynEditorOptions --
      TempOptions := SynEdit.Options;
      
      if Reg.ValueExists('Group Undo') then
        UpdateOption(ReadStrBool(Reg, 'Group Undo', True), eoGroupUndo);

      if Reg.ValueExists('Highlight Brace Pairs') then
        UpdateOption(ReadStrBool(Reg, 'Highlight Brace Pairs', True), eoBracketsHighlight);
        
      if Reg.ValueExists('Drop Files') then
        UpdateOption(ReadStrBool(Reg, 'Drop Files', True), eoDropFiles);

      SynEdit.Options := TempOptions;

      // -- TSynEditorScrollOptions --
      TempScrollOptions := SynEdit.ScrollOptions;

      // "Cursor Beyond EOF" typically allows placing caret anywhere (virtual space)
      if Reg.ValueExists('Cursor Beyond EOF') then
      begin
        // In SynEdit, this is split into two properties:
        // eoScrollPastEol (past end of line) and eoScrollPastEof (past end of file)
        // Enabling both mimics Delphi's "Cursor Beyond EOF" behavior best.
        UpdateScrollOption(ReadStrBool(Reg, 'Cursor Beyond EOF', False), eoScrollPastEol);
        UpdateScrollOption(ReadStrBool(Reg, 'Cursor Beyond EOF', False), eoScrollPastEof);
      end;

      SynEdit.ScrollOptions := TempScrollOptions;

      Reg.CloseKey;
    end;

    // 2. Pascal Source Options (TabWidth, AutoIndent, etc)
    if Reg.OpenKeyReadOnly(BaseKey + '\Editor\Source Options\Borland.EditOptions.Pascal') then
    begin
      // Tab Width
      if Reg.ValueExists('Tab Stops') then
        // Sometimes stored as string "4" or "2"
        SynEdit.TabWidth := StrToIntDef(Reg.ReadString('Tab Stops'), 4);
   

      // Options: AutoIndent, SmartTabs, TabsToSpaces
      TempOptions := SynEdit.Options;
      
      if Reg.ValueExists('Auto Indent') then
        UpdateOption(ReadStrBool(Reg, 'Auto Indent', True), eoAutoIndent);
        
      if Reg.ValueExists('Smart Tab') then
        UpdateOption(ReadStrBool(Reg, 'Smart Tab', False), eoSmartTabs);
        
      // "Tab Character" = True means use #9. False means spaces.
      // eoTabsToSpaces = True means use spaces.
      // So if "Tab Character" is True, eoTabsToSpaces should be False.
      if Reg.ValueExists('Tab Character') then
        UpdateOption(not ReadStrBool(Reg, 'Tab Character', False), eoTabsToSpaces);

      SynEdit.Options := TempOptions;
      
      Reg.CloseKey;
    end;

    // 3. Colors (Background, Active Line, Right Margin)
    // Map 'Whitespace' background to SynEdit.Color
    if Reg.OpenKeyReadOnly(BaseKey + '\Editor\Highlight\Whitespace') then
    begin
      SynEdit.Color := ReadStrColor(Reg, 'Background Color New', clWindow);
      Reg.CloseKey;
    end;

    // Active Line Color
    if Reg.OpenKeyReadOnly(BaseKey + '\Editor\Highlight\Line Highlight') then
    begin
      // SynEdit ActiveLineColor
      SynEdit.ActiveLineColor := ReadStrColor(Reg, 'Background Color New', clNone);
      Reg.CloseKey;
    end;
    
    // Right Margin Color
    if Reg.OpenKeyReadOnly(BaseKey + '\Editor\Highlight\Right margin') then
    begin
      SynEdit.RightEdgeColor := ReadStrColor(Reg, 'Foreground Color New', clSilver);
      Reg.CloseKey;
    end;

    // Gutter Colors (From "Line Number")
    if Reg.OpenKeyReadOnly(BaseKey + '\Editor\Highlight\Line Number') then
    begin
      // Background Color New -> Gutter Background
      SynEdit.Gutter.Color := ReadStrColor(Reg, 'Background Color New', clBtnFace);
      // Foreground Color New -> Gutter Font Color
      SynEdit.Gutter.Font.Color := ReadStrColor(Reg, 'Foreground Color New', clWindowText);
      Reg.CloseKey;
    end;

    // Selection Colors (From "Marked block")
    if Reg.OpenKeyReadOnly(BaseKey + '\Editor\Highlight\Marked block') then
    begin
      SynEdit.SelectedColor.Background := ReadStrColor(Reg, 'Background Color New', clHighlight);
      SynEdit.SelectedColor.Foreground := ReadStrColor(Reg, 'Foreground Color New', clHighlightText);
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure ImportToHighlighter(Syn: TSynDelphiSyn);
var
  Reg: TRegistry;
  BaseKey: string;

  procedure ImportAttri(const IDEName: string; Attri: TSynHighlighterAttributes);
  var
    Key: string;
  begin
    Key := BaseKey + '\Editor\Highlight\' + IDEName;
    if Reg.OpenKeyReadOnly(Key) then
    begin
      try
        // Colors
        Attri.Foreground := ReadStrColor(Reg, 'Foreground Color New', clWindowText);
        Attri.Background := ReadStrColor(Reg, 'Background Color New', clNone);

        // Styles - Using direct set addition/subtraction to fix E2064 error
        // Include() cannot be used on properties directly
        Attri.Style := [];
        if ReadStrBool(Reg, 'Bold', False) then 
          Attri.Style := Attri.Style + [fsBold];
          
        if ReadStrBool(Reg, 'Italic', False) then 
          Attri.Style := Attri.Style + [fsItalic];
          
        if ReadStrBool(Reg, 'Underline', False) then 
          Attri.Style := Attri.Style + [fsUnderline];
      finally
        Reg.CloseKey;
      end;
    end;
  end;

begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    BaseKey := GetIDEBaseRegistryKey;
    if BaseKey = '' then Exit;

    // Map SynDelphiSyn properties to IDE Registry Keys
    ImportAttri('Assembler', Syn.AsmAttri);
    ImportAttri('Comment', Syn.CommentAttri);
    ImportAttri('Preprocessor', Syn.DirectiveAttri);
    ImportAttri('Identifier', Syn.IdentifierAttri);
    ImportAttri('Reserved word', Syn.KeyAttri);
    ImportAttri('Number', Syn.NumberAttri);
    ImportAttri('Float', Syn.FloatAttri);
    ImportAttri('Hex', Syn.HexAttri);
    ImportAttri('Whitespace', Syn.SpaceAttri);
    ImportAttri('String', Syn.StringAttri);
    ImportAttri('Character', Syn.CharAttri);
    ImportAttri('Symbol', Syn.SymbolAttri);

  finally
    Reg.Free;
  end;
end;

// =============================================================================
//   Editor Integration
// =============================================================================

procedure TSynIDEImportEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
  begin
    if Component is TSynEdit then
    begin
      ImportToSynEdit(TSynEdit(Component));
      ShowMessage('Imported Editor Options (Font, Gutter, Selection, Tabs) from IDE.');
    end
    else if Component is TSynDelphiSyn then
    begin
      ImportToHighlighter(TSynDelphiSyn(Component));
      ShowMessage('Imported Syntax Colors from IDE.');
    end;
    
    if Assigned(Designer) then Designer.Modified;
  end;
end;

function TSynIDEImportEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Import IDE Settings';
end;

function TSynIDEImportEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponentEditor(TSynEdit, TSynIDEImportEditor);
  RegisterComponentEditor(TSynDelphiSyn, TSynIDEImportEditor);
end;

end.
