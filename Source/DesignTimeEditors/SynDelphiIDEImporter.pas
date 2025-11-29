unit SynDelphiIDEImporter;

interface

uses
  Classes, SysUtils, Vcl.Dialogs, Vcl.Graphics, System.Win.Registry,
  DesignIntf, DesignEditors, SynEdit, SynHighlighterDelphi, SynEditHighlighter;

type
  { The Component Editor that adds the Context Menu item }
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

// Helper to get the Registry Base Key for the currently running IDE
function GetIDEBaseRegistryKey: string;
var
  ModName: array[0..MAX_PATH] of Char;
  AppStr: string;
begin
  // defaults to Delphi 12 (Athens) - 23.0
  Result := 'Software\Embarcadero\BDS\23.0';

  // Attempt to detect dynamic version based on running exe (optional polish)
  if GetModuleFileName(0, ModName, SizeOf(ModName)) > 0 then
  begin
    AppStr := ExtractFileName(StrPas(ModName));
    if SameText(AppStr, 'bds.exe') then
    begin
      // Logic to grab version could go here, but usually unnecessary
      // if you compile specifically for one IDE version.
    end;
  end;
end;

procedure ImportToSynEdit(SynEdit: TSynEdit);
var
  Reg: TRegistry;
  BaseKey: string;
begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    BaseKey := GetIDEBaseRegistryKey + '\Editor\Options';

    if Reg.OpenKeyReadOnly(BaseKey) then
    begin
      // Import Font
      if Reg.ValueExists('Editor Font') then
        SynEdit.Font.Name := Reg.ReadString('Editor Font');

      if Reg.ValueExists('Font Size') then
        SynEdit.Font.Size := Reg.ReadInteger('Font Size');

      // Import Gutter
      if Reg.ValueExists('GutterVisible') then
        SynEdit.Gutter.Visible := Reg.ReadBool('GutterVisible');

      // Import Right Edge
      if Reg.ValueExists('RightMarginVisible') and Reg.ReadBool('RightMarginVisible') then
        if Reg.ValueExists('RightMargin') then
          SynEdit.RightEdge := Reg.ReadInteger('RightMargin')
        else
          SynEdit.RightEdge := 0;

      Reg.CloseKey;
    end;

    // Import Editor Colors (Background, etc)
    BaseKey := GetIDEBaseRegistryKey + '\Editor\Highlight\Pascal';
    if Reg.OpenKeyReadOnly(BaseKey) then
    begin
       // Default Background
       if Reg.ValueExists('Background Color') then
         SynEdit.Color := StringToColor(Reg.ReadString('Background Color'));
    end;

  finally
    Reg.Free;
  end;
end;

procedure ImportToHighlighter(Syn: TSynDelphiSyn);
var
  Reg: TRegistry;
  BaseKey: string;

  procedure LoadAttri(const RegName: string; Attri: TSynHighlighterAttributes);
  var
    Prefix: string;
  begin
    Prefix := RegName;
    if Reg.ValueExists(Prefix + ' Color') then
      Attri.Foreground := StringToColor(Reg.ReadString(Prefix + ' Color'));

    if Reg.ValueExists(Prefix + ' BackColor') then
      Attri.Background := StringToColor(Reg.ReadString(Prefix + ' BackColor'));

    if Reg.ValueExists(Prefix + ' Bold') and Reg.ReadBool(Prefix + ' Bold') then
      Attri.Style := Attri.Style + [fsBold]
    else
      Attri.Style := Attri.Style - [fsBold];

    if Reg.ValueExists(Prefix + ' Italic') and Reg.ReadBool(Prefix + ' Italic') then
      Attri.Style := Attri.Style + [fsItalic]
    else
      Attri.Style := Attri.Style - [fsItalic];

    if Reg.ValueExists(Prefix + ' Underline') and Reg.ReadBool(Prefix + ' Underline') then
      Attri.Style := Attri.Style + [fsUnderline]
    else
      Attri.Style := Attri.Style - [fsUnderline];
  end;

begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    BaseKey := GetIDEBaseRegistryKey + '\Editor\Highlight\Pascal';

    if Reg.OpenKeyReadOnly(BaseKey) then
    begin
      // Map IDE Registry Names to SynDelphiSyn Attributes
      LoadAttri('Assembler', Syn.AsmAttri);
      LoadAttri('Comment', Syn.CommentAttri);
      LoadAttri('Identifier', Syn.IdentifierAttri);
      LoadAttri('Reserved Word', Syn.KeyAttri);
      LoadAttri('Number', Syn.NumberAttri);
      LoadAttri('Whitespace', Syn.SpaceAttri);
      LoadAttri('String', Syn.StringAttri);
      LoadAttri('Symbol', Syn.SymbolAttri);
      LoadAttri('Preprocessor', Syn.DirectiveAttri);
      // New Delphi 13 mappings
      // Note: older IDEs map Hex/Float to Number usually, but check if keys exist
      LoadAttri('Hex', Syn.HexAttri);
      LoadAttri('Float', Syn.FloatAttri);
      LoadAttri('Character', Syn.CharAttri);
    end;
  finally
    Reg.Free;
  end;
end;

{ TSynIDEImportEditor }

procedure TSynIDEImportEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
  begin
    if Component is TSynEdit then
    begin
      ImportToSynEdit(TSynEdit(Component));
      ShowMessage('Editor Settings Imported from IDE');
    end
    else if Component is TSynDelphiSyn then
    begin
      ImportToHighlighter(TSynDelphiSyn(Component));
      ShowMessage('Highlighter Colors Imported from IDE');
    end;

    // Notify the designer that the component has changed so it saves the .dfm
    if Assigned(Designer) then Designer.Modified;
  end;
end;

function TSynIDEImportEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Import IDE Settings';
end;

function TSynIDEImportEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  // Register the editor for both components
  RegisterComponentEditor(TSynEdit, TSynIDEImportEditor);
  RegisterComponentEditor(TSynDelphiSyn, TSynIDEImportEditor);
end;

end.
