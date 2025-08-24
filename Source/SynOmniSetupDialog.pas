unit SynOmniSetupDialog;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.Classes,
  Vcl.Dialogs,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  SynHighlighterOmni;

type
  TSynOmniSynSetup = class(TForm)
    lGeneralName: TLabel;
    lMasks: TLabel;
    edLanguagelName: TEdit;
    gbComment: TGroupBox;
    cCommentAnsi: TCheckBox;
    cCommentPas: TCheckBox;
    cCommentC: TCheckBox;
    cCommentSpec: TCheckBox;
    cCommentBas: TCheckBox;
    cCommentFox: TCheckBox;
    gbStrings: TGroupBox;
    bLoad: TButton;
    bSave: TButton;
    btnCancel: TButton;
    edFileType: TEdit;
    cCommentREM: TCheckBox;
    cCommentExcl: TCheckBox;
    rSingleQuote: TCheckBox;
    rDoubleQuote: TCheckBox;
    cCommentBy: TCheckBox;
    cCommentSharp: TCheckBox;
    cCommentSlash: TCheckBox;
    cCommentPercent: TCheckBox;
    cCommentSinglQ: TCheckBox;
    gbMisc: TGroupBox;
    cCommentSQL: TCheckBox;
    cCommentDblQ: TCheckBox;
    cCommentFortran: TCheckBox;
    cCommentCStar: TCheckBox;
    cCommentDollar: TCheckBox;
    pageKeyWords: TPageControl;
    tsKeyWords1: TTabSheet;
    tsKeyWords2: TTabSheet;
    tsKeyWords4: TTabSheet;
    tsKeyWords3: TTabSheet;
    mKeyWords: TMemo;
    mResWords: TMemo;
    mKeyWords3: TMemo;
    mKeyWords2: TMemo;
    cCommentLBracket: TCheckBox;
    cCommentPoco: TCheckBox;
    cCommentSmart: TCheckBox;
    cCommentHaskell: TCheckBox;
    cCommentPipe: TCheckBox;
    cCommentWebFocus: TCheckBox;
    cCommentD: TCheckBox;
    cCommentJCL: TCheckBox;
    cCommentDMIS: TCheckBox;
    cCommentVLisp: TCheckBox;
    cCommentDead: TCheckBox;
    cCommentCLisp: TCheckBox;
    cComment2Excl: TCheckBox;
    cCommentCPL: TCheckBox;
    cCommentDollarMulti: TCheckBox;
    Bevel1: TBevel;
    cCommentForth: TCheckBox;
    cCommentHTML: TCheckBox;
    cCommentTab: TCheckBox;
    cCommentStars: TCheckBox;
    cCommentLua: TCheckBox;
    cCommentPCL: TCheckBox;
    cCommentLilypond: TCheckBox;
    cCommentSpace: TCheckBox;
    cCommentJCL2: TCheckBox;
    cCommentAutomaton: TCheckBox;
    cCommentLineC: TCheckBox;
    cCommentOkuma: TCheckBox;
    cCommentHeller: TCheckBox;
    cCommentPwShell: TCheckBox;
    cCommentDash: TCheckBox;
    cCommentSS: TCheckBox;
    cCommentBackSlash: TCheckBox;
    cCommentAngleBracket: TCheckBox;
    cCommentINI: TCheckBox;
    cCommentTexInfo: TCheckBox;
    cCommentEuklid: TCheckBox;
    rApostropheQuote: TCheckBox;
    cCommentAutoIt: TCheckBox;
    cKW2StartWith: TCheckBox;
    cKW1StartWith: TCheckBox;
    cKW3StartWith: TCheckBox;
    cKW4StartWith: TCheckBox;
    OpenDialog: TOpenDialog;
    cRubySymbols: TCheckBox;
    cPHPVariable: TCheckBox;
    cVectors: TCheckBox;
    cHTML: TCheckBox;
    btnOK: TButton;
    cPreprocessors: TCheckBox;
    eEscChar: TEdit;
    cEscString: TCheckBox;
    cLabel: TCheckBox;
    gProperties: TGroupBox;
    cCaseSensitive: TCheckBox;
    cbCodeFolding: TComboBox;
    eKeyWordChars: TEdit;
    lCodeFoldingType: TLabel;
    lKeyWordChars: TLabel;
    gbxKeywords: TGroupBox;
    procedure edLanguagelNameChange(Sender: TObject);
    procedure edFileTypeKeyPress(Sender: TObject; var Key: Char);
    procedure bLoadClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadFromHighligther(OmniHL: TSynOmniSyn);
    procedure SaveToHighligther(OmniHL: TSynOmniSyn);

    procedure SaveHighLighterToFile(const AName: string);
    procedure LoadHighLighterFromFile(const AName: string);
  end;

function EditOmniHighlighter(OmniHL: TSynOmniSyn; HighlighterDir: string = ''): Boolean;

implementation

{$R *.DFM}

uses
  System.TypInfo,
  System.SysUtils,
  System.Math,
  System.IniFiles,
  SynEditStrConst;

procedure TSynOmniSynSetup.edLanguagelNameChange(Sender: TObject);
begin
  bSave.Enabled := edLanguagelName.Text <> '';
end;

procedure TSynOmniSynSetup.edFileTypeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not CharInSet(Key, [#0..#31, '0'..'9', 'a'..'z', 'A'..'Z', '+', ',', '*', '.', '-', '_']) then Key := #0;
end;

procedure TSynOmniSynSetup.bLoadClick(Sender: TObject);
begin
  Update;
  if OpenDialog.Execute then
  begin
    LoadHighLighterFromFile(OpenDialog.FileName);
    OpenDialog.InitialDir := ExtractFileDir(OpenDialog.FileName);
  end;
end;

procedure TSynOmniSynSetup.bSaveClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  begin
    InitialDir := OpenDialog.InitialDir;
    Filter := OpenDialog.Filter;
    DefaultExt := OpenDialog.DefaultExt;
    FileName := edLanguagelName.Text + DefaultExt;
    if Execute then
      SaveHighLighterToFile(FileName);
  end;
end;

procedure TSynOmniSynSetup.LoadFromHighligther(OmniHL: TSynOmniSyn);
var
  Str: string;
  Idx: Integer;
begin
  edLanguagelName.Text := OmniHL.LangName;

  Idx := OmniHL.DefaultFilter.IndexOf('|');
  if Idx >= 0 then
  begin
    Str := OmniHL.DefaultFilter.Substring(Idx + 1);
    edFileType.Text := StringReplace(Str, ';', ',', [rfReplaceAll]);
  end
  else
    edFileType.Text := '';

  cHTML.Checked := OmniHL.HighLighterGroup = hgHTML;
  { Comments }
  cCommentAnsi.Checked := csAnsiStyle in OmniHL.Comments;
  cCommentPas.Checked := csPasStyle in OmniHL.Comments;
  cCommentC.Checked := csCStyle in OmniHL.Comments;
  cCommentDollar.Checked := csDollar in OmniHL.Comments;
  cCommentSmart.Checked := csSmartStyle in OmniHL.Comments;
  cCommentHaskell.Checked := csHaskell in OmniHL.Comments;
  cCommentD.Checked := csDStyle in OmniHL.Comments;
  cCommentJCL.Checked := csJCLStyle in OmniHL.Comments;
  cCommentVLisp.Checked := csVLisp in OmniHL.Comments;
  cCommentCLisp.Checked := csCLisp in OmniHL.Comments;
  cCommentDead.Checked := csDead in OmniHL.Comments;
  cComment2Excl.Checked := cs2Excl in OmniHL.Comments;
  cCommentDollarMulti.Checked := csDollarMulti in OmniHL.Comments;
  cCommentForth.Checked := csForth in OmniHL.Comments;
  cCommentHTML.Checked := csHTML in OmniHL.Comments;
  cCommentLua.Checked := csLUA in OmniHL.Comments;
  cCommentLilypond.Checked := csLilypond in OmniHL.Comments;
  cCommentOkuma.Checked := csOkuma in OmniHL.Comments;
  cCommentHeller.Checked := csHeller in OmniHL.Comments;
  cCommentPwShell.Checked := csPwShell in OmniHL.Comments;
  cCommentSS.Checked := csStarSemicol in OmniHL.Comments;
  cCommentTexInfo.Checked := csTexInfo in OmniHL.Comments;
  cCommentAutoIt.Checked := csAutoit in OmniHL.Comments;

  { Single line comments }
  cCommentCPL.Checked := csCPL in OmniHL.SingleLineComments;
  cCommentSpec.Checked := csSpecStyle in OmniHL.SingleLineComments;
  cCommentBackSlash.Checked := csBackSlashStyle in OmniHL.SingleLineComments;
  cCommentLineC.Checked := csLineC in OmniHL.SingleLineComments;
  cCommentBas.Checked := csBasStyle in OmniHL.SingleLineComments;
  cCommentINI.Checked := csINIStyle in OmniHL.SingleLineComments;
  cCommentFox.Checked := csFoxStyle in OmniHL.SingleLineComments;
  cCommentREM.Checked := csBatStyle in OmniHL.SingleLineComments;
  cCommentExcl.Checked := csExclStyle in OmniHL.SingleLineComments;
  cCommentBy.Checked := csByStyle in OmniHL.SingleLineComments;
  cCommentSharp.Checked := csSharpStyle in OmniHL.SingleLineComments;
  cCommentSlash.Checked := csSlashStyle in OmniHL.SingleLineComments;
  cCommentPercent.Checked := csPercentStyle in OmniHL.SingleLineComments;
  cCommentAutomaton.Checked := csAutomaton in OmniHL.SingleLineComments;
  cCommentSinglQ.Checked := csSinglQStyle in OmniHL.SingleLineComments;
  cCommentDblQ.Checked := csDblQStyle in OmniHL.SingleLineComments;
  cCommentSQL.Checked := csSQLStyle in OmniHL.SingleLineComments;
  cCommentCStar.Checked := csCStar in OmniHL.SingleLineComments;
  cCommentFortran.Checked := csFortran in OmniHL.SingleLineComments;
  cCommentLBracket.Checked := csLeftBracket in OmniHL.SingleLineComments;
  cCommentPoco.Checked := csPocoStyle in OmniHL.SingleLineComments;
  cCommentPipe.Checked := csPipe in OmniHL.SingleLineComments;
  cCommentWebFocus.Checked := csWebFocus in OmniHL.SingleLineComments;
  cCommentDMIS.Checked := csDMISStyle in OmniHL.SingleLineComments;
  cCommentTab.Checked := csTabKey in OmniHL.SingleLineComments;
  cCommentStars.Checked := cs2Stars in OmniHL.SingleLineComments;
  cCommentPCL.Checked := csPCL in OmniHL.SingleLineComments;
  cCommentEuklid.Checked := csEuklid in OmniHL.SingleLineComments;
  cCommentSpace.Checked := csSpace in OmniHL.SingleLineComments;
  cCommentJCL2.Checked := csJCL in OmniHL.SingleLineComments;
  cCommentDash.Checked := csDash in OmniHL.SingleLineComments;
  cCommentAngleBracket.Checked := csAngleBrackets in OmniHL.SingleLineComments;

  { String delimeters }
  rSingleQuote.Checked := sdSingleQuote in OmniHL.StringDelim;
  rApostropheQuote.Checked := sdApostropheQuote in OmniHL.StringDelim;
  rDoubleQuote.Checked := sdDoubleQuote in OmniHL.StringDelim;

  cEscString.Checked := OmniHL.EscapedStrings;
  eEscChar.Text := OmniHL.EscapeChar;

  cPreprocessors.Checked := OmniHL.DetectPreprocessor;
  cLabel.Checked := OmniHL.HasLabel;
  cVectors.Checked := OmniHL.VectorSupport;
  cPHPVariable.Checked := OmniHL.PHPVariable;
  cRubySymbols.Checked := OmniHL.RubySymbols;

  cCaseSensitive.Checked := OmniHL.CaseSensitive;
  cbCodeFolding.ItemIndex := Integer(OmniHL.CodeFoldingType);

  cKW1StartWith.Checked := OmniHL.KW1StartWith;
  cKW2StartWith.Checked := OmniHL.KW2StartWith;
  cKW3StartWith.Checked := OmniHL.KW3StartWith;
  cKW4StartWith.Checked := OmniHL.KW4StartWith;

  mKeyWords.Clear; mKeyWords.Lines.AddStrings(OmniHL.KeyWords);
  mResWords.Clear; mResWords.Lines.AddStrings(OmniHL.ResWords);
  mKeyWords2.Clear; mKeyWords2.Lines.AddStrings(OmniHL.KeyWords2);
  mKeyWords3.Clear; mKeyWords3.Lines.AddStrings(OmniHL.KeyWords3);
  eKeyWordChars.Text := OmniHL.KeyWordChars;
end;

procedure TSynOmniSynSetup.LoadHighLighterFromFile(const AName: string);
var
  Loaded: Boolean;
begin
  var OmniHL := TSynOmniSyn.Create(nil);
  try
    try
      Loaded := OmniHL.LoadFromIniFile(AName);
    except
      Loaded := False;
    end;
    if not Loaded then
      MessageDlg(Format(SYNS_LoadError, [AName]), mtError, [mbOK], 0)
    else
      LoadFromHighligther(OmniHL);
  finally
    OmniHL.Free;
  end;
end;

function UserCompareText(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareText(List.Strings[Index1], List.Strings[Index2]);
end;

function UserCompareStr(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(List.Strings[Index1], List.Strings[Index2]);
end;


procedure TSynOmniSynSetup.SaveToHighligther(OmniHL: TSynOmniSyn);
var
  Str: string;
  CommentStyles: TCommentStyles;
  SingleLineComments: TSingleLineComments;
begin
  OmniHL.LangName := edLanguagelName.Text;
  Str := StringReplace(edFileType.Text, ',', ';', [rfReplaceAll]);
  OmniHL.DefaultFilter :=
    Format('%s %s (%s)|%2:s', [OmniHL.LangName, SYNS_Files, Str]);
  if cHTML.Checked then
    OmniHL.HighLighterGroup := hgHTML
  else
    OmniHL.HighLighterGroup := hgNone;
  CommentStyles := [];
  SingleLineComments := [];
  { Comments }
  if cCommentAnsi.Checked then Include(CommentStyles, csAnsiStyle);
  if cCommentPas.Checked then  Include(CommentStyles, csPasStyle);
  if cCommentC.Checked then Include(CommentStyles, csCStyle);
  if cCommentDollar.Checked then Include(CommentStyles, csDollar);
  if cCommentSmart.Checked then Include(CommentStyles, csSmartStyle);
  if cCommentHaskell.Checked then Include(CommentStyles, csHaskell);
  if cCommentD.Checked then Include(CommentStyles, csDStyle);
  if cCommentJCL.Checked then Include(CommentStyles, csJCLStyle);
  if cCommentVLisp.Checked then Include(CommentStyles, csVLisp);
  if cCommentCLisp.Checked then Include(CommentStyles, csCLisp);
  if cCommentDead.Checked then Include(CommentStyles, csDead);
  if cComment2Excl.Checked then Include(CommentStyles, cs2Excl);
  if cCommentDollarMulti.Checked then Include(CommentStyles, csDollarMulti);
  if cCommentForth.Checked then Include(CommentStyles, csForth);
  if cCommentHTML.Checked then Include(CommentStyles, csHTML);
  if cCommentLua.Checked then Include(CommentStyles, csLUA);
  if cCommentLilypond.Checked then Include(CommentStyles, csLilypond);
  if cCommentOkuma.Checked then Include(CommentStyles, csOkuma);
  if cCommentHeller.Checked then Include(CommentStyles, csHeller);
  if cCommentPwShell.Checked then Include(CommentStyles, csPwShell);
  if cCommentSS.Checked then Include(CommentStyles, csStarSemicol);
  if cCommentTexInfo.Checked then Include(CommentStyles, csTexInfo);
  if cCommentAutoIt.Checked then Include(CommentStyles, csAutoit);

  { Single line comments }
  if cCommentCPL.Checked then Include(SingleLineComments, csCPL);
  if cCommentSpec.Checked then Include(SingleLineComments, csSpecStyle);
  if cCommentBackSlash.Checked then Include(SingleLineComments, csBackSlashStyle);
  if cCommentLineC.Checked then Include(SingleLineComments, csLineC);
  if cCommentBas.Checked then Include(SingleLineComments, csBasStyle);
  if cCommentINI.Checked then Include(SingleLineComments, csINIStyle);
  if cCommentFox.Checked then Include(SingleLineComments, csFoxStyle);
  if cCommentREM.Checked then Include(SingleLineComments, csBatStyle);
  if cCommentExcl.Checked then Include(SingleLineComments, csExclStyle);
  if cCommentBy.Checked then Include(SingleLineComments, csByStyle);
  if cCommentSharp.Checked then Include(SingleLineComments, csSharpStyle);
  if cCommentSlash.Checked then Include(SingleLineComments, csSlashStyle);
  if cCommentPercent.Checked then Include(SingleLineComments, csPercentStyle);
  if cCommentAutomaton.Checked then Include(SingleLineComments, csAutomaton);
  if cCommentSinglQ.Checked then Include(SingleLineComments, csSinglQStyle);
  if cCommentDblQ.Checked then Include(SingleLineComments, csDblQStyle);
  if cCommentSQL.Checked then Include(SingleLineComments, csSQLStyle);
  if cCommentCStar.Checked then Include(SingleLineComments, csCStar);
  if cCommentFortran.Checked then Include(SingleLineComments, csFortran);
  if cCommentLBracket.Checked then Include(SingleLineComments, csLeftBracket);
  if cCommentPoco.Checked then Include(SingleLineComments, csPocoStyle);
  if cCommentPipe.Checked then Include(SingleLineComments, csPipe);
  if cCommentWebFocus.Checked then Include(SingleLineComments, csWebFocus);
  if cCommentDMIS.Checked then Include(SingleLineComments, csDMISStyle);
  if cCommentTab.Checked then Include(SingleLineComments, csTabKey);
  if cCommentStars.Checked then Include(SingleLineComments, cs2Stars);
  if cCommentPCL.Checked then Include(SingleLineComments, csPCL);
  if cCommentEuklid.Checked then Include(SingleLineComments, csEuklid);
  if cCommentSpace.Checked then Include(SingleLineComments, csSpace);
  if cCommentJCL2.Checked then Include(SingleLineComments, csJCL);
  if cCommentDash.Checked then Include(SingleLineComments, csDash);
  if cCommentAngleBracket.Checked then Include(SingleLineComments, csAngleBrackets);

  OmniHL.Comments := CommentStyles;
  OmniHL.SingleLineComments := SingleLineComments;

  { String delimeters }
  OmniHL.StringDelim := [];
  if rSingleQuote.Checked then
    OmniHL.StringDelim := OmniHL.StringDelim + [sdSingleQuote];
  if rApostropheQuote.Checked then
    OmniHL.StringDelim := OmniHL.StringDelim + [sdApostropheQuote];
  if rDoubleQuote.Checked then
    OmniHL.StringDelim := OmniHL.StringDelim + [sdDoubleQuote];


  OmniHL.EscapedStrings := cEscString.Checked;
  if eEscChar.Text <> '' then
    OmniHL.EscapeChar := eEscChar.Text[1]
  else
    OmniHL.EscapeChar := '\';
  OmniHL.DetectPreprocessor := cPreprocessors.Checked;
  OmniHL.HasLabel := cLabel.Checked;
  OmniHL.VectorSupport := cVectors.Checked;
  OmniHL.PHPVariable := cPHPVariable.Checked;
  OmniHL.RubySymbols := cRubySymbols.Checked;

  OmniHL.CaseSensitive := cCaseSensitive.Checked;
  OmniHL.CodeFoldingType := TCodeFoldingType(cbCodeFolding.ItemIndex);

  OmniHL.KW1StartWith := cKW1StartWith.Checked;
  OmniHL.KW2StartWith := cKW2StartWith.Checked;
  OmniHL.KW3StartWith := cKW3StartWith.Checked;
  OmniHL.KW4StartWith := cKW4StartWith.Checked;;

  OmniHL.SetKeyWordsCompat(mKeyWords.Lines, OmniHL.KeyWords);
  OmniHL.SetKeyWordsCompat(mResWords.Lines, OmniHL.ResWords);
  OmniHL.SetKeyWordsCompat(mKeyWords2.Lines, OmniHL.KeyWords2);
  OmniHL.SetKeyWordsCompat(mKeyWords3.Lines, OmniHL.KeyWords3);
  OmniHL.KeyWordChars := eKeyWordChars.Text;
end;

procedure TSynOmniSynSetup.SaveHighLighterToFile(const AName: string);
var
  Saved: Boolean;
begin
  var OmniHL := TSynOmniSyn.Create(nil);
  try
    SaveToHighligther(OmniHL);
    try
      Saved := OmniHL.SaveToIniFile(AName);
    except
      Saved := False;
    end;
    if not Saved then
      MessageDlg(Format(SYNS_SaveError, [AName]), mtError, [mbOK], 0);
  finally
    OmniHL.Free;
  end;
end;

procedure TSynOmniSynSetup.FormCreate(Sender: TObject);
var
  I: TCodeFoldingType;
begin
  for I := Low(TCodeFoldingType) to High(TCodeFoldingType) do
    cbCodeFolding.Items.Add(
    Copy(GetEnumName(TypeInfo(TCodeFoldingType), Integer(I)), 4));
end;

function EditOmniHighlighter(OmniHL: TSynOmniSyn; HighlighterDir: string): Boolean;
begin
  Result := False;
  with TSynOmniSynSetup.Create(nil) do
  begin
    LoadFromHighligther(OmniHL);
    OpenDialog.InitialDir := HighlighterDir;
    if ShowModal = mrOK then
    begin
      SaveToHighligther(OmniHL);
      Result := True;
    end;
    Release;
  end;
end;

end.
