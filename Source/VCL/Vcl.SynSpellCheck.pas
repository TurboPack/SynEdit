{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}


unit Vcl.SynSpellCheck;
{$I synedit.inc}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.ActnList,
  SynEdit,
  SynEditMiscClasses,
  SynSpellCheckTypes,
  SynSpellCheckWinAPI;

{$REGION 'Spell Checking Type Aliases'}

// Re-export COM types so that existing code using Vcl.SynSpellCheck.ISpellChecker
// etc. continues to compile without changes.
type
  ISpellCheckerFactory = SynSpellCheckWinAPI.ISpellCheckerFactory;
  IUserDictionariesRegistrar = SynSpellCheckWinAPI.IUserDictionariesRegistrar;
  ISpellChecker = SynSpellCheckWinAPI.ISpellChecker;
  ISpellChecker2 = SynSpellCheckWinAPI.ISpellChecker2;
  IEnumSpellingError = SynSpellCheckWinAPI.IEnumSpellingError;
  ISpellingError = SynSpellCheckWinAPI.ISpellingError;
  ISpellCheckerChangedEventHandler = SynSpellCheckWinAPI.ISpellCheckerChangedEventHandler;
  IOptionDescription = SynSpellCheckWinAPI.IOptionDescription;
  CORRECTIVE_ACTION = SynSpellCheckWinAPI.CORRECTIVE_ACTION;
  TCorrectiveAction = SynSpellCheckWinAPI.TCorrectiveAction;
  SpellCheckerFactory = SynSpellCheckWinAPI.ISpellCheckerFactory;

{$ENDREGION 'Spell Checking Type Aliases'}

  TUnderlineStyle = (usCorelWordPerfect, usMicrosoftWord);

{$REGION 'TSpellCheckPlugin'}

  TSynSpellCheck = class;

  TSpellCheckPlugin = class(TSynEditPlugin)
  private
    procedure RegisterIndicatorSpec;
    procedure Changed(LangId: Boolean = False);
  protected
    FSynSpellCheck: TSynSpellCheck;
    { Procedures }
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinePut(aIndex: Integer; const OldLine: string); override;
  public
    constructor Create(AOwner: TCustomSynEdit);
    { Properties }
  end;

{$REGION 'TSpellCheckPlugin'}

{$REGION 'TSynSpellCheck'}

  { Singleton class you can/need only have one instance in your application}
  TSynSpellCheck = class(TComponent)
  public
    const SpellErrorIndicatorId: TGUID  = '{CAD19326-12B3-4190-9241-99DE3FDDE214}';
    class var GlobalInstance: TSynSpellCheck;
  private type
    TWorkItem = record
      Token: string;
      TokenPos: Integer;
      constructor Create(AToken: string; ATokenPos: Integer);
    end;
  private
    FLanguageCode: string;
    FSpellChecker: ISpellChecker;
    FProvider: ISynSpellCheckProvider;
    FEditor: TCustomSynEdit;
    FEditors: TList<TCustomSynEdit>;
    FPlugins: TList<TSpellCheckPlugin>;
    FUnderlineStyle: TUnderlineStyle;
    FPenColor: TColor;
    FAttributesChecked: TStrings;
    FUpdateCount: Integer;
    FCheckAsYouType: Boolean;
    FDictionaryNA: Boolean;
    FWorkList: TList<TWorkItem>;
    FOnChange: TNotifyEvent;
    procedure CreateSpellChecker;
    procedure SetLanguageCode(const Value: string);
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure SetPenColor(const Value: TColor);
    procedure SetUnderlineStyle(const Value: TUnderlineStyle);
    procedure SetAttributesChecked(const Value: TStrings);
    procedure SetProvider(const Value: ISynSpellCheckProvider);
    class var FSpellCheckFactory: ISpellCheckerFactory;
    procedure SetCheckAsYouType(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function SpellCheckLine(Editor: TCustomSynEdit; Line: Integer;
        StartChar:Integer = 0; EndChar: Integer = MaxInt; ErrorPos: Integer = -1):
        ISpellingError;
    class function SpellCheckFactory: ISpellCheckerFactory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Changed(LangId: Boolean = False);
    function AddEditor(AEditor: TCustomSynEdit): Integer;
    function RemoveEditor(AEditor: TCustomSynEdit): Boolean;
    // Spell checking actions applied to FEditor
    procedure CheckFile;
    procedure CheckLine;
    procedure CheckSelection;
    procedure CheckWord;
    procedure ClearErrors(Invalidate: Boolean = True);
    function ErrorAtPos(BC: TBufferCoord): ISpellingError;
    // provides access to to the SpellChecker interface
    function SpellChecker: ISpellChecker;
    // Optional provider for plugin-based spell checking (e.g. Hunspell).
    // When assigned, SpellCheckLine uses Provider.CheckWord instead of COM.
    property Provider: ISynSpellCheckProvider read FProvider write SetProvider;
    property LanguageCode: string read FLanguageCode write SetLanguageCode;
    class function SupportedLanguages: TArray<string>;
  published
    property PenColor: TColor read FPenColor write SetPenColor default clRed;
    property UnderlineStyle: TUnderlineStyle read FUnderlineStyle
      write SetUnderlineStyle default usMicrosoftWord;
    property AttributesChecked: TStrings read FAttributesChecked
      write SetAttributesChecked;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property CheckAsYouType: Boolean read FCheckAsYouType write SetCheckAsYouType;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$ENDREGION 'TSynSpellCheck'}

{$REGION 'Spell Check Actions'}

  TSynSpellCheckAction = class abstract (TAction)
  // base class for spell check actions
  private
    FControl: TCustomSynEdit;
    procedure SetControl(Value: TCustomSynEdit);
  protected
    function GetControl(Target: TObject): TCustomSynEdit; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    property Control: TCustomSynEdit read FControl write SetControl;
  end;

  TSynSpellCheckFile = class(TSynSpellCheckAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellCheckLine = class(TSynSpellCheckAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellCheckSelection = class(TSynSpellCheckAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellCheckWord = class(TSynSpellCheckAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellClearErrors = class(TSynSpellCheckAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellCheckAsYouType = class(TAction)
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellErrorAction = class abstract(TSynSpellCheckAction)
    procedure UpdateTarget(Target: TObject); override;
  end;

  TSynSpellErrorReplace = class(TSynSpellErrorAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellErrorAdd = class(TSynSpellErrorAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellErrorIgnoreOnce = class(TSynSpellErrorAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellErrorIgnore = class(TSynSpellErrorAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TSynSpellErrorDelete = class(TSynSpellErrorAction)
    procedure ExecuteTarget(Target: TObject); override;
  end;

{$ENDREGION 'Spell Check Actions'}

implementation

uses
  Winapi.Messages,
  System.Math,
  System.Win.ComObj,
  SynEditTypes,
  SynUnicode,
  SynDWrite,
  SynEditMiscProcs,
  SynEditHighlighter,
  SynEditTextBuffer;

resourcestring
  SYNS_SingletonSynSpellCheck =
    'You can not and do not need to create more than one instance of TSynSpellCheck';

{$REGION 'TSynSpellCheck Implementation'}

function TSynSpellCheck.AddEditor(AEditor: TCustomSynEdit): Integer;
var
  Plugin: TSpellCheckPlugin;
begin
  // Adds an Editor and returns its index in the list
  Result := FEditors.IndexOf(AEditor);
  if Result < 0 then
  begin
    FEditors.Add(AEditor);
    AEditor.FreeNotification(Self);
    Plugin := TSpellCheckPlugin.Create(AEditor);
    Plugin.FSynSpellCheck := Self;
    Plugin.Changed;
    FPlugins.Add(Plugin);
  end;
end;

procedure TSynSpellCheck.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSynSpellCheck.CheckFile;
var
  Line: Integer;
begin
  if not Assigned(FEditor) then Exit;

  ClearErrors(False);

  for Line := 1 to FEditor.Lines.Count do
    SpellCheckLine(FEditor, Line);
  Editor.Invalidate;
end;

procedure TSynSpellCheck.CheckSelection;
var
  BB, BE: TBufferCoord;
  Line: Integer;
begin
  if not Assigned(FEditor) then Exit;
  BB := Editor.BlockBegin;
  BE := Editor.BlockEnd;

  if BB.Line = BE.Line then
    SpellCheckLine(FEditor, BB.Line, BB.Char, BE.Char)
  else
  begin
    // First line
    SpellCheckLine(FEditor, BB.Line, BB.Char);
    for Line := BB.Line + 1 to BE.Line - 1 do
      SpellCheckLine(FEditor, Line);
    // Last line
    SpellCheckLine(FEditor, BE.Line, 1, BE.Char);
  end;
  FEditor.InvalidateSelection;
end;

procedure TSynSpellCheck.CheckLine;
begin
  if not Assigned(FEditor) then Exit;

  TCustomSynEdit(FEditor).Indicators.Clear(SpellErrorIndicatorId,
    False, TCustomSynEdit(FEditor).CaretY);
  SpellCheckLine(TCustomSynEdit(FEditor), TCustomSynEdit(FEditor).CaretY);
  Editor.InvalidateLine(TCustomSynEdit(FEditor).CaretY);
end;

procedure TSynSpellCheck.CheckWord;
var
  BB, BE: TBufferCoord;
begin
  if not Assigned(FEditor) then Exit;

  BB := FEditor.WordStart;
  BE := FEditor.WordEnd;

  if BB >= BE then Exit;

  SpellCheckLine(TCustomSynEdit(FEditor), BB.Line, BB.Char, BE.Char);
  FEditor.InvalidateRange(BB, BE);
end;

procedure TSynSpellCheck.ClearErrors(Invalidate: Boolean = True);
begin
  if not Assigned(FEditor) then Exit;
  TCustomSynEdit(FEditor).Indicators.Clear(SpellErrorIndicatorId, Invalidate);
end;

procedure TSynSpellCheck.Changed(LangId: Boolean);
var
  Plugin: TSpellCheckPlugin;
begin
  if FUpdateCount = 0 then
  begin
    for Plugin in FPlugins do
      Plugin.Changed(LangId);

    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

constructor TSynSpellCheck.Create(AOwner: TComponent);
begin
  if Assigned(GlobalInstance) then
    raise ESynError.CreateRes(@SYNS_SingletonSynSpellCheck);

  inherited Create(AOwner);
  FLanguageCode := UserLocaleName;
  FPenColor := clRed;
  FUnderlineStyle := usMicrosoftWord;
  FEditors := TList<TCustomSynEdit>.Create;
  FPlugins := TList<TSpellCheckPlugin>.Create;
  FAttributesChecked := TStringList.Create;
  with FAttributesChecked do
  begin
    Add('Comment');
    Add('Text');
    Add('String');
    Add('Documentation');
  end;
  FWorkList := TList<TWorkItem>.Create;
  GlobalInstance := Self;
end;

procedure TSynSpellCheck.CreateSpellChecker;
begin
  FSpellChecker := nil;

  if not Assigned(SpellCheckFactory()) then
  begin
    FDictionaryNA := True;
    Exit;
  end;

  try
    CheckOSError(SpellCheckFactory.CreateSpellChecker(
      PChar(FLanguageCode), FSpellChecker));
  except
    FDictionaryNA := True;
  end;
end;

destructor TSynSpellCheck.Destroy;
var
  Ed: TCustomSynEdit;
begin
  GlobalInstance := nil;
  if Assigned(FEditors) then
    for Ed in FEditors do
      Ed.RemoveFreeNotification(Self);
  FEditors.Free;
  FPlugins.Free;
  FAttributesChecked.Free;
  FWorkList.Free;
  inherited;
end;

procedure TSynSpellCheck.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

function TSynSpellCheck.ErrorAtPos(BC: TBufferCoord): ISpellingError;
var
  Indicator: TSynIndicator;
begin
  if not Assigned(FEditor) then Exit;

  if not FEditor.Indicators.IndicatorAtPos(BC, SpellErrorIndicatorId, Indicator) then
    Exit;

  Result := SpellCheckLine(FEditor, BC.Line, 0, MaxInt, BC.Char);
end;

procedure TSynSpellCheck.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent is TCustomSynEdit) and (Operation = opRemove) then
    RemoveEditor(TCustomSynEdit(AComponent));
  inherited;
end;

function TSynSpellCheck.RemoveEditor(AEditor: TCustomSynEdit): Boolean;
var
  Index: Integer;
begin
  Index := FEditors.IndexOf(AEditor);
  Result := Index >= 0;

  if Result then
  begin
    AEditor.RemoveFreeNotification(Self);
    FEditors.Delete(Index);
    FPlugins[Index].Free;
    FPlugins.Delete(Index);
    if FEditor = AEditor then
      FEditor := nil;
  end;
end;

procedure TSynSpellCheck.SetAttributesChecked(const Value: TStrings);
begin
  FAttributesChecked.Assign(Value);
  Changed;
end;

procedure TSynSpellCheck.SetCheckAsYouType(const Value: Boolean);
begin
  if Value <> FCheckAsYouType then
  begin
    FCheckAsYouType := Value;
    Changed;
  end;
end;

procedure TSynSpellCheck.SetEditor(const Value: TCustomSynEdit);
begin
  if Value <> FEditor then
  begin
    FEditor := Value;
    if Assigned(Value) then
      AddEditor(Value);
  end;
end;

procedure TSynSpellCheck.SetLanguageCode(const Value: string);
begin
  if FLanguageCode <> Value  then
  begin
    FLanguageCode := Value;
    FDictionaryNA := False;
    if Assigned(FSpellChecker) then
      CreateSpellChecker;
    Changed;
  end;
end;

procedure TSynSpellCheck.SetPenColor(const Value: TColor);
begin
  if FPenColor <> Value then
  begin
    FPenColor := Value;
    Changed;
  end;
end;

procedure TSynSpellCheck.SetUnderlineStyle(const Value: TUnderlineStyle);
begin
  if FUnderlineStyle <> Value then
  begin
    FUnderlineStyle := Value;
    Changed;
  end;
end;

procedure TSynSpellCheck.SetProvider(const Value: ISynSpellCheckProvider);
begin
  if FProvider <> Value then
  begin
    FProvider := Value;
    Changed;
  end;
end;

function TSynSpellCheck.SpellChecker: ISpellChecker;
begin
  if FDictionaryNA then Exit(nil);

  if not Assigned(FSpellChecker) then
    CreateSpellChecker;
  Result := FSpellChecker;
end;

class function TSynSpellCheck.SpellCheckFactory: ISpellCheckerFactory;
begin
  // Windows 8 required
  if not TOSVersion.Check(6, 2) then
    Exit(nil);

  if not Assigned(FSpellCheckFactory) then
    FSpellCheckFactory := CreateComObject(CLASS_SpellCheckerFactory) as ISpellCheckerFactory;
  Result := FSpellCheckFactory;
end;

function TSynSpellCheck.SpellCheckLine(Editor: TCustomSynEdit; Line:
    Integer; StartChar:Integer = 0; EndChar: Integer = MaxInt;
    ErrorPos: Integer = -1): ISpellingError;
{ The core spell checking function.   Spells checks a whole or part of
  a line.  If ErrorPos > 0 then instead of adding indicators, it returns the
  SpellingError at the ErrorPos

  Spell checking may cause repainting due to the appartment thread model
  marshalling via the Windows message queue.  Hence the use of the WorkList
  to avoid messing up the highlighter scanning, which is not reentrant}

  procedure SpellCheckToken(const Token: string; TokenPos: Integer = 0);
  var
    SpellingErrors: IEnumSpellingError;
    SpellingError: ISpellingError;
    StartIndex, Len: LongWord;
  begin
    CheckOSError(SpellChecker.Check(PChar(Token), SpellingErrors));
    while SpellingErrors.Next(SpellingError) = S_OK do
    begin
      SpellingError.Get_StartIndex(StartIndex);
      SpellingError.Get_Length(Len);

      if (Integer(StartIndex) + 1 < StartChar) then Continue;
      if (Integer(StartIndex + Len) > EndChar) then Break;

      if ErrorPos < 0 then
        Editor.Indicators.Add(Line,
          TSynIndicator.Create(SpellErrorIndicatorId,
            Integer(StartIndex) + TokenPos + 1,
            Integer(StartIndex) + TokenPos + 1 + Integer(Len)), False)
      else if InRange(ErrorPos - TokenPos, StartIndex + 1, StartIndex + Len) then
      begin
       Result := SpellingError;
       Exit;
      end;
    end;
  end;

  procedure ProviderCheckToken(const Token: string; TokenPos: Integer = 0);
  var
    Words: TArray<TWordInfo>;
    Info: TWordInfo;
    WordStartChar, WordEndChar: Integer;
  begin
    Words := ExtractWords(Token);
    for Info in Words do
    begin
      if not ContainsLetter(Info.Word) then
        Continue;

      WordStartChar := Info.StartChar + TokenPos;
      WordEndChar := Info.EndChar + TokenPos;

      if (WordStartChar < StartChar) then Continue;
      if (WordEndChar > EndChar) then Continue;

      if not FProvider.CheckWord(Info.Word) then
      begin
        if ErrorPos < 0 then
          Editor.Indicators.Add(Line,
            TSynIndicator.Create(SpellErrorIndicatorId,
              WordStartChar, WordEndChar), False);
        // Note: ErrorPos lookup not supported in provider mode (no ISpellingError)
      end;
    end;
  end;

var
  SLine, Token: string;
  Attri: TSynHighlighterAttributes;
  TokenPos: Integer;
  WorkItem: TWorkItem;
  UseProvider: Boolean;
begin
  Result := nil;
  UseProvider := Assigned(FProvider) and FProvider.IsAvailable;

  if not UseProvider and not Assigned(SpellChecker()) then Exit;

  SLine := Editor.Lines[Line - 1];
  if SLine = '' then Exit;

  if Assigned(Editor.Highlighter) and not (Editor.Highlighter.ClassName = 'TSynUriSyn') then
  begin
    if Line > 1 then
      Editor.Highlighter.SetRange(TSynEditStringList(Editor.Lines).Ranges[Line - 2])
    else
      Editor.Highlighter.ResetRange;
    Editor.Highlighter.SetLine(SLine, Line);

    FWorkList.Clear;
    while not Editor.HighLighter.GetEol do
    begin
      TokenPos := Editor.HighLighter.GetTokenPos; //TokenPos is zero based
      Token := Editor.HighLighter.GetToken;

      if TokenPos >= EndChar then Break;
      if TokenPos + Token.Length < StartChar then Continue;

      Attri := Editor.HighLighter.GetTokenAttribute;
      if (Token <> '') and (not Assigned(Attri) or
        (FAttributesChecked.IndexOf(Attri.Name) >= 0))
      then
        FWorkList.Add(TWorkItem.Create(Token,TokenPos));

      Editor.HighLighter.Next;
    end;

    for WorkItem in FWorkList do
    begin
      if UseProvider then
        ProviderCheckToken(WorkItem.Token, WorkItem.TokenPos)
      else
        SpellCheckToken(WorkItem.Token, WorkItem.TokenPos);

      // Check if ErrorPos >= 0 and we found the error
      if Assigned(Result) then Exit;
    end;
  end
  else
  begin
    if UseProvider then
      ProviderCheckToken(SLine)
    else
      SpellCheckToken(SLine);
  end;
end;

class function TSynSpellCheck.SupportedLanguages: TArray<string>;
var
  Languages: IEnumString;
  Lang: PChar;
  Fetched: LongInt;
begin
  SetLength(Result, 0);
  if not Assigned(SpellCheckFactory()) then Exit;

  SpellCheckFactory.Get_SupportedLanguages(Languages);
  while Languages.Next(1, Lang, @Fetched) = S_OK do
  begin
    Result := Result + [string(Lang)];
    CoTaskMemFree(Lang);
  end;
end;

{$ENDREGION 'TSynSpellCheck'}


{$REGION 'TSpellCheckPlugin Implementation'}

procedure TSpellCheckPlugin.Changed(LangId: Boolean);
begin
  if LangId then
    Editor.Indicators.Clear(FSynSpellCheck.SpellErrorIndicatorId);
  RegisterIndicatorSpec;
  if Editor.HandleAllocated then
    Editor.Invalidate;
end;

constructor TSpellCheckPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited;
  FHandlers := [phLinesInserted, phLinePut];
end;

procedure TSpellCheckPlugin.LinePut(aIndex: Integer; const OldLine: string);
var
  Line: string;
  Len1, Len2: Integer;
  StartingPos: Integer;
begin
  if Editor <> FSynSpellCheck.Editor then Exit;  // Chained editors

  if (Assigned(FSynSpellCheck.SpellChecker()) or
    (Assigned(FSynSpellCheck.Provider) and FSynSpellCheck.Provider.IsAvailable))
    and FSynSpellCheck.CheckAsYouType then
  begin
    Line := Editor.Lines[aIndex];
    LineDiff(Line, OldLine, StartingPos, Len1, Len2);
    if (Len1 <> 0) or (Len2 <> 1) or not Editor.IsIdentChar(Line[StartingPos]) then
      FSynSpellCheck.SpellCheckLine(Editor, aIndex + 1);
  end;
end;

procedure TSpellCheckPlugin.LinesInserted(FirstLine, Count: Integer);
var
  Line: Integer;
begin
  if Editor <> FSynSpellCheck.Editor then Exit;

  if (Assigned(FSynSpellCheck.SpellChecker()) or
    (Assigned(FSynSpellCheck.Provider) and FSynSpellCheck.Provider.IsAvailable))
    and FSynSpellCheck.CheckAsYouType then
    for Line := FirstLine + 1 to FirstLine + Count do
      FSynSpellCheck.SpellCheckLine(Editor, Line);
end;

procedure TSpellCheckPlugin.RegisterIndicatorSpec;
var
  Spec: TSynIndicatorSpec;
begin
  if FSynSpellCheck.UnderlineStyle = usMicrosoftWord then
    Spec.Style := sisSquiggleMicrosoftWord
  else
    Spec.Style := sisSquiggleWordPerfect;
  Spec.Foreground := D2D1ColorF(FSynSpellCheck.PenColor);

  Editor.Indicators.RegisterSpec(FSynSpellCheck.SpellErrorIndicatorId, Spec);
  if Editor.HandleAllocated then
    Editor.Invalidate;
end;

{$ENDREGION 'TSpellCheckPlugin'}

{$REGION 'SpellCheck Actions implementation'}

destructor TSynSpellCheckAction.Destroy;
begin
  if Assigned(FControl) then
    FControl.RemoveFreeNotification(Self);
  inherited;
end;

function TSynSpellCheckAction.GetControl(Target: TObject): TCustomSynEdit;
begin
  { We could hard cast Target as a TCustomSynEdit since HandlesTarget "should" be
    called before ExecuteTarget and UpdateTarget, however, we're being safe. }
  Result := Target as TCustomSynEdit;
end;

function TSynSpellCheckAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Assigned(TSynSpellCheck.GlobalInstance) and
    (((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TCustomSynEdit))
    and TCustomSynEdit(Target).Focused);
end;

procedure TSynSpellCheckAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then Control := nil;
end;

procedure TSynSpellCheckAction.SetControl(Value: TCustomSynEdit);
begin
  if Value <> FControl then
  begin
    FControl := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TSynSpellCheckAction.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(TSynSpellCheck.GlobalInstance.SpellChecker());
end;

{ TSynSpellCheckFile }

procedure TSynSpellCheckFile.ExecuteTarget(Target: TObject);
begin
  TSynSpellCheck.GlobalInstance.Editor := GetControl(Target);
  TSynSpellCheck.GlobalInstance.CheckFile;
end;

{ TSynSpellCheckLine }

procedure TSynSpellCheckLine.ExecuteTarget(Target: TObject);
begin
  TSynSpellCheck.GlobalInstance.Editor := GetControl(Target);
  TSynSpellCheck.GlobalInstance.CheckLine;
end;

{ TSynSpellCheckSelection }

procedure TSynSpellCheckSelection.ExecuteTarget(Target: TObject);
begin
  TSynSpellCheck.GlobalInstance.Editor := GetControl(Target);
  TSynSpellCheck.GlobalInstance.CheckSelection;
end;

{ TSpellCheckWord }

procedure TSynSpellCheckWord.ExecuteTarget(Target: TObject);
begin
  TSynSpellCheck.GlobalInstance.Editor := GetControl(Target);
  TSynSpellCheck.GlobalInstance.CheckWord;
end;

{ TSpellClearErros }

procedure TSynSpellClearErrors.ExecuteTarget(Target: TObject);
begin
  TSynSpellCheck.GlobalInstance.Editor := GetControl(Target);
  TSynSpellCheck.GlobalInstance.ClearErrors;
end;

{ TSynSpellCheckAsYouType }

procedure TSynSpellCheckAsYouType.ExecuteTarget(Target: TObject);
begin
  TSynSpellCheck.GlobalInstance.CheckAsYouType :=
    not TSynSpellCheck.GlobalInstance.CheckAsYouType;
end;

function TSynSpellCheckAsYouType.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

procedure TSynSpellCheckAsYouType.UpdateTarget(Target: TObject);
begin
  Enabled := Assigned(TSynSpellCheck.GlobalInstance.SpellChecker());
  Checked := TSynSpellCheck.GlobalInstance.CheckAsYouType;
end;

{ TSynSpellErrorAction }
procedure TSynSpellErrorAction.UpdateTarget(Target: TObject);
var
  Ed: TCustomSynEdit;
  Indicator: TSynIndicator;
begin
  inherited;
  if Enabled then
  begin
    Ed := TCustomSynEdit(Target);
    Enabled :=  Ed.Indicators.IndicatorAtPos(Ed.CaretXY, Indicator) and
      (Indicator.Id = TSynSpellCheck.SpellErrorIndicatorId);
  end;
end;

{ TSynSpellErrorAdd }

procedure TSynSpellErrorAdd.ExecuteTarget(Target: TObject);
var
  AWord: string;
  Ed: TCustomSynEdit;
  Indicator: TSynIndicator;
begin
  Ed := TCustomSynEdit(Target);
  if Ed.Indicators.IndicatorAtPos(Ed.CaretXY, Indicator) and
    (Indicator.Id = TSynSpellCheck.SpellErrorIndicatorId) then
  begin
    Ed.Indicators.Clear(Ed.CaretY, Indicator);
    AWord := Copy(Ed.Lines[Ed.CaretY - 1], Indicator.CharStart,
       Indicator.CharEnd - Indicator.CharStart);
    TSynSpellCheck.GlobalInstance.SpellChecker.Add(PChar(AWord));
  end;
end;

{ TSynSpellErrorReplace }

procedure TSynSpellErrorReplace.ExecuteTarget(Target: TObject);
var
  Line: string;
  Ed: TCustomSynEdit;
  Indicator: TSynIndicator;
begin
  Ed := TCustomSynEdit(Target);
  if Ed.Indicators.IndicatorAtPos(Ed.CaretXY, Indicator) and
    (Indicator.Id = TSynSpellCheck.SpellErrorIndicatorId) then
  begin
    Line := Ed.Lines[Ed.CaretY -1];
    Delete(Line, Indicator.CharStart, Indicator.CharEnd - Indicator.CharStart);
    Insert(Caption, Line, Indicator.CharStart);
    Ed.Lines[Ed.CaretY -1] := Line;
    TSynSpellCheck.GlobalInstance.CheckLine;

    TSynSpellCheck.GlobalInstance.Editor := Ed;
    TSynSpellCheck.GlobalInstance.CheckLine;
  end;
end;

{ TSynSpellErrorIgnoreOnce }

procedure TSynSpellErrorIgnoreOnce.ExecuteTarget(Target: TObject);
var
  Ed: TCustomSynEdit;
  Indicator: TSynIndicator;
begin
  Ed := TCustomSynEdit(Target);
  if Ed.Indicators.IndicatorAtPos(Ed.CaretXY, Indicator) and
    (Indicator.Id = TSynSpellCheck.SpellErrorIndicatorId)
  then
    Ed.Indicators.Clear(Ed.CaretY, Indicator);
end;

{ TSynSpellErrorIgnore }

procedure TSynSpellErrorIgnore.ExecuteTarget(Target: TObject);
var
  AWord: string;
  Ed: TCustomSynEdit;
  Indicator: TSynIndicator;
begin
  Ed := TCustomSynEdit(Target);
  if Ed.Indicators.IndicatorAtPos(Ed.CaretXY, Indicator) and
    (Indicator.Id = TSynSpellCheck.SpellErrorIndicatorId) then
  begin
    Ed.Indicators.Clear(Ed.CaretY, Indicator);
    AWord := Copy(Ed.Lines[Ed.CaretY - 1], Indicator.CharStart,
       Indicator.CharEnd - Indicator.CharStart);
    TSynSpellCheck.GlobalInstance.SpellChecker.Ignore(PChar(AWord));
  end;
end;

{ TSynSpellErrorDelete }

procedure TSynSpellErrorDelete.ExecuteTarget(Target: TObject);
var
  Ed: TCustomSynEdit;
  Indicator: TSynIndicator;
  Line: string;
begin
  Ed := TCustomSynEdit(Target);
  if Ed.Indicators.IndicatorAtPos(Ed.CaretXY, Indicator) and
    (Indicator.Id = TSynSpellCheck.SpellErrorIndicatorId) then
  begin
    Line := Ed.Lines[Ed.CaretY -1];
    Delete(Line, Indicator.CharStart, Indicator.CharEnd - Indicator.CharStart + 1);
    Ed.Lines[Ed.CaretY -1] := Line;

    TSynSpellCheck.GlobalInstance.Editor := Ed;
    TSynSpellCheck.GlobalInstance.CheckLine;
  end;
end;

{$ENDREGION 'SpellCheck Actions implementation'}

{ TSynSpellCheck.TWorkItem }

constructor TSynSpellCheck.TWorkItem.Create(AToken: string; ATokenPos: Integer);
begin
  Self.Token := AToken;
  Self.TokenPos := ATokenPos;
end;

end.
