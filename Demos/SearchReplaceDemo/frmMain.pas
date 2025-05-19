{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
The Original Code is: frmMain.pas, released 2000-06-23.
The Original Code is part of the SearchReplaceDemo project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.
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
unit frmMain;
{$I SynEdit.inc}
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ImgList, ActnList, SynEdit, SynEditRegexSearch,
  SynEditMiscClasses, SynEditSearch, SynUnicode, System.Actions,
  System.ImageList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection, SynEditTypes;
type
  TSearchReplaceDemoForm = class(TForm)
    ActionFileOpen: TAction;
    ActionListMain: TActionList;
    ActionSearch: TAction;
    ActionSearchNext: TAction;
    ActionSearchPrev: TAction;
    ActionSearchReplace: TAction;
    OpenDialogFile: TOpenDialog;
    Statusbar: TStatusBar;
    SynEditor: TSynEdit;
    SynEditRegexSearch: TSynEditRegexSearch;
    SynEditSearch: TSynEditSearch;
    ToolBarMain: TToolBar;
    ToolButtonFileOpen: TToolButton;
    ToolButtonSearch: TToolButton;
    ToolButtonSearchNext: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonSearchPrev: TToolButton;
    ToolButtonSearchReplace: TToolButton;
    ImageCollection: TImageCollection;
    VirtualImageList: TVirtualImageList;
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchNextExecute(Sender: TObject);
    procedure ActionSearchPrevExecute(Sender: TObject);
    procedure ActionSearchReplaceExecute(Sender: TObject);
    procedure actSearchUpdate(Sender: TObject);
    procedure ActionSearchReplaceUpdate(Sender: TObject);
    procedure SynEditorReplaceText(Sender: TObject; const ASearch,
      AReplace: string; Line, Column: Integer;
      var Action: TSynReplaceAction);
    procedure FormCreate(Sender: TObject);
    procedure SynEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure SynEditorProcessCommand(Sender: TObject; var Command:
        TSynEditorCommand; var AChar: WideChar; Data: Pointer);
  private
    procedure DoSearchReplaceText(AReplace: Boolean; ABackwards: Boolean);
    procedure ShowSearchReplaceDialog(AReplace: Boolean);
  end;
var
  SearchReplaceDemoForm: TSearchReplaceDemoForm;

implementation

{$R *.DFM}

uses
  dlgSearchText, dlgReplaceText, dlgConfirmReplace,
  SynEditMiscProcs, uSearchHighlighter, SynEditKeyCmds;

// options - to be saved to the registry
type
  TSearchOptions = record
    SearchBackwards: Boolean;
    SearchCaseSensitive: Boolean;
    SearchFromCaret: Boolean;
    SearchSelectionOnly: Boolean;
    SearchTextAtCaret: Boolean;
    SearchWholeWords: Boolean;
    SearchRegex: Boolean;
    SearchText: string;
    SearchTextHistory: string;
    ReplaceText: string;
    ReplaceTextHistory: string;
    TempSearchFromCaret: Boolean;
    TempLastMatch: TSynSelection;
    SelStorage: TSynSelStorage;
    function SetSynSearchOptions: TSynSearchOptions;
    function HasStoredSelection: Boolean;
    procedure InitSearch;
  end;

var
  SearchOptions: TSearchOptions;

resourcestring
  STextNotFound = 'Text not found';

{ TSearchReplaceDemoForm }
procedure TSearchReplaceDemoForm.FormCreate(Sender: TObject);
begin
  RegisterSearchHighlightIndicatorSpec(SynEditor);
end;

procedure TSearchReplaceDemoForm.DoSearchReplaceText(AReplace: Boolean;
  ABackwards: Boolean);
var
  Options: TSynSearchOptions;
  Start: TBufferCoord;
begin
  Statusbar.SimpleText := '';
  Options := SearchOptions.SetSynSearchOptions;
  if ABackwards then
    Include(Options, ssoBackwards)
  else
    Exclude(Options, ssoBackwards);

  if AReplace then
    Options := Options + [ssoPrompt, ssoReplace, ssoReplaceAll];

  Start := TBufferCoord.Invalid;

  if ssoSelectedOnly in Options then
  begin
    if SearchOptions.HasStoredSelection then
    begin
      SynEditor.Selections.Restore(SearchOptions.SelStorage);
      if SearchOptions.TempLastMatch.IsValid then
      begin
        if ABackwards then
          Start := SearchOptions.TempLastMatch.Normalized.Start
        else
          Start := SearchOptions.TempLastMatch.Normalized.Stop;
      end
    end
    else if SynEditor.Selections.IsEmpty then
    begin
      Exclude(Options, ssoSelectedOnly);
      SearchOptions.SearchSelectionOnly := False;
    end
    else
      SynEditor.Selections.Store(SearchOptions.SelStorage);
  end;

  if SynEditor.SearchReplace(SearchOptions.SearchText,
    SearchOptions.ReplaceText, Options, Start, TBufferCoord.Invalid) = 0
  then
  begin
    MessageBeep(MB_ICONASTERISK);
    Statusbar.SimpleText := STextNotFound;
    if ssoSelectedOnly in Options then
      // Restore Selection
      SynEditor.Selections.Restore(SearchOptions.SelStorage)
    else
      // Remove selection
      SynEditor.CaretXY := SynEditor.CaretXY;
    SearchOptions.TempLastMatch.Start := TBufferCoord.Invalid;
  end
  else
  begin
    SearchOptions.TempLastMatch := SynEditor.Selection;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TSearchReplaceDemoForm.ShowSearchReplaceDialog(AReplace: Boolean);
var
  dlg: TTextSearchDialog;
  Options: TSynSearchOptions;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do
    try
      // assign search options
      SearchBackwards := SearchOptions.SearchBackwards;
      SearchCaseSensitive := SearchOptions.SearchCaseSensitive;
      SearchFromCursor := SearchOptions.SearchFromCaret;
      SearchInSelectionOnly := SearchOptions.SearchSelectionOnly;
      SearchRegularExpression := SearchOptions.SearchRegex;
      // start with last search text
      SearchText := SearchOptions.SearchText;
      if SearchOptions.SearchTextAtCaret then
      begin
        // if something is selected search for that text
        if SynEditor.SelAvail and (SynEditor.BlockBegin.Line =
          SynEditor.BlockEnd.Line) then
          SearchText := SynEditor.SelText
        else
          SearchText := SynEditor.GetWordAtRowCol(SynEditor.CaretXY);
      end;
      SearchTextHistory := SearchOptions.SearchTextHistory;
      if AReplace then
        with dlg as TTextReplaceDialog do
        begin
          ReplaceText := SearchOptions.ReplaceText;
          ReplaceTextHistory := SearchOptions.ReplaceTextHistory;
        end;
      SearchWholeWords := SearchOptions.SearchWholeWords;
      if ShowModal = mrOK then
      begin
        SearchOptions.InitSearch;
        SearchOptions.SearchBackwards := SearchBackwards;
        SearchOptions.SearchCaseSensitive := SearchCaseSensitive;
        SearchOptions.SearchFromCaret := SearchFromCursor;
        SearchOptions.SearchSelectionOnly := SearchInSelectionOnly;
        SearchOptions.SearchWholeWords := SearchWholeWords;
        SearchOptions.SearchRegex := SearchRegularExpression;

        ClearSearchHighlight(SynEditor);
        SearchOptions.SearchText := SearchText;
        SearchOptions.SearchTextHistory := SearchTextHistory;
        if AReplace then
          with dlg as TTextReplaceDialog do
          begin
            SearchOptions.ReplaceText := ReplaceText;
            SearchOptions.ReplaceTextHistory := ReplaceTextHistory;
          end;
        SearchOptions.TempSearchFromCaret := SearchOptions.SearchFromCaret;
        if SearchOptions.SearchText <> '' then
        begin
          if SearchOptions.SearchRegex then
            SynEditor.SearchEngine := SynEditRegexSearch
          else
            SynEditor.SearchEngine := SynEditSearch;
          Options := SearchOptions.SetSynSearchOptions;
          if not AReplace then
            HighligthtSearchTerm(SearchOptions.SearchText, SynEditor, SynEditor.SearchEngine, Options);
          DoSearchReplaceText(AReplace, SearchOptions.SearchBackwards);
          SearchOptions.TempSearchFromCaret := True;
        end;
      end;
    finally
      dlg.Free;
    end;
end;

{ event handler }
procedure TSearchReplaceDemoForm.ActionFileOpenExecute(Sender: TObject);
begin
  if OpenDialogFile.Execute then
  begin
    SynEditor.LockUndo;
    try
      SynEditor.Lines.LoadFromFile(OpenDialogFile.FileName);
    finally
      SynEditor.UnlockUndo;
    end;
    SynEditor.ReadOnly := ofReadOnly in OpenDialogFile.Options;
    SynEditor.ClearUndo;
    SynEditor.Modified := False;
  end;
end;

procedure TSearchReplaceDemoForm.ActionSearchExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(False);
end;

procedure TSearchReplaceDemoForm.ActionSearchNextExecute(Sender: TObject);
begin
  DoSearchReplaceText(False, False);
end;

procedure TSearchReplaceDemoForm.ActionSearchPrevExecute(Sender: TObject);
begin
  DoSearchReplaceText(False, True);
end;

procedure TSearchReplaceDemoForm.ActionSearchReplaceExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(True);
end;

procedure TSearchReplaceDemoForm.actSearchUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := SearchOptions.SearchText <> '';
end;

procedure TSearchReplaceDemoForm.ActionSearchReplaceUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not SynEditor.ReadOnly;
end;

procedure TSearchReplaceDemoForm.SynEditorMouseDown(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SearchOptions.InitSearch;
end;

procedure TSearchReplaceDemoForm.SynEditorProcessCommand(Sender: TObject; var
    Command: TSynEditorCommand; var AChar: WideChar; Data: Pointer);
begin
  if (Command <> ecLostFocus) and (Command <> ecGotFocus) then
    SearchOptions.InitSearch;
  if Command = ecCancelSelections then
     ClearSearchHighlight(SynEditor);
end;

procedure TSearchReplaceDemoForm.SynEditorReplaceText(Sender: TObject; const
  ASearch, AReplace: string; Line, Column: Integer; var Action:
  TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else
  begin
    APos := SynEditor.ClientToScreen(
      SynEditor.RowColumnToPixels(
      SynEditor.BufferToDisplayPos(
      BufferCoord(Column, Line))));
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);
    if ConfirmReplaceDialog = nil then
      ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
    ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
      APos.Y + SynEditor.LineHeight, ASearch);
    case ConfirmReplaceDialog.ShowModal of
      mrYes: Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo: Action := raSkip;
    else
      Action := raCancel;
    end;
  end;
end;
{ TSearchOptions }

procedure TSearchOptions.InitSearch;
begin
  SelStorage.Selections := [];
  TempLastMatch.Start := TBufferCoord.Invalid;
end;

function TSearchOptions.HasStoredSelection: Boolean;
begin
  Result := Length(SelStorage.Selections) > 0
end;

function TSearchOptions.SetSynSearchOptions: TSynSearchOptions;
begin
  Result := [];
  if SearchOptions.SearchBackwards then
    Include(Result, ssoBackwards);
  if SearchOptions.SearchCaseSensitive then
    Include(Result, ssoMatchCase);
  if not SearchOptions.TempSearchFromCaret then
    Include(Result, ssoEntireScope);
  if SearchOptions.SearchSelectionOnly then
    Include(Result, ssoSelectedOnly);
  if SearchOptions.SearchWholeWords then
    Include(Result, ssoWholeWord);
end;

end.

