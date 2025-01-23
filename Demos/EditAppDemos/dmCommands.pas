{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dmCommands.pas, released 2000-09-08.

The Original Code is part of the EditAppDemos project, written by
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

unit dmCommands;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, SynHighlighterSQL, SynHighlighterPas, SynEditHighlighter,
  SynHighlighterCpp, SynEditCodeFolding, System.Actions;

type
  TCommandsDataModule = class(TDataModule)
    dlgFileOpen: TOpenDialog;
    actlMain: TActionList;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileClose: TAction;
    actFilePrint: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;
    actEditUndo: TAction;
    actEditRedo: TAction;
    actEditSelectAll: TAction;
    actSearchFind: TAction;
    actSearchFindNext: TAction;
    actSearchFindPrev: TAction;
    actSearchReplace: TAction;
    SynCppSyn1: TSynCppSyn;
    SynPasSyn1: TSynPasSyn;
    SynSQLSyn1: TSynSQLSyn;
    dlgFileSave: TSaveDialog;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveUpdate(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveAsUpdate(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFilePrintUpdate(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileCloseUpdate(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditCutUpdate(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCopyUpdate(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditPasteUpdate(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditDeleteUpdate(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actEditSelectAllUpdate(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure actEditRedoUpdate(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actEditUndoUpdate(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
    procedure actSearchFindUpdate(Sender: TObject);
    procedure actSearchFindNextExecute(Sender: TObject);
    procedure actSearchFindNextUpdate(Sender: TObject);
    procedure actSearchFindPrevExecute(Sender: TObject);
    procedure actSearchFindPrevUpdate(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure actSearchReplaceUpdate(Sender: TObject);
  private
    fHighlighters: TStringList;
    fMRUFiles: TStringList;
    fUntitledNumbers: TBits;
  public
    procedure AddMRUEntry(AFileName: string);
    function GetHighlighterForFile(AFileName: string): TSynCustomHighlighter;
    function GetMRUEntries: Integer;
    function GetMRUEntry(Index: Integer): string;
    function GetSaveFileName(var ANewName: string;
      AHighlighter: TSynCustomHighlighter): Boolean;
    function GetUntitledNumber: Integer;
    procedure ReleaseUntitledNumber(ANumber: Integer);
    procedure RemoveMRUEntry(AFileName: string);
  end;

var
  CommandsDataModule: TCommandsDataModule;

implementation

{$R *.DFM}

uses
  uHighlighterProcs, uEditAppIntfs;

const
  MAX_MRU = 5;

resourcestring
  SFilterAllFiles = 'All files|*.*|';

{ TCommandsDataModule }

procedure TCommandsDataModule.DataModuleCreate(Sender: TObject);
begin
  fHighlighters := TStringList.Create;
  GetHighlighters(Self, fHighlighters, False);
  dlgFileOpen.Filter := GetHighlightersFilter(fHighlighters) + SFilterAllFiles;
  fMRUFiles := TStringList.Create;
end;

procedure TCommandsDataModule.DataModuleDestroy(Sender: TObject);
begin
  fMRUFiles.Free;
  fHighlighters.Free;
  fUntitledNumbers.Free;
  CommandsDataModule := nil;
end;

// implementation

procedure TCommandsDataModule.AddMRUEntry(AFileName: string);
begin
  if AFileName <> '' then begin
    RemoveMRUEntry(AFileName);
    fMRUFiles.Insert(0, AFileName);
    while fMRUFiles.Count > MAX_MRU do
      fMRUFiles.Delete(fMRUFiles.Count - 1);
  end;
end;

function TCommandsDataModule.GetHighlighterForFile(
  AFileName: string): TSynCustomHighlighter;
begin
  if AFileName <> '' then
    Result := GetHighlighterFromFileExt(fHighlighters, ExtractFileExt(AFileName))
  else
    Result := nil;
end;

function TCommandsDataModule.GetMRUEntries: Integer;
begin
  Result := fMRUFiles.Count;
end;

function TCommandsDataModule.GetMRUEntry(Index: Integer): string;
begin
  if (Index >= 0) and (Index < fMRUFiles.Count) then
    Result := fMRUFiles[Index]
  else
    Result := '';
end;

function TCommandsDataModule.GetSaveFileName(var ANewName: string;
  AHighlighter: TSynCustomHighlighter): Boolean;
begin
  with dlgFileSave do begin
    if ANewName <> '' then begin
      InitialDir := ExtractFileDir(ANewName);
      FileName := ExtractFileName(ANewName);
    end else begin
      InitialDir := '';
      FileName := '';
    end;
    if AHighlighter <> nil then
      Filter := AHighlighter.DefaultFilter
    else
      Filter := SFilterAllFiles;
    if Execute then begin
      ANewName := FileName;
      Result := True;
    end else
      Result := False;
  end;
end;

function TCommandsDataModule.GetUntitledNumber: Integer;
begin
  if fUntitledNumbers = nil then
    fUntitledNumbers := TBits.Create;
  Result := fUntitledNumbers.OpenBit;
  if Result = fUntitledNumbers.Size then
    fUntitledNumbers.Size := fUntitledNumbers.Size + 32;
  fUntitledNumbers[Result] := True;
  Inc(Result);
end;

procedure TCommandsDataModule.ReleaseUntitledNumber(ANumber: Integer);
begin
  Dec(ANumber);
  if (fUntitledNumbers <> nil) and (ANumber >= 0)
    and (ANumber < fUntitledNumbers.Size)
  then
    fUntitledNumbers[ANumber] := False;
end;

procedure TCommandsDataModule.RemoveMRUEntry(AFileName: string);
var
  i: Integer;
begin
  for i := fMRUFiles.Count - 1 downto 0 do begin
    if CompareText(AFileName, fMRUFiles[i]) = 0 then
      fMRUFiles.Delete(i);
  end;
end;

procedure TCommandsDataModule.actFileSaveExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecSave;
end;

procedure TCommandsDataModule.actFileSaveUpdate(Sender: TObject);
begin
  actFileSave.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanSave;
end;

procedure TCommandsDataModule.actFileSaveAsExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecSaveAs;
end;

procedure TCommandsDataModule.actFileSaveAsUpdate(Sender: TObject);
begin
  actFileSaveAs.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanSaveAs;
end;

procedure TCommandsDataModule.actFilePrintExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecPrint;
end;

procedure TCommandsDataModule.actFilePrintUpdate(Sender: TObject);
begin
  actFilePrint.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanPrint;
end;

procedure TCommandsDataModule.actFileCloseExecute(Sender: TObject);
begin
  if GI_FileCmds <> nil then
    GI_FileCmds.ExecClose;
end;

procedure TCommandsDataModule.actFileCloseUpdate(Sender: TObject);
begin
  actFileClose.Enabled := (GI_FileCmds <> nil) and GI_FileCmds.CanClose;
end;

procedure TCommandsDataModule.actEditCutExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecCut;
end;

procedure TCommandsDataModule.actEditCutUpdate(Sender: TObject);
begin
  actEditCut.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanCut;
end;

procedure TCommandsDataModule.actEditCopyExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecCopy;
end;

procedure TCommandsDataModule.actEditCopyUpdate(Sender: TObject);
begin
  actEditCopy.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanCopy;
end;

procedure TCommandsDataModule.actEditPasteExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecPaste;
end;

procedure TCommandsDataModule.actEditPasteUpdate(Sender: TObject);
begin
  actEditPaste.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanPaste;
end;

procedure TCommandsDataModule.actEditDeleteExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecDelete;
end;

procedure TCommandsDataModule.actEditDeleteUpdate(Sender: TObject);
begin
  actEditDelete.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanDelete;
end;

procedure TCommandsDataModule.actEditSelectAllExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecSelectAll;
end;

procedure TCommandsDataModule.actEditSelectAllUpdate(Sender: TObject);
begin
  actEditSelectAll.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanSelectAll;
end;

procedure TCommandsDataModule.actEditRedoExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecRedo;
end;

procedure TCommandsDataModule.actEditRedoUpdate(Sender: TObject);
begin
  actEditRedo.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanRedo;
end;

procedure TCommandsDataModule.actEditUndoExecute(Sender: TObject);
begin
  if GI_EditCmds <> nil then
    GI_EditCmds.ExecUndo;
end;

procedure TCommandsDataModule.actEditUndoUpdate(Sender: TObject);
begin
  actEditUndo.Enabled := (GI_EditCmds <> nil) and GI_EditCmds.CanUndo;
end;

procedure TCommandsDataModule.actSearchFindExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecFind;
end;

procedure TCommandsDataModule.actSearchFindUpdate(Sender: TObject);
begin
  actSearchFind.Enabled := (GI_SearchCmds <> nil) and GI_SearchCmds.CanFind;
end;

procedure TCommandsDataModule.actSearchFindNextExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecFindNext;
end;

procedure TCommandsDataModule.actSearchFindNextUpdate(Sender: TObject);
begin
  actSearchFindNext.Enabled := (GI_SearchCmds <> nil)
    and GI_SearchCmds.CanFindNext;
end;

procedure TCommandsDataModule.actSearchFindPrevExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecFindPrev;
end;

procedure TCommandsDataModule.actSearchFindPrevUpdate(Sender: TObject);
begin
  actSearchFindPrev.Enabled := (GI_SearchCmds <> nil) and GI_SearchCmds.CanFindPrev;
end;

procedure TCommandsDataModule.actSearchReplaceExecute(Sender: TObject);
begin
  if GI_SearchCmds <> nil then
    GI_SearchCmds.ExecReplace;
end;

procedure TCommandsDataModule.actSearchReplaceUpdate(Sender: TObject);
begin
  actSearchReplace.Enabled := (GI_SearchCmds <> nil)
    and GI_SearchCmds.CanReplace;
end;

end.

 