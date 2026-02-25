{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: frmMain.pas, released 2000-09-08.

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

unit EditAppMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, uEditAppIntfs, ComCtrls, System.Actions;

type
  TMainForm = class(TForm)
    mnuMain: TMainMenu;
    mFile: TMenuItem;
    miFileExit: TMenuItem;
    miFileNew: TMenuItem;
    N1: TMenuItem;
    mEdit: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    miFileClose: TMenuItem;
    miEditUndo: TMenuItem;
    miEditRedo: TMenuItem;
    N2: TMenuItem;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    miEditDelete: TMenuItem;
    miEditSelectAll: TMenuItem;
    N3: TMenuItem;
    miEditFind: TMenuItem;
    miEditFindNext: TMenuItem;
    miEditFindPrev: TMenuItem;
    miEditReplace: TMenuItem;
    StatusBar: TStatusBar;
    miViewStatusbar: TMenuItem;
    mView: TMenuItem;
    N4: TMenuItem;
    mRecentFiles: TMenuItem;
    miFileMRU5: TMenuItem;
    miFileMRU4: TMenuItem;
    miFileMRU3: TMenuItem;
    miFileMRU2: TMenuItem;
    miFileMRU1: TMenuItem;
    N5: TMenuItem;
    miFilePrint: TMenuItem;
    actlStandard: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileExit: TAction;
    actViewStatusbar: TAction;
    actUpdateStatusBarPanels: TAction;
    actFileCloseAll: TAction;
    miFileCloseAll: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mFileClick(Sender: TObject);
    procedure actFileNewOrOpenUpdate(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure mRecentFilesClick(Sender: TObject);
    procedure actViewStatusbarUpdate(Sender: TObject);
    procedure actViewStatusbarExecute(Sender: TObject);
    procedure OnOpenMRUFile(Sender: TObject);
    procedure actUpdateStatusBarPanelsUpdate(Sender: TObject);
    procedure actFileCloseAllExecute(Sender: TObject);
    procedure actFileCloseAllUpdate(Sender: TObject);
  protected
    fMRUItems: array[1..5] of TMenuItem;
    function CanCloseAll: Boolean;
    function CmdLineOpenFiles(AMultipleFiles: Boolean): Boolean;
    function DoCreateEditor(AFileName: string): IEditor; virtual;
    procedure DoOpenFile(AFileName: string);
    procedure ReadIniSettings;
    procedure WriteIniSettings;
  end;

implementation

{$R *.DFM}

uses
  IniFiles, dmWorkbookCommands;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fMRUItems[1] := miFileMRU1;
  fMRUItems[2] := miFileMRU2;
  fMRUItems[3] := miFileMRU3;
  fMRUItems[4] := miFileMRU4;
  fMRUItems[5] := miFileMRU5;
  CommandsDataModule := TCommandsDataModule.Create(Self);
  ReadIniSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if GI_EditorFactory <> nil then
    GI_EditorFactory.CloseAll;
  WriteIniSettings;
  CommandsDataModule.Free;
end;

// implementation

function TMainForm.CanCloseAll: Boolean;
begin
  Result := True;
end;

function TMainForm.CmdLineOpenFiles(AMultipleFiles: Boolean): Boolean;
var
  I, Cnt: Integer;
begin
  Cnt := ParamCount;
  if Cnt > 0 then begin
    if not AMultipleFiles and (Cnt > 1) then
      Cnt := 1;
    for I := 1 to Cnt do
      DoOpenFile(ParamStr(I));
    Result := True;
  end else
    Result := False;
end;

function TMainForm.DoCreateEditor(AFileName: string): IEditor;
begin
  Result := nil;
end;

procedure TMainForm.DoOpenFile(AFileName: string);
var
  I: Integer;
  LEditor: IEditor;
begin
  AFileName := ExpandFileName(AFileName);
  if AFileName <> '' then begin
    CommandsDataModule.RemoveMRUEntry(AFileName);
    // activate the editor if already open
    Assert(GI_EditorFactory <> nil);
    for I := GI_EditorFactory.GetEditorCount - 1 downto 0 do begin
      LEditor := GI_EditorFactory.Editor[I];
      if CompareText(LEditor.GetFileName, AFileName) = 0 then begin
        LEditor.Activate;
        Exit;
      end;
    end;
  end;
  // create a new editor, add it to the editor list, open the file
  LEditor := DoCreateEditor(AFileName);
  if LEditor <> nil then
    LEditor.OpenFile(AFileName);
end;

procedure TMainForm.ReadIniSettings;
var
  IniFile: TIniFile;
  X, Y, W, H: Integer;
  I: Integer;
  S: string;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    X := IniFile.ReadInteger('Main', 'Left', 0);
    Y := IniFile.ReadInteger('Main', 'Top', 0);
    W := IniFile.ReadInteger('Main', 'Width', 0);
    H := IniFile.ReadInteger('Main', 'Height', 0);
    if (W > 0) and (H > 0) then
      SetBounds(X, Y, W, H);
    if IniFile.ReadInteger('Main', 'Maximized', 0) <> 0 then
      WindowState := wsMaximized;
    StatusBar.Visible := IniFile.ReadInteger('Main', 'ShowStatusbar', 1) <> 0;
    // MRU files
    for I := 5 downto 1 do begin
      S := IniFile.ReadString('MRUFiles', Format('MRUFile%d', [I]), '');
      if S <> '' then
        CommandsDataModule.AddMRUEntry(S);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.WriteIniSettings;
var
  IniFile: TIniFile;
  WP: TWindowPlacement;
  I: Integer;
  S: string;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    WP.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @WP);
    // form properties
    with WP.rcNormalPosition do begin
      IniFile.WriteInteger('Main', 'Left', Left);
      IniFile.WriteInteger('Main', 'Top', Top);
      IniFile.WriteInteger('Main', 'Width', Right - Left);
      IniFile.WriteInteger('Main', 'Height', Bottom - Top);
    end;
    IniFile.WriteInteger('Main', 'Maximized', Ord(WindowState = wsMaximized));
    IniFile.WriteInteger('Main', 'ShowStatusbar', Ord(Statusbar.Visible));
    // MRU files
    for I := 1 to 5 do begin
      S := CommandsDataModule.GetMRUEntry(I - 1);
      if S <> '' then
        IniFile.WriteString('MRUFiles', Format('MRUFile%d', [I]), S)
      else
        IniFile.DeleteKey('MRUFiles', Format('MRUFile%d', [I]));
    end;
  finally
    IniFile.Free;
  end;
end;

// action handler methods

procedure TMainForm.actFileNewOrOpenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GI_EditorFactory <> nil;
end;

procedure TMainForm.actFileNewExecute(Sender: TObject);
begin
  DoOpenFile('');
end;

procedure TMainForm.actFileOpenExecute(Sender: TObject);
begin
  with CommandsDataModule.dlgFileOpen do begin
    if Execute then
      DoOpenFile(FileName);
  end;
end;

procedure TMainForm.actFileCloseAllExecute(Sender: TObject);
var
  I: Integer;
begin
  if GI_EditorFactory <> nil then begin
    if not CanCloseAll then
      Exit;
    I := GI_EditorFactory.GetEditorCount - 1;
    // close all editor childs
    while I >= 0 do begin
      GI_EditorFactory.GetEditor(I).Close;
      Dec(I);
    end;
  end;
end;

procedure TMainForm.actFileCloseAllUpdate(Sender: TObject);
begin
  actFileCloseAll.Enabled := (GI_EditorFactory <> nil)
    and (GI_EditorFactory.GetEditorCount > 0);
end;

procedure TMainForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.mRecentFilesClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := Low(fMRUItems) to High(fMRUItems) do
    if fMRUItems[I] <> nil then begin
      S := CommandsDataModule.GetMRUEntry(I - Low(fMRUItems));
      fMRUItems[I].Visible := S <> '';
      fMRUItems[I].Caption := S;
    end;
end;

procedure TMainForm.mFileClick(Sender: TObject);
begin
  mRecentFiles.Enabled := CommandsDataModule.GetMRUEntries > 0;
end;

procedure TMainForm.actViewStatusbarUpdate(Sender: TObject);
begin
  actViewStatusbar.Checked := StatusBar.Visible;
end;

procedure TMainForm.actViewStatusbarExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TMainForm.OnOpenMRUFile(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := Low(fMRUItems) to High(fMRUItems) do
    if Sender = fMRUItems[I] then begin
      S := CommandsDataModule.GetMRUEntry(I - 1);
      if S <> '' then
        DoOpenFile(S);
    end;
end;

procedure TMainForm.actUpdateStatusBarPanelsUpdate(Sender: TObject);
resourcestring
  SModified = 'Modified';
var
  ptCaret: TPoint;
begin
  actUpdateStatusBarPanels.Enabled := True;
  if GI_ActiveEditor <> nil then begin
    ptCaret := GI_ActiveEditor.GetCaretPos;
    if (ptCaret.X > 0) and (ptCaret.Y > 0) then
      StatusBar.Panels[0].Text := Format(' %6d:%3d ', [ptCaret.Y, ptCaret.X])
    else
      StatusBar.Panels[0].Text := '';
    if GI_ActiveEditor.GetModified then
      StatusBar.Panels[1].Text := SModified
    else
      StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := GI_ActiveEditor.GetEditorState;
  end else begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
  end;
end;

end.

