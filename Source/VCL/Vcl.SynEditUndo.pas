{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Contributors to the SynEdit and mwEdit projects are listed in the
  Contributors.txt file.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  Known Issues:
  ------------------------------------------------------------------------------- }

unit Vcl.SynEditUndo;

{$I SynEdit.inc}

interface

uses
  SynEdit,
  SynEditTypes,
  SynEditSelections,
  SynEditKeyCmds;

{ Factory Method}

function CreateSynEditUndo(Editor: TCustomSynEdit): ISynEditUndo;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  Vcl.Controls,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer,
  SynEditUndoShared;

type
  TSynEditUndo = class;

  TSynCaretAndSelectionUndoItem = class(TSynUndoItem)
  private
    FBlockBegin: TBufferCoord;
    FBlockEnd: TBufferCoord;
    SelStorage: TSynSelStorage;
  public
    procedure Undo(Editor: TObject); override;
    procedure Redo(Editor: TObject); override;
    constructor Create(Editor: TCustomSynEdit);
  end;

  TSynUndoPlugin = class(TSynEditPlugin)
  private
    FSynEditUndo: TSynEditUndo;
    FDeletedLines: TArray<string>;
    FDeletedChangeFlags: TArray<TSynLineChangeFlags>;
  protected
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesBeforeDeleted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
    procedure LinePut(aIndex: Integer; const OldLine: string); override;
  public
    constructor Create(SynEditUndo: TSynEditUndo; Editor: TCustomSynEdit);
  end;

  TSynEditUndo = class(TSynEditUndoBase)
  private
    FPlugin: TSynUndoPlugin;
  protected
    function CreateCaretUndoItem(Editor: TObject): TSynUndoItem; override;
    procedure RestoreCaretAndSelection(Editor: TObject;
      Item: TSynUndoItem); override;
  public
    constructor Create(Editor: TCustomSynEdit);
  end;

{ TSynEditUndo }

constructor TSynEditUndo.Create(Editor: TCustomSynEdit);
begin
  inherited Create;
  FPlugin := TSynUndoPlugin.Create(Self, Editor);
end;

function TSynEditUndo.CreateCaretUndoItem(Editor: TObject): TSynUndoItem;
begin
  Result := TSynCaretAndSelectionUndoItem.Create(Editor as TCustomSynEdit);
end;

procedure TSynEditUndo.RestoreCaretAndSelection(Editor: TObject;
  Item: TSynUndoItem);
var
  Ed: TCustomSynEdit;
begin
  if not (Item is TSynCaretAndSelectionUndoItem) then
  begin
    Ed := Editor as TCustomSynEdit;
    Ed.Selections.Clear;
    Ed.SetCaretAndSelection(Item.FCaret, Item.FCaret, Item.FCaret);
  end;
end;

{ Factory Method}

function CreateSynEditUndo(Editor: TCustomSynEdit): ISynEditUndo;
begin
  Result := TSynEditUndo.Create(Editor);
end;

{ TSynCaretAndSelectionUndoItem }

constructor TSynCaretAndSelectionUndoItem.Create(Editor: TCustomSynEdit);
begin
  inherited Create;
  if Editor.Selections.Count = 1 then
  begin
    FCaret := Editor.CaretXY;
    FBlockBegin := Editor.BlockBegin;
    FBlockEnd := Editor.BlockEnd;
  end
  else
  begin
    Editor.Selections.Store(SelStorage);
  end;
end;

procedure TSynCaretAndSelectionUndoItem.Redo(Editor: TObject);
begin
  Undo(Editor);
end;

procedure TSynCaretAndSelectionUndoItem.Undo(Editor: TObject);
var
  Ed: TCustomSynEdit;
begin
  Ed := Editor as TCustomSynEdit;
  if Length(SelStorage.Selections) > 0 then
    Ed.Selections.Restore(SelStorage)
  else
  begin
    Ed.Selections.Clear;
    Ed.SetCaretAndSelection(FCaret, FBlockBegin, FBlockEnd);
  end;
end;

{ TSynUndoPlugin }

constructor TSynUndoPlugin.Create(SynEditUndo: TSynEditUndo;
  Editor: TCustomSynEdit);
begin
  FSynEditUndo := SynEditUndo;
  inherited Create(Editor,
    [phLinePut, phLinesInserted, phLinesBeforeDeleted, phLinesDeleted]);
end;

procedure TSynUndoPlugin.LinePut(aIndex: Integer; const OldLine: string);
var
  Line: string;
  Item: TSynLinePutUndoItem;
begin
  if Editor.IsChained or FSynEditUndo.IsLocked or FSynEditUndo.FInsideUndoRedo
  then
    Exit;

  Line := Editor.Lines[aIndex];
  if Line <> OldLine then
  begin
    Item := TSynLinePutUndoItem.Create(Editor.Lines, aIndex, OldLine,
      FSynEditUndo.FCommandProcessed);
    FSynEditUndo.AddUndoItem(Item);
  end;
end;

procedure TSynUndoPlugin.LinesBeforeDeleted(FirstLine, Count: Integer);
var
  I: Integer;
begin
  if Editor.IsChained or FSynEditUndo.IsLocked or FSynEditUndo.FInsideUndoRedo
  then
    Exit;

  SetLength(FDeletedLines, Count);
  SetLength(FDeletedChangeFlags, Count);
  for I := 0 to Count -1 do
  begin
    FDeletedLines[I] := Editor.Lines[FirstLine + I];
    FDeletedChangeFlags[I] :=
      TSynEditStringList(Editor.Lines).ChangeFlags[FirstLine + I];
  end;
end;

procedure TSynUndoPlugin.LinesDeleted(FirstLine, Count: Integer);
var
  Item: TSynLinesDeletedUndoItem;
begin
  if Editor.IsChained or FSynEditUndo.IsLocked or FSynEditUndo.FInsideUndoRedo
  then
    Exit;

  if Count > 0 then
  begin
    Item := TSynLinesDeletedUndoItem.Create(Editor.Lines, FirstLine,
      FDeletedLines, FDeletedChangeFlags);
    FSynEditUndo.AddUndoItem(Item);
  end;
end;

procedure TSynUndoPlugin.LinesInserted(FirstLine, Count: Integer);
var
  Item: TSynLinesInsertedUndoItem;
begin
  if Editor.IsChained or FSynEditUndo.IsLocked or FSynEditUndo.FInsideUndoRedo
  then
    Exit;

  if (FSynEditUndo.FUndoList.Count = 0) and
    (Editor.Lines.Count = 1)  and (Editor.Lines[0] = '')
  then
    Exit;

  if Count > 0 then
  begin
    Item := TSynLinesInsertedUndoItem.Create(Editor.Lines, FirstLine, Count);
    FSynEditUndo.AddUndoItem(Item);
  end;
end;

end.
