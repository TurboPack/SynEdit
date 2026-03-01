{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

FMX undo/redo — thin subclass of shared TSynEditUndoBase.
Only the caret/selection undo item and event-handler wiring are FMX-specific.
-------------------------------------------------------------------------------}

unit FMX.SynEditUndo;

{$I SynEdit.inc}

interface

uses
  FMX.SynEdit,
  SynEditTypes,
  SynEditKeyCmds;

{ Factory Method }
function CreateSynEditUndo(Editor: TCustomFMXSynEdit): ISynEditUndo;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  SynEditMiscProcs,
  SynEditSelections,
  SynEditTextBuffer,
  SynEditUndoShared;

type
  TSynEditUndo = class;

  TSynCaretAndSelectionUndoItem = class(TSynUndoItem)
  private
    FBlockBegin: TBufferCoord;
    FBlockEnd: TBufferCoord;
    FSelStorage: TSynSelStorage;
    FHasMultiSel: Boolean;
  public
    procedure Undo(Editor: TObject); override;
    procedure Redo(Editor: TObject); override;
    constructor Create(Editor: TCustomFMXSynEdit);
  end;

  TSynEditUndo = class(TSynEditUndoBase)
  private
    FEditor: TCustomFMXSynEdit;
    // Event handlers for TSynEditStringList
    procedure LinePut(Sender: TObject; Index: Integer; const OldLine: string);
    procedure LinesBeforeDeleted(Sender: TObject; Index, Count: Integer);
    procedure LinesDeleted(Sender: TObject; Index, Count: Integer);
    procedure LinesInserted(Sender: TObject; Index, Count: Integer);
  protected
    function CreateCaretUndoItem(Editor: TObject): TSynUndoItem; override;
    procedure RestoreCaretAndSelection(Editor: TObject;
      Item: TSynUndoItem); override;
  public
    constructor Create(AEditor: TCustomFMXSynEdit);
  end;

{ TSynEditUndo }

constructor TSynEditUndo.Create(AEditor: TCustomFMXSynEdit);
begin
  inherited Create;
  FEditor := AEditor;
  // Hook into TSynEditStringList events
  TSynEditStringList(FEditor.Lines).OnPut := LinePut;
  TSynEditStringList(FEditor.Lines).OnInserted := LinesInserted;
  TSynEditStringList(FEditor.Lines).OnBeforeDeleted := LinesBeforeDeleted;
  TSynEditStringList(FEditor.Lines).OnDeleted := LinesDeleted;
end;

function TSynEditUndo.CreateCaretUndoItem(Editor: TObject): TSynUndoItem;
begin
  Result := TSynCaretAndSelectionUndoItem.Create(
    Editor as TCustomFMXSynEdit);
end;

procedure TSynEditUndo.RestoreCaretAndSelection(Editor: TObject;
  Item: TSynUndoItem);
var
  Ed: TCustomFMXSynEdit;
begin
  if not (Item is TSynCaretAndSelectionUndoItem) then
  begin
    Ed := Editor as TCustomFMXSynEdit;
    Ed.SetCaretAndSelection(Item.FCaret, Item.FCaret, Item.FCaret);
  end;
end;

{ Event handlers - hook into TSynEditStringList }

procedure TSynEditUndo.LinePut(Sender: TObject; Index: Integer;
  const OldLine: string);
var
  Line: string;
  Item: TSynLinePutUndoItem;
begin
  if IsLocked or FInsideUndoRedo then Exit;
  // Adjust remaining selections for line-change
  FEditor.Selections.LinePut(Index, OldLine);
  Line := FEditor.Lines[Index];
  if Line <> OldLine then
  begin
    Item := TSynLinePutUndoItem.Create(FEditor.Lines, Index, OldLine,
      FCommandProcessed);
    AddUndoItem(Item);
  end;
end;

procedure TSynEditUndo.LinesBeforeDeleted(Sender: TObject; Index, Count: Integer);
var
  I: Integer;
begin
  if IsLocked or FInsideUndoRedo then Exit;
  SetLength(FDeletedLines, Count);
  SetLength(FDeletedChangeFlags, Count);
  for I := 0 to Count - 1 do
  begin
    FDeletedLines[I] := FEditor.Lines[Index + I];
    FDeletedChangeFlags[I] :=
      TSynEditStringList(FEditor.Lines).ChangeFlags[Index + I];
  end;
end;

procedure TSynEditUndo.LinesDeleted(Sender: TObject; Index, Count: Integer);
var
  Item: TSynLinesDeletedUndoItem;
begin
  if IsLocked or FInsideUndoRedo then Exit;
  // Adjust remaining selections for line-change
  FEditor.Selections.LinesDeleted(Index, Count);
  if Count > 0 then
  begin
    Item := TSynLinesDeletedUndoItem.Create(FEditor.Lines, Index,
      FDeletedLines, FDeletedChangeFlags);
    AddUndoItem(Item);
  end;
end;

procedure TSynEditUndo.LinesInserted(Sender: TObject; Index, Count: Integer);
var
  Item: TSynLinesInsertedUndoItem;
begin
  if IsLocked or FInsideUndoRedo then Exit;
  // Adjust remaining selections for line-change
  FEditor.Selections.LinesInserted(Index, Count);
  // Consider a file with one empty line as empty
  if (FUndoList.Count = 0) and
    (FEditor.Lines.Count = 1) and (FEditor.Lines[0] = '')
  then
    Exit;
  if Count > 0 then
  begin
    Item := TSynLinesInsertedUndoItem.Create(FEditor.Lines, Index, Count);
    AddUndoItem(Item);
  end;
end;

{ Factory Method }

function CreateSynEditUndo(Editor: TCustomFMXSynEdit): ISynEditUndo;
begin
  Result := TSynEditUndo.Create(Editor);
end;

{ TSynCaretAndSelectionUndoItem }

constructor TSynCaretAndSelectionUndoItem.Create(Editor: TCustomFMXSynEdit);
begin
  inherited Create;
  FCaret := Editor.CaretXY;
  FBlockBegin := Editor.BlockBegin;
  FBlockEnd := Editor.BlockEnd;
  FHasMultiSel := Editor.Selections.Count > 1;
  if FHasMultiSel then
    Editor.Selections.Store(FSelStorage);
end;

procedure TSynCaretAndSelectionUndoItem.Undo(Editor: TObject);
var
  Ed: TCustomFMXSynEdit;
begin
  Ed := Editor as TCustomFMXSynEdit;
  if FHasMultiSel then
    Ed.Selections.Restore(FSelStorage)
  else
    Ed.SetCaretAndSelection(FCaret, FBlockBegin, FBlockEnd);
end;

procedure TSynCaretAndSelectionUndoItem.Redo(Editor: TObject);
begin
  Undo(Editor);
end;

end.
