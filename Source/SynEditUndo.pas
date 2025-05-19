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

unit SynEditUndo;

{$I SynEdit.inc}

interface

uses
  SynEdit,
  SynEditTypes,
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
  SynEditTextBuffer;

type

  TSynUndoItem = class abstract(TObject)
    ChangeNumber: Integer; // Undo items with the same change number are grouped
    FCaret: TBufferCoord;
    GroupBreak: Boolean; // Singnals not to group items together
  public
    procedure Undo(Editor: TCustomSynEdit); virtual; abstract;
    procedure Redo(Editor: TCustomSynEdit); virtual; abstract;
  end;

  TSynLinePutUndoItem = class(TSynUndoItem)
  private
    FIndex: Integer;
    FStartPos: Integer;
    FOldValue: string;
    FNewValue: string;
    FChangeFlags: TSynLineChangeFlags;
    FCommandProcessed: TSynEditorCommand;
  public
    function GroupWith(Item:TSynLinePutUndoItem): Boolean;
    procedure Undo(Editor: TCustomSynEdit); override;
    procedure Redo(Editor: TCustomSynEdit); override;
    constructor Create(Editor: TCustomSynEdit; Index: Integer; OldLine: string;
        Command: TSynEditorCommand);
  end;

  TSynLinesInsertedUndoItem = class(TSynUndoItem)
  private
    FIndex: Integer;
    FLines: TArray<string>;
    FChangeFlags: TArray<TSynLineChangeFlags>;
  public
    procedure Undo(Editor: TCustomSynEdit); override;
    procedure Redo(Editor: TCustomSynEdit); override;
    constructor Create(Editor: TCustomSynEdit; Index, Count: Integer);
  end;

  TSynLinesDeletedUndoItem = class(TSynUndoItem)
  private
    FIndex: Integer;
    FLines: TArray<string>;
    FChangeFlags: TArray<TSynLineChangeFlags>;
  public
    procedure Undo(Editor: TCustomSynEdit); override;
    procedure Redo(Editor: TCustomSynEdit); override;
    constructor Create(Editor: TCustomSynEdit; Index: Integer; DeletedLines:
        TArray<string>; DeletedChangeFlags: TArray<TSynLineChangeFlags>);
  end;

  TSynCaretAndSelectionUndoItem = class(TSynUndoItem)
  private
    FBlockBegin: TBufferCoord;
    FBlockEnd: TBufferCoord;
    SelStorage: TSynSelStorage;
  public
    procedure Undo(Editor: TCustomSynEdit); override;
    procedure Redo(Editor: TCustomSynEdit); override;
    constructor Create(Editor: TCustomSynEdit);
  end;

  TSynEditUndo = class;

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

  TSynEditUndoList = class(TObjectStack<TSynUndoItem>)
  protected
    FOwner: TSynEditUndo;
    FFullUndoImposible: Boolean;
    procedure EnsureMaxEntries;
  public
    constructor Create(Owner: TSynEditUndo);
    procedure Push(const Value: TSynUndoItem);
  end;

  TSynEditUndo = class(TInterfacedObject, ISynEditUndo)
  private
    FPlugin: TSynUndoPlugin;
    FGroupUndo: Boolean;
    FBlockCount: Integer;
    FLockCount: Integer;
    FBlockChangeNumber: Integer;
    FNextChangeNumber: Integer;
    FInitialChangeNumber: Integer;
    FMaxUndoActions: Integer;
    FBlockStartModified: Boolean;
    FUndoList: TSynEditUndoList;
    FRedoList: TSynEditUndoList;
    FOnModifiedChanged: TNotifyEvent;
    FInsideUndoRedo: Boolean;
    FCommandProcessed: TSynEditorCommand;
    FBlockSelRestoreItem: TSynUndoItem;
    function GetModified: Boolean;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetFullUndoImposible: Boolean;
    function GetOnModifiedChanged: TNotifyEvent;
    function GetInsideUndoRedo: Boolean;
    procedure SetModified(const Value: Boolean);
    procedure SetCommandProcessed(const Command: TSynEditorCommand);
    procedure SetMaxUndoActions(const Value: Integer);
    procedure SetOnModifiedChanged(const Value: TNotifyEvent);
    procedure SetGroupUndo(const Value: Boolean);
    function GetMaxUndoActions: Integer;
    procedure BeginBlock(Editor: TControl);
    procedure EndBlock(Editor: TControl);
    procedure Lock;
    procedure Unlock;
    function IsLocked: Boolean;
    procedure Clear;
    procedure Undo(Editor: TControl);
    procedure Redo(Editor: TControl);
    procedure BufferSaved(Lines: TStrings);
    procedure ClearTrackChanges(Lines: TStrings);

    function NextChangeNumber: Integer;
    procedure AddGroupBreak;
    procedure AddUndoItem(Item: TSynUndoItem);
  public
    constructor Create(Editor: TCustomSynEdit);
    destructor Destroy; override;
  end;

{ TSynEditUndoList }

constructor TSynEditUndoList.Create(Owner: TSynEditUndo);
begin
  inherited Create(True);
  FOwner := Owner;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  KeepCount: Integer;
  ItemArray: TArray<TSynUndoItem>;
  I: Integer;
begin
  if FOwner.FMaxUndoActions <= 0 then Exit;

  if Count > FOwner.FMaxUndoActions then
  begin
    FFullUndoImposible := True;
    KeepCount := (FOwner.FMaxUndoActions div 4) * 3;
    ItemArray := ToArray;
    for I := 1 to KeepCount do
      Extract;
    Clear;  // Destroys remaining items
    for I := Length(ItemArray) - KeepCount to Length(ItemArray) - 1 do
      Push(ItemArray[I]);
  end;
end;

procedure TSynEditUndoList.Push(const Value: TSynUndoItem);
begin
  inherited Push(Value);
  EnsureMaxEntries;
end;

{ TSynEditUndo }

procedure TSynEditUndo.AddUndoItem(Item: TSynUndoItem);
var
  OldModified: Boolean;
begin
  Assert(not FInsideUndoRedo);
  OldModified := GetModified;
  if FBlockChangeNumber <> 0 then
    Item.ChangeNumber := FBlockChangeNumber
  else
    Item.ChangeNumber := NextChangeNumber;
  FUndoList.Push(Item);
  FRedoList.Clear;
  // Do not sent unnecessary notifications
  if (FBlockCount = 0) and (OldModified xor GetModified) and
    Assigned(FOnModifiedChanged)
  then
    FOnModifiedChanged(Self);
end;

procedure TSynEditUndo.AddGroupBreak;
begin
  if (FUndoList.Count > 0) and (FBlockCount = 0) then
    FUndoList.Peek.GroupBreak := True;
end;

procedure TSynEditUndo.BeginBlock(Editor: TControl);
begin
  if IsLocked then Exit;

  Inc(FBlockCount);
  if FBlockCount = 1 then // it was 0
  begin
    FBlockStartModified := GetModified;
    // All undo items added until the matching EndBlock is called
    // will get the same change number and will be grouped together
    FBlockChangeNumber := NextChangeNumber;

    // So that position is restored after Redo
    FBlockSelRestoreItem := TSynCaretAndSelectionUndoItem.Create(Editor as TCustomSynEdit);
    FBlockSelRestoreItem.ChangeNumber := FBlockChangeNumber;
    FUndoList.Push(FBlockSelRestoreItem);
  end;
end;

procedure TSynEditUndo.BufferSaved(Lines: TStrings);

  procedure PutItemSaved(Item: TSynLinePutUndoItem);
  begin
    if Item.FChangeFlags = [sfAsSaved] then
      Item.FChangeFlags := [sfModified];
  end;

  procedure InsertedItemSaved(Item: TSynLinesInsertedUndoItem);
  var
    I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;

  procedure DeletedItemSaved(Item: TSynLinesDeletedUndoItem);
  var
    I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;

var
  SynLines: TSynEditStringList;
  Index: Integer;
  Flags: TSynLineChangeFlags;
  Item: TSynUndoItem;
begin
  SynLines := Lines as TSynEditStringList;
  // First change the flags of TSynEditStringList
  for Index := 0 to SynLines.Count - 1 do
  begin
    Flags := SynLines.ChangeFlags[Index];
    if Flags = [sfSaved] then
      // original line saved and then restored
      SynLines.ChangeFlags[Index] := []
    else if sfModified in Flags then
      SynLines.ChangeFlags[Index] := Flags - [sfModified] + [sfSaved, sfAsSaved];
  end;
  // Then modify the Undo/Redo lists
  for Item in FUndoList do
    if Item is TSynLinePutUndoItem then
      PutItemSaved(TSynLinePutUndoItem(Item))
    else if Item is TSynLinesInsertedUndoItem then
      InsertedItemSaved(TSynLinesInsertedUndoItem(Item))
    else if Item is TSynLinesDeletedUndoItem then
      DeletedItemSaved(TSynLinesDeletedUndoItem(Item));

  for Item in FRedoList do
    if Item is TSynLinePutUndoItem then
      PutItemSaved(TSynLinePutUndoItem(Item))
    else if Item is TSynLinesInsertedUndoItem then
      InsertedItemSaved(TSynLinesInsertedUndoItem(Item))
    else if Item is TSynLinesDeletedUndoItem then
      DeletedItemSaved(TSynLinesDeletedUndoItem(Item));
end;

procedure TSynEditUndo.Clear;
begin
  FUndoList.Clear;
  FRedoList.Clear;
end;

procedure TSynEditUndo.ClearTrackChanges(Lines: TStrings);
  procedure InsertedItemClear(Item: TSynLinesInsertedUndoItem);
  var
    I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;

  procedure DeletedItemClear(Item: TSynLinesDeletedUndoItem);
  var
    I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;
var
  SynLines: TSynEditStringList;
  Index: Integer;
  Item: TSynUndoItem;
begin
  SynLines := Lines as TSynEditStringList;
  // First change the flags of TSynEditStringList
  for Index := 0 to SynLines.Count - 1 do
    SynLines.ChangeFlags[Index] := [];
  // Then modify the Undo/Redo lists
  for Item in FUndoList do
    if Item is TSynLinesInsertedUndoItem then
      InsertedItemClear(TSynLinesInsertedUndoItem(Item))
    else if Item is TSynLinesDeletedUndoItem then
      DeletedItemClear(TSynLinesDeletedUndoItem(Item));

  for Item in FRedoList do
    if Item is TSynLinesInsertedUndoItem then
      InsertedItemClear(TSynLinesInsertedUndoItem(Item))
    else if Item is TSynLinesDeletedUndoItem then
      DeletedItemClear(TSynLinesDeletedUndoItem(Item));
end;

constructor TSynEditUndo.Create(Editor: TCustomSynEdit);
begin
  inherited Create;
  FGroupUndo := True;
  FMaxUndoActions := 0;
  FNextChangeNumber := 1;
  FUndoList := TSynEditUndoList.Create(Self);
  FRedoList := TSynEditUndoList.Create(Self);
  FPlugin := TSynUndoPlugin.Create(Self, Editor);
end;

destructor TSynEditUndo.Destroy;
begin
  FUndoList.Free;
  FRedoList.Free;
  inherited;
end;

procedure TSynEditUndo.EndBlock(Editor: TControl);
var
  Item: TSynCaretAndSelectionUndoItem;
begin
  if IsLocked then Exit;

  Assert(FBlockCount > 0);
  if FBlockCount > 0 then
  begin
    Dec(FBlockCount);
    if FBlockCount = 0 then
    begin
      if (FUndoList.Count > 0) and (FUndoList.Peek = FBlockSelRestoreItem) then
        // No undo items added from BlockBegin to BlockEnd
        FUndoList.Pop
      else
      begin
        // So that position is restored after Redo
        Item := TSynCaretAndSelectionUndoItem.Create(Editor as TCustomSynEdit);
        Item.ChangeNumber := FBlockChangeNumber;
        FUndoList.Push(Item);
      end;

      FBlockChangeNumber := 0;
      AddGroupBreak;
      if FBlockStartModified xor GetModified and Assigned(FOnModifiedChanged) then
        FOnModifiedChanged(Self);
    end;
  end;
end;

function TSynEditUndo.GetCanUndo: Boolean;
begin
  Result := FUndoList.Count > 0;
end;

function TSynEditUndo.GetFullUndoImposible: Boolean;
begin
  Result := FUndoList.FFullUndoImposible;
end;

function TSynEditUndo.GetInsideUndoRedo: Boolean;
begin
  Result := FInsideUndoRedo;
end;

function TSynEditUndo.GetMaxUndoActions: Integer;
begin
  Result := FMaxUndoActions;
end;

function TSynEditUndo.GetModified: Boolean;
begin
  if FUndoList.Count = 0 then
    Result := FInitialChangeNumber <> 0
  else
    Result := FUndoList.Peek.ChangeNumber <> FInitialChangeNumber;
end;

function TSynEditUndo.GetOnModifiedChanged: TNotifyEvent;
begin
  Result := FOnModifiedChanged;
end;

function TSynEditUndo.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

function TSynEditUndo.GetCanRedo: Boolean;
begin
  Result := FRedoList.Count > 0;
end;

procedure TSynEditUndo.Lock;
begin
  Inc(FLockCount);
end;

function TSynEditUndo.NextChangeNumber: Integer;
begin
  Result := FNextChangeNumber;
  Inc(FNextChangeNumber);
end;

procedure TSynEditUndo.Redo(Editor: TControl);
var
  Item, LastItem: TSynUndoItem;
  OldChangeNumber: Integer;
  OldModified: Boolean;
  FKeepGoing: Boolean;
  LastItemHasGroupBreak: Boolean;
begin
  Assert((FBlockCount = 0) and (FBlockChangeNumber = 0));

  if FRedoList.Count > 0 then
  begin
    Item := FRedoList.Peek;
    OldModified := GetModified;
    OldChangeNumber := Item.ChangeNumber;

    repeat
      Item := FRedoList.Extract;
      LastItemHasGroupBreak := Item.GroupBreak;
      LastItem := Item;
      FInsideUndoRedo := True;
      try
        Item.Redo(Editor as TCustomSynEdit);
      finally
        FInsideUndoRedo := False;
      end;
      // Move it to the UndoList
      FUndoList.Push(Item);

      if FRedoList.Count = 0 then
        Break
      else
        Item := FRedoList.Peek;

      if Item.ChangeNumber = OldChangeNumber then
        FKeepGoing := True
      else
        FKeepGoing :=
          FGroupUndo and
          { Last Item had a group break - Stop redoing }
          not LastItemHasGroupBreak and
          { Group together same undo actions }
          (LastItem is TSynLinePutUndoItem) and
          (Item is TSynLinePutUndoItem) and
          TSynLinePutUndoItem(LastItem).GroupWith(TSynLinePutUndoItem(Item));
    until not(FKeepGoing);

    if not (Item is TSynCaretAndSelectionUndoItem) then
    begin
      (Editor as TCustomSynEdit).Selections.Clear;
      (Editor as TCustomSynEdit).CaretXY := Item.FCaret;  // removes selection
    end;
    if (OldModified xor GetModified) and Assigned(FOnModifiedChanged) then
      FOnModifiedChanged(Self);
  end;
end;

procedure TSynEditUndo.SetCommandProcessed(const Command: TSynEditorCommand);
begin
  FCommandProcessed := Command;
end;

procedure TSynEditUndo.SetGroupUndo(const Value: Boolean);
begin
  FGroupUndo := Value;
end;

procedure TSynEditUndo.SetMaxUndoActions(const Value: Integer);
begin
  if Value <> FMaxUndoActions then
  begin
    FMaxUndoActions := Value;
    FUndoList.EnsureMaxEntries;
    FRedoList.EnsureMaxEntries;
  end;
end;

procedure TSynEditUndo.SetModified(const Value: Boolean);
begin
  if not Value then
  begin
    if FUndoList.Count = 0 then
      FInitialChangeNumber := 0
    else
      FInitialChangeNumber := FUndoList.Peek.ChangeNumber;
  end
  else if FUndoList.Count = 0 then
  begin
    if FInitialChangeNumber = 0 then
      FInitialChangeNumber := -1;
  end
  else if FUndoList.Peek.ChangeNumber = FInitialChangeNumber then
    FInitialChangeNumber := -1
end;

procedure TSynEditUndo.SetOnModifiedChanged(const Value: TNotifyEvent);
begin
  FOnModifiedChanged := Value;
end;

procedure TSynEditUndo.Undo(Editor: TControl);
var
  Item, LastItem: TSynUndoItem;
  OldChangeNumber: Integer;
  OldModified: Boolean;
  FKeepGoing: Boolean;
begin
  Assert((FBlockCount = 0) and (FBlockChangeNumber = 0));

  if FUndoList.Count > 0 then
  begin
    Item := FUndoList.Peek;
    OldModified := GetModified;
    OldChangeNumber := Item.ChangeNumber;

    repeat
      Item := FUndoList.Extract;
      LastItem := Item;
      FInsideUndoRedo := True;
      try
        Item.Undo(Editor as TCustomSynEdit);
      finally
        FInsideUndoRedo := False;
      end;
      // Move it to the RedoList
      FRedoList.Push(Item);

      if FUndoList.Count = 0 then
        Break
      else
        Item := FUndoList.Peek;

      if Item.ChangeNumber = OldChangeNumber then
        FKeepGoing := True
      else
        FKeepGoing :=
          FGroupUndo and
          { Last Item had a group break - Stop redoing }
          not Item.GroupBreak and
          { Group together same undo actions }
          (LastItem is TSynLinePutUndoItem) and
          (Item is TSynLinePutUndoItem) and
          TSynLinePutUndoItem(Item).GroupWith(TSynLinePutUndoItem(LastItem));
    until not(FKeepGoing);

    if not (LastItem is TSynCaretAndSelectionUndoItem) then
    begin
      (Editor as TCustomSynEdit).Selections.Clear;
      (Editor as TCustomSynEdit).SetCaretAndSelection(LastItem.FCaret, LastItem.FCaret,
        LastItem.FCaret);
    end;
    if (OldModified xor GetModified) and Assigned(FOnModifiedChanged) then
      FOnModifiedChanged(Self);
  end;
end;

procedure TSynEditUndo.Unlock;
begin
  if FLockCount > 0 then
    Dec(FLockCount);
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

procedure TSynCaretAndSelectionUndoItem.Redo(Editor: TCustomSynEdit);
begin
  // Same as Undo
  Undo(Editor);
end;

procedure TSynCaretAndSelectionUndoItem.Undo(Editor: TCustomSynEdit);
begin
  if Length(SelStorage.Selections) > 0 then
    Editor.Selections.Restore(SelStorage)
  else
  begin
    Editor.Selections.Clear;
    Editor.SetCaretAndSelection(FCaret, FBlockBegin, FBlockEnd);
  end;
end;

{ TSynLinesDeletedUndoItem }

constructor TSynLinesDeletedUndoItem.Create(Editor: TCustomSynEdit; Index:
    Integer; DeletedLines: TArray<string>; DeletedChangeFlags:
    TArray<TSynLineChangeFlags>);
begin
  inherited Create;
  FIndex := Index;
  FLines := DeletedLines;
  FChangeFlags := DeletedChangeFlags;
end;

procedure TSynLinesDeletedUndoItem.Redo(Editor: TCustomSynEdit);
var
  I: Integer;
begin
  // Save change flags
  SetLength(FChangeFlags, Length(FLines));
  for I := 0 to Length(FLines) - 1 do
    FChangeFlags[I] := TSynEditStringList(Editor.Lines).ChangeFlags[FIndex + I];

  TSynEditStringList(Editor.Lines).DeleteLines(FIndex, Length(FLines));
  FCaret := BufferCoord(1, FIndex + 1);
end;

procedure TSynLinesDeletedUndoItem.Undo(Editor: TCustomSynEdit);
var
  I: Integer;
begin
  TSynEditStringList(Editor.Lines).InsertStrings(FIndex, FLines);

  // Restore change flags
  for I := 0 to Length(FLines) - 1 do
    TSynEditStringList(Editor.Lines).ChangeFlags[FIndex + I] := FChangeFlags[I];

  FCaret := BufferCoord(1,
    Min(Editor.Lines.Count, FIndex + Length(FLines) + 1));
end;

{ TSynLinesInsertedUndoItem }

constructor TSynLinesInsertedUndoItem.Create(Editor: TCustomSynEdit; Index,
  Count: Integer);
var
  I: Integer;
begin
  inherited Create;
  FIndex := Index;
  SetLength(FLines, Count);
  for I := 0 to Count - 1 do
  begin
    FLines[I] := Editor.Lines[Index + I];
    // Mark the lines modified
    TSynEditStringList(Editor.Lines).ChangeFlags[Index + I] := [sfModified];
  end;
end;

procedure TSynLinesInsertedUndoItem.Redo(Editor: TCustomSynEdit);
var
  I: Integer;
begin
  TSynEditStringList(Editor.Lines).InsertStrings(FIndex, FLines);

  // Restore change flags
  for I := 0 to Length(FLines) - 1 do
    TSynEditStringList(Editor.Lines).ChangeFlags[FIndex + I] := FChangeFlags[I];

  FCaret := BufferCoord(1,
    Min(Editor.Lines.Count, FIndex + Length(FLines) + 1));
end;

procedure TSynLinesInsertedUndoItem.Undo(Editor: TCustomSynEdit);
var
  I: Integer;
begin
  // Save change flags
  SetLength(FChangeFlags, Length(FLines));
  for I := 0 to Length(FLines) - 1 do
    FChangeFlags[I] := TSynEditStringList(Editor.Lines).ChangeFlags[FIndex + I];

  TSynEditStringList(Editor.Lines).DeleteLines(FIndex, Length(FLines));
  FCaret := BufferCoord(1, FIndex + 1);
end;

{ TSynLinePutUndoItem }

function TSynLinePutUndoItem.GroupWith(Item: TSynLinePutUndoItem): Boolean;
begin
  if (FNewValue.Length = Item.FNewValue.Length) and
     (FOldValue.Length = Item.FOldValue.Length) and
     (FOldValue.Length <= 1) and (FNewValue.Length <= 1) and
     (Abs(FStartPos - Item.FStartPos) <= 1)
  then
    Result := True
  else
    Result := False;
end;

constructor TSynLinePutUndoItem.Create(Editor: TCustomSynEdit; Index: Integer;
  OldLine: string; Command: TSynEditorCommand);
var
  Len1, Len2: Integer;
  Line: string;
begin
  FCommandProcessed := Command;

  FIndex := Index;
  Line := Editor.Lines[Index];

  LineDiff(Line, OldLine, FStartPos, Len1, Len2);

  FOldValue := Copy(OldLine, FStartPos, Len1);
  FNewValue := Copy(Line, FStartPos, Len2);

  FChangeFlags := TSynEditStringList(Editor.Lines).ChangeFlags[Index] -
    [sfSaved];
  TSynEditStringList(Editor.Lines).ChangeFlags[Index] :=
    TSynEditStringList(Editor.Lines).ChangeFlags[Index] +
      [sfModified] - [sfAsSaved];
end;

procedure TSynLinePutUndoItem.Redo(Editor: TCustomSynEdit);
var
  Line: string;
  Char: Integer;
  TempCF: TSynLineChangeFlags;
begin
  Line := Editor.Lines[FIndex];
  // Delete New
  Delete(Line, FStartPos, FOldValue.Length);
  Insert(FNewValue, Line, FStartPos);
  Editor.Lines[FIndex] := Line;
  // Swap change flags
  TempCF := FChangeFlags;
  FChangeFlags := TSynEditStringList(Editor.Lines).ChangeFlags[FIndex] -
    [sfSaved];
  TSynEditStringList(Editor.Lines).ChangeFlags[FIndex] :=
    TSynEditStringList(Editor.Lines).ChangeFlags[FIndex]
    - [sfModified, sfAsSaved] + TempCF;
  // Guess Caret position
  case FCommandProcessed of
    ecChar:
      if (FOldValue.Length = 1) and (FNewValue.Length = 1) then
        Char := FStartPos  // Typing in Insert Mode
      else
        Char := FStartPos + FNewValue.Length;
    ecDeleteChar,
    ecDeleteWord,
    ecDeleteEOL:  Char := FStartPos;
  else
    Char := FStartPos + FNewValue.Length;
  end;
  FCaret := BufferCoord(Char, FIndex + 1);
end;

procedure TSynLinePutUndoItem.Undo(Editor: TCustomSynEdit);
var
  Line: string;
  Char: Integer;
  TempCF: TSynLineChangeFlags;
begin
  Line := Editor.Lines[FIndex];
  // Delete New
  Delete(Line, FStartPos, FNewValue.Length);
  Insert(FOldValue, Line, FStartPos);
  Editor.Lines[FIndex] := Line;
  // Swap change flags
  TempCF := FChangeFlags;
  FChangeFlags := TSynEditStringList(Editor.Lines).ChangeFlags[FIndex] -
    [sfSaved];
  TSynEditStringList(Editor.Lines).ChangeFlags[FIndex] :=
    TSynEditStringList(Editor.Lines).ChangeFlags[FIndex]
    - [sfModified, sfAsSaved] + TempCF;
  // Guess Caret position
  case FCommandProcessed of
    ecChar:
      if (FOldValue.Length = 1) and (FNewValue.Length = 1) then
        Char := FStartPos   // Typing in Overwrite Mode
      else
        Char := FStartPos + FOldValue.Length;
    ecDeleteChar,
    ecDeleteWord,
    ecDeleteEOL:  Char := FStartPos;
  else
    Char := FStartPos + FOldValue.Length;
  end;
  FCaret := BufferCoord(Char, FIndex + 1);
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
    Item := TSynLinePutUndoItem.Create(Editor, aIndex, OldLine,
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

  // Save deleted lines and change flags
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
    Item := TSynLinesDeletedUndoItem.Create(Editor, FirstLine,
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

  // Consider a file with one empty line as empty
  // Otherwise when you type in a new file and undo it, CanUndo will still
  // return True because the initial insertion will be on the Undo list
  if (FSynEditUndo.FUndoList.Count = 0) and
    (Editor.Lines.Count = 1)  and (Editor.Lines[0] = '')
  then
    Exit;

  if Count > 0 then
  begin
    Item := TSynLinesInsertedUndoItem.Create(Editor, FirstLine, Count);
    FSynEditUndo.AddUndoItem(Item);
  end;
end;

end.
