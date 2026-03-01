{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Shared undo/redo base classes.
Platform-specific subclasses in Vcl.SynEditUndo.pas and FMX.SynEditUndo.pas
provide editor-type-aware overrides.

The "Shared" suffix avoids namespace shadowing with Vcl.SynEditUndo.
-------------------------------------------------------------------------------}

unit SynEditUndoShared;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditMiscProcs,
  SynEditTextBuffer;

type
  TSynEditUndoBase = class;

  TSynUndoItem = class abstract(TObject)
    ChangeNumber: Integer;
    FCaret: TBufferCoord;
    GroupBreak: Boolean;
  public
    procedure Undo(Editor: TObject); virtual; abstract;
    procedure Redo(Editor: TObject); virtual; abstract;
  end;

  { Line-change undo items operate on TStrings (TSynEditStringList).
    The Lines reference is captured at construction and reused in Undo/Redo. }

  TSynLinePutUndoItem = class(TSynUndoItem)
  private
    FLines: TStrings;
    FIndex: Integer;
    FStartPos: Integer;
    FOldValue: string;
    FNewValue: string;
    FChangeFlags: TSynLineChangeFlags;
    FCommandProcessed: TSynEditorCommand;
  public
    function GroupWith(Item: TSynLinePutUndoItem): Boolean;
    procedure Undo(Editor: TObject); override;
    procedure Redo(Editor: TObject); override;
    constructor Create(Lines: TStrings; Index: Integer;
      const OldLine: string; Command: TSynEditorCommand);
  end;

  TSynLinesInsertedUndoItem = class(TSynUndoItem)
  private
    FLines: TStrings;
    FIndex: Integer;
    FSavedLines: TArray<string>;
    FChangeFlags: TArray<TSynLineChangeFlags>;
  public
    procedure Undo(Editor: TObject); override;
    procedure Redo(Editor: TObject); override;
    constructor Create(Lines: TStrings; Index, Count: Integer);
  end;

  TSynLinesDeletedUndoItem = class(TSynUndoItem)
  private
    FLines: TStrings;
    FIndex: Integer;
    FSavedLines: TArray<string>;
    FChangeFlags: TArray<TSynLineChangeFlags>;
  public
    procedure Undo(Editor: TObject); override;
    procedure Redo(Editor: TObject); override;
    constructor Create(Lines: TStrings; Index: Integer;
      const DeletedLines: TArray<string>;
      const DeletedChangeFlags: TArray<TSynLineChangeFlags>);
  end;

  TSynEditUndoList = class(TObjectStack<TSynUndoItem>)
  protected
    FOwner: TSynEditUndoBase;
    FFullUndoImposible: Boolean;
    procedure EnsureMaxEntries;
  public
    constructor Create(AOwner: TSynEditUndoBase);
    procedure Push(const Value: TSynUndoItem);
  end;

  TSynEditUndoBase = class(TInterfacedObject, ISynEditUndo)
  protected
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
    // Saved lines for LinesBeforeDeleted/LinesDeleted pair
    FDeletedLines: TArray<string>;
    FDeletedChangeFlags: TArray<TSynLineChangeFlags>;
    // === Abstract methods — subclass hooks ===
    function CreateCaretUndoItem(Editor: TObject): TSynUndoItem; virtual; abstract;
    procedure RestoreCaretAndSelection(Editor: TObject;
      Item: TSynUndoItem); virtual; abstract;
  private
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
    procedure Lock;
    procedure Unlock;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    function IsLocked: Boolean;
    function NextChangeNumber: Integer;
    procedure AddGroupBreak;
    procedure AddUndoItem(Item: TSynUndoItem);
    procedure BeginBlock(Editor: TObject);
    procedure EndBlock(Editor: TObject);
    procedure Undo(Editor: TObject);
    procedure Redo(Editor: TObject);
    procedure BufferSaved(Lines: TStrings);
    procedure ClearTrackChanges(Lines: TStrings);
  end;

implementation

{$REGION 'TSynEditUndoList'}

constructor TSynEditUndoList.Create(AOwner: TSynEditUndoBase);
begin
  inherited Create(True);
  FOwner := AOwner;
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
    Clear;
    for I := Length(ItemArray) - KeepCount to Length(ItemArray) - 1 do
      Push(ItemArray[I]);
  end;
end;

procedure TSynEditUndoList.Push(const Value: TSynUndoItem);
begin
  inherited Push(Value);
  EnsureMaxEntries;
end;

{$ENDREGION}

{$REGION 'TSynEditUndoBase'}

constructor TSynEditUndoBase.Create;
begin
  inherited Create;
  FGroupUndo := True;
  FMaxUndoActions := 0;
  FNextChangeNumber := 1;
  FUndoList := TSynEditUndoList.Create(Self);
  FRedoList := TSynEditUndoList.Create(Self);
end;

destructor TSynEditUndoBase.Destroy;
begin
  FUndoList.Free;
  FRedoList.Free;
  inherited;
end;

procedure TSynEditUndoBase.AddUndoItem(Item: TSynUndoItem);
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
  if (FBlockCount = 0) and (OldModified xor GetModified) and
    Assigned(FOnModifiedChanged)
  then
    FOnModifiedChanged(Self);
end;

procedure TSynEditUndoBase.AddGroupBreak;
begin
  if (FUndoList.Count > 0) and (FBlockCount = 0) then
    FUndoList.Peek.GroupBreak := True;
end;

procedure TSynEditUndoBase.BeginBlock(Editor: TObject);
begin
  if IsLocked then Exit;
  Inc(FBlockCount);
  if FBlockCount = 1 then
  begin
    FBlockStartModified := GetModified;
    FBlockChangeNumber := NextChangeNumber;
    FBlockSelRestoreItem := CreateCaretUndoItem(Editor);
    FBlockSelRestoreItem.ChangeNumber := FBlockChangeNumber;
    FUndoList.Push(FBlockSelRestoreItem);
  end;
end;

procedure TSynEditUndoBase.EndBlock(Editor: TObject);
var
  Item: TSynUndoItem;
begin
  if IsLocked then Exit;
  Assert(FBlockCount > 0);
  if FBlockCount > 0 then
  begin
    Dec(FBlockCount);
    if FBlockCount = 0 then
    begin
      if (FUndoList.Count > 0) and (FUndoList.Peek = FBlockSelRestoreItem) then
        FUndoList.Pop
      else
      begin
        Item := CreateCaretUndoItem(Editor);
        Item.ChangeNumber := FBlockChangeNumber;
        FUndoList.Push(Item);
      end;
      FBlockChangeNumber := 0;
      AddGroupBreak;
      if (FBlockStartModified xor GetModified) and Assigned(FOnModifiedChanged) then
        FOnModifiedChanged(Self);
    end;
  end;
end;

procedure TSynEditUndoBase.Clear;
begin
  FUndoList.Clear;
  FRedoList.Clear;
end;

function TSynEditUndoBase.GetCanUndo: Boolean;
begin
  Result := FUndoList.Count > 0;
end;

function TSynEditUndoBase.GetCanRedo: Boolean;
begin
  Result := FRedoList.Count > 0;
end;

function TSynEditUndoBase.GetFullUndoImposible: Boolean;
begin
  Result := FUndoList.FFullUndoImposible;
end;

function TSynEditUndoBase.GetInsideUndoRedo: Boolean;
begin
  Result := FInsideUndoRedo;
end;

function TSynEditUndoBase.GetMaxUndoActions: Integer;
begin
  Result := FMaxUndoActions;
end;

function TSynEditUndoBase.GetModified: Boolean;
begin
  if FUndoList.Count = 0 then
    Result := FInitialChangeNumber <> 0
  else
    Result := FUndoList.Peek.ChangeNumber <> FInitialChangeNumber;
end;

function TSynEditUndoBase.GetOnModifiedChanged: TNotifyEvent;
begin
  Result := FOnModifiedChanged;
end;

function TSynEditUndoBase.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TSynEditUndoBase.Lock;
begin
  Inc(FLockCount);
end;

function TSynEditUndoBase.NextChangeNumber: Integer;
begin
  Result := FNextChangeNumber;
  Inc(FNextChangeNumber);
end;

procedure TSynEditUndoBase.SetCommandProcessed(const Command: TSynEditorCommand);
begin
  FCommandProcessed := Command;
end;

procedure TSynEditUndoBase.SetGroupUndo(const Value: Boolean);
begin
  FGroupUndo := Value;
end;

procedure TSynEditUndoBase.SetMaxUndoActions(const Value: Integer);
begin
  if Value <> FMaxUndoActions then
  begin
    FMaxUndoActions := Value;
    FUndoList.EnsureMaxEntries;
    FRedoList.EnsureMaxEntries;
  end;
end;

procedure TSynEditUndoBase.SetModified(const Value: Boolean);
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
    FInitialChangeNumber := -1;
end;

procedure TSynEditUndoBase.SetOnModifiedChanged(const Value: TNotifyEvent);
begin
  FOnModifiedChanged := Value;
end;

procedure TSynEditUndoBase.Undo(Editor: TObject);
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
        Item.Undo(Editor);
      finally
        FInsideUndoRedo := False;
      end;
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
          not Item.GroupBreak and
          (LastItem is TSynLinePutUndoItem) and
          (Item is TSynLinePutUndoItem) and
          TSynLinePutUndoItem(Item).GroupWith(TSynLinePutUndoItem(LastItem));
    until not FKeepGoing;

    RestoreCaretAndSelection(Editor, LastItem);

    if (OldModified xor GetModified) and Assigned(FOnModifiedChanged) then
      FOnModifiedChanged(Self);
  end;
end;

procedure TSynEditUndoBase.Redo(Editor: TObject);
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
        Item.Redo(Editor);
      finally
        FInsideUndoRedo := False;
      end;
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
          not LastItemHasGroupBreak and
          (LastItem is TSynLinePutUndoItem) and
          (Item is TSynLinePutUndoItem) and
          TSynLinePutUndoItem(LastItem).GroupWith(TSynLinePutUndoItem(Item));
    until not FKeepGoing;

    RestoreCaretAndSelection(Editor, LastItem);

    if (OldModified xor GetModified) and Assigned(FOnModifiedChanged) then
      FOnModifiedChanged(Self);
  end;
end;

procedure TSynEditUndoBase.Unlock;
begin
  if FLockCount > 0 then
    Dec(FLockCount);
end;

procedure TSynEditUndoBase.BufferSaved(Lines: TStrings);
var
  SynLines: TSynEditStringList;
  Index: Integer;
  Flags: TSynLineChangeFlags;
  Item: TSynUndoItem;

  procedure PutItemSaved(Item: TSynLinePutUndoItem);
  begin
    if Item.FChangeFlags = [sfAsSaved] then
      Item.FChangeFlags := [sfModified];
  end;

  procedure InsertedItemSaved(Item: TSynLinesInsertedUndoItem);
  var I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;

  procedure DeletedItemSaved(Item: TSynLinesDeletedUndoItem);
  var I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;

begin
  SynLines := Lines as TSynEditStringList;
  for Index := 0 to SynLines.Count - 1 do
  begin
    Flags := SynLines.ChangeFlags[Index];
    if Flags = [sfSaved] then
      SynLines.ChangeFlags[Index] := []
    else if sfModified in Flags then
      SynLines.ChangeFlags[Index] := Flags - [sfModified] + [sfSaved, sfAsSaved];
  end;
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

procedure TSynEditUndoBase.ClearTrackChanges(Lines: TStrings);
var
  SynLines: TSynEditStringList;
  Index: Integer;
  Item: TSynUndoItem;

  procedure InsertedItemClear(Item: TSynLinesInsertedUndoItem);
  var I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;

  procedure DeletedItemClear(Item: TSynLinesDeletedUndoItem);
  var I: Integer;
  begin
    for I := 0 to Length(Item.FChangeFlags) - 1 do
      Item.FChangeFlags[I] := [sfModified];
  end;

begin
  SynLines := Lines as TSynEditStringList;
  for Index := 0 to SynLines.Count - 1 do
    SynLines.ChangeFlags[Index] := [];
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

{$ENDREGION}

{$REGION 'TSynLinePutUndoItem'}

constructor TSynLinePutUndoItem.Create(Lines: TStrings; Index: Integer;
  const OldLine: string; Command: TSynEditorCommand);
var
  Len1, Len2: Integer;
  Line: string;
begin
  FLines := Lines;
  FCommandProcessed := Command;
  FIndex := Index;
  Line := Lines[Index];

  LineDiff(Line, OldLine, FStartPos, Len1, Len2);

  FOldValue := Copy(OldLine, FStartPos, Len1);
  FNewValue := Copy(Line, FStartPos, Len2);

  FChangeFlags := TSynEditStringList(Lines).ChangeFlags[Index] -
    [sfSaved];
  TSynEditStringList(Lines).ChangeFlags[Index] :=
    TSynEditStringList(Lines).ChangeFlags[Index] +
      [sfModified] - [sfAsSaved];
end;

function TSynLinePutUndoItem.GroupWith(Item: TSynLinePutUndoItem): Boolean;
begin
  Result := (FNewValue.Length = Item.FNewValue.Length) and
    (FOldValue.Length = Item.FOldValue.Length) and
    (FOldValue.Length <= 1) and (FNewValue.Length <= 1) and
    (Abs(FStartPos - Item.FStartPos) <= 1);
end;

procedure TSynLinePutUndoItem.Undo(Editor: TObject);
var
  Line: string;
  Char: Integer;
  TempCF: TSynLineChangeFlags;
begin
  Line := FLines[FIndex];
  Delete(Line, FStartPos, FNewValue.Length);
  Insert(FOldValue, Line, FStartPos);
  FLines[FIndex] := Line;
  TempCF := FChangeFlags;
  FChangeFlags := TSynEditStringList(FLines).ChangeFlags[FIndex] -
    [sfSaved];
  TSynEditStringList(FLines).ChangeFlags[FIndex] :=
    TSynEditStringList(FLines).ChangeFlags[FIndex]
    - [sfModified, sfAsSaved] + TempCF;
  case FCommandProcessed of
    ecChar:
      if (FOldValue.Length = 1) and (FNewValue.Length = 1) then
        Char := FStartPos
      else
        Char := FStartPos + FOldValue.Length;
    ecDeleteChar, ecDeleteWord, ecDeleteEOL:
      Char := FStartPos;
  else
    Char := FStartPos + FOldValue.Length;
  end;
  FCaret := BufferCoord(Char, FIndex + 1);
end;

procedure TSynLinePutUndoItem.Redo(Editor: TObject);
var
  Line: string;
  Char: Integer;
  TempCF: TSynLineChangeFlags;
begin
  Line := FLines[FIndex];
  Delete(Line, FStartPos, FOldValue.Length);
  Insert(FNewValue, Line, FStartPos);
  FLines[FIndex] := Line;
  TempCF := FChangeFlags;
  FChangeFlags := TSynEditStringList(FLines).ChangeFlags[FIndex] -
    [sfSaved];
  TSynEditStringList(FLines).ChangeFlags[FIndex] :=
    TSynEditStringList(FLines).ChangeFlags[FIndex]
    - [sfModified, sfAsSaved] + TempCF;
  case FCommandProcessed of
    ecChar:
      if (FOldValue.Length = 1) and (FNewValue.Length = 1) then
        Char := FStartPos
      else
        Char := FStartPos + FNewValue.Length;
    ecDeleteChar, ecDeleteWord, ecDeleteEOL:
      Char := FStartPos;
  else
    Char := FStartPos + FNewValue.Length;
  end;
  FCaret := BufferCoord(Char, FIndex + 1);
end;

{$ENDREGION}

{$REGION 'TSynLinesInsertedUndoItem'}

constructor TSynLinesInsertedUndoItem.Create(Lines: TStrings;
  Index, Count: Integer);
var
  I: Integer;
begin
  inherited Create;
  FLines := Lines;
  FIndex := Index;
  SetLength(FSavedLines, Count);
  for I := 0 to Count - 1 do
  begin
    FSavedLines[I] := Lines[Index + I];
    TSynEditStringList(Lines).ChangeFlags[Index + I] := [sfModified];
  end;
end;

procedure TSynLinesInsertedUndoItem.Undo(Editor: TObject);
var
  I: Integer;
begin
  SetLength(FChangeFlags, Length(FSavedLines));
  for I := 0 to Length(FSavedLines) - 1 do
    FChangeFlags[I] := TSynEditStringList(FLines).ChangeFlags[FIndex + I];
  TSynEditStringList(FLines).DeleteLines(FIndex, Length(FSavedLines));
  FCaret := BufferCoord(1, FIndex + 1);
end;

procedure TSynLinesInsertedUndoItem.Redo(Editor: TObject);
var
  I: Integer;
begin
  TSynEditStringList(FLines).InsertStrings(FIndex, FSavedLines);
  for I := 0 to Length(FSavedLines) - 1 do
    TSynEditStringList(FLines).ChangeFlags[FIndex + I] := FChangeFlags[I];
  FCaret := BufferCoord(1,
    Min(FLines.Count, FIndex + Length(FSavedLines) + 1));
end;

{$ENDREGION}

{$REGION 'TSynLinesDeletedUndoItem'}

constructor TSynLinesDeletedUndoItem.Create(Lines: TStrings;
  Index: Integer; const DeletedLines: TArray<string>;
  const DeletedChangeFlags: TArray<TSynLineChangeFlags>);
begin
  inherited Create;
  FLines := Lines;
  FIndex := Index;
  FSavedLines := DeletedLines;
  FChangeFlags := DeletedChangeFlags;
end;

procedure TSynLinesDeletedUndoItem.Undo(Editor: TObject);
var
  I: Integer;
begin
  TSynEditStringList(FLines).InsertStrings(FIndex, FSavedLines);
  for I := 0 to Length(FSavedLines) - 1 do
    TSynEditStringList(FLines).ChangeFlags[FIndex + I] := FChangeFlags[I];
  FCaret := BufferCoord(1,
    Min(FLines.Count, FIndex + Length(FSavedLines) + 1));
end;

procedure TSynLinesDeletedUndoItem.Redo(Editor: TObject);
var
  I: Integer;
begin
  SetLength(FChangeFlags, Length(FSavedLines));
  for I := 0 to Length(FSavedLines) - 1 do
    FChangeFlags[I] := TSynEditStringList(FLines).ChangeFlags[FIndex + I];
  TSynEditStringList(FLines).DeleteLines(FIndex, Length(FSavedLines));
  FCaret := BufferCoord(1, FIndex + 1);
end;

{$ENDREGION}

end.
