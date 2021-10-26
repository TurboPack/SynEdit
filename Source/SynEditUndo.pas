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
  SynEditTypes;

type
  TSynUndoRedoItem = procedure(Item: TSynEditUndoItem) of object;

{ Factory Method}

function CreateSynEditUndo(UndoMethod, RedoMethod: TSynUndoRedoItem):
  ISynEditUndo;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections;

type
  TSynEditUndo = class;

  TSynEditUndoList = class(TObjectStack<TSynEditUndoItem>)
  protected
    FOwner: TSynEditUndo;
    FBlockChangeNumber: Integer;
    FNextChangeNumber: Integer;
    FFullUndoImposible: Boolean;
    procedure EnsureMaxEntries;
    function NextChangeNumber: Integer;
  public
    constructor Create(Owner: TSynEditUndo);
    procedure Push(const Value: TSynEditUndoItem);
    procedure AddChange(AReason: TSynChangeReason;
      const AStart, AEnd: TBufferCoord; const ChangeText: string;
      SelMode: TSynSelectionMode; IsGroupBreak: Boolean);
    property BlockChangeNumber: Integer read FBlockChangeNumber
      write FBlockChangeNumber;
  end;

  TSynEditUndo = class(TInterfacedObject, ISynEditUndo)
  private
    FGroupUndo: Boolean;
    FBlockCount: Integer;
    FLockCount: Integer;
    FInitialChangeNumber: Integer;
    FMaxUndoActions: Integer;
    FBlockStartModified: Boolean;
    FUndoList: TSynEditUndoList;
    FRedoList: TSynEditUndoList;
    FOnModifiedChanged: TNotifyEvent;
    FInsideRedo: Boolean;
    FUndoItem: TSynUndoRedoItem;
    FRedoItem: TSynUndoRedoItem;
    function GetModified: Boolean;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetFullUndoImposible: Boolean;
    function GetOnModifiedChanged: TNotifyEvent;
    procedure SetModified(const Value: Boolean);
    procedure SetMaxUndoActions(const Value: Integer);
    procedure SetOnModifiedChanged(const Value: TNotifyEvent);
    procedure SetGroupUndo(const Value: Boolean);
    function GetMaxUndoActions: Integer;
    procedure BeginBlock;
    procedure EndBlock;
    procedure Lock;
    procedure Unlock;
    procedure Clear;
    procedure AddUndoChange(AReason: TSynChangeReason;
      const AStart, AEnd: TBufferCoord; const ChangeText: string;
      SelMode: TSynSelectionMode; IsGroupBreak: Boolean = False);
    procedure AddRedoChange(AReason: TSynChangeReason;
      const AStart, AEnd: TBufferCoord; const ChangeText: string;
      SelMode: TSynSelectionMode; IsGroupBreak: Boolean);
    procedure AddGroupBreak;
    procedure Undo;
    procedure Redo;
  public
    constructor Create(UndoMethod, RedoMethod: TSynUndoRedoItem);
    destructor Destroy; override;
  end;

{ TSynEditUndoList }

constructor TSynEditUndoList.Create(Owner: TSynEditUndo);
begin
  inherited Create(True);
  FNextChangeNumber := 1;
  FOwner := Owner;
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; const AStart,
    AEnd: TBufferCoord; const ChangeText: string; SelMode: TSynSelectionMode;
    IsGroupBreak: Boolean);
var
  NewItem: TSynEditUndoItem;
begin
  if FOwner.FLockCount = 0 then
  begin
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do
      begin
        ChangeReason := AReason;
        ChangeSelMode := SelMode;
        ChangeStartPos := AStart;
        ChangeEndPos := AEnd;
        ChangeStr := ChangeText;
        GroupBreak := IsGroupBreak;
        if FBlockChangeNumber <> 0 then
          ChangeNumber := FBlockChangeNumber
        else
          ChangeNumber := NextChangeNumber;
      end;
      Push(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  KeepCount: Integer;
  ItemArray: TArray<TSynEditUndoItem>;
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

function TSynEditUndoList.NextChangeNumber: Integer;
begin
  Result := FNextChangeNumber;
  Inc(FNextChangeNumber);
end;

procedure TSynEditUndoList.Push(const Value: TSynEditUndoItem);
begin
  inherited Push(Value);
  EnsureMaxEntries;
end;

{ TSynEditUndo }

procedure TSynEditUndo.AddRedoChange(AReason: TSynChangeReason; const AStart,
  AEnd: TBufferCoord; const ChangeText: string; SelMode: TSynSelectionMode;
  IsGroupBreak: Boolean);
begin
  FRedoList.AddChange(AReason, AStart, AEnd, ChangeText, SelMode, IsGroupBreak);
end;

procedure TSynEditUndo.AddUndoChange(AReason: TSynChangeReason; const AStart,
    AEnd: TBufferCoord; const ChangeText: string; SelMode: TSynSelectionMode;
    IsGroupBreak: Boolean);
var
  OldModified: Boolean;
begin
  OldModified := GetModified;
  FUndoList.AddChange(AReason, AStart, AEnd, ChangeText, SelMode, IsGroupBreak);
  if not FInsideRedo then
    FRedoList.Clear;
  // Do not sent unnecessary notifications
  if not FInsideRedo and (FBlockCount = 0) and (OldModified xor GetModified) and
    Assigned(FOnModifiedChanged)
  then
    FOnModifiedChanged(Self);
end;

procedure TSynEditUndo.AddGroupBreak;
begin
  if (FUndoList.Count > 0) and (FBlockCount = 0) then
    FUndoList.Peek.GroupBreak := True;
end;

procedure TSynEditUndo.BeginBlock;
begin
  if FBlockCount = 0 then
  begin
    FBlockStartModified := GetModified;
    FUndoList.FBlockChangeNumber := FUndoList.NextChangeNumber;
  end;
  Inc(FBlockCount);
end;

procedure TSynEditUndo.Clear;
begin
  FUndoList.Clear;
  FRedoList.Clear;
end;

constructor TSynEditUndo.Create(UndoMethod, RedoMethod: TSynUndoRedoItem);
begin
  inherited Create;
  FGroupUndo := True;
  FUndoItem := UndoMethod;
  FRedoItem := RedoMethod;
  FMaxUndoActions := 0;
  FUndoList := TSynEditUndoList.Create(Self);
  FRedoList := TSynEditUndoList.Create(Self);
end;

destructor TSynEditUndo.Destroy;
begin
  FUndoList.Free;
  FRedoList.Free;
  inherited;
end;

procedure TSynEditUndo.EndBlock;
begin
  if FBlockCount > 0 then
  begin
    Dec(FBlockCount);
    if FBlockCount = 0 then
    begin
      FUndoList.FBlockChangeNumber := 0;
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

function TSynEditUndo.GetCanRedo: Boolean;
begin
  Result := FRedoList.Count > 0;
end;

procedure TSynEditUndo.Lock;
begin
  Inc(FLockCount);
end;

procedure TSynEditUndo.Redo;
var
  Item: TSynEditUndoItem;
  OldChangeNumber: Integer;
  OldModified: Boolean;
  FLastChange: TSynChangeReason;
  FKeepGoing: Boolean;
  LastItemHasGroupBreak: Boolean;
begin
  Assert((FBlockCount = 0) and (FRedoList.FBlockChangeNumber = 0) and
   (FRedoList.FBlockChangeNumber = 0));

  if FRedoList.Count > 0 then
  begin
    Item := FRedoList.Peek;
    OldModified := GetModified;
    OldChangeNumber := Item.ChangeNumber;
    FUndoList.BlockChangeNumber := FUndoList.NextChangeNumber;
    try
      repeat
        Item := FRedoList.Extract;
        LastItemHasGroupBreak := Item.GroupBreak;
        FLastChange := Item.ChangeReason;
        FInsideRedo := True;
        try
        FRedoItem(Item);
        finally
          Item.Free;
          FInsideRedo := False;
        end;

        if FRedoList.Count = 0 then
          Break
        else
          Item := FRedoList.Peek;

        if Item.ChangeNumber = OldChangeNumber then
          FKeepGoing := True
        else
          FKeepGoing :=
            { Group together same undo actions }
            (FGroupUndo and (FLastChange = Item.ChangeReason) and
            { Last Item had a group break - Stop redoing }
            not LastItemHasGroupBreak and
            { crUn/Indent act as a group break }
            not(FLastChange in [crIndent, crUnindent]));
      until not(FKeepGoing);
    finally
      if (OldModified xor GetModified) and Assigned(FOnModifiedChanged) then
        FOnModifiedChanged(Self);
      FUndoList.BlockChangeNumber := 0;
    end;
  end;
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

procedure TSynEditUndo.Undo;
var
  Item: TSynEditUndoItem;
  OldChangeNumber: Integer;
  OldModified: Boolean;
  FLastChange: TSynChangeReason;
  FKeepGoing: Boolean;
begin
  Assert((FBlockCount = 0) and (FRedoList.FBlockChangeNumber = 0) and
   (FRedoList.FBlockChangeNumber = 0));


  if FUndoList.Count > 0 then
  begin
    Item := FUndoList.Peek;
    OldModified := GetModified;
    OldChangeNumber := Item.ChangeNumber;
    FRedoList.BlockChangeNumber := FRedoList.NextChangeNumber;

    try
      repeat
        Item := FUndoList.Extract;
        FLastChange := Item.ChangeReason;
        FUndoItem(Item);
        Item.Free;

        if FUndoList.Count = 0 then
          Break
        else
          Item := FUndoList.Peek;

        if Item.ChangeNumber = OldChangeNumber then
          FKeepGoing := True
        else
          FKeepGoing :=
            { Group together same undo actions }
            (FGroupUndo and (FLastChange = Item.ChangeReason) and
            // Next item is not a group break
            not Item.GroupBreak and
            { crUn/Indent act as a group break }
            not(FLastChange in [crIndent, crUnindent]));
      until not(FKeepGoing);

    finally
      if (OldModified xor GetModified) and Assigned(FOnModifiedChanged) then
        FOnModifiedChanged(Self);

      FRedoList.BlockChangeNumber := 0;
    end;
  end;
end;

procedure TSynEditUndo.Unlock;
begin
  if FLockCount > 0 then
    Dec(FLockCount);
end;

{ Factory Method}

function CreateSynEditUndo(UndoMethod, RedoMethod: TSynUndoRedoItem):
  ISynEditUndo;
begin
  Result := TSynEditUndo.Create(UndoMethod, RedoMethod);
end;

end.
