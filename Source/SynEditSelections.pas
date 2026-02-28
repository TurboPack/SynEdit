{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Shared multi-selection / multi-caret base class.
Platform-independent logic extracted from Vcl.SynEditMiscClasses.TSynSelections.
VCL and FMX subclass TSynSelectionsBase with thin editor-specific overrides.
-------------------------------------------------------------------------------}

unit SynEditSelections;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  SynEditTypes,
  SynEditMiscProcs;

type
  TSynSelStorage = record
    Selections: TArray<TSynSelection>;
    BaseIndex, ActiveIndex: Integer;
    procedure Clear;
  end;

  TSynSelectionsBase = class
  public
    type
      TKeepSelection = (ksKeepBase, ksKeepActive);
  private
    function GetCount: Integer;
    function GetActiveSelection: TSynSelection;
    function GetBaseSelection: TSynSelection;
    procedure SetActiveSelection(const Value: TSynSelection);
    procedure SetBaseSelection(const Value: TSynSelection);
    function GetSelection(Index: Integer): TSynSelection;
    procedure SetActiveSelIndex(const Index: Integer);
    function GetIsEmpty: Boolean;
  protected
    FOwner: TObject;
    FSelections: TList<TSynSelection>;
    FBaseSelIndex: Integer;
    FActiveSelIndex: Integer;
    // === Abstract methods — editor coupling points ===
    procedure CaretsChanged; virtual; abstract;
    procedure DoInvalidateSelection(const Sel: TSynSelection); virtual; abstract;
    procedure DoRestoreSelection(const Sel: TSynSelection;
      EnsureVisible: Boolean); virtual; abstract;
    function GetLineText(ALine: Integer): string; virtual; abstract;
    // Word-wrap / column selection support
    function GetWordWrap: Boolean; virtual; abstract;
    function GetScrollPastEOL: Boolean; virtual; abstract;
    function GetRowLength(ARow: Integer): Integer; virtual; abstract;
    function BufferToDisplayPos(const P: TBufferCoord): TDisplayCoord; virtual; abstract;
    function DisplayToBufferPos(const P: TDisplayCoord): TBufferCoord; virtual; abstract;
    function SelectionToDisplayRow(var Sel: TSynSelection): Integer; virtual; abstract;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    // Selection management
    procedure Clear(KeepSelection: TKeepSelection = ksKeepActive);
    function AddCaret(const ACaret: TBufferCoord;
      IsBase: Boolean = False): Boolean;
    procedure DeleteSelection(Index: Integer);
    function FindCaret(const ACaret: TBufferCoord): Integer;
    function FindSelection(const BC: TBufferCoord;
      var Index: Integer): Boolean;
    procedure MouseSelection(const Sel: TSynSelection);
    procedure ColumnSelection(Anchor, ACaret: TBufferCoord;
      LastPosX: Integer = 0);
    procedure Merge;
    function PartSelectionsForRow(
      const RowStart, RowEnd: TBufferCoord): TSynSelectionArray;
    function RowHasCaret(ARow, ALine: Integer): Boolean;
    // Invalidation
    procedure InvalidateSelection(Index: Integer);
    procedure InvalidateAll;
    // Store/Restore
    procedure Store(out SelStorage: TSynSelStorage);
    procedure Restore(const [Ref] SelStorage: TSynSelStorage); overload;
    procedure Restore(const [Ref] Sel: TSynSelection;
      EnsureVisible: Boolean = True); overload;
    // Line-change adjustment
    procedure LinesInserted(FirstLine, aCount: Integer);
    procedure LinesDeleted(FirstLine, aCount: Integer);
    procedure LinePut(aIndex: Integer; const OldLine: string);
    // Properties
    property BaseSelectionIndex: Integer read FBaseSelIndex;
    property ActiveSelection: TSynSelection read GetActiveSelection
      write SetActiveSelection;
    property BaseSelection: TSynSelection read GetBaseSelection
      write SetBaseSelection;
    property Count: Integer read GetCount;
    property ActiveSelIndex: Integer read FActiveSelIndex
      write SetActiveSelIndex;
    property IsEmpty: Boolean read GetIsEmpty;
    property Selection[Index: Integer]: TSynSelection
      read GetSelection; default;
  end;

implementation

{ TSynSelStorage }

procedure TSynSelStorage.Clear;
begin
  Selections := [];
end;

{ TSynSelectionsBase }

constructor TSynSelectionsBase.Create(Owner: TObject);
begin
  inherited Create;
  FOwner := Owner;
  FSelections := TList<TSynSelection>.Create(TComparer<TSynSelection>.Construct(
    function(const L, R: TSynSelection): Integer
    begin
      if L.Normalized.Start < R.Normalized.Start then
        Result := -1
      else if L.Normalized.Start = R.Normalized.Start then
        Result := 0
      else
        Result := 1;
    end));
end;

destructor TSynSelectionsBase.Destroy;
begin
  FSelections.Free;
  inherited;
end;

function TSynSelectionsBase.AddCaret(const ACaret: TBufferCoord;
  IsBase: Boolean): Boolean;
var
  Sel: TSynSelection;
  Index: Integer;
begin
  Result := False;
  if FindSelection(ACaret, Index) then
  begin
    DeleteSelection(Index);
    Restore(FSelections[FActiveSelIndex], False);
  end
  else if (Index > 0) and (FSelections[Index - 1].Caret = ACaret) then
  begin
    DeleteSelection(Index - 1);
    Restore(FSelections[FActiveSelIndex], False);
  end
  else
  begin
    Sel := TSynSelection.Create(ACaret, ACaret, ACaret);
    FSelections.Insert(Index, Sel);
    FActiveSelIndex := Index;
    if IsBase then
      FBaseSelIndex := Index
    else if FBaseSelIndex >= Index then
      Inc(FBaseSelIndex);
    Result := True;
  end;
end;

procedure TSynSelectionsBase.Clear(KeepSelection: TKeepSelection);
var
  Index: Integer;
begin
  if FSelections.Count = 1 then Exit;

  if (KeepSelection = ksKeepBase) and (FActiveSelIndex <> FBaseSelIndex) then
    Restore(BaseSelection);

  for Index := FSelections.Count - 1 downto 0 do
    if not (((KeepSelection = ksKeepBase) and (Index = FBaseSelIndex)) or
      ((KeepSelection = ksKeepActive) and (Index = FActiveSelIndex)))
    then
      DeleteSelection(Index);

  Assert(FSelections.Count = 1);
  FBaseSelIndex := 0;
  FActiveSelIndex := 0;
  CaretsChanged;
end;

procedure TSynSelectionsBase.ColumnSelection(Anchor, ACaret: TBufferCoord;
  LastPosX: Integer);

  procedure SetLineSelection(Index, Line, FromChar, ToChar: Integer;
    ScrollPastEOL: Boolean);
  var
    LineString: string;
    Len: Integer;
  begin
    LineString := GetLineText(Line);
    Len := LineString.Length;
    if not ScrollPastEOL then
      ToChar := EnsureRange(ToChar, 1, Len + 1);
    FromChar := EnsureRange(FromChar, 1, Len + 1);
    FSelections.List[Index].Caret := BufferCoord(ToChar, Line);
    FSelections.List[Index].Start := BufferCoord(FromChar, Line);
    FSelections.List[Index].Stop := BufferCoord(Min(ToChar, Len + 1), Line);
    FSelections.List[Index].LastPosX := LastPosX;
    InvalidateSelection(Index);
  end;

  procedure SetRowSelection(Index, Row, FromChar, ToChar: Integer;
    ScrollPastEOL: Boolean);
  var
    Len: Integer;
  begin
    Len := GetRowLength(Row);
    if not ScrollPastEOL then
      ToChar := EnsureRange(ToChar, 1, Len + 1);
    FromChar := EnsureRange(FromChar, 1, Len + 1);
    FSelections.List[Index].Caret :=
      DisplayToBufferPos(DisplayCoord(ToChar, Row));
    FSelections.List[Index].Start :=
      DisplayToBufferPos(DisplayCoord(FromChar, Row));
    FSelections.List[Index].Stop :=
      DisplayToBufferPos(DisplayCoord(Min(ToChar, Len + 1), Row));
    FSelections.List[Index].LastPosX := LastPosX;
    InvalidateSelection(Index);
  end;

var
  DC: TDisplayCoord;
  FromChar, ToChar: Integer;
  FromRow, ToRow: Integer;
  Line, Row: Integer;
  Index: Integer;
  Increment: Integer;
  ScrollPastEOL: Boolean;
begin
  Clear;
  InvalidateSelection(0);

  ScrollPastEOL := GetScrollPastEOL;

  if GetWordWrap then
  begin
    DC := BufferToDisplayPos(Anchor);
    FromChar := DC.Column;
    FromRow := DC.Row;
    DC := BufferToDisplayPos(ACaret);
    ToChar := DC.Column;
    ToRow := DC.Row;

    SetRowSelection(0, FromRow, FromChar, ToChar, ScrollPastEOL);

    Increment := Sign(ToRow - FromRow);

    Row := FromRow;
    while Row <> ToRow do
    begin
      Row := Row + Increment;
      if Increment > 0 then
        Index := FSelections.Add(TSynSelection.Invalid)
      else
      begin
        FSelections.Insert(0, TSynSelection.Invalid);
        Index := 0;
      end;
      SetRowSelection(Index, Row, FromChar, ToChar, ScrollPastEOL);
    end;
  end
  else
  begin
    FromChar := Anchor.Char;
    ToChar := ACaret.Char;
    SetLineSelection(0, Anchor.Line, FromChar, ToChar, ScrollPastEOL);

    Increment := Sign(ACaret.Line - Anchor.Line);

    Line := Anchor.Line;
    while Line <> ACaret.Line do
    begin
      Line := Line + Increment;
      if Increment > 0 then
        Index := FSelections.Add(TSynSelection.Invalid)
      else
      begin
        FSelections.Insert(0, TSynSelection.Invalid);
        Index := 0;
      end;
      SetLineSelection(Index, Line, FromChar, ToChar, ScrollPastEOL);
    end;
  end;

  if Increment >= 0 then
  begin
    FBaseSelIndex := 0;
    FActiveSelIndex := FSelections.Count - 1;
  end
  else
  begin
    FBaseSelIndex := FSelections.Count - 1;
    FActiveSelIndex := 0;
  end;

  Restore(ActiveSelection, False);
  CaretsChanged;
end;

procedure TSynSelectionsBase.DeleteSelection(Index: Integer);
var
  Sel: TSynSelection;
begin
  if FSelections.Count <= 1 then Exit;

  Sel := FSelections[Index];
  DoInvalidateSelection(Sel);
  FSelections.Delete(Index);

  if Index = FActiveSelIndex then
  begin
    if Index >= FSelections.Count then
      FActiveSelIndex := FSelections.Count - 1;
  end
  else if FActiveSelIndex > Index then
    Dec(FActiveSelIndex);

  if FBaseSelIndex = Index then
    FBaseSelIndex := FSelections.Count - 1
  else if FBaseSelIndex > Index then
    Dec(FBaseSelIndex);

  CaretsChanged;
end;

function TSynSelectionsBase.FindCaret(const ACaret: TBufferCoord): Integer;
var
  Index: Integer;
begin
  if FSelections.Count = 0 then Exit(-1);

  if FindSelection(ACaret, Index) then
  begin
    if FSelections[Index].Caret = ACaret then
      Result := Index
    else
      Result := -1;
  end
  else if (Index > 0) and (FSelections[Index - 1].Caret = ACaret) then
    Result := Index - 1
  else
    Result := -1;
end;

function TSynSelectionsBase.FindSelection(const BC: TBufferCoord;
  var Index: Integer): Boolean;
begin
  if FSelections.BinarySearch(TSynSelection.Create(BC, BC, BC), Index) then
    Exit(True);

  if Index = 0 then
    Exit(False);

  Result := FSelections[Index - 1].Contains(BC);
  if Result then
    Dec(Index);
end;

function TSynSelectionsBase.GetActiveSelection: TSynSelection;
begin
  Result := FSelections[FActiveSelIndex];
end;

function TSynSelectionsBase.GetBaseSelection: TSynSelection;
begin
  Result := FSelections[FBaseSelIndex];
end;

function TSynSelectionsBase.GetCount: Integer;
begin
  Result := FSelections.Count;
end;

function TSynSelectionsBase.GetIsEmpty: Boolean;
var
  Index: Integer;
begin
  Result := True;
  for Index := 0 to FSelections.Count - 1 do
    if not FSelections.List[Index].IsEmpty then
      Exit(False);
end;

function TSynSelectionsBase.GetSelection(Index: Integer): TSynSelection;
begin
  Result := FSelections[Index];
end;

procedure TSynSelectionsBase.InvalidateAll;
var
  Index: Integer;
begin
  for Index := 0 to FSelections.Count - 1 do
    InvalidateSelection(Index);
end;

procedure TSynSelectionsBase.InvalidateSelection(Index: Integer);
begin
  DoInvalidateSelection(FSelections[Index]);
end;

procedure TSynSelectionsBase.LinePut(aIndex: Integer; const OldLine: string);
var
  I: Integer;
  Line: string;
  OldLen, NewLen: Integer;
  StartPos: Integer;
  Delta: Integer;
begin
  if FSelections.Count <= 1 then Exit;

  Line := GetLineText(aIndex + 1);
  LineDiff(Line, OldLine, StartPos, OldLen, NewLen);
  Delta := NewLen - OldLen;

  for I := FActiveSelIndex + 1 to Count - 1 do
  begin
    with FSelections.List[I] do
    begin
      if (Start.Line > aIndex + 1) and (Stop.Line > aIndex + 1) then
        Exit;

      if Caret.Line = aIndex + 1 then Inc(Caret.Char, Delta);
      if Start.Line = aIndex + 1 then Inc(Start.Char, Delta);
      if Stop.Line = aIndex + 1 then Inc(Stop.Char, Delta);
    end;
  end;
end;

procedure TSynSelectionsBase.LinesDeleted(FirstLine, aCount: Integer);
var
  I: Integer;
  MinBC: TBufferCoord;
begin
  if FSelections.Count <= 1 then Exit;

  for I := FActiveSelIndex + 1 to Count - 1 do
    with FSelections.List[I] do
    begin
      if Caret.Line >= FirstLine + 1 then Dec(Caret.Line, aCount);
      if Start.Line >= FirstLine + 1 then Dec(Start.Line, aCount);
      if Stop.Line >= FirstLine + 1 then Dec(Stop.Line, aCount);

      if (Start.Line < FirstLine + 1) and (Stop.Line < FirstLine + 1) then
      begin
        FSelections.List[I] := TSynSelection.Invalid;
        Continue;
      end;

      MinBC := BufferCoord(FirstLine + 1, 1);
      Caret := TBufferCoord.Max(Caret, MinBC);
      Start := TBufferCoord.Max(Start, MinBC);
      Stop := TBufferCoord.Max(Stop, MinBC);
    end;
end;

procedure TSynSelectionsBase.LinesInserted(FirstLine, aCount: Integer);
var
  I: Integer;
begin
  if FSelections.Count <= 1 then Exit;

  for I := FActiveSelIndex + 1 to Count - 1 do
    with FSelections.List[I] do
    begin
      if Caret.Line >= FirstLine + 1 then Inc(Caret.Line, aCount);
      if Start.Line >= FirstLine + 1 then Inc(Start.Line, aCount);
      if Stop.Line >= FirstLine + 1 then Inc(Stop.Line, aCount);
    end;
end;

procedure TSynSelectionsBase.Merge;

  function DoMerge(const Sel, NextSel: TSynSelection): TSynSelection;
  var
    Caret, Start, Stop: TBufferCoord;
  begin
    Start := TBufferCoord.Min(
      TBufferCoord.Min(Sel.Start, Sel.Stop),
      TBufferCoord.Min(NextSel.Start, NextSel.Stop));
    Stop := TBufferCoord.Max(
      TBufferCoord.Max(Sel.Start, Sel.Stop),
      TBufferCoord.Max(NextSel.Start, NextSel.Stop));

    if NextSel.Caret = TBufferCoord.Min(NextSel.Start, NextSel.Stop) then
      Caret := Start
    else
      Caret := Stop;

    Result := TSynSelection.Create(Caret, Start, Stop);
    Result.LastPosX := Sel.LastPosX;
    Result.CaretAtEOL := Sel.CaretAtEOL;
  end;

var
  Sel, NextSel: TSynSelection;
  I: Integer;
  BC: TBufferCoord;
begin
  if FSelections.Count = 1 then Exit;

  // Remove Invalid
  for I := Count - 1 downto 0 do
    if not FSelections.List[I].IsValid then
      DeleteSelection(I);

  NextSel := FSelections.List[Count - 1];
  for I := Count - 2 downto 0 do
  begin
    Sel := FSelections.List[I];

    if (Sel = NextSel) or Sel.Intersects(NextSel) then
    begin
      Sel := DoMerge(Sel, NextSel);
      FSelections.List[I] := Sel;
      DeleteSelection(I + 1);
    end;
    NextSel := Sel;
  end;

  // Process the case of one invalid selection
  if (FSelections.Count = 1) and not FSelections.List[0].IsValid then
  begin
    BC := BufferCoord(1, 1);
    FSelections.List[0] := TSynSelection.Create(BC, BC, BC);
  end;

  // Activate the current selection
  Restore(ActiveSelection, False);
end;

procedure TSynSelectionsBase.MouseSelection(const Sel: TSynSelection);
begin
  if FSelections.Count <= 1 then Exit;

  for var Index := FSelections.Count - 1 downto 0 do
  begin
    if Index = FActiveSelIndex then
      Continue;
    if Sel.Intersects(FSelections.List[Index]) then
      DeleteSelection(Index);
  end;
end;

function TSynSelectionsBase.PartSelectionsForRow(
  const RowStart, RowEnd: TBufferCoord): TSynSelectionArray;
var
  Sel: TSynSelection;
begin
  Result := [];
  for var Index := 0 to FSelections.Count - 1 do
  begin
    Sel := FSelections.List[Index].Normalized;
    if Sel.Stop < RowStart then
      Continue
    else if Sel.Start > RowEnd then
      Exit
    else if not Sel.IsEmpty then
      Result := Result + [Sel];
  end;
end;

procedure TSynSelectionsBase.Restore(const [Ref] SelStorage: TSynSelStorage);
begin
  InvalidateAll;
  FSelections.Clear;
  FSelections.AddRange(SelStorage.Selections);
  FActiveSelIndex := SelStorage.ActiveIndex;
  FBaseSelIndex := SelStorage.BaseIndex;
  InvalidateAll;
  Restore(ActiveSelection);
  CaretsChanged;
end;

procedure TSynSelectionsBase.Restore(const [Ref] Sel: TSynSelection;
  EnsureVisible: Boolean);
begin
  DoRestoreSelection(Sel, EnsureVisible);
end;

function TSynSelectionsBase.RowHasCaret(ARow, ALine: Integer): Boolean;

  function IsCaretOnRow(Sel: TSynSelection): Boolean;
  begin
    if GetWordWrap then
      Result := SelectionToDisplayRow(Sel) = ARow
    else
      Result := Sel.Caret.Line = ALine;
  end;

var
  Sel: TSynSelection;
  Index: Integer;
begin
  FindSelection(BufferCoord(1, ALine), Index);

  Result := False;
  while Index < FSelections.Count do
  begin
    Sel := FSelections[Index].Normalized;
    if Sel.Start.Line > ALine then Break;
    Result := IsCaretOnRow(Sel);
    if Result then Break;
    Inc(Index);
  end;
end;

procedure TSynSelectionsBase.SetActiveSelection(const Value: TSynSelection);
begin
  FSelections[FActiveSelIndex] := Value;
end;

procedure TSynSelectionsBase.SetActiveSelIndex(const Index: Integer);
var
  Sel: TSynSelection;
begin
  Assert(InRange(Index, 0, Count - 1));
  if Index <> FActiveSelIndex then
  begin
    FActiveSelIndex := Index;
    Sel := ActiveSelection;
    if Sel.IsValid then
      Restore(ActiveSelection, False);
  end;
end;

procedure TSynSelectionsBase.SetBaseSelection(const Value: TSynSelection);
begin
  FSelections[FBaseSelIndex] := Value;
end;

procedure TSynSelectionsBase.Store(out SelStorage: TSynSelStorage);
begin
  SelStorage.Selections := FSelections.ToArray;
  SelStorage.BaseIndex := FBaseSelIndex;
  SelStorage.ActiveIndex := FActiveSelIndex;
end;

end.
