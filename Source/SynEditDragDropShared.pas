{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.

Cross-platform drag-and-drop helper logic shared by VCL and FMX editors.
-------------------------------------------------------------------------------}

unit SynEditDragDropShared;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  SynEditTypes;

type
  TSynDropAction = (sdaNone, sdaCopy, sdaMove);

  TSynDropInfo = record
    DoDrop: Boolean;
    DropAfter: Boolean;
  end;

  TSynDragDropHelper = record
    /// Returns True if Ctrl is held (= copy operation).
    class function IsDropCopy(Shift: TShiftState): Boolean; static;

    /// Determines whether a drop should proceed and whether the drop
    /// position is after the current selection (for internal drags).
    class function ComputeDropInfo(const DropPos, SelStart, SelEnd: TBufferCoord;
      IsInternal, IsMove: Boolean): TSynDropInfo; static;

    /// Adjusts the drop position after the selected text has been deleted
    /// (internal move).
    class function AdjustDropPos(const DropPos, SelStart, SelEnd: TBufferCoord;
      DropAfter: Boolean): TBufferCoord; static;
  end;

implementation

{ TSynDragDropHelper }

class function TSynDragDropHelper.IsDropCopy(Shift: TShiftState): Boolean;
begin
  Result := ssCtrl in Shift;
end;

class function TSynDragDropHelper.ComputeDropInfo(
  const DropPos, SelStart, SelEnd: TBufferCoord;
  IsInternal, IsMove: Boolean): TSynDropInfo;
begin
  if not IsInternal then
  begin
    // External drop: always allowed
    Result.DoDrop := True;
    Result.DropAfter := False;
    Exit;
  end;

  // Internal drag: check if drop position is inside selection
  Result.DropAfter := (DropPos.Line > SelEnd.Line)
    or ((DropPos.Line = SelEnd.Line) and ((DropPos.Char > SelEnd.Char)
    or (not IsMove and (DropPos.Char = SelEnd.Char))));

  Result.DoDrop := Result.DropAfter
    or (DropPos.Line < SelStart.Line)
    or ((DropPos.Line = SelStart.Line) and ((DropPos.Char < SelStart.Char)
    or (not IsMove and (DropPos.Char = SelStart.Char))));
end;

class function TSynDragDropHelper.AdjustDropPos(
  const DropPos, SelStart, SelEnd: TBufferCoord;
  DropAfter: Boolean): TBufferCoord;
begin
  Result := DropPos;
  if not DropAfter then
    Exit;

  // Selection was deleted before the drop position — adjust accordingly
  if DropPos.Line = SelEnd.Line then
    Dec(Result.Char, SelEnd.Char - SelStart.Char);
  if SelEnd.Line > SelStart.Line then
    Dec(Result.Line, SelEnd.Line - SelStart.Line);
end;

end.
