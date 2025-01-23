{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynEditWordWrap.pas by Flávio Etrusco, released 2003-12-11.
Unicode translation by Maël Hörz.
All Rights Reserved.

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
-------------------------------------------------------------------------------}

unit SynEditWordWrap;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  SynEditTypes,
  SynEditTextBuffer,
  SynEdit;

const
  MaxIndex = MaxInt div 16;

type
  TLineIndex = 0..MaxIndex;
  TRowIndex = 0..MaxIndex;

  // fLineOffsets[n] is the index of the first row of the [n+1]th line.
  // e.g. Starting row of first line (0) is 0. Starting row of second line (1)
  // is fLineOffsets[0]. Clear?

  TSynWordWrapPlugin = class(TInterfacedObject, ISynEditBufferPlugin)
  private
    FLineOffsets: TList<Integer>;
    FRowLengths: TList<Integer>;
    FLineCount: Integer;
    FEditor: TCustomSynEdit;
    FMaxRowWidth: Integer;
  protected
    procedure WrapLine(const Index: Integer; out RowLengths: TArray<Integer>);
    procedure WrapLines;
    function ReWrapLine(aIndex: TLineIndex; IsLineInserted: Boolean = False): Integer;
    procedure TrimArrays;
    property Editor: TCustomSynEdit read FEditor;
  public
    constructor Create(aOwner: TCustomSynEdit);
    destructor Destroy; override;
    { ISynEditBufferPlugin }
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RowCount: Integer;
    function GetRowLength(aRow: Integer): Integer;
    function LinesInserted(aIndex: Integer; aCount: Integer): Integer;
    function LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
    function LinePut(aIndex: Integer; const OldLine: string): Integer;
    procedure Reset;
    procedure DisplayChanged;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.D2D1,
  System.RTLConsts,
  System.Math,
  System.Threading,
  SynUnicode,
  SynEditMiscProcs,
  SynDWrite;


{ TSynWordWrapPlugin }

function TSynWordWrapPlugin.BufferToDisplayPos(
  const aPos: TBufferCoord): TDisplayCoord;
var
  vStartRow: Integer; // first row of the line
  cRow: Integer;
  vRowLen: Integer;
begin
  Assert(aPos.Char > 0);
  Assert(aPos.Line > 0);
  if FLineCount < aPos.Line then
  begin
    // beyond EOF
    Result.Column := aPos.Char;
    Result.Row := FRowLengths.Count + (aPos.Line - FLineCount);
    Exit;
  end;
  if aPos.Line = 1 then
    vStartRow := 0
  else
    vStartRow := FLineOffsets[aPos.Line - 2];
  vRowLen := 0;
  for cRow := vStartRow to FLineOffsets[aPos.Line - 1] - 1 do
  begin
    Inc(vRowLen, FRowLengths[cRow]);
    if aPos.Char <= vRowLen then
    begin
      Result.Column := aPos.Char - vRowLen + FRowLengths[cRow];
      Result.Row := cRow + 1;
      Exit;
    end;
  end;
  // beyond EOL
  Result.Column := aPos.Char - vRowLen + FRowLengths[FLineOffsets[aPos.Line - 1] - 1];
  Result.Row := FLineOffsets[aPos.Line - 1];
end;

constructor TSynWordWrapPlugin.Create(aOwner: TCustomSynEdit);
begin
  inherited Create; // just to work as reminder in case I revert it to a TComponent...
  if aOwner = nil then
    raise Exception.Create( 'Owner of TSynWordWrapPlugin must be a TCustomSynEdit' );
  FEditor := aOwner;
  FLineCount := FEditor.Lines.Count;
  FLineOffsets := TList<Integer>.Create;
  FRowLengths := TList<Integer>.Create;
  Reset;
end;

destructor TSynWordWrapPlugin.Destroy;
begin
  inherited;
  FLineOffsets.Free;;
  FRowLengths.Free;
end;

procedure TSynWordWrapPlugin.DisplayChanged;
begin
  if Max(Editor.WrapAreaWidth, 2 * Editor.CharWidth) <> FMaxRowWidth then
    Reset;
end;

function TSynWordWrapPlugin.DisplayToBufferPos(
  const aPos: TDisplayCoord): TBufferCoord;
var
  cLine: Integer;
  cRow: Integer;
begin
  Assert(aPos.Column > 0);
  Assert(aPos.Row > 0);
  if aPos.Row > FRowLengths.Count then
  begin
    // beyond EOF
    Result.Char := aPos.Column;
    Result.Line := aPos.Row - FRowLengths.Count + FLineCount;
    Exit;
  end;
  //Optimized loop start point but could use binary search
  for cLine := Min(aPos.Row - 2, FLineCount - 2) downto 0 do
    if aPos.Row > FLineOffsets[cLine] then
    begin
      Result.Line := cLine + 2;
      if aPos.Row = FLineOffsets[cLine + 1] then //last row of line
        Result.Char := aPos.Column
      else
        Result.Char := Min(aPos.Column, FRowLengths[aPos.Row - 1] + 1);
      for cRow := FLineOffsets[cLine] to aPos.Row - 2 do
        Inc(Result.Char, FRowLengths[cRow]);
      Exit;
    end;
  // first line
  Result.Line := 1;
  if aPos.Row = FLineOffsets[0] then //last row of line
    Result.Char := aPos.Column
  else
    Result.Char := Min(aPos.Column, FRowLengths[aPos.Row - 1] + 1);
  for cRow := 0 to aPos.Row - 2 do
    Inc(Result.Char, FRowLengths[cRow]);
end;

function TSynWordWrapPlugin.GetRowLength(aRow: Integer): Integer;
// aRow is 1-based...
begin
  if (aRow <= 0) or (aRow > FRowLengths.Count) then
    TList.Error(SListIndexError, aRow);
  Result := FRowLengths[aRow - 1];
end;

function TSynWordWrapPlugin.LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
// Returns the number of rows deleted
var
  vStartRow: Integer;
  vEndRow: Integer;
  cLine: Integer;
begin
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex + aCount <= FLineCount);

  if aIndex = 0 then
    vStartRow := 0
  else
    vStartRow := FLineOffsets[aIndex - 1];
  vEndRow := FLineOffsets[aIndex + aCount - 1];
  Result := vEndRow - vStartRow;
  // resize FRowLengths
  if vStartRow < FRowLengths.Count then
    FRowLengths.DeleteRange(vStartRow, Result);
  // resize FLineOffsets
  FLineOffsets.DeleteRange(aIndex, aCount);
  Dec(FLineCount, aCount);
  // update offsets
  for cLine := aIndex to FLineCount - 1 do
    Dec(FLineOffsets.List[cLine], Result);
  if FLineCount = 0 then
    TrimArrays;
end;

function TSynWordWrapPlugin.LinesInserted(aIndex: Integer; aCount: Integer): Integer;
// Returns the number of rows inserted
var
  cLine: Integer;
  TempArray: TArray<Integer>;
begin
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex <= FLineCount);
  Inc(FLineCount, aCount);
  // resize FLineOffsets
  SetLength(TempArray, aCount);
  FLineOffsets.InsertRange(aIndex, TempArray);
  // Rewrap
  Result := 0;
  for cLine := aIndex to aIndex + aCount - 1 do
    Inc(Result, ReWrapLine(cLine, True));
  // Adjust lines below
  for cLine := aIndex + aCount to FLineCount - 1 do
    Inc(FLineOffsets.List[cLine], Result);
end;

function TSynWordWrapPlugin.LinePut(aIndex: Integer; const OldLine: string): Integer;
var
  cLine: Integer;
begin
  Assert(aIndex >= 0);
  Assert(aIndex < FLineCount);
  // Rewrap
  Result := ReWrapLine(aIndex);
  // Adjust lines below
  if Result <> 0 then
    for cLine := aIndex + 1 to FLineCount - 1 do
      Inc(FLineOffsets.List[cLine], Result);
end;

procedure TSynWordWrapPlugin.Reset;
begin
  // Ensure minimum line length
  FMaxRowWidth := Max(Editor.WrapAreaWidth, 2 * Editor.CharWidth);

  WrapLines;
end;

function TSynWordWrapPlugin.ReWrapLine(aIndex: TLineIndex; IsLineInserted: Boolean): Integer;
// Wraps the line and adjusts fRowLenghts and FLineOffsets[aIndex]
// Returns RowCount delta (how many wrapped lines were added or removed by this change).
var
  RowLengths: TArray<Integer>;
  PrevOffset: Integer;
  PrevRowCount: Integer;
begin
  WrapLine(aIndex, RowLengths);

  if aIndex = 0 then
    PrevOffset := 0
  else
    PrevOffset := FLineOffsets[aIndex - 1];
  if IsLineInserted then
    PrevRowCount := 0
  else
    PrevRowCount := FLineOffsets[aIndex] - PrevOffset;
  if PrevRowCount > 0 then
    FRowLengths.DeleteRange(PrevOffset, PrevRowCount);
  FRowLengths.InsertRange(PrevOffset, RowLengths);
  FLineOffsets[aIndex] := PrevOffset + Length(RowLengths);
  Result := Length(RowLengths) - PrevRowCount;
end;

procedure TSynWordWrapPlugin.WrapLines;
var
  cLine: Integer;
  RowLengths: TArray<TArray<Integer>>;
begin
  FLineOffsets.Clear;
  FLineOffsets.Capacity := Editor.Lines.Count;
  FRowLengths.Clear;
  FRowLengths.Capacity := Editor.Lines.Count;

  if (Editor.Lines.Count = 0) or (FMaxRowWidth < Editor.CharWidth) then
    Exit;

  SetLength(RowLengths, Editor.Lines.Count);
  TParallel.&For(0, Editor.Lines.Count - 1, procedure(I: Integer)
  begin
    WrapLine(I, RowLengths[I]);
  end);

  for cLine := 0 to Editor.Lines.Count - 1 do
  begin
    FRowLengths.AddRange(RowLengths[cLine]);
    FLineOffsets.Add(FRowLengths.Count);
  end;
end;

function TSynWordWrapPlugin.RowCount: Integer;
begin
  Result := FRowLengths.Count;
  if FLineCount > 0 then
    Assert(Result = FLineOffsets[FLineCount - 1])
  else
    Assert(Result = 0);
end;

procedure TSynWordWrapPlugin.TrimArrays;
begin
  FLineOffsets.TrimExcess;
  FRowLengths.TrimExcess;
end;

procedure TSynWordWrapPlugin.WrapLine(const Index: Integer;
  out RowLengths: TArray<Integer>);
var
  SLine: string;
  Layout: TSynTextLayout;
  W: Integer;
  P, P2, PStart, PEnd, PBreak: PChar;
  CW, TW, LW: Integer;
  IsTrailing, IsInside: BOOL;
  fWorkList: TList<Integer>;
  HTM: TDwriteHitTestMetrics;
begin
  CW := Editor.CharWidth;
  TW := Editor.TabWidth * Editor.CharWidth;
  SLine := Editor.Lines[Index];

  PStart := PChar(SLine);
  PEnd := PStart + SLine.Length;
  if (PEnd - PStart) * CW < FMaxRowWidth div 3 then
    // Optimization.  Assume line will fit!
    RowLengths := [SLine.Length]
  else
  begin
    fWorkList := TList<Integer>.Create;
    try
      // Preallocation helps with very long lines
      fWorkList.Capacity := MulDiv(SLine.Length, CW + 1, FMaxRowWidth);
      P := PStart;
      PBreak := nil;
      W := 0;
      while (P < PEnd) do
      begin
        while (P < PEnd) do
        begin
          // Special case with space. Keep it on the row even if it does't fit.
          if (P > PStart) and Editor.IsWordBreakChar(P^) then
            PBreak := P + IfThen(P^ = #32, 1, 0);
          case P^ of
             #9: Inc(W, TW - W mod TW);
             #32..#126, #$00A0: Inc(W, CW);
          else
            Break;
          end;
          if W > FMaxRowWidth then
            Break;
          if (P > PStart) and Editor.IsWordBreakChar(P^) then
            // Keep opening brackets with the next line
            PBreak := P + IfThen(Word(P^) in [40, 91], 0, 1);
          Inc(P);
        end;

        if (P < PEnd) and (W <= FMaxRowWidth) then
        begin
          // Just in case P is followed by combining characters
          if (P > PStart) and not (Word((P-1)^) in [9, 32]) then
          begin
            Dec(P);
            Dec(W, CW);
          end;

          // Measure non-ascii text code points
          P2 := P;
          while P2 < PEnd do
          begin
            Inc(P2);
            if Word(P2^) in [9, 65..90, 97..122] then Break;
          end;

          Layout.Create(Editor.TextFormat, P, P2-P, MaxInt, Editor.LineHeight);
          LW := Round(Layout.TextMetrics.width);

          if W + LW >= FMaxRowWidth then
          begin
            CheckOSError(Layout.IDW.HitTestPoint(FMaxRowWidth - W,
              Editor.LineHeight div 2, IsTrailing, IsInside, HTM));
            Inc(P, HTM.textPosition);
          end
          else
            P := P2;
          Inc(W, LW);
        end;

        if W >= FMaxRowWidth then
        begin
          if Assigned(PBreak) then
          begin
            FWorkList.Add(PBreak - PStart);
            PStart := PBreak;
            P := PStart;
            PBreak := nil;
          end
          else
          begin
            // "emergency" wrapping
            FWorkList.Add(P - PStart);
            PStart := P;
          end;
          W := 0;
        end;
      end;
      if P > PStart then
        FWorkList.Add(P - PStart);

      RowLengths := fWorkList.ToArray;
    finally
      fWorkList.Free;
    end;
  end;
end;

end.
