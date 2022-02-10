{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynEditWordWrap.pas by Fl�vio Etrusco, released 2003-12-11.
Unicode translation by Ma�l H�rz.
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
    fLineOffsets: TList<Integer>;
    fRowLengths: TList<Integer>;
    fLineCount: integer;

    fEditor: TCustomSynEdit;
    fMaxRowWidth: Integer;
    procedure SetEmpty;
    function PrepareLine(const S: string): string;
  protected
    procedure WrapLine(const Index: Integer; out RowLengths: TArray<Integer>);
    procedure WrapLines;
    function ReWrapLine(aIndex: TLineIndex): integer;
    procedure TrimArrays;
    property Editor: TCustomSynEdit read fEditor;
  public
    constructor Create(aOwner: TCustomSynEdit);
    destructor Destroy; override;
    { ISynEditBufferPlugin }
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RowCount: integer;
    function GetRowLength(aRow: integer): integer;
    function LinesInserted(aIndex: integer; aCount: integer): integer;
    function LinesDeleted(aIndex: integer; aCount: integer): integer;
    function LinePut(aIndex: integer; const OldLine: string): integer;
    procedure Reset;
    procedure DisplayChanged;
  end;

implementation

uses
  Winapi.D2D1,
  System.RTLConsts,
  System.Math,
  SynUnicode,
  SynEditMiscProcs,
  SynDWrite;


{ TSynWordWrapPlugin }

function TSynWordWrapPlugin.BufferToDisplayPos(
  const aPos: TBufferCoord): TDisplayCoord;
var
  vStartRow: integer; // first row of the line
  cRow: integer;
  vRowLen: integer;
begin
  Assert(aPos.Char > 0);
  Assert(aPos.Line > 0);
  if fLineCount < aPos.Line then
  begin
    // beyond EOF
    Result.Column := aPos.Char;
    Result.Row := RowCount + (aPos.Line - fLineCount);
    Exit;
  end;
  if aPos.Line = 1 then
    vStartRow := 0
  else
    vStartRow := fLineOffsets[aPos.Line - 2];
  vRowLen := 0;
  for cRow := vStartRow to fLineOffsets[aPos.Line - 1] - 1 do
  begin
    Inc(vRowLen, fRowLengths[cRow]);
    if aPos.Char <= vRowLen then
    begin
      Result.Column := aPos.Char - vRowLen + fRowLengths[cRow];
      Result.Row := cRow + 1;
      Exit;
    end;
  end;
  // beyond EOL
  Result.Column := aPos.Char - vRowLen + fRowLengths[fLineOffsets[aPos.Line - 1] - 1];
  Result.Row := fLineOffsets[aPos.Line - 1];
end;

constructor TSynWordWrapPlugin.Create(aOwner: TCustomSynEdit);
begin
  inherited Create; // just to work as reminder in case I revert it to a TComponent...
  if aOwner = nil then
    raise Exception.Create( 'Owner of TSynWordWrapPlugin must be a TCustomSynEdit' );
  fEditor := aOwner;
  fLineCount := fEditor.Lines.Count;
  fLineOffsets := TList<Integer>.Create;
  fRowLengths := TList<Integer>.Create;
  Reset;
end;

destructor TSynWordWrapPlugin.Destroy;
begin
  inherited;
  fLineOffsets.Free;;
  fRowLengths.Free;
end;

procedure TSynWordWrapPlugin.DisplayChanged;
begin
  if Editor.WrapAreaWidth <> fMaxRowWidth then
    Reset;
end;

function TSynWordWrapPlugin.DisplayToBufferPos(
  const aPos: TDisplayCoord): TBufferCoord;
var
  cLine: integer;
  cRow: integer;
begin
  Assert(aPos.Column > 0);
  Assert(aPos.Row > 0);
  if aPos.Row > RowCount then
  begin
    // beyond EOF
    Result.Char := aPos.Column;
    Result.Line := aPos.Row - RowCount + fLineCount;
    Exit;
  end;
  //Optimized loop start point but could use binary search
  for cLine := Min(aPos.Row - 2, fLineCount - 2) downto 0 do
    if aPos.Row > fLineOffsets[cLine] then
    begin
      Result.Line := cLine + 2;
      if aPos.Row = fLineOffsets[cLine + 1] then //last row of line
        Result.Char := aPos.Column
      else
        Result.Char := Min(aPos.Column, fRowLengths[aPos.Row - 1] + 1);
      for cRow := fLineOffsets[cLine] to aPos.Row - 2 do
        Inc(Result.Char, fRowLengths[cRow]);
      Exit;
    end;
  // first line
  Result.Line := 1;
  if aPos.Row = fLineOffsets[0] then //last row of line
    Result.Char := aPos.Column
  else
    Result.Char := Min(aPos.Column, fRowLengths[aPos.Row - 1] + 1);
  for cRow := 0 to aPos.Row - 2 do
    Inc(Result.Char, fRowLengths[cRow]);
end;

function TSynWordWrapPlugin.GetRowLength(aRow: integer): integer;
// aRow is 1-based...
begin
  if (aRow <= 0) or (aRow > RowCount) then
    TList.Error(SListIndexError, aRow);
  Result := fRowLengths[aRow - 1];
end;

function TSynWordWrapPlugin.LinesDeleted(aIndex: integer; aCount: integer): integer;
// Returns the number of rows deleted
var
  vStartRow: integer;
  vEndRow: integer;
  cLine: integer;
begin
  if fMaxRowWidth < Editor.CharWidth then Exit(0);
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex + aCount <= fLineCount);

  if aIndex = 0 then
    vStartRow := 0
  else
    vStartRow := fLineOffsets[aIndex - 1];
  vEndRow := fLineOffsets[aIndex + aCount - 1];
  Result := vEndRow - vStartRow;
  // resize fRowLengths
  if vStartRow < RowCount then
    fRowLengths.DeleteRange(vStartRow, Result);
  // resize fLineOffsets
  fLineOffsets.DeleteRange(aIndex, aCount);
  Dec(fLineCount, aCount);
  // update offsets
  for cLine := aIndex to fLineCount - 1 do
    Dec(fLineOffsets.List[cLine], Result);
  if fLineCount = 0 then
    SetEmpty;
end;

function TSynWordWrapPlugin.LinesInserted(aIndex: integer; aCount: integer): integer;
// Returns the number of rows inserted
var
  vPrevOffset: TRowIndex;
  cLine: integer;
  TempArray: TArray<Integer>;
begin
  if fMaxRowWidth < Editor.CharWidth then Exit(0);
  Assert(aIndex >= 0);
  Assert(aCount >= 1);
  Assert(aIndex <= fLineCount);
  Inc(fLineCount, aCount);
  // set offset to same as previous line
  if aIndex = 0 then
    vPrevOffset := 0
  else
    vPrevOffset := fLineOffsets[aIndex - 1];
  // resize fLineOffsets
  SetLength(TempArray, aCount);
  fLineOffsets.InsertRange(aIndex, TempArray);
  // Rewrap
  Result := 0;
  for cLine := aIndex to aIndex + aCount - 1 do
  begin
    fLineOffsets[cLine] := vPrevOffset;
    ReWrapLine(cLine);
    Inc(Result, fLineOffsets[cLine] - vPrevOffset);
    vPrevOffset := fLineOffsets[cLine];
  end;
  // Adjust lines below
  for cLine := aIndex + aCount to fLineCount - 1 do
    Inc(fLineOffsets.List[cLine], Result);
end;

function TSynWordWrapPlugin.LinePut(aIndex: integer; const OldLine: string): integer;
var
  cLine: integer;
begin
  if fMaxRowWidth < Editor.CharWidth then Exit(0);
  Assert(aIndex >= 0);
  Assert(aIndex < fLineCount);
  // Rewrap
  Result := ReWrapLine(aIndex);
  // Adjust lines below
  if Result <> 0 then
    for cLine := aIndex + 1 to fLineCount - 1 do
      Inc(fLineOffsets.List[cLine], Result);
end;

procedure TSynWordWrapPlugin.Reset;
begin
  fMaxRowWidth := Editor.WrapAreaWidth;

  WrapLines;
end;

function TSynWordWrapPlugin.ReWrapLine(aIndex: TLineIndex): integer;
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
    PrevOffset := fLineOffsets[aIndex - 1];
  PrevRowCount := fLineOffsets[aIndex] - PrevOffset;
  if PrevRowCount > 0 then
    fRowLengths.DeleteRange(PrevOffset, PrevRowCount);
  fRowLengths.InsertRange(PrevOffset, RowLengths);
  fLineOffsets[aIndex] := PrevOffset + Length(RowLengths);
  Result := Length(RowLengths) - PrevRowCount;
end;

procedure TSynWordWrapPlugin.WrapLines;
var
  cRow: Integer;
  cLine: Integer;
  RowLengths: TArray<Integer>;
begin
  fLineOffsets.Clear;
  fLineOffsets.Capacity := Editor.Lines.Count;
  fRowLengths.Clear;
  fRowLengths.Capacity := Editor.Lines.Count;

  if (Editor.Lines.Count = 0) or (fMaxRowWidth < Editor.CharWidth) then
    Exit;

  cRow := 0;
  for cLine := 0 to Editor.Lines.Count - 1 do
  begin
    WrapLine(cLine, RowLengths);
    fRowLengths.AddRange(RowLengths);
    Inc(cRow, Length(RowLengths));
    fLineOffsets.Add(cRow);
  end;
end;

function TSynWordWrapPlugin.RowCount: integer;
begin
  if fLineCount > 0 then
    Result := fLineOffsets[fLineCount - 1]
  else
    Result := 0;
  Assert(fRowLengths.Count = Result);
end;

procedure TSynWordWrapPlugin.SetEmpty;
begin
  fLineCount := 0;
  // free unsused memory
  TrimArrays;
end;

procedure TSynWordWrapPlugin.TrimArrays;
begin
  fLineOffsets.TrimExcess;
  fRowLengths.TrimExcess;
end;

function TSynWordWrapPlugin.PrepareLine(const S: string): string;
{
  Serves two purposes
  - Take account of WordBreakChars defined in Editor or Highlighter
  - DWrite ignores white space at the end of the line and this is a trick
    around this.
}
var
  I: Integer;
begin
  Result := S;
  for I := 1 to S.Length do
  begin
    if not (Word(Result[I]) in [9, 32]) and Editor.IsWordBreakChar(Result[I]) then
      Result[I] := '-';
    if (Result[I] = ' ') and (I > 1) and (Word(Result[I - 1]) in [9, 32]) then
      Result[I] := '-';
  end;
end;

procedure TSynWordWrapPlugin.WrapLine(const Index: Integer;
  out RowLengths: TArray<Integer>);
var
  SLine: string;
  Layout: TSynTextLayout;
  LineMetrics: array of TDwriteLineMetrics;
  NRows: Cardinal;
  I: Integer;
begin
  SLine := PrepareLine(Editor.Lines[Index]);
  if SLine.Length * Editor.CharWidth < fMaxRowWidth div 2 then
  begin
    // Optimization.  Assume line will fit!
    NRows := 1;
    SetLength(RowLengths, 1);
    RowLengths[0] := SLine.Length;
  end
  else
  begin
    SetLength(LineMetrics, 1);
    // Editor.TextMargin is subtracted from Layout width to allow space for
    // cursor, text overhangs etc.
    Layout.Create(Editor.TextFormat, PChar(SLine), SLine.Length,
      fMaxRowWidth,  MaxInt, True);
    Layout.IDW.GetLineMetrics(@LineMetrics[0], Length(LineMetrics), NRows);
    if Integer(NRows) > Length(LineMetrics) then
    begin
      SetLength(LineMetrics, NRows);
      Layout.IDW.GetLineMetrics(@LineMetrics[0], Length(LineMetrics), NRows);
    end;
    SetLength(RowLengths, NRows);
    for I := 0 to NRows - 1 do
      RowLengths[I] := LineMetrics[I].length;
  end;
end;

end.
