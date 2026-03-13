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
  ------------------------------------------------------------------------------- }
unit SynEditCodeFolding;
{
   Introduction
   ============
   This unit adds code folding support for SynEdit.
   It blends well with the Synedit highligting infrastructure and provides
   fast and efficient code folding that can cope with files with tens of
   thousands of line without lags.

   Converting existing highlighters
   ================================

   To support code folding a Highlighter must inherit from
   TSynCustomCodeFoldingHighlighter and implement one abstact procedure
   ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: TSynNativeInt; ToLine: TSynNativeInt);
   For each line ScanForFoldRanges needs to call one of the following:
      FoldRanges.StartFoldRange
      FoldRanges.StopFoldRange
      FoldRanges.NoFoldInfo
   ScanForFoldRanges is called after the standard highlighter scanning has taken
   place so one can use the Range information stored inside LinesToScan, which
   is a TSynEditStringList, to avoid duplicating effort.

   Initally two highlighters have been converted SynHighlighterJScript and
   SynHighlighterPython, to serve as examples of adding code folding suppot to
   brace-based and indentation-based languagges.

   Alternatively, code folding support can be provided just by implementing
   the SynEdit OnScanForFoldRangesEvent event.

   Demo of Coding Folding
   ======================
   A Folding demo has been added that demonstrates the use of the JScript and
   Python highlighters as well as the use of the OnScanForFoldRangesEvent event
   to support code folding in C++ files.

   Synedit Commants and Shortcuts
   =========
   The following commands have been added:
     ecFoldAll, ecUnfoldAll, ecFoldNearest, ecUnfoldNearest, ecFoldLevel1,
     ecFoldLevel2, ecFoldLevel3,, ecUnfoldLevel1, ecUnfoldLevel2,
     ecUnfoldLevel3, ecFoldRegions

    The default customisable shortcuts are:
      AddKey(ecFoldAll, VK_OEM_MINUS, [ssCtrl, ssShift]);   //- _
      AddKey(ecUnfoldAll,  VK_OEM_PLUS, [ssCtrl, ssShift]); //= +
      AddKey(ecFoldNearest, VK_OEM_2, [ssCtrl]);  // Divide //'/'
      AddKey(ecUnfoldNearest, VK_OEM_2, [ssCtrl, ssShift]);
      AddKey(ecFoldLevel1, ord('K'), [ssCtrl], Ord('1'), [ssCtrl]);
      AddKey(ecFoldLevel2, ord('K'), [ssCtrl], Ord('2'), [ssCtrl]);
      AddKey(ecFoldLevel3, ord('K'), [ssCtrl], Ord('3'), [ssCtrl]);
      AddKey(ecUnfoldLevel1, ord('K'), [ssCtrl, ssShift], Ord('1'), [ssCtrl, ssShift]);
      AddKey(ecUnfoldLevel2, ord('K'), [ssCtrl, ssShift], Ord('2'), [ssCtrl, ssShift]);
      AddKey(ecUnfoldLevel3, ord('K'), [ssCtrl, ssShift], Ord('3'), [ssCtrl, ssShift]);

   Limitations
   ===========
   -  Code folding can not be used simultaneously with Wordwrap.  Synedit takes
      care of that.
   -  The code uses generic collections, so it cannot be used with Delphi
      versions prior to Delphi 2009.

   Improvements
   ============
   Although the code folding infrastructure is fairly complete, improvements
   can be made in providing the use with more painting options
   (folding hints etc.)

   Technical details
   =================
   The main code folding structure is TSynFoldRanges.  It contains a public
   TList<TSynFoldRange> (sorted by starting line numbers).  This list is used by
   Synedit to paint the gutter and lines, fold and unfold ranges etc.
   Internally, TSynFoldRange maintains a TList<TLineFoldInfo> that is modified
   during scanning.  The TList<TSynFoldRange> is reconstructed from the
   TList<TLineFoldInfo> only when it is necessary.

}
interface

uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  Winapi.Windows,
  Vcl.Graphics,
  SynEditHighlighter,
  SynFunc;

type
  // Custom COde Folding Exception
  TSynCodeFoldingException = class(Exception)
  end;

  // A single fold
  // Important: FromLine, ToLine are 1-based
  TSynFoldRange = record
    FromLine: TSynNativeInt; // Beginning line
    ToLine: TSynNativeInt; // End line
    FoldType: TSynNativeInt;  // Could be used by some complex highlighters
    Indent: TSynNativeInt;   // Only used for Indent based folding (Python)
  private
    FCollapsed: Boolean; // Is collapsed?
    // The following private variables are used for efficienct
    // conversion from lines to rows and vice versa.
    // They are updated in AdjustRangeRows
    FFromRow: TSynNativeInt; // starting row
    FCollapsedIndex: TSynNativeInt; // Index of enclosing collapsed range
  public
    procedure Move(Count: TSynNativeInt);
    property Collapsed: Boolean read FCollapsed;
    constructor Create(AFromLine: TSynNativeInt; AToLine: TSynNativeInt = -1; AFoldType: TSynNativeInt = 1; AIndent: TSynNativeInt = -1; ACollapsed: Boolean = False);
  end;

  PSynFoldRange = ^TSynFoldRange;

  {Support for indendation based code folding as in Python, F#}
  TSynCodeFoldingMode = (cfmStandard, cfmIndentation);

  TSynFoldRanges = class;
  TSynAdjustRangesProc = procedure(FoldRanges: TSynFoldRanges;
    LinesToScan: TStrings) of object;

  TSynFoldRanges = class(TObject)
  {
    The main code folding data structure.
    Scanning affects the fFoldInfoList data structure
    SynEdit Painting is based on the fRanges structure
    fRanges gets updated from fFoldInfoList when needed
    Both fRanges and fFoldInfoList are kept sorted by FromLine
    Line indices in both fRanges and fFoldInfoList are 1-based
  }
  private
    type
      TFoldOpenClose = (focOpen, focClose, focCloseOpen);

      TLineFoldInfo = record
        Line: TSynNativeInt;
        FoldOpenClose: TFoldOpenClose;
        FoldType: TSynNativeInt;
        Indent: TSynNativeInt;
        constructor Create(ALine: TSynNativeInt;
          AFoldOpenClose: TFoldOpenClose = focOpen;
          AFoldType: TSynNativeInt = 1; AIndent: TSynNativeInt = -1);
      end;
  private
    fCodeFoldingMode: TSynCodeFoldingMode;
    fRangesNeedFixing: Boolean;
    fRanges: TList<TSynFoldRange>;
    fCollapsedState: TList<TSynNativeInt>;
    fFoldInfoList: TList<TLineFoldInfo>;
    FRowComparer: IComparer<TSynFoldRange>;
    FAdjustRangesProc: TSynAdjustRangesProc;
    function Get(Index: TSynNativeInt): TSynFoldRange;
    function GetCount: TSynNativeInt;
    procedure RecreateFoldRanges(Lines: TStrings);
    procedure AdjustRangeRows;
  public
    constructor Create;
    destructor Destroy; override;

    {utility routines}
    procedure CollapseAll;
    function Collapse(RangeIndex: TSynNativeInt): Boolean;
    procedure CollapseLevel(Level: TSynNativeInt);
    procedure CollapseFoldType(FoldType: TSynNativeInt);
    procedure UnCollapseAll;
    procedure UnCollapse(RangeIndex: TSynNativeInt);
    procedure UnCollapseLevel(Level: TSynNativeInt);
    procedure UnCollapseFoldType(FoldType: TSynNativeInt);
    function FoldStartAtLine(Line: TSynNativeInt): Boolean; overload;
    function FoldStartAtLine(Line: TSynNativeInt; out Index: TSynNativeInt): Boolean; overload;
    function CollapsedFoldStartAtLine(Line: TSynNativeInt): Boolean; overload;
    function CollapsedFoldStartAtLine(Line: TSynNativeInt; out Index: TSynNativeInt): Boolean; overload;
    function FoldEndAtLine(Line: TSynNativeInt): Boolean; overload;
    function FoldEndAtLine(Line: TSynNativeInt; out Index: TSynNativeInt): Boolean; overload;
    function FoldAroundLineEx(Line: TSynNativeInt; WantCollapsed, AcceptFromLine,
      AcceptToLine: Boolean; out Index: TSynNativeInt): Boolean;
    function FoldAroundLine(Line: TSynNativeInt): Boolean; overload;
    function FoldAroundLine(Line: TSynNativeInt; out Index: TSynNativeInt): Boolean; overload;
    function FoldHidesLine(Line: TSynNativeInt): Boolean; overload;
    function FoldHidesLine(Line: TSynNativeInt; out Index: TSynNativeInt): Boolean; overload;
    function FoldsAtLevel(Level: TSynNativeInt): TArray<TSynNativeInt>;
    function FoldsOfType(aType: TSynNativeInt): TArray<TSynNativeInt>;
    function FoldRangesForTextRange(FromLine, ToLine: TSynNativeInt):
        TArray<TSynFoldRange>;

    {Scanning support}
    procedure StoreCollapsedState; overload;
    procedure RestoreCollapsedState; overload;
    procedure StoreCollapsedState(Stream: TStream); overload;
    procedure RestoreCollapsedState(Stream: TStream); overload;
    procedure StartScanning;
    function  StopScanning(Lines: TStrings): Boolean; // Returns True of Ranges were updated
    procedure AddLineInfo(ALine: TSynNativeInt; AFoldType: TSynNativeInt;
      AFoldOpenClose: TFoldOpenClose;  AIndent: TSynNativeInt);
    procedure StartFoldRange(ALine: TSynNativeInt; AFoldType: TSynNativeInt; AIndent: TSynNativeInt = -1);
    procedure StopFoldRange(ALine: TSynNativeInt; AFoldType: TSynNativeInt; AIndent: TSynNativeInt = -1);
    procedure StopStartFoldRange(ALine: TSynNativeInt; AFoldType: TSynNativeInt; AIndent: TSynNativeInt = -1);
    procedure NoFoldInfo(ALine: TSynNativeInt);
    function  GetIndentLevel(Line: TSynNativeInt): TSynNativeInt;

    // plugin notifications and support routines
    function FoldLineToRow(Line: TSynNativeInt): TSynNativeInt;
    function FoldRowToLine(Row: TSynNativeInt): TSynNativeInt;
    function LinesInserted(aIndex: TSynNativeInt; aCount: TSynNativeInt): TSynNativeInt;
    function LinesDeleted(aIndex: TSynNativeInt; aCount: TSynNativeInt): TSynNativeInt;
    function LinePut(aIndex: TSynNativeInt; const OldLine: string): TSynNativeInt;
    procedure Reset;

    {Access to the internal FoldRange list routines}
    procedure AddByParts(AFoldType: TSynNativeInt; AFromLine: TSynNativeInt; AToLine: TSynNativeInt = -1);
    procedure AddFoldRange(FoldRange: TSynFoldRange);
    property  CodeFoldingMode: TSynCodeFoldingMode
              read fCodeFoldingMode write fCodeFoldingMode;
    property AdjustRangesProc: TSynAdjustRangesProc write fAdjustRangesProc;
    property Count: TSynNativeInt read GetCount;
    property FoldRange[Index: TSynNativeInt]: TSynFoldRange read Get; default;
    property Ranges: TList<TSynFoldRange> read fRanges;
  end;

  TSynCodeFoldingChangeEvent = procedure(Sender: TObject) of object;

  TSynCodeFolding = class(TPersistent)
    { Class to store and expose to the designer Code Folding properties }
  private
    fCollapsedLineColor: TColor;
    fFolderBarLinesColor: TColor;
    fShowCollapsedLine: Boolean;
    fShowHintMark: Boolean;
    fGutterShapeSize:  TSynNativeInt;
    fOnChange: TSynCodeFoldingChangeEvent;
    procedure SetCollapsedLineColor(const Value: TColor);
    procedure SetFolderBarLinesColor(const Value: TColor);
    procedure SetShowCollapsedLine(const Value: Boolean);
    procedure SetShowHintMark(const Value: Boolean);
    procedure SetGutterShapeSize(const Value: TSynNativeInt);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function ScaledGutterShapeSize(PPI: TSynNativeInt): TSynNativeInt;
    property OnChange: TSynCodeFoldingChangeEvent read fOnChange write fOnChange;
  published
    // Size of the gutter shapes in pixels at 96 PPI
    property  GutterShapeSize: TSynNativeInt read fGutterShapeSize
      write SetGutterShapeSize default 9;
    property CollapsedLineColor: TColor read fCollapsedLineColor
      write SetCollapsedLineColor default clGrayText;
    property FolderBarLinesColor: TColor read fFolderBarLinesColor
      write SetFolderBarLinesColor default clGrayText;
    property ShowCollapsedLine: Boolean read fShowCollapsedLine
      write SetShowCollapsedLine default False;
    property ShowHintMark: Boolean read fShowHintMark
      write SetShowHintMark default True;
  end;

  TSynCustomCodeFoldingHighlighter = class(TSynCustomHighlighter)
  protected
    // Utility functions
    function GetLineRange(Lines: TStrings; Line: TSynNativeInt): Pointer;
    function TabWidth(LinesToScan: TStrings): TSynNativeInt;
  public
    // Called when a Highlighter is assigned to Synedit;
    // No need to override except to change the SynCodeFoldingMode
    procedure InitFoldRanges(FoldRanges: TSynFoldRanges); virtual;
    // Called after Highlighter ranges have been set
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: TSynNativeInt; ToLine: TSynNativeInt); virtual; abstract;
    // Called immediately after FoldRanges have been recreated
    // Override only if some finetuning of the FoldRanges is need.
    procedure AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings); virtual;
    class function GetCapabilities: TSynHighlighterCapabilities; override;
  end;

const
  FoldRegionType: TSynNativeInt = 99;

implementation

Uses
  System.Math,
  SynEditTextBuffer;

{ TSynEditFoldRanges }

function TSynFoldRanges.Collapse(RangeIndex: TSynNativeInt): Boolean;
begin
  Result := False;
  if not InRange(RangeIndex, 0, Count - 1) then Exit;

  with fRanges.List[RangeIndex] do
    if not Collapsed and (FromLine < ToLine) then
    begin
      FCollapsed := True;
      AdjustRangeRows;
      Result := True;
    end;
end;

procedure TSynFoldRanges.CollapseAll;
var
  Index: TSynNativeInt;
begin
  for Index := 0 to  Count -1 do
    fRanges.List[Index].FCollapsed := True;
  AdjustRangeRows;
end;

function TSynFoldRanges.CollapsedFoldStartAtLine(Line: TSynNativeInt): Boolean;
var
  Index: TSynNativeInt;
begin
  Result := CollapsedFoldStartAtLine(Line, Index);
end;

function TSynFoldRanges.CollapsedFoldStartAtLine(Line: TSynNativeInt;
  out Index: TSynNativeInt): Boolean;
begin
  Result := fRanges.BinarySearch(TSynFoldRange.Create(Line), Index);
  if Result then
    Result := Result and fRanges[Index].Collapsed;
end;

procedure TSynFoldRanges.CollapseFoldType(FoldType: TSynNativeInt);
var
  I: TSynNativeInt;
begin
  for I := 0 to Count - 1 do
    if fRanges.List[I].FoldType = FoldType then
      fRanges.List[I].FCollapsed := True;

  AdjustRangeRows;
end;

procedure TSynFoldRanges.CollapseLevel(Level: TSynNativeInt);
var
  I: NativeInt;
  RangeIndices: TArray<TSynNativeInt>;
begin
  RangeIndices := FoldsAtLevel(Level);

  for I := Low(RangeIndices) to High(RangeIndices) do
    fRanges.List[RangeIndices[I]].FCollapsed := True;

  AdjustRangeRows;
end;

constructor TSynFoldRanges.Create;
begin
  inherited;
  fCodeFoldingMode := cfmStandard;

  fRanges := TList<TSynFoldRange>.Create(TComparer<TSynFoldRange>.Construct(
    function(const L, R: TSynFoldRange): Integer
    begin
      Result := CompareValue(L.FromLine, R.FromLine);
    end));

  FRowComparer := TComparer<TSynFoldRange>.Construct(
    function(const L, R: TSynFoldRange): Integer
    begin
      Result := CompareValue(L.FFromRow, R.FFromRow);
    end);

  fCollapsedState := TList<TSynNativeInt>.Create;

  fFoldInfoList := TList<TLineFoldInfo>.Create(TComparer<TLineFoldInfo>.Construct(
    function(const L, R: TLineFoldInfo): Integer
    begin
      Result := CompareValue(L.Line, R.Line);
    end));
end;

destructor TSynFoldRanges.Destroy;
begin
  fRanges.Free;
  fCollapsedState.Free;
  fFoldInfoList.Free;
  inherited;
end;

function TSynFoldRanges.FoldAroundLine(Line: TSynNativeInt): Boolean;
var
  Index: TSynNativeInt;
begin
  Result := FoldAroundLine(Line, Index);
end;

function TSynFoldRanges.FoldAroundLine(Line: TSynNativeInt;
  out Index: TSynNativeInt): Boolean;
begin
  Result := FoldAroundLineEx(Line, False, False, False, Index);
end;

function TSynFoldRanges.FoldAroundLineEx(Line: TSynNativeInt;
  WantCollapsed, AcceptFromLine, AcceptToLine: Boolean;
  out Index: TSynNativeInt): Boolean;
var
  I: TSynNativeInt;
  Idx: TSynNativeInt;
begin
  Index := -1;
  Result := False;
  if Count = 0 then Exit;

  // binary search
  if FoldStartAtLine(Line, Idx) and AcceptFromLine then
    with fRanges.List[Idx] do  // used to avoid copying records
    begin
      if WantCollapsed then
      begin
        if Collapsed then
        begin
          Index := Idx;
          Exit(True)
        end
        else if FCollapsedIndex >= 0 then
        begin
          Index := FCollapsedIndex;
          Exit(True);
        end;
      end
      else if not Collapsed then
        Exit(True);
    end;

  Index := -1;
  for I := Idx - 1 downto 0 do
    with fRanges.List[I] do
      begin
        if WantCollapsed then
        begin
          // Will always exit without examining other ranges
          // thanks to the FCollapsedIndex optimization
          if Collapsed and
            ((ToLine > Line) or ((ToLine = Line) and AcceptToLine)) then
          begin
            Index := I;
            Exit(True);
          end
          else if FCollapsedIndex >= 0 then
          begin
            Index := FCollapsedIndex;
            with fRanges.List[FCollapsedIndex] do
              Exit((ToLine > Line) or ((ToLine = Line) and AcceptToLine));
          end
          else
            Exit(False);
        end
        else if not Collapsed and
          ((ToLine > Line) or ((ToLine = Line) and AcceptToLine)) then
        begin
          Index := I;
          Exit(True);
        end;
      end;
end;

function TSynFoldRanges.FoldEndAtLine(Line: TSynNativeInt;
  out Index: TSynNativeInt): Boolean;
var
  i: TSynNativeInt;
begin
  Result := False;
  for i := 0 to fRanges.Count - 1 do
    with fRanges.List[i] do
      if (ToLine = Line) then
      begin
        Index := i;
        Result := True;
        Break;
      end
      else if FromLine > Line then
        Break; // sorted by line. don't bother scanning further
end;

function TSynFoldRanges.FoldEndAtLine(Line: TSynNativeInt): Boolean;
var
  Index: TSynNativeInt;
begin
   Result := FoldEndAtLine(Line, Index);
end;

function TSynFoldRanges.FoldHidesLine(Line: TSynNativeInt): Boolean;
var
  Index: TSynNativeInt;
begin
  Result := FoldHidesLine(Line, Index);
end;

function TSynFoldRanges.FoldHidesLine(Line: TSynNativeInt;
  out Index: TSynNativeInt): Boolean;
begin
  Result := FoldAroundLineEx(Line, True, False, True, Index);
end;

function TSynFoldRanges.FoldLineToRow(Line: TSynNativeInt): TSynNativeInt;
var
  Index: TSynNativeInt;
  CollapsedLines: TSynNativeInt;
  Range: TSynFoldRange;
begin
  if Count = 0 then Exit(Line);

  // binary search
  if FoldStartAtLine(Line, Index) then
    Exit(fRanges.List[Index].FFromRow);

  if Index = 0 then Exit(Line);  // Line before first fold range

  // Index > 0 - Check previous range
  Range := fRanges[Index - 1];
  if Range.FCollapsedIndex >= 0 then
    Range := fRanges[Range.FCollapsedIndex];
  CollapsedLines := Range.FromLine - Range.FFromRow;
  if Range.Collapsed then
  begin
    Inc(CollapsedLines, Range.ToLine - Range.FromLine);
    if Line <= Range.ToLine then Exit(Range.FFromRow);
  end;
  Result := Line - CollapsedLines;
end;

function TSynFoldRanges.FoldRangesForTextRange(FromLine,
  ToLine: TSynNativeInt): TArray<TSynFoldRange>;
// Used for structure highlight
var
  Range: TSynFoldRange;
begin
  Result := [];
  for Range in fRanges do
  begin
    if Range.ToLine < FromLine then Continue;
    if Range.FromLine > ToLine then Break;
    // Only add if indent > 0
    if not Range.Collapsed and (Range.Indent > 0) then
      Result := Result + [Range];
  end;
end;

function TSynFoldRanges.FoldRowToLine(Row: TSynNativeInt): TSynNativeInt;
var
  Index: TSynNativeInt;
  Range: TSynFoldRange;
begin
  if Count = 0 then Exit(Row);

  // binary search
  if fRanges.BinarySearch(TSynFoldRange.Create(Row), Index, FRowComparer) then
  begin
    // deal with duplicates return the top row
    while (Index > 0) and (fRanges.List[Index - 1].FFromRow = Row) do
      Dec(Index);
    Exit(fRanges.List[Index].FromLine);
  end;

  if Index = 0 then Exit(Row); // Row before first fold

  // Index > 0 - Check previous range
  Range := fRanges[Index - 1];
  if Range.FCollapsedIndex >= 0 then
    Range := fRanges[Range.FCollapsedIndex];
  if Range.Collapsed then
    Result := Range.ToLine + Row - Range.FFromRow
  else
    Result := Range.FromLine + Row - Range.FFromRow;
end;

function TSynFoldRanges.FoldsAtLevel(Level: TSynNativeInt): TArray<TSynNativeInt>;
{
   Returns an array of indices of folds with level = Level
   ignoring fold ranges of type FoldRegionType
}
var
  I: TSynNativeInt;
  FRStack: TList<TSynNativeInt>;
  ResultList: TList<TSynNativeInt>;

   procedure RemoveClosed(Line: TSynNativeInt);
   var
     J: TSynNativeInt;
   begin
     for J := FRStack.Count-1 downto 0 do
       if fRanges.List[FRStack[J]].ToLine <= Line then
         FRStack.Delete(J);
   end;
begin
  FRStack := TList<TSynNativeInt>.Create;
  ResultList := TList<TSynNativeInt>.Create;
  try
    for I := 0 to fRanges.Count - 1 do
    begin
      if fRanges.List[I].FoldType = FoldRegionType then
        Continue;
      RemoveClosed(fRanges.List[I].FromLine);
      FRStack.Add(I);
      if FRStack.Count = Level then
        ResultList.Add(I);
    end;
    Result := ResultList.ToArray;
  finally
    FRStack.Free;
    ResultList.Free;
  end;
end;

function TSynFoldRanges.FoldsOfType(aType: TSynNativeInt): TArray<TSynNativeInt>;
{
   Returns an array of indices of folds with FoldType = aType
}
var
  I: TSynNativeInt;
  ResultList: TList<TSynNativeInt>;
begin
  ResultList := TList<TSynNativeInt>.Create;
  try
    for I := 0 to fRanges.Count - 1 do
    begin
      if fRanges.List[I].FoldType = aType then
        ResultList.Add(I);
    end;
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TSynFoldRanges.FoldStartAtLine(Line: TSynNativeInt): Boolean;
var
  Index: TSynNativeInt;
begin
  Result := FoldStartAtLine(Line, Index);
end;

function TSynFoldRanges.FoldStartAtLine(Line: TSynNativeInt; out Index: TSynNativeInt): Boolean;
{
  If Result is False it Returns the First Index with Line greater than Line
}
begin
  Result := fRanges.BinarySearch(TSynFoldRange.Create(Line), Index);
end;

procedure TSynFoldRanges.AddByParts(AFoldType: TSynNativeInt; AFromLine: TSynNativeInt;
  AToLine: TSynNativeInt);
var
  Index: TSynNativeInt;
  FR: TSynFoldRange;
begin
  // Insert keeping the list sorted
  FR := TSynFoldRange.Create(AFromLine, AToLine, AFoldType);
  if FoldStartAtLine(AFromLine, Index) then
    fRanges.List[Index] := FR
  else
    fRanges.Insert(Index, FR);
end;

procedure TSynFoldRanges.AddFoldRange(FoldRange: TSynFoldRange);
begin
  fRanges.Add(FoldRange);
end;

procedure TSynFoldRanges.AddLineInfo(ALine: TSynNativeInt; AFoldType: TSynNativeInt;
  AFoldOpenClose: TFoldOpenClose; AIndent: TSynNativeInt);
var
  LineFoldInfo: TLineFoldInfo;
  Index: TSynNativeInt;
begin
  LineFoldInfo := TLineFoldInfo.Create(ALine, AFoldOpenClose, AFoldType, AIndent);

  // Insert keeping the list sorted
  if fFoldInfoList.BinarySearch(LineFoldInfo, Index) then
  begin
    if not CompareMem(@LineFoldInfo, @fFoldInfoList.List[Index],
      SizeOf(TLineFoldInfo))
    then
    begin
      fFoldInfoList.List[Index] := LineFoldInfo;
      fRangesNeedFixing := True;
    end;
  end
  else begin
    fFoldInfoList.Insert(Index, LineFoldInfo);
    fRangesNeedFixing := True;
  end;
end;

procedure TSynFoldRanges.AdjustRangeRows;
var
  I: TSynNativeInt;
  CollapsedTo: TSynNativeInt;
  CollapsedCount: TSynNativeInt;
  CollapsedFromRow: TSynNativeInt;
  CollapsedIndex: TSynNativeInt;
begin
  if fRanges.Count = 0 then Exit;

  CollapsedTo := 0;
  CollapsedCount := 0;
  CollapsedFromRow := MaxInt;
  CollapsedIndex := -1;

  for I := 0 to fRanges.Count - 1 do
    with fRanges.List[I] do
    begin
      FFromRow := FromLine;
      FCollapsedIndex := -1;

      if  FromLine < CollapsedTo then
      begin
        FFromRow := CollapsedFromRow;
        FCollapsedIndex := CollapsedIndex;
      end
      else
        Dec(FFromRow, CollapsedCount);

      if Collapsed then
      begin
        if CollapsedTo < FromLine then
        begin
          Inc(CollapsedCount, ToLine - FromLine);
          CollapsedTo := ToLine;
          CollapsedFromRow := FFromRow;
          FCollapsedIndex := -1;
          CollapsedIndex := I;
        end
        else if CollapsedTo < ToLine then
        begin
          Inc(CollapsedCount, ToLine - CollapsedTo);
          CollapsedTo := ToLine;
          CollapsedFromRow := Min(FFromRow, CollapsedFromRow);
          FCollapsedIndex := -1;
          CollapsedIndex := I;
        end;
      end;
    end;
end;

function TSynFoldRanges.Get(Index: TSynNativeInt): TSynFoldRange;
begin
  Result := TSynFoldRange(fRanges[Index]);
end;

function TSynFoldRanges.GetCount: TSynNativeInt;
begin
  Result := fRanges.Count;
end;


function TSynFoldRanges.GetIndentLevel(Line: TSynNativeInt): TSynNativeInt;
var
  Index: TSynNativeInt;
  I: TSynNativeInt;
begin
  Result := -1;
  fFoldInfoList.BinarySearch(TLineFoldInfo.Create(Line), Index);
  // Search above Line
  for I := Index - 1 downto 0 do
    if fFoldInfoList.List[I].Indent >= 0 then begin
      Result := fFoldInfoList.List[I].Indent;
      Break
    end;
end;

function TSynFoldRanges.LinesDeleted(aIndex, aCount: TSynNativeInt): TSynNativeInt;
{
  Adjust fFoldInfoList and fRanges
  aIndex is 0-based fFoldInfoList and fRanges are 1-based
  If needed recreate fRanges
}
var
  I: TSynNativeInt;
begin
  fRangesNeedFixing := False;

  Result := aCount;
  // Adjust fFoldInfoList
  // aIndex is 0-based fFoldInfoList is 1-based
  for I := fFoldInfoList.Count - 1 downto 0 do
    with fFoldInfoList.List[I] do
      if Line > aIndex + aCount then
         Dec(fFoldInfoList.List[I].Line, aCount)
      else if Line > aIndex then begin
        fRangesNeedFixing := True;
        fFoldInfoList.Delete(I);
      end else
         Break;

  if fRangesNeedFixing then Exit;

  for I := fRanges.Count - 1 downto 0 do
    with fRanges.List[I] do
      if (FromLine > aIndex + aCount) then
        // Move after affected area
        Ranges.List[I].Move(-aCount)
      else if FromLine > aIndex then
      begin
        fRangesNeedFixing := True;
        fRanges.Delete(I);
      end else if ToLine > aIndex + aCount then
        Dec(fRanges.List[I].ToLine, aCount)
      else if ToLine > aIndex then
        Dec(fRanges.List[I].ToLine, ToLine - aIndex)
end;

function TSynFoldRanges.LinesInserted(aIndex, aCount: TSynNativeInt): TSynNativeInt;
{
  Adjust fFoldInfoList and fRanges
  aIndex is 0-based fFoldInfoList and fRanges are 1-based
}
var
  I: TSynNativeInt;
begin
  Result := aCount;
  for I := fFoldInfoList.Count - 1 downto 0 do
    with fFoldInfoList.List[I] do
      if Line > aIndex then
         Inc(fFoldInfoList.List[I].Line, aCount)
      else
         Break;

  for I := fRanges.Count - 1 downto 0 do
    with fRanges.List[I] do
    begin
      if (FromLine > aIndex) then // insertion of count lines above FromLine
        fRanges.List[I].Move(aCount)
      else if (ToLine > aIndex) then
        Inc(fRanges.List[I].ToLine,  aCount);
    end;
end;

function TSynFoldRanges.LinePut(aIndex: TSynNativeInt; const OldLine: string): TSynNativeInt;
begin
   Result := 1;
end;

procedure TSynFoldRanges.NoFoldInfo(ALine: TSynNativeInt);
var
  Index: TSynNativeInt;
begin
  if fFoldInfoList.BinarySearch(TLineFoldInfo.Create(ALine), Index)
  then
  begin
    // we have deleted an existing fold open or close mark
    fRangesNeedFixing := True;
    fFoldInfoList.Delete(Index);
  end
  else if (Index < fFoldInfoList.Count) and
    (fFoldInfoList.List[Index].Line = ALine + 1) and
    (fFoldInfoList.List[Index].FoldOpenClose = focCloseOpen)
  then
    fRangesNeedFixing := True;
end;

procedure TSynFoldRanges.RecreateFoldRanges(Lines: TStrings);
var
  OpenFoldStack: TList<TSynNativeInt>;
  LFI: TLineFoldInfo;
  PFoldRange: PSynFoldRange;
  I: TSynNativeInt;
  Line: TSynNativeInt;
begin
   { TODO: Account for type }
  fRanges.Clear;

  OpenFoldStack := TList<TSynNativeInt>.Create;
  try
    for LFI in fFoldInfoList do
    begin
      if LFI.FoldOpenClose in [focClose, focCloseOpen] then
      begin
        if LFI.Indent >= 0 then begin
          for I := OpenFoldStack.Count - 1 downto 0 do
          begin
            // Close all Fold Ranges with greater Indent
            PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
            if (PFoldRange^.Indent >= LFI.Indent) then begin
              PFoldRange^.ToLine := LFI.Line - 1; // Do not include Line
              OpenFoldStack.Delete(I);
              if PFoldRange^.FromLine = PFoldRange^.ToLine then
                fRanges.Remove(PFoldRange^);
            end;
          end;
        end
        else
          for I := OpenFoldStack.Count - 1 downto 0 do
          begin
            PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
            if (PFoldRange^.FoldType = LFI.FoldType) then begin
              PFoldRange^.ToLine := IfThen(LFI.FoldOpenClose = focClose,
                LFI.Line, LFI.Line - 1);
              OpenFoldStack.Delete(I);
              Break;
            end;
          end;
      end;

      if LFI.FoldOpenClose in [focOpen, focCloseOpen] then
      begin
        if LFI.Indent >= 0 then begin
          for I := OpenFoldStack.Count - 1 downto 0 do
          begin
            // Close all Fold Ranges with less Indent
            PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
            if (PFoldRange^.Indent >= LFI.Indent) then begin
              PFoldRange^.ToLine := LFI.Line - 1; // Do not include Line
              OpenFoldStack.Delete(I);
              if PFoldRange^.FromLine = PFoldRange^.ToLine then
                fRanges.Remove(PFoldRange^);
            end;
          end;
        end;
        fRanges.Add(TSynFoldRange.Create(LFI.Line, LFI.Line, LFI.FoldType, LFI.Indent));
        OpenFoldStack.Add(FRanges.Count -1);
      end;
    end;

    if CodeFoldingMode = cfmIndentation then
    begin
      // close all open indent based folds
      for I := OpenFoldStack.Count - 1 downto 0 do
      begin
        // Close all Fold Ranges with less Indent
        PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
        if (PFoldRange^.Indent >= 0) then begin
          PFoldRange^.ToLine := Lines.Count; //
          OpenFoldStack.Delete(I);
        end;
      end;
      // Adjust LineTo for Indent based folds with empty lines in the end
      for I := fRanges.Count - 1 downto 0 do begin
        PFoldRange := @fRanges.List[I];
        if PFoldRange^.Indent >= 0 then
        begin
          Line := PFoldRange^.ToLine;
          while (Line > PFoldRange^.FromLine) and (TrimLeft(Lines.ItemsNative[Line - 1]) = '') do
          begin
            Dec(PFoldRange^.ToLine);
            Dec(Line);
          end;
          if PFoldRange^.FromLine = PFoldRange^.ToLine then
            fRanges.Delete(I);
        end;
      end;
    end;
  finally
    OpenFoldStack.Free;
  end;
end;

procedure TSynFoldRanges.Reset;
begin
  fRanges.Clear;
  fCollapsedState.Clear;
  fFoldInfoList.Clear;
  fRangesNeedFixing := False;
end;

procedure TSynFoldRanges.RestoreCollapsedState(Stream: TStream);
var
  Size: Int64;
  Line: TSynNativeInt;
  Index: TSynNativeInt;
begin
  Size := Stream.Size;
  while Stream.Position < Size do begin
    Stream.ReadData(Line);
    if FoldStartAtLine(Line, Index) then
      fRanges.List[Index].FCollapsed := True;
  end;

  AdjustRangeRows;
end;

procedure TSynFoldRanges.RestoreCollapsedState;
var
  I: TSynNativeInt;
  Index: TSynNativeInt;
begin
  for I in fCollapsedState do begin
    if FoldStartAtLine(I, Index) then
      fRanges.List[Index].FCollapsed := True;
  end;
  fCollapsedState.Clear;

  AdjustRangeRows;
end;

procedure TSynFoldRanges.StartFoldRange(ALine: TSynNativeInt; AFoldType: TSynNativeInt; AIndent: TSynNativeInt = -1);
begin
  AddLineInfo(ALine, AFoldType, focOpen, AIndent);
end;

procedure TSynFoldRanges.StartScanning;
begin
end;

procedure TSynFoldRanges.StopFoldRange(ALine: TSynNativeInt; AFoldType: TSynNativeInt; AIndent: TSynNativeInt = -1);
begin
  AddLineInfo(ALine, AFoldType, focClose, AIndent);
end;

procedure TSynFoldRanges.StopStartFoldRange(ALine: TSynNativeInt; AFoldType: TSynNativeInt; AIndent: TSynNativeInt = -1);
begin
  AddLineInfo(ALine, AFoldType, focCloseOpen, AIndent);
end;

function TSynFoldRanges.StopScanning(Lines: TStrings): Boolean;
{
  Returns True if fold ranges changed
  Recreates FoldRanges if needed
}
begin
  Result := fRangesNeedFixing;

  if Result then begin
    StoreCollapsedState;
    RecreateFoldRanges(Lines);
    // Adjustment of ranges by highlighters
    if Assigned(FAdjustRangesProc) then
      FAdjustRangesProc(Self, Lines);
    // RestoreCollapsedState calls AdjustRangeRows
    // which stores Row numbers for quick access
    RestoreCollapsedState;
    fRangesNeedFixing := False;
  end;
end;

procedure TSynFoldRanges.StoreCollapsedState(Stream: TStream);
var
  FoldRange: TSynFoldRange;
begin
  for FoldRange in fRanges do
    if FoldRange.Collapsed then
       Stream.WriteData(FoldRange.FromLine);
end;

procedure TSynFoldRanges.UnCollapse(RangeIndex: TSynNativeInt);
begin
  if not InRange(RangeIndex, 0, Count - 1) then Exit;

  if fRanges.List[RangeIndex].Collapsed then
  begin
    fRanges.List[RangeIndex].FCollapsed := False;
    AdjustRangeRows;
  end;
end;

procedure TSynFoldRanges.UnCollapseAll;
var
  Index: TSynNativeInt;
begin
  for Index := 0 to  Count -1 do
    fRanges.List[Index].FCollapsed := False;
  AdjustRangeRows;
end;

procedure TSynFoldRanges.UnCollapseFoldType(FoldType: TSynNativeInt);
var
  I: TSynNativeInt;
begin
  for I := 0 to Count - 1 do
    if fRanges.List[I].FoldType = FoldType then
      fRanges.List[I].FCollapsed := False;

  AdjustRangeRows;
end;

procedure TSynFoldRanges.UnCollapseLevel(Level: TSynNativeInt);
var
  I: NativeInt;
  RangeIndices: TArray<TSynNativeInt>;
begin
  RangeIndices := FoldsAtLevel(Level);

  for I := Low(RangeIndices) to High(RangeIndices) do
    fRanges.List[RangeIndices[I]].FCollapsed := False;

  AdjustRangeRows;
end;

procedure TSynFoldRanges.StoreCollapsedState;
var
  FoldRange: TSynFoldRange;
begin
  fCollapsedState.Clear;
  for FoldRange in fRanges do
    if FoldRange.Collapsed then
       fCollapsedState.Add(FoldRange.FromLine);
end;

{ TSynEditFoldRange }

constructor TSynFoldRange.Create(AFromLine: TSynNativeInt; AToLine: TSynNativeInt = -1; AFoldType: TSynNativeInt = 1; AIndent: TSynNativeInt = -1; ACollapsed: Boolean = False);
begin
  FromLine := AFromLine;
  FFromRow := AFromLine;
  ToLine := AToLine;
  FoldType := AFoldType;
  Indent := AIndent;
  FCollapsed := ACollapsed;
end;

procedure TSynFoldRange.Move(Count: TSynNativeInt);
begin
  Inc(FromLine, Count);
  Inc(ToLine, Count);
  Inc(FFromRow, Count);
end;

{ TSynFoldRanges.TLineFoldInfo }

constructor TSynFoldRanges.TLineFoldInfo.Create(ALine: TSynNativeInt;
  AFoldOpenClose: TFoldOpenClose; AFoldType: TSynNativeInt; AIndent: TSynNativeInt);
begin
    Line := ALine;
    FoldOpenClose := AFoldOpenClose;
    FoldType := AFoldType;
    Indent := AIndent;
end;

{ TSynCustomCodeFoldingHighlighter }

procedure TSynCustomCodeFoldingHighlighter.AdjustFoldRanges(
  FoldRanges: TSynFoldRanges; LinesToScan: TStrings);
begin
  // Do nothing
end;

class function TSynCustomCodeFoldingHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcCodeFolding];
end;

function TSynCustomCodeFoldingHighlighter.GetLineRange(Lines: TStrings;
  Line: TSynNativeInt): Pointer;
begin
  if (Line >= 0) and (Line < Lines.Count) then
    Result := TSynEditStringList(Lines).Ranges[Line]
  else
    Result := nil;
end;

procedure TSynCustomCodeFoldingHighlighter.InitFoldRanges(
  FoldRanges: TSynFoldRanges);
begin
  FoldRanges.CodeFoldingMode := cfmStandard;
end;

function TSynCustomCodeFoldingHighlighter.TabWidth(
  LinesToScan: TStrings): TSynNativeInt;
begin
  Result := TSynEditStringList(LinesToScan).TabWidth;
end;

{ TSynCodeFolding }

procedure TSynCodeFolding.Assign(Source: TPersistent);
begin
 if Source is TSynCodeFolding then
 begin
   fCollapsedLineColor := TSynCodeFolding(Source).fCollapsedLineColor;
   fFolderBarLinesColor := TSynCodeFolding(Source).fFolderBarLinesColor;
   fShowCollapsedLine := TSynCodeFolding(Source).fShowCollapsedLine;
   fShowHintMark := TSynCodeFolding(Source).fShowHintMark;
   fGutterShapeSize := TSynCodeFolding(Source).fGutterShapeSize;
   if Assigned(fOnChange) then fOnChange(Self);
 end
 else
   inherited Assign(Source);
end;

constructor TSynCodeFolding.Create;
begin
  fCollapsedLineColor := clGrayText;
  fFolderBarLinesColor := clGrayText;
  fShowCollapsedLine := False;
  fShowHintMark := True;
  fGutterShapeSize := 9;
end;

function TSynCodeFolding.ScaledGutterShapeSize(PPI: TSynNativeInt): TSynNativeInt;
{ Always returns an odd number }
begin
  Result := MulDiv(fGutterShapeSize, PPI, 96);
  if not Odd(Result) then
    Dec(Result);
end;

procedure TSynCodeFolding.SetCollapsedLineColor(const Value: TColor);
begin
  if fCollapsedLineColor <> Value then begin
    fCollapsedLineColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetFolderBarLinesColor(const Value: TColor);
begin
  if fFolderBarLinesColor <> Value then begin
    fFolderBarLinesColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetGutterShapeSize(const Value: TSynNativeInt);
begin
  if fGutterShapeSize <> Value then begin
    fGutterShapeSize := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetShowHintMark(const Value: Boolean);
begin
  if fShowHintMark <> Value then begin
    fShowHintMark := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetShowCollapsedLine(const Value: Boolean);
begin
  if fShowCollapsedLine <> Value then begin
    fShowCollapsedLine := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;


end.
