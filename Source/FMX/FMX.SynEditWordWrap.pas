{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Based on Vcl.SynEditWordWrap.pas by Flávio Etrusco, released 2003-12-11.
FMX port uses character-count wrapping (monospace font assumption).
-------------------------------------------------------------------------------}

unit FMX.SynEditWordWrap;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Math,
  SynEditTypes;

type
  // FLineOffsets[N] is the 0-based first row of line N+1.
  // e.g. Starting row of first line (0) is 0. Starting row of second line (1)
  // is FLineOffsets[0].

  TFMXWordWrapHelper = class
  private
    FLineOffsets: TList<Integer>;
    FRowLengths: TList<Integer>;
    FLineCount: Integer;
    FMaxCharsPerRow: Integer;
    FTabWidth: Integer;
    procedure WrapLine(const ALine: string; out RowLengths: TArray<Integer>);
    procedure WrapLines(Lines: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetWrapWidth(AMaxCharsPerRow, ATabWidth: Integer);
    procedure Reset(Lines: TStrings);
    function LineToRow(aLine: Integer): Integer;
    function RowToLine(aRow: Integer): Integer;
    function RowCount: Integer;
    function GetRowLength(aRow: Integer): Integer;
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    property MaxCharsPerRow: Integer read FMaxCharsPerRow;
  end;

implementation

{ TFMXWordWrapHelper }

constructor TFMXWordWrapHelper.Create;
begin
  inherited;
  FLineOffsets := TList<Integer>.Create;
  FRowLengths := TList<Integer>.Create;
  FMaxCharsPerRow := 80;
  FTabWidth := 8;
end;

destructor TFMXWordWrapHelper.Destroy;
begin
  FLineOffsets.Free;
  FRowLengths.Free;
  inherited;
end;

procedure TFMXWordWrapHelper.SetWrapWidth(AMaxCharsPerRow, ATabWidth: Integer);
begin
  FMaxCharsPerRow := Max(AMaxCharsPerRow, 1);
  FTabWidth := Max(ATabWidth, 1);
end;

procedure TFMXWordWrapHelper.Reset(Lines: TStrings);
begin
  FLineCount := Lines.Count;
  WrapLines(Lines);
end;

procedure TFMXWordWrapHelper.WrapLine(const ALine: string;
  out RowLengths: TArray<Integer>);
var
  P, PStart, PEnd, PBreak: Integer; // 1-based char indices
  VisCol: Integer; // current visual column (0-based)
  WorkList: TList<Integer>;
  C: WideChar;
begin
  if (Length(ALine) = 0) then
  begin
    RowLengths := [0];
    Exit;
  end;

  // Quick check: if the line can't possibly exceed wrap width
  // (even with all tabs expanded to max)
  if Length(ALine) <= FMaxCharsPerRow then
  begin
    // Check if tabs could push it over
    if Pos(#9, ALine) = 0 then
    begin
      RowLengths := [Length(ALine)];
      Exit;
    end;
  end;

  WorkList := TList<Integer>.Create;
  try
    PStart := 1;
    PEnd := Length(ALine);
    P := PStart;
    PBreak := 0;
    VisCol := 0;

    while P <= PEnd do
    begin
      C := ALine[P];

      // Track word break points before advancing column
      if (P > PStart) then
      begin
        case C of
          ' ':
            PBreak := P; // break after space (space stays on current row)
          ')', ']', '}', ',', ';':
            PBreak := P; // break before closing bracket
          '(', '[', '{':
            if PBreak = 0 then
              PBreak := P - 1; // break before opening bracket
        end;
      end;

      // Advance visual column
      if C = #9 then
        VisCol := VisCol + FTabWidth - (VisCol mod FTabWidth)
      else
        Inc(VisCol);

      // Check if we need to wrap
      if VisCol > FMaxCharsPerRow then
      begin
        if (PBreak > 0) and (PBreak >= PStart) then
        begin
          // Wrap at word boundary
          WorkList.Add(PBreak - PStart + 1);
          PStart := PBreak + 1;
          P := PStart;
          PBreak := 0;
          VisCol := 0;
          Continue;
        end
        else
        begin
          // Emergency wrap: break right before this character
          if P > PStart then
          begin
            WorkList.Add(P - PStart);
            PStart := P;
          end
          else
          begin
            // Single character wider than wrap width (tab) - include it anyway
            WorkList.Add(1);
            PStart := P + 1;
            Inc(P);
          end;
          PBreak := 0;
          VisCol := 0;
          Continue;
        end;
      end;

      Inc(P);
    end;

    // Remainder
    if PStart <= PEnd + 1 then
      WorkList.Add(PEnd - PStart + 1);

    if WorkList.Count = 0 then
      RowLengths := [0]
    else
      RowLengths := WorkList.ToArray;
  finally
    WorkList.Free;
  end;
end;

procedure TFMXWordWrapHelper.WrapLines(Lines: TStrings);
var
  I: Integer;
  RowLens: TArray<Integer>;
begin
  FLineOffsets.Clear;
  FRowLengths.Clear;

  if Lines.Count = 0 then
    Exit;

  FLineOffsets.Capacity := Lines.Count;
  FRowLengths.Capacity := Lines.Count;

  for I := 0 to Lines.Count - 1 do
  begin
    WrapLine(Lines[I], RowLens);
    FRowLengths.AddRange(RowLens);
    FLineOffsets.Add(FRowLengths.Count);
  end;
end;

function TFMXWordWrapHelper.LineToRow(aLine: Integer): Integer;
begin
  Assert(aLine > 0);
  if FLineCount < aLine then
    Exit(FRowLengths.Count + (aLine - FLineCount));

  if aLine = 1 then
    Result := 1
  else
    Result := FLineOffsets[aLine - 2] + 1;
end;

function TFMXWordWrapHelper.RowToLine(aRow: Integer): Integer;
var
  cLine: Integer;
begin
  Assert(aRow > 0);
  if aRow > FRowLengths.Count then
    Exit(FLineCount + aRow - FRowLengths.Count);

  // Search from an optimized start point
  for cLine := Min(aRow, FLineCount) - 2 downto 0 do
    if aRow > FLineOffsets[cLine] then
      Exit(cLine + 2);

  Result := 1;
end;

function TFMXWordWrapHelper.RowCount: Integer;
begin
  Result := FRowLengths.Count;
end;

function TFMXWordWrapHelper.GetRowLength(aRow: Integer): Integer;
begin
  if (aRow <= 0) or (aRow > FRowLengths.Count) then
    Exit(0);
  Result := FRowLengths[aRow - 1];
end;

function TFMXWordWrapHelper.BufferToDisplayPos(
  const aPos: TBufferCoord): TDisplayCoord;
var
  vStartRow: Integer;
  cRow: Integer;
  vRowLen: Integer;
begin
  Assert(aPos.Char > 0);
  Assert(aPos.Line > 0);

  if FLineCount < aPos.Line then
  begin
    // Beyond EOF
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

  // Beyond EOL - place on last row of the line
  Result.Column := aPos.Char - vRowLen + FRowLengths[FLineOffsets[aPos.Line - 1] - 1];
  Result.Row := FLineOffsets[aPos.Line - 1];
end;

function TFMXWordWrapHelper.DisplayToBufferPos(
  const aPos: TDisplayCoord): TBufferCoord;
var
  cRow: Integer;
  FirstRow: Integer;
begin
  Assert(aPos.Column > 0);
  Assert(aPos.Row > 0);

  Result.Line := RowToLine(aPos.Row);
  if Result.Line > FLineCount then
  begin
    // Beyond EOF
    Result.Char := aPos.Column;
    Exit;
  end;

  if aPos.Row = FLineOffsets[Result.Line - 1] then
    // Last row of line - allow positions beyond EOL
    Result.Char := aPos.Column
  else
    Result.Char := Min(aPos.Column, FRowLengths[aPos.Row - 1] + 1);

  if Result.Line = 1 then
    FirstRow := 0
  else
    FirstRow := FLineOffsets[Result.Line - 2];

  for cRow := FirstRow to aPos.Row - 2 do
    Inc(Result.Char, FRowLengths[cRow]);
end;

end.
