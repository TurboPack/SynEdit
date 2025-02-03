{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: SynEditMiscProcs.pas, released 2000-04-07.
  The Original Code is based on the mwSupportProcs.pas file from the
  mwEdit component suite by Martin Waldenburg and other developers, the Initial
  Author of this file is Michael Hieke.
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
  ------------------------------------------------------------------------------- }

unit SynEditMiscProcs;

{$I SynEdit.inc}

interface

uses
  Winapi.Windows,
  System.Math,
  System.Classes,
  System.RegularExpressions,
  Vcl.Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode;

const
  MaxIntArraySize = MaxInt div 16;

type
  PIntArray = ^TIntArray;
  TIntArray = array [0 .. MaxIntArraySize - 1] of Integer;

{ Similar to System.Math.EnsureRange but ma can be less than mi }
function MinMax(x, mi, ma: Integer): Integer;
procedure SwapInt(var l, r: Integer);

// Expand tabs to spaces
function ExpandTabs(const Line: string; TabWidth: Integer): string;
function ExpandTabsEx(const Line: string; TabWidth: Integer;
  var HasTabs: Boolean): string;
function GetExpandedLength(const aStr: string; aTabWidth: Integer): Integer;
function LeftSpaces(const Line: string; ExpandTabs: Boolean;
  TabWidth: Integer = 2): Integer;


function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: string): Integer;
function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: string;
  var InsideTabChar: Boolean): Integer;

// search for the first char of set AChars in Line, starting at index Start
function StrScanForCharInCategory(const Line: string; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;
// the same, but searching backwards
function StrRScanForCharInCategory(const Line: string; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;

function GetEOL(P: PChar): PWideChar;
function CountLines(const S: string): Integer;
function StringToLines(const Value: string): TArray<string>;

// Remove all '/' characters from string by changing them into '\.'.
// Change all '\' characters into '\\' to allow for unique decoding.
function EncodeString(s: string): string;

// Decodes string, encoded with EncodeString.
function DecodeString(s: string): string;

type
  THighlighterAttriProc = function(Highlighter: TSynCustomHighlighter;
    Attri: TSynHighlighterAttributes; UniqueAttriName: string;
    Params: array of Pointer): Boolean of object;

  // Enums all child highlighters and their attributes of a TSynMultiSyn through a
  // callback function.
  // This function also handles nested TSynMultiSyns including their MarkerAttri.
function EnumHighlighterAttris(Highlighter: TSynCustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer): Boolean;

type
  // Procedural type for adding keyword entries when enumerating keyword
  // lists using the EnumerateKeywords procedure below.
  TEnumerateKeywordEvent = procedure(AKeyword: string; AKind: Integer)
    of object;

  //  This procedure will call AKeywordProc for all keywords in KeywordList. A
  //  keyword is considered any number of successive chars that are contained in
  //  Identifiers, with chars not contained in Identifiers before and after them.
  procedure EnumerateKeywords(AKind: Integer; KeywordList: string;
    IsIdentChar: TCategoryMethod; AKeywordProc: TEnumerateKeywordEvent);

{$IFDEF SYN_HEREDOC}
// Calculates Frame Check Sequence (FCS) 16-bit Checksum (as defined in RFC 1171)
function CalcFCS(const ABuf; ABufSize: Cardinal): Word;
{$ENDIF}
function DeleteTypePrefixAndSynSuffix(s: string): string;
function CeilOfIntDiv(Dividend, Divisor: Cardinal): Integer;

// In Windows Vista or later use the Consolas font
function DefaultFontName: string;

function GetCorrectFontWeight(Font: TFont): Integer;

// Calculates the difference between two lines
// Returns the starting point of the difference and the lengths of the change
procedure LineDiff(const Line, OldLine: string; out StartPos, OldLen, NewLen:
    Integer);

// Tests whether a color is dark
function IsColorDark(AColor: TColor): Boolean;

// Substitutes control characters with Unicode control pictures
procedure SubstituteControlChars(var Input: string);

// Returns a compiled regular expression
function CompiledRegEx(const Pattern: string; Options: TRegExOptions = []): TRegEx;

// Converts TColor to an HTML color string
function ColorToHTML(Color: TColor): string;

// Bracket functions (Brackets have the form '()[]{}')
function IsBracket(Chr: Char; const Brackets: string): Boolean;
function IsOpeningBracket(Chr: Char; const Brackets: string): Boolean;
function BracketAtPos(Idx: Integer; const Brackets, Line: string): Boolean;
function MatchingBracket(Bracket: Char; const Brackets: string): Char;

{$IF CompilerVersion <= 32}
function GrowCollection(OldCapacity, NewCount: Integer): Integer;
{$ENDIF}

implementation

uses
  System.UITypes,
  System.SysUtils,
  System.RegularExpressionsCore,
  SynHighlighterMulti,
  Winapi.D2D1,
  Vcl.Forms,
  SynDWrite;

function MinMax(x, mi, ma: Integer): Integer;
begin
  x := Min(x, ma);
  Result := Max(x, mi);
end;

procedure SwapInt(var l, r: Integer);
var
  tmp: Integer;
begin
  tmp := r;
  r := l;
  l := tmp;
end;

// Please don't change this function; no stack frame and efficient register use.
function GetHasTabs(pLine: PWideChar; var CharsBefore: Integer): Boolean;
begin
  CharsBefore := 0;
  if Assigned(pLine) then
  begin
    while pLine^ <> #0 do
    begin
      if pLine^ = #9 then
        Break;
      Inc(CharsBefore);
      Inc(pLine);
    end;
    Result := pLine^ = #9;
  end
  else
    Result := False;
end;

function ExpandTabsEx(const Line: string; TabWidth: Integer;
  var HasTabs: Boolean): string;
var
  i, DestLen, TabCount: Integer;
  pSrc, pDest: PWideChar;
begin
  Result := Line; // increment reference count only
  if GetHasTabs(Pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if pSrc^ = #9 then
      begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PWideChar(Line);
    pDest := PWideChar(Result);
    repeat
      if pSrc^ = #9 then
      begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := ' ';
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          Exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end
  else
    HasTabs := False;
end;

function ExpandTabs(const Line: string; TabWidth: Integer): string;
var
  HasTabs: Boolean;
begin
  Result := ExpandTabsEx(Line, TabWidth, HasTabs);
end;

function GetExpandedLength(const aStr: string; aTabWidth: Integer): Integer;
var
  iRun: PWideChar;
begin
  Result := 0;
  iRun := PWideChar(aStr);
  while iRun^ <> #0 do
  begin
    if iRun^ = #9 then
      Inc(Result, aTabWidth - (Result mod aTabWidth))
    else
      Inc(Result);
    Inc(iRun);
  end;
end;

function LeftSpaces(const Line: string; ExpandTabs: Boolean;
  TabWidth: Integer = 2): Integer;
var
  P: PChar;
begin
  Result := 0;
  P := PChar(Line);
  while (P^ >= #1) and ((P^ <= #32) or (P^ = #$00A0)) do
  begin
    if (P^ = #9) and ExpandTabs then
      Inc(Result, TabWidth - (Result mod TabWidth))
    else
      Inc(Result);
    Inc(P);
  end;
end;

function CharIndex2CaretPos(Index, TabWidth: Integer;
  const Line: string): Integer;
var
  iChar: Integer;
  pNext: PWideChar;
begin
  // possible sanity check here: Index := Max(Index, Length(Line));
  if Index > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(Pointer(Line), iChar) then
      Result := Index
    else
    begin
      if iChar + 1 >= Index then
        Result := Index
      else
      begin
        // iChar is number of chars before first #9
        Result := iChar;
        // Index is *not* zero-based
        Inc(iChar);
        Dec(Index, iChar);
        pNext := @Line[iChar];
        while Index > 0 do
        begin
          case pNext^ of
            #0:
              begin
                Inc(Result, Index);
                Break;
              end;
            #9:
              begin
                // Result is still zero-based
                Inc(Result, TabWidth);
                Dec(Result, Result mod TabWidth);
              end;
          else
            Inc(Result);
          end;
          Dec(Index);
          Inc(pNext);
        end;
        // done with zero-based computation
        Inc(Result);
      end;
    end;
  end
  else
    Result := 1;
end;

function CaretPos2CharIndex(Position, TabWidth: Integer; const Line: string;
  var InsideTabChar: Boolean): Integer;
var
  iPos: Integer;
  pNext: PWideChar;
begin
  InsideTabChar := False;
  if Position > 1 then
  begin
    if (TabWidth <= 1) or not GetHasTabs(Pointer(Line), iPos) then
      Result := Position
    else
    begin
      if iPos + 1 >= Position then
        Result := Position
      else
      begin
        // iPos is number of chars before first #9
        Result := iPos + 1;
        pNext := @Line[Result];
        // for easier computation go zero-based (mod-operation)
        Dec(Position);
        while iPos < Position do
        begin
          case pNext^ of
            #0:
              Break;
            #9:
              begin
                Inc(iPos, TabWidth);
                Dec(iPos, iPos mod TabWidth);
                if iPos > Position then
                begin
                  InsideTabChar := True;
                  Break;
                end;
              end;
          else
            Inc(iPos);
          end;
          Inc(Result);
          Inc(pNext);
        end;
      end;
    end;
  end
  else
    Result := Position;
end;

function StrScanForCharInCategory(const Line: string; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;
var
  p: PWideChar;
begin
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    p := PWideChar(@Line[Start]);
    repeat
      if IsOfCategory(p^) then
      begin
        Result := Start;
        Exit;
      end;
      Inc(p);
      Inc(Start);
    until p^ = #0;
  end;
  Result := 0;
end;

function StrRScanForCharInCategory(const Line: string; Start: Integer;
  IsOfCategory: TCategoryMethod): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (Start > 0) and (Start <= Length(Line)) then
  begin
    for i := Start downto 1 do
      if IsOfCategory(Line[i]) then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function GetEOL(P: PChar): PWideChar;
begin
  Result := P;
  if Assigned(Result) then
    while (Word(Result^) > 13) or not (Word(Result^) in [0, 10, 13]) do
      Inc(Result);
end;

function CountLines(const S: string): Integer;
// At least one line possibly empty
var
  P, PEnd: PChar;
begin
  Result := 0;
  P := PChar(S);
  PEnd := P + Length(S);
  while P < PEnd do
  begin
    //  We do it that way instead of checking for $0 as well
    //  so the we properly deal with strings containing #0  (who knows)
    while (P < PEnd) and ((Word(P^) > 13) or not (Word(P^) in [10, 13])) do
      Inc(P);
    Inc(Result);
    if P^ = #13 then Inc(P);
    if P^ = #10 then Inc(P);
  end;
  // Include Empty line at the end?
  if (S <> '') and (Word(S[S.Length]) in [10, 13]) then
    Inc(Result);
end;

function StringToLines(const Value: string): TArray<string>;
var
  Count: Integer;
  P, PStart, PEnd: PChar;
  S: string;
begin
  P := PChar(Value);
  Count := CountLines(Value);
  SetLength(Result, Count);

  Count := 0;
  PEnd := P + Length(Value);
  while P < PEnd do
  begin
    PStart := P;
    //  We do it that way instead of checking for $0 as well
    //  so the we properly deal with strings containing #0  (who knows)
    while (P < PEnd) and ((Word(P^) > 13) or not (Word(P^) in [10, 13])) do
      Inc(P);
    SetString(S, PStart, P - PStart);
    Result[Count] := S;
    Inc(Count);
    if P^ = #13 then Inc(P);
    if P^ = #10 then Inc(P);
  end;
end;

{$IFOPT R+}{$DEFINE RestoreRangeChecking}{$ELSE}{$UNDEF RestoreRangeChecking}{$ENDIF}
{$R-}

function EncodeString(s: string): string;
var
  i, j: Integer;
begin
  SetLength(Result, 2 * Length(s)); // worst case
  j := 0;
  for i := 1 to Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Result[j] := '\';
      Result[j + 1] := '\';
      Inc(j);
    end
    else if s[i] = '/' then
    begin
      Result[j] := '\';
      Result[j + 1] := '.';
      Inc(j);
    end
    else
      Result[j] := s[i];
  end; // for
  SetLength(Result, j);
end; { EncodeString }

function DecodeString(s: string): string;
var
  i, j: Integer;
begin
  SetLength(Result, Length(s)); // worst case
  j := 0;
  i := 1;
  while i <= Length(s) do
  begin
    Inc(j);
    if s[i] = '\' then
    begin
      Inc(i);
      if s[i] = '\' then
        Result[j] := '\'
      else
        Result[j] := '/';
    end
    else
      Result[j] := s[i];
    Inc(i);
  end; // for
  SetLength(Result, j);
end; { DecodeString }
{$IFDEF RestoreRangeChecking}{$R+}{$ENDIF}

function DeleteTypePrefixAndSynSuffix(s: string): string;
begin
  Result := s;
  if CharInSet(Result[1], ['T', 't']) then
  // ClassName is never empty so no AV possible
    if Pos('tsyn', LowerCase(Result)) = 1 then
      Delete(Result, 1, 4)
    else
      Delete(Result, 1, 1);

  if Copy(LowerCase(Result), Length(Result) - 2, 3) = 'syn' then
    SetLength(Result, Length(Result) - 3);
end;

function GetHighlighterIndex(Highlighter: TSynCustomHighlighter;
  HighlighterList: TList): Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to HighlighterList.Count - 1 do
    if HighlighterList[i] = Highlighter then
      Exit
    else if Assigned(HighlighterList[i]) and
      (TObject(HighlighterList[i]).ClassType = Highlighter.ClassType) then
      Inc(Result);
end;

function InternalEnumHighlighterAttris(Highlighter: TSynCustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer; HighlighterList: TList): Boolean;
var
  i: Integer;
  UniqueAttriName: string;
begin
  Result := True;

  if (HighlighterList.IndexOf(Highlighter) >= 0) then
  begin
    if SkipDuplicates then
      Exit;
  end
  else
    HighlighterList.Add(Highlighter);

  if Highlighter is TSynMultiSyn then
    with TSynMultiSyn(Highlighter) do
    begin
      Result := InternalEnumHighlighterAttris(DefaultHighlighter,
        SkipDuplicates, HighlighterAttriProc, Params, HighlighterList);
      if not Result then
        Exit;

      for i := 0 to Schemes.Count - 1 do
      begin
        UniqueAttriName := Highlighter.ExportName +
          IntToStr(GetHighlighterIndex(Highlighter, HighlighterList)) + '.' +
          Schemes[i].MarkerAttri.Name + IntToStr(i + 1);

        Result := HighlighterAttriProc(Highlighter, Schemes[i].MarkerAttri,
          UniqueAttriName, Params);
        if not Result then
          Exit;

        Result := InternalEnumHighlighterAttris(Schemes[i].Highlighter,
          SkipDuplicates, HighlighterAttriProc, Params, HighlighterList);
        if not Result then
          Exit
      end
    end
  else if Assigned(Highlighter) then
    for i := 0 to Highlighter.AttrCount - 1 do
    begin
      UniqueAttriName := Highlighter.ExportName +
        IntToStr(GetHighlighterIndex(Highlighter, HighlighterList)) + '.' +
        Highlighter.Attribute[i].Name;

      Result := HighlighterAttriProc(Highlighter, Highlighter.Attribute[i],
        UniqueAttriName, Params);
      if not Result then
        Exit
    end
end;

function EnumHighlighterAttris(Highlighter: TSynCustomHighlighter;
  SkipDuplicates: Boolean; HighlighterAttriProc: THighlighterAttriProc;
  Params: array of Pointer): Boolean;
var
  HighlighterList: TList;
begin
  if not Assigned(Highlighter) or not Assigned(HighlighterAttriProc) then
  begin
    Result := False;
    Exit;
  end;

  HighlighterList := TList.Create;
  try
    Result := InternalEnumHighlighterAttris(Highlighter, SkipDuplicates,
      HighlighterAttriProc, Params, HighlighterList)
  finally
    HighlighterList.Free
  end
end;

procedure EnumerateKeywords(AKind: Integer; KeywordList: string;
  IsIdentChar: TCategoryMethod; AKeywordProc: TEnumerateKeywordEvent);
var
  pStart, pEnd: PWideChar;
  Keyword: string;
begin
  if Assigned(AKeywordProc) and (KeywordList <> '') then
  begin
    pEnd := PWideChar(KeywordList);
    pStart := pEnd;
    repeat
      // skip over chars that are not in Identifiers
      while (pStart^ <> #0) and not IsIdentChar(pStart^) do
        Inc(pStart);
      if pStart^ = #0 then Break;
      // find the last char that is in Identifiers
      pEnd := pStart + 1;
      while (pEnd^ <> #0) and IsIdentChar(pEnd^) do
        Inc(pEnd);
      // call the AKeywordProc with the keyword
      SetString(Keyword, pStart, pEnd - pStart);
      AKeywordProc(Keyword, AKind);
      Keyword := '';
      // pEnd points to a char not in Identifiers, restart after that
      if pEnd^ <> #0 then
        pStart := pEnd + 1;
    until (pStart^ = #0) or (pEnd^ = #0);
  end;
end;

{$IFDEF SYN_HEREDOC}
// Fast Frame Check Sequence (FCS) Implementation
// Translated from sample code given with RFC 1171 by Marko Njezic

const
  fcstab: array [Byte] of Word = ($0000, $1189, $2312, $329B, $4624, $57AD,
    $6536, $74BF, $8C48, $9DC1, $AF5A, $BED3, $CA6C, $DBE5, $E97E, $F8F7, $1081,
    $0108, $3393, $221A, $56A5, $472C, $75B7, $643E, $9CC9, $8D40, $BFDB, $AE52,
    $DAED, $CB64, $F9FF, $E876, $2102, $308B, $0210, $1399, $6726, $76AF, $4434,
    $55BD, $AD4A, $BCC3, $8E58, $9FD1, $EB6E, $FAE7, $C87C, $D9F5, $3183, $200A,
    $1291, $0318, $77A7, $662E, $54B5, $453C, $BDCB, $AC42, $9ED9, $8F50, $FBEF,
    $EA66, $D8FD, $C974, $4204, $538D, $6116, $709F, $0420, $15A9, $2732, $36BB,
    $CE4C, $DFC5, $ED5E, $FCD7, $8868, $99E1, $AB7A, $BAF3, $5285, $430C, $7197,
    $601E, $14A1, $0528, $37B3, $263A, $DECD, $CF44, $FDDF, $EC56, $98E9, $8960,
    $BBFB, $AA72, $6306, $728F, $4014, $519D, $2522, $34AB, $0630, $17B9, $EF4E,
    $FEC7, $CC5C, $DDD5, $A96A, $B8E3, $8A78, $9BF1, $7387, $620E, $5095, $411C,
    $35A3, $242A, $16B1, $0738, $FFCF, $EE46, $DCDD, $CD54, $B9EB, $A862, $9AF9,
    $8B70, $8408, $9581, $A71A, $B693, $C22C, $D3A5, $E13E, $F0B7, $0840, $19C9,
    $2B52, $3ADB, $4E64, $5FED, $6D76, $7CFF, $9489, $8500, $B79B, $A612, $D2AD,
    $C324, $F1BF, $E036, $18C1, $0948, $3BD3, $2A5A, $5EE5, $4F6C, $7DF7, $6C7E,
    $A50A, $B483, $8618, $9791, $E32E, $F2A7, $C03C, $D1B5, $2942, $38CB, $0A50,
    $1BD9, $6F66, $7EEF, $4C74, $5DFD, $B58B, $A402, $9699, $8710, $F3AF, $E226,
    $D0BD, $C134, $39C3, $284A, $1AD1, $0B58, $7FE7, $6E6E, $5CF5, $4D7C, $C60C,
    $D785, $E51E, $F497, $8028, $91A1, $A33A, $B2B3, $4A44, $5BCD, $6956, $78DF,
    $0C60, $1DE9, $2F72, $3EFB, $D68D, $C704, $F59F, $E416, $90A9, $8120, $B3BB,
    $A232, $5AC5, $4B4C, $79D7, $685E, $1CE1, $0D68, $3FF3, $2E7A, $E70E, $F687,
    $C41C, $D595, $A12A, $B0A3, $8238, $93B1, $6B46, $7ACF, $4854, $59DD, $2D62,
    $3CEB, $0E70, $1FF9, $F78F, $E606, $D49D, $C514, $B1AB, $A022, $92B9, $8330,
    $7BC7, $6A4E, $58D5, $495C, $3DE3, $2C6A, $1EF1, $0F78);

function CalcFCS(const ABuf; ABufSize: Cardinal): Word;
var
  CurFCS: Word;
  p: ^Byte;
begin
  CurFCS := $FFFF;
  p := @ABuf;
  while ABufSize <> 0 do
  begin
    CurFCS := (CurFCS shr 8) xor fcstab[(CurFCS xor p^) and $FF];
    Dec(ABufSize);
    Inc(p);
  end;
  Result := CurFCS;
end;
{$ENDIF}

function CeilOfIntDiv(Dividend, Divisor: Cardinal): Integer;
var
  Res: UInt64;
  Remainder: UInt64;
begin
  DivMod(Dividend,  Divisor, Res, Remainder);
  if Remainder > 0 then
    Inc(Res);
  Result := Integer(Res);
end;

function DefaultFontName: string;
begin
  if CheckWin32Version(6) then
  begin
    Result := 'Consolas';
    if Screen.Fonts.IndexOf(Result) >= 0 then
      Exit;
  end;

  Result := 'Lucida Console';
  if Screen.Fonts.IndexOf(Result) >= 0 then
    Exit;

  Result := 'Courier New';
  if Screen.Fonts.IndexOf(Result) < 0 then
    Result := 'Courier';
end;

function WeightEnumFontsProc(EnumLogFontExDV: PEnumLogFontExDV;
  EnumTextMetric: PEnumTextMetric;
  FontType: DWORD; LParam: LPARAM): Integer; stdcall;
begin;
  PInteger(LPARAM)^ :=  EnumLogFontExDV.elfEnumLogfontEx.elfLogFont.lfWeight;
  Result := 0;
end;

function GetCorrectFontWeight(Font: TFont): Integer;
var
  DC: HDC;
  LogFont: TLogFont;
begin
  if TFontStyle.fsBold in Font.Style then
    Result := FW_BOLD
  else
  begin
    Result := FW_NORMAL;
    DC := GetDC(0);
    FillChar(LogFont, SizeOf(LogFont), 0);
    LogFont.lfCharSet := DEFAULT_CHARSET;
    StrPLCopy(LogFont.lfFaceName, Font.Name, Length(LogFont.lfFaceName) - 1);
    EnumFontFamiliesEx(DC, LogFont, @WeightEnumFontsProc, LPARAM(@Result), 0);
    ReleaseDC(0, DC);
  end;
end;

{$IF CompilerVersion <= 32}
function GrowCollection(OldCapacity, NewCount: Integer): Integer;
begin
  Result := OldCapacity;
  repeat
    if Result > 64 then
      Result := (Result * 3) div 2
    else
      if Result > 8 then
        Result := Result + 16
      else
        Result := Result + 4;
    if Result < 0 then
      OutOfMemoryError;
  until Result >= NewCount;
end;
{$ENDIF}

procedure LineDiff(const Line, OldLine: string; out StartPos, OldLen, NewLen: Integer);
begin
  OldLen := OldLine.Length;
  NewLen := Line.Length;
  // Compare from start
  StartPos := 1;
  while (OldLen > 0) and (NewLen > 0) and (OldLine[StartPos] = Line[StartPos]) do
  begin
    Dec(OldLen);
    Dec(NewLen);
    Inc(StartPos);
  end;
  // Compare from end
  while (OldLen > 0) and (NewLen > 0) and
    (OldLine[OldLen + StartPos - 1] = Line[NewLen + StartPos - 1]) do
  begin
    Dec(OldLen);
    Dec(NewLen);
  end;
end;

function IsColorDark(AColor: TColor): Boolean;
var
  ACol: Longint;
begin
  ACol := ColorToRGB(AColor) and $00FFFFFF;
  Result := ((2.99 * GetRValue(ACol) + 5.87 * GetGValue(ACol) +
                 1.14 * GetBValue(ACol)) < $400);
end;

procedure SubstituteControlChars(var Input: string);
const
  ControlChars: set of Byte = [1..31, 127];
  GraphicChars: array[1..31] of Char = (
      #$02401, #$02402, #$02403, #$02404, #$02405, #$02406, #$02407, #$02408,
      #$02409, #$0240A, #$0240B, #$0240C, #$0240D, #$0240E, #$0240F, #$02410,
      #$02411, #$02412, #$02413, #$02414, #$02415, #$02416, #$02417, #$02418,
      #$02419, #$0241A, #$0241B, #$0241C, #$0241D, #$0241E, #$0241F);
  DeleteChar  = #$02421;
var
  I: Integer;
begin
  UniqueString(Input);
  for I := 1 to Input.Length do
    case Ord(Input[I]) of
      1..8, 10..31: Input[I] := GraphicChars[Byte(Ord(Input[I]))];
      127: Input[I] := DeleteChar;
    end;
end;

function CompiledRegEx(const Pattern: string; Options: TRegExOptions): TRegEx;
begin
  Result := TRegEx.Create(Pattern, Options + [roCompiled]);
  {$IF (CompilerVersion > 35) or Declared(RTLVersion112)}
  Result.Study([preJIT]);
  {$ENDIF}
end;

function ColorToHTML(Color: TColor): string;
var
  R: TColorRef;
begin
  R := ColorToRGB(Color);
  Result := Format('#%.2x%.2x%.2x', [GetRValue(R), GetGValue(R), GetBValue(R)]);
end;

function IsBracket(Chr: Char; const Brackets: string): Boolean;
begin
  Result := Brackets.IndexOf(Chr) >= 0;
end;

function IsOpeningBracket(Chr: Char; const Brackets: string): Boolean;
var
  Idx: Integer;
begin
  Idx := Brackets.IndexOf(Chr);
  Result := (Idx >= 0) and not Odd(Idx);
end;

function BracketAtPos(Idx: Integer; const Brackets, Line: string): Boolean;
begin
  Result := InRange(Idx, 1, Line.Length) and IsBracket(Line[Idx], Brackets);
end;

function MatchingBracket(Bracket: Char; const Brackets: string): Char;
var
  Idx: Integer; // zero based char index
begin
  Idx := Brackets.IndexOf(Bracket);
  if Idx < 0 then
    Result := #0
  else
    Result := Brackets.Chars[Idx xor 1];
end;

end.
