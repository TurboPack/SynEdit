{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditSearch.pas, released 2000-04-07.

The Original Code is based on the mwEditSearch.pas file from the mwEdit
component suite by Martin Waldenburg and other developers.
Portions created by Martin Waldenburg are Copyright 1999 Martin Waldenburg.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

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
-------------------------------------------------------------------------------}

unit SynEditSearch;

{$I SynEdit.inc}

interface

uses
  SynEditTypes,
  SynEditMiscClasses,
  SynUnicode,
  Classes;

type
  TSynEditSearch = class(TSynEditSearchCustom)
  private
    Run: PWideChar;
    Origin: PWideChar;
    TheEnd: PWideChar;
    Pat, CasedPat: string;
    fCount: Integer;
    fTextLen: Integer;
    FLineStart: PWideChar;
    Look_At: Integer;
    PatLen, PatLenSucc: Integer;
    Shift: array[WideChar] of Integer;
    fBackwards: Boolean;
    fCaseSensitive: Boolean;
    fWhole: Boolean;
    fResults: TList;
    fShiftInitialized: Boolean;
    FTextToSearch: string;
    function GetFinished: Boolean;
    procedure InitShiftTable;
    procedure SetCaseSensitive(const Value: Boolean);
    function IsWordBreakChar(C: WideChar): Boolean;
  protected
    function TestWholeWord: Boolean;
    procedure SetPattern(const Value: string); override;
    function GetPattern: string; override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function FindAll(const NewText: string; StartChar: Integer = 1;
      EndChar: Integer = 0): Integer; override;
    function Replace(const aOccurrence, aReplacement: string): string; override;
    function FindFirst(const NewText: string; StartChar, EndChar: Integer): Integer;
    function FindLast(const NewText: string; StartChar, EndChar: Integer): Integer;
    procedure FixResults(First, Delta: Integer);
    function Next: Integer;
    function Prev: Integer;
    property Count: Integer read fCount write fCount;
    property Finished: Boolean read GetFinished;
    property Pattern read CasedPat;
    property Backwards: Boolean read fBackwards write fBackwards;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property Whole: Boolean read fWhole write fWhole;
  end;

implementation

uses
  Windows,
  SysUtils;

constructor TSynEditSearch.Create(aOwner: TComponent);
begin
  inherited;
  fResults := TList.Create;
end;

function TSynEditSearch.GetFinished: Boolean;
begin
  Result := (Run >= TheEnd) or (PatLen >= fTextLen);
end;

function TSynEditSearch.GetResult(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < fResults.Count) then
    Result := Integer(fResults[Index]);
end;

function TSynEditSearch.GetResultCount: Integer;
begin
  Result := fResults.Count;
end;

procedure TSynEditSearch.FixResults(First, Delta: Integer);
var
  I: Integer;
begin
  if (Delta <> 0) and (fResults.Count > 0) then begin
    I := Pred(fResults.Count);
    while I >= 0 do begin
      if Integer(fResults[I]) <= First then Break;
      fResults[I] := pointer(Integer(fResults[I]) - Delta);
      Dec(I);
    end;
  end;
end;

procedure TSynEditSearch.InitShiftTable;
var
  C: WideChar;
  I: Integer;
begin
  PatLenSucc := PatLen + 1;
  Look_At := 1;
  for C := Low(WideChar) to High(WideChar) do Shift[C] := PatLenSucc;
  for I := 1 to PatLen do Shift[Pat[I]] := PatLenSucc - I;
  while Look_at < PatLen do
  begin
    if Pat[PatLen] = Pat[PatLen - Look_at] then Break;
    Inc(Look_at);
  end;
  fShiftInitialized := True;
end;

function TSynEditSearch.IsWordBreakChar(C: WideChar): Boolean;
begin
  if Assigned(FIsWordBreakFunction) then
    Exit(FIsWordBreakFunction(C));

  case C of
      #0..#32, '.', ',', ';', ':', '"', '''', #$00B4, '`',
      #$00B0, '^', '!', '?', '&', '$', '@', #$00A7, '%',
      '#', '~', '[', ']', '(', ')', '{', '}', '<', '>', '-', '=', '+', '*',
      '/', '\', '|':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynEditSearch.TestWholeWord: Boolean;
var
  Test: PWideChar;
begin
  Test := Run - PatLen;

  Result := ((Test < FLineStart) or IsWordBreakChar(Test[0])) and
    ((Run >= FLineStart + fTextLen) or IsWordBreakChar(Run[1]));
end;

function TSynEditSearch.Next: Integer;
var
  I: Integer;
  J: PWideChar;
begin
  Result := 0;
  Inc(Run, PatLen);
  while Run < TheEnd do
  begin
    if Pat[Patlen] <> Run^ then
      Inc(Run, Shift[(Run + 1)^])
    else
    begin
      J := Run - PatLen + 1;
      I := 1;
      while Pat[I] = J^ do
      begin
        if I = PatLen then
        begin
          if fWhole and not TestWholeWord then Break;
          Inc(fCount);
          Result := Run - FLineStart - Patlen + 2;
          Exit;
        end;
        Inc(I);
        Inc(J);
      end;
      Inc(Run, Look_At);
      if Run >= TheEnd then
        Break;
      Inc(Run, Shift[Run^] - 1);
    end;
  end;
end;

function TSynEditSearch.Prev: Integer;
// "Naive" backward search
// Run points to the last char of the pattern in the search string
// as in Next so that we can reuse TestWholeWord.
var
  I: Integer;
  PTrial: PChar;
begin
  Result := 0;
  while Run >= Origin + PatLen do
  begin
    Dec(Run);
    PTrial := Run - PatLen + 1;
    I := PatLen;
    while I >= 1 do
    begin
      if PTrial[I - 1] <> Pat[I] then
        Break;
      Dec(I);
    end;
    if (I = 0) and (not fWhole or TestWholeWord) then
    begin
      Result := PTrial - FLineStart + 1;
      Run := PTrial;
      Exit;
    end;
  end;
  Run := Origin;
end;

destructor TSynEditSearch.Destroy;
begin
  fResults.Free;
  inherited Destroy;
end;

procedure TSynEditSearch.SetPattern(const Value: string);
begin
  if Pat <> Value then
  begin
    CasedPat := Value;
    if CaseSensitive then
      Pat := CasedPat
    else
      Pat := SysUtils.AnsiLowerCase(CasedPat);
    fShiftInitialized := False;
  end;
  fCount := 0;
end;

procedure TSynEditSearch.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    fCaseSensitive := Value;
    if fCaseSensitive then
      Pat := CasedPat
    else
      Pat := SysUtils.AnsiLowerCase(CasedPat);
    fShiftInitialized := False;
  end;
end;

function TSynEditSearch.FindAll(const NewText: string; StartChar: Integer = 1;
      EndChar: Integer = 0): Integer;
// Uses a Boyer-Moore algorithm for forward seach and a "naive" one
// for backward search
var
  Found: Integer;
begin
  if EndChar = 0 then
    EndChar := NewText.Length + 1;
  fTextLen := Length(NewText);
  PatLen := Length(Pat);

  if Patlen = 0 then
    raise Exception.Create('Pattern is empty');

  // never shrink Capacity
  fResults.Count := 0;

  if (fTextLen = 0) or (EndChar <= StartChar) or (fTextLen < PatLen) or
    (PatLen > EndChar - StartChar)
  then
    Exit(0);

  if CaseSensitive then
    FTextToSearch := NewText
  else
    FTextToSearch := SysUtils.AnsiLowerCase(NewText);
  FLineStart := PWideChar(FTextToSearch);

  if Backwards then
  begin
    Found := FindLast(NewText, StartChar, EndChar);
    while Found > 0 do
    begin
      fResults.Insert(0, Pointer(Found));
      Found := Prev;
    end;
  end
  else
  begin
    Found := FindFirst(NewText, StartChar, EndChar);
    while Found > 0 do
    begin
      fResults.Add(Pointer(Found));
      Found := Next;
    end;
  end;
  Result := fResults.Count;
end;

function TSynEditSearch.Replace(const aOccurrence, aReplacement: string): string;
begin
  Result := aReplacement;
end;

function TSynEditSearch.FindFirst(const NewText: string;
  StartChar, EndChar: Integer): Integer;
begin
  if not fShiftInitialized then
    InitShiftTable;

  Origin := FLineStart + StartChar - 1;
  TheEnd := Origin + (EndChar - StartChar);
  Run := (Origin - 1);
  Result := Next;
end;

function TSynEditSearch.FindLast(const NewText: string;
  StartChar, EndChar: Integer): Integer;
begin
  Origin := FLineStart + StartChar - 1;
  TheEnd := Origin + EndChar - StartChar;
  Run := TheEnd;
  Result := Prev;
end;

function TSynEditSearch.GetLength(Index: Integer): Integer;
begin
  Result := PatLen;
end;

function TSynEditSearch.GetPattern: string;
begin
  Result := CasedPat;
end;

procedure TSynEditSearch.SetOptions(const Value: TSynSearchOptions);
begin
  CaseSensitive := ssoMatchCase in Value;
  Whole := ssoWholeWord in Value;
  Backwards := ssoBackwards in Value;
end;

end.

