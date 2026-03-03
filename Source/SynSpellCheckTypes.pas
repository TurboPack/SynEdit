{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{ Framework-neutral spell check types, interfaces, and word extraction helpers.
  Shared by both VCL and FMX spell check implementations. }

unit SynSpellCheckTypes;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

{$REGION 'Spell Check Token Types'}

type
  TSynSpellCheckToken = (sctComment, sctString, sctIdentifier);
  TSynSpellCheckTokens = set of TSynSpellCheckToken;

{$ENDREGION 'Spell Check Token Types'}

{$REGION 'Spell Error Record'}

  TSynSpellError = record
    Line: Integer;       // 1-based line
    StartChar: Integer;  // 1-based char
    EndChar: Integer;    // 1-based char (exclusive)
    Word: string;
  end;

{$ENDREGION 'Spell Error Record'}

{$REGION 'Provider Interface'}

  ISynSpellCheckProvider = interface
    ['{A1F5B2C3-D4E6-4789-AB01-23456789ABCD}']
    function CheckWord(const AWord: string): Boolean;
    function Suggest(const AWord: string): TArray<string>;
    function IsAvailable: Boolean;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
  end;

{$ENDREGION 'Provider Interface'}

{$REGION 'Word Extraction Helpers'}

type
  TWordInfo = record
    Word: string;
    StartChar: Integer; // 1-based
    EndChar: Integer;   // 1-based, exclusive
  end;

function IsSpellWordBreakChar(C: WideChar): Boolean; inline;
function ExtractWords(const ALine: string): TArray<TWordInfo>;
function ContainsLetter(const S: string): Boolean;

{$ENDREGION 'Word Extraction Helpers'}

implementation

uses
  System.Character;

function IsSpellWordBreakChar(C: WideChar): Boolean;
begin
  case C of
    'A'..'Z', 'a'..'z', '0'..'9', '_', '''': Result := False;
  else
    Result := True;
  end;
end;

function ExtractWords(const ALine: string): TArray<TWordInfo>;
var
  I, Len, WordStart: Integer;
  List: TList<TWordInfo>;
  Info: TWordInfo;
begin
  Len := Length(ALine);
  List := TList<TWordInfo>.Create;
  try
    I := 1;
    while I <= Len do
    begin
      // Skip non-word characters
      while (I <= Len) and IsSpellWordBreakChar(ALine[I]) do
        Inc(I);
      if I > Len then Break;
      // Start of a word
      WordStart := I;
      while (I <= Len) and not IsSpellWordBreakChar(ALine[I]) do
        Inc(I);
      Info.StartChar := WordStart;
      Info.EndChar := I;
      Info.Word := Copy(ALine, WordStart, I - WordStart);
      if Info.Word.Length > 0 then
        List.Add(Info);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function ContainsLetter(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I].IsLetter then
      Exit(True);
  Result := False;
end;

end.
