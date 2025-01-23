{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditRegexSearch.pas, released 2002-07-26.

Original Code by Eduardo Mauro, Gerald Nunn and Flávio Etrusco.
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
  - ssoWholeWords is not used
  - backward search may not work as expected if you start the search within a
    match (e.g.  search for 'aa' in 'aaa^a' where ^ is the current position)
-------------------------------------------------------------------------------}

unit SynEditRegexSearch;

{$I SynEdit.inc}

interface

uses
  SynEditTypes,
  RegularExpressions,
  RegularExpressionsCore,
  SynEditMiscClasses,
  SynUnicode,
  Classes;

type
  TSynEditRegexSearch = class(TSynEditSearchCustom)
  private
    RegEx: TRegEx;
    fMatchCollection: TMatchCollection;
    fOptions: TRegExOptions;
    fPattern: string;
    fResultCount: Integer;
  protected
    function GetPattern: string; override;
    procedure SetPattern(const Value: string); override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    function FindAll(const NewText: string; StartChar: Integer = 1;
      EndChar: Integer = 0): Integer; override;
    function PreprocessReplaceExpression(const AReplace: string): string; override;
    function Replace(const aOccurrence, aReplacement: string): string; override;
  end;

  ESynRegEx = ERegularExpressionError;

implementation

uses
  RegularExpressionsAPI,
  System.SysUtils,
  Consts;

{$IF (CompilerVersion <= 35) and not Declared(RTLVersion112)}
type
  { TPerlRegExHelper }

  TPerlRegExHelper = class helper for TPerlRegEx
    procedure AddRawOptions(PCREOptions: Integer);
  end;

procedure TPerlRegExHelper.AddRawOptions(PCREOptions: Integer);
begin
  with Self do FPCREOptions := FPCREOptions or PCREOptions;
end;

type
  TRegExHelper = record helper for TRegEx
  public
    procedure AddRawOptions(PCREOptions: Integer);
  end;

procedure TRegExHelper.AddRawOptions(PCREOptions: Integer);
begin
  with Self do FRegEx.AddRawOptions(PCREOptions);
end;
{$ENDIF}

{ TSynEditRegexSearch }

constructor TSynEditRegexSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOptions := [];
end;

function TSynEditRegexSearch.FindAll(const NewText: string;
  StartChar: Integer = 1; EndChar: Integer = 0): Integer;
begin
  if NewText = '' then
    Exit(0);

  fMatchCollection :=  RegEx.Matches(NewText, StartChar);

  Result := fMatchCollection.Count;
  if (EndChar > 0) and (EndChar < NewText.Length + 1) then
    // Exclude results beyond EndChar
    while (Result > 0) and ((fMatchCollection[Result - 1].Index + fMatchCollection[Result - 1].Length) > EndChar) do
      Dec(Result);
  fResultCount := Result;
end;

// replace new line and tab symbol to real chars
function TSynEditRegexSearch.PreprocessReplaceExpression(
  const AReplace: string): string;
begin
  Result := StringReplace(AReplace, '\n', WideCRLF, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
end;

function TSynEditRegexSearch.Replace(const aOccurrence, aReplacement: string): string;
begin
  Result := RegEx.Replace(aOccurrence, aReplacement);
end;

function TSynEditRegexSearch.GetLength(Index: Integer): Integer;
begin
  Result := fMatchCollection[Index].Length;
end;

function TSynEditRegexSearch.GetPattern: string;
begin
  Result := fPattern;
end;

function TSynEditRegexSearch.GetResult(Index: Integer): Integer;
begin
  Result := fMatchCollection[Index].Index;
end;

function TSynEditRegexSearch.GetResultCount: Integer;
begin
  Result := fResultCount;
end;

procedure TSynEditRegexSearch.SetOptions(const Value: TSynSearchOptions);
begin
  if ssoMatchCase in Value then
    fOptions := []
  else
    fOptions := [roIgnoreCase];
  RegEx := TRegEx.Create(fPattern, fOptions);
  RegEx.AddRawOptions(PCRE_UCP);
end;

procedure TSynEditRegexSearch.SetPattern(const Value: string);
begin
  fPattern := Value;
  RegEx := TRegEx.Create(fPattern, fOptions);
  RegEx.AddRawOptions(PCRE_UCP);
end;

end.

