{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uHighlighterProcs.pas, released 2000-06-23.

The Initial Author of the Original Code is Michael Hieke.
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
-------------------------------------------------------------------------------}

unit uHighlighterProcs;

interface

uses
  System.Classes,
  SynEditHighlighter;

procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStringList;
  AppendToList: Boolean);
function GetHighlightersFilter(AHighlighters: TStringList): string;
function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;

implementation

uses
  System.SysUtils,
  SynHighlighterOmni;

procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStringList;
  AppendToList: Boolean);
var
  Comp: TComponent;
  Highlighter: TSynCustomHighlighter;
  LangName: string;
begin
  if Assigned(AOwner) and Assigned(AHighlighters) then begin
    if not AppendToList then
      AHighlighters.Clear;
    for Comp in AOwner do
    begin
      if not (Comp is TSynCustomHighlighter) then
        Continue;
      Highlighter := TSynCustomHighlighter(Comp);
      // Only one highlighter for each language
      // Omni highlighters are a special case
      if Highlighter is TSynOmniSyn then
        LangName := TSynOmniSyn(Highlighter).LangName
      else
        LangName := Highlighter.LanguageName;

      if LangName = '' then
        Continue;

      if AHighlighters.IndexOf(LangName) = -1 then
        AHighlighters.AddObject(LangName, Highlighter);
    end;
    AHighlighters.Sort;
  end;
end;

function GetHighlightersFilter(AHighlighters: TStringList): string;
var
  I: Integer;
  Highlighter: TSynCustomHighlighter;
begin
  Result := '';
  if Assigned(AHighlighters) then
    for I := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[I] is TSynCustomHighlighter) then
        Continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[I]);
      if Highlighter.DefaultFilter = '' then
        Continue;
      Result := Result + Highlighter.DefaultFilter;
      if Result[Length(Result)] <> '|' then
        Result := Result + '|';
    end;
end;

function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;
var
  ExtLen: Integer;
  I, J: Integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
begin
  Extension := LowerCase(Extension);
  ExtLen := Length(Extension);
  if Assigned(AHighlighters) and (ExtLen > 0) then begin
    for I := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[I] is TSynCustomHighlighter) then
        Continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[I]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      J := Pos('|', Filter);
      if J > 0 then begin
        Delete(Filter, 1, J);
        J := Pos(Extension, Filter);
        if (J > 0) and
           ((J + ExtLen > Length(Filter)) or (Filter[J + ExtLen] = ';'))
        then begin
          Result := Highlighter;
          Exit;
        end;
      end;
    end;
  end;
  Result := nil;
end;

end.
