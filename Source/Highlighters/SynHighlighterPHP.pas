{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPHP.pas, released 2000-04-21.
The Original Code is based on the wmPHPSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Willo van der Merwe.
"Heredoc" syntax highlighting implementation by Marko Njezic.
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
{
@abstract(Provides a PHP syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterPHP unit provides SynEdit with a PHP syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterPHP;

{$I SynEdit.inc}

interface

uses
  Graphics,
  System.Win.Registry,
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  SynUnicode,
  Classes,
//++ CodeFolding
  SynEditCodeFolding;
//++ CodeFolding

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkDocument, tkIdentifier, tkNull,
    tkNumber, tkSpace, tkString, tkUnknown, tkVariable);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsString96, rsComment, rsDocument, rsVarExpansion,
    rsHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsString96, rsComment, rsDocument, rsVarExpansion);
{$ENDIF}

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
//  TSynPHPSyn = class(TSynCustomHighlighter)
  TSynPHPSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength: Byte;
    fHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..438] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fDocumentAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure MultiplyProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PoundProc;
    procedure RemainderSymbolProc;
    procedure SymbolProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure VarExpansionProc;
    procedure VariableProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure AnsiCProc;
    procedure String39Proc;
    procedure String34Proc;
    procedure String96Proc;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
//++ CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
//-- CodeFolding
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DocumentAttri: TSynHighlighterAttributes read fDocumentAttri
      write fDocumentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst,
  Windows;

const

{ expanded keyword list }
  KeyWords: array[0..110] of string = (
    '__autoload', '__call', '__callstatic', '__class__', '__clone',
    '__construct', '__debuginfo', '__destruct', '__dir__', '__file__',
    '__function__', '__get', '__halt_compiler', '__invoke', '__isset',
    '__line__', '__method__', '__namespace__', '__set', '__set_state',
    '__sleep', '__tostring', '__trait__', '__unset', '__wakeup', 'abstract',
    'and', 'array', 'as', 'binary', 'bool', 'boolean', 'break', 'callable',
    'case', 'catch', 'cfunction', 'class', 'clone', 'const', 'continue',
    'declare', 'default', 'die', 'do', 'double', 'echo', 'else', 'elseif',
    'empty', 'enddeclare', 'endfor', 'endforeach', 'endif', 'endswitch',
    'endwhile', 'eval', 'exception', 'exit', 'extends', 'false', 'final',
    'finally', 'float', 'for', 'foreach', 'function', 'global', 'goto', 'if',
    'implements', 'include', 'include_once', 'instanceof', 'insteadof', 'int',
    'integer', 'interface', 'isset', 'list', 'mixed', 'namespace', 'new',
    'null', 'object', 'old_function', 'or', 'parent', 'print', 'private',
    'protected', 'public', 'real', 'require', 'require_once', 'return', 'self',
    'static', 'string', 'switch', 'throw', 'trait', 'true', 'try', 'unset',
    'use', 'var', 'void', 'while', 'xor', 'yield'
  );

  KeyIndices: array[0..438] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, -1, -1, -1,
    25, -1, 110, -1, -1, 72, 88, -1, 67, -1, -1, -1, -1, -1, -1, -1, -1, 75, -1,
    -1, 38, -1, 80, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, 53, -1, 102,
    43, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 52, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 104, -1, 40, -1, -1, -1, -1, -1, -1, -1, 76, -1, 101,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, 98, -1, -1, 64, -1,
    -1, -1, 37, -1, 29, -1, -1, -1, 36, 60, -1, -1, -1, -1, 54, -1, -1, -1, -1,
    -1, -1, -1, -1, 27, -1, 32, -1, 51, 94, -1, 73, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 79, -1, -1, -1, 10, -1, -1, -1, -1, -1, 96, -1, -1, 9, -1, 100, -1,
    -1, -1, -1, -1, 57, -1, -1, -1, -1, -1, -1, 85, -1, -1, -1, 39, -1, -1, 109,
    0, 44, -1, -1, -1, 78, -1, -1, -1, 65, 19, -1, 81, -1, 46, 1, 6, -1, 69, -1,
    14, -1, -1, 35, -1, 71, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1, -1, -1, 50,
    -1, -1, -1, -1, -1, 4, -1, 20, 8, -1, -1, 47, 7, -1, 68, -1, 5, 49, -1, -1,
    -1, 93, -1, -1, 89, -1, -1, -1, 34, -1, 59, -1, 86, -1, -1, 103, -1, -1, -1,
    -1, -1, 26, -1, -1, -1, -1, -1, 23, -1, 83, 66, 58, -1, -1, -1, 63, -1, 45,
    -1, -1, -1, -1, -1, -1, -1, 70, -1, -1, 105, -1, -1, 62, -1, -1, -1, -1, -1,
    -1, 42, 82, 24, 30, -1, -1, -1, 97, -1, -1, -1, 17, -1, 74, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 106, -1, 22, 3, -1, -1, -1, 90, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 15, -1, -1, -1, -1, 41, -1, 16, -1, -1, -1, -1,
    -1, -1, -1, -1, 61, -1, -1, -1, -1, -1, -1, 77, -1, 84, -1, -1, 92, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 18, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    13, -1, 48, 12, -1, 28, -1, -1, -1, -1, -1, -1, 21, -1, 95, 108, 91, -1, -1,
    -1, -1, -1, 99, -1, -1, -1, -1, -1, 55, -1, -1, 107, 56, -1, -1, -1, -1, -1,
    87
  );

{$Q-}
function TSynPHPSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 155 + Ord(Str^) * 90;
    Inc(Str);
  end;
  Result := Result mod 439;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynPHPSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynPHPSyn.InitIdent;
var
  I: Integer;
begin
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[I] = -1 then
      fIdentFuncTable[I] := AltFunc;

  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[I] = nil then
      fIdentFuncTable[I] := KeyWordFunc;
end;

function TSynPHPSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPHPSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynPHPSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDocumentAttri := TSynHighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_FriendlyAttrDocumentation);
  fDocumentAttri.Style := [fsItalic];
  AddAttribute(fDocumentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterPHP;
  fRange := rsUnknown;
end;

procedure TSynPHPSyn.AndSymbolProc;
begin
  begin
    fTokenID := tkSymbol;
    case FLine[Run + 1] of
      '=':  Inc(Run, 2);                 {and assign}
      '&':  Inc(Run, 2);                 {conditional and}
    else                                 {and}
        Inc(Run);
    end;
  end;
end;

procedure TSynPHPSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
end;

procedure TSynPHPSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                  {logical equal}
    '>': Inc(Run, 2);                  {Hash operator}
  else                                 {assign}
      Inc(Run);
  end;
end;

procedure TSynPHPSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                   {greater than or equal to}
    '>': Inc(Run, 2);
  else                                 {greater than}
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do Inc(Run);
end;

procedure TSynPHPSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynPHPSyn.LowerProc;
{$IFDEF SYN_HEREDOC}
var
  I, Len: Integer;
{$ENDIF}
begin
    case FLine[Run + 1] of
      '=':                               {less than or equal to}
        begin
          Inc(Run, 2);
          fTokenID := tkSymbol;
        end;
      '<':
        begin
          fTokenID := tkSymbol;
{$IFDEF SYN_HEREDOC}
          if (FLine[Run + 2] = '<') and IsIdentChar(FLine[Run + 3]) then
          begin
            Inc(Run, 3);

            I := Run;
            while IsIdentChar(FLine[I]) do Inc(I);
            Len := I - Run;

            if Len > 255 then
            begin
              fTokenID := tkUnknown;
              Exit;
            end;

            fRange := rsHeredoc;
            fHeredocLength := Len;
            fHeredocChecksum := CalcFCS(FLine[Run], Len);

            Inc(Run, Len);
            fTokenID := tkString;
          end
          else
{$ENDIF}
          if FLine[Run + 2] = '=' then   {shift left assign}
          begin
            Inc(Run, 3)
          end
          else                           {shift left}
          begin
            Inc(Run, 2);
          end;
        end;
    else                                 {less than}
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    end;
end;

procedure TSynPHPSyn.MinusProc;
begin
    fTokenID := tkSymbol;
    case FLine[Run + 1] of
      '=': Inc(Run, 2);                  {subtract assign}
      '-': Inc(Run, 2);                  {decrement}
      '>': Inc(Run, 2);                  {Class operator}
    else                                 {subtract}
      Inc(Run);
    end;
end;

procedure TSynPHPSyn.MultiplyProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                  {multiply assign}
  else                                 {multiply}
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.NotSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                  {not equal}
  else                                 {logical complement}
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynPHPSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', '-', 'l', 'L', 'x', 'X', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                  {inclusive or assign}
    '|': Inc(Run, 2);                  {conditional or}
  else                                 {inclusive or}
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                  {add assign}
    '|': Inc(Run, 2);                  {increment}
  else                                 {add}
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.PoundProc;
begin
  repeat
    Inc(Run);
  until IsLineEnd(Run);
  fTokenID := tkComment;
end;

procedure TSynPHPSyn.RemainderSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                  {remainder assign}
  else                                 {remainder}
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

//-- CodeFolding
procedure TSynPHPSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: string;
  Line: Integer;

  function LineHasChar(Line: Integer; character: char;
  StartCol: Integer): Boolean; // faster than Pos!
  var
    I: Integer;
  begin
    Result := False;
    for I := StartCol to Length(CurLine) do begin
      if CurLine[I] = character then begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, I) <> fCommentAttri then begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  function FindBraces(Line: Integer): Boolean;
  var
    Col: Integer;
  begin
    Result := False;

    for Col := 1 to Length(CurLine) do
    begin
      // We've found a starting character
      if CurLine[col] = '{' then
      begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, Col) <> fCommentAttri then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '}', col + 1) then begin
            FoldRanges.StartFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          Break;
        end;
      end else if CurLine[col] = '}' then
      begin
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, Col) <> fCommentAttri then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '{', col + 1) then begin
            FoldRanges.StopFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          Break;
        end;
      end;
    end; // for Col
  end;

  function FoldRegion(Line: Integer): Boolean;
  var
    S: string;
  begin
    Result := False;
    S := TrimLeft(CurLine);
    if Uppercase(Copy(S, 1, 9)) = '//#REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 12)) = '//#ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline comments (Fold Type 2)
    if TRangeState(GetLineRange(LinesToScan, Line)) in [rsComment, rsDocument] then
    begin
      if not (TRangeState(GetLineRange(LinesToScan, Line - 1)) in [rsComment, rsDocument]) then
        FoldRanges.StartFoldRange(Line + 1, 2)
      else
        FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end
    else if TRangeState(GetLineRange(LinesToScan, Line - 1)) in [rsComment, rsDocument] then
    begin
      FoldRanges.StopFoldRange(Line + 1, 2);
      Continue;
    end;

    CurLine := LinesToScan[Line];

    // Skip empty lines
    if CurLine = '' then begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // Find Fold regions
    if FoldRegion(Line) then
      Continue;

    // Find an braces on this line  (Fold Type 1)
    if not FindBraces(Line) then
      FoldRanges.NoFoldInfo(Line + 1);
  end; // while Line
end;
//-- CodeFolding

procedure TSynPHPSyn.SlashProc;
begin
    case FLine[Run + 1] of
      '/':                               {c++ style comments}
        begin
          Inc(Run, 2);
          fTokenID := tkComment;
          while not IsLineEnd(Run) do
            Inc(Run);
        end;
      '*':
        begin
          if (fLine[Run+2] = '*') and (fLine[Run+3] <> '/') then     {documentation comment}
          begin
            fRange := rsDocument;
            fTokenID := tkDocument;
            Inc(Run);
          end
          else                           {c style comment}
          begin
            fRange := rsComment;
            fTokenID := tkComment;
            Inc(Run);
          end;

          Inc(Run);
          while not IsLineEnd(Run) do
            if fLine[Run] = '*' then
            begin
              if fLine[Run + 1] = '/' then
              begin
                fRange := rsUnKnown;
                Inc(Run, 2);
                Break;
              end
              else
                Inc(Run)
            end
            else
              Inc(Run);
        end;
      '=':                               {division assign}
        begin
          Inc(Run, 2);
          fTokenID := tkSymbol;
        end;
    else                                 {division}
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    end;
end;

procedure TSynPHPSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynPHPSyn.StringProc;

  function IsEscaped: Boolean;
  var
    iFirstSlashPos: Integer;
  begin
    iFirstSlashPos := Run -1;
    while (iFirstSlashPos > 0) and (FLine[iFirstSlashPos] = '\') do
      Dec(iFirstSlashPos);
    Result := (Run - iFirstSlashPos + 1) mod 2 <> 0;
  end;

var
  iCloseChar: WideChar;
begin
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;
  case fRange of
    rsString39: iCloseChar := #39;
    rsString34: iCloseChar := #34;
    rsString96: iCloseChar := '`';
  else
    iCloseChar := #0;
  end;
  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = iCloseChar) and not IsEscaped then
      Break;
    if (FLine[Run] = '$') and (iCloseChar = '"') and
      ((FLine[Run + 1] = '{') or IsIdentChar(FLine[Run + 1])) then
    begin
      if (Run > 1) and (FLine[Run -1] = '{') then { complex syntax }
        Dec(Run);
      if not IsEscaped then
      begin
        { Break the token to process the variable }
        fRange := rsVarExpansion;
        Exit;
      end
      else if FLine[Run] = '{' then
        Inc(Run); { restore Run if we previously deincremented it }
    end;
    Inc(Run);
  end;
  if (FLine[Run] = iCloseChar) then
    fRange := rsUnKnown;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynPHPSyn.VarExpansionProc;
type
  TExpansionSyntax = (esNormal, esComplex, esBrace);
var
  iSyntax: TExpansionSyntax;
  iOpenBraces: Integer;
  iOpenBrackets: Integer;
  iTempRun: Integer;
begin
  fRange := rsString34; { var expansion only occurs in double quoted strings }
  FTokenID := tkVariable;
  if FLine[Run] = '{' then
  begin
    iSyntax := esComplex;
    Inc(Run, 2); { skips '{$' }
  end
  else
  begin
    Inc( Run );
    if FLine[Run] = '{' then
    begin
      iSyntax := esBrace;
      Inc(Run);
    end
    else
      iSyntax := esNormal;
  end;
  if iSyntax in [esBrace, esComplex] then
  begin
    iOpenBraces := 1;
    while not IsLineEnd(Run) do
    begin
      if FLine[Run] = '}' then
      begin
        Dec(iOpenBraces);
        if iOpenBraces = 0 then
        begin
          Inc(Run);
          Break;
        end;
      end;
      if FLine[Run] = '{' then
        Inc(iOpenBraces);
      Inc(Run);
    end;
  end
  else
  begin
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
    iOpenBrackets := 0;
    iTempRun := Run;
    { process arrays and objects }
    while not IsLineEnd(iTempRun) do
    begin
      if FLine[iTempRun] = '[' then
      begin
        Inc( iTempRun );
        if FLine[iTempRun] = #39 then
        begin
          Inc(iTempRun);
          while not IsLineEnd(iTempRun) and (FLine[iTempRun] <> #39) do
            Inc(iTempRun);
          if (FLine[iTempRun] = #39) and (fLine[iTempRun +1 ] = ']') then
          begin
            Inc(iTempRun, 2);
            Run := iTempRun;
            Continue;
          end
          else
            Break;
        end
        else
          Inc(iOpenBrackets);
      end
      else if (FLine[iTempRun] = '-') and (FLine[iTempRun +1] = '>') then
        Inc(iTempRun, 2)
      else
        Break;

      if not IsIdentChar(FLine[iTempRun]) then
        Break
      else
        repeat
          Inc(iTempRun);
        until not IsIdentChar(FLine[iTempRun]);

      while FLine[iTempRun] = ']' do
      begin
        if iOpenBrackets = 0 then
          Break;
        Dec(iOpenBrackets);
        Inc(iTempRun);
      end;
      if iOpenBrackets = 0 then
        Run := iTempRun;
    end;
  end;
end;

procedure TSynPHPSyn.VariableProc;
begin
{begin}
  if IsIdentChar(fLine[Run+1]) then
  begin
    Inc(Run);
    { checking function name }
    if fLine[Run-1] = '@' then
    begin
      fTokenID := IdentKind((fLine + Run));
      { isn't function, must be variable }
      if FTokenID = tkIdentifier then
        fTokenID := tkVariable;
    end
    { rest are variables }
    else
      fTokenID := tkVariable;
    while IsIdentChar(fLine[Run]) do
    begin
      Inc(Run);
    end;
  end
  else
  begin
    fTokenID := tkSymbol;
    Inc(Run);
  end;
{end}
end;

procedure TSynPHPSyn.XOrSymbolProc;
begin
  fTokenID := tkSymbol;
  case FLine[Run + 1] of
    '=': Inc(Run, 2);                  {xor assign}
  else                                 {xor}
    Inc(Run);
  end;
end;

procedure TSynPHPSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPHPSyn.AnsiCProc;
begin
  if fRange = rsComment then
    fTokenID := tkComment
  else
    fTokenID := tkDocument;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '*' then
    begin
      if fLine[Run + 1] = '/' then
      begin
        Inc(Run, 2);
        fRange := rsUnKnown;
        Break;
      end
      else
        Inc(Run);
    end
    else
      Inc(Run);
end;

procedure TSynPHPSyn.String39Proc;
begin
  fRange := rsString39;
  Inc( Run );
  StringProc;
end;

procedure TSynPHPSyn.String34Proc;
begin
  fRange := rsString34;
  Inc( Run );
  StringProc;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynPHPSyn.HeredocProc;

  procedure SkipToEOL;
  begin
    case FLine[Run] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    else
      repeat
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;

var
  I: Integer;
begin
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;

  if Run = 0 then
  begin
    I := 0;

    while not (IsLineEnd(FLine[I]) or (FLine[I] = ';')) do
    begin
      if I > fHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(I);
    end;

    if I <> fHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[0], I) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      Run := I;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynPHPSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment, rsDocument: AnsiCProc;
    rsString39, rsString34: StringProc;
    rsVarExpansion: VarExpansionProc;
{$IFDEF SYN_HEREDOC}
    rsHeredoc: HeredocProc;
{$ENDIF}
    else
    begin
      fRange := rsUnknown;
      NextProcedure;
    end;
  end;

  // ensure that one call of Next is enough to reach next token
  if (fOldRun = Run) and not GetEol then Next;

  inherited;
end;

procedure TSynPHPSyn.NextProcedure;
begin
  case fLine[Run] of
    '&': AndSymbolProc;
    #39: String39Proc; // single quote
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    '=': EqualProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    '<': LowerProc;
    '-': MinusProc;
    '*': MultiplyProc;
    '!': NotSymbolProc;
    '0'..'9': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '#': PoundProc;
    '%': RemainderSymbolProc;
    '(', ')', '{', '}', '[', ']', '@', ':', ',', '.', '\', '?', '~', ';': SymbolProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34: String34Proc; // double quote
    '`': String96Proc;
    '$': VariableProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
end;

function TSynPHPSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPHPSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPHPSyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange = rsHeredoc then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynPHPSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPHPSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDocument: Result := fDocumentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynPHPSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynPHPSyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynPHPSyn.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  fRange := TRangeState(RangePointer.Range);
  fHeredocLength := 0;
  fHeredocChecksum := 0;
  if fRange = rsHeredoc then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

function TSynPHPSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPHP;
end;

class function TSynPHPSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPHP;
end;

function TSynPHPSyn.GetSampleSource: string;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  $number = 1234;'#13#10+
            '  print "The number is $number";'#13#10+
            '  for ($i = 0; $i <= $number; $i++)'#13#10+
            '  {'#13#10+
            '    $x++;'#13#10+
            '    $x--;'#13#10+
            '    $x += 1.0;'#13#10+
            '  }'#13#10+
            '}';

end;

class function TSynPHPSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangPHP;
end;

function TSynPHPSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or CharInSet(AChar, ['_', '$']);
end;

function TSynPHPSyn.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  case AChar of
    #0..#32, '.', ',', ';', ':', '"', '''', '+', '`', '-', '^', '!', '?', '&',
    '@', '§', '%', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>',
    '=', '*', '/', '\', '|':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TSynPHPSyn.String96Proc;
begin
  fRange := rsString96;
  Inc( Run );
  StringProc;
end;

initialization
  RegisterPlaceableHighlighter(TSynPHPSyn);
end.
