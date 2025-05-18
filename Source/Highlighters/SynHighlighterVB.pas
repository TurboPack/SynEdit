{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVB.pas, released 2000-04-20.
The Original Code is based on the wbADSP21xxSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Max Horv-th.
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

$Id: SynHighlighterVB.pas,v 1.14.2.6 2005/12/16 17:13:16 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Visual Basic highlighter for SynEdit)
@author(Max Horv-th <TheProfessor@gmx.de>, converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(5 December 1999, converted to SynEdit April 21, 2000)
@lastmod(2000-06-23)
The SynHighlighterVB unit provides SynEdit with a Visual Basic (.bas) highlighter.
}

unit SynHighlighterVB;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, Controls, Graphics, System.Win.Registry,
  SynEditHighlighter,
  SynEditTypes,
  SysUtils,
  SynUnicode,
  Classes,
//++ CodeFolding
  System.RegularExpressions,
  SynEditCodeFolding;
//++ CodeFolding

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber, tkSpace,
    tkString, tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
//  TSynVBSyn = class(TSynCustomHighlighter)
//++ CodeFolding
  TSynVBSyn = class(TSynCustomCodeFoldingHighlighter)
//-- CodeFolding
  private
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..1510] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
//++ CodeFolding
    RE_BlockBegin : TRegEx;
    RE_BlockEnd : TRegEx;
//-- CodeFolding
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncRem(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure SymbolProc;
    procedure ApostropheProc;
    procedure CRProc;
    procedure DateProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    procedure Next; override;
//++ CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
    procedure AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings); override;
//-- CodeFolding
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
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
  end;

implementation

uses
  SynEditStrConst;

const
  {added keywords: catch, try, import}
  KeyWords: array[0..225] of string = (
    'abs', 'and', 'appactivate', 'array', 'as', 'asc', 'atn', 'attribute',
    'base', 'beep', 'begin', 'boolean', 'byref', 'byte', 'byval', 'call',
    'case', 'catch', 'cbool', 'cbyte', 'ccur', 'cdate', 'cdbl', 'cint',
    'circle', 'class', 'clear', 'clng', 'close', 'command', 'compare', 'const',
    'cos', 'createobject', 'csng', 'cstr', 'curdir', 'currency', 'cvar',
    'cverr', 'date', 'dateadd', 'datediff', 'datepart', 'dateserial',
    'datevalue', 'ddb', 'deftype', 'dim', 'dir', 'do', 'doevents', 'double',
    'each', 'else', 'elseif', 'empty', 'end', 'enum', 'environ', 'eof', 'eqv',
    'erase', 'err', 'error', 'exit', 'exp', 'explicit', 'false', 'fileattr',
    'filecopy', 'filedatetime', 'filelen', 'fix', 'for', 'form', 'format',
    'freefile', 'friend', 'function', 'fv', 'get', 'getattr', 'getobject',
    'gosub', 'goto', 'hex', 'hour', 'chdir', 'chdrive', 'chr', 'if', 'iif',
    'imp', 'imports', 'input', 'instr', 'int', 'integer', 'ipmt', 'irr', 'is',
    'isarray', 'isdate', 'isempty', 'iserror', 'ismissing', 'isnull',
    'isnumeric', 'isobject', 'kill', 'lbound', 'lcase', 'left', 'len', 'let',
    'line', 'loc', 'local', 'lock', 'lof', 'log', 'long', 'loop', 'lset',
    'ltrim', 'me', 'mid', 'minute', 'mirr', 'mkdir', 'mod', 'module', 'month',
    'msgbox', 'name', 'new', 'next', 'not', 'nothing', 'now', 'nper', 'npv',
    'object', 'oct', 'on', 'open', 'option', 'or', 'pmt', 'ppmt', 'print',
    'private', 'property', 'pset', 'public', 'put', 'pv', 'qbcolor', 'raise',
    'randomize', 'rate', 'redim', 'rem', 'reset', 'resume', 'return', 'rgb',
    'right', 'rmdir', 'rnd', 'rset', 'rtrim', 'second', 'seek', 'select',
    'sendkeys', 'set', 'setattr', 'sgn', 'shell', 'sin', 'single', 'sln',
    'space', 'spc', 'sqr', 'static', 'step', 'stop', 'str', 'strcomp',
    'strconv', 'string', 'sub', 'switch', 'syd', 'system', 'tab', 'tan', 'then',
    'time', 'timer', 'timeserial', 'timevalue', 'to', 'trim', 'true', 'try',
    'typename', 'typeof', 'ubound', 'ucase', 'unlock', 'until', 'val',
    'variant', 'vartype', 'version', 'weekday', 'wend', 'while', 'width',
    'with', 'write', 'xor'
  );

  KeyIndices: array[0..1510] of Integer = (
    100, -1, -1, -1, -1, -1, -1, -1, 91, -1, -1, -1, 140, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1, 74,
    -1, -1, -1, 176, -1, -1, 181, 146, 171, -1, -1, -1, -1, 5, -1, -1, -1, -1,
    -1, -1, -1, -1, 184, 213, -1, -1, -1, 31, -1, -1, 145, -1, 28, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 141, -1, -1, -1, -1,
    203, 195, -1, -1, 159, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 225, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, -1, -1, -1, -1, -1,
    -1, 175, 211, -1, -1, -1, 186, -1, -1, 174, -1, -1, -1, -1, -1, -1, 149, -1,
    -1, -1, -1, 193, -1, 192, -1, -1, -1, -1, -1, -1, -1, -1, 180, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 169, -1, 131, -1, -1, -1, -1, -1, -1, 33, -1, 44, -1, 118, -1,
    -1, 54, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 150, -1, -1, -1, -1, -1,
    -1, -1, -1, 183, -1, -1, -1, -1, 167, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 107, 57, -1, -1, 173, -1, -1, -1, -1, 38,
    -1, 139, -1, -1, -1, -1, -1, -1, -1, -1, 185, -1, 125, -1, -1, -1, 201, -1,
    -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, 161, 130, -1, -1, -1, -1, -1, -1,
    -1, -1, 19, -1, -1, 119, -1, 36, -1, -1, -1, -1, 219, -1, -1, 114, -1, -1,
    -1, 56, -1, -1, -1, -1, 103, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 23, 190, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 214, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 116, -1, -1, -1, -1, -1, -1, -1, -1, 177, -1, -1,
    -1, 18, 222, -1, -1, 94, -1, -1, -1, -1, -1, -1, -1, 202, -1, -1, -1, -1,
    -1, -1, -1, -1, 188, -1, -1, -1, 223, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 47, -1, -1, -1, -1, -1, 3, 115, -1, -1, -1, -1, -1, 32, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 143, -1, -1, -1, -1, -1, -1, 154, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 191, -1, -1, -1, -1, -1, -1, -1, -1, -1, 95,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 16, 67, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 200, 182, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 132, -1, 134, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 217, -1, -1, -1, -1, -1, -1, 165, -1, -1, -1, -1,
    -1, 189, 72, 39, 187, -1, -1, -1, -1, -1, 194, 29, -1, -1, -1, 17, -1, -1,
    84, 79, -1, -1, -1, 40, 37, -1, 71, -1, -1, 55, -1, -1, -1, -1, -1, 75, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 166, -1, 80, 104, -1, -1, -1, 113, -1,
    -1, -1, -1, -1, 76, -1, -1, -1, 148, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 92, -1, -1, -1, -1, -1, -1, 66, -1, -1, -1, 178, -1, -1,
    147, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, 198,
    48, -1, -1, -1, 197, 216, -1, -1, -1, -1, -1, -1, -1, 70, -1, -1, 123, -1,
    -1, 204, -1, -1, -1, -1, -1, -1, 210, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 215, -1, -1, -1, -1, -1, -1, -1, -1, 0,
    -1, -1, -1, -1, 126, -1, -1, -1, -1, 12, -1, -1, -1, -1, 220, -1, 117, -1,
    -1, -1, -1, -1, 101, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 208, -1,
    -1, 22, 41, -1, -1, -1, -1, -1, 168, -1, 157, -1, -1, -1, -1, 162, -1, -1,
    -1, -1, -1, -1, -1, 34, -1, -1, -1, -1, -1, -1, -1, 52, -1, -1, -1, -1, -1,
    -1, -1, 82, -1, 120, -1, 61, -1, -1, -1, -1, -1, -1, -1, -1, 163, -1, 6, 86,
    -1, -1, -1, 164, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, 27, -1, -1, -1, -1,
    50, -1, -1, -1, -1, 69, -1, -1, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1,
    158, -1, 68, -1, -1, -1, -1, -1, 221, -1, -1, -1, -1, 64, -1, -1, -1, 83,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 42, -1, 179, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, -1, -1, -1, -1,
    -1, -1, 199, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 212, -1, 207, -1, -1, -1, -1, -1, -1, 122, -1, -1, -1, 99, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 160, -1, -1, -1, -1, 30, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 218, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    151, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 59, -1, -1, -1, -1, -1, -1, -1, -1, 144, -1, 53, -1,
    -1, -1, -1, -1, -1, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 96, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 106, -1, -1,
    -1, -1, -1, -1, 15, -1, -1, -1, -1, 135, -1, -1, -1, -1, -1, 156, -1, -1,
    105, -1, -1, -1, -1, 133, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 43, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 1, -1, 4, 20, -1, -1, -1, 155, -1, -1, -1, 170,
    -1, -1, -1, 2, -1, -1, -1, -1, -1, 73, -1, -1, 65, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 97, 172, 89, -1, -1, -1, 10, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 108, -1, -1, -1, 224, -1, -1, -1, -1, -1, 14, -1, -1, -1, 112,
    -1, -1, -1, 127, -1, -1, 63, -1, -1, -1, 124, -1, 209, -1, -1, -1, 206, -1,
    -1, -1, -1, -1, -1, -1, 85, -1, -1, -1, -1, -1, 81, -1, -1, -1, -1, -1, -1,
    -1, -1, 121, -1, -1, -1, -1, 136, -1, -1, -1, 51, -1, -1, -1, 58, -1, -1,
    110, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, -1, -1, -1, 88, -1, -1, -1,
    -1, 153, 62, -1, -1, -1, -1, -1, -1, 109, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 196, -1, -1, -1, -1, 137, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 205, -1, -1, 26, -1, -1, 21, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, -1, 9, 152, -1, 138, -1,
    -1, 128, 129, -1, -1, 78, -1, -1, -1, -1, -1, -1, 8, -1, -1, 90, 77, -1, -1,
    -1, -1, -1, 111, -1, 98, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 87, -1, -1, -1, -1
  );

{$Q-}
function TSynVBSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 573 + Ord(Str^) * 524;
    inc(Str);
  end;
  Result := Result mod 1511;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynVBSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynVBSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[842] := FuncRem;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;
end;

function TSynVBSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynVBSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

function TSynVBSyn.FuncRem(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    ApostropheProc;
    fStringLen := 0;
    Result := tkComment;
  end
  else
    Result := tkIdentifier;
end;

constructor TSynVBSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterVisualBASIC;

//++ CodeFolding
  RE_BlockBegin := TRegEx.Create('\b(sub |function |private sub |private function |if |for |select case)\b', [roIgnoreCase]);
  RE_BlockEnd := TRegEx.Create('\b(end sub|end function|end if|next|end select)\b', [roIgnoreCase]);
//-- CodeFolding
end;

//++ CodeFolding
Const
  FT_Standard = 1;  // begin end, class end, record end
  FT_Comment = 11;
  FT_CodeDeclaration = 16;
  FT_CodeDeclarationWithBody = 17;
  FT_Implementation = 18;

procedure TSynVBSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer);
var
  CurLine: String;
  Line: Integer;
  ok: Boolean;

  function BlockDelimiter(Line: Integer): Boolean;
  var
    Index: Integer;
    mcb: TMatchCollection;
    mce: TMatchCollection;
    match: TMatch;
  begin
    Result := False;

    mcb := RE_BlockBegin.Matches(CurLine);
    if mcb.Count > 0 then
    begin
      // Char must have proper highlighting (ignore stuff inside comments...)
      Index :=  mcb.Item[0].Index;
      if GetHighlighterAttriAtRowCol(LinesToScan, Line, Index) <> fCommentAttri then
      begin
        ok := False;
        // And ignore lines with both opening and closing chars in them
        for match in Re_BlockEnd.Matches(CurLine) do
          if match.Index > Index then
          begin
            OK := True;
            Break;
          end;
        if not OK then begin
          FoldRanges.StartFoldRange(Line + 1, FT_Standard);
          Result := True;
        end;
      end;
    end
    else
    begin
      mce := RE_BlockEnd.Matches(CurLine);
      if mce.Count > 0 then
      begin
        Index :=  mce.Item[0].Index;
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, Index) <> fCommentAttri then
        begin
          FoldRanges.StopFoldRange(Line + 1, FT_Standard);
          Result := True;
        end;
      end;
    end;
  end;

  function FoldRegion(Line: Integer): Boolean;
  var
    S: string;
  begin
    Result := False;
    S := TrimLeft(CurLine);
    if Uppercase(Copy(S, 1, 7)) = '#REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 11)) = '#END REGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline statements

    CurLine := LinesToScan[Line];

    // Skip empty lines
    if CurLine = '' then begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // Find Fold regions
    if FoldRegion(Line) then
      Continue;

    // Find begin or end  (Fold Type 1)
    if not BlockDelimiter(Line) then
      FoldRanges.NoFoldInfo(Line + 1);
  end; //for Line
end;

procedure TSynVBSyn.AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings);
{
   Provide folding for procedures and functions included nested ones.
}
Var
  i, j, SkipTo: Integer;
  ImplementationIndex: Integer;
  FoldRange: TSynFoldRange;
  mc: TMatchCollection;
begin
  ImplementationIndex := - 1;
  for i  := FoldRanges.Ranges.Count - 1 downto 0 do
  begin
    if FoldRanges.Ranges.List[i].FoldType = FT_Implementation then
      ImplementationIndex := i
    else
    if FoldRanges.Ranges.List[i].FoldType = FT_CodeDeclaration then
    begin
      if ImplementationIndex >= 0 then begin
        // Code declaration in the Interface part of a unit
        FoldRanges.Ranges.Delete(i);
        Dec(ImplementationIndex);
        continue;
      end;
      // Examine the following ranges
      SkipTo := 0;
      j := i + 1;
      while J < FoldRanges.Ranges.Count do begin
        FoldRange := FoldRanges.Ranges.List[j];
        Inc(j);
        case FoldRange.FoldType of
          // Nested procedure or function
          FT_CodeDeclarationWithBody:
            begin
              SkipTo := FoldRange.ToLine;
              continue;
            end;
          FT_Standard:
          // possibly begin end;
            if FoldRange.ToLine <= SkipTo then
              Continue
            else
            begin
              mc := RE_BlockBegin.Matches(LinesToScan[FoldRange.FromLine - 1]);
              if mc.Count > 0 then
              begin
                if mc.Item[0].Value.ToLower = 'begin' then
                begin
                  // function or procedure followed by begin end block
                  // Adjust ToLine
                  FoldRanges.Ranges.List[i].ToLine := FoldRange.ToLine;
                  FoldRanges.Ranges.List[i].FoldType := FT_CodeDeclarationWithBody;
                  break
                end else
                begin
                  // class or record declaration follows, so
                  FoldRanges.Ranges.Delete(i);
                  break;
                 end;
              end else
                Assert(False, 'TSynVBSSyn.AdjustFoldRanges');
            end;
        else
          begin
            if FoldRange.ToLine <= SkipTo then
              Continue
            else begin
              // Otherwise delete
              // eg. function definitions within a class definition
              FoldRanges.Ranges.Delete(i);
              break
            end;
          end;
        end;
      end;
    end;
  end;
  if ImplementationIndex >= 0 then
    // Looks better without it
    //FoldRanges.Ranges.List[ImplementationIndex].ToLine := LinesToScan.Count;
    FoldRanges.Ranges.Delete(ImplementationIndex);
end;
//-- CodeFolding

procedure TSynVBSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVBSyn.ApostropheProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynVBSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynVBSyn.DateProc;
begin
  fTokenID := tkString;
  repeat
    if IsLineEnd(Run) then break;
    inc(Run);
  until FLine[Run] = '#';
  if not IsLineEnd(Run) then inc(Run);
end;

procedure TSynVBSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynVBSyn.IdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynVBSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVBSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSynVBSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynVBSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do inc(Run);
end;

procedure TSynVBSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynVBSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    if IsLineEnd(Run) then break;
    inc(Run);
  until FLine[Run] = #34;
  if not IsLineEnd(Run) then inc(Run);
end;

procedure TSynVBSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynVBSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    '&': SymbolProc;
    #39: ApostropheProc;
    '}': SymbolProc;
    '{': SymbolProc;
    #13: CRProc;
    ':': SymbolProc;
    ',': SymbolProc;
    '#': DateProc;
    '=': SymbolProc;
    '^': SymbolProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': SymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '+': SymbolProc;
    '.': SymbolProc;
    ')': SymbolProc;
    '(': SymbolProc;
    ';': SymbolProc;
    '/': SymbolProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '*': SymbolProc;
    #34: StringProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynVBSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
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

function TSynVBSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynVBSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVBSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynVBSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynVBSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterVisualBASIC;
end;

class function TSynVBSyn.GetLanguageName: string;
begin
  Result := SYNS_LangVisualBASIC;
end;

function TSynVBSyn.GetSampleSource: string;
begin
  Result := ''' Syntax highlighting'#13#10+
            'Function PrintNumber'#13#10+
            '  Dim Number'#13#10+
            '  Dim X'#13#10+
            ''#13#10+
            '  Number = 123456'#13#10+
            '  Response.Write "The number is " & number'#13#10+
            ''#13#10+
            '  For I = 0 To Number'#13#10+
            '    X = X + &h4c'#13#10+
            '    X = X - &o8'#13#10+
            '    X = X + 1.0'#13#10+
            '  Next'#13#10+
            ''#13#10+
            '  I = I + @;  '' illegal character'#13#10+
            'End Function';
end;

class function TSynVBSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangVisualBASIC;
end;

initialization
  RegisterPlaceableHighlighter(TSynVBSyn);
end.
