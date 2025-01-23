{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVBScript.pas, released 2000-04-18.
The Original Code is based on the lbVBSSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Luiz C. Vaz de Brito.
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
@abstract(Provides a VBScript highlighter for SynEdit)
@author(Luiz C. Vaz de Brito, converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(20 January 1999, converted to SynEdit April 18, 2000)
@lastmod(2000-06-23)
The SynHighlighterVBScript unit provides SynEdit with a VisualBasic Script (.vbs) highlighter.
Thanks to Primoz Gabrijelcic and Martin Waldenburg.
}

unit SynHighlighterVBScript;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  Vcl.Graphics,
  SynEditTypes,
  SynEditHighlighter,
  System.RegularExpressions,
  SynEditCodeFolding;

const
  SYNS_AttrConst = 'Constant';

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkConst, tkIdentifier, tkNull,
    tkNumber, tkSpace, tkString, tkFunction, tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynVBScriptSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fCOnstAttri: TSynHighlighterAttributes;
    FKeywords: TDictionary<string, TtkTokenKind>;
    RE_BlockBegin: TRegEx;
    RE_BlockEnd: TRegEx;
    procedure DoAddKeyword(AKeyword: string; AKind: Integer);
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure ApostropheProc;
    procedure CRProc;
    procedure DateProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure REMProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
    procedure AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property ConstAttri: TSynHighlighterAttributes read fCOnstAttri
      write fConstAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
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
  SynEditMiscProcs,
  SynEditStrConst;

const
  Keywords: string =
	'and, as, boolean, byref, byte, byval, call, case, class, const, currency,' +
	'default, description , dim, do, double, each, else, elseif, empty, end,' +
	'endif, enum, eqv, error, event, exit, explicit, false, firstindex , for,' +
	'function, get, global , goto, helpcontext , helpfile , if, ignorecase, imp,' +
	'implements, in, integer, is, length , let, like, long, loop, lset, me, mod,' +
	'new, next, not, nothing, null, number , on, option, optional, or, paramarray,' +
	'pattern, preserve, private, property, public, raiseevent, redim, resume,' +
	'rset, select, set, shared, single, source , static, sub, submatches, then,' +
	'to, true, type, typeof, until, value, variant, wend, while, with, xor';

  FunctionConsts: string =
	'vbabort, vbabortretryignore, vbapplicationmodal, vbarray, vbbinarycompare,' +
	'vbblack, vbblue, vbboolean, vbbyte, vbcancel, vbcr, vbcritical, vbcrlf,' +
	'vbcurrency, vbcyan, vbdatabasecompare, vbdataobject, vbdate, vbdecimal,' +
	'vbdefaultbutton1, vbdefaultbutton2, vbdefaultbutton3, vbdefaultbutton4,' +
	'vbdouble, vbempty, vberror, vbexclamation, vbfalse, vbfirstfourdays,' +
	'vbfirstfullweek, vbfirstjan1, vbformfeed, vbfriday, vbgeneraldate, vbgreen,' +
	'vbignore, vbinformation, vbinteger, vblf, vblong, vblongdate, vblongtime,' +
	'vbmagenta, vbmonday, vbmsgboxhelpbutton, vbmsgboxright, vbmsgboxrtlreading,' +
	'vbmsgboxsetforeground, vbnewline, vbno, vbnull, vbnullchar, vbnullstring,' +
	'vbobject, vbobjecterror, vbok, vbokcancel, vbokonly, vbquestion, vbred,' +
	'vbretry, vbretrycancel, vbsaturday, vbshortdate, vbshorttime, vbsingle,' +
	'vbstring, vbsunday, vbsystemmodal, vbtab, vbtextcompare, vbthursday, vbtrue,' +
	'vbtuesday, vbusedefault, vbusesystem, vbusesystemdayofweek, vbvariant,' +
	'vbverticaltab, vbwednesday, vbwhite, vbyellow, vbyes, vbyesno, vbyesnocancel';

  Functions: string =
	'abs, anchor, array, asc, ascb, ascw, atn, cbool, cbyte, ccur, cdate, cdbl,' +
	'chr, chrb, chrw, cint, class_initialize, class_terminate, clear, clng, cos,' +
	'createcomponent, createobject, csng, cstr, date, dateadd,' +
	'datediff, datepart, dateserial, datevalue, day, debug, dictionary, document,' +
	'element, erase, err, escape, eval, execute, executeglobal, exp,' +
	'filesystemobject, filter, fix, form, formatcurrency, formatdatetime,' +
	'formatnumber, formatpercent, getlocale, getobject, getref,' +
	'getresource, hex, history, hour, inputbox, instr, instrb, instrrev, int,' +
	'isarray, isdate, isempty, isnull, isnumeric, isobject, join, lbound, lcase,' +
	'left, leftb, len, lenb, link, loadpicture, location, log, ltrim, mid, midb,' +
	'minute, month, monthname, msgbox, navigator, now, oct, raise, randomize,' +
	'regexp, rem, replace, rgb, right, rightb, rnd, round, rtrim, scriptengine,' +
	'scriptenginebuildversion, scriptenginemajorversion, scriptengineminorversion,' +
	'second, setlocale, sgn, sin, space, split, sqr, step, stop, strcomp, string,' +
	'strreverse, tan, test, textstream, time, timer, timeserial, timevalue, trim,' +
	'typename, ubound, ucase, unescape, vartype, weekday, weekdayname, window,' +
	'write, writeline, year';

procedure TSynVBScriptSyn.DoAddKeyword(AKeyword: string; AKind: Integer);
begin
  if not FKeywords.ContainsKey(AKeyword) then
    FKeywords.Add(AKeyword, TtkTokenKind(AKind));
end;

function TSynVBScriptSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  fToIdent := MayBe;
  while IsIdentChar(MayBe^) do
    Inc(Maybe);
  fStringLen := Maybe - fToIdent;
  SetString(S, fToIdent, fStringLen);
  if FKeywords.ContainsKey(S) then
    Result := FKeywords[S]
  else
    Result := tkIdentifier;
end;

constructor TSynVBScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;
  // Create the keywords dictionary case-insensitive
  FKeywords := TDictionary<string, TtkTokenKind>.Create(TIStringComparer.Ordinal);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fConstAttri := TSynHighlighterAttributes.Create(SYNS_AttrConst, SYNS_AttrConst);
  fConstAttri.Style := [fsbold];
  AddAttribute(fConstAttri);

  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  AddAttribute(fFunctionAttri);
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
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterVBScript;

  EnumerateKeywords(Ord(tkKey), KeyWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkConst), FunctionConsts, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), Functions, IsIdentChar, DoAddKeyword);

  RE_BlockBegin := CompiledRegEx('\b^(sub |function |private sub |private function |class )\b', [roIgnoreCase]);
  RE_BlockEnd := CompiledRegEx('\b^(end sub|end function|end class)\b', [roIgnoreCase]);
end;

destructor TSynVBScriptSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

const
  FT_Standard = 1;  // begin end, class end, record end
  FT_Comment = 11;
  FT_CodeDeclaration = 16;
  FT_CodeDeclarationWithBody = 17;
  FT_Implementation = 18;

procedure TSynVBScriptSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer);
var
  CurLine: string;
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
    if Uppercase(Copy(S, 1, 7)) = '''REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 10)) = '''ENDREGION' then
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

procedure TSynVBScriptSyn.AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings);
{
   Provide folding for procedures and functions included nested ones.
}
var
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
    else if FoldRanges.Ranges.List[i].FoldType = FT_CodeDeclaration then
    begin
      if ImplementationIndex >= 0 then begin
        // Code declaration in the Interface part of a unit
        FoldRanges.Ranges.Delete(i);
        Dec(ImplementationIndex);
        Continue;
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
              Continue;
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
                  Break
                end else
                begin
                  // class or record declaration follows, so
                  FoldRanges.Ranges.Delete(i);
                  Break;
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
              Break
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

procedure TSynVBScriptSyn.ApostropheProc;
begin
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynVBScriptSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynVBScriptSyn.DateProc;
begin
  fTokenID := tkString;
  repeat
    if IsLineEnd(Run) then Break;
    Inc(Run);
  until FLine[Run] = '#';
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynVBScriptSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynVBScriptSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynVBScriptSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynVBScriptSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(fLine[Run], ['=', '>']) then Inc(Run);
end;

procedure TSynVBScriptSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynVBScriptSyn.NumberProc;

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
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do Inc(Run);
end;

procedure TSynVBScriptSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynVBScriptSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then Break;
    Inc(Run);
  until FLine[Run] = #34;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynVBScriptSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVBScriptSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkIdentifier;
end;

procedure TSynVBScriptSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    #39: ApostropheProc;
    #13: CRProc;
    '#': DateProc;
    '>': GreaterProc;
    'A'..'Q', 'S'..'Z', 'a'..'q', 's'..'z', '_': IdentProc;
    'R', 'r': REMProc;
    #10: LFProc;
    '<': LowerProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34: StringProc;
    '&', '{', '}', ':', ',', '=', '^', '-',
    '+', '.', '(', ')', ';', '/', '*': SymbolProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynVBScriptSyn.GetDefaultAttribute(Index: Integer):
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

function TSynVBScriptSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynVBScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVBScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkConst: Result := fCOnstAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkFunction: Result := fFunctionAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynVBScriptSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynVBScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterVBScript;
end;

class function TSynVBScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangVBSScript;
end;

function TSynVBScriptSyn.GetSampleSource: string;
begin
  Result := ''' Syntax highlighting'#13#10 +
            'function printNumber()'#13#10 +
            '  number = 12345'#13#10 +
            '  document.write("The number is " + number)'#13#10 +
            '  for i = 0 to 10'#13#10 +
            '    x = x + 1.0'#13#10 +
            '  next'#13#10 +
            'end function';
end;

class function TSynVBScriptSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangVBSScript;
end;

procedure TSynVBScriptSyn.REMProc;
begin
  if CharInSet(FLine[Run+1], ['E', 'e']) and
    CharInSet(FLine[Run+2], ['M', 'm']) and (FLine[Run+3] <= #32) then
    ApostropheProc
  else
  begin
    fTokenID := tkIdentifier;
    IdentProc;
  end;
end;

initialization
  RegisterPlaceableHighlighter(TSynVBScriptSyn);
end.
