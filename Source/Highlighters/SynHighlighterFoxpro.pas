{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterFoxpro.pas, released 2000-04-21.
The Original Code is based on the mwFoxproSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is "riceball".
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

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Foxpro Syntax highlighter for SynEdit)
@author(riceball <teditor@mailroom.com>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(2000, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterFoxpro unit provides SynEdit with a Foxpro syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterFoxpro;

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

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber, tkSpace,
    tkString, tkUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynFoxproSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    FKeywords: TDictionary<string, TtkTokenKind>;
    RE_BlockBegin: TRegEx;
    RE_BlockEnd: TRegEx;
    procedure DoAddKeyword(AKeyword: string; AKind: Integer);
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure AtSymbolProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure QuestionProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure XOrSymbolProc;
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
  KeyWords: string =
    '_curobj, _msysmenu, _pageno, _screen, _vfp, abs, accept, aclass, acopy, acos, ' +
    'acti, activate, adatabases, adbobjects, add, additive, adel, adir, aelement, ' +
    'aerror, afields, afont, after, again, ains, ainstance, alen, alias, alines, ' +
    'all, alltrim, alt, alter, alternate, amembers, and, ansi, ansitooem, any, ' +
    'aplabout, app, appe, append, application, aprinters, array, as, asc, ascan, ' +
    'ascending, ascii, aselobj, asin, asort, assert, asserts, assist, asubscript, ' +
    'at, at_c, atan, atc, atcc, atcline, atline, atn2, aused, auto, autoincerror, ' +
    'autosave, average, avg, backcolor, bar, barcount, barprompt, baseclass, before,' +
     'begin, bell, between, bintoc, bitand, bitclear, bitlshift, bitnot, bitor, ' +
    'bitrshift, bitset, bittest, bitxor, blan, blank, blink, blocksize, bof, border, ' +
     'bott, bottom, box, brow, browse, browseime, brstatus, build, by, calculate, ' +
    'call, cancel, candidate, capslock, caption, carry, case, catch, cd, cdow, cdx, ' +
    'ceiling, century, class, classlib, clear, clock, clos, close, cls, cmonth, cnt, ' +
     'cntbar, cntpad, codepage, col, collate, color, colorscheme, comm, command, ' +
    'commands, comment, compact, compatible, compile, compobj, confirm, connection, ' +
    'connections, connstring, console, continue, copy, cos, count, coverage, ' +
    'cpcompile, cpconvert, cpcurrent, cpdbf, cpdialog, cpnotrans, create, ' +
    'createobject, createoffline, ctobin, ctod, ctot, curdir, currency, cursor, ' +
    'curval, custom, cycle, database, databases, datasession, date, ' +
    'dateformat, datemark, datetime, day, dbalias, dbc, dbf, dbused, ddeaborttrans, ' +
    'ddeadvise, ddeenabled, ddeexecute, ddeinitiate, ddelasterror, ddepoke, ' +
    'dderequest, ddesetoption, ddesetservice, ddesettopic, ddeterminate, deactivate, ' +
     'debug, debugout, decimals, declare, default, define, dele, delete, ' +
     'deleted, delimite, delimited, delimiters, descending, desktop, development, ' +
    'device, dif, difference, dim, dimension, dir, directory, disable, ' +
    'diskspace, display, displayvalue, distinct, dll, dlls, dmy, do, dock, ' +
    'dodefault, doevents, dohistory, double, dow, drop, dtoc, dtor, dtos, dtot, ' +
    'each, edit, echo, eject, else, empty, enable, end, endcase, ' +
    'enddefine, enddo, endfor, endfunc, endif, endprintjob, endproc, endscan, ' +
    'endtext, endwith, enginebehavior, environment, eof, erase, error, escape, ' +
    'evaluate, event, eventhandler, eventlist, events, eventtracking, exact, except, ' +
     'exclusive, exe, exists, exit, exp, export, expression, extended, external, ' +
    'false, fclose, fcount, fcreate, fdow, feof, ferror, fetch, fflush, fgets, fchsize, ' +
    'field, fields, file, files, fill, filter, finally, find, fixed, fklabel, fkmax, ' +
     'float, flock, floor, flush, font, fontmetric, footer, fopen, for, force, ' +
    'foreign, form, format, found, fox2x, foxplus, foxpro), fputs, fread, free, ' +
    'freeze, from, fseek, fsize, fullpath, func, functi, function, fv, fw2, fweek, ' +
    'fwrite, gath, gather, general, get, getbar, getcolor, getcp, getdir, getenv, ' +
    'getexpr, getfile, getfldstate, getfont, getnextmodified, getobject, getpad, ' +
    'getpict, getprinter, gets, global, go, gomonth, gotfocus, goto, group, grow, ' +
    'halfheight, having, header, heading, headings, height, help, helpcontextid, ' +
    'helpfilter, hidden, hide, highlight, hour, hours, change, char, chdir, ' +
    'check, chr, chrsaw, chrtran, chrtranc, icon, id, idxcollate, if, ifdef, ifndef, ' +
     'iif, import, in, include, indbc, index, indexes, inkey, inlist, input, insert, ' +
     'insmode, int, integer, intensity, interval, into, is, isalpha, iscolor, ' +
    'isdigit, isexclusive, isflocked, islower, isnull, isreadonly, isrlocked, ' +
    'isupper, join, key, keyboard, keycomp, keymatch, label, last, lastkey, ledit, ' +
    'left, leftc, len, lenc, level, library, like, line, lineno, linked, list, ' +
     'load, loadpicture, local, locate, locfile, lock, lockscreen, log, log10, ' +
    'logerrors, logout, long, lookup, loop, lower, lparameter, lparameters, ' +
    'lpartition, ltrim, lupdate, macdesktop, mackey, macro, macros, machelp, margin, ' +
     'mark, master, max, mcol, md, mdown, mdx, mdy, memlines, memo, memory, memos, ' +
    'memowidth, memvar, menu, menus, message, messagebox, messages, middle, min, ' +
    'minimize, minute, mkdir, mline, mod, modal, modi, modify, module, month, mouse, ' +
     'movable, move, moved, mover, mrkbar, mrkpad, mrow, mtdll, mton, multilocks, ' +
    'multiselect, mvcount, mwindow, name, ndx, near, negotiate, network, newobject, ' +
    'next, noalias, noappend, noclear, noclose, noconsole, nocptrans, nodata, ' +
    'nodebug, nodefault, nodelete, noedit, noeject, noenvironment, nofloat, nogrow, ' +
    'noinit, nolgrid, nolink, nolock, nomargin, nomdi, nomenu, nominimize, nomodify, ' +
     'nomouse, none, nooptimize, nooverwrite, noprompt, noread, norefresh, ' +
    'norequery, norgrid, normal, normalize, nosave, noshadow, noshow, nospace, not, ' +
    'note, notify, noupdate, novalidate, noverify, nowait, nowindow, nowrap, nozoom, ' +
     'npv, ntom, null, nulldisplay, number, numlock, nvl, object, objects, ' +
    'objnum, objtoclient, objvar, occurs, odometer, oemtoansi, of, off, oldval, ' +
    'oleclass, olecontrol, oleobject, olepublic, on, only, open, optimize, or, ' +
    'order, os, otherwise, outer, overview, overwrite, pack, pad, padc, padl, padr, ' +
    'page, palette, panel, para, parameter, parameters, partition, path, payment, ' +
    'pcol, pdox, pdsetup, pen, pi, pictres, picture, pixels, plain, play, point, ' +
    'pop, popup, popups, preference, preview, primary, print, printer, printjob, ' +
    'printstatus, private, prmbar, prmpad, proc, proced, procedure, procedures, ' +
    'production, program, project, prompt, proper, protected, prow, prtinfo, public, ' +
     'push, putfile, pv, query, quit, rand, range, rat, ratc, ratline, rd, rdlevel, ' +
    'read, readborder, readerror, readkey, recall, reccount, recno, record, recover, ' +
     'recsize, redit, reference, references, refresh, region, regional, reindex, ' +
    'rela, relati, relation, relative, release, remote, remove, rename, repl, repla, ' +
     'replace, replicate, report, reprocess, requery, reset, resizable, resize, ' +
    'resource, resources, rest, restore, resume, retry, retu, return, rgb, ' +
    'rgbscheme, right, rightc, rightclick, righttoleft, rlock, rmdir, rollback, ' +
    'round, row, rtod, rtrim, run, runscript, runtime, safety, same, save, say, ' +
    'scan, scat, scatt, scatter, scols, scoreboard, screen, scroll, sdf, ' +
    'second, seconds, seek, sele, select, selected, selection, separator, ' +
    'set, shadow, shadows, shape, show, shutdown, scheme, ' +
    'schemes, sign, sin, single, sizable, size, skip, skpbar, skppad, some, sort, ' +
    'sorted, soundex, space, sql, sqlcommit, sqlrollback, sqlstringconnect, sqrt, ' +
    'srows, status, statusbartext, std, step, sticky, store, str, strconv, ' +
    'strictdate, string, strtran, structure, stuff, stuffc, style, sub, substr, substrc, ' +
    'sum, summary, suspend, sylk, sys, sysformats, sysmenu, sysmenus, sysmetric, ' +
    'system, tab, tabindex, table, tablerevert, tables, tableupdate, tablevalidate, ' +
    'tabstop, tag, talk, tan, target, text, textmerge, textwidth, then, this, thisform, ' +
    'thisformset, time, timeout, timer, title, titles, to, top, topic, total, ' +
    'transaction, transform, trap, trbetween, trigger, trim, true, try, ttoc, ttod, ' +
    'txnlevel, txtwidth, type, typeahead, udfparms, undefine, union, unique, unlock, ' +
     'unpack, until, update, updated, upper, use, used, val, valid, validate, value, ' +
    'values, var, varread, vartype, version, view, views, volume, wait, wborder, ' +
    'wcols, week, wexist, wfont, when, where, while, wchild, width, window, ' +
    'windowlist, windows, with, wk1, wk3, wks, wlast, wlcol, wlrow, wontop, ' +
    'workarea, woutput, wparent, wr1, wrap, wread, writeexpression, writemethod, ' +
    'wrk, wrows, wtitle, wvisible, xcmdfile, xl5, xls, year, zap, zoom, zorder, ' +
    'zorderset';

function TSynFoxProSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

constructor TSynFoxproSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;
  // Create the keywords dictionary case-insensitive
  FKeywords := TDictionary<string, TtkTokenKind>.Create(TIStringComparer.Ordinal);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
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
  fDefaultFilter := SYNS_FilterFoxpro;

  EnumerateKeywords(Ord(tkKey), KeyWords, IsIdentChar, DoAddKeyword);
  RE_BlockBegin := CompiledRegEx('\b(function |procedure )\b', [roIgnoreCase]);
  RE_BlockEnd := CompiledRegEx('\b(endproc|endfunc)\b', [roIgnoreCase]);
end;

const
  FT_Standard = 1;  // begin end, class end, record end
  FT_Comment = 11;
  FT_CodeDeclaration = 16;
  FT_CodeDeclarationWithBody = 17;
  FT_Implementation = 18;

procedure TSynFoxproSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
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
    if Uppercase(Copy(S, 1, 7)) = '*REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 11)) = '*ENDREGION' then
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

procedure TSynFoxproSyn.AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings);
{
   Provide folding for procedures and functions included nested ones.
}
var
  I, J, SkipTo: Integer;
  ImplementationIndex: Integer;
  FoldRange: TSynFoldRange;
  mc: TMatchCollection;
begin
  ImplementationIndex := - 1;
  for I  := FoldRanges.Ranges.Count - 1 downto 0 do
  begin
    if FoldRanges.Ranges.List[I].FoldType = FT_Implementation then
      ImplementationIndex := I
    else
    if FoldRanges.Ranges.List[I].FoldType = FT_CodeDeclaration then
    begin
      if ImplementationIndex >= 0 then begin
        // Code declaration in the Interface part of a unit
        FoldRanges.Ranges.Delete(I);
        Dec(ImplementationIndex);
        Continue;
      end;
      // Examine the following ranges
      SkipTo := 0;
      J := I + 1;
      while J < FoldRanges.Ranges.Count do begin
        FoldRange := FoldRanges.Ranges.List[J];
        Inc(J);
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
                  FoldRanges.Ranges.List[I].ToLine := FoldRange.ToLine;
                  FoldRanges.Ranges.List[I].FoldType := FT_CodeDeclarationWithBody;
                  Break
                end else
                begin
                  // class or record declaration follows, so
                  FoldRanges.Ranges.Delete(I);
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
              FoldRanges.Ranges.Delete(I);
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

procedure TSynFoxproSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '&':                               {Comments}
      begin
        Inc(Run, 2);
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: Break;
          end; //case
          Inc(Run);
        end;
      end;
  else                                 {and}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFoxproSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynFoxproSyn.AtSymbolProc;
begin
  fTokenID := tkKey;
  Inc(Run);
end;

procedure TSynFoxproSyn.BraceOpenProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      #92:
        if FLine[Run + 1] = #10 then Inc(Run);
    end;
    Inc(Run);
  until FLine[Run] = '}';
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynFoxproSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
    else Inc(Run);
  end;
end;

destructor TSynFoxproSyn.Destroy;
begin
  fKeywords.Free;
  inherited;
end;

procedure TSynFoxproSyn.DoAddKeyword(AKeyword: string; AKind: Integer);
begin
  if not FKeywords.ContainsKey(AKeyword) then
    FKeywords.Add(AKeyword, TtkTokenKind(AKind));
end;

procedure TSynFoxproSyn.ColonProc;
begin
  {colon}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.CommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFoxproSyn.GreaterProc;
begin
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
          Inc(Run, 3)
        else                           {shift right}
          Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFoxproSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do Inc(Run);
end;

procedure TSynFoxproSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynFoxproSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
          Inc(Run, 3)
        else                           {shift left}
          Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynFoxproSyn.MinusProc;
begin
  {subtract}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.ModSymbolProc;
begin
  {mod}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.NotSymbolProc;
begin
  {not}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynFoxproSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'x', 'X', 'e', 'E', 'f', 'F':
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

procedure TSynFoxproSyn.OrSymbolProc;
begin
  {or}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.PlusProc;
begin
  {subtract}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.PointProc;
begin
  if ((UpperCase(FLine[Run + 1]) = 'T') or      {.t.}
    (UpperCase(FLine[Run + 1]) = 'F')) and     {.f.}
    (FLine[Run + 2] = '.') then
  begin
    Inc(Run, 3);
    fTokenID := tkSymbol;
  end
  else if (((UpperCase(FLine[Run + 1]) = 'A') and
    (UpperCase(FLine[Run + 2]) = 'N') and
    (UpperCase(FLine[Run + 3]) = 'D')) or   {.and.}
    ((UpperCase(FLine[Run + 1]) = 'N') and
    (UpperCase(FLine[Run + 2]) = 'O') and
    (UpperCase(FLine[Run + 3]) = 'T'))) and   {.not.}
    (FLine[Run + 4] = '.') then
  begin
    Inc(Run, 5);
    fTokenID := tkSymbol;
  end
  else if (UpperCase(FLine[Run + 1]) = 'O') and
    (UpperCase(FLine[Run + 2]) = 'R') and
    (FLine[Run + 3] = '.') then  {.or.}
  begin
    Inc(Run, 4);
    fTokenID := tkSymbol;
  end
  else                                 {point}
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynFoxproSyn.QuestionProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.RoundCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.RoundOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SemiColonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SlashProc;
begin
  {division}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynFoxproSyn.SquareCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.SquareOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.StarProc;
begin
  if (Run = 0) or (Trim(Copy(fLine, 1, Run)) = '') then
  begin                        {Foxpro Comments}
    Inc(Run);
    fTokenID := tkComment;
    while FLine[Run] <> #0 do
    begin
      case FLine[Run] of
        #10, #13: Break;
      end;
      Inc(Run);
    end;
  end
  else
  begin
    {star}
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynFoxproSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      #92:
        if FLine[Run + 1] = #10 then Inc(Run);
    end;
    Inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynFoxproSyn.TildeProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynFoxproSyn.XOrSymbolProc;
begin
  {xor}
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynFoxproSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynFoxproSyn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    '&': AndSymbolProc;
    #39: AsciiCharProc;
    '@': AtSymbolProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '=': EqualProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '%': ModSymbolProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '.': PointProc;
    '?': QuestionProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    '*': StarProc;
    #34: StringProc;
    '~': TildeProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynFoxproSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynFoxproSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynFoxproSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynFoxproSyn.GetTokenAttribute: TSynHighlighterAttributes;
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

function TSynFoxproSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynFoxproSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterFoxpro;
end;

function TSynFoxproSyn.GetSampleSource: string;
begin
  Result :=
    '* Some sample Foxpro code to test highlighting' + #13#10 +
    'CLEAR ALL' + #13#10 +
    'SET CONFIRM  ON' + #13#10 +
    '' + #13#10 +
    'PROCEDURE ErrTrap' + #13#10 +
    'LPARAMETERS nLine, cProg, cMessage, cMessage1' + #13#10 +
    'OnError = ON("Error")' + #13#10 +
    'ON ERROR' + #13#10 +
    'IF NOT FILE ( [ERRORS.DBF] )' + #13#10 +
    '  CREATE TABLE ERRORS (  ;' + #13#10 +
    '  Date   Date,     ;' + #13#10 +
    '  Time   Char(5),   ;' + #13#10 +
    '  LineNum Integer,   ;' + #13#10 +
    '  ProgName Char(30),   ;' + #13#10 +
    '  Msg   Char(240),  ;' + #13#10 +
    '  CodeLine Char(240)   )' + #13#10 +
    'ENDIF' + #13#10 +
    'IF NOT USED ( [Errors] )' + #13#10 +
    '  USE ERRORS IN 0' + #13#10 +
    'ENDIF' + #13#10 +
    'SELECT Errors' + #13#10 +
    'INSERT INTO Errors VALUES ( ;' + #13#10 +
    ' DATE(), LEFT(TIME(),5), nLine, cProg, cMessage, cMessage1 )' + #13#10 +
    'USE IN Errors' + #13#10 +
    'cStr = [Error at line ] + TRANSFORM(nLine) + [ of ] + cprog + [:] + CHR(13)  ;' + #13#10 +
    '   + cMessage + CHR(13) + [Code that caused the error:] + CHR(13) + cMessage1' + #13#10 +
    'IF MESSAGEBOX( cStr, 292, [Continue] ) <> 6' + #13#10 +
    '  SET SYSMENU TO DEFAULT' + #13#10 +
    '  IF TYPE ( [_Screen.Title1] ) <> [U]' + #13#10 +
    '   _Screen.RemoveObject ( [Title2] )' + #13#10 +
    '   _Screen.RemoveObject ( [Title1] )' + #13#10 +
    '  ENDIF' + #13#10 +
    '  CLOSE ALL' + #13#10 +
    '  RELEASE ALL' + #13#10 +
    '  CANCEL' + #13#10 +
    ' ELSE' + #13#10 +
    '  ON ERROR &OnError' + #13#10 +
    'ENDIF';
end;

class function TSynFoxproSyn.GetLanguageName: string;
begin
  Result := SYNS_LangFoxpro;
end;

class function TSynFoxproSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangFoxpro;
end;

initialization
  RegisterPlaceableHighlighter(TSynFoxproSyn);
end.
