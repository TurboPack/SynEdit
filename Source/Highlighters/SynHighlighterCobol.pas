{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterCobol.pas, released 2002-08-26.
Description: COBOL Syntax Parser/Highlighter
The author of this file is Andrey Ustinov.
Copyright (c) 2002 Software Mining, http://www.softwaremining.com/.
Unicode translation by Maël Hörz.
All rights reserved.

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

unit SynHighlighterCobol;

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
  TtkTokenKind = (
    tkBracket,
    tkKey,
    tkIndicator,
    tkComment,
    tkIdentifier,
    tkAIdentifier,
    tkPreprocessor,
    tkBoolean,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSequence,
    tkTagArea,
    tkDebugLines,
    tkUnknown);

  TRangeState = (rsUnknown,
                 rsQuoteString, rsApostString,
                 rsPseudoText,
                 rsQuoteStringMayBe, rsApostStringMayBe,
                 rsComment, rsDebug);

type
  TSynCobolSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIndicator: WideChar;

    fCodeStartPos: Integer;
    fCodeMediumPos: Integer;
    fCodeEndPos: Integer;

    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fAIdentifierAttri: TSynHighlighterAttributes;
    fPreprocessorAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fBooleanAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSequenceAttri: TSynHighlighterAttributes;
    fIndicatorAttri: TSynHighlighterAttributes;
    fTagAreaAttri: TSynHighlighterAttributes;
    fDebugLinesAttri: TSynHighlighterAttributes;
    fBracketAttri: TSynHighlighterAttributes;
    FKeywords: TDictionary<string, TtkTokenKind>;
    RE_BlockBegin: TRegEx;
    RE_BlockEnd: TRegEx;
    procedure DoAddKeyword(AKeyword: string; AKind: Integer);
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure NumberProc;
    procedure PointProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure StringEndProc;
    procedure FirstCharsProc;
    procedure LastCharsProc;
    procedure CommentProc;
    procedure InlineCommentProc;
    procedure DebugProc;
    procedure BracketProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;

    procedure SetCodeStartPos(Value: Integer);
    procedure SetCodeMediumPos(Value: Integer);
    procedure SetCodeEndPos(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
//++ CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
//-- CodeFolding
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property AreaAIdentifierAttri: TSynHighlighterAttributes read fAIdentifierAttri write fAIdentifierAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read fPreprocessorAttri write fPreprocessorAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property BooleanAttri: TSynHighlighterAttributes read fBooleanAttri write fBooleanAttri;
    property BracketAttri: TSynHighlighterAttributes read fBracketAttri write fBracketAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SequenceAttri: TSynHighlighterAttributes read fSequenceAttri write fSequenceAttri;
    property IndicatorAttri: TSynHighlighterAttributes read fIndicatorAttri write fIndicatorAttri;
    property TagAreaAttri: TSynHighlighterAttributes read fTagAreaAttri write fTagAreaAttri;
    property DebugLinesAttri: TSynHighlighterAttributes read fDebugLinesAttri write fDebugLinesAttri;

    property AreaAStartPos: Integer read fCodeStartPos write SetCodeStartPos;
    property AreaBStartPos: Integer read fCodeMediumPos write SetCodeMediumPos;
    property CodeEndPos: Integer read fCodeEndPos write SetCodeEndPos;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst;

const
  BooleanWords: string =
    'false, true';

  KeyWords: string =
	'3d, absent, abstract, accept, access, acquire, action, action-copy, ' +
	'action-current-page, action-cut, action-delete, action-first-page, ' +
	'action-hide-drag, action-last-page, action-next, action-next-page, ' +
	'action-paste, action-previous, action-previous-page, action-undo, active-class, ' +
	'actual, add, address, adjustable-columns, advancing, afp-5a, after, aligned, ' +
	'alignment, all, allocate, allow, allowing, alphabet, alphabetic, ' +
	'alphabetic-lower, alphabetic-upper, alphanumeric, alphanumeric-edited, also, ' +
	'alter, alternate, and, any, apply, are, area, areas, area-value, arithmetic, ' +
	'as, ascending, assembly-attributes, assembly-name, assign, at, attribute, ' +
	'attributes, author, auto, auto-decimal, auto-hyphen-skip, automatic, ' +
	'auto-minimize, auto-resize, auto-skip, auto-spin, autoterminate, ' +
	'background-color, background-colour, background-high, background-low, ' +
	'background-standard, backward, b-and, bar, based, beep, before, beginning, ' +
	'bell, b-exor, binary, binary-double, binary-char, binary-long, binary-short, ' +
	'bind, bit, bitmap, bitmap-end, bitmap-frame, bitmap-handle, bitmap-load, ' +
	'bitmap-number, bitmap-start, bitmap-timer, bitmap-trailing, bitmap-width, bits, ' +
	'blank, b-left, b-less, blink, blinking, blob, blob-file, blob-locator, block, ' +
	'b-not, bold, boolean, b-or, bottom, box, boxed, b-right, browser, browsing, ' +
	'bulk-addition, busy, buttons, b-xor, by, c01, c02, c03, c04, c05, c06, c07, ' +
	'c08, c09, c10, c11, c12, calendar-font, call, called, cancel, cancel-button, ' +
	'card-punch, card-reader, case, cassette, catch, cbl-ctr, ccol, cd, cell, ' +
	'cell-color, cell-data, cell-font, cell-protection, cells, center, centered, ' +
	'centered-headings, century-date, century-day, cf, class, class-attributes, ' +
	'class-control, class-id, class-name, class-object, clear-selection, cline, ' +
	'clines, clob, clob-file, clob-locator, clock-units, close, cobol, code, ' +
	'code-set, coercion, col, collating, color, colors, colour, cols, column, ' +
	'column-color, column-dividers, column-font, column-headings, column-protection, ' +
	'columns, combo-box, comma, command-line, commit, commitment, common, ' +
	'communication, comp, comp-0, comp-1, comp-2, comp-3, comp-4, comp-5, comp-6, ' +
	'comp-7, comp-8, comp-9, comp-n, compression, computational, computational-0, ' +
	'computational-1, computational-2, computational-3, computational-4, ' +
	'computational-5, computational-6, computational-7, computational-8, ' +
	'computational-9, computational-n, computational-x, compute, comp-x, com-reg, ' +
	'condition-value, configuration, connect, console, constant, constrain, ' +
	'constraints, constructor, contained, contains, content, continue, control-area, ' +
	'controls, conversion, convert, converting, copy-selection, core-index, corr, ' +
	'corresponding, count, create, creating, crt, crt-under, csize, csp, culture, ' +
	'currency, current, current-date, cursor, cursor-col, cursor-color, ' +
	'cursor-frame-width, cursor-row, cursor-x, cursor-y, custom-attribute, ' +
	'custom-print-template, cycle, cyl-index, cyl-overflow, dashed, data, ' +
	'database-key, database-key-long, data-columns, data-pointer, date, ' +
	'date-and-time, date-compiled, date-entry, date-record, date-written, ' +
	'davf-hhmmss, davf-hhmmsshh, davf-yymmdd, davf-yyyymmdd, davf-yyyymmddhhmmsshh, ' +
	'day, day-and-time, day-of-week, db, db-access-control-key, dbclob, dbclob-file, ' +
	'dbclob-locator, dbcs, db-data-name, db-exception, db-format-name, ' +
	'db-record-name, db-set-name, db-status, dd, de, debug, debug-contents, ' +
	'debugging, debug-item, debug-line, debug-name, debug-sub-1, debug-sub-2, ' +
	'debug-sub-3, decimal, decimal-point, declaratives, default, default-button, ' +
	'definition, delegate, delegate-id, delimited, delimiter, depending, descending, ' +
	'descriptor, destination, destroy, detail, disable, disc, disconnect, ' +
	'disjoining, disk, disp, display, display-1, display-2, display-3, display-4, ' +
	'display-5, display-6, display-7, display-8, display-9, display-columns, ' +
	'display-format, display-st, divide, divider-color, dividers, division, ' +
	'dot-dash, dotted, double, down, drag-color, draw, drop, drop-down, drop-list, ' +
	'duplicate, duplicates, dynamic, ebcdic, egcs, egi, echo, element, else, emi, ' +
	'empty, empty-check, enable, enabled, encoding, encryption, end, end-accept, ' +
	'end-add, end-call, end-compute, end-delete, end-disable, end-display, ' +
	'end-divide, end-enable, end-evaluate, end-exec, end-chain, endif, end-if, ' +
	'ending, end-invoke, end-modify, end-move, end-multiply, end-of-page, ' +
	'end-perform, end-read, end-receive, end-return, end-rewrite, end-search, ' +
	'end-send, end-set, end-start, end-string, end-subtract, end-transceive, ' +
	'end-try, end-unstring, end-use, end-wait, end-write, end-xml, engraved, ' +
	'ensure-visible, enter, entry, entry-field, entry-reason, enum, enum-id, ' +
	'environment, environment-name, environment-value, eol, eop, eos, equal, equals, ' +
	'error, escape, escape-button, esi, evaluate, event, event-action-fail, ' +
	'event-pointer, event-type, every, exact, examine, exceeds, exception, ' +
	'exception-object, exception-value, excess-3, exclusive, exec, execute, exhibit, ' +
	'exit, exit-pushed, expand, expands, extend, extended, extended-search, ' +
	'external, external-form, externally-described-key, f, factory, fd, fetch, ' +
	'fh--fcd, fh--keydef, file, file-control, file-id, file-name, file-path, ' +
	'file-pos, file-prefix, fill-color, fill-color2, filler, fill-percent, final, ' +
	'finally, find, finish, finish-reason, first, fixed, flat, flat-buttons, float, ' +
	'float-extended, floating, float-long, float-short, font, footing, for, ' +
	'foreground-color, foreground-colour, forever, form, format, frame, framed, ' +
	'free, from, full, full-height, function, function-id, function-pointer, ' +
	'generate, get, giving, global, go, goback, go-back, goforward, go-forward, ' +
	'gohome, go-home, gosearch, go-search, graphical, grdsrch-found, ' +
	'grdsrch-not-found, grdsrch-wrapped, greater, grid, grid-searchall, ' +
	'grid-searchcolumn, grid-searchforwards, grid-searchhidden, grid-searchignore, ' +
	'grid-searchmatch, grid-searchmoves, grid-searchskip, grid-searchvisible, ' +
	'grid-searchwrap, grip, group, group-usage, group-value, handle, has-childen, ' +
	'heading, heading-color, heading-divider-color, heading-font, headings, heavy, ' +
	'height, help-id, hidden-data, high, high-color, highlight, horizontal, ' +
	'hot-track, hscroll, hscroll-pos, ch, chain, chaining, changed, char, character, ' +
	'characters, chart, char-varying, check-box, checked, checking, icon, id, ' +
	'identification, identified, if, ignore, ignoring, implements, in, include, ' +
	'independent, index, index-1, index-2, index-3, index-4, index-5, index-6, ' +
	'index-7, index-8, index-9, indexed, indic, indicate, indicator, indicators, ' +
	'inheriting, inherits, initial, initialize, initialized, initiate, input, ' +
	'input-output, inquire, insertion-index, insert-rows, inspect, installation, ' +
	'instance, interface, interface-id, internal, into, intrinsic, invalid, invoke, ' +
	'invoked, i-o, i-o-control, is, item, item-text, item-to-add, item-to-delete, ' +
	'item-to-empty, item-value, japanese, jcllib, job, joining, just, justified, ' +
	'kanji, keep, kept, key, keyboard, key-yy, label, label-offset, last, last-row, ' +
	'layout-data, layout-manager, ld, leading, leading-shift, leave, left, ' +
	'left-justify, leftline, left-text, length, length-check, less, like, lin, ' +
	'linage, linage-counter, line, line-counter, lines, lines-at-root, link, ' +
	'linkage, list-box, locale, locally, local-storage, lock, lock-holding, locking, ' +
	'long-date, long-varbinary, long-varchar, low, low-color, lower, lowered, ' +
	'lowlight, magnetic-tape, manual, mass-update, master-index, max-lines, ' +
	'max-text, max-val, max-value, member, memory, menu, merge, message, messages, ' +
	'metaclass, method, method-id, methods, min-val, min-value, mixed, modal, mode, ' +
	'modeless, modified, modify, modless, module, modules, monitor-pointer, ' +
	'more-labels, move, msg-begin-entry, multiline, multiple, multiply, multline, ' +
	'mutex-pointer, name, named, namespace, namespace-prefix, national, ' +
	'national-edited, native, navigate, negative, nested, new, newable, next, ' +
	'next-item, nextpage, nchar, no, no-auto-default, no-autosel, no-box, ' +
	'no-dividers, no-echo, no-f4, no-focus, no-group-tab, no-key-letter, nominal, ' +
	'none, nonnumeric, normal, no-search, not, no-tab, note, notify, ' +
	'notify-dblclick, notify-change, notify-selchange, no-updown, nstd-reels, null, ' +
	'nulls, number, num-col-headings, num-columns, numeric, numeric-edited, ' +
	'numeric-fill, num-rows, object, object-computer, object-id, object-reference, ' +
	'object-storage, occurs, of, off, o-fill, ok-button, omitted, on, only, ' +
	'oostackptr, open, operator, operator-id, optional, options, or, order, ' +
	'organization, other, others, otherwise, output, overflow, overlapped, overline, ' +
	'override, owner, packed-decimal, padding, page, page-counter, paged, ' +
	'paged-at-end, paged-at-start, paged-empty, page-setup, page-size, palette, ' +
	'panel-index, panel-style, panel-text, panel-widht, paragraph, parse, partial, ' +
	'password, pend, perform, pf, ph, pic, picture, pixel, pixels, placement, ' +
	'pl-sort-default, pl-sort-native, pl-sort-native-ignore-case, pl-sort-none, ' +
	'plus, pointer, pop-up, pos, position, positioning, position-shift, positive, ' +
	'prefixing, present, previous, print, print-control, printer, printer-1, ' +
	'printing, print-no-prompt, print-preview, print-switch, prior, priority, ' +
	'private, proc, procedure, procedure-pointer, procedures, proceed, process, ' +
	'processing, profile, program, program-id, program-pointer, prompt, properties, ' +
	'property, protected, prototype, public, purge, push-button, query-index, queue, ' +
	'quote, quotes, radio-button, raise, raised, raising, random, range, rd, read, ' +
	'readers, reading, read-only, realm, receive, reconnect, record, record-data, ' +
	'recording, record-name, record-overflow, record-position, records, ' +
	'record-to-add, record-to-delete, recover, recovery, recursive, redefine, ' +
	'redefines, redefinition, reel, reference, references, refresh, region-color, ' +
	'relation, relative, release, remainder, remarks, removal, renames, ' +
	'reorg-criteria, repeated, replacing, report, reporting, reports, repository, ' +
	'required, reread, rerun, reserve, reset, reset-grid, reset-list, reset-tabs, ' +
	'resident, resizable, resource, restricted, result-set-locator, resume, ' +
	'retaining, retrieval, retry, return, return-code, returning, return-unsigned, ' +
	'reverse, reversed, reverse-video, rewind, rewrite, rf, rh, right, right-align, ' +
	'right-justify, right-text, rimmed, rollback, rolling, rounded, row-color, ' +
	'row-color-pattern, row-dividers, row-font, row-headings, rowid, row-protection, ' +
	'run, s, s01, s02, s03, s04, s05, same, save-as, save-as-no-prompt, screen, ' +
	'scroll, scroll-bar, sd, search, search-options, search-text, seconds, section, ' +
	'secure, security, seek, segment, segment-limit, select, select-all, ' +
	'selection-index, selection-text, selective, self, self-act, selfclass, ' +
	'semaphore-pointer, send, sentence, separate, separation, separator, sequence, ' +
	'sequential, session-id, set, shading, shadow, shared, sharing, shift-in, ' +
	'shift-out, short-date, show-lines, show-none, show-sel-always, sign, signed, ' +
	'signed-int, signed-long, signed-short, singleline, size, solid, sort, ' +
	'sort-control, sort-core-size, sort-file-size, sort-merge, sort-message, ' +
	'sort-mode-size, sort-option, sort-order, sort-return, sort-tape, sort-tapes, ' +
	'sort-work, source, source-computer, sources, space-fill, special-names, ' +
	'spinner, sql, square, standard, standard-1, standard-2, standard-3, standard-4, ' +
	'start, starting, start-x, start-y, static, static-list, status, status-bar, ' +
	'step, stop, stop-browser, store, string, strong, strong-name, style, subfile, ' +
	'subprogram, sub-queue-1, sub-queue-2, sub-queue-3, sub-schema, subtract, ' +
	'subwindow, suffixing, sum, super, suppress, switch, switch-1, switch-2, ' +
	'switch-3, switch-4, switch-5, switch-6, switch-7, switch-8, symbol, symbolic, ' +
	'sync, synchronized, sysin, sysipt, syslist, syslst, sysout, syspch, syspunch, ' +
	'system, system-default, system-info, tab, table, tabs, tab-to-add, ' +
	'tab-to-delete, tally, tallying, tape, tapes, tenant, terminal, terminal-info, ' +
	'terminate, termination, termination-value, test, text, than, then, thread, ' +
	'thread-local, thread-local-storage, thread-pointer, threads, through, thru, ' +
	'thumb-position, tiled-headings, time, time-of-day, timeout, time-out, ' +
	'time-record, times, timestamp, timestamp-offset, timestamp-offset-record, ' +
	'timestamp-record, title, title-bar, title-position, to, tool-bar, top, totaled, ' +
	'totaling, trace, track-area, track-limit, tracks, track-thumb, trailing, ' +
	'trailing-shift, trailing-sign, transaction, transaction-status, transceive, ' +
	'transform, transparent, transparent-color, tree-view, try, tvni-first-visible, ' +
	'tvni-child, tvni-next, tvni-next-visible, tvni-parent, tvni-previous, ' +
	'tvni-previous-visible, tvni-root, tvplace-first, tvplace-last, tvplace-sort, ' +
	'type, typedef, u, ucs-4, unbounded, underline, underlined, unequal, unframed, ' +
	'unit, units, universal, unlock, unsigned, unsigned-int, unsigned-long, ' +
	'unsigned-short, unsorted, unstring, until, up, update, updaters, upon, upper, ' +
	'upsi-0, upsi-1, upsi-2, upsi-3, upsi-4, upsi-5, upsi-6, upsi-7, usage, ' +
	'usage-mode, use, user, user-default, use-return, use-tab, using, utf-16, utf-8, ' +
	'v, valid, validate, validating, value, value-default, value-format, ' +
	'value-multiple, value-picture, valuetype, valuetype-id, value-variable, ' +
	'varbinary, variable, varying, version, vertical, very-heavy, virtual-width, ' +
	'visible, vpadding, vscroll, vscroll-bar, vscrool-pos, vtop, wait, when, ' +
	'when-compiled, wide, width, window, with, within, words, working-storage, wrap, ' +
	'write, write-only, writers, write-verify, writing, xml, xml-code, ' +
	'xml-declaration, xml-event, xml-ntext, xml-schema, xml-text, yyyyddd, yyyymmdd, ' +
	'zero-fill';
  PreprocessorWords: string =
    'basis, cbl, control, copy, delete, eject, erase,  ready, reload, ' +
    'replace, skip1, skip2, skip3';

  StringWords: string =
    'file-limit, file-limits, high-value, high-values, limit, limits, low-value, low-values, ' +
    'space, spaces, values, zero, zeroes, zeros';

  // Ambigious means that a simple string comparision is not enough
  AmbigiousWords: string =
    '';

const
  StringChars: array[TRangeState] of WideChar = (#0, '"', '''', '=',  '"', '''', #0, #0);

procedure TSynCobolSyn.DoAddKeyword(AKeyword: string; AKind: Integer);
begin
  if not FKeywords.ContainsKey(AKeyword) then
    FKeywords.Add(AKeyword, TtkTokenKind(AKind));
end;

function TSynCobolSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  I: Integer;
  LRun: Integer;
  S: string;
begin
  fToIdent := MayBe;
  LRun := Run;
  while IsIdentChar(MayBe^) and (LRun <= fCodeEndPos) do
  begin
    Inc(MayBe);
    Inc(LRun);
  end;
  fStringLen := Maybe - fToIdent;
  SetString(S, fToIdent, fStringLen);
  if FKeywords.ContainsKey(S) then
  begin
    Result := FKeywords[S];
    if Result = tkUnknown then // handling of "ambigious" words
    begin
      if IsCurrentToken('label') then
      begin
        I := Run + Length('label');
        while fLine[I] = ' ' do
          Inc(I);
        if (AnsiStrLComp(PWideChar(@fLine[I]), 'record', Length('record')) = 0)
          and (I + Length('record') - 1 <= fCodeEndPos) then
            Result := tkKey
          else
            Result := tkPreprocessor;
      end
      else
        Result := tkIdentifier;
    end;
  end
  else
    Result := tkIdentifier;
end;

procedure TSynCobolSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(fLine[Run], [#1..#32]);
end;

procedure TSynCobolSyn.FirstCharsProc;
var
  I: Integer;
begin
  if IsLineEnd(Run) then
    NextProcedure
  else if Run < fCodeStartPos - 1 then
  begin
    fTokenID := tkSequence;
    repeat
      Inc(Run);
    until (Run = fCodeStartPos - 1) or IsLineEnd(Run);
  end
  else
  begin
    fTokenID := tkIndicator;
    case fLine[Run] of
      '*', '/': fRange := rsComment;                                            //JaFi
      'D', 'd': fRange := rsDebug;                                              //JaFi
//      '*', '/', 'D', 'd': fIndicator := fLine[Run];                           //JaFi
      '-': if fRange in [rsQuoteStringMayBe, rsApostStringMayBe] then
           begin
             I := Run + 1;
             while fLine[I] = ' ' do
               Inc(I);
             if (AnsiStrLComp(PWideChar(@fLine[I]), PWideChar(StringOfChar(StringChars[fRange], 2)), 2) <> 0)
               or (I + 1 > fCodeEndPos) then
                 fRange := rsUnknown;
           end;
    end;
    Inc(Run);
  end;
end;

procedure TSynCobolSyn.LastCharsProc;
begin
  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    fTokenID := tkTagArea;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynCobolSyn.CommentProc;
begin
  fIndicator := #0;
  fRange := rsUnknown;
  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run); // or (Run > fCodeEndPos);
  end;
end;

procedure TSynCobolSyn.InlineCommentProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
  if fLine[Run] = '>' then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  if not IsLineEnd(Run) then
    UnknownProc;
end;

procedure TSynCobolSyn.DebugProc;
begin
  fIndicator := #0;
  fRange := rsUnknown;
  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    fTokenID := tkDebugLines;
    repeat
      Inc(Run);
    until IsLineEnd(Run) or (Run > fCodeEndPos);
  end;
end;

procedure TSynCobolSyn.PointProc;
begin
  if (Run < fCodeEndPos) and CharInSet(FLine[Run + 1], ['0'..'9', 'e', 'E']) then
    NumberProc
  else
    UnknownProc;
end;

procedure TSynCobolSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  fFloat: Boolean;
begin
  fTokenID := tkNumber;
  Inc(Run);
  fFloat := False;

  while IsNumberChar and (Run <= fCodeEndPos) do
  begin
    case FLine[Run] of
      '.':
        if not CharInSet(FLine[Run + 1], ['0'..'9', 'e', 'E']) then
          Break
        else
          fFloat := True;
      'e', 'E':
          if not CharInSet(FLine[Run - 1], ['0'..'9', '.']) then
            Break
          else fFloat := True;
      '-', '+':
        begin
          if not fFloat or not CharInSet(FLine[Run - 1], ['e', 'E']) then
            Break;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynCobolSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynCobolSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynCobolSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynCobolSyn.StringOpenProc;
begin
  case fLine[Run] of
    '"': fRange := rsQuoteString;
    '''': fRange := rsApostString;
    else
      if fLine[Run + 1] = '=' then
      begin
        fRange := rsPseudoText;
        Inc(Run);
      end
      else
      begin
        UnknownProc;
        Exit;
      end;
  end;

  Inc(Run);
  StringProc;
//  fTokenID := tkString;
end;

procedure TSynCobolSyn.StringProc;
begin
  fTokenID := tkString;

  if Run <= fCodeEndPos then
  repeat
    if (fLine[Run] = StringChars[fRange])
      and ((fLine[Run] <> '=') or ((Run > 0) and (fLine[Run - 1] = '='))) then
    begin
      if (Run = fCodeEndPos) and (fRange in [rsQuoteString, rsApostString]) then
        Inc(fRange, 3)
      else
      begin
        fRange := rsUnknown;
      end;
      Inc(Run);
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run) or (Run > fCodeEndPos);
end;

procedure TSynCobolSyn.StringEndProc;
begin
  if IsLineEnd(Run) then
    NextProcedure
  else
  begin
    fTokenID := tkString;

    if (fRange <> rsPseudoText) and (Run <= fCodeEndPos) then
    repeat
      if (fLine[Run] = StringChars[fRange]) then
      begin
        if fRange in [rsQuoteString, rsApostString] then
          Inc(Run)
        else
        begin
          Inc(Run, 2);
          Dec(fRange, 3);
        end;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run) or (Run > fCodeEndPos);

    StringProc;
  end;
end;

constructor TSynCobolSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  // Create the keywords dictionary case-insensitive
  FKeywords := TDictionary<string, TtkTokenKind>.Create(TIStringComparer.Ordinal);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGray;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fAIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrAreaAIdentifier, SYNS_FriendlyAttrAreaAIdentifier);
  fAIdentifierAttri.Foreground := clTeal;
  fAIdentifierAttri.Style := [fsBold];
  AddAttribute(fAIdentifierAttri);

  fPreprocessorAttri := TSynHighLighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fPreprocessorAttri.Foreground := clMaroon;
  AddAttribute(fPreprocessorAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clGreen;
  AddAttribute(fNumberAttri);

  fBooleanAttri := TSynHighLighterAttributes.Create(SYNS_AttrBoolean, SYNS_FriendlyAttrBoolean);
  fBooleanAttri.Foreground := clGreen;
  AddAttribute(fBooleanAttri);

  fBracketAttri := TSynHighLighterAttributes.Create(SYNS_AttrBrackets, SYNS_AttrBrackets);
  AddAttribute(fBracketAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fSequenceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSequence, SYNS_FriendlyAttrSequence);
  fSequenceAttri.Foreground := clDkGray;
  AddAttribute(fSequenceAttri);

  fIndicatorAttri := TSynHighLighterAttributes.Create(SYNS_AttrIndicator, SYNS_FriendlyAttrIndicator);
  fIndicatorAttri.Foreground := clRed;
  AddAttribute(fIndicatorAttri);

  fTagAreaAttri := TSynHighLighterAttributes.Create(SYNS_AttrTagArea, SYNS_FriendlyAttrTagArea);
  fTagAreaAttri.Foreground := clMaroon;
  AddAttribute(fTagAreaAttri);

  fDebugLinesAttri := TSynHighLighterAttributes.Create(SYNS_AttrDebugLines, SYNS_FriendlyAttrDebugLines);
  fDebugLinesAttri.Foreground := clDkGray;
  AddAttribute(fDebugLinesAttri);
  SetAttributesOnChange(DefHighlightChange);

  fDefaultFilter := SYNS_FilterCOBOL;
  fRange := rsUnknown;
  fIndicator := #0;

  fCodeStartPos := 7;
  fCodeMediumPos := 11;
  fCodeEndPos := 71;

  EnumerateKeywords(Ord(tkBoolean), BooleanWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), KeyWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkPreprocessor), PreprocessorWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkString), StringWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkUnknown), AmbigiousWords, IsIdentChar, DoAddKeyword);

  RE_BlockBegin := CompiledRegEx('\b^(IF |EVALUATE |EXEC |READ |WRITE |PERFORM |STRING |ACCEPT )\b', [roIgnoreCase]);
  RE_BlockEnd := CompiledRegEx('(END\-IF|END\-EVALUATE|END\-EXEC|END\-READ|END\-WRITE|END\-PERFORM|END\-STRING|END\-ACCEPT)', [roIgnoreCase]);
end;

const
  FT_Standard = 1;  // begin end, class end, record end
  FT_Comment = 11;
  FT_CodeDeclaration = 16;
  FT_CodeDeclarationWithBody = 17;
  FT_Implementation = 18;
  FT_Region: Integer = 99;

procedure TSynCobolSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer);
var
  Line: Integer;
  iList: TList<Integer>;
  CurLine: string;
  ok: Boolean;
  IsLastDot: Boolean;

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
          if (match.Index > Index) then
          begin
            OK := True;
            Break;
          end;
        // line ends with dot - it replaces END-XXX
        if not OK and IsLastDot then
          OK := True;

        if not OK then begin
          FoldRanges.StartFoldRange(Line + 1, FT_Standard);
          Result := True;
        end;
      end;
    end
    else
    begin
      mce := RE_BlockEnd.Matches(CurLine);
      if (mce.Count > 0) or IsLastDot then
      begin
        Index := 0;
        if mce.Count > 0 then
          Index :=  mce.Item[0].Index;
        if IsLastDot or (GetHighlighterAttriAtRowCol(LinesToScan, Line, Index) <> fCommentAttri) then
        begin
          FoldRanges.StopFoldRange(Line + 1, FT_Standard);
          Result := True;
        end;
      end;
    end;
  end;

begin
  iList := TList<Integer>.Create;

  for Line := 0 to LinesToScan.Count - 1 do
  begin
    CurLine := Trim(LinesToScan[Line]);
    IsLastDot := Copy(CurLine, Length(CurLine), 1) = '.';

    // Divisions
    if Pos(' DIVISION.', UpperCase(CurLine)) > 0 then
    begin
      FoldRanges.StopFoldRange(Line+1, FT_CodeDeclarationWithBody, 0);
      FoldRanges.StartFoldRange(Line + 1, FT_CodeDeclarationWithBody, 0);
    end
    else
    // Sections
    if Pos(' SECTION.', UpperCase(CurLine)) > 0 then
    begin
      FoldRanges.StopFoldRange(Line, FT_CodeDeclarationWithBody, 1);
      FoldRanges.StartFoldRange(Line +1, FT_CodeDeclarationWithBody, 1);
    end
    else
    // Standard XXX ... END-XXX ranges
    if not BlockDelimiter(Line) then
      FoldRanges.NoFoldInfo(Line + 1);

  end;
  // finally we end all open ranges
  FoldRanges.StopFoldRange(LinesToScan.Count, FT_CodeDeclarationWithBody, 1);
  FoldRanges.StopFoldRange(LinesToScan.Count, FT_CodeDeclarationWithBody, 0);

  iList.Free;
end;

destructor TSynCobolSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynCobolSyn.IdentProc;
begin
  if CharInSet(fLine[Run], ['x', 'g', 'X', 'G'])
    and (Run < fCodeEndPos) and CharInSet(fLine[Run + 1], ['"', '''']) then
  begin
    Inc(Run);
    StringOpenProc;
  end
  else
  begin
    fTokenID := IdentKind((fLine + Run));
    if (fTokenID = tkIdentifier) and (Run < fCodeMediumPos) then
      fTokenID := tkAIdentifier;
    Inc(Run, fStringLen);

    while IsIdentChar(fLine[Run]) and (Run <= fCodeEndPos) do
      Inc(Run);
  end;
end;

procedure TSynCobolSyn.UnknownProc;
begin
  fTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynCobolSyn.Next;
begin
  fTokenPos := Run;

  if fTokenPos < fCodeStartPos then
    FirstCharsProc
  else
//    case fIndicator of
//      '*', '/': CommentProc;
//      'D', 'd': DebugProc;
    case fRange of
        rsComment: CommentProc;
        rsDebug: DebugProc;
      else
        if fTokenPos > fCodeEndPos then
          LastCharsProc
        else
          case fRange of
            rsQuoteString..rsApostStringMayBe: StringEndProc;
          else
            begin
              fRange := rsUnknown;
              NextProcedure;
            end;
          end;
    end;
  inherited;
end;

procedure TSynCobolSyn.NextProcedure;
begin
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    '"': StringOpenProc;
    '''': StringOpenProc;
    '=': StringOpenProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '.': PointProc;
    '0'..'9': NumberProc;
    '-', 'A'..'Z', 'a'..'z': IdentProc;
    '(', ')': BracketProc;
    '*': InlineCommentProc;  // comment *> to end of line
    else UnknownProc;
  end;
end;

function TSynCobolSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER:  Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynCobolSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynCobolSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCobolSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkAIdentifier: Result := fAIdentifierAttri;
    tkPreprocessor: Result := fPreprocessorAttri;
    tkKey: Result := fKeyAttri;
    tkBoolean: Result := fBooleanAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSequence: Result := fSequenceAttri;
    tkIndicator: Result := fIndicatorAttri;
    tkTagArea: Result := fTagAreaAttri;
    tkDebugLines: Result := fDebugLinesAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkBracket: Result := fBracketAttri;
  else
    Result := nil;
  end;
end;

function TSynCobolSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCobolSyn.GetSampleSource: string;
begin
  Result := '000100* This is a sample file to be used to show all TSynCobolSyn''s'#13#10 +
            '000200* features.'#13#10 +
            '000300* This isn''t a valid COBOL program.'#13#10 +
            '000400'#13#10 +
            '000500* 1. Supported COBOL features.'#13#10 +
            '000600'#13#10 +
            '000700* 1.1  Sequence area.'#13#10 +
            '000800*    First six columns in COBOL are reserved for enumeration'#13#10 +
            '000900*    of source lines.'#13#10 +
            '001000* 1.2  Indicator area.'#13#10 +
            '001100*    7th column in COBOL is reserved for special markers like ''*'''#13#10 +
            '001200*    or ''D''.'#13#10 +
            '001300* 1.3  Comment lines.'#13#10 +
            '001400*    Any line started from ''*'' in 7th column is a comment.'#13#10 +
            '001500*    No separate word highlighting will be done by the editor.'#13#10 +
            '001600* 1.4  Debug lines.'#13#10 +
            '001700D    Any line started from ''D'' will be treated as containing debug'#13#10 +
            '001800D    commands. No separate word highlighting will be done'#13#10 +
            '001900D    by the editor.'#13#10 +
            '002000* 1.5  Tag area.'#13#10 +
            '002100*    Only columns from 8th till 72th can be used for COBOL        TAG_AREA'#13#10 +
            '002200*    program. Columns beyond the 72th one may be used by some     TAG_AREA'#13#10 +
            '002300*    COBOL compilers to tag the code in some internal way.        TAG_AREA'#13#10 +
            '002400* 1.6  Area A identifiers.'#13#10 +
            '002500*    In area A (from 8th column till'#13#10 +
            '002600*    11th one) you should type only sections''/paragraphs'' names.'#13#10 +
            '002700*    For example "SOME" is a section name:'#13#10 +
            '002800 SOME SECTION.'#13#10 +
            '002900* 1.7  Preprocessor directives.'#13#10 +
            '003000*    For example "COPY" is a preprocessor directive:'#13#10 +
            '003100     COPY "PRD-DATA.SEL".'#13#10 +
            '003200* 1.8  Key words.'#13#10 +
            '003300*    For example "ACCEPT" and "AT" are COBOL key words:'#13#10 +
            '003400     ACCEPT WS-ENTRY AT 2030.'#13#10 +
            '003500* 1.9  Boolean constants.'#13#10 +
            '003600*    These are "TRUE" and "FALSE" constants. For example:'#13#10 +
            '003700     EVALUATE TRUE.'#13#10 +
            '003800* 1.10 Numbers.'#13#10 +
            '003900*    Here are the examples of numbers:'#13#10 +
            '004000 01  WSV-TEST-REC.'#13#10 +
            '004100     03  WSV-INT-T	       PIC 9(5) VALUE 12345.'#13#10 +
            '004200     03  WSV-PRICES              PIC 9(4)V99 COMP-3 VALUE 0000.33. 		'#13#10 +
            '004300     03  WSV-Z-PRICES            PIC Z(5)9.99- VALUE -2.12. 		'#13#10 +
            '004400     03  WSV-STORE-DATE          PIC 9(4)V99E99 VALUE 0001.33E02.'#13#10 +
            '004500* 1.11 String.'#13#10 +
            '004600*    The following types of string are supported:'#13#10 +
            '004700*    1.11.1 Quoted string.'#13#10 +
            '004800         MOVE "The name of field is ""PRODUCT""" TO WS-ERR-MESS.'#13#10 +
            '004900         MOVE ''The name of field is ''''PRODUCT'''''' TO WS-ERR-MESS.'#13#10 +
            '005000*    1.11.2 Pseudo-text.'#13#10 +
            '005100         COPY'#13#10 +
            '005200             REPLACING ==+00001== BY  +2'#13#10 +
            '005300                       == 1 ==    BY  -3.'#13#10 +
            '005400*    1.11.3 Figurative constants.'#13#10 +
            '005500*        For example "SPACES" is figurative constant:'#13#10 +
            '005600             DISPLAY SPACES UPON CRT.'#13#10 +
            '005700* 1.12 Continued lines.'#13#10 +
            '005800*    Only continued strings are supported. For example:'#13#10 +
            '005900         MOVE "The name of figurative constant field is'#13#10 +
            '006000-"SPACES" TO WS-ERR-MESS.'#13#10 +
            '006100*    Or (a single quotation mark in 72th column):'#13#10 +
            '005900         MOVE "The name of figurative constant field is  ""SPACES"'#13#10 +
            '006000-""" TO WS-ERR-MESS.'#13#10 +
            '006100'#13#10 +
            '006200* 2. Unsupported COBOL features.'#13#10 +
            '006300'#13#10 +
            '006400* 2.1 Continued lines.'#13#10 +
            '006500*    Continuation of key words is not supported. For example,'#13#10 +
            '006600*    the following COBOL code is valid but TSynCobolSyn won''t'#13#10 +
            '006700*    highlight "VALUE" keyword properly:'#13#10 +
            '006800     03  WSV-STORE-DATE                         PIC 9(4)V99E99 VAL'#13#10 +
            '006900-UE 0001.33E02.'#13#10 +
            '007000* 2.2 Identifiers started from digits.'#13#10 +
            '007100*    They are valid in COBOL but won''t be highlighted properly'#13#10 +
            '007200*    by TSynCobolSyn. For example, "000-main" is a paragraph'#13#10 +
            '007300*    name and should be highlighted as Area A identifier:'#13#10 +
            '007400 000-main.'#13#10 +
            '007500* 2.3 Comment entries in optional paragraphs'#13#10 +
            '007600*    The so called comment-entries in the optional paragraphs'#13#10 +
            '007700*    of the Identification Division are not supported and won''t'#13#10 +
            '007800*    be highlighted properly.';
end;

function TSynCobolSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCOBOL;
end;

function TSynCobolSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '-', '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TSynCobolSyn.SetCodeStartPos(Value: Integer);
begin
  if Value <= fCodeMediumPos then
    fCodeStartPos := Value
  else
    fCodeStartPos := fCodeMediumPos;
end;

procedure TSynCobolSyn.SetCodeMediumPos(Value: Integer);
begin
  if (fCodeStartPos <= Value) and (Value <= fCodeEndPos) then
    fCodeMediumPos := Value
  else
    if Value > fCodeEndPos
    then fCodeMediumPos := fCodeEndPos
    else fCodeMediumPos := fCodeStartPos;
end;

procedure TSynCobolSyn.SetCodeEndPos(Value: Integer);
begin
  if Value > fCodeMediumPos then
    fCodeEndPos := Value
  else
    fCodeEndPos := fCodeMediumPos;
end;

class function TSynCobolSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCOBOL;
end;

procedure TSynCobolSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCobolSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynCobolSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynCobolSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangCOBOL;
end;

procedure TSynCobolSyn.BracketProc;
begin
  Inc(Run);
  fTokenID := tkBracket;
end;

initialization
  RegisterPlaceableHighlighter(TSynCobolSyn);
end.
