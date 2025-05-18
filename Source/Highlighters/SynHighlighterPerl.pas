{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPerl.pas, released 2000-04-10.
The Original Code is based on the DcjSynPerl.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Trier.
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

$Id: SynHighlighterPerl.pas,v 1.14.2.7 2005/12/16 20:09:37 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - Using q, qq, qw, qx, m, s, tr will not properly parse the contained
    information.
  - Not very optimized.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Perl syntax highlighter for SynEdit)
@author(Michael Trier)
@created(1999, converted to SynEdit 2000-04-10 by Michael Hieke)
@lastmod(2000-06-23)
The SynHighlighterPerl unit provides SynEdit with a Perl syntax highlighter.
}

unit SynHighlighterPerl;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  Windows,                                                                      //Fiala
  SysUtils,
  SynUnicode,
  Classes,
//++ CodeFolding
  SynEditCodeFolding;
//++ CodeFolding

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber, tkOperator,
    tkPragma, tkSpace, tkString, tkUnknown, tkVariable);

  TRangeState = (rsUnknown, rsString, rsQuotedString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

//  TSynPerlSyn = class(TSynCustomHighlighter)
//++ CodeFolding
  TSynPerlSyn = class(TSynCustomCodeFoldingHighlighter)
//-- CodeFolding
  private
    FRange: TRangeState;                                                        //Fiala
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..2728] of TIdentFuncTableFunc;                     //Fiala
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;
    fPragmaAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    function FuncKey(Index: Integer): TtkTokenKind;
    function FuncOperator(Index: Integer): TtkTokenKind;
    function FuncPragma(Index: Integer): TtkTokenKind;

    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommentProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure qFunctionProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringInterpProc;
    procedure StringLiteralProc;
    procedure StringEndProc;
    procedure SymbolProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure VariableProc;                                                     //Fiala
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
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;               //Fiala
    procedure Next; override;
    function  GetRange: Pointer; override;                                      //Fiala
    procedure ResetRange; override;                                             //Fiala
    procedure SetRange(Value: Pointer); override;                               //Fiala
//++ CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
//-- CodeFolding
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri
      write fOperatorAttri;
    property PragmaAttri: TSynHighlighterAttributes read fPragmaAttri
      write fPragmaAttri;
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
  SynEditStrConst;

const
  { expanded keywords list }                                                    //Fiala
  KeyWords: array[0..295] of string = (
    '$ACCUMULATOR', '$ARG', '$ARGV', '$BASETIME', '$DEBUGGING', 
    '$EFFECTIVE_GROUP_ID', '$EFFECTIVE_USER_ID', '$EGID', '$ENV', '$ERRNO', 
    '$EUID', '$EVAL_ERROR', '$EXECUTABLE_NAME', '$FORMAT_FORMFEED', 
    '$FORMAT_LINE_BREAK_CHARACTERS', '$FORMAT_LINES_LEFT', 
    '$FORMAT_LINES_PER_PAGE', '$FORMAT_NAME', '$FORMAT_PAGE_NUMBER', 
    '$FORMAT_TOP_NAME', '$GID', '$CHILD_ERROR', '$INPLACE_EDIT', 
    '$INPUT_LINE_NUMBER', '$INPUT_RECORD_SEPARATOR', '$LAST_PAREN_MATCH', 
    '$LIST_SEPARATOR', '$MATCH', '$MULTILINE_MATCHING', '$NR', '$OFMT', '$ORS', 
    '$OS_ERROR', '$OUTPUT_AUTOFLUSH', '$OUTPUT_FIELD_SEPARATOR', 
    '$PERL_VERSION', '$PERLDB', '$PID', '$POSTMATCH', '$PREMATCH', 
    '$PROCESS_ID', '$PROGRAM_NAME', '$REAL_GROUP_ID', '$REAL_USER_ID', '$RS', 
    '$SIG', '$SUBSCRIPT_SEPARATOR', '$SUBSEP', '$SYSTEM_FD_MAX', '$UID',
    '$WARNING', '%INC', '@ARGV', '@INC', 'abs', 'accept', 'alarm', 'and', 
    'atan2', 'bind', 'binmode', 'bless', 'break', 'caller', 'close', 'closedir', 
    'cmp', 'connect', 'constant', 'continue', 'cos', 'crypt', 'dbmclose', 
    'dbmopen', 'defined', 'delete', 'diagnostics', 'die', 'do', 'dump', 'each', 
    'else', 'elsif', 'endgrent', 'endhostent', 'endnetent', 'endprotoent', 
    'endpwent', 'endservent', 'eof', 'eq', 'eval', 'exec', 'exists', 'exit',
    'exp', 'fcntl', 'fileno', 'flock', 'for', 'foreach', 'fork', 'format', 
    'formline', 'ge', 'getc', 'getgrent', 'getgrgid', 'getgrnam', 
    'gethostbyaddr', 'gethostbyname', 'gethostent', 'getlogin', 'getnetbyaddr', 
    'getnetbyname', 'getnetent', 'getpeername', 'getpgrp', 'getppid', 
    'getpriority', 'getprotobyname', 'getprotobynumber', 'getprotoent', 
    'getpwent', 'getpwnam', 'getpwuid', 'getservbyname', 'getservbyport', 
    'getservent', 'getsockname', 'getsockopt', 'glob', 'gmtime', 'goto', 'grep', 
    'gt', 'hex', 'chdir', 'chmod', 'chomp', 'chop', 'chown', 'chr', 'chroot', 
    'if', 'import', 'index', 'int', 'integer', 'ioctl', 'join', 'keys', 'kill', 
    'last', 'lc', 'lcfirst', 'le', 'length', 'less', 'link', 'listen', 'local', 
    'locale', 'localtime', 'lock', 'log', 'lstat', 'lt', 'm', 'map', 'mkdir', 
    'msgctl', 'msgget', 'msgrcv', 'msgsnd', 'my', 'ne', 'next', 'no', 'not', 
    'oct', 'open', 'opendir', 'or', 'ord', 'our', 'pack', 'package', 'pipe', 
    'pop', 'pos', 'print', 'printf', 'push', 'q', 'qq', 'qr', 'quotemeta', 'qw', 
    'qx', 'rand', 'read', 'readdir', 'readline', 'readlink', 'readpipe', 'recv', 
    'redo', 'ref', 'rename', 'require', 'reset', 'return', 'reverse', 
    'rewinddir', 'rindex', 'rmdir', 'say', 'scalar', 'seek', 'seekdir', 
    'select', 'semctl', 'semget', 'semop', 'send', 'setgrent', 'sethostent', 
    'setnetent', 'setpgrp', 'setpriority', 'setprotoent', 'setpwent', 
    'setservent', 'setsockopt', 'shift', 'shmctl', 'shmget', 'shmread', 
    'shmwrite', 'shutdown', 'sigtrap', 'sin', 'sleep', 'socket', 'socketpair', 
    'sort', 'splice', 'split', 'sprintf', 'sqrt', 'srand', 'stat', 'state', 
    'strict', 'study', 'sub', 'subs', 'substr', 'symlink', 'syscall', 'sysopen', 
    'sysread', 'sysseek', 'system', 'syswrite', 'tell', 'telldir', 'tie', 
    'tied', 'time', 'times', 'tr', 'truncate', 'uc', 'ucfirst', 'umask', 
    'undef', 'unless', 'unlink', 'unpack', 'unshift', 'untie', 'until', 'use', 
    'utime', 'values', 'vars', 'vec', 'wait', 'waitpid', 'wantarray', 'warn', 
    'while', 'write', 'xor' 
  );

  KeyIndices: array[0..2728] of Integer = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 222, -1, -1, -1, -1, -1, -1,
    -1, -1, 201, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 203, -1, -1, -1, -1, -1, -1, -1, -1, 87, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 217, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 64, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 198, 10, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 157, -1, -1, 167, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 58, -1, -1, -1, -1, -1, -1, -1, -1, -1, 126,
    -1, -1, -1, -1, -1, -1, -1, -1, 274, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 200, -1, 143, 275, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 254, -1, -1, -1, -1, -1, -1, -1, 118, -1, -1, 
    -1, -1, -1, -1, 259, -1, -1, -1, -1, -1, -1, -1, -1, 114, -1, -1, -1, -1, 
    -1, -1, -1, -1, 23, -1, 237, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 148, 185, -1, -1, -1, 110, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 270, 16, -1, -1, -1, 103, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 60, 44, -1, 50, -1, -1, -1, -1, 135, -1, -1, 137, -1, 
    -1, -1, -1, -1, -1, -1, -1, 83, -1, -1, 53, -1, -1, 63, -1, -1, -1, -1, -1, 
    166, -1, -1, -1, -1, -1, -1, -1, 158, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 22, -1, -1, -1, -1, -1, -1, 149, 202, -1, -1, 77, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 70, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 258, -1, -1, -1, -1, -1, -1, 204, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 277, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 194, -1, 41, -1, -1, 243, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 230, 71, -1, -1, -1, -1, -1, -1, 
    231, -1, -1, -1, -1, -1, 283, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 107, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 216, -1, -1, -1, -1, -1, 124, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 265, -1, -1, -1, -1, -1, 
    152, -1, -1, -1, -1, -1, 72, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 146, 92, -1, -1, -1, -1, -1, -1, 147, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 19, -1, 
    -1, 129, -1, -1, -1, -1, -1, -1, -1, -1, -1, 213, -1, -1, -1, 8, -1, -1, -1, 
    -1, -1, 36, -1, -1, -1, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 233, -1, -1, -1, -1, 121, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 281, -1, 271, -1, -1, -1, -1, -1, -1, -1, 196, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 111, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 248, -1, -1, -1, 5, -1, -1, -1, -1, 20, -1, -1, -1, -1, -1, 
    -1, -1, -1, 207, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    176, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 131, -1, -1, -1, -1, 
    -1, -1, -1, 142, -1, 3, -1, -1, -1, -1, 249, 108, -1, -1, -1, -1, 133, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 211, -1, -1, -1, -1, -1, 
    -1, 130, -1, 21, -1, -1, -1, -1, -1, -1, 225, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 287, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 17, -1, -1, -1, 59, -1, -1, 169, -1, 82, -1, -1, 81, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 240, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 123, -1, -1, -1, -1,
    14, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 192, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 235, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 205, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 42, -1, -1, 45, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 38, -1, -1, -1, 189, -1, -1, -1, -1, -1, 187, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    246, 164, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 105, -1, -1, -1, -1, 100, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 276, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 199, -1, -1, -1, -1, -1, 288, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    242, -1, -1, -1, -1, -1, -1, -1, -1, 94, -1, -1, 122, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 138, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 284, -1, 179, 112, -1, 
    -1, 253, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 239, 190, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    165, -1, -1, -1, -1, -1, 218, -1, -1, -1, 106, -1, 75, 255, -1, -1, 97, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 191, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 40, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 279, -1, -1, -1, -1, -1, 
    -1, 224, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 212, -1, -1, -1, -1, -1, 
    -1, 125, -1, -1, -1, 139, 267, -1, 269, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 73, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 98, 136, -1, 
    -1, -1, -1, 90, -1, -1, -1, -1, -1, -1, -1, -1, 74, -1, -1, 263, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 278, -1, -1, 30, -1, -1, -1, 
    -1, -1, -1, -1, -1, 159, -1, -1, -1, -1, -1, -1, -1, -1, -1, 66, -1, -1, -1, 
    -1, 32, -1, 95, 161, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 209, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    264, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, 291, -1, -1, -1, -1, 61, 
    -1, -1, -1, -1, 244, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 27, -1, -1, -1, 31, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 182, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 256, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    150, -1, -1, 215, 260, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 54, -1, -1, -1, -1, -1, 197, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 210, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 175, -1, -1, -1, -1, -1, -1, -1, -1, 141, -1,
    -1, -1, -1, -1, -1, -1, 67, -1, -1, -1, -1, -1, 65, 272, -1, -1, -1, -1, -1, 
    -1, -1, -1, 29, -1, -1, -1, -1, 292, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 227, -1, -1, -1, -1, -1, -1, -1, -1, -1, 160, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    56, -1, -1, -1, 49, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 57, -1, -1, -1, -1, -1, -1, -1, 25, -1, -1, -1, 
    -1, 228, 266, 170, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 180, 234, -1, -1, -1, -1, -1, 
    -1, 69, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 289, 
    -1, 93, -1, -1, -1, -1, 55, -1, 183, -1, -1, -1, -1, -1, 163, -1, 162, -1, 
    -1, -1, 132, -1, -1, -1, 282, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 247, -1, 188, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 174, -1, -1, -1, 280, -1, -1, 261, 51, -1, -1, -1, -1, -1, 144, 
    156, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 229, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 193, 172, -1, 
    -1, -1, -1, -1, 219, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 208, -1, -1, 
    -1, -1, -1, -1, -1, 6, 113, -1, -1, -1, 250, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 145, 28, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 109, 
    -1, -1, 168, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 186, -1, 151, -1, -1, -1, 153, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, -1, 251, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 184, -1, 262, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 140, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 68, 238, -1, -1, -1, -1, 
    221, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 173, -1, -1, -1, 104, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 9, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 223, -1, 101, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 76, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, 119, 
    -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, -1, -1, -1, 294, -1, 85, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 18, -1, -1, -1, -1, -1, 
    -1, 37, -1, -1, -1, -1, 220, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 99, 
    -1, -1, -1, -1, -1, -1, -1, 178, -1, -1, -1, -1, -1, -1, -1, -1, 293, 34,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 252, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 285, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 232, -1, -1, -1, -1, -1, -1, -1, 52, 
    -1, -1, 43, 117, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 120, -1, 128, 48, -1, -1, -1, -1, -1, -1, -1, 47, -1, 
    -1, -1, 39, 127, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 268, -1,
    -1, -1, -1, -1, -1, 86, -1, -1, -1, -1, -1, -1, 89, -1, 236, -1, -1, -1, -1, 
    -1, -1, 13, -1, -1, -1, -1, -1, 96, 155, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    286, -1, -1, -1, -1, -1, -1, -1, -1, 33, 1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, 195, 80, 62, -1, -1, -1, 290, -1, -1, -1, -1, 26, -1, -1, -1, -1, 
    -1, 115, -1, -1, 91, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 134, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 245, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 12, 
    -1, -1, 171, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 214, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 295, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 273, -1, -1, 
    -1, 181, -1, -1, -1, -1, 116, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 226, 257, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 241, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 206, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, 
    -1, -1, 102, -1, -1, 79, -1, -1, -1, -1, -1, -1, 84, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 177, -1, 
    -1, -1, -1, -1, -1, -1, 154, -1, -1, -1, -1, -1 
  );

{$Q-}
function TSynPerlSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 714 + Ord(Str^) * 970;
    inc(Str);
  end;
  Result := Result mod 2729;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynPerlSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
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

procedure TSynPerlSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc
    else
      fIdentFuncTable[i] := FuncKey;

{variable                                                                       //Fiala
  all functions starting FuncNN (NN = number)
}

{pragma
  FuncConstant;
  FuncDiagnostics;
  FuncInteger;
  FuncLess;
  FuncLocale;
  FuncSigtrap;
  FuncStrict;
  FuncSubs;
  FuncVars;
}
{operators
  FuncAnd;
  FuncCmp;
  FuncEq;
  FuncGe;
  FuncGt;
  FuncLe;
  FuncLt;
  FuncNe;
  FuncNot;
  FuncOr;
  FuncXor;
}

  fIdentFuncTable[2206] := FuncVar;
  fIdentFuncTable[2446] := FuncVar;
  fIdentFuncTable[144] := FuncVar;
  fIdentFuncTable[785] := FuncVar;
  fIdentFuncTable[1434] := FuncVar;
  fIdentFuncTable[732] := FuncVar;
  fIdentFuncTable[1994] := FuncVar;
  fIdentFuncTable[184] := FuncVar;
  fIdentFuncTable[627] := FuncVar;
  fIdentFuncTable[2152] := FuncVar;
  fIdentFuncTable[91] := FuncVar;
  fIdentFuncTable[1456] := FuncVar;
  fIdentFuncTable[2529] := FuncVar;
  fIdentFuncTable[2419] := FuncVar;
  fIdentFuncTable[911] := FuncVar;
  fIdentFuncTable[637] := FuncVar;
  fIdentFuncTable[282] := FuncVar;
  fIdentFuncTable[855] := FuncVar;
  fIdentFuncTable[2243] := FuncVar;
  fIdentFuncTable[610] := FuncVar;
  fIdentFuncTable[737] := FuncVar;
  fIdentFuncTable[820] := FuncVar;
  fIdentFuncTable[354] := FuncVar;
  fIdentFuncTable[247] := FuncVar;
  fIdentFuncTable[2070] := FuncVar;
  fIdentFuncTable[1803] := FuncVar;
  fIdentFuncTable[2468] := FuncVar;
  fIdentFuncTable[1473] := FuncVar;
  fIdentFuncTable[2014] := FuncVar;
  fIdentFuncTable[1678] := FuncVar;
  fIdentFuncTable[1352] := FuncVar;
  fIdentFuncTable[1477] := FuncVar;
  fIdentFuncTable[1376] := FuncVar;
  fIdentFuncTable[2445] := FuncVar;
  fIdentFuncTable[2285] := FuncVar;
  fIdentFuncTable[2218] := FuncVar;
  fIdentFuncTable[633] := FuncVar;
  fIdentFuncTable[2250] := FuncVar;
  fIdentFuncTable[969] := FuncVar;
  fIdentFuncTable[2382] := FuncVar;
  fIdentFuncTable[1220] := FuncVar;
  fIdentFuncTable[452] := FuncVar;
  fIdentFuncTable[956] := FuncVar;
  fIdentFuncTable[2345] := FuncVar;
  fIdentFuncTable[304] := FuncVar;
  fIdentFuncTable[959] := FuncVar;
  fIdentFuncTable[2183] := FuncVar;
  fIdentFuncTable[2378] := FuncVar;
  fIdentFuncTable[2370] := FuncVar;
  fIdentFuncTable[1773] := FuncVar;
  fIdentFuncTable[306] := FuncVar;
  fIdentFuncTable[1927] := FuncVar;
  fIdentFuncTable[2342] := FuncVar;
  fIdentFuncTable[326] := FuncVar;

  fIdentFuncTable[2112] := FuncPragma;
  fIdentFuncTable[2197] := FuncPragma;
  fIdentFuncTable[263] := FuncPragma;
  fIdentFuncTable[343] := FuncPragma;
  fIdentFuncTable[1878] := FuncPragma;
  fIdentFuncTable[2638] := FuncPragma;
  fIdentFuncTable[214] := FuncPragma;
  fIdentFuncTable[396] := FuncPragma;
  fIdentFuncTable[838] := FuncPragma;

  fIdentFuncTable[1795] := FuncOperator;
  fIdentFuncTable[1371] := FuncOperator;
  fIdentFuncTable[1302] := FuncOperator;
  fIdentFuncTable[2135] := FuncOperator;
  fIdentFuncTable[311] := FuncOperator;
  fIdentFuncTable[1934] := FuncOperator;
  fIdentFuncTable[110] := FuncOperator;
  fIdentFuncTable[762] := FuncOperator;
  fIdentFuncTable[1129] := FuncOperator;
  fIdentFuncTable[1870] := FuncOperator;
  fIdentFuncTable[2564] := FuncOperator;
end;

function TSynPerlSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

constructor TSynPerlSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator, SYNS_FriendlyAttrOperator);
  AddAttribute(fOperatorAttri);
  fPragmaAttri := TSynHighlighterAttributes.Create(SYNS_AttrPragma, SYNS_FriendlyAttrPragma);
  fPragmaAttri.Style := [fsBold];
  AddAttribute(fPragmaAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  fSpaceAttri.Foreground := clWindow;
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  fVariableAttri.Style := [fsBold];
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterPerl;
  fRange := rsUnknown;
end; { Create }

procedure TSynPerlSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {bit and assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '&':
      begin
        if FLine[Run + 2] = '=' then   {logical and assign}
          inc(Run, 3)
        else                           {logical and}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {bit and}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynPerlSyn.ColonProc;
begin
  Case FLine[Run + 1] of
    ':':                               {double colon}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {colon}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.CommentProc;
begin
  if (Run = 0) or (Run > 0) and (FLine[Run-1] <> '\') then
  begin
    fTokenID := tkComment;
    repeat
      case FLine[Run] of
        #0, #10, #13: break;
      end;
      inc(Run);
    until FLine[Run] = #0;
  end
  else
    Inc(Run);
end;

procedure TSynPerlSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {digraph}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '~':                               {bind scalar to pattern}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.GreaterProc;
begin
  Case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        if FLine[Run + 2] = '=' then   {shift right assign}
          inc(Run, 3)
        else                           {shift right}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.IdentProc;
begin
  case FLine[Run] of
    '$':
      begin
        Case FLine[Run + 1] of
          '!'..'+', '-'..'@', '['..']', '_', '`', '|', '~':
            begin                      {predefined variables}
              inc(Run, 2);
              fTokenID := tkVariable;
              exit;
            end;
          '^':
            begin
              Case FLine[Run + 2] of
                'A', 'D', 'F', 'I', 'L', 'P', 'T', 'W', 'X':
                  begin                {predefined variables}
                    inc(Run, 3);
                    fTokenID := tkVariable;
                    exit;
                  end;
                #0, #10, #13:          {predefined variables}
                  begin
                    inc(Run, 2);
                    fTokenID := tkVariable;
                    exit;
                  end;
              end;
            end;
        end;
      end;
    '%':
      begin
        Case FLine[Run + 1] of
          '=':                         {mod assign}
            begin
              inc(Run, 2);
              fTokenID := tkSymbol;
              exit;
            end;
          #0, #10, #13:                {mod}
            begin
              inc(Run);
              fTokenID := tkSymbol;
              exit;
            end;
        end;
      end;
    'x':
      begin
        Case FLine[Run + 1] of
          '=':                         {repetition assign}
            begin
              inc(Run, 2);
              fTokenID := tkSymbol;
              exit;
            end;
          #0, #10, #13:                {repetition}
            begin
              inc(Run);
              fTokenID := tkSymbol;
              exit;
            end;
        end;
      end;
  end;
  {regular identifier}
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

procedure TSynPerlSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPerlSyn.LowerProc;
begin
  case FLine[Run + 1] of
    '=':
      begin
        if FLine[Run + 2] = '>' then   {compare - less than, equal, greater}
          inc(Run, 3)
        else                           {less than or equal to}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        if FLine[Run + 2] = '=' then   {shift left assign}
          inc(Run, 3)
        else                           {shift left}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {less than}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {arrow}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '~':                               {logical negated bind like =~}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '=':                               {not equal}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {not}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynPerlSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '-', '_', '.', 'A'..'F', 'a'..'f', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  if FLine[Run] = '.' then
  begin
    case FLine[Run + 1] of
      '.':
        begin
          inc(Run, 2);
          if FLine[Run] = '.' then     {sed range}
            inc(Run);

          fTokenID := tkSymbol;        {range}
          exit;
        end;
      '=':
        begin
          inc(Run, 2);
          fTokenID := tkSymbol;        {concatenation assign}
          exit;
        end;
      'a'..'z', 'A'..'Z', '_':
        begin
          fTokenID := tkSymbol;        {concatenation}
          inc(Run);
          exit;
        end;
    end;
  end;
  inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
      '-':                             {check for e notation}
        if not ((FLine[Run + 1] = 'e') or (FLine[Run + 1] = 'E')) then break;
    end;
    inc(Run);
  end;
end;

procedure TSynPerlSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {bit or assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '|':
      begin
        if FLine[Run + 2] = '=' then   {logical or assign}
          inc(Run, 3)
        else                           {logical or}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {bit or}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {add}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.qFunctionProc;                                            //Fiala
var
  myBracket: char;

  function GetFirstBracket: char;
  var
    i: integer;
  begin
    Result := #0;
    i := Run + 1;
    while not IsLineEnd(i) do
    begin
      if CharInSet(FLine[i], [' ', #8, '(', '{']) then
        { we will return opening bracket }
        case FLine[i] of
          '(': begin
            Result := ')';
            Break;
          end;
          '{': begin
            Result := '}';
            Break;
          end;
        end
      { when other than opening bracket or space, we will stop }
      else
        Break;
      inc(i)
    end;

  end;

begin
  { qfunction }
//  myBracket := GetFirstBracket;
//  if ((Run = 0) or (Run > 0) and (FLine[Run-1] = ' ')) and (myBracket <> #0) then
  if (FLine[Run+1] = 'q') and (FLine[Run+2] = '(') or  (FLine[Run+1] = '(') then
  begin
    myBracket := ')';
    fTokenID := tkString;
    repeat
      inc(Run);
    until (FLine[Run] = myBracket) or IsLineEnd(Run);
    if not IsLineEnd(Run) then
      inc(Run);
  end
  else
  begin
  { standard identifier }
    fTokenID := tkIdentifier;
    inc(run);
  end;
end;

procedure TSynPerlSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '=':                               {division assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynPerlSyn.StarProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '*':
      begin
        if FLine[Run + 2] = '=' then   {exponentiation assign}
          inc(Run, 3)
        else                           {exponentiation}
          inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {multiply}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.StringInterpProc;
var
  fBackslashCount : Integer;
begin
  { modification for first quote is backshlashed }
  if (FRange = rsUnknown ) and (FLine[Run - 1] = '\') then                      // Fiala
  begin
    Inc(Run);
    fTokenID := tkSymbol;
    Exit;
  end;

  fTokenID := tkString;
  if FRange = rsUnknown then
    FRange := rsString
  else
    Inc(Run);
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        { If we're looking at a backslash, and the following character is an
          end quote, and it's preceeded by an odd number of backslashes, then
          it shouldn't mark the end of the string.  If it's preceeded by an
          even number, then it should. }
        if (FLine[Run + 1] = #34) then
          begin
            fBackslashCount := 1;

            while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = #92)) do
              fBackslashCount := fBackslashCount + 1;

            if (fBackslashCount mod 2 = 1) then inc(Run)
          end;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] = #34 then
    FRange := rsUnknown;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPerlSyn.StringLiteralProc;
begin
  fTokenID := tkString;
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynPerlSyn.SymbolProc;
begin
  if (Run = 0) or  (Run>0) and (FLine[Run - 1] <> '\') then
    fTokenId := tkSymbol
  else
    fTokenId := tkUnknown;
  inc(Run);
end;

procedure TSynPerlSyn.XOrSymbolProc;
begin
  Case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPerlSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPerlSyn.Next;
begin
  fTokenPos := Run;
  if not IsLineEnd(Run) and (fRange = rsString) then
  begin
    StringEndProc;
  end
  else
  case fLine[Run] of
    '&': AndSymbolProc;
    #13: CRProc;
    ':': ColonProc;
    '#': CommentProc;
    '=': EqualProc;
    '>': GreaterProc;
//    '%', '@', '$', 'A'..'Z', 'a'..'z', '_': IdentProc;
    '$', '%', '@': VariableProc;
    'q': qFunctionProc;                                                         //Fiala
    'A'..'Z', 'a'..'p', 'r'..'z', '_': IdentProc;                               //Fiala
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9', '.': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '*': StarProc;
    '"': StringInterpProc;
    #39: StringLiteralProc;
    '^': XOrSymbolProc;
    '(', ')', '[', ']', '\', '{', '}', ',', ';', '?', '~': SymbolProc;
    else UnknownProc;
  end;
  inherited;
end;

function TSynPerlSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynPerlSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPerlSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPerlSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkOperator: Result := fOperatorAttri;
    tkPragma: Result := fPragmaAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fInvalidAttri;
    tkVariable: Result := fVariableAttri;
    else Result := nil;
  end;
end;

function TSynPerlSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynPerlSyn.GetSampleSource: string;
begin
  Result :=
    '#!/bin/perl'#13#10 +
    'require "cgi-lib.pl";'#13#10 +
    'use sigtrap;'#13#10 +
    'do ''envars.pl'';'#13#10 +
    '$_ = $password1;'#13#10 +
    'sub WriteBack {'#13#10 +
    '        while ($_ ne "fred")    {'#13#10 +
    '                sleep 5;'#13#10 +
    '        }'#13#10 +
    '}';
end;

function TSynPerlSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPerl;
end;

function TSynPerlSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or CharInSet(AChar, ['_', '$', '@', '%']) //Fiala
end;

class function TSynPerlSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPerl;
end;

class function TSynPerlSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangPerl;
end;

{begin}                                                                         //Fiala
procedure TSynPerlSyn.VariableProc;
begin
  { $" is Perl vaiable }                                                        //Fiala
  if CharInSet(fLine[Run + 1], ['$', '"', '%', 'a'..'z', 'A'..'Z', '_', '#']) then
  begin
    fTokenID := tkVariable;
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'z', 'A'..'Z', '_']) do
      Inc(Run);
  end
  else
    IdentProc;
end;

function TSynPerlSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkVariable
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncKey(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncOperator(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkOperator
  else
    Result := tkIdentifier;
end;

function TSynPerlSyn.FuncPragma(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkPragma
  else
    Result := tkIdentifier;
end;
{end}                                                                           //Fiala

function TSynPerlSyn.IsWordBreakChar(AChar: WideChar): Boolean;                 //Fiala
begin
  case AChar of
    #0..#32, '.', ',', ';', ':', '"', '''', '+', '`', '-', '^', '!', '?', '&',
    '§', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>',
    '=', '*', '/', '\', '|':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynPerlSyn.GetRange: Pointer;                                         //Fiala
begin
  Result := Pointer(fRange);
end;

procedure TSynPerlSyn.ResetRange;                                               //Fiala
begin
  fRange:= rsUnknown;
end;

//++ CodeFolding
procedure TSynPerlSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: String;
  Line: Integer;

  function LineHasChar(Line: Integer; character: char;
  StartCol : Integer): boolean; // faster than Pos!
  var
    i: Integer;
  begin
    result := false;
    for I := StartCol to Length(CurLine) do begin
      if CurLine[i] = character then begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, I) <> fCommentAttri then begin
          result := true;
          break;
        end;
      end;
    end;
  end;

  function FindBraces(Line: Integer) : Boolean;
  Var
    Col : Integer;
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
          break;
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
          break;
        end;
      end;
    end; // for Col
  end;

  function FoldRegion(Line: Integer): Boolean;
  Var
    S : string;
  begin
    Result := False;
    S := TrimLeft(CurLine);
    if Uppercase(Copy(S, 1, 9)) = '#REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 12)) = '#ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
  for Line := FromLine to ToLine do
  begin

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

procedure TSynPerlSyn.SetRange(Value: Pointer);                                 //Fiala
begin
  fRange := TRangeState(Value);
end;

procedure TSynPerlSyn.StringEndProc;                                            //Fiala
var
  fBackslashCount : Integer;
begin
  FTokenID := tkString;
  repeat
    case FLine[Run] of
      #34: begin
            Inc(Run);
            fRange := rsUnknown;
            Break;
          end;
      #0, #10, #13: break;
      #92:
        { If we're looking at a backslash, and the following character is an
          end quote, and it's preceeded by an odd number of backslashes, then
          it shouldn't mark the end of the string.  If it's preceeded by an
          even number, then it should. }
        if (FLine[Run + 1] = #34) then
          begin
            fBackslashCount := 1;

            while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = #92)) do
              fBackslashCount := fBackslashCount + 1;

            if (fBackslashCount mod 2 = 1) then inc(Run)
          end;
    end;
    inc(Run);
  until IsLineEnd(Run);
end;

initialization
  RegisterPlaceableHighlighter(TSynPerlSyn);
end.
