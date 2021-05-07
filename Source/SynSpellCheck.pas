////////////////////////////////////////////////////////////////////////////////
// TSynSpellCheck 1.30
//
// Copyright (c) 2002 Jacob Dybala a.k.a. "m3Rlin". All rights reserved.
//
// E-Mail: jacobdybala@synspellcheck.prv.pl
// WWW   : http://www.synspellcheck.prv.pl/ SynSpellCheck Home
//         http://www.delphifaq.net/        Merlin's Delphi Forge
//
// Elf hash algorithm
//   Copyright (c) 1998-2002 Scalabium
//   <http://www.scalabium.com/faq/dct0136.htm>
// SoundEx algorithm
//   Copyright (c) 1995-2001 Borland Software Corporation
// Metaphone Phonetic Hash Algorithm
//   Copyright (c) Tom White <wcstom@yahoo.com>
// Word differences algorithm JHCMP...
//   Copyright (c) Josef Hampl
//
// Created : Jan-10-2002
// Modified: Aug-31-2002
////////////////////////////////////////////////////////////////////////////////
// All dictionaries are located in the 'Program Files\Common\SynSpell' folder.
// This is to limit the number of copies of the same dictionary on a single
// computer to one file.
//
// Dictionaries are flat text files with a single word in each line. All words
// MUST be lowercase. The dictionaries are case insensitive.
////////////////////////////////////////////////////////////////////////////////
// Changes:
//
// 1.30 (Contributed in large by Jan Fiala)
//   * Many, many minor adjustments, optimizations.
//   * Rewritten SetApostrophes().
//   + New word suggestion algorithm: haDiff. Finds words based on differences.
//     haSoundex and haMetaphone *may* be removed in upcoming versions.
//   + New action added: ACTION_UNDO.
//   + New function: GetDictionaryDir(). This allows users to specify their own
//     paths.
//   + Dutch (compiled by Arno Verhoeven) dictionary added.
//
// 1.24 Released privately to certain users.
//   * Bug Fix: PChar and string incompatiblity. Fixed.
//
// 1.23 Released privately to certain users.
//   * Minor code adjustments.
//   + New dictionaries: Norwegian and Spanish.
//
// 1.22
//   * Bug Fix: The Apostrophes property did not allow changing. Fixed.
//     Submitted by R.K. Wittye.
//   * Bug Fix: ClearDictWords did not properly dispose of words creating a
//     rather large memory leak. Fixed. Submitted by Ascher Stefan.
//   * English and Polish dictionaries updated.
//   + Added Value field to TWordRec record. Each word is assigned an Elf value
//     and is checked by it. Major speed optimization. Suggested by Jan Fiala
//     (CRC32).
//
// 1.21
//   * Bug Fix: %ProgramFilesDir%\Common Files was read instead of %CommonFilesDir%.
//     This created problems on non-English versions of Windows. The directory
//     was not found. Fixed.
//   * English and Polish dictionaries updated.
//
// 1.20
//   * FindWord() routine rewritten to make use of cache array. Other functions
//     have only been slightly modified yet no functions have been broken.
//   * LoadDictionary() routine now converts all words to lowercase.
//   * LoadSkipList() does not add the words one-by-one any more. They are
//     assigned in whole.
//   * FSkipList is now cleared when a dictionary is closed.
//   * SaveSkipList() now removes all empty lines before saving to file.
//   + Added cache array to speed up word checks.
//   + ENoDictionaryLoaded is now thrown when no dictionary has been loaded.
//
// 1.19
//   * Bug Fix: Word underlining would also draw on gutter when the word was
//     partially scrolled under it. Fixed.
//   * SoundexLength property converted to HashLength.
//   * PaintUnderLine() code modified to directly color pixels instead of drawing
//     lines.
//   * Dictionary updates: English (1.1.2), Polish (1.1.1). The Polish word list
//     has been *significantly* reduced due to the fact that this word list is
//     being started all over to include words with non-latin characters.
//   + New option: sscoTrimApostrophes.
//   + New properties: Busy and UnderlineStyle (to mimic Corel Wordperfect
//     auto spell checking).
//   + MetaPhone algorithm has been finally implemented. In beta stage (works,
//     but slow on big lists).
//   + AddDictSkipList(), AddDictWordList() routines added.
//   + New dictionaries: German (by Ascher Stefan) and Russian.
//
// 1.18
//   * Bug Fix: OnSkipWord event did not return proper ASkipAll value. Fixed.
//   * Bug Fix: GetDictionaryList() included all copies of dictionaries for a
//     specific language instead of newest. Fixed.
//   * DupeString() has been corrected with proper compiler conditional.
//   * Minor code changes to always pass lowercase words to FindWord().
//   * English dictionary updated to version 1.1.0.
//   * Updated component demo.
//   + New option: sscoMaintainCase. Idea suggested by Jeff Rafter.
//   + New event: OnAddWord.
//   + Added support for words with apostrophes. Idea by Stefan van As.
//   + GetDictionaryList() now returns a sorted list.
//
// 1.17
//   * SelectWordAtCursor() made public.
//   + Added support for localized languages and numbers.
//
// 1.16
//   * Bug Fix: Compiler conditional around SoundEx() routines was broken.
//     Fixed.
//   * Bug Fix: sscoSelectWord did not work when set to False. Fixed.
//   + SelectWordAtCursor() routine added. Contributed by Stefan van As.
//
// 1.15
//   * Bug Fix: PenColor property did not work. Fixed by Jeff Corbets.
//   * Bug Fix: OnAbort event was not called when spell checking was aborted.
//     Fixed.
//   * TSoundEx class has been removed in favor of Borland implementation of
//     SoundEx() function.
//   * Minor code modifications.
//   + Added support for dashed words.
//   + New option: sscoGoUp.
//   + New property: SoundExLength.
//
// 1.14
//   * Bug Fix: If the editor had no text and sscoHourGlass was set the cursor
//     did not revert to it's previous value. Fixed by Jeff Corbets.
//
// 1.13
//   * Bug Fix: When empty lines in base dictionary and user dictionary were
//     added to word list and raised AV when attempting to calculate word hash.
//     Fixed.
//
// 1.12
//   * Bug Fix: GetSuggestions did not properly support words with uppercase
//     characters. Fixed. Found by Jeff Rafter.
//   + Added Metaphon algorithm for word hashes. Not working, just skeleton for
//     now.
//
// 1.11
//   + Added support for multiple editors: AddEditor() and RemoveEditor().
//
// 1.10 (code contributed by Ricardo Cardona)
//   * Bug Fix: When not highlighter was selected and sscoAutoSpellCheck was set
//     in Options the component generated an AV. Fixed.
//   * New property: CheckAttribs.
//   * Improved code for underlining unknown words.
//
// 1.09
//   * Bug Fix: FWordList did not free memory when the component was destroyed.
//     It just cleared the word and hash lists. Fixed.
//
// 1.08
//   * Bug Fix: FindWord() function was case sensitive. Fix contributed by
//     Gerald Nunn.
//   + New events: OnDictClose and OnDictLoad.
//   + New options: sscoAutoSpellCheck (contributed by Ricardo Cardona),
//     sscoIgnoreWordsWithNumbers and sscoIgnoreSingleChars.
//   + New property: PenColor.
//   + Added support for Java documentation.
//
// 1.07
//   * Bug Fix: When spell checking the last word under certain conditions the
//     component would enter an infinite loop. Fixed.
//
// 1.06
//   * Bug Fix: When correcting words in OnCheckWord event the word would not be
//     replaced but added to the beginning of the old one. Fixed.
//   + New dictionary: Danish.
//   + New property: OpenDictionary.
//   + New option: sscoSelectWord.
//
// 1.05
//   + New events: OnCorrectWord and OnSkipWord.
//   + Demo added.
//
// 1.04
//   * Bug Fix: Would not compile under Delphi 6 due to duplicate resource
//     error. Fixed.
//   * GetDictionaryList() now searches for file that match the correct naming
//     scheme - name.major-minor-revision.dic, where major, minor and revision
//     are single characters.
//   + New dictionaries: Italian, Latin, Japanese, Polish, Spanish (Thanks to
//     Ricardo Cardona), and Turkish.
//   + New routines: CloseDictionary(), GetWordCount().
//   + New property: Dictionary.
//   - Removed {$IFDEF SYN_WIN32} directive from GetDictionaryList(). The
//     routines are available under Kylix also.
//   - Removed Version parameter from LoadDictionary.
//
// 1.03
//   + Added /usr/local/SynSpell dir under Linux as the default dictionary
//     directory.
//   + Added Language property.
//   + %ProgramFiles%\Common Files\SynSpell is now dynamically read from system
//     Registry.
//   + Added user dictionary.
//   + Added GetDictionaryList().
//
// 1.02
//   * Bug Fix: When the word list was cleared, the SoundEx list still hogged up
//     the memory =) Fixed.
//   * Bug Fix: When a word was deleted from the dictionary, the SoundEx hash
//     remained undeleted. Therefor, after deleting a word the whole SoundEx
//     hash list after the deleted word was wrong (1 up).
//   * Bug Fix: Suggestions were not passed in ASuggestions in OnCheckWord
//     event. Fixed.
//   * Bug Fix: DeleteSkipWord() fixed to delete form skip word list, not word
//     list ;-)
//   * Bug Fix: editor did not update when searching for words, the screen
//     would "blur". Fixed.
//   * GetSuggestions() changed from procedure to function to return number or
//     words in list.
//   * FWordList is now type of TList instead of TStringList.
//   + If no AAction is specified in the OnCheckWord event, then ACTION_SKIP is
//     default.
//   + Now double words are automatically ignored in FWordList.
//   + Added sscoSuggestWords option.
//   + Added OnAbort event.
//   + Added support for HTML Text.
//   - Removed unsupported options from Options property.
//
// 1.01
//   + Added Options property (support for selecting unknown words in editor,
//     spell checking from cursor, and spell checking only selection, hour glass
//     cursor during spell check, removing cursor visibility during spell
//     check).
//   + Added word suggestion list.
////////////////////////////////////////////////////////////////////////////////

{-----------------------------------------------------------------
  Usage:

  UserDirectory := 'some paths in user profile';
  LoadDictionary('some DIC file');
  Editor := Synedit1;
  // for words underline like words
  Options := Options + [sscoAutoSpellCheck];

  or you can start SpellCheck with dialog, in this case handle events
  In case you add some words to user dictionary, you can save it

  if SynSpellCheck.Modified then
    SaveUserDictionary;
-----------------------------------------------------------------}


{$define ONLY_HADIFF_ALGORITHM}  //don't use Metaphone method to find near word //Fiala

unit SynSpellCheck;

{$I synedit.inc}
{$Define SYN_WIN32}

interface

uses
  Math,
  Classes,
  Graphics,
  Windows,
  Controls,
  Forms,
  JclUnicode,
  StrUtils,
  SysUtils,
  SynEdit,
  SynEditTypes,
{$IFNDEF ONLY_HADIFF_ALGORITHM}
  SynSpellCheckMetaphone,
{$ENDIF}
  comctrls;

type
  THashLength = 1..16;
  TSoundExLength = 1..8;

  TJHCMPLongintArray = array of Longint;
  TJHCMPLongintMatrix = array of TJHCMPLongintArray;

  TLanguageRec = record
    Name, Version: String[50];
  end;

  PWordRec = ^TWordRec;
  TWordRec = record
    Word, Hash: String;  //*
    Value: Integer;
    User: Boolean;
  end;

  TSynEditEx = class(TCustomSynEdit)
  public
    function GetWordAtRowColEx(XY: TBufferCoord; SpellIsIdentChar: TCategoryMethod;
      OverrideHighlighterChars: Boolean): string;
    function SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar: TCategoryMethod):
      TBufferCoord;
    function SCPrevWordPosEx(SpellIsIdentChar, SpellIsWhiteChar: TCategoryMethod):
      TBufferCoord;
    function SCWordEndEx(SpellIsWhiteChar: TCategoryMethod): TBufferCoord;
    function SCWordStartEx(SpellIsWhiteChar: TCategoryMethod): TBufferCoord;
  end;

{$IFNDEF ONLY_HADIFF_ALGORITHM}
  TMetaphone = class(TComponent)
  private
    LengthVar: Integer;
    sIn, sOut: string;
    { Procedures }
    procedure SetLength(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    { Procedures }
    procedure MetaPhone(A: string; lg: Integer; var Res: string);
    procedure Execute;
  published
    { Properties }
    property InString: string read sIn write sIn;
    property OutLength: Integer read LengthVar write SetLength default 20;
    property OutString: string read sOut write sOut;
  end;
{$ENDIF}

  TSynSpellCheck = class;
  TUnderlineStyle = (usCorelWordPerfect, usMicrosoftWord);

  TDrawAutoSpellCheckPlugin = class(TSynEditPlugin)
  private
    FPenColor: TColor;
    FUnderlineStyle: TUnderlineStyle;
    { Procedures }
    procedure SetPenColor(const Value: TColor);
    procedure SetUnderlineStyle(const Value: TUnderlineStyle);
  protected
    FSynSpellCheck: TSynSpellCheck;
    { Procedures }
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect; FirstLine,
      LastLine: Integer); override;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  public
    constructor Create(AOwner: TCustomSynEdit);
    { Properties }
    property PenColor: TColor read FPenColor write SetPenColor default clRed;
    property UnderlineStyle: TUnderlineStyle read FUnderlineStyle
      write SetUnderlineStyle default usMicrosoftWord;
  end;

  { Procedure types }
  TOnAddWord = procedure(Sender: TObject; AWord: string) of object;
  TOnCheckWord = procedure(Sender: TObject; AWord: string;
    ASuggestions: TstringList; var ACorrectWord: string; var AAction: Integer;
    const AUndoEnabled: Boolean = True) of object;
  TOnCorrectWord = procedure(Sender: TObject; AWord, ACorrectWord: string)
    of object;
  TOnSkipWord = procedure(Sender: TObject; AWord: string; ASkipAll: Boolean)
    of object;

  { Sets }
  HashAlgorithms = (haSoundEx, haMetaphone, haDiff);
  SynSpellCheckOptions = (
    sscoAutoSpellCheck,
    sscoGoUp,
    sscoHideCursor,
    sscoHourGlass,
    sscoIgnoreSingleChars,
    sscoIgnoreWordsWithNumbers,
    sscoMaintainCase,
    sscoSelectWord,
    sscoStartFromCursor,
    sscoSuggestWords,
    sscoTrimApostrophes
    );
  TSynSpellCheckOptions = set of SynSpellCheckOptions;

  TSynSpellCheck = class(TComponent)
  private
    FAnsi2Ascii: array[128..255] of Char;  //*
    FCacheArray: array[0..255] of array[0..1] of Cardinal;
//    FIdentChars: set of Char;
    FBusy, FModified, FOpenDictionary, FUseUserDictionary: Boolean;
    FHashAlgorithm: HashAlgorithms;
    FMaxWordLength: Integer;
    FApostrophes, FDictPath, FUserFileName, FUserDictPath: string;
    FDictionary: string;
    FPenColor: TColor;
    FCursor: TCursor;
    FEditor: TCustomSynEdit;
    FDrawAutoSpellCheck: TDrawAutoSpellCheckPlugin;
    FHashLength: THashLength;
    FOnAddWord: TOnAddWord;
    FLanguage: TLanguageRec;
    FEditors, FPlugins, FWordList: TList;
{$IFNDEF ONLY_HADIFF_ALGORITHM}
    FMetaphone: TMetaphone;
{$ENDIF}
    FOnAbort, FOnDictClose, FOnDictLoad, FOnDone, FOnStart: TNotifyEvent;
    FOnCheckWord: TOnCheckWord;
    FOnCorrectWord: TOnCorrectWord;
    FOnSkipWord: TOnSkipWord;
    FCheckAttribs: TstringList;
    FSkipList: TStringList;
    FOptions: TSynSpellCheckOptions;
    FUnderlineStyle: TUnderlineStyle;
    FIntEnc: TEncoding;                                                         //Fiala
    { Functions }
    function FindWord(sWord: string): Integer;
    function GetDefaultDictionaryDir: string;
    function GetDictionaryDir: string;
    function GetUserDictionaryDir: string;
    { Procedures }
    procedure CalculateCacheArray;
    procedure JHCMPInit(const Max1, Max2: longint; var Differences:
      TJHCMPLongintMatrix);
    procedure SetSkipList(Value: TStringList);  //*
    procedure SortWordList;
    procedure SetCheckAttribs(const Value: TstringList);
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure SetHashAlgorithm(const Value: HashAlgorithms);
    procedure SetPenColor(const Value: TColor);
    procedure SetHashLength(const Value: THashLength);
    procedure SetUnderlineStyle(const Value: TUnderlineStyle);
  public
{begin moved from private}                                                      //Fiala
    // Compare table alocation
    // Compare str1 and str2 and Max1 and Max2 are their max lengths
    function JHCMPDiffCount(const Str1, Str2: string): Longint; overload;
    function JHCMPDiffCount(const Str1, Str2: string; Differences: TJHCMPLongintMatrix): Longint; overload;
    function JHCMPFindSimilar(const Word: string; const MaxDiffCount: Integer; const MaxDiffLength: Integer; Similar: Tstrings): Integer;
    // Count number of differences str1 and str2 (case sensitive)
    function JHCMPIsSimilar(const Str1, Str2: string; const MaxDiffCount: Longint): Boolean; overload;
    function JHCMPIsSimilar(const Str1, Str2: string; const MaxDiffCount: Longint; Differences: TJHCMPLongintMatrix): Boolean; overload;
{end}                                                                           //Fiala
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Functions }
    function SpellIsIdentChar(AChar: WideChar): Boolean;
    function SpellIsWhiteChar(AChar: WideChar): Boolean;

    function AddEditor(AEditor: TCustomSynEdit): Integer;
    function Ansi2Ascii(const sString: string): string;       //*
    function CheckWord(Word: string): Boolean;
    function DictionaryExists(Language: string; Path: string = ''): Boolean;
    function GetNewestDictionary(Language: string): string;
    function GetSuggestions(Word: string; SuggestionList: TstringList): Integer;
    function GetWordCount: Integer;
    function GetWordFromASCIIWord(sWord: string): string;
    function IsDictWord(Word: string): Boolean;
    function IsSkipWord(Word: string): Boolean;
    function RemoveEditor(AEditor: TCustomSynEdit): Boolean;
    { Procedures }
    procedure AddDiacritic(Progress: TProgressBar);
    procedure AddDictWord(Word: string);
    procedure AddDictWordList(WordList: TstringList);
    procedure AddSkipWord(Word: string);
    procedure AddSkipWordList(WordList: TstringList);
    procedure ClearDictWords;
    procedure ClearSkipWords;
    procedure CloseDictionary;
    procedure DeleteDictWord(Word: string);
    procedure DeleteSkipWord(Word: string);
    procedure FixLists;
    procedure GetDictionaryList(var tslList: TstringList);
    procedure LoadDictionary(Language: string; FileName: string = '');
    procedure LoadSkipList(FileName: string);
    procedure SaveSkipList(FileName: string);
    procedure SaveUserDictionary;
    procedure SelectWordAtCursor;
    procedure SpellCheck;
  published
    { Properties }
    property Algorithm: HashAlgorithms read FHashAlgorithm write SetHashAlgorithm
      default haDiff;
    property Apostrophes: string read FApostrophes write FApostrophes;
    property Busy: Boolean read FBusy default False;
    property CheckAttribs: TstringList read FCheckAttribs write SetCheckAttribs;
    property Dictionary: string read FDictionary;
    property DictionaryPath: string read GetDictionaryDir write FDictPath;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property HashLength: THashLength read FHashLength write SetHashLength
      default 4;
    property Language: TLanguageRec read FLanguage;
    property Modified: Boolean read FModified write FModified default False;
    property OpenDictionary: Boolean read FOpenDictionary;
    property Options: TSynSpellCheckOptions read FOptions write FOptions;
    property PenColor: TColor read FPenColor write SetPenColor default clRed;
    property SkipList: TStringList read FSkipList write SetSkipList;
    property UnderlineStyle: TUnderlineStyle read FUnderlineStyle
      write SetUnderlineStyle default usMicrosoftWord;
    property UserDirectory: string read GetUserDictionaryDir write
      FUserDictPath;
    property UseUserDictionary: Boolean read FUseUserDictionary write
      FUseUserDictionary default True;
    { Events }
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnAddWord: TOnAddWord read FOnAddWord write FOnAddWord;
    property OnCheckWord: TOnCheckWord read FOnCheckWord write FOnCheckWord;
    property OnCorrectWord: TOnCorrectWord read FOnCorrectWord
      write FOnCorrectWord;
    property OnDictClose: TNotifyEvent read FOnDictClose write FOnDictClose;
    property OnDictLoad: TNotifyEvent read FOnDictLoad write FOnDictLoad;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnSkipWord: TOnSkipWord read FOnSkipWord write FOnSkipWord;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

  ENoDictionaryLoaded = class(EExternal);

resourcestring
  SNoDictionaryLoaded = 'No dictionary is loaded.';

function DupeString(const AText: string; ACount: Integer): string;
{$IFNDEF ONLY_HADIFF_ALGORITHM}
function SoundEx(const AText: string; ALength: TSoundExLength): string;
{$ENDIF}
function ElfHash(const Value: string): Integer;
function TrimEx(const sWord: string; const chChar: WideChar): string;

const
  //////////////////////////////////////////////////////////////////////////////
  // Action constants
  //////////////////////////////////////////////////////////////////////////////
  ACTION_ABORT = -1;
  ACTION_SKIP = 0;
  ACTION_SKIPALL = 1;
  ACTION_CORRECT = 2;
  ACTION_ADD = 3;
  ACTION_UNDO = -2;

procedure Register;

implementation

{$IFDEF SYN_WIN32}
uses
  Dialogs,
  System.Win.Registry,
  SynEditHighlighter,
  SynEditMiscProcs,
  SynHighlighterURI,
  System.UITypes;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('SynEdit', [TSynSpellCheck]);
end;

function ContainsNumbers(sWord: string): Boolean;
var
  iI: Integer;
begin
  Result := False;
  for iI := 1 to Length(sWord) do
    if CharInSet(sWord[iI], ['0'..'9']) then
    begin
      Result := True;
      Break;
    end;
end;

function DupeString(const AText: string; ACount: Integer): string;
var
  P: PWideChar;
  C: Integer;
begin
  C := Length(AText);
  SetLength(Result, C * ACount);
  P := Pointer(Result);
  if P = nil then
    Exit;
  while ACount > 0 do
  begin
    Move(Pointer(AText)^, P^, C * sizeof(WideChar));
    Inc(P, C);
    Dec(ACount);
  end;
end;

function ElfHash(const Value: string): Integer;
var
  iI, iJ: Integer;
begin
  Result := 0;
  for iI := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[iI]);
    iJ := Result and $F0000000;
    if (iJ <> 0) then
      Result := Result xor (iJ shr 24);
    Result := Result and (not iJ);
  end;
end;

procedure JHCMPMatrix(X, Y: Longint; var LongintMatrix: TJHCMPLongintMatrix);
var
  lI: longint;
begin
  SetLength(LongintMatrix, X);
  for lI := Low(LongintMatrix) to High(LongintMatrix) do
    SetLength(LongintMatrix[lI], Y);
end;

procedure JHCMPMatrixInit(var LongintMatrix: TJHCMPLongintMatrix);
var
  lI: Longint;
begin
  for lI := Low(LongintMatrix) to High(LongintMatrix) do
    LongintMatrix[lI][0] := lI;
  for lI := Low(LongintMatrix[0]) to High(LongintMatrix[0]) do
    LongintMatrix[0][lI] := lI;
end;

function JHCMPMin(A, B, C: Longint): Longint;
begin
  Result := Min(Min(A, B), C);
end;

{ TSynSpellCheck }

function TSynSpellCheck.GetDefaultDictionaryDir: string;
begin
{$IFDEF SYN_WIN32}
  Result := 'C:\Program Files\Common Files\SynSpell\';
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion') then
    begin
      if ValueExists('CommonFilesDir') then
        Result := ReadString('CommonFilesDir') + '\SynSpell\';
      CloseKey;
    end;
  finally
    Free;
  end;
{$ELSE}
  Result := '/usr/local/SynSpell/';
{$ENDIF}
end;

function TSynSpellCheck.GetDictionaryDir: string;
begin
{$IFDEF SYN_WIN32}
  if FDictPath <> '' then
    Result := IncludeTrailingBackslash(FDictPath)
  else
    Result := IncludeTrailingBackslash(GetDefaultDictionaryDir);
{$ELSE}
  if FDictPath <> '' then
    Result := FDictPath
  else
    Result := '/usr/local/SynSpell/';
{$ENDIF}
end;

function TSynSpellCheck.GetUserDictionaryDir;
begin
{$IFDEF SYN_WIN32}
  if FUserDictPath <> '' then
    Result := IncludeTrailingBackslash(FUserDictPath)
  else
    Result := IncludeTrailingBackslash(GetDefaultDictionaryDir);
{$ELSE}
  if FUserDictPath <> '' then
    Result := FUserDictPath
  else
    Result := '/usr/local/SynSpell/';
{$ENDIF}
end;

function IsNumber(const PWord: PWideChar): Boolean;
var
  iI: Integer;
begin
  Result := True;
  for iI := 1 to StrLen(PWord) do
    if not CharInSet((PWord + iI)[1], ['0'..'9']) then
    begin
      Result := False;
      Break;
    end;
end;

function SortFunc(Item1, Item2: Pointer): Integer;
begin
 Result := CompareStr(TWordRec(Item1^).Word, TWordRec(Item2^).Word);
end;

{$IFNDEF ONLY_HADIFF_ALGORITHM}
function SoundEx(const AText: string; ALength: TSoundExLength): string;
const
  // This table gives the Soundex score for all characters upper- and lower-
  // case hence no need to convert.  This is faster than doing an UpCase on the
  // whole input string.  The 5 non characters in middle are just given 0.
  CSoundExTable: array[65..122] of ShortInt =
  // A  B  C  D  E  F  G  H   I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W   X  Y  Z
  (0, 1, 2, 3, 0, 1, 2, -1, 0, 2, 2, 4, 5, 5, 0, 1, 2, 6, 2, 3, 0, 1, -1, 2, 0,
    2,
    // [  /  ]  ^  _  '
    0, 0, 0, 0, 0, 0,
    // a  b  c  d  e  f  g  h   i  j  k  l  m  n  o  p  q  r  s  t  u  v  w   x  y  z
    0, 1, 2, 3, 0, 1, 2, -1, 0, 2, 2, 4, 5, 5, 0, 1, 2, 6, 2, 3, 0, 1, -1, 2, 0,
      2);

  function Score(AChar: Integer): Integer;
  begin
    Result := 0;
    if (AChar >= Low(CSoundExTable)) and (AChar <= High(CSoundExTable)) then
      Result := CSoundExTable[AChar];
  end;

var
  iI, LScore, LPrevScore: Integer;
begin
  Result := '';
  if AText <> '' then
  begin
    Result := WideUpperCase(AText[1]);
    LPrevScore := Score(Ord(AText[1]));
    for iI := 2 to Length(AText) do
    begin
      LScore := Score(Ord(AText[iI]));
      if (LScore > 0) and (LScore <> LPrevScore) then
      begin
        Result := Result + IntToStr(LScore);
        if Length(Result) = ALength then
          Break;
      end;
      if LScore <> -1 then
        LPrevScore := LScore;
    end;
    if Length(Result) < ALength then
      Result := Copy(Result + DupeString('0', ALength), 1, ALength);
  end;
end;
{$ENDIF}

function TrimEx(const sWord: string; const chChar: WideChar): string;
var
  iI, iLength: Integer;
begin
  iLength := Length(sWord);
  iI := 1;
  while (iI <= iLength) and (sWord[iI] <= chChar) do
    Inc(iI);
  if iI > iLength then
    Result := ''
  else
  begin
    while sWord[iLength] = chChar do
      Dec(iLength);
    Result := Copy(sWord, iI, iLength - iI + 1);
  end;
end;

{$IFNDEF ONLY_HADIFF_ALGORITHM}
{ TMetaphone }
constructor TMetaphone.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LengthVar := 20; // Looks like a kinda nice value
end;

procedure TMetaphone.Execute;
begin
  if (sIn <> '') and (LengthVar > 0) then
    Metaphone(sIn, LengthVar, sOut)
  else
    sOut := '';
end;

procedure TMetaphone.SetLength(Value: Integer);
begin
  if (Value >= 1) and (Value <= 99) then
    LengthVar := Value;
end;

procedure TMetaphone.Metaphone(A: string; lg: Integer; var Res: string);
var
  iI: Integer;
begin
  for iI := 1 to lg do
    Res := Res + ' ';
  Res := SynSpellCheckMetaphone.Metaphone(PWideChar(a), lg);
end;
{$ENDIF}

{ TDrawAutoSpellCheckPlugin }

constructor TDrawAutoSpellCheckPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited;
  FPenColor := clRed;
  FUnderlineStyle := usMicrosoftWord;
end;

procedure TDrawAutoSpellCheckPlugin.AfterPaint(ACanvas: TCanvas; const AClip:
  TRect;
  FirstLine, LastLine: Integer);
var
  LH, CX: Integer;
  CurrentWord: string;
  Editor: TSynEdit;
  CurrentXY: TBufferCoord;
  TP: TPoint;

  procedure PaintUnderLine;
  var
    MaxX,
      NewPoint,
      NewY: Integer;

    procedure DrawPoint;
    begin
      // Do not draw on gutter.
      // This happens when a word is underlined and part of it is "hidden" under
      // the gutter.
      if TP.X <= Editor.Gutter.RealGutterWidth(Editor.CharWidth) then
        Exit;
      with ACanvas do
      begin
        if NewY = TP.Y - 1 then
          Pen.Color := Editor.Color
        else
          Pen.Color := FPenColor;
        Pixels[TP.X, NewY] := Pen.Color;
      end;
    end;

  const
    // Microsoft Word style
//  MW_POINTS: array[0..6] of ShortInt = (1, 2, 2, 1, 0, 0, 0);
    MW_POINTS: array[0..3] of ShortInt = (0, 1, 2, 1);
    // Corel Word Perfect style
//  WP_POINTS: array[0..4] of ShortInt = (3, 2, 1, -1, -1);
    WP_POINTS: array[0..3] of ShortInt = (2, 1, 0, -1);

  begin
    Inc(TP.Y, LH - 3);
    NewPoint := 0;
    if FUnderlineStyle = usMicrosoftWord then
      NewY := TP.Y + MW_POINTS[NewPoint]
    else
      NewY := TP.Y + WP_POINTS[NewPoint];
    DrawPoint;
    MaxX := TP.X + ACanvas.TextWidth(CurrentWord);
    while TP.X <= MaxX do
    begin
      DrawPoint;
      Inc(NewPoint);
      if FUnderlineStyle = usMicrosoftWord then
      begin
        if NewPoint > High(MW_POINTS) then
          NewPoint := 0
      end
      else
      begin
        if NewPoint > High(WP_POINTS) then
          NewPoint := 0;
      end;
      DrawPoint;
      Inc(TP.X);
      if FUnderlineStyle = usMicrosoftWord then
        NewY := TP.Y + MW_POINTS[NewPoint]
      else
        NewY := TP.Y + WP_POINTS[NewPoint];
    end;
  end;

var
  sToken: string;
  Attri: TSynHighlighterAttributes;
begin
  if not Assigned(FSynSpellCheck) or not Assigned(FSynSpellCheck.Editor) or
    not(sscoAutoSpellCheck in FSynSpellCheck.Options)
  then
    Exit;
  Editor := TSynEdit(FSynSpellCheck.Editor);
  LH := Editor.LineHeight;
  ACanvas.Font.Assign(Editor.Font);
{
  if Editor.WordWrap then
  begin
    FirstLine := Editor.DisplayY;
    LastLine := Editor.DisplayY + Editor.LinesInWindow;
  end;
}
  while FirstLine <= LastLine do
  begin
    // Paint "Bad Words"
    CX := 1;
    while CX < Length(Editor.Lines[FirstLine - 1]) do
    begin
      CurrentXY := BufferCoord(CX, FirstLine);
//      CurrentWord := TSynEditEx(Editor).GetWordAtRowColEx(CurrentXY,
//        TSynEditEx(Editor).IsIdentChar, True);
      CurrentWord := TSynEditEx(Editor).GetWordAtRowColEx(CurrentXY,
        FSynSpellCheck.SpellIsIdentChar, True);
      TP := Editor.RowColumnToPixels(Editor.BufferToDisplayPos(CurrentXY));
      if TP.X > ACanvas.ClipRect.Right - ACanvas.ClipRect.Left then
        Break;
      if Assigned(Editor.Highlighter) and not (Editor.Highlighter is TSynUriSyn) then
      begin
        if Editor.GetHighlighterAttriAtRowCol(CurrentXY, sToken, Attri) = False then
          Attri := Editor.Highlighter.WhitespaceAttribute;
        if Assigned(Attri) and (FSynSpellCheck.FCheckAttribs.IndexOf(Attri.Name) <> -1) and
          (CurrentWord <> '') then
          if FSynSpellCheck.CheckWord(CurrentWord) = False then
            PaintUnderLine;
      end
      else
        if FSynSpellCheck.CheckWord(CurrentWord) = False then
          PaintUnderLine;
      Inc(CX, Length(CurrentWord));
      Inc(CX);
    end;
    Inc(FirstLine);
  end;
end;

procedure TDrawAutoSpellCheckPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
  // This is only for the compiler hint
end;

procedure TDrawAutoSpellCheckPlugin.LinesInserted(FirstLine, Count: Integer);
begin
  // This is only for the compiler hint
end;

procedure TDrawAutoSpellCheckPlugin.SetPenColor(const Value: TColor);
begin
  if FPenColor <> Value then
  begin
    FPenColor := Value;
//    if Editor <> nil then                                                     //Fiala
//      Editor.Repaint;
  end;
end;

procedure TDrawAutoSpellCheckPlugin.SetUnderlineStyle(
  const Value: TUnderlineStyle);
begin
  if FUnderlineStyle <> Value then
  begin
    FUnderlineStyle := Value;
    if Editor <> nil then
      try
        Editor.Repaint;
      except

      end;
  end;
end;

{ TSynSpellCheck }

constructor TSynSpellCheck.Create(AOwner: TComponent);
const
  CP_ASCII = 20127;
var
  iI: Integer;
begin
  inherited Create(AOwner);
  FPenColor := clRed;
  FBusy := False;
  FModified := False;
  FHashAlgorithm := haDiff;
  FHashLength := 4;
  FMaxWordLength := 0;
  FUnderlineStyle := usMicrosoftWord;
  FUseUserDictionary := True;
  FApostrophes := '''`+';
  //////////////////////////////////////////////////////////////////////////////
  // Lists
  //////////////////////////////////////////////////////////////////////////////
  FEditors := TList.Create;
  FPlugins := TList.Create;
  FWordList := TList.Create;
  FSkipList := TStringList.Create;
  FSkipList.Duplicates := dupIgnore;

  FCheckAttribs := TstringList.Create;
  with FCheckAttribs do
  begin
    Add('Comment');
    Add('Text');
    Add('String');
    Add('Documentation');
  end;
  for iI := 1 to 255 do
  begin
    FCacheArray[iI][0] := 0;
    FCacheArray[iI][1] := 0;
  end;
  FIntEnc := TEncoding.GetEncoding(CP_ASCII);                                   //Fiala
end;

destructor TSynSpellCheck.Destroy;
var
  i: Integer;
begin
  for i := FEditors.Count - 1 downto 0 do
    RemoveEditor(TCustomSynEdit(FEditors.Items[i]));
  CloseDictionary;
  //////////////////////////////////////////////////////////////////////////////
  // Free used memory
  //////////////////////////////////////////////////////////////////////////////
  FCheckAttribs.Free;
  FEditors.Free;
{$IFNDEF ONLY_HADIFF_ALGORITHM}
  FMetaphone.Free;
{$ENDIF}
  FPlugins.Free;
  FSkipList.Free;
  FWordList.Free;
  FIntEnc.Free;
  inherited;
end;

function TSynSpellCheck.Ansi2Ascii(const sString: String): String;
var
  by: TBytes;
begin
  by := TEncoding.Unicode.GetBytes(sString);
  by := TEncoding.Convert(TEncoding.Unicode, FIntEnc, by);
  Result := FIntEnc.GetString(by);
end;

function TSynSpellCheck.DictionaryExists(Language: string; Path: string = ''): Boolean;
var
  sTemp: string;
begin
  if WideTrim(Path) = '' then
    sTemp := GetDictionaryDir // Search in shared dictionary directory
  else
    sTemp := Path; // Search in user specified directory
  Result := FileExists(sTemp + Language + '.dic');                              //Fiala
end;

function TSynSpellCheck.GetNewestDictionary(Language: string): string;
var
  srDict: TSearchRec;
  tslTemp: TstringList;
begin
  tslTemp := TstringList.Create;
  if FindFirst(GetDictionaryDir + Language + '.?-?-?.dic', faAnyFile,
    srDict) = 0 then
  begin
    if Pos('.user.', srDict.Name) = 0 then
      tslTemp.Add(WideLowerCase(srDict.Name));
    while FindNext(srDict) = 0 do
    begin
      if Pos('.user.', srDict.Name) = 0 then
        tslTemp.Add(WideLowerCase(srDict.Name));
    end;
  end;
  with tslTemp do
  begin
    if Count > 0 then
    begin
      Sort;
      Result := Strings[Count - 1];
    end;
    Free;
  end;
  SysUtils.FindClose(srDict);
end;

function TSynSpellCheck.GetWordCount: Integer;
begin
  Result := FWordList.Count;
end;

// Returns word from word without diacritic

function TSynSpellCheck.GetWordFromASCIIWord(sWord: string): string;
var
  iI, iJ, iLength: Integer;
  sLower, sTemp: string;

  function CorrectCase(const AsWord: string; const Word: string): string;
  var
    s1, s2, s3, s4: string;
    iX: Integer;
  begin
    s1 := WideUpperCase(AsWord);
    s2 := WideLowerCase(AsWord);
    s3 := WideUpperCase(Word);
    s4 := WideLowerCase(Word);
    Result := Word;
    for iX := 1 to Length(Word) do
    begin
      if s1[iX] = AsWord[iX] then
        Result[iX] := s3[iX]
      else if s2[iX] = AsWord[iX] then
        Result[iX] := s4[iX];
    end;
  end;

begin
  // Are there any words at all starting with this letter?
  sLower := AnsiLowerCase(sWord);
  if FCacheArray[Ord(sLower[1])][1] = 0 then
    Exit;
  if FindWord(sLower) <> -1 then
    Exit;
  iLength := Length(sLower);
  for iI := FCacheArray[Ord(sLower[1])][0] to FCacheArray[Ord(sLower[1])][1] do
  begin
    sTemp := PWordRec(FWordList.Items[iI])^.Word;
    if iLength = Length(sTemp) then
    begin
      // Remove diacritic in dictionary and try find word
      if Ansi2Ascii(sTemp) = sLower then
      begin
        Result := CorrectCase(sWord, sTemp);
        Exit;
      end;
    end;
  end;

  // Not found in base, first char has diacritic, we must continue search
  for iI := 128 to 254 do
  begin
    // Some optimalization
    if FAnsi2Ascii[iI] = sLower[1] then
      for iJ := FCacheArray[iI][0] to FCacheArray[iI][1] do
      begin
        sTemp := PWordRec(FWordList.Items[iJ])^.Word;
        if iLength = Length(sTemp) then
          // Remove diacritic in dictionary and try find word
          if Ansi2Ascii(sTemp) = sLower then
          begin
            Result := CorrectCase(sWord, sTemp);
            Exit;
          end;
      end;
  end;
end;

function TSynSpellCheck.JHCMPDiffCount(const Str1, Str2: String;
  Differences: TJHCMPLongintMatrix): Longint;
var
  I1, I2,
    Length1,
    Length2: Longint;
begin
  Length1 := Length(Str1);
  Length2 := Length(Str2);
  for I1 := 1 to Length1 do
    for I2 := 1 to Length2 do
      if Str1[I1] = Str2[I2] then
        Differences[I1][I2] := Differences[I1 - 1][I2 - 1]
      else
        Differences[I1][I2] := JHCMPMin(Differences[I1 - 1][I2],
          Differences[I1][I2 - 1], Differences[I1 - 1][I2 - 1]) + 1;
  Result := Differences[Length1][Length2];
end;

function TSynSpellCheck.JHCMPDiffCount(const Str1, Str2: String): longint;
var
  Differences: TJHCMPLongintMatrix;
begin
  JHCMPInit(length(Str1), length(Str2), Differences);
  Result := JHCMPDiffCount(Str1, Str2, Differences);
end;

procedure TSynSpellCheck.JHCMPInit(const Max1, Max2: Integer;
  var Differences: TJHCMPLongintMatrix);
begin
  JHCMPMatrix(Max1 + 1, Max2 + 1, Differences);
  JHCMPMatrixInit(Differences);
end;

function TSynSpellCheck.JHCMPIsSimilar(const Str1, Str2: String;
  const MaxDiffCount: Integer; Differences: TJHCMPLongintMatrix): boolean;
begin
  Result := (JHCMPDiffCount(Str1, Str2, Differences) <= MaxDiffCount);
end;

function TSynSpellCheck.JHCMPIsSimilar(const Str1, Str2: String;
  const MaxDiffCount: Integer): Boolean;
var
  Differences: TJHCMPLongintMatrix;
begin
  JHCMPInit(Length(Str1), Length(Str2), Differences);
  Result := JHCMPIsSimilar(Str1, Str2, MaxDiffCount, Differences);
end;


procedure TSynSpellCheck.AddDictWord(Word: string);
var
  AWordItem: PWordRec;

  { Return list position for insert new word }
  function GetInsertPos(const Word: string): Integer;
  var
    iI: Integer;
  begin
    Result := 0;
    // If not any words at all starting with this letter, we find next word
    if FCacheArray[Ord(Word[1])][1] = 0 then
    begin
      for iI := Ord(Word[1]) + 1 to 255 do
        if FCacheArray[iI][1] <> 0 then
        begin
          Result := FCacheArray[iI][0];
          Break;
        end;
    end
    else
      // Words with this letter exists, we find right pos
      for iI := FCacheArray[Ord(Word[1])][0] to
        Succ(FCacheArray[Ord(Word[1])][1]) do
        if PWordRec(FWordList.Items[iI])^.Word > Word then
        begin
          Result := iI;
          Break;
        end;
  end;

begin
  if WideTrim(Word) = '' then
    Exit;
  Word := WideLowerCase(Word);
  if FindWord(Word) = -1 then
  begin
    New(AWordItem);
    FMaxWordLength := Max(FMaxWordLength, Length(Word));
    AWordItem^.Word := Word;
    AWordItem^.User := True;
    AWordItem^.Value := ElfHash(Word);
{$IFNDEF ONLY_HADIFF_ALGORITHM}
    if FHashAlgorithm <> haDiff then
    begin
      if FHashAlgorithm = haSoundEx then
        AWordItem^.Hash := SoundEx(Word, FHashLength)
      else
        AWordItem^.Hash := MetaPhone(PWideChar(Word), FHashLength);
    end;
{$ENDIF}
    // Quickest way is insert one word than add and than sort whole list
    FWordList.Insert(GetInsertPos(Word), AWordItem);
    CalculateCacheArray; // Calculate cache array to speed up searches
    FModified := True;
    if Assigned(FOnAddWord) then
      FOnAddWord(Self, Word);
  end;
end;

procedure TSynSpellCheck.AddDictWordList(WordList: TstringList);
var
  iI: Integer;
begin
  for iI := 0 to WordList.Count - 1 do
    AddDictWord(WordList.Strings[iI]);
end;

function TSynSpellCheck.AddEditor(AEditor: TCustomSynEdit): integer;
var
  i, iI: Integer;                                                               //Fiala
  Plugin: TDrawAutoSpellCheckPlugin;
begin
  // Adds an Editor and returns its index in the list
  Result := -1;
  try
    if Assigned(AEditor) then                                                     //Fiala
    begin
      iI := -1;
      for i := 0 to FEditors.Count - 1 do
        if FEditors.Items[i] = AEditor then
        begin
          iI := i;
          Break;
        end;
      if iI = -1 then
      begin
        Plugin := TDrawAutoSpellCheckPlugin.Create(AEditor);

        with Plugin do
        begin
          FSynSpellCheck := Self;
          PenColor := Self.FPenColor;
          UnderlineStyle := Self.FUnderlineStyle;
        end;
        iI := FEditors.Add(AEditor);
        Sleep(10);
        FPlugIns.Add(Plugin);
        Result := iI;
      end
      else
        Result := iI;
    end;
  except
    // there is problem in x64 editor (not in debug version)                    //Fiala
  end;
end;

procedure TSynSpellCheck.AddSkipWord(Word: string);
begin
  if WideTrim(Word) <> '' then
    FSkipList.Add(WideLowerCase(Word));
end;

procedure TSynSpellCheck.AddSkipWordList(WordList: TstringList);
var
  iI: Integer;
begin
  for iI := 0 to WordList.Count - 1 do
    AddSkipWord(WordList.Strings[iI]);
end;

procedure TSynSpellCheck.CalculateCacheArray;
var
  chOld, chNew: Char;  //*
  iI: Integer;
begin
  if FWordList.Count = 0 then
    Exit;

  chOld := TWordRec(FWordList.Items[0]^).Word[1];
  chNew := chOld;
  FCacheArray[Ord(chOld)][0] := 0; 
  FCacheArray[Ord(chOld)][1] := 0; 
  for iI := 0 to FWordList.Count - 1 do
    if chOld <> TWordRec(FWordList.Items[iI]^).Word[1] then
    begin
      chNew := TWordRec(FWordList.Items[iI]^).Word[1];
      FCacheArray[Ord(chOld)][1] := iI - 1; // Last occurence of previous letter
      FCacheArray[Ord(chNew)][0] := iI; // First occurence of new letter
      chOld := chNew;
    end;
  // Last occurence of last letter
  FCacheArray[Ord(chNew)][1] := FWordList.Count - 1;
end;

function TSynSpellCheck.CheckWord(Word: string): Boolean;
var
  iI: Integer;
begin
  Word := WideTrim(Word);
  if (Word = '') or (sscoIgnoreSingleChars in FOptions) and (Length(Word) = 1)
    then
  begin
    Result := True;
    Exit;
  end;
  // It's quicker to check before checking word list
  if sscoIgnoreWordsWithNumbers in FOptions then
    for iI := 1 to Length(Word) do
      if CharInSet(Word[iI], ['0'..'9']) then
      begin
        Result := True;
        Exit;
      end;

  //////////////////////////////////////////////////////////////////////////////
  // Check if word consists only of dashes or apostrophes. Quite often these
  // are used when dividing sections in ASCII text files.
  //////////////////////////////////////////////////////////////////////////////
  if (TrimEx(Word, '-') = '') or (TrimEx(Word, '''') = '') then
  begin
    Result := True;
    Exit;
  end;

  if sscoTrimApostrophes in FOptions then
    Word := TrimEx(Word, ''''); // Trim apostrophes
  //////////////////////////////////////////////////////////////////////////////
  // Main Searching Routine
  //////////////////////////////////////////////////////////////////////////////
  Result := (FindWord(WideLowerCase(Word)) > -1);
  if not Result and (FSkipList.IndexOf(WideLowerCase(Word)) <> -1) then
    Result := True;
end;

procedure TSynSpellCheck.ClearDictWords;
var
  iI: Integer;
  AWordItem: PWordRec;
begin
  for iI := 0 to FWordList.Count - 1 do
  begin
    AWordItem := FWordList.Items[iI];
    Dispose(AWordItem);
  end;
  FWordList.Clear;
end;

procedure TSynSpellCheck.ClearSkipWords;
begin
  FSkipList.Clear;
end;

procedure TSynSpellCheck.CloseDictionary;
var
  iI: Integer;
begin
  for iI := 0 to 255 do
  begin
    FCacheArray[iI][0] := 0;
    FCacheArray[iI][1] := 0;
  end;
  ClearDictWords;
  FSkipList.Clear;
  FOpenDictionary := False;
  if Assigned(FOnDictClose) then
    FOnDictClose(Self);
end;

procedure TSynSpellCheck.DeleteDictWord(Word: string);
begin
  Dispose(PWordRec(FWordList.Items[FindWord(WideLowerCase(Word))]^));
end;

procedure TSynSpellCheck.DeleteSkipWord(Word: string);
begin
  with FSkipList do
    Delete(IndexOf(WideLowerCase(Word)));
end;

function TSynSpellCheck.IsDictWord(Word: string): Boolean;
begin
  Result := (FindWord(WideLowerCase(Word)) <> -1);
end;

function TSynSpellCheck.IsSkipWord(Word: string): Boolean;
begin
  Result := (FSkipList.IndexOf(WideLowerCase(Word)) <> -1);
end;

procedure TSynSpellCheck.FixLists;
var
  iI: Integer;
begin
  for iI := 0 to FSkipList.Count - 1 do
    FSkipList.Strings[iI] := WideLowerCase(FSkipList.Strings[iI]);
end;

procedure TSynSpellCheck.GetDictionaryList(var tslList: TstringList);
var
  srDics: TSearchRec;

  procedure AddDictionary;
  var
    sLanguage: string;
  begin
    sLanguage := Copy(srDics.Name, 1, Pos('.', srDics.Name) - 1);
    if (tslList.IndexOf(sLanguage) = -1) and (Pos('.user.', srDics.Name) = 0)
      then
      tslList.Add(sLanguage);
  end;

var
  iI: Integer;
begin
  if FindFirst(GetDictionaryDir + '*.?-?-?.dic', faAnyFile, srDics) = 0 then
  begin
    AddDictionary;
    while FindNext(srDics) = 0 do
      AddDictionary;
  end;
  SysUtils.FindClose(srDics);
  for iI := 0 to tslList.Count - 1 do
    tslList.Strings[iI] := WideUpperCase(tslList.Strings[iI][1]) +
      Copy(tslList.Strings[iI], 2, Length(tslList.Strings[iI]));
  tslList.Sort;
end;

function TSynSpellCheck.GetSuggestions(Word: string;
  SuggestionList: TstringList): Integer;
var
  iI,
    iLength: Integer;
  sHash,
    sWord: string;
begin
  Result := 0;
  if not (sscoSuggestWords in FOptions) then
    Exit;
  if Assigned(SuggestionList) then
  begin
    ////////////////////////////////////////////////////////////////////////////
    // Select algorithm
    ////////////////////////////////////////////////////////////////////////////
{$IFNDEF ONLY_HADIFF_ALGORITHM}
    if FHashAlgorithm = haSoundEx then
      sHash := SoundEx(Word, FHashLength)
    else if FHashAlgorithm = haMetaphone then
      sHash := MetaPhone(PWideChar(Word), FHashLength);
{$ENDIF}
    iLength := Length(Word);
    for iI := 0 to FWordList.Count - 1 do
      if (TWordRec(FWordList.Items[iI]^).Hash = sHash) and (Abs(iLength -
        Length(TWordRec(FWordList.Items[iI]^).Word)) < 2) then
      begin
        sWord := TWordRec(FWordList.Items[iI]^).Word;
        if sscoMaintainCase in FOptions then
        begin
          //////////////////////////////////////////////////////////////////////
          // Maintain case for uppercase and capitalized words.
          //////////////////////////////////////////////////////////////////////
          if WideUpperCase(Word) = Word then
            sWord := WideUpperCase(sWord)
          else if WideUpperCase(sWord[1])[1] = Word[1] then
            sWord[1] := WideUpperCase(sWord[1])[1];
        end;
        SuggestionList.Add(sWord);
      end;
    Result := SuggestionList.Count;
  end;
end;

function TSynSpellCheck.JHCMPFindSimilar(const Word: String;
  const MaxDiffCount: Integer; const MaxDiffLength: Integer;
  Similar: Tstrings): Integer;
var
  chFirst: Char;
  iI, iJ, iLength: Integer;
  sLower, sWord: String;
  Differences: TJHCMPLongintMatrix;
begin
  Result := 0;
  if WideTrim(Word) = '' then
    Exit;
  sLower := WideLowerCase(Word);
  chFirst := Ansi2Ascii(sLower[1])[1];
  Similar.Clear;
  JHCMPInit(Length(Word), FMaxWordLength, Differences);

  iLength := Length(Word);
  for iI := FCacheArray[Ord(chFirst)][0] to FCacheArray[Ord(chFirst)][1] do
  begin
    sWord := PWordRec(FWordList.Items[iI])^.Word;
    if Abs(iLength - Length(sWord)) > MaxDiffLength then
      Continue;
    if JHCMPIsSimilar(sLower, sWord, MaxDiffCount, Differences) then
    begin
      if WideUpperCase(Word) = Word then
        sWord := WideUpperCase(sWord)
      else if AnsiUpperCase(Word[1])[1] = Word[1] then
        sWord[1] := AnsiUpperCase(sWord[1])[1];
      Similar.Add(sWord);
    end;
  end;

  // Not found in base, first char has diacritic, we must continue search
  for iJ := 128 to 254 do
    // Some optimalizations
    if (FAnsi2Ascii[iJ] = chFirst) and (FCacheArray[iJ][1] > 0) then
      for iI := FCacheArray[iJ][0] to FCacheArray[iJ][1] do
      begin
        sWord := PWordRec(FWordList.Items[iI])^.Word;
        if Abs(iLength - Length(sWord)) > MaxDiffCount then
          Continue;
        if JHCMPIsSimilar(sLower, sWord, MaxDiffCount, Differences) then
        begin
          if WideUpperCase(Word) = Word then
            sWord := WideUpperCase(sWord)
          else if AnsiUpperCase(Word[1])[1] = Word[1] then
            sWord[1] := AnsiUpperCase(sWord[1])[1];
          Similar.Add(sWord);
        end;
      end;

  Result := Similar.Count;
end;

procedure TSynSpellCheck.LoadDictionary(Language: string; FileName: string = '');
var
  AWordItem: PWordRec;
  sLine, sName: string;
  fOut: TextFile;

  procedure SaveToDriveC;
  var
    i: Integer;
    sl: TStringList;
  begin
    sl := TStringList.Create;
    for i := 0 to FWordList.Count - 1 do
      sl.Add(PWordRec(FWordList.Items[I])^.Word);
    sl.SaveToFile('C:\Dic.txt');
    sl.Free;
  end;

  procedure AddNewWord(sWord: string; IsUser: Boolean);
  begin
    New(AWordItem);
    with AWordItem^ do
    begin
      Word := sWord;
      User := IsUser;
    end;
{$IFNDEF ONLY_HADIFF_ALGORITHM}
    if (sscoSuggestWords in FOptions) and (FHashAlgorithm <> haDiff) then
    begin
      if FHashAlgorithm <> haDiff then
      begin
        if FHashAlgorithm = haSoundEx then
          AWordItem^.Hash := SoundEx(sWord, FHashLength)
        else
          AWordItem^.Hash := MetaPhone(PWideChar(sWord), FHashLength);
      end;
    end;
{$ENDIF}
    FWordList.Add(AWordItem);
  end;

begin
  FMaxWordLength := 0;
  FDictionary := ChangeFileExt(Language, '');
  if WideTrim(FileName) = '' then
    sName := GetDictionaryDir + GetNewestDictionary(Language)
  else
    sName := FileName;
  AssignFile(fOut, sName);
  Reset(fOut);
  while not Eof(fOut) do
  begin
    ReadLn(fOut, sLine);
    if WideTrim(sLine) <> '' then
    begin
      FMaxWordLength := Max(FMaxWordLength, Length(sLine));
      AddNewWord(sLine, False);
    end;
  end;
  CloseFile(fOut);
  sName := ExtractFileName(sName);
  with FLanguage do
  begin
    Name := ShortString(FDictionary);
    Version := ShortString(Copy(sName, Pos('.', sName) + 1, 5));
  end;
  FUserFileName := string(FLanguage.Name) + '.user.dic';
  //////////////////////////////////////////////////////////////////////////////
  // Load user's dictionary if present
  //////////////////////////////////////////////////////////////////////////////
  FModified := False;
  if FUseUserDictionary then
  begin
    if FUserDictPath = '' then
      FUserDictPath := GetUserDictionaryDir;
    sName := IncludeTrailingBackslash(FUserDictPath) + FUserFileName;
    if FileExists(sName) then
    begin
      AssignFile(fOut, sName);
      Reset(fOut);
      while not Eof(fOut) do
      begin
        ReadLn(fOut, sLine);
        FMaxWordLength := Max(FMaxWordLength, Length(sLine));
        if WideTrim(sLine) <> '' then
          AddNewWord(sLine, True);
      end;
      CloseFile(fOut);
      FModified := False;
    end;
  end;

  SortWordList; // Sort the whole word list
  CalculateCacheArray; // Calculate cache array to speed up searches
  FOpenDictionary := True;
  if (sscoAutoSpellCheck in FOptions) and (Assigned(FEditor)) then
    FEditor.Invalidate;
end;

function TSynSpellCheck.FindWord(sWord: String): Integer;
var
  L, H, I, C: Integer;
  sw: string;

begin
  Result := -1;
  if sWord = '' then Exit;
  // Are there any words at all starting with this letter?
  sw := sWord;
  if FCacheArray[Ord(sw[1])][1] = 0 then
    Exit;
  L := FCacheArray[Ord(sw[1])][0];
  H := FCacheArray[Ord(sw[1])][1];
  while L <= H do
  begin
    I := (L + H) shr 1;
    { weak place, in some cases i was greater than word count }                 //Fiala
    if I >= FWordList.Count then
    begin
      Result := -1;
      Exit;
    end;
    { must be CompareStr not AnsiCompareStr, because dictionary is ASCII sorted }
    C := CompareStr(PWordRec(FWordList.Items[I])^.Word, sw);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
        Result := I;
    end;
  end;
end;

procedure TSynSpellCheck.LoadSkipList(FileName: string);
begin
  if FileExists(FileName) then
    FSkipList.LoadFromFile(FileName);
end;

function TSynSpellCheck.RemoveEditor(AEditor: TCustomSynEdit): Boolean;
var
  i, iI: Integer;                                                               //Fiala
begin
  Result := False;
  try
    if Assigned(AEditor) then                                                     //Fiala
    begin
      iI := -1;
      for i := 0 to FEditors.Count - 1 do
        if TCustomSynEdit(FEditors.Items[i]) = AEditor then
        begin
          iI := i;
          Break;
        end;
      if iI > -1 then
      begin
        if FEditor = AEditor then
        begin
          FEditor := nil;
          FDrawAutoSpellCheck := nil;
        end;
        FEditors.Delete(iI);
        FPlugIns.Delete(iI);
        Result := True;
      end;
    end;
  except
    // quiet exception for x64 compiler in 10.4.2                               //Fiala
  end;
end;

procedure TSynSpellCheck.SaveSkipList(FileName: string);
var
  iI: Integer;
begin
  for iI := 0 to FSkipList.Count - 1 do
    if WideTrim(FSkipList.Strings[iI]) = '' then
      FSkipList.Delete(iI);
  FSkipList.SaveToFile(FileName);
end;

procedure TSynSpellCheck.SaveUserDictionary;
var
  iI: Integer;
  fIn: TextFile;
begin
  if not FModified then Exit;
  if not ForceDirectories(ExtractFileDir(FUserDictPath)) then Exit;
  try
    AssignFile(fIn, IncludeTrailingBackslash(FUserDictPath) + FUserFileName);
    Rewrite(fIn);
    for iI := 0 to FWordList.Count - 1 do
      if TWordRec(FWordList.Items[iI]^).User then
        WriteLn(fIn, TWordRec(FWordList.Items[iI]^).Word);
    CloseFile(fIn);
  except
  end;
  FModified := False;
end;

procedure TSynSpellCheck.SelectWordAtCursor;
begin
  if FEditor = nil then
    Exit;
  with TSynEditEx(FEditor) do
  begin
    BlockBegin := SCWordStartEx(SpellIsWhiteChar);
    BlockEnd := SCWordEndEx(SpellIsWhiteChar);
  end;
end;

procedure TSynSpellCheck.SetCheckAttribs(const Value: TstringList);
begin
  FCheckAttribs.Assign(Value);
end;

procedure TSynSpellCheck.SetEditor(const Value: TCustomSynEdit);
var
  iI: Integer;
begin
  if Value <> FEditor then
  begin
    iI := AddEditor(Value);
    if iI > -1 then
    begin
      FEditor := FEditors[iI];
      FDrawAutoSpellCheck := FPlugIns[iI];
      with FDrawAutoSpellCheck do
      begin
        FSynSpellCheck := Self;
        PenColor := Self.FPenColor;
      end;
    end
    else
    begin
      FEditor := nil;
      FDrawAutoSpellCheck := nil;
    end;
  end;
end;

procedure TSynSpellCheck.SetHashAlgorithm(const Value: HashAlgorithms);
{$IFNDEF ONLY_HADIFF_ALGORITHM}
var
  iI: Integer;
  AWordItem: PWordRec;
{$ENDIF}
begin
{$IFDEF ONLY_HADIFF_ALGORITHM}
  FHashAlgorithm := haDiff;
{$ELSE}
  if Value <> FHashAlgorithm then
  begin
    FHashAlgorithm := Value;
    if FWordList.Count > 0 then
      for iI := 0 to FWordList.Count - 1 do
      begin
        AWordItem := FWordList.Items[iI];
        if FHashAlgorithm = haSoundEx then
          AWordItem^.Hash := SoundEx(AWordItem^.Word, FHashLength)
        else
          AWordItem^.Hash := MetaPhone(PWideChar(AWordItem^.Word), FHashLength);
      end;
  end;
{$ENDIF}
end;

procedure TSynSpellCheck.SetHashLength(const Value: THashLength);
{$IFNDEF ONLY_HADIFF_ALGORITHM}
var
  iI: Integer;
  AWordItem: PWordRec;
{$ENDIF}
begin
{$IFDEF ONLY_HADIFF_ALGORITHM}
  FHashLength := Value;
{$ELSE}
  if FHashLength <> Value then
  begin
    ////////////////////////////////////////////////////////////////////////////
    // Soundex hashes are supported up to 8 characters long.
    ////////////////////////////////////////////////////////////////////////////
    if (FHashLength > 8) and (FHashAlgorithm = haMetaphone) then
      FHashLength := 8;
    FHashLength := Value;
    if FWordList.Count > 0 then
      for iI := 0 to FWordList.Count - 1 do
      begin
        AWordItem := FWordList.Items[iI];
        if FHashAlgorithm = haSoundEx then
          AWordItem^.Hash := SoundEx(AWordItem^.Word, FHashLength)
        else
          AWordItem^.Hash := MetaPhone(PWideChar(AWordItem^.Word), FHashLength);
      end;
  end;
{$ENDIF}
end;

procedure TSynSpellCheck.SetPenColor(const Value: TColor);
begin
  FPenColor := Value;
  if FDrawAutoSpellCheck <> nil then
    FDrawAutoSpellCheck.PenColor := Value;
end;

procedure TSynSpellCheck.SetSkipList(Value: TStringList);
begin
  SkipList.Assign(Value);
end;

procedure TSynSpellCheck.SetUnderlineStyle(const Value: TUnderlineStyle);
begin
  FUnderlineStyle := Value;
  if FDrawAutoSpellCheck <> nil then
    FDrawAutoSpellCheck.UnderlineStyle := Value;
end;

procedure TSynSpellCheck.SortWordList;
begin
  FWordList.Sort(SortFunc);
end;

procedure TSynSpellCheck.SpellCheck;
var
  bAborted, bUndoEnabled: Boolean;
  sToken, sWord: string;
  pLastWord, pNextWord: TBufferCoord;
  tslSuggestions: TstringList;
  Attri: TSynHighlighterAttributes;

  function InternalCheckWord(Word: string): Boolean;
  var
    iAction: Integer;
    sCorrectWord: string;
  begin
    Result := True;
    if not CheckWord(WideLowerCase(Word)) then
    begin
      if sscoHideCursor in FOptions then
        FEditor.EndUpdate;
      with FEditor do
      begin
        Update;
        EnsureCursorPosVisible;
      end;
      if sscoHourGlass in FOptions then
        Screen.Cursor := FCursor;
      if Assigned(FOnCheckWord) then
      begin
        // Get suggestions
        if sscoSuggestWords in FOptions then
          if FHashAlgorithm = haDiff then
            JHCMPFindSimilar(Word, 2, 2, tslSuggestions)
          else
            GetSuggestions(Word, tslSuggestions);
        if sscoSelectWord in FOptions then
          SelectWordAtCursor;
        FOnCheckWord(Self, Word, tslSuggestions, sCorrectWord, iAction);
        tslSuggestions.Clear; // Remove items to free some memory
        case iAction of
          ACTION_ABORT:
            begin
              Result := False;
              bAborted := True;
              with FEditor do
              begin
                BlockBegin := CaretXY;
                BlockEnd := BlockBegin;
              end;
            end;
          ACTION_ADD: AddDictWord(sWord);
          ACTION_CORRECT:
            begin
              SelectWordAtCursor;
              TSynEditEx(FEditor).SelText := sCorrectWord;
              if Assigned(FOnCorrectWord) then
                FOnCorrectWord(Self, sWord, sCorrectWord);
            end;
          ACTION_SKIPALL:
            begin
              AddSkipWord(sWord);
              if Assigned(FOnSkipWord) then
                FOnSkipWord(Self, sWord, True);
            end;
          ACTION_SKIP: if Assigned(FOnSkipWord) then
              FOnSkipWord(Self, sWord, False);
          ACTION_UNDO:
            begin
              with TSynEditEx(FEditor) do
              begin
                Undo;
                CaretXY := pLastWord;
                if sscoGoUp in FOptions then
                  CaretXY := SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar)
                else
                  CaretXY := SCPrevWordPosEx(SpellIsIdentChar, SpellIsWhiteChar);
                bUndoEnabled := False;
              end;
            end;
        end;
      end;
      if sscoHourGlass in FOptions then
        Screen.Cursor := crHourGlass;
      if sscoHideCursor in FOptions then
        FEditor.BeginUpdate;
    end;
  end;

begin
  bUndoEnabled := False;
  // If no dictionary if loaded and spell checking is requested and Exception
  // is thrown.
  if not FOpenDictionary then
    raise ENoDictionaryLoaded.CreateRes(@SNoDictionaryLoaded);

  FBusy := True;
//  if Assigned(FOnStart) then
//    FOnStart(Self);
  bAborted := False;
  if sscoHourGlass in FOptions then
  begin
    FCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;
  tslSuggestions := TstringList.Create;

  with TSynEditEx(FEditor) do
  begin
    if WideTrim(Lines.Text) = '' then
    begin
      Screen.Cursor := FCursor;
      FOnDone(Self);
      FBusy := False;
      Exit;
    end;
    if not (sscoStartFromCursor in FOptions) then
      CaretXY := BufferCoord(1, 1);
    if sscoHideCursor in FOptions then
      BeginUpdate;
    if sscoGoUp in FOptions then
      pNextWord := SCPrevWordPosEx(SpellIsIdentChar, SpellIsWhiteChar)
    else
      pNextWord := SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar);
    pLastWord := pNextWord;
    while pNextWord.Char > 0 do
    begin
      Attri := nil;
      //////////////////////////////////////////////////////////////////////////
      // Check if the word is the last word
      // Is cursor at end of text?
      //////////////////////////////////////////////////////////////////////////
      if sscoGoUp in FOptions then
      begin
        if (SCPrevWordPosEx(SpellIsIdentChar, SpellIsWhiteChar).Char = CaretX) and
          (Lines.Count = CaretY) then
          Break;
      end
      else
      begin
        if (SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar).Char = CaretX) and
          (Lines.Count = CaretY) then
          Break;
      end;
      //////////////////////////////////////////////////////////////////////////
      // Make sure we do not get any 'blank' words
      //////////////////////////////////////////////////////////////////////////
      while WideTrim(GetWordAtRowColEx(CaretXY, SpellIsIdentChar, True)) = '' do
      begin
        { Just move to next word }
        if sscoGoUp in FOptions then
          pNextWord := SCPrevWordPosEx(SpellIsIdentChar, SpellIsWhiteChar)
        else
          pNextWord := SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar);
        CaretXY := pNextWord;
        { If it the last word then exit loop }
        if pNextWord.Char = 0 then
          Break;
      end;
      if pNextWord.Char = 0 then
        Break;
      sWord := GetWordAtRowColEx(CaretXY, SpellIsIdentChar, True);
      //////////////////////////////////////////////////////////////////////////
      // Check if the word is in the dictionary
      //////////////////////////////////////////////////////////////////////////
      if (Highlighter = nil) or (Highlighter is TSynURISyn) then
      begin
        if InternalCheckWord(sWord) = False then
          Break;
      end
      else
      begin
        if GetHighlighterAttriAtRowCol(CaretXY, sToken, Attri) = False then
          Attri := Highlighter.WhitespaceAttribute;
        if Assigned(Attri) and (FCheckAttribs.IndexOf(Attri.Name) <> -1) and
          (not InternalCheckWord(sWord)) then
          Break;
      end;
      //////////////////////////////////////////////////////////////////////////
      // Prepare next word position
      //////////////////////////////////////////////////////////////////////////
      if sscoGoUp in FOptions then
        pNextWord := SCPrevWordPosEx(SpellIsIdentChar, SpellIsWhiteChar)
      else
        pNextWord := SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar);
      CaretXY := pNextWord;
    end;
    if sscoHideCursor in FOptions then
      EndUpdate;
  end;
  tslSuggestions.Free;
  if sscoHourGlass in FOptions then
    Screen.Cursor := FCursor;
  //////////////////////////////////////////////////////////////////////////////
  // Remove last word selection
  //////////////////////////////////////////////////////////////////////////////
  with FEditor do
  begin
    BlockBegin := CaretXY;
    BlockEnd := BlockBegin;
  end;
  if bAborted then
  begin
    if Assigned(FOnAbort) then
      FOnAbort(Self)
  end
  else if Assigned(FOnDone) then
    FOnDone(Self);
  FBusy := False;
end;

procedure TSynSpellCheck.AddDiacritic(Progress: TProgressBar);
var
  sToken, sWord: string;
  pNextWord: TBufferCoord;
  Attri: TSynHighlighterAttributes;
  blBeg, blEnd: TBufferCoord;

  procedure InternalCheckWord(Word: string);
  var
    sCorrectWord: string;
  begin
    sCorrectWord := GetWordFromASCIIWord(Word);
    if sCorrectWord <> '' then
    begin
      SelectWordAtCursor;
      TSynEditEx(FEditor).SelText := sCorrectWord;
    end;
  end;

begin
  // If no dictionary if loaded and spell checking is requested and Exception is
  // thrown.
  if not FOpenDictionary then
    raise ENoDictionaryLoaded.CreateRes(@SNoDictionaryLoaded);
  if FEditor = nil then
    Exit;

  if FEditor.SelAvail then
  begin
    if (FEditor.BlockBegin.Line > FEditor.BlockEnd.Line) or
       ((FEditor.BlockBegin.Line = FEditor.BlockEnd.Line) and
       (FEditor.BlockBegin.Char > FEditor.BlockEnd.Char)) then
    begin
      blEnd := FEditor.BlockBegin;
      blBeg := FEditor.BlockEnd;
    end
    else
    begin
      blBeg := FEditor.BlockBegin;
      blEnd := FEditor.BlockEnd;
    end;
  end
  else
  begin
    blBeg := BufferCoord(1, 1);
    blEnd := BufferCoord(0, 0);
  end;

  FBusy := True;
  with TSynEditEx(FEditor) do
  begin
    if WideTrim(Lines.Text) = '' then
    begin
      FBusy := False;
      Exit;
    end;
    CaretXY := blBeg;
    if Progress <> nil then
    begin
      Progress.Min := 0;
      Progress.Max := Lines.Count;
    end;
    pNextWord := SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar);
    while pNextWord.Char > 0 do
    begin
      Attri := nil;
      //////////////////////////////////////////////////////////////////////////
      // Check if the word is the last word
      // Is cursor at end of text?
      //////////////////////////////////////////////////////////////////////////
      if (SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar).Char = CaretX) and
        (Lines.Count = CaretY) then
        Break;
      //////////////////////////////////////////////////////////////////////////
      // Make sure we do not get any 'blank' words
      //////////////////////////////////////////////////////////////////////////
      while WideTrim(GetWordAtRowColEx(CaretXY, SpellIsIdentChar, True)) = '' do
      begin
        pNextWord := SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar);
        CaretXY := pNextWord;
        { If it the last word then exit loop }
        if pNextWord.Char = 0 then
          Break;
      end;
      if pNextWord.Char = 0 then
        Continue;
      sWord := GetWordAtRowColEx(CaretXY, SpellIsIdentChar, True);
      //////////////////////////////////////////////////////////////////////////
      // Check if the word is in the dictionary
      //////////////////////////////////////////////////////////////////////////
      if (Highlighter = nil) or (Highlighter is TSynURISyn) then
        InternalCheckWord(sWord)
      else
      begin
        if GetHighlighterAttriAtRowCol(CaretXY, sToken, Attri) = False then
          Attri := Highlighter.WhitespaceAttribute;
        if Assigned(Attri) and (FCheckAttribs.IndexOf(Attri.Name) <> -1) then
          InternalCheckWord(sWord);
      end;
      //////////////////////////////////////////////////////////////////////////
      // Prepare next word position
      //////////////////////////////////////////////////////////////////////////
      pNextWord := SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar);

      { only work on selection }
      if (blEnd.Line > 0) and ((pNextWord.Line > blEnd.Line) or
        (blEnd.Line = pNextWord.Line) and (pNextWord.Char > blEnd.Char)) then
        Break;

      CaretXY := pNextWord;
      if Progress <> nil then
        if Progress.Position <> CaretY then
        begin
          Progress.Position := CaretY;
          Progress.Update;
        end;
    end;
  end;
  FBusy := False;
end;

function TSynSpellCheck.SpellIsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (Pos(AChar, FApostrophes) > 0) or (AChar='-')
end;

function TSynSpellCheck.SpellIsWhiteChar(AChar: WideChar): Boolean;
begin
  case AChar of
    #0..#32:
      Result := True;
    else
      Result := not SpellIsIdentChar(AChar);
  end
end;

{ TSynEditEx }

function TSynEditEx.GetWordAtRowColEx(XY: TBufferCoord;
  SpellIsIdentChar: TCategoryMethod; OverrideHighlighterChars: Boolean): string;
var
  Line: string;
  Len, Stop: Integer;
begin
  Result := '';
  if (XY.Line >= 1) and (XY.Line <= Lines.Count) then
  begin
    Line := Lines[XY.Line - 1];
    Len := Length(Line);
    if (XY.Char >= 1) and (XY.Char <= Len + 1) then
    begin
      Stop := XY.Char;
      while (Stop <= Len) and SpellIsIdentChar(Line[Stop]) do
        Inc(Stop);
      while (XY.Char > 1) and SpellIsIdentChar(Line[XY.Char - 1]) do
        Dec(XY.Char);
      if Stop > XY.Char then
        Result := Copy(Line, XY.Char, Stop - XY.Char);
    end;
  end;
end;

function TSynEditEx.SCNextWordPosEx(SpellIsIdentChar, SpellIsWhiteChar: TCategoryMethod): TBufferCoord;
var
  CX, CY, LineLen: integer;
  Line: string;

  procedure CheckOnNextLine;
  begin
    // find first IdentChar or multibyte char in the next line
    if CY < Lines.Count then
    begin
      Line := Lines[CY];
      Inc(CY);
      CX := StrScanForCharInCategory(Line, 1, SpellIsIdentChar);
      if CX = 0 then
        CheckOnNextLine;
    end;
  end;

begin
  CX := CaretX;
  CY := CaretY;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    LineLen := Length(Line);
    if CX >= LineLen then
    begin
      CheckOnNextLine;
    end
    else
    begin
      // find next "whitespace" if current char is an IdentChar
      if SpellIsIdentChar(Line[CX]) then
        CX := StrScanForCharInCategory(Line, CX, SpellIsWhiteChar);

      // if "whitespace" found, find the next IdentChar
      if (CX > 0) and (CX < LineLen) then
      begin
        CX := StrScanForCharInCategory(Line, CX, SpellIsIdentChar);
        // if one of those failed just position at the end of the line
        if CX = 0 then
          CheckOnNextLine;
      end
      else
        CheckOnNextLine;
    end;
  end;
  Result := BufferCoord(CX, CY);
end;

function TSynEditEx.SCPrevWordPosEx(SpellIsIdentChar, SpellIsWhiteChar: TCategoryMethod): TBufferCoord;
var
  CX, CY: Integer;
  Line: string;

  procedure CheckForIdentChar;
  begin
    if CX <= 1 then
      Exit;
    // If previous char is a "whitespace" search for the last IdentChar
    if SpellIsWhiteChar(Line[CX - 1]) then
      CX := StrRScanForCharInCategory(Line, CX - 1, SpellIsIdentChar);
    if CX > 0 then
      // Search for the first IdentChar of this "word"
      CX := StrRScanForCharInCategory(Line, CX - 1, SpellIsWhiteChar) + 1;

    if CX = 0 then
    begin
      // Same as CheckOnPrevLine, but we can't have a circular reference
      //  find last cIdentChar in the previous line
      if CY > 1 then
      begin
        Dec(CY);
        Line := Lines[CY - 1];
        while (CY > 1) and (Line = '') do
        begin
          Dec(CY);
          Line := Lines[CY - 1];
        end;
        if Line = '' then
          CX := 1
        else
        begin
          CX := Length(Line) + 1;
          CheckForIdentChar;
        end;
      end
      else
        CX := 1;
    end;
  end;

  procedure CheckOnPrevLine;
  begin
    // Find last IdentChar in the previous line
    if CY > 1 then
    begin
      Dec(CY);
      Line := Lines[CY - 1];
      CX := Length(Line) + 1;
      CheckForIdentChar;
    end
    else
      CX := 1;
  end;

begin
  CX := CaretX;
  CY := CaretY;
  // Valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);
    if CX <= 1 then
      CheckOnPrevLine
    else
      CheckForIdentChar;
  end;
  Result := BufferCoord(CX, CY);
end;

function TSynEditEx.SCWordEndEx(SpellIsWhiteChar: TCategoryMethod): TBufferCoord;
var
  CX, CY: Integer;
  Line: string;
begin
  CX := CaretX;
  CY := CaretY;
  // Valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := StrScanForCharInCategory(Line, CX, SpellIsWhiteChar);
    // If no "whitespace" is found just position at the end of the line
    if CX = 0 then
      CX := Length(Line) + 1;
  end;
  Result := BufferCoord(CX, CY);
end;

function TSynEditEx.SCWordStartEx(SpellIsWhiteChar: TCategoryMethod): TBufferCoord;
var
  CX, CY: Integer;
  Line: string;
begin
  CX := CaretX;
  CY := CaretY;
  // Valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);
    if CX > 1 then // Only find previous char, if not already on start of line
      // If previous char isn't a "whitespace" search for the last IdentChar
      if not SpellIsWhiteChar(Line[CX - 1]) then
        CX := StrRScanForCharInCategory(Line, CX - 1, SpellIsWhiteChar) + 1;
  end;
  Result := BufferCoord(CX, CY);
end;

end.

