{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterRuby.pas, released 2001-11-13.
The Initial Author of this file is Stefan Ascher.
Portions by Jan Verhoeven (http://jansfreeware.com/jfdelphi.htm)
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
@abstract(Provides a Ruby highlighter for SynEdit)
@author(Stefan Ascher <stievie2002@yahoo.com>)
@created(21 May 2001)
@lastmod(2001-11-13)
The SynHighlighterVisualLisp unit provides SynEdit with a Ruby highlighter.
}

unit SynHighlighterRuby;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynFunc,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey,
    tkSpace, tkString, tkSymbol, tkUnknown, tkVariable, tkConstant,
    tkAttribute, tkRegex);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnknown, rsHeredoc, rsIndentedHeredoc, rsBlockComment);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnknown, rsBlockComment);
{$ENDIF}

type
  TSynRubySyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength: Byte;
    fHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fConstantAttri: TSynHighlighterAttributes;
    fAttributeAttri: TSynHighlighterAttributes;
    fRegexAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSecondKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fSecondKeys: TStrings;
    procedure BacktickProc;
    procedure BlockCommentProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PercentProc;
    procedure ReadDelimitedLiteral(AOpenDelim, ACloseDelim: WideChar;
      AAllowNesting: Boolean; ATokenID: TtkTokenKind);
    procedure RegexpProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure VariableProc;
    function CanStartRegexp: Boolean;
    function IsAtLineStart: Boolean;
    function LineStartsWith(const AText: string): Boolean;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
    procedure SetSecondKeys(const Value: TStrings);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function IsKeyword(const AKeyword: string): Boolean; override;
    function IsSecondKeyWord(aToken: string): Boolean;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SecondKeyAttri: TSynHighlighterAttributes read fSecondKeyAttri
      write fSecondKeyAttri;
    property SecondKeyWords: TStrings read fSecondKeys write SetSecondKeys;
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
    property ConstantAttri: TSynHighlighterAttributes read fConstantAttri
      write fConstantAttri;
    property AttributeAttri: TSynHighlighterAttributes read fAttributeAttri
      write fAttributeAttri;
    property RegexAttri: TSynHighlighterAttributes read fRegexAttri
      write fRegexAttri;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst;

const
  RubyKeywordList =
    'BEGIN END __ENCODING__ __FILE__ __LINE__ ' +
    'alias and begin break case class def defined? do else elsif end ensure ' +
    'false for if in module next nil not or redo rescue retry return self ' +
    'super then true undef unless until when while yield';

  RubySecondKeywordList =
    'ARGF ARGV DATA ENV STDERR STDIN STDOUT TOPLEVEL_BINDING ' +
    'RUBY_COPYRIGHT RUBY_DESCRIPTION RUBY_ENGINE RUBY_PATCHLEVEL ' +
    'RUBY_PLATFORM RUBY_RELEASE_DATE RUBY_REVISION RUBY_VERSION ' +
    'Array BasicObject Binding Class Comparable Complex Data Dir Encoding ' +
    'Enumerable Enumerator Exception FalseClass Fiber File Float GC Hash IO ' +
    'Integer Kernel MatchData Math Method Module Mutex NilClass Numeric Object ' +
    'Process Proc Queue Random Range Rational Regexp RubyVM Signal SizedQueue ' +
    'StandardError String Struct Symbol Thread Time TracePoint TrueClass ' +
    'ArgumentError EncodingError FiberError IOError IndexError Interrupt ' +
    'KeyError LoadError LocalJumpError NameError NoMemoryError NoMethodError ' +
    'NotImplementedError RangeError RegexpError RuntimeError ScriptError ' +
    'SecurityError SignalException SyntaxError SystemCallError SystemExit ' +
    'SystemStackError ThreadError TypeError ZeroDivisionError ' +
    '__callee__ __dir__ __method__ abort alias_method append_features ' +
    'at_exit attr attr_accessor attr_reader attr_writer binding block_given? ' +
    'call caller caller_locations catch chomp chop const_defined? const_get ' +
    'const_missing const_set constants define_method delete detect display dup ' +
    'each each_byte each_char each_cons each_entry each_key each_line each_pair ' +
    'each_slice each_value each_with_index empty? enum_for eql? equal? eval ' +
    'exec exit exit! extend fail fetch find first fork format freeze frozen? ' +
    'gem gets global_variables grep group_by has_key? include include? included ' +
    'inherited initialize inject inspect instance_eval instance_exec is_a? ' +
    'itself lambda last length load local_variables loop map method method_missing ' +
    'methods module_function new nil? object_id open p prepend print printf private ' +
    'private_class_method proc protected public public_class_method putc puts ' +
    'raise rand readline readlines reject remove_method require require_relative ' +
    'respond_to? select send set_trace_func singleton_class size sleep sort ' +
    'sort_by spawn sprintf super_method syscall system tap test then throw ' +
    'to_a to_enum to_h to_i to_s trace_var trap untrace_var warn zip';

procedure AddSpaceSeparatedWords(AList: TStrings; const AWords: string);
begin
  ExtractStrings([' ', #9, #10, #13], [], PChar(AWords), AList);
end;

function IsRubyIdentStart(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'A'..'Z', 'a'..'z':
      Result := True;
  else
    Result := False;
  end;
end;

function IsRubyIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'A'..'Z', 'a'..'z':
      Result := True;
  else
    Result := False;
  end;
end;

function IsRubyHexChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'A'..'F', 'a'..'f':
      Result := True;
  else
    Result := False;
  end;
end;

function IsRubyDecChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9':
      Result := True;
  else
    Result := False;
  end;
end;

function IsPercentLiteralType(AChar: WideChar): Boolean;
begin
  case AChar of
    'q', 'Q', 'w', 'W', 'i', 'I', 'r', 's', 'x':
      Result := True;
  else
    Result := False;
  end;
end;

function IsLiteralDelimiter(AChar: WideChar): Boolean;
begin
  Result := (AChar > #32) and not IsRubyIdentChar(AChar);
end;

function MatchingDelimiter(AOpenDelim: WideChar): WideChar;
begin
  case AOpenDelim of
    '(':
      Result := ')';
    '[':
      Result := ']';
    '{':
      Result := '}';
    '<':
      Result := '>';
  else
    Result := AOpenDelim;
  end;
end;

function TSynRubySyn.IsKeyword(const AKeyword: string): Boolean;
begin
  Result := fKeywords.IndexOf(AKeyword) >= 0;
end; { IsKeyWord }

function TSynRubySyn.IsSecondKeyWord(aToken: string): Boolean;
begin
  Result := fSecondKeys.IndexOf(aToken) >= 0;
end; { IsSecondKeyWord }

constructor TSynRubySyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Ruby is case-sensitive.  The original highlighter upper-cased tokens but
  // stored lower-case keywords, so many real Ruby keywords were never matched.
  fCaseSensitive := True;

  fKeyWords := TStringList.Create;
  with TStringList(fKeyWords) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := True;
  end;

  fSecondKeys := TStringList.Create;
  with TStringList(fSecondKeys) do
  begin
    Sorted := True;
    Duplicates := dupIgnore;
    CaseSensitive := True;
  end;

  AddSpaceSeparatedWords(fKeyWords, RubyKeywordList);
  AddSpaceSeparatedWords(fSecondKeys, RubySecondKeywordList);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clGreen;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  fSecondKeyAttri.Foreground := clTeal;
  AddAttribute(fSecondKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clFuchsia;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clRed;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Foreground := clNavy;
  AddAttribute(fSymbolAttri);

  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  fVariableAttri.Foreground := clTeal;
  AddAttribute(fVariableAttri);

  fConstantAttri := TSynHighlighterAttributes.Create(SYNS_AttrConstant, SYNS_FriendlyAttrConstant);
  fConstantAttri.Foreground := clNavy;
  fConstantAttri.Style := [fsBold];
  AddAttribute(fConstantAttri);

  fAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrAttribute, SYNS_FriendlyAttrAttribute);
  fAttributeAttri.Foreground := clTeal;
  AddAttribute(fAttributeAttri);

  fRegexAttri := TSynHighlighterAttributes.Create('RegularExpression', 'Regular Expression');
  fRegexAttri.Foreground := clPurple;
  AddAttribute(fRegexAttri);

  SetAttributesOnChange(DefHighlightChange);

  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterRuby;
end; { Create }

destructor TSynRubySyn.Destroy;
begin
  fKeyWords.Free;
  fSecondKeys.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynRubySyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRubySyn.PointCommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRubySyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
end;

procedure TSynRubySyn.IdentProc;
var
  Token: string;
begin
  while IsIdentChar(fLine[Run]) do Inc(Run);

  // Ruby method names and the reserved word defined? may end with ? or !.
  if FLine[Run] in [WideChar('?'), WideChar('!')] then
    Inc(Run);

  Token := GetToken;
  if IsKeyWord(Token) then
  begin
    fTokenId := tkKey;
    Exit;
  end;

  // Ruby 1.9+ keyword arguments / hash labels: name:, active:, keyword_init:
  if (FLine[Run] = ':') and (FLine[Run + 1] <> ':') then
  begin
    Inc(Run);
    fTokenId := tkAttribute;
    Exit;
  end;

  if IsSecondKeyWord(Token) then
    fTokenId := tkSecondKey
  else if (Token <> '') and (Token[1] in ['A'..'Z']) then
    fTokenId := tkConstant
  else
    fTokenId := tkIdentifier;
end;

procedure TSynRubySyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynRubySyn.LowerProc;
{$IFDEF SYN_HEREDOC}
var
  i, Len, SkipRun: TSynNativeInt;
  IndentedHeredoc: Boolean;
  QuoteChar: WideChar;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  if FLine[Run + 1] = '<' then
  begin
    fTokenID := tkSymbol;

    SkipRun := 0;
    QuoteChar := #0;
    if (FLine[Run + 2] = '-') and (FLine[Run + 3] in
      [WideChar('"'), WideChar(''''), WideChar('`')]) then
    begin
      SkipRun := 2;
      QuoteChar := FLine[Run + 3];
    end
    else
    if (FLine[Run + 2] in [WideChar('-'), WideChar('"'), WideChar(''''), WideChar('`')]) then
    begin
      SkipRun := 1;
      if FLine[Run + 2] <> '-' then
        QuoteChar := FLine[Run + 2];
    end;
    IndentedHeredoc := (SkipRun > 0) and (FLine[Run + 2] = '-');

    if IsIdentChar(FLine[Run + SkipRun + 2]) then
    begin
      Inc(Run, 2);

      i := Run;
      while IsIdentChar(FLine[SkipRun + i]) do Inc(i);
      Len := i - Run;

      if Len > 255 then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if (QuoteChar <> #0) and (FLine[Run + SkipRun + Len] <> QuoteChar) then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if IndentedHeredoc then
        fRange := rsIndentedHeredoc
      else
        fRange := rsHeredoc;
      fHeredocLength := Len;
      fHeredocChecksum := CalcFCS(FLine[Run + SkipRun], Len);

      Inc(Run, SkipRun + Len);
      fTokenID := tkString;
    end
    else
      Inc(Run, 2);
  end
  else
{$ENDIF}
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynRubySyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynRubySyn.NumberProc;
var
  SaveRun: TSynNativeInt;
begin
  fTokenID := tkNumber;

  if FLine[Run] = '0' then
  begin
    case FLine[Run + 1] of
      'x', 'X':
        begin
          Inc(Run, 2);
          while IsRubyHexChar(FLine[Run]) do Inc(Run);
          Exit;
        end;
      'b', 'B':
        begin
          Inc(Run, 2);
          while FLine[Run] in ['_', '0', '1'] do Inc(Run);
          Exit;
        end;
      'o', 'O':
        begin
          Inc(Run, 2);
          while FLine[Run] in ['_', '0'..'7'] do Inc(Run);
          Exit;
        end;
      'd', 'D':
        begin
          Inc(Run, 2);
          while IsRubyDecChar(FLine[Run]) do Inc(Run);
          Exit;
        end;
    end;
  end;

  while IsRubyDecChar(FLine[Run]) do Inc(Run);

  if (FLine[Run] = '.') and (FLine[Run + 1] <> '.') then
  begin
    Inc(Run);
    while IsRubyDecChar(FLine[Run]) do Inc(Run);
  end;

  if FLine[Run] in ['e', 'E'] then
  begin
    SaveRun := Run;
    Inc(Run);
    if FLine[Run] in ['+', '-'] then Inc(Run);
    if FLine[Run] in ['0'..'9'] then
      while IsRubyDecChar(FLine[Run]) do Inc(Run)
    else
      Run := SaveRun;
  end;

  // Ruby rational / imaginary suffixes: 1r, 2.5r, 3i
  if FLine[Run] in ['r', 'i'] then
    Inc(Run);
end;

procedure TSynRubySyn.RoundOpenProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynRubySyn.SlashProc;
begin
  case FLine[Run] of
    '#':
      begin
        fTokenID := tkComment;
        while FLine[Run] <> #0 do
        begin
          case FLine[Run] of
            #10, #13: Break;
          end;
          Inc(Run);
        end;
      end;
    '/':
      begin
        if CanStartRegexp and not IsLineEnd(Run + 1) then
          RegexpProc
        else
          SymbolProc;
      end;
  else
    SymbolProc;
  end;
end;

procedure TSynRubySyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynRubySyn.StringProc;
var
  QuoteChar: WideChar;
begin
  fTokenID := tkString;
  QuoteChar := FLine[Run];      // either " or '
  Inc(Run);

  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not IsLineEnd(Run) then Inc(Run);
      Continue;
    end;

    if FLine[Run] = QuoteChar then
    begin
      Inc(Run);
      Break;
    end;

    Inc(Run);
  end;
end;

procedure TSynRubySyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRubySyn.VariableProc;
begin
  fTokenID := tkVariable;

  // Instance variables, class variables and globals: @x, @@x, $x, $1, $!
  Inc(Run);
  if (FLine[fTokenPos] = '@') and (FLine[Run] = '@') then
    Inc(Run);

  if IsRubyIdentStart(FLine[Run]) then
  begin
    while IsRubyIdentChar(FLine[Run]) do Inc(Run);
    if FLine[Run] in [WideChar('?'), WideChar('!')] then Inc(Run);
  end
  else if FLine[fTokenPos] = '$' then
  begin
    if FLine[Run] in ['0'..'9'] then
      while FLine[Run] in ['0'..'9'] do Inc(Run)
    else if not IsLineEnd(Run) then
      Inc(Run);
  end;
end;

procedure TSynRubySyn.ColonProc;
begin
  if FLine[Run + 1] = ':' then
  begin
    Inc(Run, 2);
    fTokenID := tkSymbol;
  end
  else if IsRubyIdentStart(FLine[Run + 1]) then
  begin
    Inc(Run);
    while IsRubyIdentChar(FLine[Run]) do Inc(Run);
    if FLine[Run] in [WideChar('?'), WideChar('!')] then Inc(Run);
    fTokenID := tkAttribute;
  end
  else
    SymbolProc;
end;

function TSynRubySyn.IsAtLineStart: Boolean;
var
  I: TSynNativeInt;
begin
  Result := True;
  for I := 0 to Run - 1 do
    if not (FLine[I] in [#$0009, #$0020]) then
    begin
      Result := False;
      Exit;
    end;
end;

function TSynRubySyn.LineStartsWith(const AText: string): Boolean;
var
  I, P: TSynNativeInt;
begin
  P := Run;
  while FLine[P] in [#$0009, #$0020] do Inc(P);

  Result := False;
  for I := 1 to Length(AText) do
    if FLine[P + I - 1] <> AText[I] then
      Exit;

  if not IsLineEnd(P + Length(AText)) then
    case FLine[P + Length(AText)] of
      #9, #32: ;
    else
      Exit;
    end;

  Result := True;
end;

procedure TSynRubySyn.EqualProc;
begin
  if IsAtLineStart and LineStartsWith('=begin') then
  begin
    fTokenID := tkComment;
    fRange := rsBlockComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
    SymbolProc;
end;

procedure TSynRubySyn.BlockCommentProc;
begin
  fTokenID := tkComment;
  if LineStartsWith('=end') then
    fRange := rsUnknown;
  while not IsLineEnd(Run) do Inc(Run);
end;

function TSynRubySyn.CanStartRegexp: Boolean;
var
  I: TSynNativeInt;
begin
  I := Run - 1;
  while (I >= 0) and (FLine[I] in [#$0009, #$0020]) do Dec(I);
  if I < 0 then
  begin
    Result := True;
    Exit;
  end;

  case FLine[I] of
    '(', '[', '{', '=', ',', ';', ':', '!', '~', '?', '&', '|', '^', '+', '-', '*', '%', '<', '>':
      Result := True;
  else
    Result := False;
  end;
end;

procedure TSynRubySyn.RegexpProc;
var
  Escaped, InCharClass: Boolean;
begin
  fTokenID := tkRegex;
  Escaped := False;
  InCharClass := False;

  Inc(Run); // leading /
  while not IsLineEnd(Run) do
  begin
    if Escaped then
    begin
      Escaped := False;
      Inc(Run);
      Continue;
    end;

    case FLine[Run] of
      '\':
        begin
          Escaped := True;
          Inc(Run);
          Continue;
        end;
      '[':
        InCharClass := True;
      ']':
        InCharClass := False;
      '/':
        if not InCharClass then
        begin
          Inc(Run);
          while FLine[Run] in ['a'..'z', 'A'..'Z'] do Inc(Run);
          Exit;
        end;
    end;

    Inc(Run);
  end;
end;

procedure TSynRubySyn.ReadDelimitedLiteral(AOpenDelim, ACloseDelim: WideChar;
  AAllowNesting: Boolean; ATokenID: TtkTokenKind);
var
  Depth: Integer;
  Escaped: Boolean;
begin
  fTokenID := ATokenID;
  Depth := 1;
  Escaped := False;

  while not IsLineEnd(Run) do
  begin
    if Escaped then
    begin
      Escaped := False;
      Inc(Run);
      Continue;
    end;

    if FLine[Run] = '\' then
    begin
      Escaped := True;
      Inc(Run);
      Continue;
    end;

    if AAllowNesting and (FLine[Run] = AOpenDelim) then
    begin
      Inc(Depth);
      Inc(Run);
      Continue;
    end;

    if FLine[Run] = ACloseDelim then
    begin
      Dec(Depth);
      Inc(Run);
      if Depth = 0 then Break;
      Continue;
    end;

    Inc(Run);
  end;

  if ATokenID = tkRegex then
    while FLine[Run] in ['a'..'z', 'A'..'Z'] do Inc(Run);
end;

procedure TSynRubySyn.PercentProc;
var
  Prefix, OpenDelim, CloseDelim: WideChar;
  TokenKind: TtkTokenKind;
  AllowNesting: Boolean;
begin
  Prefix := #0;

  if IsPercentLiteralType(FLine[Run + 1]) then
  begin
    Prefix := FLine[Run + 1];
    OpenDelim := FLine[Run + 2];
    if not IsLiteralDelimiter(OpenDelim) then
    begin
      SymbolProc;
      Exit;
    end;
    Inc(Run, 3); // %, type and opening delimiter
  end
  else
  begin
    OpenDelim := FLine[Run + 1];
    if not IsLiteralDelimiter(OpenDelim) then
    begin
      SymbolProc;
      Exit;
    end;
    Inc(Run, 2); // % and opening delimiter
  end;

  CloseDelim := MatchingDelimiter(OpenDelim);
  AllowNesting := OpenDelim in ['(', '[', '{', '<'];
  if Prefix = 'r' then
    TokenKind := tkRegex
  else if Prefix = 's' then
    TokenKind := tkAttribute
  else
    TokenKind := tkString;

  ReadDelimitedLiteral(OpenDelim, CloseDelim, AllowNesting, TokenKind);
end;

procedure TSynRubySyn.BacktickProc;
begin
  fTokenID := tkString;
  Inc(Run);

  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not IsLineEnd(Run) then Inc(Run);
      Continue;
    end;

    if FLine[Run] = '`' then
    begin
      Inc(Run);
      Break;
    end;

    Inc(Run);
  end;
end;

procedure TSynRubySyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynRubySyn.HeredocProc;

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
  I: TSynNativeInt;
begin
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;

  if fRange = rsIndentedHeredoc then
    while FLine[Run] in [#$0009, #$0020] do Inc(Run);

  if ((Run = 0) and (fRange = rsHeredoc)) or (fRange = rsIndentedHeredoc) then
  begin
    I := 0;

    while not IsLineEnd(FLine[Run + I]) do
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

    if (CalcFCS(FLine[Run], I) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      Run := Run + I;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynRubySyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsBlockComment then
    BlockCommentProc
{$IFDEF SYN_HEREDOC}
  else if fRange in [rsHeredoc, rsIndentedHeredoc] then
    HeredocProc
{$ENDIF}
  else
    NextProcedure;
  inherited;
end;

procedure TSynRubySyn.NextProcedure;
begin
  case fLine[Run] of
    '<': LowerProc;
    '#': SlashProc;
    '{', '}', '(', ')', '[', ']', ';', ',', '.', '+', '-', '*', '&', '|', '^', '~', '!', '?', '>': SymbolProc;
    ':': ColonProc;
    '=': EqualProc;
    '@', '$': VariableProc;
    '%': PercentProc;
    '`': BacktickProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34, #39: StringProc;
    else UnknownProc;
  end;
end;

function TSynRubySyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynRubySyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynRubySyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynRubySyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynRubySyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSecondKey: Result := fSecondKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkConstant: Result := fConstantAttri;
    tkAttribute: Result := fAttributeAttri;
    tkRegex: Result := fRegexAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynRubySyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(fTokenId);
end;

procedure TSynRubySyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynRubySyn.SetRange(Value: Pointer);
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
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

procedure TSynRubySyn.SetSecondKeys(const Value: TStrings);
begin
  fSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynRubySyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterRuby;
end;

class function TSynRubySyn.GetLanguageName: string;
begin
  Result := SYNS_LangRuby;
end;

function TSynRubySyn.GetSampleSource: string;
begin
  Result :=
    '# Ruby highlighter sample'+#13#10+
    'User = Struct.new(:id, :name, :active, keyword_init: true) do'+#13#10+
    '  def label'+#13#10+
    '    status = active ? "active" : "inactive"'+#13#10+
    '    "#{id}: #{name} (#{status})"'+#13#10+
    '  end'+#13#10+
    'end'+#13#10+
    ''+#13#10+
    'class Registry'+#13#10+
    '  attr_reader :users'+#13#10+
    '  def initialize(users)'+#13#10+
    '    @users = users'+#13#10+
    '  end'+#13#10+
    'end';
end;

class function TSynRubySyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangRuby;
end;

initialization
  RegisterPlaceableHighlighter(TSynRubySyn);
end.
