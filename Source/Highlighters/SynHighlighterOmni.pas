{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGeneral.pas, released 2000-04-07.
The Original Code is based on the mwGeneralSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions written by Martin Waldenburg are copyright 1999 Martin Waldenburg.

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
@abstract(Provides an Omnipotent customizable highlighter for SynEdit)
@author(Martin Waldenburg, converted to SynEdit by Michael Hieke massively extented by Jan Fiala)
The SynHighlighterOmni unit provides a customizable highlighter for SynEdit.
}

{ TODO : Resource strings }
{ TODO : Fix Structural Highlighting }

unit SynHighlighterOmni;

{$I SynEdit.Inc}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  SynEditHighlighter,
  SynUnicode,
//  JCLUnicode,
  System.Classes,
  SynEditCodeFolding;

const
  SYNS_AttrKey2  = 'Keywords 2';
  SYNS_AttrKey3  = 'Keywords 3';

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber,
    tkPreprocessor, tkSpace, tkString, tkUnknown,
    tkLabel, tkReserved, tkKey2, tkKey3, tkHex, tkValue, tkVariable, tkAttribute);

  { multiline comments }
  TCommentStyle = (csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csDollar,
                   csSmartStyle, csHaskell, csDStyle, csJCLStyle, csHTML,
                   csVLisp, csCLisp, csDead, cs2Excl, csDollarMulti, csForth,
                   csLUA, csLilypond, csHeller, csOkuma, csPwShell, csStarSemicol,
                   csTexInfo, csAutoit);
  TCommentStyles = set of TCommentStyle;

  { singleline comments }
  TSingleStyle = (csCPL, csSharpStyle, csExclStyle, csPercentStyle, csSinglQStyle,
                  csDblQStyle, csByStyle, csPipe, csBasStyle, csLeftBracket,
                  csDMISStyle, csSQLStyle, csFortran, csCStar, csWebFocus,
                  csFoxStyle, csPocoStyle, csSpecStyle, csSlashStyle, csLineC,
                  csBatStyle, csTabKey, cs2Stars, csPCL, csSpace, csJCL,
                  csAutomaton, csDash, csBackSlashStyle, csAngleBrackets,
                  csINIStyle, csEuklid);
  TSingleLineComments = set of TSingleStyle;

  THLGroup = (hgHTML, hgNone);

  TRangeState = (rsUnKnown, rsAnsi, rsPasStyle, rsCStyle, rsDollar,
                 rsSmart, rsHaskell, rsD, rsJCL, rsVLisp, rsCLisp,
                 rsHTML, rs2Excl, rsDead, rsDollarMulti, rsForth,
                 rsLua, rsLilypond, rsHeller, rsOkuma, rsFileName,
                 rsPwShell, rsStarSemicol, rsTexInfo, rsTexInfoBye,
                 rsEuklid, rsRuby, rsAutoIt, rsRubyString1, rsRubyString2);

  TStringDelim = (sdSingleQuote, sdDoubleQuote, sdApostropheQuote);
  TStringDelimiters = set of TStringDelim;

  TCodeFoldingType = (cftNone, cftCurlyBracket);

type
  TSynOmniSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FPreprocessorAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FHexAttri: TSynHighlighterAttributes;
    FKeyWords: TStrings;
    FComments: TCommentStyles;
    FSingleComments: TSingleLineComments;
    FStringDelimCh: WideChar;
    FDetectPreprocessor: Boolean;
    FResAttri: TSynHighlighterAttributes;
    FKey2Attri: TSynHighlighterAttributes;
    FKey3Attri: TSynHighlighterAttributes;
    FLabelAttri: TSynHighlighterAttributes;
    FStringDelim: TStringDelimiters;
    FStringDelimChars: TSysCharSet;
    FKeyWords3: TStrings;
    FResWords: TStrings;
    FKeyWords2: TStrings;
    FLangName: string;
    FKeywordChars: string;
    FHighlighterGroup: THLGroup;
    FHasLabel: Boolean;
    FEscapedStrings: Boolean;
    FEscapeChar: WideChar;
    FVectorSupport: Boolean;
    FCaseSensitive: Boolean;
    FValueAttri: TSynHighlighterAttributes;
    FPHPVariable: Boolean;
    FVariableAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FRubySymbols: Boolean;
    FCodeFoldingType: TCodeFoldingType;
    FRubyStringChar: char;
    FKW2StartWith: Boolean;
    FKW3StartWith: Boolean;
    FKW1StartWith: Boolean;
    FKW4StartWith: Boolean;
    procedure AsciiCharProc;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure BackSlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;

    procedure SymbolProc;
    procedure FortranCommProc;
    procedure REMCommentProc;
    procedure BatCommentProc;
    procedure HTMLCommentProc;
    procedure HexProc;
    procedure PipeProc;
    procedure CommentRangeProc;
    procedure MinusProc;
    procedure LabelProc;
    procedure AsteriskProc;
    procedure PerCentProc;
    procedure ExclamationProc;
    procedure AmpersandProc;
    procedure SingleQuoteProc;
    procedure DoubleQuoteProc;
    procedure RubyStringProc;
    procedure RubyStringProc2;
    procedure RubyRangeProc(const AType: Integer);
    procedure TabKeyProc;
    procedure FileNameProc;
    procedure RestIsCommentProc;
    procedure SetLangName(const Value: string);
    procedure SetKeyWords(const Value: TStrings);
    procedure SetComments(Value: TCommentStyles);
    procedure SetStringDelim(const Value: TStringDelimiters);
    procedure SetDetectPreprocessor(Value: Boolean);
    procedure SetKeyWords2(const Value: TStrings);
    procedure SetKeyWords3(const Value: TStrings);
    procedure SetResWords(const Value: TStrings);
    procedure SetSingleComments(const Value: TSingleLineComments);
    function  IsEsc(const ARun: Integer): Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function GetKeyWords(TokenKind: Integer): string; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    // ScanForFoldRanges is used for CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
    procedure SetRange(Value: Pointer); override;
    function SaveToIniFile(const FileName: string): Boolean;
    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function LoadFromIniFile(const FileName: string): Boolean;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; override;
    procedure SetKeyWordsCompat(FromStrings: TStrings; ToStrings: TStrings);
    function IsReservedWord(const AKeyword: string; KeyList: TStrings; AKWStartWith: Boolean): Boolean;
  published
    property HighLighterGroup: THLGroup read FHighlighterGroup write FHighlighterGroup;
    property KeyWordChars: string read FKeywordChars write FKeywordChars;
    property SingleLineComments: TSingleLineComments read FSingleComments
              write SetSingleComments;
    property HasLabel: Boolean read FHasLabel write FHasLabel;
    property EscapedStrings: Boolean read FEscapedStrings write FEscapedStrings;
    property EscapeChar: WideChar read FEscapeChar write FEscapeChar;
    property VectorSupport: Boolean read FVectorSupport write FVectorSupport;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property KW1StartWith: Boolean read FKW1StartWith write FKW1StartWith;
    property KW2StartWith: Boolean read FKW2StartWith write FKW2StartWith;
    property KW3StartWith: Boolean read FKW3StartWith write FKW3StartWith;
    property KW4StartWith: Boolean read FKW4StartWith write FKW4StartWith;
    property LangName: string read FLangName write SetLangName;
    property CodeFoldingType: TCodeFoldingType read FCodeFoldingType write FCodeFoldingType;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property Comments: TCommentStyles read FComments write SetComments;
    property DetectPreprocessor: Boolean read FDetectPreprocessor
      write SetDetectPreprocessor;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property KeyWords: TStrings read FKeyWords write SetKeyWords;
    property ResAttri: TSynHighlighterAttributes read FResAttri write FResAttri;
    property ResWords: TStrings read FResWords write SetResWords;
    property KeyWords2: TStrings read FKeyWords2 write SetKeyWords2;
    property KeyWords3: TStrings read FKeyWords3 write SetKeyWords3;
    property PHPVariable: Boolean read FPHPVariable write FPHPVariable;
    property RubySymbols: Boolean read FRubySymbols write FRubySymbols;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property PreprocessorAttri: TSynHighlighterAttributes
      read FPreprocessorAttri write FPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
    property StringDelim: TStringDelimiters read FStringDelim write SetStringDelim
      default [sdSingleQuote];
    property LabelAttri: TSynHighlighterAttributes read FLabelAttri
      write FLabelAttri;
    property HexAttri: TSynHighlighterAttributes read FHexAttri
      write FHexAttri;
    property ValueAttri: TSynHighlighterAttributes read FValueAttri
      write FValueAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri
      write FVariableAttri;
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri
      write FAttributeAttri;
  end;

implementation

uses
  System.Win.Registry,
  System.TypInfo,
  SyStem.IniFiles,
  SynEditStrConst,
  SynEditMiscProcs,
  Vcl.Graphics;


function TSynOmniSyn.IsEsc(const ARun: Integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  if not FEscapedStrings then Exit;
  j := 0;
  for i := ARun downto 0 do
    if FLine[i] <> FEscapeChar then
      Break
    else
    if FLine[i] = FEscapeChar then
      Inc(j);
  Result := j Mod 2 = 1;
end;

function TSynOmniSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (Pos(AChar, FKeywordChars) > 0)
    or (FPHPVariable and (AChar='$')) or ((csAutoit in FComments) and (AChar = '#'));
end;

function TSynOmniSyn.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  if (csBasStyle in SingleLineComments) and (AChar = ';') then
    Result := True
  else
    Result := inherited IsWordBreakChar(AChar) and not IsIdentChar(AChar);
end;

constructor TSynOmniSyn.Create(AOwner: TComponent);
  function CreateStringsList: TStringList;
  begin
    Result := TStringList.Create;
    { important, cause we want to use CompareXXX not AnsiCompareXXX }
    Result.UseLocale := False;
    Result.Sorted := True;
    Result.Duplicates := dupIgnore;
  end;
begin
  inherited Create(AOwner);
  FHasLabel := True;
  FKeyWords := CreateStringsList;
  FKeyWords2 := CreateStringsList;
  FKeyWords3 := CreateStringsList;
  FResWords := CreateStringsList;
  FCodeFoldingType := cftNone;
  FRubyStringChar := ' ';
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);
  FResAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_AttrReservedWord);
  FResAttri.Style := [fsBold];
  FResAttri.Foreground := clMaroon;
  AddAttribute(FResAttri);
  FKey2Attri := TSynHighlighterAttributes.Create(SYNS_AttrKey2, SYNS_AttrKey2);
  FKey2Attri.Style := [fsBold];
  AddAttribute(FKey2Attri);
  FKey3Attri := TSynHighlighterAttributes.Create(SYNS_AttrKey3, SYNS_AttrKey3);
  FKey3Attri.Style := [fsBold];
  AddAttribute(FKey3Attri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(FPreprocessorAttri);
  SetAttributesOnChange(DefHighlightChange);
  FLabelAttri := TSynHighlighterAttributes.Create(SYNS_AttrLabel, SYNS_AttrLabel);
  AddAttribute(FLabelAttri);
  FHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(FHexAttri);
  FValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  AddAttribute(FValueAttri);
  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(FVariableAttri);
  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrAttribute, SYNS_FriendlyAttrAttribute);
  AddAttribute(FAttributeAttri);
  FStringDelim := [sdSingleQuote];
  FStringDelimCh := '''';
  FRange := rsUnknown;
  FLangName := SYNS_FriendlyLangUnknown;
  FHighlighterGroup := hgNone;
  FCaseSensitive := False;
  FEscapedStrings := True;
end;

destructor TSynOmniSyn.Destroy;
begin
  FKeyWords.Free;
  FResWords.Free;
  FKeyWords2.Free;
  FKeyWords3.Free;
  inherited Destroy;
end;

procedure TSynOmniSyn.AsciiCharProc;
var
  WasWord: Boolean;
begin
  if (csCLisp in FComments) and (FLine[Run+1] = '|') then
  begin
    FTokenID := tkComment;
    FRange := rsCLisp;
    Inc(Run, 2);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  { HEX value #$0123456789abcdef }
  if (FLine[Run+1] = '$') and CharInSet(FLine[Run + 2], ['0'..'9', 'a'..'f', 'A'..'F']) then
  begin
    FTokenID := tkValue;
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
    begin
      Inc(Run);
    end;
  end
  else
  if (csAutoIt in FComments) and (FLine[Run+1] = 'c') and (FLine[Run+2] = 's') then
  begin
    FTokenID := tkComment;
    FRange := rsAutoIt;
    Inc(Run, 3);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if (csSharpStyle in FSingleComments) and (FLine[Run+1] <> '{') then
  begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  if csPasStyle in Comments then
  begin
    FTokenID := tkString;
    repeat
      Inc(Run);
    until not IsCharAlphaNumeric(FLine[Run]);
  end
  else
  if FDetectPreprocessor then
  begin
    if Trim(FLine)[1] <> '#' then
    begin
      FTokenID := tkSymbol;
      Inc(Run);
      Exit;
    end;
    WasWord := False;
    FTokenID := tkPreprocessor;
    repeat
      if not WasWord and IsIdentChar(FLine[Run]) then WasWord := True;
      Inc(Run);
    until IsLineEnd(Run) or (FLine[Run] = ' ') and WasWord;
(*
    FTokenID := tkPreprocessor;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
*)
  end
  else
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynOmniSyn.Assign(Source: TPersistent);
var
  SourceHL: TSynOmniSyn;
begin
  if Source is TSynOmniSyn then
  begin
    SourceHL := TSynOmniSyn(Source);
    FLangName := SourceHL.LangName;
    FCodeFoldingType := SourceHL.CodeFoldingType;
    FComments := SourceHL.Comments;
    FDetectPreprocessor := SourceHL.DetectPreprocessor;
    FKeyWords.Assign(SourceHL.KeyWords);
    FKeyWords.Assign(SourceHL.KeyWords);
    FResWords.Assign(SourceHL.ResWords);
    FKeyWords2.Assign(SourceHL.KeyWords2);
    FKeyWords3.Assign(SourceHL.KeyWords3);
    FPHPVariable := SourceHL.PHPVariable;
    FRubySymbols := SourceHL.RubySymbols;
    FHighlighterGroup := SourceHL.HighLighterGroup;
    FKeywordChars := SourceHL.KeyWordChars;
    FSingleComments := SourceHL.SingleLineComments;
    FHasLabel := SourceHL.HasLabel;
    FEscapedStrings := SourceHL.EscapedStrings;
    FEscapeChar := SourceHL.EscapeChar;
    FVectorSupport := SourceHL.VectorSupport;
    FCaseSensitive := SourceHL.CaseSensitive;
    FKW1StartWith := SourceHL.KW1StartWith;
    FKW2StartWith := SourceHL.KW2StartWith;
    FKW3StartWith := SourceHL.KW3StartWith;
    FKW4StartWith := SourceHL.KW4StartWith;
  end;
  inherited;
end;

procedure TSynOmniSyn.BraceOpenProc;
begin
  if csLeftBracket in FSingleComments then
  begin
    FTokenID := tkComment;
    FRange := rsUnKnown;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  if (csSmartStyle in FComments) and (FLine[Run+1] = '*') then
  begin
    FTokenID := tkComment;
    FRange := rsSmart;
    Inc(Run, 2);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if (csHaskell in FComments) and (FLine[Run+1] = '-') then
  begin
    FTokenID := tkComment;
    FRange := rsHaskell;
    Inc(Run, 2);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if csPasStyle in FComments then
  begin
    FTokenID := tkComment;
    FRange := rsPasStyle;
    Inc(Run);
    if not IsLineEnd(Run) then
      CommentRangeProc;
//      PasStyleProc;
  end
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynOmniSyn.PointCommaProc;
begin
  if (csVLisp in FComments) and (FLine[Run + 1] = '|') then
  begin
    FTokenID := tkComment;
    FRange := rsVLisp;
    Inc(Run, 2);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  // VLisp comment can start directly after text
  if ((csVLisp in FComments) or (csAutoit in FComments)) and
      (csBasStyle in FSingleComments) then            // Fiala
  begin
    FTokenID := tkComment;
    FRange := rsUnknown;
    Inc(Run);
    while FLine[Run] <> #0 do
    begin
      FTokenID := tkComment;
      Inc(Run);
    end;
  end
  else
  if (csASmStyle in FComments) or (csBasStyle in FSingleComments) then
  begin
    if (Run=0) or CharInSet(FLine[Run-1], [#32, #9]) then
    begin
      FTokenID := tkComment;
      FRange := rsUnknown;
      Inc(Run);
      while FLine[Run] <> #0 do
      begin
        FTokenID := tkComment;
        Inc(Run);
      end;
    end
    else
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end
  else
  //  INI komentar je prvni znak na radku
  if (csINIStyle in FSingleComments) and (Run=0) then
  begin
    FTokenID := tkComment;
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynOmniSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynOmniSyn.IdentProc;
var
  OldRun: Integer;
  tmpToken: string;
  WhiteStart: Boolean;
  I: Integer;
begin
  OldRun := Run;
  WhiteStart := False;
  { check for label - there can be several whitespaces from line begin
    before label: starts }
  if Run < 10 then
  begin
    WhiteStart := True;
    for I := Run - 1 downto 1 do
      if not IsWhiteChar(FLine[I]) then
      begin
        WhiteStart := False;
        Break;
      end;
  end;
  while IsIdentChar(FLine[Run]) do Inc(Run);
  if FHasLabel and (FLine[Run] = ':') and IsWhiteChar(FLine[Run + 1]) and (WhiteStart or
      (OldRun = 2) and CharInSet(FLine[OldRun-1], ['R', 'r'])) then
    begin
      Inc(Run);
      FTokenID := tkLabel;
    end
  else
  begin
    tmpToken := GetToken;
    if IsReservedWord(tmpToken, FKeyWords, FKW1StartWith) then FTokenID := tkKey else
    if IsReservedWord(tmpToken, FResWords, FKW2StartWith) then FTokenID := tkReserved else
    if IsReservedWord(tmpToken, FKeyWords2, FKW3StartWith) then FTokenID := tkKey2 else
    if IsReservedWord(tmpToken, FKeyWords3, FKW4StartWith) then FTokenID := tkKey3 else
      FTokenID := tkIdentifier;
  end;
end;

procedure TSynOmniSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;


begin
  if (csDollarMulti in Comments) then
  begin
    Inc(Run);
    FRange := rsDollarMulti;
    FTokenID := tkComment;
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if (csDollar in Comments) and (FLine[Run + 1] = '(') then
  begin
    Inc(Run);
    FRange := rsDollar;
    FTokenID := tkComment;
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if (csDMISStyle in FSingleComments) and (FLine[Run + 1] = '$') or
     (csDollar in Comments) and (FLine[Run + 1] = '*') then
  begin
    Inc(Run);
    FTokenID := tkComment;
    while not IsLineEnd(Run) do
      Inc(Run);
  end
  else
  if (csDollar in Comments) and (FLine[Run + 1] = ')') then
  begin
    Inc(Run);
    FRange := rsUnKnown;
    FTokenID := tkSymbol;
  end
  else
  if (csCPL in SingleLineComments) then
  begin
    FRange := rsUnKnown;
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  if FPHPVariable then
  begin
    FTokenID := tkVariable;
    repeat
      Inc(Run);
    until not (IsCharAlphaNumeric(FLine[Run]) or (FLine[Run] = '_'));
  end
  else
(*
  if ((csPasStyle in Comments) or (csCStyle in Comments)) and
    ((Run=0) or IsCharAlphaNumeric(FLine[Run-1])) then
  begin
    Inc(Run);
    FTokenID := tkNumber;
    IsNum := True;
    while IsCharAlphaNumeric(FLine[Run]) do
    begin
      if not IsIntegerChar then IsNum := False;
      Inc(Run);
    end;
    if not IsNum then FTokenID := tkUnknown;
  end
  else
*)
  { HEX number $0123456789abcdef }
  if not ((csDollar in Comments) or (csDollarMulti in Comments) or
          (csCPL in SingleLineComments) or (csDMISStyle in SingleLineComments)) and
          CharInSet(FLine[Run + 1], ['0'..'9', 'a'..'f', 'A'..'F']) and
          ((Run = 0) or (FLine[Run-1] <> '#')) then
  begin
    FTokenID := tkHex;
    Inc(Run);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
    begin
      Inc(Run);
    end;
    { zkontroluju, jestli je to po��d ��slo t�eba takov� $Default bralo prvn� 4 znaky jako hex }
    if IsCharAlpha(FLine[Run]) then
      FTokenID := tkUnknown;
  end
  else
  begin
    Inc(Run);
    FTokenID := tkUnknown;
  end;
end;

procedure TSynOmniSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynOmniSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynOmniSyn.NumberProc;
var
  OldRun: Integer;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F', 'h', 'H', '.', 'x':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  OldRun := Run;
  { we will test label started with number }
  while IsIdentChar(FLine[Run]) do Inc(Run);
  if FHasLabel and (OldRun = 0) and (FLine[Run] = ':') then
    begin
      Inc(Run);
      FTokenID := tkLabel;
    end
  else
  { standard number test }
  begin
    Run := OldRun;
    Inc(Run);
    FTokenID := tkNumber;
    while IsNumberChar do
    begin
      case FLine[Run] of
        'x': begin // handle C style hex numbers
               HexProc;
               Break;
             end;
        'a'..'f', 'A'..'F', 'h', 'H': begin
                HexProc;
                Break;
             end;
        '.':
          if FLine[Run + 1] = '.' then Break;
      end;
      Inc(Run);
    end;
  end;
end;

procedure TSynOmniSyn.RoundOpenProc;
begin
  Inc(Run);
  if (csOkuma in FComments) then
  begin
    FRange := rsOkuma;
    FTokenID := tkComment;
    Dec(run);
    repeat
      Inc(Run);
      if (FLine[Run] = ')') then
      begin
        Inc(Run);
        FRange := rsUnKnown;
        Break;
      end
    until IsLineEnd(Run);
  end
  else
  if (csHeller in FComments) and ((FLine[Run] = '*') or (FLine[Run] = '/')) then
  begin
    FRange := rsHeller;
    FTokenID := tkComment;
    repeat
      Inc(Run);
      if (FLine[Run] = ')') then
      begin
        Inc(Run);
        FRange := rsUnKnown;
        Break;
      end
    until IsLineEnd(Run);
  end
  else
  if csAnsiStyle in FComments then
  begin
    case FLine[Run] of
      '*':
        begin
          FTokenID := tkComment;
          FRange := rsAnsi;
          Inc(Run);
          while FLine[Run] <> #0 do
            case FLine[Run] of
              '*':
                if FLine[Run + 1] = ')' then
                begin
                  FRange := rsUnKnown;
                  Inc(Run, 2);
                  Break;
                end else Inc(Run);
              #10: Break;
              #13: Break;
            else Inc(Run);
            end;
        end;
      '.':
        begin
          Inc(Run);
          FTokenID := tkSymbol;
        end;
    else
      begin
        FTokenID := tkSymbol;
      end;
    end;
  end
  else
  if (csForth in FComments) and (FLine[Run] = ' ') then
  begin
    FRange := rsForth;
    FTokenID := tkComment;
    repeat
      Inc(Run);
      if (FLine[Run] = ')') then
      begin
        Inc(Run);
        FRange := rsUnKnown;
        Break;
      end
    until IsLineEnd(Run);
  end
  else
  { Ruby gsub(/.../, ...) - highlight of the content of /.../ }
  if FRubySymbols and (FLine[Run] = '/') then
  begin
    FTokenID := tkSymbol;
    repeat
      Inc(Run);
    until IsLineEnd(Run) or (FLine[Run] = '/');
    Inc(Run);
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynOmniSyn.RubyRangeProc(const AType: Integer);
var
  myLevel: Integer;
  IsRegExp: Boolean;
begin
  myLevel := 0;
  { regular expression }
  if AType > 10 then
    FTokenID := tkAttribute
  { symbols }
  else
    FTokenID := tkValue;

    { handle end of line - we wrote last char }
    case AType of
      1, 11: FRange := rsRubyString1;
      2, 12: FRange := rsRubyString2;
    else
      FRange := rsUnknown;
    end;

    { regular expressions /[ ... ]/  }
    if AType > 10 then
    begin
      repeat
        Inc(Run);
      until IsLineEnd(Run) or (FLine[Run] = ']') and not IsEsc(Run-1) and (FLine[Run+1] = '/');
      if FLine[Run] = ']' then
      begin
        Inc(Run, 2);
        case AType of
          11: FRange := rsRubyString1;
          12: FRange := rsRubyString2;
        else
          FRange := rsUnknown;
        end;
      end
    end
    // symbols #{...}
    else
    begin
      IsRegExp := False;
      { we need to count brackets to get end of symbol }
      repeat
        Inc(Run);
        { we need to skip regular expressions /[...]/ }
        if (FLine[Run-1] = '/') and (FLine[Run] = '[') then
          IsRegExp := True;
        if (FLine[Run] = '/') and (FLine[Run-1] = ']') and  not IsEsc(Run-2) then
          IsRegExp := False;

        { if we are inside regular expression, we don't count brackets }
        if not IsRegExp then
        begin
          if (FLine[Run] = '{') and not IsEsc(Run-1) then
            Inc(myLevel)
          else
          if (FLine[Run] = '}') and not IsEsc(Run-1) then
            // special stupid case of string %}...}
            if FLine[Run-1] = '%' then
              Inc(myLevel)
            else
              Dec(MyLevel);
        end;
      until IsLineEnd(Run) or (FLine[Run] = '}') and (myLevel <= 0);
      if FLine[Run] = '}' then
      begin
        Inc(Run);
        case AType of
          1: FRange := rsRubyString1;
          2: FRange := rsRubyString2;
        else
          FRange := rsUnknown;
        end;
      end;
    end;
end;

procedure TSynOmniSyn.RubyStringProc;
var
  ch: WideChar;
begin
  ch := FLine[Run];
  { RUBY }
  { we are returning from section or symbol to string }
  if (FRange = rsRubyString1) and (ch = '"') then
  begin
    Inc(run);
    FTokenID := tkString;
    FRange := rsUnKnown;
    Exit;
  end;

  { here we continue in multiline string }
  FTokenID := tkString;
  FRange := rsRubyString1;
  { multiline string - we must set string end char }
  if (Run = 0) or (FLine[Run] <> '"') then
    ch := '"';
  if (FLine[Run + 1] = ch) and (FLine[Run + 2] = ch) then
    Inc(Run, 2);
  repeat
    { RUBY - zacatek sekce ve stringu }
    if (FLine[Run] = '#') and (FLine[Run+ 1] = '{') then
    begin
      FRange := rsRubyString1;
      Exit;
    end;
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = ch) and not IsEsc(Run-1);
  if (FLine[Run] = ch) then FRange := rsUnknown;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynOmniSyn.RubyStringProc2;
begin
  FTokenID := tkString;
  { enter string procedure first time - we need to remember string begin char }
  if fRubyStringChar = ' ' then
  begin
    Inc(Run);
    { normal strings %[...], %!...!, e.t.c.}
    if FLine[Run-1] = '%' then
    begin
      { we will handle special case %sx ..... x }
      if FLine[Run] = 's' then
        Inc(Run);
      FRubyStringChar := FLine[Run];
    end;
  end
  else
  { we return from range and first char is end of the string = end }
    if FLine[Run] = FRubyStringChar then
    begin
      Inc(Run);
      FRubyStringChar := ' ';
      FRange := rsUnKnown;
      Exit;
    end;

  FRange := rsRubyString2;
  { fix the string close char }
  case FRubyStringChar of
    '(': FRubyStringChar := ')';
    '<': FRubyStringChar := '>';
    '[': FRubyStringChar := ']';
    '{': FRubyStringChar := '}';
  end;
  repeat
    { RUBY - start of symbol - we end and continue in RubyRangeProc }
    if (FLine[Run] = '#') and (FLine[Run+ 1] = '{') then Exit;
    Inc(Run);
  until IsLineEnd(Run) or (FLine[Run] = '''') and not IsEsc(Run-1)
    or (FRubyStringChar <> ' ') and (FLine[Run] = FRubyStringChar) and not IsEsc(Run-1);
  { end of the string }
  if (FLine[Run] = '''') and not IsEsc(Run-1) or (FRubyStringChar <> ' ') and (FLine[Run] = FRubyStringChar) then
  begin
    Inc(Run);
    fRubyStringChar := ' ';
    FRange := rsUnknown;
  end;
end;

procedure TSynOmniSyn.SlashProc;
var
  IsComment: Boolean;
  i: Integer;
begin
  if (csSpecStyle in FSingleComments) then
  begin
    { slash is first char on line }
    IsComment := (Run = 0);
    if not IsComment then
      for i := Run - 1 downto 1 do
      begin
        if FLine[i] = ';' then
        begin
          IsComment := True;
          Break;
        end;
        if not CharInSet(FLine[i], [#32, #9]) then
        begin
          IsComment := False;
          Break;
        end;
      end;
    if IsComment then
    begin
      Inc(Run);
      FTokenID := tkComment;
      while not IsLineEnd(Run) do Inc(Run);
      Exit;
    end;
  end;
  case FLine[Run + 1] of
    '/':
      begin
        { block comment //* .. *// }
        if (csJCLStyle in FComments) and (FLine[Run + 2] = '*') then
        begin
          Inc(Run, 3);
          FTokenID := tkComment;
          FRange := rsJCL;
          while FLine[Run] <> #0 do
            case FLine[Run] of
              '*':
                if (FLine[Run + 1] = '/') and (FLine[Run + 2] = '/') then
                begin
                  FRange := rsUnKnown;
                  Inc(Run, 3);
                  Break;
                end
                else
                  Inc(Run);
              #10: Break;
              #13: Break;
            else
              Inc(Run);
            end;
        end
        else
        { line comment //* }
        if (csJCL in FSingleComments) and (FLine[Run + 2] = '*') then
        begin
          Inc(Run, 3);
          FTokenID := tkComment;
          while not IsLineEnd(Run) do Inc(Run);
        end
        else
        { line comment // }
        if csSlashStyle in FSingleComments then
        begin
          Inc(Run, 2);
          FTokenID := tkComment;
          while not IsLineEnd(Run) do
          begin
            Inc(Run);
            // PSL support: -- PSL something...
            if Copy(FLine, Run-4, 5) = ' psl ' then
              Break;
          end;
        end
        else
        begin
          Inc(Run);
          FTokenID := tkSymbol;
        end;
      end;
    '*':
      begin
        { block comment /* .. */ }
        if csCStyle in FComments then
        begin
          FTokenID := tkComment;
          FRange := rsCStyle;
          Inc(Run);
          while FLine[Run] <> #0 do
            case FLine[Run] of
              '*':
                if FLine[Run + 1] = '/' then
                begin
                  FRange := rsUnKnown;
                  Inc(Run, 2);
                  Break;
                end
                else
                  Inc(Run);
              #10: Break;
              #13: Break;
            else
              Inc(Run);
            end;
        end
        else
        // line comment /*
        if csLineC in FSingleComments then
        begin
          Inc(Run, 2);
          FTokenID := tkComment;
          while not IsLineEnd(Run) do Inc(Run);
        end
        else
        begin
          Inc(Run);
          FTokenID := tkSymbol;
        end;
      end;
    '+':
      begin
        { block comment /+ .. +/ }
        if csDStyle in FComments then
        begin
          FTokenID := tkComment;
          FRange := rsD;
          Inc(Run);
          while FLine[Run] <> #0 do
            case FLine[Run] of
              '*':
                if FLine[Run + 1] = '/' then
                begin
                  FRange := rsUnKnown;
                  Inc(Run, 2);
                  Break;
                end else Inc(Run);
              #10: Break;
              #13: Break;
            else
              Inc(Run);
            end;
        end
        else
        begin
          Inc(Run);
          FTokenID := tkSymbol;
        end;
      end;
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
  end;

end;

procedure TSynOmniSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynOmniSyn.StringProc;
var
  ch: WideChar;
begin
  ch := FLine[Run];
  { in MathLab is v' vector and not string }
  if FVectorSupport and (ch = '''') then
    if (Run > 0) and IsCharAlphaW(FLine[Run - 1]) then
    begin
      Inc(Run);
      Exit;
    end;
  if (ch = '''') and (csSinglQStyle in FSingleComments) then
  begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  { rest of strings }
  begin
    FTokenID := tkString;
    if (FLine[Run + 1] = ch) and (FLine[Run + 2] = ch) then
      Inc(Run, 2);
    repeat
      case FLine[Run] of
        #0, #10, #13: Break;
      end;
      Inc(Run);
    until (FLine[Run] = ch) and not IsEsc(Run-1);
//    until ((ch = '''') and (FLine[Run] = '''') or (FLine[Run] = '"') or (FLine[Run] = '`'))and not IsEsc(Run-1);
    if IsLineEnd(Run) then
    begin
      if FRubySymbols then
        FRange := rsRubyString2;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynOmniSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynOmniSyn.Next;
begin
  fTokenPos := Run;
  if (FRange = rsRubyString1) and (FLine[Run] = '#') and (FLine[Run+1] = '{') then
    RubyRangeProc(1)
  else
  if (FRange = rsRubyString2) and (FLine[Run] = '#') and (FLine[Run+1] = '{') then
    RubyRangeProc(2)
  else
  if (FRange = rsRubyString1) and not IsLineEnd(Run) then
    RubyStringProc
  else
  if (FRange = rsRubyString2) and not IsLineEnd(Run) then
    RubyStringProc2
  else
  if (FRange <> rsUnknown) and (FRange <> rsRuby) and (FRange <> rsRubyString1)  then
    CommentRangeProc
  else
  {TexInfo}
  if (csTexInfo in Comments) and (Run=0) and (FLine[run] = '@') then
  begin
    if (Copy(FLine, 1, 3) = '@c ') or (Copy(FLine, 1, 8) = '@comment') then
      RestIsCommentProc
    else
    if (Copy(FLine, 1, 7) = '@ignore') then
    begin
      FRange := rsTexInfo;
      RestIsCommentProc;
    end
    else
    if (Copy(FLine, 1, 8) = '@bye') then
    begin
      FRange := rsTexInfoBye;
      RestIsCommentProc;
    end
    else
    begin
      Inc(Run);
      IdentProc;
    end;
  end
  else
  if not ((cs2Stars in FSingleComments) and (FLine[Run] = '*') and (FLine[Run + 1] = '*') or
        (csAutomaton in FSingleComments) and (FLine[Run] = '%') and (FLine[Run + 1] = '%') or
        (csSQLStyle in FSingleComments) and (FLine[Run] = '-') and (FLine[Run + 1] = '-')) and
      (Pos(FLine[Run], FKeywordChars) > 0) then
    IdentProc
  else
  // Descript.ion comment starts with fist space to end of line
  if csSpace in FSingleComments then
  begin
    if Run = 0 then
      FileNameProc
    else
      RestIsCommentProc;
  end
  else
  {PowerShell attribute in square brackets}
  if (csPwShell in FComments) and (Run>0) and (FLine[Run-1] = '[') and IsCharAlpha(FLine[Run]) then
  begin
    FTokenID := tkAttribute;
    while IsCharAlphaNumeric(FLine[Run]) do Inc(Run);
  end
  else
  begin
      case FLine[Run] of
          '''': if (csDead in FComments) and (FLine[Run+1] = '''') then
                begin
                  FTokenID := tkComment;
                  FRange := rsDead;
                  Inc(Run);
                  CommentRangeProc;
                end
                else
                if (csSinglQStyle in FSingleComments) then
                  SingleQuoteProc
                else
                if CharInSet('''', FStringDelimChars) then
                  StringProc
                else
                  UnknownProc;
          '"': if csDblQStyle in FSingleComments then
                  DoubleQuoteProc
                else
                if FRubySymbols and not IsEsc(Run-1) then
                  RubyStringProc
                else
                if CharInSet('"', FStringDelimChars) and not IsEsc(Run-1) then
                  StringProc
                else
                  UnknownProc;
          '`': if CharinSet('`', FStringDelimChars) then
                  StringProc
                else
                  UnknownProc;
          '#':  if FRubySymbols and (FLine[Run+1] = '{') then
                  RubyRangeProc(3)
                else
                  AsciiCharProc;
          '{': BraceOpenProc;
          ';': PointCommaProc;
          #13: CRProc;
          'A', 'B', 'D'..'Q', 'S'..'Z',
          'a', 'b', 'd'..'q', 's'..'z': IdentProc;
//          '_': UnknownProc;
          'C', 'c': FortranCommProc;
          'R', 'r': REMCommentProc;
          '$': IntegerProc;
          #10: LFProc;
          #0: NullProc;
          '|': PipeProc;
          '0'..'9': NumberProc;
          '(': RoundOpenProc;
          '/': SlashProc;
          '\': BackSlashProc;
          #1..#8, #11, #12, #14..#32: SpaceProc;
          #9: TabKeyProc;
          '-': MinusProc;
          '@': LabelProc;
          '*': AsteriskProc;
          '!': ExclamationProc;
          '&': AmpersandProc;
          '%': if FRubySymbols and
                  CharInSet(FLine[Run+1], ['<', '(', '{', '.', '''', '!', '[', ']', '&', 's', '_', '/']) then
                RubyStringProc2
               else
                 PerCentProc;
          ':': BatCommentProc;
          '}', ',', '=', '^', '+', '.', ')', '[', ']', '<': HTMLCommentProc;
          '>', '?': SymbolProc;
        else
          UnknownProc;
      end;
  end;
  inherited;
end;

class function TSynOmniSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcStructureHighlight];
end;

function TSynOmniSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynOmniSyn.GetEol: Boolean;
begin
  Result := (Run >= FLineLen + 1);
end;

function TSynOmniSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynOmniSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynOmniSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkPreprocessor: Result := FPreprocessorAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    tkReserved: Result := FResAttri;
    tkKey2: Result := FKey2Attri;
    tkKey3: Result := FKey3Attri;
    tkLabel: Result := FLabelAttri;
    tkHex: Result := FHexAttri;
    tkValue: Result := FValueAttri;
    tkVariable: Result := FVariableAttri;
    tkAttribute: Result := FAttributeAttri;
  else
    Result := nil;
  end;
end;

function TSynOmniSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynOmniSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynOmniSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynOmniSyn.SetKeyWords(const Value: TStrings);
begin
  SetKeyWordsCompat(Value, FKeyWords);
end;

procedure TSynOmniSyn.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive <> Value then
  begin
    FCaseSensitive := Value;
    TstringList(ResWords).CaseSensitive := FCaseSensitive;
    TstringList(KeyWords).CaseSensitive := FCaseSensitive;
    TstringList(KeyWords2).CaseSensitive := FCaseSensitive;
    TstringList(KeyWords3).CaseSensitive := FCaseSensitive;
  end;
end;

procedure TSynOmniSyn.SetComments(Value: TCommentStyles);
begin
  FComments := Value;
  DefHighLightChange(nil);
end;

class function TSynOmniSyn.GetLanguageName: string;
begin
  Result := SYNS_LangOmni;
end;

function TSynOmniSyn.LoadFromIniFile(const FileName: string): Boolean;
var
  Str: string;
  IniFile: TMemIniFile;
  SL: TStringList;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    IniFile := TMemIniFile.Create(FileName);
    try
      with IniFile do
      begin
        FLangName := ReadString('Settings', 'Name', '');

        Str := ReadString('Settings', 'FileType', '');
        if (Str <> '') and (FLangName <> '') then
        begin
          Str := StringReplace(Str, ',', ';', [rfReplaceAll]);
          FDefaultFilter :=
            Format('%s %s (%s)|%2:s', [FLangName, SYNS_Files, Str]);
        end
        else
          fDefaultFilter := '';

        FCaseSensitive := ReadBool('Settings', 'CaseSensitive', False);

        FVectorSupport := ReadBool('Settings', 'Vectors', False);
        if ReadBool('Settings', 'HTMLGroup', False) then
          FHighLighterGroup := hgHTML
        else
          FHighLighterGroup := hgNone;

        FComments := [];
        if ReadBool('Settings', 'ANSIComment', False) then Include(FComments, csAnsiStyle);
        if ReadBool('Settings', 'PasComment',  False) then Include(FComments, csPasStyle);
        if ReadBool('Settings', 'CComment', False) then Include(FComments, csCStyle);
        if ReadBool('Settings', 'DollarComment', False) then Include(FComments, csDollar);
        if ReadBool('Settings', 'SmartComment', False) then Include(FComments, csSmartStyle);
        if ReadBool('Settings', 'HaskellComment', False) then Include(FComments, csHaskell);
        if ReadBool('Settings', 'DComment', False) then Include(FComments, csDStyle);
        if ReadBool('Settings', 'JCLComment', False) then Include(FComments, csJCLStyle);
        if ReadBool('Settings', 'VLispComment', False) then Include(FComments, csVLisp);
        if ReadBool('Settings', 'CLispComment', False) then Include(FComments, csCLisp);
        if ReadBool('Settings', 'DeadComment', False) then Include(FComments, csDead);
        if ReadBool('Settings', '2Exclamation', False) then Include(FComments, cs2Excl);
        if ReadBool('Settings', 'DollarMultiComment', False) then Include(FComments, csDollarMulti);
        if ReadBool('Settings', 'ForthComment', False) then Include(FComments, csForth);
        if ReadBool('Settings', 'HTMLComment', False) then Include(FComments, csHTML);
        if ReadBool('Settings', 'LuaComment', False) then Include(FComments, csLUA);
        if ReadBool('Settings', 'LilypondComment', False) then Include(FComments, csLilypond);
        if ReadBool('Settings', 'OkumaComment', False) then Include(FComments, csOkuma);
        if ReadBool('Settings', 'HellerComment', False) then Include(FComments, csHeller);
        if ReadBool('Settings', 'PwShellComment', False) then Include(FComments, csPwShell);
        if ReadBool('Settings', 'SSComment', False) then Include(FComments, csStarSemicol);
        if ReadBool('Settings', 'TexInfoComment', False) then Include(FComments, csTexInfo);
        if ReadBool('Settings', 'AutoitBlockComm', False) then Include(FComments, csAutoit);

        FSingleComments := [];
        if ReadBool('Settings', 'CPLComment', False) then Include(FSingleComments, csCPL);
        if ReadBool('Settings', 'SpecComment', False) then Include(FSingleComments, csSpecStyle);
        if ReadBool('Settings', 'BackSlashComment', False) then Include(FSingleComments, csBackSlashStyle);
        if ReadBool('Settings', 'LineCComment', False) then Include(FSingleComments, csLineC);
        if ReadBool('Settings', 'BasComment', False) then Include(FSingleComments, csBasStyle);
        if ReadBool('Settings', 'INIComment', False) then Include(FSingleComments, csINIStyle);
        if ReadBool('Settings', 'FoxComment', False) then Include(FSingleComments, csFoxStyle);
        if ReadBool('Settings', 'REMComment', False) then Include(FSingleComments, csBatStyle);
        if ReadBool('Settings', 'ExclComment', False) then Include(FSingleComments, csExclStyle);
        if ReadBool('Settings', 'ByComment', False) then Include(FSingleComments, csByStyle);
        if ReadBool('Settings', 'SharpComment', False) then Include(FSingleComments, csSharpStyle);
        if ReadBool('Settings', 'SlashComment', False) then Include(FSingleComments, csSlashStyle);
        if ReadBool('Settings', 'PerCentComment', False) then Include(FSingleComments, csPercentStyle);
        if ReadBool('Settings', 'AutomatonComment', False) then Include(FSingleComments, csAutomaton);
        if ReadBool('Settings', 'SinglQComment', False) then Include(FSingleComments, csSinglQStyle);
        if ReadBool('Settings', 'DblQComment', False) then Include(FSingleComments, csDblQStyle);
        if ReadBool('Settings', 'SQLComment', False) then Include(FSingleComments, csSQLStyle);
        if ReadBool('Settings', 'CStarComment', False) then Include(FSingleComments, csCStar);
        if ReadBool('Settings', 'FortranComment', False) then Include(FSingleComments, csFortran);
        if ReadBool('Settings', 'LBracketComment', False) then Include(FSingleComments, csLeftBracket);
        if ReadBool('Settings', 'PocoComment', False) then Include(FSingleComments, csPocoStyle);
        if ReadBool('Settings', 'PipeComment', False) then Include(FSingleComments, csPipe);
        if ReadBool('Settings', 'WebFocusComment', False) then Include(FSingleComments, csWebFocus);
        if ReadBool('Settings', 'DMISComment', False) then Include(FSingleComments, csDMISStyle);
        if ReadBool('Settings', 'TabKeyComment', False) then Include(FSingleComments, csTabKey);
        if ReadBool('Settings', '2StarsComment', False) then Include(FSingleComments, cs2Stars);
        if ReadBool('Settings', 'PCLComment', False) then Include(FSingleComments, csPCL);
        if ReadBool('Settings', 'EuklidComment', False) then Include(FSingleComments, csEuklid);
        if ReadBool('Settings', 'SpaceComment', False) then Include(FSingleComments, csSpace);
        if ReadBool('Settings', 'JCLsingleComment', False) then Include(FSingleComments, csJCL);
        if ReadBool('Settings', 'DashComment', False) then Include(FSingleComments, csDash);
        if ReadBool('Settings', 'AngleBrackets', False) then Include(FSingleComments, csAngleBrackets);

        FStringDelim := [];
        if ReadBool('Settings', 'SingleQuote', False) then Include(FStringDelim, sdSingleQuote);
        if ReadBool('Settings', 'ApostropheQuote', False)  then Include(FStringDelim, sdApostropheQuote);
        if ReadBool('Settings', 'DoubleQuote', True) then Include(FStringDelim, sdDoubleQuote);
        FEscapedStrings := ReadBool('Settings', 'EscString', True);
        FEscapeChar := ReadString('Settings', 'EscChar', '\')[1];

        FDetectPreprocessor := ReadBool('Settings', 'Preprocessors', False);
        FHasLabel := ReadBool('Settings', 'Label', True);
        FPHPVariable := ReadBool('Settings', 'PHPVariable', False);
        FRubySymbols := ReadBool('Settings', 'RubySymbols', False);
        FKeyWordChars := ReadString('Settings', 'KeyWordChars', '_');

        FCodeFoldingType := TCodeFoldingType(GetEnumValue(
          TypeInfo(TCodeFoldingType),
          ReadString('Settings', 'CodeFolding', 'ftNone')));
        FKW1StartWith := ReadBool('Settings', 'KW1StartWith', False);
        FKW2StartWith := ReadBool('Settings', 'KW2StartWith', False);
        FKW3StartWith := ReadBool('Settings', 'KW3StartWith', False);
        FKW4StartWith := ReadBool('Settings', 'KW4StartWith', False);

        SL := TStringList.Create;
        try
          SL.Clear; ReadSection('KeyWords', SL);
          SetKeyWordsCompat(SL, KeyWords);
          SL.Clear; ReadSection('ReservedWords', SL);
          SetKeyWordsCompat(SL, ResWords);
          SL.Clear; ReadSection('KeyWords2', SL);
          SetKeyWordsCompat(SL, KeyWords2);
          SL.Clear; ReadSection('KeyWords3', SL);
          SetKeyWordsCompat(SL, KeyWords3);
        finally
          SL.Free;
        end;
      end;
      Result := True;
    finally
      IniFile.Free;
    end;
  end;
end;

function TSynOmniSyn.LoadFromRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TRegistry;
begin
  r:= TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('KeyWords') then KeyWords.Text:= r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else Result := false;
  finally r.Free; end;
end;

function TSynOmniSyn.SaveToIniFile(const FileName: string): Boolean;
var
  SL : TStringList;

  function BToC(BoolValue: Boolean): string;
  begin
    if BoolValue then Result := '1'
    else Result := '0';
  end;

  procedure SortList(AStrings: TStrings);
  var
    Str, TrimmedStr: string;
    SortedSL: TStringList;
  begin
      SortedSL := TStringList.Create;
      try
        SortedSL.Clear;
        { important, cause we want CompareXXX, not AnsiCompareXXX when Sort }
        SortedSL.UseLocale := False;
        SortedSL.Sorted := True;
        SortedSL.Duplicates := dupIgnore;
        for Str in AStrings do
        begin
          TrimmedStr := Trim(Str);
          if TrimmedStr <> '' then
            SortedSL.Add(TrimmedStr);
        end;
        AStrings.Assign(SortedSL);
      finally
        SortedSL.Free;
      end;
  end;

  procedure AddIf(const AKey: string; b: Boolean);
  begin
    if b then SL.Add(AKey + '=1');
  end;

  procedure AddBool(const AKey: string; b: Boolean);
  begin
    if b then
      SL.Add(AKey + '=1')
    else
      SL.Add(AKey + '=0');
  end;

var
  KeyWord: string;
  Idx: Integer;
  Str: string;
begin
  Result := False;

  if (FileName <> '') then
  begin
    SL := TStringList.Create;
    try
      SL.Add(';SynEdit Omni HighLighter Definition File');
      SL.Add('[Settings]');
      SL.Add('Name=' + FLangName);
      SL.Add('HTMLGroup=' + BToC(FHighLighterGroup = hgHTML));
      SL.Add('Vectors=' + BToC(FVectorSupport));
      SL.Add('Label=' + BToC(FHasLabel));
      SL.Add('PHPVariable=' + BToC(FPHPVariable));
      SL.Add('RubySymbols=' + BToC(FRubySymbols));

      Idx := FDefaultFilter.IndexOf('|');
      if Idx >= 0 then
      begin
        Str := FDefaultFilter.Substring(Idx + 1);
        Str := StringReplace(Str, ';', ',', [rfReplaceAll]);
        SL.Add('FileType=' + Str);
      end;
      AddIf('CaseSensitive', FCaseSensitive);
      AddIf('Preprocessors', FDetectPreprocessor);

      AddIf('ANSIComment', csAnsiStyle in FComments);
      AddIf('PasComment', csPasStyle in FComments);
      AddIf('CComment', csCStyle in FComments);
      AddIf('DollarComment', csDollar in FComments);
      AddIf('SmartComment', csSmartStyle in FComments);
      AddIf('HaskellComment', csHaskell in FComments);
      AddIf('DComment', csDStyle in FComments);
      AddIf('JCLComment', csJCLStyle in FComments);
      AddIf('VLispComment', csVLisp in FComments);
      AddIf('CLispComment', csCLisp in FComments);
      AddIf('DeadComment', csDead in FComments);
      AddIf('2Exclamation', cs2Excl in FComments);
      AddIf('DollarMultiComment', csDollarMulti in FComments);
      AddIf('ForthComment', csForth in FComments);
      AddIf('HTMLComment', csHTML in FComments);
      AddIf('LuaComment', csLUA in FComments);
      AddIf('LilypondComment', csLilypond in FComments);
      AddIf('OkumaComment', csOkuma in FComments);
      AddIf('HellerComment', csHeller in FComments);
      AddIf('PwShellComment', csPwShell in FComments);
      AddIf('SSComment', csStarSemicol in FComments);
      AddIf('TexInfoComment', csTexInfo in FComments);
      AddIf('AutoitBlockComm', csAutoit in FComments);

      AddIf('CPLComment', csCPL in FSingleComments);
      AddIf('SpecComment', csSpecStyle in FSingleComments);
      AddIf('BackSlashComment', csBackSlashStyle in FSingleComments);
      AddIf('LineCComment', csLineC in FSingleComments);
      AddIf('BasComment', csBasStyle in FSingleComments);
      AddIf('INIComment', csINIStyle in FSingleComments);
      AddIf('FoxComment', csFoxStyle in FSingleComments);
      AddIf('REMComment', csBatStyle in FSingleComments);
      AddIf('ExclComment', csExclStyle in FSingleComments);
      AddIf('ByComment', csByStyle in FSingleComments);
      AddIf('SharpComment', csSharpStyle in FSingleComments);
      AddIf('SlashComment', csSlashStyle in FSingleComments);
      AddIf('PerCentComment', csPercentStyle in FSingleComments);
      AddIf('AutomatonComment', csAutomaton in FSingleComments);
      AddIf('SinglQComment', csSinglQStyle in FSingleComments);
      AddIf('DblQComment', csDblQStyle in FSingleComments);
      AddIf('SQLComment', csSQLStyle in FSingleComments);
      AddIf('CStarComment', csCStar in FSingleComments);
      AddIf('FortranComment', csFortran in FSingleComments);
      AddIf('LBracketComment', csLeftBracket in FSingleComments);
      AddIf('PocoComment', csPocoStyle in FSingleComments);
      AddIf('PipeComment', csPipe in FSingleComments);
      AddIf('WebFocusComment', csWebFocus in FSingleComments);
      AddIf('DMISComment', csDMISStyle in FSingleComments);
      AddIf('TabKeyComment', csTabKey in FSingleComments);
      AddIf('2StarsComment', cs2Stars in FSingleComments);
      AddIf('PCLComment', csPCL in FSingleComments);
      AddIf('EuklidComment', csEuklid in FSingleComments);
      AddIf('SpaceComment', csSpace in FSingleComments);
      AddIf('JCLsingleComment', csJCL in FSingleComments);
      AddIf('DashComment', csDash in FSingleComments);
      AddIf('AngleBrackets', csAngleBrackets in FSingleComments);

      AddBool('SingleQuote', sdSingleQuote in FStringDelim);
      AddBool('DoubleQuote', sdDoubleQuote in FStringDelim);
      AddBool('ApostropheQuote', sdApostropheQuote in FStringDelim);
      SL.Add('EscString=' + BToC(FEscapedStrings));
      SL.Add('EscChar=' + FEscapeChar);

      AddBool('KW1StartWith', FKW1StartWith);
      AddBool('KW2StartWith', FKW2StartWith);
      AddBool('KW3StartWith', FKW3StartWith);
      AddBool('KW4StartWith', FKW4StartWith);


      SL.Add('KeyWordChars=' + FKeywordChars);
      SL.Add('CodeFolding=' +
        GetEnumName(TypeInfo(TCodeFoldingType), Integer(FCodeFoldingType)));

      SL.Add('[KeyWords]');
      for KeyWord in FKeyWords do
        SL.Add(KeyWord+'=');
      SL.Add('[ReservedWords]');
      for KeyWord in FResWords do
        SL.Add(KeyWord+'=');
      SL.Add('[KeyWords2]');
      for KeyWord in FKeyWords2 do
        SL.Add(KeyWord+'=');
      SL.Add('[KeyWords3]');
      for KeyWord in FKeyWords3 do
        SL.Add(KeyWord+'=');

      SL.SaveToFile(FileName);
      Result := True;
    finally
      SL.Free;
    end;
  end;
end;

function TSynOmniSyn.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TRegistry;
begin
  r:= TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,true) then begin
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else Result := false;
  finally r.Free; end;
end;

procedure TSynOmniSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: String;

  function FindBraces(Line: Integer): Boolean;
  // Covers the following line patterns: {, }, {}, }{, {}{, }{}

    function LineHasChar(AChar: Char; StartCol: Integer; out Col: Integer): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      Col := 0;
      for I := StartCol to Length(CurLine) do begin
        if CurLine[I] = AChar then begin
          // Char must have proper highlighting (ignore stuff inside comments...)
          if GetHighlighterAttriAtRowCol(LinesToScan, Line, I) = fSymbolAttri then
          begin
            Col := I;
            Exit(True);
          end;
        end;
      end;
    end;

    function Indent: Integer;
    begin
      Result := LeftSpaces(CurLine, True, TabWidth(LinesToScan));
    end;

  var
    OpenIdx: Integer;
    CloseIdx: Integer;
    Idx: Integer;
  begin
    LineHasChar('{', 1, OpenIdx);
    LineHasChar('}', 1, CloseIdx);

    Result := True;
    if (OpenIdx <= 0) and (CloseIdx <= 0) then
      Result := False
    else if (OpenIdx > 0) and (CloseIdx <= 0) then
      FoldRanges.StartFoldRange(Line + 1, 1, Indent)
    else if (OpenIdx <= 0) and (CloseIdx > 0) then
      FoldRanges.StopFoldRange(Line + 1, 1)
    else if CloseIdx >= OpenIdx then // {}
    begin
      if LineHasChar('{', CloseIdx, Idx) then
        FoldRanges.StartFoldRange(Line + 1, 1, Indent)
      else
        Result := False;
    end
    else // }{
    begin
      if LineHasChar('}', OpenIdx, Idx) then
        FoldRanges.StopFoldRange(Line + 1, 1)
      else
        FoldRanges.StopStartFoldRange(Line + 1, 1, Indent);
    end;
  end;

  function FoldRegion(Line: Integer): Boolean;
  var
    S : string;
  begin
    Result := False;
    S := AnsiUpperCase(TrimLeft(CurLine));
    if S.Contains('#REGION') then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else
    if S.Contains('#ENDREGION') then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

var
  Line: Integer;
  RangeState: TRangeState;

begin
  if FCodeFoldingType <> cftCurlyBracket then Exit;

  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline comments (Fold Type 2)
    RangeState := TRangeState(GetLineRange(LinesToScan, Line));
    if RangeState in [rsAnsi, rsPasStyle, rsCStyle, rsPwShell] then
    begin
      if TRangeState(GetLineRange(LinesToScan, Line - 1)) = RangeState then
        FoldRanges.NoFoldInfo(Line + 1)
      else
        FoldRanges.StartFoldRange(Line + 1, 2);
      Continue;
    end
    else
    begin
      RangeState := TRangeState(GetLineRange(LinesToScan, Line - 1));
      if RangeState in [rsAnsi, rsPasStyle, rsCStyle, rsPwShell] then
      begin
        FoldRanges.StopFoldRange(Line + 1, 2);
        Continue;
      end;
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

procedure TSynOmniSyn.SetDetectPreprocessor(Value: Boolean);
begin
  if Value <> FDetectPreprocessor then
  begin
    FDetectPreprocessor := Value;
    DefHighlightChange(Self);
  end;
end;

class function TSynOmniSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangOmni;
end;

procedure TSynOmniSyn.SetLangName(const Value: string);
begin
  FLangName := Value;
  if Value = '' then
    FLangName := SYNS_FriendlyLangUnknown;
end;

procedure TSynOmniSyn.SetKeyWords2(const Value: TStrings);
begin
  SetKeyWordsCompat(Value, FKeyWords2);
end;

procedure TSynOmniSyn.SetKeyWords3(const Value: TStrings);
begin
  SetKeyWordsCompat(Value, FKeyWords3);
end;

procedure TSynOmniSyn.SetResWords(const Value: TStrings);
begin
  SetKeyWordsCompat(Value, FResWords);
end;

procedure TSynOmniSyn.SetStringDelim(const Value: TStringDelimiters);
begin
  FStringDelim := Value;
  FStringDelimChars := [];
  if sdSingleQuote in FStringDelim then Include(FStringDelimChars, '''');
  if sdDoubleQuote in FStringDelim then Include(FStringDelimChars, '"');
  if sdApostropheQuote in FStringDelim then Include(FStringDelimChars, '`');
end;

procedure TSynOmniSyn.CommentRangeProc;
var
  OK: Boolean;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    OK := False;

    repeat
      case FRange of
        rsPasStyle: OK := (FLine[Run] = '}');
        rsStarSemicol: OK := (FLine[Run] = ';');
        rsForth, rsHeller, rsOkuma: OK := (FLine[Run] = ')');
        rsDollarMulti: OK := (FLine[Run] = '$');
        rsHaskell: OK := (FLine[Run] = '-') and (FLine[Run+1] = '}');
        rsSmart: OK := (FLine[Run] = '*') and (FLine[Run+1] = '}');
        rsDollar: OK := (FLine[Run] = '$') and (FLine[Run + 1] = ')');
        rsAnsi: OK := (FLine[Run] = '*') and (FLine[Run + 1] = ')');
        rsD: OK := (FLine[Run] = '+') and (FLine[Run + 1] = '/');
        rsCStyle: OK := (FLine[Run] = '*') and (FLine[Run + 1] = '/');
        rsVLisp: OK := (FLine[Run] = '|') and (FLine[Run + 1] = ';');
        rsCLisp: OK := (FLine[Run] = '|') and (FLine[Run + 1] = '#');
        rsDead: OK := (FLine[Run] = '''') and (FLine[Run + 1] = '''');
        rs2Excl: OK := (FLine[Run] = '!') and (FLine[Run + 1] = '!');
        rsLua: OK := (FLine[Run] = ']') and (FLine[Run + 1] = ']');
        rsPwShell: OK := (FLine[Run] = '#') and (FLine[Run + 1] = '>');
        rsLilypond: OK := (FLine[Run] = '%') and (FLine[Run + 1] = '}');
        rsJCL: OK := (FLine[Run] = '*') and (FLine[Run + 1] = '/') and (FLine[Run + 2] = '/');
        rsHTML: OK := (FLine[Run] = '-') and (FLine[Run + 1] = '-') and (FLine[Run + 2] = '>');
        rsTexInfo: OK := WideSameStr(Copy(FLine, 1, 11), '@end ignore');
        rsAutoIt: OK := (FLine[Run] = '#') and (FLine[Run + 1] = 'c') and (FLine[Run + 2] = 'e');
      end;
      if OK then
      begin
        if FRange in [rsPasStyle, rsDollarMulti, rsForth, rsOkuma, rsHeller, rsStarSemicol] then
          Inc(Run)
        else
          Inc(Run, 2);
        if FRange in [rsJCL, rsHTML, rsAutoIt] then
          Inc(Run);
        if FRange = rsTexInfo then
          Inc(Run, 9);
        FRange := rsUnKnown;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

function TSynOmniSyn.IsReservedWord(const AKeyword: string; KeyList: TStrings;
  AKWStartWith: Boolean): Boolean;
var
  First, Last, I, L, Compare: Integer;
  Token: string;
  Chr: Char;
begin
  Result := False;
  if KeyList.Count = 0 then Exit;
  First := 0;
  Last := KeyList.Count - 1;
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Token := KeyList[i];
    L := Length(Token);

    { token starts with list item and is followed by number }
    if AKWStartWith then
    begin
      Chr := String(Copy(AKeyWord, L + 1, 1) + ' ')[1];
      if FCaseSensitive then
        Compare := CompareStr(Token, Copy(AKeyWord, 1, L))
      else
        Compare := CompareText(Token, Copy(AKeyWord, 1, L));
      { followed by numbers only (space is added when equal to token ) }
      if (Compare = 0) and not CharInSet(Chr, (['0'..'9','-', ' '])) then
        Compare := 1;

    end
    else
    begin
      if FCaseSensitive then
        Compare := CompareStr(Token, AKeyWord)
      else
        Compare := CompareText(Token, AKeyWord);
    end;
    if Compare = 0 then
    begin
      Result := True;
      if AKWStartWith then
        Run := Run - Length(AKeyword) + L;
      Break;
    end else
      if Compare < 0 then
        First := I + 1
      else
        Last := I - 1;
  end;
end;

procedure TSynOmniSyn.SetKeyWordsCompat(FromStrings, ToStrings: TStrings);
var
  Str: string;
begin
  if (FromStrings <> nil) and (Tostrings <> nil) then
  begin
    Tostrings.Clear;
    ToStrings.BeginUpdate;
    for Str in FromStrings do
    begin
      if FCaseSensitive then
        ToStrings.Add(Str)
      else
        ToStrings.Add(UpperCase(Str));
    end;
    ToStrings.EndUpdate;

    DefHighLightChange(nil);
  end;
end;

procedure TSynOmniSyn.HexProc;

  function IsHexChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f', 'H', 'h': Result := True;
    else
      Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkHex;
  while IsHexChar do
    Inc(Run);
end;

{-----------------------------------------------------------------
  Fotran comment and C* comment
------------------------------------------------------------------}
procedure TSynOmniSyn.FortranCommProc;

  function IsFirstChar: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := Run - 1 downto 0 do
      if not CharInSet(FLine[I], [#32, #9]) then
      begin
        Result := False;
        Break;
      end;
  end;

begin
  if (csFortran in FSingleComments) and (Run=0) {IsFirstChar} or
    (csCStar in FSingleComments) and (FLine[Run+1] = '*') then
  begin
    Inc(Run);
    FTokenID := tkComment;
    while (FLine[Run] <> #0) do begin
      case FLine[Run] of
        #10, #13: Break;
      end;
      Inc(Run);
    end;
  end
  else
  begin
    FTokenID := tkIdentifier;
    IdentProc;
  end;
end;

procedure TSynOmniSyn.REMCommentProc;
begin
  if (csBatStyle in FSingleComments) and CharInSet(FLine[Run+1], ['E', 'e'])
    and CharInSet(FLine[Run+2], ['M', 'm']) and (FLine[Run+3] <= #32) then
  begin
    FTokenID := tkComment;
    Inc(Run, 3);
    while (FLine[Run] <> #0) do begin
      case FLine[Run] of
        #10, #13: Break;
      end; { case }
      Inc(Run);
    end; { while }
  end
  else begin
    FTokenID := tkIdentifier;
    IdentProc;
  end;
end;

procedure TSynOmniSyn.BatCommentProc;
var
  myChar: Char;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (csBatStyle in FSingleComments) and (FLine[Run] = ':') then
  begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  { Ruby symbol :something }
  if FRubySymbols and (Run > 1) and IsIdentChar(FLine[Run]) and (FLine[Run-2] = ' ') then
  begin
    FTokenID := tkLabel;
    repeat Inc(Run) until not IsIdentChar(FLine[Run]) or IsLineEnd(Run);
//    if not IsLineEnd(Run) then Inc(Run);
  end
  else
  { Ruby symbol :"something" or :'something' }
  if FRubySymbols and CharInSet(FLine[Run], ['"', '''']) then
  begin
    myChar := FLine[Run];
    FTokenID := tkLabel;
    repeat Inc(Run) until (FLine[Run] = myChar) or IsLineEnd(Run);
    if not IsLineEnd(Run) then Inc(Run);
  end
  else
  if FHasLabel and (Run = 1) and (FLine[Run] <> ':') then
  begin
    FTokenID := tkLabel;
    while IsIdentChar(FLine[Run]) do Inc(Run);
  end
  else
  if FRubySymbols and (Run > 1) and (FLine[Run] = ':') then
    Inc(Run);

end;

procedure TSynOmniSyn.LabelProc;
begin
  { char @ is marked as comment char }
  if csByStyle in FSingleComments then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  { this is part for label }
  else
  if FHasLabel and (Run = 0) then
  begin
    Inc(Run);
    FTokenID := tkLabel;
    while IsIdentChar(FLine[Run]) do Inc(Run);
  end
  else
  if FRubySymbols then
  begin
    Inc(Run);
    FTokenID := tkVariable;
    while IsIdentChar(FLine[Run]) do Inc(Run);
  end
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynOmniSyn.AsteriskProc;
var
  IsComm: Boolean;
  i: Integer;

  function IsSasComment: Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := Run-1 downto 0 do
      if not CharInSet(FLine[i], [#32, #9, ';']) then
      begin
        Result := False;
        Break;
      end
      else
        if FLine[i] = ';' then
          Break;
  end;

begin
  if (cs2Stars in FSingleComments) and (FLine[Run+1] = '*') then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) or (FLine[Run] = ';') do Inc(Run);
  end
  else
  if csStarSemicol in FComments then
  begin
    FTokenID := tkComment;
    FRange := rsStarSemicol;
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if csFoxStyle in FSingleComments then
  begin
    IsComm := True;
    if Run > 0 then
      for i := Run - 1 downto 0 do
        if not CharInSet(FLine[i], [#32, #9]) then
        begin
          IsComm := False;
          Break;
        end;
    if IsComm then
    begin
      FTokenID := tkComment;
      while not IsLineEnd(Run) do Inc(Run);
    end
    else begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
  end
  else
  if (csPocoStyle in FSingleComments) and IsSasComment then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) or (FLine[Run] = ';') do Inc(Run);
  end
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynOmniSyn.PipeProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (csPipe in FSingleComments) then
  begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynOmniSyn.MinusProc;
begin
  if (csDash in FSingleComments) and (Run = 0) and (FLine[Run+1] = ' ') then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
  if (csLua in FComments) and (Copy(FLine, Run+1, 4) = '--[[') then
  begin
    FTokenID := tkComment;
    FRange := rsLua;
    Inc(Run, 4);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if (csWebFocus in FSingleComments) and (FLine[Run+1] = '*') then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
  if (csSQLStyle in FSingleComments) and (FLine[Run+1] = '-') then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do
    begin
      Inc(Run);
      // PSL support: -- PSL something...
      if Copy(FLine, Run-4, 5) = ' psl ' then
        Break;
    end;
  end
  else
  begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynOmniSyn.AmpersandProc;
begin
  if (csFoxStyle in FSingleComments) and (FLine[Run+1] = '&') then begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else begin
    Inc(Run);
    FTokenID := tkSymbol;
  end;
end;

procedure TSynOmniSyn.PerCentProc;
begin
  if (csAutomaton in FSingleComments) and (FLine[Run+1] = '%') then
  begin
    FTokenID := tkComment;
    Inc(Run, 2);
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
  if (csLilypond in FComments) and (FLine[Run+1] = '{') then
  begin
    FTokenID := tkComment;
    FRange := rsLilypond;
    Inc(Run, 2);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if csPerCentStyle in FSingleComments then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
end;

procedure TSynOmniSyn.ExclamationProc;
begin
  if (cs2Excl in Comments) and (FLine[Run+1] = '!') then
  begin
    FTokenID := tkComment;
    FRange := rs2Excl;
    Inc(Run, 2);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  if (csPCL in FSingleComments) and (FLine[Run+1] = '*') or (csExclStyle in FSingleComments) then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
    begin
      Inc(Run);
      FTokenID := tkSymbol;
    end;
end;

procedure TSynOmniSyn.DoubleQuoteProc;
begin
  FTokenID := tkComment;
  while not IsLineEnd(Run) do
  begin
    Inc(Run);
    if CharInSet(FLine[Run], ['"', ';']) then
    begin
      if FLine[Run] = '"' then Inc(Run);
      Break;
    end;
  end;
end;

procedure TSynOmniSyn.SingleQuoteProc;
begin
  FTokenID := tkComment;
  while not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynOmniSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynOmniSyn.SetSingleComments(
  const Value: TSingleLineComments);
begin
  FSingleComments := Value;
  DefHighLightChange(nil);
end;

procedure TSynOmniSyn.BackSlashProc;
begin
  Inc(Run);
  if (csForth in FComments) and (FLine[Run] = ' ') or
    (csBackSlashStyle in FSingleComments) then //and ((Run=1) or (FLine[Run-2]=' ')) and (FLine[Run]=' ') then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynOmniSyn.TabKeyProc;
begin
  Inc(Run);
  if (csTabKey in FSingleComments) then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
    FTokenID := tkSpace;;
end;

procedure TSynOmniSyn.FileNameProc;
var
  isQuote: Boolean;
begin
  isQuote := (FLine[Run] = '"');
  FTokenID := tkLabel;
  repeat
    Inc(Run);
    if FLine[Run] = '"' then
      isQuote := False;
  until IsLineEnd(Run) or not isQuote and CharInSet(FLine[Run], [#32, #9]);
end;

procedure TSynOmniSyn.RestIsCommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

function TSynOmniSyn.GetKeyWords(TokenKind: Integer): string;
begin
  Result := '';
end;

procedure TSynOmniSyn.HTMLCommentProc;
var
  myChar: Char;
begin
  FTokenID := tkSymbol;
  myChar := FLine[Run];
  Inc(Run);
  { AngleBracket comment }
  if (csAngleBrackets in FSingleComments) and (FLine[Run] = '<') and (FLine[Run+1] = '<') then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
  { HTML multiline comment }
  if (csHTML in FComments) and (FLine[Run] = '!') and (FLine[Run+1] = '-') then
  begin
    FTokenID := tkComment;
    FRange := rsHTML;
    Inc(Run, 2);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  { PowerShell multiline comment }
  if (csPwShell in FComments) and (myChar='<') and (FLine[Run] = '#') then
  begin
    FTokenID := tkComment;
    FRange := rsPwShell;
    Inc(Run, 1);
    if not IsLineEnd(Run) then
      CommentRangeProc;
  end
  else
  {Euklid comment }
  if (csEuklid in FSingleComments) and (myChar = '.') and (FLine[Run] = '.') then
  begin
    FTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
end;

initialization
  RegisterPlaceableHighlighter(TSynOmniSyn);
end.

