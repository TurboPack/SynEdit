{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  https://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is SynHighlighterMarkdown.pas, released 2025-04-26.

  The Initial Developer of the Original Code is MASUDA Takshi.
  Portions created by the Initial Developer are Copyright (C) 2025
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  Alternatively, the contents of this file may be used under the terms of
  the GNU General Public License Version 2 or later (the  "GPL"),
  in which case the provisions of GPL are applicable instead of those
  above.  If you wish to allow use of your version of this file only
  under the terms of the GPL and not to allow others to use your version of
  this file under the MPL, indicate your decision by deleting the provisions
  above and replace  them with the notice and other provisions required
  by the GPL. If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the GPL.
  ------------------------------------------------------------------------------- }

unit SynHighlighterMarkdown;

interface

uses
  System.Classes, System.Generics.Collections, System.RegularExpressions,
  System.StrUtils, System.SysUtils, Vcl.Graphics, SynEditHighlighter, SynFunc;

type
  TtkTokenKind = (tkUnknown, tkBlockQuote, tkCode, tkDelete, tkEmphasis,
    tkEntityReference, tkHeader, tkHtmlAttrName, tkHtmlAttrValue, tkHtmlComment,
    tkHtmlTag, tkLink, tkList, tkSpace);

  TRangeState = (rsUnKnown, rsBacktickFencedCodeBlock,
    rsHtmlAttrDoubleQuoteValue, rsHtmlAttrName, rsHtmlAttrQuoteValue,
    rsHtmlAttrValue, rsHtmlComment, rsHtmlTag, rsTildeFencedCodeBlock);

  TRangeInfo = packed record
    case Boolean of
      False:
        (p: Pointer);
      True:
        (State: Word; Length: Word);
  end;

  TSynMarkdownSyn = class(TSynCustomHighlighter)
  private
    FBlockQuoteAttri: TSynHighlighterAttributes;
    FCodeAttri: TSynHighlighterAttributes;
    FDeleteAttri: TSynHighlighterAttributes;
    FEmphasisAttri: TSynHighlighterAttributes;
    FEntityReferenceAttri: TSynHighlighterAttributes;
    FHeadingAttri: TSynHighlighterAttributes;
    FHtmlAttrNameAttri: TSynHighlighterAttributes;
    FHtmlAttrValueAttri: TSynHighlighterAttributes;
    FHtmlCommentAttri: TSynHighlighterAttributes;
    FHtmlTagAttri: TSynHighlighterAttributes;
    FLinkAttri: TSynHighlighterAttributes;
    FListAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;

    FRange: TRangeInfo;
    FTokenID: TtkTokenKind;

    function AtxHeadingProc: Boolean;
    function BlockQuoteProc: Boolean;
    function CodeSpanProc: Boolean;
    function DeleteProc: Boolean;
    function EmphasisProc: Boolean;
    function EntityReferenceProc: Boolean;
    function FencedCodeBlockBeginProc: Boolean;
    function FencedCodeBlockEndProc: Boolean;
    procedure HtmlAttrDoubleQuoteValueProc;
    procedure HtmlAttrNameProc;
    procedure HtmlAttrQuoteValueProc;
    procedure HtmlAttrValueProc;
    procedure HtmlCommentProc;
    function HtmlOpenProc: Boolean;
    procedure HtmlTagProc;
    function IndentedCodeBlockProc: Boolean;
    function ListProc: Boolean;
    function PageLinkProc: Boolean;
    function SetextHeadingProc: Boolean;
    function UrlLinkProc: Boolean;

    function GetFencedCodeBlockType(const Ch: Char): TRangeState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDefaultAttribute(Index: Integer)
      : TSynHighlighterAttributes; override;
    class function GetFriendlyLanguageName: String; override;
    function GetEol: Boolean; override;
    class function GetLanguageName: String; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property BlockQuoteAttri: TSynHighlighterAttributes read FBlockQuoteAttri
      write FBlockQuoteAttri;
    property CodeAttri: TSynHighlighterAttributes read FCodeAttri
      write FCodeAttri;
    property DeleteAttri: TSynHighlighterAttributes read FDeleteAttri
      write FDeleteAttri;
    property EmphasisAttri: TSynHighlighterAttributes read FEmphasisAttri
      write FEmphasisAttri;
    property EntityReferenceAttri: TSynHighlighterAttributes
      read FEntityReferenceAttri write FEntityReferenceAttri;
    property HeadingAttri: TSynHighlighterAttributes read FHeadingAttri
      write FHeadingAttri;
    property HtmlAttrNametAttri: TSynHighlighterAttributes
      read FHtmlAttrNameAttri write FHtmlAttrNameAttri;
    property HtmlAttrValueAttri: TSynHighlighterAttributes
      read FHtmlAttrValueAttri write FHtmlAttrValueAttri;
    property HtmlCommentAttri: TSynHighlighterAttributes read FHtmlCommentAttri
      write FHtmlCommentAttri;
    property HtmlTagAttri: TSynHighlighterAttributes read FHtmlTagAttri
      write FHtmlTagAttri;
    property LinkAttri: TSynHighlighterAttributes read FLinkAttri
      write FLinkAttri;
    property ListAttri: TSynHighlighterAttributes read FListAttri
      write FListAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property TextAttri: TSynHighlighterAttributes read FTextAttri
      write FTextAttri;
  end;

procedure SetEntities(Dict: TDictionary<String, String>);

implementation

var
  EntityReferenceDict: TDictionary<String, String>;
  RegexAtxHeading: TRegEx;
  RegexBlockQuote: TRegEx;
  RegexCodeSpan: TRegEx;
  RegexDelete: TRegEx;
  RegexEmpasis1: TRegEx;
  RegexEmpasis2: TRegEx;
  RegexEntityReference: TRegEx;
  RegexFencedCodeBlockBegin: TRegEx;
  RegexFencedCodeBlockEnd: TRegEx;
  RegexIndentedCodeBlock: TRegEx;
  RegexList: TRegEx;
  RegexUrlLink: TRegEx;
  RegexPageLink: TRegEx;
  RegexSetextHeading: TRegEx;

  { TSynMarkdownSyn }

function TSynMarkdownSyn.AtxHeadingProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexAtxHeading.Match(FLine);
  if Ret.Success then
  begin
    FTokenID := tkHeader;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

function TSynMarkdownSyn.BlockQuoteProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexBlockQuote.Match(FLine);
  if Ret.Success then
  begin
    FTokenID := tkBlockQuote;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

function TSynMarkdownSyn.CodeSpanProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexCodeSpan.Match(FLine, ToInt32(Run + 1));
  if Ret.Success and (Run = (Ret.Index - 1)) then
  begin
    FTokenID := tkCode;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

constructor TSynMarkdownSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBlockQuoteAttri := TSynHighlighterAttributes.Create('BlockQuote',
    'Block Quote');
  FBlockQuoteAttri.Foreground := clWebDimGray;
  AddAttribute(FBlockQuoteAttri);

  FCodeAttri := TSynHighlighterAttributes.Create('Code', 'Code');
  FCodeAttri.Foreground := clWebFirebrick;
  AddAttribute(FCodeAttri);

  FDeleteAttri := TSynHighlighterAttributes.Create('Delete', 'Delete');
  FDeleteAttri.Foreground := clWebDimGray;
  AddAttribute(FDeleteAttri);

  FEmphasisAttri := TSynHighlighterAttributes.Create('Emphasis', 'Emphasis');
  FEmphasisAttri.Foreground := clWebDeepPink;
  AddAttribute(FEmphasisAttri);

  FEntityReferenceAttri := TSynHighlighterAttributes.Create('EntityReference',
    'Entity Reference');
  FEntityReferenceAttri.Foreground := clGreen;
  AddAttribute(FEntityReferenceAttri);

  FHeadingAttri := TSynHighlighterAttributes.Create('Heading', 'Heading');
  FHeadingAttri.Foreground := clWebMediumBlue;
  AddAttribute(FHeadingAttri);

  FHtmlAttrNameAttri := TSynHighlighterAttributes.Create('HtmlAttrName',
    'HTML Attribute Name');
  FHtmlAttrNameAttri.Foreground := clWebMediumBlue;
  AddAttribute(FHtmlAttrNameAttri);

  FHtmlAttrValueAttri := TSynHighlighterAttributes.Create('HtmlAttrValue',
    'HTML Attribute Value');
  FHtmlAttrValueAttri.Foreground := clWebCrimson;
  AddAttribute(FHtmlAttrValueAttri);

  FHtmlCommentAttri := TSynHighlighterAttributes.Create('HtmlComment',
    'HTML Comment');
  FHtmlCommentAttri.Foreground := clWebDimGray;
  AddAttribute(FHtmlCommentAttri);

  FHtmlTagAttri := TSynHighlighterAttributes.Create('HtmlTag', 'HTML Tag');
  FHtmlTagAttri.Foreground := clWebDarkViolet;
  AddAttribute(FHtmlTagAttri);

  FLinkAttri := TSynHighlighterAttributes.Create('Link', 'Link');
  FLinkAttri.Foreground := clBlue;
  AddAttribute(FLinkAttri);

  FListAttri := TSynHighlighterAttributes.Create('List', 'List');
  FListAttri.Foreground := clWebDeepPink;
  AddAttribute(FListAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create('Space', 'Space');
  FSpaceAttri.Foreground := clWebCornFlowerBlue;
  AddAttribute(FSpaceAttri);

  FTextAttri := TSynHighlighterAttributes.Create('Text', 'Text');
  AddAttribute(FTextAttri);

  FBrackets := '<>()[]{}';
  FCaseSensitive := True;
end;

function TSynMarkdownSyn.DeleteProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexDelete.Match(FLine, ToInt32(Run + 1));
  if Ret.Success and (Run = (Ret.Index - 1)) then
  begin
    FTokenID := tkDelete;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

destructor TSynMarkdownSyn.Destroy;
begin

  inherited;
end;

function TSynMarkdownSyn.EmphasisProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexEmpasis1.Match(FLine, ToInt32(Run + 1));
  if Ret.Success and (Run = (Ret.Index - 1)) then
  begin
    FTokenID := tkEmphasis;
    Run := Run + Ret.Length;
    Exit(True);
  end;

  Ret := RegexEmpasis2.Match(FLine, ToInt32(Run + 1));
  if Ret.Success and (Run = (Ret.Index - 1)) then
  begin
    FTokenID := tkEmphasis;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

function TSynMarkdownSyn.EntityReferenceProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexEntityReference.Match(FLine, ToInt32(Run + 1));
  if Ret.Success and (Run = (Ret.Index - 1)) then
  begin
    if FLine[Run + 1] <> '#' then
    begin
      if not EntityReferenceDict.ContainsKey(Ret.Groups[1].Value) then
        Exit(False);
    end;

    FTokenID := tkEntityReference;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

function TSynMarkdownSyn.FencedCodeBlockBeginProc: Boolean;
var
  Fence: string;
  Ret: TMatch;
begin
  Ret := RegexFencedCodeBlockBegin.Match(FLine);
  if Ret.Success then
  begin
    Fence := Ret.Groups[1].Value;
    FRange.State := Ord(GetFencedCodeBlockType(Fence[1]));
    FRange.Length := ToWord(Length(Fence));
    FTokenID := tkCode;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

function TSynMarkdownSyn.FencedCodeBlockEndProc: Boolean;
var
  Fence: string;
  Ret: TMatch;
begin
  if Run > 0 then
    Exit(False);

  Ret := RegexFencedCodeBlockEnd.Match(FLine);
  if Ret.Success then
  begin
    Fence := Ret.Groups[1].Value;
    if (TRangeState(FRange.State) = GetFencedCodeBlockType(Fence[1])) and
      (FRange.Length <= Length(Fence)) then
    begin
      ResetRange;
      Run := Run + Ret.Length;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TSynMarkdownSyn.GetDefaultAttribute(Index: Integer)
  : TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_WHITESPACE:
      Result := FSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynMarkdownSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynMarkdownSyn.GetFencedCodeBlockType(const Ch: Char): TRangeState;
begin
  if Ch = '`' then
    Result := rsBacktickFencedCodeBlock
  else
    Result := rsTildeFencedCodeBlock;
end;

class function TSynMarkdownSyn.GetFriendlyLanguageName: String;
begin
  Result := 'Markdown';
end;

class function TSynMarkdownSyn.GetLanguageName: String;
begin
  Result := 'Markdown';
end;

function TSynMarkdownSyn.GetRange: Pointer;
begin
  Result := FRange.p;
end;

function TSynMarkdownSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkUnknown:
      Result := FTextAttri;
    tkBlockQuote:
      Result := FBlockQuoteAttri;
    tkCode:
      Result := FCodeAttri;
    tkDelete:
      Result := FDeleteAttri;
    tkEmphasis:
      Result := FEmphasisAttri;
    tkEntityReference:
      Result := FEntityReferenceAttri;
    tkHeader:
      Result := FHeadingAttri;
    tkHtmlAttrName:
      Result := FHtmlAttrNameAttri;
    tkHtmlAttrValue:
      Result := FHtmlAttrValueAttri;
    tkHtmlComment:
      Result := FHtmlCommentAttri;
    tkHtmlTag:
      Result := FHtmlTagAttri;
    tkLink:
      Result := FLinkAttri;
    tkList:
      Result := FListAttri;
    tkSpace:
      Result := FSpaceAttri
  else
    Result := nil;
  end;
end;

function TSynMarkdownSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

procedure TSynMarkdownSyn.HtmlAttrDoubleQuoteValueProc;
begin
  FTokenID := tkHtmlAttrValue;
  while not CharInSet(FLine[Run], ['"']) do
  begin
    if FLine[Run] = #0 then
      Exit;
    Inc(Run);
  end;

  FRange.State := Ord(rsHtmlAttrName);
  Inc(Run);
end;

procedure TSynMarkdownSyn.HtmlAttrNameProc;
begin
  if FLine[Run] = '>' then
  begin
    Inc(Run);
    FRange.State := Ord(rsUnKnown);
    FTokenID := tkUnknown;
    Exit;
  end;

  if FLine[Run] = '=' then
  begin
    Inc(Run);
    while CharInSet(FLine[Run], [#9, ' ']) do
      Inc(Run);
    FRange.State := Ord(rsHtmlAttrValue);
    FTokenID := tkUnknown;
    Exit;
  end;

  if CharInSet(FLine[Run], [#9, ' ']) then
  begin
    while CharInSet(FLine[Run], [#9, ' ']) do
      Inc(Run);

    FTokenID := tkUnknown;
    Exit;
  end;

  FTokenID := tkHtmlAttrName;
  while not CharInSet(FLine[Run], [#9, ' ', '=', '>', '/']) do
  begin
    if FLine[Run] = #0 then
      Exit;
    Inc(Run);
  end;
end;

procedure TSynMarkdownSyn.HtmlAttrQuoteValueProc;
begin
  FTokenID := tkHtmlAttrValue;
  while not CharInSet(FLine[Run], ['''']) do
  begin
    if FLine[Run] = #0 then
      Exit;
    Inc(Run);
  end;

  FRange.State := Ord(rsHtmlAttrName);
  Inc(Run);
end;

procedure TSynMarkdownSyn.HtmlAttrValueProc;
begin
  if CharInSet(FLine[Run], [#9, ' ']) then
  begin
    while CharInSet(FLine[Run], [#9, ' ']) do
      Inc(Run);
    FRange.State := Ord(rsHtmlAttrName);
    FTokenID := tkUnknown;
    Exit;
  end;

  FTokenID := tkHtmlAttrValue;
  if FLine[Run] = '''' then
  begin
    Inc(Run);
    FRange.State := Ord(rsHtmlAttrQuoteValue);
    Exit;
  end;

  if FLine[Run] = '"' then
  begin
    Inc(Run);
    FRange.State := Ord(rsHtmlAttrDoubleQuoteValue);
    Exit;
  end;

  while not CharInSet(FLine[Run], [#9, ' ', '=', '>', '/']) do
  begin
    if FLine[Run] = #0 then
    begin
      FRange.State := Ord(rsHtmlAttrName);
      Exit;
    end;
    Inc(Run);
  end;

  if FLine[Run] = '>' then
  begin
    FRange.State := Ord(rsUnKnown);
    Exit;
  end;
end;

procedure TSynMarkdownSyn.HtmlCommentProc;
begin
  FTokenID := tkHtmlComment;
  while FLine[Run] <> #0 do
  begin
    if (FLine[Run] = '-') and (FLine[Run + 1] = '-') and (FLine[Run + 2] = '>')
    then
    begin
      Inc(Run, 3);
      FRange.State := Ord(rsUnKnown);
      Exit;
    end;
    Inc(Run);
  end;
end;

function TSynMarkdownSyn.HtmlOpenProc: Boolean;
begin
  if FLine[Run] = '<' then
  begin
    if (FLine[Run + 1] = '!') and (FLine[Run + 2] = '-') and
      (FLine[Run + 3] = '-') then
    begin
      FTokenID := tkHtmlComment;
      FRange.State := Ord(rsHtmlComment);
      Inc(Run, 4);
      Exit(True);
    end;

    FTokenID := tkUnknown;
    FRange.State := Ord(rsHtmlTag);
    FRange.Length := 0;
    Inc(Run);
    Exit(True);
  end;
  Result := False;
end;

procedure TSynMarkdownSyn.HtmlTagProc;
begin
  FTokenID := tkHtmlTag;
  if CharInSet(FLine[Run], ['/', '!']) then
    Inc(Run);

  while CharInSet(FLine[Run], ['a' .. 'z', 'A' .. 'Z']) do
    Inc(Run);

  if CharInSet(FLine[Run], [#0, #9, ' ']) then
    FRange.State := Ord(rsHtmlAttrName)
  else
    FRange.State := Ord(rsUnKnown);
end;

function TSynMarkdownSyn.IndentedCodeBlockProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexIndentedCodeBlock.Match(FLine);
  if Ret.Success then
  begin
    FTokenID := tkCode;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

function TSynMarkdownSyn.ListProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexList.Match(FLine);
  if Ret.Success then
  begin
    FTokenID := tkList;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

procedure TSynMarkdownSyn.Next;
var
  Processed: Boolean;
begin
  FTokenPos := Run;

  case TRangeState(FRange.State) of
    rsBacktickFencedCodeBlock, rsTildeFencedCodeBlock:
      begin
        FTokenID := tkCode;
        FencedCodeBlockEndProc;
        Run := FLineLen;
      end;
    rsHtmlAttrDoubleQuoteValue:
      HtmlAttrDoubleQuoteValueProc;
    rsHtmlAttrName:
      HtmlAttrNameProc;
    rsHtmlAttrQuoteValue:
      HtmlAttrQuoteValueProc;
    rsHtmlAttrValue:
      HtmlAttrValueProc;
    rsHtmlComment:
      HtmlCommentProc;
    rsHtmlTag:
      HtmlTagProc;
  else
    begin
      Processed := False;
      if Run = 0 then
      begin
        Processed := AtxHeadingProc or BlockQuoteProc or ListProc or
          SetextHeadingProc or IndentedCodeBlockProc or
          FencedCodeBlockBeginProc;
      end;

      if not Processed then
      begin
        case FLine[Run] of
          '`':
            CodeSpanProc;
          '~':
            DeleteProc;
          '*', '_':
            EmphasisProc;
          '&':
            EntityReferenceProc;
          '<':
            HtmlOpenProc;
          'h', 'H':
            UrlLinkProc;
          '[':
            PageLinkProc;
        end;
      end;
    end;
  end;

  if FTokenPos = Run then
  begin
    FTokenID := tkUnknown;
    Inc(Run);
  end;

  inherited;
end;

function TSynMarkdownSyn.PageLinkProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexPageLink.Match(FLine, ToInt32(Run + 1));
  if Ret.Success and (Run = (Ret.Index - 1)) then
  begin
    FTokenID := tkLink;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

procedure TSynMarkdownSyn.ResetRange;
begin
  FRange.p := nil;
end;

function TSynMarkdownSyn.SetextHeadingProc: Boolean;
var
  Ret: TMatch;
begin
  Ret := RegexSetextHeading.Match(FLine);
  if Ret.Success then
  begin
    FTokenID := tkHeader;
    Run := Run + Ret.Length;
    Exit(True);
  end;
  Result := False;
end;

procedure TSynMarkdownSyn.SetRange(Value: Pointer);
begin
  FRange.p := Value;
end;

function TSynMarkdownSyn.UrlLinkProc: Boolean;
var
  Ret: TMatch;
  NewRun: Integer;
  Open: Integer;
  Close: Integer;
begin
  Ret := RegexUrlLink.Match(FLine, ToInt32(Run + 1));
  if Ret.Success and (Run = (Ret.Index - 1)) then
  begin
    NewRun := ToInt32(Run) + Ret.Length;

    if FLine[NewRun - 1] = ')' then
    begin
      Open := Ret.Value.CountChar('(');
      Close := Ret.Value.CountChar(')');
      while (Open < Close) and (FLine[NewRun - 1] = ')') do
      begin
        Dec(NewRun);
        Dec(Close);
      end;
    end;

    FTokenID := tkLink;
    Run := NewRun;
    Exit(True);
  end;
  Result := False;
end;

procedure SetEntities(Dict: TDictionary<String, String>);
begin
  // ref. https://html.spec.whatwg.org/entities.json
  Dict.Add('&AElig;', #$00C6);
  Dict.Add('&AMP;', #$0026);
  Dict.Add('&Aacute;', #$00C1);
  Dict.Add('&Abreve;', #$0102);
  Dict.Add('&Acirc;', #$00C2);
  Dict.Add('&Acy;', #$0410);
  Dict.Add('&Afr;', #$D835#$DD04);
  Dict.Add('&Agrave;', #$00C0);
  Dict.Add('&Alpha;', #$0391);
  Dict.Add('&Amacr;', #$0100);
  Dict.Add('&And;', #$2A53);
  Dict.Add('&Aogon;', #$0104);
  Dict.Add('&Aopf;', #$D835#$DD38);
  Dict.Add('&ApplyFunction;', #$2061);
  Dict.Add('&Aring;', #$00C5);
  Dict.Add('&Ascr;', #$D835#$DC9C);
  Dict.Add('&Assign;', #$2254);
  Dict.Add('&Atilde;', #$00C3);
  Dict.Add('&Auml;', #$00C4);
  Dict.Add('&Backslash;', #$2216);
  Dict.Add('&Barv;', #$2AE7);
  Dict.Add('&Barwed;', #$2306);
  Dict.Add('&Bcy;', #$0411);
  Dict.Add('&Because;', #$2235);
  Dict.Add('&Bernoullis;', #$212C);
  Dict.Add('&Beta;', #$0392);
  Dict.Add('&Bfr;', #$D835#$DD05);
  Dict.Add('&Bopf;', #$D835#$DD39);
  Dict.Add('&Breve;', #$02D8);
  Dict.Add('&Bscr;', #$212C);
  Dict.Add('&Bumpeq;', #$224E);
  Dict.Add('&CHcy;', #$0427);
  Dict.Add('&COPY;', #$00A9);
  Dict.Add('&Cacute;', #$0106);
  Dict.Add('&Cap;', #$22D2);
  Dict.Add('&CapitalDifferentialD;', #$2145);
  Dict.Add('&Cayleys;', #$212D);
  Dict.Add('&Ccaron;', #$010C);
  Dict.Add('&Ccedil;', #$00C7);
  Dict.Add('&Ccirc;', #$0108);
  Dict.Add('&Cconint;', #$2230);
  Dict.Add('&Cdot;', #$010A);
  Dict.Add('&Cedilla;', #$00B8);
  Dict.Add('&CenterDot;', #$00B7);
  Dict.Add('&Cfr;', #$212D);
  Dict.Add('&Chi;', #$03A7);
  Dict.Add('&CircleDot;', #$2299);
  Dict.Add('&CircleMinus;', #$2296);
  Dict.Add('&CirclePlus;', #$2295);
  Dict.Add('&CircleTimes;', #$2297);
  Dict.Add('&ClockwiseContourIntegral;', #$2232);
  Dict.Add('&CloseCurlyDoubleQuote;', #$201D);
  Dict.Add('&CloseCurlyQuote;', #$2019);
  Dict.Add('&Colon;', #$2237);
  Dict.Add('&Colone;', #$2A74);
  Dict.Add('&Congruent;', #$2261);
  Dict.Add('&Conint;', #$222F);
  Dict.Add('&ContourIntegral;', #$222E);
  Dict.Add('&Copf;', #$2102);
  Dict.Add('&Coproduct;', #$2210);
  Dict.Add('&CounterClockwiseContourIntegral;', #$2233);
  Dict.Add('&Cross;', #$2A2F);
  Dict.Add('&Cscr;', #$D835#$DC9E);
  Dict.Add('&Cup;', #$22D3);
  Dict.Add('&CupCap;', #$224D);
  Dict.Add('&DD;', #$2145);
  Dict.Add('&DDotrahd;', #$2911);
  Dict.Add('&DJcy;', #$0402);
  Dict.Add('&DScy;', #$0405);
  Dict.Add('&DZcy;', #$040F);
  Dict.Add('&Dagger;', #$2021);
  Dict.Add('&Darr;', #$21A1);
  Dict.Add('&Dashv;', #$2AE4);
  Dict.Add('&Dcaron;', #$010E);
  Dict.Add('&Dcy;', #$0414);
  Dict.Add('&Del;', #$2207);
  Dict.Add('&Delta;', #$0394);
  Dict.Add('&Dfr;', #$D835#$DD07);
  Dict.Add('&DiacriticalAcute;', #$00B4);
  Dict.Add('&DiacriticalDot;', #$02D9);
  Dict.Add('&DiacriticalDoubleAcute;', #$02DD);
  Dict.Add('&DiacriticalGrave;', #$0060);
  Dict.Add('&DiacriticalTilde;', #$02DC);
  Dict.Add('&Diamond;', #$22C4);
  Dict.Add('&DifferentialD;', #$2146);
  Dict.Add('&Dopf;', #$D835#$DD3B);
  Dict.Add('&Dot;', #$00A8);
  Dict.Add('&DotDot;', #$20DC);
  Dict.Add('&DotEqual;', #$2250);
  Dict.Add('&DoubleContourIntegral;', #$222F);
  Dict.Add('&DoubleDot;', #$00A8);
  Dict.Add('&DoubleDownArrow;', #$21D3);
  Dict.Add('&DoubleLeftArrow;', #$21D0);
  Dict.Add('&DoubleLeftRightArrow;', #$21D4);
  Dict.Add('&DoubleLeftTee;', #$2AE4);
  Dict.Add('&DoubleLongLeftArrow;', #$27F8);
  Dict.Add('&DoubleLongLeftRightArrow;', #$27FA);
  Dict.Add('&DoubleLongRightArrow;', #$27F9);
  Dict.Add('&DoubleRightArrow;', #$21D2);
  Dict.Add('&DoubleRightTee;', #$22A8);
  Dict.Add('&DoubleUpArrow;', #$21D1);
  Dict.Add('&DoubleUpDownArrow;', #$21D5);
  Dict.Add('&DoubleVerticalBar;', #$2225);
  Dict.Add('&DownArrow;', #$2193);
  Dict.Add('&DownArrowBar;', #$2913);
  Dict.Add('&DownArrowUpArrow;', #$21F5);
  Dict.Add('&DownBreve;', #$0311);
  Dict.Add('&DownLeftRightVector;', #$2950);
  Dict.Add('&DownLeftTeeVector;', #$295E);
  Dict.Add('&DownLeftVector;', #$21BD);
  Dict.Add('&DownLeftVectorBar;', #$2956);
  Dict.Add('&DownRightTeeVector;', #$295F);
  Dict.Add('&DownRightVector;', #$21C1);
  Dict.Add('&DownRightVectorBar;', #$2957);
  Dict.Add('&DownTee;', #$22A4);
  Dict.Add('&DownTeeArrow;', #$21A7);
  Dict.Add('&Downarrow;', #$21D3);
  Dict.Add('&Dscr;', #$D835#$DC9F);
  Dict.Add('&Dstrok;', #$0110);
  Dict.Add('&ENG;', #$014A);
  Dict.Add('&ETH;', #$00D0);
  Dict.Add('&Eacute;', #$00C9);
  Dict.Add('&Ecaron;', #$011A);
  Dict.Add('&Ecirc;', #$00CA);
  Dict.Add('&Ecy;', #$042D);
  Dict.Add('&Edot;', #$0116);
  Dict.Add('&Efr;', #$D835#$DD08);
  Dict.Add('&Egrave;', #$00C8);
  Dict.Add('&Element;', #$2208);
  Dict.Add('&Emacr;', #$0112);
  Dict.Add('&EmptySmallSquare;', #$25FB);
  Dict.Add('&EmptyVerySmallSquare;', #$25AB);
  Dict.Add('&Eogon;', #$0118);
  Dict.Add('&Eopf;', #$D835#$DD3C);
  Dict.Add('&Epsilon;', #$0395);
  Dict.Add('&Equal;', #$2A75);
  Dict.Add('&EqualTilde;', #$2242);
  Dict.Add('&Equilibrium;', #$21CC);
  Dict.Add('&Escr;', #$2130);
  Dict.Add('&Esim;', #$2A73);
  Dict.Add('&Eta;', #$0397);
  Dict.Add('&Euml;', #$00CB);
  Dict.Add('&Exists;', #$2203);
  Dict.Add('&ExponentialE;', #$2147);
  Dict.Add('&Fcy;', #$0424);
  Dict.Add('&Ffr;', #$D835#$DD09);
  Dict.Add('&FilledSmallSquare;', #$25FC);
  Dict.Add('&FilledVerySmallSquare;', #$25AA);
  Dict.Add('&Fopf;', #$D835#$DD3D);
  Dict.Add('&ForAll;', #$2200);
  Dict.Add('&Fouriertrf;', #$2131);
  Dict.Add('&Fscr;', #$2131);
  Dict.Add('&GJcy;', #$0403);
  Dict.Add('&GT;', #$003E);
  Dict.Add('&Gamma;', #$0393);
  Dict.Add('&Gammad;', #$03DC);
  Dict.Add('&Gbreve;', #$011E);
  Dict.Add('&Gcedil;', #$0122);
  Dict.Add('&Gcirc;', #$011C);
  Dict.Add('&Gcy;', #$0413);
  Dict.Add('&Gdot;', #$0120);
  Dict.Add('&Gfr;', #$D835#$DD0A);
  Dict.Add('&Gg;', #$22D9);
  Dict.Add('&Gopf;', #$D835#$DD3E);
  Dict.Add('&GreaterEqual;', #$2265);
  Dict.Add('&GreaterEqualLess;', #$22DB);
  Dict.Add('&GreaterFullEqual;', #$2267);
  Dict.Add('&GreaterGreater;', #$2AA2);
  Dict.Add('&GreaterLess;', #$2277);
  Dict.Add('&GreaterSlantEqual;', #$2A7E);
  Dict.Add('&GreaterTilde;', #$2273);
  Dict.Add('&Gscr;', #$D835#$DCA2);
  Dict.Add('&Gt;', #$226B);
  Dict.Add('&HARDcy;', #$042A);
  Dict.Add('&Hacek;', #$02C7);
  Dict.Add('&Hat;', #$005E);
  Dict.Add('&Hcirc;', #$0124);
  Dict.Add('&Hfr;', #$210C);
  Dict.Add('&HilbertSpace;', #$210B);
  Dict.Add('&Hopf;', #$210D);
  Dict.Add('&HorizontalLine;', #$2500);
  Dict.Add('&Hscr;', #$210B);
  Dict.Add('&Hstrok;', #$0126);
  Dict.Add('&HumpDownHump;', #$224E);
  Dict.Add('&HumpEqual;', #$224F);
  Dict.Add('&IEcy;', #$0415);
  Dict.Add('&IJlig;', #$0132);
  Dict.Add('&IOcy;', #$0401);
  Dict.Add('&Iacute;', #$00CD);
  Dict.Add('&Icirc;', #$00CE);
  Dict.Add('&Icy;', #$0418);
  Dict.Add('&Idot;', #$0130);
  Dict.Add('&Ifr;', #$2111);
  Dict.Add('&Igrave;', #$00CC);
  Dict.Add('&Im;', #$2111);
  Dict.Add('&Imacr;', #$012A);
  Dict.Add('&ImaginaryI;', #$2148);
  Dict.Add('&Implies;', #$21D2);
  Dict.Add('&Int;', #$222C);
  Dict.Add('&Integral;', #$222B);
  Dict.Add('&Intersection;', #$22C2);
  Dict.Add('&InvisibleComma;', #$2063);
  Dict.Add('&InvisibleTimes;', #$2062);
  Dict.Add('&Iogon;', #$012E);
  Dict.Add('&Iopf;', #$D835#$DD40);
  Dict.Add('&Iota;', #$0399);
  Dict.Add('&Iscr;', #$2110);
  Dict.Add('&Itilde;', #$0128);
  Dict.Add('&Iukcy;', #$0406);
  Dict.Add('&Iuml;', #$00CF);
  Dict.Add('&Jcirc;', #$0134);
  Dict.Add('&Jcy;', #$0419);
  Dict.Add('&Jfr;', #$D835#$DD0D);
  Dict.Add('&Jopf;', #$D835#$DD41);
  Dict.Add('&Jscr;', #$D835#$DCA5);
  Dict.Add('&Jsercy;', #$0408);
  Dict.Add('&Jukcy;', #$0404);
  Dict.Add('&KHcy;', #$0425);
  Dict.Add('&KJcy;', #$040C);
  Dict.Add('&Kappa;', #$039A);
  Dict.Add('&Kcedil;', #$0136);
  Dict.Add('&Kcy;', #$041A);
  Dict.Add('&Kfr;', #$D835#$DD0E);
  Dict.Add('&Kopf;', #$D835#$DD42);
  Dict.Add('&Kscr;', #$D835#$DCA6);
  Dict.Add('&LJcy;', #$0409);
  Dict.Add('&LT;', #$003C);
  Dict.Add('&Lacute;', #$0139);
  Dict.Add('&Lambda;', #$039B);
  Dict.Add('&Lang;', #$27EA);
  Dict.Add('&Laplacetrf;', #$2112);
  Dict.Add('&Larr;', #$219E);
  Dict.Add('&Lcaron;', #$013D);
  Dict.Add('&Lcedil;', #$013B);
  Dict.Add('&Lcy;', #$041B);
  Dict.Add('&LeftAngleBracket;', #$27E8);
  Dict.Add('&LeftArrow;', #$2190);
  Dict.Add('&LeftArrowBar;', #$21E4);
  Dict.Add('&LeftArrowRightArrow;', #$21C6);
  Dict.Add('&LeftCeiling;', #$2308);
  Dict.Add('&LeftDoubleBracket;', #$27E6);
  Dict.Add('&LeftDownTeeVector;', #$2961);
  Dict.Add('&LeftDownVector;', #$21C3);
  Dict.Add('&LeftDownVectorBar;', #$2959);
  Dict.Add('&LeftFloor;', #$230A);
  Dict.Add('&LeftRightArrow;', #$2194);
  Dict.Add('&LeftRightVector;', #$294E);
  Dict.Add('&LeftTee;', #$22A3);
  Dict.Add('&LeftTeeArrow;', #$21A4);
  Dict.Add('&LeftTeeVector;', #$295A);
  Dict.Add('&LeftTriangle;', #$22B2);
  Dict.Add('&LeftTriangleBar;', #$29CF);
  Dict.Add('&LeftTriangleEqual;', #$22B4);
  Dict.Add('&LeftUpDownVector;', #$2951);
  Dict.Add('&LeftUpTeeVector;', #$2960);
  Dict.Add('&LeftUpVector;', #$21BF);
  Dict.Add('&LeftUpVectorBar;', #$2958);
  Dict.Add('&LeftVector;', #$21BC);
  Dict.Add('&LeftVectorBar;', #$2952);
  Dict.Add('&Leftarrow;', #$21D0);
  Dict.Add('&Leftrightarrow;', #$21D4);
  Dict.Add('&LessEqualGreater;', #$22DA);
  Dict.Add('&LessFullEqual;', #$2266);
  Dict.Add('&LessGreater;', #$2276);
  Dict.Add('&LessLess;', #$2AA1);
  Dict.Add('&LessSlantEqual;', #$2A7D);
  Dict.Add('&LessTilde;', #$2272);
  Dict.Add('&Lfr;', #$D835#$DD0F);
  Dict.Add('&Ll;', #$22D8);
  Dict.Add('&Lleftarrow;', #$21DA);
  Dict.Add('&Lmidot;', #$013F);
  Dict.Add('&LongLeftArrow;', #$27F5);
  Dict.Add('&LongLeftRightArrow;', #$27F7);
  Dict.Add('&LongRightArrow;', #$27F6);
  Dict.Add('&Longleftarrow;', #$27F8);
  Dict.Add('&Longleftrightarrow;', #$27FA);
  Dict.Add('&Longrightarrow;', #$27F9);
  Dict.Add('&Lopf;', #$D835#$DD43);
  Dict.Add('&LowerLeftArrow;', #$2199);
  Dict.Add('&LowerRightArrow;', #$2198);
  Dict.Add('&Lscr;', #$2112);
  Dict.Add('&Lsh;', #$21B0);
  Dict.Add('&Lstrok;', #$0141);
  Dict.Add('&Lt;', #$226A);
  Dict.Add('&Map;', #$2905);
  Dict.Add('&Mcy;', #$041C);
  Dict.Add('&MediumSpace;', #$205F);
  Dict.Add('&Mellintrf;', #$2133);
  Dict.Add('&Mfr;', #$D835#$DD10);
  Dict.Add('&MinusPlus;', #$2213);
  Dict.Add('&Mopf;', #$D835#$DD44);
  Dict.Add('&Mscr;', #$2133);
  Dict.Add('&Mu;', #$039C);
  Dict.Add('&NJcy;', #$040A);
  Dict.Add('&Nacute;', #$0143);
  Dict.Add('&Ncaron;', #$0147);
  Dict.Add('&Ncedil;', #$0145);
  Dict.Add('&Ncy;', #$041D);
  Dict.Add('&NegativeMediumSpace;', #$200B);
  Dict.Add('&NegativeThickSpace;', #$200B);
  Dict.Add('&NegativeThinSpace;', #$200B);
  Dict.Add('&NegativeVeryThinSpace;', #$200B);
  Dict.Add('&NestedGreaterGreater;', #$226B);
  Dict.Add('&NestedLessLess;', #$226A);
  Dict.Add('&NewLine;', #$000A);
  Dict.Add('&Nfr;', #$D835#$DD11);
  Dict.Add('&NoBreak;', #$2060);
  Dict.Add('&NonBreakingSpace;', #$00A0);
  Dict.Add('&Nopf;', #$2115);
  Dict.Add('&Not;', #$2AEC);
  Dict.Add('&NotCongruent;', #$2262);
  Dict.Add('&NotCupCap;', #$226D);
  Dict.Add('&NotDoubleVerticalBar;', #$2226);
  Dict.Add('&NotElement;', #$2209);
  Dict.Add('&NotEqual;', #$2260);
  Dict.Add('&NotEqualTilde;', #$2242#$0338);
  Dict.Add('&NotExists;', #$2204);
  Dict.Add('&NotGreater;', #$226F);
  Dict.Add('&NotGreaterEqual;', #$2271);
  Dict.Add('&NotGreaterFullEqual;', #$2267#$0338);
  Dict.Add('&NotGreaterGreater;', #$226B#$0338);
  Dict.Add('&NotGreaterLess;', #$2279);
  Dict.Add('&NotGreaterSlantEqual;', #$2A7E#$0338);
  Dict.Add('&NotGreaterTilde;', #$2275);
  Dict.Add('&NotHumpDownHump;', #$224E#$0338);
  Dict.Add('&NotHumpEqual;', #$224F#$0338);
  Dict.Add('&NotLeftTriangle;', #$22EA);
  Dict.Add('&NotLeftTriangleBar;', #$29CF#$0338);
  Dict.Add('&NotLeftTriangleEqual;', #$22EC);
  Dict.Add('&NotLess;', #$226E);
  Dict.Add('&NotLessEqual;', #$2270);
  Dict.Add('&NotLessGreater;', #$2278);
  Dict.Add('&NotLessLess;', #$226A#$0338);
  Dict.Add('&NotLessSlantEqual;', #$2A7D#$0338);
  Dict.Add('&NotLessTilde;', #$2274);
  Dict.Add('&NotNestedGreaterGreater;', #$2AA2#$0338);
  Dict.Add('&NotNestedLessLess;', #$2AA1#$0338);
  Dict.Add('&NotPrecedes;', #$2280);
  Dict.Add('&NotPrecedesEqual;', #$2AAF#$0338);
  Dict.Add('&NotPrecedesSlantEqual;', #$22E0);
  Dict.Add('&NotReverseElement;', #$220C);
  Dict.Add('&NotRightTriangle;', #$22EB);
  Dict.Add('&NotRightTriangleBar;', #$29D0#$0338);
  Dict.Add('&NotRightTriangleEqual;', #$22ED);
  Dict.Add('&NotSquareSubset;', #$228F#$0338);
  Dict.Add('&NotSquareSubsetEqual;', #$22E2);
  Dict.Add('&NotSquareSuperset;', #$2290#$0338);
  Dict.Add('&NotSquareSupersetEqual;', #$22E3);
  Dict.Add('&NotSubset;', #$2282#$20D2);
  Dict.Add('&NotSubsetEqual;', #$2288);
  Dict.Add('&NotSucceeds;', #$2281);
  Dict.Add('&NotSucceedsEqual;', #$2AB0#$0338);
  Dict.Add('&NotSucceedsSlantEqual;', #$22E1);
  Dict.Add('&NotSucceedsTilde;', #$227F#$0338);
  Dict.Add('&NotSuperset;', #$2283#$20D2);
  Dict.Add('&NotSupersetEqual;', #$2289);
  Dict.Add('&NotTilde;', #$2241);
  Dict.Add('&NotTildeEqual;', #$2244);
  Dict.Add('&NotTildeFullEqual;', #$2247);
  Dict.Add('&NotTildeTilde;', #$2249);
  Dict.Add('&NotVerticalBar;', #$2224);
  Dict.Add('&Nscr;', #$D835#$DCA9);
  Dict.Add('&Ntilde;', #$00D1);
  Dict.Add('&Nu;', #$039D);
  Dict.Add('&OElig;', #$0152);
  Dict.Add('&Oacute;', #$00D3);
  Dict.Add('&Ocirc;', #$00D4);
  Dict.Add('&Ocy;', #$041E);
  Dict.Add('&Odblac;', #$0150);
  Dict.Add('&Ofr;', #$D835#$DD12);
  Dict.Add('&Ograve;', #$00D2);
  Dict.Add('&Omacr;', #$014C);
  Dict.Add('&Omega;', #$03A9);
  Dict.Add('&Omicron;', #$039F);
  Dict.Add('&Oopf;', #$D835#$DD46);
  Dict.Add('&OpenCurlyDoubleQuote;', #$201C);
  Dict.Add('&OpenCurlyQuote;', #$2018);
  Dict.Add('&Or;', #$2A54);
  Dict.Add('&Oscr;', #$D835#$DCAA);
  Dict.Add('&Oslash;', #$00D8);
  Dict.Add('&Otilde;', #$00D5);
  Dict.Add('&Otimes;', #$2A37);
  Dict.Add('&Ouml;', #$00D6);
  Dict.Add('&OverBar;', #$203E);
  Dict.Add('&OverBrace;', #$23DE);
  Dict.Add('&OverBracket;', #$23B4);
  Dict.Add('&OverParenthesis;', #$23DC);
  Dict.Add('&PartialD;', #$2202);
  Dict.Add('&Pcy;', #$041F);
  Dict.Add('&Pfr;', #$D835#$DD13);
  Dict.Add('&Phi;', #$03A6);
  Dict.Add('&Pi;', #$03A0);
  Dict.Add('&PlusMinus;', #$00B1);
  Dict.Add('&Poincareplane;', #$210C);
  Dict.Add('&Popf;', #$2119);
  Dict.Add('&Pr;', #$2ABB);
  Dict.Add('&Precedes;', #$227A);
  Dict.Add('&PrecedesEqual;', #$2AAF);
  Dict.Add('&PrecedesSlantEqual;', #$227C);
  Dict.Add('&PrecedesTilde;', #$227E);
  Dict.Add('&Prime;', #$2033);
  Dict.Add('&Product;', #$220F);
  Dict.Add('&Proportion;', #$2237);
  Dict.Add('&Proportional;', #$221D);
  Dict.Add('&Pscr;', #$D835#$DCAB);
  Dict.Add('&Psi;', #$03A8);
  Dict.Add('&QUOT;', #$0022);
  Dict.Add('&Qfr;', #$D835#$DD14);
  Dict.Add('&Qopf;', #$211A);
  Dict.Add('&Qscr;', #$D835#$DCAC);
  Dict.Add('&RBarr;', #$2910);
  Dict.Add('&REG;', #$00AE);
  Dict.Add('&Racute;', #$0154);
  Dict.Add('&Rang;', #$27EB);
  Dict.Add('&Rarr;', #$21A0);
  Dict.Add('&Rarrtl;', #$2916);
  Dict.Add('&Rcaron;', #$0158);
  Dict.Add('&Rcedil;', #$0156);
  Dict.Add('&Rcy;', #$0420);
  Dict.Add('&Re;', #$211C);
  Dict.Add('&ReverseElement;', #$220B);
  Dict.Add('&ReverseEquilibrium;', #$21CB);
  Dict.Add('&ReverseUpEquilibrium;', #$296F);
  Dict.Add('&Rfr;', #$211C);
  Dict.Add('&Rho;', #$03A1);
  Dict.Add('&RightAngleBracket;', #$27E9);
  Dict.Add('&RightArrow;', #$2192);
  Dict.Add('&RightArrowBar;', #$21E5);
  Dict.Add('&RightArrowLeftArrow;', #$21C4);
  Dict.Add('&RightCeiling;', #$2309);
  Dict.Add('&RightDoubleBracket;', #$27E7);
  Dict.Add('&RightDownTeeVector;', #$295D);
  Dict.Add('&RightDownVector;', #$21C2);
  Dict.Add('&RightDownVectorBar;', #$2955);
  Dict.Add('&RightFloor;', #$230B);
  Dict.Add('&RightTee;', #$22A2);
  Dict.Add('&RightTeeArrow;', #$21A6);
  Dict.Add('&RightTeeVector;', #$295B);
  Dict.Add('&RightTriangle;', #$22B3);
  Dict.Add('&RightTriangleBar;', #$29D0);
  Dict.Add('&RightTriangleEqual;', #$22B5);
  Dict.Add('&RightUpDownVector;', #$294F);
  Dict.Add('&RightUpTeeVector;', #$295C);
  Dict.Add('&RightUpVector;', #$21BE);
  Dict.Add('&RightUpVectorBar;', #$2954);
  Dict.Add('&RightVector;', #$21C0);
  Dict.Add('&RightVectorBar;', #$2953);
  Dict.Add('&Rightarrow;', #$21D2);
  Dict.Add('&Ropf;', #$211D);
  Dict.Add('&RoundImplies;', #$2970);
  Dict.Add('&Rrightarrow;', #$21DB);
  Dict.Add('&Rscr;', #$211B);
  Dict.Add('&Rsh;', #$21B1);
  Dict.Add('&RuleDelayed;', #$29F4);
  Dict.Add('&SHCHcy;', #$0429);
  Dict.Add('&SHcy;', #$0428);
  Dict.Add('&SOFTcy;', #$042C);
  Dict.Add('&Sacute;', #$015A);
  Dict.Add('&Sc;', #$2ABC);
  Dict.Add('&Scaron;', #$0160);
  Dict.Add('&Scedil;', #$015E);
  Dict.Add('&Scirc;', #$015C);
  Dict.Add('&Scy;', #$0421);
  Dict.Add('&Sfr;', #$D835#$DD16);
  Dict.Add('&ShortDownArrow;', #$2193);
  Dict.Add('&ShortLeftArrow;', #$2190);
  Dict.Add('&ShortRightArrow;', #$2192);
  Dict.Add('&ShortUpArrow;', #$2191);
  Dict.Add('&Sigma;', #$03A3);
  Dict.Add('&SmallCircle;', #$2218);
  Dict.Add('&Sopf;', #$D835#$DD4A);
  Dict.Add('&Sqrt;', #$221A);
  Dict.Add('&Square;', #$25A1);
  Dict.Add('&SquareIntersection;', #$2293);
  Dict.Add('&SquareSubset;', #$228F);
  Dict.Add('&SquareSubsetEqual;', #$2291);
  Dict.Add('&SquareSuperset;', #$2290);
  Dict.Add('&SquareSupersetEqual;', #$2292);
  Dict.Add('&SquareUnion;', #$2294);
  Dict.Add('&Sscr;', #$D835#$DCAE);
  Dict.Add('&Star;', #$22C6);
  Dict.Add('&Sub;', #$22D0);
  Dict.Add('&Subset;', #$22D0);
  Dict.Add('&SubsetEqual;', #$2286);
  Dict.Add('&Succeeds;', #$227B);
  Dict.Add('&SucceedsEqual;', #$2AB0);
  Dict.Add('&SucceedsSlantEqual;', #$227D);
  Dict.Add('&SucceedsTilde;', #$227F);
  Dict.Add('&SuchThat;', #$220B);
  Dict.Add('&Sum;', #$2211);
  Dict.Add('&Sup;', #$22D1);
  Dict.Add('&Superset;', #$2283);
  Dict.Add('&SupersetEqual;', #$2287);
  Dict.Add('&Supset;', #$22D1);
  Dict.Add('&THORN;', #$00DE);
  Dict.Add('&TRADE;', #$2122);
  Dict.Add('&TSHcy;', #$040B);
  Dict.Add('&TScy;', #$0426);
  Dict.Add('&Tab;', #$0009);
  Dict.Add('&Tau;', #$03A4);
  Dict.Add('&Tcaron;', #$0164);
  Dict.Add('&Tcedil;', #$0162);
  Dict.Add('&Tcy;', #$0422);
  Dict.Add('&Tfr;', #$D835#$DD17);
  Dict.Add('&Therefore;', #$2234);
  Dict.Add('&Theta;', #$0398);
  Dict.Add('&ThickSpace;', #$205F#$200A);
  Dict.Add('&ThinSpace;', #$2009);
  Dict.Add('&Tilde;', #$223C);
  Dict.Add('&TildeEqual;', #$2243);
  Dict.Add('&TildeFullEqual;', #$2245);
  Dict.Add('&TildeTilde;', #$2248);
  Dict.Add('&Topf;', #$D835#$DD4B);
  Dict.Add('&TripleDot;', #$20DB);
  Dict.Add('&Tscr;', #$D835#$DCAF);
  Dict.Add('&Tstrok;', #$0166);
  Dict.Add('&Uacute;', #$00DA);
  Dict.Add('&Uarr;', #$219F);
  Dict.Add('&Uarrocir;', #$2949);
  Dict.Add('&Ubrcy;', #$040E);
  Dict.Add('&Ubreve;', #$016C);
  Dict.Add('&Ucirc;', #$00DB);
  Dict.Add('&Ucy;', #$0423);
  Dict.Add('&Udblac;', #$0170);
  Dict.Add('&Ufr;', #$D835#$DD18);
  Dict.Add('&Ugrave;', #$00D9);
  Dict.Add('&Umacr;', #$016A);
  Dict.Add('&UnderBar;', #$005F);
  Dict.Add('&UnderBrace;', #$23DF);
  Dict.Add('&UnderBracket;', #$23B5);
  Dict.Add('&UnderParenthesis;', #$23DD);
  Dict.Add('&Union;', #$22C3);
  Dict.Add('&UnionPlus;', #$228E);
  Dict.Add('&Uogon;', #$0172);
  Dict.Add('&Uopf;', #$D835#$DD4C);
  Dict.Add('&UpArrow;', #$2191);
  Dict.Add('&UpArrowBar;', #$2912);
  Dict.Add('&UpArrowDownArrow;', #$21C5);
  Dict.Add('&UpDownArrow;', #$2195);
  Dict.Add('&UpEquilibrium;', #$296E);
  Dict.Add('&UpTee;', #$22A5);
  Dict.Add('&UpTeeArrow;', #$21A5);
  Dict.Add('&Uparrow;', #$21D1);
  Dict.Add('&Updownarrow;', #$21D5);
  Dict.Add('&UpperLeftArrow;', #$2196);
  Dict.Add('&UpperRightArrow;', #$2197);
  Dict.Add('&Upsi;', #$03D2);
  Dict.Add('&Upsilon;', #$03A5);
  Dict.Add('&Uring;', #$016E);
  Dict.Add('&Uscr;', #$D835#$DCB0);
  Dict.Add('&Utilde;', #$0168);
  Dict.Add('&Uuml;', #$00DC);
  Dict.Add('&VDash;', #$22AB);
  Dict.Add('&Vbar;', #$2AEB);
  Dict.Add('&Vcy;', #$0412);
  Dict.Add('&Vdash;', #$22A9);
  Dict.Add('&Vdashl;', #$2AE6);
  Dict.Add('&Vee;', #$22C1);
  Dict.Add('&Verbar;', #$2016);
  Dict.Add('&Vert;', #$2016);
  Dict.Add('&VerticalBar;', #$2223);
  Dict.Add('&VerticalLine;', #$007C);
  Dict.Add('&VerticalSeparator;', #$2758);
  Dict.Add('&VerticalTilde;', #$2240);
  Dict.Add('&VeryThinSpace;', #$200A);
  Dict.Add('&Vfr;', #$D835#$DD19);
  Dict.Add('&Vopf;', #$D835#$DD4D);
  Dict.Add('&Vscr;', #$D835#$DCB1);
  Dict.Add('&Vvdash;', #$22AA);
  Dict.Add('&Wcirc;', #$0174);
  Dict.Add('&Wedge;', #$22C0);
  Dict.Add('&Wfr;', #$D835#$DD1A);
  Dict.Add('&Wopf;', #$D835#$DD4E);
  Dict.Add('&Wscr;', #$D835#$DCB2);
  Dict.Add('&Xfr;', #$D835#$DD1B);
  Dict.Add('&Xi;', #$039E);
  Dict.Add('&Xopf;', #$D835#$DD4F);
  Dict.Add('&Xscr;', #$D835#$DCB3);
  Dict.Add('&YAcy;', #$042F);
  Dict.Add('&YIcy;', #$0407);
  Dict.Add('&YUcy;', #$042E);
  Dict.Add('&Yacute;', #$00DD);
  Dict.Add('&Ycirc;', #$0176);
  Dict.Add('&Ycy;', #$042B);
  Dict.Add('&Yfr;', #$D835#$DD1C);
  Dict.Add('&Yopf;', #$D835#$DD50);
  Dict.Add('&Yscr;', #$D835#$DCB4);
  Dict.Add('&Yuml;', #$0178);
  Dict.Add('&ZHcy;', #$0416);
  Dict.Add('&Zacute;', #$0179);
  Dict.Add('&Zcaron;', #$017D);
  Dict.Add('&Zcy;', #$0417);
  Dict.Add('&Zdot;', #$017B);
  Dict.Add('&ZeroWidthSpace;', #$200B);
  Dict.Add('&Zeta;', #$0396);
  Dict.Add('&Zfr;', #$2128);
  Dict.Add('&Zopf;', #$2124);
  Dict.Add('&Zscr;', #$D835#$DCB5);
  Dict.Add('&aacute;', #$00E1);
  Dict.Add('&abreve;', #$0103);
  Dict.Add('&ac;', #$223E);
  Dict.Add('&acE;', #$223E#$0333);
  Dict.Add('&acd;', #$223F);
  Dict.Add('&acirc;', #$00E2);
  Dict.Add('&acute;', #$00B4);
  Dict.Add('&acy;', #$0430);
  Dict.Add('&aelig;', #$00E6);
  Dict.Add('&af;', #$2061);
  Dict.Add('&afr;', #$D835#$DD1E);
  Dict.Add('&agrave;', #$00E0);
  Dict.Add('&alefsym;', #$2135);
  Dict.Add('&aleph;', #$2135);
  Dict.Add('&alpha;', #$03B1);
  Dict.Add('&amacr;', #$0101);
  Dict.Add('&amalg;', #$2A3F);
  Dict.Add('&amp;', #$0026);
  Dict.Add('&and;', #$2227);
  Dict.Add('&andand;', #$2A55);
  Dict.Add('&andd;', #$2A5C);
  Dict.Add('&andslope;', #$2A58);
  Dict.Add('&andv;', #$2A5A);
  Dict.Add('&ang;', #$2220);
  Dict.Add('&ange;', #$29A4);
  Dict.Add('&angle;', #$2220);
  Dict.Add('&angmsd;', #$2221);
  Dict.Add('&angmsdaa;', #$29A8);
  Dict.Add('&angmsdab;', #$29A9);
  Dict.Add('&angmsdac;', #$29AA);
  Dict.Add('&angmsdad;', #$29AB);
  Dict.Add('&angmsdae;', #$29AC);
  Dict.Add('&angmsdaf;', #$29AD);
  Dict.Add('&angmsdag;', #$29AE);
  Dict.Add('&angmsdah;', #$29AF);
  Dict.Add('&angrt;', #$221F);
  Dict.Add('&angrtvb;', #$22BE);
  Dict.Add('&angrtvbd;', #$299D);
  Dict.Add('&angsph;', #$2222);
  Dict.Add('&angst;', #$00C5);
  Dict.Add('&angzarr;', #$237C);
  Dict.Add('&aogon;', #$0105);
  Dict.Add('&aopf;', #$D835#$DD52);
  Dict.Add('&ap;', #$2248);
  Dict.Add('&apE;', #$2A70);
  Dict.Add('&apacir;', #$2A6F);
  Dict.Add('&ape;', #$224A);
  Dict.Add('&apid;', #$224B);
  Dict.Add('&apos;', #$0027);
  Dict.Add('&approx;', #$2248);
  Dict.Add('&approxeq;', #$224A);
  Dict.Add('&aring;', #$00E5);
  Dict.Add('&ascr;', #$D835#$DCB6);
  Dict.Add('&ast;', #$002A);
  Dict.Add('&asymp;', #$2248);
  Dict.Add('&asympeq;', #$224D);
  Dict.Add('&atilde;', #$00E3);
  Dict.Add('&auml;', #$00E4);
  Dict.Add('&awconint;', #$2233);
  Dict.Add('&awint;', #$2A11);
  Dict.Add('&bNot;', #$2AED);
  Dict.Add('&backcong;', #$224C);
  Dict.Add('&backepsilon;', #$03F6);
  Dict.Add('&backprime;', #$2035);
  Dict.Add('&backsim;', #$223D);
  Dict.Add('&backsimeq;', #$22CD);
  Dict.Add('&barvee;', #$22BD);
  Dict.Add('&barwed;', #$2305);
  Dict.Add('&barwedge;', #$2305);
  Dict.Add('&bbrk;', #$23B5);
  Dict.Add('&bbrktbrk;', #$23B6);
  Dict.Add('&bcong;', #$224C);
  Dict.Add('&bcy;', #$0431);
  Dict.Add('&bdquo;', #$201E);
  Dict.Add('&becaus;', #$2235);
  Dict.Add('&because;', #$2235);
  Dict.Add('&bemptyv;', #$29B0);
  Dict.Add('&bepsi;', #$03F6);
  Dict.Add('&bernou;', #$212C);
  Dict.Add('&beta;', #$03B2);
  Dict.Add('&beth;', #$2136);
  Dict.Add('&between;', #$226C);
  Dict.Add('&bfr;', #$D835#$DD1F);
  Dict.Add('&bigcap;', #$22C2);
  Dict.Add('&bigcirc;', #$25EF);
  Dict.Add('&bigcup;', #$22C3);
  Dict.Add('&bigodot;', #$2A00);
  Dict.Add('&bigoplus;', #$2A01);
  Dict.Add('&bigotimes;', #$2A02);
  Dict.Add('&bigsqcup;', #$2A06);
  Dict.Add('&bigstar;', #$2605);
  Dict.Add('&bigtriangledown;', #$25BD);
  Dict.Add('&bigtriangleup;', #$25B3);
  Dict.Add('&biguplus;', #$2A04);
  Dict.Add('&bigvee;', #$22C1);
  Dict.Add('&bigwedge;', #$22C0);
  Dict.Add('&bkarow;', #$290D);
  Dict.Add('&blacklozenge;', #$29EB);
  Dict.Add('&blacksquare;', #$25AA);
  Dict.Add('&blacktriangle;', #$25B4);
  Dict.Add('&blacktriangledown;', #$25BE);
  Dict.Add('&blacktriangleleft;', #$25C2);
  Dict.Add('&blacktriangleright;', #$25B8);
  Dict.Add('&blank;', #$2423);
  Dict.Add('&blk12;', #$2592);
  Dict.Add('&blk14;', #$2591);
  Dict.Add('&blk34;', #$2593);
  Dict.Add('&block;', #$2588);
  Dict.Add('&bne;', #$003D#$20E5);
  Dict.Add('&bnequiv;', #$2261#$20E5);
  Dict.Add('&bnot;', #$2310);
  Dict.Add('&bopf;', #$D835#$DD53);
  Dict.Add('&bot;', #$22A5);
  Dict.Add('&bottom;', #$22A5);
  Dict.Add('&bowtie;', #$22C8);
  Dict.Add('&boxDL;', #$2557);
  Dict.Add('&boxDR;', #$2554);
  Dict.Add('&boxDl;', #$2556);
  Dict.Add('&boxDr;', #$2553);
  Dict.Add('&boxH;', #$2550);
  Dict.Add('&boxHD;', #$2566);
  Dict.Add('&boxHU;', #$2569);
  Dict.Add('&boxHd;', #$2564);
  Dict.Add('&boxHu;', #$2567);
  Dict.Add('&boxUL;', #$255D);
  Dict.Add('&boxUR;', #$255A);
  Dict.Add('&boxUl;', #$255C);
  Dict.Add('&boxUr;', #$2559);
  Dict.Add('&boxV;', #$2551);
  Dict.Add('&boxVH;', #$256C);
  Dict.Add('&boxVL;', #$2563);
  Dict.Add('&boxVR;', #$2560);
  Dict.Add('&boxVh;', #$256B);
  Dict.Add('&boxVl;', #$2562);
  Dict.Add('&boxVr;', #$255F);
  Dict.Add('&boxbox;', #$29C9);
  Dict.Add('&boxdL;', #$2555);
  Dict.Add('&boxdR;', #$2552);
  Dict.Add('&boxdl;', #$2510);
  Dict.Add('&boxdr;', #$250C);
  Dict.Add('&boxh;', #$2500);
  Dict.Add('&boxhD;', #$2565);
  Dict.Add('&boxhU;', #$2568);
  Dict.Add('&boxhd;', #$252C);
  Dict.Add('&boxhu;', #$2534);
  Dict.Add('&boxminus;', #$229F);
  Dict.Add('&boxplus;', #$229E);
  Dict.Add('&boxtimes;', #$22A0);
  Dict.Add('&boxuL;', #$255B);
  Dict.Add('&boxuR;', #$2558);
  Dict.Add('&boxul;', #$2518);
  Dict.Add('&boxur;', #$2514);
  Dict.Add('&boxv;', #$2502);
  Dict.Add('&boxvH;', #$256A);
  Dict.Add('&boxvL;', #$2561);
  Dict.Add('&boxvR;', #$255E);
  Dict.Add('&boxvh;', #$253C);
  Dict.Add('&boxvl;', #$2524);
  Dict.Add('&boxvr;', #$251C);
  Dict.Add('&bprime;', #$2035);
  Dict.Add('&breve;', #$02D8);
  Dict.Add('&brvbar;', #$00A6);
  Dict.Add('&bscr;', #$D835#$DCB7);
  Dict.Add('&bsemi;', #$204F);
  Dict.Add('&bsim;', #$223D);
  Dict.Add('&bsime;', #$22CD);
  Dict.Add('&bsol;', #$005C);
  Dict.Add('&bsolb;', #$29C5);
  Dict.Add('&bsolhsub;', #$27C8);
  Dict.Add('&bull;', #$2022);
  Dict.Add('&bullet;', #$2022);
  Dict.Add('&bump;', #$224E);
  Dict.Add('&bumpE;', #$2AAE);
  Dict.Add('&bumpe;', #$224F);
  Dict.Add('&bumpeq;', #$224F);
  Dict.Add('&cacute;', #$0107);
  Dict.Add('&cap;', #$2229);
  Dict.Add('&capand;', #$2A44);
  Dict.Add('&capbrcup;', #$2A49);
  Dict.Add('&capcap;', #$2A4B);
  Dict.Add('&capcup;', #$2A47);
  Dict.Add('&capdot;', #$2A40);
  Dict.Add('&caps;', #$2229#$FE00);
  Dict.Add('&caret;', #$2041);
  Dict.Add('&caron;', #$02C7);
  Dict.Add('&ccaps;', #$2A4D);
  Dict.Add('&ccaron;', #$010D);
  Dict.Add('&ccedil;', #$00E7);
  Dict.Add('&ccirc;', #$0109);
  Dict.Add('&ccups;', #$2A4C);
  Dict.Add('&ccupssm;', #$2A50);
  Dict.Add('&cdot;', #$010B);
  Dict.Add('&cedil;', #$00B8);
  Dict.Add('&cemptyv;', #$29B2);
  Dict.Add('&cent;', #$00A2);
  Dict.Add('&centerdot;', #$00B7);
  Dict.Add('&cfr;', #$D835#$DD20);
  Dict.Add('&chcy;', #$0447);
  Dict.Add('&check;', #$2713);
  Dict.Add('&checkmark;', #$2713);
  Dict.Add('&chi;', #$03C7);
  Dict.Add('&cir;', #$25CB);
  Dict.Add('&cirE;', #$29C3);
  Dict.Add('&circ;', #$02C6);
  Dict.Add('&circeq;', #$2257);
  Dict.Add('&circlearrowleft;', #$21BA);
  Dict.Add('&circlearrowright;', #$21BB);
  Dict.Add('&circledR;', #$00AE);
  Dict.Add('&circledS;', #$24C8);
  Dict.Add('&circledast;', #$229B);
  Dict.Add('&circledcirc;', #$229A);
  Dict.Add('&circleddash;', #$229D);
  Dict.Add('&cire;', #$2257);
  Dict.Add('&cirfnint;', #$2A10);
  Dict.Add('&cirmid;', #$2AEF);
  Dict.Add('&cirscir;', #$29C2);
  Dict.Add('&clubs;', #$2663);
  Dict.Add('&clubsuit;', #$2663);
  Dict.Add('&colon;', #$003A);
  Dict.Add('&colone;', #$2254);
  Dict.Add('&coloneq;', #$2254);
  Dict.Add('&comma;', #$002C);
  Dict.Add('&commat;', #$0040);
  Dict.Add('&comp;', #$2201);
  Dict.Add('&compfn;', #$2218);
  Dict.Add('&complement;', #$2201);
  Dict.Add('&complexes;', #$2102);
  Dict.Add('&cong;', #$2245);
  Dict.Add('&congdot;', #$2A6D);
  Dict.Add('&conint;', #$222E);
  Dict.Add('&copf;', #$D835#$DD54);
  Dict.Add('&coprod;', #$2210);
  Dict.Add('&copy;', #$00A9);
  Dict.Add('&copysr;', #$2117);
  Dict.Add('&crarr;', #$21B5);
  Dict.Add('&cross;', #$2717);
  Dict.Add('&cscr;', #$D835#$DCB8);
  Dict.Add('&csub;', #$2ACF);
  Dict.Add('&csube;', #$2AD1);
  Dict.Add('&csup;', #$2AD0);
  Dict.Add('&csupe;', #$2AD2);
  Dict.Add('&ctdot;', #$22EF);
  Dict.Add('&cudarrl;', #$2938);
  Dict.Add('&cudarrr;', #$2935);
  Dict.Add('&cuepr;', #$22DE);
  Dict.Add('&cuesc;', #$22DF);
  Dict.Add('&cularr;', #$21B6);
  Dict.Add('&cularrp;', #$293D);
  Dict.Add('&cup;', #$222A);
  Dict.Add('&cupbrcap;', #$2A48);
  Dict.Add('&cupcap;', #$2A46);
  Dict.Add('&cupcup;', #$2A4A);
  Dict.Add('&cupdot;', #$228D);
  Dict.Add('&cupor;', #$2A45);
  Dict.Add('&cups;', #$222A#$FE00);
  Dict.Add('&curarr;', #$21B7);
  Dict.Add('&curarrm;', #$293C);
  Dict.Add('&curlyeqprec;', #$22DE);
  Dict.Add('&curlyeqsucc;', #$22DF);
  Dict.Add('&curlyvee;', #$22CE);
  Dict.Add('&curlywedge;', #$22CF);
  Dict.Add('&curren;', #$00A4);
  Dict.Add('&curvearrowleft;', #$21B6);
  Dict.Add('&curvearrowright;', #$21B7);
  Dict.Add('&cuvee;', #$22CE);
  Dict.Add('&cuwed;', #$22CF);
  Dict.Add('&cwconint;', #$2232);
  Dict.Add('&cwint;', #$2231);
  Dict.Add('&cylcty;', #$232D);
  Dict.Add('&dArr;', #$21D3);
  Dict.Add('&dHar;', #$2965);
  Dict.Add('&dagger;', #$2020);
  Dict.Add('&daleth;', #$2138);
  Dict.Add('&darr;', #$2193);
  Dict.Add('&dash;', #$2010);
  Dict.Add('&dashv;', #$22A3);
  Dict.Add('&dbkarow;', #$290F);
  Dict.Add('&dblac;', #$02DD);
  Dict.Add('&dcaron;', #$010F);
  Dict.Add('&dcy;', #$0434);
  Dict.Add('&dd;', #$2146);
  Dict.Add('&ddagger;', #$2021);
  Dict.Add('&ddarr;', #$21CA);
  Dict.Add('&ddotseq;', #$2A77);
  Dict.Add('&deg;', #$00B0);
  Dict.Add('&delta;', #$03B4);
  Dict.Add('&demptyv;', #$29B1);
  Dict.Add('&dfisht;', #$297F);
  Dict.Add('&dfr;', #$D835#$DD21);
  Dict.Add('&dharl;', #$21C3);
  Dict.Add('&dharr;', #$21C2);
  Dict.Add('&diam;', #$22C4);
  Dict.Add('&diamond;', #$22C4);
  Dict.Add('&diamondsuit;', #$2666);
  Dict.Add('&diams;', #$2666);
  Dict.Add('&die;', #$00A8);
  Dict.Add('&digamma;', #$03DD);
  Dict.Add('&disin;', #$22F2);
  Dict.Add('&div;', #$00F7);
  Dict.Add('&divide;', #$00F7);
  Dict.Add('&divideontimes;', #$22C7);
  Dict.Add('&divonx;', #$22C7);
  Dict.Add('&djcy;', #$0452);
  Dict.Add('&dlcorn;', #$231E);
  Dict.Add('&dlcrop;', #$230D);
  Dict.Add('&dollar;', #$0024);
  Dict.Add('&dopf;', #$D835#$DD55);
  Dict.Add('&dot;', #$02D9);
  Dict.Add('&doteq;', #$2250);
  Dict.Add('&doteqdot;', #$2251);
  Dict.Add('&dotminus;', #$2238);
  Dict.Add('&dotplus;', #$2214);
  Dict.Add('&dotsquare;', #$22A1);
  Dict.Add('&doublebarwedge;', #$2306);
  Dict.Add('&downarrow;', #$2193);
  Dict.Add('&downdownarrows;', #$21CA);
  Dict.Add('&downharpoonleft;', #$21C3);
  Dict.Add('&downharpoonright;', #$21C2);
  Dict.Add('&drbkarow;', #$2910);
  Dict.Add('&drcorn;', #$231F);
  Dict.Add('&drcrop;', #$230C);
  Dict.Add('&dscr;', #$D835#$DCB9);
  Dict.Add('&dscy;', #$0455);
  Dict.Add('&dsol;', #$29F6);
  Dict.Add('&dstrok;', #$0111);
  Dict.Add('&dtdot;', #$22F1);
  Dict.Add('&dtri;', #$25BF);
  Dict.Add('&dtrif;', #$25BE);
  Dict.Add('&duarr;', #$21F5);
  Dict.Add('&duhar;', #$296F);
  Dict.Add('&dwangle;', #$29A6);
  Dict.Add('&dzcy;', #$045F);
  Dict.Add('&dzigrarr;', #$27FF);
  Dict.Add('&eDDot;', #$2A77);
  Dict.Add('&eDot;', #$2251);
  Dict.Add('&eacute;', #$00E9);
  Dict.Add('&easter;', #$2A6E);
  Dict.Add('&ecaron;', #$011B);
  Dict.Add('&ecir;', #$2256);
  Dict.Add('&ecirc;', #$00EA);
  Dict.Add('&ecolon;', #$2255);
  Dict.Add('&ecy;', #$044D);
  Dict.Add('&edot;', #$0117);
  Dict.Add('&ee;', #$2147);
  Dict.Add('&efDot;', #$2252);
  Dict.Add('&efr;', #$D835#$DD22);
  Dict.Add('&eg;', #$2A9A);
  Dict.Add('&egrave;', #$00E8);
  Dict.Add('&egs;', #$2A96);
  Dict.Add('&egsdot;', #$2A98);
  Dict.Add('&el;', #$2A99);
  Dict.Add('&elinters;', #$23E7);
  Dict.Add('&ell;', #$2113);
  Dict.Add('&els;', #$2A95);
  Dict.Add('&elsdot;', #$2A97);
  Dict.Add('&emacr;', #$0113);
  Dict.Add('&empty;', #$2205);
  Dict.Add('&emptyset;', #$2205);
  Dict.Add('&emptyv;', #$2205);
  Dict.Add('&emsp13;', #$2004);
  Dict.Add('&emsp14;', #$2005);
  Dict.Add('&emsp;', #$2003);
  Dict.Add('&eng;', #$014B);
  Dict.Add('&ensp;', #$2002);
  Dict.Add('&eogon;', #$0119);
  Dict.Add('&eopf;', #$D835#$DD56);
  Dict.Add('&epar;', #$22D5);
  Dict.Add('&eparsl;', #$29E3);
  Dict.Add('&eplus;', #$2A71);
  Dict.Add('&epsi;', #$03B5);
  Dict.Add('&epsilon;', #$03B5);
  Dict.Add('&epsiv;', #$03F5);
  Dict.Add('&eqcirc;', #$2256);
  Dict.Add('&eqcolon;', #$2255);
  Dict.Add('&eqsim;', #$2242);
  Dict.Add('&eqslantgtr;', #$2A96);
  Dict.Add('&eqslantless;', #$2A95);
  Dict.Add('&equals;', #$003D);
  Dict.Add('&equest;', #$225F);
  Dict.Add('&equiv;', #$2261);
  Dict.Add('&equivDD;', #$2A78);
  Dict.Add('&eqvparsl;', #$29E5);
  Dict.Add('&erDot;', #$2253);
  Dict.Add('&erarr;', #$2971);
  Dict.Add('&escr;', #$212F);
  Dict.Add('&esdot;', #$2250);
  Dict.Add('&esim;', #$2242);
  Dict.Add('&eta;', #$03B7);
  Dict.Add('&eth;', #$00F0);
  Dict.Add('&euml;', #$00EB);
  Dict.Add('&euro;', #$20AC);
  Dict.Add('&excl;', #$0021);
  Dict.Add('&exist;', #$2203);
  Dict.Add('&expectation;', #$2130);
  Dict.Add('&exponentiale;', #$2147);
  Dict.Add('&fallingdotseq;', #$2252);
  Dict.Add('&fcy;', #$0444);
  Dict.Add('&female;', #$2640);
  Dict.Add('&ffilig;', #$FB03);
  Dict.Add('&fflig;', #$FB00);
  Dict.Add('&ffllig;', #$FB04);
  Dict.Add('&ffr;', #$D835#$DD23);
  Dict.Add('&filig;', #$FB01);
  Dict.Add('&fjlig;', #$0066#$006A);
  Dict.Add('&flat;', #$266D);
  Dict.Add('&fllig;', #$FB02);
  Dict.Add('&fltns;', #$25B1);
  Dict.Add('&fnof;', #$0192);
  Dict.Add('&fopf;', #$D835#$DD57);
  Dict.Add('&forall;', #$2200);
  Dict.Add('&fork;', #$22D4);
  Dict.Add('&forkv;', #$2AD9);
  Dict.Add('&fpartint;', #$2A0D);
  Dict.Add('&frac12;', #$00BD);
  Dict.Add('&frac13;', #$2153);
  Dict.Add('&frac14;', #$00BC);
  Dict.Add('&frac15;', #$2155);
  Dict.Add('&frac16;', #$2159);
  Dict.Add('&frac18;', #$215B);
  Dict.Add('&frac23;', #$2154);
  Dict.Add('&frac25;', #$2156);
  Dict.Add('&frac34;', #$00BE);
  Dict.Add('&frac35;', #$2157);
  Dict.Add('&frac38;', #$215C);
  Dict.Add('&frac45;', #$2158);
  Dict.Add('&frac56;', #$215A);
  Dict.Add('&frac58;', #$215D);
  Dict.Add('&frac78;', #$215E);
  Dict.Add('&frasl;', #$2044);
  Dict.Add('&frown;', #$2322);
  Dict.Add('&fscr;', #$D835#$DCBB);
  Dict.Add('&gE;', #$2267);
  Dict.Add('&gEl;', #$2A8C);
  Dict.Add('&gacute;', #$01F5);
  Dict.Add('&gamma;', #$03B3);
  Dict.Add('&gammad;', #$03DD);
  Dict.Add('&gap;', #$2A86);
  Dict.Add('&gbreve;', #$011F);
  Dict.Add('&gcirc;', #$011D);
  Dict.Add('&gcy;', #$0433);
  Dict.Add('&gdot;', #$0121);
  Dict.Add('&ge;', #$2265);
  Dict.Add('&gel;', #$22DB);
  Dict.Add('&geq;', #$2265);
  Dict.Add('&geqq;', #$2267);
  Dict.Add('&geqslant;', #$2A7E);
  Dict.Add('&ges;', #$2A7E);
  Dict.Add('&gescc;', #$2AA9);
  Dict.Add('&gesdot;', #$2A80);
  Dict.Add('&gesdoto;', #$2A82);
  Dict.Add('&gesdotol;', #$2A84);
  Dict.Add('&gesl;', #$22DB#$FE00);
  Dict.Add('&gesles;', #$2A94);
  Dict.Add('&gfr;', #$D835#$DD24);
  Dict.Add('&gg;', #$226B);
  Dict.Add('&ggg;', #$22D9);
  Dict.Add('&gimel;', #$2137);
  Dict.Add('&gjcy;', #$0453);
  Dict.Add('&gl;', #$2277);
  Dict.Add('&glE;', #$2A92);
  Dict.Add('&gla;', #$2AA5);
  Dict.Add('&glj;', #$2AA4);
  Dict.Add('&gnE;', #$2269);
  Dict.Add('&gnap;', #$2A8A);
  Dict.Add('&gnapprox;', #$2A8A);
  Dict.Add('&gne;', #$2A88);
  Dict.Add('&gneq;', #$2A88);
  Dict.Add('&gneqq;', #$2269);
  Dict.Add('&gnsim;', #$22E7);
  Dict.Add('&gopf;', #$D835#$DD58);
  Dict.Add('&grave;', #$0060);
  Dict.Add('&gscr;', #$210A);
  Dict.Add('&gsim;', #$2273);
  Dict.Add('&gsime;', #$2A8E);
  Dict.Add('&gsiml;', #$2A90);
  Dict.Add('&gt;', #$003E);
  Dict.Add('&gtcc;', #$2AA7);
  Dict.Add('&gtcir;', #$2A7A);
  Dict.Add('&gtdot;', #$22D7);
  Dict.Add('&gtlPar;', #$2995);
  Dict.Add('&gtquest;', #$2A7C);
  Dict.Add('&gtrapprox;', #$2A86);
  Dict.Add('&gtrarr;', #$2978);
  Dict.Add('&gtrdot;', #$22D7);
  Dict.Add('&gtreqless;', #$22DB);
  Dict.Add('&gtreqqless;', #$2A8C);
  Dict.Add('&gtrless;', #$2277);
  Dict.Add('&gtrsim;', #$2273);
  Dict.Add('&gvertneqq;', #$2269#$FE00);
  Dict.Add('&gvnE;', #$2269#$FE00);
  Dict.Add('&hArr;', #$21D4);
  Dict.Add('&hairsp;', #$200A);
  Dict.Add('&half;', #$00BD);
  Dict.Add('&hamilt;', #$210B);
  Dict.Add('&hardcy;', #$044A);
  Dict.Add('&harr;', #$2194);
  Dict.Add('&harrcir;', #$2948);
  Dict.Add('&harrw;', #$21AD);
  Dict.Add('&hbar;', #$210F);
  Dict.Add('&hcirc;', #$0125);
  Dict.Add('&hearts;', #$2665);
  Dict.Add('&heartsuit;', #$2665);
  Dict.Add('&hellip;', #$2026);
  Dict.Add('&hercon;', #$22B9);
  Dict.Add('&hfr;', #$D835#$DD25);
  Dict.Add('&hksearow;', #$2925);
  Dict.Add('&hkswarow;', #$2926);
  Dict.Add('&hoarr;', #$21FF);
  Dict.Add('&homtht;', #$223B);
  Dict.Add('&hookleftarrow;', #$21A9);
  Dict.Add('&hookrightarrow;', #$21AA);
  Dict.Add('&hopf;', #$D835#$DD59);
  Dict.Add('&horbar;', #$2015);
  Dict.Add('&hscr;', #$D835#$DCBD);
  Dict.Add('&hslash;', #$210F);
  Dict.Add('&hstrok;', #$0127);
  Dict.Add('&hybull;', #$2043);
  Dict.Add('&hyphen;', #$2010);
  Dict.Add('&iacute;', #$00ED);
  Dict.Add('&ic;', #$2063);
  Dict.Add('&icirc;', #$00EE);
  Dict.Add('&icy;', #$0438);
  Dict.Add('&iecy;', #$0435);
  Dict.Add('&iexcl;', #$00A1);
  Dict.Add('&iff;', #$21D4);
  Dict.Add('&ifr;', #$D835#$DD26);
  Dict.Add('&igrave;', #$00EC);
  Dict.Add('&ii;', #$2148);
  Dict.Add('&iiiint;', #$2A0C);
  Dict.Add('&iiint;', #$222D);
  Dict.Add('&iinfin;', #$29DC);
  Dict.Add('&iiota;', #$2129);
  Dict.Add('&ijlig;', #$0133);
  Dict.Add('&imacr;', #$012B);
  Dict.Add('&image;', #$2111);
  Dict.Add('&imagline;', #$2110);
  Dict.Add('&imagpart;', #$2111);
  Dict.Add('&imath;', #$0131);
  Dict.Add('&imof;', #$22B7);
  Dict.Add('&imped;', #$01B5);
  Dict.Add('&in;', #$2208);
  Dict.Add('&incare;', #$2105);
  Dict.Add('&infin;', #$221E);
  Dict.Add('&infintie;', #$29DD);
  Dict.Add('&inodot;', #$0131);
  Dict.Add('&int;', #$222B);
  Dict.Add('&intcal;', #$22BA);
  Dict.Add('&integers;', #$2124);
  Dict.Add('&intercal;', #$22BA);
  Dict.Add('&intlarhk;', #$2A17);
  Dict.Add('&intprod;', #$2A3C);
  Dict.Add('&iocy;', #$0451);
  Dict.Add('&iogon;', #$012F);
  Dict.Add('&iopf;', #$D835#$DD5A);
  Dict.Add('&iota;', #$03B9);
  Dict.Add('&iprod;', #$2A3C);
  Dict.Add('&iquest;', #$00BF);
  Dict.Add('&iscr;', #$D835#$DCBE);
  Dict.Add('&isin;', #$2208);
  Dict.Add('&isinE;', #$22F9);
  Dict.Add('&isindot;', #$22F5);
  Dict.Add('&isins;', #$22F4);
  Dict.Add('&isinsv;', #$22F3);
  Dict.Add('&isinv;', #$2208);
  Dict.Add('&it;', #$2062);
  Dict.Add('&itilde;', #$0129);
  Dict.Add('&iukcy;', #$0456);
  Dict.Add('&iuml;', #$00EF);
  Dict.Add('&jcirc;', #$0135);
  Dict.Add('&jcy;', #$0439);
  Dict.Add('&jfr;', #$D835#$DD27);
  Dict.Add('&jmath;', #$0237);
  Dict.Add('&jopf;', #$D835#$DD5B);
  Dict.Add('&jscr;', #$D835#$DCBF);
  Dict.Add('&jsercy;', #$0458);
  Dict.Add('&jukcy;', #$0454);
  Dict.Add('&kappa;', #$03BA);
  Dict.Add('&kappav;', #$03F0);
  Dict.Add('&kcedil;', #$0137);
  Dict.Add('&kcy;', #$043A);
  Dict.Add('&kfr;', #$D835#$DD28);
  Dict.Add('&kgreen;', #$0138);
  Dict.Add('&khcy;', #$0445);
  Dict.Add('&kjcy;', #$045C);
  Dict.Add('&kopf;', #$D835#$DD5C);
  Dict.Add('&kscr;', #$D835#$DCC0);
  Dict.Add('&lAarr;', #$21DA);
  Dict.Add('&lArr;', #$21D0);
  Dict.Add('&lAtail;', #$291B);
  Dict.Add('&lBarr;', #$290E);
  Dict.Add('&lE;', #$2266);
  Dict.Add('&lEg;', #$2A8B);
  Dict.Add('&lHar;', #$2962);
  Dict.Add('&lacute;', #$013A);
  Dict.Add('&laemptyv;', #$29B4);
  Dict.Add('&lagran;', #$2112);
  Dict.Add('&lambda;', #$03BB);
  Dict.Add('&lang;', #$27E8);
  Dict.Add('&langd;', #$2991);
  Dict.Add('&langle;', #$27E8);
  Dict.Add('&lap;', #$2A85);
  Dict.Add('&laquo;', #$00AB);
  Dict.Add('&larr;', #$2190);
  Dict.Add('&larrb;', #$21E4);
  Dict.Add('&larrbfs;', #$291F);
  Dict.Add('&larrfs;', #$291D);
  Dict.Add('&larrhk;', #$21A9);
  Dict.Add('&larrlp;', #$21AB);
  Dict.Add('&larrpl;', #$2939);
  Dict.Add('&larrsim;', #$2973);
  Dict.Add('&larrtl;', #$21A2);
  Dict.Add('&lat;', #$2AAB);
  Dict.Add('&latail;', #$2919);
  Dict.Add('&late;', #$2AAD);
  Dict.Add('&lates;', #$2AAD#$FE00);
  Dict.Add('&lbarr;', #$290C);
  Dict.Add('&lbbrk;', #$2772);
  Dict.Add('&lbrace;', #$007B);
  Dict.Add('&lbrack;', #$005B);
  Dict.Add('&lbrke;', #$298B);
  Dict.Add('&lbrksld;', #$298F);
  Dict.Add('&lbrkslu;', #$298D);
  Dict.Add('&lcaron;', #$013E);
  Dict.Add('&lcedil;', #$013C);
  Dict.Add('&lceil;', #$2308);
  Dict.Add('&lcub;', #$007B);
  Dict.Add('&lcy;', #$043B);
  Dict.Add('&ldca;', #$2936);
  Dict.Add('&ldquo;', #$201C);
  Dict.Add('&ldquor;', #$201E);
  Dict.Add('&ldrdhar;', #$2967);
  Dict.Add('&ldrushar;', #$294B);
  Dict.Add('&ldsh;', #$21B2);
  Dict.Add('&le;', #$2264);
  Dict.Add('&leftarrow;', #$2190);
  Dict.Add('&leftarrowtail;', #$21A2);
  Dict.Add('&leftharpoondown;', #$21BD);
  Dict.Add('&leftharpoonup;', #$21BC);
  Dict.Add('&leftleftarrows;', #$21C7);
  Dict.Add('&leftrightarrow;', #$2194);
  Dict.Add('&leftrightarrows;', #$21C6);
  Dict.Add('&leftrightharpoons;', #$21CB);
  Dict.Add('&leftrightsquigarrow;', #$21AD);
  Dict.Add('&leftthreetimes;', #$22CB);
  Dict.Add('&leg;', #$22DA);
  Dict.Add('&leq;', #$2264);
  Dict.Add('&leqq;', #$2266);
  Dict.Add('&leqslant;', #$2A7D);
  Dict.Add('&les;', #$2A7D);
  Dict.Add('&lescc;', #$2AA8);
  Dict.Add('&lesdot;', #$2A7F);
  Dict.Add('&lesdoto;', #$2A81);
  Dict.Add('&lesdotor;', #$2A83);
  Dict.Add('&lesg;', #$22DA#$FE00);
  Dict.Add('&lesges;', #$2A93);
  Dict.Add('&lessapprox;', #$2A85);
  Dict.Add('&lessdot;', #$22D6);
  Dict.Add('&lesseqgtr;', #$22DA);
  Dict.Add('&lesseqqgtr;', #$2A8B);
  Dict.Add('&lessgtr;', #$2276);
  Dict.Add('&lesssim;', #$2272);
  Dict.Add('&lfisht;', #$297C);
  Dict.Add('&lfloor;', #$230A);
  Dict.Add('&lfr;', #$D835#$DD29);
  Dict.Add('&lg;', #$2276);
  Dict.Add('&lgE;', #$2A91);
  Dict.Add('&lhard;', #$21BD);
  Dict.Add('&lharu;', #$21BC);
  Dict.Add('&lharul;', #$296A);
  Dict.Add('&lhblk;', #$2584);
  Dict.Add('&ljcy;', #$0459);
  Dict.Add('&ll;', #$226A);
  Dict.Add('&llarr;', #$21C7);
  Dict.Add('&llcorner;', #$231E);
  Dict.Add('&llhard;', #$296B);
  Dict.Add('&lltri;', #$25FA);
  Dict.Add('&lmidot;', #$0140);
  Dict.Add('&lmoust;', #$23B0);
  Dict.Add('&lmoustache;', #$23B0);
  Dict.Add('&lnE;', #$2268);
  Dict.Add('&lnap;', #$2A89);
  Dict.Add('&lnapprox;', #$2A89);
  Dict.Add('&lne;', #$2A87);
  Dict.Add('&lneq;', #$2A87);
  Dict.Add('&lneqq;', #$2268);
  Dict.Add('&lnsim;', #$22E6);
  Dict.Add('&loang;', #$27EC);
  Dict.Add('&loarr;', #$21FD);
  Dict.Add('&lobrk;', #$27E6);
  Dict.Add('&longleftarrow;', #$27F5);
  Dict.Add('&longleftrightarrow;', #$27F7);
  Dict.Add('&longmapsto;', #$27FC);
  Dict.Add('&longrightarrow;', #$27F6);
  Dict.Add('&looparrowleft;', #$21AB);
  Dict.Add('&looparrowright;', #$21AC);
  Dict.Add('&lopar;', #$2985);
  Dict.Add('&lopf;', #$D835#$DD5D);
  Dict.Add('&loplus;', #$2A2D);
  Dict.Add('&lotimes;', #$2A34);
  Dict.Add('&lowast;', #$2217);
  Dict.Add('&lowbar;', #$005F);
  Dict.Add('&loz;', #$25CA);
  Dict.Add('&lozenge;', #$25CA);
  Dict.Add('&lozf;', #$29EB);
  Dict.Add('&lpar;', #$0028);
  Dict.Add('&lparlt;', #$2993);
  Dict.Add('&lrarr;', #$21C6);
  Dict.Add('&lrcorner;', #$231F);
  Dict.Add('&lrhar;', #$21CB);
  Dict.Add('&lrhard;', #$296D);
  Dict.Add('&lrm;', #$200E);
  Dict.Add('&lrtri;', #$22BF);
  Dict.Add('&lsaquo;', #$2039);
  Dict.Add('&lscr;', #$D835#$DCC1);
  Dict.Add('&lsh;', #$21B0);
  Dict.Add('&lsim;', #$2272);
  Dict.Add('&lsime;', #$2A8D);
  Dict.Add('&lsimg;', #$2A8F);
  Dict.Add('&lsqb;', #$005B);
  Dict.Add('&lsquo;', #$2018);
  Dict.Add('&lsquor;', #$201A);
  Dict.Add('&lstrok;', #$0142);
  Dict.Add('&lt;', #$003C);
  Dict.Add('&ltcc;', #$2AA6);
  Dict.Add('&ltcir;', #$2A79);
  Dict.Add('&ltdot;', #$22D6);
  Dict.Add('&lthree;', #$22CB);
  Dict.Add('&ltimes;', #$22C9);
  Dict.Add('&ltlarr;', #$2976);
  Dict.Add('&ltquest;', #$2A7B);
  Dict.Add('&ltrPar;', #$2996);
  Dict.Add('&ltri;', #$25C3);
  Dict.Add('&ltrie;', #$22B4);
  Dict.Add('&ltrif;', #$25C2);
  Dict.Add('&lurdshar;', #$294A);
  Dict.Add('&luruhar;', #$2966);
  Dict.Add('&lvertneqq;', #$2268#$FE00);
  Dict.Add('&lvnE;', #$2268#$FE00);
  Dict.Add('&mDDot;', #$223A);
  Dict.Add('&macr;', #$00AF);
  Dict.Add('&male;', #$2642);
  Dict.Add('&malt;', #$2720);
  Dict.Add('&maltese;', #$2720);
  Dict.Add('&map;', #$21A6);
  Dict.Add('&mapsto;', #$21A6);
  Dict.Add('&mapstodown;', #$21A7);
  Dict.Add('&mapstoleft;', #$21A4);
  Dict.Add('&mapstoup;', #$21A5);
  Dict.Add('&marker;', #$25AE);
  Dict.Add('&mcomma;', #$2A29);
  Dict.Add('&mcy;', #$043C);
  Dict.Add('&mdash;', #$2014);
  Dict.Add('&measuredangle;', #$2221);
  Dict.Add('&mfr;', #$D835#$DD2A);
  Dict.Add('&mho;', #$2127);
  Dict.Add('&micro;', #$00B5);
  Dict.Add('&mid;', #$2223);
  Dict.Add('&midast;', #$002A);
  Dict.Add('&midcir;', #$2AF0);
  Dict.Add('&middot;', #$00B7);
  Dict.Add('&minus;', #$2212);
  Dict.Add('&minusb;', #$229F);
  Dict.Add('&minusd;', #$2238);
  Dict.Add('&minusdu;', #$2A2A);
  Dict.Add('&mlcp;', #$2ADB);
  Dict.Add('&mldr;', #$2026);
  Dict.Add('&mnplus;', #$2213);
  Dict.Add('&models;', #$22A7);
  Dict.Add('&mopf;', #$D835#$DD5E);
  Dict.Add('&mp;', #$2213);
  Dict.Add('&mscr;', #$D835#$DCC2);
  Dict.Add('&mstpos;', #$223E);
  Dict.Add('&mu;', #$03BC);
  Dict.Add('&multimap;', #$22B8);
  Dict.Add('&mumap;', #$22B8);
  Dict.Add('&nGg;', #$22D9#$0338);
  Dict.Add('&nGt;', #$226B#$20D2);
  Dict.Add('&nGtv;', #$226B#$0338);
  Dict.Add('&nLeftarrow;', #$21CD);
  Dict.Add('&nLeftrightarrow;', #$21CE);
  Dict.Add('&nLl;', #$22D8#$0338);
  Dict.Add('&nLt;', #$226A#$20D2);
  Dict.Add('&nLtv;', #$226A#$0338);
  Dict.Add('&nRightarrow;', #$21CF);
  Dict.Add('&nVDash;', #$22AF);
  Dict.Add('&nVdash;', #$22AE);
  Dict.Add('&nabla;', #$2207);
  Dict.Add('&nacute;', #$0144);
  Dict.Add('&nang;', #$2220#$20D2);
  Dict.Add('&nap;', #$2249);
  Dict.Add('&napE;', #$2A70#$0338);
  Dict.Add('&napid;', #$224B#$0338);
  Dict.Add('&napos;', #$0149);
  Dict.Add('&napprox;', #$2249);
  Dict.Add('&natur;', #$266E);
  Dict.Add('&natural;', #$266E);
  Dict.Add('&naturals;', #$2115);
  Dict.Add('&nbsp;', #$00A0);
  Dict.Add('&nbump;', #$224E#$0338);
  Dict.Add('&nbumpe;', #$224F#$0338);
  Dict.Add('&ncap;', #$2A43);
  Dict.Add('&ncaron;', #$0148);
  Dict.Add('&ncedil;', #$0146);
  Dict.Add('&ncong;', #$2247);
  Dict.Add('&ncongdot;', #$2A6D#$0338);
  Dict.Add('&ncup;', #$2A42);
  Dict.Add('&ncy;', #$043D);
  Dict.Add('&ndash;', #$2013);
  Dict.Add('&ne;', #$2260);
  Dict.Add('&neArr;', #$21D7);
  Dict.Add('&nearhk;', #$2924);
  Dict.Add('&nearr;', #$2197);
  Dict.Add('&nearrow;', #$2197);
  Dict.Add('&nedot;', #$2250#$0338);
  Dict.Add('&nequiv;', #$2262);
  Dict.Add('&nesear;', #$2928);
  Dict.Add('&nesim;', #$2242#$0338);
  Dict.Add('&nexist;', #$2204);
  Dict.Add('&nexists;', #$2204);
  Dict.Add('&nfr;', #$D835#$DD2B);
  Dict.Add('&ngE;', #$2267#$0338);
  Dict.Add('&nge;', #$2271);
  Dict.Add('&ngeq;', #$2271);
  Dict.Add('&ngeqq;', #$2267#$0338);
  Dict.Add('&ngeqslant;', #$2A7E#$0338);
  Dict.Add('&nges;', #$2A7E#$0338);
  Dict.Add('&ngsim;', #$2275);
  Dict.Add('&ngt;', #$226F);
  Dict.Add('&ngtr;', #$226F);
  Dict.Add('&nhArr;', #$21CE);
  Dict.Add('&nharr;', #$21AE);
  Dict.Add('&nhpar;', #$2AF2);
  Dict.Add('&ni;', #$220B);
  Dict.Add('&nis;', #$22FC);
  Dict.Add('&nisd;', #$22FA);
  Dict.Add('&niv;', #$220B);
  Dict.Add('&njcy;', #$045A);
  Dict.Add('&nlArr;', #$21CD);
  Dict.Add('&nlE;', #$2266#$0338);
  Dict.Add('&nlarr;', #$219A);
  Dict.Add('&nldr;', #$2025);
  Dict.Add('&nle;', #$2270);
  Dict.Add('&nleftarrow;', #$219A);
  Dict.Add('&nleftrightarrow;', #$21AE);
  Dict.Add('&nleq;', #$2270);
  Dict.Add('&nleqq;', #$2266#$0338);
  Dict.Add('&nleqslant;', #$2A7D#$0338);
  Dict.Add('&nles;', #$2A7D#$0338);
  Dict.Add('&nless;', #$226E);
  Dict.Add('&nlsim;', #$2274);
  Dict.Add('&nlt;', #$226E);
  Dict.Add('&nltri;', #$22EA);
  Dict.Add('&nltrie;', #$22EC);
  Dict.Add('&nmid;', #$2224);
  Dict.Add('&nopf;', #$D835#$DD5F);
  Dict.Add('&not;', #$00AC);
  Dict.Add('&notin;', #$2209);
  Dict.Add('&notinE;', #$22F9#$0338);
  Dict.Add('&notindot;', #$22F5#$0338);
  Dict.Add('&notinva;', #$2209);
  Dict.Add('&notinvb;', #$22F7);
  Dict.Add('&notinvc;', #$22F6);
  Dict.Add('&notni;', #$220C);
  Dict.Add('&notniva;', #$220C);
  Dict.Add('&notnivb;', #$22FE);
  Dict.Add('&notnivc;', #$22FD);
  Dict.Add('&npar;', #$2226);
  Dict.Add('&nparallel;', #$2226);
  Dict.Add('&nparsl;', #$2AFD#$20E5);
  Dict.Add('&npart;', #$2202#$0338);
  Dict.Add('&npolint;', #$2A14);
  Dict.Add('&npr;', #$2280);
  Dict.Add('&nprcue;', #$22E0);
  Dict.Add('&npre;', #$2AAF#$0338);
  Dict.Add('&nprec;', #$2280);
  Dict.Add('&npreceq;', #$2AAF#$0338);
  Dict.Add('&nrArr;', #$21CF);
  Dict.Add('&nrarr;', #$219B);
  Dict.Add('&nrarrc;', #$2933#$0338);
  Dict.Add('&nrarrw;', #$219D#$0338);
  Dict.Add('&nrightarrow;', #$219B);
  Dict.Add('&nrtri;', #$22EB);
  Dict.Add('&nrtrie;', #$22ED);
  Dict.Add('&nsc;', #$2281);
  Dict.Add('&nsccue;', #$22E1);
  Dict.Add('&nsce;', #$2AB0#$0338);
  Dict.Add('&nscr;', #$D835#$DCC3);
  Dict.Add('&nshortmid;', #$2224);
  Dict.Add('&nshortparallel;', #$2226);
  Dict.Add('&nsim;', #$2241);
  Dict.Add('&nsime;', #$2244);
  Dict.Add('&nsimeq;', #$2244);
  Dict.Add('&nsmid;', #$2224);
  Dict.Add('&nspar;', #$2226);
  Dict.Add('&nsqsube;', #$22E2);
  Dict.Add('&nsqsupe;', #$22E3);
  Dict.Add('&nsub;', #$2284);
  Dict.Add('&nsubE;', #$2AC5#$0338);
  Dict.Add('&nsube;', #$2288);
  Dict.Add('&nsubset;', #$2282#$20D2);
  Dict.Add('&nsubseteq;', #$2288);
  Dict.Add('&nsubseteqq;', #$2AC5#$0338);
  Dict.Add('&nsucc;', #$2281);
  Dict.Add('&nsucceq;', #$2AB0#$0338);
  Dict.Add('&nsup;', #$2285);
  Dict.Add('&nsupE;', #$2AC6#$0338);
  Dict.Add('&nsupe;', #$2289);
  Dict.Add('&nsupset;', #$2283#$20D2);
  Dict.Add('&nsupseteq;', #$2289);
  Dict.Add('&nsupseteqq;', #$2AC6#$0338);
  Dict.Add('&ntgl;', #$2279);
  Dict.Add('&ntilde;', #$00F1);
  Dict.Add('&ntlg;', #$2278);
  Dict.Add('&ntriangleleft;', #$22EA);
  Dict.Add('&ntrianglelefteq;', #$22EC);
  Dict.Add('&ntriangleright;', #$22EB);
  Dict.Add('&ntrianglerighteq;', #$22ED);
  Dict.Add('&nu;', #$03BD);
  Dict.Add('&num;', #$0023);
  Dict.Add('&numero;', #$2116);
  Dict.Add('&numsp;', #$2007);
  Dict.Add('&nvDash;', #$22AD);
  Dict.Add('&nvHarr;', #$2904);
  Dict.Add('&nvap;', #$224D#$20D2);
  Dict.Add('&nvdash;', #$22AC);
  Dict.Add('&nvge;', #$2265#$20D2);
  Dict.Add('&nvgt;', #$003E#$20D2);
  Dict.Add('&nvinfin;', #$29DE);
  Dict.Add('&nvlArr;', #$2902);
  Dict.Add('&nvle;', #$2264#$20D2);
  Dict.Add('&nvlt;', #$003C#$20D2);
  Dict.Add('&nvltrie;', #$22B4#$20D2);
  Dict.Add('&nvrArr;', #$2903);
  Dict.Add('&nvrtrie;', #$22B5#$20D2);
  Dict.Add('&nvsim;', #$223C#$20D2);
  Dict.Add('&nwArr;', #$21D6);
  Dict.Add('&nwarhk;', #$2923);
  Dict.Add('&nwarr;', #$2196);
  Dict.Add('&nwarrow;', #$2196);
  Dict.Add('&nwnear;', #$2927);
  Dict.Add('&oS;', #$24C8);
  Dict.Add('&oacute;', #$00F3);
  Dict.Add('&oast;', #$229B);
  Dict.Add('&ocir;', #$229A);
  Dict.Add('&ocirc;', #$00F4);
  Dict.Add('&ocy;', #$043E);
  Dict.Add('&odash;', #$229D);
  Dict.Add('&odblac;', #$0151);
  Dict.Add('&odiv;', #$2A38);
  Dict.Add('&odot;', #$2299);
  Dict.Add('&odsold;', #$29BC);
  Dict.Add('&oelig;', #$0153);
  Dict.Add('&ofcir;', #$29BF);
  Dict.Add('&ofr;', #$D835#$DD2C);
  Dict.Add('&ogon;', #$02DB);
  Dict.Add('&ograve;', #$00F2);
  Dict.Add('&ogt;', #$29C1);
  Dict.Add('&ohbar;', #$29B5);
  Dict.Add('&ohm;', #$03A9);
  Dict.Add('&oint;', #$222E);
  Dict.Add('&olarr;', #$21BA);
  Dict.Add('&olcir;', #$29BE);
  Dict.Add('&olcross;', #$29BB);
  Dict.Add('&oline;', #$203E);
  Dict.Add('&olt;', #$29C0);
  Dict.Add('&omacr;', #$014D);
  Dict.Add('&omega;', #$03C9);
  Dict.Add('&omicron;', #$03BF);
  Dict.Add('&omid;', #$29B6);
  Dict.Add('&ominus;', #$2296);
  Dict.Add('&oopf;', #$D835#$DD60);
  Dict.Add('&opar;', #$29B7);
  Dict.Add('&operp;', #$29B9);
  Dict.Add('&oplus;', #$2295);
  Dict.Add('&or;', #$2228);
  Dict.Add('&orarr;', #$21BB);
  Dict.Add('&ord;', #$2A5D);
  Dict.Add('&order;', #$2134);
  Dict.Add('&orderof;', #$2134);
  Dict.Add('&ordf;', #$00AA);
  Dict.Add('&ordm;', #$00BA);
  Dict.Add('&origof;', #$22B6);
  Dict.Add('&oror;', #$2A56);
  Dict.Add('&orslope;', #$2A57);
  Dict.Add('&orv;', #$2A5B);
  Dict.Add('&oscr;', #$2134);
  Dict.Add('&oslash;', #$00F8);
  Dict.Add('&osol;', #$2298);
  Dict.Add('&otilde;', #$00F5);
  Dict.Add('&otimes;', #$2297);
  Dict.Add('&otimesas;', #$2A36);
  Dict.Add('&ouml;', #$00F6);
  Dict.Add('&ovbar;', #$233D);
  Dict.Add('&par;', #$2225);
  Dict.Add('&para;', #$00B6);
  Dict.Add('&parallel;', #$2225);
  Dict.Add('&parsim;', #$2AF3);
  Dict.Add('&parsl;', #$2AFD);
  Dict.Add('&part;', #$2202);
  Dict.Add('&pcy;', #$043F);
  Dict.Add('&percnt;', #$0025);
  Dict.Add('&period;', #$002E);
  Dict.Add('&permil;', #$2030);
  Dict.Add('&perp;', #$22A5);
  Dict.Add('&pertenk;', #$2031);
  Dict.Add('&pfr;', #$D835#$DD2D);
  Dict.Add('&phi;', #$03C6);
  Dict.Add('&phiv;', #$03D5);
  Dict.Add('&phmmat;', #$2133);
  Dict.Add('&phone;', #$260E);
  Dict.Add('&pi;', #$03C0);
  Dict.Add('&pitchfork;', #$22D4);
  Dict.Add('&piv;', #$03D6);
  Dict.Add('&planck;', #$210F);
  Dict.Add('&planckh;', #$210E);
  Dict.Add('&plankv;', #$210F);
  Dict.Add('&plus;', #$002B);
  Dict.Add('&plusacir;', #$2A23);
  Dict.Add('&plusb;', #$229E);
  Dict.Add('&pluscir;', #$2A22);
  Dict.Add('&plusdo;', #$2214);
  Dict.Add('&plusdu;', #$2A25);
  Dict.Add('&pluse;', #$2A72);
  Dict.Add('&plusmn;', #$00B1);
  Dict.Add('&plussim;', #$2A26);
  Dict.Add('&plustwo;', #$2A27);
  Dict.Add('&pm;', #$00B1);
  Dict.Add('&pointint;', #$2A15);
  Dict.Add('&popf;', #$D835#$DD61);
  Dict.Add('&pound;', #$00A3);
  Dict.Add('&pr;', #$227A);
  Dict.Add('&prE;', #$2AB3);
  Dict.Add('&prap;', #$2AB7);
  Dict.Add('&prcue;', #$227C);
  Dict.Add('&pre;', #$2AAF);
  Dict.Add('&prec;', #$227A);
  Dict.Add('&precapprox;', #$2AB7);
  Dict.Add('&preccurlyeq;', #$227C);
  Dict.Add('&preceq;', #$2AAF);
  Dict.Add('&precnapprox;', #$2AB9);
  Dict.Add('&precneqq;', #$2AB5);
  Dict.Add('&precnsim;', #$22E8);
  Dict.Add('&precsim;', #$227E);
  Dict.Add('&prime;', #$2032);
  Dict.Add('&primes;', #$2119);
  Dict.Add('&prnE;', #$2AB5);
  Dict.Add('&prnap;', #$2AB9);
  Dict.Add('&prnsim;', #$22E8);
  Dict.Add('&prod;', #$220F);
  Dict.Add('&profalar;', #$232E);
  Dict.Add('&profline;', #$2312);
  Dict.Add('&profsurf;', #$2313);
  Dict.Add('&prop;', #$221D);
  Dict.Add('&propto;', #$221D);
  Dict.Add('&prsim;', #$227E);
  Dict.Add('&prurel;', #$22B0);
  Dict.Add('&pscr;', #$D835#$DCC5);
  Dict.Add('&psi;', #$03C8);
  Dict.Add('&puncsp;', #$2008);
  Dict.Add('&qfr;', #$D835#$DD2E);
  Dict.Add('&qint;', #$2A0C);
  Dict.Add('&qopf;', #$D835#$DD62);
  Dict.Add('&qprime;', #$2057);
  Dict.Add('&qscr;', #$D835#$DCC6);
  Dict.Add('&quaternions;', #$210D);
  Dict.Add('&quatint;', #$2A16);
  Dict.Add('&quest;', #$003F);
  Dict.Add('&questeq;', #$225F);
  Dict.Add('&quot;', #$0022);
  Dict.Add('&rAarr;', #$21DB);
  Dict.Add('&rArr;', #$21D2);
  Dict.Add('&rAtail;', #$291C);
  Dict.Add('&rBarr;', #$290F);
  Dict.Add('&rHar;', #$2964);
  Dict.Add('&race;', #$223D#$0331);
  Dict.Add('&racute;', #$0155);
  Dict.Add('&radic;', #$221A);
  Dict.Add('&raemptyv;', #$29B3);
  Dict.Add('&rang;', #$27E9);
  Dict.Add('&rangd;', #$2992);
  Dict.Add('&range;', #$29A5);
  Dict.Add('&rangle;', #$27E9);
  Dict.Add('&raquo;', #$00BB);
  Dict.Add('&rarr;', #$2192);
  Dict.Add('&rarrap;', #$2975);
  Dict.Add('&rarrb;', #$21E5);
  Dict.Add('&rarrbfs;', #$2920);
  Dict.Add('&rarrc;', #$2933);
  Dict.Add('&rarrfs;', #$291E);
  Dict.Add('&rarrhk;', #$21AA);
  Dict.Add('&rarrlp;', #$21AC);
  Dict.Add('&rarrpl;', #$2945);
  Dict.Add('&rarrsim;', #$2974);
  Dict.Add('&rarrtl;', #$21A3);
  Dict.Add('&rarrw;', #$219D);
  Dict.Add('&ratail;', #$291A);
  Dict.Add('&ratio;', #$2236);
  Dict.Add('&rationals;', #$211A);
  Dict.Add('&rbarr;', #$290D);
  Dict.Add('&rbbrk;', #$2773);
  Dict.Add('&rbrace;', #$007D);
  Dict.Add('&rbrack;', #$005D);
  Dict.Add('&rbrke;', #$298C);
  Dict.Add('&rbrksld;', #$298E);
  Dict.Add('&rbrkslu;', #$2990);
  Dict.Add('&rcaron;', #$0159);
  Dict.Add('&rcedil;', #$0157);
  Dict.Add('&rceil;', #$2309);
  Dict.Add('&rcub;', #$007D);
  Dict.Add('&rcy;', #$0440);
  Dict.Add('&rdca;', #$2937);
  Dict.Add('&rdldhar;', #$2969);
  Dict.Add('&rdquo;', #$201D);
  Dict.Add('&rdquor;', #$201D);
  Dict.Add('&rdsh;', #$21B3);
  Dict.Add('&real;', #$211C);
  Dict.Add('&realine;', #$211B);
  Dict.Add('&realpart;', #$211C);
  Dict.Add('&reals;', #$211D);
  Dict.Add('&rect;', #$25AD);
  Dict.Add('&reg;', #$00AE);
  Dict.Add('&rfisht;', #$297D);
  Dict.Add('&rfloor;', #$230B);
  Dict.Add('&rfr;', #$D835#$DD2F);
  Dict.Add('&rhard;', #$21C1);
  Dict.Add('&rharu;', #$21C0);
  Dict.Add('&rharul;', #$296C);
  Dict.Add('&rho;', #$03C1);
  Dict.Add('&rhov;', #$03F1);
  Dict.Add('&rightarrow;', #$2192);
  Dict.Add('&rightarrowtail;', #$21A3);
  Dict.Add('&rightharpoondown;', #$21C1);
  Dict.Add('&rightharpoonup;', #$21C0);
  Dict.Add('&rightleftarrows;', #$21C4);
  Dict.Add('&rightleftharpoons;', #$21CC);
  Dict.Add('&rightrightarrows;', #$21C9);
  Dict.Add('&rightsquigarrow;', #$219D);
  Dict.Add('&rightthreetimes;', #$22CC);
  Dict.Add('&ring;', #$02DA);
  Dict.Add('&risingdotseq;', #$2253);
  Dict.Add('&rlarr;', #$21C4);
  Dict.Add('&rlhar;', #$21CC);
  Dict.Add('&rlm;', #$200F);
  Dict.Add('&rmoust;', #$23B1);
  Dict.Add('&rmoustache;', #$23B1);
  Dict.Add('&rnmid;', #$2AEE);
  Dict.Add('&roang;', #$27ED);
  Dict.Add('&roarr;', #$21FE);
  Dict.Add('&robrk;', #$27E7);
  Dict.Add('&ropar;', #$2986);
  Dict.Add('&ropf;', #$D835#$DD63);
  Dict.Add('&roplus;', #$2A2E);
  Dict.Add('&rotimes;', #$2A35);
  Dict.Add('&rpar;', #$0029);
  Dict.Add('&rpargt;', #$2994);
  Dict.Add('&rppolint;', #$2A12);
  Dict.Add('&rrarr;', #$21C9);
  Dict.Add('&rsaquo;', #$203A);
  Dict.Add('&rscr;', #$D835#$DCC7);
  Dict.Add('&rsh;', #$21B1);
  Dict.Add('&rsqb;', #$005D);
  Dict.Add('&rsquo;', #$2019);
  Dict.Add('&rsquor;', #$2019);
  Dict.Add('&rthree;', #$22CC);
  Dict.Add('&rtimes;', #$22CA);
  Dict.Add('&rtri;', #$25B9);
  Dict.Add('&rtrie;', #$22B5);
  Dict.Add('&rtrif;', #$25B8);
  Dict.Add('&rtriltri;', #$29CE);
  Dict.Add('&ruluhar;', #$2968);
  Dict.Add('&rx;', #$211E);
  Dict.Add('&sacute;', #$015B);
  Dict.Add('&sbquo;', #$201A);
  Dict.Add('&sc;', #$227B);
  Dict.Add('&scE;', #$2AB4);
  Dict.Add('&scap;', #$2AB8);
  Dict.Add('&scaron;', #$0161);
  Dict.Add('&sccue;', #$227D);
  Dict.Add('&sce;', #$2AB0);
  Dict.Add('&scedil;', #$015F);
  Dict.Add('&scirc;', #$015D);
  Dict.Add('&scnE;', #$2AB6);
  Dict.Add('&scnap;', #$2ABA);
  Dict.Add('&scnsim;', #$22E9);
  Dict.Add('&scpolint;', #$2A13);
  Dict.Add('&scsim;', #$227F);
  Dict.Add('&scy;', #$0441);
  Dict.Add('&sdot;', #$22C5);
  Dict.Add('&sdotb;', #$22A1);
  Dict.Add('&sdote;', #$2A66);
  Dict.Add('&seArr;', #$21D8);
  Dict.Add('&searhk;', #$2925);
  Dict.Add('&searr;', #$2198);
  Dict.Add('&searrow;', #$2198);
  Dict.Add('&sect;', #$00A7);
  Dict.Add('&semi;', #$003B);
  Dict.Add('&seswar;', #$2929);
  Dict.Add('&setminus;', #$2216);
  Dict.Add('&setmn;', #$2216);
  Dict.Add('&sext;', #$2736);
  Dict.Add('&sfr;', #$D835#$DD30);
  Dict.Add('&sfrown;', #$2322);
  Dict.Add('&sharp;', #$266F);
  Dict.Add('&shchcy;', #$0449);
  Dict.Add('&shcy;', #$0448);
  Dict.Add('&shortmid;', #$2223);
  Dict.Add('&shortparallel;', #$2225);
  Dict.Add('&shy;', #$00AD);
  Dict.Add('&sigma;', #$03C3);
  Dict.Add('&sigmaf;', #$03C2);
  Dict.Add('&sigmav;', #$03C2);
  Dict.Add('&sim;', #$223C);
  Dict.Add('&simdot;', #$2A6A);
  Dict.Add('&sime;', #$2243);
  Dict.Add('&simeq;', #$2243);
  Dict.Add('&simg;', #$2A9E);
  Dict.Add('&simgE;', #$2AA0);
  Dict.Add('&siml;', #$2A9D);
  Dict.Add('&simlE;', #$2A9F);
  Dict.Add('&simne;', #$2246);
  Dict.Add('&simplus;', #$2A24);
  Dict.Add('&simrarr;', #$2972);
  Dict.Add('&slarr;', #$2190);
  Dict.Add('&smallsetminus;', #$2216);
  Dict.Add('&smashp;', #$2A33);
  Dict.Add('&smeparsl;', #$29E4);
  Dict.Add('&smid;', #$2223);
  Dict.Add('&smile;', #$2323);
  Dict.Add('&smt;', #$2AAA);
  Dict.Add('&smte;', #$2AAC);
  Dict.Add('&smtes;', #$2AAC#$FE00);
  Dict.Add('&softcy;', #$044C);
  Dict.Add('&sol;', #$002F);
  Dict.Add('&solb;', #$29C4);
  Dict.Add('&solbar;', #$233F);
  Dict.Add('&sopf;', #$D835#$DD64);
  Dict.Add('&spades;', #$2660);
  Dict.Add('&spadesuit;', #$2660);
  Dict.Add('&spar;', #$2225);
  Dict.Add('&sqcap;', #$2293);
  Dict.Add('&sqcaps;', #$2293#$FE00);
  Dict.Add('&sqcup;', #$2294);
  Dict.Add('&sqcups;', #$2294#$FE00);
  Dict.Add('&sqsub;', #$228F);
  Dict.Add('&sqsube;', #$2291);
  Dict.Add('&sqsubset;', #$228F);
  Dict.Add('&sqsubseteq;', #$2291);
  Dict.Add('&sqsup;', #$2290);
  Dict.Add('&sqsupe;', #$2292);
  Dict.Add('&sqsupset;', #$2290);
  Dict.Add('&sqsupseteq;', #$2292);
  Dict.Add('&squ;', #$25A1);
  Dict.Add('&square;', #$25A1);
  Dict.Add('&squarf;', #$25AA);
  Dict.Add('&squf;', #$25AA);
  Dict.Add('&srarr;', #$2192);
  Dict.Add('&sscr;', #$D835#$DCC8);
  Dict.Add('&ssetmn;', #$2216);
  Dict.Add('&ssmile;', #$2323);
  Dict.Add('&sstarf;', #$22C6);
  Dict.Add('&star;', #$2606);
  Dict.Add('&starf;', #$2605);
  Dict.Add('&straightepsilon;', #$03F5);
  Dict.Add('&straightphi;', #$03D5);
  Dict.Add('&strns;', #$00AF);
  Dict.Add('&sub;', #$2282);
  Dict.Add('&subE;', #$2AC5);
  Dict.Add('&subdot;', #$2ABD);
  Dict.Add('&sube;', #$2286);
  Dict.Add('&subedot;', #$2AC3);
  Dict.Add('&submult;', #$2AC1);
  Dict.Add('&subnE;', #$2ACB);
  Dict.Add('&subne;', #$228A);
  Dict.Add('&subplus;', #$2ABF);
  Dict.Add('&subrarr;', #$2979);
  Dict.Add('&subset;', #$2282);
  Dict.Add('&subseteq;', #$2286);
  Dict.Add('&subseteqq;', #$2AC5);
  Dict.Add('&subsetneq;', #$228A);
  Dict.Add('&subsetneqq;', #$2ACB);
  Dict.Add('&subsim;', #$2AC7);
  Dict.Add('&subsub;', #$2AD5);
  Dict.Add('&subsup;', #$2AD3);
  Dict.Add('&succ;', #$227B);
  Dict.Add('&succapprox;', #$2AB8);
  Dict.Add('&succcurlyeq;', #$227D);
  Dict.Add('&succeq;', #$2AB0);
  Dict.Add('&succnapprox;', #$2ABA);
  Dict.Add('&succneqq;', #$2AB6);
  Dict.Add('&succnsim;', #$22E9);
  Dict.Add('&succsim;', #$227F);
  Dict.Add('&sum;', #$2211);
  Dict.Add('&sung;', #$266A);
  Dict.Add('&sup1;', #$00B9);
  Dict.Add('&sup2;', #$00B2);
  Dict.Add('&sup3;', #$00B3);
  Dict.Add('&sup;', #$2283);
  Dict.Add('&supE;', #$2AC6);
  Dict.Add('&supdot;', #$2ABE);
  Dict.Add('&supdsub;', #$2AD8);
  Dict.Add('&supe;', #$2287);
  Dict.Add('&supedot;', #$2AC4);
  Dict.Add('&suphsol;', #$27C9);
  Dict.Add('&suphsub;', #$2AD7);
  Dict.Add('&suplarr;', #$297B);
  Dict.Add('&supmult;', #$2AC2);
  Dict.Add('&supnE;', #$2ACC);
  Dict.Add('&supne;', #$228B);
  Dict.Add('&supplus;', #$2AC0);
  Dict.Add('&supset;', #$2283);
  Dict.Add('&supseteq;', #$2287);
  Dict.Add('&supseteqq;', #$2AC6);
  Dict.Add('&supsetneq;', #$228B);
  Dict.Add('&supsetneqq;', #$2ACC);
  Dict.Add('&supsim;', #$2AC8);
  Dict.Add('&supsub;', #$2AD4);
  Dict.Add('&supsup;', #$2AD6);
  Dict.Add('&swArr;', #$21D9);
  Dict.Add('&swarhk;', #$2926);
  Dict.Add('&swarr;', #$2199);
  Dict.Add('&swarrow;', #$2199);
  Dict.Add('&swnwar;', #$292A);
  Dict.Add('&szlig;', #$00DF);
  Dict.Add('&target;', #$2316);
  Dict.Add('&tau;', #$03C4);
  Dict.Add('&tbrk;', #$23B4);
  Dict.Add('&tcaron;', #$0165);
  Dict.Add('&tcedil;', #$0163);
  Dict.Add('&tcy;', #$0442);
  Dict.Add('&tdot;', #$20DB);
  Dict.Add('&telrec;', #$2315);
  Dict.Add('&tfr;', #$D835#$DD31);
  Dict.Add('&there4;', #$2234);
  Dict.Add('&therefore;', #$2234);
  Dict.Add('&theta;', #$03B8);
  Dict.Add('&thetasym;', #$03D1);
  Dict.Add('&thetav;', #$03D1);
  Dict.Add('&thickapprox;', #$2248);
  Dict.Add('&thicksim;', #$223C);
  Dict.Add('&thinsp;', #$2009);
  Dict.Add('&thkap;', #$2248);
  Dict.Add('&thksim;', #$223C);
  Dict.Add('&thorn;', #$00FE);
  Dict.Add('&tilde;', #$02DC);
  Dict.Add('&times;', #$00D7);
  Dict.Add('&timesb;', #$22A0);
  Dict.Add('&timesbar;', #$2A31);
  Dict.Add('&timesd;', #$2A30);
  Dict.Add('&tint;', #$222D);
  Dict.Add('&toea;', #$2928);
  Dict.Add('&top;', #$22A4);
  Dict.Add('&topbot;', #$2336);
  Dict.Add('&topcir;', #$2AF1);
  Dict.Add('&topf;', #$D835#$DD65);
  Dict.Add('&topfork;', #$2ADA);
  Dict.Add('&tosa;', #$2929);
  Dict.Add('&tprime;', #$2034);
  Dict.Add('&trade;', #$2122);
  Dict.Add('&triangle;', #$25B5);
  Dict.Add('&triangledown;', #$25BF);
  Dict.Add('&triangleleft;', #$25C3);
  Dict.Add('&trianglelefteq;', #$22B4);
  Dict.Add('&triangleq;', #$225C);
  Dict.Add('&triangleright;', #$25B9);
  Dict.Add('&trianglerighteq;', #$22B5);
  Dict.Add('&tridot;', #$25EC);
  Dict.Add('&trie;', #$225C);
  Dict.Add('&triminus;', #$2A3A);
  Dict.Add('&triplus;', #$2A39);
  Dict.Add('&trisb;', #$29CD);
  Dict.Add('&tritime;', #$2A3B);
  Dict.Add('&trpezium;', #$23E2);
  Dict.Add('&tscr;', #$D835#$DCC9);
  Dict.Add('&tscy;', #$0446);
  Dict.Add('&tshcy;', #$045B);
  Dict.Add('&tstrok;', #$0167);
  Dict.Add('&twixt;', #$226C);
  Dict.Add('&twoheadleftarrow;', #$219E);
  Dict.Add('&twoheadrightarrow;', #$21A0);
  Dict.Add('&uArr;', #$21D1);
  Dict.Add('&uHar;', #$2963);
  Dict.Add('&uacute;', #$00FA);
  Dict.Add('&uarr;', #$2191);
  Dict.Add('&ubrcy;', #$045E);
  Dict.Add('&ubreve;', #$016D);
  Dict.Add('&ucirc;', #$00FB);
  Dict.Add('&ucy;', #$0443);
  Dict.Add('&udarr;', #$21C5);
  Dict.Add('&udblac;', #$0171);
  Dict.Add('&udhar;', #$296E);
  Dict.Add('&ufisht;', #$297E);
  Dict.Add('&ufr;', #$D835#$DD32);
  Dict.Add('&ugrave;', #$00F9);
  Dict.Add('&uharl;', #$21BF);
  Dict.Add('&uharr;', #$21BE);
  Dict.Add('&uhblk;', #$2580);
  Dict.Add('&ulcorn;', #$231C);
  Dict.Add('&ulcorner;', #$231C);
  Dict.Add('&ulcrop;', #$230F);
  Dict.Add('&ultri;', #$25F8);
  Dict.Add('&umacr;', #$016B);
  Dict.Add('&uml;', #$00A8);
  Dict.Add('&uogon;', #$0173);
  Dict.Add('&uopf;', #$D835#$DD66);
  Dict.Add('&uparrow;', #$2191);
  Dict.Add('&updownarrow;', #$2195);
  Dict.Add('&upharpoonleft;', #$21BF);
  Dict.Add('&upharpoonright;', #$21BE);
  Dict.Add('&uplus;', #$228E);
  Dict.Add('&upsi;', #$03C5);
  Dict.Add('&upsih;', #$03D2);
  Dict.Add('&upsilon;', #$03C5);
  Dict.Add('&upuparrows;', #$21C8);
  Dict.Add('&urcorn;', #$231D);
  Dict.Add('&urcorner;', #$231D);
  Dict.Add('&urcrop;', #$230E);
  Dict.Add('&uring;', #$016F);
  Dict.Add('&urtri;', #$25F9);
  Dict.Add('&uscr;', #$D835#$DCCA);
  Dict.Add('&utdot;', #$22F0);
  Dict.Add('&utilde;', #$0169);
  Dict.Add('&utri;', #$25B5);
  Dict.Add('&utrif;', #$25B4);
  Dict.Add('&uuarr;', #$21C8);
  Dict.Add('&uuml;', #$00FC);
  Dict.Add('&uwangle;', #$29A7);
  Dict.Add('&vArr;', #$21D5);
  Dict.Add('&vBar;', #$2AE8);
  Dict.Add('&vBarv;', #$2AE9);
  Dict.Add('&vDash;', #$22A8);
  Dict.Add('&vangrt;', #$299C);
  Dict.Add('&varepsilon;', #$03F5);
  Dict.Add('&varkappa;', #$03F0);
  Dict.Add('&varnothing;', #$2205);
  Dict.Add('&varphi;', #$03D5);
  Dict.Add('&varpi;', #$03D6);
  Dict.Add('&varpropto;', #$221D);
  Dict.Add('&varr;', #$2195);
  Dict.Add('&varrho;', #$03F1);
  Dict.Add('&varsigma;', #$03C2);
  Dict.Add('&varsubsetneq;', #$228A#$FE00);
  Dict.Add('&varsubsetneqq;', #$2ACB#$FE00);
  Dict.Add('&varsupsetneq;', #$228B#$FE00);
  Dict.Add('&varsupsetneqq;', #$2ACC#$FE00);
  Dict.Add('&vartheta;', #$03D1);
  Dict.Add('&vartriangleleft;', #$22B2);
  Dict.Add('&vartriangleright;', #$22B3);
  Dict.Add('&vcy;', #$0432);
  Dict.Add('&vdash;', #$22A2);
  Dict.Add('&vee;', #$2228);
  Dict.Add('&veebar;', #$22BB);
  Dict.Add('&veeeq;', #$225A);
  Dict.Add('&vellip;', #$22EE);
  Dict.Add('&verbar;', #$007C);
  Dict.Add('&vert;', #$007C);
  Dict.Add('&vfr;', #$D835#$DD33);
  Dict.Add('&vltri;', #$22B2);
  Dict.Add('&vnsub;', #$2282#$20D2);
  Dict.Add('&vnsup;', #$2283#$20D2);
  Dict.Add('&vopf;', #$D835#$DD67);
  Dict.Add('&vprop;', #$221D);
  Dict.Add('&vrtri;', #$22B3);
  Dict.Add('&vscr;', #$D835#$DCCB);
  Dict.Add('&vsubnE;', #$2ACB#$FE00);
  Dict.Add('&vsubne;', #$228A#$FE00);
  Dict.Add('&vsupnE;', #$2ACC#$FE00);
  Dict.Add('&vsupne;', #$228B#$FE00);
  Dict.Add('&vzigzag;', #$299A);
  Dict.Add('&wcirc;', #$0175);
  Dict.Add('&wedbar;', #$2A5F);
  Dict.Add('&wedge;', #$2227);
  Dict.Add('&wedgeq;', #$2259);
  Dict.Add('&weierp;', #$2118);
  Dict.Add('&wfr;', #$D835#$DD34);
  Dict.Add('&wopf;', #$D835#$DD68);
  Dict.Add('&wp;', #$2118);
  Dict.Add('&wr;', #$2240);
  Dict.Add('&wreath;', #$2240);
  Dict.Add('&wscr;', #$D835#$DCCC);
  Dict.Add('&xcap;', #$22C2);
  Dict.Add('&xcirc;', #$25EF);
  Dict.Add('&xcup;', #$22C3);
  Dict.Add('&xdtri;', #$25BD);
  Dict.Add('&xfr;', #$D835#$DD35);
  Dict.Add('&xhArr;', #$27FA);
  Dict.Add('&xharr;', #$27F7);
  Dict.Add('&xi;', #$03BE);
  Dict.Add('&xlArr;', #$27F8);
  Dict.Add('&xlarr;', #$27F5);
  Dict.Add('&xmap;', #$27FC);
  Dict.Add('&xnis;', #$22FB);
  Dict.Add('&xodot;', #$2A00);
  Dict.Add('&xopf;', #$D835#$DD69);
  Dict.Add('&xoplus;', #$2A01);
  Dict.Add('&xotime;', #$2A02);
  Dict.Add('&xrArr;', #$27F9);
  Dict.Add('&xrarr;', #$27F6);
  Dict.Add('&xscr;', #$D835#$DCCD);
  Dict.Add('&xsqcup;', #$2A06);
  Dict.Add('&xuplus;', #$2A04);
  Dict.Add('&xutri;', #$25B3);
  Dict.Add('&xvee;', #$22C1);
  Dict.Add('&xwedge;', #$22C0);
  Dict.Add('&yacute;', #$00FD);
  Dict.Add('&yacy;', #$044F);
  Dict.Add('&ycirc;', #$0177);
  Dict.Add('&ycy;', #$044B);
  Dict.Add('&yen;', #$00A5);
  Dict.Add('&yfr;', #$D835#$DD36);
  Dict.Add('&yicy;', #$0457);
  Dict.Add('&yopf;', #$D835#$DD6A);
  Dict.Add('&yscr;', #$D835#$DCCE);
  Dict.Add('&yucy;', #$044E);
  Dict.Add('&yuml;', #$00FF);
  Dict.Add('&zacute;', #$017A);
  Dict.Add('&zcaron;', #$017E);
  Dict.Add('&zcy;', #$0437);
  Dict.Add('&zdot;', #$017C);
  Dict.Add('&zeetrf;', #$2128);
  Dict.Add('&zeta;', #$03B6);
  Dict.Add('&zfr;', #$D835#$DD37);
  Dict.Add('&zhcy;', #$0436);
  Dict.Add('&zigrarr;', #$21DD);
  Dict.Add('&zopf;', #$D835#$DD6B);
  Dict.Add('&zscr;', #$D835#$DCCF);
  Dict.Add('&zwj;', #$200D);
  Dict.Add('&zwnj;', #$200C);
end;

initialization

RegisterPlaceableHighlighter(TSynMarkdownSyn);

RegexAtxHeading := TRegEx.Create('^ {0,3}#{1,6} .+$', [roCompiled]);
RegexBlockQuote := TRegEx.Create('^ *>.+$', [roCompiled]);
RegexCodeSpan := TRegEx.Create('`[^`]+`', [roCompiled]);
RegexDelete := TRegEx.Create('(~{1,2})[^~]+\1', [roCompiled]);
RegexEmpasis1 := TRegEx.Create('(\*{1,3})[^*]+?\1', [roCompiled]);
RegexEmpasis2 := TRegEx.Create('(\b_{1,3})[^_].*?\1\b', [roCompiled]);
RegexEntityReference := TRegEx.Create
  ('(&([a-zA-Z0-9]+|#[0-9]{1,7}|#[xX][a-fA-F0-9]{1,6});)', [roCompiled]);
RegexFencedCodeBlockBegin :=
  TRegEx.Create('^ {0,3}(([`~])\2{2,})(?: *)([^ ]*)(.*)$', [roCompiled]);
RegexFencedCodeBlockEnd := TRegEx.Create('^ {0,3}(([`~])\2{2,})$',
  [roCompiled]);
RegexIndentedCodeBlock := TRegEx.Create('^( {0,3}\t| {4,}).*', [roCompiled]);
RegexList := TRegEx.Create('^ *([-+*]|[0-9]{1,9}\.)(?= )', [roCompiled]);
RegexUrlLink := TRegEx.Create('https?://[\w!?/+\-_~=;.,*&@#$%()'']+',
  [roCompiled]);
RegexPageLink := TRegEx.Create('\[\[.+?\]\]', [roCompiled]);
RegexSetextHeading := TRegEx.Create('^ {0,3}([=-])\1* *$', [roCompiled]);

EntityReferenceDict := TDictionary<String, String>.Create(2200);
SetEntities(EntityReferenceDict);

finalization

EntityReferenceDict.Free;

end.
