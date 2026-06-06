{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Dockerfile syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterDockerfile;

{$I SynEdit.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynFunc,
  SynUnicode;

type
  TtkTokenKind = (tkComment, tkDirective, tkInstruction, tkFlag, tkString,
    tkVariable, tkNumber, tkIdentifier, tkSymbol, tkSpace, tkNull, tkUnknown);

  // rsCont = the previous line ended with '\', so this line continues it
  TRangeState = (rsUnknown, rsCont);

  TSynDockerfileSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FExpectInstr: Boolean;
    FContSeen: Boolean;
    FTokenID: TtkTokenKind;
    FInstructions: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FDirectiveAttri: TSynHighlighterAttributes;
    FInstructionAttri: TSynHighlighterAttributes;
    FFlagAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    procedure BackslashProc;
    procedure CommentProc;
    procedure CRProc;
    procedure FlagProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure VariableProc;
    procedure WordProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirectiveAttri write FDirectiveAttri;
    property InstructionAttri: TSynHighlighterAttributes read FInstructionAttri write FInstructionAttri;
    property FlagAttri: TSynHighlighterAttributes read FFlagAttri write FFlagAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterDockerfile = 'Dockerfile (Dockerfile*;*.dockerfile)|Dockerfile;Dockerfile.*;*.dockerfile;Containerfile';
  SYNS_LangDockerfile = 'Dockerfile';
  SYNS_FriendlyLangDockerfile = 'Dockerfile';

  Instructions: string =
    'ADD,ARG,CMD,COPY,ENTRYPOINT,ENV,EXPOSE,FROM,HEALTHCHECK,LABEL,MAINTAINER,' +
    'ONBUILD,RUN,SHELL,STOPSIGNAL,USER,VOLUME,WORKDIR';

constructor TSynDockerfileSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInstructions := TStringList.Create;
  FInstructions.CaseSensitive := False;
  FInstructions.Sorted := True;
  FInstructions.Duplicates := dupIgnore;
  FInstructions.CommaText := Instructions;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FDirectiveAttri.Foreground := clTeal;
  FDirectiveAttri.Style := [fsBold];
  AddAttribute(FDirectiveAttri);

  FInstructionAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FInstructionAttri.Foreground := clNavy;
  FInstructionAttri.Style := [fsBold];
  AddAttribute(FInstructionAttri);

  FFlagAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  FFlagAttri.Foreground := clOlive;
  AddAttribute(FFlagAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clMaroon;
  FVariableAttri.Style := [fsBold];
  AddAttribute(FVariableAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterDockerfile;
  FRange := rsUnknown;
  FExpectInstr := True;
  FContSeen := False;
end;

destructor TSynDockerfileSyn.Destroy;
begin
  FInstructions.Free;
  inherited Destroy;
end;

function TSynDockerfileSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

procedure TSynDockerfileSyn.CRProc;
begin
  if (FRange = rsCont) and not FContSeen then
    FRange := rsUnknown;
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynDockerfileSyn.LFProc;
begin
  if (FRange = rsCont) and not FContSeen then
    FRange := rsUnknown;
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynDockerfileSyn.NullProc;
begin
  if (FRange = rsCont) and not FContSeen then
    FRange := rsUnknown;
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynDockerfileSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynDockerfileSyn.CommentProc;
var
  I, P: Integer;
  W: string;
begin
  // A # comment, or a parser directive: # syntax= / # escape= / # check=
  I := Run + 1;
  while CharInSet(FLine[I], [#9, ' ']) do
    Inc(I);
  P := I;
  while CharInSet(FLine[I], ['A'..'Z', 'a'..'z']) do
    Inc(I);
  SetString(W, FLine + P, I - P);
  while CharInSet(FLine[I], [#9, ' ']) do
    Inc(I);
  if (FLine[I] = '=') and (SameText(W, 'syntax') or SameText(W, 'escape') or
    SameText(W, 'check')) then
    FTokenID := tkDirective
  else
    FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynDockerfileSyn.WordProc;
var
  S: string;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  SetString(S, FLine + FTokenPos, Run - FTokenPos);
  if FExpectInstr and (FInstructions.IndexOf(S) >= 0) then
  begin
    FTokenID := tkInstruction;
    // ONBUILD is followed by another instruction
    FExpectInstr := SameText(S, 'ONBUILD');
  end
  else if S = 'AS' then
  begin
    FTokenID := tkInstruction;
    FExpectInstr := False;
  end
  else
  begin
    FTokenID := tkIdentifier;
    FExpectInstr := False;
  end;
end;

procedure TSynDockerfileSyn.NumberProc;
begin
  FTokenID := tkNumber;
  while CharInSet(FLine[Run], ['0'..'9']) do
    Inc(Run);
  if (FLine[Run] = '.') and CharInSet(FLine[Run + 1], ['0'..'9']) then
  begin
    Inc(Run);
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
  end;
end;

procedure TSynDockerfileSyn.VariableProc;
begin
  FTokenID := tkVariable;
  Inc(Run); // '$'
  if FLine[Run] = '{' then
  begin
    Inc(Run);
    while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> '}') do
      Inc(Run);
    if FLine[Run] = '}' then
      Inc(Run);
  end
  else
    while CharInSet(FLine[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(Run);
end;

procedure TSynDockerfileSyn.FlagProc;
begin
  // a --flag option (the value after '=' is tokenised separately)
  FTokenID := tkFlag;
  Inc(Run, 2); // '--'
  while CharInSet(FLine[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) do
    Inc(Run);
end;

procedure TSynDockerfileSyn.StringProc;
var
  Quote: WideChar;
  Escapes: Boolean;
begin
  Quote := FLine[Run];
  Escapes := Quote = '"';
  FTokenID := tkString;
  Inc(Run); // opening quote
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if Escapes and (FLine[Run] = '\') then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = Quote then
    begin
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynDockerfileSyn.BackslashProc;
var
  I: Integer;
begin
  // a '\' at the end of the line continues the instruction onto the next line
  FTokenID := tkSymbol;
  I := Run + 1;
  while CharInSet(FLine[I], [#9, ' ']) do
    Inc(I);
  if CharInSet(FLine[I], [#0, #10, #13]) then
  begin
    FRange := rsCont;
    FContSeen := True;
  end;
  Inc(Run);
end;

procedure TSynDockerfileSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynDockerfileSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynDockerfileSyn.Next;
begin
  FTokenPos := Run;
  if Run = 0 then
  begin
    FExpectInstr := FRange = rsUnknown;
    FContSeen := False;
  end;
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '#': CommentProc;
    '"', '''': StringProc;
    '$':
      if CharInSet(FLine[Run + 1], ['{', 'A'..'Z', 'a'..'z', '_']) then
        VariableProc
      else
        SymbolProc;
    '-':
      if FLine[Run + 1] = '-' then
        FlagProc
      else
        SymbolProc;
    '0'..'9': NumberProc;
    'A'..'Z', 'a'..'z', '_': WordProc;
    '\': BackslashProc;
    '=', ':', ',', ';', '.', '/', '(', ')', '[', ']', '{', '}', '+', '*',
    '@', '&', '|', '<', '>', '!', '?', '~', '%', '^': SymbolProc;
  else
    UnknownProc;
  end;
  inherited;
end;

function TSynDockerfileSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FInstructionAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDockerfileSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynDockerfileSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangDockerfile;
end;

class function TSynDockerfileSyn.GetLanguageName: string;
begin
  Result := SYNS_LangDockerfile;
end;

function TSynDockerfileSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynDockerfileSyn.GetSampleSource: string;
begin
  Result :=
    '# syntax=docker/dockerfile:1'#13#10 +
    '# Build stage'#13#10 +
    'FROM golang:1.22-alpine AS build'#13#10 +
    'WORKDIR /src'#13#10 +
    'COPY go.mod go.sum ./'#13#10 +
    'RUN go mod download'#13#10 +
    'COPY . .'#13#10 +
    'RUN --mount=type=cache,target=/root/.cache/go-build \'#13#10 +
    '    CGO_ENABLED=0 go build -o /bin/app ./cmd/app'#13#10 +
    ''#13#10 +
    '# Final stage'#13#10 +
    'FROM alpine:3.19'#13#10 +
    'RUN apk add --no-cache ca-certificates'#13#10 +
    'COPY --from=build /bin/app /usr/local/bin/app'#13#10 +
    'ENV APP_PORT=8080 \'#13#10 +
    '    APP_ENV=production'#13#10 +
    'EXPOSE 8080'#13#10 +
    'USER nobody'#13#10 +
    'HEALTHCHECK --interval=30s --timeout=3s \'#13#10 +
    '  CMD wget -qO- http://localhost:${APP_PORT}/health || exit 1'#13#10 +
    'ENTRYPOINT ["/usr/local/bin/app"]'#13#10 +
    'CMD ["--config", "/etc/app/config.yaml"]';
end;

function TSynDockerfileSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkDirective: Result := FDirectiveAttri;
    tkInstruction: Result := FInstructionAttri;
    tkFlag: Result := FFlagAttri;
    tkString: Result := FStringAttri;
    tkVariable: Result := FVariableAttri;
    tkNumber: Result := FNumberAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynDockerfileSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynDockerfileSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynDockerfileSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterDockerfile;
end;

procedure TSynDockerfileSyn.ResetRange;
begin
  FRange := rsUnknown;
  FExpectInstr := True;
  FContSeen := False;
end;

procedure TSynDockerfileSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynDockerfileSyn);

end.
