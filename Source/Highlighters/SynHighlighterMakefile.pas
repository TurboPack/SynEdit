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
@abstract(Provides a Makefile syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterMakefile;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkTarget, tkVariable,
    tkNumber, tkNull, tkSpace, tkSymbol, tkUnknown);

  TSynMakefileSyn = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FInTargetPart: Boolean;
    FKeywords: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FTargetAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function IsTargetChar(AChar: WideChar): Boolean;
    function CurrentLineIsRule: Boolean;
    procedure CommentProc;
    procedure CRProc;
    procedure DollarProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure TargetProc;
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
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property TargetAttri: TSynHighlighterAttributes read FTargetAttri write FTargetAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterMakefile = 'Makefiles (*.mak;*.mk;makefile;Makefile;GNUmakefile)|*.mak;*.mk;makefile;Makefile;GNUmakefile';
  SYNS_LangMakefile = 'Makefile';
  SYNS_FriendlyLangMakefile = 'Makefile';

constructor TSynMakefileSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText :=
    'define,else,endef,endif,export,if,ifdef,ifeq,ifndef,ifneq,include,load,' +
    'override,private,sinclude,undefine,unexport,vpath';

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clBlue;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FTargetAttri := TSynHighlighterAttributes.Create(SYNS_AttrLabel, SYNS_FriendlyAttrLabel);
  FTargetAttri.Foreground := clTeal;
  FTargetAttri.Style := [fsBold];
  AddAttribute(FTargetAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clMaroon;
  AddAttribute(FVariableAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  AddAttribute(FSymbolAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  FUnknownAttri.Foreground := clRed;
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterMakefile;
end;

destructor TSynMakefileSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

function TSynMakefileSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynMakefileSyn.IsTargetChar(AChar: WideChar): Boolean;
begin
  Result := CharInSet(AChar,
    ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '%', '/', '+', '-']);
end;

function TSynMakefileSyn.CurrentLineIsRule: Boolean;
var
  I, Depth: Integer;
begin
  Result := False;
  if FLine[0] = #9 then
    Exit;
  I := 0;
  Depth := 0;
  while not CharInSet(FLine[I], [#0, #10, #13]) do
  begin
    if FLine[I] = '$' then
    begin
      if CharInSet(FLine[I + 1], ['(', '{']) then
        Inc(Depth);
      Inc(I, 2);
      Continue;
    end;
    if Depth > 0 then
    begin
      if CharInSet(FLine[I], ['(', '{']) then
        Inc(Depth)
      else if CharInSet(FLine[I], [')', '}']) then
        Dec(Depth);
      Inc(I);
      Continue;
    end;

    case FLine[I] of
      '#':
        Exit;
      '=':
        Exit;
      '?', '+', '!':
        if FLine[I + 1] = '=' then
          Exit;
      ':':
        begin
          if FLine[I + 1] = '=' then
            Exit;
          if (FLine[I + 1] = ':') and (FLine[I + 2] = '=') then
            Exit;
          Result := True;
          Exit;
        end;
    end;
    Inc(I);
  end;
end;

procedure TSynMakefileSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynMakefileSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynMakefileSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynMakefileSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynMakefileSyn.CommentProc;
begin
  FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynMakefileSyn.DollarProc;
var
  Depth: Integer;
begin
  FTokenID := tkVariable;
  Inc(Run);
  case FLine[Run] of
    '$':
      begin
        // '$$' is an escaped dollar (a literal '$' for the shell).
        FTokenID := tkSymbol;
        Inc(Run);
      end;
    '(', '{':
      begin
        Depth := 0;
        while not CharInSet(FLine[Run], [#0, #10, #13]) do
        begin
          if CharInSet(FLine[Run], ['(', '{']) then
            Inc(Depth)
          else if CharInSet(FLine[Run], [')', '}']) then
          begin
            Dec(Depth);
            if Depth <= 0 then
            begin
              Inc(Run);
              Break;
            end;
          end;
          Inc(Run);
        end;
      end;
    #0, #10, #13:
      ; // a lone '$' at end of line
  else
    Inc(Run);
  end;
end;

procedure TSynMakefileSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynMakefileSyn.TargetProc;
begin
  FTokenID := tkTarget;
  while IsTargetChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynMakefileSyn.NumberProc;
begin
  FTokenID := tkNumber;
  while CharInSet(FLine[Run], ['0'..'9', '.']) do
    Inc(Run);
end;

procedure TSynMakefileSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    ':':
      begin
        if (FLine[Run + 1] = ':') and (FLine[Run + 2] = '=') then
          Inc(Run, 2)
        else if FLine[Run + 1] = '=' then
          Inc(Run)
        else
        begin
          if FLine[Run + 1] = ':' then
            Inc(Run);
          FInTargetPart := False;
        end;
      end;
    '?', '+', '!':
      if FLine[Run + 1] = '=' then Inc(Run);
  end;
  Inc(Run);
end;

procedure TSynMakefileSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

function TSynMakefileSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynMakefileSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynMakefileSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangMakefile;
end;

class function TSynMakefileSyn.GetLanguageName: string;
begin
  Result := SYNS_LangMakefile;
end;

function TSynMakefileSyn.GetRange: Pointer;
begin
  Result := nil;
end;

function TSynMakefileSyn.GetSampleSource: string;
begin
  Result :=
    '# Simple Makefile'#13#10 +
    'CC := gcc'#13#10 +
    'CFLAGS ?= -O2 -Wall'#13#10 +
    'SRCS := $(wildcard *.c)'#13#10 +
    'OBJS := $(SRCS:.c=.o)'#13#10 +
    ''#13#10 +
    '.PHONY: all clean'#13#10 +
    ''#13#10 +
    'all: program'#13#10 +
    ''#13#10 +
    'program: $(OBJS)'#13#10 +
    #9'$(CC) $(CFLAGS) -o $@ $^'#13#10 +
    ''#13#10 +
    '%.o: %.c'#13#10 +
    #9'$(CC) $(CFLAGS) -c $< -o $@'#13#10 +
    ''#13#10 +
    'clean:'#13#10 +
    #9'rm -f $(OBJS) program';
end;

function TSynMakefileSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkTarget: Result := FTargetAttri;
    tkVariable: Result := FVariableAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynMakefileSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynMakefileSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynMakefileSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterMakefile;
end;

function TSynMakefileSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

procedure TSynMakefileSyn.Next;
begin
  FTokenPos := Run;

  if FTokenPos = 0 then
    FInTargetPart := CurrentLineIsRule;

  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '#': CommentProc;
    '$': DollarProc;
  else
    if FInTargetPart and IsTargetChar(FLine[Run]) then
      TargetProc
    else
      case FLine[Run] of
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '0'..'9': NumberProc;
        ':', '=', '?', '+', '!', '|', '\', '/', '*', '<', '>', '&', ';', ',',
        '(', ')', '{', '}', '[', ']', '.', '%', '@', '~', '-', '"', '''':
          SymbolProc;
      else
        UnknownProc;
      end;
  end;
  inherited;
end;

procedure TSynMakefileSyn.ResetRange;
begin
  // Stateless: nothing to reset.
end;

procedure TSynMakefileSyn.SetRange(Value: Pointer);
begin
  // Stateless: nothing to restore.
end;

initialization
  RegisterPlaceableHighlighter(TSynMakefileSyn);

end.
