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
@abstract(Provides an HCL / Terraform syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterHCL;

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
  TtkTokenKind = (tkComment, tkBlockType, tkKeyword, tkType, tkFunction,
    tkIdentifier, tkString, tkNumber, tkSymbol, tkSpace, tkNull, tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment, rsHeredoc);

  TSynHCLSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FTypes: TStringList;
    FBlockTypes: TStringList;
    FFunctions: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FBlockTypeAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FTypeAttri: TSynHighlighterAttributes;
    FFunctionAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function NextNonSpaceIsParen: Boolean;
    procedure BlockCommentProc;
    procedure CommentProc;
    procedure CRProc;
    procedure HeredocOpenProc;
    procedure HeredocProc;
    procedure IdentProc;
    procedure LessProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
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
    property BlockTypeAttri: TSynHighlighterAttributes read FBlockTypeAttri write FBlockTypeAttri;
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write FKeywordAttri;
    property TypeAttri: TSynHighlighterAttributes read FTypeAttri write FTypeAttri;
    property FunctionAttri: TSynHighlighterAttributes read FFunctionAttri write FFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterHCL = 'HCL/Terraform Files (*.tf;*.tfvars;*.hcl)|*.tf;*.tfvars;*.hcl';
  SYNS_LangHCL = 'HCL';
  SYNS_FriendlyLangHCL = 'HCL (Terraform)';

  Keywords: string =
    'else,endfor,endif,false,for,if,in,null,true';

  Types: string =
    'any,bool,list,map,number,object,set,string,tuple';

  BlockTypes: string =
    'backend,check,connection,data,dynamic,import,lifecycle,locals,module,' +
    'moved,output,provider,provisioner,removed,resource,terraform,variable';

  Functions: string =
    'abs,abspath,alltrue,anytrue,base64decode,base64encode,base64gzip,' +
    'base64sha256,base64sha512,basename,bcrypt,can,ceil,chomp,chunklist,' +
    'cidrhost,cidrnetmask,cidrsubnet,cidrsubnets,coalesce,coalescelist,' +
    'compact,concat,contains,csvdecode,dirname,distinct,element,endswith,file,' +
    'filebase64,filebase64sha256,fileexists,fileset,flatten,floor,format,' +
    'formatdate,formatlist,indent,index,join,jsondecode,jsonencode,keys,length,' +
    'log,lookup,lower,matchkeys,max,md5,merge,min,nonsensitive,one,parseint,' +
    'pathexpand,plantimestamp,pow,range,regex,regexall,replace,reverse,' +
    'sensitive,setintersection,setproduct,setsubtract,setunion,sha1,sha256,' +
    'sha512,signum,slice,sort,split,startswith,strcontains,strrev,substr,sum,' +
    'templatefile,textdecodebase64,textencodebase64,timeadd,timecmp,timestamp,' +
    'title,tobool,tolist,tomap,tonumber,toset,tostring,transpose,trim,' +
    'trimprefix,trimspace,trimsuffix,try,upper,urlencode,uuid,uuidv5,values,' +
    'yamldecode,yamlencode,zipmap';

constructor TSynHCLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText := Keywords;

  FTypes := TStringList.Create;
  FTypes.CaseSensitive := True;
  FTypes.Sorted := True;
  FTypes.Duplicates := dupIgnore;
  FTypes.CommaText := Types;

  FBlockTypes := TStringList.Create;
  FBlockTypes.CaseSensitive := True;
  FBlockTypes.Sorted := True;
  FBlockTypes.Duplicates := dupIgnore;
  FBlockTypes.CommaText := BlockTypes;

  FFunctions := TStringList.Create;
  FFunctions.CaseSensitive := True;
  FFunctions.Sorted := True;
  FFunctions.Duplicates := dupIgnore;
  FFunctions.CommaText := Functions;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FBlockTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  FBlockTypeAttri.Foreground := clPurple;
  FBlockTypeAttri.Style := [fsBold];
  AddAttribute(FBlockTypeAttri);

  FKeywordAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeywordAttri.Foreground := clNavy;
  FKeywordAttri.Style := [fsBold];
  AddAttribute(FKeywordAttri);

  FTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  FTypeAttri.Foreground := clTeal;
  FTypeAttri.Style := [fsBold];
  AddAttribute(FTypeAttri);

  FFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  FFunctionAttri.Foreground := clOlive;
  AddAttribute(FFunctionAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clMaroon;
  AddAttribute(FNumberAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterHCL;
  FRange := rsUnknown;
end;

destructor TSynHCLSyn.Destroy;
begin
  FFunctions.Free;
  FBlockTypes.Free;
  FTypes.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynHCLSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

function TSynHCLSyn.NextNonSpaceIsParen: Boolean;
var
  I: Integer;
begin
  I := Run;
  while CharInSet(FLine[I], [#9, ' ']) do
    Inc(I);
  Result := FLine[I] = '(';
end;

function TSynHCLSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKeyword
  else if FTypes.IndexOf(S) >= 0 then
    Result := tkType
  else if FBlockTypes.IndexOf(S) >= 0 then
    Result := tkBlockType
  else if (FFunctions.IndexOf(S) >= 0) and NextNonSpaceIsParen then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

procedure TSynHCLSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynHCLSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynHCLSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynHCLSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynHCLSyn.CommentProc;
begin
  // # line comment
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until CharInSet(FLine[Run], [#0, #10, #13]);
end;

procedure TSynHCLSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':
      begin
        FTokenID := tkComment;
        Inc(Run, 2);
        while not CharInSet(FLine[Run], [#0, #10, #13]) do
          Inc(Run);
      end;
    '*':
      begin
        FRange := rsBlockComment;
        Inc(Run, 2);
        BlockCommentProc;
      end;
  else
    SymbolProc;
  end;
end;

procedure TSynHCLSyn.BlockCommentProc;
begin
  if (Run = FTokenPos) and CharInSet(FLine[Run], [#0, #10, #13]) then
  begin
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    end;
    Exit;
  end;

  FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
    begin
      Inc(Run, 2);
      FRange := rsUnknown;
      Exit;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynHCLSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynHCLSyn.NumberProc;
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
  if CharInSet(FLine[Run], ['e', 'E']) then
  begin
    Inc(Run);
    if CharInSet(FLine[Run], ['+', '-']) then
      Inc(Run);
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
  end;
end;

procedure TSynHCLSyn.StringProc;
begin
  // "..." string; \ escapes; ${} / %{} interpolation kept inline.
  FTokenID := tkString;
  Inc(Run); // opening "
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '\' then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '"' then
    begin
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynHCLSyn.LessProc;
begin
  if (FLine[Run + 1] = '<') and
    (CharInSet(FLine[Run + 2], ['A'..'Z', 'a'..'z', '_']) or
    ((FLine[Run + 2] = '-') and
    CharInSet(FLine[Run + 3], ['A'..'Z', 'a'..'z', '_']))) then
    HeredocOpenProc
  else
    SymbolProc;
end;

procedure TSynHCLSyn.HeredocOpenProc;
begin
  FTokenID := tkString;
  Inc(Run, 2); // <<
  if FLine[Run] = '-' then
    Inc(Run);
  while CharInSet(FLine[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
    Inc(Run);
  FRange := rsHeredoc;
end;

procedure TSynHCLSyn.HeredocProc;
var
  I: Integer;
begin
  if (Run = FTokenPos) and CharInSet(FLine[Run], [#0, #10, #13]) then
  begin
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    end;
    Exit;
  end;

  I := Run;
  while CharInSet(FLine[I], [#9, ' ']) do
    Inc(I);
  if CharInSet(FLine[I], ['A'..'Z', 'a'..'z', '_']) then
  begin
    while CharInSet(FLine[I], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(I);
    while CharInSet(FLine[I], [#9, ' ']) do
      Inc(I);
    if CharInSet(FLine[I], [#0, #10, #13]) then
    begin
      FTokenID := tkString;
      while not CharInSet(FLine[Run], [#0, #10, #13]) do
        Inc(Run);
      FRange := rsUnknown;
      Exit;
    end;
  end;

  FTokenID := tkString;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynHCLSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '=':
      if FLine[Run + 1] = '=' then
        Inc(Run)
      else if FLine[Run + 1] = '>' then
        Inc(Run);
    '!': if FLine[Run + 1] = '=' then Inc(Run);
    '<': if FLine[Run + 1] = '=' then Inc(Run);
    '>': if FLine[Run + 1] = '=' then Inc(Run);
    '&': if FLine[Run + 1] = '&' then Inc(Run);
    '|': if FLine[Run + 1] = '|' then Inc(Run);
    '.':
      if (FLine[Run + 1] = '.') and (FLine[Run + 2] = '.') then
        Inc(Run, 2);
  end;
  Inc(Run);
end;

procedure TSynHCLSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynHCLSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsBlockComment: BlockCommentProc;
    rsHeredoc: HeredocProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '#': CommentProc;
      '/': SlashProc;
      '"': StringProc;
      '<': LessProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '=', '!', '>', '&', '|', '.', ',', ':', ';', '?', '+', '-', '*', '%',
      '(', ')', '[', ']', '{', '}', '~', '^', '@': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynHCLSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeywordAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynHCLSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynHCLSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangHCL;
end;

class function TSynHCLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHCL;
end;

function TSynHCLSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynHCLSyn.GetSampleSource: string;
begin
  Result :=
    '# main.tf -- example'#13#10 +
    'terraform {'#13#10 +
    '  required_version = ">= 1.5"'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'variable "region" {'#13#10 +
    '  type    = string'#13#10 +
    '  default = "us-east-1"'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    '/* multi-line'#13#10 +
    '   block comment */'#13#10 +
    'resource "aws_instance" "web" {'#13#10 +
    '  ami           = data.aws_ami.ubuntu.id'#13#10 +
    '  instance_type = "t3.micro"'#13#10 +
    '  count         = 2'#13#10 +
    ''#13#10 +
    '  tags = {'#13#10 +
    '    Name = "web-${count.index}"'#13#10 +
    '    Env  = upper(var.region)'#13#10 +
    '  }'#13#10 +
    ''#13#10 +
    '  user_data = <<-EOF'#13#10 +
    '    #!/bin/bash'#13#10 +
    '    echo "hello"'#13#10 +
    '  EOF'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    'output "ips" {'#13#10 +
    '  value = [for i in aws_instance.web : i.private_ip]'#13#10 +
    '}';
end;

function TSynHCLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkBlockType: Result := FBlockTypeAttri;
    tkKeyword: Result := FKeywordAttri;
    tkType: Result := FTypeAttri;
    tkFunction: Result := FFunctionAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkString: Result := FStringAttri;
    tkNumber: Result := FNumberAttri;
    tkSymbol: Result := FSymbolAttri;
    tkSpace: Result := FSpaceAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynHCLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynHCLSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynHCLSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterHCL;
end;

procedure TSynHCLSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynHCLSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynHCLSyn);

end.
