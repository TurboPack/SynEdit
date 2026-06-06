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
@abstract(Provides a MATLAB syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterMatlab;

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
  TtkTokenKind = (tkComment, tkSection, tkKeyword, tkBuiltin, tkIdentifier,
    tkString, tkNumber, tkSymbol, tkSpace, tkNull, tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment);

  TSynMatlabSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FAfterValue: Boolean;
    FKeywords: TStringList;
    FBuiltins: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FSectionAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FBuiltinAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function IsAtLineStart: Boolean;
    function RestIsBlank(FromPos: Integer): Boolean;
    procedure UpdateValueState;
    procedure BlockCommentProc;
    procedure CommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SingleQuoteProc;
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
    property SectionAttri: TSynHighlighterAttributes read FSectionAttri write FSectionAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property BuiltinAttri: TSynHighlighterAttributes read FBuiltinAttri write FBuiltinAttri;
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
  SYNS_FilterMatlab = 'MATLAB Files (*.m)|*.m';
  SYNS_LangMatlab = 'MATLAB';
  SYNS_FriendlyLangMatlab = 'MATLAB';

  Keywords: string =
    'arguments,break,case,catch,classdef,continue,else,elseif,end,enumeration,' +
    'events,for,function,global,if,import,methods,otherwise,parfor,persistent,' +
    'properties,return,spmd,switch,try,while';

  Builtins: string =
    'abs,acos,all,angle,any,ans,arrayfun,asin,assert,atan,atan2,bar,cast,cat,' +
    'ceil,cell,cellfun,char,class,clc,clear,clf,close,colorbar,colormap,conj,' +
    'containers,contour,cos,cosh,cumprod,cumsum,deg2rad,disp,display,double,' +
    'eps,error,exp,eye,false,feval,fclose,fieldnames,figure,find,fix,fliplr,' +
    'flipud,floor,fopen,fprintf,fread,func2str,fwrite,gcd,grid,histogram,hold,' +
    'horzcat,imag,imagesc,input,int16,int2str,int32,int64,int8,interp1,isa,' +
    'iscell,ischar,isempty,isequal,isfield,isinf,islogical,ismatrix,isnan,' +
    'isnumeric,isobject,isreal,isscalar,isstruct,isvector,legend,length,' +
    'linspace,load,log,log10,log2,logical,lower,magic,mat2str,max,mean,median,' +
    'mesh,min,mod,nan,nargin,nargout,nchoosek,ndims,numel,num2str,ones,pause,' +
    'pi,plot,plot3,polyfit,polyval,prod,rad2deg,rand,randi,randn,real,rem,' +
    'repmat,reshape,round,save,scatter,sign,signal,sin,single,sinh,size,sort,' +
    'sortrows,sprintf,sqrt,std,str2double,str2func,str2num,strcmp,strcmpi,' +
    'strfind,strjoin,strncmp,strrep,strsplit,strtrim,struct,subplot,sum,surf,' +
    'tan,tanh,tic,title,toc,true,typecast,uint16,uint32,uint64,uint8,unique,' +
    'upper,var,varargin,varargout,vertcat,warning,xlabel,xlim,ylabel,ylim,' +
    'zeros,zlabel,Inf,NaN';

constructor TSynMatlabSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText := Keywords;

  FBuiltins := TStringList.Create;
  FBuiltins.CaseSensitive := True;
  FBuiltins.Sorted := True;
  FBuiltins.Duplicates := dupIgnore;
  FBuiltins.CommaText := Builtins;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  FSectionAttri.Foreground := clTeal;
  FSectionAttri.Style := [fsBold, fsItalic];
  AddAttribute(FSectionAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FBuiltinAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  FBuiltinAttri.Foreground := clOlive;
  AddAttribute(FBuiltinAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clMaroon;
  AddAttribute(FStringAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterMatlab;
  FRange := rsUnknown;
  FAfterValue := False;
end;

destructor TSynMatlabSyn.Destroy;
begin
  FBuiltins.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynMatlabSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

function TSynMatlabSyn.IsAtLineStart: Boolean;
var
  I: Integer;
begin
  for I := 0 to Run - 1 do
    if FLine[I] > ' ' then
      Exit(False);
  Result := True;
end;

function TSynMatlabSyn.RestIsBlank(FromPos: Integer): Boolean;
var
  P: Integer;
begin
  P := FromPos;
  while not CharInSet(FLine[P], [#0, #10, #13]) do
  begin
    if FLine[P] > ' ' then
      Exit(False);
    Inc(P);
  end;
  Result := True;
end;

function TSynMatlabSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKeyword
  else if FBuiltins.IndexOf(S) >= 0 then
    Result := tkBuiltin
  else
    Result := tkIdentifier;
end;

procedure TSynMatlabSyn.UpdateValueState;
begin
  case FTokenID of
    tkSpace, tkComment, tkSection, tkNull: ; // transparent
    tkIdentifier, tkNumber, tkString, tkBuiltin:
      FAfterValue := True;
    tkKeyword:
      FAfterValue := False;
    tkSymbol:
      FAfterValue := (Run > 0) and
        CharInSet(FLine[Run - 1], [')', ']', '}', '''']);
  else
    FAfterValue := False;
  end;
end;

procedure TSynMatlabSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynMatlabSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynMatlabSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynMatlabSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynMatlabSyn.CommentProc;
begin
  if (FLine[Run + 1] = '{') and IsAtLineStart and RestIsBlank(Run + 2) then
  begin
    // '%{' alone on a line opens a block comment.
    FTokenID := tkComment;
    FRange := rsBlockComment;
    while not CharInSet(FLine[Run], [#0, #10, #13]) do
      Inc(Run);
    Exit;
  end;

  if FLine[Run + 1] = '%' then
    FTokenID := tkSection // '%%' section/cell marker
  else
    FTokenID := tkComment;
  repeat
    Inc(Run);
  until CharInSet(FLine[Run], [#0, #10, #13]);
end;

procedure TSynMatlabSyn.BlockCommentProc;
var
  P: Integer;
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
  // A line consisting only of '%}' (ignoring surrounding spaces) closes it.
  P := Run;
  while CharInSet(FLine[P], [#9, ' ']) do
    Inc(P);
  if (FLine[P] = '%') and (FLine[P + 1] = '}') and RestIsBlank(P + 2) then
    FRange := rsUnknown;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynMatlabSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynMatlabSyn.NumberProc;
begin
  FTokenID := tkNumber;
  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
      Inc(Run);
  end
  else if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['b', 'B']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0', '1']) do
      Inc(Run);
  end
  else
  begin
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
    if FLine[Run] = '.' then
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
    if CharInSet(FLine[Run], ['i', 'j', 'I', 'J']) then
      Inc(Run);
  end;
end;

procedure TSynMatlabSyn.StringProc;
var
  Quote: WideChar;
begin
  // 'char array' or "string"; a doubled quote is an escaped quote.
  FTokenID := tkString;
  Quote := FLine[Run];
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = Quote then
    begin
      if FLine[Run + 1] = Quote then
        Inc(Run, 2)
      else
      begin
        Inc(Run);
        Break;
      end;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynMatlabSyn.SingleQuoteProc;
begin
  if FAfterValue then
  begin
    // Transpose operator (e.g. A'), not a string.
    FTokenID := tkSymbol;
    Inc(Run);
  end
  else
    StringProc;
end;

procedure TSynMatlabSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '=', '~', '<', '>': if FLine[Run + 1] = '=' then Inc(Run);
    '&': if FLine[Run + 1] = '&' then Inc(Run);
    '|': if FLine[Run + 1] = '|' then Inc(Run);
    '.':
      case FLine[Run + 1] of
        '''', '*', '/', '\', '^': Inc(Run);    // .'  .*  ./  .\  .^
        '.': if FLine[Run + 2] = '.' then Inc(Run, 2); // ... continuation
      end;
  end;
  Inc(Run);
end;

procedure TSynMatlabSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynMatlabSyn.Next;
begin
  FTokenPos := Run;
  if Run = 0 then
    FAfterValue := False;
  case FRange of
    rsBlockComment: BlockCommentProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '%': CommentProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      '.':
        if CharInSet(FLine[Run + 1], ['0'..'9']) then
          NumberProc
        else
          SymbolProc;
      '"': StringProc;
      '''': SingleQuoteProc;
      '+', '-', '*', '/', '\', '^', '=', '<', '>', '~', '&', '|', ':', ';',
      ',', '(', ')', '[', ']', '{', '}', '@', '!': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  UpdateValueState;
  inherited;
end;

function TSynMatlabSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynMatlabSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynMatlabSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangMatlab;
end;

class function TSynMatlabSyn.GetLanguageName: string;
begin
  Result := SYNS_LangMatlab;
end;

function TSynMatlabSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynMatlabSyn.GetSampleSource: string;
begin
  Result :=
    '%% Section: factorial demo'#13#10 +
    '% compute n! with a loop'#13#10 +
    'function y = fact(n)'#13#10 +
    '    %{'#13#10 +
    '      A block comment'#13#10 +
    '      spanning several lines.'#13#10 +
    '    %}'#13#10 +
    '    y = 1;'#13#10 +
    '    for k = 1:n'#13#10 +
    '        y = y * k;'#13#10 +
    '    end'#13#10 +
    '    A = [1 2 3; 4 5 6];'#13#10 +
    '    B = A'';            % transpose, not a string'#13#10 +
    '    s = ''hello world''; % char array'#13#10 +
    '    str = "a string";   % string scalar'#13#10 +
    '    z = 3 + 4i;'#13#10 +
    '    fprintf(''%d\n'', y);'#13#10 +
    'end';
end;

function TSynMatlabSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkSection: Result := FSectionAttri;
    tkKeyword: Result := FKeyAttri;
    tkBuiltin: Result := FBuiltinAttri;
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

function TSynMatlabSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynMatlabSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynMatlabSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterMatlab;
end;

procedure TSynMatlabSyn.ResetRange;
begin
  FRange := rsUnknown;
  FAfterValue := False;
end;

procedure TSynMatlabSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynMatlabSyn);

end.
