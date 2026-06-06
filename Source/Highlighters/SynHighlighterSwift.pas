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
@abstract(Provides a Swift syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterSwift;

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
  TtkTokenKind = (tkComment, tkKeyword, tkType, tkAttribute, tkDirective,
    tkIdentifier, tkString, tkNumber, tkSymbol, tkSpace, tkNull, tkUnknown);

  // rsBlockComment uses the level field as the nesting depth; rsMultiString
  // uses it as the number of '#' delimiters (0 for a plain """ string).
  TRangeState = (rsUnknown, rsBlockComment, rsMultiString);

  TSynSwiftSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FRangeLevel: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FTypes: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FTypeAttri: TSynHighlighterAttributes;
    FAttributeAttri: TSynHighlighterAttributes;
    FDirectiveAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function HashRun(APos, ACount: Integer): Boolean;
    procedure AttributeProc;
    procedure BacktickProc;
    procedure BlockCommentProc;
    procedure CRProc;
    procedure DoubleQuoteProc;
    procedure HashProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MultiStringProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc(AHashCount: Integer);
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
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property TypeAttri: TSynHighlighterAttributes read FTypeAttri write FTypeAttri;
    property AttributeAttri: TSynHighlighterAttributes read FAttributeAttri write FAttributeAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirectiveAttri write FDirectiveAttri;
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
  SYNS_FilterSwift = 'Swift Files (*.swift)|*.swift';
  SYNS_LangSwift = 'Swift';
  SYNS_FriendlyLangSwift = 'Swift';

  Keywords: string =
    'as,associatedtype,async,await,break,case,catch,class,continue,convenience,' +
    'default,defer,deinit,didSet,distributed,do,dynamic,else,enum,extension,' +
    'fallthrough,false,fileprivate,final,for,func,get,guard,if,import,in,' +
    'indirect,infix,init,inout,internal,is,isolated,lazy,let,mutating,nil,' +
    'nonisolated,nonmutating,open,operator,optional,override,package,postfix,' +
    'prefix,private,protocol,public,repeat,required,rethrows,return,self,Self,' +
    'set,some,static,struct,subscript,super,switch,throw,throws,true,try,' +
    'typealias,unowned,var,weak,where,while,willSet,actor,any,Any';

  Types: string =
    'AnyClass,AnyHashable,AnyObject,Array,ArraySlice,Bool,CaseIterable,CGFloat,' +
    'CGPoint,CGRect,CGSize,Character,ClosedRange,Codable,Collection,Comparable,' +
    'ContiguousArray,CustomStringConvertible,Data,Date,Decimal,Decodable,' +
    'Dictionary,Double,Encodable,Equatable,Error,Float,Float16,Float32,Float64,' +
    'Float80,Hashable,Identifiable,IndexPath,Int,Int16,Int32,Int64,Int8,' +
    'IteratorProtocol,Never,Notification,NSObject,NSString,Optional,OptionSet,' +
    'Range,Result,Sequence,Set,StaticString,String,Substring,TimeInterval,' +
    'UInt,UInt16,UInt32,UInt64,UInt8,Unicode,URL,UUID,Void';

constructor TSynSwiftSyn.Create(AOwner: TComponent);
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

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType, SYNS_FriendlyAttrDataType);
  FTypeAttri.Foreground := clTeal;
  FTypeAttri.Style := [fsBold];
  AddAttribute(FTypeAttri);

  FAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro, SYNS_FriendlyAttrMacro);
  FAttributeAttri.Foreground := clOlive;
  AddAttribute(FAttributeAttri);

  FDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FDirectiveAttri.Foreground := clPurple;
  FDirectiveAttri.Style := [fsBold];
  AddAttribute(FDirectiveAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
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
  FDefaultFilter := SYNS_FilterSwift;
  FRange := rsUnknown;
  FRangeLevel := 0;
end;

destructor TSynSwiftSyn.Destroy;
begin
  FTypes.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynSwiftSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_') or (AChar = '$');
end;

function TSynSwiftSyn.HashRun(APos, ACount: Integer): Boolean;
var
  K: Integer;
begin
  for K := 0 to ACount - 1 do
    if FLine[APos + K] <> '#' then
      Exit(False);
  Result := True;
end;

function TSynSwiftSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKeyword
  else if FTypes.IndexOf(S) >= 0 then
    Result := tkType
  else
    Result := tkIdentifier;
end;

procedure TSynSwiftSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynSwiftSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynSwiftSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynSwiftSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynSwiftSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynSwiftSyn.AttributeProc;
begin
  // @available, @objc, @escaping, @MainActor, ...
  FTokenID := tkAttribute;
  Inc(Run); // '@'
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynSwiftSyn.BacktickProc;
begin
  // `escaped` identifier (using a keyword as a name)
  FTokenID := tkIdentifier;
  Inc(Run); // opening `
  while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> '`') do
    Inc(Run);
  if FLine[Run] = '`' then
    Inc(Run);
end;

procedure TSynSwiftSyn.NumberProc;
begin
  FTokenID := tkNumber;
  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
      Inc(Run);
    if (FLine[Run] = '.') and
      CharInSet(FLine[Run + 1], ['0'..'9', 'a'..'f', 'A'..'F']) then
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
        Inc(Run);
    end;
    if CharInSet(FLine[Run], ['p', 'P']) then
    begin
      Inc(Run);
      if CharInSet(FLine[Run], ['+', '-']) then
        Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9', '_']) do
        Inc(Run);
    end;
  end
  else if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['o', 'O', 'b', 'B']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', '_']) do
      Inc(Run);
  end
  else
  begin
    while CharInSet(FLine[Run], ['0'..'9', '_']) do
      Inc(Run);
    if (FLine[Run] = '.') and CharInSet(FLine[Run + 1], ['0'..'9']) then
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9', '_']) do
        Inc(Run);
    end;
    if CharInSet(FLine[Run], ['e', 'E']) then
    begin
      Inc(Run);
      if CharInSet(FLine[Run], ['+', '-']) then
        Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9', '_']) do
        Inc(Run);
    end;
  end;
end;

procedure TSynSwiftSyn.SlashProc;
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
        FRangeLevel := 1;
        Inc(Run, 2);
        BlockCommentProc;
      end;
  else
    SymbolProc;
  end;
end;

procedure TSynSwiftSyn.BlockCommentProc;
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
    if (FLine[Run] = '/') and (FLine[Run + 1] = '*') then
    begin
      Inc(FRangeLevel);
      Inc(Run, 2);
    end
    else if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
    begin
      if FRangeLevel > 0 then
        Dec(FRangeLevel);
      Inc(Run, 2);
      if FRangeLevel <= 0 then
      begin
        FRange := rsUnknown;
        FRangeLevel := 0;
        Exit;
      end;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynSwiftSyn.DoubleQuoteProc;
begin
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then
  begin
    // multi-line """ ... """ string
    FRange := rsMultiString;
    FRangeLevel := 0;
    Inc(Run, 3);
    MultiStringProc;
  end
  else
    StringProc(0);
end;

procedure TSynSwiftSyn.StringProc(AHashCount: Integer);
begin
  FTokenID := tkString;
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if (AHashCount = 0) and (FLine[Run] = '\') then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if FLine[Run] = '"' then
    begin
      Inc(Run);
      if AHashCount = 0 then
        Break
      else if HashRun(Run, AHashCount) then
      begin
        Inc(Run, AHashCount);
        Break;
      end;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynSwiftSyn.MultiStringProc;
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

  FTokenID := tkString;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if (FRangeLevel = 0) and (FLine[Run] = '\') then
    begin
      Inc(Run);
      if not CharInSet(FLine[Run], [#0, #10, #13]) then
        Inc(Run);
    end
    else if (FLine[Run] = '"') and (FLine[Run + 1] = '"') and
      (FLine[Run + 2] = '"') and HashRun(Run + 3, FRangeLevel) then
    begin
      Inc(Run, 3);
      Inc(Run, FRangeLevel);
      FRange := rsUnknown;
      FRangeLevel := 0;
      Exit;
    end
    else
      Inc(Run);
  end;
end;

procedure TSynSwiftSyn.HashProc;
var
  N, P: Integer;
begin
  N := 0;
  P := Run;
  while FLine[P] = '#' do
  begin
    Inc(N);
    Inc(P);
  end;
  if FLine[P] = '"' then
  begin
    if (FLine[P + 1] = '"') and (FLine[P + 2] = '"') then
    begin
      // multi-line raw string  #"""  ...  """#
      FRange := rsMultiString;
      FRangeLevel := N;
      Run := P + 3;
      MultiStringProc;
    end
    else
    begin
      // single-line raw string  #"..."#
      Run := P;
      StringProc(N);
    end;
  end
  else if IsCharAlpha(FLine[P]) then
  begin
    // compiler directive / literal: #if, #available, #selector, #file, ...
    FTokenID := tkDirective;
    Run := P;
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
  end
  else
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynSwiftSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '-': if CharInSet(FLine[Run + 1], ['>', '=']) then Inc(Run);
    '+', '*', '%', '/', '^': if FLine[Run + 1] = '=' then Inc(Run);
    '=':
      if FLine[Run + 1] = '=' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end;
    '!':
      if FLine[Run + 1] = '=' then
      begin
        Inc(Run);
        if FLine[Run + 1] = '=' then Inc(Run);
      end;
    '<': if CharInSet(FLine[Run + 1], ['=', '<']) then Inc(Run);
    '>': if CharInSet(FLine[Run + 1], ['=', '>']) then Inc(Run);
    '&': if CharInSet(FLine[Run + 1], ['&', '=']) then Inc(Run);
    '|': if CharInSet(FLine[Run + 1], ['|', '=']) then Inc(Run);
    '?': if CharInSet(FLine[Run + 1], ['?', '.']) then Inc(Run);
    '.':
      if FLine[Run + 1] = '.' then
      begin
        Inc(Run);
        if CharInSet(FLine[Run + 1], ['.', '<']) then Inc(Run); // ... or ..<
      end;
  end;
  Inc(Run);
end;

procedure TSynSwiftSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynSwiftSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsBlockComment: BlockCommentProc;
    rsMultiString: MultiStringProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_', '$': IdentProc;
      '0'..'9': NumberProc;
      '/': SlashProc;
      '"': DoubleQuoteProc;
      '#': HashProc;
      '@': AttributeProc;
      '`': BacktickProc;
      '+', '-', '*', '%', '=', '<', '>', '!', '&', '|', '^', '~', '?', ':',
      ';', ',', '.', '(', ')', '[', ']', '{', '}', '\': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynSwiftSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynSwiftSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynSwiftSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangSwift;
end;

class function TSynSwiftSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSwift;
end;

function TSynSwiftSyn.GetRange: Pointer;
begin
  Result := Pointer(NativeInt(Ord(FRange)) or (NativeInt(FRangeLevel) shl 8));
end;

function TSynSwiftSyn.GetSampleSource: string;
begin
  Result :=
    '// Swift sample'#13#10 +
    'import Foundation'#13#10 +
    ''#13#10 +
    '@available(iOS 15, *)'#13#10 +
    'protocol Greeter {'#13#10 +
    '    func greet(_ name: String) -> String'#13#10 +
    '}'#13#10 +
    ''#13#10 +
    '/* a /* nested */ comment */'#13#10 +
    'struct Person: Greeter, Codable {'#13#10 +
    '    let id: Int'#13#10 +
    '    var name: String'#13#10 +
    ''#13#10 +
    '    func greet(_ name: String) -> String {'#13#10 +
    '        let count = 0xFF + 1_000'#13#10 +
    '        let raw = #"a \"raw\" string"#'#13#10 +
    '        return """'#13#10 +
    '        Hello, \(name)!'#13#10 +
    '        Count: \(count)'#13#10 +
    '        """'#13#10 +
    '    }'#13#10 +
    '}';
end;

function TSynSwiftSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkKeyword: Result := FKeyAttri;
    tkType: Result := FTypeAttri;
    tkAttribute: Result := FAttributeAttri;
    tkDirective: Result := FDirectiveAttri;
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

function TSynSwiftSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynSwiftSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynSwiftSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterSwift;
end;

procedure TSynSwiftSyn.ResetRange;
begin
  FRange := rsUnknown;
  FRangeLevel := 0;
end;

procedure TSynSwiftSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(NativeInt(Value) and $FF);
  FRangeLevel := NativeInt(Value) shr 8;
end;

initialization
  RegisterPlaceableHighlighter(TSynSwiftSyn);

end.
