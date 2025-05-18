{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterJScript.pas, released 2000-04-14.
The Original Code is based on the mwJScript.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony de Buys.
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

$Id: SynHighlighterJScript.pas,v 1.21.2.7 2005/12/16 16:10:37 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a JavaScript/JScript highlighter for SynEdit)
@author(Tony De Buys [tony@lad.co.za], converted to SynEdit by David Muir <david@loanhead45.freeserve.co.uk>)
@created(December 1999, converted to SynEdit April 14, 2000)
@lastmod(2000-06-23)
The SynHighlighterJScript unit provides SynEdit with a JScript/JavaScript (.js) highlighter.
The highlighter formats JavaScript source code highlighting keywords, strings, numbers and characters.
}

unit SynHighlighterJScript;

{$I SynEdit.inc}

interface

uses
  Graphics,
  System.Win.Registry,
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  SynUnicode,
  Classes,
//++ CodeFolding
  SynEditCodeFolding;
//++ CodeFolding


type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber, tkSpace,
    tkString, tkUnknown, tkNonReservedKey, tkEvent, tkSpecVar, tkTemplate);

  TRangeState = (rsUnknown, rsANSI, rsLiteral, rsLiteralTemplate);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
//  TSynJScriptSyn = class(TSynCustomHighLighter)
//++ CodeFolding
  TSynJScriptSyn = class(TSynCustomCodeFoldingHighlighter)
//-- CodeFolding
  private
    fRange: TRangeState;
    fLiteralLevel: Integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNonReservedKeyAttri: TSynHighlighterAttributes;
    fEventAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSpecVarAttri: TSynHighlighterAttributes;
    fTemplateAttri: TSynHighlighterAttributes;

    procedure AndSymbolProc;
    procedure CommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure LiteralsProc;
    procedure LiteralsRangeProc;
    procedure LiteralsTemplateRangeProc;
    procedure SymbolProc;
    procedure UnderScoreProc;
    procedure UnknownProc;
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
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function IsKeyword(const AKeyword: string): Boolean; override;
    function IsEvent(const AKeyword: string): Boolean;
    function IsNonReserwedKeyWord(const AKeyword: string): Boolean;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
//++ CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
//-- CodeFolding
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    { ths attribut will hghlight words start wth underscore }
    property SpecVarAttri: TSynHighlighterAttributes read fSpecVarAttri
      write fSpecVarAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NonReservedKeyAttri: TSynHighlighterAttributes read fNonReservedKeyAttri write fNonReservedKeyAttri;
    property EventAttri: TSynHighlighterAttributes read fEventAttri write fEventAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property TemplateAttri: TSynHighlighterAttributes read FTemplateAttri write FTemplateAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  // IMPORTANT!!! Cause JavaScript is case sensitive, list must be sorted by ASCII values
  NonReserwedKeyWords: array[0..317] of string = (
    'All', 'Anchor', 'Applet', 'Area', 'Arguments', 'Array', 'Boolean',
    'Button', 'Checkbox', 'Date', 'E', 'Embed', 'FileUpload', 'Float',
    'Form', 'Frame', 'Function', 'Global', 'Hidden', 'History', 'Image',
    'Infinity', 'LN10', 'LN2', 'LOG10E', 'LOG2E', 'Layer', 'Link',
    'Location', 'MAX_VALUE', 'MIN_VALUE', 'Math', 'MimeType',
    'NEGATIVE_INFINITY', 'Navigator', 'Null', 'Number', 'Object',
    'Option', 'PI', 'POSITIVE_INFINITY', 'Packages', 'Password', 'Plugin',
    'Radio', 'RegExp', 'Reset', 'SQRT1_2', 'SQRT2', 'Select', 'String',
    'Submit', 'Text', 'Textarea', 'URL', 'UTC', 'Undefined', 'Window',
    'abs', 'acos', 'action', 'alert', 'align', 'alinkColor', 'all',
    'anchor', 'anchors', 'appCodeName', 'appName', 'appVersion',
    'applets', 'arguments', 'asin', 'atan', 'atan2', 'await', 'back',
    'background', 'bgColor', 'big', 'blink', 'blur', 'body', 'bold',
    'border', 'bottom', 'call', 'caller', 'captureEvents', 'ceil',
    'charAt', 'charCodeAt', 'checked', 'clear', 'clearInterval',
    'clearTimeout', 'click', 'close', 'closed', 'color', 'complete',
    'concat', 'confirm', 'cookie', 'cos', 'current', 'defaultChecked',
    'defaultSelected', 'defaultStatus', 'defaultValue', 'description',
    'display', 'document', 'domain', 'elements', 'embeds',
    'enabledPlugin', 'encoding', 'escape', 'eval', 'event', 'exp',
    'fgColor', 'filename', 'find', 'fixed', 'floor', 'focus', 'fontcolor',
    'fontsize', 'form', 'forms', 'forward', 'frames', 'fromCharCode',
    'getDate', 'getDay', 'getElementById', 'getFullYear', 'getHours',
    'getMilliseconds', 'getMinutes', 'getMonth', 'getSeconds', 'getTime',
    'getTimezoneOffset', 'getUTCDate', 'getUTCDay', 'getUTCFullYear',
    'getUTCHours', 'getUTCMilliseconds', 'getUTCMinutes', 'getUTCMonth',
    'getUTCSeconds', 'getYear', 'go', 'handleEvent', 'hash', 'height',
    'history', 'home', 'host', 'hostname', 'href', 'hspace', 'images',
    'index', 'indexOf', 'innerHeight', 'innerWidth', 'input', 'isFinite',
    'isNaN', 'italics', 'java', 'javaEnabled', 'join', 'lastIndexOf',
    'lastModified', 'layers', 'left', 'length', 'let', 'link',
    'linkColor', 'links', 'location', 'locationbar', 'log', 'logon',
    'lowsrc', 'match', 'max', 'menubar', 'method', 'mimeTypes', 'min',
    'moveBy', 'moveTo', 'name', 'navigator', 'netscape', 'next', 'open',
    'opener', 'options', 'outerHeight', 'outerWidth', 'pageX',
    'pageXOffset', 'pageY', 'pageYOffset', 'parent', 'parse',
    'parseFloat', 'parseInt', 'pathname', 'personalbar', 'platform',
    'plugins', 'port', 'pow', 'previous', 'print', 'prompt', 'protocol',
    'random', 'referrer', 'refresh', 'releaseEvents', 'reload', 'replace',
    'reset', 'resizeBy', 'resizeTo', 'reverse', 'right', 'round',
    'routeEvent', 'screen', 'scroll', 'scrollBy', 'scrollTo',
    'scrollbars', 'search', 'select', 'selected', 'selectedIndex', 'self',
    'setDate', 'setFullYear', 'setHours', 'setInterval',
    'setMilliseconds', 'setMinutes', 'setMonth', 'setSeconds', 'setTime',
    'setTimeout', 'setUTCDate', 'setUTCFullYear', 'setUTCHours',
    'setUTCMilliseconds', 'setUTCMinutes', 'setUTCMonth', 'setUTCSeconds',
    'setYear', 'sin', 'slice', 'small', 'sort', 'split', 'sqrt', 'src',
    'status', 'statusbar', 'stop', 'strike', 'style', 'sub', 'submit',
    'substr', 'substring', 'suffixes', 'sup', 'tags', 'taint',
    'taintEnabled', 'tan', 'target', 'text', 'title', 'toGMTString',
    'toLocaleString', 'toLowerCase', 'toSource', 'toString',
    'toUTCString', 'toUpperCase', 'toolbar', 'top', 'type', 'undefined',
    'unescape', 'untaint', 'unwatch', 'userAgent', 'value', 'valueOf',
    'visibility', 'vlinkColor', 'vspace', 'watch', 'width', 'window',
    'write', 'writeln', 'zIndex'
  );

  KeyWords: array[0..61] of string = (
    'abstract', 'boolean', 'break', 'byte', 'callee', 'case',
    'catch', 'char', 'const', 'constructor', 'continue',
    'debugger', 'default', 'delete', 'do', 'double', 'else',
    'enum', 'export', 'extends', 'false', 'final', 'finally',
    'float', 'for', 'function', 'goto', 'if', 'implements',
    'import', 'in', 'instanceof', 'int', 'interface', 'long',
    'NaN', 'native', 'new', 'null', 'package', 'private',
    'protected', 'prototype', 'public', 'return', 'short',
    'start', 'static', 'super', 'switch', 'synchronized',
    'this', 'throw', 'throws', 'transient', 'true', 'try',
    'typeof', 'var', 'void', 'while', 'with'
  );

  Events: array[0..19] of string = (
    'onAbort', 'onBlur', 'onChange', 'onClick', 'onDblClick',
    'onError', 'onFocus', 'onKeyDown', 'onKeyPress', 'onKeyUp',
    'onLoad', 'onMouseDown', 'onMouseMove', 'onMouseOut',
    'onMouseOver', 'onMouseUp', 'onReset', 'onSelect',
    'onSubmit', 'onUnload'
  );

function TSynJScriptSyn.IsKeyword(const AKeyword: string): Boolean;
var
  First, Last, I, Compare: Integer;
begin
  First := 0;
  Last := High(Keywords);
  Result := False;

  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(Keywords[I], AKeyWord);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end;

function TSynJScriptSyn.IsEvent(const AKeyword: string): Boolean;
var
  First, Last, I, Compare: Integer;
begin
  First := 0;
  Last := High(Events);
  Result := False;

  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(Events[I], AKeyWord);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end;

function TSynJScriptSyn.IsNonReserwedKeyWord(const AKeyword: string): Boolean;
var
  First, Last, I, Compare: Integer;
begin
  First := 0;
  Last := High(NonReserwedKeyWords);
  Result := False;

  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(NonReserwedKeyWords[I], AKeyWord);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end;

constructor TSynJScriptSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fLiteralLevel := 0;
  fCaseSensitive := True;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fSpecVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpecialVariable, SYNS_FriendlyAttrSpecialVariable);
  AddAttribute(fSpecVarAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNonReservedKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  AddAttribute(fNonReservedKeyAttri);
  fEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrEvent, SYNS_FriendlyAttrEvent);
  AddAttribute(fEventAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fTemplateAttri := TSynHighlighterAttributes.Create(SYNS_AttrTemplate, SYNS_FriendlyAttrTemplate);
  AddAttribute(fTemplateAttri);
  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := SYNS_FilterJScript;
  fRange := rsUnknown;
end;

procedure TSynJScriptSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if CharInSet(fLine[Run], ['=', '&']) then inc(Run);
end;

procedure TSynJScriptSyn.CommentProc;
begin
  if fLine[Run] = #0 then
    NullProc                                                           
  else 
  begin
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then 
      begin
        fRange := rsUnKnown;
        inc(Run, 2);
        break;
      end;
      inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynJScriptSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynJScriptSyn.IdentProc;
begin
  while IsIdentChar(fLine[Run]) do inc(Run);
  if IsEvent(GetToken) then
  begin
    fTokenId := tkEvent;
  end
  else
  if IsKeyWord(GetToken) then
  begin
    fTokenId := tkKey;
  end
  else
  if IsNonReserwedKeyWord(GetToken) then
    fTokenId := tkNonReservedKey
  else
    fTokenId := tkIdentifier;
end;

procedure TSynJScriptSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynJScriptSyn.LiteralsProc;
begin
  fTokenID := tkString;
  fRange := rsLiteral;
  Inc(Run);
  if not IsLineEnd(Run) then
    LiteralsRangeProc;
end;

procedure TSynJScriptSyn.LiteralsRangeProc;
var
  OK: Boolean;
  myLit: Boolean;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkString;

    repeat
      ok := (FLine[Run] = '`') and ((Run = 0) or (FLine[Pred(Run)] <> '\'));
      myLit := (FLine[Run] = '$') and (FLine[Run + 1] = '{');
      if myLit then
      begin
        Inc(fLiteralLevel);
        fRange := rsLiteralTemplate;
        Break;
      end;
      if OK then
      begin
        Inc(Run);
          fRange := rsUnKnown;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynJScriptSyn.LiteralsTemplateRangeProc;
var
  OK: Boolean;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkTemplate;
    if (FLine[Run] = '$') and (FLine[Run+1] = '{') then
      Inc(Run, 2);

    repeat
      ok := (FLine[Run] = '}');
      { end of Template = switch back to Literal }
      if OK then
      begin
        Dec(fLiteralLevel);
        Inc(Run);
        fRange := rsLiteral;
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;

end;

procedure TSynJScriptSyn.MinusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if CharInSet(fLine[Run], ['=', '-', '>']) then inc(Run);
end;

procedure TSynJScriptSyn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynJScriptSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynJScriptSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsHexChar(Run: Integer): Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'a'..'f', 'A'..'F':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  isHex: Boolean;
begin
  fTokenID := tkNumber;
  isHex := False;
  idx1 := Run;
  Inc(Run);
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Succ(Run)] = '.' then
          Break;
      'a'..'f', 'A'..'F':
        if not isHex then
          Break;
      'x', 'X':
        begin
          if (FLine[idx1] <> '0') or (Run > Succ(idx1)) then
            Break;
          if not IsHexChar(Succ(Run)) then
            Break;
          isHex := True;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynJScriptSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if CharInSet(fLine[Run], ['=', '|']) then inc(Run);
end;

procedure TSynJScriptSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if CharInSet(fLine[Run], ['=', '+']) then inc(Run);
end;

procedure TSynJScriptSyn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') and (fLine[Run + 1] = '.') then inc(Run, 2);
end;

procedure TSynJScriptSyn.SlashProc;
begin
  Inc(Run);
  { we should handle escaped slash \// } 
  if (Run > 1) and (FLine[run-2] <> '\') or (Run=1) then
  case fLine[Run] of
    '/': begin
           fTokenID := tkComment;
           repeat
             Inc(Run);
           until IsLineEnd(Run);
         end;
    '*': begin
           fTokenID := tkComment;
           fRange := rsAnsi;
           repeat
             Inc(Run);
             if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
               fRange := rsUnKnown;
               Inc(Run, 2);
               break;
             end;
           until IsLineEnd(Run);
         end;
    '=': begin
           Inc(Run);
           fTokenID := tkSymbol;
         end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynJScriptSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynJScriptSyn.StarProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynJScriptSyn.StringProc;
var
  l_strChar: WideChar;
  IsEscaped: Boolean;
begin
  fTokenID := tkString;
  l_strChar := FLine[Run];   // We could have '"' or #39
  { we will handle escaped quotes }
  IsEscaped := False;
  if (FLine[Run + 1] = l_strChar) and (FLine[Run + 2] = l_strChar) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      '\': IsEscaped := not IsEscaped;
    else
      IsEscaped := False;
    end;
    inc(Run);
  until (FLine[Run] = l_strChar) and (FLine[Pred(Run)] <> '\') and not IsEscaped;
  if not IsLineEnd(Run) then
    Inc(Run);
end;

procedure TSynJScriptSyn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynJScriptSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynJScriptSyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsANSI then
    CommentProc
  else
  if fRange = rsLiteralTemplate then
    LiteralsTemplateRangeProc
  else
  if fRange = rsLiteral then
    LiteralsRangeProc
  else
    case fLine[Run] of
      '&': AndSymbolProc;
      #13: CRProc;
      'A'..'Z', 'a'..'z': IdentProc;
      '_': UnderscoreProc;
      #10: LFProc;
      '-': MinusProc;
      '%': ModSymbolProc;
      #0: NullProc;
      '0'..'9': NumberProc;
      '|': OrSymbolProc;
      '+': PlusProc;
      '.': PointProc;
      '/': SlashProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      '*': StarProc;
      '"', #39: StringProc;
      '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=':
        SymbolProc;
      '`': LiteralsProc;
      else UnknownProc;
    end;
  inherited;
end;

function TSynJScriptSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynJScriptSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynJScriptSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynJScriptSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynJScriptSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonReservedKey: Result := fNonReservedKeyAttri;
    tkEvent: Result := fEventAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkSpecVar: Result := fSpecVarAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTemplate: Result := FTemplateAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynJScriptSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynJScriptSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynJScriptSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynJScriptSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterJScript;
end;

class function TSynJScriptSyn.GetLanguageName: string;
begin
  Result := SYNS_LangJScript;
end;

function TSynJScriptSyn.GetSampleSource: string;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  var number = 1234;'#13#10+
            '  var x;'#13#10+
            '  document.write("The number is " + number);'#13#10+
            '  for (var i = 0; i <= number; i++)'#13#10+
            '  {'#13#10+
            '    x++;'#13#10+
            '    x--;'#13#10+
            '    x += 1.0;'#13#10+
            '  }'#13#10+
            '  i += @; // illegal character'#13#10+
            '}'#13#10+
            'body.onLoad = printNumber;';
end;

class function TSynJScriptSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangJScript;
end;

procedure TSynJScriptSyn.UnderScoreProc; 
begin
  if (Run = 0) or not IsIdentChar(fLine[Run-1]) then
    fTokenID := tkSpecVar
  else
    fTokenID := tkIdentifier;
  inc(Run);
  while IsIdentChar(fLine[Run]) do inc(Run);
end;

//++ CodeFolding
procedure TSynJScriptSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
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
    if Uppercase(Copy(S, 1, 9)) = '//#REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if Uppercase(Copy(S, 1, 12)) = '//#ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline comments (Fold Type 2)
    if TRangeState(GetLineRange(LinesToScan, Line)) = rsANSI then
    begin
      if TRangeState(GetLineRange(LinesToScan, Line - 1)) <> rsANSI then
        FoldRanges.StartFoldRange(Line + 1, 2)
      else
        FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end
    else if TRangeState(GetLineRange(LinesToScan, Line - 1)) = rsANSI then
    begin
      FoldRanges.StopFoldRange(Line + 1, 2);
      Continue;
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
//-- CodeFolding


initialization
  RegisterPlaceableHighlighter(TSynJScriptSyn);
end.
