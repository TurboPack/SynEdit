{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterTclTk.pas, released 2000-05-05.
The Original Code is based on the siTclTkSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Igor Shitikov.
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
@abstract(Provides a TCL/Tk highlighter for SynEdit)
@author(Igor Shitikov, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(5 December 1999, converted to SynEdit April 18, 2000)
@lastmod(2000-06-23)
The SynHighlighterTclTk unit provides SynEdit with a TCL/Tk highlighter.
}

unit SynHighlighterTclTk;

{$I SynEdit.inc}

interface

uses
  Windows,
  Registry,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber, tkSecondKey,
    tkTixKey, tkSpace, tkString, tkOptions, tkVariable, tkWidgetKey, tkPath, tkUnknown);

  TRangeState = (rsUnknown, rsAnsi, rsPasStyle, rsCStyle);

type
  TSynTclTkSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSecondKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fOptionsAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fPathAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fSecondKeys: TStrings;
    fTixWords: TStrings;
    fTixKeyAttri: TSynHighlighterAttributes;
    fWidgetWords: TStrings;
    fWidgetKeyAttri: TSynHighlighterAttributes;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure VariableProc;
    procedure PathProc;
    procedure MinusProc;
    procedure SymbolProc;
    procedure SetKeyWords(const Value: TStrings);
    procedure SetSecondKeys(const Value: TStrings);
    function IsKeywordListStored: Boolean;
    function IsSecondKeywordListStored: Boolean;
    function InternalIsKeyword(const AKeyword: string;
        KeyWordList: TStrings; ACaseSensitive: Boolean = False): Boolean;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
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
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property KeyWords: TStrings read fKeyWords write SetKeyWords
      stored IsKeywordListStored;
    property SecondKeyAttri: TSynHighlighterAttributes read fSecondKeyAttri
      write fSecondKeyAttri;
    property SecondKeyWords: TStrings read fSecondKeys write SetSecondKeys
      stored IsSecondKeywordListStored;
    property TixKeyAttri: TSynHighlighterAttributes read fTixKeyAttri
      write fTixKeyAttri;
    property TixWords: TStrings read fTixWords;
    property WidgetKeyAttri: TSynHighlighterAttributes read fWidgetKeyAttri
      write fWidgetKeyAttri;
    property WidgetWords: TStrings read fWidgetWords;

    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property OptionsAttri: TSynHighlighterAttributes read fOptionsAttri
      write fOptionsAttri;
    property PathAttri: TSynHighlighterAttributes read fPathAttri
      write fPathAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  TclTkKeys: array[0..128] of string = (
    'after', 'append', 'array', 'auto_execok', 'auto_import', 'auto_load', 
    'auto_mkindex', 'auto_mkindex_old', 'auto_qualify', 'auto_reset', 'base', 
    'bgerror', 'binary', 'body', 'break', 'catch', 'cd', 'class', 'clock', 
    'close', 'code', 'concat', 'configbody', 'constructor', 'continue', 'dde',
    'delete', 'destructor', 'else', 'elseif', 'encoding', 'ensemble', 'eof', 
    'error', 'eval', 'exec', 'exit', 'expr', 'fblocked', 'fconfigure', 'fcopy', 
    'file', 'fileevent', 'filename', 'find', 'flush', 'for', 'foreach', 
    'format', 'gets', 'glob', 'global', 'history', 'http', 'if', 'incr', 'info', 
    'inherit', 'interp', 'is', 'join', 'lappend', 'lindex', 'linsert', 'list', 
    'llength', 'load', 'local', 'lrange', 'lreplace', 'lsearch', 'lset', 
    'lsort', 'memory', 'method', 'msgcat', 'namespace', 'open', 'package', 
    'parray', 'pid', 'pkg_mkindex', 'private', 'proc', 'protected', 'public', 
    'puts', 'pwd', 're_syntax', 'read', 'regexp', 'registry', 'regsub', 
    'rename', 'resource', 'return', 'safe', 'safebase', 'scan', 'scope', 'seek', 
    'set', 'socket', 'source', 'split', 'string', 'subst', 'switch', 'tcl', 
    'tcl_endofword', 'tcl_findlibrary', 'tcl_startofnextword', 
    'tcl_startofpreviousword', 'tcl_wordbreakafter', 'tcl_wordbreakbefore', 
    'tcltest', 'tclvars', 'tell', 'then', 'time', 'trace', 'unknown', 'unset', 
    'update', 'uplevel', 'upvar', 'variable', 'vwait', 'while' 
  );
   
  SecondTclTkKeys: array[0..91] of string = (
    'bell', 'bind', 'bindidproc', 'bindproc', 'bindtags', 'bitmap', 'button', 
    'canvas', 'checkbutton', 'clipboard', 'colors', 'combobox', 'console', 
    'cursors', 'debug', 'destroy', 'entry', 'event', 'exp_after', 'exp_before', 
    'exp_continue', 'exp_internal', 'exp_send', 'expect', 'focus', 'font', 
    'frame', 'grab', 'grid', 'image', 'interact', 'interpreter', 'keysyms', 
    'label', 'labelframe', 'listbox', 'loadtk', 'log_file', 'log_user', 'lower', 
    'menu', 'menubutton', 'message', 'namespupd', 'option', 'options', 'pack', 
    'panedwindow', 'photo', 'place', 'radiobutton', 'raise', 'rgb', 'scale', 
    'scrollbar', 'selection', 'send', 'send_error', 'send_log', 'send_tty', 
    'send_user', 'sendout', 'sleep', 'spawn', 'spinbox', 'stty', 'text', 'tk', 
    'tk_bisque', 'tk_choosecolor', 'tk_choosedirectory', 'tk_dialog', 
    'tk_focusfollowsmouse', 'tk_focusnext', 'tk_focusprev', 'tk_getopenfile', 
    'tk_getsavefile', 'tk_menusetfocus', 'tk_messagebox', 'tk_optionmenu', 
    'tk_popup', 'tk_setpalette', 'tk_textcopy', 'tk_textcut', 'tk_textpaste', 
    'tkerror', 'tkvars', 'tkwait', 'toplevel', 'wait', 'winfo', 'wm' 
  );

  TixKeys: array[0..43] of string = (
    'compound', 'pixmap', 'tix', 'tixballoon', 'tixbuttonbox', 'tixchecklist', 
    'tixcombobox', 'tixcontrol', 'tixdestroy', 'tixdirlist', 
    'tixdirselectdialog', 'tixdirtree', 'tixdisplaystyle', 'tixexfileselectbox', 
    'tixexfileselectdialog', 'tixfileentry', 'tixfileselectbox', 
    'tixfileselectdialog', 'tixform', 'tixgetboolean', 'tixgetint', 'tixgrid', 
    'tixhlist', 'tixinputonly', 'tixlabelentry', 'tixlabelframe', 
    'tixlistnotebook', 'tixmeter', 'tixmwm', 'tixnbframe', 'tixnotebook', 
    'tixoptionmenu', 'tixpanedwindow', 'tixpopupmenu', 'tixscrolledhlist', 
    'tixscrolledlistbox', 'tixscrolledtext', 'tixscrolledwindow', 'tixselect', 
    'tixstdbuttonbox', 'tixtlist', 'tixtree', 'tixutils', 'tixwish' 
  );
  
  WidgetKeys: array[0..32] of string = (
    'ArrowButton', 'Button', 'ButtonBox', 'BWidget', 'ComboBox', 'Dialog', 
    'DragSite', 'DropSite', 'DynamicHelp', 'Entry', 'Label', 'LabelEntry', 
    'LabelFrame', 'ListBox', 'MainFrame', 'MessageDlg', 'NoteBook', 
    'PagesManager', 'PanedWindow', 'PasswdDlg', 'ProgressBar', 'ProgressDlg', 
    'ScrollableFrame', 'ScrollableWindow', 'ScrolledWindow', 'ScrollView', 
    'SelectColor', 'SelectFont', 'Separator', 'SpinBox', 'TitleFrame', 'Tree', 
    'Widget' 
  );

function TSynTclTkSyn.InternalIsKeyword(const AKeyword: string;
  KeyWordList: TStrings; ACaseSensitive: Boolean = False): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: string;
begin
  First := 0;
  Last := KeyWordList.Count - 1;
  Result := False;
  if ACaseSensitive then
    Token := AKeyword
  else
    Token := SysUtils.AnsiLowerCase(AKeyword);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(KeyWordList[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      Break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end;

function TSynTclTkSyn.IsKeyword(const AKeyword: string): Boolean;
begin
  Result := InternalIsKeyword(AKeyword, fWidgetWords, True) or
    InternalIsKeyword(AKeyword, fTixWords) or
    InternalIsKeyword(AKeyword, fKeyWords) or
    InternalIsKeyword(AKeyword, fSecondKeys);
end;

constructor TSynTclTkSyn.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;
  fSecondKeys := TStringList.Create;
  TStringList(fSecondKeys).Sorted := True;
  TStringList(fSecondKeys).Duplicates := dupIgnore;
  fTixWords := TStringList.Create;
  TStringList(fTixWords).Sorted := True;
  TStringList(fTixWords).Duplicates := dupIgnore;
  fWidgetWords := TStringList.Create;
  TStringList(fWidgetWords).Sorted := True;
  TStringList(fWidgetWords).Duplicates := dupIgnore;
  fKeyWords.BeginUpdate;
  for I := Low(TclTkKeys) to High(TclTkKeys) do
    FKeyWords.Add(TclTkKeys[I]);
  fKeyWords.EndUpdate;
  fSecondKeys.BeginUpdate;
  for I := Low(SecondTclTkKeys) to High(SecondTclTkKeys) do
    fSecondKeys.Add(SecondTclTkKeys[I]);
  fSecondKeys.EndUpdate;
  fTixWords.BeginUpdate;
  for I := Low(TixKeys) to High(TixKeys) do
    FTixWords.Add(TixKeys[I]);
  fTixWords.EndUpdate;
  fWidgetWords.BeginUpdate;
  for I := Low(WidgetKeys) to High(WidgetKeys) do
    FWidgetWords.Add(WidgetKeys[I]);
  fWidgetWords.EndUpdate;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  fSecondKeyAttri.Style := [fsBold];
  AddAttribute(fSecondKeyAttri);

  fTixKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrTixKeyWords, SYNS_FriendlyAttrTixKeyWords);
  fTixKeyAttri.Style := [fsBold, fsItalic];
  AddAttribute(fTixKeyAttri);

  fWidgetKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrWidgetWords, SYNS_FriendlyAttrWidgetWords);
  fWidgetKeyAttri.Style := [fsBold, fsItalic];
  AddAttribute(fWidgetKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);
  fOptionsAttri := TSynHighlighterAttributes.Create(SYNS_AttrOptions, SYNS_FriendlyAttrOptions);
  AddAttribute(fOptionsAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);
  fPathAttri := TSynHighlighterAttributes.Create(SYNS_AttrPath, SYNS_FriendlyAttrPath);
  AddAttribute(fPathAttri);

  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterTclTk;
end;

destructor TSynTclTkSyn.Destroy;
begin
  fWidgetWords.Free;
  fTixWords.Free;
  fSecondKeys.Free;
  fKeyWords.Free;
  inherited Destroy;
end;

procedure TSynTclTkSyn.AnsiProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if fLine[Run] = '*' then
    begin
      if fLine[Run + 1] = ')' then
      begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end
      else
        Inc(Run)
    end
    else
      Inc(Run);
end;

procedure TSynTclTkSyn.PasStyleProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '}' then
    begin
      fRange := rsUnKnown;
      Inc(Run);
      Break;
    end
    else
      Inc(Run);
end;

procedure TSynTclTkSyn.CStyleProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if fLine[Run] = '*' then
    begin
      if fLine[Run + 1] = '/' then
      begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end
      else Inc(Run)
    end
    else
      Inc(Run);
end;

procedure TSynTclTkSyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynTclTkSyn.PointCommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynTclTkSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
    else Inc(Run);
  end;
end;

procedure TSynTclTkSyn.IdentProc;
begin
  while IsIdentChar(fLine[Run]) do Inc(Run);
  if InternalIsKeyword(GetToken, fWidgetWords, True) then
    fTokenId := tkWidgetKey
  else if InternalIsKeyword(GetToken, fTixWords) then
    fTokenId := tkTixKey
  else if InternalIsKeyword(GetToken, fKeyWords) then
    fTokenId := tkKey
  else if InternalIsKeyword(GetToken, fSecondKeys) then
    fTokenId := tkSecondKey
  else
    fTokenId := tkIdentifier;
end;

procedure TSynTclTkSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynTclTkSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynTclTkSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynTclTkSyn.RoundOpenProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynTclTkSyn.SlashProc;
begin
  if FLine[Run] = '#' then
  begin
    fTokenID := tkComment;
    while not IsLineEnd(Run) do Inc(Run);
  end
  else
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynTclTkSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynTclTkSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
    Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then Break;
    Inc(Run);
  until (FLine[Run] = #34) and (FLine[Pred(Run)] <> '\');
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynTclTkSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnKnown;
end;

procedure TSynTclTkSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
    else
      case fLine[Run] of
        '-': MinusProc;
        '#': SlashProc;
        '{': BraceOpenProc;
        ';': PointCommaProc;
        #13: CRProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        #10: LFProc;
        #0: NullProc;
        '0'..'9':  NumberProc;
        '(': RoundOpenProc;
        '/': SlashProc;
        '[', ']', ')', '}': SymbolProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        #34: StringProc;
        '$': VariableProc;
        '.': PathProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynTclTkSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynTclTkSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynTclTkSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynTclTkSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynTclTkSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSecondKey: Result := fSecondKeyAttri;
    tkTixKey: Result := fTixKeyAttri;
    tkWidgetKey: Result := fWidgetKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkOptions: Result := fOptionsAttri;
    tkVariable: Result := fVariableAttri;
    tkPath: Result := fPathAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynTclTkSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynTclTkSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynTclTkSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynTclTkSyn.SetKeyWords(const Value: TStrings);
var
  I: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for I := 0 to Value.Count - 1 do
        Value[I] := SysUtils.AnsiUpperCase(Value[I]);
      Value.EndUpdate;
    end;
  fKeyWords.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynTclTkSyn.SetSecondKeys(const Value: TStrings);
var
  I: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for I := 0 to Value.Count - 1 do
        Value[I] := SysUtils.AnsiUpperCase(Value[I]);
      Value.EndUpdate;
    end;
  fSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynTclTkSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterTclTk;
end;

class function TSynTclTkSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTclTk;
end;

function TSynTclTkSyn.LoadFromRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then
    begin
      if r.ValueExists('KeyWords') then KeyWords.Text := r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynTclTkSyn.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TRegistry;
begin
  r:= TRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key, True) then
    begin
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynTclTkSyn.IsKeywordListStored: Boolean;
var
  Keys: TStringList;
  DefKey: Integer;
  Index: Integer;
begin
  Keys := TStringList.Create;
  try
    Keys.Assign(KeyWords);
    Index := 0;
    for DefKey := Low(TclTkKeys) to High(TclTkKeys) do
    begin
      if not Keys.Find(TclTkKeys[DefKey], Index) then
      begin
        Result := True;
        Exit;
      end;
      Keys.Delete(Index);
    end;
    Result := Keys.Count <> 0;
  finally
    Keys.Free;
  end;
end;

function TSynTclTkSyn.GetSampleSource: string;
begin
  Result :=
    '#!/usr/local/tclsh8.0'#13#10 +
    'if {$argc < 2} {'#13#10 +
    '	puts stderr "Usage: $argv0 parameter"'#13#10 +
    '	exit 1'#13#10 +
    '}';
end;

class function TSynTclTkSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangTclTk;
end;

procedure TSynTclTkSyn.MinusProc;
const
  EmptyChars = [' ', #9, #0, #10, #13];
var
  OK: Boolean;
begin
  OK := False;
  Inc(Run);
  { minus like symbol }
  if CharInSet(fLine[Run], ['0'..'9']) then
    FTokenID := tkSymbol
  else
  { special option -- }
  if (fLine[Run] = '-') and CharInSet(fLine[Run + 1], EmptyChars) then
  begin
    OK := True;
    Inc(Run);
  end
  { normal options -options }
  else begin
    if CharInSet(fLine[Run], ['a'..'z', 'A'..'Z']) then
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['a'..'z', 'A'..'Z']) do
        Inc(Run);
      OK := CharInSet(fLine[Run], EmptyChars);
    end
    { bad option syntax }
    else
      while not CharInSet(FLine[Run], EmptyChars) do
        Inc(Run);
  end;
  if OK then
    FTokenID := tkOptions
  else
    FTokenID := tkUnknown;
end;

procedure TSynTclTkSyn.PathProc;
begin
  if CharInSet(FLine[Run + 1], ['a'..'z', 'A'..'Z']) then
  begin
    fTokenID := tkPath;
    Inc(Run);
    while CharInSet(FLine[Run], ['a'..'z', 'A'..'Z', '0'..'9']) do Inc(Run);
  end
  else
  begin
    FTokenID := tkSymbol;
    Inc(Run);
  end;
end;

procedure TSynTclTkSyn.VariableProc;
begin
  fTokenId := tkVariable;
  Inc(Run);
  while CharInSet(FLine[Run], ['_', '0'..'9', 'A'..'Z', 'a'..'z']) do Inc(Run);
end;

function TSynTclTkSyn.IsSecondKeywordListStored: Boolean;
var
  Keys: TStringList;
  DefKey: Integer;
  Index: Integer;
begin
  Keys := TStringList.Create;
  try
    Keys.Assign(SecondKeyWords);
    Index := 0;
    for DefKey := Low(SecondTclTkKeys) to High(SecondTclTkKeys) do
    begin
      if not Keys.Find(SecondTclTkKeys[DefKey], Index) then
      begin
        Result := True;
        Exit;
      end;
      Keys.Delete(Index);
    end;
    Result := Keys.Count <> 0;
  finally
    Keys.Free;
  end;
end;

procedure TSynTclTkSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
end;

initialization
  RegisterPlaceableHighlighter(TSynTclTkSyn);
end.
