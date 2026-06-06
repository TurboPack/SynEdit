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
@abstract(Provides an NSIS (Nullsoft Scriptable Install System) highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterNSIS;

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
  TtkTokenKind = (tkComment, tkConstant, tkIdentifier, tkKey, tkNull, tkNumber,
    tkPredefined, tkPreprocessor, tkSpace, tkString, tkSymbol, tkUnknown,
    tkVariable);

  TRangeState = (rsUnknown, rsBlockComment);

  TSynNSISSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FCommands: TStringList;
    FPredefined: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FConstantAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FPredefinedAttri: TSynHighlighterAttributes;
    FPreprocessorAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure BangProc;
    procedure BlockCommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LineCommentProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure VariableProc;
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
    property ConstantAttri: TSynHighlighterAttributes read FConstantAttri write FConstantAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property PredefinedAttri: TSynHighlighterAttributes read FPredefinedAttri write FPredefinedAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read FPreprocessorAttri write FPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterNSIS = 'NSIS Script Files (*.nsi;*.nsh)|*.nsi;*.nsh';
  SYNS_LangNSIS = 'NSIS';
  SYNS_FriendlyLangNSIS = 'NSIS Script';

  Commands: string =
    'abort,addbrandingimage,addsize,allowrootdirinstall,allowskipfiles,' +
    'autoclosewindow,bgfont,bggradient,brandingtext,bringtofront,call,' +
    'callinstdll,caption,changeui,checkbitmap,clearerrors,completedtext,' +
    'componenttext,copyfiles,crccheck,createdirectory,createfont,' +
    'createshortcut,delete,deleteinifile,deleteinisec,deleteinistr,' +
    'deleteregkey,deleteregvalue,detailprint,detailsbuttontext,dirtext,' +
    'dirvar,dirverify,enablewindow,enumregkey,enumregvalue,exch,exec,' +
    'execshell,execshellwait,execwait,expandenvstrings,fileerrortext,' +
    'filebufsize,file,fileclose,fileopen,fileread,filereadbyte,' +
    'filereadutf16le,fileseek,filewrite,filewritebyte,filewriteutf16le,' +
    'findclose,findfirst,findnext,findwindow,flushini,function,functionend,' +
    'getcuraddress,getcurinsttype,getdiskfreespace,getdiskfreespaceex,' +
    'getdlgitem,getdllversion,getdllversionlocal,geterrorlevel,getfiletime,' +
    'getfiletimelocal,getfullpathname,getfunctionaddress,getinstdirerror,' +
    'getlabeladdress,gettempfilename,goto,hidewindow,icon,ifabort,iferrors,' +
    'iffileexists,ifrebootflag,ifshellvarcontextall,ifsilent,' +
    'installbuttontext,installcolors,installdir,installdirregkey,' +
    'instprogressflags,insttype,insttypegettext,insttypesettext,int64cmp,' +
    'int64cmpu,int64fmt,intcmp,intcmpu,intfmt,intop,intptrcmp,intptrcmpu,' +
    'intptrop,languagestring,licensebkcolor,licensedata,' +
    'licenseforceselection,licenselangstring,licensetext,loadlanguagefile,' +
    'lockwindow,logset,logtext,manifestdpiaware,manifestsupportedos,' +
    'messagebox,miscbuttontext,name,nop,outfile,page,pagecallbacks,pageex,' +
    'pageexend,pop,push,quit,readenvstr,readinistr,readregdword,readregstr,' +
    'reboot,regdll,rename,requestexecutionlevel,reservefile,return,rmdir,' +
    'searchpath,section,sectionend,sectiongetflags,sectiongetinsttypes,' +
    'sectiongetsize,sectiongettext,sectiongroup,sectiongroupend,sectionin,' +
    'sectionsetflags,sectionsetinsttypes,sectionsetsize,sectionsettext,' +
    'sendmessage,setautoclose,setbrandingimage,setcompress,setcompressor,' +
    'setcompressordictsize,setctlcolors,setcurinsttype,setdatablockoptimize,' +
    'setdatesave,setdetailsprint,setdetailsview,seterrorlevel,seterrors,' +
    'setfileattributes,setfont,setoutpath,setoverwrite,setpluginunload,' +
    'setregview,setrebootflag,setshellvarcontext,setsilent,showinstdetails,' +
    'showuninstdetails,showwindow,silentinstall,silentuninstall,sleep,' +
    'spacetexts,strcmp,strcmps,strcpy,strlen,subcaption,subsection,' +
    'subsectionend,target,unicode,uninstallbuttontext,uninstallcaption,' +
    'uninstallicon,uninstallsubcaption,uninstalltext,uninstpage,unregdll,' +
    'var,viaddversionkey,vifileversion,viproductversion,windowicon,writeini,' +
    'writeinistr,writeregbin,writeregdword,writeregexpandstr,writeregmultistr,' +
    'writeregnone,writeregstr,writeuninstaller,xpstyle';

  Predefined: string =
    'mb_ok,mb_okcancel,mb_abortretryignore,mb_retrycancel,mb_yesno,' +
    'mb_yesnocancel,mb_iconexclamation,mb_iconinformation,mb_iconquestion,' +
    'mb_iconstop,mb_usericon,mb_topmost,mb_setforeground,mb_right,' +
    'mb_rtlreading,mb_defbutton1,mb_defbutton2,mb_defbutton3,mb_defbutton4,' +
    'idabort,idcancel,idignore,idno,idok,idretry,idyes,' +
    'hkcr,hklm,hkcu,hku,hkcc,hkdd,hkpd,shctx,hkey_classes_root,' +
    'hkey_local_machine,hkey_current_user,hkey_users,hkey_current_config,' +
    'hkey_dyn_data,hkey_performance_data,' +
    'sw_hide,sw_show,sw_shownormal,sw_showminimized,sw_showmaximized,' +
    'sw_showminnoactive,sw_showna,sw_shownoactivate,sw_minimize,sw_restore,' +
    'normal,archive,hidden,offline,readonly,system,temporary,' +
    'all,current,true,false,on,off,none,auto,force,show,hide,both,textonly,' +
    'listonly,leave,ifnewer,ifdiff,lastused,alwaysoff,bold,ro,user,highest,' +
    'admin,zlib,bzip2,lzma,silent,silentlog,colored,smooth,' +
    '.oninit,.onguiinit,.onguiend,.oninstsuccess,.oninstfailed,.onuserabort,' +
    '.onverifyinstdir,.onselchange,.onmouseoversection,.onrebootfailed,' +
    'un.oninit,un.onguiinit,un.onguiend,un.onuninstsuccess,un.onuninstfailed,' +
    'un.onuserabort,un.onrebootfailed';

constructor TSynNSISSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCommands := TStringList.Create;
  FCommands.CaseSensitive := False;
  FCommands.Sorted := True;
  FCommands.Duplicates := dupIgnore;
  FCommands.CommaText := Commands;

  FPredefined := TStringList.Create;
  FPredefined.CaseSensitive := False;
  FPredefined.Sorted := True;
  FPredefined.Duplicates := dupIgnore;
  FPredefined.CommaText := Predefined;

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Foreground := clNavy;
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FPreprocessorAttri.Foreground := clPurple;
  FPreprocessorAttri.Style := [fsBold];
  AddAttribute(FPreprocessorAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clMaroon;
  AddAttribute(FVariableAttri);

  FConstantAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro, SYNS_FriendlyAttrMacro);
  FConstantAttri.Foreground := clTeal;
  AddAttribute(FConstantAttri);

  FPredefinedAttri := TSynHighlighterAttributes.Create(SYNS_AttrSystem, SYNS_FriendlyAttrSystem);
  FPredefinedAttri.Foreground := clOlive;
  AddAttribute(FPredefinedAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterNSIS;
  FRange := rsUnknown;
end;

destructor TSynNSISSyn.Destroy;
begin
  FPredefined.Free;
  FCommands.Free;
  inherited Destroy;
end;

function TSynNSISSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_') or (AChar = '.');
end;

function TSynNSISSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FCommands.IndexOf(S) >= 0 then
    Result := tkKey
  else if FPredefined.IndexOf(S) >= 0 then
    Result := tkPredefined
  else
    Result := tkIdentifier;
end;

procedure TSynNSISSyn.BlockCommentProc;
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
    end;
    Inc(Run);
  end;
end;

procedure TSynNSISSyn.BangProc;
begin
  // '!define', '!include', ... compile-time commands. A bare '!' (e.g. as a
  // logical-not operator in an !if expression) is treated as a symbol.
  if IsCharAlpha(FLine[Run + 1]) then
  begin
    FTokenID := tkPreprocessor;
    Inc(Run);
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
  end
  else
    SymbolProc;
end;

procedure TSynNSISSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynNSISSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynNSISSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynNSISSyn.LineCommentProc;
begin
  // ';' and '#' begin a comment that runs to the end of the line.
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until CharInSet(FLine[Run], [#0, #10, #13]);
end;

procedure TSynNSISSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynNSISSyn.NumberProc;
begin
  FTokenID := tkNumber;
  if (FLine[Run] = '0') and CharInSet(FLine[Run + 1], ['x', 'X']) then
  begin
    Inc(Run, 2);
    while CharInSet(FLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
      Inc(Run);
  end
  else
  begin
    while CharInSet(FLine[Run], ['0'..'9']) do
      Inc(Run);
    // dotted version numbers, e.g. 1.2.3.4
    while (FLine[Run] = '.') and CharInSet(FLine[Run + 1], ['0'..'9']) do
    begin
      Inc(Run);
      while CharInSet(FLine[Run], ['0'..'9']) do
        Inc(Run);
    end;
  end;
end;

procedure TSynNSISSyn.SlashProc;
begin
  if FLine[Run + 1] = '*' then
  begin
    FRange := rsBlockComment;
    Inc(Run, 2);
    BlockCommentProc;
  end
  else
    SymbolProc;
end;

procedure TSynNSISSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not CharInSet(FLine[Run], [#1..#9, #11, #12, #14..#32]);
end;

procedure TSynNSISSyn.StringProc;
var
  Quote: WideChar;
begin
  FTokenID := tkString;
  Quote := FLine[Run];
  Inc(Run);
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
  begin
    if FLine[Run] = '$' then
    begin
      // '$\X' is an escape sequence; '$$' is a literal '$'.
      if FLine[Run + 1] = '\' then
      begin
        Inc(Run, 2);
        if not CharInSet(FLine[Run], [#0, #10, #13]) then
          Inc(Run);
      end
      else if FLine[Run + 1] = '$' then
        Inc(Run, 2)
      else
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

procedure TSynNSISSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '<', '>', '=': if FLine[Run + 1] = '=' then Inc(Run);
    '&': if FLine[Run + 1] = '&' then Inc(Run);
    '|': if FLine[Run + 1] = '|' then Inc(Run);
  end;
  Inc(Run);
end;

procedure TSynNSISSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

procedure TSynNSISSyn.VariableProc;
begin
  Inc(Run); // '$'
  case FLine[Run] of
    '{':
      begin
        // ${Define} or LogicLib macro such as ${If}
        FTokenID := tkConstant;
        Inc(Run);
        while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> '}') do
          Inc(Run);
        if FLine[Run] = '}' then
          Inc(Run);
      end;
    '(':
      begin
        // $(LangString)
        FTokenID := tkConstant;
        Inc(Run);
        while not CharInSet(FLine[Run], [#0, #10, #13]) and (FLine[Run] <> ')') do
          Inc(Run);
        if FLine[Run] = ')' then
          Inc(Run);
      end;
    '\':
      begin
        // '$\X' escape encountered outside a string
        FTokenID := tkVariable;
        Inc(Run);
        if not CharInSet(FLine[Run], [#0, #10, #13]) then
          Inc(Run);
      end;
    '$':
      begin
        FTokenID := tkVariable;
        Inc(Run);
      end;
  else
    FTokenID := tkVariable;
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
  end;
end;

procedure TSynNSISSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsBlockComment: BlockCommentProc;
  else
    case FLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      ';', '#': LineCommentProc;
      '/': SlashProc;
      '"', '''', '`': StringProc;
      '$': VariableProc;
      '!': BangProc;
      '0'..'9': NumberProc;
      'A'..'Z', 'a'..'z', '_', '.': IdentProc;
      '<', '>', '=', '&', '|', '(', ')', '[', ']', '{', '}', '\', '+', '-',
      '*', '%', '^', '~', ',', ':', '?': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

function TSynNSISSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynNSISSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynNSISSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangNSIS;
end;

class function TSynNSISSyn.GetLanguageName: string;
begin
  Result := SYNS_LangNSIS;
end;

function TSynNSISSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynNSISSyn.GetSampleSource: string;
begin
  Result :=
    '; sample.nsi'#13#10 +
    '!define APP "My App"'#13#10 +
    '!include "MUI2.nsh"'#13#10 +
    ''#13#10 +
    'Name "${APP}"'#13#10 +
    'OutFile "setup.exe"'#13#10 +
    'InstallDir "$PROGRAMFILES\${APP}"'#13#10 +
    'RequestExecutionLevel admin'#13#10 +
    ''#13#10 +
    'Section "Install" SecMain'#13#10 +
    '  SetOutPath "$INSTDIR"'#13#10 +
    '  File "myapp.exe"'#13#10 +
    '  WriteRegStr HKLM "Software\${APP}" "Path" "$INSTDIR"'#13#10 +
    '  /* a block comment */'#13#10 +
    '  MessageBox MB_OK "Installed to $INSTDIR$\n"'#13#10 +
    'SectionEnd'#13#10 +
    ''#13#10 +
    'Function .onInit'#13#10 +
    '  StrCpy $0 0x1F'#13#10 +
    'FunctionEnd';
end;

function TSynNSISSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkConstant: Result := FConstantAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkPredefined: Result := FPredefinedAttri;
    tkPreprocessor: Result := FPreprocessorAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynNSISSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynNSISSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynNSISSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterNSIS;
end;

procedure TSynNSISSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynNSISSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynNSISSyn);

end.
