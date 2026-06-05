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
@abstract(Provides an AutoIt v3 language syntax highlighter for SynEdit)
@created(2026-06-05)
}

unit SynHighlighterAutoIt;

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
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkFunction, tkMacro,
    tkVariable, tkPreprocessor, tkNull, tkNumber, tkSpace, tkString, tkSymbol,
    tkUnknown);

  TRangeState = (rsUnknown, rsBlockComment);

  TSynAutoItSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;
    FFunctions: TStringList;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FFunctionAttri: TSynHighlighterAttributes;
    FMacroAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FPreprocessorAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function LineClosesComment: Boolean;
    procedure BlockCommentProc;
    procedure CommentProc;
    procedure CRProc;
    procedure DirectiveProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MacroProc;
    procedure NullProc;
    procedure NumberProc;
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
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property FunctionAttri: TSynHighlighterAttributes read FFunctionAttri write FFunctionAttri;
    property MacroAttri: TSynHighlighterAttributes read FMacroAttri write FMacroAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read FPreprocessorAttri write FPreprocessorAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property UnknownAttri: TSynHighlighterAttributes read FUnknownAttri write FUnknownAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  SYNS_FilterAutoIt = 'AutoIt Files (*.au3)|*.au3';
  SYNS_LangAutoIt = 'AutoIt';
  SYNS_FriendlyLangAutoIt = 'AutoIt';

constructor TSynAutoItSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FKeywords := TStringList.Create;
  FKeywords.CaseSensitive := False;
  FKeywords.Sorted := True;
  FKeywords.Duplicates := dupIgnore;
  FKeywords.CommaText :=
    'And,ByRef,Case,Const,ContinueCase,ContinueLoop,Default,Dim,Do,Else,' +
    'ElseIf,EndFunc,EndIf,EndSelect,EndSwitch,EndWith,Enum,Exit,ExitLoop,' +
    'False,For,Func,Global,If,In,Local,Mod,Next,Not,Null,Or,ReDim,Return,' +
    'Select,Static,Step,Switch,Then,To,True,Until,Volatile,WEnd,While,With';

  FFunctions := TStringList.Create;
  FFunctions.CaseSensitive := False;
  FFunctions.Sorted := True;
  FFunctions.Duplicates := dupIgnore;
  FFunctions.CommaText :=
    'Abs,ACos,AdlibRegister,AdlibUnRegister,Asc,AscW,ASin,Assign,ATan,' +
    'AutoItSetOption,AutoItWinGetTitle,AutoItWinSetTitle,Beep,Binary,' +
    'BinaryLen,BinaryMid,BinaryToString,BitAND,BitNOT,BitOR,BitRotate,' +
    'BitShift,BitXOR,BlockInput,Break,Call,CDTray,Ceiling,Chr,ChrW,ClipGet,' +
    'ClipPut,ConsoleRead,ConsoleWrite,ConsoleWriteError,ControlClick,' +
    'ControlCommand,ControlDisable,ControlEnable,ControlFocus,ControlGetFocus,' +
    'ControlGetHandle,ControlGetPos,ControlGetText,ControlHide,ControlListView,' +
    'ControlMove,ControlSend,ControlSetText,ControlShow,ControlTreeView,Cos,' +
    'Dec,DirCopy,DirCreate,DirGetSize,DirMove,DirRemove,DllCall,' +
    'DllCallAddress,DllCallbackFree,DllCallbackGetPtr,DllCallbackRegister,' +
    'DllClose,DllOpen,DllStructCreate,DllStructGetData,DllStructGetPtr,' +
    'DllStructGetSize,DllStructSetData,DriveGetDrive,DriveGetFileSystem,' +
    'DriveGetLabel,DriveGetSerial,DriveGetType,DriveMapAdd,DriveMapDel,' +
    'DriveMapGet,DriveSetLabel,DriveSpaceFree,DriveSpaceTotal,DriveStatus,' +
    'EnvGet,EnvSet,EnvUpdate,Eval,Execute,Exp,FileChangeDir,FileClose,' +
    'FileCopy,FileCreateNTFSLink,FileCreateShortcut,FileDelete,FileExists,' +
    'FileFindFirstFile,FileFindNextFile,FileFlush,FileGetAttrib,' +
    'FileGetEncoding,FileGetLongName,FileGetPos,FileGetShortcut,' +
    'FileGetShortName,FileGetSize,FileGetTime,FileGetVersion,FileInstall,' +
    'FileMove,FileOpen,FileOpenDialog,FileRead,FileReadLine,FileReadToArray,' +
    'FileRecycle,FileRecycleEmpty,FileSaveDialog,FileSelectFolder,' +
    'FileSetAttrib,FileSetEnd,FileSetPos,FileSetTime,FileWrite,FileWriteLine,' +
    'Floor,FtpSetProxy,FuncName,GUICreate,GUICtrlCreateAvi,GUICtrlCreateButton,' +
    'GUICtrlCreateCheckbox,GUICtrlCreateCombo,GUICtrlCreateContextMenu,' +
    'GUICtrlCreateDate,GUICtrlCreateDummy,GUICtrlCreateEdit,' +
    'GUICtrlCreateGraphic,GUICtrlCreateGroup,GUICtrlCreateIcon,' +
    'GUICtrlCreateInput,GUICtrlCreateLabel,GUICtrlCreateList,' +
    'GUICtrlCreateListView,GUICtrlCreateListViewItem,GUICtrlCreateMenu,' +
    'GUICtrlCreateMenuItem,GUICtrlCreateObj,GUICtrlCreatePic,' +
    'GUICtrlCreateProgress,GUICtrlCreateRadio,GUICtrlCreateSlider,' +
    'GUICtrlCreateTab,GUICtrlCreateTabItem,GUICtrlCreateTreeView,' +
    'GUICtrlCreateTreeViewItem,GUICtrlCreateUpdown,GUICtrlDelete,' +
    'GUICtrlGetHandle,GUICtrlGetState,GUICtrlRead,GUICtrlRecvMsg,' +
    'GUICtrlRegisterListViewSort,GUICtrlSendMsg,GUICtrlSendToDummy,' +
    'GUICtrlSetBkColor,GUICtrlSetColor,GUICtrlSetCursor,GUICtrlSetData,' +
    'GUICtrlSetDefBkColor,GUICtrlSetDefColor,GUICtrlSetFont,GUICtrlSetGraphic,' +
    'GUICtrlSetImage,GUICtrlSetLimit,GUICtrlSetOnEvent,GUICtrlSetPos,' +
    'GUICtrlSetResizing,GUICtrlSetState,GUICtrlSetStyle,GUICtrlSetTip,' +
    'GUIDelete,GUIGetCursorInfo,GUIGetMsg,GUIGetStyle,GUIRegisterMsg,' +
    'GUISetAccelerators,GUISetBkColor,GUISetCoord,GUISetCursor,GUISetFont,' +
    'GUISetHelp,GUISetIcon,GUISetOnEvent,GUISetState,GUISetStyle,GUIStartGroup,' +
    'GUISwitch,Hex,HotKeySet,HttpSetProxy,HttpSetUserAgent,HWnd,InetClose,' +
    'InetGet,InetGetInfo,InetGetSize,InetRead,IniDelete,IniRead,' +
    'IniReadSection,IniReadSectionNames,IniRenameSection,IniWrite,' +
    'IniWriteSection,InputBox,Int,IsAdmin,IsArray,IsBinary,IsBool,IsDeclared,' +
    'IsDllStruct,IsFloat,IsFunc,IsHWnd,IsInt,IsKeyword,IsMap,IsNumber,IsObj,' +
    'IsPtr,IsString,Log,MapAppend,MapExists,MapKeys,MapRemove,MemGetStats,' +
    'MouseClick,MouseClickDrag,MouseDown,MouseGetCursor,MouseGetPos,MouseMove,' +
    'MouseUp,MouseWheel,MsgBox,Number,ObjCreate,ObjCreateInterface,ObjEvent,' +
    'ObjGet,ObjName,OnAutoItExitRegister,OnAutoItExitUnRegister,Ping,' +
    'PixelChecksum,PixelGetColor,PixelSearch,ProcessClose,ProcessExists,' +
    'ProcessGetStats,ProcessList,ProcessSetPriority,ProcessWait,' +
    'ProcessWaitClose,ProgressOff,ProgressOn,ProgressSet,Ptr,Random,RegDelete,' +
    'RegEnumKey,RegEnumVal,RegRead,RegWrite,Round,Run,RunAs,RunAsWait,RunWait,' +
    'Send,SendKeepActive,SetError,SetExtended,ShellExecute,ShellExecuteWait,' +
    'Shutdown,Sin,Sleep,SoundPlay,SoundSetWaveVolume,SplashImageOn,SplashOff,' +
    'SplashTextOn,Sqrt,SRandom,StatusbarGetText,StderrRead,StdinWrite,' +
    'StdioClose,StdoutRead,String,StringAddCR,StringCompare,StringFormat,' +
    'StringFromASCIIArray,StringInStr,StringIsAlNum,StringIsAlpha,StringIsASCII,' +
    'StringIsDigit,StringIsFloat,StringIsInt,StringIsLower,StringIsSpace,' +
    'StringIsUpper,StringIsXDigit,StringLeft,StringLen,StringLower,StringMid,' +
    'StringRegExp,StringRegExpReplace,StringReplace,StringReverse,StringRight,' +
    'StringSplit,StringStripCR,StringStripWS,StringToASCIIArray,' +
    'StringToBinary,StringTrimLeft,StringTrimRight,StringUpper,Tan,TCPAccept,' +
    'TCPCloseSocket,TCPConnect,TCPListen,TCPNameToIP,TCPRecv,TCPSend,' +
    'TCPShutdown,TCPStartup,TimerDiff,TimerInit,ToolTip,TrayCreateItem,' +
    'TrayCreateMenu,TrayGetMsg,TrayItemDelete,TrayItemGetHandle,' +
    'TrayItemGetState,TrayItemGetText,TrayItemSetOnEvent,TrayItemSetState,' +
    'TrayItemSetText,TraySetClick,TraySetIcon,TraySetOnEvent,TraySetPauseIcon,' +
    'TraySetState,TraySetToolTip,TrayTip,UBound,UDPBind,UDPCloseSocket,UDPOpen,' +
    'UDPRecv,UDPSend,UDPShutdown,UDPStartup,VarGetType,WinActivate,WinActive,' +
    'WinClose,WinExists,WinFlash,WinGetCaretPos,WinGetClassList,' +
    'WinGetClientSize,WinGetHandle,WinGetPos,WinGetProcess,WinGetState,' +
    'WinGetText,WinGetTitle,WinKill,WinList,WinMenuSelectItem,WinMinimizeAll,' +
    'WinMinimizeAllUndo,WinMove,WinSetOnTop,WinSetState,WinSetTitle,' +
    'WinSetTrans,WinWait,WinWaitActive,WinWaitClose,WinWaitNotActive';

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

  FFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  FFunctionAttri.Foreground := clTeal;
  AddAttribute(FFunctionAttri);

  FMacroAttri := TSynHighlighterAttributes.Create(SYNS_AttrMacro, SYNS_FriendlyAttrMacro);
  FMacroAttri.Foreground := clOlive;
  AddAttribute(FMacroAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  FVariableAttri.Foreground := clMaroon;
  AddAttribute(FVariableAttri);

  FPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FPreprocessorAttri.Foreground := clTeal;
  FPreprocessorAttri.Style := [fsBold];
  AddAttribute(FPreprocessorAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clPurple;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsBold];
  AddAttribute(FSymbolAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  FUnknownAttri.Foreground := clRed;
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter := SYNS_FilterAutoIt;
  FRange := rsUnknown;
end;

destructor TSynAutoItSyn.Destroy;
begin
  FFunctions.Free;
  FKeywords.Free;
  inherited Destroy;
end;

function TSynAutoItSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  SetString(S, MayBe, Run - FTokenPos);
  if FKeywords.IndexOf(S) >= 0 then
    Result := tkKey
  else if FFunctions.IndexOf(S) >= 0 then
    Result := tkFunction
  else
    Result := tkIdentifier;
end;

function TSynAutoItSyn.LineClosesComment: Boolean;
var
  P, WStart: Integer;
  W: string;
begin
  Result := False;
  P := Run;
  while CharInSet(FLine[P], [#9, ' ']) do
    Inc(P);
  if FLine[P] <> '#' then
    Exit;
  Inc(P);
  WStart := P;
  while IsIdentChar(FLine[P]) or (FLine[P] = '-') do
    Inc(P);
  SetString(W, FLine + WStart, P - WStart);
  Result := SameText(W, 'ce') or SameText(W, 'comments-end');
end;

procedure TSynAutoItSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynAutoItSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynAutoItSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynAutoItSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until not IsWhiteChar(FLine[Run]);
end;

procedure TSynAutoItSyn.CommentProc;
begin
  FTokenID := tkComment;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynAutoItSyn.DirectiveProc;
var
  WStart: Integer;
  Dir: string;
begin
  Inc(Run);
  WStart := Run;
  while IsIdentChar(FLine[Run]) or (FLine[Run] = '-') do
    Inc(Run);
  SetString(Dir, FLine + WStart, Run - WStart);

  if SameText(Dir, 'cs') or SameText(Dir, 'comments-start') then
  begin
    FRange := rsBlockComment;
    FTokenID := tkComment;
    while not CharInSet(FLine[Run], [#0, #10, #13]) do
      Inc(Run);
  end
  else if SameText(Dir, 'ce') or SameText(Dir, 'comments-end') then
  begin
    FTokenID := tkComment;
    while not CharInSet(FLine[Run], [#0, #10, #13]) do
      Inc(Run);
  end
  else
  begin
    FTokenID := tkPreprocessor;
    while not CharInSet(FLine[Run], [#0, #10, #13]) do
      Inc(Run);
  end;
end;

procedure TSynAutoItSyn.BlockCommentProc;
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

  if LineClosesComment then
    FRange := rsUnknown;
  while not CharInSet(FLine[Run], [#0, #10, #13]) do
    Inc(Run);
end;

procedure TSynAutoItSyn.IdentProc;
begin
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
  FTokenID := IdentKind(FLine + FTokenPos);
end;

procedure TSynAutoItSyn.VariableProc;
begin
  FTokenID := tkVariable;
  Inc(Run);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynAutoItSyn.MacroProc;
begin
  FTokenID := tkMacro;
  Inc(Run);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynAutoItSyn.NumberProc;
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
  end;
end;

procedure TSynAutoItSyn.StringProc;
var
  Quote: WideChar;
begin
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

procedure TSynAutoItSyn.SymbolProc;
begin
  FTokenID := tkSymbol;
  case FLine[Run] of
    '=': if FLine[Run + 1] = '=' then Inc(Run);
    '<': if CharInSet(FLine[Run + 1], ['=', '>']) then Inc(Run);
    '>': if FLine[Run + 1] = '=' then Inc(Run);
    '+', '-', '*', '/', '&', '^':
      if FLine[Run + 1] = '=' then Inc(Run);
  end;
  Inc(Run);
end;

procedure TSynAutoItSyn.UnknownProc;
begin
  FTokenID := tkUnknown;
  Inc(Run);
end;

function TSynAutoItSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynAutoItSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

class function TSynAutoItSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangAutoIt;
end;

class function TSynAutoItSyn.GetLanguageName: string;
begin
  Result := SYNS_LangAutoIt;
end;

function TSynAutoItSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynAutoItSyn.GetSampleSource: string;
begin
  Result :=
    '#include <MsgBoxConstants.au3>'#13#10 +
    '#cs'#13#10 +
    '    A block comment that spans'#13#10 +
    '    several lines.'#13#10 +
    '#ce'#13#10 +
    ''#13#10 +
    '; Greet the current user'#13#10 +
    'Func Greet($sName)'#13#10 +
    '    Local $iCount = 0x10'#13#10 +
    '    If $sName == "" Then'#13#10 +
    '        $sName = @UserName'#13#10 +
    '    EndIf'#13#10 +
    '    Return StringFormat("Hi %s!" & @CRLF, $sName)'#13#10 +
    'EndFunc'#13#10 +
    ''#13#10 +
    'MsgBox($MB_OK, "Demo", Greet("World"), 3.5)';
end;

function TSynAutoItSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkFunction: Result := FFunctionAttri;
    tkMacro: Result := FMacroAttri;
    tkVariable: Result := FVariableAttri;
    tkPreprocessor: Result := FPreprocessorAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynAutoItSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynAutoItSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynAutoItSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterAutoIt;
end;

function TSynAutoItSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or (AChar = '_');
end;

procedure TSynAutoItSyn.Next;
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
      ';': CommentProc;
      '#': DirectiveProc;
      '$': VariableProc;
      '@': MacroProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0'..'9': NumberProc;
      '.':
        if CharInSet(FLine[Run + 1], ['0'..'9']) then
          NumberProc
        else
          SymbolProc;
      '"', '''': StringProc;
      '+', '-', '*', '/', '^', '&', '=', '<', '>', '?', ':', ',',
      '(', ')', '[', ']': SymbolProc;
    else
      UnknownProc;
    end;
  end;
  inherited;
end;

procedure TSynAutoItSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynAutoItSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

initialization
  RegisterPlaceableHighlighter(TSynAutoItSyn);

end.
