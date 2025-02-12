{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
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
// TODO: introduce friendly names for the Commands (EditorCommandStrs is not good enough for end-users)

unit SynEditKeyCmds;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Menus,
  SynUnicode,
  SynEditTypes;

const
  //****************************************************************************
  // NOTE!  If you add an editor command, you must also update the
  //    EditorCommandStrs constant array in implementation section below, or the
  //    command will not show up in the IDE.
  //
  //****************************************************************************

  // "Editor Commands".  Key strokes are translated from a table into these
  // I used constants instead of a set so that additional commands could be
  // added in descendants (you can't extend a set)

  // There are two ranges of editor commands: the ecViewXXX commands are always
  // valid, while the ecEditXXX commands are ignored when the editor is in
  // read-only mode

  ecNone             =    0; // Nothing. Useful for user event to handle command
  ecViewCommandFirst =    0;
  ecViewCommandLast  =  500;
  ecEditCommandFirst =  501;
  ecEditCommandLast  = 1000;

  ecLeft            = 1;    // Move cursor left one char
  ecRight           = 2;    // Move cursor right one char
  ecUp              = 3;    // Move cursor up one line
  ecDown            = 4;    // Move cursor down one line
  ecWordLeft        = 5;    // Move cursor left one word
  ecWordRight       = 6;    // Move cursor right one word
  ecLineStart       = 7;    // Move cursor to beginning of line
  ecLineEnd         = 8;    // Move cursor to end of line
  ecPageUp          = 9;    // Move cursor up one page
  ecPageDown        = 10;   // Move cursor down one page
  ecPageLeft        = 11;   // Move cursor right one page
  ecPageRight       = 12;   // Move cursor left one page
  ecPageTop         = 13;   // Move cursor to top of page
  ecPageBottom      = 14;   // Move cursor to bottom of page
  ecEditorTop       = 15;   // Move cursor to absolute beginning
  ecEditorBottom    = 16;   // Move cursor to absolute end
  ecGotoXY          = 17;   // Move cursor to specific coordinates, Data = PPoint

  ecSelection       = 100;  // Add this to ecXXX command to get equivalent
                            // command, but with selection enabled. This is not
                            // a command itself.
  // Same as commands above, except they affect selection, too
  ecSelLeft         = ecLeft + ecSelection;
  ecSelRight        = ecRight + ecSelection;
  ecSelUp           = ecUp + ecSelection;
  ecSelDown         = ecDown + ecSelection;
  ecSelWordLeft     = ecWordLeft + ecSelection;
  ecSelWordRight    = ecWordRight + ecSelection;
  ecSelLineStart    = ecLineStart + ecSelection;
  ecSelLineEnd      = ecLineEnd + ecSelection;
  ecSelPageUp       = ecPageUp + ecSelection;
  ecSelPageDown     = ecPageDown + ecSelection;
  ecSelPageLeft     = ecPageLeft + ecSelection;
  ecSelPageRight    = ecPageRight + ecSelection;
  ecSelPageTop      = ecPageTop + ecSelection;
  ecSelPageBottom   = ecPageBottom + ecSelection;
  ecSelEditorTop    = ecEditorTop + ecSelection;
  ecSelEditorBottom = ecEditorBottom + ecSelection;
  ecSelGotoXY       = ecGotoXY + ecSelection;  // Data = PPoint

  ecSelWord         = 198;
  ecSelectAll       = 199;  // Select entire contents of editor, cursor to end

  ecCopy            = 201;  // Copy selection to clipboard

  ecScrollUp        = 211;  // Scroll up one line leaving cursor position unchanged.
  ecScrollDown      = 212;  // Scroll down one line leaving cursor position unchanged.
  ecScrollLeft      = 213;  // Scroll left one char leaving cursor position unchanged.
  ecScrollRight     = 214;  // Scroll right one char leaving cursor position unchanged.

  ecInsertMode      = 221;  // Set insert mode
  ecOverwriteMode   = 222;  // Set overwrite mode
  ecToggleMode      = 223;  // Toggle ins/ovr mode

  ecMatchBracket    = 250;  // Go to matching bracket
  ecSelMatchBracket = 251;  // Select to matching bracket

  ecCommentBlock    = 260;  // Comment Block

  ecNextChange      = 270;  // Goto next change.
  ecPreviousChange  = 271;  // Goto previous change.

  ecGotoMarker0     = 301;  // Goto marker
  ecGotoMarker1     = 302;  // Goto marker
  ecGotoMarker2     = 303;  // Goto marker
  ecGotoMarker3     = 304;  // Goto marker
  ecGotoMarker4     = 305;  // Goto marker
  ecGotoMarker5     = 306;  // Goto marker
  ecGotoMarker6     = 307;  // Goto marker
  ecGotoMarker7     = 308;  // Goto marker
  ecGotoMarker8     = 309;  // Goto marker
  ecGotoMarker9     = 310;  // Goto marker
  ecSetMarker0      = 351;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker1      = 352;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker2      = 353;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker3      = 354;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker4      = 355;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker5      = 356;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker6      = 357;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker7      = 358;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker8      = 359;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker9      = 360;  // Set marker, Data = PPoint - X, Y Pos

  ecZoomIn          = 401;
  ecZoomOut         = 402;
  ecZoomReset       = 403;

  ecGotFocus        = 480;
  ecLostFocus       = 481;

  ecContextHelp     = 490;  // Help on Word, Data = Word

  ecDeleteLastChar  = 501;  // Delete last char (i.e. backspace key)
  ecDeleteChar      = 502;  // Delete char at cursor (i.e. delete key)
  ecDeleteWord      = 503;  // Delete from cursor to end of word
  ecDeleteLastWord  = 504;  // Delete from cursor to start of word
  ecDeleteBOL       = 505;  // Delete from cursor to beginning of line
  ecDeleteEOL       = 506;  // Delete from cursor to end of line
  ecDeleteLine      = 507;  // Delete current line
  ecClearAll        = 508;  // Delete everything
  ecLineBreak       = 509;  // Break line at current position, move caret to new line
  ecInsertLine      = 510;  // Break line at current position, leave caret
  ecChar            = 511;  // Insert a character at current position

  ecImeStr          = 550;  // Insert character(s) from IME

  ecUndo            = 601;  // Perform undo if available
  ecRedo            = 602;  // Perform redo if available
  ecCut             = 603;  // Cut selection to clipboard
  ecPaste           = 604;  // Paste clipboard to current position

  ecBlockIndent     = 610;  // Indent selection
  ecBlockUnindent   = 611;  // Unindent selection
  ecTab             = 612;  // Tab key
  ecShiftTab        = 613;  // Shift+Tab key

  ecUpperCase       = 620; // apply to the current selction or word
  ecLowerCase       = 621;
  ecToggleCase      = 622;
  ecTitleCase       = 623;

  ecString          = 630;  //Insert a whole string

  ecAutoCompletion  = 650;

  ecCopyLineUp      = 661;
  ecCopyLineDown    = 662;
  ecMoveLineUp      = 663;
  ecMoveLineDown    = 664;

  // CodeFolding
  ecFoldAll         = 701;
  ecUnfoldAll       = 702;
  ecFoldNearest     = 705;
  ecUnfoldNearest   = 706;
  ecFoldLevel1      = 711;
  ecFoldLevel2      = 712;
  ecFoldLevel3      = 713;
  ecUnfoldLevel1    = 721;
  ecUnfoldLevel2    = 722;
  ecUnfoldLevel3    = 723;
  ecFoldRegions     = 731;
  ecUnfoldRegions   = 732;

  // Multi-caret
  ecSelectColumn       = 150;

  ecCancelSelections   = 800;
  ecDeleteSelections   = 801; // Internal command
  ecSelColumnLeft      = ecLeft + ecSelectColumn;
  ecSelColumnRight     = ecRight + ecSelectColumn;
  ecSelColumnUp        = ecUp + ecSelectColumn;
  ecSelColumnDown      = ecDown + ecSelectColumn;
  ecSelColumnPageUp    = ecPageUp + ecSelectColumn;
  ecSelColumnPageDown  = ecPageDown + ecSelectColumn;
  ecSelMatchingText    = 802;
  ecCaretsAtLineEnds   = 803;

  ecUserFirst       = 1001; // Start of user-defined commands

type
  ESynKeyError = class(Exception);

  TSynCommandKind = (ckStandard, ckSingleCaret, ckMultiCaret);

  TSynCommandInfo = record
    CommandKind: TSynCommandKind;
    StoreMultiCaret: Boolean;
    constructor Create(ACommandKind: TSynCommandKind; AStoreMultiCaret: Boolean);
  end;

  TSynEditKeyStroke = class(TCollectionItem)
  private
    FKey: word;          // Virtual keycode, i.e. VK_xxx
    FShift: TShiftState;
    FKey2: word;
    FShift2: TShiftState;
    FCommand: TSynEditorCommand;
    function GetShortCut: TShortCut;
    function GetShortCut2: TShortCut;
    procedure SetCommand(const Value: TSynEditorCommand);
    procedure SetKey(const Value: word);
    procedure SetKey2(const Value: word);
    procedure SetShift(const Value: TShiftState);
    procedure SetShift2(const Value: TShiftState);
    procedure SetShortCut(const Value: TShortCut);
    procedure SetShortCut2(const Value: TShortCut);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    // No duplicate checking is done if assignment made via these properties!
    property Key: word read FKey write SetKey;
    property Key2: word read FKey2 write SetKey2;
    property Shift: TShiftState read FShift write SetShift;
    property Shift2: TShiftState read FShift2 write SetShift2;
  published
    property Command: TSynEditorCommand read FCommand write SetCommand;
    property ShortCut: TShortCut read GetShortCut write SetShortCut
      default 0;
    property ShortCut2: TShortCut read GetShortCut2 write SetShortCut2
      default 0;
  end;

  TSynEditKeyStrokes = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TSynEditKeyStroke;
    procedure SetItem(Index: Integer; Value: TSynEditKeyStroke);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSynEditKeyStroke;
    procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
       const AShift: TShiftState; const AKey2: word = 0;
       const AShift2: TShiftState = []);
    function FindCommand(Cmd: TSynEditorCommand): Integer;
    function FindKeycode(Code: word; SS: TShiftState): Integer;
    function FindKeycode2(Code1: word; SS1: TShiftState;
      Code2: word; SS2: TShiftState): Integer;
    function FindShortcut(SC: TShortcut): Integer;
    function FindShortcut2(SC, SC2: TShortcut): Integer;
    procedure LoadFromStream(AStream: TStream);
    procedure ResetDefaults;
    procedure SaveToStream(AStream: TStream);
  public
    property Items[Index: Integer]: TSynEditKeyStroke read GetItem
      write SetItem; default;
  end;

// These are mainly for the TSynEditorCommand property editor, but could be
// useful elsewhere.
function EditorCommandToDescrString(Cmd: TSynEditorCommand): string;
function EditorCommandToCodeString(Cmd: TSynEditorCommand): string;
function EditorCommandToExtendedCodeString(Cmd: TSynEditorCommand): string;
procedure GetEditorCommandValues(Proc: TGetStrProc);
procedure GetEditorCommandExtended(Proc: TGetStrProc);
function IdentToEditorCommand(const Ident: string; var Cmd: Integer): Boolean;
function EditorCommandToIdent(Cmd: Integer; var Ident: string): Boolean;
function ConvertCodeStringToExtended(AString: string): string;
function ConvertExtendedToCodeString(AString: string): string;
function ConvertExtendedToCommand(AString: string): TSynEditorCommand;
function ConvertCodeStringToCommand(AString: string): TSynEditorCommand;
function IndexToEditorCommand(const AIndex: Integer): Integer;

// Synedit command information
var
  SynCommandsInfo: TDictionary<Integer, TSynCommandInfo>;

implementation

uses
  Winapi.Windows,
  SynEditKeyConst,
  SynEditStrConst;

{ Command mapping routines }

const
  EditorCommandStrs: array[0..124] of TIdentMapEntry = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecSelLeft; Name: 'ecSelLeft'),
    (Value: ecSelRight; Name: 'ecSelRight'),
    (Value: ecSelUp; Name: 'ecSelUp'),
    (Value: ecSelDown; Name: 'ecSelDown'),
    (Value: ecSelWordLeft; Name: 'ecSelWordLeft'),
    (Value: ecSelWordRight; Name: 'ecSelWordRight'),
    (Value: ecSelLineStart; Name: 'ecSelLineStart'),
    (Value: ecSelLineEnd; Name: 'ecSelLineEnd'),
    (Value: ecSelPageUp; Name: 'ecSelPageUp'),
    (Value: ecSelPageDown; Name: 'ecSelPageDown'),
    (Value: ecSelPageLeft; Name: 'ecSelPageLeft'),
    (Value: ecSelPageRight; Name: 'ecSelPageRight'),
    (Value: ecSelPageTop; Name: 'ecSelPageTop'),
    (Value: ecSelPageBottom; Name: 'ecSelPageBottom'),
    (Value: ecSelEditorTop; Name: 'ecSelEditorTop'),
    (Value: ecSelEditorBottom; Name: 'ecSelEditorBottom'),
    (Value: ecSelGotoXY; Name: 'ecSelGotoXY'),
    (Value: ecSelWord; Name: 'ecSelWord'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBOL; Name: 'ecDeleteBOL'),
    (Value: ecDeleteEOL; Name: 'ecDeleteEOL'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClearAll; Name: 'ecClearAll'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecMatchBracket; Name: 'ecMatchBracket'),
    (Value: ecSelMatchBracket; Name: 'ecSelMatchBracket'),
    (Value: ecCommentBlock; Name: 'ecCommentBlock'),
    (Value: ecAutoCompletion; Name: 'ecAutoCompletion'),
    (Value: ecContextHelp; Name: 'ecContextHelp'),
    (Value: ecNextChange; Name: 'ecNextChange'),
    (Value: ecPreviousChange; Name: 'ecPrevChange'),
    (Value: ecGotoMarker0; Name: 'ecGotoMarker0'),
    (Value: ecGotoMarker1; Name: 'ecGotoMarker1'),
    (Value: ecGotoMarker2; Name: 'ecGotoMarker2'),
    (Value: ecGotoMarker3; Name: 'ecGotoMarker3'),
    (Value: ecGotoMarker4; Name: 'ecGotoMarker4'),
    (Value: ecGotoMarker5; Name: 'ecGotoMarker5'),
    (Value: ecGotoMarker6; Name: 'ecGotoMarker6'),
    (Value: ecGotoMarker7; Name: 'ecGotoMarker7'),
    (Value: ecGotoMarker8; Name: 'ecGotoMarker8'),
    (Value: ecGotoMarker9; Name: 'ecGotoMarker9'),
    (Value: ecSetMarker0; Name: 'ecSetMarker0'),
    (Value: ecSetMarker1; Name: 'ecSetMarker1'),
    (Value: ecSetMarker2; Name: 'ecSetMarker2'),
    (Value: ecSetMarker3; Name: 'ecSetMarker3'),
    (Value: ecSetMarker4; Name: 'ecSetMarker4'),
    (Value: ecSetMarker5; Name: 'ecSetMarker5'),
    (Value: ecSetMarker6; Name: 'ecSetMarker6'),
    (Value: ecSetMarker7; Name: 'ecSetMarker7'),
    (Value: ecSetMarker8; Name: 'ecSetMarker8'),
    (Value: ecSetMarker9; Name: 'ecSetMarker9'),
    (Value: ecZoomIn; Name: 'ecZoomIn'),
    (Value: ecZoomOut; Name: 'ecZoomOut'),
    (Value: ecZoomReset; Name: 'ecZoomReset'),
    (Value: ecUpperCase; Name: 'ecUpperCase'),
    (Value: ecLowerCase; Name: 'ecLowerCase'),
    (Value: ecToggleCase; Name: 'ecToggleCase'),
    (Value: ecTitleCase; Name: 'ecTitleCase'),
    (Value: ecCopyLineUp; Name:'ecCopyLineUp'),
    (Value: ecCopyLineDown; Name:'ecCopyLineDown'),
    (Value: ecMoveLineUp; Name:'ecMoveLineUp'),
    (Value: ecMoveLineDown; Name:'ecMoveLineDown'),
    (Value: ecString; Name:'ecString'),
    // CodeFolding
    (Value: ecFoldAll; Name:'ecFoldAll'),
    (Value: ecUnfoldAll; Name:'ecUnfoldAll'),
    (Value: ecFoldNearest; Name:'ecFoldNearest'),
    (Value: ecUnfoldNearest; Name:'ecUnfoldNearest'),
    (Value: ecFoldLevel1; Name:'ecFoldLevel1'),
    (Value: ecFoldLevel2; Name:'ecFoldLevel2'),
    (Value: ecFoldLevel3; Name:'ecFoldLevel3'),
    (Value: ecUnfoldLevel1; Name:'ecUnfoldLevel1'),
    (Value: ecUnfoldLevel2; Name:'ecUnfoldLevel2'),
    (Value: ecUnfoldLevel3; Name:'ecUnfoldLevel3'),
    (Value: ecFoldRegions; Name:'ecFoldRanges'),
    (Value: ecUnfoldRegions; Name:'ecUnfoldRanges'),
    // Multi-caret
    (Value: ecCancelSelections; Name:'ecCancelSelections'),
    (Value: ecSelColumnLeft; Name:'ecSelColumnLeft'),
    (Value: ecSelColumnRight; Name:'ecSelColumnRight'),
    (Value: ecSelColumnUp; Name:'ecSelColumnUp'),
    (Value: ecSelColumnDown; Name:'ecSelColumnDown'),
    (Value: ecSelColumnPageUp; Name:'ecSelColumnPageUp'),
    (Value: ecSelColumnPageDown; Name:'ecSelColumnPageDown'),
    (Value: ecSelMatchingText; Name:'ecSelMatchingText'),
    (Value: ecCaretsAtLineEnds; Name:'ecCaretsAtLineEnds')
    );

// GetEditorCommandValues and GetEditorCommandExtended for editing key assignments
procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    case EditorCommandStrs[I].Value of
      ecNone, ecChar, ecString, ecImeStr, ecGotoXY, ecSelGotoXY:
        ;// skip commands that cannot be used by the end-user
    else
      Proc(EditorCommandStrs[I].Name);
    end;
end;

procedure GetEditorCommandExtended(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    case EditorCommandStrs[I].Value of
      ecNone, ecChar, ecString, ecImeStr, ecGotoXY, ecSelGotoXY:
        ;// skip commands that cannot be used by the end-user
    else
      Proc(ConvertCodeStringToExtended(EditorCommandStrs[I].Name));
    end;
end;

function IdentToEditorCommand(const Ident: string; var Cmd: Integer): Boolean;
begin
    Result := IdentToInt(Ident, Cmd, EditorCommandStrs);
end;

function EditorCommandToIdent(Cmd: Integer; var Ident: string): Boolean;
begin
  Result := IntToIdent(Cmd, Ident, EditorCommandStrs);
end;

function EditorCommandToDescrString(Cmd: TSynEditorCommand): string;
begin
  // Doesn't do anything yet.
  Result := '';
end;

function EditorCommandToCodeString(Cmd: TSynEditorCommand): string;
begin
  if not EditorCommandToIdent(Cmd, Result) then
    Result := IntToStr(Cmd);
end;

function EditorCommandToExtendedCodeString(Cmd: TSynEditorCommand): string;
begin
  Result := ConvertCodeStringToExtended(EditorCommandToCodeString(Cmd));
end;


{ TSynEditKeyStroke }

procedure TSynEditKeyStroke.Assign(Source: TPersistent);
begin
  if Source is TSynEditKeyStroke then
  begin
    Command := TSynEditKeyStroke(Source).Command;
    Key := TSynEditKeyStroke(Source).Key;
    Key2 := TSynEditKeyStroke(Source).Key2;
    Shift := TSynEditKeyStroke(Source).Shift;
    Shift2 := TSynEditKeyStroke(Source).Shift2;
  end else
    inherited Assign(Source);
end;

function TSynEditKeyStroke.GetDisplayName: string;
begin
  Result := EditorCommandToCodeString(Command) + ' - ' + ShortCutToText(ShortCut);
  if ShortCut2 <> 0 then
    Result := Result + ' ' + ShortCutToText(ShortCut2);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TSynEditKeyStroke.GetShortCut: TShortCut;
begin
  Result := Vcl.Menus.ShortCut(Key, Shift);
end;

procedure TSynEditKeyStroke.SetCommand(const Value: TSynEditorCommand);
begin
  if Value <> FCommand then
    FCommand := Value;
end;

procedure TSynEditKeyStroke.SetKey(const Value: word);
begin
  if Value <> FKey then
    FKey := Value;
end;

procedure TSynEditKeyStroke.SetShift(const Value: TShiftState);
begin
  if Value <> FShift then
    FShift := Value;
end;

procedure TSynEditKeyStroke.SetShortCut(const Value: TShortCut);
var
  NewKey: Word;
  NewShift: TShiftState;
  Dup: Integer;
  CurrCmd, DupCmd: string;
begin
  // Duplicate values of no shortcut are OK.
  if Value <> 0 then
  begin
    // Check for duplicate shortcut in the collection and disallow if there is.
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(Value, ShortCut2);
    if (Dup <> -1) and (Dup <> Self.Index) then
    begin
      CurrCmd := EditorCommandToExtendedCodeString(FCommand);
      DupCmd := EditorCommandToExtendedCodeString(
        TSynEditKeyStrokes(Collection)[Dup].Command);

      raise ESynKeyError.CreateFmt(SYNS_EDuplicateShortCut, [CurrCmd, DupCmd]);
    end;
  end;

  Vcl.Menus.ShortCutToKey(Value, NewKey, NewShift);

  if (NewKey <> Key) or (NewShift <> Shift) then
  begin
    Key := NewKey;
    Shift := NewShift;
  end;
end;

procedure TSynEditKeyStroke.SetKey2(const Value: word);
begin
  if Value <> FKey2 then
    FKey2 := Value;
end;

procedure TSynEditKeyStroke.SetShift2(const Value: TShiftState);
begin
  if Value <> FShift2 then
    FShift2 := Value;
end;

procedure TSynEditKeyStroke.SetShortCut2(const Value: TShortCut);
var
  NewKey: Word;
  NewShift: TShiftState;
  Dup: Integer;
  CurrCmd, DupCmd: string;
begin
  // Duplicate values of no shortcut are OK.
  if Value <> 0 then
  begin
    // Check for duplicate shortcut in the collection and disallow if there is.
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(ShortCut, Value);
    if (Dup <> -1) and (Dup <> Self.Index) then
    begin
      CurrCmd := EditorCommandToExtendedCodeString(FCommand);
      DupCmd := EditorCommandToExtendedCodeString(
        TSynEditKeyStrokes(Collection)[Dup].Command);

      raise ESynKeyError.CreateFmt(SYNS_EDuplicateShortCut, [CurrCmd, DupCmd]);
    end;
  end;

  Vcl.Menus.ShortCutToKey(Value, NewKey, NewShift);
  if (NewKey <> Key2) or (NewShift <> Shift2) then
  begin
    Key2 := NewKey;
    Shift2 := NewShift;
  end;
end;

function TSynEditKeyStroke.GetShortCut2: TShortCut;
begin
  Result := Vcl.Menus.ShortCut(Key2, Shift2);
end;

procedure TSynEditKeyStroke.LoadFromStream(AStream: TStream);
begin
  with AStream do begin
    Read(fKey, SizeOf(fKey));
    Read(fShift, SizeOf(fShift));
    Read(fKey2, SizeOf(fKey2));
    Read(fShift2, SizeOf(fShift2));
    Read(fCommand, SizeOf(fCommand));
  end;
end;

procedure TSynEditKeyStroke.SaveToStream(AStream: TStream);
begin
  with AStream do begin
    Write(fKey, SizeOf(fKey));
    Write(fShift, SizeOf(fShift));
    Write(fKey2, SizeOf(fKey2));
    Write(fShift2, SizeOf(fShift2));
    Write(fCommand, SizeOf(fCommand));
  end;
end;


{ TSynEditKeyStrokes }

function TSynEditKeyStrokes.Add: TSynEditKeyStroke;
begin
  Result := TSynEditKeyStroke(inherited Add);
end;

//++ CodeFolding
procedure TSynEditKeyStrokes.AddKey(const ACmd: TSynEditorCommand; const AKey: word;
  const AShift: TShiftState; const AKey2: word; const AShift2: TShiftState);
var
  NewKeystroke: TSynEditKeyStroke;
begin
  NewKeystroke := Add;
  try
    NewKeystroke.Key := AKey;
    NewKeystroke.Shift := AShift;
    NewKeystroke.Key2 := AKey2;
    NewKeystroke.Shift2 := AShift2;
    NewKeystroke.Command := ACmd;
  except
    NewKeystroke.Free;
    raise;
  end;
end;
//-- CodeFolding

constructor TSynEditKeyStrokes.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TSynEditKeyStroke);
end;

function TSynEditKeyStrokes.FindCommand(Cmd: TSynEditorCommand): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Count-1 do
    if Items[Idx].Command = Cmd then
    begin
      Result := Idx;
      Break;
    end;
end;

function TSynEditKeyStrokes.FindKeycode(Code: word; SS: TShiftState): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Count-1 do
    if (Items[Idx].Key = Code) and (Items[Idx].Shift = SS) and (Items[Idx].Key2 = 0)
    then begin
      Result := Idx;
      Break;
    end;
end;

function TSynEditKeyStrokes.FindKeycode2(Code1: word; SS1: TShiftState;
  Code2: word; SS2: TShiftState): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Count-1 do
    if (Items[Idx].Key = Code1) and (Items[Idx].Shift = SS1) and
       (Items[Idx].Key2 = Code2) and (Items[Idx].Shift2 = SS2) then
    begin
      Result := Idx;
      Break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut(SC: TShortcut): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Count-1 do
    if Items[Idx].Shortcut = SC then
    begin
      Result := Idx;
      Break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut2(SC, SC2: TShortcut): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Count - 1 do
    if (Items[Idx].Shortcut = SC) and (Items[Idx].Shortcut2 = SC2) then
    begin
      Result := Idx;
      Break;
    end;
end;

function TSynEditKeyStrokes.GetItem(Index: Integer): TSynEditKeyStroke;
begin
 Result := TSynEditKeyStroke(inherited GetItem(Index));
end;

procedure TSynEditKeyStrokes.LoadFromStream(AStream: TStream);
var
  Num: Integer;
begin
  Clear;
  AStream.Read(Num, SizeOf(Num));
  while Num > 0 do begin
    with Add do
      LoadFromStream(AStream);
    Dec(Num);
  end;
end;

procedure TSynEditKeyStrokes.ResetDefaults;
begin
  Clear;

  AddKey(ecUp, SYNEDIT_UP, []);
  AddKey(ecSelUp, SYNEDIT_UP, [ssShift]);
  AddKey(ecScrollUp, SYNEDIT_UP, [ssCtrl]);
  AddKey(ecDown, SYNEDIT_DOWN, []);
  AddKey(ecSelDown, SYNEDIT_DOWN, [ssShift]);
  AddKey(ecScrollDown, SYNEDIT_DOWN, [ssCtrl]);
  AddKey(ecLeft, SYNEDIT_LEFT, []);
  AddKey(ecSelLeft, SYNEDIT_LEFT, [ssShift]);
  AddKey(ecWordLeft, SYNEDIT_LEFT, [ssCtrl]);
  AddKey(ecSelWordLeft, SYNEDIT_LEFT, [ssShift,ssCtrl]);
  AddKey(ecRight, SYNEDIT_RIGHT, []);
  AddKey(ecSelRight, SYNEDIT_RIGHT, [ssShift]);
  AddKey(ecWordRight, SYNEDIT_RIGHT, [ssCtrl]);
  AddKey(ecSelWordRight, SYNEDIT_RIGHT, [ssShift,ssCtrl]);
  AddKey(ecSelWord, Ord('W'), [ssCtrl]);
  AddKey(ecPageDown, SYNEDIT_NEXT, []);
  AddKey(ecSelPageDown, SYNEDIT_NEXT, [ssShift]);
  AddKey(ecPageBottom, SYNEDIT_NEXT, [ssCtrl]);
  AddKey(ecSelPageBottom, SYNEDIT_NEXT, [ssShift,ssCtrl]);
  AddKey(ecPageUp, SYNEDIT_PRIOR, []);
  AddKey(ecSelPageUp, SYNEDIT_PRIOR, [ssShift]);
  AddKey(ecPageTop, SYNEDIT_PRIOR, [ssCtrl]);
  AddKey(ecSelPageTop, SYNEDIT_PRIOR, [ssShift,ssCtrl]);
  AddKey(ecLineStart, SYNEDIT_HOME, []);
  AddKey(ecSelLineStart, SYNEDIT_HOME, [ssShift]);
  AddKey(ecEditorTop, SYNEDIT_HOME, [ssCtrl]);
  AddKey(ecSelEditorTop, SYNEDIT_HOME, [ssShift,ssCtrl]);
  AddKey(ecLineEnd, SYNEDIT_END, []);
  AddKey(ecSelLineEnd, SYNEDIT_END, [ssShift]);
  AddKey(ecEditorBottom, SYNEDIT_END, [ssCtrl]);
  AddKey(ecSelEditorBottom, SYNEDIT_END, [ssShift,ssCtrl]);
  AddKey(ecToggleMode, SYNEDIT_INSERT, []);
  AddKey(ecCopy, SYNEDIT_INSERT, [ssCtrl]);
  AddKey(ecCut, SYNEDIT_DELETE, [ssShift]);
  AddKey(ecPaste, SYNEDIT_INSERT, [ssShift]);
  AddKey(ecDeleteChar, SYNEDIT_DELETE, []);
  AddKey(ecDeleteLastChar, SYNEDIT_BACK, []);
  AddKey(ecDeleteBOL, SYNEDIT_BACK, [ssShift]);
  AddKey(ecDeleteLastWord, SYNEDIT_BACK, [ssCtrl]);
  AddKey(ecUndo, SYNEDIT_BACK, [ssAlt]);
  AddKey(ecRedo, SYNEDIT_BACK, [ssAlt,ssShift]);
  AddKey(ecLineBreak, SYNEDIT_RETURN, []);
  AddKey(ecLineBreak, SYNEDIT_RETURN, [ssShift]);
  AddKey(ecTab, SYNEDIT_TAB, []);
  AddKey(ecShiftTab, SYNEDIT_TAB, [ssShift]);
  AddKey(ecContextHelp, SYNEDIT_F1, []);

  AddKey(ecSelectAll, Ord('A'), [ssCtrl]);
  AddKey(ecCopy, Ord('C'), [ssCtrl]);
  AddKey(ecPaste, Ord('V'), [ssCtrl]);
  AddKey(ecCut, Ord('X'), [ssCtrl]);
  AddKey(ecBlockIndent, Ord('I'), [ssCtrl,ssShift]);
  AddKey(ecBlockUnindent, Ord('U'), [ssCtrl,ssShift]);
  AddKey(ecLineBreak, Ord('M'), [ssCtrl]);
  AddKey(ecInsertLine, Ord('N'), [ssCtrl]);
  AddKey(ecDeleteWord, Ord('T'), [ssCtrl]);
  AddKey(ecDeleteLine, Ord('Y'), [ssCtrl]);
  AddKey(ecDeleteEOL, Ord('Y'), [ssCtrl,ssShift]);
  AddKey(ecUndo, Ord('Z'), [ssCtrl]);
  AddKey(ecRedo, Ord('Z'), [ssCtrl,ssShift]);
  AddKey(ecNextChange, SYNEDIT_ADD, [ssCtrl,ssShift]);
  AddKey(ecPreviousChange, SYNEDIT_SUBTRACT, [ssCtrl,ssShift]);
  AddKey(ecGotoMarker0, Ord('0'), [ssCtrl]);
  AddKey(ecGotoMarker1, Ord('1'), [ssCtrl]);
  AddKey(ecGotoMarker2, Ord('2'), [ssCtrl]);
  AddKey(ecGotoMarker3, Ord('3'), [ssCtrl]);
  AddKey(ecGotoMarker4, Ord('4'), [ssCtrl]);
  AddKey(ecGotoMarker5, Ord('5'), [ssCtrl]);
  AddKey(ecGotoMarker6, Ord('6'), [ssCtrl]);
  AddKey(ecGotoMarker7, Ord('7'), [ssCtrl]);
  AddKey(ecGotoMarker8, Ord('8'), [ssCtrl]);
  AddKey(ecGotoMarker9, Ord('9'), [ssCtrl]);
  AddKey(ecSetMarker0, Ord('0'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker1, Ord('1'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker2, Ord('2'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker3, Ord('3'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker4, Ord('4'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker5, Ord('5'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker6, Ord('6'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker7, Ord('7'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker8, Ord('8'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker9, Ord('9'), [ssCtrl,ssShift]);
  AddKey(ecZoomIn, SYNEDIT_ADD, [ssAlt]);
  AddKey(ecZoomOut, SYNEDIT_SUBTRACT, [ssAlt]);
  AddKey(ecZoomReset, VK_NUMPAD0, [ssAlt]);
  AddKey(ecMatchBracket, VK_OEM_6, [ssCtrl]); // VK_OEM_6 is ']' in US keyboards
  AddKey(ecSelMatchBracket, VK_OEM_6, [ssCtrl,ssShift]);
  AddKey(ecLowerCase, Ord('K'), [ssCtrl], Ord('L'), [ssCtrl]);
  AddKey(ecUpperCase, Ord('K'), [ssCtrl], Ord('U'), [ssCtrl]);
  AddKey(ecTitleCase, Ord('K'), [ssCtrl], Ord('T'), [ssCtrl]);
  AddKey(ecCopyLineDown, Ord('D'), [ssCtrl]);
  AddKey(ecMoveLineUp, SYNEDIT_UP, [ssAlt]);
  AddKey(ecMoveLineDown, SYNEDIT_DOWN, [ssAlt]);
  // CodeFolding
  AddKey(ecFoldAll, VK_OEM_MINUS, [ssCtrl, ssShift]);   {- _}
  AddKey(ecUnfoldAll,  VK_OEM_PLUS, [ssCtrl, ssShift]); {= +}
  AddKey(ecFoldNearest, VK_OEM_2, [ssCtrl]);  // Divide {'/'}
  AddKey(ecUnfoldNearest, VK_OEM_2, [ssCtrl, ssShift]);
  AddKey(ecFoldLevel1, Ord('K'), [ssCtrl], Ord('1'), [ssCtrl]);
  AddKey(ecFoldLevel2, Ord('K'), [ssCtrl], Ord('2'), [ssCtrl]);
  AddKey(ecFoldLevel3, Ord('K'), [ssCtrl], Ord('3'), [ssCtrl]);
  AddKey(ecUnfoldLevel1, Ord('K'), [ssCtrl, ssShift], Ord('1'), [ssCtrl, ssShift]);
  AddKey(ecUnfoldLevel2, Ord('K'), [ssCtrl, ssShift], Ord('2'), [ssCtrl, ssShift]);
  AddKey(ecUnfoldLevel3, Ord('K'), [ssCtrl, ssShift], Ord('3'), [ssCtrl, ssShift]);
  // Multi-caret
  AddKey(ecCancelSelections, SYNEDIT_ESCAPE, []);
  AddKey(ecSelColumnLeft, SYNEDIT_LEFT, [ssShift, ssAlt]);
  AddKey(ecSelColumnRight, SYNEDIT_RIGHT, [ssShift, ssAlt]);
  AddKey(ecSelColumnUp, SYNEDIT_UP, [ssShift, ssAlt]);
  AddKey(ecSelColumnDown, SYNEDIT_DOWN, [ssShift, ssAlt]);
  AddKey(ecSelColumnPageUp, SYNEDIT_PRIOR, [ssShift, ssAlt]);
  AddKey(ecSelColumnPageDown, SYNEDIT_NEXT, [ssShift, ssAlt]);
  AddKey(ecSelMatchingText, Ord('W'), [ssShift, ssCtrl]);
  AddKey(ecCaretsAtLineEnds, SYNEDIT_END, [ssAlt]);
end;

procedure TSynEditKeyStrokes.SetItem(Index: Integer; Value: TSynEditKeyStroke);
begin
 inherited SetItem(Index, Value);
end;

procedure TSynEditKeyStrokes.SaveToStream(AStream: TStream);
var
  i, Num: Integer;
begin
  Num := Count;
  AStream.Write(Num, SizeOf(Num));
  for i := 0 to Num - 1 do
    Items[i].SaveToStream(AStream);
end;

function ConvertCodeStringToExtended(AString: string): string;
var
  i: Integer;
  WorkStr: string;
begin
  if pos('ec', AString) = 1 then
  begin
    Delete(AString, 1, 2);
    WorkStr := '';

    for i := length(AString) downto 1 do
      if CharInSet(AString[i], ['A'..'Z', '0'..'9']) and (i > 1) and
         not CharInSet(AString[i - 1], ['A'..'Z', '0'..'9']) then
      begin
        WorkStr := ' ' + AString[i] + WorkStr
      end
      else
        WorkStr := AString[i] + WorkStr;

    trim(WorkStr);

    i := pos('Sel ', WorkStr);
    while i <> 0 do
    begin
      Delete(WorkStr, i, Length('Sel '));
      Insert('Select ', WorkStr, i);
      i := pos('Sel ', WorkStr);
    end;

    i := pos('Marker ', WorkStr);
    while i <> 0 do
    begin
      Delete(WorkStr, i, Length('Marker '));
      Insert('Bookmark ', WorkStr,i);
      i := pos('Marker ', WorkStr);
    end;

    Result := Trim(WorkStr);
  end
  else
    Result := AString;
end;

function ConvertExtendedToCodeString(AString: string): string;
var
  i: Integer;
  WorkStr: string;
begin
  if pos('ec', AString) = 1 then
  begin
    Result := AString;
    Exit;
  end;

  WorkStr := AString;

  I := pos('Select All', WorkStr);
  if I = 0 then
  begin
    I := pos('Select ', WorkStr);
    while I <> 0 do
    begin
      Delete(WorkStr,I,Length('Select '));
      Insert('Sel ',WorkStr,I);
      I := pos('Select ', WorkStr);
    end;
  end;

  I := pos('Bookmark ', WorkStr);
  while I <> 0 do
  begin
    Delete(WorkStr,I,Length('Bookmark '));
    Insert('Marker ',WorkStr,I);
    I := pos('Bookmark ', WorkStr);
  end;

  I := pos(' ', WorkStr);
  while I <> 0 do
  begin
    delete(WorkStr,I,1);
    I := pos(' ', WorkStr);
  end;

  Result := 'ec' + WorkStr;
end;

function IndexToEditorCommand(const AIndex: Integer): Integer;
begin
  Result := EditorCommandStrs[AIndex].Value;
end;

function ConvertExtendedToCommand(AString: string): TSynEditorCommand;
begin
  Result := ConvertCodeStringToCommand(ConvertExtendedToCodeString(AString));
end;

function ConvertCodeStringToCommand(AString: string): TSynEditorCommand;
var
  I: Integer;
begin
  Result := ecNone;

  AString := Uppercase(AString);
  for I := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    if Uppercase(EditorCommandStrs[I].Name) = AString then
    begin
      Result := EditorCommandStrs[I].Value;
      Break;
    end;
end;

procedure CreateSysCommandsInfo;
begin
  SynCommandsInfo := TDictionary<Integer, TSynCommandInfo>.Create;
  SynCommandsInfo.Add(ecLeft, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecRight, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecUp, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecDown, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecWordLeft, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecWordRight, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecLineStart, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecLineEnd, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecPageUp, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecPageDown, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecPageLeft, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecPageRight, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecPageTop, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecPageBottom, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecEditorTop, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecEditorBottom, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecGotoXY, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelLeft, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelRight, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelUp, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelDown, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelWordLeft, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelWordRight, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelLineStart, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelLineEnd, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelPageUp, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelPageDown, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelPageLeft, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelPageRight, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelPageTop, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelPageBottom, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelEditorTop, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelEditorBottom, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelGotoXY, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelWord, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecSelectAll, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecCopy, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecScrollUp, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecScrollDown, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecScrollLeft, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecScrollRight, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecInsertMode, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecOverwriteMode, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecToggleMode, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecMatchBracket, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecSelMatchBracket, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecCommentBlock, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecNextChange, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecPreviousChange, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecGotoMarker0, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker1, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker2, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker3, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker4, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker5, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker6, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker7, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker8, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotoMarker9, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker0, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker1, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker2, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker3, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker4, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker5, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker6, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker7, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker8, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSetMarker9, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecZoomIn, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecZoomOut, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecZoomReset, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecGotFocus, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecLostFocus, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecContextHelp, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecDeleteLastChar, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecDeleteChar, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecDeleteWord, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecDeleteLastWord, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecDeleteBOL, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecDeleteEOL, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecDeleteLine, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecClearAll, TSynCommandInfo.Create(ckSingleCaret, True));
  SynCommandsInfo.Add(ecLineBreak, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecInsertLine, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecChar, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecImeStr, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecUndo, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecRedo, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecCut, TSynCommandInfo.Create(ckStandard, True));
  SynCommandsInfo.Add(ecPaste, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecBlockIndent, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecBlockUnindent, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecTab, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecShiftTab, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecUpperCase, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecLowerCase, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecToggleCase, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecTitleCase, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecString, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecAutoCompletion, TSynCommandInfo.Create(ckSingleCaret, False));
  SynCommandsInfo.Add(ecCopyLineUp, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecCopyLineDown, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecMoveLineUp, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecMoveLineDown, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecFoldAll, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecUnfoldAll, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecFoldNearest, TSynCommandInfo.Create(ckMultiCaret, False));
  SynCommandsInfo.Add(ecUnfoldNearest, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecFoldLevel1, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecFoldLevel2, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecFoldLevel3, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecUnfoldLevel1, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecUnfoldLevel2, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecUnfoldLevel3, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecFoldRegions, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecUnfoldRegions, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecCancelSelections, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecDeleteSelections, TSynCommandInfo.Create(ckMultiCaret, True));
  SynCommandsInfo.Add(ecSelColumnLeft, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSelColumnRight, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSelColumnUp, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSelColumnDown, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSelColumnPageUp, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSelColumnPageDown, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecSelMatchingText, TSynCommandInfo.Create(ckStandard, False));
  SynCommandsInfo.Add(ecCaretsAtLineEnds, TSynCommandInfo.Create(ckStandard, False));
end;

{ TSynCommandInfo }

constructor TSynCommandInfo.Create(ACommandKind: TSynCommandKind;
  AStoreMultiCaret: Boolean);
begin
  Self.CommandKind := ACommandKind;
  Self.StoreMultiCaret := AStoreMultiCaret;
end;

initialization
  RegisterIntegerConsts(TypeInfo(TSynEditorCommand), IdentToEditorCommand,
     EditorCommandToIdent);
  CreateSysCommandsInfo;
finalization
  SynCommandsInfo.Free;
end.
