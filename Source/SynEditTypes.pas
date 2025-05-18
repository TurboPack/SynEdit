{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTypes.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditTypes;

{$I SynEdit.inc}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.Math,
  Vcl.Controls,
  System.SysUtils,
  System.Classes;

const
// These might need to be localized depending on the characterset because they might be
// interpreted as valid ident characters.
  SynTabGlyph: WideChar = #$2192;       //'->'
  SynSoftBreakGlyph: WideChar = #$00AC; //'¬'
  SynLineBreakGlyph: WideChar = #$00B6; //'¶'
  SynSpaceGlyph: WideChar = #$2219;     //'·'

type
  ESynError = class(Exception);

  // DOS: CRLF, UNIX: LF, Mac: CR, Unicode: LINE SEPARATOR
  TSynEditFileFormat = (sffDos, sffUnix, sffMac, sffUnicode);

  TSynSearchOption = (ssoMatchCase, ssoWholeWord, ssoBackwards,
    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt);
  TSynSearchOptions = set of TSynSearchOption;

  TCategoryMethod = function(AChar: WideChar): Boolean of object;

  TSynEditorCommand = type Word;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: Boolean;
    var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
    Data: pointer; HandlerData: pointer) of object;

  TSynInfoLossEvent = procedure (var Encoding: TEncoding; Cancel: Boolean) of object;

  PSynSelectionMode = ^TSynSelectionMode;
  TSynSelectionMode = (smNormal, smLine, smColumn);

  TBufferCoord = record
    Char: integer;
    Line: integer;
    class operator Equal(a, b: TBufferCoord): Boolean;
    class operator NotEqual(a, b: TBufferCoord): Boolean;
    class operator LessThan(a, b: TBufferCoord): Boolean;
    class operator LessThanOrEqual(a, b: TBufferCoord): Boolean;
    class operator GreaterThan(a, b: TBufferCoord): Boolean;
    class operator GreaterThanOrEqual(a, b: TBufferCoord): Boolean;
    class function Min(a, b: TBufferCoord): TBufferCoord; static;
    class function Max(a, b: TBufferCoord): TBufferCoord; static;
  end;

  TDisplayCoord = record
    Column: integer;
    Row: integer;
    class operator Equal(a, b: TDisplayCoord): Boolean;
    class operator NotEqual(a, b: TDisplayCoord): Boolean;
    class operator LessThan(a, b: TDisplayCoord): Boolean;
    class operator LessThanOrEqual(a, b: TDisplayCoord): Boolean;
    class operator GreaterThan(a, b: TDisplayCoord): Boolean;
    class operator GreaterThanOrEqual(a, b: TDisplayCoord): Boolean;
    class function Min(a, b: TDisplayCoord): TDisplayCoord; static;
    class function Max(a, b: TDisplayCoord): TDisplayCoord; static;
  end;

  (*  Helper methods for TControl - for backwward compatibility *)
  {$IF CompilerVersion <= 32}
  TControlHelper = class helper for TControl
  public
    function CurrentPPI: Integer;
    function FCurrentPPI: Integer;
  end;
  {$ENDIF}

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
function BufferCoord(AChar, ALine: Integer): TBufferCoord;
function LineBreakFromFileFormat(FileFormat: TSynEditFileFormat): string;

type
{ ************************* For ScrollBars ********************************}

  ISynEditScrollBars = interface
    function UpdateScrollBars: Boolean;
    function GetIsScrolling: Boolean;
    procedure WMHScroll(var AMsg: TWMScroll);
    procedure WMVScroll(var AMsg: TWMScroll);
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint);
    property IsScrolling: Boolean read GetIsScrolling;
  end;

{ ************************* For Word Wrap ********************************}

  // aIndex parameters of Line notifications are 0-based.
  // aRow parameter of GetRowLength() is 1-based.
  ISynEditBufferPlugin = interface
    // conversion methods
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function RowCount: Integer;
    function GetRowLength(aRow: Integer): Integer;
    // plugin notifications
    function LinesInserted(aIndex: Integer; aCount: Integer): Integer;
    function LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
    function LinePut(aIndex: Integer; const OldLine: string): Integer;
    // font or size change
    procedure DisplayChanged;
    // pretty clear, heh?
    procedure Reset;
    property RowLength[RowIndex: integer]: integer read GetRowLength;
  end;

{ ************************* For Undo Redo ********************************}

  // Note: several undo entries can be chained together via the ChangeNumber
  // see also TCustomSynEdit.[Begin|End]UndoBlock methods
  TSynChangeReason = (
    crNothing,
    crInsert, crDelete, crSilentDelete,
    crLineBreak,
    crIndent, crUnindent,
    crCaret, // just restore the Caret, allowing better Undo behavior
    crSelection, // restore Selection
    crWhiteSpaceAdd
    // for undo/redo of adding a character past EOL and repositioning the caret
    );

  TSynEditUndoItem = class(TObject)
    ChangeStartPos: TBufferCoord;
    ChangeEndPos: TBufferCoord;
    ChangeStr: string;
    ChangeNumber: Integer;
    ChangeReason: TSynChangeReason;
    ChangeSelMode: TSynSelectionMode;
    // the following undo item cannot be grouped with this one  when undoing
    // don't group the previous one with this one when redoing
    GroupBreak: Boolean;
  end;

  { Handles undo/redo and manages Modified status }
  ISynEditUndo =  interface
    function GetModified: Boolean;
    function GetMaxUndoActions: Integer;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetFullUndoImposible: Boolean;
    function GetOnModifiedChanged: TNotifyEvent;
    procedure SetModified(const Value: Boolean);
    procedure SetMaxUndoActions(const Value: Integer);
    procedure SetGroupUndo(const Value: Boolean);
    procedure SetOnModifiedChanged(const Event: TNotifyEvent);
    procedure SetCommandProcessed(const Command: TSynEditorCommand);
    {  Begin/EndBlock pairs group undo actions together and also
       store/restore editor caret and selection
       We need to pass the Editor so that they works with chained SynEdits
    }
    procedure BeginBlock(Editor: TControl);
    procedure EndBlock(Editor: TControl);
    { Lock disables undo/redo - useful if you are about to do a large number of
      changes and planning to clear undo afterwards }
    procedure Lock;
    procedure Unlock;
    procedure Clear;
    { Call AddGroupBreak to signal that the next undo action
      cannot be grouped with the current one }
    procedure AddGroupBreak;
    {Note: Undo/Redo are not reentrant}
    procedure Undo(Editor: TControl);
    procedure Redo(Editor: TControl);
    {TrackChanges stuff}
    procedure BufferSaved(Lines: TStrings);
    procedure ClearTrackChanges(Lines: TStrings);

    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property GroupUndo: Boolean write SetGroupUndo;
    property Modified: Boolean read GetModified write SetModified;
    { Used by SynEdit to inform the Undo system about the command being
      processed }
    property CommandProcessed: TSynEditorCommand write SetCommandProcessed;
    { MaxUndoActions zero or less indicates unlimited undo. It grows as needed.
      If it is a positive number, when the limit is reached 1/4 of the
      Undo history is discarded to make space for following undo actions }
    property MaxUndoActions: Integer read GetMaxUndoActions
      write SetMaxUndoActions;
    property FullUndoImpossible: Boolean read GetFullUndoImposible;
    property OnModifiedChanged: TNotifyEvent read GetOnModifiedChanged
      write SetOnModifiedChanged;
  end;

implementation
Uses
{$IF CompilerVersion <= 32}
  Vcl.Forms,
{$ENDIF}
  SynUnicode;

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
begin
  Result.Column := AColumn;
  Result.Row := ARow;
end;

function BufferCoord(AChar, ALine: Integer): TBufferCoord;
begin
  Result.Char := AChar;
  Result.Line := ALine;
end;

{ TBufferCoord }

class operator TBufferCoord.Equal(a, b: TBufferCoord): Boolean;
begin
  Result := (a.Char = b.Char) and (a.Line = b.Line);
end;

class operator TBufferCoord.GreaterThan(a, b: TBufferCoord): Boolean;
begin
  Result :=  (b.Line < a.Line)
    or ((b.Line = a.Line) and (b.Char < a.Char))
end;

class operator TBufferCoord.GreaterThanOrEqual(a, b: TBufferCoord): Boolean;
begin
  Result :=  (b.Line < a.Line)
    or ((b.Line = a.Line) and (b.Char <= a.Char))
end;

class operator TBufferCoord.LessThan(a, b: TBufferCoord): Boolean;
begin
  Result :=  (b.Line > a.Line)
    or ((b.Line = a.Line) and (b.Char > a.Char))
end;

class operator TBufferCoord.LessThanOrEqual(a, b: TBufferCoord): Boolean;
begin
  Result :=  (b.Line > a.Line)
    or ((b.Line = a.Line) and (b.Char >= a.Char))
end;

class function TBufferCoord.Max(a, b: TBufferCoord): TBufferCoord;
begin
  if (b.Line < a.Line)
    or ((b.Line = a.Line) and (b.Char < a.Char))
  then
    Result := a
  else
    Result := b;
end;

class function TBufferCoord.Min(a, b: TBufferCoord): TBufferCoord;
begin
  if (b.Line < a.Line)
    or ((b.Line = a.Line) and (b.Char < a.Char))
  then
    Result := b
  else
    Result := a;
end;

class operator TBufferCoord.NotEqual(a, b: TBufferCoord): Boolean;
begin
  Result := (a.Char <> b.Char) or (a.Line <> b.Line);
end;

{ TDisplayCoord }

class operator TDisplayCoord.Equal(a, b: TDisplayCoord): Boolean;
begin
  Result := (a.Row = b.Row) and (a.Column = b.Column);
end;

class operator TDisplayCoord.GreaterThan(a, b: TDisplayCoord): Boolean;
begin
  Result :=  (b.Row < a.Row)
    or ((b.Row = a.Row) and (b.Column < a.Column))
end;

class operator TDisplayCoord.GreaterThanOrEqual(a, b: TDisplayCoord): Boolean;
begin
  Result :=  (b.Row < a.Row)
    or ((b.Row = a.Row) and (b.Column <= a.Column))
end;

class operator TDisplayCoord.LessThan(a, b: TDisplayCoord): Boolean;
begin
  Result :=  (b.Row > a.Row)
    or ((b.Row = a.Row) and (b.Column > a.Column))
end;

class operator TDisplayCoord.LessThanOrEqual(a, b: TDisplayCoord): Boolean;
begin
  Result :=  (b.Row > a.Row)
    or ((b.Row = a.Row) and (b.Column >= a.Column))
end;

class function TDisplayCoord.Max(a, b: TDisplayCoord): TDisplayCoord;
begin
  if (b.Row < a.Row)
    or ((b.Row = a.Row) and (b.Column < a.Column))
  then
    Result := a
  else
    Result := b;
end;

class function TDisplayCoord.Min(a, b: TDisplayCoord): TDisplayCoord;
begin
  if (b.Row < a.Row)
    or ((b.Row = a.Row) and (b.Column < a.Column))
  then
    Result := b
  else
    Result := a;
end;

class operator TDisplayCoord.NotEqual(a, b: TDisplayCoord): Boolean;
begin
  Result := (a.Row <> b.Row) or (a.Column <> b.Column);
end;

function LineBreakFromFileFormat(FileFormat: TSynEditFileFormat): string;
begin
  case FileFormat of
    sffDos: Result := WideCRLF;
    sffUnix: Result := WideLF;
    sffMac: Result := WideCR;
    sffUnicode: Result := WideLineSeparator;
  end;
end;

{$IF CompilerVersion <= 32}
{ TControlHelper }

function TControlHelper.CurrentPPI: Integer;
begin
  Result := Screen.PixelsPerInch;
end;

function TControlHelper.FCurrentPPI: Integer;
begin
  Result := Screen.PixelsPerInch;
end;
{$ENDIF}



end.

