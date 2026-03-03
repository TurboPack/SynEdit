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
Unicode translation by Ma�l H�rz.
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
  System.Types,
  System.Math,
  System.SysUtils,
  System.Classes,
  System.UITypes;

const
  DefaultBrackets = '()[]{}';

  { Color constants - aliases for TColors.* to allow shared code to use the
    traditional cl* names without depending on Vcl.Graphics }
  clBlack = TColors.Black;
  clMaroon = TColors.Maroon;
  clGreen = TColors.Green;
  clOlive = TColors.Olive;
  clNavy = TColors.Navy;
  clPurple = TColors.Purple;
  clTeal = TColors.Teal;
  clGray = TColors.Gray;
  clSilver = TColors.Silver;
  clRed = TColors.Red;
  clLime = TColors.Lime;
  clYellow = TColors.Yellow;
  clBlue = TColors.Blue;
  clFuchsia = TColors.Fuchsia;
  clAqua = TColors.Aqua;
  clLtGray = TColors.LtGray;
  clDkGray = TColors.DkGray;
  clWhite = TColors.White;
  clSkyBlue = TColors.LegacySkyBlue;
  clNone = TColors.SysNone;
  clWindow = TColors.SysWindow;
  clWindowText = TColors.SysWindowText;
  clGrayText = TColors.SysGrayText;
  clHighlight = TColors.SysHighlight;
  clHighlightText = TColors.SysHighlightText;

  { Font style constants - re-exported from System.UITypes where they are
    scoped enums, so shared code can use unqualified fsBold etc. }
  fsBold = System.UITypes.TFontStyle.fsBold;
  fsItalic = System.UITypes.TFontStyle.fsItalic;
  fsUnderline = System.UITypes.TFontStyle.fsUnderline;
  fsStrikeOut = System.UITypes.TFontStyle.fsStrikeOut;

type
  { Re-export font types so shared code doesn't need Vcl.Graphics }
  TFontStyle = System.UITypes.TFontStyle;
  TFontStyles = System.UITypes.TFontStyles;

  TSynAlignment = TAlignment;

var
  SynTabAlignment: TSynAlignment = taCenter;

type
  ESynError = class(Exception);

  TSynEditorOption = (
    eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               //Allows the editor accept OLE file drops
    eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
    eoEnhanceEndKey,           //enhances End key positioning, similar to JDeveloper
    eoGroupUndo,               //When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoKeepCaretX,              //When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret,                 //Makes it so the caret is never visible
    eoNoSelection,             //Disables selecting text
    eoRightMouseMovesCursor,   //When clicking with the right mouse for a popup menu, move the cursor to that location
    eoSmartTabDelete,          //similar to Smart Tabs, but when you delete characters
    eoSmartTabs,               //When tabbing, the cursor will go to the next non-white space character of the previous line
    eoSpecialLineDefaultFg,    //disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent,               //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,            //Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,      //Spaces at the end of lines will be trimmed and not saved
    eoShowLigatures,           //Shows font ligatures, by default it is disabled
    eoCopyPlainText,           //Do not include additional clipboard formats when you copy to Clipboard or drag text
    eoNoHTMLBackground,        //Ignore SynEdit background color when copying in HTML format
    eoWrapWithRightEdge,       //WordWrap with RightEdge position instead of the whole text area
    eoBracketsHighlight,       //Enable bracket highlighting
    eoAccessibility,           //Enable accessibility support
    eoCompleteBrackets,        //When an opening bracket is entered complete the matching one
    eoCompleteQuotes           //When an quote char (" ') is entered add a second one
    );
  TSynEditorOptions = set of TSynEditorOption;

  TSynEditorScrollOption = (
    eoDisableScrollArrows,     //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoHalfPageScroll,          //When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      //if enabled, then the scrollbars will only show when necessary.
    eoScrollByOneLess,         //Forces scrolling to be one less
    eoScrollHintFollows,       //The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof,           //Allows the cursor to go past the end of file marker
    eoScrollPastEol,           //Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint           //Shows a hint of the visible line numbers when scrolling vertically
    );
  TSynEditorScrollOptions = set of TSynEditorScrollOption;


const
  SYNEDIT_DEFAULT_OPTIONS = [
    eoAutoIndent, eoDragDropEditing, eoKeepCaretX,
    eoEnhanceHomeKey, eoEnhanceEndKey, eoTabIndent, eoTabsToSpaces,
    eoSmartTabDelete, eoGroupUndo, eoDropFiles, eoShowLigatures,
    eoBracketsHighlight, eoAccessibility, eoCompleteBrackets, eoCompleteQuotes];
  SYNEDIT_DEFAULT_SCROLLOPTIONS =
    [eoHideShowScrollbars, eoDisableScrollArrows, eoShowScrollHint];

type
  TSynFlowControl = (fcNone, fcContinue, fcBreak, fcExit);

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

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynSpecialChars = (scWhitespace, scControlChars, scEOL);
  TSynVisibleSpecialChars = set of TSynSpecialChars;

  TSynInfoLossEvent = procedure (var Encoding: TEncoding; Cancel: Boolean) of object;

  TBufferCoord = record
    // Char and Line are 1-based
    Char: Integer;
    Line: Integer;
    function ToString(ShortForm: Boolean = True): string;
    class operator Equal(a, b: TBufferCoord): Boolean;
    class operator NotEqual(a, b: TBufferCoord): Boolean;
    class operator LessThan(a, b: TBufferCoord): Boolean;
    class operator LessThanOrEqual(a, b: TBufferCoord): Boolean;
    class operator GreaterThan(a, b: TBufferCoord): Boolean;
    class operator GreaterThanOrEqual(a, b: TBufferCoord): Boolean;
    class function Min(a, b: TBufferCoord): TBufferCoord; static;
    class function Max(a, b: TBufferCoord): TBufferCoord; static;
    class function Invalid: TBufferCoord; static;
    function IsValid: Boolean;
    procedure Swap(var Other: TBufferCoord);
  end;

  TDisplayCoord = record
    Column: Integer;
    Row: Integer;
    class operator Equal(a, b: TDisplayCoord): Boolean;
    class operator NotEqual(a, b: TDisplayCoord): Boolean;
    class operator LessThan(a, b: TDisplayCoord): Boolean;
    class operator LessThanOrEqual(a, b: TDisplayCoord): Boolean;
    class operator GreaterThan(a, b: TDisplayCoord): Boolean;
    class operator GreaterThanOrEqual(a, b: TDisplayCoord): Boolean;
    class function Min(a, b: TDisplayCoord): TDisplayCoord; static;
    class function Max(a, b: TDisplayCoord): TDisplayCoord; static;
  end;

  TSynSelection = record
    Caret: TBufferCoord;
    Start: TBufferCoord;
    Stop: TBufferCoord;
    CaretAtEOL: Boolean;  // used by wordwrap
    LastPosX: Integer;    // in pixels. Used in vertical movements
    procedure Normalize;
    function Normalized: TSynSelection;
    function IsEmpty: Boolean;
    procedure Join(const Sel: TSynSelection);
    function Intersects(const Other: TSynSelection): Boolean;
    function Contains(const BC: TBufferCoord): Boolean;
    constructor Create(const ACaret, AStart, AStop: TBufferCoord; ACaretAtEOL:
        Boolean = False; ALastPosX: Integer = 0);
    class operator Equal(a, b: TSynSelection): Boolean;
    class operator NotEqual(a, b: TSynSelection): Boolean;
    class function Invalid: TSynSelection; static;
    function IsValid: Boolean;
  end;

  TSynSelectionArray = TArray<TSynSelection>;

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
function BufferCoord(AChar, ALine: Integer): TBufferCoord;

type
{ *************************** For Carets **********************************}

TCaretShape = record
  Width: Integer;
  Height: Integer;
  Offset: TPoint;
  constructor Create(AWidth, AHeight: Integer; AOffset: TPoint);
end;


{ ************************* For Word Wrap ********************************}

  // aIndex parameters of Line notifications are 0-based.
  // aRow parameter of GetRowLength() is 1-based.
  ISynEditBufferPlugin = interface
    // conversion methods
    function BufferToDisplayPos(const aPos: TBufferCoord): TDisplayCoord;
    function DisplayToBufferPos(const aPos: TDisplayCoord): TBufferCoord;
    function GetRowLength(aRow: Integer): Integer;
    function RowCount: Integer;
    function RowToLine(aRow: Integer): Integer;
    function LineToRow(aLine: Integer): Integer;
    // plugin notifications
    function LinesInserted(aIndex: Integer; aCount: Integer): Integer;
    function LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
    function LinePut(aIndex: Integer; const OldLine: string): Integer;
    // font or size change
    procedure DisplayChanged;
    procedure Reset;
    property RowLength[RowIndex: Integer]: Integer read GetRowLength;
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

{ ******************** Multicast event chains ****************************}

  ESynMethodChain = class(Exception);
  TSynExceptionEvent = procedure(Sender: TObject; E: Exception;
    var DoContinue: Boolean) of object;

  TSynMethodChain = class(TObject)
  private
    FNotifyProcs: TList;
    FExceptionHandler: TSynExceptionEvent;
  protected
    procedure DoFire(const AEvent: TMethod); virtual; abstract;
    function DoHandleException(E: Exception): Boolean; virtual;
    property ExceptionHandler: TSynExceptionEvent read FExceptionHandler
      write FExceptionHandler;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AEvent: TMethod);
    procedure Remove(AEvent: TMethod);
    procedure Fire;
  end;

  TSynNotifyEventChain = class(TSynMethodChain)
  private
    FSender: TObject;
  protected
    procedure DoFire(const AEvent: TMethod); override;
  public
    constructor CreateEx(ASender: TObject);
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    property ExceptionHandler;
    property Sender: TObject read FSender write FSender;
  end;

{ ************************* For Undo Redo ********************************}

  TSynEditUndoItem = class(TObject)
  public
    ChangeStartPos: TBufferCoord;
    ChangeEndPos: TBufferCoord;
    ChangeStr: string;
    ChangeNumber: Integer;
    ChangeReason: TSynChangeReason;
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
    function GetInsideUndoRedo: Boolean;
    procedure SetModified(const Value: Boolean);
    procedure SetMaxUndoActions(const Value: Integer);
    procedure SetGroupUndo(const Value: Boolean);
    procedure SetOnModifiedChanged(const Event: TNotifyEvent);
    procedure SetCommandProcessed(const Command: TSynEditorCommand);
    {  Begin/EndBlock pairs group undo actions together and also
       store/restore editor caret and selection
       We need to pass the Editor so that they works with chained SynEdits
    }
    procedure BeginBlock(Editor: TObject);
    procedure EndBlock(Editor: TObject);
    { Lock disables undo/redo - useful if you are about to do a large number of
      changes and planning to clear undo afterwards }
    procedure Lock;
    procedure Unlock;
    procedure Clear;
    { Call AddGroupBreak to signal that the next undo action
      cannot be grouped with the current one }
    procedure AddGroupBreak;
    {Note: Undo/Redo are not reentrant}
    procedure Undo(Editor: TObject);
    procedure Redo(Editor: TObject);
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
    property InsideUndoRedo: Boolean read GetInsideUndoRedo;
  end;

{ ************************** Selected Color *********************************}

  TSynSelectedColor = class(TPersistent)
  private
    FBG: TColor;
    FFG: TColor;
    FOnChange: TNotifyEvent;
    FOpacity: Byte;
    FFillWholeLines: Boolean;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
    procedure SetOpacity(Value: Byte);
    procedure SetFillWholeLines(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Background: TColor read FBG write SetBG default clHighLight;
    property Foreground: TColor read FFG write SetFG default clHighLightText;
    property Opacity: Byte read FOpacity write SetOpacity default 115;
    property FillWholeLines: Boolean read FFillWholeLines write SetFillWholeLines
      default True;
  end;

{ *************************** Search Engine ********************************}

  TSynIsWordBreakFunction = function(C: WideChar): Boolean of object;

  TSynEditSearchCustom = class(TComponent)
  protected
    FIsWordBreakFunction: TSynIsWordBreakFunction;
    function GetPattern: string; virtual; abstract;
    procedure SetPattern(const Value: string); virtual; abstract;
    function GetLength(Index: Integer): Integer; virtual; abstract;
    function GetResult(Index: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure SetOptions(const Value: TSynSearchOptions); virtual; abstract;
  public
    function FindAll(const NewText: string; StartChar: Integer = 1;
      EndChar: Integer = 0): Integer; virtual; abstract;
    function PreprocessReplaceExpression(const AReplace: string): string; virtual;
    function Replace(const aOccurrence, aReplacement: string): string;
      virtual; abstract;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[Index: Integer]: Integer read GetResult;
    property Lengths[Index: Integer]: Integer read GetLength;
    property Options: TSynSearchOptions write SetOptions;
    property IsWordBreakFunction: TSynIsWordBreakFunction write FIsWordBreakFunction;
  end;

implementation
Uses
  SynEditStrConst,
  SynUnicodeShared;

{$REGION 'TSynSelectedColor'}

constructor TSynSelectedColor.Create;
begin
  inherited Create;
  FBG := clHighLight;
  FFG := clHighLightText;
  FFillWholeLines := True;
  FOpacity := 115;
end;

procedure TSynSelectedColor.Assign(Source: TPersistent);
begin
  if Source is TSynSelectedColor then
  begin
    var Src := TSynSelectedColor(Source);
    FBG := Src.FBG;
    FFG := Src.FFG;
    FOpacity := Src.Opacity;
    FFillWholeLines := Src.FillWholeLines;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if FBG <> Value then
  begin
    FBG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if FFG <> Value then
  begin
    FFG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetOpacity(Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFillWholeLines(const Value: Boolean);
begin
  if FFillWholeLines <> Value then
  begin
    FFillWholeLines := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{$ENDREGION}

{ TSynEditSearchCustom }

function TSynEditSearchCustom.PreprocessReplaceExpression(const AReplace
  : string): string;
begin
  Result := AReplace;
end;

{$REGION 'TSynMethodChain'}

procedure TSynMethodChain.Add(AEvent: TMethod);
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt
      ('%s.Entry: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    Add(Code);
    Add(Data);
  end
end;

constructor TSynMethodChain.Create;
begin
  inherited;
  FNotifyProcs := TList.Create;
end;

destructor TSynMethodChain.Destroy;
begin
  FNotifyProcs.Free;
  inherited;
end;

function TSynMethodChain.DoHandleException(E: Exception): Boolean;
begin
  if not Assigned(FExceptionHandler) then
    raise E
  else
    try
      Result := True;
      FExceptionHandler(Self, E, Result);
    except
      raise ESynMethodChain.CreateFmt
        ('%s.DoHandleException: MUST NOT occur any kind of exception in ' +
        'ExceptionHandler', [ClassName]);
    end;
end;

procedure TSynMethodChain.Fire;
var
  AMethod: TMethod;
  I: Integer;
begin
  I := 0;
  with FNotifyProcs, AMethod do
    while I < Count do
      try
        repeat
          Code := Items[I];
          Inc(I);
          Data := Items[I];
          Inc(I);

          DoFire(AMethod)
        until I >= Count;
      except
        on E: Exception do
          if not DoHandleException(E) then
            I := MaxInt;
      end;
end;

procedure TSynMethodChain.Remove(AEvent: TMethod);
var
  I: Integer;
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt
      ('%s.Remove: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    I := Count - 1;
    while I > 0 do
      if Items[I] <> Data then
        Dec(I, 2)
      else
      begin
        Dec(I);
        if Items[I] = Code then
        begin
          Delete(I);
          Delete(I);
        end;
        Dec(I);
      end;
  end;
end;

{$ENDREGION}

{$REGION 'TSynNotifyEventChain'}

procedure TSynNotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  inherited Add(TMethod(AEvent));
end;

constructor TSynNotifyEventChain.CreateEx(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
end;

procedure TSynNotifyEventChain.DoFire(const AEvent: TMethod);
begin
  TNotifyEvent(AEvent)(FSender);
end;

procedure TSynNotifyEventChain.Remove(AEvent: TNotifyEvent);
begin
  inherited Remove(TMethod(AEvent));
end;

{$ENDREGION}

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

class function TBufferCoord.Invalid: TBufferCoord;
begin
  Result.Char := 0;
  Result.Line := 0;
end;

function TBufferCoord.IsValid: Boolean;
begin
  Result := (Char > 0) and (Line > 0);
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

procedure TBufferCoord.Swap(var Other: TBufferCoord);
var
  Temp: TBufferCoord;
begin
  Temp := Other;
  Other := Self;
  Self := Temp;
end;

function TBufferCoord.ToString(ShortForm: Boolean = True): string;
begin
  if ShortForm then
    Result := Format('%d:%d', [Self.Line, Self.Char])
  else
    Result := Format('%s: %d %s: %d',[SYNS_Line, Self.Line, SYNS_Char, Self.Char]);
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

{ TSynSelection }

function TSynSelection.Contains(const BC: TBufferCoord): Boolean;
begin
  Result := (BC >= TBufferCoord.Min(Start, Stop)) and
    (BC < TBufferCoord.Max(Start, Stop));
end;

constructor TSynSelection.Create(const ACaret, AStart, AStop: TBufferCoord;
    ACaretAtEOL: Boolean = False; ALastPosX: Integer = 0);
begin
  Caret := ACaret;
  Start := AStart;
  Stop := AStop;
  CaretAtEOL := ACaretAtEOL;
  LastPosX := ALastPosX;
end;

class operator TSynSelection.Equal(a, b: TSynSelection): Boolean;
begin
  Result := (a.Start = b.Start) and (a.Stop = b.Stop);
end;

function TSynSelection.Intersects(const Other: TSynSelection): Boolean;
begin
  Result := Self.Contains(Other.Start) or Self.Contains(Other.Stop) or
    Other.Contains(TBufferCoord.Min(Self.Start, Self.Stop));
end;

class function TSynSelection.Invalid: TSynSelection;
begin
  Result := TSynSelection.Create(TBufferCoord.Invalid, TBufferCoord.Invalid,
    TBufferCoord.Invalid);
end;

function TSynSelection.IsEmpty: Boolean;
begin
  Result := Start = Stop
end;

function TSynSelection.IsValid: Boolean;
begin
  Result := Caret.IsValid and Start.IsValid and Stop.IsValid;
end;

procedure TSynSelection.Join(const Sel: TSynSelection);
var
  N1, N2: TSynSelection;
begin
  N1 := Normalized;
  N2 := Sel.Normalized;
  Start := TBufferCoord.Min(N1.Start, N2.Start);
  Stop := TBufferCoord.Max(N1.Stop, N2.Stop);
  //  Set the caret to the Start or the Stop depending on the orignal carets
  if (Sel.Caret = Start) or (Sel.Caret = Stop) then
    Caret := Sel.Caret
  else if not (Caret = Start) and not (Caret = Stop) then
    Caret := Stop;
end;

procedure TSynSelection.Normalize;
begin
  if Start > Stop then
  begin
    var Temp := Start;
    Start := Stop;
    Stop := Temp;
    Caret := Stop;
  end;
end;

function TSynSelection.Normalized: TSynSelection;
begin
  Result := Self;
  Result.Normalize;
end;

class operator TSynSelection.NotEqual(a, b: TSynSelection): Boolean;
begin
  Result := (a.Start <> b.Start) or (a.Stop <> b.Stop)
end;

{ TCaretShape }

constructor TCaretShape.Create(AWidth, AHeight: Integer; AOffset: TPoint);
begin
  Width := AWidth;
  Height := AHeight;
  Offset := AOffset;
end;

end.

