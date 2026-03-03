{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMacroRecorder.pas, released 2001-10-17.

Author of this file is Fl�vio Etrusco.
Portions created by Fl�vio Etrusco are Copyright 2001 Fl�vio Etrusco.
Unicode translation by Ma�l H�rz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

SynMacroRecorderShared: Cross-platform shared macro event types and factory.
No VCL/FMX imports.
-------------------------------------------------------------------------------}

unit SynMacroRecorderShared;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  SynEditTypes,
  SynEditKeyCmds;

resourcestring
  sCannotRecord = 'Cannot record macro; already recording or playing';
  sCannotPlay = 'Cannot playback macro; already playing or recording';
  sCannotPause = 'Can only pause when recording';
  sCannotResume = 'Can only resume when paused';

type
  TSynMacroState = (msStopped, msRecording, msPlaying, msPaused);
  TSynMacroCommand = (mcRecord, mcPlayback);

  TSynMacroPlaybackProc = procedure(Command: TSynEditorCommand;
    AChar: WideChar; Data: Pointer) of object;

  TSynMacroEvent = class(TObject)
  protected
    fRepeatCount: Byte;
    function GetAsString: string; virtual; abstract;
  public
    procedure InitEventParameters(aStr: string); virtual; abstract;
    constructor Create; virtual;
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
      aData: Pointer); virtual; abstract;
    { the CommandID must not be read inside LoadFromStream/SaveToStream. It's
      read by the MacroRecorder component to decide which MacroEvent class to
      instantiate }
    procedure LoadFromStream(aStream: TStream); virtual; abstract;
    procedure SaveToStream(aStream: TStream); virtual; abstract;
    procedure PlaybackTo(const APlaybackProc: TSynMacroPlaybackProc);
      virtual; abstract;
    property AsString: string read GetAsString;
    property RepeatCount: Byte read fRepeatCount write fRepeatCount;
  end;

  TSynBasicEvent = class(TSynMacroEvent)
  protected
    fCommand: TSynEditorCommand;
    function GetAsString: string; override;
  public
    procedure InitEventParameters(aStr: string); override;
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
      aData: Pointer); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure PlaybackTo(const APlaybackProc: TSynMacroPlaybackProc); override;
    property Command: TSynEditorCommand read fCommand write fCommand;
  end;

  TSynCharEvent = class(TSynMacroEvent)
  protected
    fKey: WideChar;
    function GetAsString: string; override;
  public
    procedure InitEventParameters(aStr: string); override;
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
      aData: Pointer); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure PlaybackTo(const APlaybackProc: TSynMacroPlaybackProc); override;
    property Key: WideChar read fKey write fKey;
  end;

  TSynStringEvent = class(TSynMacroEvent)
  protected
    fString: string;
    function GetAsString: string; override;
  public
    procedure InitEventParameters(aStr: string); override;
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
      aData: Pointer); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure PlaybackTo(const APlaybackProc: TSynMacroPlaybackProc); override;
    property Value: string read fString write fString;
  end;

  TSynPositionEvent = class(TSynBasicEvent)
  protected
    fPosition: TBufferCoord;
    function GetAsString: string; override;
  public
    procedure InitEventParameters(aStr: string); override;
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
      aData: Pointer); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure PlaybackTo(const APlaybackProc: TSynMacroPlaybackProc); override;
    property Position: TBufferCoord read fPosition write fPosition;
  end;

  TSynDataEvent = class(TSynBasicEvent)
  protected
    fData: Pointer;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
      aData: Pointer); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure PlaybackTo(const APlaybackProc: TSynMacroPlaybackProc); override;
  end;

function CreateMacroEvent(aCmd: TSynEditorCommand): TSynMacroEvent;

implementation

uses
  SynEditMiscProcs,
  SynUnicodeShared;

{ TSynMacroEvent }

constructor TSynMacroEvent.Create;
begin
  inherited Create;
  fRepeatCount := 1;
end;

{ TSynBasicEvent }

function TSynBasicEvent.GetAsString: string;
var
  Ident: string;
begin
  EditorCommandToIdent(Command, Ident);
  Result := Ident;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynBasicEvent.InitEventParameters(aStr: string);
begin
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynBasicEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  Command := aCmd;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  if (aChar <> #0) or (aData <> nil) then
    raise Exception.Create('TSynBasicEvent cannot handle Char <> #0 or Data <> nil');
{$ENDIF}
end;

procedure TSynBasicEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(fRepeatCount, SizeOf(fRepeatCount));
end;

procedure TSynBasicEvent.PlaybackTo(
  const APlaybackProc: TSynMacroPlaybackProc);
var
  i: Integer;
begin
  for i := 1 to RepeatCount do
    APlaybackProc(Command, #0, nil);
end;

procedure TSynBasicEvent.SaveToStream(aStream: TStream);
begin
  aStream.Write(Command, SizeOf(TSynEditorCommand));
  aStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TSynCharEvent }

function TSynCharEvent.GetAsString: string;
var
  Ident: string;
begin
  EditorCommandToIdent(ecChar, Ident);
  Result := Ident + ' ' + Key;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynCharEvent.InitEventParameters(aStr: string);
begin
  if Length(aStr) >= 1 then
    Key := aStr[1]
  else
    Key := ' ';
  Delete(aStr, 1, 1);
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynCharEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  Key := aChar;
  Assert(aData = nil);
end;

procedure TSynCharEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(fKey, SizeOf(Key));
  aStream.Read(fRepeatCount, SizeOf(fRepeatCount));
end;

procedure TSynCharEvent.PlaybackTo(
  const APlaybackProc: TSynMacroPlaybackProc);
var
  i: Integer;
begin
  for i := 1 to RepeatCount do
    APlaybackProc(ecChar, Key, nil);
end;

procedure TSynCharEvent.SaveToStream(aStream: TStream);
const
  iCharCommand: TSynEditorCommand = ecChar;
begin
  aStream.Write(iCharCommand, SizeOf(TSynEditorCommand));
  aStream.Write(Key, SizeOf(Key));
  aStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TSynStringEvent }

function TSynStringEvent.GetAsString: string;
var
  Ident: string;
begin
  EditorCommandToIdent(ecString, Ident);
  Result := Ident + ' ' + AnsiQuotedStr(Value, #39);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynStringEvent.InitEventParameters(aStr: string);
var
  o, c: Integer;
  valStr: string;
begin
  o := Pos('''', aStr);
  c := LastDelimiter('''', aStr);
  valStr := Copy(aStr, o + 1, c - o - 1);
  Value := StringReplace(valStr, '''''', '''', [rfReplaceAll]);
  Delete(aStr, 1, c);
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynStringEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  Value := string(aData);
end;

procedure TSynStringEvent.LoadFromStream(aStream: TStream);
var
  l: Integer;
  Buff: PWideChar;
begin
  aStream.Read(l, SizeOf(l));
  GetMem(Buff, l * SizeOf(WideChar));
  try
    FillChar(Buff^, l * SizeOf(WideChar), 0);
    aStream.Read(Buff^, l * SizeOf(WideChar));
    fString := Buff;
  finally
    FreeMem(Buff);
  end;
  aStream.Read(fRepeatCount, SizeOf(fRepeatCount));
end;

procedure TSynStringEvent.PlaybackTo(
  const APlaybackProc: TSynMacroPlaybackProc);
var
  i, j: Integer;
begin
  for j := 1 to RepeatCount do
  begin
    // SynEdit doesn't actually support the ecString command so we convert
    // it into ecChar commands
    for i := 1 to Length(Value) do
      APlaybackProc(ecChar, Value[i], nil);
  end;
end;

procedure TSynStringEvent.SaveToStream(aStream: TStream);
const
  StrCommand: TSynEditorCommand = ecString;
var
  l: Integer;
  Buff: PWideChar;
begin
  aStream.Write(StrCommand, SizeOf(StrCommand));
  l := Length(Value) + 1;
  aStream.Write(l, SizeOf(l));
  GetMem(Buff, l * SizeOf(WideChar));
  try
    FillChar(Buff^, l * SizeOf(WideChar), 0);
    Move(PWideChar(Value)^, Buff^, (l - 1) * SizeOf(WideChar));
    aStream.Write(Buff^, l * SizeOf(WideChar));
  finally
    FreeMem(Buff);
  end;
  aStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TSynPositionEvent }

function TSynPositionEvent.GetAsString: string;
begin
  Result := inherited GetAsString;
  Result := Result + Format(' (%d, %d)', [Position.Char, Position.Line]);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynPositionEvent.InitEventParameters(aStr: string);
var
  i, o, c, x, y: Integer;
  valStr: string;
begin
  inherited;
  aStr := Trim(aStr);
  i := Pos(',', aStr);
  o := Pos('(', aStr);
  c := Pos(')', aStr);
  if (not ((i = 0) or (o = 0) or (c = 0))) and
     ((i > o) and (i < c)) then
  begin
    valStr := Copy(aStr, o + 1, i - o - 1);
    x := StrToIntDef(valStr, 1);
    Delete(aStr, 1, i);
    aStr := Trim(aStr);
    c := Pos(')', aStr);
    valStr := Copy(aStr, 1, c - 1);
    y := StrToIntDef(valStr, 1);
    Position := BufferCoord(x, y);
    Delete(aStr, 1, c);
    aStr := Trim(aStr);
    RepeatCount := StrToIntDef(aStr, 1);
  end;
end;

procedure TSynPositionEvent.Initialize(aCmd: TSynEditorCommand;
  aChar: WideChar; aData: Pointer);
begin
  inherited;
  if aData <> nil then
    Position := TBufferCoord(aData^)
  else
    Position := BufferCoord(0, 0);
end;

procedure TSynPositionEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(fPosition, SizeOf(Position));
end;

procedure TSynPositionEvent.PlaybackTo(
  const APlaybackProc: TSynMacroPlaybackProc);
begin
  if (Position.Char <> 0) or (Position.Line <> 0) then
    APlaybackProc(Command, #0, @fPosition)
  else
    APlaybackProc(Command, #0, nil);
end;

procedure TSynPositionEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write(Position, SizeOf(Position));
end;

{ TSynDataEvent }

procedure TSynDataEvent.Initialize(aCmd: TSynEditorCommand; aChar: WideChar;
  aData: Pointer);
begin
  fCommand := aCmd;
  Assert(aChar = #0);
  fData := aData;
end;

procedure TSynDataEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read(fData, SizeOf(fData));
end;

procedure TSynDataEvent.PlaybackTo(
  const APlaybackProc: TSynMacroPlaybackProc);
begin
  APlaybackProc(Command, #0, fData);
end;

procedure TSynDataEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write(fData, SizeOf(fData));
end;

{ Factory }

function CreateMacroEvent(aCmd: TSynEditorCommand): TSynMacroEvent;
begin
  case aCmd of
    ecGotoXY, ecSelGotoXY, ecSetMarker0..ecSetMarker9:
      begin
        Result := TSynPositionEvent.Create;
        TSynPositionEvent(Result).Command := aCmd;
      end;
    ecChar:
      Result := TSynCharEvent.Create;
    ecString:
      Result := TSynStringEvent.Create;
  else
    begin
      Result := TSynBasicEvent.Create;
      TSynBasicEvent(Result).Command := aCmd;
    end;
  end;
end;

end.
