{ ------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: SynEditTextBuffer.pas, released 2000-04-07.
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

  Known Issues:
  -----------------------------------------------------------------------------}

unit SynEditTextBuffer;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  SynEditTypes,
  SynEditMiscProcs,
  SynFunc,
  SynUnicode;

type
  TSynEditRange = Pointer;

  TSynEditStringFlag = (sfTextWidthUnknown, sfModified, sfSaved, sfAsSaved);
  TSynEditStringFlags = set of TSynEditStringFlag;

  // Managed by Undo
  TSynLineChangeFlag = sfModified..sfAsSaved;
  TSynLineChangeFlags = set of TSynLineChangeFlag;

  PSynEditStringRec = ^TSynEditStringRec;

  TSynEditStringRec = record
    FString: string;
    FObject: TObject;
    FRange: TSynEditRange;
    FTextWidth: TSynNativeInt;
    FCharIndex: TSynNativeInt;
    FFlags: TSynEditStringFlags;
  end;

  TSynEditTwoWideChars = record
    One, Two: WideChar;
  end;

  PSynEditTwoWideChars = ^TSynEditTwoWideChars;

const
  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  NullRange = TSynEditRange(-1);

type
  TSynEditStringRecList = TArray<TSynEditStringRec>;

  TStringListChangeEvent = procedure(Sender: TObject; Index: TSynNativeInt;
    Count: TSynNativeInt) of object;
  TStringListPutEvent = procedure(Sender: TObject; Index: TSynNativeInt;
    const OldLine: string) of object;

  TTextWidthFunc = function(const S: string): TSynNativeInt of object;

  TSynEditStringList = class(TStrings)
  strict private
    FTextWidthFunc: TTextWidthFunc;
    FList: TSynEditStringRecList;
    FCount: TSynNativeInt;
    FCapacity: TSynNativeInt;
    FFileFormat: TSynEditFileFormat;
    FMaxWidth: TSynNativeInt;
    FTabWidth: Integer;
    FCharIndexesAreValid: Boolean;
    FDetectUTF8: Boolean;
    FUTF8CheckLen: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOnCleared: TNotifyEvent;
    FOnBeforeDeleted: TStringListChangeEvent;
    FOnDeleted: TStringListChangeEvent;
    FOnInserted: TStringListChangeEvent;
    FOnPut: TStringListPutEvent;
    FOnInfoLoss: TSynInfoLossEvent;
    function GetCapacityNative: TSynNativeInt;
    function GetTextWidth(Index: TSynNativeInt): TSynNativeInt;
    function GetMaxWidth: TSynNativeInt;
    function GetRange(Index: TSynNativeInt): TSynEditRange;
    procedure Grow;
    procedure InsertItem(Index: TSynNativeInt; const S: string);
    procedure PutRange(Index: TSynNativeInt; ARange: TSynEditRange);
    function GetChangeFlags(Index: TSynNativeInt): TSynLineChangeFlags;
    function GetCountNative: TSynNativeInt;
    procedure SetChangeFlags(Index: TSynNativeInt; const Value: TSynLineChangeFlags);
    function GetFileFormat: TSynEditFileFormat;
    function GetNative(Index: TSynNativeInt): string;
    function GetObjectNative(Index: TSynNativeInt): TObject;
    procedure PutNative(Index: TSynNativeInt; const S: string);
    procedure PutObjectNative(AIndex: TSynNativeInt; const AValue: TObject);
    procedure SetFileFormat(const Value: TSynEditFileFormat);
    procedure SetCapacityNative(const AValue: TSynNativeInt);
  strict protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    // TStrings overriden protected methods
    function Get(Index: Integer): string; override;{$IF COMPILERVERSION >= 38} deprecated;{$IFEND}
    function GetCapacity: Integer; override; deprecated;
    function GetCount: Integer; override; deprecated;
    function GetObject(Index: Integer): TObject; override;{$IF COMPILERVERSION >= 38} deprecated;{$IFEND}
    procedure Put(Index: Integer; const S: string); override;{$IF COMPILERVERSION >= 38} deprecated;{$IFEND}
    procedure PutObject(Index: Integer; AObject: TObject); override;{$IF COMPILERVERSION >= 38} deprecated;{$IFEND}
    procedure SetCapacity(AValue: Integer); override; deprecated;
    procedure SetTextStr(const Value: string); override;
    procedure SetUpdateState(Updating: Boolean); override; deprecated;
    // Other protected methods
    procedure SetTabWidth(Value: Integer);
    procedure UpdateCharIndexes;
  public
    // TStrings overriden public methods
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure DeleteNative(Index: TSynNativeInt);
    procedure DeleteLines(Index, NumLines: TSynNativeInt);
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertNative(Index: TSynNativeInt; const S: string);
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); override;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); override;
    procedure SetEncoding(const Value: TEncoding); override; // just to elevate
    // Other public methods
    constructor Create(TextWidthFunc: TTextWidthFunc);
    destructor Destroy; override;
    procedure InsertStrings(Index: TSynNativeInt; Strings: TArray<string>;
      FromIndex: TSynNativeInt = 0);
    procedure InsertText(Index: TSynNativeInt; NewText: string);
    function GetSeparatedText(Separators: string): string;
    procedure ResetMaxWidth;
    function LineCharLength(Index: TSynNativeInt): Integer;
    function LineCharIndex(Index: TSynNativeInt): TSynNativeInt;
    procedure SetTextAndFileFormat(const Value: string);

    property CapacityNative: TSynNativeInt read GetCapacityNative write SetCapacityNative;
    property CountNative: TSynNativeInt read GetCountNative;
    property ObjectsNative[Index: TSynNativeInt]: TObject read GetObjectNative write PutObjectNative;
    property StringsNative[Index: TSynNativeInt]: string read GetNative write PutNative; default;
    // FileFormat is deprecated and will be removed - Use LineBreak instead
    property FileFormat: TSynEditFileFormat read GetFileFormat write SetFileFormat;
    property TextWidth[Index: TSynNativeInt]: TSynNativeInt read GetTextWidth;
    property MaxWidth: TSynNativeInt read GetMaxWidth;
    property Ranges[Index: TSynNativeInt]: TSynEditRange read GetRange write PutRange;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property UTF8CheckLen: Integer read FUTF8CheckLen write FUTF8CheckLen;
    property DetectUTF8: Boolean read FDetectUTF8 write FDetectUTF8;
    property ChangeFlags[Index: TSynNativeInt]: TSynLineChangeFlags read GetChangeFlags
      write SetChangeFlags;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnBeforeDeleted: TStringListChangeEvent read FOnBeforeDeleted
      write FOnBeforeDeleted;
    property OnDeleted: TStringListChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TStringListChangeEvent read FOnInserted
      write FOnInserted;
    property OnPut: TStringListPutEvent read FOnPut write FOnPut;
    property OnInfoLoss: TSynInfoLossEvent read FOnInfoLoss write FOnInfoLoss;
  end;

  ESynEditStringList = class(Exception);

implementation
uses
  System.Math,
  System.Threading;

resourcestring
  SListIndexOutOfBounds = 'Invalid stringlist index %d';
  SInvalidCapacity = 'Stringlist capacity cannot be smaller than count';

  { TSynEditStringList }

procedure ListIndexOutOfBounds(Index: TSynNativeInt);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create;
begin
  inherited Create;
  FFileFormat := sffDos;
  FMaxWidth := -1;
  FTabWidth := 8;
  FUTF8CheckLen := -1;
{$IF COMPILERVERSION >= 31}
  Options := Options - [soWriteBOM, soTrailingLineBreak];
{$ELSE}
  WriteBOM := False;
{$ENDIF}
  FDetectUTF8 := True;
  FTextWidthFunc := TextWidthFunc;
end;

destructor TSynEditStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  FCount := 0;
  SetCapacityNative(0);
end;

procedure TSynEditStringList.Changed;
begin
  if not Updating and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynEditStringList.Changing;
begin
  FCharIndexesAreValid := False;
  if not Updating and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TSynEditStringList.Clear;
var
  OldCount: TSynNativeInt;
begin
  if FCount <> 0 then
  begin
    Changing;
    OldCount := FCount;
    if Assigned(FOnBeforeDeleted) then
      FOnBeforeDeleted(Self, 0, OldCount);
    FCount := 0;
    SetCapacityNative(0);
    if Assigned(FOnDeleted) then
      FOnDeleted(Self, 0, OldCount);
    if Assigned(FOnCleared) then
      FOnCleared(Self);
    Changed;
  end;
  FMaxWidth := -1;
end;

procedure TSynEditStringList.Delete(Index: Integer);
begin
  DeleteNative(ToSynNativeInt(Index));
end;

procedure TSynEditStringList.DeleteNative(Index: TSynNativeInt);
begin
  if (Index < 0) or (Index >= FCount) then
    ListIndexOutOfBounds(Index);
  Changing;
  if Assigned(FOnBeforeDeleted) then
    FOnBeforeDeleted(Self, Index, 1);
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SynEditStringRecSize);
  end;
  FMaxWidth := -1;
  if Assigned(FOnDeleted) then
    FOnDeleted(Self, Index, 1);
  Changed;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: TSynNativeInt);
var
  LinesAfter: TSynNativeInt;
begin
  if NumLines > 0 then
  begin
    if (Index < 0) or (Index >= FCount) then
      ListIndexOutOfBounds(Index);

    Changing;
    if Assigned(FOnBeforeDeleted) then
      FOnBeforeDeleted(Self, Index, NumLines);

    LinesAfter := FCount - (Index + NumLines);
    if LinesAfter < 0 then
      NumLines := FCount - Index;

    if LinesAfter > 0 then
      System.Move(FList[Index + NumLines], FList[Index],
        LinesAfter * SynEditStringRecSize);
    Dec(FCount, NumLines);
    if Assigned(FOnDeleted) then
      FOnDeleted(Self, Index, NumLines);
    Changed;
  end;
end;

function TSynEditStringList.Get(Index: Integer): string;
begin
  Result := GetNative(ToSynNativeInt(Index));
end;

procedure TSynEditStringList.UpdateCharIndexes;
var
  I, N: TSynNativeInt;
  P: PSynEditStringRec;
begin
  FCharIndexesAreValid := True;
  if FCount = 0 then
    Exit;
  P := @FList[0];
  N := 0;
  for I := 1 to FCount do
  begin
    P.FCharIndex := N;
    Inc(N, Length(P.FString));
    Inc(P);
  end;
end;

function TSynEditStringList.LineCharLength(Index: TSynNativeInt): Integer;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := Length(FList[Index].FString)
  else
    Result := 0;
end;

function TSynEditStringList.LineCharIndex(Index: TSynNativeInt): TSynNativeInt;
begin
  if (Index >= 0)  and (Index < FCount) then
  begin
    if not FCharIndexesAreValid then
      UpdateCharIndexes;
    Result := FList[Index].FCharIndex;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetCapacity: Integer;
begin
  Result := ToInt32(FCapacity);
end;

function TSynEditStringList.GetCapacityNative: TSynNativeInt;
begin
  Result := FCapacity;
end;

function TSynEditStringList.GetChangeFlags(Index: TSynNativeInt): TSynLineChangeFlags;
begin
  if (Index >= 0) and (Index < FCount) then
    Result :=  FList[Index].FFlags * [sfModified..sfAsSaved]
  else
    Result := [];
end;

function TSynEditStringList.GetCount: Integer;
begin
  Result := ToInt32(FCount);
end;

function TSynEditStringList.GetCountNative: TSynNativeInt;
begin
  Result := FCount;
end;

function TSynEditStringList.GetFileFormat: TSynEditFileFormat;
begin
  if LineBreak = WideLF then
    Result := sffUnix
  else if LineBreak = WideCR then
    Result := sffMac
  else if LineBreak = WideLineSeparator then
    Result := sffUnicode
  else
    Result := sffDos;
end;

function TSynEditStringList.GetTextWidth(Index: TSynNativeInt): TSynNativeInt;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    if sfTextWidthUnknown in FList[Index].FFlags then
    begin
      Result := FTextWidthFunc(FList[Index].FString);
      FList[Index].FTextWidth := Result;
      Exclude(FList[Index].FFlags, sfTextWidthUnknown);
    end
    else
      Result := FList[Index].FTextWidth;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetMaxWidth: TSynNativeInt;
begin
  if FMaxWidth > 0 then
    Result := FMaxWidth
  else if FCount = 0 then
    Result := 0
  else
  begin
    TParallel.&For(0, FCount - 1, procedure(I: TSynNativeInt)
    var
      LMaxW: TSynNativeInt;
      PRec: PSynEditStringRec;
    begin
      PRec := @FList[I];
      if sfTextWidthUnknown in PRec^.FFlags then
      begin
        PRec^.FTextWidth := FTextWidthFunc(PRec^.FString);
        Exclude(PRec^.FFlags, sfTextWidthUnknown);
      end;
      repeat
        LMaxW := FMaxWidth;
        if PRec^.FTextWidth <= LMaxW then
          Break;
      until AtomicCmpExchange(FMaxWidth, PRec^.FTextWidth, LMaxW) = LMaxW;
    end);
    Result := Max(FMaxWidth, 0);
  end;
end;

function TSynEditStringList.GetObject(Index: Integer): TObject;
begin
  Result := GetObjectNative(ToSynNativeInt(Index));
end;

function TSynEditStringList.GetObjectNative(Index: TSynNativeInt): TObject;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index].FObject
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: TSynNativeInt): TSynEditRange;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index].FRange
  else
    Result := nil;
end;

function TSynEditStringList.GetSeparatedText(Separators: string): string;
{ Optimized by Eric Grange }
var
  I: TSynNativeInt;
  Size: TSynNativeInt;
  L, LineBreakSize: Integer;
  P, PLineBreak: PChar;
  PRec: PSynEditStringRec;
begin
  if FCount = 0 then
  begin
    Result := '';
    Exit;
  end;
  LineBreakSize := Length(Separators);
  PLineBreak := Pointer(Separators);

  // compute buffer size
  Size := (FCount - 1) * LineBreakSize + LineCharIndex(FCount - 1) +
    ToSynNativeInt(Length(FList[FCount - 1].FString));
  SetLength(Result, Size);

  P := Pointer(Result);
  PRec := @FList[0];

  // handle 1st line separately (to avoid trailing line break)
  L := Length(PRec.FString);
  if L <> 0 then
  begin
    System.Move(Pointer(PRec.FString)^, P^, L * SizeOf(Char));
    Inc(P, L);
  end;
  Inc(PRec);

  for I := 1 to FCount - 1 do
  begin
    case LineBreakSize of
      0:
        ;
      1:
        begin
          P^ := PLineBreak^;
          Inc(P);
        end;
      2:
        begin
          PSynEditTwoWideChars(P)^ := PSynEditTwoWideChars(PLineBreak)^;
          Inc(P, 2);
        end;
    else
      System.Move(PLineBreak^, P^, LineBreakSize * SizeOf(Char));
      Inc(P, LineBreakSize);
    end;
    if Pointer(PRec.FString) <> nil then
    begin
      L := Length(PRec.FString);
      System.Move(Pointer(PRec.FString)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    Inc(PRec);
  end;
end;

function TSynEditStringList.GetNative(Index: TSynNativeInt): string;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index].FString
  else
    Result := '';
end;

procedure TSynEditStringList.Grow;
begin
  SetCapacityNative(GrowCollection(FCapacity, FCount + 1));
end;

procedure TSynEditStringList.Insert(Index: Integer; const S: string);
begin
  InsertNative(ToSynNativeInt(Index), S);
end;

procedure TSynEditStringList.InsertNative(Index: TSynNativeInt; const S: string);
begin
  if Pos(#10, S) > 0 then
    InsertText(Index, S)
  else
    InsertStrings(Index, [S]);
end;

procedure TSynEditStringList.InsertItem(Index: TSynNativeInt; const S: string);
begin
  if FCount = FCapacity then
    Grow;

  Changing;
  if Index < FCount then
  begin
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SynEditStringRecSize);
  end;
  FMaxWidth := -1;
  with FList[Index] do
  begin
    Pointer(FString) := nil;
    FString := S;
    FObject := nil;
    FRange := NullRange;
    FTextWidth := -1;
    FFlags := [sfTextWidthUnknown];
  end;
  Inc(FCount);
  Changed;
end;

procedure TSynEditStringList.InsertStrings(Index: TSynNativeInt;
  Strings: TArray<string>; FromIndex: TSynNativeInt);
var
  I: TSynNativeInt;
  LineCount: TSynNativeInt;
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);

  LineCount := ToSynNativeInt(Length(Strings) - FromIndex);
  if LineCount > 0 then
  begin
    if FCapacity < FCount + LineCount then
      SetCapacityNative(GrowCollection(FCapacity, FCount + LineCount));

    Changing;
    if Index < FCount then
    begin
      System.Move(FList[Index], FList[Index + LineCount],
        (FCount - Index) * SynEditStringRecSize);
    end;
    for I := 0 to LineCount - 1 do
      with FList[Index + I] do
      begin
        Pointer(FString) := nil;
        FString := Strings[FromIndex + I];
        FObject := nil;
        FRange := NullRange;
        FTextWidth := -1;
        FFlags := [sfTextWidthUnknown];
      end;
    Inc(FCount, LineCount);
    FMaxWidth := -1;
    if Assigned(OnInserted) then
      OnInserted(Self, Index, LineCount);
    Changed;
  end;
end;

procedure TSynEditStringList.InsertText(Index: TSynNativeInt; NewText: string);
var
  TmpStringList: TStringList;
begin
  if NewText = '' then
    Exit;

  TmpStringList := TStringList.Create;
  try
    TmpStringList.Text := NewText;
    InsertStrings(Index, TmpStringList.ToStringArray);
  finally
    TmpStringList.Free;
  end;
end;

procedure TSynEditStringList.LoadFromStream(Stream: TStream;
  Encoding: TEncoding);
var
  Size: TSynNativeInt;
  Buffer: TBytes;
  DecodedText: string;
begin
  Size := TSynNativeInt(Stream.Size - Stream.Position);
  SetLength(Buffer, Size);
  Stream.Read(Buffer, 0, Size);
  Size := TEncoding.GetBufferEncoding(Buffer, Encoding, DefaultEncoding);
  WriteBOM := Size > 0; // Keep WriteBom in case the stream is saved
  // If the encoding is ANSI and DetectUtf8 is True try to Detect UTF8
  if (Encoding = TEncoding.ANSI) and DetectUTF8 and IsUTF8(Buffer, ToInt32(Size)) then
    Encoding := TEncoding.UTF8;
  SetEncoding(Encoding); // Keep Encoding in case the stream is saved
  DecodedText := Encoding.GetString(Buffer, ToInt32(Size), ToInt32(Length(Buffer) - Size));
  SetLength(Buffer, 0); // Free the buffer here to reduce memory footprint
  SetTextAndFileFormat(DecodedText);
end;

procedure TSynEditStringList.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Cancel: Boolean;
  S: string;
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := DefaultEncoding;

  S := GetTextStr;

  Cancel := False;
  if (Encoding = TEncoding.ANSI) and Assigned(FOnInfoLoss) and not IsAnsiOnly(S)
  then
  begin
    FOnInfoLoss(Encoding, Cancel);
    if Cancel then
      Exit;
    if Encoding <> TEncoding.ANSI then
      SetEncoding(Encoding);
  end;

  Buffer := Encoding.GetBytes(S);
  if WriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

procedure TSynEditStringList.Put(Index: Integer; const S: string);
begin
  PutNative(ToSynNativeInt(Index), S);
end;

procedure TSynEditStringList.PutNative(Index: TSynNativeInt; const S: string);
var
  OldLine: string;
  OldWidth: TSynNativeInt;
begin
  if (Index = 0) and (FCount = 0) or (FCount = Index) then
    Add('');

  if (Index < 0) or (Index >= FCount) then
    ListIndexOutOfBounds(Index);

  Changing;
  with FList[Index] do
  begin
    OldLine := FString;
    FString := S;

    if FMaxWidth >= 0 then
    begin
      // Optimization:  We calculate text width here, thus
      // in most cases avoiding to recalc FMaxWidth the hard way
      OldWidth := FTextWidth;
      FTextWidth := FTextWidthFunc(FString);
      Exclude(FFlags, sfTextWidthUnknown);
      if (FMaxWidth = OldWidth) and (OldWidth > FTextWidth) then
        FMaxWidth := -1
      else if FTextWidth > FMaxWidth then
        FMaxWidth := FTextWidth
    end
    else
      Include(FFlags, sfTextWidthUnknown);
  end;
  if Assigned(FOnPut) then
    FOnPut(Self, Index, OldLine);
  Changed;
end;

procedure TSynEditStringList.PutObject(Index: Integer; AObject: TObject);
begin
  PutObjectNative(ToSynNativeInt(Index), AObject);
end;

procedure TSynEditStringList.PutObjectNative(AIndex: TSynNativeInt; const AValue: TObject);
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    ListIndexOutOfBounds(AIndex);
  Changing;
  FList[AIndex].FObject := AValue;
  Changed;
end;

procedure TSynEditStringList.PutRange(Index: TSynNativeInt; ARange: TSynEditRange);
begin
  if (Index < 0) or (Index >= FCount) then
    ListIndexOutOfBounds(Index);
  FList[Index].FRange := ARange;
end;

procedure TSynEditStringList.SetCapacity(AValue: Integer);
begin
  CapacityNative := ToSynNativeInt(AValue);
end;

procedure TSynEditStringList.SetChangeFlags(Index: TSynNativeInt;
  const Value: TSynLineChangeFlags);
begin
  if (Index >= 0) and (Index < FCount) then
    FList[Index].FFlags :=  FList[Index].FFlags - [sfModified..sfAsSaved]
     + Value;
end;

procedure TSynEditStringList.SetEncoding(const Value: TEncoding);
begin
  inherited;
end;

procedure TSynEditStringList.SetFileFormat(const Value: TSynEditFileFormat);
begin
  case Value of
    sffDos: LineBreak := WideCRLF;
    sffUnix: LineBreak := WideLF;
    sffMac: LineBreak := WideCR;
    sffUnicode: LineBreak := WideLineSeparator;
  end;
end;

procedure TSynEditStringList.SetTabWidth(Value: Integer);
begin
  if Value <> FTabWidth then
  begin
    FTabWidth := Value;
    ResetMaxWidth;
  end;
end;

procedure TSynEditStringList.SetTextAndFileFormat(const Value: string);
var
  S: string;
  Size: Integer;
  P, Start, Pmax: PChar;
  fCR, fLF, fUnicodeSeparator: Boolean;
begin
  fUnicodeSeparator := False;
  fCR := False;
  fLF := False;
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
    begin
      Size := Length(Value);
      Pmax := P + Length(Value);
      while (P < Pmax) do
      begin
        Start := P;
        while (P < Pmax) and not (Word(P^) in [10, 13]) and (P^ <> WideLineSeparator)
        do
          Inc(P);
        if P <> Start then
        begin
          SetString(S, Start, P - Start);
          InsertItem(FCount, S);
        end
        else
          InsertItem(FCount, '');
        if P^ = #0 then
          Inc(P);
        if P^ = WideLineSeparator then
        begin
          fUnicodeSeparator := True;
          Inc(P);
        end;
        if P^ = WideCR then
        begin
          fCR := True;
          Inc(P);
        end;
        if P^ = WideLF then
        begin
          fLF := True;
          Inc(P);
        end;
      end;
      // keep the old format of the file
      if not TrailingLineBreak and (CharInSet(Value[Size], [#10, #13]) or
        (Value[Size] = WideLineSeparator)) then
        InsertItem(FCount, '');
    end;
    if Assigned(OnInserted) and (FCount > 0) then
      OnInserted(Self, 0, FCount);
  finally
    EndUpdate;
  end;
  if fUnicodeSeparator then
    LineBreak := WideLineSeparator
  else if fCR and not fLF then
    LineBreak := WideCR
  else if fLF and not fCR then
    LineBreak := WideLF
  else
    LineBreak := WideCRLF;
end;

procedure TSynEditStringList.SetTextStr(const Value: string);
begin
  SetTextAndFileFormat(Value);
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

procedure TSynEditStringList.ResetMaxWidth;
var
  I: TSynNativeInt;
begin
  FMaxWidth := -1;
  for I := 0 to FCount - 1 do
    with FList[I] do
    begin
      FTextWidth := -1;
      Include(FFlags, sfTextWidthUnknown);
    end;
end;

procedure TSynEditStringList.SetCapacityNative(const AValue: TSynNativeInt);
begin
  if AValue < CountNative then
    EListError.Create(SInvalidCapacity);
  SetLength(FList, AValue);
  FCapacity := AValue;
end;

end.
