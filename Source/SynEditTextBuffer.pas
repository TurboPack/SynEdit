{ -------------------------------------------------------------------------------
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

  Known Issues:
  ------------------------------------------------------------------------------- }
// todo: Avoid calculating expanded string unncessarily (just calculate expandedLength instead).

unit SynEditTextBuffer;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  SynEditTypes,
  SynEditMiscProcs,
  SynUnicode;

type
  TSynEditRange = Pointer;

  TSynEditStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TSynEditStringFlags = set of TSynEditStringFlag;

  PSynEditStringRec = ^TSynEditStringRec;

  TSynEditStringRec = record
    FString: string;
    FObject: TObject;
    FRange: TSynEditRange;
    FExpandedLength: Integer;
    FCharIndex: Integer;
    FFlags: TSynEditStringFlags;
  end;

  TSynEditTwoWideChars = record
    One, Two: WideChar;
  end;

  PSynEditTwoWideChars = ^TSynEditTwoWideChars;

const
  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  MaxSynEditStrings = MaxInt div SynEditStringRecSize;

  NullRange = TSynEditRange(-1);

type
  PSynEditStringRecList = ^TSynEditStringRecList;
  TSynEditStringRecList = array [0 .. MaxSynEditStrings - 1]
    of TSynEditStringRec;

  TStringListChangeEvent = procedure(Sender: TObject; Index: Integer;
    Count: Integer) of object;
  TStringListPutEvent = procedure(Sender: TObject; Index: Integer;
    const OldLine: string) of object;

  TExpandAtWideGlyphsFunc = function(const S: string): string of object;

  TSynEditStringList = class(TStrings)
  private
    FList: PSynEditStringRecList;
    FCount: Integer;
    FCapacity: Integer;
    FFileFormat: TSynEditFileFormat;
    FConvertTabsProc: TConvertTabsProcEx;
    FIndexOfLongestLine: Integer;
    FTabWidth: Integer;
    FExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc;
    FCharIndexesAreValid: Boolean;
    FDetectUTF8: Boolean;
    FUTF8CheckLen: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOnCleared: TNotifyEvent;
    FOnDeleted: TStringListChangeEvent;
    FOnInserted: TStringListChangeEvent;
    FOnPut: TStringListPutEvent;
    FOnInfoLoss: TSynInfoLossEvent;
    function ExpandString(Index: Integer): string;
    function GetExpandedString(Index: Integer): string;
    function GetExpandedStringLength(Index: Integer): Integer;
    function GetLengthOfLongestLine: Integer;
    function GetRange(Index: Integer): TSynEditRange;
    procedure Grow;
    procedure InsertItem(Index: Integer; const S: string);
    procedure PutRange(Index: Integer; ARange: TSynEditRange);
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetTabWidth(Value: Integer);
    procedure SetUpdateState(Updating: Boolean); override;
    procedure UpdateCharIndexes;
  public
    constructor Create(AExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc);
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure DeleteLines(Index, NumLines: Integer);
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertLines(Index, NumLines: Integer);
    procedure InsertStrings(Index: Integer; Strings: TArray<string>;
      FromIndex: Integer = 0); overload;
    procedure InsertStrings(Index: Integer; Strings: TStrings); overload;
    procedure InsertText(Index: Integer; NewText: string);
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); override;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); override;
    function GetSeparatedText(Separators: string): string;
    procedure FontChanged;
    function LineCharLength(Index: Integer): Integer;
    function LineCharIndex(Index: Integer): Integer;
    procedure SetTextAndFileFormat(const Value: string);
    procedure SetEncoding(const Value: TEncoding); override;

    property FileFormat: TSynEditFileFormat read FFileFormat write FFileFormat;
    property ExpandedStrings[Index: Integer]: string read GetExpandedString;
    property ExpandedStringLengths[Index: Integer]: Integer
      read GetExpandedStringLength;
    property LengthOfLongestLine: Integer read GetLengthOfLongestLine;
    property Ranges[Index: Integer]: TSynEditRange read GetRange write PutRange;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property UTF8CheckLen: Integer read FUTF8CheckLen write FUTF8CheckLen;
    property DetectUTF8: Boolean read FDetectUTF8 write FDetectUTF8;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleted: TStringListChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TStringListChangeEvent read FOnInserted
      write FOnInserted;
    property OnPut: TStringListPutEvent read FOnPut write FOnPut;
    property OnInfoLoss: TSynInfoLossEvent read FOnInfoLoss write FOnInfoLoss;
  end;

  ESynEditStringList = class(Exception);

implementation

resourcestring
  SListIndexOutOfBounds = 'Invalid stringlist index %d';
  SInvalidCapacity = 'Stringlist capacity cannot be smaller than count';

  { TSynEditStringList }

procedure ListIndexOutOfBounds(Index: Integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create(AExpandAtWideGlyphsFunc
  : TExpandAtWideGlyphsFunc);
begin
  inherited Create;
  FExpandAtWideGlyphsFunc := AExpandAtWideGlyphsFunc;
  FFileFormat := sffDos;
  FIndexOfLongestLine := -1;
  TabWidth := 8;
  FUTF8CheckLen := -1;
  Options := Options - [soWriteBOM, soTrailingLineBreak];
  FDetectUTF8 := True;
end;

destructor TSynEditStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then
    Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TSynEditStringList.Add(const S: string): Integer;
begin
  BeginUpdate;
  Result := FCount;
  InsertItem(Result, S);
  if Assigned(OnInserted) then
    OnInserted(Self, Result, 1);
  EndUpdate;
end;

procedure TSynEditStringList.AddStrings(Strings: TStrings);
begin
  InsertStrings(FCount, Strings);
end;

procedure TSynEditStringList.SetEncoding(const Value: TEncoding);
begin
  inherited;
end;

procedure TSynEditStringList.Clear;
begin
  if FCount <> 0 then
  begin
    BeginUpdate;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    if Assigned(FOnCleared) then
      FOnCleared(Self);
    EndUpdate;
  end;
  FIndexOfLongestLine := -1;
end;

procedure TSynEditStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SynEditStringRecSize);
  end;
  FIndexOfLongestLine := -1;
  if Assigned(FOnDeleted) then
    FOnDeleted(Self, Index, 1);
  EndUpdate;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: Integer;
begin
  if NumLines > 0 then
  begin
    if (Index < 0) or (Index >= FCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := FCount - (Index + NumLines);
    if LinesAfter < 0 then
      NumLines := FCount - Index;
    Finalize(FList^[Index], NumLines);

    BeginUpdate;
    try
      if LinesAfter > 0 then
        System.Move(FList^[Index + NumLines], FList^[Index],
          LinesAfter * SynEditStringRecSize);
      Dec(FCount, NumLines);
      if Assigned(FOnDeleted) then
        FOnDeleted(Self, Index, NumLines);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.Exchange(Index1, Index2: Integer);
var
  Temp: TSynEditStringRec;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    ListIndexOutOfBounds(Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    ListIndexOutOfBounds(Index2);
  BeginUpdate;
  Temp := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Temp;
  if FIndexOfLongestLine = Index1 then
    FIndexOfLongestLine := Index2
  else if FIndexOfLongestLine = Index2 then
    FIndexOfLongestLine := Index1;
  EndUpdate;
end;

function TSynEditStringList.ExpandString(Index: Integer): string;
var
  HasTabs: Boolean;
begin
  with FList^[Index] do
    if Length(FString) = 0 then
    begin
      Result := '';
      Exclude(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Include(FFlags, sfHasNoTabs);
      FExpandedLength := 0;
    end
    else
    begin
      Result := FConvertTabsProc(FString, FTabWidth, HasTabs);
      FExpandedLength := Length(FExpandAtWideGlyphsFunc(Result));
      Exclude(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Exclude(FFlags, sfHasNoTabs);
      if HasTabs then
        Include(FFlags, sfHasTabs)
      else
        Include(FFlags, sfHasNoTabs);
    end;
end;

function TSynEditStringList.Get(Index: Integer): string;
begin
  if Cardinal(Index) < Cardinal(FCount) then
    Result := FList^[Index].FString
  else
    Result := '';
end;

procedure TSynEditStringList.UpdateCharIndexes;
var
  I, N: Integer;
  P: PSynEditStringRec;
begin
  FCharIndexesAreValid := True;
  if FCount = 0 then
    Exit;
  P := @FList^[0];
  N := 0;
  for I := 1 to FCount do
  begin
    P.FCharIndex := N;
    Inc(N, Length(P.FString));
    Inc(P);
  end;
end;

function TSynEditStringList.LineCharLength(Index: Integer): Integer;
begin
  if Cardinal(Index) < Cardinal(FCount) then
    Result := Length(FList^[Index].FString)
  else
    Result := 0;
end;

function TSynEditStringList.LineCharIndex(Index: Integer): Integer;
begin
  if Cardinal(Index) < Cardinal(FCount) then
  begin
    if not FCharIndexesAreValid then
      UpdateCharIndexes;
    Result := FList^[Index].FCharIndex;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TSynEditStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TSynEditStringList.GetExpandedString(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    if sfHasNoTabs in FList^[Index].FFlags then
      Result := Get(Index)
    else
      Result := ExpandString(Index);
  end
  else
    Result := '';
end;

function TSynEditStringList.GetExpandedStringLength(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    if sfExpandedLengthUnknown in FList^[Index].FFlags then
      Result := Length(ExpandedStrings[index])
    else
      Result := FList^[Index].FExpandedLength;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetLengthOfLongestLine: Integer;
var
  I, MaxLen: Integer;
  PRec: PSynEditStringRec;
begin
  if FIndexOfLongestLine < 0 then
  begin
    MaxLen := 0;
    if FCount > 0 then
    begin
      PRec := @FList^[0];
      for I := 0 to FCount - 1 do
      begin
        if sfExpandedLengthUnknown in PRec^.FFlags then
          ExpandString(I);
        if PRec^.FExpandedLength > MaxLen then
        begin
          MaxLen := PRec^.FExpandedLength;
          FIndexOfLongestLine := I;
        end;
        Inc(PRec);
      end;
    end;
  end;
  if (FIndexOfLongestLine >= 0) and (FIndexOfLongestLine < FCount) then
    Result := FList^[FIndexOfLongestLine].FExpandedLength
  else
    Result := 0;
end;

function TSynEditStringList.GetObject(Index: Integer): TObject;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList^[Index].FObject
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: Integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList^[Index].FRange
  else
    Result := nil;
end;

function TSynEditStringList.GetSeparatedText(Separators: string): string;
{ Optimized by Eric Grange }
var
  I, L, Size, LineBreakSize: Integer;
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
    Length(FList^[FCount - 1].FString);
  SetLength(Result, Size);

  P := Pointer(Result);
  PRec := @FList^[0];

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

procedure TSynEditStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    Delta := 16;
  SetCapacity(FCapacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: Integer; const S: string);
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(FOnInserted) then
    FOnInserted(Self, Index, 1);
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: Integer; const S: string);
begin
  BeginUpdate;
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
  begin
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SynEditStringRecSize);
  end;
  FIndexOfLongestLine := -1;
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FString := S;
    FObject := nil;
    FRange := NullRange;
    FExpandedLength := -1;
    FFlags := [sfExpandedLengthUnknown];
  end;
  Inc(FCount);
  EndUpdate;
end;

procedure TSynEditStringList.InsertLines(Index, NumLines: Integer);
var
  c_Line: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);
  if NumLines > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(FCount + NumLines);
      if Index < FCount then
      begin
        System.Move(FList^[Index], FList^[Index + NumLines],
          (FCount - Index) * SynEditStringRecSize);
      end;
      for c_Line := Index to Index + NumLines - 1 do
        with FList^[c_Line] do
        begin
          Pointer(FString) := nil;
          FObject := nil;
          FRange := NullRange;
          FExpandedLength := -1;
          FFlags := [sfExpandedLengthUnknown];
        end;
      Inc(FCount, NumLines);
      if Assigned(OnInserted) then
        OnInserted(Self, Index, NumLines);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: Integer;
  Strings: TArray<string>; FromIndex: Integer);
var
  I: Integer;
  LineCount: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);

  LineCount := Length(Strings) - FromIndex;
  if LineCount > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(FCount + LineCount);
      if Index < FCount then
      begin
        System.Move(FList^[Index], FList^[Index + LineCount],
          (FCount - Index) * SynEditStringRecSize);
      end;
      for I := 0 to LineCount - 1 do
        with FList^[Index + I] do
        begin
          Pointer(FString) := nil;
          FString := Strings[FromIndex + I];
          FObject := nil;
          FRange := NullRange;
          FExpandedLength := -1;
          FFlags := [sfExpandedLengthUnknown];
        end;
      Inc(FCount, LineCount);
      if Assigned(OnInserted) then
        OnInserted(Self, Index, LineCount);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: Integer; Strings: TStrings);
begin
  InsertStrings(Index, Strings.ToStringArray);
end;

procedure TSynEditStringList.InsertText(Index: Integer; NewText: string);
var
  TmpStringList: TStringList;
begin
  if NewText = '' then
    Exit;

  TmpStringList := TStringList.Create;
  try
    TmpStringList.Text := NewText;
    InsertStrings(Index, TmpStringList);
  finally
    TmpStringList.Free;
  end;
end;

procedure TSynEditStringList.LoadFromStream(Stream: TStream;
  Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
  DecodedText: string;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer, 0, Size);
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, DefaultEncoding);
    WriteBOM := Size > 0; // Keep WriteBom in case the stream is saved
    // If the encoding is ANSI and DetectUtf8 is True try to Detect UTF8
    if (Encoding = TEncoding.ANSI) and DetectUTF8 and IsUTF8(Buffer, Size) then
      Encoding := TEncoding.UTF8;
    SetEncoding(Encoding); // Keep Encoding in case the stream is saved
    DecodedText := Encoding.GetString(Buffer, Size, Length(Buffer) - Size);
    SetLength(Buffer, 0); // Free the buffer here to reduce memory footprint
    SetTextAndFileFormat(DecodedText);
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.SaveToStream(Stream: TStream; Encoding: TEncoding);
Var
  Cancel: Boolean;
  OldLineBreak: string;
  S: string;
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := DefaultEncoding;

  OldLineBreak := LineBreak;
  try
    LineBreak := LineBreakFromFileFormat(FFileFormat);
    S := GetTextStr;
  finally
    LineBreak := OldLineBreak;
  end;

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
var
  OldLine: string;
begin
  if (Index = 0) and (FCount = 0) or (FCount = Index) then
    Add(S)
  else
  begin
    if Cardinal(Index) >= Cardinal(FCount) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    FIndexOfLongestLine := -1;
    with FList^[Index] do
    begin
      Include(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Exclude(FFlags, sfHasNoTabs);
      OldLine := FString;
      FString := S;
    end;
    if Assigned(FOnPut) then
      FOnPut(Self, Index, OldLine);
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  FList^[Index].FObject := AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: Integer; ARange: TSynEditRange);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    ListIndexOutOfBounds(Index);
  FList^[Index].FRange := ARange;
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < Count then
    EListError.Create(SInvalidCapacity);
  ReallocMem(FList, NewCapacity * SynEditStringRecSize);
  FCapacity := NewCapacity;
end;

procedure TSynEditStringList.SetTabWidth(Value: Integer);
var
  I: Integer;
begin
  if Value <> FTabWidth then
  begin
    FTabWidth := Value;
    FConvertTabsProc := GetBestConvertTabsProcEx(FTabWidth);
    FIndexOfLongestLine := -1;
    for I := 0 to FCount - 1 do
      with FList^[I] do
      begin
        FExpandedLength := -1;
        Exclude(FFlags, sfHasNoTabs);
        Include(FFlags, sfExpandedLengthUnknown);
      end;
  end;
end;

procedure TSynEditStringList.SetTextAndFileFormat(const Value: string);
var
  S: string;
  Size: Integer;
  P, Start, Pmax: PChar;
  fCR, fLF, fLINESEPARATOR: Boolean;
begin
  fLINESEPARATOR := False;
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
      while (P <= Pmax) do
      begin
        Start := P;
        while ((P < Pmax) and (Word(P^) > 13)) or
          not((Word(P^) in [10, 13]) or (P^ = WideLineSeparator)) do
        begin
          Inc(P);
        end;
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
          fLINESEPARATOR := True;
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
  if fLINESEPARATOR then
    FileFormat := sffUnicode
  else if fCR and not fLF then
    FileFormat := sffMac
  else if fLF and not fCR then
    FileFormat := sffUnix
  else
    FileFormat := sffDos;
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  FCharIndexesAreValid := False;
  if Updating then
  begin
    if Assigned(FOnChanging) then
      FOnChanging(Self);
  end
  else
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynEditStringList.FontChanged;
var
  I: Integer;
begin
  FIndexOfLongestLine := -1;
  for I := 0 to FCount - 1 do
    with FList^[I] do
    begin
      FExpandedLength := -1;
      Exclude(FFlags, sfHasNoTabs);
      Include(FFlags, sfExpandedLengthUnknown);
    end;
end;

end.
