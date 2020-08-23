{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynUnicode.pas by Maël Hörz, released 2004-05-30.
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

Provides:
- Unicode(PWideChar) versions of the most important PAnsiChar-functions in
  SysUtils and some functions unavailable in Delphi 5.
- function for loading and saving of Unicode files, and detecting the encoding
- Unicode clipboard support
- Unicode-version of TCanvas-methods
- Some character constants like CR&LF.

------------------------------------------------------------------------------}

unit SynUnicode;

{$I SynEdit.inc}

interface

uses
  Windows,
  Messages,
  Controls,
  Forms,
  Graphics,
  Clipbrd,
  Types,
  Classes,
  SysUtils,
  TypInfo;

const
  SLineBreak = #13#10;
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  UTF16BOMLE: array[0..1] of Byte = ($FF, $FE);
  UTF16BOMBE: array[0..1] of Byte = ($FE, $FF);
  UTF32BOMLE: array[0..3] of Byte = ($FF, $FE, $00, $00);
  UTF32BOMBE: array[0..3] of Byte = ($00, $00, $FE, $FF);

const
  // constants describing range of the Unicode Private Use Area (Unicode 3.2)
  PrivateUseLow = WideChar($E000);
  PrivateUseHigh = WideChar($F8FF);
  // filler char: helper for painting wide glyphs
  FillerChar = PrivateUseLow;

const
  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar(#10);
  WideLineFeed = WideChar(#10);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideCR = WideChar(#13);
  WideCarriageReturn = WideChar(#13);
  WideCRLF = string(#13#10);
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

  // byte order marks for Unicode files
  // Unicode text files (in UTF-16 format) should contain $FFFE as first character to
  // identify such a file clearly. Depending on the system where the file was created
  // on this appears either in big endian or little endian style.
  BOM_LSB_FIRST = WideChar($FEFF);
  BOM_MSB_FIRST = WideChar($FFFE);

type
  TFontCharSet = 0..255;

function SynCharNext(P: PWideChar): PWideChar; overload;
function SynCharNext(P: PWideChar; out Element: String): PWideChar; overload;
function SynUniElementsCount(S: string) : integer;

{ functions taken from JCLUnicode.pas }
procedure StrSwapByteOrder(Str: PWideChar);
function CharSetFromLocale(Language: LCID): TFontCharSet;
function CodePageFromLocale(Language: LCID): Integer;
function KeyboardCodePage: Word;

{ functions providing same behavior on Win9x and WinNT based systems}
function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;

{ Unicode streaming-support }
type
  TSynEncoding = (seUTF8, seUTF16LE, seUTF16BE, seAnsi);
  TSynEncodings = set of TSynEncoding;

function IsAnsiOnly(const WS: string): Boolean;
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean; overload;
function IsUTF8(const FileName: string; out WithBOM: Boolean): Boolean; overload;
function GetEncoding(const FileName: string; out WithBOM: Boolean): TSynEncoding; overload;
function GetEncoding(Stream: TStream; out WithBOM: Boolean): TSynEncoding; overload;
procedure SaveToFile(const WS: string; const FileName: string;
  Encoding: TSynEncoding; WithBom: Boolean = True); overload;
procedure SaveToFile(UnicodeStrings: TStrings; const FileName: string;
  Encoding: TSynEncoding; WithBom: Boolean = True); overload;
function LoadFromFile(UnicodeStrings: TStrings; const FileName: string;
  out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromFile(UnicodeStrings: TStrings; const FileName: string;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding; overload;
procedure SaveToStream(const WS: string; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean  = True); overload;
procedure SaveToStream(UnicodeStrings: TStrings; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean  = True); overload;
function LoadFromStream(UnicodeStrings: TStrings; Stream: TStream;
  out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromStream(UnicodeStrings: TStrings; Stream: TStream;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding; overload;
function LoadFromStream(UnicodeStrings: TStrings; Stream: TStream;
  Encoding: TSynEncoding): TSynEncoding; overload;

function ClipboardProvidesText: Boolean;
function GetClipboardText: string;
procedure SetClipboardText(const Text: string);

{ misc functions }
function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
function IsUnicodeStringMappableToAnsi(const WS: string): Boolean;

implementation

uses
  SynEditTextBuffer,
  Math,
  SysConst,
  RTLConsts;


function SynCharNext(P: PWideChar): PWideChar;
begin
  Result := Windows.CharNext(P);
end;

function SynCharNext(P: PWideChar; out Element: String): PWideChar; overload;
Var
  Start : PWideChar;
begin
  Start := P;
  Result := Windows.CharNext(P);
  SetString(Element, Start, Result - Start);
end;

function SynUniElementsCount(S: string) : integer;
Var
  P : PWideChar;
begin
  Result := 0;
  P := PWideChar(S);
  while P^ <> #0 do
  begin
    P := Windows.CharNext(P);
    Inc(Result);
  end;
end;

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string
procedure StrSwapByteOrder(Str: PWideChar);
var
  P: PWord;
begin
  P := PWord(Str);
  while P^ <> 0 do
  begin
    P^ := MakeWord(HiByte(P^), LoByte(P^));
    Inc(P);
  end;
end;

function TranslateCharsetInfoEx(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall;
  external 'gdi32.dll' name 'TranslateCharsetInfo';

function CharSetFromLocale(Language: LCID): TFontCharSet;
var
  CP: Cardinal;
  CSI: TCharsetInfo;
begin
  CP:= CodePageFromLocale(Language);
  TranslateCharsetInfoEx(Pointer(CP), CSI, TCI_SRCCODEPAGE);
  Result:= CSI.ciCharset;
end;

// determines the code page for a given locale
function CodePageFromLocale(Language: LCID): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;

  begin
    GetTextExtentPoint32W(DC, Str, Count, Result);
  end;
end;

function IsAnsiOnly(const WS: string): Boolean;
begin
  Result := IsUnicodeStringMappableToAnsi(WS);
end;

function IsUTF8(const FileName: string; out WithBOM: Boolean): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsUTF8(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

// checks for a BOM in UTF-8 format or searches the first 4096 bytes for
// typical UTF-8 octet sequences
function IsUTF8(Stream: TStream; out WithBOM: Boolean): Boolean;
const
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = $4000;
var
  Buffer: array of Byte;
  BufferSize, i, FoundUTF8Strings: Integer;

  // 3 trailing bytes are the maximum in valid UTF-8 streams,
  // so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    inc(i);
    while (i < BufferSize) and (Result < 4) do
    begin
      if Buffer[i] in [$80..$BF] then
        inc(Result)
      else
        Break;
      inc(i);
    end;
  end;

begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result

  // start analysis at actual Stream.Position
  BufferSize := Min(MaxBufferSize, Stream.Size - Stream.Position);

  // if no special characteristics are found it is not UTF-8
  Result := False;
  WithBOM := False;

  if BufferSize > 0 then
  begin
    SetLength(Buffer, BufferSize);
    Stream.ReadBuffer(Buffer[0], BufferSize);
    Stream.Seek(-BufferSize, soFromCurrent);

    { first search for BOM }

    if (BufferSize >= Length(UTF8BOM)) and CompareMem(@Buffer[0], @UTF8BOM[0], Length(UTF8BOM)) then
    begin
      WithBOM := True;
      Result := True;
      Exit;
    end;

    { If no BOM was found, check for leading/trailing byte sequences,
      which are uncommon in usual non UTF-8 encoded text.

      NOTE: There is no 100% save way to detect UTF-8 streams. The bigger
            MinimumCountOfUTF8Strings, the lower is the probability of
            a false positive. On the other hand, a big MinimumCountOfUTF8Strings
            makes it unlikely to detect files with only little usage of non
            US-ASCII chars, like usual in European languages. }

    FoundUTF8Strings := 0;
    i := 0;
    while i < BufferSize do
    begin
      case Buffer[i] of
        $00..$7F: // skip US-ASCII characters as they could belong to various charsets
          ;
        $C2..$DF:
          if CountOfTrailingBytes = 1 then
            inc(FoundUTF8Strings)
          else
            Break;
        $E0:
          begin
            inc(i);
            if (i < BufferSize) and (Buffer[i] in [$A0..$BF]) and (CountOfTrailingBytes = 1) then
              inc(FoundUTF8Strings)
            else
              Break;
          end;
        $E1..$EC, $EE..$EF:
          if CountOfTrailingBytes = 2 then
            inc(FoundUTF8Strings)
          else
            Break;
        $ED:
          begin
            inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80..$9F]) and (CountOfTrailingBytes = 1) then
              inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F0:
          begin
            inc(i);
            if (i < BufferSize) and (Buffer[i] in [$90..$BF]) and (CountOfTrailingBytes = 2) then
              inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F1..$F3:
          if CountOfTrailingBytes = 3 then
            inc(FoundUTF8Strings)
          else
            Break;
        $F4:
          begin
            inc(i);
            if (i < BufferSize) and (Buffer[i] in [$80..$8F]) and (CountOfTrailingBytes = 2) then
              inc(FoundUTF8Strings)
            else
              Break;
          end;
        $C0, $C1, $F5..$FF: // invalid UTF-8 bytes
          Break;
        $80..$BF: // trailing bytes are consumed when handling leading bytes,
                   // any occurence of "orphaned" trailing bytes is invalid UTF-8
          Break;
      end;

      if FoundUTF8Strings = MinimumCountOfUTF8Strings then
      begin
        Result := True;
        Break;
      end;

      inc(i);
    end;
  end;
end;

function GetEncoding(const FileName: string; out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetEncoding(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

function GetEncoding(Stream: TStream; out WithBOM: Boolean): TSynEncoding;
var
  BOM: WideChar;
  Size: Integer;
begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result

  // start analysis at actual Stream.Position
  Size := Stream.Size - Stream.Position;

  // if no special characteristics are found it is probably ANSI
  Result := seAnsi;

  if IsUTF8(Stream, WithBOM) then
  begin
    Result := seUTF8;
    Exit;
  end;

  { try to detect UTF-16 by finding a BOM in UTF-16 format }

  if Size >= 2 then
  begin
    Stream.ReadBuffer(BOM, sizeof(BOM));
    Stream.Seek(-sizeof(BOM), soFromCurrent);
    if BOM = WideChar(UTF16BOMLE) then
    begin
      Result := seUTF16LE;
      WithBOM := True;
      Exit;
    end
    else if BOM = WideChar(UTF16BOMBE) then
    begin
      Result := seUTF16BE;
      WithBOM := True;
      Exit;
    end
  end;
end;

procedure SaveToFile(const WS: string; const FileName: string;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(WS, Stream, Encoding, WithBom);
  finally
    Stream.Free;
  end;
end;

procedure SaveToFile(UnicodeStrings: TStrings; const FileName: string;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(UnicodeStrings, Stream, Encoding, WithBom);
  finally
    Stream.Free;
  end;
end;

function LoadFromFile(UnicodeStrings: TStrings; const FileName: string;
  out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(UnicodeStrings, Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

function LoadFromFile(UnicodeStrings: TStrings; const FileName: string;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(UnicodeStrings, Stream, Encoding, WithBOM);
  finally
    Stream.Free;
  end;
end;

procedure SaveToStream(const WS: string; Stream: TStream; Encoding: TSynEncoding;
  WithBom: Boolean  = True);
var
  UTF16BOM: string;

  UTF8Str: UTF8String;
  AnsiStr: AnsiString;
begin
  if WithBom then
    case Encoding of
      seUTF8:
        Stream.WriteBuffer(UTF8BOM, 3);
      seUTF16LE:
        begin
          UTF16BOM := BOM_LSB_FIRST;
          Stream.WriteBuffer(PWideChar(UTF16BOM)^, 2);
        end;
      seUTF16BE:
        begin
          UTF16BOM := BOM_MSB_FIRST;
          Stream.WriteBuffer(PWideChar(UTF16BOM)^, 2);
        end;
    end;

  case Encoding of
    seUTF8:
      begin
        UTF8Str := UTF8Encode(WS);
        Stream.WriteBuffer(UTF8Str[1], Length(UTF8Str));
      end;
    seUTF16LE:
      Stream.WriteBuffer(WS[1], Length(WS) * sizeof(WideChar));
    seUTF16BE:
      begin
        StrSwapByteOrder(PWideChar(WS));
        Stream.WriteBuffer(WS[1], Length(WS) * sizeof(WideChar));
      end;
    seAnsi:
      begin
        AnsiStr := AnsiString(PWideChar(WS));
        Stream.WriteBuffer(AnsiStr[1], Length(AnsiStr));
      end;
  end;
end;

type
  TSynEditStringListAccess = class(TSynEditStringList);

procedure SaveToStream(UnicodeStrings: TStrings; Stream: TStream;
  Encoding: TSynEncoding; WithBom: Boolean = True);
var
  SText: string;
  SaveFStreaming: Boolean;
begin
  // if UnicodeStrings or Stream is nil, let Delphi raise the exception to flag the error

  if UnicodeStrings is TSynEditStringList then
  begin
    SaveFStreaming := TSynEditStringListAccess(UnicodeStrings).FStreaming;
    TSynEditStringListAccess(UnicodeStrings).FStreaming := True;
    SText := UnicodeStrings.Text;
    TSynEditStringListAccess(UnicodeStrings).FStreaming := SaveFStreaming;
  end
  else
    SText := UnicodeStrings.Text;
  SaveToStream(SText, Stream, Encoding, WithBom);
end;

function LoadFromStream(UnicodeStrings: TStrings; Stream: TStream;
  out WithBOM: Boolean): TSynEncoding;
var
  Dummy: Boolean;
begin
  Result := LoadFromStream(UnicodeStrings, Stream, GetEncoding(Stream, WithBOM),
    Dummy);
end;

function LoadFromStream(UnicodeStrings: TStrings; Stream: TStream;
  Encoding: TSynEncoding): TSynEncoding; overload;
var
  Dummy: Boolean;
begin
  Result := LoadFromStream(UnicodeStrings, Stream, Encoding, Dummy);
end;

function LoadFromStream(UnicodeStrings: TStrings; Stream: TStream;
  Encoding: TSynEncoding; out WithBOM: Boolean): TSynEncoding;
var
  WideStr: string;
  UTF8Str: UTF8String;
  AnsiStr: AnsiString;
  Size: Integer;

  function SkipBOM: Boolean;
  var
    BOM: array of Byte;
  begin
    Result := False;
    case Encoding of
      seUTF8:
        begin
          SetLength(BOM, Min(Length(UTF8BOM), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF8BOM)) or
            not CompareMem(@BOM[0], @UTF8BOM[0], Length(UTF8BOM))
          then
            Stream.Seek(-Length(BOM), soCurrent)
          else
            Result := True;
        end;
      seUTF16LE:
        begin
          SetLength(BOM, Min(Length(UTF16BOMLE), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF16BOMLE)) or
            not CompareMem(@BOM[0], @UTF16BOMLE[0], Length(UTF16BOMLE))
          then
            Stream.Seek(-Length(BOM), soCurrent)
          else
            Result := True;
        end;
      seUTF16BE:
        begin
          SetLength(BOM, Min(Length(UTF16BOMBE), Size));
          Stream.ReadBuffer(BOM[0], Length(BOM));
          if (Length(BOM) <> Length(UTF16BOMBE)) or
            not CompareMem(@BOM[0], @UTF16BOMBE[0], Length(UTF16BOMBE))
          then
            Stream.Seek(-Length(BOM), soCurrent)
          else
            Result := True;
        end;
    end;
    Size := Stream.Size - Stream.Position;
  end;

begin
  // if UnicodeStrings or Stream is nil, let Delphi raise the exception to
  // signal an invalid result
  UnicodeStrings.BeginUpdate;
  try
    Result := Encoding;
    // start decoding at actual Stream.Position
    Size := Stream.Size - Stream.Position;

    // skip BOM, if it exists
    WithBOM := SkipBOM;

    case Result of
      seUTF8:
        begin
          SetLength(UTF8Str, Size);
          Stream.ReadBuffer(UTF8Str[1], Size);
          UnicodeStrings.Text := UTF8ToUnicodeString(UTF8Str);
        end;
      seUTF16LE:
        begin
          SetLength(WideStr, Size div 2);
          Stream.ReadBuffer(WideStr[1], Size);
          UnicodeStrings.Text := WideStr;
        end;
      seUTF16BE:
        begin
          SetLength(WideStr, Size div 2);
          Stream.ReadBuffer(WideStr[1], Size);
          StrSwapByteOrder(PWideChar(WideStr));
          UnicodeStrings.Text := WideStr;
        end;
      seAnsi:
        begin
          SetLength(AnsiStr, Size);
          Stream.ReadBuffer(AnsiStr[1], Size);
          UnicodeStrings.Text := string(AnsiStr);
        end;
    end;
  finally
    UnicodeStrings.EndUpdate
  end
end;

function ClipboardProvidesText: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_UNICODETEXT);
end;

function GetClipboardText: string;
begin
  Result := Clipboard.AsText;
end;

procedure SetClipboardText(const Text: string);
begin
  Clipboard.AsText := Text;
end;

function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(@WC), 1, nil, 0, nil,
    @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function IsUnicodeStringMappableToAnsi(const WS: string): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(WS), Length(WS), nil, 0,
    nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

initialization
  Assert(Win32Platform = VER_PLATFORM_WIN32_NT, 'Unsupported Windows version');

end.
