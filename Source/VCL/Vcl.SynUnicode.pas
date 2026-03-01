{-------------------------------------------------------------------------------
VCL Unicode facade for SynEdit.

Re-exports the encoding types, constants, and helper functions from
SynUnicodeShared and adds VCL-specific clipboard routines (Vcl.Clipbrd).
This lets VCL consumer code 'uses Vcl.SynUnicode' as a single import
without needing to know about the shared/VCL split.

The shared unit was renamed to SynUnicodeShared because Delphi's scope
resolution makes a bare 'SynUnicode' shadow this 'Vcl.SynUnicode' unit
(exact name match wins), which broke the intended layering.
-------------------------------------------------------------------------------}

unit Vcl.SynUnicode;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  SynUnicodeShared;

{ Re-exported constants from SynUnicodeShared }
const
  WideNull      = SynUnicodeShared.WideNull;
  WideTab       = SynUnicodeShared.WideTab;
  WideSpace     = SynUnicodeShared.WideSpace;
  WideLF        = SynUnicodeShared.WideLF;
  WideLineFeed  = SynUnicodeShared.WideLineFeed;
  WideVerticalTab     = SynUnicodeShared.WideVerticalTab;
  WideFormFeed        = SynUnicodeShared.WideFormFeed;
  WideCR              = SynUnicodeShared.WideCR;
  WideCarriageReturn  = SynUnicodeShared.WideCarriageReturn;
  WideCRLF            = SynUnicodeShared.WideCRLF;
  WideLineSeparator     = SynUnicodeShared.WideLineSeparator;
  WideParagraphSeparator = SynUnicodeShared.WideParagraphSeparator;

{ Re-exported types from SynUnicodeShared }
type
  TSynEncoding = SynUnicodeShared.TSynEncoding;
  TSynEncodings = SynUnicodeShared.TSynEncodings;

{ Re-exported functions from SynUnicodeShared }
procedure StrSwapByteOrder(Str: PWideChar); inline;
function IsAnsiOnly(const WS: string): Boolean; inline;
function IsUTF8(Stream: TStream; out WithBOM: Boolean; BytesToCheck: Integer = $4000): Boolean; overload; inline;
function IsUTF8(const FileName: string; out WithBOM: Boolean; BytesToCheck: Integer = $4000): Boolean; overload; inline;
function IsUTF8(const Bytes: TBytes; Start: Integer = 0; BytesToCheck: Integer = $4000): Boolean; overload; inline;
function GetEncoding(const FileName: string; out WithBOM: Boolean): TEncoding; overload; inline;
function GetEncoding(Stream: TStream; out WithBOM: Boolean): TEncoding; overload; inline;
function IsWideCharMappableToAnsi(const WC: WideChar): Boolean; inline;

{ VCL clipboard functions }
function ClipboardProvidesText: Boolean;
function GetClipboardText: string;
procedure SetClipboardText(const Text: string);

implementation

uses
  Winapi.Windows,
  Vcl.Clipbrd;

procedure StrSwapByteOrder(Str: PWideChar);
begin
  SynUnicodeShared.StrSwapByteOrder(Str);
end;

function IsAnsiOnly(const WS: string): Boolean;
begin
  Result := SynUnicodeShared.IsAnsiOnly(WS);
end;

function IsUTF8(Stream: TStream; out WithBOM: Boolean; BytesToCheck: Integer): Boolean;
begin
  Result := SynUnicodeShared.IsUTF8(Stream, WithBOM, BytesToCheck);
end;

function IsUTF8(const FileName: string; out WithBOM: Boolean; BytesToCheck: Integer): Boolean;
begin
  Result := SynUnicodeShared.IsUTF8(FileName, WithBOM, BytesToCheck);
end;

function IsUTF8(const Bytes: TBytes; Start: Integer; BytesToCheck: Integer): Boolean;
begin
  Result := SynUnicodeShared.IsUTF8(Bytes, Start, BytesToCheck);
end;

function GetEncoding(const FileName: string; out WithBOM: Boolean): TEncoding;
begin
  Result := SynUnicodeShared.GetEncoding(FileName, WithBOM);
end;

function GetEncoding(Stream: TStream; out WithBOM: Boolean): TEncoding;
begin
  Result := SynUnicodeShared.GetEncoding(Stream, WithBOM);
end;

function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
begin
  Result := SynUnicodeShared.IsWideCharMappableToAnsi(WC);
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

end.
