{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynUnicode;

{ FMX Unicode facade for SynEdit.

  Re-exports encoding types, constants, and helper functions from
  SynUnicodeShared and adds FMX-specific clipboard routines
  (IFMXClipboardService).  Mirrors the Vcl.SynUnicode API so that
  FMX consumer code can 'uses FMX.SynUnicode' as a single import. }

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  FMX.Platform,
  SynUnicodeShared,
  SynEditTypes;

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
function IsUTF8(Stream: TStream; out WithBOM: Boolean; BytesToCheck: Integer = $4000): Boolean; overload; inline;
function IsUTF8(const FileName: string; out WithBOM: Boolean; BytesToCheck: Integer = $4000): Boolean; overload; inline;
function IsUTF8(const Bytes: TBytes; Start: Integer = 0; BytesToCheck: Integer = $4000): Boolean; overload; inline;
function GetEncoding(const FileName: string; out WithBOM: Boolean): TEncoding; overload; inline;
function GetEncoding(Stream: TStream; out WithBOM: Boolean): TEncoding; overload; inline;

{ FMX clipboard functions }
function ClipboardProvidesText: Boolean;
function GetClipboardText: string;
procedure SetClipboardText(const Text: string);

implementation

procedure StrSwapByteOrder(Str: PWideChar);
begin
  SynUnicodeShared.StrSwapByteOrder(Str);
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

function ClipboardProvidesText: Boolean;
var
  ClipService: IFMXClipboardService;
  Value: TValue;
begin
  Result := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipService) then
  begin
    Value := ClipService.GetClipboard;
    Result := Value.IsType<string>;
  end;
end;

function GetClipboardText: string;
var
  ClipService: IFMXClipboardService;
  Value: TValue;
begin
  Result := '';
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipService) then
  begin
    Value := ClipService.GetClipboard;
    if Value.IsType<string> then
      Result := Value.AsString;
  end;
end;

procedure SetClipboardText(const Text: string);
var
  ClipService: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipService) then
    ClipService.SetClipboard(Text);
end;

end.
