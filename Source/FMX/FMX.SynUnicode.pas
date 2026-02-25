{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynUnicode;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  FMX.Platform,
  SynEditTypes;

function ClipboardProvidesText: Boolean;
function GetClipboardText: string;
procedure SetClipboardText(const Text: string);

implementation

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
