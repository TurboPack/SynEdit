{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
CONTENTS:
  Abstract printer info provider interface and concrete FMX implementation.
  The ISynPrinterInfo interface is platform-independent.
  TSynFMXPrinterInfo provides a concrete implementation using FMX.Printer
  on Windows and a default (A4/300dpi) fallback on other platforms.
-------------------------------------------------------------------------------}

unit FMX.SynEditPrinterInfo;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes;

type
  { Abstract printer info provider - platform-independent interface }
  ISynPrinterInfo = interface
    ['{A1B2C3D4-E5F6-4A7B-8C9D-0E1F2A3B4C5D}']
    function GetPhysicalWidth: Integer;
    function GetPhysicalHeight: Integer;
    function GetPrintableWidth: Integer;
    function GetPrintableHeight: Integer;
    function GetLeftMargin: Integer;
    function GetTopMargin: Integer;
    function GetRightMargin: Integer;
    function GetBottomMargin: Integer;
    function GetXPixPerInch: Integer;
    function GetYPixPerInch: Integer;
    function GetXPixPerMM: Single;
    function GetYPixPerMM: Single;
    function IsAvailable: Boolean;
    procedure UpdateInfo;

    { Pixel offset calculations from paper edges }
    function PixFromLeft(mmValue: Double): Integer;
    function PixFromRight(mmValue: Double): Integer;
    function PixFromTop(mmValue: Double): Integer;
    function PixFromBottom(mmValue: Double): Integer;

    property PhysicalWidth: Integer read GetPhysicalWidth;
    property PhysicalHeight: Integer read GetPhysicalHeight;
    property PrintableWidth: Integer read GetPrintableWidth;
    property PrintableHeight: Integer read GetPrintableHeight;
    property LeftMargin: Integer read GetLeftMargin;
    property TopMargin: Integer read GetTopMargin;
    property RightMargin: Integer read GetRightMargin;
    property BottomMargin: Integer read GetBottomMargin;
    property XPixPerInch: Integer read GetXPixPerInch;
    property YPixPerInch: Integer read GetYPixPerInch;
    property XPixPerMM: Single read GetXPixPerMM;
    property YPixPerMM: Single read GetYPixPerMM;
  end;

  { Concrete FMX printer info implementation.
    On Windows, uses FMX.Printer and Winapi.Windows.GetDeviceCaps.
    On other platforms, uses a sensible A4/300dpi default. }
  TSynFMXPrinterInfo = class(TInterfacedObject, ISynPrinterInfo)
  private
    FPhysicalWidth: Integer;
    FPhysicalHeight: Integer;
    FPrintableWidth: Integer;
    FPrintableHeight: Integer;
    FLeftGutter: Integer;
    FRightGutter: Integer;
    FTopGutter: Integer;
    FBottomGutter: Integer;
    FXPixPerInch: Integer;
    FYPixPerInch: Integer;
    FXPixPerMM: Single;
    FYPixPerMM: Single;
    FIsUpdated: Boolean;
    procedure FillDefault;
    procedure EnsureUpdated;
  public
    constructor Create;

    { ISynPrinterInfo }
    function GetPhysicalWidth: Integer;
    function GetPhysicalHeight: Integer;
    function GetPrintableWidth: Integer;
    function GetPrintableHeight: Integer;
    function GetLeftMargin: Integer;
    function GetTopMargin: Integer;
    function GetRightMargin: Integer;
    function GetBottomMargin: Integer;
    function GetXPixPerInch: Integer;
    function GetYPixPerInch: Integer;
    function GetXPixPerMM: Single;
    function GetYPixPerMM: Single;
    function IsAvailable: Boolean;
    procedure UpdateInfo;

    function PixFromLeft(mmValue: Double): Integer;
    function PixFromRight(mmValue: Double): Integer;
    function PixFromTop(mmValue: Double): Integer;
    function PixFromBottom(mmValue: Double): Integer;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  FMX.Printer;
{$ELSE}
  FMX.Printer;
{$ENDIF}

const
  mmPerInch = 25.4;

{ TSynFMXPrinterInfo }

constructor TSynFMXPrinterInfo.Create;
begin
  inherited Create;
  FIsUpdated := False;
end;

procedure TSynFMXPrinterInfo.FillDefault;
{ Default values based on HP LaserJet A4 paper at 300 DPI -
  same defaults as the VCL version }
begin
  FPhysicalWidth := 2481;
  FPhysicalHeight := 3507;
  FPrintableWidth := 2358;
  FPrintableHeight := 3407;
  FLeftGutter := 65;
  FRightGutter := 58;
  FTopGutter := 50;
  FBottomGutter := 50;
  FXPixPerInch := 300;
  FYPixPerInch := 300;
  FXPixPerMM := FXPixPerInch / mmPerInch;
  FYPixPerMM := FYPixPerInch / mmPerInch;
end;

procedure TSynFMXPrinterInfo.EnsureUpdated;
begin
  if not FIsUpdated then
    UpdateInfo;
end;

function TSynFMXPrinterInfo.IsAvailable: Boolean;
begin
{$IFDEF MSWINDOWS}
  try
    Result := TPrinterService.Current <> nil;
    if Result then
      Result := TPrinterService.Current.Count > 0;
  except
    Result := False;
  end;
{$ELSE}
  try
    Result := TPrinterService.Current <> nil;
    if Result then
      Result := TPrinterService.Current.Count > 0;
  except
    Result := False;
  end;
{$ENDIF}
end;

procedure TSynFMXPrinterInfo.UpdateInfo;
{$IFDEF MSWINDOWS}
var
  Prn: TPrinter;
  DC: HDC;
{$ENDIF}
begin
  FIsUpdated := True;

{$IFDEF MSWINDOWS}
  if not IsAvailable then
  begin
    FillDefault;
    Exit;
  end;

  try
    Prn := Printer;
    if Prn = nil then
    begin
      FillDefault;
      Exit;
    end;

    DC := Prn.Canvas.Handle;
    if DC = 0 then
    begin
      FillDefault;
      Exit;
    end;

    FPhysicalWidth := GetDeviceCaps(DC, Winapi.Windows.PHYSICALWIDTH);
    FPhysicalHeight := GetDeviceCaps(DC, Winapi.Windows.PHYSICALHEIGHT);
    FPrintableWidth := GetDeviceCaps(DC, HORZRES);
    FPrintableHeight := GetDeviceCaps(DC, VERTRES);
    FLeftGutter := GetDeviceCaps(DC, Winapi.Windows.PHYSICALOFFSETX);
    FTopGutter := GetDeviceCaps(DC, Winapi.Windows.PHYSICALOFFSETY);
    FRightGutter := FPhysicalWidth - FPrintableWidth - FLeftGutter;
    FBottomGutter := FPhysicalHeight - FPrintableHeight - FTopGutter;
    FXPixPerInch := GetDeviceCaps(DC, LOGPIXELSX);
    FYPixPerInch := GetDeviceCaps(DC, LOGPIXELSY);
    FXPixPerMM := FXPixPerInch / mmPerInch;
    FYPixPerMM := FYPixPerInch / mmPerInch;
  except
    FillDefault;
  end;
{$ELSE}
  { Non-Windows platforms: use defaults.
    Future enhancement: query platform-specific printer APIs. }
  FillDefault;
{$ENDIF}
end;

function TSynFMXPrinterInfo.GetPhysicalWidth: Integer;
begin
  EnsureUpdated;
  Result := FPhysicalWidth;
end;

function TSynFMXPrinterInfo.GetPhysicalHeight: Integer;
begin
  EnsureUpdated;
  Result := FPhysicalHeight;
end;

function TSynFMXPrinterInfo.GetPrintableWidth: Integer;
begin
  EnsureUpdated;
  Result := FPrintableWidth;
end;

function TSynFMXPrinterInfo.GetPrintableHeight: Integer;
begin
  EnsureUpdated;
  Result := FPrintableHeight;
end;

function TSynFMXPrinterInfo.GetLeftMargin: Integer;
begin
  EnsureUpdated;
  Result := FLeftGutter;
end;

function TSynFMXPrinterInfo.GetTopMargin: Integer;
begin
  EnsureUpdated;
  Result := FTopGutter;
end;

function TSynFMXPrinterInfo.GetRightMargin: Integer;
begin
  EnsureUpdated;
  Result := FRightGutter;
end;

function TSynFMXPrinterInfo.GetBottomMargin: Integer;
begin
  EnsureUpdated;
  Result := FBottomGutter;
end;

function TSynFMXPrinterInfo.GetXPixPerInch: Integer;
begin
  EnsureUpdated;
  Result := FXPixPerInch;
end;

function TSynFMXPrinterInfo.GetYPixPerInch: Integer;
begin
  EnsureUpdated;
  Result := FYPixPerInch;
end;

function TSynFMXPrinterInfo.GetXPixPerMM: Single;
begin
  EnsureUpdated;
  Result := FXPixPerMM;
end;

function TSynFMXPrinterInfo.GetYPixPerMM: Single;
begin
  EnsureUpdated;
  Result := FYPixPerMM;
end;

function TSynFMXPrinterInfo.PixFromLeft(mmValue: Double): Integer;
begin
  EnsureUpdated;
  Result := Round(mmValue * FXPixPerMM - FLeftGutter);
end;

function TSynFMXPrinterInfo.PixFromRight(mmValue: Double): Integer;
begin
  EnsureUpdated;
  Result := Round(mmValue * FXPixPerMM - FRightGutter);
end;

function TSynFMXPrinterInfo.PixFromTop(mmValue: Double): Integer;
begin
  EnsureUpdated;
  Result := Round(mmValue * FYPixPerMM - FTopGutter);
end;

function TSynFMXPrinterInfo.PixFromBottom(mmValue: Double): Integer;
begin
  EnsureUpdated;
  Result := Round(mmValue * FYPixPerMM - FBottomGutter);
end;

end.
