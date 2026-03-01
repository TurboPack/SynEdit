{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
CONTENTS:
  Class handling all sizes involved when printing.
  Uses ISynPrinterInfo for DPI information - pure math, no platform
  dependencies beyond the printer info interface.

  Design-time properties:
    UnitSystem : Units used to specify sizes. Internally always mm.
    Left       : Distance from left edge of paper to text.
    Right      : Distance from right edge of paper to text.
    Top        : Distance from top edge of paper to top of text.
    Bottom     : Distance from bottom edge of paper to bottom of text.
    Gutter     : Binding gutter - added to right margin (or left if 2-sided)
    Header     : Distance from top edge of paper to line below header.
    Footer     : Distance from bottom edge of paper to line above footer.
    LeftHFTextIndent  : Distance from left margin to first left-aligned
                        character in header or footer.
    RightHFTextIndent : Distance from right margin to last right-aligned
                        character in header or footer.
    HFInternalMargin  : Internal margin between frame line and text in
                        header and footer.
    MirrorMargins     : Mirror margins for 2-sided printing.

  Run-time properties (after InitPage):
    PLeft, PRight, PTop, PBottom : Text area in device units (pixels).
    PHeader, PFooter             : Header/footer positions in device units.
    PLeftHFTextIndent, PRightHFTextIndent : Header/footer text indents.
    PHFInternalMargin            : Internal margin in device units.
    PGutter                      : Binding gutter in device units.
-------------------------------------------------------------------------------}

unit FMX.SynEditPrintMargins;

{$M+}
{$I SynEdit.inc}

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Math,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  FMX.Graphics,
  FMX.TextLayout,
  FMX.SynEditPrintTypes,
  FMX.SynEditPrinterInfo;

type
  TSynEditPrintMargins = class(TPersistent)
  private
    FLeft: Double;
    FRight: Double;
    FTop: Double;
    FBottom: Double;
    FHeader: Double;
    FFooter: Double;
    FLeftHFTextIndent: Double;
    FRightHFTextIndent: Double;
    FHFInternalMargin: Double;
    FGutter: Double;
    FMirrorMargins: Boolean;
    FUnitSystem: TUnitSystem;
    function ConvertTo(Value: Double): Double;
    function ConvertFrom(Value: Double): Double;
    function GetBottom: Double;
    function GetFooter: Double;
    function GetGutter: Double;
    function GetHeader: Double;
    function GetLeft: Double;
    function GetRight: Double;
    function GetTop: Double;
    function GetLeftHFTextIndent: Double;
    function GetRightHFTextIndent: Double;
    function GetHFInternalMargin: Double;
    procedure SetBottom(const Value: Double);
    procedure SetFooter(const Value: Double);
    procedure SetGutter(const Value: Double);
    procedure SetHeader(const Value: Double);
    procedure SetLeft(const Value: Double);
    procedure SetRight(const Value: Double);
    procedure SetTop(const Value: Double);
    procedure SetLeftHFTextIndent(const Value: Double);
    procedure SetRightHFTextIndent(const Value: Double);
    procedure SetHFInternalMargin(const Value: Double);
  public
    { Computed pixel positions - valid after InitPage }
    PLeft: Integer;
    PRight: Integer;
    PTop: Integer;
    PBottom: Integer;
    PHeader: Integer;
    PFooter: Integer;
    PLeftHFTextIndent: Integer;
    PRightHFTextIndent: Integer;
    PHFInternalMargin: Integer;
    PGutter: Integer;

    constructor Create;
    procedure InitPage(AFont: TFont; ATabWidth: Integer; PageNum: Integer;
      PrinterInfo: ISynPrinterInfo; LineNumbers, LineNumbersInMargin: Boolean;
      MaxLineNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    { Direct pixel-position queries using an ISynPrinterInfo, without
      calling InitPage. Useful for custom layout calculations. }
    function GetPixelLeft(PrinterInfo: ISynPrinterInfo): Integer;
    function GetPixelRight(PrinterInfo: ISynPrinterInfo): Integer;
    function GetPixelTop(PrinterInfo: ISynPrinterInfo): Integer;
    function GetPixelBottom(PrinterInfo: ISynPrinterInfo): Integer;
    function GetPixelHeader(PrinterInfo: ISynPrinterInfo): Integer;
    function GetPixelFooter(PrinterInfo: ISynPrinterInfo): Integer;
  published
    property UnitSystem: TUnitSystem read FUnitSystem write FUnitSystem
      default usMM;
    property Left: Double read GetLeft write SetLeft;
    property Right: Double read GetRight write SetRight;
    property Top: Double read GetTop write SetTop;
    property Bottom: Double read GetBottom write SetBottom;
    property Header: Double read GetHeader write SetHeader;
    property Footer: Double read GetFooter write SetFooter;
    property LeftHFTextIndent: Double read GetLeftHFTextIndent
      write SetLeftHFTextIndent;
    property RightHFTextIndent: Double read GetRightHFTextIndent
      write SetRightHFTextIndent;
    property HFInternalMargin: Double read GetHFInternalMargin
      write SetHFInternalMargin;
    property Gutter: Double read GetGutter write SetGutter;
    property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins;
  end;

implementation

const
  mmPerInch = 25.4;
  mmPerCm = 10;

{ TSynEditPrintMargins }

constructor TSynEditPrintMargins.Create;
begin
  inherited;
  FUnitSystem := usMM;
  FLeft := DefLeft;
  FRight := DefRight;
  FTop := DefTop;
  FBottom := DefBottom;
  FHeader := DefHeader;
  FFooter := DefFooter;
  FLeftHFTextIndent := DefLeftHFTextIndent;
  FRightHFTextIndent := DefRightHFTextIndent;
  FHFInternalMargin := DefHFInternalMargin;
  FGutter := DefGutter;
  FMirrorMargins := False;
end;

function TSynEditPrintMargins.ConvertTo(Value: Double): Double;
{ Convert from current UnitSystem to mm }
begin
  case FUnitSystem of
    usCM: Result := Value * mmPerCm;
    usInch: Result := Value * mmPerInch;
    muThousandthsOfInches: Result := mmPerInch * Value / 1000;
  else
    Result := Value;
  end;
end;

function TSynEditPrintMargins.ConvertFrom(Value: Double): Double;
{ Convert from mm to current UnitSystem }
begin
  case FUnitSystem of
    usCM: Result := Value / mmPerCm;
    usInch: Result := Value / mmPerInch;
    muThousandthsOfInches: Result := 1000 * Value / mmPerInch;
  else
    Result := Value;
  end;
end;

function TSynEditPrintMargins.GetBottom: Double;
begin
  Result := ConvertFrom(FBottom);
end;

function TSynEditPrintMargins.GetFooter: Double;
begin
  Result := ConvertFrom(FFooter);
end;

function TSynEditPrintMargins.GetGutter: Double;
begin
  Result := ConvertFrom(FGutter);
end;

function TSynEditPrintMargins.GetHeader: Double;
begin
  Result := ConvertFrom(FHeader);
end;

function TSynEditPrintMargins.GetLeft: Double;
begin
  Result := ConvertFrom(FLeft);
end;

function TSynEditPrintMargins.GetRight: Double;
begin
  Result := ConvertFrom(FRight);
end;

function TSynEditPrintMargins.GetTop: Double;
begin
  Result := ConvertFrom(FTop);
end;

function TSynEditPrintMargins.GetLeftHFTextIndent: Double;
begin
  Result := ConvertFrom(FLeftHFTextIndent);
end;

function TSynEditPrintMargins.GetRightHFTextIndent: Double;
begin
  Result := ConvertFrom(FRightHFTextIndent);
end;

function TSynEditPrintMargins.GetHFInternalMargin: Double;
begin
  Result := ConvertFrom(FHFInternalMargin);
end;

procedure TSynEditPrintMargins.SetBottom(const Value: Double);
begin
  FBottom := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetFooter(const Value: Double);
begin
  FFooter := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetGutter(const Value: Double);
begin
  FGutter := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetHeader(const Value: Double);
begin
  FHeader := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetLeft(const Value: Double);
begin
  FLeft := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetRight(const Value: Double);
begin
  FRight := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetTop(const Value: Double);
begin
  FTop := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetLeftHFTextIndent(const Value: Double);
begin
  FLeftHFTextIndent := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetRightHFTextIndent(const Value: Double);
begin
  FRightHFTextIndent := ConvertTo(Value);
end;

procedure TSynEditPrintMargins.SetHFInternalMargin(const Value: Double);
begin
  FHFInternalMargin := ConvertTo(Value);
end;

{ Direct pixel queries }

function TSynEditPrintMargins.GetPixelLeft(PrinterInfo: ISynPrinterInfo): Integer;
begin
  Result := MulDiv(PrinterInfo.PixFromLeft(FLeft + FGutter), 96,
    PrinterInfo.XPixPerInch);
end;

function TSynEditPrintMargins.GetPixelRight(PrinterInfo: ISynPrinterInfo): Integer;
begin
  Result := MulDiv(PrinterInfo.PrintableWidth -
    PrinterInfo.PixFromRight(FRight), 96, PrinterInfo.XPixPerInch);
end;

function TSynEditPrintMargins.GetPixelTop(PrinterInfo: ISynPrinterInfo): Integer;
begin
  Result := MulDiv(PrinterInfo.PixFromTop(FTop), 96,
    PrinterInfo.YPixPerInch);
end;

function TSynEditPrintMargins.GetPixelBottom(PrinterInfo: ISynPrinterInfo): Integer;
begin
  Result := MulDiv(PrinterInfo.PrintableHeight -
    PrinterInfo.PixFromBottom(FBottom), 96, PrinterInfo.YPixPerInch);
end;

function TSynEditPrintMargins.GetPixelHeader(PrinterInfo: ISynPrinterInfo): Integer;
begin
  Result := MulDiv(PrinterInfo.PixFromTop(FHeader), 96,
    PrinterInfo.YPixPerInch);
end;

function TSynEditPrintMargins.GetPixelFooter(PrinterInfo: ISynPrinterInfo): Integer;
begin
  Result := MulDiv(PrinterInfo.PrintableHeight -
    PrinterInfo.PixFromBottom(FFooter), 96, PrinterInfo.YPixPerInch);
end;

{ InitPage - called by the print component to compute pixel positions.
  All P* values are calculated relative to 96 PPI coordinates. }
procedure TSynEditPrintMargins.InitPage(AFont: TFont; ATabWidth: Integer;
  PageNum: Integer; PrinterInfo: ISynPrinterInfo;
  LineNumbers, LineNumbersInMargin: Boolean; MaxLineNum: Integer);
var
  Layout: TTextLayout;
  Str: string;
  LineNumWidth: Single;
begin
  if FMirrorMargins and ((PageNum mod 2) = 0) then
  begin
    PLeft := MulDiv(PrinterInfo.PixFromLeft(FRight), 96,
      PrinterInfo.XPixPerInch);
    PRight := MulDiv(PrinterInfo.PrintableWidth -
      PrinterInfo.PixFromRight(FLeft + FGutter), 96, PrinterInfo.XPixPerInch);
  end
  else begin
    PLeft := MulDiv(PrinterInfo.PixFromLeft(FLeft + FGutter), 96,
      PrinterInfo.XPixPerInch);
    PRight := MulDiv(PrinterInfo.PrintableWidth -
      PrinterInfo.PixFromRight(FRight), 96, PrinterInfo.XPixPerInch);
  end;

  { If line numbers are not in the margin, shift text left boundary }
  if LineNumbers and (not LineNumbersInMargin) then
  begin
    Str := IntToStr(MaxLineNum) + ': ';
    Layout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      Layout.BeginUpdate;
      try
        Layout.Font.Assign(AFont);
        Layout.Text := Str;
        Layout.MaxSize := TPointF.Create(10000, 10000);
      finally
        Layout.EndUpdate;
      end;
      LineNumWidth := Layout.TextWidth;
    finally
      Layout.Free;
    end;
    PLeft := PLeft + Round(LineNumWidth);
  end;

  PTop := MulDiv(PrinterInfo.PixFromTop(FTop), 96, PrinterInfo.YPixPerInch);
  PBottom := MulDiv(PrinterInfo.PrintableHeight -
    PrinterInfo.PixFromBottom(FBottom), 96, PrinterInfo.YPixPerInch);
  PHeader := MulDiv(PrinterInfo.PixFromTop(FHeader), 96,
    PrinterInfo.YPixPerInch);
  PFooter := MulDiv(PrinterInfo.PrintableHeight -
    PrinterInfo.PixFromBottom(FFooter), 96, PrinterInfo.YPixPerInch);
  PHFInternalMargin := MulDiv(Round(PrinterInfo.YPixPerMM * FHFInternalMargin),
    96, PrinterInfo.XPixPerInch);
  PGutter := MulDiv(Round(PrinterInfo.XPixPerMM * FGutter), 96,
    PrinterInfo.XPixPerInch);
  PRightHFTextIndent := PRight -
    MulDiv(Round(PrinterInfo.XPixPerMM * FRightHFTextIndent), 96,
      PrinterInfo.XPixPerInch);
  PLeftHFTextIndent := PLeft +
    MulDiv(Round(PrinterInfo.XPixPerMM * FLeftHFTextIndent), 96,
      PrinterInfo.XPixPerInch);
end;

procedure TSynEditPrintMargins.Assign(Source: TPersistent);
var
  Src: TSynEditPrintMargins;
begin
  if (Source <> nil) and (Source is TSynEditPrintMargins) then
  begin
    Src := TSynEditPrintMargins(Source);
    FLeft := Src.FLeft;
    FRight := Src.FRight;
    FTop := Src.FTop;
    FBottom := Src.FBottom;
    FHeader := Src.FHeader;
    FFooter := Src.FFooter;
    FLeftHFTextIndent := Src.FLeftHFTextIndent;
    FRightHFTextIndent := Src.FRightHFTextIndent;
    FHFInternalMargin := Src.FHFInternalMargin;
    FGutter := Src.FGutter;
    FMirrorMargins := Src.FMirrorMargins;
    FUnitSystem := Src.FUnitSystem;
  end
  else
    inherited;
end;

procedure TSynEditPrintMargins.LoadFromStream(AStream: TStream);
begin
  with AStream do
  begin
    Read(FUnitSystem, SizeOf(FUnitSystem));
    Read(FLeft, SizeOf(FLeft));
    Read(FRight, SizeOf(FRight));
    Read(FTop, SizeOf(FTop));
    Read(FBottom, SizeOf(FBottom));
    Read(FHeader, SizeOf(FHeader));
    Read(FFooter, SizeOf(FFooter));
    Read(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
    Read(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
    Read(FHFInternalMargin, SizeOf(FHFInternalMargin));
    Read(FGutter, SizeOf(FGutter));
    Read(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

procedure TSynEditPrintMargins.SaveToStream(AStream: TStream);
begin
  with AStream do
  begin
    Write(FUnitSystem, SizeOf(FUnitSystem));
    Write(FLeft, SizeOf(FLeft));
    Write(FRight, SizeOf(FRight));
    Write(FTop, SizeOf(FTop));
    Write(FBottom, SizeOf(FBottom));
    Write(FHeader, SizeOf(FHeader));
    Write(FFooter, SizeOf(FFooter));
    Write(FLeftHFTextIndent, SizeOf(FLeftHFTextIndent));
    Write(FRightHFTextIndent, SizeOf(FRightHFTextIndent));
    Write(FHFInternalMargin, SizeOf(FHFInternalMargin));
    Write(FGutter, SizeOf(FGutter));
    Write(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

end.
