{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditRenderer;

{ FMX Canvas-based rendering engine for SynEdit.
  Replaces the VCL Direct2D/DirectWrite renderer (SynDWrite). }

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.SysUtils,
  System.Classes,
  System.Math,
  FMX.Types,
  FMX.Graphics,
  FMX.TextLayout;

type
  TSynFMXRenderer = class
  private
    FFont: TFont;
    FCharWidth: Single;
    FLineHeight: Single;
    FLayout: TTextLayout;
    procedure UpdateMetrics;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetFont(AFont: TFont);

    { Paint a text token at the given pixel position }
    procedure PaintToken(Canvas: TCanvas; X, Y: Single;
      const Text: string; ForeColor, BackColor: TAlphaColor;
      Style: TFontStyles);

    { Fill a rectangle }
    procedure FillRect(Canvas: TCanvas; const R: TRectF; Color: TAlphaColor);

    { Draw a line }
    procedure DrawLine(Canvas: TCanvas; X1, Y1, X2, Y2: Single;
      Color: TAlphaColor; StrokeWidth: Single = 1.0);

    { Paint right-aligned text (for gutter line numbers) }
    procedure PaintLineNumber(Canvas: TCanvas; const R: TRectF;
      const Text: string; Color: TAlphaColor);

    property CharWidth: Single read FCharWidth;
    property LineHeight: Single read FLineHeight;
    property Font: TFont read FFont;
  end;

{ Converts TColor ($00BBGGRR) to TAlphaColor ($AARRGGBB) }
function TColorToAlphaColor(AColor: TColor): TAlphaColor;

implementation

function TColorToAlphaColor(AColor: TColor): TAlphaColor;
begin
  if (AColor = TColors.SysNone) or (Integer(AColor) < 0) then
    Exit(TAlphaColors.Null);
  // TColor = $00BBGGRR -> TAlphaColor = $AARRGGBB
  Result := $FF000000 or
    (Cardinal(AColor and $FF) shl 16) or
    (Cardinal(AColor and $FF00)) or
    (Cardinal(AColor shr 16) and $FF);
end;

{ TSynFMXRenderer }

constructor TSynFMXRenderer.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.Family := 'Consolas';
  FFont.Size := 10;
  FLayout := TTextLayoutManager.DefaultTextLayout.Create;
  UpdateMetrics;
end;

destructor TSynFMXRenderer.Destroy;
begin
  FLayout.Free;
  FFont.Free;
  inherited;
end;

procedure TSynFMXRenderer.SetFont(AFont: TFont);
begin
  FFont.Assign(AFont);
  UpdateMetrics;
end;

procedure TSynFMXRenderer.UpdateMetrics;
begin
  FLayout.BeginUpdate;
  try
    FLayout.Font.Assign(FFont);
    FLayout.Text := 'M';
    FLayout.MaxSize := TPointF.Create(10000, 10000);
  finally
    FLayout.EndUpdate;
  end;
  FCharWidth := FLayout.TextWidth;
  FLineHeight := FLayout.TextHeight;
  if FLineHeight < 1 then FLineHeight := FFont.Size * 1.5;
  if FCharWidth < 1 then FCharWidth := FFont.Size * 0.6;
end;

procedure TSynFMXRenderer.PaintToken(Canvas: TCanvas; X, Y: Single;
  const Text: string; ForeColor, BackColor: TAlphaColor;
  Style: TFontStyles);
var
  R: TRectF;
begin
  if Text = '' then Exit;
  R := RectF(X, Y, X + Length(Text) * FCharWidth, Y + FLineHeight);

  if BackColor <> TAlphaColors.Null then
  begin
    Canvas.Fill.Color := BackColor;
    Canvas.FillRect(R, 0, 0, AllCorners, 1.0);
  end;

  Canvas.Font.Assign(FFont);
  Canvas.Font.Style := Style;
  Canvas.Fill.Color := ForeColor;
  Canvas.FillText(R, Text, False, 1.0, [], TTextAlign.Leading, TTextAlign.Leading);
end;

procedure TSynFMXRenderer.FillRect(Canvas: TCanvas; const R: TRectF;
  Color: TAlphaColor);
begin
  if Color = TAlphaColors.Null then Exit;
  Canvas.Fill.Color := Color;
  Canvas.FillRect(R, 0, 0, AllCorners, 1.0);
end;

procedure TSynFMXRenderer.DrawLine(Canvas: TCanvas; X1, Y1, X2, Y2: Single;
  Color: TAlphaColor; StrokeWidth: Single);
begin
  Canvas.Stroke.Color := Color;
  Canvas.Stroke.Thickness := StrokeWidth;
  Canvas.DrawLine(PointF(X1, Y1), PointF(X2, Y2), 1.0);
end;

procedure TSynFMXRenderer.PaintLineNumber(Canvas: TCanvas; const R: TRectF;
  const Text: string; Color: TAlphaColor);
begin
  Canvas.Font.Assign(FFont);
  Canvas.Font.Style := [];
  Canvas.Fill.Color := Color;
  Canvas.FillText(R, Text, False, 1.0, [],
    TTextAlign.Trailing, TTextAlign.Leading);
end;

end.
