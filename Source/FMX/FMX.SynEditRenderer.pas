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
  System.SysUtils,
  System.Classes,
  System.Math,
  FMX.Types,
  FMX.Graphics,
  FMX.TextLayout,
  SynEditTypes;

type
  { Font quality settings for FMX rendering }
  TSynFontQuality = (fqDefault, fqAntialiased, fqClearType);

  { Text layout cache for FMX rendering }
  TSynTextLayoutCache = class
  private
    FCanvas: TCanvas;
    FFont: TFont;
    FCharWidth: Single;
    FLineHeight: Single;
    procedure UpdateMetrics;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetFont(ACanvas: TCanvas; AFont: TFont);
    function TextWidth(const S: string): Single;
    function TextExtent(const S: string): TSizeF;
    property CharWidth: Single read FCharWidth;
    property LineHeight: Single read FLineHeight;
  end;

implementation

{ TSynTextLayoutCache }

constructor TSynTextLayoutCache.Create;
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TSynTextLayoutCache.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TSynTextLayoutCache.SetFont(ACanvas: TCanvas; AFont: TFont);
begin
  FCanvas := ACanvas;
  FFont.Assign(AFont);
  UpdateMetrics;
end;

procedure TSynTextLayoutCache.UpdateMetrics;
var
  Layout: TTextLayout;
begin
  if FCanvas = nil then Exit;
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.Font.Assign(FFont);
    Layout.Text := 'W';
    Layout.MaxSize := TPointF.Create(10000, 10000);
    FCharWidth := Layout.TextWidth;
    FLineHeight := Layout.TextHeight;
  finally
    Layout.Free;
  end;
end;

function TSynTextLayoutCache.TextWidth(const S: string): Single;
begin
  if S = '' then Exit(0);
  Result := Length(S) * FCharWidth;
end;

function TSynTextLayoutCache.TextExtent(const S: string): TSizeF;
begin
  Result.cx := TextWidth(S);
  Result.cy := FLineHeight;
end;

end.
