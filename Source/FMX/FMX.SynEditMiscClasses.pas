{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditMiscClasses;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditKeyConst;

type
  { Selected text color }
  TSynSelectedColor = class(TPersistent)
  private
    FBG: TColor;
    FFG: TColor;
    FOnChange: TNotifyEvent;
    FOpacity: Byte;
    FFillWholeLines: Boolean;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
    procedure SetOpacity(Value: Byte);
    procedure SetFillWholeLines(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Background: TColor read FBG write SetBG default clHighlight;
    property Foreground: TColor read FFG write SetFG default clHighlightText;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property FillWholeLines: Boolean read FFillWholeLines write SetFillWholeLines
      default True;
  end;

  { TSynEditSearchCustom is now in the shared SynEditTypes.pas unit }

implementation

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited;
  FBG := clHighlight;
  FFG := clHighlightText;
  FOpacity := 255;
  FFillWholeLines := True;
end;

procedure TSynSelectedColor.Assign(Source: TPersistent);
begin
  if Source is TSynSelectedColor then
  begin
    FBG := TSynSelectedColor(Source).FBG;
    FFG := TSynSelectedColor(Source).FFG;
    FOpacity := TSynSelectedColor(Source).FOpacity;
    FFillWholeLines := TSynSelectedColor(Source).FFillWholeLines;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited;
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if FBG <> Value then
  begin
    FBG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if FFG <> Value then
  begin
    FFG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetOpacity(Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFillWholeLines(const Value: Boolean);
begin
  if FFillWholeLines <> Value then
  begin
    FFillWholeLines := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

end.
