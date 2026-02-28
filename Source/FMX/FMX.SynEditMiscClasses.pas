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

const
  { Cross-platform selection color defaults.
    TColors.SysHighlight/SysHighlightText are system colors (negative TColor
    values) that resolve via GetSysColor on Windows.  On non-Windows FMX
    targets TColorToAlphaColor returns TAlphaColors.Null for system colors.
    Use explicit TColor values (BGR format) that work on all platforms. }
  clDefaultSelectionBG = TColors.Dodgerblue;  // $FF901E in BGR
  clDefaultSelectionFG = TColors.White;       // $FFFFFF in BGR

type
  { Bookmark/mark for FMX SynEdit }
  TSynFMXEditMark = class
  private
    FLine: Integer;
    FChar: Integer;
    FBookmarkNum: Integer;
    FVisible: Boolean;
    function GetIsBookmark: Boolean;
  public
    constructor Create;
    property Line: Integer read FLine write FLine;
    property Char: Integer read FChar write FChar;
    property BookmarkNumber: Integer read FBookmarkNum write FBookmarkNum;
    property IsBookmark: Boolean read GetIsBookmark;
    property Visible: Boolean read FVisible write FVisible;
  end;

  { List of marks/bookmarks }
  TSynFMXEditMarkList = class(TObjectList<TSynFMXEditMark>)
  public
    function GetMarksForLine(ALine: Integer): TArray<TSynFMXEditMark>;
    procedure ClearLine(ALine: Integer);
  end;

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
    property Background: TColor read FBG write SetBG;
    property Foreground: TColor read FFG write SetFG;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property FillWholeLines: Boolean read FFillWholeLines write SetFillWholeLines
      default True;
  end;

  { TSynEditSearchCustom is now in the shared SynEditTypes.pas unit }

implementation

{ TSynFMXEditMark }

constructor TSynFMXEditMark.Create;
begin
  inherited;
  FBookmarkNum := -1;
  FVisible := True;
end;

function TSynFMXEditMark.GetIsBookmark: Boolean;
begin
  Result := FBookmarkNum >= 0;
end;

{ TSynFMXEditMarkList }

function TSynFMXEditMarkList.GetMarksForLine(ALine: Integer): TArray<TSynFMXEditMark>;
var
  I, Count: Integer;
begin
  Count := 0;
  SetLength(Result, Self.Count);
  for I := 0 to Self.Count - 1 do
    if Items[I].Line = ALine then
    begin
      Result[Count] := Items[I];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

procedure TSynFMXEditMarkList.ClearLine(ALine: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if (Items[I].Line = ALine) and not Items[I].IsBookmark then
      Delete(I);
end;

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited;
  FBG := clDefaultSelectionBG;
  FFG := clDefaultSelectionFG;
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
