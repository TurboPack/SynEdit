{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditScrollBars;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.SynEditTypes,
  SynEditTypes;

{ Factory method }
function CreateSynEditScrollBars(Editor: TControl): ISynEditScrollBars;

implementation

uses
  FMX.SynEdit;

type
  TSynFMXScrollBars = class(TInterfacedObject, ISynEditScrollBars)
  private
    FEditor: TCustomFMXSynEdit;
    FVScrollBar: TScrollBar;
    FHScrollBar: TScrollBar;
    FIsScrolling: Boolean;
    FUpdating: Boolean;
    procedure VScrollChange(Sender: TObject);
    procedure HScrollChange(Sender: TObject);
  public
    constructor Create(AEditor: TControl);
    function UpdateScrollBars: Boolean;
    function GetIsScrolling: Boolean;
    function GetVisibleVScrollBarWidth: Single;
    function GetVisibleHScrollBarHeight: Single;
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPointF);
  end;

function CreateSynEditScrollBars(Editor: TControl): ISynEditScrollBars;
begin
  Result := TSynFMXScrollBars.Create(Editor);
end;

{ TSynFMXScrollBars }

constructor TSynFMXScrollBars.Create(AEditor: TControl);
begin
  inherited Create;
  FEditor := AEditor as TCustomFMXSynEdit;

  FVScrollBar := TScrollBar.Create(FEditor);
  FVScrollBar.Parent := FEditor;
  FVScrollBar.Orientation := TOrientation.Vertical;
  FVScrollBar.Align := TAlignLayout.Right;
  FVScrollBar.Width := 16;
  FVScrollBar.Visible := False;
  FVScrollBar.SmallChange := 1;
  FVScrollBar.OnChange := VScrollChange;

  FHScrollBar := TScrollBar.Create(FEditor);
  FHScrollBar.Parent := FEditor;
  FHScrollBar.Orientation := TOrientation.Horizontal;
  FHScrollBar.Align := TAlignLayout.Bottom;
  FHScrollBar.Height := 16;
  FHScrollBar.Visible := False;
  FHScrollBar.SmallChange := 1;
  FHScrollBar.OnChange := HScrollChange;
end;

procedure TSynFMXScrollBars.VScrollChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FIsScrolling := True;
  try
    FEditor.TopLine := Round(FVScrollBar.Value);
  finally
    FIsScrolling := False;
  end;
end;

procedure TSynFMXScrollBars.HScrollChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FIsScrolling := True;
  try
    FEditor.LeftChar := Round(FHScrollBar.Value);
  finally
    FIsScrolling := False;
  end;
end;

function TSynFMXScrollBars.UpdateScrollBars: Boolean;
var
  MaxLines, MaxCols, VisLines, VisCols: Integer;
  ShowVert, ShowHorz: Boolean;
begin
  Result := False;
  FUpdating := True;
  try
    VisLines := FEditor.LinesInWindow;
    VisCols := FEditor.CharsInWindow;
    MaxLines := FEditor.LineCount;
    if eoScrollPastEof in FEditor.ScrollOptions then
      Inc(MaxLines, VisLines - 1);
    MaxCols := FEditor.MaxScrollWidth;

    ShowVert := (VisLines > 0) and (MaxLines > VisLines);
    ShowHorz := (VisCols > 0) and (MaxCols > VisCols);

    // Vertical
    FVScrollBar.Visible := ShowVert;
    if ShowVert then
    begin
      FVScrollBar.Min := 1;
      FVScrollBar.Max := Max(1, MaxLines);
      FVScrollBar.ViewportSize := VisLines;
      FVScrollBar.Value := FEditor.TopLine;
    end;

    // Horizontal
    FHScrollBar.Visible := ShowHorz;
    if ShowHorz then
    begin
      FHScrollBar.Min := 1;
      FHScrollBar.Max := Max(1, MaxCols);
      FHScrollBar.ViewportSize := VisCols;
      FHScrollBar.Value := FEditor.LeftChar;
    end;
  finally
    FUpdating := False;
  end;
end;

function TSynFMXScrollBars.GetIsScrolling: Boolean;
begin
  Result := FIsScrolling;
end;

function TSynFMXScrollBars.GetVisibleVScrollBarWidth: Single;
begin
  if FVScrollBar.Visible then
    Result := FVScrollBar.Width
  else
    Result := 0;
end;

function TSynFMXScrollBars.GetVisibleHScrollBarHeight: Single;
begin
  if FHScrollBar.Visible then
    Result := FHScrollBar.Height
  else
    Result := 0;
end;

procedure TSynFMXScrollBars.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPointF);
const
  SCROLL_LINES = 3;
begin
  if ssShift in Shift then
    FEditor.LeftChar := FEditor.LeftChar - Sign(WheelDelta) * SCROLL_LINES
  else
    FEditor.TopLine := FEditor.TopLine - Sign(WheelDelta) * SCROLL_LINES;
end;

end.
