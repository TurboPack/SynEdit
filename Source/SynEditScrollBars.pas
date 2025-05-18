{ ------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Contributors to the SynEdit and mwEdit projects are listed in the
  Contributors.txt file.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.
  ---------------------------------------------------------------------------- }

unit SynEditScrollBars;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.UITypes,
  Vcl.Controls,
  SynEditTypes;

{ Factory Method }
function CreateSynEditScrollBars(Editor: TCustomControl): ISynEditScrollBars;

implementation

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Forms,
  SynEdit,
  SynEditTextBuffer,
  SynEditMiscProcs,
  SynEditStrConst,
  SynEditKeyConst;

type
  TSynScrollBarState = record
    Kind: TScrollBarKind;
    Active: Boolean;
    nMin: Integer;
    nMax: Integer;
    nPage: Integer;   // Note: Struct define is UINT
    nPos: Integer;
    class operator Equal(a, b: TSynScrollBarState): Boolean;
    class operator NotEqual(a, b: TSynScrollBarState): Boolean;
  end;

  TSynEditScrollBars = class(TInterfacedObject, ISynEditScrollBars)
  private
    FOwner: TCustomSynEdit;
    FIsScrolling: Boolean;
    FMouseWheelVertAccumulator: Integer;
    FMouseWheelHorzAccumulator: Integer;
    FPrevHorzSBState: TSynScrollBarState;  // Last applied horizontal scrollbar state
    FPrevVertSBState: TSynScrollBarState;  // Last applied vertical scrollbar state
    FNewHorzSBState: TSynScrollBarState;   // New Horizontal ScrollBar state
    FNewVertSBState: TSynScrollBarState;   // New Vertical ScrollBar state
    function GetIsScrolling: Boolean;
    function GetHorzPageInChars: Integer;
    function GetBarScrollInfo(AKind: TScrollBarKind): TScrollInfo;
    procedure SetScrollBarFromState(const AState: TSynScrollBarState);
    procedure SetScrollBarPos(AKind: TScrollBarKind; APos: Integer; ARefresh: Boolean);
    procedure ApplyButtonState(const AState: TSynScrollBarState);
    procedure UpdateScrollBarsState;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    function UpdateScrollBars: Boolean;
    procedure WMHScroll(var AMsg: TWMScroll);
    procedure WMVScroll(var AMsg: TWMScroll);
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint);
  end;

{ TSynEditScrollBars }

constructor TSynEditScrollBars.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  FOwner := AOwner;
  FPrevHorzSBState.Kind := sbHorizontal;
  FPrevVertSBState.Kind := sbVertical;
  FNewHorzSBState.Kind := sbHorizontal;
  FNewVertSBState.Kind := sbVertical;
end;

destructor TSynEditScrollBars.Destroy;
begin
  inherited;
end;

function TSynEditScrollBars.GetBarScrollInfo(AKind: TScrollBarKind): TScrollInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(Result);
  Result.fMask := SIF_ALL;
  if AKind = sbHorizontal then
    GetScrollInfo(FOwner.Handle, SB_HORZ, Result)
  else
    GetScrollInfo(FOwner.Handle, SB_VERT, Result);
end;

function TSynEditScrollBars.GetHorzPageInChars: Integer;
begin
  Result := FOwner.TextAreaWidth div FOwner.CharWidth;
end;

function TSynEditScrollBars.GetIsScrolling: Boolean;
begin
  Result := FIsScrolling;
end;

procedure TSynEditScrollBars.ApplyButtonState(const AState: TSynScrollBarState);
var
  BarKind: Integer;
  Btn1Enabled: Boolean;
  Btn2Enabled: Boolean;
begin
  if AState.Kind = sbHorizontal then
    BarKind := SB_HORZ
  else
    BarKind := SB_VERT;
  Btn1Enabled := True;
  Btn2Enabled := True;
  if (eoDisableScrollArrows in FOwner.Options) then
  begin
    Btn1Enabled := (AState.nPos > AState.nMin);
    Btn2Enabled := ((AState.nPos + AState.nPage - AState.nMin) < AState.nMax);
  end;
  if not Btn1Enabled and not Btn2Enabled then
    EnableScrollBar(FOwner.Handle, BarKind, ESB_DISABLE_BOTH)
  else
  begin
    EnableScrollBar(FOwner.Handle, BarKind, ESB_ENABLE_BOTH);
    if not Btn1Enabled then
      EnableScrollBar(FOwner.Handle, BarKind, ESB_DISABLE_LTUP);
    if not Btn2Enabled then
      EnableScrollBar(FOwner.Handle, BarKind, ESB_DISABLE_RTDN);
  end;
end;

procedure TSynEditScrollBars.SetScrollBarPos(AKind: TScrollBarKind;
  APos: Integer; ARefresh: Boolean);
var
  BarKind: Integer;
  ScrollInfo: TScrollInfo;
begin
  if AKind = sbHorizontal then
    BarKind := SB_HORZ
  else
    BarKind := SB_VERT;
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_POS;
  ScrollInfo.nPos := APos;
  SetScrollInfo(FOwner.Handle, BarKind, ScrollInfo, ARefresh);
end;

procedure TSynEditScrollBars.SetScrollBarFromState(const AState: TSynScrollBarState);
var
  WindowStyle: NativeInt;
  ScrollInfo: TScrollInfo;
  AutoVis: Boolean;
  BarKind: Integer;
  BarWSCode: Integer;
  ScrollbarVisible: Boolean;
  HideEnabled: Boolean;
begin
  WindowStyle := GetWindowLong(FOwner.Handle, GWL_STYLE);
  HideEnabled := (eoHideShowScrollbars in FOwner.Options);
  if AState.Kind = sbHorizontal then
  begin
    BarKind := SB_HORZ;
    BarWSCode := WS_HSCROLL;
  end
  else
  begin
    BarKind := SB_VERT;
    BarWSCode := WS_VSCROLL;
  end;
  ScrollbarVisible := (WindowStyle and BarWSCode <> 0);
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  if AState.Active then
  begin

    if not HideEnabled then
      ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;

    ScrollInfo.nMin := AState.nMin;
    ScrollInfo.nMax := AState.nMax;
    ScrollInfo.nPage := AState.nPage;
    ScrollInfo.nPos := AState.nPos;
    AutoVis := (AState.nMax > AState.nMin) and
               (AState.nPage <= (AState.nMax - AState.nMin + 1));

    if not HideEnabled then
      if not ScrollbarVisible then
        ShowScrollBar(FOwner.Handle, BarKind, True);

    // Avoid flicker when using HideEnabled option
    if HideEnabled and AutoVis then
      ApplyButtonState(AState);

    SetScrollInfo(FOwner.Handle, BarKind, ScrollInfo, True);

    // With not HideEnabled option enabling must happen after SetScrollInfo
    if not HideEnabled and AutoVis then
      ApplyButtonState(AState);
  end
  else
  begin
    // Clear the scroll info before hiding
    SetScrollInfo(FOwner.Handle, BarKind, ScrollInfo, True);
    ShowScrollBar(FOwner.Handle, BarKind, False);
  end;
end;

procedure TSynEditScrollBars.UpdateScrollBarsState;
var
  MaxScroll: Integer;
begin
  // Do Horz First
  FNewHorzSBState.Active :=
    (FOwner.ScrollBars in [TScrollStyle.ssBoth, TScrollStyle.ssHorizontal]) and
    (not FOwner.WordWrap or
     (FOwner.WordWrap and (eoWrapWithRightEdge in FOwner.Options) and
     (FOwner.WrapAreaWidth > FOwner.TextAreaWidth)));
  if FNewHorzSBState.Active then
  begin
    if FOwner.WordWrap and (eoWrapWithRightEdge in FOwner.Options) then
    begin
      MaxScroll := (FOwner.WrapAreaWidth div FOwner.CharWidth + 1);
    end
    else
    begin
      MaxScroll := (CeilOfIntDiv(TSynEditStringList(FOwner.Lines).MaxWidth,
        FOwner.CharWidth) + 1);
      if (eoScrollPastEol in FOwner.Options) or (FOwner.LeftChar > 1) then
        MaxScroll := Max(MaxScroll + 1,
          FOwner.LeftChar - 1 + GetHorzPageInChars);  // PastEOL adds 1 to MaxScroll.
    end;
    FNewHorzSBState.nMin := 1;
    FNewHorzSBState.nMax := MaxScroll;
    FNewHorzSBState.nPage := Max(1, GetHorzPageInChars);
    FNewHorzSBState.nPos := FOwner.LeftChar;
  end;

  // Now do Vert
  FNewVertSBState.Active := FOwner.ScrollBars in [TScrollStyle.ssBoth, TScrollStyle.ssVertical];
  if FNewVertSBState.Active then
  begin
    MaxScroll := FOwner.DisplayRowCount;
    if (eoScrollPastEof in FOwner.Options) then
      Inc(MaxScroll, FOwner.LinesInWindow - 1);
    FNewVertSBState.nMin := 1;
    FNewVertSBState.nMax := Max(1, MaxScroll);
    FNewVertSBState.nPage := FOwner.LinesInWindow;
    FNewVertSBState.nPos := FOwner.TopLine;
  end;
end;

function TSynEditScrollBars.UpdateScrollBars: Boolean;
begin
  // Update the scrollbars from the new state info but only if changed.
  if FIsScrolling then Exit(False);

  Result := False;
  if FOwner.ScrollBars = TScrollStyle.ssNone then
  begin
    ShowScrollBar(FOwner.Handle, SB_BOTH, False);
    Exit;
  end;
  UpdateScrollBarsState;
  if FNewHorzSBState <> FPrevHorzSBState then
  begin
    SetScrollBarFromState(FNewHorzSBState);
    FPrevHorzSBState := FNewHorzSBState;
    Result := True;
  end;
  if FNewVertSBState <> FPrevVertSBState then
  begin
    SetScrollBarFromState(FNewVertSBState);
    FPrevVertSBState := FNewVertSBState;
    Result := True;
  end;
end;

procedure TSynEditScrollBars.WMHScroll(var AMsg: TWMScroll);
var
  ScrollInfo: TScrollInfo;
begin
  AMsg.Result := 0;
  case AMsg.ScrollCode of
      // Scrolls to start / end of the line
    SB_LEFT: FOwner.LeftChar := 1;
    SB_RIGHT:
      // Simply set LeftChar property to MaxWidth/FCharWidth
      // it would do the range checking and constrain the value if necessary
      FOwner.LeftChar := CeilOfIntDiv(TSynEditStringList(FOwner.Lines).MaxWidth, FOwner.CharWidth);
      // Scrolls one char left / right
    SB_LINERIGHT: FOwner.LeftChar := FOwner.LeftChar + 1;
    SB_LINELEFT: FOwner.LeftChar := FOwner.LeftChar - 1;
      // Scrolls one page of chars left / right
    SB_PAGERIGHT: FOwner.LeftChar := FOwner.LeftChar
      + (GetHorzPageInChars - Ord(eoScrollByOneLess in FOwner.Options));
    SB_PAGELEFT: FOwner.LeftChar := FOwner.LeftChar
      - (GetHorzPageInChars - Ord(eoScrollByOneLess in FOwner.Options));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
    begin
      FIsScrolling := True;
      ScrollInfo := GetBarScrollInfo(sbHorizontal);
      FOwner.LeftChar := ScrollInfo.nTrackPos;
      SetScrollBarPos(sbHorizontal, ScrollInfo.nTrackPos, False);
    end;
    SB_ENDSCROLL:
    begin
      FIsScrolling := False;
      UpdateScrollBars;
    end;
  end;
  if Assigned(FOwner.OnScroll) then FOwner.OnScroll(Self, sbHorizontal);
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then
    ScrollHintWnd := HintWindowClass.Create(Application);
  Result := ScrollHintWnd;
end;

procedure TSynEditScrollBars.WMVScroll(var AMsg: TWMScroll);
var
  s: string;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
  ButtonH: Integer;
  ScrollInfo: TScrollInfo;
begin
  AMsg.Result := 0;
  case AMsg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP: FOwner.TopLine := 1;
    SB_BOTTOM: FOwner.TopLine := FOwner.DisplayRowCount;
      // Scrolls one line up / down
    SB_LINEDOWN: FOwner.TopLine := FOwner.TopLine + 1;
    SB_LINEUP: FOwner.TopLine := FOwner.TopLine - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: FOwner.TopLine := FOwner.TopLine
      + (FOwner.LinesInWindow - Ord(eoScrollByOneLess in FOwner.Options));
    SB_PAGEUP: FOwner.TopLine := FOwner.TopLine
      - (FOwner.LinesInWindow - Ord(eoScrollByOneLess in FOwner.Options));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        FIsScrolling := True;
        ScrollInfo := GetBarScrollInfo(sbVertical);
        FOwner.TopLine := ScrollInfo.nTrackPos;
        if eoShowScrollHint in FOwner.Options then
        begin
          ScrollHint := GetScrollHint;
          ScrollHint.Color := FOwner.ScrollHintColor;
          case FOwner.ScrollHintFormat of
            shfTopLineOnly:
              s := Format(SYNS_ScrollInfoFmtTop, [FOwner.RowToLine(FOwner.TopLine)]);
            else
              s := Format(SYNS_ScrollInfoFmt, [FOwner.RowToLine(FOwner.TopLine),
                FOwner.RowToLine(FOwner.TopLine + Min(FOwner.LinesInWindow, FOwner.DisplayRowCount - FOwner.TopLine))]);
          end;

          rc := ScrollHint.CalcHintRect(200, s, nil);
          if eoScrollHintFollows in FOwner.Options then
          begin
            ButtonH := GetSystemMetrics(SM_CYVSCROLL);
            pt := FOwner.ClientToScreen(Point(FOwner.ClientWidth - rc.Right - 4,
              ((rc.Bottom - rc.Top) shr 1) +                                    //half the size of the hint window
              Round((ScrollInfo.nTrackPos / ScrollInfo.nMax) *                  //The percentage of the page that has been scrolled
                    (FOwner.ClientHeight - (ButtonH * 2)))                      //The height minus the arrow buttons
                   + ButtonH));                                                 //The height of the top button
          end
          else
            pt := FOwner.ClientToScreen(Point(FOwner.ClientWidth - rc.Right - 4, 10));

          OffsetRect(rc, pt.x, pt.y);
          ScrollHint.ActivateHint(rc, s);
          ScrollHint.Update;
        end;
        SetScrollBarPos(sbVertical, ScrollInfo.nTrackPos, False);
      end;
      // Ends scrolling
    SB_ENDSCROLL:
      begin
        FIsScrolling := False;
        if eoShowScrollHint in FOwner.Options then
          ShowWindow(GetScrollHint.Handle, SW_HIDE);
        UpdateScrollBars;
      end;
  end;
  if Assigned(FOwner.OnScroll) then FOwner.OnScroll(Self,sbVertical);
end;

procedure TSynEditScrollBars.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint);
var
  WheelClicks: Integer;
  LinesToScroll: Integer;
  CharsToScroll: Integer;
begin
  // We are hard setting these but we could use Mouse.WheelScrollLines and
  // SPI_GETWHEELSCROLLCHARS to get user settings.
  LinesToScroll := 3;
  CharsToScroll := 3;
  if not (ssShift in Shift) and not (ssHorizontal in Shift) then
  begin
    // Vertical wheel scrolling
    if GetKeyState(SYNEDIT_CONTROL) < 0 then
      LinesToScroll := FOwner.LinesInWindow shr Ord(eoHalfPageScroll in FOwner.Options);
    Inc(FMouseWheelVertAccumulator, WheelDelta);
    WheelClicks := FMouseWheelVertAccumulator div WHEEL_DELTA;
    FMouseWheelVertAccumulator := FMouseWheelVertAccumulator mod WHEEL_DELTA;
    FOwner.TopLine := FOwner.TopLine - WheelClicks * LinesToScroll;
    if Assigned(FOwner.OnScroll) then FOwner.OnScroll(Self, sbVertical);
  end
  else
  begin
    // Horizontal wheel or tilt scrolling
    if GetKeyState(SYNEDIT_CONTROL) < 0 then
      CharsToScroll := GetHorzPageInChars shr Ord(eoHalfPageScroll in FOwner.Options);
    Inc(FMouseWheelHorzAccumulator, WheelDelta);
    WheelClicks := FMouseWheelHorzAccumulator div WHEEL_DELTA;
    FMouseWheelHorzAccumulator := FMouseWheelHorzAccumulator mod WHEEL_DELTA;
    FOwner.LeftChar := FOwner.LeftChar - WheelClicks * CharsToScroll;
    if Assigned(FOwner.OnScroll) then FOwner.OnScroll(Self, sbHorizontal);
  end;
end;

{ TSynScrollBarState }

class operator TSynScrollBarState.Equal(a, b: TSynScrollBarState): Boolean;
begin
  Result :=
    (a.Kind = b.Kind) and
    (a.Active = b.Active) and
    (a.nMin = b.nMin) and
    (a.nMax = b.nMax) and
    (a.nPage = b.nPage) and
    (a.nPos = b.nPos);
end;

class operator TSynScrollBarState.NotEqual(a, b: TSynScrollBarState): Boolean;
begin
  Result := not (a = b);
end;

{ Factory Method}

function CreateSynEditScrollBars(Editor: TCustomControl): ISynEditScrollBars;
begin
  if (Editor is TCustomSynEdit) then
    Result := TSynEditScrollBars.Create(TCustomSynEdit(Editor))
  else
    raise Exception.Create('SynEditScrollBars will only work with SynEdit.');
end;

end.
