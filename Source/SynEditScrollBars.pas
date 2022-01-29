{ -------------------------------------------------------------------------------
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

  Known Issues:
  ------------------------------------------------------------------------------- }

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
  System.Math,
  Vcl.Forms,
  SynEdit,
  SynEditTextBuffer,
  SynEditMiscProcs;

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
    FPrevHorzSBState: TSynScrollBarState;  // Last applied horizontal scrollbar state
    FPrevVertSBState: TSynScrollBarState;  // Last applied vertical scrollbar state
    FNewHorzSBState: TSynScrollBarState;   // New Horizontal ScrollBar state
    FNewVertSBState: TSynScrollBarState;   // New Vertical ScrollBar state
    function GetRealScrollInfo(AKind: TScrollBarKind): TScrollInfo;
    procedure SetScrollBarFromState(const AState: TSynScrollBarState);
    procedure ApplyButtonState(const AState: TSynScrollBarState);
    procedure UpdateScrollBarsState;
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    function GetVertScrollInfo: TScrollInfo;
    function GetHorzScrollInfo: TScrollInfo;
    function UpdateScrollBars: Boolean;
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

function TSynEditScrollBars.GetRealScrollInfo(AKind: TScrollBarKind): TScrollInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(Result);
  Result.fMask := SIF_ALL;
  if AKind = sbHorizontal then
    GetScrollInfo(FOwner.Handle, SB_HORZ, Result)
  else
    GetScrollInfo(FOwner.Handle, SB_VERT, Result);
end;

function TSynEditScrollBars.GetHorzScrollInfo: TScrollInfo;
begin
  Result := GetRealScrollInfo(sbHorizontal);
end;

function TSynEditScrollBars.GetVertScrollInfo: TScrollInfo;
begin
  Result := GetRealScrollInfo(sbVertical);
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
  Btn1Enabled := (AState.nPos > AState.nMin);
  Btn2Enabled := ((AState.nPos + AState.nPage - AState.nMin) < AState.nMax);
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

procedure TSynEditScrollBars.SetScrollBarFromState(const AState: TSynScrollBarState);
var
  WindowStyle: NativeInt;
  ScrollInfo: TScrollInfo;
  AutoVis: Boolean;
  BarKind: Integer;
  BarWSCode: Integer;
  ScrollbarVisible: Boolean;
  HideEnabled: Boolean;
  DisableArrows: Boolean;
begin
  WindowStyle := GetWindowLong(FOwner.Handle, GWL_STYLE);
  HideEnabled := (eoHideShowScrollbars in FOwner.Options);
  DisableArrows := (eoDisableScrollArrows in FOwner.Options);
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
  if AState.Active then
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    if HideEnabled then
      ScrollInfo.fMask := SIF_ALL
    else
      ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;

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
    if DisableArrows and AutoVis and HideEnabled then
      ApplyButtonState(AState);

    SetScrollInfo(FOwner.Handle, BarKind, ScrollInfo, True);

    // With not HideEnabled option enabling must happen after SetScrollInfo
    if DisableArrows and AutoVis and not HideEnabled then
      ApplyButtonState(AState);
  end
  else
    ShowScrollBar(FOwner.Handle, BarKind, False);
end;

procedure TSynEditScrollBars.UpdateScrollBarsState;
var
  nMaxScroll: Integer;
  nMaxPage: Integer;
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
      // This may have to be adjusted.
      nMaxPage := FOwner.TextAreaWidth;
      FNewHorzSBState.nMin := 0;
      FNewHorzSBState.nMax := FOwner.WrapAreaWidth + FOwner.CharWidth;;
      FNewHorzSBState.nPage := nMaxPage;
      FNewHorzSBState.nPos := ((FOwner.LeftChar -1) * FOwner.CharWidth);
    end
    else
    begin
      FNewHorzSBState.nMin := 0;
      FNewHorzSBState.nMax := TSynEditStringList(FOwner.Lines).MaxWidth + FOwner.CharWidth;
      FNewHorzSBState.nPage := FOwner.TextAreaWidth;
      FNewHorzSBState.nPos := ((FOwner.LeftChar - 1) * FOwner.CharWidth);
    end;
  end;

  // Now do Vert
  FNewVertSBState.Active := FOwner.ScrollBars in [TScrollStyle.ssBoth, TScrollStyle.ssVertical];
  if FNewVertSBState.Active then
  begin
    nMaxScroll := FOwner.DisplayLineCount;
    if (eoScrollPastEof in FOwner.Options) then
      Inc(nMaxScroll, FOwner.LinesInWindow - 1);
    FNewVertSBState.nMin := 1;
    FNewVertSBState.nMax := Max(1, nMaxScroll);
    FNewVertSBState.nPage := FOwner.LinesInWindow;
    FNewVertSBState.nPos := FOwner.TopLine;
  end;
end;

function TSynEditScrollBars.UpdateScrollBars: Boolean;
begin
  // Update the scrollbars from the new state info but only if changed.
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
