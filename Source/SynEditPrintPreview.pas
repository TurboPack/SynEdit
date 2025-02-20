{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintPreview.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
Portions written by Michael Hieke are copyright 2000 Michael Hieke.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

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
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Print preview component. Allmost identical to code developed by Michael Hieke.
  It is important to call UpdatePreview whenever things change (i.e. just
  before the preview is shown, and when the printer is changed)
-------------------------------------------------------------------------------}

unit SynEditPrintPreview;

{$I SynEdit.inc}

{$M+}
interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.Classes,
  System.SysUtils,
  Vcl.Themes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
  SynEditPrint;

type
//Event raised when page is changed in preview
  TPreviewPageEvent = procedure(Sender: TObject; PageNumber: Integer) of object;
  TSynPreviewScale = (pscWholePage, pscPageWidth, pscUserScaled);

  TSynEditPrintPreview = class(TCustomControl)
  private
    FPaperRect: TRect;
  protected
    FBorderStyle: TBorderStyle;
    FSynEditPrint: TSynEditPrint;
    FScaleMode: TSynPreviewScale;
    FScalePercent: Integer;
    // these are in pixels ( = screen device units)
    FVirtualSize: TSize;
    FVirtualOffset: TPoint;
    FPageSize: TSize;
    FScrollPosition: TPoint;
    FPageBG: TColor;
    FPageNumber: Integer;
    FShowScrollHint: Boolean;
    FOnPreviewPage: TPreviewPageEvent;
    FOnScaleChange: TNotifyEvent;
    FWheelVAccumulator: Integer;
    FWheelHAccumulator: Integer;
    FMargin_X: Integer;
    FMargin_Y: Integer;
    FShadow_Size: Integer;

    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetPageBG(Value: TColor);
    procedure SetSynEditPrint(Value: TSynEditPrint);
    procedure SetScaleMode(Value: TSynPreviewScale);
    procedure SetScalePercent(Value: Integer);
  private
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMMouseHWheel(var Message: TWMMouseWheel); message WM_MOUSEHWHEEL;
    procedure PaintPaper;
    function GetPageCount: Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPageHeightFromWidth(AWidth: Integer): Integer;
    function GetPageHeight100Percent: Integer;
    function GetPageWidthFromHeight(AHeight: Integer): Integer;
    function GetPageWidth100Percent: Integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ScrollHorzFor(Value: Integer);
    procedure ScrollHorzTo(Value: Integer); virtual;
    procedure ScrollVertFor(Value: Integer);
    procedure ScrollVertTo(Value: Integer); virtual;
    procedure UpdateScrollbars; virtual;
    procedure SizeChanged; virtual;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure UpdatePreview;
    procedure NextPage;
    procedure PreviousPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Print;
    property PageNumber: Integer read FPageNumber;
    property PageCount: Integer read GetPageCount;
  published
    property Align default alClient;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property Color default clAppWorkspace;
    property Cursor;
    property PageBGColor: TColor read FPageBG write SetPageBG default clWhite;
    property PopupMenu;
    property SynEditPrint: TSynEditPrint read FSynEditPrint
      write SetSynEditPrint;
    property ScaleMode: TSynPreviewScale read FScaleMode write SetScaleMode
      default pscUserScaled;
    property ScalePercent: Integer read FScalePercent write SetScalePercent
      default 100;
    property Visible default True;
    property ShowScrollHint: Boolean read FShowScrollHint write FShowScrollHint
      default True;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnPreviewPage: TPreviewPageEvent read FOnPreviewPage
      write FOnPreviewPage;
    property OnScaleChange: TNotifyEvent read FOnScaleChange
      write FOnScaleChange;
  end;

implementation

uses
  Winapi.D2D1,
  SynDWrite,
  SynEditStrConst;

const
  MARGIN_X = 12; // margin width left and right of page
  MARGIN_Y = 12; // margin height above and below page
  SHADOW_SIZE = 2; // page shadow width

{ TSynEditPrintPreview }

procedure TSynEditPrintPreview.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  if isDpiChange then
  begin
    FMargin_X := MulDiv(FMargin_X, M, D);
    FMargin_Y := MulDiv(FMargin_Y, M, D);
    FShadow_Size := MulDiv(FShadow_Size, M, D);
  end;
  inherited ChangeScale(M, D, isDpiChange);
end;

constructor TSynEditPrintPreview.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csNeedsBorderPaint];
  FBorderStyle := bsSingle;
  FScaleMode := pscUserScaled;
  FScalePercent := 100;
  FPageBG := clWhite;
  Width := 200;
  Height := 120;
  ParentColor := False;
  Color := clAppWorkspace;
  Visible := True;
  FPageNumber := 1;
  FShowScrollHint := True;
  Align := alClient;
  FWheelVAccumulator := 0;
  FWheelHAccumulator := 0;
  FMargin_X:= MARGIN_X;
  FMargin_Y:= MARGIN_Y;
  FShadow_Size:= SHADOW_SIZE;
end;

procedure TSynEditPrintPreview.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWord = (0, WS_BORDER);
begin
  inherited;
  with Params do begin
    Style := Style or WS_HSCROLL or WS_VSCROLL or BorderStyles[FBorderStyle]
      or WS_CLIPCHILDREN;
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

function TSynEditPrintPreview.GetPageHeightFromWidth(AWidth: Integer): Integer;
begin
  if Assigned(FSynEditPrint) then begin
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(AWidth, PhysicalHeight, PhysicalWidth);
  end
  else
    Result := MulDiv(AWidth, 141, 100); // fake A4 size
end;

function TSynEditPrintPreview.GetPageWidthFromHeight(AHeight: Integer): Integer;
begin
  if Assigned(FSynEditPrint) then begin
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(AHeight, PhysicalWidth, PhysicalHeight);
  end
  else
    Result := MulDiv(AHeight, 100, 141); // fake A4 size
end;

function TSynEditPrintPreview.GetPageHeight100Percent: Integer;
begin
  Result := 0;
  if Assigned(FSynEditPrint) then
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(PhysicalHeight, Screen.PixelsPerInch, YPixPrInch);
end;

function TSynEditPrintPreview.GetPageWidth100Percent: Integer;
begin
  Result := 0;
  if Assigned(FSynEditPrint) then
    with FSynEditPrint.PrinterInfo do
      Result := MulDiv(PhysicalWidth, Screen.PixelsPerInch, XPixPrInch);
end;

procedure TSynEditPrintPreview.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSynEditPrint) then
    SynEditPrint := nil;
end;

procedure TSynEditPrintPreview.PaintPaper;
var
  rcClip: TRect;
  rgnPaper: HRGN;
  i: Integer;
begin
  with Canvas do begin
      // we work in MM_TEXT mapping mode here...
    rcClip := ClipRect;
    if IsRectEmpty(rcClip) then Exit;
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    if (csDesigning in ComponentState) or (not Assigned(FSynEditPrint)) then begin
      FillRect(rcClip);
      Brush.Color := FPageBG;
      Rectangle(FMargin_X, FMargin_Y, FMargin_X + 30, FMargin_Y + 43);
      Exit;
    end;
      // fill background around paper
    with FPaperRect do begin
      Left := FVirtualOffset.X + FScrollPosition.X;
      if ScaleMode = pscWholePage then
        Top := FVirtualOffset.Y
      else
        Top := FVirtualOffset.Y + FScrollPosition.Y;
      Right := Left + FPageSize.Width;
      Bottom := Top + FPageSize.Height;
      rgnPaper := CreateRectRgn(Left, Top, Right, Bottom);
    end;
    if (NULLREGION <> ExtSelectClipRgn(Handle, rgnPaper, RGN_DIFF)) then
      FillRect(rcClip);
      // paper shadow
    Brush.Color := clDkGray;
    with FPaperRect do begin
      for i := 1 to FShadow_Size do
        PolyLine([Point(Left + i, Bottom + i), Point(Right + i, Bottom + i),
          Point(Right + i, Top + i)]);
    end;
      // paint paper background
    SelectClipRgn(Handle, rgnPaper);
    DeleteObject(rgnPaper);
  end;
end;

procedure TSynEditPrintPreview.Paint;
begin
  PaintPaper;
  if (csDesigning in ComponentState) or (not Assigned(FSynEditPrint)) then
    Exit;

  FSynEditPrint.PrintToCanvas(Canvas, FPaperRect, Canvas.ClipRect, FPageNumber);
end;

procedure TSynEditPrintPreview.ScrollHorzFor(Value: Integer);
begin
  ScrollHorzTo(FScrollPosition.X + Value);
end;

procedure TSynEditPrintPreview.ScrollHorzTo(Value: Integer);
var
  nW, n: Integer;
begin
  nW := ClientWidth;
  n := nW - FVirtualSize.Width;
  if (Value < n) then Value := n;
  if (Value > 0) then Value := 0;
  if (Value <> FScrollPosition.X) then
  begin
    n := Value - FScrollPosition.X;
    FScrollPosition.X := Value;
    UpdateScrollbars;
    if (Abs(n) > nW div 2) then
      Invalidate
    else
    begin
      ScrollWindow(Handle, n, 0, nil, nil);
      Update;
    end;
  end;
end;

procedure TSynEditPrintPreview.ScrollVertFor(Value: Integer);
begin
  ScrollVertTo(FScrollPosition.Y + Value);
end;

procedure TSynEditPrintPreview.ScrollVertTo(Value: Integer);
var
  nH, n: Integer;
begin
  nH := ClientHeight;
  n := nH - FVirtualSize.Height;
  if (Value < n) then Value := n;
  if (Value > 0) then Value := 0;
  if (Value <> FScrollPosition.Y) then
  begin
    n := Value - FScrollPosition.Y;
    FScrollPosition.Y := Value;
    UpdateScrollbars;
    if (Abs(n) > nH div 2) then
      Invalidate
    else
    begin
      ScrollWindow(Handle, 0, n, nil, nil);
      Update;
    end;
  end;
end;

procedure TSynEditPrintPreview.SizeChanged;
var
  nWDef: Integer;
begin
  if not (HandleAllocated and Assigned(FSynEditPrint)) then Exit;
  // compute paper size
  case fScaleMode of
    pscWholePage: begin
        FPageSize.Width := ClientWidth - 2 * FMargin_X - FShadow_Size;
        FPageSize.Height := ClientHeight - 2 * FMargin_Y - FShadow_Size;
        nWDef := GetPageWidthFromHeight(FPageSize.Height);
        if (nWDef < FPageSize.Width) then
          FPageSize.Width := nWDef
        else
          FPageSize.Height := GetPageHeightFromWidth(FPageSize.Width);
      end;
    pscPageWidth: begin
        FPageSize.Width := ClientWidth - 2 * FMargin_X - FShadow_Size;
        FPageSize.Height := GetPageHeightFromWidth(FPageSize.Width);
      end;
    pscUserScaled: begin
        FPageSize.Width := MulDiv(GetPageWidth100Percent, fScalePercent, 100);
        FPageSize.Height := MulDiv(GetPageHeight100Percent, fScalePercent, 100);
      end;
  end;
  FVirtualSize.Width := FPageSize.Width + 2 * FMargin_X + FShadow_Size;
  FVirtualSize.Height := FPageSize.Height + 2 * FMargin_Y + FShadow_Size;
  FVirtualOffset.X := FMargin_X;
  if (FVirtualSize.Width < ClientWidth) then
    Inc(FVirtualOffset.X, (ClientWidth - FVirtualSize.Width) div 2);
  FVirtualOffset.Y := FMargin_Y;
  if (FVirtualSize.Height < ClientHeight) then
    Inc(FVirtualOffset.Y, (ClientHeight - FVirtualSize.Height) div 2);
  UpdateScrollbars;
// TODO
  FScrollPosition.X := 0;
  FScrollPosition.Y := 0;
end;


procedure TSynEditPrintPreview.UpdateScrollbars;
var
  si: TScrollInfo;
begin
  FillChar(si, SizeOf(TScrollInfo), 0);
  si.cbSize := SizeOf(TScrollInfo);
  si.fMask := SIF_ALL;
  case FScaleMode of
    pscWholePage: begin
        // hide horizontal scrollbar
        ShowScrollbar(Handle, SB_HORZ, False);
        // show vertical scrollbar, enable if more than one page
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        si.nMin := 1;
        if Assigned(FSynEditPrint) then begin
          si.nMax := FSynEditPrint.PageCount;
          si.nPos := FPageNumber;
        end
        else begin
          si.nMax := 1;
          si.nPos := 1;
        end;
        si.nPage := 1;
        SetScrollInfo(Handle, SB_VERT, si, True);
      end;
    pscPageWidth: begin
        // hide horizontal scrollbar
        ShowScrollbar(Handle, SB_HORZ, False);
        // show vertical scrollbar
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        si.nMax := FVirtualSize.Height;
        si.nPos := -FScrollPosition.Y;
        si.nPage := ClientHeight;
        SetScrollInfo(Handle, SB_VERT, si, True);
      end;
    pscUserScaled: begin
        ShowScrollbar(Handle, SB_HORZ, True);
        ShowScrollbar(Handle, SB_VERT, True);
        si.fMask := si.fMask or SIF_DISABLENOSCROLL;
        // show horizontal scrollbar
        si.nMax := FVirtualSize.Width;
        si.nPos := -FScrollPosition.X;
        si.nPage := ClientWidth;
        SetScrollInfo(Handle, SB_HORZ, si, True);
        // show vertical scrollbar
        si.nMax := FVirtualSize.Height;
        si.nPos := -FScrollPosition.Y;
        si.nPage := ClientHeight;
        SetScrollInfo(Handle, SB_VERT, si, True);
      end;
  end;
end;

procedure TSynEditPrintPreview.SetBorderStyle(Value: TBorderStyle);
begin
  if (Value <> FBorderStyle) then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TSynEditPrintPreview.SetPageBG(Value: TColor);
begin
  if (FPageBG <> Value) then
  begin
    FPageBG := Value;
    Invalidate;
  end;
end;

procedure TSynEditPrintPreview.SetSynEditPrint(Value: TSynEditPrint);
begin
  if (FSynEditPrint <> Value) then
  begin
    FSynEditPrint := Value;
    if Assigned(FSynEditPrint) then
      FSynEditPrint.FreeNotification(Self);
  end;
end;

procedure TSynEditPrintPreview.SetScaleMode(Value: TSynPreviewScale);
begin
  if (FScaleMode <> Value) then begin
    FScaleMode := Value;
    FScrollPosition := Point(0, 0);
    SizeChanged;
    if Assigned(FOnScaleChange) then
      FOnScaleChange(Self);
    Invalidate;
  end;
end;

procedure TSynEditPrintPreview.SetScalePercent(Value: Integer);
begin
  if (FScalePercent <> Value) then begin
    FScaleMode := pscUserScaled;
    FScrollPosition := Point(0, 0);
    FScalePercent := Value;
    SizeChanged;
    Invalidate;
  end else
    ScaleMode := pscUserScaled;
  if Assigned(FOnScaleChange) then
    FOnScaleChange(Self);
end;

procedure TSynEditPrintPreview.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TSynEditPrintPreview.WMHScroll(var Msg: TWMHScroll);
var
  nW: Integer;
begin
  if (FScaleMode <> pscWholePage) then begin
    nW := ClientWidth;
    case Msg.ScrollCode of
      SB_TOP: ScrollHorzTo(0);
      SB_BOTTOM: ScrollHorzTo(-FVirtualSize.Width);
      SB_LINEDOWN: ScrollHorzFor(-(nW div 10));
      SB_LINEUP: ScrollHorzFor(nW div 10);
      SB_PAGEDOWN: ScrollHorzFor(-(nW div 2));
      SB_PAGEUP: ScrollHorzFor(nW div 2);
      SB_THUMBPOSITION, SB_THUMBTRACK: ScrollHorzTo(-Msg.Pos);
    end;
  end;
end;

procedure TSynEditPrintPreview.WMSize(var Msg: TWMSize);
begin
  inherited;
  if not (csDesigning in ComponentState) then SizeChanged;
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Visible := False;
  end;
  Result := ScrollHintWnd;
end;

procedure TSynEditPrintPreview.WMVScroll(var Msg: TWMVScroll);
var
  nH: Integer;
  s: string;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
begin
  if (FScaleMode = pscWholePage) then begin
    if Assigned(FSynEditPrint) then
      case Msg.ScrollCode of
        SB_TOP: FPageNumber := 1;
        SB_BOTTOM: FPageNumber := FSynEditPrint.PageCount;
        SB_LINEDOWN, SB_PAGEDOWN: begin
            FPageNumber := FPageNumber + 1;
            if FPageNumber > FSynEditPrint.PageCount then
              FPageNumber := FSynEditPrint.PageCount;
          end;
        SB_LINEUP, SB_PAGEUP: begin
            FPageNumber := FPageNumber - 1;
            if FPageNumber < 1 then
              FPageNumber := 1;
          end;
        SB_THUMBPOSITION, SB_THUMBTRACK: begin
            FPageNumber := Msg.Pos;
              //Showing hint window - principle copied from SynEdit.pas
            if FShowScrollHint then begin
              ScrollHint := GetScrollHint;
              if not ScrollHint.Visible then begin
                ScrollHint.Color := Application.HintColor;
                ScrollHint.Visible := True;
              end;
              s := Format(SYNS_PreviewScrollInfoFmt, [FPageNumber]);
              rc := ScrollHint.CalcHintRect(200, s, nil);
              pt := ClientToScreen(Point(ClientWidth - rc.Right - 4, 10));
              OffsetRect(rc, pt.x, pt.y);
              ScrollHint.ActivateHint(rc, s);
              SendMessage(ScrollHint.Handle, WM_NCPAINT, 1, 0);
              ScrollHint.Update;
            end;
          end;
        SB_ENDSCROLL: begin
            if FShowScrollHint then
            begin
              ScrollHint := GetScrollHint;
              ScrollHint.Visible := False;
              ShowWindow(ScrollHint.Handle, SW_HIDE);
            end;
          end;
      end;
      {Updating scroll position and redrawing}
    FScrollPosition.Y := -(FPageNumber - 1);
    UpdateScrollbars;
    if Assigned(FOnPreviewPage) then
      FOnPreviewPage(Self, FPageNumber);
    Invalidate;
  end
  else begin
    nH := ClientHeight;
    case Msg.ScrollCode of
      SB_TOP: ScrollVertTo(0);
      SB_BOTTOM: ScrollVertTo(-FVirtualSize.Height);
      SB_LINEDOWN: ScrollVertFor(-(nH div 10));
      SB_LINEUP: ScrollVertFor(nH div 10);
      SB_PAGEDOWN: ScrollVertFor(-(nH div 2));
      SB_PAGEUP: ScrollVertFor(nH div 2);
      SB_THUMBPOSITION, SB_THUMBTRACK: ScrollVertTo(-Msg.Pos);
    end;
  end;
end;

// Shared mousewheel function called by both vertical and horizontal wheel message handlers.
function TSynEditPrintPreview.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  // Note that we do not call inherited as we do all handling here.
  Result := False;
  if Assigned(OnMouseWheel) then
    OnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    if not (ssHorizontal in Shift) then
    begin
      // Vertical Mouse Wheel
      Inc(FWheelVAccumulator, WheelDelta);
      while Abs(FWheelVAccumulator) >= WHEEL_DELTA do
      begin
        IsNeg := FWheelVAccumulator < 0;
        FWheelVAccumulator := Abs(FWheelVAccumulator) - WHEEL_DELTA;
        if IsNeg then
        begin
          // Vertical Mouse Wheel Down;
          if FWheelVAccumulator <> 0 then FWheelVAccumulator := -FWheelVAccumulator;
          Result := DoMouseWheelDown(Shift, MousePos);
          if not Result then
          begin
            if (ssCtrl in Shift) and (fPageNumber < PageCount) then
              NextPage
            else
              ScrollVertFor(-WHEEL_DELTA);
            Result := True;
          end;
        end
        else
        begin
          // Vertical Mouse Wheel Up;
          Result := DoMouseWheelUp(Shift, MousePos);
          if not Result then
          begin
            if (ssCtrl in Shift) and (fPageNumber > 1) then
              PreviousPage
            else
              ScrollVertFor(WHEEL_DELTA);
            Result := True;
          end;
        end;
      end;
    end
    else
    begin
      // Horizontal Mouse Wheel
      Inc(FWheelHAccumulator, WheelDelta);
      while Abs(FWheelHAccumulator) >= WHEEL_DELTA do
      begin
        IsNeg := FWheelHAccumulator < 0;
        FWheelHAccumulator := Abs(FWheelHAccumulator) - WHEEL_DELTA;
        if IsNeg then
        begin
          // Horizontal Mouse Wheel Down/Right;
          if FWheelHAccumulator <> 0 then FWheelHAccumulator := -FWheelHAccumulator;
          ScrollHorzFor(-WHEEL_DELTA);
        end
        else
        begin
          // Horizontal Mouse Wheel Up/Left;
          ScrollHorzFor(WHEEL_DELTA);
        end;
        Result := True;
      end;
    end;
  end;
end;

procedure TSynEditPrintPreview.WMMouseWheel(var Message: TWMMouseWheel);
var
  Shift: TShiftState;
  WheelDelta: SmallInt;
  MousePos: TSmallPoint;
begin
  Shift := KeysToShiftState(Message.Keys);
  // Shift-MouseWheel causes horizonal scrolling.  This is a semi-standard
  //   used in several products including VsCode and Edge.
  if ssShift in Shift then
    Include(Shift, System.Classes.ssHorizontal);
  WheelDelta := Message.WheelDelta;
  MousePos := Message.Pos;
  if DoMouseWheel(Shift, WheelDelta, MousePos) then
    Message.Result := 1;
end;

procedure TSynEditPrintPreview.WMMouseHWheel(var Message: TWMMouseWheel);
var
  Shift: TShiftState;
  WheelDelta: SmallInt;
  MousePos: TSmallPoint;
begin
  Shift := KeysToShiftState(Message.Keys);
  Include(Shift, System.Classes.ssHorizontal);
  // HWheel directions are reversed from Wheel - retest
  WheelDelta := - Message.WheelDelta;
  MousePos := Message.Pos;
  if DoMouseWheel(Shift, WheelDelta, MousePos) then
    Message.Result := 1;
end;

procedure TSynEditPrintPreview.UpdatePreview;
var
  OldScale: Integer;
  OldMode: TSynPreviewScale;
begin
  OldScale := ScalePercent;
  OldMode := ScaleMode;
  ScalePercent := 100;
  if Assigned(FSynEditPrint) then
    FSynEditPrint.InitPrint;
  SizeChanged;
  Invalidate;
  ScaleMode := OldMode;
  if ScaleMode = pscUserScaled then
    ScalePercent := OldScale;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
end;

procedure TSynEditPrintPreview.FirstPage;
begin
  FPageNumber := 1;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.LastPage;
begin
  if Assigned(FSynEditPrint) then
    FPageNumber := FSynEditPrint.PageCount;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.NextPage;
begin
  FPageNumber := FPageNumber + 1;
  if Assigned(FSynEditPrint) and (FPageNumber > FSynEditPrint.PageCount) then
    FPageNumber := FSynEditPrint.PageCount;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.PreviousPage;
begin
  FPageNumber := FPageNumber - 1;
  if Assigned(FSynEditPrint) and (FPageNumber < 1) then
    FPageNumber := 1;
  if Assigned(FOnPreviewPage) then
    FOnPreviewPage(Self, FPageNumber);
  Invalidate;
end;

procedure TSynEditPrintPreview.Print;
begin
  if Assigned(FSynEditPrint) then begin
    FSynEditPrint.Print;
    UpdatePreview;
  end;
end;

function TSynEditPrintPreview.GetPageCount: Integer;
begin
  Result := SynEditPrint.PageCount;
end;

initialization
 TCustomStyleEngine.RegisterStyleHook(TSynEditPrintPreview, TScrollingStyleHook);

finalization
 TCustomStyleEngine.UnRegisterStyleHook(TSynEditPrintPreview, TScrollingStyleHook);

end.
