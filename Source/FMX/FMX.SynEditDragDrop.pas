{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.

FMX drag-and-drop platform abstraction. Provides ISynDragDropPlatform
interface with platform-specific implementations.
Implements Windows OLE drag source and drop target. The custom OLE
IDropTarget works around an FMX framework bug where TWinDropTarget.Drop
fails to set dwEffect, causing drag sources to never see DROPEFFECT_MOVE.
-------------------------------------------------------------------------------}

unit FMX.SynEditDragDrop;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Controls,
  SynEditTypes,
  SynEditHighlighter,
  SynEditDragDropShared;

type
  TSynDragData = record
    Text: string;
    Highlighter: TSynCustomHighlighter;
    FontFamily: string;
    FontSize: Integer;
    BackgroundColor: TAlphaColor;
    UseBackground: Boolean;
  end;

  TSynDragResult = record
    Effect: TSynDropAction;
  end;

  ISynDragDropPlatform = interface
    ['{A1B2C3D4-E5F6-4789-ABCD-EF0123456789}']
    function StartDrag(AControl: TControl;
      const AData: TSynDragData): TSynDragResult;
  end;

function CreateSynDragDropPlatform: ISynDragDropPlatform;

/// Register a fixed OLE drop target on the control's form HWND.
/// Works around an FMX framework bug where TWinDropTarget.Drop does not
/// set dwEffect, so external drag sources (VCL, Notepad) never see
/// DROPEFFECT_MOVE. The replacement delegates to the same Form.DragOver /
/// Form.DragDrop calls FMX uses, but correctly reports dwEffect.
/// Returns an opaque token; release it to unregister.
function RegisterOleDropTarget(AControl: TControl): IInterface;
procedure UnregisterOleDropTarget(var AToken: IInterface);

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Generics.Collections,
  FMX.Types,
  FMX.Forms,
  FMX.Platform.Win,
  SynEditDragDropWin;

type
  /// Fixed OLE IDropTarget that replaces FMX's built-in TWinDropTarget.
  /// Delegates to Form.DragOver / Form.DragDrop (same as FMX) but correctly
  /// reports dwEffect in the Drop method.
  TSynFixedDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FForm: TCommonCustomForm;
    FDataObj: IDataObject;
    FLastEffect: Longint;
    function GetDataObject: TDragObject;
    // IDropTarget
    function DragEnter(const DataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const DataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  public
    constructor Create(AForm: TCommonCustomForm);
  end;

  /// Token that unregisters on release.
  TSynDropTargetToken = class(TInterfacedObject)
  private
    FHWnd: HWND;
  public
    constructor Create(AHWnd: HWND);
    destructor Destroy; override;
  end;

  /// Simple IDataObject for FMX drag: CF_UNICODETEXT + optional HTML fragment.
  /// Uses building blocks from SynEditDragDropWin.
  TSynFMXTextDataObject = class(TInterfacedObject, IDataObject)
  private
    FText: string;
    FHtmlData: TBytes;
    FFormats: TList<TClipFormat>;
  public
    constructor Create(const AData: TSynDragData);
    destructor Destroy; override;
    // IDataObject
    function GetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium): HResult; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc;
      out Medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
      out FormatEtcOut: TFormatEtc): HResult; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint;
      out Enum: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const FormatEtc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink;
      out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(
      out EnumAdvise: IEnumStatData): HResult; stdcall;
  end;

  TSynWinDragDropPlatform = class(TInterfacedObject, ISynDragDropPlatform)
  public
    function StartDrag(AControl: TControl;
      const AData: TSynDragData): TSynDragResult;
  end;
{$ENDIF}

function CreateSynDragDropPlatform: ISynDragDropPlatform;
begin
  {$IFDEF MSWINDOWS}
  Result := TSynWinDragDropPlatform.Create;
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}

{ TSynWinDragDropPlatform }

function TSynWinDragDropPlatform.StartDrag(AControl: TControl;
  const AData: TSynDragData): TSynDragResult;
var
  DataObj: IDataObject;
  DragSrc: IDropSource;
  dwEffect: Longint;
begin
  DataObj := TSynFMXTextDataObject.Create(AData);
  DragSrc := TSynDragSource.Create;
  if DoDragDrop(DataObj, DragSrc,
    DROPEFFECT_COPY or DROPEFFECT_MOVE, dwEffect) = DRAGDROP_S_DROP then
    case dwEffect of
      DROPEFFECT_MOVE: Result.Effect := sdaMove;
      DROPEFFECT_COPY: Result.Effect := sdaCopy;
    else
      Result.Effect := sdaNone;
    end
  else
    Result.Effect := sdaNone;
end;

{ TSynFMXTextDataObject }

constructor TSynFMXTextDataObject.Create(const AData: TSynDragData);
begin
  inherited Create;
  FFormats := TList<TClipFormat>.Create;
  FText := AData.Text;
  FFormats.Add(CF_UNICODETEXT);
  if Assigned(AData.Highlighter) and (FText <> '') then
  begin
    FHtmlData := GenerateHTMLFragment(AData.Text, AData.Highlighter,
      AData.FontFamily, AData.FontSize, AData.BackgroundColor,
      AData.UseBackground);
    if Length(FHtmlData) > 0 then
      FFormats.Add(HTMLClipboardFormat);
  end;
end;

destructor TSynFMXTextDataObject.Destroy;
begin
  FFormats.Free;
  inherited;
end;

function TSynFMXTextDataObject.GetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HResult;
begin
  ZeroMemory(@Medium, SizeOf(TStgMedium));
  Result := QueryGetData(FormatEtcIn);
  if Result = S_OK then
  try
    Medium.tymed := TYMED_HGLOBAL;
    if FormatEtcIn.cfFormat = CF_UNICODETEXT then
      Medium.hGlobal := MakeGlobal(FText)
    else if (FormatEtcIn.cfFormat = HTMLClipboardFormat)
      and (Length(FHtmlData) > 0) then
      Medium.hGlobal := MakeGlobal(FHtmlData[0], Length(FHtmlData));
  except
    Result := E_UNEXPECTED;
  end;
end;

function TSynFMXTextDataObject.GetDataHere(const FormatEtc: TFormatEtc;
  out Medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSynFMXTextDataObject.QueryGetData(
  const FormatEtc: TFormatEtc): HResult;
begin
  if (FormatEtc.tymed and TYMED_HGLOBAL = TYMED_HGLOBAL)
    and FFormats.Contains(FormatEtc.cfFormat) then
    Result := S_OK
  else
    Result := DV_E_FORMATETC;
end;

function TSynFMXTextDataObject.GetCanonicalFormatEtc(
  const FormatEtc: TFormatEtc; out FormatEtcOut: TFormatEtc): HResult;
begin
  FormatEtcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
end;

function TSynFMXTextDataObject.SetData(const FormatEtc: TFormatEtc;
  var Medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSynFMXTextDataObject.EnumFormatEtc(dwDirection: Longint;
  out Enum: IEnumFormatEtc): HResult;
begin
  try
    if dwDirection = DATADIR_GET then
    begin
      Enum := TSynEnumFormatEtc.Create(FFormats.ToArray);
      Result := S_OK;
    end
    else
      Result := E_NOTIMPL;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TSynFMXTextDataObject.DAdvise(const FormatEtc: TFormatEtc;
  advf: Longint; const advSink: IAdviseSink;
  out dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSynFMXTextDataObject.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSynFMXTextDataObject.EnumDAdvise(
  out EnumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

{ TSynFixedDropTarget }

constructor TSynFixedDropTarget.Create(AForm: TCommonCustomForm);
begin
  inherited Create;
  FForm := AForm;
end;

function TSynFixedDropTarget.GetDataObject: TDragObject;
var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  S: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FDataObj = nil then Exit;

  // Extract CF_UNICODETEXT (same priority as FMX's TWinDropTarget)
  FormatEtc.cfFormat := CF_UNICODETEXT;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;

  if FDataObj.GetData(FormatEtc, Medium) = S_OK then
  begin
    try
      S := PChar(GlobalLock(Medium.hGlobal));
      Result.Data := S;
    finally
      GlobalUnlock(Medium.hGlobal);
      ReleaseStgMedium(Medium);
    end;
    Exit;
  end;
end;

function TSynFixedDropTarget.DragEnter(const DataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  // Match FMX's TWinDropTarget.DragEnter: just store data, set NONE
  FDataObj := DataObj;
  dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

function TSynFixedDropTarget.DragOver(grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HResult;
var
  Operation: TDragOperation;
  P: TPointF;
begin
  Result := E_UNEXPECTED;
  try
    dwEffect := DROPEFFECT_NONE;
    // Use PxToDp for coordinate conversion — same as FMX's TWinDropTarget
    P := PxToDp(pt);
    Operation := TDragOperation.None;
    FForm.DragOver(GetDataObject, P, Operation);
    case Operation of
      TDragOperation.None: dwEffect := DROPEFFECT_NONE;
      TDragOperation.Move: dwEffect := DROPEFFECT_MOVE;
      TDragOperation.Copy: dwEffect := DROPEFFECT_COPY;
      TDragOperation.Link: dwEffect := DROPEFFECT_LINK;
    end;
    FLastEffect := dwEffect;
    Result := S_OK;
  except
    dwEffect := DROPEFFECT_NONE;
  end;
end;

function TSynFixedDropTarget.DragLeave: HResult;
begin
  FForm.DragLeave;
  FDataObj := nil;
  FLastEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

function TSynFixedDropTarget.Drop(const DataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  P: TPointF;
begin
  Result := S_OK;
  try
    if DataObj = nil then Exit;
    FDataObj := DataObj;
    P := PxToDp(pt);
    FForm.DragDrop(GetDataObject, P);
    // FMX's TWinDropTarget.Drop forgets to set dwEffect — that's the bug.
    // We report the last agreed-upon effect from DragOver.
    dwEffect := FLastEffect;
  finally
    FDataObj := nil;
  end;
end;

{ TSynDropTargetToken }

constructor TSynDropTargetToken.Create(AHWnd: HWND);
begin
  inherited Create;
  FHWnd := AHWnd;
end;

destructor TSynDropTargetToken.Destroy;
begin
  Winapi.ActiveX.RevokeDragDrop(FHWnd);
  inherited;
end;

{ Registration }

function RegisterOleDropTarget(AControl: TControl): IInterface;
var
  Form: TCommonCustomForm;
  Wnd: HWND;
  OurTarget: IDropTarget;
begin
  Result := nil;
  if AControl.Root = nil then Exit;
  if not (AControl.Root.GetObject is TCommonCustomForm) then Exit;
  Form := TCommonCustomForm(AControl.Root.GetObject);
  if Form.Handle = nil then Exit;

  Wnd := WindowHandleToPlatform(Form.Handle).Wnd;
  if Wnd = 0 then Exit;

  // Revoke FMX's built-in drop target (registered in CreateHandle)
  Winapi.ActiveX.RevokeDragDrop(Wnd);

  // Register our fixed drop target that delegates to the same
  // Form.DragOver / Form.DragDrop as FMX but correctly sets dwEffect
  OurTarget := TSynFixedDropTarget.Create(Form);
  if Winapi.ActiveX.RegisterDragDrop(Wnd, OurTarget) = S_OK then
    Result := TSynDropTargetToken.Create(Wnd) as IInterface;
end;

procedure UnregisterOleDropTarget(var AToken: IInterface);
begin
  AToken := nil; // Release triggers TSynDropTargetToken.Destroy
end;

{$ENDIF}

{$IFNDEF MSWINDOWS}

function RegisterOleDropTarget(AControl: TControl): IInterface;
begin
  Result := nil;
end;

procedure UnregisterOleDropTarget(var AToken: IInterface);
begin
  AToken := nil;
end;

{$ENDIF}

end.
