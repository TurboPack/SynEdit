unit SynAccessibility;
{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
interface
-------------------------------------------------------------------------------}
interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Classes,
  System.Types,
  SynEdit;

{$REGION 'Header Tranlsation of UIAutomationCore.h'}

// Used in WM_GETOBJECT
const
  UiaRootObjectId: NativeInt = -25;

// Constants for enum ProviderOptions
type
  ProviderOptions = TOleEnum;
const
  ProviderOptions_ClientSideProvider = $00000001;
  ProviderOptions_ServerSideProvider = $00000002;
  ProviderOptions_NonClientAreaProvider = $00000004;
  ProviderOptions_OverrideProvider = $00000008;
  ProviderOptions_ProviderOwnsSetFocus = $00000010;
  ProviderOptions_UseComThreading = $00000020;
  ProviderOptions_RefuseNonClientSupport = $00000040;
  ProviderOptions_HasNativeIAccessible = $00000080;
  ProviderOptions_UseClientCoordinates = $00000100;

// Constants for enum TextPatternRangeEndpoint
type
  TextPatternRangeEndpoint = TOleEnum;
const
  TextPatternRangeEndpoint_Start = $00000000;
  TextPatternRangeEndpoint_End = $00000001;

// Constants for enum TextUnit
type
  TextUnit = TOleEnum;
const
  TextUnit_Character = $00000000;
  TextUnit_Format = $00000001;
  TextUnit_Word = $00000002;
  TextUnit_Line = $00000003;
  TextUnit_Paragraph = $00000004;
  TextUnit_Page = $00000005;
  TextUnit_Document = $00000006;

// Constants for enum SupportedTextSelection
type
  SupportedTextSelection = TOleEnum;
const
  SupportedTextSelection_None = $00000000;
  SupportedTextSelection_Single = $00000001;
  SupportedTextSelection_Multiple = $00000002;

// Constants for enum TextEditChangeType
type
  TextEditChangeType = TOleEnum;
const
  TextEditChangeType_None = $00000000;
  TextEditChangeType_AutoCorrect = $00000001;
  TextEditChangeType_Composition = $00000002;
  TextEditChangeType_CompositionFinalized = $00000003;
  TextEditChangeType_AutoComplete = $00000004;

// Constants for enum ScrollAmount
type
  ScrollAmount = TOleEnum;
const
  ScrollAmount_LargeDecrement = $00000000;
  ScrollAmount_SmallDecrement = $00000001;
  ScrollAmount_NoAmount = $00000002;
  ScrollAmount_LargeIncrement = $00000003;
  ScrollAmount_SmallIncrement = $00000004;

// Control Type, Pattern, Property and Event ids
const
  UIA_EditControlTypeId = 50004;
  UIA_DocumentControlTypeId = 50030;

  UIA_ValuePatternId = 10002;
  UIA_RangeValuePatternId	 = 10003;
  UIA_ScrollPatternId = 10004;
  UIA_TextPatternId	= 10014;
  UIA_TextPattern2Id	= 10024;

  UIA_AutomationIdPropertyId = 30011;
  UIA_BoundingRectanglePropertyId =  30001;
  UIA_ClassNamePropertyId = 30012;
  UIA_ClickablePointPropertyId	 =  30014;
  UIA_ControlTypePropertyId = 30003;
  UIA_HasKeyboardFocusPropertyId = 30008;
  UIA_IsContentElementPropertyId = 30017;
  UIA_IsControlElementPropertyId = 30016;
  UIA_IsEnabledPropertyId = 30010;
  UIA_IsKeyboardFocusablePropertyId = 30009;
  UIA_IsPasswordPropertyId = 30019;
  UIA_LabeledByPropertyId = 30018;
  UIA_LocalizedControlTypePropertyId = 30004;
  UIA_NamePropertyId = 30005;
  UIA_NativeWindowHandlePropertyId = 30020;
  UIA_ProviderDescriptionPropertyId = 30107;

  UIA_AutomationFocusChangedEventId = 20005;
  UIA_Text_TextSelectionChangedEventId = 20014;
  UIA_Text_TextChangedEventId = 20015;

type
  {$ALIGN 8}
  UiaPoint = record
    x: Double;
    y: Double;
  end;

// *********************************************************************//
// Interface: IRawElementProviderSimple
// Flags:     (256) OleAutomation
// GUID:      {D6DD68D1-86FD-4332-8666-9ABEDEA2D24C}
// *********************************************************************//
  IRawElementProviderSimple = interface(IUnknown)
    ['{D6DD68D1-86FD-4332-8666-9ABEDEA2D24C}']
    function Get_ProviderOptions(out RetVal: ProviderOptions): HResult; stdcall;
    function GetPatternProvider(patternId: SYSINT; out RetVal: IUnknown): HResult; stdcall;
    function GetPropertyValue(propertyId: SYSINT; out pRetVal: OleVariant): HResult; stdcall;
    function Get_HostRawElementProvider(out RetVal: IRawElementProviderSimple): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRangeValueProvider
// Flags:     (256) OleAutomation
// GUID:      {36DC7AEF-33E6-4691-AFE1-2BE7274B3D33}
// *********************************************************************//
  IRangeValueProvider = interface(IUnknown)
    ['{36DC7AEF-33E6-4691-AFE1-2BE7274B3D33}']
    function SetValue(val: Double): HResult; stdcall;
    function Get_Value(out RetVal: Double): HResult; stdcall;
    function Get_IsReadOnly(out RetVal: BOOL): HResult; stdcall;
    function Get_Maximum(out RetVal: Double): HResult; stdcall;
    function Get_Minimum(out RetVal: Double): HResult; stdcall;
    function Get_LargeChange(out RetVal: Double): HResult; stdcall;
    function Get_SmallChange(out RetVal: Double): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IValueProvider
// Flags:     (256) OleAutomation
// GUID:      {C7935180-6FB3-4201-B174-7DF73ADBF64A}
// *********************************************************************//
  IValueProvider = interface(IUnknown)
    ['{C7935180-6FB3-4201-B174-7DF73ADBF64A}']
    function SetValue(val: PWideChar): HResult; stdcall;
    function Get_Value(out RetVal: WideString): HResult; stdcall;
    function Get_IsReadOnly(out RetVal: BOOL): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITextRangeProvider
// Flags:     (256) OleAutomation
// GUID:      {5347AD7B-C355-46F8-AFF5-909033582F63}
// *********************************************************************//
  ITextRangeProvider = interface(IUnknown)
    ['{5347AD7B-C355-46F8-AFF5-909033582F63}']
    function Clone(out RetVal: ITextRangeProvider): HResult; stdcall;
    function Compare(const range: ITextRangeProvider; out RetVal: BOOL): HResult; stdcall;
    function CompareEndpoints(endpoint: TextPatternRangeEndpoint;
                              const targetRange: ITextRangeProvider;
                              targetEndpoint: TextPatternRangeEndpoint; out RetVal: SYSINT): HResult; stdcall;
    function ExpandToEnclosingUnit(AUnit: TextUnit): HResult; stdcall;
    function FindAttribute(attributeId: SYSINT; val: OleVariant; backward: Integer;
                           out RetVal: ITextRangeProvider): HResult; stdcall;
    function FindText(const text: WideString; backward: BOOL; ignoreCase: BOOL;
                      out RetVal: ITextRangeProvider): HResult; stdcall;
    function GetAttributeValue(attributeId: SYSINT; out RetVal: OleVariant): HResult; stdcall;
    function GetBoundingRectangles(out RetVal: PSafeArray): HResult; stdcall;
    function GetEnclosingElement(out RetVal: IRawElementProviderSimple): HResult; stdcall;
    function GetText(maxLength: SYSINT; out RetVal: WideString): HResult; stdcall;
    function Move(AUunit: TextUnit; count: SYSINT; out RetVal: SYSINT): HResult; stdcall;
    function MoveEndpointByUnit(endpoint: TextPatternRangeEndpoint; Aunit: TextUnit; count: SYSINT;
                                out RetVal: SYSINT): HResult; stdcall;
    function MoveEndpointByRange(endpoint: TextPatternRangeEndpoint;
                                 const targetRange: ITextRangeProvider;
                                 targetEndpoint: TextPatternRangeEndpoint): HResult; stdcall;
    function Select: HResult; stdcall;
    function AddToSelection: HResult; stdcall;
    function RemoveFromSelection: HResult; stdcall;
    function ScrollIntoView(alignToTop: BOOL): HResult; stdcall;
    function GetChildren(out RetVal: PSafeArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITextProvider
// Flags:     (256) OleAutomation
// GUID:      {3589C92C-63F3-4367-99BB-ADA653B77CF2}
// *********************************************************************//
  ITextProvider = interface(IUnknown)
    ['{3589C92C-63F3-4367-99BB-ADA653B77CF2}']
    function GetSelection(out RetVal: PSafeArray): HResult; stdcall;
    function GetVisibleRanges(out RetVal: PSafeArray): HResult; stdcall;
    function RangeFromChild(const childElement: IRawElementProviderSimple;
                            out RetVal: ITextRangeProvider): HResult; stdcall;
    function RangeFromPoint(point: UiaPoint; out RetVal: ITextRangeProvider): HResult; stdcall;
    function Get_DocumentRange(out RetVal: ITextRangeProvider): HResult; stdcall;
    function Get_SupportedTextSelection(out RetVal: SupportedTextSelection): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITextProvider2
// Flags:     (256) OleAutomation
// GUID:      {0DC5E6ED-3E16-4BF1-8F9A-A979878BC195}
// *********************************************************************//
  ITextProvider2 = interface(ITextProvider)
    ['{0DC5E6ED-3E16-4BF1-8F9A-A979878BC195}']
    function RangeFromAnnotation(const annotationElement: IRawElementProviderSimple;
                                 out pRetVal: ITextRangeProvider): HResult; stdcall;
    function GetCaretRange(out isActive: BOOL; out RetVal: ITextRangeProvider): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITextEditProvider
// Flags:     (256) OleAutomation
// GUID:      {EA3605B4-3A05-400E-B5F9-4E91B40F6176}
// *********************************************************************//
  ITextEditProvider = interface(ITextProvider)
    ['{EA3605B4-3A05-400E-B5F9-4E91B40F6176}']
    function GetActiveComposition(out pRetVal: ITextRangeProvider): HResult; stdcall;
    function GetConversionTarget(out pRetVal: ITextRangeProvider): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITextRangeProvider2
// Flags:     (256) OleAutomation
// GUID:      {9BBCE42C-1921-4F18-89CA-DBA1910A0386}
// *********************************************************************//
  ITextRangeProvider2 = interface(ITextRangeProvider)
    ['{9BBCE42C-1921-4F18-89CA-DBA1910A0386}']
    function ShowContextMenu: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IScrollProvider
// Flags:     (256) OleAutomation
// GUID:      {B38B8077-1FC3-42A5-8CAE-D40C2215055A}
// *********************************************************************//
  IScrollProvider = interface(IUnknown)
    ['{B38B8077-1FC3-42A5-8CAE-D40C2215055A}']
    function Scroll(horizontalAmount: ScrollAmount; verticalAmount: ScrollAmount): HResult; stdcall;
    function SetScrollPercent(horizontalPercent: Double; verticalPercent: Double): HResult; stdcall;
    function Get_HorizontalScrollPercent(out pRetVal: Double): HResult; stdcall;
    function Get_VerticalScrollPercent(out pRetVal: Double): HResult; stdcall;
    function Get_HorizontalViewSize(out pRetVal: Double): HResult; stdcall;
    function Get_VerticalViewSize(out pRetVal: Double): HResult; stdcall;
    function Get_HorizontallyScrollable(out pRetVal: Integer): HResult; stdcall;
    function Get_VerticallyScrollable(out pRetVal: Integer): HResult; stdcall;
  end;

// imported functions
const
  UIAutomationLib = 'UIAutomationCore.dll';
{$WARN SYMBOL_PLATFORM OFF}
function UiaClientsAreListening(): BOOL; stdcall; external UIAutomationLib name 'UiaClientsAreListening' delayed;
function UiaGetReservedNotSupportedValue(out unkNotSupportedValue: IUnknown): HRESULT; stdcall; external UIAutomationLib name 'UiaGetReservedNotSupportedValue' delayed;
function UiaHostProviderFromHwnd(hwnd: HWND; out Provider: IRawElementProviderSimple): HRESULT; stdcall; external UIAutomationLib name 'UiaHostProviderFromHwnd' delayed;
function UiaReturnRawElementProvider(hwnd: HWND; wParam: WPARAM; lParam: LPARAM; element: IRawElementProviderSimple): LRESULT; stdcall; external UIAutomationLib name 'UiaReturnRawElementProvider' delayed;
function UiaDisconnectProvider(Provider: IRawElementProviderSimple): HRESULT; stdcall; external UIAutomationLib name 'UiaDisconnectProvider' delayed;
function UiaRaiseAutomationPropertyChangedEvent(Provider: IRawElementProviderSimple; id: SYSINT; oldValue, newValue: OleVariant): HRESULT; stdcall; external UIAutomationLib name 'UiaRaiseAutomationPropertyChangedEvent' delayed;
function UiaRaiseAutomationEvent(Provider: IRawElementProviderSimple; id: SYSINT): HRESULT; stdcall; external UIAutomationLib name 'UiaRaiseAutomationEvent' delayed;
function UiaRaiseTextEditTextChangedEvent(Provider: IRawElementProviderSimple; textEditChangeType: TextEditChangeType; ChangedData: PSafeArray): HRESULT; stdcall; external UIAutomationLib name 'UiaRaiseTextEditTextChangedEvent' delayed;
{$WARN SYMBOL_PLATFORM ON}

{$ENDREGION 'Header Tranlsation of UIAutomationCore.h'}

type
  TSynTextRangeProvider = class(TInterfacedObject, IUnknown, ITextRangeProvider)
  private
    FSynEdit: TCustomSynEdit;
    BB, BE: TBufferCoord;
  private
    function Clone(out RetVal: ITextRangeProvider): HResult; stdcall;
    function Compare(const range: ITextRangeProvider; out RetVal: BOOL): HResult; stdcall;
    function CompareEndpoints(endpoint: TextPatternRangeEndpoint; const
        targetRange: ITextRangeProvider; targetEndpoint: TextPatternRangeEndpoint;
        out RetVal: SYSINT): HResult; stdcall;
    function ExpandToEnclosingUnit(AUnit: TextUnit): HResult; stdcall;
    function FindAttribute(attributeId: SYSINT; val: OleVariant; backward: Integer;
                           out RetVal: ITextRangeProvider): HResult; stdcall;
    function FindText(const text: WideString; backward: BOOL; ignoreCase: BOOL;
                      out RetVal: ITextRangeProvider): HResult; stdcall;
    function GetAttributeValue(attributeId: SYSINT; out RetVal: OleVariant): HResult; stdcall;
    function GetBoundingRectangles(out RetVal: PSafeArray): HResult; stdcall;
    function GetEnclosingElement(out RetVal: IRawElementProviderSimple): HResult; stdcall;
    function GetText(maxLength: SYSINT; out RetVal: WideString): HResult; stdcall;
    function Move(AUnit: TextUnit; count: SYSINT; out RetVal: SYSINT): HResult; stdcall;
    function MoveEndpointByUnit(endpoint: TextPatternRangeEndpoint; AUnit:
        TextUnit; count: SYSINT; out RetVal: SYSINT): HResult; stdcall;
    function MoveEndpointByRange(endpoint: TextPatternRangeEndpoint;
                                 const targetRange: ITextRangeProvider;
                                 targetEndpoint: TextPatternRangeEndpoint): HResult; stdcall;
    function Select: HResult; stdcall;
    function AddToSelection: HResult; stdcall;
    function RemoveFromSelection: HResult; stdcall;
    function ScrollIntoView(alignToTop: BOOL): HResult; stdcall;
    function GetChildren(out RetVal: PSafeArray): HResult; stdcall;
  public
    constructor Create(ASynEdit: TCustomSynEdit; ABB, ABE: TBufferCoord);
  end;


  TSynUIAutomationProvider = class(TInterfacedObject, //IUnknown,
    IRawElementProviderSimple, IValueProvider, ITextProvider, ITextProvider2)
  private
    FSynEdit: TCustomSynEdit;
    OldBoundingRectangle: OleVariant;
    function BoundingRectangle: OleVariant;
    function ClickablePoint: OleVariant;
    function NameProperty: OleVariant;
    // IRawElementProviderSimple implementation
    function Get_ProviderOptions(out RetVal: ProviderOptions): HResult; stdcall;
    function GetPatternProvider(patternId: SYSINT; out RetVal: IUnknown): HResult; stdcall;
    function GetPropertyValue(propertyId: SYSINT; out RetVal: OleVariant): HResult; stdcall;
    function Get_HostRawElementProvider(out RetVal: IRawElementProviderSimple): HResult; stdcall;
    // IValueProvider
    function SetValue(val: PWideChar): HResult; stdcall;
    function Get_Value(out RetVal: WideString): HResult; stdcall;
    function Get_IsReadOnly(out RetVal: BOOL): HResult; stdcall;
    // ITextProvider
    function GetSelection(out RetVal: PSafeArray): HResult; stdcall;
    function GetVisibleRanges(out RetVal: PSafeArray): HResult; stdcall;
    function RangeFromChild(const childElement: IRawElementProviderSimple;
                            out RetVal: ITextRangeProvider): HResult; stdcall;
    function RangeFromPoint(point: UiaPoint; out RetVal: ITextRangeProvider): HResult; stdcall;
    function Get_DocumentRange(out RetVal: ITextRangeProvider): HResult; stdcall;
    function Get_SupportedTextSelection(out RetVal: SupportedTextSelection): HResult; stdcall;
    // ITextProvider2
    function RangeFromAnnotation(const annotationElement: IRawElementProviderSimple;
                                 out pRetVal: ITextRangeProvider): HResult; stdcall;
    function GetCaretRange(out isActive: BOOL; out RetVal: ITextRangeProvider): HResult; stdcall;
  public
    constructor Create(ASynEdit: TCustomSynEdit);
    procedure NotifyBoundingRectangleChange;
    procedure RaiseTextChangedEvent;
    procedure RaiseTextSelectionChangedEvent;
    procedure EditorDestroyed;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Variants,
  Vcl.Controls,
  SynEditTypes,
  SynUnicode;

resourcestring
  rsLocalizedControlType = 'editor';

{$REGION 'TSynUIAutomationProvider'}

constructor TSynUIAutomationProvider.Create(ASynEdit: TCustomSynEdit);
begin
  inherited Create;
  FSynEdit := ASynEdit;
end;

procedure TSynUIAutomationProvider.EditorDestroyed;
begin
  UiaDisconnectProvider(IRawElementProviderSimple(Self));
  FSynEdit := nil;
end;

function TSynUIAutomationProvider.BoundingRectangle: OleVariant;
var
  P: TPoint;
begin
  P := FSynEdit.ClientToScreen(FSynEdit.ClientRect.TopLeft);
  Result := VarArrayCreate([0, 3], varDouble);
  Result[0] := P.X;
  Result[1] := P.Y;
  Result[2] := FSynEdit.Width;
  Result[3] := FSynEdit.Height;
end;

function TSynUIAutomationProvider.ClickablePoint: OleVariant;
var
  P: TPoint;
begin
  P := FSynEdit.ClientToScreen(FSynEdit.ClientRect.TopLeft);
  Result := VarArrayCreate([0, 1], varDouble);
  Result[0] := P.X + FSynEdit.Width div 2;
  Result[1] := P.Y + FSynEdit.Height div 2;
end;

function TSynUIAutomationProvider.NameProperty: OleVariant;
begin
  if FSynEdit.AccessibleName <> '' then
    Result := FSynEdit.AccessibleName
  else if FSynEdit.Name <> '' then
    Result := FSynEdit.Name
  else
    Result := FSynEdit.ClassName;
end;

procedure TSynUIAutomationProvider.NotifyBoundingRectangleChange;
begin
  if UiaClientsAreListening then
    TThread.ForceQueue(nil, procedure
    begin
      if FSynEdit = nil then Exit;

      UiaRaiseAutomationPropertyChangedEvent(IRawElementProviderSimple(Self),
        UIA_BoundingRectanglePropertyId, OldBoundingRectangle, BoundingRectangle);
    end);
end;

function TSynUIAutomationProvider.GetCaretRange(out isActive: BOOL;
  out RetVal: ITextRangeProvider): HResult;
begin
  IsActive := FSynEdit.Focused;
  RetVal := TSynTextRangeProvider.Create(FSynEdit, FSynEdit.CaretXY, FSynEdit.CaretXY);
  Result := S_OK;
end;

function TSynUIAutomationProvider.GetPatternProvider(patternId: SYSINT; out
    RetVal: IUnknown): HResult;
begin
  Retval := nil;

  if not Assigned(FSynEdit) then
    Exit(S_FALSE)
  else
    Result := S_OK;

  case patternID of
    UIA_ValuePatternID: RetVal := Self as IValueProvider;
    UIA_TextPatternId: RetVal := Self as ITextProvider;
    UIA_TextPattern2Id: RetVal := Self as ITextProvider2;
  end
end;

function TSynUIAutomationProvider.GetPropertyValue(propertyId: SYSINT; out
    RetVal: OleVariant): HResult;
begin
  RetVal := Unassigned;

  if not Assigned(FSynEdit) then
    Exit(S_FALSE)
  else
    Result := S_OK;

  case propertyId of
    UIA_AutomationIdPropertyId: RetVal := FSynEdit.Name;
    UIA_BoundingRectanglePropertyId:
      begin
        OldBoundingRectangle := BoundingRectangle;
        RetVal := OldBoundingRectangle;
      end;
    UIA_ClickablePointPropertyId: RetVal := ClickablePoint;
    UIA_ClassNamePropertyId: RetVal := FSynEdit.ClassName;
    UIA_ControlTypePropertyId: RetVal := UIA_DocumentControlTypeId;
    UIA_HasKeyboardFocusPropertyId: RetVal := FSynEdit.CanFocus;  // Focused may return False!
    UIA_IsContentElementPropertyId: RetVal := True;
    UIA_IsControlElementPropertyId: RetVal := True;
    UIA_IsEnabledPropertyId: RetVal := FSynEdit.Enabled;
    UIA_IsKeyboardFocusablePropertyId: RetVal := FSynEdit.CanFocus;
    UIA_IsPasswordPropertyId: RetVal := False;
    UIA_LocalizedControlTypePropertyId: RetVal := rsLocalizedControlType;
    UIA_NamePropertyId: RetVal := NameProperty;
    UIA_NativeWindowHandlePropertyId: RetVal := FSynEdit.Handle;
    UIA_ProviderDescriptionPropertyId:  RetVal := GetLongHint(FSynEdit.Hint);
  end;
end;

function TSynUIAutomationProvider.GetSelection(out RetVal: PSafeArray): HResult;
var
  TextRange: IUnknown;
  Index: Integer;
begin
  if FSynEdit = nil then
  begin
    RetVal := nil;
    Exit(E_UNEXPECTED);
  end;

  RetVal := SafeArrayCreateVector(VT_UNKNOWN, 0, 1);
  if RetVal = nil then
    Exit(E_UNEXPECTED);

  TextRange := TSynTextRangeProvider.Create(FSynEdit, FSynEdit.BlockBegin,
    FSynEdit.BlockEnd) as IUnknown;
  Index := 0;
  Result := SafeArrayPutElement(RetVal, Index, Pointer(TextRange)^);
  if Result <> S_OK then
    SafeArrayDestroy(RetVal);
end;

function TSynUIAutomationProvider.GetVisibleRanges(
  out RetVal: PSafeArray): HResult;
var
  TextRange: IUnknown;
  Index: Integer;
  DC1, DC2: TDisplayCoord;
begin
  if FSynEdit = nil then
  begin
    RetVal := nil;
    Exit(E_UNEXPECTED);
  end;

  RetVal := SafeArrayCreateVector(VT_UNKNOWN, 0, 1);
  if RetVal = nil then
    Exit(E_UNEXPECTED);

  DC1 := DisplayCoord(1, FSynEdit.TopLine);
  DC2 := DisplayCoord(1, Min(DC1.Row + FSynEdit.LinesInWindow, FSynEdit.DisplayRowCount));

  TextRange := TSynTextRangeProvider.Create(FSynEdit,
    FSynEdit.DisplayToBufferPos(DC1),
    FSynEdit.DisplayToBufferPos(DC2)) as IUnknown;

  Index := 0;
  Result := SafeArrayPutElement(RetVal, Index, Pointer(TextRange)^);
  if Result <> S_OK then
    SafeArrayDestroy(RetVal);
end;

function TSynUIAutomationProvider.Get_DocumentRange(
  out RetVal: ITextRangeProvider): HResult;
var
  BC: TBufferCoord;
begin
  if FSynEdit = nil then
    Exit(E_UNEXPECTED);

  TThread.Synchronize(nil, procedure
  begin
    if (FSynEdit = nil) or (FSynEdit.Lines.Count = 0) then
      BC := BufferCoord(1, 1)
    else
      BC := BufferCoord(FSynEdit.Lines[FSynEdit.Lines.Count -1].Length + 1,
              FSynEdit.Lines.Count);
  end);

  RetVal := TSynTextRangeProvider.Create(FSynEdit, BufferCoord(1, 1), BC);
  Result := S_OK;
end;

function TSynUIAutomationProvider.Get_HostRawElementProvider(out RetVal:
    IRawElementProviderSimple): HResult;
begin
  if Assigned(FSynEdit) and FSynEdit.HandleAllocated then
    Result := UiaHostProviderFromHwnd(FSynEdit.Handle, RetVal)
  else
    Result := S_FALSE;
end;

function TSynUIAutomationProvider.Get_IsReadOnly(out RetVal: BOOL): HResult;
begin
  if not Assigned(FSynEdit) then
    Exit(E_UNEXPECTED)
  else
    Result := S_OK;

  RetVal := FSynEdit.ReadOnly;
end;

function TSynUIAutomationProvider.Get_ProviderOptions(
  out RetVal: ProviderOptions): HResult;
begin
  RetVal:= ProviderOptions_ServerSideProvider;
  Result := S_OK;
end;

function TSynUIAutomationProvider.Get_SupportedTextSelection(
  out RetVal: SupportedTextSelection): HResult;
begin
  RetVal := SupportedTextSelection_Single;
  Result := S_OK;
end;

function TSynUIAutomationProvider.Get_Value(out RetVal: WideString): HResult;
begin
  if not Assigned(FSynEdit) then
    Exit(S_FALSE)
  else
    Result := S_OK;

  RetVal := FSynEdit.Text;
end;

procedure TSynUIAutomationProvider.RaiseTextChangedEvent;
begin
  if UiaClientsAreListening then
    TThread.ForceQueue(nil, procedure
    begin
      if Assigned(FSynEdit) then
        UiaRaiseAutomationEvent(IRawElementProviderSimple(Self),
          UIA_Text_TextChangedEventId);
    end);
end;

procedure TSynUIAutomationProvider.RaiseTextSelectionChangedEvent;
begin
  if UiaClientsAreListening then
    TThread.ForceQueue(nil, procedure
    begin
      if Assigned(FSynEdit) then
        UiaRaiseAutomationEvent(IRawElementProviderSimple(Self),
          UIA_Text_TextSelectionChangedEventId);
    end);
end;

function TSynUIAutomationProvider.RangeFromAnnotation(
  const annotationElement: IRawElementProviderSimple;
  out pRetVal: ITextRangeProvider): HResult;
begin
  pRetVal := nil;
  Result := S_FALSE;
end;

function TSynUIAutomationProvider.RangeFromChild(
  const childElement: IRawElementProviderSimple;
  out RetVal: ITextRangeProvider): HResult;
begin
  RetVal := nil;
  Result := E_INVALIDARG;
end;

function TSynUIAutomationProvider.RangeFromPoint(point: UiaPoint;
  out RetVal: ITextRangeProvider): HResult;
var
  P: TPoint;
  BC: TBufferCoord;
begin
  if FSynEdit = nil then
  begin
    RetVal := nil;
    Exit(E_UNEXPECTED);
  end;

  TThread.Synchronize(nil, procedure
  begin
    if (FSynEdit = nil) or (FSynEdit.Lines.Count = 0) then
      BC := BufferCoord(1, 1)
    else
    begin
      P := TPoint.Create(Round(point.x), Round(point.y));
      P := FSynEdit.ScreenToClient(P);
      BC := FSynEdit.DisplayToBufferPos(FSynEdit.PixelsToNearestRowColumn(P.X, P.Y));
    end;
  end);

  RetVal := TSynTextRangeProvider.Create(FSynEdit, BC, BC);
  Result := S_OK;
end;

function TSynUIAutomationProvider.SetValue(val: PWideChar): HResult;
begin
  if not Assigned(FSynEdit) or FSynEdit.ReadOnly then
    Exit(S_FALSE)
  else
    Result := S_OK;

  TThread.Synchronize(nil, procedure
  begin
    if Assigned(FSynEdit) then
      FSynEdit.Text := val;
  end);
end;

{$ENDREGION 'TSynUIAutomationProvider'}

{$REGION 'TSynTextRangeProvider'}

function TSynTextRangeProvider.AddToSelection: HResult;
begin
  // Multi-selection not supported
  Result := S_FALSE;
end;

function TSynTextRangeProvider.Clone(out RetVal: ITextRangeProvider): HResult;
begin
  RetVal := TSynTextRangeProvider.Create(FSynEdit, BB, BE);
  Result := S_OK;
end;

function TSynTextRangeProvider.Compare(const range: ITextRangeProvider; out
    RetVal: BOOL): HResult;
var
  SynRange: TSynTextRangeProvider;
begin
  SynRange := range as TSynTextRangeProvider;
  RetVal := Assigned(SynRange) and (SynRange.FSynEdit = FSynEdit) and
    (SynRange.BB = BB) and (SynRange.BE = BE);
  Result := S_OK;
end;

function TSynTextRangeProvider.CompareEndpoints(
  endpoint: TextPatternRangeEndpoint; const targetRange: ITextRangeProvider;
  targetEndpoint: TextPatternRangeEndpoint; out RetVal: SYSINT): HResult;
var
  BC1, BC2: TBufferCoord;
begin
  if endpoint = TextPatternRangeEndpoint_Start then
    BC1 := BB
  else
    BC1 := BE;
  if targetEndpoint = TextPatternRangeEndpoint_Start then
    BC2 := (targetRange as TSynTextRangeProvider).BB
  else
    BC2 :=  (targetRange as TSynTextRangeProvider).BE;

  if BC1 > BC2 then
    RetVal := 1
  else if BC1 = BC2 then
    RetVal := 0
  else
    RetVal := -1;

  Result := S_OK;
end;

constructor TSynTextRangeProvider.Create(ASynEdit: TCustomSynEdit; ABB,
    ABE: TBufferCoord);
begin
  inherited Create;
  FSynEdit := ASynEdit;
  BB := ABB;
  BE := ABE;
end;

function TSynTextRangeProvider.ExpandToEnclosingUnit(AUnit: TextUnit): HResult;
begin
  if FSynEdit = nil then
    Exit(E_UNEXPECTED);

  Result := S_OK;

  TThread.Synchronize(nil, procedure
  begin
    if FSynEdit = nil then Exit;

    if FSynEdit.Lines.Count = 0 then
    begin
      BB := BufferCoord(1, 1);
      BE := BB;
      Exit;
    end;

    if AUnit = TextUnit_Format then
      AUnit := TextUnit_Word
    else if AUnit = TextUnit_Paragraph then
      AUnit := TextUnit_Page;

    BB.Line := EnsureRange(BB.Line, 1, FSynEdit.Lines.Count);
    BB.Char := EnsureRange(BB.Char, 1, FSynEdit.Lines[BB.Line - 1].Length + 1);

    case AUnit of
      TextUnit_Character:
        begin
          BE := BB;
          BE.Char := Min(BB.Char + 1, FSynEdit.Lines[BB.Line - 1].Length + 1);
        end;
      TextUnit_Word:
        // Words include whitespace at the end.
        // https://learn.microsoft.com/en-us/windows/win32/winauto/uiauto-uiautomationtextunits#word
        begin
          if BB.Char <= FSynEdit.Lines[BB.Line -1].Length then
          begin
            if FSynEdit.IsIdentChar(FSynEdit.Lines[BB.Line - 1][BB.Char]) then
              // if BB is inside a word expand to the left
              BB := FSynEdit.WordStartEx(BB)
            else if (BB.Char > 1) and FSynEdit.IsWhiteChar(FSynEdit.Lines[BB.Line - 1][BB.Char])then
              BB := FSynEdit.PrevWordPosEx(BB);
            BE := FSynEdit.NextWordPosEx(BB);
          end
          else
            BE := BB;

          if BB >= BE then
          begin
            BE := BB;
            ExpandToEnclosingUnit(TextUnit_Character);
          end;
        end;
      TextUnit_Line:
        begin
          BB.Char := 1;
          BE.Line := BB.Line;
          BE.Char := FSynEdit.Lines[BE.Line - 1].Length + 1;
        end;
      TextUnit_Page:
        begin
          BB.Char := 1;
          BE.Line := BB.Line + FSynEdit.LinesInWindow;
          BE.Line := EnsureRange(BE.Line, 1, FSynEdit.Lines.Count);
          BE.Char := FSynEdit.Lines[BE.Line - 1].Length + 1;
        end;
      TextUnit_Document:
        begin
          BB := BufferCoord(1, 1);
          BE.Line := FSynEdit.Lines.Count;
          BE.Char := FSynEdit.Lines[BB.Line - 1].Length + 1;
        end;
    end;
  end);
end;

function TSynTextRangeProvider.FindAttribute(attributeId: SYSINT; val:
    OleVariant; backward: Integer; out RetVal: ITextRangeProvider): HResult;
begin
  RetVal := nil;
  Result := S_OK;
end;

function TSynTextRangeProvider.FindText(const text: WideString; backward:
    BOOL; ignoreCase: BOOL; out RetVal: ITextRangeProvider): HResult;
var
  TextRange: ITextRangeProvider;
begin
  RetVal := nil;
  if FSynEdit = nil then
    Exit(E_UNEXPECTED)
  else if FSynEdit.Lines.Count = 0 then
    Exit(S_OK);

  Result := S_OK;
  if BB = BE then
    Exit;

  TThread.Synchronize(nil, procedure
  var
    SearchS: string;
    Line: string;
    I: Integer;
    Index, StartIndex: Integer;
  begin
    if FSynEdit = nil then
    begin
      TextRange := nil;
      Exit;
    end;

    BB.Line := EnsureRange(BB.Line, 1, FSynEdit.Lines.Count);
    BB.Char := EnsureRange(BB.Char, 1, FSynEdit.Lines[BB.Line -1].Length + 1);
    BE.Line := EnsureRange(BE.Line, BB.Line, FSynEdit.Lines.Count);
    BE.Char := EnsureRange(BE.Char, 1, FSynEdit.Lines[BB.Line -1].Length + 1);

    if IgnoreCase then
      SearchS := AnsiLowerCase(text)
    else
      SearchS := text;

    if BackWard then
    begin
      for I := BE.Line downto BB.Line do
      begin
        Line := FSynEdit.Lines[I - 1];
        if I = BE.Line then
          Line := Copy(Line, 1, BE.Char - 1);
        if I = BB.Line then
          StartIndex := BB.Char
        else
          StartIndex := 1;

        Index := Line.LastIndexOf(SearchS, Line.Length - 1, Line.Length - StartIndex + 1);
        // Index is zero-based
        if Index >= 0 then
        begin
          TextRange := TSynTextRangeProvider.Create(FSynEdit,
            BufferCoord(Index + 1, I), BufferCoord(Index + SearchS.Length + 1, I));
          Exit;
        end;
      end;
    end
    else
    begin
      for I := BB.Line to BE.Line do
      begin
        Line := FSynEdit.Lines[I - 1];
        if I = BE.Line then
          Line := Copy(Line, 1, BE.Char - 1);
        if I = BB.Line then
          StartIndex := BB.Char
        else
          StartIndex := 1;

        Index := Pos(SearchS, Line, StartIndex);
        if Index > 0 then
        begin
          TextRange := TSynTextRangeProvider.Create(FSynEdit,
            BufferCoord(Index, I), BufferCoord(Index + SearchS.Length, I));
          Exit;
        end;
      end;
    end;
  end);
  RetVal := TextRange;
end;

function TSynTextRangeProvider.GetAttributeValue(attributeId: SYSINT; out
  RetVal: OleVariant): HResult;
var
  unkNotSupportedValue: IUnknown;
begin
  Result := UiaGetReservedNotSupportedValue(unkNotSupportedValue);
  if Result = S_OK then
    RetVal := unkNotSupportedValue;
end;

function TSynTextRangeProvider.GetBoundingRectangles(out RetVal: PSafeArray): HResult;
var
  I, Index: Integer;
  R: TRect;
  BC: TBufferCoord;
  P: TPoint;
  DVal: Double;
begin
  RetVal := nil;
  if FSynEdit = nil then
    Exit(E_UNEXPECTED);

  Result := S_OK;

  if FSynEdit.Lines.Count = 0 then
    R := Rect(FSynEdit.GutterWidth + FSynEdit.TextMargin, 0, FSynEdit.CharWidth,
      FSynEdit.LineHeight)
  else
  begin
    BB.Line := EnsureRange(BB.Line, 1, FSynEdit.Lines.Count);
    BB.Char := EnsureRange(BB.Char, 1, FSynEdit.Lines[BB.Line - 1].Length + 1);
    BE.Line := EnsureRange(BE.Line, BB.Line, FSynEdit.Lines.Count);
    BE.Char := EnsureRange(BE.Char, 1, FSynEdit.Lines[BE.Line - 1].Length + 1);

    if (BB.Char = FSynEdit.Lines[BB.Line - 1].Length + 1) and
      (BE = BufferCoord(1, BB.Line + 1)) then
    begin
      // Special case for the line breaks
      P := FSynEdit.RowColumnToPixels(FSynEdit.BufferToDisplayPos(BB));
      R := Rect(P.X, P.Y, P.X + 0, P.Y + FSynEdit.LineHeight);
    end
    else
    begin
      R := Rect(MaxInt, 0, 0, 0);

      for I := BB.Line to BE.Line do
      begin
        if I = BB.Line then
          BC := BB
        else
          BC := BufferCoord(1, I);
        P := FSynEdit.RowColumnToPixels(FSynEdit.BufferToDisplayPos(BC));

        if I = BB.Line then
          R.Top := P.Y;

        R.Left := Min(P.X, R.Left);

        if I = BE.Line then
          BC := BE
        else
          BC := BufferCoord(FSynEdit.Lines[I - 1].Length + 1, I);
        P := FSynEdit.RowColumnToPixels(FSynEdit.BufferToDisplayPos(BC));

        R.Right := Max(R.Right, P.X);
        if I = BE.Line then
          R.Bottom := P.Y + FSynEdit.LineHeight;
      end;
    end;
  end;

  R.Left := Max(R.Left, FSynEdit.GutterWidth + FSynEdit.TextMargin);
  R.Right := Max(R.Right, R.Left + FSynEdit.CharWidth);
  R := TRect.Intersect(R, FSynEdit.ClientRect);

  R := FSynEdit.ClientToScreen(R);

  RetVal := SafeArrayCreateVector(VT_R8, 0, 4);
  if RetVal = nil then
    Exit(E_UNEXPECTED);
  Index := 0; DVal := R.Left; SafeArrayPutElement(RetVal, Index, DVal);
  Index := 1; DVal := R.Top; SafeArrayPutElement(RetVal, Index, DVal);
  Index := 2; DVal := R.Width; SafeArrayPutElement(RetVal, Index, DVal);
  Index := 3; DVal := R.Height; SafeArrayPutElement(RetVal, Index, DVal);
end;

function TSynTextRangeProvider.GetChildren(out RetVal: PSafeArray): HResult;
begin
  RetVal := SafeArrayCreateVector(VT_UNKNOWN, 0, 0);
  if RetVal = nil then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TSynTextRangeProvider.GetEnclosingElement(
  out RetVal: IRawElementProviderSimple): HResult;
begin
  Result := S_FALSE;
  if Assigned(FSynEdit) and FSynEdit.HandleAllocated then
    Result := UiaHostProviderFromHwnd(FSynEdit.Handle, RetVal);
end;

function TSynTextRangeProvider.GetText(maxLength: SYSINT; out RetVal:
    WideString): HResult;
var
  S: string;
begin
  RetVal := '';
  if not Assigned(FSynEdit)  then
    Exit(S_FALSE);

  Result := S_OK;

  TThread.Synchronize(nil, procedure
  var
    I: Integer;
  begin
    if FSynEdit = nil then
    begin
      S := '';
      Exit;
    end;

    if FSynEdit.Lines.Count = 0 then
      Exit;

    BB.Line := EnsureRange(BB.Line, 1, FSynEdit.Lines.Count);
    BB.Char := EnsureRange(BB.Char, 1, FSynEdit.Lines[BB.Line - 1].Length + 1);
    BE.Line := EnsureRange(BE.Line, BB.Line, FSynEdit.Lines.Count);
    BE.Char := EnsureRange(BE.Char, 1, FSynEdit.Lines[BE.Line - 1].Length + 1);

    if BB = BE then
    begin
      if BB.Char = FSynEdit.Lines[BB.Line - 1].Length + 1 then
        S := WideLineFeed; // SLineBreak confused NVDA
    end
    else if BB.Line = BE.Line then
      S := Copy(FSynEdit.Lines[BB.Line - 1], BB.Char, BE.Char - BB.Char)
    else
    begin
      S := Copy(FSynEdit.Lines[BB.Line - 1], BB.Char);  // first line
      for I := BB.Line + 1 to BE.Line - 1 do
      begin
        S := S + SLineBreak + FSynEdit.Lines[I - 1];
        if (maxLength >= 0) and (S.Length >= maxLength) then
          Break;
      end;
      S := S + SLineBreak + Copy(FSynEdit.Lines[BE.Line - 1], 1, BE.Char - 1); // last line
    end;
  end);

  if maxLength < 0 then
    RetVal := S
  else
    RetVal := Copy(S, 1, maxLength);
end;

function TSynTextRangeProvider.Move(AUnit: TextUnit; count: SYSINT; out RetVal:
    SYSINT): HResult;
var
  NMoves: SYSINT;
begin
  if FSynEdit = nil then
    Exit(E_UNEXPECTED);

  Result := S_OK;
  RetVal := 0;

  if Count = 0 then
    Exit
  else if FSynEdit.Lines.Count = 0 then
  begin
    BB := BufferCoord(1, 1);
    BE := BB;
    Exit;
  end;

  if AUnit = TextUnit_Format then
    AUnit := TextUnit_Word
  else if AUnit = TextUnit_Paragraph then
    AUnit := TextUnit_Page;

  TThread.Synchronize(nil, procedure
  var
    I: Integer;
    IsDegenerate: Boolean;
  begin
    IsDegenerate := BB = BE;
    NMoves := 0;
    if FSynEdit = nil then Exit;

    case AUnit of
      TextUnit_Character:
        begin
          while Abs(NMoves) < Abs(Count) do
          begin
            if (Count < 0) and (BB.Char = 1) then
            begin
              if BB.Line = 1 then
                Break
              else
              begin
                Dec(BB.Line);
                BB.Char := FSynEdit.Lines[BB.Line - 1].Length + 1;
                Dec(NMoves);
              end;
            end
            else if (Count > 0) and (BB.Char = FSynEdit.Lines[BB.Line - 1].Length + 1) then
            begin
              if BB.Line = FSynEdit.Lines.Count then
                Break
              else
              begin
                Inc(BB.Line);
                BB.Char := 1;
                Inc(NMoves);
              end;
            end
            else
            begin
              BE := BB;
              Inc(BB.Char, Count - NMoves);
              BB.Char := EnsureRange(BB.Char, 1, FSynEdit.Lines[BB.Line - 1].Length + 1);
              Inc(NMoves, BB.Char - BE.Char);
            end;
          end;
          BE := BB;
          if not IsDegenerate then
            ExpandToEnclosingUnit(TextUnit_Character);
        end;
      TextUnit_Word:
        begin
          if Count > 0 then
          begin
            for I := 1 to Count do
            begin
              BB := BE;
              BE := FSynEdit.NextWordPosEx(BE);
              if BB = BE then
                Break;
              BB := BE;
              if not IsDegenerate then
                ExpandToEnclosingUnit(TextUnit_Word);
              Inc(NMoves);
            end;
          end
          else
          begin
            for I := 1 to -Count do
            begin
              BE := BB;
              // move to the start of the word if BB is inside a word
              BB := FSynEdit.WordStartEx(BB);

              BB := FSynEdit.PrevWordPosEx(BB);
              if BB = BE then
                Break;
              BE := BB;
              if not IsDegenerate then
                ExpandToEnclosingUnit(TextUnit_Word);
              Dec(NMoves)
            end;
          end;
        end;
      TextUnit_Line:
        begin
          BE := BB;
          Inc(BB.Line, Count);
          BB.Line := EnsureRange(BB.Line, 1, FSynEdit.Lines.Count);
          BB.Char := 1;
          NMoves := BB.Line - BE.Line;
          BE := BB;
          if not IsDegenerate then
            BE.Char := FSynEdit.Lines[BE.Line - 1].Length + 1;
        end;
      TextUnit_Page:
        begin
          BE := BB;
          Inc(BB.Line, Count * FSynEdit.LinesInWindow);
          BB.Line := EnsureRange(BB.Line, 1, FSynEdit.Lines.Count);
          BB.Char := 1;
          NMoves := Ceil((BB.Line - BE.Line) / FSynEdit.LinesInWindow);
          if IsDegenerate then
            BE := BB
          else
          begin
            BE.Line := Min(BB.Line + FSynEdit.LinesInWindow - 1, FSynEdit.Lines.Count);
            BE.Char := FSynEdit.Lines[BE.Line - 1].Length + 1;
          end;
        end;
      TextUnit_Document:
        if Count > 0 then
        begin
          BB.Line := FSynEdit.Lines.Count;
          BB.Char := FSynEdit.Lines[BB.Line - 1].Length + 1;
          BE := BB;
          NMoves := 1;
        end
        else
        begin
          BB.Line := 1;
          BB.Char := 1;
          BE := BB;
          NMoves := -1;
        end
    end;
  end);
  RetVal := NMoves;
end;

function TSynTextRangeProvider.MoveEndpointByRange(
  endpoint: TextPatternRangeEndpoint; const targetRange: ITextRangeProvider;
  targetEndpoint: TextPatternRangeEndpoint): HResult;
var
  BC: TBufferCoord;
begin
  if FSynEdit = nil then
    Exit(E_UNEXPECTED);

  Result := S_OK;

  if targetEndpoint = TextPatternRangeEndpoint_Start then
    BC := (targetRange as TSynTextRangeProvider).BB
  else if targetEndpoint = TextPatternRangeEndpoint_End then
    BC := (targetRange as TSynTextRangeProvider).BE;

  if endpoint = TextPatternRangeEndpoint_Start then
  begin
    BB := BC;
    if BB > BE then
      BE := BB;
  end
  else if endpoint = TextPatternRangeEndpoint_End then
  begin
    BE := BC;
    if BB > BE then
      BB := BE;
  end;
end;

function TSynTextRangeProvider.MoveEndpointByUnit(endpoint:
    TextPatternRangeEndpoint; AUnit: TextUnit; count: SYSINT; out RetVal:
    SYSINT): HResult;
var
  BC: TBufferCoord;
begin
  if FSynEdit = nil then
    Exit(E_UNEXPECTED);

  Result := S_OK;

  if endpoint = TextPatternRangeEndpoint_Start then
  begin
    BC := BE; // store
    BE := BB;
    Move(AUnit, Count, RetVal);
    BE := BC; // restore

    if BB > BE then
      BE := BB;
  end
  else if endpoint = TextPatternRangeEndpoint_End then
  begin
    BC := BB; // store
    BB := BE;
    Move(AUnit, Count, RetVal);
    // restore
    BE := BB;
    BB := BC;

    if BB > BE then
      BB := BE;
  end
end;

function TSynTextRangeProvider.RemoveFromSelection: HResult;
begin
  // Multi-selection not supported
  Result := S_FALSE;
end;

function TSynTextRangeProvider.ScrollIntoView(alignToTop: BOOL): HResult;
var
  DC: TDisplayCoord;
begin
  if Assigned(FSynEdit) and FSynEdit.HandleAllocated then
    Result := S_OK
  else
    Exit(S_FALSE);

  TThread.Synchronize(nil, procedure
  begin
    if FSynEdit = nil then Exit;

    if alignToTop then
    begin
      DC := FSynEdit.BufferToDisplayPos(BB);
      FSynEdit.TopLine := DC.Row;
    end
    else
    begin
      DC := FSynEdit.BufferToDisplayPos(BE);
      FSynEdit.TopLine := DC.Row - FSynEdit.LinesInWindow + 1;
    end;
  end);
end;

function TSynTextRangeProvider.Select: HResult;
begin
  Result := S_FALSE;
  if Assigned(FSynEdit) and FSynEdit.HandleAllocated then
  begin
    FSynEdit.SetCaretAndSelection(BB, BB, BE);
    Result := S_OK;
  end;
end;

{$ENDREGION 'TSynTextRangeProvider'}


end.
