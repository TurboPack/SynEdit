unit SynDWrite;
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

The Initial Author of this unit is pyscripter.
-------------------------------------------------------------------------------}
interface
Uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.Wincodec,
  Winapi.ActiveX,
  Winapi.D2D1,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.ImgList;

{$IF CompilerVersion <= 33}
// some constants missing from old Delphi D2D1 unit
const
  DWRITE_WORD_WRAPPING_EMERGENCY_BREAK = 2;
  D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT = $00000004;
  WICBitmapInterpolationModeHighQualityCubic = $00000004;
{$ENDIF}

type
{$REGION 'IDWriteFactory Redeclaration'}
  // Redeclaring the inteface to fix the declaration of CreateGdiCompatibleTextLayout
  // https://quality.embarcadero.com/browse/RSP-36642
  // The root factory interface for all DWrite objects.
  IDWriteFactory = interface(IUnknown)
    [SID_IDWriteFactory]
    function GetSystemFontCollection(out fontCollection: IDWriteFontCollection;
      checkForUpdates: BOOL = FALSE): HResult; stdcall;

    function CreateCustomFontCollection(
      const collectionLoader: IDWriteFontCollectionLoader; collectionKey: Pointer;
      collectionKeySize: Cardinal;
      out fontCollection: IDWriteFontCollection): HResult; stdcall;

    function RegisterFontCollectionLoader(
      const fontCollectionLoader: IDWriteFontCollectionLoader): HResult; stdcall;

    function UnregisterFontCollectionLoader(
      const fontCollectionLoader: IDWriteFontCollectionLoader): HResult; stdcall;

    function CreateFontFileReference(const filePath: PWCHAR;
      lpLastWriteTime: PFILETIME;
      out fontFile: IDWriteFontFile): HResult; stdcall;

    function CreateCustomFontFileReference(fontFileReferenceKey: Pointer;
      fontFileReferenceKeySize: Cardinal; const fontFileLoader: IDWriteFontFileLoader;
      out fontFile: IDWriteFontFile): HResult; stdcall;

    function CreateFontFace(fontFaceType: DWRITE_FONT_FACE_TYPE;
      numberOfFiles: Cardinal; fontFiles: PIDWriteFontFile;
      faceIndex: Cardinal; fontFaceSimulationFlags: DWRITE_FONT_SIMULATIONS;
      out fontFace: IDWriteFontFace): HResult; stdcall;

    function CreateRenderingParams(
      out renderingParams: IDWriteRenderingParams): HResult; stdcall;

    function CreateMonitorRenderingParams(monitor: HMONITOR;
      out renderingParams: IDWriteRenderingParams): HResult; stdcall;

    function CreateCustomRenderingParams(gamma: Single; enhancedContrast: Single;
      clearTypeLevel: Single; pixelGeometry: DWRITE_PIXEL_GEOMETRY;
      renderingMode: DWRITE_RENDERING_MODE;
      out renderingParams: IDWriteRenderingParams): HResult; stdcall;

    function RegisterFontFileLoader(
      const fontFileLoader: IDWriteFontFileLoader): HResult; stdcall;

    function UnregisterFontFileLoader(
      const fontFileLoader: IDWriteFontFileLoader): HResult; stdcall;

    function CreateTextFormat(const fontFamilyName: PWideChar;
      fontCollection: IDWriteFontCollection; fontWeight: DWRITE_FONT_WEIGHT;
      fontStyle: DWRITE_FONT_STYLE; fontStretch: DWRITE_FONT_STRETCH;
      fontSize: Single; const localeName: PWideChar;
      out textFormat: IDWriteTextFormat): HResult; stdcall;

    function CreateTypography(
      out typography: IDWriteTypography): HResult; stdcall;

    function GetGdiInterop(out gdiInterop: IDWriteGdiInterop): HResult; stdcall;

    function CreateTextLayout(_string: PWCHAR; stringLength: Cardinal;
      const textFormat: IDWriteTextFormat; maxWidth: Single; maxHeight: Single;
      out textLayout: IDWriteTextLayout): HResult; stdcall;

    function CreateGdiCompatibleTextLayout(_string: PWCHAR; stringLength: Cardinal;
      const textFormat: IDWriteTextFormat; layoutWidth: Single; layoutHeight: Single;
      pixelsPerDip: Single; transform: PDwriteMatrix; useGdiNatural: BOOL;
      out textLayout: IDWriteTextLayout): HResult; stdcall;

    function CreateEllipsisTrimmingSign(textFormat: IDWriteTextFormat;
      out trimmingSign: IDWriteInlineObject): HResult; stdcall;

    function CreateTextAnalyzer(
      out textAnalyzer: IDWriteTextAnalyzer): HResult; stdcall;

    function CreateNumberSubstitution(
      substitutionMethod: DWRITE_NUMBER_SUBSTITUTION_METHOD;
      var localeName: WideString; ignoreUserOverride: BOOL;
      out numberSubstitution: IDWriteNumberSubstitution): HResult; stdcall;

    function CreateGlyphRunAnalysis(var glyphRun: TDwriteGlyphRun;
      pixelsPerDip: Single; var transform: TDwriteMatrix;
      renderingMode: TDWriteRenderingMode; measuringMode: TDWriteMeasuringMode;
      baselineOriginX: Single; baselineOriginY: Single;
      out glyphRunAnalysis: IDWriteGlyphRunAnalysis): HResult; stdcall;
  end;
  {$EXTERNALSYM IDWriteFactory}
{$ENDREGION}

{$REGION 'IDWriteFontFace Redeclaration'}
  // Redeclaring the inteface to fix the declaration of GetGDICompatibleGlyphMetrics
  // https://quality.embarcadero.com/browse/RSP-36687
 IDWriteFontFace = interface(IUnknown)
    [SID_IDWriteFontFace]
    function GetType: DWRITE_FONT_FACE_TYPE; stdcall;

    function GetFiles(var numberOfFiles: Cardinal;
      out fontFiles: IDWriteFontFile): HResult; stdcall;

    function GetIndex: UINT32; stdcall;

    function GetSimulations: DWRITE_FONT_SIMULATIONS; stdcall;

    function IsSymbolFont: BOOL; stdcall;

    procedure GetMetrics(var fontFaceMetrics: TDwriteFontMetrics); stdcall;

    function GetGlyphCount: UINT16; stdcall;

    function GetDesignGlyphMetrics(glyphIndices: PWord; glyphCount: Cardinal;
      glyphMetrics: PDwriteGlyphMetrics; isSideways: BOOL = False): HResult; stdcall;

    function GetGlyphIndices(var codePoints: Cardinal; codePointCount: Cardinal;
      var glyphIndices: Word): HResult; stdcall;

    function TryGetFontTable(openTypeTableTag: Cardinal; var tableData: Pointer;
      var tableSize: Cardinal; var tableContext: Pointer;
      var exists: BOOL): HResult; stdcall;

    procedure ReleaseFontTable(tableContext: Pointer); stdcall;

    function GetGlyphRunOutline(emSize: Single; const glyphIndices: PWord;
      const glyphAdvances: PSingle; const glyphOffsets: PDwriteGlyphOffset;
      glyphCount: Cardinal; isSideways: BOOL; isRightToLeft: BOOL;
      geometrySink: IDWriteGeometrySink): HResult; stdcall;

    function GetRecommendedRenderingMode(emSize: Single; pixelsPerDip: Single;
      measuringMode: TDWriteMeasuringMode;
      const renderingParams: IDWriteRenderingParams;
      var renderingMode: TDWriteRenderingMode): HResult; stdcall;

    function GetGdiCompatibleMetrics(emSize: Single; pixelsPerDip: Single;
      transform: PDwriteMatrix; var fontFaceMetrics: DWRITE_FONT_METRICS): HResult; stdcall;

    function GetGdiCompatibleGlyphMetrics(emSize: single; pixelsPerDip: single;
      transform: PDwriteMatrix; useGdiNatural: BOOL; glyphIndices: PWord;
      glyphCount: Cardinal; fontFaceMetrics: PDwriteGlyphMetrics;
      isSideways: BOOL = False): HResult; stdcall;  end;
{$EXTERNALSYM IDWriteFontFace}
{$ENDREGION}

{$REGION 'ID2D1GdiInteropRenderTarget Redeclaration'}
  ID2D1GdiInteropRenderTarget = interface(IUnknown)
    [SID_ID2D1GdiInteropRenderTarget]
    function GetDC(mode: D2D1_DC_INITIALIZE_MODE; out hdc: HDC): HResult; stdcall;

    function ReleaseDC(update: PRect): HResult; stdcall;
  end;
{$EXTERNALSYM ID2D1GdiInteropRenderTarget}
{$ENDREGION}

{$REGION 'DWrite_1.h DWrite_2.h DWrite_3.h'}
  IDWriteFont1 = interface(IDWriteFont)
    ['{acd16696-8c14-4f5d-877e-fe3fc1d32738}']
    procedure GetMetrics(fontMetrics: Pointer); stdcall; //DWRITE_FONT_METRICS1 not translated
    procedure GetPanose(panose: Pointer); stdcall; // panose structure not translated
    function GetUnicodeRanges(maxRangeCount: UINT32; unicodeRanges: Pointer; // DWRITE_UNICODE_RANGE not tranlsated
        out actualRangeCount: UINT32): HResult; stdcall;
    function IsMonospacedFont(): longbool; stdcall;
  end;
  {$EXTERNALSYM IDWriteFont1}

  IDWriteTextLayout1 = interface(IDWriteTextLayout)
    ['{9064D822-80A7-465C-A986-DF65F78B8FEB}']
    function SetPairKerning(isPairKerningEnabled: longbool; textRange: TDwriteTextRange): HResult; stdcall;
    function GetPairKerning(currentPosition: UINT32; out isPairKerningEnabled: longbool;
        out textRange: TDwriteTextRange): HResult; stdcall;
    function SetCharacterSpacing(leadingSpacing: single; trailingSpacing: single; minimumAdvanceWidth: single;
        textRange: TDwriteTextRange): HResult; stdcall;
    function GetCharacterSpacing(currentPosition: UINT32; out leadingSpacing: single; out trailingSpacing: single;
        out minimumAdvanceWidth: single; out textRange: TDwriteTextRange): HResult; stdcall;
  end;
  {$EXTERNALSYM IDWriteTextLayout1}
{$ENDREGION}

  TSynDWrite = class
  private
    class var SingletonD2DFactory: ID2D1Factory;
    class var SingletonRenderTarget: ID2D1DCRenderTarget;
    class var SingletonDWriteFactory: IDWriteFactory;
    class var SingletonGDIInterop: IDWriteGdiInterop;
    class var SingletonDottedStrokeStyle: ID2D1StrokeStyle;
    class var SingletonImagingFactory: IWICImagingFactory;
    class var FSolidBrushes: TDictionary<TD2D1ColorF, ID2D1SolidColorBrush>;
    class var FGradientGutterBrush: ID2D1LinearGradientBrush;
    class var FGradientBrushStartColor, FGradientBrushEndColor: TColor;
  public
    class function D2DFactory(factoryType: TD2D1FactoryType=D2D1_FACTORY_TYPE_SINGLE_THREADED;
      factoryOptions: PD2D1FactoryOptions=nil): ID2D1Factory; static;
    class function RenderTarget: ID2D1DCRenderTarget; static;
    class function DWriteFactory: IDWriteFactory; static;
    class function GDIInterop: IDWriteGdiInterop; static;
    class function ImagingFactory: IWICImagingFactory; static;
    class function SolidBrush(Color: TColor): ID2D1SolidColorBrush; overload; static;
    class function SolidBrush(Color: TD2D1ColorF): ID2D1SolidColorBrush; overload; static;
    class function DottedStrokeStyle: ID2D1StrokeStyle; static;
    class function GradientGutterBrush(StartColor, EndColor: TColor): ID2D1LinearGradientBrush;
    class procedure ResetRenderTarget; static;
  end;

  TSynTextFormat = record
  private
    FIDW: IDWriteTextFormat;
    FCharExtra: Cardinal;
    FUseGDINatural: Boolean;
    FCharWidth: Cardinal;
    FLineHeight: Cardinal;
  public
    constructor Create(AFont: TFont; TabWidth, CharExtra, LineSpacingExtra: Cardinal);
    property IDW: IDWriteTextFormat read FIDW;
    property CharWidth: Cardinal read FCharWidth;
    property LineHeight: Cardinal read FLineHeight;
    property CharExtra: Cardinal read FCharExtra;
    property UseGDINatural: Boolean read FUseGDINatural;
  end;

  TSynTypography = (typEmpty, typDefault, typNoLigatures);

  TSynTextLayout = record
  private
    FIDW: IDWriteTextLayout;
    FCount: Integer;
  public
    TextOptions: D2D1_DRAW_TEXT_OPTIONS;
    property IDW: IDWriteTextLayout read FIDW;
    constructor Create(TextFormat: TSynTextFormat; Text: PChar; const Count,
        LayoutWidth, layoutHeight: Cardinal; WordWrap: Boolean = False;
        PixelsPerDip: Single = 1);
    procedure SetFontStyle(FontStyles: System.UITypes.TFontStyles; const Start,
        Count: Integer);
    procedure SetFontColor(Color: TD2D1ColorF; const Start, Count: Integer); overload;
    procedure SetFontColor(Color: TColor; const Start, Count: Integer); overload;
    procedure SetTypography(Typography: TSynTypography; const Start, Count: Integer);
    procedure Draw(RT: ID2D1RenderTarget; X, Y: Integer; FontColor: TColor; Alpha: Single = 1);
    procedure DrawClipped(RT: ID2D1RenderTarget; X, Y: Integer; ClipRect: TRect;
      FontColor: TColor; Alpha: Single = 1);
    function TextMetrics: TDwriteTextMetrics;
  end;

  ISynWicRenderTarget = interface
    ['{1142A46F-9BF4-449C-9C4A-A22B19716202}']
    function GetIDW: ID2D1RenderTarget;
    property IDW: ID2D1RenderTarget read GetIDW;
  end;

  TSynWicRenderTarget = class(TInterfacedObject, ISynWicRenderTarget)
  private
    FWicBitmap: IWICBitmap;
    FIDW: ID2D1RenderTarget;
    function GetIDW: ID2D1RenderTarget;
  public
    constructor Create(const Width, Height: integer);
  end;

  function SynWicRenderTarget(const Width, Height: integer): ISynWicRenderTarget;

type
  TGraphemeEnumerator = record
  private
    FTextLayout: IDWriteTextLayout;
    FStart: integer;
    FString: string;
    FCurrent: string;
  public
    constructor Create(const AValue: string);
    function MoveNext: Boolean;
    function GetCurrent: string; inline;
    property Current: string read GetCurrent;
  end;

  TGraphemeEnumeratorHelper = record
  private
    FString: string;
  public
    constructor Create(const AValue: string);
    function  GetEnumerator: TGraphemeEnumerator;
  end;

function Graphemes(const AValue: string): TGraphemeEnumeratorHelper;

// Support functions
function D2D1ColorF(const AColor: TColor): TD2D1ColorF; overload;
function D2D1ColorF(const AColor: TColor; Opacity: Single): TD2D1ColorF; overload;
function DWTextRange(startPosition: Cardinal; length: Cardinal): TDwriteTextRange;
function DWFontFeature(nameTag: DWRITE_FONT_FEATURE_TAG; parameter: Cardinal): TDwriteFontFeature;
function DWGetTypography(Features: array of Integer) : IDWriteTypography;
function WicBitmapFromBitmap(Bitmap: TBitmap): IWICBitmap;
function ScaledWicBitmap(Source: IWICBitmap;
  const ScaledWidth, ScaledHeight: Integer): IWICBitmap;
procedure ImageListDraw(RT: ID2D1RenderTarget; IL: TCustomImageList; X, Y,
    Index: Integer);
function IsFontMonospacedAndValid(Font: TFont): Boolean;
function FontFamilyName(Font: IDWriteFont): string;

var
  clNoneF: TD2D1ColorF;

implementation

Uses
  Winapi.CommCtrl,
  Winapi.DxgiFormat,
  System.Math,
  Vcl.Forms,
  SynUnicode,
  SynEditTypes,
  SynEditMiscProcs;

{$REGION 'Support functions'}
function D2D1ColorF(const AColor: TColor): TD2D1ColorF; overload;
var
  RGB: Cardinal;
const
  CScale = 1 / 255;
begin
  RGB := ColorToRGB(AColor);
  Result.r := TColors(RGB).R * CScale;
  Result.g := TColors(RGB).G * CScale;
  Result.b := TColors(RGB).B * CScale;
  Result.a :=  1.0;
end;

function D2D1ColorF(const AColor: TColor; Opacity: Single): TD2D1ColorF; overload;
begin
  Result := D2D1ColorF(AColor);
  Result.a := Opacity;
end;

function DWTextRange(startPosition: Cardinal; length: Cardinal): TDwriteTextRange;
begin
  // startPosition is zero-based
  Result.startPosition := startPosition - 1;
  Result.Length := length;
end;

function DWFontFeature(nameTag: DWRITE_FONT_FEATURE_TAG; parameter: Cardinal): TDwriteFontFeature;
begin
  Result.nameTag := nameTag;
  Result.parameter := parameter;
end;

function DWGetTypography(Features: array of Integer) : IDWriteTypography;
var
  Feature: Integer;
begin
  CheckOSError(TSynDWrite.DWriteFactory.CreateTypography(Result));
  for Feature in Features do
    CheckOSError(Result.AddFontFeature(DWFontFeature(Feature, 1)));
end;

function WicBitmapFromBitmap(Bitmap: TBitmap): IWICBitmap;
begin
  Assert(Bitmap.PixelFormat = pf32bit);
  Bitmap.AlphaFormat := afDefined;
  CheckOSError(TSynDWrite.ImagingFactory.CreateBitmapFromHBITMAP(Bitmap.Handle,
    0, WICBitmapUsePremultipliedAlpha, Result));
end;

function ScaledWicBitmap(Source: IWICBitmap;
  const ScaledWidth, ScaledHeight: Integer): IWICBitmap;
var
  Scaler: IWICBitmapScaler;
begin
  TSynDWrite.ImagingFactory.CreateBitmapScaler(Scaler);
  Scaler.Initialize(Source, ScaledWidth, ScaledHeight,
    WICBitmapInterpolationModeHighQualityCubic);
  Result := IWICBitmap(Scaler);
end;

procedure ImageListDraw(RT: ID2D1RenderTarget; IL: TCustomImageList;
  X, Y, Index: Integer);
var
  Icon: HIcon;
  WicBitmap: IWicBitmap;
  Bitmap: ID2D1Bitmap;
  R: TRectF;
  BMProps: TD2D1BitmapProperties;
begin
  Icon := ImageList_GetIcon(IL.Handle, Index, ILD_NORMAL);
  try
    CheckOSError(TSynDWrite.ImagingFactory.CreateBitmapFromHICON(Icon, WicBitmap));
    BMProps.dpiX := 1;
    BMProps.dpiY := 1;
    BMProps.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
    BMProps.pixelFormat.alphaMode :=  D2D1_ALPHA_MODE_PREMULTIPLIED;
    CheckOSError(RT.CreateBitmapFromWicBitmap(WicBitmap, @BMProps, Bitmap));
    R := Rect(X, Y, X + IL.Width, Y + IL.Height);
    RT.DrawBitmap(Bitmap, @R);
  finally
    DestroyIcon(Icon);
  end;
end;

function IsFontMonospacedAndValid(Font: TFont): Boolean;
var
  LogFont: TLogFont;
  DWFont: IDWriteFont;
begin
  if GetObject(Font.Handle, SizeOf(TLogFont), @LogFont) = 0 then
    Exit(False);
  try
    CheckOSError(TSynDWrite.GDIInterop.CreateFontFromLOGFONT(LogFont, DWFont));
    Result := (DWFont as IDWriteFont1).IsMonospacedFont;
    if (FontFamilyName(DWFont) <> Font.Name) and (fsBold in Font.Style) then
      Font.Style := Font.Style - [fsBold];
  except
    Exit(False);
  end;
end;

function FontFamilyName(Font: IDWriteFont): string;
var
  FontFamily: IDWriteFontFamily;
  Names: IDWriteLocalizedStrings;
  Index: Cardinal;
  Exists: BOOL;
  NameLength: Cardinal;
begin
  Result := '';

  CheckOSError(Font.GetFontFamily(FontFamily));
  CheckOSError(FontFamily.GetFamilyNames(Names));
  if Names.GetCount > 0 then
  begin
    CheckOSError(Names.FindLocaleName(UserLocaleName, Index, Exists));
    if not Exists then
    begin
      CheckOSError(Names.FindLocaleName('en-us', Index, Exists));
      if not Exists then
        Index := 0;
    end;
    CheckOSError(Names.GetStringLength(Index, NameLength));
    SetLength(Result, NameLength);
    CheckOSError(Names.GetString(Index, PChar(Result), NameLength + 1));
  end
  else
    raise ESynError.Create('Font family name not found');
end;

{$ENDREGION}

{$REGION 'TSynDWrite'}

class function TSynDWrite.D2DFactory(factoryType: TD2D1FactoryType;
  factoryOptions: PD2D1FactoryOptions): ID2D1Factory;
var
  LD2DFactory: ID2D1Factory;
begin
  if SingletonD2DFactory = nil then
  begin
    if not Succeeded(D2D1CreateFactory(factoryType, IID_ID2D1Factory,
      factoryOptions, LD2DFactory))
    then
      RaiseLastOSError;
    if InterlockedCompareExchangePointer(Pointer(SingletonD2DFactory), Pointer(LD2DFactory), nil) = nil then
      LD2DFactory._AddRef;
  end;
  Result := SingletonD2DFactory;
end;

class function TSynDWrite.DottedStrokeStyle: ID2D1StrokeStyle;
var
  LocalStrokeStyle: ID2D1StrokeStyle;
begin
  if SingletonDottedStrokeStyle = nil then
  begin
    CheckOSError(D2DFactory.CreateStrokeStyle(
      D2D1StrokeStyleProperties(D2D1_CAP_STYLE_ROUND, D2D1_CAP_STYLE_ROUND,
        D2D1_CAP_STYLE_ROUND, D2D1_LINE_JOIN_MITER, 10, D2D1_DASH_STYLE_DOT, -0.5),
        nil, 0, LocalStrokeStyle));
    if InterlockedCompareExchangePointer(Pointer(SingletonDottedStrokeStyle),
      Pointer(LocalStrokeStyle), nil) = nil
    then
      SingletonDottedStrokeStyle._AddRef;
  end;
  Result := SingletonDottedStrokeStyle;
end;

class function TSynDWrite.DWriteFactory: IDWriteFactory;
var
  LocalDWriteFactory: IUnknown;
begin
  if SingletonDWriteFactory = nil then
  begin
    CheckOSError(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IID_IDWriteFactory,
      LocalDWriteFactory));
    if InterlockedCompareExchangePointer(Pointer(SingletonDWriteFactory),
      Pointer(LocalDWriteFactory), nil) = nil
    then
      SingletonDWriteFactory._AddRef;
  end;
  Result := SingletonDWriteFactory;
end;

class function TSynDWrite.GDIInterop: IDWriteGdiInterop;
var
  LocalGDIInterop: IDWriteGdiInterop;
begin
  if SingletonGDIInterop = nil then
  begin
    CheckOSError(DWriteFactory.GetGdiInterop(LocalGDIInterop));
    if InterlockedCompareExchangePointer(Pointer(SingletonGDIInterop),
      Pointer(LocalGDIInterop), nil) = nil
    then
      SingletonGDIInterop._AddRef;
  end;
  Result := SingletonGDIInterop;
end;

class function TSynDWrite.GradientGutterBrush(StartColor, EndColor: TColor):
    ID2D1LinearGradientBrush;
var
  BrushProperties: TD2D1BrushProperties;
  Stops: array [0..1] of TD2D1GradientStop;
  GradientStopCollection: ID2D1GradientStopCollection;
begin
  if (FGradientGutterBrush = nil) or (StartColor <> FGradientBrushStartColor) or
    (EndColor <> FGradientBrushEndColor) then
  begin
    // store colors
    FGradientBrushStartColor := StartColor;
    FGradientBrushEndColor := EndColor;

    BrushProperties.opacity := 1;
    BrushProperties.transform := TD2DMatrix3X2F.Identity;

    Stops[0].position := 0;
    Stops[1].position := 1;
    Stops[0].color := D2D1ColorF(StartColor);
    Stops[1].color := D2D1ColorF(EndColor);
    CheckOSError(RenderTarget.CreateGradientStopCollection(
    @Stops[0], 2, D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_CLAMP,
    GradientStopCollection));

    CheckOSError(RenderTarget.CreateLinearGradientBrush(
      D2D1LinearGradientBrushProperties(Point(0, 0), Point(1, 0)),
      @BrushProperties, GradientStopCollection,
      FGradientGutterBrush));
  end;
  Result := FGradientGutterBrush;
end;

class function TSynDWrite.ImagingFactory: IWICImagingFactory;
var
  ImgFactory: IWICImagingFactory;
begin
  if SingletonImagingFactory = nil then
  begin
    CheckOSError(CoCreateInstance(CLSID_WICImagingFactory, nil,
     CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown, ImgFactory));
    if InterlockedCompareExchangePointer(Pointer(SingletonImagingFactory),
      Pointer(ImgFactory), nil) = nil
    then
      SingletonImagingFactory._AddRef;
  end;
  Result := SingletonImagingFactory;
end;

class function TSynDWrite.RenderTarget: ID2D1DCRenderTarget;
Var
  RT: ID2D1DCRenderTarget;
begin
  if SingletonRenderTarget = nil then
  begin
    CheckOSError(D2DFactory.CreateDCRenderTarget(
      D2D1RenderTargetProperties(
        {$IFDEF GPUSupport}
        D2D1_RENDER_TARGET_TYPE_DEFAULT,
        {$ELSE}
        D2D1_RENDER_TARGET_TYPE_SOFTWARE, // much faster in my desktop with a slow GPU
        {$ENDIF}
        D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED),
        0, 0, D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE),
        RT));
    if InterlockedCompareExchangePointer(Pointer(SingletonRenderTarget),
      Pointer(RT), nil) = nil
    then
    begin
      SingletonRenderTarget._AddRef;
      SingletonRenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      SingletonRenderTarget.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE);
    end;
  end;
  Result := SingletonRenderTarget;
end;

class procedure TSynDWrite.ResetRenderTarget;
begin
  FSolidBrushes.Clear;
  FGradientGutterBrush := nil;
  SingletonRenderTarget := nil;
end;

class function TSynDWrite.SolidBrush(Color: TD2D1ColorF): ID2D1SolidColorBrush;
begin
  if FSolidBrushes = nil then
    FSolidBrushes := TDictionary<TD2D1ColorF, ID2D1SolidColorBrush>.Create;
  if FSolidBrushes.ContainsKey(Color) then
    Exit(FSolidBrushes[Color]);

  CheckOSError(RenderTarget.CreateSolidColorBrush(Color, nil, Result));
  FSolidBrushes.Add(Color, Result);
end;

class function TSynDWrite.SolidBrush(Color: TColor): ID2D1SolidColorBrush;
begin
  Result := SolidBrush(D2D1ColorF(Color));
end;
{$ENDREGION}

{$REGION 'Text Element Enumberator'}
{ TGraphemeEnumerator }

constructor TGraphemeEnumerator.Create(const AValue: string);
var
  TextFormat: IDWriteTextFormat;
begin
  FString:= AValue;
  FStart := 0;
  CheckOSError(TSynDWrite.DWriteFactory.CreateTextFormat('Segoe UI', nil,
    DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL,
    MulDiv(9, 96, 72), UserLocaleName, TextFormat));
  CheckOSError(TSynDWrite.DWriteFactory.CreateTextLayout(PChar(FString), FString.Length,
    TextFormat, MaxInt, MaxInt, FTextLayout));
end;

function TGraphemeEnumerator.GetCurrent: string;
begin
  Result := FCurrent;
end;

function TGraphemeEnumerator.MoveNext: Boolean;
var
  X, Y: Single;
  HTM: TDwriteHitTestMetrics;
begin
  if FStart >= FString.Length then Exit(False);
  FTextLayout.HitTestTextPosition(FStart, True, X, Y, HTM);
  FCurrent := Copy(FString, FStart + 1, HTM.Length);
  Inc(FStart, HTM.Length);
  Result := True;
end;

{ TGraphemeEnumeratorHelper }

constructor TGraphemeEnumeratorHelper.Create(const AValue: string);
begin
  FString := AValue;
end;

function TGraphemeEnumeratorHelper.GetEnumerator: TGraphemeEnumerator;
begin
   Result.Create(FString);
end;

function Graphemes(const AValue: string): TGraphemeEnumeratorHelper;
begin
  Result.Create(AValue);
end;
{$ENDREGION}

{ TSynTextFormat }

constructor TSynTextFormat.Create(AFont: TFont; TabWidth, CharExtra,
    LineSpacingExtra: Cardinal);
var
  DWFontStyle: DWRITE_FONT_STYLE;
  DWFont: IDWriteFont;
  LogFont: TLogFont;
  FontMetrics: TDwriteFontMetrics;
  FontFace: WinApi.D2D1.IDWriteFontFace;
  CodePoint: Cardinal;
  FontIndex: Word;
  GlyphMetrics: TDwriteGlyphMetrics;
  //Trimming: TDwriteTrimming;
  Baseline: Single;
begin
  FUseGDINatural := AFont.Quality = TFontQuality.fqClearTypeNatural;
  GetObject(AFont.Handle, SizeOf(TLogFont), @LogFont);
  LogFont.lfWeight := GetCorrectFontWeight(AFont);

  CheckOSError(TSynDWrite.GDIInterop.CreateFontFromLOGFONT(LogFont, DWFont));
  CheckOSError(DWFont.CreateFontFace(FontFace));
  FontFace.GetGdiCompatibleMetrics(-AFont.Height, 1, PDwriteMatrix(nil)^, FontMetrics);
  CodePoint := Ord('W');
  FontFace.GetGlyphIndices(CodePoint, 1, FontIndex);
  IDWriteFontFace(FontFace).GetGdiCompatibleGlyphMetrics(
    -AFont.Height, 1, nil, UseGDINatural,
    @FontIndex, 1, @GlyphMetrics);

  // Split LineSpacingExtra between top and bottom
  FLineHeight := Round(
    (FontMetrics.ascent + FontMetrics.descent + FontMetrics.lineGap) *
    (-AFont.Height) / FontMetrics.designUnitsPerEm) +
    (LineSpacingExtra div 2) * 2;
  FCharWidth := Round(-GlyphMetrics.advanceWidth *
    AFont.Height / FontMetrics.designUnitsPerEm) + (CharExtra div 2) * 2;
  Baseline := Round(FontMetrics.ascent * (-AFont.Height) /
    FontMetrics.designUnitsPerEm) + (LineSpacingExtra div 2);

  if TFontStyle.fsItalic in AFont.Style then
    DWFontStyle := DWRITE_FONT_STYLE_ITALIC
  else
    DWFontStyle := DWRITE_FONT_STYLE_NORMAL;

  CheckOSError(TSynDWrite.DWriteFactory.CreateTextFormat(
    PChar(FontFamilyName(DWFont)), nil,
    DWFont.GetWeight, DWFontStyle, DWRITE_FONT_STRETCH_NORMAL,
    -AFont.Height, UserLocaleName, FIDW));
  FIDW.SetIncrementalTabStop(TabWidth * FCharWidth);

//  Trimming.granularity := DWRITE_TRIMMING_GRANULARITY_CHARACTER;
//  Trimming.delimiter := 0;
//  Trimming.delimiterCount := 0;
//  CheckOSError(FIDW.SetTrimming(Trimming, nil));
  FIDW.SetLineSpacing(DWRITE_LINE_SPACING_METHOD_UNIFORM, LineHeight, Baseline);
end;

{ TSynTextLayout }

constructor TSynTextLayout.Create(TextFormat: TSynTextFormat; Text: PChar;
    const Count, LayoutWidth, layoutHeight: Cardinal; WordWrap: Boolean =
    False; PixelsPerDip: Single = 1);
var
  TextLayout1: IDWriteTextLayout1;
begin
  FCount := Count;
  CheckOSError(TSynDWrite.DWriteFactory.CreateGdiCompatibleTextLayout(Text,
    Count, TextFormat.FIDW, LayoutWidth, LayoutHeight,
    PixelsPerDip, nil, TextFormat.UseGDINatural, FIDW));
  if (TextFormat.CharExtra > 0) and
    Supports(FIDW, IDWriteTextLayout1, TextLayout1)
  then
    CheckOSError(TextLayout1.SetCharacterSpacing(TextFormat.CharExtra / 2,
      TextFormat.CharExtra / 2, 0, DWTextRange(1, Count)));
  if not WordWrap then
    FIDW.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP)
  else if TOSVersion.Check(6, 3) then  // 8.1 or higher
    FIDW.SetWordWrapping(DWRITE_WORD_WRAPPING_EMERGENCY_BREAK)
  else
    FIDW.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);

  TextOptions := D2D1_DRAW_TEXT_OPTIONS_CLIP;
  if TOSVersion.Check(6, 3) then
    TextOptions := TextOptions + D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT;
end;

procedure TSynTextLayout.Draw(RT: ID2D1RenderTarget; X, Y: Integer; FontColor:
    TColor; Alpha: Single = 1);
begin
  RT.DrawTextLayout(D2D1PointF(X, Y), FIDW,
    TSynDWrite.SolidBrush(D2D1ColorF(FontColor, Alpha)), TextOptions);
end;

procedure TSynTextLayout.DrawClipped(RT: ID2D1RenderTarget; X, Y: Integer;
    ClipRect: TRect; FontColor: TColor; Alpha: Single = 1);
begin
  RT.PushAxisAlignedClip(ClipRect, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
  Draw(RT, X, Y, FontColor, Alpha);
  RT.PopAxisAlignedClip;
end;

function TSynTextLayout.TextMetrics: TDwriteTextMetrics;
begin
  CheckOSError(FIDW.GetMetrics(Result));
end;

procedure TSynTextLayout.SetFontColor(Color: TD2D1ColorF; const Start,
  Count: Integer);
var
  Range: TDwriteTextRange;
  FirstChar, LastChar: Cardinal;
begin
  LastChar := Min(FCount, Start + Count - 1);
  FirstChar := Max(Start, 1);
  if FirstChar > LastChar then Exit;
  Range := DWTextRange(FirstChar, LastChar - FirstChar + 1);

  FIDW.SetDrawingEffect(TSynDWrite.SolidBrush(Color), Range);
end;

procedure TSynTextLayout.SetFontColor(Color: TColor; const Start, Count:
    Integer);
begin
  SetFontColor(D2D1ColorF(Color), Start, Count);
end;

procedure TSynTextLayout.SetFontStyle(FontStyles: System.UITypes.TFontStyles;
    const Start, Count: Integer);
var
  Range: TDwriteTextRange;
  FirstChar, LastChar: Cardinal;
begin
  LastChar := Min(FCount, Start + Count - 1);
  FirstChar := Max(Start, 1);
  if FirstChar > LastChar then Exit;
  Range := DWTextRange(FirstChar, LastChar - FirstChar + 1);

  if fsBold in FontStyles then
    FIDW.SetFontWeight(DWRITE_FONT_WEIGHT_BOLD, Range);
  if fsItalic in FontStyles then
    FIDW.SetFontStyle(DWRITE_FONT_STYLE_ITALIC, Range);
  if fsUnderline in FontStyles then
    FIDW.SetUnderline(True, Range);
  if fsStrikeOut in FontStyles then
    FIDW.SetStrikethrough(True, Range);
end;

procedure TSynTextLayout.SetTypography(Typography: TSynTypography; const Start,
    Count: Integer);
const
  DefaultTypoFeatures: array[0..7] of integer =
  (DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES,             // clig
   DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES,            // calt
   DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION,  // ccmp
   DWRITE_FONT_FEATURE_TAG_DISCRETIONARY_LIGATURES,          // dlig
   DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES,               // liga
   DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING,                 // mark
   DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING,         // mkmk
   DWRITE_FONT_FEATURE_TAG_REQUIRED_LIGATURES);              // rlig
  TypoFeaturesNoLigatures: array[0..2] of integer =
  (DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION,  // ccmp
   DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING,                 // mark
   DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING);        // salt

var
  DWTypography: IDWriteTypography;
  Range: TDwriteTextRange;
  FirstChar, LastChar: Cardinal;
begin
  LastChar := Min(FCount, Start + Count - 1);
  FirstChar := Max(Start, 1);
  if FirstChar > LastChar then Exit;
  Range := DWTextRange(FirstChar, LastChar - FirstChar + 1);

  case Typography of
    typEmpty: DWTypography := DWGetTypography([]);
    typDefault: DWTypography := DWGetTypography(DefaultTypoFeatures);
    typNoLigatures: DWTypography := DWGetTypography(TypoFeaturesNoLigatures);
  end;
  FIDW.SetTypography(DWTypography, Range);
end;

{ TSynWICRenderTarget }

constructor TSynWICRenderTarget.Create(const Width, Height: integer);
var
  RenderTargetProp: TD2D1RenderTargetProperties;
begin
  inherited Create;
  CheckOSError(TSynDWrite.ImagingFactory.CreateBitmap(Width, Height,
    @GUID_WICPixelFormat32bppPBGRA, WICBitmapCacheOnDemand, FWicBitmap));

  RenderTargetProp :=
    D2D1RenderTargetProperties(
      {$IFDEF GPUSupport}
      D2D1_RENDER_TARGET_TYPE_DEFAULT,
      {$ELSE}
      D2D1_RENDER_TARGET_TYPE_SOFTWARE, // much faster in my desktop with a slow GPU
      {$ENDIF}
      D2D1PixelFormat(DXGI_FORMAT_UNKNOWN, D2D1_ALPHA_MODE_UNKNOWN), // use image format
      0, 0, D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE);

  CheckOSError(TSynDWrite.D2DFactory.CreateWicBitmapRenderTarget(FWicBitmap,
    RenderTargetProp,FIDW));
end;

function TSynWicRenderTarget.GetIDW: ID2D1RenderTarget;
begin
  Result := FIDW;
end;

function SynWicRenderTarget(const Width, Height: integer): ISynWicRenderTarget;
begin
  Result := TSynWicRenderTarget.Create(Width, Height);
end;

initialization
  clNoneF := D2D1ColorF(0, 0, 0, 0);
finalization
  // Delphi 10.1 does not support class destructors
  TSynDWrite.FSolidBrushes.Free;
end.
