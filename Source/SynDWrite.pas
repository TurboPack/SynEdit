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
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics;

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
      var renderingParams: IDWriteRenderingParams;
      var renderingMode: TDWriteRenderingMode): HResult; stdcall;

    function GetGdiCompatibleMetrics(emSize: Single; pixelsPerDip: Single;
      transform: PDwriteMatrix; var fontFaceMetrics: DWRITE_FONT_METRICS): HResult; stdcall;

    function GetGdiCompatibleGlyphMetrics(emSize: single; pixelsPerDip: single;
      transform: PDwriteMatrix; useGdiNatural: BOOL; glyphIndices: PWord;
      glyphCount: Cardinal; fontFaceMetrics: PDwriteGlyphMetrics;
      isSideways: BOOL = False): HResult; stdcall;  end;
{$ENDREGION}

{$REGION 'DWrite_1.h DWrite_2.h DWrite_3.h'}
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
{$ENDREGION}

  TSynDWrite = class
  strict private
    class var SingletonD2DFactory: ID2D1Factory;
    class var SingletonRenderTarget: ID2D1DCRenderTarget;
    class var SingletonDWriteFactory: IDWriteFactory;
    class var SingletonGDIInterop: IDWriteGdiInterop;
    class var FSolidBrushes: TDictionary<TColor, ID2D1SolidColorBrush>;
  public
    class function D2DFactory(factoryType: TD2D1FactoryType=D2D1_FACTORY_TYPE_SINGLE_THREADED;
      factoryOptions: PD2D1FactoryOptions=nil): ID2D1Factory; static;
    class function RenderTarget: ID2D1DCRenderTarget; static;
    class function DWriteFactory: IDWriteFactory;
    class function GDIInterop: IDWriteGdiInterop;
    class function SolidBrush(Color: TColor): ID2D1SolidColorBrush;
    class destructor Destroy;
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
  public
    TextOptions: D2D1_DRAW_TEXT_OPTIONS;
    property IDW: IDWriteTextLayout read FIDW;
    constructor Create(TextFormat: TSynTextFormat; Text: PChar; const Count,
        LayoutWidth, layoutHeight: Cardinal; WordWrap: Boolean = False;
        PixelsPerDip: Single = 1);
    procedure SetFontStyle(FontStyles: System.UITypes.TFontStyles; const Start,
        Count: Cardinal);
    procedure SetFontColor(Color: TColor; const Start, Count: Cardinal);
    procedure SetTypography(Typography: TSynTypography; const Start, Count: Cardinal);
    procedure Draw(RT: ID2D1RenderTarget; X, Y: Integer; FontColor: TColor);
    procedure DrawClipped(RT: ID2D1RenderTarget; X, Y: Integer; ClipRect: TRect;
      FontColor: TColor);
  end;

  ISynWicRenderTarget = interface
    ['{1142A46F-9BF4-449C-9C4A-A22B19716202}']
    function GetIDW: ID2D1RenderTarget;
    function GetWicImage: TWICImage;
    property IDW: ID2D1RenderTarget read GetIDW;
    property WicImage: TWICImage read GetWicImage;
  end;

  TSynWicRenderTarget = class(TInterfacedObject, ISynWicRenderTarget)
  private
    FWicImage: TWICImage;
    FIDW: ID2D1RenderTarget;
    function GetIDW: ID2D1RenderTarget;
    function GetWicImage: TWICImage;
  public
    constructor Create(const Width, Height: integer);
    destructor Destroy; override;
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
function DWTextRange(startPosition: Cardinal; length: Cardinal): TDwriteTextRange;
function DWFontFeature(nameTag: DWRITE_FONT_FEATURE_TAG; parameter: Cardinal): TDwriteFontFeature;
function DWGetTypography(Features: array of Integer) : IDWriteTypography;

var
  DefaultLocaleName: array [0..LOCALE_NAME_MAX_LENGTH - 1] of Char;

implementation

Uses
  Winapi.DxgiFormat;

{$REGION 'Support functions'}
function D2D1ColorF(const AColor: TColor): TD2D1ColorF; overload;
var
  RGB: Cardinal;
const
  CScale = 1 / 255;
begin
  RGB := TColorRec.ColorToRGB(AColor);
  Result.r :=   RGB         and $FF  * CScale;
  Result.g := ((RGB shr  8) and $FF) * CScale;
  Result.b := ((RGB shr 16) and $FF) * CScale;
  Result.a :=  1.0;
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
begin
  CheckOSError(TSynDWrite.DWriteFactory.CreateTypography(Result));
  for var Feature in Features do
    CheckOSError(Result.AddFontFeature(DWFontFeature(Feature, 1)));
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

class destructor TSynDWrite.Destroy;
begin
  FSolidBrushes.Free;
end;

class function TSynDWrite.DWriteFactory: IDWriteFactory;
var
  LocalDWriteFactory: IUnknown;
begin
  if SingletonDWriteFactory = nil then
  begin
    if not Succeeded(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IID_IDWriteFactory,
      LocalDWriteFactory))
    then
      RaiseLastOSError;
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
    if not Succeeded(DWriteFactory.GetGdiInterop(LocalGDIInterop))
    then
      RaiseLastOSError;
    if InterlockedCompareExchangePointer(Pointer(SingletonGDIInterop),
      Pointer(LocalGDIInterop), nil) = nil
    then
      SingletonGDIInterop._AddRef;
  end;
  Result := SingletonGDIInterop;
end;

class function TSynDWrite.RenderTarget: ID2D1DCRenderTarget;
Var
  RT: ID2D1DCRenderTarget;
begin
  if SingletonRenderTarget = nil then
  begin
    if not Succeeded(D2DFactory.CreateDCRenderTarget(
      D2D1RenderTargetProperties(
        {$IFDEF GPUSupport}
        D2D1_RENDER_TARGET_TYPE_DEFAULT,
        {$ELSE}
        D2D1_RENDER_TARGET_TYPE_SOFTWARE, // much faster in my desktop with a slow GPU
        {$ENDIF}
        D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED),
        0, 0, D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE),
        RT))
    then
      RaiseLastOSError;
    if InterlockedCompareExchangePointer(Pointer(SingletonRenderTarget),
      Pointer(RT), nil) = nil
    then
      SingletonRenderTarget._AddRef;
  end;
  Result := SingletonRenderTarget;
end;

class function TSynDWrite.SolidBrush(Color: TColor): ID2D1SolidColorBrush;
var
  RGB: TColor;
begin
  if FSolidBrushes = nil then
    FSolidBrushes := TDictionary<TColor, ID2D1SolidColorBrush>.Create;
  RGB := ColorToRGB(Color);
  if FSolidBrushes.ContainsKey(RGB) then
    Exit(FSolidBrushes[RGB]);

  CheckOSError(RenderTarget.CreateSolidColorBrush(D2D1ColorF(RGB), nil, Result));
  FSolidBrushes.Add(RGB, Result);
end;
{$ENDREGION}

{$REGION 'Text Element Enumberator'}
{ TGraphemeEnumerator }

constructor TGraphemeEnumerator.Create(const AValue: string);
var
  D2DLocale: array [0..84] of Char;
  TextFormat: IDWriteTextFormat;
begin
  FString:= AValue;
  FStart := 0;
  if LCIDToLocaleName(GetUserDefaultLCID, D2DLocale, 85, 0) = 0 then
    RaiseLastOSError;
  CheckOSError(TSynDWrite.DWriteFactory.CreateTextFormat('Segoe UI', nil,
    DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL,
    MulDiv(9, 96, 72), D2DLocale, TextFormat));
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
  DWFontWeight: DWRITE_FONT_WEIGHT;
  DWFont: IDWriteFont;
  tagLOGFONT: TLogFont;
  FontMetrics: TDwriteFontMetrics;
  FontFace: WinApi.D2D1.IDWriteFontFace;
  CodePoint: Cardinal;
  FontIndex: Word;
  GlyphMetrics: TDwriteGlyphMetrics;
  Trimming: TDwriteTrimming;
  Baseline: Single;
begin
  FUseGDINatural := AFont.Quality = TFontQuality.fqClearTypeNatural;
  GetObject(AFont.Handle, SizeOf(TLogFont), @tagLOGFONT);
  CheckOSError(TSynDWrite.GDIInterop.CreateFontFromLOGFONT(tagLOGFONT, DWFont));
  DWFont.CreateFontFace(FontFace);
  FontFace.GetGdiCompatibleMetrics(-AFont.Height, 1, PDwriteMatrix(nil)^, FontMetrics);
  CodePoint := Ord('W');
  FontFace.GetGlyphIndices(CodePoint, 1, FontIndex);
  FontFace.GetDesignGlyphMetrics(@FontIndex, 1, @GlyphMetrics);
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
  if TFontStyle.fsBold in AFont.Style then
    DWFontWeight := DWRITE_FONT_WEIGHT_BOLD
  else
    DWFontWeight := DWRITE_FONT_WEIGHT_NORMAL;

  CheckOSError(TSynDWrite.DWriteFactory.CreateTextFormat(PChar(AFont.Name), nil,
    DWFontWeight, DWFontStyle, DWRITE_FONT_STRETCH_NORMAL,
    -AFont.Height, DefaultLocaleName, FIDW));
  FIDW.SetIncrementalTabStop(TabWidth * FCharWidth);

  Trimming.granularity := DWRITE_TRIMMING_GRANULARITY_CHARACTER;
  Trimming.delimiter := 0;
  Trimming.delimiterCount := 0;
  CheckOSError(FIDW.SetTrimming(Trimming, nil));
  FIDW.SetLineSpacing(DWRITE_LINE_SPACING_METHOD_UNIFORM, LineHeight, Baseline);
end;

{ TSynTextLayout }

constructor TSynTextLayout.Create(TextFormat: TSynTextFormat; Text: PChar;
    const Count, LayoutWidth, layoutHeight: Cardinal; WordWrap: Boolean =
    False; PixelsPerDip: Single = 1);
var
  TextLayout1: IDWriteTextLayout1;
begin
//  CheckOSError(TSynDWrite.DWriteFactory.CreateTextLayout(Text,
//    Count, TextFormat.FIDW, LayoutWidth, LayoutHeight, FIDW));
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

  TextOptions :=
    D2D1_DRAW_TEXT_OPTIONS_CLIP + D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT;
end;

procedure TSynTextLayout.Draw(RT: ID2D1RenderTarget; X, Y: Integer;
  FontColor: TColor);
begin
  RT.DrawTextLayout(D2D1PointF(X, Y), FIDW, TSynDWrite.SolidBrush(FontColor),
     TextOptions);
end;

procedure TSynTextLayout.DrawClipped(RT: ID2D1RenderTarget; X, Y: Integer;
  ClipRect: TRect; FontColor: TColor);
begin
  RT.PushAxisAlignedClip(ClipRect, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
  RT.DrawTextLayout(D2D1PointF(X, Y), FIDW, TSynDWrite.SolidBrush(FontColor),
     TextOptions);
  RT.PopAxisAlignedClip;
end;

procedure TSynTextLayout.SetFontColor(Color: TColor; const Start,
  Count: Cardinal);
begin
  FIDW.SetDrawingEffect(TSynDWrite.SolidBrush(Color), DWTextRange(Start, Count));
end;

procedure TSynTextLayout.SetFontStyle(FontStyles: System.UITypes.TFontStyles;
  const Start, Count: Cardinal);
var
  Range: TDwriteTextRange;
begin
  Range := DWTextRange(Start, Count);
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
  Count: Cardinal);
var
  DWTypography: IDWriteTypography;
const
  DefaultTypoFeatures: array[0..8] of integer =
  (DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES,             // clig
   DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES,            // calt
   DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION,  // ccmp
   DWRITE_FONT_FEATURE_TAG_DISCRETIONARY_LIGATURES,          // dlig
   DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES,               // liga
   DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING,                 // mark
   DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING,         // mkmk
   DWRITE_FONT_FEATURE_TAG_REQUIRED_LIGATURES,               // rlig
   DWRITE_FONT_FEATURE_TAG_STYLISTIC_ALTERNATES);            // salt
  TypoFeaturesNoLigatures: array[0..3] of integer =
  (DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION,  // ccmp
   DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING,                 // mark
   DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING,         // mkmk
   DWRITE_FONT_FEATURE_TAG_STYLISTIC_ALTERNATES);            // salt

begin
  case Typography of
    typEmpty: DWTypography := DWGetTypography([]);
    typDefault: DWTypography := DWGetTypography(DefaultTypoFeatures);
    typNoLigatures: DWTypography := DWGetTypography(TypoFeaturesNoLigatures);
  end;
  FIDW.SetTypography(DWTypography, DWTextRange(Start, Count));
end;

{ TSynWICRenderTarget }

constructor TSynWICRenderTarget.Create(const Width, Height: integer);
var
  BM: TBitmap;
  RenderTargetProp: TD2D1RenderTargetProperties;
begin
  inherited Create;
  FWicImage := TWicImage.Create;
  FWicImage.InterpolationMode := wipmHighQualityCubic;
  BM := TBitmap.Create(Width, Height);
  try
    BM.AlphaFormat := afDefined;
    FWicImage.Assign(BM);
  finally
    BM.Free;
  end;

  RenderTargetProp :=
    D2D1RenderTargetProperties(
      {$IFDEF GPUSupport}
      D2D1_RENDER_TARGET_TYPE_DEFAULT,
      {$ELSE}
      D2D1_RENDER_TARGET_TYPE_SOFTWARE, // much faster in my desktop with a slow GPU
      {$ENDIF}
      D2D1PixelFormat(DXGI_FORMAT_UNKNOWN, D2D1_ALPHA_MODE_UNKNOWN), // use image format
      0, 0, D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE);

  CheckOSError(TSynDWrite.D2DFactory.CreateWicBitmapRenderTarget(FWicImage.Handle,
    RenderTargetProp,FIDW));
end;

destructor TSynWICRenderTarget.Destroy;
begin
  FWICImage.Free;
  inherited;
end;

function TSynWicRenderTarget.GetIDW: ID2D1RenderTarget;
begin
  Result := FIDW;
end;

function TSynWicRenderTarget.GetWicImage: TWICImage;
begin
  Result := FWicImage;
end;

function SynWicRenderTarget(const Width, Height: integer): ISynWicRenderTarget;
begin
  Result := TSynWicRenderTarget.Create(Width, Height);
end;

initialization
  if LCIDToLocaleName(GetUserDefaultLCID, DefaultLocaleName, LOCALE_NAME_MAX_LENGTH, 0) = 0 then
    RaiseLastOSError;
end.
