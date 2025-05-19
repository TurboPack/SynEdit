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
  WinApi.DXGI,
  Winapi.DxgiFormat,
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


{$REGION 'Missing d2d1_1.h declarations'}
type
  // Represents a 4-by-4 matrix.
  PD2D_MATRIX_4X4_F = ^D2D_MATRIX_4X4_F;
  D2D_MATRIX_4X4_F = record
    _11: Single;
    _12: Single;
    _13: Single;
    _14: Single;

    _21: Single;
    _22: Single;
    _23: Single;
    _24: Single;

    _31: Single;
    _32: Single;
    _33: Single;
    _34: Single;

    _41: Single;
    _42: Single;
    _43: Single;
    _44: Single;
  end;
  {$EXTERNALSYM D2D_MATRIX_4X4_F}
  PD2D1_MATRIX_4X4_F = ^D2D_MATRIX_4X4_F;
  D2D1_MATRIX_4X4_F = D2D_MATRIX_4X4_F;
  {$EXTERNALSYM D2D1_MATRIX_4X4_F}

type
  // A blend mode that applies to all primitives drawn on the context.
  PD2D1_PRIMITIVE_BLEND = ^D2D1_PRIMITIVE_BLEND;
  D2D1_PRIMITIVE_BLEND = DWord;
  {$EXTERNALSYM D2D1_PRIMITIVE_BLEND}
const
  D2D1_PRIMITIVE_BLEND_SOURCE_OVER = D2D1_PRIMITIVE_BLEND(0);
  D2D1_PRIMITIVE_BLEND_COPY        = D2D1_PRIMITIVE_BLEND(1);
  D2D1_PRIMITIVE_BLEND_MIN         = D2D1_PRIMITIVE_BLEND(2);
  D2D1_PRIMITIVE_BLEND_ADD         = D2D1_PRIMITIVE_BLEND(3);
  D2D1_PRIMITIVE_BLEND_MAX         = D2D1_PRIMITIVE_BLEND(4);
  //D2D1_PRIMITIVE_BLEND_FORCE_DWORD = FORCEDWORD;

type
  // This is used to specify the quality of image scaling with
  // ID2D1DeviceContext.DrawImage and with the 2D Affine Transform Effect.
  PD2D1_INTERPOLATION_MODE = ^D2D1_INTERPOLATION_MODE;
  D2D1_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE}
const
  D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR    = 0;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR}
  D2D1_INTERPOLATION_MODE_LINEAR              = 1;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_LINEAR}
  D2D1_INTERPOLATION_MODE_CUBIC               = 2;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_CUBIC}
  D2D1_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR =3;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_MULTI_SAMPLE_LINEAR}
  D2D1_INTERPOLATION_MODE_ANISOTROPIC         = 4;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_ANISOTROPIC}
  D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC  = 5;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC}
  D2D1_INTERPOLATION_MODE_DEFINITION_FANT = 6;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_FANT}
  D2D1_INTERPOLATION_MODE_DEFINITION_MIPMAP_LINEAR = 7;
  {$EXTERNALSYM D2D1_INTERPOLATION_MODE_DEFINITION_MIPMAP_LINEAR}
  //D2D1_INTERPOLATION_MODE_FORCE_DWORD         = FORCEDWORD;

type
  // This specifies what units should be accepted by the D2D API.
  PD2D1_UNIT_MODE = ^D2D1_UNIT_MODE;
  D2D1_UNIT_MODE = DWord;
  {$EXTERNALSYM D2D1_UNIT_MODE}
const
  D2D1_UNIT_MODE_DIPS        = D2D1_UNIT_MODE(0);
  {$EXTERNALSYM D2D1_UNIT_MODE_DIPS}
  D2D1_UNIT_MODE_PIXELS      = D2D1_UNIT_MODE(1);
  {$EXTERNALSYM D2D1_UNIT_MODE_PIXELS}
  //D2D1_UNIT_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Defines a color space.
  PD2D1_COLOR_SPACE = ^D2D1_COLOR_SPACE;
  D2D1_COLOR_SPACE = DWord;
  {$EXTERNALSYM D2D1_COLOR_SPACE}

  // Specifies which way a color profile is defined.
  PD2D1_COLOR_CONTEXT_TYPE = ^D2D1_COLOR_CONTEXT_TYPE;
  D2D1_COLOR_CONTEXT_TYPE = DWord;
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE}

type
  // Specifies the pixel snapping policy when rendering color bitmap glyphs.
  PD2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = ^D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION;
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION = DWord;
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION}
const
  // Color bitmap glyph positions are snapped to the nearest pixel if the bitmap
  // resolution matches that of the device context.
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT   = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION(0);
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DEFAULT}
  // Color bitmap glyph positions are not snapped.
  D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DISABLE   = D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION(1);
  {$EXTERNALSYM D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_DISABLE}
  //D2D1_COLOR_BITMAP_GLYPH_SNAP_OPTION_FORCE_DWORD = FORCEDWORD;

type
  // This determines what gamma is used for interpolation/blending.
  PD2D1_GAMMA1 = ^D2D1_GAMMA1;
  D2D1_GAMMA1 = DWord;
  {$EXTERNALSYM D2D1_GAMMA1}

const
  // Colors are manipulated in 2.2 gamma color space.
  D2D1_GAMMA1_G22     = D2D1_GAMMA_2_2;
  // Colors are manipulated in 1.0 gamma color space.
  D2D1_GAMMA1_G10     = D2D1_GAMMA_1_0;
  // Colors are manipulated in ST.2084 PQ gamma color space.
  D2D1_GAMMA1_G2084     = D2D1_GAMMA1(2);
  //D2D1_GAMMA1_FORCE_DWORD = FORCEDWORD;

  D2D1_COLOR_CONTEXT_TYPE_ICC     = D2D1_COLOR_CONTEXT_TYPE(0);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_ICC}
  D2D1_COLOR_CONTEXT_TYPE_SIMPLE  = D2D1_COLOR_CONTEXT_TYPE(1);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_SIMPLE}
  D2D1_COLOR_CONTEXT_TYPE_DXGI    = D2D1_COLOR_CONTEXT_TYPE(2);
  {$EXTERNALSYM D2D1_COLOR_CONTEXT_TYPE_DXGI}
  //D2D1_COLOR_CONTEXT_TYPE_FORCE_DWORD = FORCEDWORD;

type
  // Specifies the composite mode that will be applied.
  PD2D1_COMPOSITE_MODE = ^D2D1_COMPOSITE_MODE;
  D2D1_COMPOSITE_MODE = DWord;
  {$EXTERNALSYM D2D1_COMPOSITE_MODE}
const
  D2D1_COMPOSITE_MODE_SOURCE_OVER         = D2D1_COMPOSITE_MODE(0);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_OVER}
  D2D1_COMPOSITE_MODE_DESTINATION_OVER    = D2D1_COMPOSITE_MODE(1);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_OVER}
  D2D1_COMPOSITE_MODE_SOURCE_IN           = D2D1_COMPOSITE_MODE(2);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_IN}
  D2D1_COMPOSITE_MODE_DESTINATION_IN      = D2D1_COMPOSITE_MODE(3);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_IN}
  D2D1_COMPOSITE_MODE_SOURCE_OUT          = D2D1_COMPOSITE_MODE(4);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_OUT}
  D2D1_COMPOSITE_MODE_DESTINATION_OUT     = D2D1_COMPOSITE_MODE(5);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_OUT}
  D2D1_COMPOSITE_MODE_SOURCE_ATOP         = D2D1_COMPOSITE_MODE(6);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_ATOP}
  D2D1_COMPOSITE_MODE_DESTINATION_ATOP    = D2D1_COMPOSITE_MODE(7);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_DESTINATION_ATOP}
  D2D1_COMPOSITE_MODE_XOR                 = D2D1_COMPOSITE_MODE(8);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_XOR}
  D2D1_COMPOSITE_MODE_PLUS                = D2D1_COMPOSITE_MODE(9);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_PLUS}
  D2D1_COMPOSITE_MODE_SOURCE_COPY         = D2D1_COMPOSITE_MODE(10);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_SOURCE_COPY}
  D2D1_COMPOSITE_MODE_BOUNDED_SOURCE_COPY = D2D1_COMPOSITE_MODE(11);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_BOUNDED_SOURCE_COPY}
  D2D1_COMPOSITE_MODE_MASK_INVERT         = D2D1_COMPOSITE_MODE(12);
  {$EXTERNALSYM D2D1_COMPOSITE_MODE_MASK_INVERT}
  //D2D1_COMPOSITE_MODE_FORCE_DWORD         = FORCEDWORD;

// D2D1_LAYER_OPTIONS1
type
  // Specifies how the layer contents should be prepared.
  PD2D1_LAYER_OPTIONS1 = ^D2D1_LAYER_OPTIONS1;
  D2D1_LAYER_OPTIONS1 = Dword;
  {$EXTERNALSYM D2D1_LAYER_OPTIONS1}
const
  D2D1_LAYER_OPTIONS1_NONE                       = D2D1_LAYER_OPTIONS1(0);
  D2D1_LAYER_OPTIONS1_INITIALIZE_FROM_BACKGROUND = D2D1_LAYER_OPTIONS1(1);
  D2D1_LAYER_OPTIONS1_IGNORE_ALPHA               = D2D1_LAYER_OPTIONS1(2);
  //D2D1_LAYER_OPTIONS1_FORCE_DWORD                = FORCEDWORD;

type
  // This specifies the precision that should be used in buffers allocated by D2D.
  PD2D1_BUFFER_PRECISION = ^D2D1_BUFFER_PRECISION;
  D2D1_BUFFER_PRECISION = DWord;
  {$EXTERNALSYM D2D1_BUFFER_PRECISION}
const
  D2D1_BUFFER_PRECISION_UNKNOWN         = D2D1_BUFFER_PRECISION(0);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_UNKNOWN}
  D2D1_BUFFER_PRECISION_8BPC_UNORM      = D2D1_BUFFER_PRECISION(1);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_8BPC_UNORM}
  D2D1_BUFFER_PRECISION_8BPC_UNORM_SRGB = D2D1_BUFFER_PRECISION(2);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_8BPC_UNORM_SRGB}
  D2D1_BUFFER_PRECISION_16BPC_UNORM     = D2D1_BUFFER_PRECISION(3);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_16BPC_UNORM}
  D2D1_BUFFER_PRECISION_16BPC_FLOAT     = D2D1_BUFFER_PRECISION(4);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_16BPC_FLOAT}
  D2D1_BUFFER_PRECISION_32BPC_FLOAT     = D2D1_BUFFER_PRECISION(5);
  {$EXTERNALSYM D2D1_BUFFER_PRECISION_32BPC_FLOAT}
  //D2D1_BUFFER_PRECISION_FORCE_DWORD     = FORCEDWORD;

type
  // Specifies how the bitmap can be used.
  PD2D1_BITMAP_OPTIONS = ^D2D1_BITMAP_OPTIONS;
  D2D1_BITMAP_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_BITMAP_OPTIONS}
const
  // The bitmap is created with default properties.
  D2D1_BITMAP_OPTIONS_NONE           = D2D1_BITMAP_OPTIONS($00000000);
  // The bitmap can be specified as a target in ID2D1DeviceContext.SetTarget
  D2D1_BITMAP_OPTIONS_TARGET         = D2D1_BITMAP_OPTIONS($00000001);
  // The bitmap cannot be used as an input to DrawBitmap, DrawImage, in a bitmap
  // brush or as an input to an effect.
  D2D1_BITMAP_OPTIONS_CANNOT_DRAW    = D2D1_BITMAP_OPTIONS($00000002);
  // The bitmap can be read from the CPU.
  D2D1_BITMAP_OPTIONS_CPU_READ       = D2D1_BITMAP_OPTIONS($00000004);
  // The bitmap works with the ID2D1GdiInteropRenderTarget.GetDC API.
  D2D1_BITMAP_OPTIONS_GDI_COMPATIBLE = D2D1_BITMAP_OPTIONS($00000008);
  // D2D1_BITMAP_OPTIONS_FORCE_DWORD = FORCEDWORD;

type
  // This describes how the individual mapping operation should be performed.
  PD2D1_MAP_OPTIONS = ^D2D1_MAP_OPTIONS;
  D2D1_MAP_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_MAP_OPTIONS}
const
  // The mapped pointer has undefined behavior.
  D2D1_MAP_OPTIONS_NONE        = D2D1_MAP_OPTIONS(0);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_NONE}

  // The mapped pointer can be read from.
  D2D1_MAP_OPTIONS_READ        = D2D1_MAP_OPTIONS(1);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_READ}

  // The mapped pointer can be written to.
  D2D1_MAP_OPTIONS_WRITE       = D2D1_MAP_OPTIONS(2);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_WRITE}

  // The previous contents of the bitmap are discarded when it is mapped.
  D2D1_MAP_OPTIONS_DISCARD     = D2D1_MAP_OPTIONS(4);
  {$EXTERNALSYM D2D1_MAP_OPTIONS_DISCARD}
  //D2D1_MAP_OPTIONS_FORCE_DWORD = FORCEDWORD;


type
  // This enum defines the valid property types that can be used in an effect property
  // interface.
  PD2D1_PROPERTY_TYPE = ^D2D1_PROPERTY_TYPE;
  D2D1_PROPERTY_TYPE = DWord;
  {$EXTERNALSYM D2D1_PROPERTY_TYPE}
const
  D2D1_PROPERTY_TYPE_UNKNOWN       = D2D1_PROPERTY_TYPE(0);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_UNKNOWN}
  D2D1_PROPERTY_TYPE_STRING        = D2D1_PROPERTY_TYPE(1);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_STRING}
  D2D1_PROPERTY_TYPE_BOOL          = D2D1_PROPERTY_TYPE(2);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_BOOL}
  D2D1_PROPERTY_TYPE_UINT32        = D2D1_PROPERTY_TYPE(3);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_UINT32}
  D2D1_PROPERTY_TYPE_INT32         = D2D1_PROPERTY_TYPE(4);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_INT32}
  D2D1_PROPERTY_TYPE_FLOAT         = D2D1_PROPERTY_TYPE(5);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_FLOAT}
  D2D1_PROPERTY_TYPE_VECTOR2       = D2D1_PROPERTY_TYPE(6);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR2}
  D2D1_PROPERTY_TYPE_VECTOR3       = D2D1_PROPERTY_TYPE(7);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR3}
  D2D1_PROPERTY_TYPE_VECTOR4       = D2D1_PROPERTY_TYPE(8);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_VECTOR4}
  D2D1_PROPERTY_TYPE_BLOB          = D2D1_PROPERTY_TYPE(9);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_BLOB}
  D2D1_PROPERTY_TYPE_IUNKNOWN      = D2D1_PROPERTY_TYPE(10);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_IUNKNOWN}
  D2D1_PROPERTY_TYPE_ENUM          = D2D1_PROPERTY_TYPE(11);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_ENUM}
  D2D1_PROPERTY_TYPE_ARRAY         = D2D1_PROPERTY_TYPE(12);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_ARRAY}
  D2D1_PROPERTY_TYPE_CLSID         = D2D1_PROPERTY_TYPE(13);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_CLSID}
  D2D1_PROPERTY_TYPE_MATRIX_3X2    = D2D1_PROPERTY_TYPE(14);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_3X2}
  D2D1_PROPERTY_TYPE_MATRIX_4X3    = D2D1_PROPERTY_TYPE(15);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_4X3}
  D2D1_PROPERTY_TYPE_MATRIX_4X4    = D2D1_PROPERTY_TYPE(16);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_4X4}
  D2D1_PROPERTY_TYPE_MATRIX_5X4    = D2D1_PROPERTY_TYPE(17);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_MATRIX_5X4}
  D2D1_PROPERTY_TYPE_COLOR_CONTEXT = D2D1_PROPERTY_TYPE(18);
  {$EXTERNALSYM D2D1_PROPERTY_TYPE_COLOR_CONTEXT}
//  D2D1_PROPERTY_TYPE_FORCE_DWORD   = FORCEDWORD;

type
  // This specifies how colors are interpolated.
  PD2D1_COLOR_INTERPOLATION_MODE = ^D2D1_COLOR_INTERPOLATION_MODE;
  D2D1_COLOR_INTERPOLATION_MODE = DWord;
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE}
const
  // Colors will be interpolated in straight alpha space.
  D2D1_COLOR_INTERPOLATION_MODE_STRAIGHT      = D2D1_COLOR_INTERPOLATION_MODE(0);
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE_STRAIGHT}
  // Colors will be interpolated in premultiplied alpha space.
  D2D1_COLOR_INTERPOLATION_MODE_PREMULTIPLIED = D2D1_COLOR_INTERPOLATION_MODE(1);
  {$EXTERNALSYM D2D1_COLOR_INTERPOLATION_MODE_PREMULTIPLIED}
  //D2D1_COLOR_INTERPOLATION_MODE_FORCE_DWORD   = FORCEDWORD;

type
  // This specifies options that apply to the device context for its lifetime.
  PD2D1_DEVICE_CONTEXT_OPTIONS = ^D2D1_DEVICE_CONTEXT_OPTIONS;
  D2D1_DEVICE_CONTEXT_OPTIONS = DWord;
  {$EXTERNALSYM D2D1_DEVICE_CONTEXT_OPTIONS}
const
  D2D1_DEVICE_CONTEXT_OPTIONS_NONE                               = D2D1_DEVICE_CONTEXT_OPTIONS(0);
  // Geometry rendering will be performed on many threads in parallel); a single
  // thread is the default.
  D2D1_DEVICE_CONTEXT_OPTIONS_ENABLE_MULTITHREADED_OPTIMIZATIONS = D2D1_DEVICE_CONTEXT_OPTIONS(1);
  //D2D1_DEVICE_CONTEXT_OPTIONS_FORCE_DWORD                        = FORCEDWORD;

type
  // Defines when font resources should be subset during printing.
  PD2D1_PRINT_FONT_SUBSET_MODE = ^D2D1_PRINT_FONT_SUBSET_MODE;
  D2D1_PRINT_FONT_SUBSET_MODE = DWord;
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE}
const
  // Subset for used glyphs, send and discard font resource after every five pages
  D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT     = D2D1_PRINT_FONT_SUBSET_MODE(0);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_DEFAULT}
  // Subset for used glyphs, send and discard font resource after each page
  D2D1_PRINT_FONT_SUBSET_MODE_EACHPAGE    = D2D1_PRINT_FONT_SUBSET_MODE(1);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_EACHPAGE}
  // Do not subset, reuse font for all pages, send it after first page
  D2D1_PRINT_FONT_SUBSET_MODE_NONE        = D2D1_PRINT_FONT_SUBSET_MODE(2);
  {$EXTERNALSYM D2D1_PRINT_FONT_SUBSET_MODE_NONE}
  //D2D1_PRINT_FONT_SUBSET_MODE_FORCE_DWORD = FORCEDWORD;

type
  // Describes mapped memory from the ID2D1Bitmap1.Map API.
  PD2D1_MAPPED_RECT = ^D2D1_MAPPED_RECT;
  D2D1_MAPPED_RECT = record
    pitch: UINT32;
    bits: PByte;
  end;
  {$EXTERNALSYM D2D1_MAPPED_RECT}

  // All parameters related to pushing a layer.
  PD2D1_LAYER_PARAMETERS1 = ^D2D1_LAYER_PARAMETERS1;
  D2D1_LAYER_PARAMETERS1 = record
    contentBounds: D2D1_RECT_F;
    geometricMask: ID2D1Geometry;
    maskAntialiasMode: D2D1_ANTIALIAS_MODE;
    maskTransform: D2D1_MATRIX_3X2_F;
    opacity: Single;
    opacityBrush: ID2D1Brush;
    layerOptions: D2D1_LAYER_OPTIONS1;
  end;
  {$EXTERNALSYM D2D1_LAYER_PARAMETERS1}

  // Contains the position and color of a gradient stop.
  PD2D1_GRADIENT_STOP = ^D2D1_GRADIENT_STOP;
  D2D1_GRADIENT_STOP = record
    position: Single;
    color: D2D1_COLOR_F;
  end;
  {$EXTERNALSYM D2D1_GRADIENT_STOP}

  // Creation properties for an image brush.
  PD2D1_IMAGE_BRUSH_PROPERTIES = ^D2D1_IMAGE_BRUSH_PROPERTIES;
  D2D1_IMAGE_BRUSH_PROPERTIES = record
    sourceRectangle: D2D1_RECT_F;
    extendModeX: D2D1_EXTEND_MODE;
    extendModeY: D2D1_EXTEND_MODE;
    interpolationMode: D2D1_INTERPOLATION_MODE;
  end;
  {$EXTERNALSYM D2D1_IMAGE_BRUSH_PROPERTIES}


  // Describes the extend modes and the interpolation mode of an ID2D1BitmapBrush.
  PD2D1_BITMAP_BRUSH_PROPERTIES1 = ^D2D1_BITMAP_BRUSH_PROPERTIES1;
  D2D1_BITMAP_BRUSH_PROPERTIES1 = record
    extendModeX: D2D1_EXTEND_MODE;
    extendModeY: D2D1_EXTEND_MODE;
    interpolationMode: D2D1_INTERPOLATION_MODE;
  end;
  {$EXTERNALSYM D2D1_BITMAP_BRUSH_PROPERTIES1}

  // This controls advanced settings of the Direct2D imaging pipeline.
  PD2D1_RENDERING_CONTROLS = ^D2D1_RENDERING_CONTROLS;
  D2D1_RENDERING_CONTROLS = record
    // The default buffer precision, used if the precision isn't otherwise specified.
    bufferPrecision: D2D1_BUFFER_PRECISION;
    // The size of allocated tiles used to render imaging effects.
    tileSize: D2D1_SIZE_U;
  end;
  {$EXTERNALSYM D2D1_RENDERING_CONTROLS}


  // The creation properties for a ID2D1PrintControl object.
  PD2D1_PRINT_CONTROL_PROPERTIES = ^D2D1_PRINT_CONTROL_PROPERTIES;
  D2D1_PRINT_CONTROL_PROPERTIES = record
    fontSubset: D2D1_PRINT_FONT_SUBSET_MODE;
    // DPI for rasterization of all unsupported D2D commands or options, defaults to
    // 150.0
    rasterDPI: Single;
    // Color space for vector graphics in XPS package
    colorSpace: D2D1_COLOR_SPACE;
  end;
  {$EXTERNALSYM D2D1_PRINT_CONTROL_PROPERTIES}

  // Interface ID2D1ColorContext
  // ===========================
  // Represents a color context that can be used with an ID2D1Bitmap1 object.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ColorContext);'}
  {$EXTERNALSYM ID2D1ColorContext}
  ID2D1ColorContext = interface(ID2D1Resource)
  ['{1c4820bb-5771-4518-a581-2fe4dd0ec657}']

    // Retrieves the color space of the color context.
    function GetColorSpace(): D2D1_COLOR_SPACE; stdcall;

    // Retrieves the size of the color profile, in bytes.
    function GetProfileSize(): UINT32; stdcall;

    // Retrieves the color profile bytes.
    function GetProfile(out profile: PByte;
                        profileSize: UINT32): HResult; stdcall;

  end;
  IID_ID2D1ColorContext = ID2D1ColorContext;
  {$EXTERNALSYM IID_ID2D1ColorContext}


  // Interface ID2D1GradientStopCollection1
  // ======================================
  // Represents an collection of gradient stops that can then be the source resource
  // for either a linear or radial gradient brush.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GradientStopCollection1);'}
  {$EXTERNALSYM ID2D1GradientStopCollection1}
  ID2D1GradientStopCollection1 = interface(ID2D1GradientStopCollection)
  ['{ae1572f4-5dd0-4777-998b-9279472ae63b}']

    // Copies the gradient stops from the collection into the caller's memory. If this
    // object was created using ID2D1DeviceContext.CreateGradientStopCollection, this
    // method returns the same values as were specified in the creation method. If this
    // object was created using ID2D1RenderTarget.CreateGradientStopCollection, the
    // stops returned here will first be transformed into the gamma space specified by
    // the colorInterpolationGamma parameter.
    procedure GetGradientStops1(out gradientStops: PD2D1_GRADIENT_STOP;
                                gradientStopsCount: UINT32); stdcall;

    // Returns the color space in which interpolation occurs. If this object was
    // created using ID2D1RenderTarget.CreateGradientStopCollection, this method
    // returns the color space related to the color interpolation gamma.
    function GetPreInterpolationSpace(): D2D1_COLOR_SPACE; stdcall;

    // Returns the color space colors will be converted to after interpolation occurs.
    // If this object was created using
    // ID2D1RenderTarget.CreateGradientStopCollection, this method returns
    // D2D1_COLOR_SPACE_SRGB.
    function GetPostInterpolationSpace(): D2D1_COLOR_SPACE; stdcall;

    // Returns the buffer precision of this gradient. If this object was created using
    // ID2D1RenderTarget.CreateGradientStopCollection, this method returns
    // D2D1_BUFFER_PRECISION_8BPC_UNORM.
    function GetBufferPrecision(): D2D1_BUFFER_PRECISION; stdcall;

    // Returns the interpolation mode used to interpolate colors in the gradient.
    function GetColorInterpolationMode(): D2D1_COLOR_INTERPOLATION_MODE; stdcall;

  end;
  IID_ID2D1GradientStopCollection1 = ID2D1GradientStopCollection1;
  {$EXTERNALSYM IID_ID2D1GradientStopCollection1}


  // Extended bitmap properties.
  PD2D1_BITMAP_PROPERTIES1 = ^D2D1_BITMAP_PROPERTIES1;
  D2D1_BITMAP_PROPERTIES1 = record
    _pixelFormat: D2D1_PIXEL_FORMAT;
    dpiX: Single;
    dpiY: Single;
    // Specifies how the bitmap can be used.
    bitmapOptions: D2D1_BITMAP_OPTIONS;
    colorContext: ID2D1ColorContext;
  end;
  {$EXTERNALSYM D2D1_BITMAP_PROPERTIES1}

  // Interface ID2D1Image
  // ====================
  // Represents a producer of pixels that can fill an arbitrary 2D plane.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Image);'}
  {$EXTERNALSYM ID2D1Image}
  ID2D1Image = interface(ID2D1Resource)
  ['{65019f75-8da2-497c-b32c-dfa34e48ede6}']

  end;
  IID_ID2D1Image = ID2D1Image;
  {$EXTERNALSYM IID_ID2D1Image}

  // Interface ID2D1GdiMetafileSink
  // ==============================
  // User-implementable interface for introspecting on a metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafileSink);'}
  {$EXTERNALSYM ID2D1GdiMetafileSink}
  ID2D1GdiMetafileSink = interface(IUnknown)
  ['{82237326-8111-4f7c-bcf4-b5c1175564fe}']

    // Callback for examining a metafile record.
    function ProcessRecord(recordType: DWORD;
                           recordData: Pointer;
                           recordDataSize: DWORD): HResult; stdcall;

  end;
  IID_ID2D1GdiMetafileSink = ID2D1GdiMetafileSink;
  {$EXTERNALSYM IID_ID2D1GdiMetafileSink}


  // Interface ID2D1GdiMetafile
  // ==========================
  // Interface encapsulating a GDI/GDI+ metafile.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1GdiMetafile);'}
  {$EXTERNALSYM ID2D1GdiMetafile}
  ID2D1GdiMetafile = interface(ID2D1Resource)
  ['{2f543dc3-cfc1-4211-864f-cfd91c6f3395}']

    // Play the metafile into a caller-supplied sink interface.
    function Stream(sink: ID2D1GdiMetafileSink): HResult; stdcall;


    // Gets the bounds of the metafile.
    function GetBounds(out bounds: D2D1_RECT_F): HResult; stdcall;

  end;
  IID_ID2D1GdiMetafile = ID2D1GdiMetafile;
  {$EXTERNALSYM IID_ID2D1GdiMetafile}

  // Interface ID2D1Bitmap1
  // ======================
  // Represents a bitmap that can be used as a surface for an ID2D1DeviceContext or
  // mapped into system memory, and can contain additional color context information.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Bitmap1);'}
  {$EXTERNALSYM ID2D1Bitmap1}
  ID2D1Bitmap1 = interface(ID2D1Bitmap)
  ['{a898a84c-3873-4588-b08b-ebbf978df041}']

    // Retrieves the color context information associated with the bitmap.
    procedure GetColorContext(out colorContext: ID2D1ColorContext); stdcall;

    // Retrieves the bitmap options used when creating the API.
    function GetOptions(): D2D1_BITMAP_OPTIONS; stdcall;

    // Retrieves the DXGI surface from the corresponding bitmap, if the bitmap was
    // created from a device derived from a D3D device.
    function GetSurface(out dxgiSurface: IDXGISurface): HResult; stdcall;

    // Maps the given bitmap into memory. The bitmap must have been created with the
    // D2D1_BITMAP_OPTIONS_CPU_READ flag.
    function Map(options: D2D1_MAP_OPTIONS;
                 out mappedRect: D2D1_MAPPED_RECT): HResult; stdcall;

    // Unmaps the given bitmap from memory.
    function Unmap(): HResult; stdcall;

  end;
  IID_ID2D1Bitmap1 = ID2D1Bitmap1;
  {$EXTERNALSYM IID_ID2D1Bitmap1}

  // Interface ID2D1CommandSink
  // ==========================
  // Caller-supplied implementation of an interface to receive the recorded command
  // list.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandSink);'}
  {$EXTERNALSYM ID2D1CommandSink}
  ID2D1CommandSink = interface(IUnknown)
  ['{54d7898a-a061-40a7-bec7-e465bcba2c4f}']

    function BeginDraw(): HResult; stdcall;

    function EndDraw(): HResult; stdcall;

    function SetAntialiasMode(antialiasMode: D2D1_ANTIALIAS_MODE): HResult; stdcall;

    function SetTags(tag1: D2D1_TAG;
                     tag2: D2D1_TAG): HResult; stdcall;

    function SetTextAntialiasMode(textAntialiasMode: D2D1_TEXT_ANTIALIAS_MODE): HResult; stdcall;
    // The text rendering options to be applied to all subsequent text and glyph
    // drawing operations; IUnknown(Nil) to clear current text rendering options.
    function SetTextRenderingParams(textRenderingParams: IDWriteRenderingParams): HResult; stdcall;

    function SetTransform(transform: D2D1_MATRIX_3X2_F): HResult; stdcall;

    function SetPrimitiveBlend(primitiveBlend: D2D1_PRIMITIVE_BLEND): HResult; stdcall;

    function SetUnitMode(unitMode: D2D1_UNIT_MODE): HResult; stdcall;

    function Clear(color: D2D1_COLOR_F): HResult; stdcall;

    function DrawGlyphRun(baselineOrigin: D2D1_POINT_2F;
                          glyphRun: DWRITE_GLYPH_RUN;
                          glyphRunDescription: DWRITE_GLYPH_RUN_DESCRIPTION;
                          foregroundBrush: ID2D1Brush;
                          measuringMode: DWRITE_MEASURING_MODE): HResult; stdcall;

    function DrawLine(point0: D2D1_POINT_2F;
                      point1: D2D1_POINT_2F;
                      brush: ID2D1Brush;
                      strokeWidth: Single;
                      strokeStyle: ID2D1StrokeStyle): HResult; stdcall;

    function DrawGeometry(geometry: ID2D1Geometry;
                          brush: ID2D1Brush;
                          strokeWidth: Single;
                          strokeStyle: ID2D1StrokeStyle): HResult; stdcall;

    function DrawRectangle(rect: D2D1_RECT_F;
                           brush: ID2D1Brush;
                           strokeWidth: Single;
                           strokeStyle: ID2D1StrokeStyle): HResult; stdcall;

    function DrawBitmap(bitmap: ID2D1Bitmap;
                        destinationRectangle: D2D1_RECT_F;
                        opacity: Single;
                        interpolationMode: D2D1_INTERPOLATION_MODE;
                        sourceRectangle: D2D1_RECT_F;
                        perspectiveTransform: D2D1_MATRIX_4X4_F): HResult; stdcall;

    function DrawImage(image: ID2D1Image;
                       targetOffset: D2D1_POINT_2F;
                       imageRectangle: D2D1_RECT_F;
                       interpolationMode: D2D1_INTERPOLATION_MODE;
                       compositeMode: D2D1_COMPOSITE_MODE): HResult; stdcall;

    function DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile;
                             targetOffset: D2D1_POINT_2F): HResult; stdcall;

    function FillMesh(mesh: ID2D1Mesh;
                      brush: ID2D1Brush): HResult; stdcall;

    function FillOpacityMask(opacityMask: ID2D1Bitmap;
                             brush: ID2D1Brush;
                             destinationRectangle: D2D1_RECT_F;
                             sourceRectangle: D2D1_RECT_F): HResult; stdcall;

    function FillGeometry(geometry: ID2D1Geometry;
                          brush: ID2D1Brush;
                          opacityBrush: ID2D1Brush): HResult; stdcall;

    function FillRectangle(rect: D2D1_RECT_F;
                           brush: ID2D1Brush): HResult; stdcall;

    function PushAxisAlignedClip(clipRect: D2D1_RECT_F;
                                 antialiasMode: D2D1_ANTIALIAS_MODE): HResult; stdcall;

    function PushLayer(layerParameters1: D2D1_LAYER_PARAMETERS1;
                       layer: ID2D1Layer): HResult; stdcall;

    function PopAxisAlignedClip(): HResult; stdcall;

    function PopLayer(): HResult; stdcall;

  end;
  IID_ID2D1CommandSink = ID2D1CommandSink;
  {$EXTERNALSYM IID_ID2D1CommandSink}

  // Interface ID2D1CommandList
  // ==========================
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1CommandList);'}
  {$EXTERNALSYM ID2D1CommandList}
  ID2D1CommandList = interface(ID2D1Image)
  ['{b4f34a19-2383-4d76-94f6-ec343657c3dc}']

    // Play the command list into a caller-supplied sink interface.
    function Stream(sink: ID2D1CommandSink): HResult; stdcall;

    // Marks the command list as ready for use.
    function Close(): HResult; stdcall;

  end;
  IID_ID2D1CommandList = ID2D1CommandList;
  {$EXTERNALSYM IID_ID2D1CommandList}

type
  // Interface ID2D1ImageBrush
  // =========================
  // Provides a brush that can take any effect, command list or bitmap and use it to
  // fill a 2D shape.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1ImageBrush);'}
  {$EXTERNALSYM ID2D1ImageBrush}
  ID2D1ImageBrush = interface(ID2D1Brush)
  ['{fe9e984d-3f95-407c-b5db-cb94d4e8f87c}']

    procedure SetImage(image: ID2D1Image); stdcall;

    procedure SetExtendModeX(extendModeX: D2D1_EXTEND_MODE); stdcall;

    procedure SetExtendModeY(extendModeY: D2D1_EXTEND_MODE); stdcall;

    procedure SetInterpolationMode(interpolationMode: D2D1_INTERPOLATION_MODE); stdcall;

    procedure SetSourceRectangle(sourceRectangle: D2D1_RECT_F); stdcall;

    procedure GetImage(out image: ID2D1Image); stdcall;

    function GetExtendModeX(): D2D1_EXTEND_MODE; stdcall;

    function GetExtendModeY(): D2D1_EXTEND_MODE; stdcall;

    function GetInterpolationMode(): D2D1_INTERPOLATION_MODE; stdcall;

    procedure GetSourceRectangle(out sourceRectangle: D2D1_RECT_F); stdcall;

  end;
  IID_ID2D1ImageBrush = ID2D1ImageBrush;
  {$EXTERNALSYM IID_ID2D1ImageBrush}


  // Interface ID2D1BitmapBrush1
  // ===========================
  // A bitmap brush allows a bitmap to be used to fill a geometry.  Interpolation
  // mode is specified with D2D1_INTERPOLATION_MODE
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1BitmapBrush1);'}
  {$EXTERNALSYM ID2D1BitmapBrush1}
  ID2D1BitmapBrush1 = interface(ID2D1BitmapBrush)
  ['{41343a53-e41a-49a2-91cd-21793bbb62e5}']

    // Sets the interpolation mode used when this brush is used.
    procedure SetInterpolationMode1(interpolationMode: D2D1_INTERPOLATION_MODE); stdcall;

    function GetInterpolationMode1(): D2D1_INTERPOLATION_MODE; stdcall;

  end;
  IID_ID2D1BitmapBrush1 = ID2D1BitmapBrush1;
  {$EXTERNALSYM IID_ID2D1BitmapBrush1}

  // Interface ID2D1Properties
  // =========================
  // Represents a set of run-time bindable and discoverable properties that allow a
  // data-driven application to modify the state of a Direct2D effect.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Properties);'}
  {$EXTERNALSYM ID2D1Properties}
  ID2D1Properties = interface(IUnknown)
  ['{483473d7-cd46-4f9d-9d3a-3112aa80159d}']

    // Returns the total number of custom properties in this interface.
    function GetPropertyCount(): UINT32; stdcall;

    // Retrieves the property name from the given property index.
    function GetPropertyName(index: UINT32;
                             out name: LPWSTR;
                             nameCount: UINT32): HResult; stdcall;

    // Returns the length of the property name from the given index.
    function GetPropertyNameLength(index: UINT32): UINT32; stdcall;

    // Retrieves the type of the given property.
    function GetType(index: UINT32): D2D1_PROPERTY_TYPE; stdcall;

    // Retrieves the property index for the given property name.
    function GetPropertyIndex(name: LPWSTR): UINT32; stdcall;


    // Sets the value of the given property using its name.
    function SetValueByName(name: LPWSTR;
                            _type: D2D1_PROPERTY_TYPE;
                            data: PByte;
                            dataSize: UINT32): HResult; stdcall;

    // Sets the given value using the property index.
    function SetValue(index: UINT32;
                      _type: D2D1_PROPERTY_TYPE;
                      data: PByte;
                      dataSize: UINT32): HResult; stdcall;

    // Retrieves the given property or sub-property by name. '.' is the delimiter for
    // sub-properties.
    function GetValueByName(name: LPWSTR;
                            _type: D2D1_PROPERTY_TYPE;
                            data: PByte;
                            dataSize: UINT32): HResult; stdcall;


    // Retrieves the given value by index.
    function GetValue(index: UINT32;
                      _type: D2D1_PROPERTY_TYPE;
                      data: PByte;
                      dataSize: UINT32): HResult; stdcall;


    // Returns the value size for the given property index.
    function GetValueSize(index: UINT32): UINT32; stdcall;


    // Retrieves the sub-properties of the given property by index.
    function GetSubProperties(index: UINT32;
                              out subProperties: ID2D1Properties): HResult; stdcall;

  end;
  IID_ID2D1Properties = ID2D1Properties;
  {$EXTERNALSYM IID_ID2D1Properties}


  // Interface ID2D1Effect
  // =====================
  // The effect interface. Properties control how the effect is rendered. The effect
  // is Drawn with the DrawImage call.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Effect);'}
  {$EXTERNALSYM ID2D1Effect}
  ID2D1Effect = interface(ID2D1Properties)
  ['{28211a43-7d89-476f-8181-2d6159b220ad}']

    // Sets the input to the given effect. The input can be a concrete bitmap or the
    // output of another effect.
    procedure SetInput(index: UINT32;
                       input: ID2D1Image = Nil;
                       invalidate: BOOL = TRUE); stdcall;

    // If the effect supports a variable number of inputs, this sets the number of
    // input that are currently active on the effect.
    function SetInputCount(inputCount: UINT32): HResult; stdcall;

    // Returns the input image to the effect. The input could be another effect or a
    // bitmap.
    procedure GetInput(index: UINT32;
                       out input: ID2D1Image); stdcall;

    // This returns the number of input that are bound into this effect.
    function GetInputCount(): UINT32; stdcall;


    // Returns the output image of the given effect. This can be set as the input to
    // another effect or can be drawn with DrawImage.
    procedure GetOutput(out outputImage: ID2D1Image); stdcall;

  end;
  IID_ID2D1Effect = ID2D1Effect;
  {$EXTERNALSYM IID_ID2D1Effect}

  // This identifies a certain input connection of a certain effect.
  PD2D1_EFFECT_INPUT_DESCRIPTION = ^D2D1_EFFECT_INPUT_DESCRIPTION;
  D2D1_EFFECT_INPUT_DESCRIPTION = record
    // The effect whose input connection is being specified.
    effect: ID2D1Effect;
    // The index of the input connection into the specified effect.
    inputIndex: UINT32;
    // The rectangle which would be available on the specified input connection during
    // render operations.
    inputRectangle: D2D1_RECT_F;
  end;
  {$EXTERNALSYM D2D1_EFFECT_INPUT_DESCRIPTION}

  // Interface IPrintDocumentPackageTarget
  // =====================================
  // Document Target IPrintDocumentPackageTarget interface:
  // Allows user to enumerate supported package target types and create one with type ID.
  // It also supports tracking package printing progess and cancelling.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPrintDocumentPackageTarget);'}
  {$EXTERNALSYM IPrintDocumentPackageTarget}
  IPrintDocumentPackageTarget = interface(IUnknown)
  ['{1b8efec4-3019-4c27-964e-367202156906}']

    // This method is called for enumerating supported target types.
    // The first GUID is preferred type by target.
    function GetPackageTargetTypes(out targetCount: UINT32;
                                   out targetTypes: PGUID): HResult; stdcall;

    // This method is called for createing a target instance.")]
    function GetPackageTarget({in} const guidTargetType: TGUID;
                              {in} const riid: TGUID;
                              out ppvTarget: Pointer): HResult; stdcall;

    function Cancel(): HResult; stdcall;
  end;
  IID_IPrintDocumentPackageTarget = IPrintDocumentPackageTarget;
  {$EXTERNALSYM IID_IPrintDocumentPackageTarget}

  PPrintDocumentPackageCompletion = ^PrintDocumentPackageCompletion;
  PrintDocumentPackageCompletion              = (
    PrintDocumentPackageCompletion_InProgress	= 0,
    PrintDocumentPackageCompletion_Completed	= ( PrintDocumentPackageCompletion_InProgress + 1),
    PrintDocumentPackageCompletion_Canceled	= ( PrintDocumentPackageCompletion_Completed + 1),
    PrintDocumentPackageCompletion_Failed	= ( PrintDocumentPackageCompletion_Canceled + 1)
  );
  {$EXTERNALSYM PrintDocumentPackageCompletion}


  PPrintDocumentPackageStatus = ^PrintDocumentPackageStatus;
  PrintDocumentPackageStatus = record
    JobId: UINT32;
    CurrentDocument: INT32;
    CurrentPage: INT32;
    CurrentPageTotal: INT32;
    Completion: PrintDocumentPackageCompletion;
    PackageStatus: HResult;
  end;
  {$EXTERNALSYM PrintDocumentPackageStatus}


  // Interface ID2D1PrintControl
  // ===========================
  // Converts Direct2D primitives stored in an ID2D1CommandList into a fixed page
  // representation. The print sub-system then consumes the primitives.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1PrintControl);'}
  {$EXTERNALSYM ID2D1PrintControl}
  ID2D1PrintControl = interface(IUnknown)
  ['{2c1d867d-c290-41c8-ae7e-34a98702e9a5}']

    function AddPage(commandList: ID2D1CommandList;
                     pageSize: D2D_SIZE_F;
                     pagePrintTicketStream: IStream;
                     {out_opt} tag1: PD2D1TAG = Nil;
                     {out_opt} tag2: PD2D1TAG = Nil): HResult; stdcall;

    function Close(): HResult; stdcall;

  end;
  IID_ID2D1PrintControl = ID2D1PrintControl;
  {$EXTERNALSYM IID_ID2D1PrintControl}


  PID2D1Device = ^ID2D1Device;
  ID2D1Device = interface;

  // Interface ID2D1DeviceContext
  // ============================
  // The device context represents a set of state and a command buffer that is used
  // to render to a target bitmap.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1DeviceContext);'}
  {$EXTERNALSYM ID2D1DeviceContext}
  ID2D1DeviceContext = interface(ID2D1RenderTarget)
  ['{e8f7fe7a-191c-466d-ad95-975678bda998}']

    // Creates a bitmap with extended bitmap properties, potentially from a block of
    // memory.
    function CreateBitmap(size: D2D1_SIZE_U;
                          sourceData: Pointer;
                          pitch: UINT32;
                          bitmapProperties: PD2D1_BITMAP_PROPERTIES1;
                          out bitmap: ID2D1Bitmap1): HResult; stdcall;

    // Create a D2D bitmap by copying a WIC bitmap.
    function CreateBitmapFromWicBitmap(wicBitmapSource: IWICBitmapSource;
                                       bitmapProperties: PD2D1_BITMAP_PROPERTIES1;
                                       out bitmap: ID2D1Bitmap1): HResult; stdcall;

    // Creates a color context from a color space.  If the space is Custom, the context
    // is initialized from the profile/profileSize arguments.  Otherwise the context is
    // initialized with the profile bytes associated with the space and
    // profile/profileSize are ignored.
    function CreateColorContext(space: D2D1_COLOR_SPACE;
                                profile: PByte;
                                profileSize: UINT32;
                                out colorContext: ID2D1ColorContext): HResult; stdcall;

    function CreateColorContextFromFilename(filename: LPWSTR;
                                            out colorContext: ID2D1ColorContext): HResult; stdcall;

    function CreateColorContextFromWicColorContext(wicColorContext: IWICColorContext;
                                                   out colorContext: ID2D1ColorContext): HResult; stdcall;

    // Creates a bitmap from a DXGI surface with a set of extended properties.
    function CreateBitmapFromDxgiSurface(surface: IDXGISurface;
                                         {opt} bitmapProperties: PD2D1_BITMAP_PROPERTIES1;
                                         out bitmap: ID2D1Bitmap1): HResult; stdcall;

    // Create a new effect, the effect must either be built in or previously registered
    // through ID2D1Factory1.RegisterEffectFromStream or
    // ID2D1Factory1.RegisterEffectFromString.
    function CreateEffect(const effectId: TGUID;
                          out effect: ID2D1Effect): HResult; stdcall;

    // A gradient stop collection represents a set of stops in an ideal unit length.
    // This is the source resource for a linear gradient and radial gradient brush.

    // <param name="preInterpolationSpace">Specifies both the input color space and the
    // space in which the color interpolation occurs.</param>
    // <param name="postInterpolationSpace">Specifies the color space colors will be
    // converted to after interpolation occurs.</param>
    // <param name="bufferPrecision">Specifies the precision in which the gradient
    // buffer will be held.</param>
    // <param name="extendMode">Specifies how the gradient will be extended outside of
    // the unit length.</param>
    // <param name="colorInterpolationMode">Determines if colors will be interpolated
    // in straight alpha or premultiplied alpha space.</param>
    function CreateGradientStopCollection(straightAlphaGradientStops: D2D1_GRADIENT_STOP;
                                          straightAlphaGradientStopsCount: UINT32;
                                          preInterpolationSpace: D2D1_COLOR_SPACE;
                                          postInterpolationSpace: D2D1_COLOR_SPACE;
                                          bufferPrecision: D2D1_BUFFER_PRECISION;
                                          extendMode: D2D1_EXTEND_MODE;
                                          colorInterpolationMode: D2D1_COLOR_INTERPOLATION_MODE;
                                          out gradientStopCollection1: ID2D1GradientStopCollection1): HResult; stdcall;

    // Creates an image brush, the input image can be any type of image, including a
    // bitmap, effect and a command list.
    function CreateImageBrush(image: ID2D1Image;
                              imageBrushProperties: D2D1_IMAGE_BRUSH_PROPERTIES;
                              brushProperties: D2D1_BRUSH_PROPERTIES;
                              out imageBrush: ID2D1ImageBrush): HResult; stdcall;

    function CreateBitmapBrush(bitmap: ID2D1Bitmap;
                               bitmapBrushProperties: PD2D1_BITMAP_BRUSH_PROPERTIES1;
                               brushProperties: D2D1_BRUSH_PROPERTIES;
                               out bitmapBrush: ID2D1BitmapBrush1): HResult; stdcall;

    // Creates a new command list.
    function CreateCommandList(out commandList: ID2D1CommandList): HResult; stdcall;

    // Indicates whether the format is supported by D2D.
    function IsDxgiFormatSupported(format: DXGI_FORMAT): BOOL; stdcall;

    // Indicates whether the buffer precision is supported by D2D.
    function IsBufferPrecisionSupported(bufferPrecision: D2D1_BUFFER_PRECISION): BOOL; stdcall;

    // This retrieves the local-space bounds in DIPs of the current image using the
    // device context DPI.
    function GetImageLocalBounds(image: ID2D1Image;
                                 out localBounds: D2D1_RECT_F): HResult; stdcall;

    // This retrieves the world-space bounds in DIPs of the current image using the
    // device context DPI.
    function GetImageWorldBounds(image: ID2D1Image;
                                 out worldBounds: D2D1_RECT_F): HResult; stdcall;

    // Retrieves the world-space bounds in DIPs of the glyph run using the device
    // context DPI.
    function GetGlyphRunWorldBounds(baselineOrigin: D2D1_POINT_2F;
                                    glyphRun: DWRITE_GLYPH_RUN;
                                    measuringMode: DWRITE_MEASURING_MODE;
                                    out bounds: D2D1_RECT_F): HResult; stdcall;

    // Retrieves the device associated with this device context.
    procedure GetDevice(out device: ID2D1Device); stdcall;

    // Sets the target for this device context to point to the given image. The image
    // can be a command list or a bitmap created with the D2D1_BITMAP_OPTIONS_TARGET
    // flag.
    procedure SetTarget(image: ID2D1Image); stdcall;

    // Gets the target that this device context is currently pointing to.
    procedure GetTarget(out image: ID2D1Image); stdcall;

    // Sets tuning parameters for internal rendering inside the device context.
    procedure SetRenderingControls(renderingControls: D2D1_RENDERING_CONTROLS); stdcall;

    // This retrieves the rendering controls currently selected into the device
    // context.
    procedure GetRenderingControls(out renderingControls: D2D1_RENDERING_CONTROLS); stdcall;

    // Changes the primitive blending mode for all of the rendering operations.
    procedure SetPrimitiveBlend(primitiveBlend: D2D1_PRIMITIVE_BLEND); stdcall;

    // Returns the primitive blend currently selected into the device context.
    function GetPrimitiveBlend(): D2D1_PRIMITIVE_BLEND; stdcall;

    // Changes the units used for all of the rendering operations.
    procedure SetUnitMode(unitMode: D2D1_UNIT_MODE); stdcall;

    // Returns the unit mode currently set on the device context.
    function GetUnitMode(): D2D1_UNIT_MODE; stdcall;

    // Draws the glyph run with an extended description to describe the glyphs.
    procedure DrawGlyphRun(baselineOrigin: D2D1_POINT_2F;
                           glyphRun: DWRITE_GLYPH_RUN;
                           glyphRunDescription: DWRITE_GLYPH_RUN_DESCRIPTION;
                           foregroundBrush: ID2D1Brush;
                           measuringMode: DWRITE_MEASURING_MODE = DWRITE_MEASURING_MODE_NATURAL); stdcall;

    // Draw an image to the device context. The image represents either a concrete
    // bitmap or the output of an effect graph.
    procedure DrawImage(image: ID2D1Image;
                        targetOffset: PD2D1POINT2F = Nil;
                        imageRectangle: PD2D1RECTF = Nil;
                        interpolationMode: D2D1_INTERPOLATION_MODE = D2D1_INTERPOLATION_MODE_LINEAR;
                        compositeMode: D2D1_COMPOSITE_MODE = D2D1_COMPOSITE_MODE_SOURCE_OVER); stdcall;

    // Draw a metafile to the device context.
    procedure DrawGdiMetafile(gdiMetafile: ID2D1GdiMetafile;
                              targetOffset: PD2D1POINT2F = Nil); stdcall;

    procedure DrawBitmap(bitmap: ID2D1Bitmap;
                         destinationRectangle: D2D1_RECT_F;
                         opacity: Single;
                         interpolationMode: D2D1_INTERPOLATION_MODE;
                         sourceRectangle: PD2D1RECTF = Nil;
                         perspectiveTransform: PD2D1_MATRIX_4X4_F = Nil); stdcall;

    // Push a layer on the device context.
    procedure PushLayer(layerParameters: D2D1_LAYER_PARAMETERS1;
                        layer: ID2D1Layer); stdcall;

    // This indicates that a portion of an effect's input is invalid. This method can
    // be called many times.
    function InvalidateEffectInputRectangle(effect: ID2D1Effect;
                                            input: UINT32;
                                            inputRectangle: D2D1_RECT_F): HResult; stdcall;

    // Gets the number of invalid ouptut rectangles that have accumulated at the
    // effect.
    function GetEffectInvalidRectangleCount(effect: ID2D1Effect;
                                            out rectangleCount: UINT32): HResult; stdcall;

    // Gets the invalid rectangles that are at the output of the effect.
    function GetEffectInvalidRectangles(effect: ID2D1Effect;
                                        out rectangles: PD2D1RECTF; // pointer to array of D2D1_RECT_F
                                        rectanglesCount: UINT32): HResult; stdcall;

    // Gets the maximum region of each specified input which would be used during a
    // subsequent rendering operation
    function GetEffectRequiredInputRectangles(renderEffect: ID2D1Effect;
                                              renderImageRectangle: D2D1_RECT_F;
                                              inputDescriptions: PD2D1_EFFECT_INPUT_DESCRIPTION;
                                              out requiredInputRects: D2D1_RECT_F; // pointer to array of D2D1_RECT_F
                                              inputCount: UINT32): HResult; stdcall;

    // Fill using the alpha channel of the supplied opacity mask bitmap. The brush
    // opacity will be modulated by the mask. The render target antialiasing mode must
    // be set to aliased.
    procedure FillOpacityMask(opacityMask: ID2D1Bitmap;
                              brush: ID2D1Brush;
                              destinationRectangle: PD2D1RECTF = Nil;
                              sourceRectangle: PD2D1RECTF = Nil); stdcall;

  end;
  IID_ID2D1DeviceContext = ID2D1DeviceContext;
  {$EXTERNALSYM IID_ID2D1DeviceContext}

   // Interface ID2D1Device
  // =====================
  // The device defines a resource domain whose objects and device contexts can be
  // used together.
  //
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID2D1Device);'}
  {$EXTERNALSYM ID2D1Device}
  ID2D1Device = interface(ID2D1Resource)
  ['{47dd575d-ac05-4cdd-8049-9b02cd16f44c}']

    // Creates a new device context with no initially assigned target.
    function CreateDeviceContext(options: D2D1_DEVICE_CONTEXT_OPTIONS;
                                 out deviceContext: ID2D1DeviceContext): HResult; stdcall;

    // Creates a D2D print control.
    function CreatePrintControl(const wicFactory: IWICImagingFactory;
                                const documentTarget: IPrintDocumentPackageTarget;
                                printControlProperties: PD2D1_PRINT_CONTROL_PROPERTIES;
                                out printControl: ID2D1PrintControl): HResult; stdcall;

    // Sets the maximum amount of texture memory to maintain before evicting caches.
    procedure SetMaximumTextureMemory(maximumInBytes: UINT64); stdcall;

    // Gets the maximum amount of texture memory to maintain before evicting caches.
    function GetMaximumTextureMemory(): UINT64; stdcall;

    // Clears all resources that are cached but not held in use by the application
    // through an interface reference.
    procedure ClearResources(millisecondsSinceUse: UINT32 = 0); stdcall;

  end;
  IID_ID2D1Device = ID2D1Device;
  {$EXTERNALSYM IID_ID2D1Device}
{$ENDREGION 'Missing d2d1_1.h declarations}

{$REGION 'Missing documenttarget.h declarations'}
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IPrintDocumentPackageTargetFactory);'}
  {$EXTERNALSYM IPrintDocumentPackageTargetFactory}
  IPrintDocumentPackageTargetFactory = interface(IUnknown)
    ['{D2959BF7-B31B-4A3D-9600-712EB1335BA4}']
    function CreateDocumentPackageTargetForPrintJob(printerName: PWideChar; jobName: PWideChar;
                                                    const jobOutputStream: IStream;
                                                    const jobPrintTicketStream: IStream;
                                                    out docPackageTarget: IPrintDocumentPackageTarget): HResult; stdcall;
  end;
  IID_IPrintDocumentPackageTargetFactory = IPrintDocumentPackageTargetFactory;
  {$EXTERNALSYM IID_IPrintDocumentPackageTargetFactory}

{$ENDREGION 'Missing documenttarget.h declarations'}

type
  TSynDWrite = class
  private
    class var SingletonD2DFactory: ID2D1Factory;
    class var SingletonRenderTarget: ID2D1DCRenderTarget;
    class var SingletonDWriteFactory: IDWriteFactory;
    class var SingletonGDIInterop: IDWriteGdiInterop;
    class var SingletonDottedStrokeStyle: ID2D1StrokeStyle;
    class var SingletonImagingFactory: IWICImagingFactory;
    class var SingletonPrintDocumentPackageTargetFactory: IPrintDocumentPackageTargetFactory;
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
    class function PrintDocumentPackageTargetFactory: IPrintDocumentPackageTargetFactory; static;
    class function SolidBrush(Color: TColor): ID2D1SolidColorBrush; overload; static;
    class function SolidBrush(Color: TD2D1ColorF): ID2D1SolidColorBrush; overload; static;
    class function DottedStrokeStyle: ID2D1StrokeStyle; static;
    class function GradientGutterBrush(StartColor, EndColor: TColor): ID2D1LinearGradientBrush;
    class procedure ResetRenderTarget; static;
    // if SynEdit inside a DLL call Finalize before unloading the DLL (https://github.com/TurboPack/SynEdit/issues/249)
    class procedure Finalize;
  end;

  TSynTextFormat = record
  private
    FIDW: IDWriteTextFormat;
    FCharExtra: Cardinal;
    FUseGDINatural: Boolean;
    FCharWidth: Cardinal;
    FLineHeight: Cardinal;
  public
    constructor Create(AFont: TFont; TabWidth: Cardinal = 2;
      CharExtra: Cardinal = 0; LineSpacingExtra: Cardinal = 0);
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
    constructor Create(TextFormat: TSynTextFormat; Text: PChar; const Count: Cardinal;
        const LayoutWidth: Cardinal = MaxInt; const layoutHeight: Cardinal = MaxInt;
        WordWrap: Boolean = False; PixelsPerDip: Single = 1);
    procedure SetFontStyle(FontStyles: System.UITypes.TFontStyles; const Start,
        Count: Integer);
    procedure SetFontColor(Color: TD2D1ColorF; const Start, Count: Integer); overload;
    procedure SetFontColor(Color: TColor; const Start, Count: Integer); overload;
    procedure SetTypography(Typography: TSynTypography; const Start, Count: Integer);
    procedure SetTextAlignment(TextAlignment: DWRITE_TEXT_ALIGNMENT);
    procedure SetParagraphAlignment(ParagraphAlignment: DWRITE_PARAGRAPH_ALIGNMENT);
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
    constructor Create(const Width, Height: Integer);
  end;

  function SynWicRenderTarget(const Width, Height: Integer): ISynWicRenderTarget;

type
  TGraphemeEnumerator = record
  private
    FTextLayout: IDWriteTextLayout;
    FStart: Integer;
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
function D2D1ColorF(const AColor: TColor; Opacity: Single = 1.0): TD2D1ColorF; overload;
function DWTextRange(startPosition: Cardinal; length: Cardinal): TDwriteTextRange;
function DWFontFeature(nameTag: DWRITE_FONT_FEATURE_TAG; parameter: Cardinal): TDwriteFontFeature;
function DWGetTypography(Features: array of Integer): IDWriteTypography;
function WicBitmapFromBitmap(Bitmap: TBitmap): IWICBitmap;
function ScaledWicBitmap(Source: IWICBitmap;
  const ScaledWidth, ScaledHeight: Integer): IWICBitmap;
procedure ImageListDraw(RT: ID2D1RenderTarget; IL: TCustomImageList; X, Y,
    Index: Integer);
function IsFontMonospacedAndValid(Font: TFont): Boolean;
function FontFamilyName(Font: IDWriteFont): string;
/// <summary>
///   Converts a Delphi bitmap to a ID2D1Bitmap.
///   Similar to the one in Vcl.Direct2D
/// </summary>
function D2D1BitmapFromBitmap(Bitmap: TBitmap; RT: ID2D1RenderTarget): ID2D1Bitmap;

var
  clNoneF: TD2D1ColorF;

implementation

Uses
  Winapi.CommCtrl,
  System.Math,
  System.Win.ComObj,
  Vcl.Forms,
  SynUnicode,
  SynEditTypes,
  SynEditMiscProcs;

resourcestring
  SYNS_FontFamilyNotFound = 'Font family name not found';

{$REGION 'Support functions'}

function D2D1ColorF(const AColor: TColor; Opacity: Single): TD2D1ColorF;
var
  RGB: Cardinal;
const
  CScale = 1 / 255;
begin
  RGB := ColorToRGB(AColor);
  Result.r := TColors(RGB).R * CScale;
  Result.g := TColors(RGB).G * CScale;
  Result.b := TColors(RGB).B * CScale;
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

function DWGetTypography(Features: array of Integer): IDWriteTypography;
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
  Bitmap: ID2D1Bitmap;
  BitmapInfo: TBitmapInfo;
  Buf: array of Byte;
  BitmapProperties: TD2D1BitmapProperties;
  Icon: HIcon;
  IconInfo: TIconInfo;
  R: TRectF;
  DC: HDC;
begin
  Icon := ImageList_GetIcon(IL.Handle, Index, ILD_NORMAL);
  try
    if not GetIconInfo(Icon, IconInfo) then
      Exit;
    try
      DC := CreateCompatibleDC(0);
      try
        FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
        BitmapInfo.bmiHeader.biSize := Sizeof(BitmapInfo.bmiHeader);
        // call with nil to get the Bitmap Info filled.
        if (GetDIBits(DC, IconInfo.hbmColor, 0, IL.Height, nil, BitmapInfo, DIB_RGB_COLORS) = 0) or
          (BitmapInfo.bmiHeader.biBitCount <> 32)
        then
          Exit;  // Exit if it fails or if biBitCount <> 32
        BitmapInfo.bmiHeader.biCompression := BI_RGB; // set to uncompressed
        SetLength(Buf, IL.Height * IL.Width * 4);
        if GetDIBits(DC, IconInfo.hbmColor, 0, IL.Height, @Buf[0], BitmapInfo, DIB_RGB_COLORS) = 0 then
          Exit;
      finally
        DeleteDC(DC);
      end;
    finally
      DeleteObject(IconInfo.hbmColor);
      DeleteObject(IconInfo.hbmMask);
    end;
  finally
    DestroyIcon(Icon);
  end;

  BitmapProperties := D2D1BitmapProperties(D2D1PixelFormat(
    DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED), 0, 0);

  CheckOSError(RT.CreateBitmap(D2D1SizeU(IL.Width, IL.Height), @Buf[0],
                4 * IL.Width, BitmapProperties, Bitmap));

  R := Rect(X, Y, X + IL.Width, Y + IL.Height);
  RT.DrawBitmap(Bitmap, @R, 1);
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
    raise ESynError.CreateRes(@SYNS_FontFamilyNotFound);
end;

function D2D1BitmapFromBitmap(Bitmap: TBitmap; RT: ID2D1RenderTarget): ID2D1Bitmap;
var
  BitmapInfo: TBitmapInfo;
  buf: array of Byte;
  BitmapProperties: TD2D1BitmapProperties;
  Hbmp: HBitmap;
begin
  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := Sizeof(BitmapInfo.bmiHeader);
  BitmapInfo.bmiHeader.biHeight := -Bitmap.Height;
  BitmapInfo.bmiHeader.biWidth := Bitmap.Width;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;

  SetLength(buf, Bitmap.Height * Bitmap.Width * 4);
  // Forces evaluation of Bitmap.Handle before Bitmap.Canvas.Handle
  Hbmp := Bitmap.Handle;
  GetDIBits(Bitmap.Canvas.Handle, Hbmp, 0, Bitmap.Height, @buf[0], BitmapInfo, DIB_RGB_COLORS);

  BitmapProperties.dpiX := 0;
  BitmapProperties.dpiY := 0;
  BitmapProperties.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
  if (Bitmap.PixelFormat <> pf32bit) or (Bitmap.AlphaFormat = afIgnored) then
    BitmapProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE
  else
    BitmapProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;


  RT.CreateBitmap(D2D1SizeU(Bitmap.Width, Bitmap.Height), @buf[0], 4*Bitmap.Width, BitmapProperties, Result)
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

class procedure TSynDWrite.Finalize;
begin
  FreeAndNil(TSynDWrite.FSolidBrushes);
  TSynDWrite.SingletonDottedStrokeStyle := nil;
  TSynDWrite.SingletonRenderTarget := nil;
  TSynDWrite.SingletonGDIInterop := nil;
  TSynDWrite.SingletonImagingFactory := nil;
  TSynDWrite.SingletonPrintDocumentPackageTargetFactory := nil;
  TSynDWrite.SingletonD2DFactory := nil;
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

class function TSynDWrite.PrintDocumentPackageTargetFactory: IPrintDocumentPackageTargetFactory;
const
  CLASS_PrintDocumentPackageTargetFactory: TGUID = '{348EF17D-6C81-4982-92B4-EE188A43867A}';
var
  PDPTFactory: IPrintDocumentPackageTargetFactory;
begin
  if SingletonPrintDocumentPackageTargetFactory = nil then
  begin
    PDPTFactory := CreateComObject(CLASS_PrintDocumentPackageTargetFactory) as IPrintDocumentPackageTargetFactory;
    Assert(PDPTFactory <> nil);
    if InterlockedCompareExchangePointer(Pointer(SingletonPrintDocumentPackageTargetFactory),
      Pointer(PDPTFactory), nil) = nil
    then
      SingletonPrintDocumentPackageTargetFactory._AddRef;
  end;
  Result := SingletonPrintDocumentPackageTargetFactory;
end;

class function TSynDWrite.RenderTarget: ID2D1DCRenderTarget;
var
  RT: ID2D1DCRenderTarget;
begin
  if SingletonRenderTarget = nil then
  begin
    CheckOSError(D2DFactory.CreateDCRenderTarget(
      D2D1RenderTargetProperties(
        {$IFNDEF DisableGPUSupport}
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

constructor TSynTextFormat.Create(AFont: TFont; TabWidth: Cardinal = 2;
    CharExtra: Cardinal = 0; LineSpacingExtra: Cardinal = 0);
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
    const Count: Cardinal; const LayoutWidth: Cardinal = MaxInt; const
    layoutHeight: Cardinal = MaxInt; WordWrap: Boolean = False; PixelsPerDip:
    Single = 1);
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

procedure TSynTextLayout.SetParagraphAlignment(
  ParagraphAlignment: DWRITE_PARAGRAPH_ALIGNMENT);
begin
  FIDW.SetParagraphAlignment(ParagraphAlignment);
end;

procedure TSynTextLayout.SetTextAlignment(TextAlignment: DWRITE_TEXT_ALIGNMENT);
begin
  FIDW.SetTextAlignment(TextAlignment);
end;

procedure TSynTextLayout.SetTypography(Typography: TSynTypography; const Start,
    Count: Integer);
const
  DefaultTypoFeatures: array[0..7] of Integer =
  (DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_LIGATURES,             // clig
   DWRITE_FONT_FEATURE_TAG_CONTEXTUAL_ALTERNATES,            // calt
   DWRITE_FONT_FEATURE_TAG_GLYPH_COMPOSITION_DECOMPOSITION,  // ccmp
   DWRITE_FONT_FEATURE_TAG_DISCRETIONARY_LIGATURES,          // dlig
   DWRITE_FONT_FEATURE_TAG_STANDARD_LIGATURES,               // liga
   DWRITE_FONT_FEATURE_TAG_MARK_POSITIONING,                 // mark
   DWRITE_FONT_FEATURE_TAG_MARK_TO_MARK_POSITIONING,         // mkmk
   DWRITE_FONT_FEATURE_TAG_REQUIRED_LIGATURES);              // rlig
  TypoFeaturesNoLigatures: array[0..2] of Integer =
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

constructor TSynWICRenderTarget.Create(const Width, Height: Integer);
var
  RenderTargetProp: TD2D1RenderTargetProperties;
begin
  inherited Create;
  CheckOSError(TSynDWrite.ImagingFactory.CreateBitmap(Width, Height,
    @GUID_WICPixelFormat32bppPBGRA, WICBitmapCacheOnDemand, FWicBitmap));

  RenderTargetProp :=
    D2D1RenderTargetProperties(
      {$IFNDEF DisableGPUSupport}
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

function SynWicRenderTarget(const Width, Height: Integer): ISynWicRenderTarget;
begin
  Result := TSynWicRenderTarget.Create(Width, Height);
end;


initialization
  clNoneF := D2D1ColorF(0, 0, 0, 0);
finalization
  // Delphi 10.1 does not support class destructors
  TSynDWrite.Finalize;
end.
