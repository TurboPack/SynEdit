{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: SynEditMiscClasses.pas, released 2000-04-07.
  The Original Code is based on the mwSupportClasses.pas file from the
  mwEdit component suite by Martin Waldenburg and other developers, the Initial
  Author of this file is Michael Hieke.
  Unicode translation by Ma�l H�rz.
  All Rights Reserved.

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
  ------------------------------------------------------------------------------- }

unit Vcl.SynEditMiscClasses;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.D2D1,
  Winapi.Wincodec,
  Vcl.Consts,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ExtCtrls,
  Vcl.StdActns,
  SynDWrite,
  SynEditTypes,
  SynEditSelections,
  SynEditKeyCmds,
  SynEditKeyConstShared,
  SynUnicodeShared;

type
  {$REGION 'Selected Color'}
  TSynSelectedColor = SynEditTypes.TSynSelectedColor;
  {$ENDREGION 'Selected Color'}

  {$REGION 'Indentation Guides'}
  TSynIdentGuidesStyle = (igsSolid, igsDotted);

  TSynStructureColor = class(TCollectionItem)
  private
    FColor: TColor;
    procedure SetColor(const Value: TColor);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor;
  end;

  TSynStructureColors = class(TOwnedCollection)
  private
    function GetColors(Index: Integer): TSynStructureColor;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    property Colors[Index: Integer]: TSynStructureColor read GetColors; default;
  end;

  TSynIndentGuides = class(TPersistent)
  private
    FOwner: TPersistent;
    FColor: TColor;
    FVisible: Boolean;
    FStyle: TSynIdentGuidesStyle;
    FOnChange: TNotifyEvent;
    FStructureColors: TSynStructureColors;
    FStructureHighlight: Boolean;
    FUseStructureColors: Boolean;
    FStructuredColorsModified: Boolean;
    procedure Changed;
    procedure SetColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
    procedure SetStyle(const Value: TSynIdentGuidesStyle);
    procedure SetStructureColors(const Value: TSynStructureColors);
    procedure SetUseStructureColors(const Value: Boolean);
    procedure SetStructureHighlight(const Value: Boolean);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create; overload;
    constructor Create(Owner: TPersistent); overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Style: TSynIdentGuidesStyle read FStyle write SetStyle
      default igsSolid;
    property Color: TColor read FColor write SetColor
      default clMedGray;
    property UseStructureColors: Boolean read FUseStructureColors
      write SetUseStructureColors default True;
    property StructureHighlight: Boolean read FStructureHighlight
      write SetStructureHighlight default True;
    property StructureColors: TSynStructureColors read FStructureColors
      write SetStructureColors stored FStructuredColorsModified;
  end;
  {$ENDREGION 'Indentation Guides'}

  {$REGION 'Bands'}

  TSynGutterBorderStyle = (gbsNone, gbsMiddle, gbsRight);

  TGutterBandPaintEvent = procedure(RT: ID2D1RenderTarget; ClipR: TRect;
    const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean) of object;

  TGutterBandClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    X, Y, Row, Line: Integer) of object;

  TGutterBandContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint;
    Row, Line: Integer; var Handled: Boolean) of object;

  TGutterMouseCursorEvent = procedure(Sender: TObject; X, Y, Row, Line: Integer;
    var Cursor: TCursor) of object;

  TSynGutter = class;

  { When created TGutter contains four bands (Marks, Line Numbers,
    Code Folding and Margin).  The order, width and other properties of the
    bands can be set at design time through the Bands property of TSynGutter.
    Custom bands can also be created.  They can be painted using
    OnPaintLines event handler.
    The width of the Line Numbers, Track Changes and Code Folding band
    are calculated from properites of TSynGutter and not set directly at
    design time }
  TSynGutterBandKind = (gbkCustom, gbkMarks, gbkLineNumbers, gbkFold, gbkMargin,
    gbkTrackChanges);
  TSynGutterBandBackground = (gbbNone, gbbGutter, gbbEditor);

  TSynTrackChanges = class(TPersistent)
  private
    FOwner: TSynGutter;
    FVisible: Boolean;
    FSavedColor: TColor;
    FModifiedColor: TColor;
    FSavedModifiedColor: TColor;
    FOriginalColor: TColor;
    FWidth: Integer;
    procedure SetModifiedColor(const Value: TColor);
    procedure SetOriginalColor(const Value: TColor);
    procedure SetSavedColor(const Value: TColor);
    procedure SetSavedModifiedColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Gutter: TSynGutter);
    procedure Assign(Source: TPersistent); override;
  published
    property Width: Integer read FWidth write SetWidth default 4;
    property Visible: Boolean read FVisible write SetVisible default False;
    property SavedColor: TColor read FSavedColor write SetSavedColor
      default $0033AA33;
    property ModifiedColor: TColor read FModifiedColor write SetModifiedColor
      default $0000DFFF;
    property SavedModifiedColor: TColor read FSavedModifiedColor
      write SetSavedModifiedColor default clWebOrange;
    property OriginalColor: TColor read FOriginalColor write SetOriginalColor
      default $FF6633;
  end;

  TSynGutterBand = class(TCollectionItem)
  public const
    MarginX = 2;
  private
    FKind: TSynGutterBandKind;
    FVisible: Boolean;
    FWidth: Integer;
    FBackground: TSynGutterBandBackground;
    FOnPaintLines: TGutterBandPaintEvent;
    FOnClick: TGutterBandClickEvent;
    FOnContextPopup: TGutterBandContextPopupEvent;
    FOnMouseCursor: TGutterMouseCursorEvent;
    function GetSynGutter: TSynGutter;
    function GetEditor: TComponent;
    procedure DoPaintLines(RT: ID2D1RenderTarget; ClipR: TRect; const FirstRow,
      LastRow: Integer);
    procedure PaintMarks(RT: ID2D1RenderTarget; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintLineNumbers(RT: ID2D1RenderTarget; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintFoldShapes(RT: ID2D1RenderTarget; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintMargin(RT: ID2D1RenderTarget; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintTrackChanges(RT: ID2D1RenderTarget; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure SetBackground(const Value: TSynGutterBandBackground);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetKind(Kind: TSynGutterBandKind);
    procedure SetOnPaintLines(const Value: TGutterBandPaintEvent);
    function IsWidthStored: Boolean;
    function GetWidth: Integer;
    function GetVisible: Boolean;
    function GetLeftX: Integer;
    function FoldShapeRect(Row, Line: Integer): TRect;
    function IsVisibleStored: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function RealWidth: Integer;
    procedure PaintLines(RT: ID2D1RenderTarget; ClipR: TRect; const FirstRow, LastRow:
        Integer);
    procedure DoClick(Sender: TObject; Button: TMouseButton;
      X, Y, Row, Line: Integer);
    procedure DoMouseCursor(Sender: TObject; X, Y, Row, Line: Integer;
      var Cursor: TCursor);
    property LeftX: Integer read GetLeftX;
    property Editor: TComponent read GetEditor;
    property Gutter: TSynGutter read GetSynGutter;
  published
    property Kind: TSynGutterBandKind read FKind write SetKind;
    property Visible: Boolean read GetVisible write SetVisible
      stored IsVisibleStored;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property Background: TSynGutterBandBackground read FBackground
      write SetBackground default gbbGutter;
    property OnPaintLines: TGutterBandPaintEvent read FOnPaintLines
      write SetOnPaintLines;
    property OnCLick: TGutterBandClickEvent read FOnClick write FOnClick;
    property OnContextPopup: TGutterBandContextPopupEvent
      read FOnContextPopup write FOnContextPopup;
    property OnMouseCursor: TGutterMouseCursorEvent read FOnMouseCursor
      write FOnMouseCursor;
  end;

  TSynBandsCollection = class(TOwnedCollection)
  private
    function GetBands(Index: Integer): TSynGutterBand;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    property Bands[Index: Integer]: TSynGutterBand read GetBands; default;
  end;
  {$ENDREGION 'Bands'}

  TSynInternalImage = class;

  {$REGION 'TSynGutter'}
  TSynGutter = class(TPersistent)
  private
    FOwner: TPersistent; // Synedit
    FUpdateCount: Integer;
    FCurrentPPI: Integer;
    FFont: TFont;
    FCharWidth: Integer;
    FColor: TColor;
    FBorderColor: TColor;
    FDigitCount: Integer;
    FLeadingZeros: Boolean;
    FZeroStart: Boolean;
    FOnChange: TNotifyEvent;
    FCursor: TCursor;
    FVisible: Boolean;
    FShowLineNumbers: Boolean;
    FUseFontStyle: Boolean;
    FAutoSize: Boolean;
    FAutoSizeDigitCount: Integer;
    FBorderStyle: TSynGutterBorderStyle;
    FLineNumberStart: Integer;
    FGradient: Boolean;
    FGradientStartColor: TColor;
    FGradientEndColor: TColor;
    FGradientSteps: Integer;
    FInternalImage: TSynInternalImage;
    FTrackChanges: TSynTrackChanges;
    FBands: TSynBandsCollection;
    FTextFormat: TSynTextFormat;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetDigitCount(Value: Integer);
    procedure SetLeadingZeros(const Value: Boolean);
    procedure SetShowLineNumbers(const Value: Boolean);
    procedure SetUseFontStyle(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure SetZeroStart(const Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure FontChanged(Sender: TObject);
    procedure SetBorderStyle(const Value: TSynGutterBorderStyle);
    procedure SetLineNumberStart(const Value: Integer);
    procedure SetGradient(const Value: Boolean);
    procedure SetGradientStartColor(const Value: TColor);
    procedure SetGradientEndColor(const Value: TColor);
    procedure SetGradientSteps(const Value: Integer);
    procedure SetBands(const Value: TSynBandsCollection);
    function GetInternalImage: TSynInternalImage;
    function GetBandByKind(Kind: TSynGutterBandKind): TSynGutterBand;
    procedure Changed;
  protected
    function GetOwner: TPersistent; override;
  public
    AssignableBands: Boolean;
    constructor Create; overload;
    constructor Create(Owner: TPersistent); overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AutoSizeDigitCount;
    function FormatLineNumber(Line: Integer): string;
    function RealGutterWidth: Integer;
    function BandAtX(X: Integer): TSynGutterBand;
    // ++ DPI-Aware
    procedure ChangeScale(M, D: Integer); virtual;
    // -- DPI-Aware
    property InternalImage: TSynInternalImage read GetInternalImage;
    // Band returns the first band of a given kind
    property Band[Kind: TSynGutterBandKind]: TSynGutterBand read GetBandByKind;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property BorderStyle: TSynGutterBorderStyle read FBorderStyle
      write SetBorderStyle default gbsMiddle;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property BorderColor: TColor read FBorderColor write SetBorderColor
      default clWindow;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property DigitCount: Integer read FDigitCount write SetDigitCount default 4;
    property Font: TFont read FFont write SetFont;
    property ShowLineNumbers: Boolean read FShowLineNumbers
      write SetShowLineNumbers default False;
    property LeadingZeros: Boolean read FLeadingZeros write SetLeadingZeros
      default False;
    property UseFontStyle: Boolean read FUseFontStyle write SetUseFontStyle
      default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property ZeroStart: Boolean read FZeroStart write SetZeroStart
      default False;
    property LineNumberStart: Integer read FLineNumberStart
      write SetLineNumberStart default 1;
    property Gradient: Boolean read FGradient write SetGradient default False;
    property GradientStartColor: TColor read FGradientStartColor
      write SetGradientStartColor default clWindow;
    property GradientEndColor: TColor read FGradientEndColor
      write SetGradientEndColor default clBtnFace;
    property GradientSteps: Integer read FGradientSteps write SetGradientSteps
      default 48;
    property TrackChanges: TSynTrackChanges read FTrackChanges
      write FTrackChanges;
    property Bands: TSynBandsCollection read FBands write SetBands;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  {$ENDREGION 'TSynGutter'}

  {$REGION 'TSynBookmarkOpt'}

  TSynBookmarkOpt = class(TPersistent)
  private
    FBookmarkImages: TCustomImageList;
    FDrawBookmarksFirst: Boolean;
    FEnableKeys: Boolean;
    FGlyphsVisible: Boolean;
    FLeftMargin: Integer;
    FOwner: TComponent;
    FXoffset: Integer;
    FOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TCustomImageList);
    procedure SetDrawBookmarksFirst(Value: Boolean);
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: Integer);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    procedure ChangeScale(M, D: Integer); virtual;
  published
    property BookmarkImages: TCustomImageList read FBookmarkImages
      write SetBookmarkImages;
    property DrawBookmarksFirst: Boolean read FDrawBookmarksFirst
      write SetDrawBookmarksFirst default True;
    property EnableKeys: Boolean read FEnableKeys write FEnableKeys
      default True;
    property GlyphsVisible: Boolean read FGlyphsVisible write SetGlyphsVisible
      default True;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 2;
    property Xoffset: Integer read FXoffset write SetXOffset default 12;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$ENDREGION 'TSynBookmarkOpt'}

  {$REGION 'TSynGlyph'}

  /// <summary>
  ///   Encapsulates a bitmap that is either loaded from resources or assigned.
  /// </summary>
  /// <remarks>
  ///   Used for the word wrap glyph painted in the Gutter.
  /// </remarks>
  TSynGlyph = class(TPersistent)
  private
    FVisible: Boolean;
    FInternalGlyph, FGlyph: TBitmap;
    FPPI: Cardinal;
    FOnChange: TNotifyEvent;
    procedure SetGlyph(Value: TBitmap);
    procedure Changed;
    procedure SetVisible(Value: Boolean);
    function GetSize: TSize;
  public
    constructor Create(aModule: THandle; const aName: string);
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
    procedure ChangeScale(M, D: Integer); virtual;
    function D2D1Bitmap(RT: ID2D1RenderTarget): ID2D1Bitmap;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: TSize read GetSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$ENDREGION 'TSynGlyph'}

  {$REGION 'TSynInternalImage'}
  { TSynInternalImage }

  /// <summary>
  ///   Manages a bitmap containing many glyphs and loaded from resources
  /// </summary>
  /// <remarks>
  ///   Used for the bookmark images
  /// </remarks>
  TSynInternalImage = class(TObject)
  private
    FImages: TBitmap;
    FWidth: Integer;
    FHeight: Integer;
    FPPI: Integer;
    FCount: Integer;
  public
    constructor Create(aModule: THandle; const Name: string; Count: Integer);
    destructor Destroy; override;
    procedure Draw(RT: ID2D1RenderTarget; Number, X, Y, LineHeight: Integer);
    procedure ChangeScale(M, D: Integer); virtual;
  end;

  {$ENDREGION 'TSynInternalImage'}

  {$REGION 'TSynHotKey'}

const
  BorderWidth = 0;

type
  TSynBorderStyle = TBorderStyle;

  THKModifier = (hkShift, hkCtrl, hkAlt);
  THKModifiers = set of THKModifier;
  THKInvalidKey = (hcNone, hcShift, hcCtrl, hcAlt, hcShiftCtrl, hcShiftAlt,
    hcCtrlAlt, hcShiftCtrlAlt);
  THKInvalidKeys = set of THKInvalidKey;

  TSynHotKey = class(TCustomControl)
  private
    FBorderStyle: TSynBorderStyle;
    FHotKey: TShortCut;
    FInvalidKeys: THKInvalidKeys;
    FModifiers: THKModifiers;
    FPressedOnlyModifiers: Boolean;
    procedure SetBorderStyle(const Value: TSynBorderStyle);
    procedure SetHotKey(const Value: TShortCut);
    procedure SetInvalidKeys(const Value: THKInvalidKeys);
    procedure SetModifiers(const Value: THKModifiers);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Font;
    property Color;
    property BorderStyle: TSynBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property HotKey: TShortCut read FHotKey write SetHotKey
      default $0041; { Alt+A }
    property InvalidKeys: THKInvalidKeys read FInvalidKeys write SetInvalidKeys
      default [hcNone, hcShift];
    property Modifiers: THKModifiers read FModifiers write SetModifiers
      default [hkAlt];
  end;
  {$ENDREGION 'TSynHotKey'}

  {$REGION 'Indicators'}

  TSynIndicatorStyle = (sisTextDecoration, sisSquiggleMicrosoftWord,
    sisSquiggleWordPerfect, sisRectangle, sisFilledRectangle,
    sisRoundedRectangle, sisRoundedFilledRectangle);

  TSynIndicatorSpec = record
    Style: TSynIndicatorStyle;
    Foreground,
    Background: TD2D1ColorF;
    FontStyle: TFontStyles;
    constructor Create(AStyle: TSynIndicatorStyle; AForeground, ABackground: TD2D1ColorF;
      AFontStyle: TFontStyles);
    class function New(AStyle: TSynIndicatorStyle; AForeground, ABackground: TD2D1ColorF;
      AFontStyle: TFontStyles): TSynIndicatorSpec; static;
  end;

  TSynIndicator = record
    Id: TGUID;
    CharStart, CharEnd: Integer;
    Tag: NativeInt;  // for storing user data
    KeepOnLineChange: Boolean; // do not remove when line change but adjust CharStart/End
    constructor Create(aId: TGUID; aCharStart, aCharEnd: Integer;
      aTag: NativeInt = 0; aKeepOnLineChange: Boolean = False);
    class function New(aId: TGUID; aCharStart, aCharEnd: Integer;
      aTag: NativeInt = 0; aKeepOnLineChange: Boolean = False): TSynIndicator; static;
    class operator Equal(const A, B: TSynIndicator): Boolean;
  end;

  TSynIndicators = class
  private
    FOwner: TCustomControl;
    FRegister: TDictionary<TGUID, TSynIndicatorSpec>;
    FList: TDictionary<Integer, TArray<TSynIndicator>>;
    procedure InvalidateIndicator(Line: Integer; const Indicator: TSynIndicator);
  public
    constructor Create(Owner: TCustomControl);
    destructor Destroy; override;
    procedure RegisterSpec(Id: TGUID; Spec: TSynIndicatorSpec);
    function GetSpec(const Id: TGUID): TSynIndicatorSpec;
    procedure Add(Line: Integer; const Indicator: TSynIndicator; Invalidate: Boolean = True);
    // Clears all indicators
    procedure Clear; overload;
    // Clears all indicators with a given Id
    procedure Clear(const Id: TGUID; Invalidate: Boolean = True; Line: Integer = -1);
        overload;
    // Clears just one indicator
    procedure Clear(Line: Integer; const Indicator: TSynIndicator); overload;
    // Returns the indicators of a given line
    function LineIndicators(Line: Integer): TArray<TSynIndicator>;
    // Get all indicatoros of with a given Id or an array of Ids
    function GetById(const Id: TGUID): TArray<TPair<Integer, TSynIndicator>>; overload;
    function GetById(const Ids: TArray<TGUID>): TArray<TPair<Integer, TSynIndicator>>; overload;
    // Return the indicator at a given buffer or window position
    function IndicatorAtPos(Pos: TBufferCoord; const Id: TGUID; var Indicator: TSynIndicator): Boolean; overload;
    function IndicatorAtPos(Pos: TBufferCoord; const Ids: TArray<TGUID>; var Indicator: TSynIndicator): Boolean; overload;
    function IndicatorAtPos(Pos: TBufferCoord; var Indicator: TSynIndicator): Boolean; overload;
    function IndicatorAtMousePos(MousePos: TPoint; const Id: TGUID; var Indicator: TSynIndicator): Boolean; overload;
    function IndicatorAtMousePos(MousePos: TPoint; var Indicator: TSynIndicator): Boolean; overload;
    // Should only used by Synedit
    procedure LinesInserted(FirstLine, Count: Integer);
    procedure LinesDeleted(FirstLine, Count: Integer);
    procedure LinePut(aIndex: Integer; const OldLine: string);
    class procedure Paint(RT: ID2D1RenderTarget; Spec: TSynIndicatorSpec; const
        ClipR: TRect; StartOffset: Integer);
  end;
  {$ENDREGION 'TSynIndicators'}

  {$REGION 'TSynBracketsHighlight'}

  TSynBracketsHighlight = class
  public
    const MatchingBracketsIndicatorID: TGUID = '{EC19D246-8F03-42FE-BDFB-A11F3E60B00B}';
    const UnbalancedBracketIndicatorID: TGUID = '{259B198E-9963-4BA3-BDC7-34BA12F3CB10}';
  private
    FOwner: TPersistent;
  public
    constructor Create(Owner: TPersistent);
    procedure SetFontColorsAndStyle(const MatchingBracketsColor,
        UnbalancedBracketColor: TColor; FontStyle: TFontStyles);
    // SetIndicatorSpecs provides more painting options
    procedure SetIndicatorSpecs(const MatchingBracketsSpec,
      UnbalancedBracketSpec: TSynIndicatorSpec);
  end;

  {$ENDREGION 'TSynBracketsHighlight'}

  {$REGION 'TSynCarets'}

  TSynSelections = class;

  TSynCarets = class
  private
    FCaretsShown: Boolean;
    FBlinkTimer: TTimer;
    FCanvas: TCanvas;
    procedure Blink(Sender: TObject);
    procedure InvertCarets;
  public
    CaretSize: Integer; // is DPI scaled
    Shape: TCaretShape;
    CaretRects: TList<TRect>;
    constructor Create(Canvas: TCanvas);
    destructor Destroy; override;
    procedure HideCarets;
    procedure ShowCarets;
  end;

  {$ENDREGION 'TSynCarets'}

  {$REGION 'TSynSelections'}

  // TSynSelStorage and TSynSelectionsBase are now in shared SynEditSelections.pas
  // TSynSelections inherits all logic from TSynSelectionsBase and provides
  // thin VCL-specific overrides.
  TSynSelections = class(TSynSelectionsBase)
  protected
    procedure CaretsChanged; override;
    procedure DoInvalidateSelection(const Sel: TSynSelection); override;
    procedure DoRestoreSelection(const Sel: TSynSelection;
      EnsureVisible: Boolean); override;
    function GetLineText(ALine: Integer): string; override;
    function GetWordWrap: Boolean; override;
    function GetScrollPastEOL: Boolean; override;
    function GetRowLength(ARow: Integer): Integer; override;
    function BufferToDisplayPos(const P: TBufferCoord): TDisplayCoord; override;
    function DisplayToBufferPos(const P: TDisplayCoord): TBufferCoord; override;
    function SelectionToDisplayRow(var Sel: TSynSelection): Integer; override;
  public
    constructor Create(Owner: TPersistent);
  end;

  {$ENDREGION 'TSynSelections'}

  {$REGION 'Scrollbar Annotations'}
  TSynScrollbarAnnType = (sbaCarets, sbaBookmark, sbaTrackChanges,
     sbaCustom1, sbaCustom2, sbaCustom3);

  TSynScrollbarAnnPos = (sbpLeft, sbpSecondLeft, sbpMiddle,
    sbpSecondRight, sbpRight, sbpFullWidth);

  TScrollbarAnnotationInfoEvent = procedure(Sender: TObject;
    AnnType: TSynScrollbarAnnType; var Rows: TArray<Integer>;
    var Colors: TArray<TColor>) of object;

  TSynScrollbarAnnItem = class(TCollectionItem)
  private
    FAnnType: TSynScrollbarAnnType;
    FAnnPos: TSynScrollbarAnnPos;
    FOnGetInfo: TScrollbarAnnotationInfoEvent;
    FSelectionColor: TColor;
    FBookmarkColor: TColor;
    FFullRow: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetInfo(out Rows: TArray<Integer>; out Colors: TArray<TColor>);
  published
    property AnnType: TSynScrollbarAnnType read FAnnType write FAnnType;
    property AnnPos: TSynScrollbarAnnPos read FAnnPos write FAnnPos;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor
      default clDefault;
    property BookmarkColor: TColor read FBookmarkColor write FBookmarkColor
      default clDefault;
    property FullRow: Boolean read FFullRow write FFullRow;
    property OnGetInfo: TScrollbarAnnotationInfoEvent read FOnGetInfo
      write FOnGetInfo;
  end;

  TSynScrollbarAnnotations = class(TOwnedCollection)
  private
    function GetAnnotations(Index: Integer): TSynScrollbarAnnItem;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    procedure SetDefaultAnnotations;
    property Annotations[Index: Integer]: TSynScrollbarAnnItem
      read GetAnnotations; default;
  end;

  {$ENDREGION 'Scrollbar Annotations'}

  {$REGION 'TSynDisplayFlowControl'}

  TSynDisplayFlowControl = class(TPersistent)
  private
    FEnabled: Boolean;
    FColor: TColor;
  published
    constructor Create;
    procedure Assign(aSource: TPersistent); override;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Color: TColor read FColor write FColor default $0045FF; //clWebOrangeRed
  end;

   {$ENDREGION 'TSynDisplayFlowControl'}

  {$REGION 'TSynEditRedo'}

  TSynEditRedo = class(TEditAction);

  {$ENDREGION 'TSynEditRedo'}

  {$REGION 'TSynEditorOptionsContainer'}
  // Container class for storing/persisting SynEdit options
  // This class assign compatible with SynEdit
  TSynEditorOptionsContainer = class(TComponent)
  private
    FHideSelection: Boolean;
    FWantTabs: Boolean;
    FWordWrap: Boolean;
    FMaxUndo: Integer;
    FExtraLineSpacing: Integer;
    FTabWidth: Integer;
    FRightEdge: Integer;
    FSelectedColor: TSynSelectedColor;
    FIndentGuides: TSynIndentGuides;
    FDisplayFlowControl: TSynDisplayFlowControl;
    FRightEdgeColor: TColor;
    FFont: TFont;
    FBookmarks: TSynBookmarkOpt;
    FOverwriteCaret: TSynEditCaretType;
    FInsertCaret: TSynEditCaretType;
    FKeystrokes: TSynEditKeyStrokes;
    FOptions: TSynEditorOptions;
    FScrollOptions: TSynEditorScrollOptions;
    FSynGutter: TSynGutter;
    FColor: TColor;
    FActiveLineColor: TColor;
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    procedure SetBookmarks(const Value: TSynBookmarkOpt);
    procedure SetFont(const Value: TFont);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetSynGutter(const Value: TSynGutter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Options: TSynEditorOptions read FOptions write FOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property ScrollOptions: TSynEditorScrollOptions read FScrollOptions
      write FScrollOptions default SYNEDIT_DEFAULT_SCROLLOPTIONS;
    property BookmarkOptions: TSynBookmarkOpt read FBookmarks write SetBookmarks;
    property Color: TColor read FColor write FColor default clWindow;
    property Font: TFont read FFont write SetFont;
    property ExtraLineSpacing: Integer read FExtraLineSpacing
      write FExtraLineSpacing default 2;
    property Gutter: TSynGutter read FSynGutter write SetSynGutter;
    property RightEdge: Integer read FRightEdge write FRightEdge default 80;
    property RightEdgeColor: TColor read FRightEdgeColor write FRightEdgeColor
      default clSilver;
    property WantTabs: Boolean read FWantTabs write FWantTabs default True;
    property WordWrap: Boolean read FWordWrap write FWordWrap default False;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write FInsertCaret default ctVerticalLine;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write FOverwriteCaret default ctBlock;
    property HideSelection: Boolean read FHideSelection write FHideSelection
      default False;
    property MaxUndo: Integer read FMaxUndo write FMaxUndo default 0;
    property SelectedColor: TSynSelectedColor read FSelectedColor;
    property IndentGuides: TSynIndentGuides read FIndentGuides;
    property DisplayFlowControl: TSynDisplayFlowControl read FDisplayFlowControl;
    property TabWidth: Integer read FTabWidth write FTabWidth default 8;
    property Keystrokes: TSynEditKeyStrokes read FKeystrokes write SetKeystrokes;
    property ActiveLineColor: TColor read FActiveLineColor
      write FActiveLineColor default clNone;
    property VisibleSpecialChars: TSynVisibleSpecialChars
      read FVisibleSpecialChars write FVisibleSpecialChars default [];
  end;

  {$ENDREGION 'TSynEditorOptionsContainer'}

{$REGION 'VCL Utility Functions'}
// VCL-specific utility functions (moved from SynEditMiscProcs)
function DefaultFontName: string;
function GetCorrectFontWeight(Font: TFont): Integer;
function IsColorDark(AColor: TColor): Boolean;
function ColorToHTML(Color: TColor): string;
{$ENDREGION 'VCL Utility Functions'}

implementation

uses
  System.Rtti,
  System.Generics.Defaults,
  Vcl.GraphUtil,
  Vcl.Themes,
  SynEditMiscProcs,
  SynEditCodeFolding,
  SynEdit,
  SynEditTextBuffer;

{$REGION 'TSynSelectedColor - now in SynEditTypes.pas'}
{$ENDREGION}


{$REGION 'TSynGutter'}

procedure TSynGutter.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynGutter.ChangeScale(M, D: Integer);
begin
  BeginUpdate;
  try
    if Assigned(FInternalImage) then
      FInternalImage.ChangeScale(M, D);
    FCurrentPPI := M; // Vcl does the same
    FontChanged(Self); // Do font scaling
  finally
    EndUpdate;
  end;
end;

constructor TSynGutter.Create;
  procedure AddBand(AKind: TSynGutterBandKind; AWidth: Integer;
    IsVisible: Boolean);
  begin
    with FBands.Add as TSynGutterBand do
    begin
      FKind := AKind;
      FWidth := AWidth;
      FVisible := IsVisible;
    end;
  end;

begin
  inherited Create;
  FCurrentPPI := 96;
  FFont := TFont.Create;
  FFont.Name := DefaultFontName;
  FFont.Style := [];
  FFont.PixelsPerInch := Screen.DefaultPixelsPerInch;
  FFont.Size := 8;
  {$IF CompilerVersion >= 36}
  FFont.IsScreenFont := True;
  {$IFEND CompilerVersion >= 36}
  FUseFontStyle := True;
  FFont.OnChange := FontChanged;
  FontChanged(Self);

  FColor := clBtnFace;
  FVisible := True;
  FDigitCount := 4;
  FAutoSizeDigitCount := FDigitCount;
  FAutoSize := True;
  FBorderColor := clWindow;
  FBorderStyle := gbsMiddle;
  FLineNumberStart := 1;
  FZeroStart := False;
  FGradient := False;
  FGradientStartColor := clWindow;
  FGradientEndColor := clBtnFace;
  FGradientSteps := 48;

  AutoSizeDigitCount;

  FTrackChanges := TSynTrackChanges.Create(Self);

  FBands := TSynBandsCollection.Create(Self, TSynGutterBand);
  Bands.BeginUpdate;
  try
    AddBand(gbkMarks, 13, True);
    AddBand(gbkLineNumbers, 0, False);
    AddBand(gbkFold, 0, False);
    AddBand(gbkTrackChanges, 0, False);
    AddBand(gbkMargin, 3, True);
  finally
    Bands.EndUpdate;
  end;
  AssignableBands := True;
end;

constructor TSynGutter.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  Create;
end;

destructor TSynGutter.Destroy;
begin
  FOwner := nil;
  FFont.Free;
  FTrackChanges.Free;
  FBands.Free;
  FInternalImage.Free;
  inherited Destroy;
end;

procedure TSynGutter.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TSynGutter.Assign(Source: TPersistent);
var
  Src: TSynGutter;
begin
  if Source is TSynGutter then
  begin
    BeginUpdate;
    try
      Src := TSynGutter(Source);
      FFont.Assign(Src.Font);
      FUseFontStyle := Src.FUseFontStyle;
      FColor := Src.FColor;
      FVisible := Src.FVisible;
      FLeadingZeros := Src.FLeadingZeros;
      FZeroStart := Src.FZeroStart;
      FDigitCount := Src.FDigitCount;
      FAutoSize := Src.FAutoSize;
      FAutoSizeDigitCount := Src.FAutoSizeDigitCount;
      FShowLineNumbers := Src.FShowLineNumbers;
      FLineNumberStart := Src.FLineNumberStart;
      FBorderColor := Src.FBorderColor;
      FBorderStyle := Src.FBorderStyle;
      FGradient := Src.FGradient;
      FGradientStartColor := Src.FGradientStartColor;
      FGradientEndColor := Src.FGradientEndColor;
      FGradientSteps := Src.FGradientSteps;
      if AssignableBands and Src.AssignableBands then
        FBands.Assign(Src.FBands);
      FTrackChanges.Assign(Src.FTrackChanges);
      AutoSizeDigitCount;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TSynGutter.AutoSizeDigitCount;
var
  nDigits: Integer;
  SynEdit: TCustomSynEdit;
  LinesCount: Integer;
begin
  SynEdit := TCustomSynEdit(FOwner);
  if Assigned(SynEdit) and FAutoSize then
  begin
    LinesCount := SynEdit.Lines.Count;
    if FZeroStart then
      Dec(LinesCount)
    else if FLineNumberStart > 1 then
      Inc(LinesCount, FLineNumberStart - 1);

    nDigits := Max(Length(IntToStr(LinesCount)), FDigitCount);
    if FAutoSizeDigitCount <> nDigits then
    begin
      FAutoSizeDigitCount := nDigits;
      Changed;
    end;
  end
  else
    FAutoSizeDigitCount := FDigitCount;
end;

function TSynGutter.BandAtX(X: Integer): TSynGutterBand;
var
  I, L: Integer;
  Band: TSynGutterBand;
begin
  Result := nil;
  L := 0;
  for I := 0 to Bands.Count - 1 do
  begin
    Band := Bands[I];
    if not Band.Visible then
      Continue;
    Inc(L, Band.RealWidth);
    if X < L then
      Exit(Band);
  end;
end;

procedure TSynGutter.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TSynGutter.FormatLineNumber(Line: Integer): string;
var
  I: Integer;
begin
  if FZeroStart then
    Dec(Line)
  else if FLineNumberStart > 1 then
    Inc(Line, FLineNumberStart - 1);
  Result := Format('%*d', [FAutoSizeDigitCount, Line]);
  if FLeadingZeros then
    for I := 1 to FAutoSizeDigitCount - 1 do
    begin
      if (Result[I] <> ' ') then
        Break;
      Result[I] := '0';
    end;
end;

function TSynGutter.RealGutterWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  if FVisible and Assigned(FOwner) then
  begin
    for I := 0 to Bands.Count - 1 do
      Inc(Result, Bands[I].RealWidth);
  end;
end;

procedure TSynGutter.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AutoSizeDigitCount;
    Changed;
  end;
end;

procedure TSynGutter.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetFont(Value: TFont);
begin
  if Screen.Fonts.IndexOf(Font.Name) < 0 then
    Font.Name := DefaultFontName;
  FFont.Assign(Value);
end;

procedure TSynGutter.FontChanged(Sender: TObject);
var
  TempFont: TFont;
begin
  FFont.OnChange := nil;  // avoid recursion
  if Assigned(FOwner) then
    FFont.Quality := TCustomSynEdit(FOWner).FontQuality;
  // revert to default font if not monospaced or invalid
  if not IsFontMonospacedAndValid(FFont) then
    Font.Name := DefaultFontName;
  Font.OnChange := FontChanged;
  TempFont := TFont.Create;
  try
    // scale font height
    TempFont.PixelsPerInch := FCurrentPPI;
    TempFont.Assign(FFont);
    FTextFormat.Create(TempFont, 1, 0, 0);
  finally
    TempFont.Free;
  end;
  FCharWidth := FTextFormat.CharWidth;
  Changed;
end;

procedure TSynGutter.SetDigitCount(Value: Integer);
begin
  Value := MinMax(Value, 2, 12);
  if FDigitCount <> Value then
  begin
    FDigitCount := Value;
    AutoSizeDigitCount;
    Changed;
  end;
end;

procedure TSynGutter.SetLeadingZeros(const Value: Boolean);
begin
  if FLeadingZeros <> Value then
  begin
    FLeadingZeros := Value;
    AutoSizeDigitCount;
    Changed;
  end;
end;

procedure TSynGutter.SetShowLineNumbers(const Value: Boolean);
begin
  if FShowLineNumbers <> Value then
  begin
    FShowLineNumbers := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetUseFontStyle(Value: Boolean);
begin
  if FUseFontStyle <> Value then
  begin
    FUseFontStyle := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetZeroStart(const Value: Boolean);
begin
  if FZeroStart <> Value then
  begin
    FZeroStart := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetBorderStyle(const Value: TSynGutterBorderStyle);
begin
  FBorderStyle := Value;
  Changed;
end;

procedure TSynGutter.SetLineNumberStart(const Value: Integer);
begin
  if Value <> FLineNumberStart then
  begin
    FLineNumberStart := Value;
    if FLineNumberStart < 0 then
      FLineNumberStart := 0;
    if FLineNumberStart = 0 then
      FZeroStart := True
    else
      FZeroStart := False;
    AutoSizeDigitCount;
    Changed;
  end;
end;

procedure TSynGutter.SetBands(const Value: TSynBandsCollection);
begin
  FBands.Assign(Value);
end;

procedure TSynGutter.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetGradient(const Value: Boolean);
begin
  if Value <> FGradient then
  begin
    FGradient := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetGradientEndColor(const Value: TColor);
begin
  if Value <> FGradientEndColor then
  begin
    FGradientEndColor := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetGradientStartColor(const Value: TColor);
begin
  if Value <> FGradientStartColor then
  begin
    FGradientStartColor := Value;
    Changed;
  end;
end;

procedure TSynGutter.SetGradientSteps(const Value: Integer);
begin
  if Value <> FGradientSteps then
  begin
    FGradientSteps := Value;
    if FGradientSteps < 2 then
      FGradientSteps := 2;
    Changed;
  end;
end;

function TSynGutter.GetBandByKind(Kind: TSynGutterBandKind): TSynGutterBand;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Bands.Count - 1 do
    if Bands[I].Kind = Kind then
      Exit(Bands[I])
end;

function TSynGutter.GetInternalImage: TSynInternalImage;
begin
  if not Assigned(FInternalImage) then
  begin
    FInternalImage := TSynInternalImage.Create(HINSTANCE,
      'SynEditInternalImages', 10);

    if Assigned(FOwner) then
      FInternalImage.ChangeScale(FCurrentPPI, 96);
  end;
  Result := FInternalImage;
end;

function TSynGutter.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{$ENDREGION}


{$REGION 'TSynBookmarkOpt'}

procedure TSynBookmarkOpt.ChangeScale(M, D: Integer);
begin
  FLeftMargin := MulDiv(FLeftMargin, M, D);
  FXoffset := MulDiv(FXoffset, M, D);
end;

constructor TSynBookmarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  FDrawBookmarksFirst := True;
  FEnableKeys := True;
  FGlyphsVisible := True;
  FLeftMargin := 2;
  FOwner := AOwner;
  FXoffset := 12;
end;

procedure TSynBookmarkOpt.Assign(Source: TPersistent);
var
  Src: TSynBookmarkOpt;
begin
  if Source is TSynBookmarkOpt then
  begin
    Src := TSynBookmarkOpt(Source);
    FBookmarkImages := Src.FBookmarkImages;
    FDrawBookmarksFirst := Src.FDrawBookmarksFirst;
    FEnableKeys := Src.FEnableKeys;
    FGlyphsVisible := Src.FGlyphsVisible;
    FLeftMargin := Src.FLeftMargin;
    FXoffset := Src.FXoffset;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TSynBookmarkOpt.SetBookmarkImages(const Value: TCustomImageList);
begin
  if FBookmarkImages <> Value then
  begin
    FBookmarkImages := Value;
    if Assigned(FBookmarkImages) then
      FBookmarkImages.FreeNotification(FOwner);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynBookmarkOpt.SetDrawBookmarksFirst(Value: Boolean);
begin
  if Value <> FDrawBookmarksFirst then
  begin
    FDrawBookmarksFirst := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynBookmarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if FGlyphsVisible <> Value then
  begin
    FGlyphsVisible := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynBookmarkOpt.SetLeftMargin(Value: Integer);
begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynBookmarkOpt.SetXOffset(Value: Integer);
begin
  if FXoffset <> Value then
  begin
    FXoffset := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{$ENDREGION}


{$REGION 'TSynGlyph'}

procedure TSynGlyph.ChangeScale(M, D: Integer);
begin
  FPPI := M; // As Delphi does
end;

constructor TSynGlyph.Create(aModule: THandle; const aName: string);
begin
  inherited Create;

  if aName <> '' then
  begin
    FInternalGlyph := TBitmap.Create;
    FInternalGlyph.LoadFromResourceName(aModule, aName);
    FInternalGlyph.AlphaFormat := afDefined;
  end;

  FPPI := Screen.DefaultPixelsPerInch;

  FVisible := True;
  FGlyph := TBitmap.Create;
end;

function TSynGlyph.D2D1Bitmap(RT: ID2D1RenderTarget): ID2D1Bitmap;
var
  BM: TBitmap;
begin
  if FGlyph.Empty then
    BM := FInternalGlyph
  else
    BM := FGlyph;
  Result := D2D1BitmapFromBitmap(BM, RT);
end;

destructor TSynGlyph.Destroy;
begin
  if Assigned(FInternalGlyph) then
    FreeAndNil(FInternalGlyph);

  FGlyph.Free;
  inherited Destroy;
end;

function TSynGlyph.GetSize: TSize;
var
  BM: TBitmap;
begin
  if FGlyph.Empty then
    BM := FInternalGlyph
  else
    BM := FGlyph;
  Result.Create(MulDiv(BM.Width, FPPI, 96), MulDiv(BM.Height, FPPI, 96));
end;

procedure TSynGlyph.Assign(aSource: TPersistent);
var
  vSrc: TSynGlyph;
begin
  if aSource is TSynGlyph then
  begin
    vSrc := TSynGlyph(aSource);
    FInternalGlyph.Assign(vSrc.FInternalGlyph);
    FVisible := vSrc.FVisible;
    FGlyph.Assign(vSrc.FGlyph);
  end
  else
    inherited;
  Changed;
end;

procedure TSynGlyph.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  FGlyph.AlphaFormat := afDefined;
  Changed;
end;

procedure TSynGlyph.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynGlyph.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{$ENDREGION}




{$REGION 'TSynInternalImage'}

type
  TInternalResource = class(TObject)
  public
    UsageCount: Integer;
    Name: string;
    Bitmap: TBitmap;
  end;

procedure TSynInternalImage.ChangeScale(M, D: Integer);
begin
  FPPI := M; // As Delphi does
end;

constructor TSynInternalImage.Create(aModule: THandle; const Name: string;
  Count: Integer);
begin
  inherited Create;
  FPPI := 96;
  FImages := TBitmap.Create;
  FImages.LoadFromResourceName(aModule, Name);
  FImages.AlphaFormat := afPremultiplied;
  FWidth := (FImages.Width + Count shr 1) div Count;
  FHeight := FImages.Height;
  FCount := Count;
end;

destructor TSynInternalImage.Destroy;
begin
  FImages.Free;
  inherited;
end;

procedure TSynInternalImage.Draw(RT: ID2D1RenderTarget;
  Number, X, Y, LineHeight: Integer);
var
  ScaledW, ScaledH: Integer;
  rcSrc, rcDest: TRectF;
  BM: ID2D1Bitmap;
begin
  if (Number >= 0) and (Number < FCount) then
  begin
    ScaledW := MulDiv(FWidth, FPPI, 96);
    ScaledH := MulDiv(FHeight, FPPI, 96);

    rcSrc := Rect(Number * FWidth, 0, (Number + 1) * FWidth, FHeight);
    rcDest := Rect(0, 0, ScaledW, ScaledH);
    rcDest := rcDest.FitInto(Rect(X, Y, X + ScaledW, Y + LineHeight)).Round;

    BM := D2D1BitmapFromBitmap(FImages, RT);
    RT.DrawBitmap(BM, PD2D1RectF(@rcDest), 1,
      D2D1_BITMAP_INTERPOLATION_MODE_LINEAR, PD2D1RectF(@rcSrc));
  end;
end;

{$ENDREGION}


{$REGION 'TSynHotKey'}

function KeySameAsShiftState(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (Key = SYNEDIT_SHIFT) and (ssShift in Shift) or
    (Key = SYNEDIT_CONTROL) and (ssCtrl in Shift) or (Key = SYNEDIT_MENU) and
    (ssAlt in Shift);
end;

function ModifiersToShiftState(Modifiers: THKModifiers): TShiftState;
begin
  Result := [];
  if hkShift in Modifiers then
    Include(Result, ssShift);
  if hkCtrl in Modifiers then
    Include(Result, ssCtrl);
  if hkAlt in Modifiers then
    Include(Result, ssAlt);
end;

function ShiftStateToTHKInvalidKey(Shift: TShiftState): THKInvalidKey;
begin
  Shift := Shift * [ssShift, ssAlt, ssCtrl];
  if Shift = [ssShift] then
    Result := hcShift
  else if Shift = [ssCtrl] then
    Result := hcCtrl
  else if Shift = [ssAlt] then
    Result := hcAlt
  else if Shift = [ssShift, ssCtrl] then
    Result := hcShiftCtrl
  else if Shift = [ssShift, ssAlt] then
    Result := hcShiftAlt
  else if Shift = [ssCtrl, ssAlt] then
    Result := hcCtrlAlt
  else if Shift = [ssShift, ssCtrl, ssAlt] then
    Result := hcShiftCtrlAlt
  else
    Result := hcNone;
end;

function ShortCutToTextEx(Key: Word; Shift: TShiftState): string;
begin
  if ssCtrl in Shift then
    Result := SmkcCtrl;
  if ssShift in Shift then
    Result := Result + SmkcShift;
  if ssAlt in Shift then
    Result := Result + SmkcAlt;

  Result := Result + ShortCutToText(TShortCut(Key));
  if Result = '' then
    Result := srNone;
end;

constructor TSynHotKey.Create(AOwner: TComponent);
begin
  inherited;

  BorderStyle := bsSingle;
  ControlStyle := ControlStyle + [csNeedsBorderPaint];

  FInvalidKeys := [hcNone, hcShift];
  FModifiers := [hkAlt];
  SetHotKey($0041); { Alt+A }

  ParentColor := False;
  Color := clWindow;
  TabStop := True;
end;

procedure TSynHotKey.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TSynBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or BorderStyles[FBorderStyle] or WS_CLIPCHILDREN;

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TSynHotKey.DoExit;
begin
  inherited;
  if FPressedOnlyModifiers then
  begin
    Text := srNone;
    Invalidate;
  end;
end;

procedure TSynHotKey.KeyDown(var Key: Word; Shift: TShiftState);
var
  MaybeInvalidKey: THKInvalidKey;
  SavedKey: Word;
begin
  SavedKey := Key;
  FPressedOnlyModifiers := KeySameAsShiftState(Key, Shift);

  MaybeInvalidKey := ShiftStateToTHKInvalidKey(Shift);
  if MaybeInvalidKey in FInvalidKeys then
    Shift := ModifiersToShiftState(FModifiers);

  if not FPressedOnlyModifiers then
  begin
    FHotKey := ShortCut(Key, Shift)
  end
  else
  begin
    FHotKey := 0;
    Key := 0;
  end;

  if Text <> ShortCutToTextEx(Key, Shift) then
  begin
    Text := ShortCutToTextEx(Key, Shift);
    Invalidate;
    SetCaretPos(BorderWidth + 1 + Canvas.TextWidth(Text), BorderWidth + 1);
  end;

  Key := SavedKey;
end;

procedure TSynHotKey.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FPressedOnlyModifiers then
  begin
    Text := srNone;
    Invalidate;
    SetCaretPos(BorderWidth + 1 + Canvas.TextWidth(Text), BorderWidth + 1);
  end;
end;

procedure TSynHotKey.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  SetFocus;
end;

procedure TSynHotKey.Paint;
var
  R: TRect;
begin
  R := ClientRect;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  InflateRect(R, -BorderWidth, -BorderWidth);
  Canvas.FillRect(R);
  Canvas.Font := Font;
  Canvas.TextRect(R, BorderWidth + 1, BorderWidth + 1, Text);
end;

procedure TSynHotKey.SetBorderStyle(const Value: TSynBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TSynHotKey.SetHotKey(const Value: TShortCut);
var
  Key: Word;
  Shift: TShiftState;
  MaybeInvalidKey: THKInvalidKey;
begin
  ShortCutToKey(Value, Key, Shift);

  MaybeInvalidKey := ShiftStateToTHKInvalidKey(Shift);
  if MaybeInvalidKey in FInvalidKeys then
    Shift := ModifiersToShiftState(FModifiers);

  FHotKey := ShortCut(Key, Shift);
  Text := ShortCutToTextEx(Key, Shift);
  Invalidate;
  if not Visible then
    SetCaretPos(BorderWidth + 1 + Canvas.TextWidth(Text), BorderWidth + 1);
end;

procedure TSynHotKey.SetInvalidKeys(const Value: THKInvalidKeys);
begin
  FInvalidKeys := Value;
  SetHotKey(FHotKey);
end;

procedure TSynHotKey.SetModifiers(const Value: THKModifiers);
begin
  FModifiers := Value;
  SetHotKey(FHotKey);
end;

procedure TSynHotKey.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

procedure TSynHotKey.WMKillFocus(var Msg: TWMKillFocus);
begin
  DestroyCaret;
end;

procedure TSynHotKey.WMSetFocus(var Msg: TWMSetFocus);
begin
  Canvas.Font := Font;
  CreateCaret(Handle, 0, 1, -Canvas.Font.Height + 2);
  SetCaretPos(BorderWidth + 1 + Canvas.TextWidth(Text), BorderWidth + 1);
  ShowCaret(Handle);
end;


{$ENDREGION}


{$REGION 'TSynGutterBand'}

procedure TSynGutterBand.Assign(Source: TPersistent);
var
  Src: TSynGutterBand;
begin
  if Source is TSynGutterBand then
  begin
    Src := TSynGutterBand(Source);
    FKind := Src.FKind;
    FVisible := Src.FVisible;
    FBackground := Src.FBackground;
    FWidth := Src.FWidth;
    Changed(False);
  end
  else
    inherited;
end;

constructor TSynGutterBand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FBackground := gbbGutter;
end;

procedure TSynGutterBand.DoClick(Sender: TObject; Button: TMouseButton;
  X, Y, Row, Line: Integer);
var
  SynEdit: TCustomSynEdit;
  Index, I, BmkX, BmkY: Integer;
  rcFold: TRect;
begin
  SynEdit := TCustomSynEdit(Editor);

  if Visible and (FKind = gbkMarks) then
  begin
    // Click on marks band clears bookmark on that line
    for I := 0 to 9 do
      if SynEdit.GetBookmark(I, BmkX, BmkY) and (BmkY = Line) then
      begin
        SynEdit.ClearBookmark(I);
        Break;
      end;
  end
  else if Visible and (FKind = gbkFold) then
  begin
    if SynEdit.AllFoldRanges.FoldStartAtLine(Line, Index) then
    begin
      rcFold := FoldShapeRect(Row, Line);
      // See if we actually clicked on the rectangle...
      if not rcFold.IsEmpty and PtInRect(rcFold, Point(X, Y)) then
      begin
        if SynEdit.AllFoldRanges.Ranges[Index].Collapsed then
          SynEdit.Uncollapse(Index)
        else
          SynEdit.Collapse(Index);
      end;
    end;
  end;

  if Assigned(FOnClick) then
    FOnClick(Sender, Button, X, Y, Row, Line);
end;

procedure TSynGutterBand.DoMouseCursor(Sender: TObject;
  X, Y, Row, Line: Integer; var Cursor: TCursor);
var
  SynEdit: TCustomSynEdit;
  Index: Integer;
  rcFold: TRect;
begin
  if Visible and (FKind = gbkFold) then
  begin
    SynEdit := TCustomSynEdit(Editor);
    if SynEdit.AllFoldRanges.FoldStartAtLine(Line, Index) then
    begin
      rcFold := FoldShapeRect(Row, Line);
      if not rcFold.IsEmpty and PtInRect(rcFold, Point(X, Y)) then
        Cursor := crHandPoint
    end;
  end;

  if Assigned(FOnMouseCursor) then
    FOnMouseCursor(Sender, X, Y, Row, Line, Cursor);
end;

procedure TSynGutterBand.DoPaintLines(RT: ID2D1RenderTarget; ClipR: TRect;
    const FirstRow, LastRow: Integer);
// Drawing of builtin bands
begin
  case FKind of
    gbkMarks:
      PaintMarks(RT, ClipR, FirstRow, LastRow);
    gbkLineNumbers:
      PaintLineNumbers(RT, ClipR, FirstRow, LastRow);
    gbkFold:
      PaintFoldShapes(RT, ClipR, FirstRow, LastRow);
    gbkTrackChanges:
      PaintTrackChanges(RT, ClipR, FirstRow, LastRow);
    gbkMargin:
      PaintMargin(RT, ClipR, FirstRow, LastRow);
  end;
end;

function TSynGutterBand.FoldShapeRect(Row, Line: Integer): TRect;
// Given that WordWrap and CodeFolding are mutally exclusive Row = Line
// But at some point this could be relaxed
var
  SynEdit: TCustomSynEdit;
  L, Index: Integer;
  ShapeSize: Integer;
  Margin: Integer;
begin
  Result := TRect.Empty;
  if not Visible or (FKind <> gbkFold) then
    Exit;
  SynEdit := TCustomSynEdit(Editor);
  if SynEdit.RowToLine(Row) <> Line then
    Exit;

  if SynEdit.AllFoldRanges.FoldStartAtLine(Line, Index) then
  begin
    ShapeSize := SynEdit.CodeFolding.ScaledGutterShapeSize(Gutter.FCurrentPPI);
    L := LeftX;
    if L < 0 then
      Exit;
    Margin := MulDiv(MarginX, Gutter.FCurrentPPI, 96);
    Result.TopLeft := Point(L + Margin, (Row - SynEdit.TopLine) *
      SynEdit.LineHeight + (SynEdit.LineHeight - ShapeSize) div 2);
    Result.BottomRight := Result.TopLeft;
    Result.BottomRight.Offset(ShapeSize, ShapeSize);
  end;
end;

function TSynGutterBand.GetDisplayName: string;
begin
  Result := Format('%s - kind: %s',
    [ClassName, TRttiEnumerationType.GetName<TSynGutterBandKind>(FKind)])
end;

function TSynGutterBand.GetEditor: TComponent;
begin
  if Assigned(Gutter) then
    Result := Gutter.GetOwner as TComponent
  else
    Result := nil;
end;

function TSynGutterBand.GetLeftX: Integer;
var
  I, L: Integer;
  Band: TSynGutterBand;
begin
  Result := -1;
  L := 0;
  if Assigned(Gutter) then
    for I := 0 to Gutter.Bands.Count - 1 do
    begin
      Band := Gutter.Bands[I];
      if not Band.Visible then
        Continue;
      if Gutter.Bands[I] = Self then
        Exit(L);
      Inc(L, Band.RealWidth);
    end;
end;

function TSynGutterBand.GetSynGutter: TSynGutter;
begin
  if Assigned(Collection) then
    Result := Collection.Owner as TSynGutter
  else
    Result := nil;
end;

function TSynGutterBand.GetVisible: Boolean;
begin
  Result := FVisible;
  case FKind of
    gbkLineNumbers:
      Result := Assigned(Gutter) and Gutter.ShowLineNumbers;
    gbkFold:
      Result := Assigned(Editor) and TCustomSynEdit(Editor).UseCodeFolding;
    gbkTrackChanges:
      Result := Assigned(Gutter) and Gutter.TrackChanges.Visible;
  end;
end;

function TSynGutterBand.GetWidth: Integer;
begin
  case FKind of
    gbkLineNumbers, gbkFold, gbkTrackChanges:
      Result := 0;
  else
    Result := FWidth;
  end;
end;

function TSynGutterBand.IsVisibleStored: Boolean;
begin
  Result := not FVisible and
    not(FKind in [gbkLineNumbers, gbkFold, gbkTrackChanges]);
end;

function TSynGutterBand.IsWidthStored: Boolean;
begin
  Result := not(FKind in [gbkLineNumbers, gbkFold, gbkTrackChanges]);
end;

procedure TSynGutterBand.PaintFoldShapes(RT: ID2D1RenderTarget; ClipR: TRect;
    const FirstRow, LastRow: Integer);
var
  SynEdit: TCustomSynEdit;
  vLine: Integer;
  cRow: Integer;
  rcFold: TRect;
  X, Y: Integer;
  FoldRange: TSynFoldRange;
  Index: Integer;
  Margin: Integer;
  PMMargin: Integer;
  ShapeSize: Integer;
  PPI: Integer;
  Brush: ID2D1Brush;
begin
  SynEdit := TCustomSynEdit(Editor);
  Assert(Assigned(SynEdit));
  Assert(Assigned(Gutter));
  PPI := Gutter.FCurrentPPI;

  // Draw the folding lines and squares
  if SynEdit.UseCodeFolding then
  begin
    Margin := MulDiv(MarginX, PPI, 96);
    PMMargin := Margin div 2;
    ShapeSize := SynEdit.CodeFolding.ScaledGutterShapeSize(PPI);

    Brush := TSynDWrite.SolidBrush(SynEdit.CodeFolding.FolderBarLinesColor);

    RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
    for cRow := FirstRow to LastRow do
    begin
      vLine := SynEdit.RowToLine(cRow);
      if (vLine > SynEdit.Lines.Count) { and not (SynEdit.Lines.Count = 0) }
      then
        Break;

      rcFold.TopLeft := Point(ClipR.Left + Margin, (cRow - SynEdit.TopLine) *
        SynEdit.LineHeight + (SynEdit.LineHeight - ShapeSize) div 2);
      rcFold.BottomRight := rcFold.TopLeft;
      // Direct2D includes both the first and the last point in the rectangle!
      rcFold.BottomRight.Offset(ShapeSize - 1, ShapeSize - 1);

      // Any fold ranges beginning on this line?
      if SynEdit.AllFoldRanges.FoldStartAtLine(vLine, Index) then
      begin
        FoldRange := SynEdit.AllFoldRanges.Ranges[Index];
        // Paint the square
        RT.DrawRectangle(rcFold, Brush);

        // Paint minus sign
        Y := rcFold.Top + ShapeSize div 2;
        // DrawLine paints the last pixel as well in Direct2D
        RT.DrawLine(
          Point(rcFold.Left + PMMargin, Y),
          Point(rcFold.Right - PMMargin - 1, Y), Brush);

        // Paint vertical line of plus sign
        if FoldRange.Collapsed then
        begin
          X := rcFold.Left + ShapeSize div 2;
          RT.DrawLine(
            Point(X, rcFold.Top  + PMMargin),
            Point(X, rcFold.Bottom - PMMargin - 1), Brush);
        end
        else
        // Draw the bottom part of a line
        begin
          X := rcFold.Left + ShapeSize div 2;
          RT.DrawLine(D2D1PointF(X, rcFold.Bottom),
            D2D1PointF(X, (cRow - SynEdit.TopLine + 1) * SynEdit.LineHeight), Brush);
        end;
      end
      else
      begin
        // Need to paint a line end?
        if SynEdit.AllFoldRanges.FoldEndAtLine(vLine, Index) then
        begin
          X := rcFold.Left + ShapeSize div 2;
          Y := rcFold.Top + (rcFold.Bottom - rcFold.Top) div 2;
          RT.DrawLine(
            D2D1PointF(X, (cRow - SynEdit.TopLine) * SynEdit.LineHeight),
            D2D1PointF(X, Y),
            Brush);
          RT.DrawLine(
            D2D1PointF(X, Y),
            D2D1PointF(rcFold.Right, Y),
            Brush);
        end;
        // Need to paint a line?
        if SynEdit.AllFoldRanges.FoldAroundLine(vLine, Index) then
        begin
          X := rcFold.Left + ShapeSize div 2;
          RT.DrawLine(
            D2D1PointF(X, (cRow - SynEdit.TopLine) * SynEdit.LineHeight),
            D2D1PointF(X, (cRow - SynEdit.TopLine + 1) * SynEdit.LineHeight),
            Brush);
        end;
      end;
    end;
    RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
  end;
end;

procedure TSynGutterBand.PaintLineNumbers(RT: ID2D1RenderTarget; ClipR: TRect;
    const FirstRow, LastRow: Integer);
var
  SynEdit: TCustomSynEdit;
  Row, Line: Integer;
  LineTop: Integer;
  LineRect: TRect;
  PPI: Integer;
  S: string;
  TextFormat: TSynTextFormat;
  WordWrapGlyph: ID2D1Bitmap;
  RectF: TRectF;
  FontColor: TColor;
begin
  SynEdit := TCustomSynEdit(Editor);
  Assert(Assigned(Gutter));
  Assert(Assigned(SynEdit));
  PPI := Gutter.FCurrentPPI;

  if Gutter.UseFontStyle then
  begin
    TextFormat := Gutter.FTextFormat;
    FontColor := Gutter.Font.Color;
  end
  else
  begin
    TextFormat := SynEdit.TextFormat;
    FontColor := SynEdit.Font.Color;
  end;
  TextFormat.IDW.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
  TextFormat.IDW.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);

  if SynEdit.WordWrap and SynEdit.WordWrapGlyph.Visible then
  try
    WordWrapGlyph := SynEdit.WordWrapGlyph.D2D1Bitmap(RT);
  except
    WordWrapGlyph := nil;
  end;

  for Row := FirstRow to LastRow do
  begin
    Line := SynEdit.RowToLine(Row);
    LineTop := (Row - SynEdit.TopLine) * SynEdit.LineHeight;
    LineRect := Rect(ClipR.Left + MulDiv(MarginX, PPI, 96), LineTop,
      ClipR.Right, LineTop + SynEdit.LineHeight);

    if SynEdit.WordWrap and SynEdit.WordWrapGlyph.Visible and
      (Row <> SynEdit.LineToRow(Line)) and Assigned(WordWrapGlyph)
    then
    begin
      // paint wrapped line glyphs
      RectF := LineRect;
      RectF := Rect(0, 0, 0, 0);
      RectF.Size := SynEdit.WordWrapGlyph.Size;
      RectF.Offset(LineRect.Left + LineRect.Width - RectF.Width,
        LineRect.Top + (LineRect.Height - RectF.Height) / 2);
      if not LineRect.Contains(RectF.Round) then
      begin
        RectF := RectF.FitInto(LineRect);
        RectF.Offset(LineRect.Right - RectF.Right, 0);
      end;
      RT.DrawBitmap(WordWrapGlyph, PD2D1RectF(@RectF), 1000);
    end
    else
    begin
      // paint line numbers
      S := Gutter.FormatLineNumber(Line);
      if Assigned(SynEdit.OnGutterGetText) then
        SynEdit.OnGutterGetText(SynEdit, Line, S);
      RT.DrawText(PChar(S), S.Length, TextFormat.IDW, LineRect,
        TSynDWrite.SolidBrush(FontColor),
        D2D1_DRAW_TEXT_OPTIONS_CLIP +
        IfThen(TOSVersion.Check(6,3), D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT, 0),
        DWRITE_MEASURING_MODE_GDI_NATURAL);
    end;
  end;

  if not Gutter.UseFontStyle then
  begin
    TextFormat.IDW.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
    TextFormat.IDW.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
  end;
end;

procedure TSynGutterBand.PaintLines(RT: ID2D1RenderTarget; ClipR: TRect; const
    FirstRow, LastRow: Integer);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  if Assigned(FOnPaintLines) then
    FOnPaintLines(RT, ClipR, FirstRow, LastRow, DoDefault);
  if DoDefault then
    DoPaintLines(RT, ClipR, FirstRow, LastRow);
end;

procedure TSynGutterBand.PaintMargin(RT: ID2D1RenderTarget; ClipR: TRect; const
    FirstRow, LastRow: Integer);
var
  Offset: Integer;
begin
  if (Gutter.BorderStyle <> gbsNone) then
  begin
    RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
      if Gutter.BorderStyle = gbsMiddle then
        Offset := Max(2, (ClipR.Right - ClipR.Left) div 2)
      else
        Offset := 1;
      RT.DrawLine(
        Point(ClipR.Right - Offset, ClipR.Top),
        Point(ClipR.Right - Offset, ClipR.Bottom),
        TSynDWrite.SolidBrush(Gutter.BorderColor));
    RT.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
  end;
end;

procedure TSynGutterBand.PaintMarks(RT: ID2D1RenderTarget; ClipR: TRect; const
    FirstRow, LastRow: Integer);
var
  SynEdit: TCustomSynEdit;

  procedure DrawMark(aMark: TSynEditMark; var aGutterOff: Integer;
    aMarkRow: Integer);
  var
    VOffset: Integer;
    ScaleF: Single;
  begin
    if (not aMark.InternalImage) and
      Assigned(SynEdit.BookMarkOptions.BookmarkImages) and
      InRange(aMark.ImageIndex, 0,
        SynEdit.BookMarkOptions.BookmarkImages.Count - 1) then
    begin
      if aMark.IsBookmark = SynEdit.BookMarkOptions.DrawBookmarksFirst then
        aGutterOff := 0
      else if aGutterOff = 0 then
        aGutterOff := SynEdit.BookMarkOptions.Xoffset;
      vOffset := Max(0, (SynEdit.LineHeight -
        SynEdit.BookMarkOptions.BookmarkImages.Height) div 2);
      ScaleF := Min(SynEdit.LineHeight/SynEdit.BookMarkOptions.BookmarkImages.Height, 1);
      ImageListDraw(RT, SynEdit.BookMarkOptions.BookmarkImages,
        ClipR.Left + SynEdit.BookMarkOptions.LeftMargin + aGutterOff,
        (aMarkRow - SynEdit.TopLine) * SynEdit.LineHeight + VOffset,
        aMark.ImageIndex, ScaleF);
      Inc(aGutterOff, SynEdit.BookMarkOptions.Xoffset);
    end
    else if aMark.InternalImage and (aMark.ImageIndex in [0 .. 9]) then
    begin
      if aGutterOff = 0 then
      begin
        Gutter.InternalImage.Draw(RT, aMark.ImageIndex,
          ClipR.Left + SynEdit.BookMarkOptions.LeftMargin + aGutterOff,
          (aMarkRow - SynEdit.TopLine) * SynEdit.LineHeight,
          SynEdit.LineHeight);
      end;
      Inc(aGutterOff, SynEdit.BookMarkOptions.Xoffset);
    end;
  end;

var
  vFirstLine: Integer;
  vLastLine: Integer;
  cMark: Integer;
  vMarkRow: Integer;
  aGutterOffs: TArray<Integer>;
  bHasOtherMarks: Boolean;
  Index: Integer;
begin
  SynEdit := TCustomSynEdit(Editor);
  Assert(Assigned(SynEdit));

  vFirstLine := SynEdit.RowToLine(FirstRow);
  vLastLine := SynEdit.RowToLine(LastRow);

  if SynEdit.BookMarkOptions.GlyphsVisible and (SynEdit.Marks.Count > 0) and
    (vLastLine >= vFirstLine) then
  begin
    SetLength(aGutterOffs, LastRow - FirstRow + 1);
    // Instead of making a two pass loop we look while drawing the bookmarks
    // whether there is any other mark to be drawn
    bHasOtherMarks := False;
    for cMark := 0 to SynEdit.Marks.Count - 1 do
      with SynEdit.Marks[cMark] do
        if Visible and (Line >= vFirstLine) and (Line <= vLastLine) and
          (Line <= SynEdit.Lines.Count) and
          not (SynEdit.UseCodeFolding and SynEdit.AllFoldRanges.FoldHidesLine
          (Line, Index)) then
        begin
          if IsBookmark <> SynEdit.BookMarkOptions.DrawBookmarksFirst then
            bHasOtherMarks := True
          else
          begin
            vMarkRow := SynEdit.LineToRow(Line);
            if vMarkRow >= FirstRow then
              DrawMark(SynEdit.Marks[cMark], aGutterOffs[vMarkRow - FirstRow],
                vMarkRow);
          end
        end;
    if bHasOtherMarks then
      for cMark := 0 to SynEdit.Marks.Count - 1 do
        with SynEdit.Marks[cMark] do
        begin
          if Visible and
            (IsBookmark <> SynEdit.BookMarkOptions.DrawBookmarksFirst) and
            (Line >= vFirstLine) and (Line <= vLastLine) and
            (Line <= SynEdit.Lines.Count) and
            not (SynEdit.UseCodeFolding and SynEdit.AllFoldRanges.FoldHidesLine
            (Line, Index)) then
          begin
            vMarkRow := SynEdit.LineToRow(Line);
            if vMarkRow >= FirstRow then
              DrawMark(SynEdit.Marks[cMark], aGutterOffs[vMarkRow - FirstRow],
                vMarkRow);
          end;
        end;
  end
end;

procedure TSynGutterBand.PaintTrackChanges(RT: ID2D1RenderTarget; ClipR: TRect;
  const FirstRow, LastRow: Integer);
var
  SynEdit: TCustomSynEdit;
  Row, Line: Integer;
  LineTop: Integer;
  LineRect: TRect;
  PPI: Integer;
  Color: TColor;
  Flags: TSynLineChangeFlags;
begin
  SynEdit := TCustomSynEdit(Editor);
  Assert(Assigned(Gutter));
  Assert(Assigned(SynEdit));
  PPI := Gutter.FCurrentPPI;

  for Row := FirstRow to LastRow do
  begin
    Line := SynEdit.RowToLine(Row);
    if (Line < 1) or (Line > SynEdit.Lines.Count) then Continue;

    LineTop := (Row - SynEdit.TopLine) * SynEdit.LineHeight;
    Flags := TSynEditStringList(SynEdit.Lines).ChangeFlags[Line - 1];
    Color := clNone;
    if Flags = [sfModified] then
      Color := Gutter.TrackChanges.ModifiedColor
    else if Flags = [sfSaved, sfAsSaved] then
      Color := Gutter.TrackChanges.SavedColor
    else if Flags = [sfSaved] then
      Color := Gutter.TrackChanges.OriginalColor
    else if Flags = [sfSaved, sfModified] then
      Color := Gutter.TrackChanges.SavedModifiedColor;

    if Color <> clNone then
    begin
      LineRect := Rect(ClipR.Left + MulDiv(MarginX, PPI, 96), LineTop,
        ClipR.Right, LineTop + SynEdit.LineHeight);
      RT.FillRectangle(LineRect, TSynDWrite.SolidBrush(Color));
    end;
  end;
end;

function TSynGutterBand.RealWidth: Integer;
var
  PPI: Integer;
begin
  Assert(Assigned(Editor));
  Assert(Assigned(Gutter));
  PPI := Gutter.FCurrentPPI;
  if Visible then
    case FKind of
      // A margin of two pixels at the end
      gbkLineNumbers:
        Result := Gutter.FAutoSizeDigitCount *
          IfThen(Gutter.UseFontStyle, Gutter.FCharWidth, TCustomSynEdit(Editor).CharWidth) +
          MulDiv(MarginX, PPI, 96);
      gbkFold:
        Result := TCustomSynEdit(Editor).CodeFolding.ScaledGutterShapeSize(PPI)
          + MulDiv(MarginX, PPI, 96);
      gbkTrackChanges:
        Result := MulDiv(Gutter.TrackChanges.Width + MarginX, PPI, 96);
    else
      Result := MulDiv(FWidth, PPI, 96);
    end
  else
    Result := 0;
end;

procedure TSynGutterBand.SetBackground(const Value: TSynGutterBandBackground);
begin
  FBackground := Value;
  Changed(False);
end;

procedure TSynGutterBand.SetKind(Kind: TSynGutterBandKind);
begin
  FKind := Kind;
  Changed(False);
end;

procedure TSynGutterBand.SetOnPaintLines(const Value: TGutterBandPaintEvent);
begin
  FOnPaintLines := Value;
  Changed(False);
end;

procedure TSynGutterBand.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed(False);
end;

procedure TSynGutterBand.SetWidth(const Value: Integer);
begin
  if not(FKind in [gbkLineNumbers, gbkFold]) then
  begin
    FWidth := Value;
    Changed(False);
  end;
end;

{$ENDREGION}


{$REGION 'TSynBandsCollection'}

function TSynBandsCollection.GetBands(Index: Integer): TSynGutterBand;
begin
  Result := TSynGutterBand(Items[Index]);
end;

procedure TSynBandsCollection.Update(Item: TCollectionItem);
var
  Gutter: TSynGutter;
begin
  inherited;
  Gutter := TSynGutter(GetOwner);
  if Assigned(Gutter) then
    Gutter.Changed;
end;

{$ENDREGION}


{$REGION 'TTrackChanges'}

procedure TSynTrackChanges.Assign(Source: TPersistent);
begin
  if Source is TSynTrackChanges then
  begin
    var Src := TSynTrackChanges(Source);
    if Assigned(FOwner) then
      FOwner.BeginUpdate;
    try
      FVisible := Src.Visible;
      FWidth := Src.Width;
      FSavedColor := Src.SavedColor;
      FModifiedColor := Src.ModifiedColor;
      FSavedModifiedColor :=  Src.SavedModifiedColor;
      FOriginalColor := Src.OriginalColor;
    finally
      if Assigned(FOwner) then
        FOwner.EndUpdate;
    end;
  end
  else
    inherited;
end;

constructor TSynTrackChanges.Create(Gutter: TSynGutter);
begin
  inherited Create;
  FOwner := Gutter;
  FWidth := 4;
  FSavedColor := $33AA33;
  FModifiedColor := $0000DFFF;
  FSavedModifiedColor :=  clWebOrange;
  FOriginalColor := $00FF6633;
end;

function TSynTrackChanges.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSynTrackChanges.SetModifiedColor(const Value: TColor);
begin
  if FModifiedColor <> Value then
  begin
    FModifiedColor := Value;
    if FVisible and Assigned(FOwner) then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetOriginalColor(const Value: TColor);
begin
  if FOriginalColor <> Value then
  begin
    FOriginalColor := Value;
    if FVisible and Assigned(FOwner) then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetSavedColor(const Value: TColor);
begin
  if FSavedColor <> Value then
  begin
    FSavedColor := Value;
    if FVisible and Assigned(FOwner) then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetSavedModifiedColor(const Value: TColor);
begin
  if FSavedModifiedColor <> Value then
  begin
    FSavedModifiedColor := Value;
    if FVisible and Assigned(FOwner) then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOwner) then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    if FVisible and Assigned(FOwner) then
      FOwner.Changed;
  end;
end;

{$ENDREGION}


{$REGION 'TSynIndentGuides'}

{ TSynStructureColor }

procedure TSynStructureColor.Assign(Source: TPersistent);
begin
  if Source is TSynStructureColor then
    Self.FColor := TSynStructureColor(Source).Color
  else
    inherited;
end;

procedure TSynStructureColor.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

{ TSynStructureColors }

function TSynStructureColors.GetColors(Index: Integer): TSynStructureColor;
begin
  Result := TSynStructureColor(Items[Index]);
end;

{ TSynStructureColors }
procedure TSynIndentGuides.Assign(Source: TPersistent);
var
  Src: TSynIndentGuides;
begin
  if Source is TSynIndentGuides then
  begin
    Src := TSynIndentGuides(Source);
    FVisible := Src.FVisible;
    FStyle := Src.FStyle;
    FColor := Src.FColor;
    FUseStructureColors := Src.UseStructureColors;
    FStructureHighlight := Src.StructureHighlight;
    FStructureColors.Assign(Src.StructureColors);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TSynIndentGuides.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSynIndentGuides.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  Create;
end;

constructor TSynIndentGuides.Create;
begin
  inherited Create;
  FVisible := True;
  FStyle := igsSolid;
  FColor := clMedGray;
  FStructureHighlight := True;
  FUseStructureColors := True;
  FStructureColors := TSynStructureColors.Create(Self, TSynStructureColor);
  // Initialize structure colors
  FStructureColors.BeginUpdate;
  try
    with TSynStructureColor(FStructureColors.Add) do Color := $FF901E;  // clWebDodgerBlue;
    with TSynStructureColor(FStructureColors.Add) do Color := $008CFF;  // clWebDarkOrange;
    with TSynStructureColor(FStructureColors.Add) do Color := $20A5DA;  // clWebGoldenRod;
    with TSynStructureColor(FStructureColors.Add) do Color := $008080;  // clWebOlive
  finally
    FStructureColors.EndUpdate;
  end;
  // So that StructuredColors are not stored unless modified
  FStructuredColorsModified := False;
end;

destructor TSynIndentGuides.Destroy;
begin
  FStructureColors.Free;
  inherited;
end;

function TSynIndentGuides.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSynIndentGuides.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSynIndentGuides.SetStructureColors(const Value: TSynStructureColors);
begin
  FStructureColors.Assign(Value);
  Changed;
end;

procedure TSynIndentGuides.SetStructureHighlight(const Value: Boolean);
begin
  if FStructureHighlight <> Value then
  begin
    FStructureHighlight := Value;
    Changed;
  end;
end;

procedure TSynIndentGuides.SetStyle(const Value: TSynIdentGuidesStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TSynIndentGuides.SetUseStructureColors(const Value: Boolean);
begin
  if FUseStructureColors <> Value then
  begin
    FUseStructureColors := Value;
    Changed;
  end;
end;

procedure TSynIndentGuides.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{$ENDREGION}


{$REGION 'TSynIndicators'}

procedure TSynIndicators.Add(Line: Integer; const Indicator: TSynIndicator;
    Invalidate: Boolean = True);
var
  Arr: TArray<TSynIndicator>;
begin
  if FList.TryGetValue(Line, Arr) then
    FList[Line] := Arr + [Indicator]
  else
    FList.Add(Line, [Indicator]);
  if Invalidate then
    InvalidateIndicator(Line, Indicator);
end;

procedure TSynIndicators.Clear;
begin
  FList.Clear;
end;

procedure TSynIndicators.Clear(const Id: TGUID; Invalidate: Boolean = True;
    Line: Integer = -1);

  procedure ProcessLine(ALine: Integer);
  var
    Indicators: TArray<TSynIndicator>;
    I: Integer;
  begin
    if FList.TryGetValue(ALine, Indicators) then
    begin
      for I := Length(Indicators) - 1 downto 0 do
        if Indicators[I].Id = Id then
        begin
          if Invalidate then
            InvalidateIndicator(ALine, Indicators[I]);
          Delete(Indicators, I, 1);
        end;
      if Length(Indicators) = 0 then
        FList.Remove(ALine)
      else
        FList[ALine] := Indicators;
    end;
  end;

var
  ALine: Integer;
begin
  if Line < 0  then
    for ALine in FList.Keys.ToArray do
      ProcessLine(ALine)
  else
    ProcessLine(Line);
end;

procedure TSynIndicators.Clear(Line: Integer; const Indicator: TSynIndicator);
var
  Indicators: TArray<TSynIndicator>;
  I: Integer;
begin
  if FList.TryGetValue(Line, Indicators) then
  begin
    for I := 0 to Length(Indicators) - 1 do
      if Indicators[I] = Indicator then
      begin
        InvalidateIndicator(Line, Indicator);
        Delete(Indicators, I, 1);
        if Length(Indicators) = 0 then
          FList.Remove(Line)
        else
          FList[Line] := Indicators;
        Break;
      end;
  end;
end;

constructor TSynIndicators.Create(Owner: TCustomControl);
begin
  inherited Create;
  FOwner := Owner;
  FList := TDictionary<Integer, TArray<TSynIndicator>>.Create;
end;

destructor TSynIndicators.Destroy;
begin
  FRegister.Free;
  FList.Free;
  inherited;
end;

function TSynIndicators.GetById(
  const Id: TGUID): TArray<TPair<Integer, TSynIndicator>>;
begin
  Result := GetById([Id]);
end;

function TSynIndicators.GetById(
  const Ids: TArray<TGUID>): TArray<TPair<Integer, TSynIndicator>>;
var
  IndicatorList: TList<TPair<Integer, TSynIndicator>>;
  Line: Integer;
  Indicator: TSynIndicator;
  Id: TGUID;
begin
  IndicatorList := TList<TPair<Integer, TSynIndicator>>.Create;
  try
    for Line in FList.Keys do
      for Indicator in FList[Line] do
        for Id in Ids do
          if Id = Indicator.Id then
            IndicatorList.Add(TPair<Integer, TSynIndicator>.Create(Line, Indicator));
    Result := IndicatorList.ToArray;
  finally
    IndicatorList.Free;
  end;
end;

function TSynIndicators.GetSpec(const Id: TGUID): TSynIndicatorSpec;
begin
  Result := FRegister[Id];
end;

function TSynIndicators.IndicatorAtMousePos(MousePos: TPoint; const Id: TGUID;
    var Indicator: TSynIndicator): Boolean;
var
  DC: TDisplayCoord;
  BC: TBufferCoord;
  Editor: TCustomSynEdit;
begin
  Editor := FOwner as TCustomSynEdit;
  DC := Editor.PixelsToRowColumn(MousePos.X, MousePos.Y);
  BC := Editor.DisplayToBufferPos(DC);
  Result := IndicatorAtPos(BC, Id, Indicator);
end;

function TSynIndicators.IndicatorAtMousePos(MousePos: TPoint;
  var Indicator: TSynIndicator): Boolean;
begin
  Result := IndicatorAtMousePos(MousePos, TGUID.Empty, Indicator);
end;

function TSynIndicators.IndicatorAtPos(Pos: TBufferCoord;
  var Indicator: TSynIndicator): Boolean;
begin
  Result := IndicatorAtPos(Pos, TGUID.Empty, Indicator);
end;

function TSynIndicators.IndicatorAtPos(Pos: TBufferCoord;
  const Ids: TArray<TGUID>; var Indicator: TSynIndicator): Boolean;
var
  LineIndicators:  TArray<TSynIndicator>;
  LIndicator: TSynIndicator;
  Id: TGUID;
begin
  Result := False;
  if FList.TryGetValue(Pos.Line, LineIndicators) then
  begin
    for LIndicator in LineIndicators do
      for Id in Ids do
        if InRange(Pos.Char, LIndicator.CharStart, LIndicator.CharEnd - 1) and
         ((Id = TGUID.Empty) or (LIndicator.Id = Id)) then
        begin
          Indicator := LIndicator;
          Exit(True);
        end;
  end;
end;

function TSynIndicators.IndicatorAtPos(Pos: TBufferCoord; const Id: TGUID; var
    Indicator: TSynIndicator): Boolean;
begin
  Result := IndicatorAtPos(Pos, [Id], Indicator);
end;

procedure TSynIndicators.InvalidateIndicator(Line: Integer;  const Indicator: TSynIndicator);
begin
  TCustomSynEdit(FOwner).InvalidateRange(BufferCoord(Indicator.CharStart, Line),
    BufferCoord(Indicator.CharEnd, Line), Rect(0, 0, 1, 0));
end;

function TSynIndicators.LineIndicators(Line: Integer): TArray<TSynIndicator>;
begin
  // Sets Result to [] if not found
  FList.TryGetValue(Line, Result);
end;

procedure TSynIndicators.LinePut(aIndex: Integer; const OldLine: string);
{  aIndex 0-based Indicator lines 1-based}

  function AdjustIndicator(const Indicator:
    TSynIndicator; out AdjIndicator: TSynIndicator): Boolean;
  // Returns False if the indicator is removed
  var
    Line: string;
    StartPos, Len1, Len2: Integer;
  begin
    Result := False;
    AdjIndicator := Indicator;
    Line := TCustomSynEdit(FOwner).Lines[aIndex];
    LineDiff(Line, OldLine, StartPos, Len1, Len2);
    if StartPos > AdjIndicator.CharEnd then
      Result := True
    else if StartPos + Len1 < AdjIndicator.CharStart then
    begin
      Inc(AdjIndicator.CharStart, Len2 - Len1);
      Inc(AdjIndicator.CharEnd, Len2 - Len1);
      Result := True;
    end
    else if StartPos < AdjIndicator.CharStart then
    begin
      AdjIndicator.CharStart := StartPos;
      Inc(AdjIndicator.CharEnd, Len2 - Len1);
      Result := True;
    end
    else if StartPos + Len1 <= AdjIndicator.CharEnd then
    begin
      Inc(AdjIndicator.CharEnd, Len2 - Len1);
      Result := True;
    end
    else if StartPos <= AdjIndicator.CharEnd then
    begin
      AdjIndicator.CharEnd := StartPos;
      Result := True;
    end;

    if Result then
    begin
      AdjIndicator.CharStart := Max(1, AdjIndicator.CharStart);
      if AdjIndicator.CharEnd < AdjIndicator.CharStart then
        Result := False;
    end;
  end;

var
  Indicators, AdjIndicators: TArray<TSynIndicator>;
  Indicator, AdjIndicator: TSynIndicator;
begin
  if not FList.TryGetValue(aIndex + 1, Indicators) then
    Exit;

  AdjIndicators := [];
  for Indicator in Indicators do
    if Indicator.KeepOnLineChange and AdjustIndicator(Indicator, AdjIndicator) then
      AdjIndicators := AdjIndicators + [AdjIndicator];

  if Length(AdjIndicators) = 0 then
    FList.Remove(aIndex + 1)
  else
    FList[aIndex + 1] := AdjIndicators;
end;

procedure TSynIndicators.LinesDeleted(FirstLine, Count: Integer);
{ Adjust Indicator lines for deletion -
  FirstLine 0-based Indicator lines 1-based}
var
  Keys: TArray<Integer>;
  Line: Integer;
begin
  Keys := FList.Keys.ToArray;
  TArray.Sort<Integer>(Keys);
  for Line in Keys do
  begin
    if InRange(Line, FirstLine + 1, FirstLine + Count) then
      FList.Remove(Line)
    else if Line > FirstLine + Count then
    begin
      FList.Add(Line - Count, FList[Line]);
      FList.Remove(Line);
    end;
  end;
end;

procedure TSynIndicators.LinesInserted(FirstLine, Count: Integer);
{ Adjust Indicator lines for insertion -
  FirstLine 0-based. Indicator lines 1-based.}
var
  Keys: TArray<Integer>;
  I, Line: Integer;
begin
  Keys := FList.Keys.ToArray;
  TArray.Sort<Integer>(Keys);
  for I := Length(Keys) - 1 downto 0 do
  begin
    Line := Keys[I];
    if Line > FirstLine then
    begin
      FList.Add(Line + Count, FList[Line]);
      FList.Remove(Line);
    end;
  end;
end;

class procedure TSynIndicators.Paint(RT: ID2D1RenderTarget;
  Spec: TSynIndicatorSpec; const ClipR: TRect; StartOffset: Integer);
var
  Geometry: ID2D1PathGeometry;
  Sink: ID2D1GeometrySink;
  Delta: Integer;
  P: TPoint;
  R: TRect;
begin
  R := ClipR;
  Dec(R.Left, StartOffset);
  RT.PushAxisAlignedClip(ClipR, D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
  case Spec.Style of
    sisTextDecoration:
      // Otherwise it is already hanlded
      if not SameValue(Spec.Background.a, 0) and
        not SameValue(Spec.Background.a, 1)
      then
        RT.FillRectangle(R, TSynDWrite.SolidBrush(Spec.Background));
    sisSquiggleMicrosoftWord,
    sisSquiggleWordPerfect:
      begin
        Dec(R.Right);
        CheckOSError(TSynDWrite.D2DFactory.CreatePathGeometry(Geometry));
        CheckOSError(Geometry.Open(Sink));
        Delta := Round(R.Height / 6);
        if Spec.Style = sisSquiggleMicrosoftWord then
        begin
          P := Point(R.Left, R.Bottom - Delta);
          Sink.BeginFigure(P, D2D1_FIGURE_BEGIN_HOLLOW);
          while P.X < R.Right do
          begin
            Inc(P.X, Abs(Delta));
            Inc(P.Y, Delta);
            Delta := -Delta;
            Sink.AddLine(P);
          end;
          Sink.EndFigure(D2D1_FIGURE_END_OPEN);
        end
        else
        begin
          P := Point(R.Left, R.Bottom);
          while P.X < R.Right do
          begin
            Sink.BeginFigure(P, D2D1_FIGURE_BEGIN_HOLLOW);
            P.Offset(Delta, -Delta);
            Sink.AddLine(P);
            Sink.EndFigure(D2D1_FIGURE_END_OPEN);
            P.Offset(Delta - 1, Delta)
          end;
        end;
        CheckOSError(Sink.Close);

        RT.DrawGeometry(Geometry, TSynDWrite.SolidBrush(Spec.Foreground));
      end;
    sisRectangle,
    sisFilledRectangle:
      begin
        R.Inflate(-1, -1);
        if Spec.Style = sisFilledRectangle then
          RT.FillRectangle(R, TSynDWrite.SolidBrush(Spec.Background));
        if TAlphaColorF(Spec.Foreground) <> TAlphaColorF(clNoneF) then
          RT.DrawRectangle(R, TSynDWrite.SolidBrush(Spec.Foreground));
      end;
    sisRoundedRectangle,
    sisRoundedFilledRectangle:
      begin
        R.Inflate(-1, -1);
        if Spec.Style = sisRoundedFilledRectangle then
         RT.FillRoundedRectangle(D2D1RoundedRect(R, R.Height div 4, R.Height div 4),
            TSynDWrite.SolidBrush(Spec.Background));
        RT.DrawRoundedRectangle(D2D1RoundedRect(R, R.Height div 4, R.Height div 4),
           TSynDWrite.SolidBrush(Spec.Foreground));
      end;
  end;
  RT.PopAxisAlignedClip;
end;

procedure TSynIndicators.RegisterSpec(Id: TGUID; Spec: TSynIndicatorSpec);
begin
  if FRegister = nil then
    FRegister := TDictionary<TGUID, TSynIndicatorSpec>.Create;
  FRegister.AddOrSetValue(Id, Spec);
end;

procedure TSynStructureColors.Update(Item: TCollectionItem);
var
  IndentGuides : TSynIndentGuides;
begin
  inherited;
  IndentGuides := TSynIndentGuides(GetOwner);
  if Assigned(IndentGuides) then
  begin
    IndentGuides.FStructuredColorsModified := True;
    IndentGuides.Changed;
  end;
end;

{ TSynIndicatorSpec }

constructor TSynIndicatorSpec.Create(AStyle: TSynIndicatorStyle; AForeground,
  ABackground: TD2D1ColorF; AFontStyle: TFontStyles);
begin
  Self.Style := AStyle;
  Self.Foreground := AForeground;
  Self.Background := ABackground;
  Self.FontStyle := AFontStyle;
end;

class function TSynIndicatorSpec.New(AStyle: TSynIndicatorStyle; AForeground,
  ABackground: TD2D1ColorF; AFontStyle: TFontStyles): TSynIndicatorSpec;
begin
  Result.Create(AStyle, AForeground, ABackground, AFontStyle);
end;

{ TSynIndicator }

constructor TSynIndicator.Create(aId: TGUID; aCharStart, aCharEnd: Integer;
    aTag: NativeInt = 0; aKeepOnLineChange: Boolean = False);
begin
  Self.Id := aId;
  Self.CharStart := aCharStart;
  Self.CharEnd := aCharEnd;
  Self.Tag := aTag;
  Self.KeepOnLineChange := aKeepOnLineChange;
end;

class operator TSynIndicator.Equal(const A, B: TSynIndicator): Boolean;
begin
  Result := (A.Id = B.Id) and (A.CharStart = B.CharStart)
    and (A.CharEnd = B.CharEnd);
end;

class function TSynIndicator.New(aId: TGUID; aCharStart, aCharEnd: Integer;
    aTag: NativeInt = 0; aKeepOnLineChange: Boolean = False): TSynIndicator;
begin
  Result.Create(aId, aCharStart, aCharEnd, aTag, aKeepOnLineChange);
end;

{$ENDREGION}

{$REGION 'TSynBracketsHighlight'}

constructor TSynBracketsHighlight.Create(Owner: TPersistent);
begin
  inherited Create;
  FOwner := Owner;
  // Initialize with a blueish color
  SetFontColorsAndStyle($FF8800, clRed, [fsBold]);
end;

procedure TSynBracketsHighlight.SetFontColorsAndStyle(
  const MatchingBracketsColor, UnbalancedBracketColor: TColor;
  FontStyle: TFontStyles);
begin
  SetIndicatorSpecs(
    TSynIndicatorSpec.New(sisTextDecoration, D2D1ColorF(MatchingBracketsColor), clNoneF, FontStyle),
    TSynIndicatorSpec.New(sisTextDecoration, D2D1ColorF(UnbalancedBracketColor), clNoneF, FontStyle));
end;

procedure TSynBracketsHighlight.SetIndicatorSpecs(const MatchingBracketsSpec,
  UnbalancedBracketSpec: TSynIndicatorSpec);
begin
  if FOwner is TCustomSynEdit then
  begin
    TCustomSynEdit(FOwner).Indicators.RegisterSpec(MatchingBracketsIndicatorID, MatchingBracketsSpec);
    TCustomSynEdit(FOwner).Indicators.RegisterSpec(UnbalancedBracketIndicatorID, UnbalancedBracketSpec);
  end;
end;

{$ENDREGION}

{$REGION 'TSynSelections'}

constructor TSynSelections.Create(Owner: TPersistent);
begin
  inherited Create(Owner);
end;

procedure TSynSelections.CaretsChanged;
begin
  TCustomSynEdit(FOwner).StateFlags :=
    TCustomSynEdit(FOwner).StateFlags + [sfCaretChanged, sfScrollbarChanged];
end;

procedure TSynSelections.DoInvalidateSelection(const Sel: TSynSelection);
begin
  TCustomSynEdit(FOwner).InvalidateSelection(Sel);
end;

procedure TSynSelections.DoRestoreSelection(const Sel: TSynSelection;
  EnsureVisible: Boolean);
var
  TrimTrailingActive: Boolean;
begin
  TrimTrailingActive := eoTrimTrailingSpaces in TCustomSynEdit(FOwner).Options;
  if TrimTrailingActive then
    TCustomSynEdit(FOwner).Options := TCustomSynEdit(FOwner).Options -
      [eoTrimTrailingSpaces];
  TCustomSynEdit(FOwner).SetCaretAndSelection(Sel, EnsureVisible);
  if TrimTrailingActive then
    TCustomSynEdit(FOwner).Options := TCustomSynEdit(FOwner).Options +
      [eoTrimTrailingSpaces];
end;

function TSynSelections.GetLineText(ALine: Integer): string;
begin
  Result := TCustomSynEdit(FOwner).Lines[ALine - 1];
end;

function TSynSelections.GetWordWrap: Boolean;
begin
  Result := TCustomSynEdit(FOwner).WordWrap;
end;

function TSynSelections.GetScrollPastEOL: Boolean;
begin
  Result := eoScrollPastEol in TCustomSynEdit(FOwner).ScrollOptions;
end;

function TSynSelections.GetRowLength(ARow: Integer): Integer;
begin
  Result := TCustomSynEdit(FOwner).RowLength[ARow];
end;

function TSynSelections.BufferToDisplayPos(const P: TBufferCoord): TDisplayCoord;
begin
  Result := TCustomSynEdit(FOwner).BufferToDisplayPos(P);
end;

function TSynSelections.DisplayToBufferPos(const P: TDisplayCoord): TBufferCoord;
begin
  Result := TCustomSynEdit(FOwner).DisplayToBufferPos(P);
end;

function TSynSelections.SelectionToDisplayRow(var Sel: TSynSelection): Integer;
begin
  Result := TCustomSynEdit(FOwner).SelectionToDisplayCoord(Sel).Row;
end;

{$ENDREGION 'TSynSelections'}

{$REGION 'TSynCarets'}

procedure TSynCarets.Blink(Sender: TObject);
begin
  InvertCarets;
end;

constructor TSynCarets.Create(Canvas: TCanvas);
begin
  inherited Create;
  FCanvas := Canvas;
  FBlinkTimer := TTimer.Create(nil);
  FBlinkTimer.Interval := GetCaretBlinkTime;
  FBlinkTimer.OnTimer := Blink;
  FBlinkTimer.Enabled := False;
  CaretRects := TList<TRect>.Create;
  CaretSize := 2;
end;

destructor TSynCarets.Destroy;
begin
  FBlinkTimer.Free;
  CaretRects.Free;
  inherited;
end;

procedure TSynCarets.HideCarets;
begin
  FBlinkTimer.Enabled := False;

  if FCaretsShown then
    InvertCarets;
end;

procedure TSynCarets.InvertCarets;
var
  R: TRect;
begin
  for R in CaretRects do
    InvertRect(FCanvas.Handle, R);

  FCaretsShown := not FCaretsShown;
end;

procedure TSynCarets.ShowCarets;
begin
  Assert(not FCaretsShown);
  if CaretRects.Count > 0 then
  begin
    InvertCarets; // show immediately
    FBlinkTimer.Enabled := True;
  end;
  FCaretsShown := True;
end;

{$ENDREGION 'TSynCarets'}

{$REGION 'Scrollbar Annotations'}

{ TSynScrollbarAnnItem }

procedure TSynScrollbarAnnItem.Assign(Source: TPersistent);
var
  Src: TSynScrollbarAnnItem;
begin
  if Source is TSynScrollbarAnnItem then
  begin
    Src := TSynScrollbarAnnItem(Source);
    FAnnType := Src.AnnType;
    FAnnPos := Src.AnnPos;
    FOnGetInfo := Src.OnGetInfo;
    FSelectionColor := Src.SelectionColor;
    FBookmarkColor := Src.BookmarkColor;
    FFullRow := Src.FullRow;
  end
  else
    inherited;
end;

constructor TSynScrollbarAnnItem.Create(Collection: TCollection);
begin
  inherited;
  FSelectionColor := clDefault;
  FBookmarkColor := clDefault;
end;

procedure TSynScrollbarAnnItem.GetInfo(out Rows: TArray<Integer>;
  out Colors: TArray<TColor>);
var
  Editor: TCustomSynEdit;
  I, Line, Row: Integer;
  Caret: TBufferCoord;
  RowList: TList<Integer>;
  ColorList: TList<TColor>;
  Color: TColor;
  Mark: TSynEditMark;
  Flags: TSynLineChangeFlags;
  RowCount: Integer;
begin
  Editor := TCustomSynEdit(Collection.Owner);

  if Assigned(FOnGetInfo) then
     FOnGetInfo(Editor, FAnnType, Rows, Colors)
  else
  begin
    RowList := TList<Integer>.Create;
    try
      case FAnnType of
        sbaCarets:
          begin
            for I := 0 to Editor.Selections.Count - 1 do
            begin
              Caret := Editor.Selections[I].Caret;
              Row := Editor.BufferToDisplayPos(Caret).Row;
              if (I > 0) and (Row = RowList.Last) then Continue;
              RowList.Add(Row);
            end;
            if FSelectionColor <> clDefault then
              Color := FSelectionColor
            else
              Color := StyleServices.GetSystemColor(clHighlight);
            Colors := [Color];
          end;
        sbaBookmark:
          begin
            for Mark in Editor.Marks do
              if Mark.IsBookmark then
                RowList.Add(Editor.LineToRow(Mark.Line));
            if FBookmarkColor <> clDefault then
              Color := FBookmarkColor
            else
              Color := $AAB220;
            Colors := [Color];
          end;
        sbaTrackChanges:
          begin
            RowCount := Editor.DisplayRowCount;
            RowList.Capacity := RowCount;
            ColorList := TList<TColor>.Create;
            ColorList.Capacity := RowCount;
            try
              for Row := 1 to RowCount do
              begin
                Line := Editor.RowToLine(Row);
                Color := clNone;
                Flags := TSynEditStringList(Editor.Lines).ChangeFlags[Line - 1];
                if Flags = [sfModified] then
                  Color := Editor.Gutter.TrackChanges.ModifiedColor
                else if Flags = [sfSaved, sfAsSaved] then
                  Color := Editor.Gutter.TrackChanges.SavedColor
                else if Flags = [sfSaved] then
                  Color := Editor.Gutter.TrackChanges.OriginalColor
                else if Flags = [sfSaved, sfModified] then
                  Color := Editor.Gutter.TrackChanges.SavedModifiedColor;

                if Color <> clNone then
                begin
                  RowList.Add(Row);
                  ColorList.Add(Color);
                end;
              end;
              Colors := ColorList.ToArray;
            finally
              ColorList.Free;
            end;
          end;
      end;
      Rows := RowList.ToArray;
    finally
      RowList.Free;
    end;
  end;
end;

{ TScrollbarAnnotations }

function TSynScrollbarAnnotations.GetAnnotations(
  Index: Integer): TSynScrollbarAnnItem;
begin
  Result := TSynScrollbarAnnItem(Items[Index]);
end;

{$ENDREGION 'Scrollbar Annotations'}

procedure TSynScrollbarAnnotations.SetDefaultAnnotations;
begin
  Clear;
  with Add as TSynScrollbarAnnItem do
  begin
    AnnPos := sbpFullWidth;
    AnnType := sbaCarets;
  end;
  with Add as TSynScrollbarAnnItem do
  begin
    AnnPos := sbpLeft;
    AnnType := sbaBookmark;
    FullRow := True;
  end;
  with Add as TSynScrollbarAnnItem do
  begin
    AnnPos := sbpRight;
    AnnType := sbaTrackChanges;
    FullRow := True;
  end;
end;

procedure TSynScrollbarAnnotations.Update(Item: TCollectionItem);
begin
  inherited;
  with TCustomSynEdit(Owner) do
    if HandleAllocated then
      SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

{$REGION 'TSynDisplayFlowControl'}

 { TSynDisplayFlowControl }

procedure TSynDisplayFlowControl.Assign(aSource: TPersistent);
begin
  if aSource is TSynDisplayFlowControl then
  begin
    FEnabled := TSynDisplayFlowControl(aSource).Enabled;
    FColor := TSynDisplayFlowControl(aSource).Color;
  end
  else
    inherited;
end;

constructor TSynDisplayFlowControl.Create;
begin
  inherited;
  FEnabled := True;
  FColor := $0045FF;  // clWebOrangeRed
end;

{$ENDREGION 'TSynDisplayFlowControl'}


{$REGION 'TSynEditorOptionsContainer'}

procedure TSynEditorOptionsContainer.Assign(Source: TPersistent);
var
  PPI: Integer;
begin
  if Source is TCustomSynEdit then
  begin
    Self.Font.Assign(TCustomSynEdit(Source).Font);
    Self.BookmarkOptions.Assign(TCustomSynEdit(Source).BookmarkOptions);
    Self.Gutter.Assign(TCustomSynEdit(Source).Gutter);
    Self.Keystrokes.Assign(TCustomSynEdit(Source).Keystrokes);
    Self.SelectedColor.Assign(TCustomSynEdit(Source).SelectedColor);
    Self.IndentGuides.Assign(TCustomSynEdit(Source).IndentGuides);
    Self.DisplayFlowControl.Assign(TCustomSynEdit(Source).DisplayFlowControl);

    Self.Color := TCustomSynEdit(Source).Color;
    Self.Options := TCustomSynEdit(Source).Options;
    Self.ScrollOptions := TCustomSynEdit(Source).ScrollOptions;
    Self.ExtraLineSpacing := TCustomSynEdit(Source).ExtraLineSpacing;
    Self.HideSelection := TCustomSynEdit(Source).HideSelection;
    Self.InsertCaret := TCustomSynEdit(Source).InsertCaret;
    Self.OverwriteCaret := TCustomSynEdit(Source).OverwriteCaret;
    Self.MaxUndo := TCustomSynEdit(Source).MaxUndo;
    Self.RightEdge := TCustomSynEdit(Source).RightEdge;
    Self.RightEdgeColor := TCustomSynEdit(Source).RightEdgeColor;
    Self.TabWidth := TCustomSynEdit(Source).TabWidth;
    Self.WantTabs := TCustomSynEdit(Source).WantTabs;
    Self.WordWrap := TCustomSynEdit(Source).WordWrap;
    Self.ActiveLineColor := TCustomSynEdit(Source).ActiveLineColor;
    Self.VisibleSpecialChars := TCustomSynEdit(Source).VisibleSpecialChars;
    // store unscaled
    PPI := TCustomSynEdit(Source).CurrentPPI;
    Self.BookmarkOptions.ChangeScale(96, PPI);
    Self.ExtraLineSpacing := MulDiv(Self.ExtraLineSpacing, 96, PPI);
  end else if Source is TSynEditorOptionsContainer then
  begin
    Self.Font.Assign(TSynEditorOptionsContainer(Source).Font);
    Self.BookmarkOptions.Assign(TSynEditorOptionsContainer(Source).BookmarkOptions);
    Self.Gutter.Assign(TSynEditorOptionsContainer(Source).Gutter);
    Self.Keystrokes.Assign(TSynEditorOptionsContainer(Source).Keystrokes);
    Self.SelectedColor.Assign(TSynEditorOptionsContainer(Source).SelectedColor);
    Self.IndentGuides.Assign(TSynEditorOptionsContainer(Source).IndentGuides);
    Self.DisplayFlowControl.Assign(TSynEditorOptionsContainer(Source).DisplayFlowControl);
    Self.Color := TSynEditorOptionsContainer(Source).Color;
    Self.Options := TSynEditorOptionsContainer(Source).Options;
    Self.ScrollOptions := TSynEditorOptionsContainer(Source).ScrollOptions;
    Self.ExtraLineSpacing := TSynEditorOptionsContainer(Source).ExtraLineSpacing;
    Self.HideSelection := TSynEditorOptionsContainer(Source).HideSelection;
    Self.InsertCaret := TSynEditorOptionsContainer(Source).InsertCaret;
    Self.OverwriteCaret := TSynEditorOptionsContainer(Source).OverwriteCaret;
    Self.MaxUndo := TSynEditorOptionsContainer(Source).MaxUndo;
    Self.RightEdge := TSynEditorOptionsContainer(Source).RightEdge;
    Self.RightEdgeColor := TSynEditorOptionsContainer(Source).RightEdgeColor;
    Self.TabWidth := TSynEditorOptionsContainer(Source).TabWidth;
    Self.WantTabs := TSynEditorOptionsContainer(Source).WantTabs;
    Self.WordWrap := TSynEditorOptionsContainer(Source).WordWrap;
    Self.ActiveLineColor := TSynEditorOptionsContainer(Source).ActiveLineColor;
    Self.VisibleSpecialChars := TSynEditorOptionsContainer(Source).VisibleSpecialChars;
  end else
    inherited;
end;

procedure TSynEditorOptionsContainer.AssignTo(Dest: TPersistent);
var
  PPI: Integer;
begin
  if Dest is TCustomSynEdit then
  begin
    TCustomSynEdit(Dest).BeginUpdate;
    try
      TCustomSynEdit(Dest).Font := Self.Font;
      TCustomSynEdit(Dest).BookmarkOptions.Assign(Self.BookmarkOptions);
      TCustomSynEdit(Dest).Gutter.Assign(Self.Gutter);
      TCustomSynEdit(Dest).Keystrokes.Assign(Self.Keystrokes);
      TCustomSynEdit(Dest).SelectedColor.Assign(Self.SelectedColor);
      TCustomSynEdit(Dest).IndentGuides.Assign(Self.IndentGuides);
      TCustomSynEdit(Dest).DisplayFlowControl.Assign(Self.DisplayFlowControl);
      TCustomSynEdit(Dest).Color := Self.Color;
      TCustomSynEdit(Dest).Options := Self.Options;
      TCustomSynEdit(Dest).ScrollOptions := Self.ScrollOptions;
      TCustomSynEdit(Dest).ExtraLineSpacing := Self.ExtraLineSpacing;
      TCustomSynEdit(Dest).HideSelection := Self.HideSelection;
      TCustomSynEdit(Dest).InsertCaret := Self.InsertCaret;
      TCustomSynEdit(Dest).OverwriteCaret := Self.OverwriteCaret;
      TCustomSynEdit(Dest).MaxUndo := Self.MaxUndo;
      TCustomSynEdit(Dest).RightEdge := Self.RightEdge;
      TCustomSynEdit(Dest).RightEdgeColor := Self.RightEdgeColor;
      TCustomSynEdit(Dest).TabWidth := Self.TabWidth;
      TCustomSynEdit(Dest).WantTabs := Self.WantTabs;
      TCustomSynEdit(Dest).WordWrap := Self.WordWrap;
      TCustomSynEdit(Dest).ActiveLineColor := Self.ActiveLineColor;
      TCustomSynEdit(Dest).VisibleSpecialChars := Self.VisibleSpecialChars;
      // scale for editor PPI
      PPI := TCustomSynEdit(Dest).CurrentPPI;
      TCustomSynEdit(Dest).BookmarkOptions.ChangeScale(PPI, 96);
      TCustomSynEdit(Dest).ExtraLineSpacing :=
        MulDiv(TCustomSynEdit(Dest).ExtraLineSpacing, PPI, 96);
    finally
      TCustomSynEdit(Dest).EndUpdate;
    end;
  end else
    inherited;
end;

constructor TSynEditorOptionsContainer.Create(AOwner: TComponent);
begin
  inherited;
  FBookmarks := TSynBookMarkOpt.Create(Self);
  FKeystrokes := TSynEditKeyStrokes.Create(Self);
  FKeystrokes.ResetDefaults;
  FSynGutter := TSynGutter.Create;
  FSynGutter.AssignableBands := False;
  FSelectedColor := TSynSelectedColor.Create;
  FIndentGuides := TSynIndentGuides.Create;
    FActiveLineColor := clNone;
  FDisplayFlowControl := TSynDisplayFlowControl.Create;
  FFont := TFont.Create;
  FFont.Name := DefaultFontName;
  FFont.Size := 10;
  {$IF CompilerVersion >= 36}
  FFont.IsScreenFont := True;
  {$ENDIF}
  FColor := clWindow;
  FOptions := SYNEDIT_DEFAULT_OPTIONS;
  FScrollOptions := SYNEDIT_DEFAULT_SCROLLOPTIONS;
  FExtraLineSpacing := 2;
  FHideSelection := False;
  FInsertCaret := ctVerticalLine;
  FOverwriteCaret := ctBlock;
  FMaxUndo := 0;
  FRightEdge := 80;
  FRightEdgeColor := clSilver;
  FTabWidth := 8;
  FWantTabs := True;
end;

destructor TSynEditorOptionsContainer.Destroy;
begin
  FBookmarks.Free;
  FKeystrokes.Free;
  FSynGutter.Free;
  FSelectedColor.Free;
  FIndentGuides.Free;
  FDisplayFlowControl.Free;
  FFont.Free;
  inherited;
end;

procedure TSynEditorOptionsContainer.SetBookmarks(
  const Value: TSynBookMarkOpt);
begin
  FBookmarks.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetKeystrokes(
  const Value: TSynEditKeyStrokes);
begin
  FKeystrokes.Assign(Value);
end;

procedure TSynEditorOptionsContainer.SetSynGutter(const Value: TSynGutter);
begin
  FSynGutter.Assign(Value);
end;



{$ENDREGION 'TSynEditorOptionsContainer'}

{$REGION 'VCL Utility Functions'}

function DefaultFontName: string;
begin
  if CheckWin32Version(6) then
  begin
    Result := 'Consolas';
    if Screen.Fonts.IndexOf(Result) >= 0 then
      Exit;
  end;

  Result := 'Lucida Console';
  if Screen.Fonts.IndexOf(Result) >= 0 then
    Exit;

  Result := 'Courier New';
  if Screen.Fonts.IndexOf(Result) < 0 then
    Result := 'Courier';
end;

function WeightEnumFontsProc(EnumLogFontExDV: PEnumLogFontExDV;
  EnumTextMetric: PEnumTextMetric;
  FontType: DWORD; LParam: LPARAM): Integer; stdcall;
begin;
  PInteger(LPARAM)^ :=  EnumLogFontExDV.elfEnumLogfontEx.elfLogFont.lfWeight;
  Result := 0;
end;

function GetCorrectFontWeight(Font: TFont): Integer;
var
  DC: HDC;
  LogFont: TLogFont;
begin
  if TFontStyle.fsBold in Font.Style then
    Result := FW_BOLD
  else
  begin
    Result := FW_NORMAL;
    DC := GetDC(0);
    FillChar(LogFont, SizeOf(LogFont), 0);
    LogFont.lfCharSet := DEFAULT_CHARSET;
    StrPLCopy(LogFont.lfFaceName, Font.Name, Length(LogFont.lfFaceName) - 1);
    EnumFontFamiliesEx(DC, LogFont, @WeightEnumFontsProc, LPARAM(@Result), 0);
    ReleaseDC(0, DC);
  end;
end;

function IsColorDark(AColor: TColor): Boolean;
var
  ACol: Longint;
begin
  ACol := ColorToRGB(AColor) and $00FFFFFF;
  Result := ((2.99 * GetRValue(ACol) + 5.87 * GetGValue(ACol) +
                 1.14 * GetBValue(ACol)) < $400);
end;

function ColorToHTML(Color: TColor): string;
var
  R: TColorRef;
begin
  R := ColorToRGB(Color);
  Result := Format('#%.2x%.2x%.2x', [GetRValue(R), GetGValue(R), GetBValue(R)]);
end;

{$ENDREGION 'VCL Utility Functions'}

end.
