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
  Unicode translation by Maël Hörz.
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

  Known Issues:
  ------------------------------------------------------------------------------- }

unit SynEditMiscClasses;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Win.Registry,
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
  SynDWrite,
  SynEditTypes,
  SynEditKeyConst,
  SynUnicode;

type
  TSynSelectedColor = class(TPersistent)
  private
    FBG: TColor;
    FFG: TColor;
    FOnChange: TNotifyEvent;
    FAlpha: Single;
    FFillWholeLines: Boolean;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
    procedure SetAlpha(Value: Single);
    procedure SetFillWholeLines(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Background: TColor read FBG write SetBG default clHighLight;
    property Foreground: TColor read FFG write SetFG default clHighLightText;
    property Alpha: Single read FAlpha write SetAlpha;
    property FillWholeLines: Boolean read FFillWholeLines write SetFillWholeLines
      default True;
  end;

  TSynIdentGuidesStyle = (igsSolid, igsDotted);

  TSynIndentGuides = class(TPersistent)
  private
    FColor: TColor;
    FVisible: Boolean;
    FStyle: TSynIdentGuidesStyle;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
    procedure SetStyle(const Value: TSynIdentGuidesStyle);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Style: TSynIdentGuidesStyle read FStyle write SetStyle
      default igsSolid;
    property Color: TColor read FColor write SetColor
      default clMedGray;
  end;

  TSynGutterBorderStyle = (gbsNone, gbsMiddle, gbsRight);

  TGutterBandPaintEvent = procedure(Canvas: TCanvas; ClipR: TRect;
    const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean) of object;

  TGutterBandClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    X, Y, Row, Line: Integer) of object;

  TGutterMouseCursorEvent = procedure(Sender: TObject; X, Y, Row, Line: Integer;
    var Cursor: TCursor) of object;

  TSynGutter = class;

  { When created TGutter contains four bands (Marks, Line Numbers,
    Code Folding and Margin).  The order, width and other properties of the
    bands can be set at design time through the Bands property of TSynGutter.
    Custom bands can also be created.  They can be painted using
    OnPaintLines event handler.
    The width of the Line Numbers and Code Folding band is automatically
    calculated and not set at design time }
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
    procedure Assign(Source: TPersistent); Override;
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
    FOnMouseCursor: TGutterMouseCursorEvent;
    function GetSynGutter: TSynGutter;
    function GetEditor: TPersistent;
    procedure DoPaintLines(Canvas: TCanvas; ClipR: TRect; const FirstRow,
      LastRow: Integer);
    procedure PaintMarks(Canvas: TCanvas; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintLineNumbers(Canvas: TCanvas; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintFoldShapes(Canvas: TCanvas; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintMargin(Canvas: TCanvas; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure PaintTrackChanges(Canvas: TCanvas; ClipR: TRect;
      const FirstRow, LastRow: Integer);
    procedure SetBackground(const Value: TSynGutterBandBackground);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
    procedure SetKind(Kind: TSynGutterBandKind);
    procedure SetOnPaintLines(const Value: TGutterBandPaintEvent);
    function IsWidthStored: Boolean;
    function GetWidth: Integer;
    function GetVisible: Boolean;
    procedure SetOnClick(const Value: TGutterBandClickEvent);
    function GetLeftX: Integer;
    function FoldShapeRect(Row, Line: Integer): TRect;
    procedure SetOnMouseCursor(const Value: TGutterMouseCursorEvent);
    function IsVisibleStored: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function RealWidth: Integer;
    procedure PaintLines(Canvas: TCanvas; ClipR: TRect; const FirstRow, LastRow:
        Integer);
    procedure DoClick(Sender: TObject; Button: TMouseButton;
      X, Y, Row, Line: Integer);
    procedure DoMouseCursor(Sender: TObject; X, Y, Row, Line: Integer;
      var Cursor: TCursor);
    property LeftX: Integer read GetLeftX;
    property Editor: TPersistent read GetEditor;
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
    property OnCLick: TGutterBandClickEvent read FOnClick write SetOnClick;
    property OnMouseCursor: TGutterMouseCursorEvent read FOnMouseCursor
      write SetOnMouseCursor;
  end;

  TSynBandsCollection = class(TOwnedCollection)
  private
    function GetBands(Index: Integer): TSynGutterBand;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    property Bands[Index: Integer]: TSynGutterBand read GetBands; default;
  end;

  TSynInternalImage = class;

  TSynGutter = class(TPersistent)
  private
    [Weak]
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
    procedure OnFontChange(Sender: TObject);
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

  TSynBookMarkOpt = class(TPersistent)
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
    // ++ DPI-Aware
    procedure ChangeScale(M, D: Integer); virtual;
    // -- DPI-Aware
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

  TSynGlyph = class(TPersistent)
  private
    FVisible: Boolean;
    FInternalGlyph, FGlyph: TBitmap;
    FWICImage: TWICImage;
    FScaledBitmap: IWICBitmap;
    FScaledW, FScaledH: Cardinal;
    FOnChange: TNotifyEvent;
    procedure SetGlyph(Value: TBitmap);
    procedure Changed;
    procedure SetVisible(Value: Boolean);
    function GetSize: TSize;
    procedure CreateScaledBitmap;
    function GetWicBitmap: IWICBitmap;
  public
    constructor Create(aModule: THandle; const aName: string);
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
    procedure ChangeScale(M, D: Integer); virtual;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property WicBitmap: IWICBitmap read GetWicBitmap;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Size: TSize read GetSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TSynMethodChain }

  ESynMethodChain = class(Exception);
  TSynExceptionEvent = procedure(Sender: TObject; E: Exception;
    var DoContinue: Boolean) of object;

  TSynMethodChain = class(TObject)
  private
    FNotifyProcs: TList;
    FExceptionHandler: TSynExceptionEvent;
  protected
    procedure DoFire(const AEvent: TMethod); virtual; abstract;
    function DoHandleException(E: Exception): Boolean; virtual;
    property ExceptionHandler: TSynExceptionEvent read FExceptionHandler
      write FExceptionHandler;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AEvent: TMethod);
    procedure Remove(AEvent: TMethod);
    procedure Fire;
  end;

  { TSynNotifyEventChain }

  TSynNotifyEventChain = class(TSynMethodChain)
  private
    FSender: TObject;
  protected
    procedure DoFire(const AEvent: TMethod); override;
  public
    constructor CreateEx(ASender: TObject);
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    property ExceptionHandler;
    property Sender: TObject read FSender write FSender;
  end;

  { TSynInternalImage }

  TSynInternalImage = class(TObject)
  private
    FImages: TBitmap;
    FWidth: Integer;
    FHeight: Integer;
    FCount: Integer;
  public
    constructor Create(aModule: THandle; const Name: string; Count: Integer);
    destructor Destroy; override;
    procedure Draw(aCanvas: TCanvas; Number, X, Y, LineHeight: Integer);
    // ++ DPI-Aware
    procedure ChangeScale(M, D: Integer); virtual;
    // -- DPI-Aware
  end;

  { TSynHotKey }

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

  TSynEditSearchCustom = class(TComponent)
  protected
    function GetPattern: string; virtual; abstract;
    procedure SetPattern(const Value: string); virtual; abstract;
    function GetLength(Index: Integer): Integer; virtual; abstract;
    function GetResult(Index: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure SetOptions(const Value: TSynSearchOptions); virtual; abstract;
  public
    function FindAll(const NewText: string): Integer; virtual; abstract;
    function PreprocessReplaceExpression(const AReplace: string)
      : string; virtual;
    function Replace(const aOccurrence, aReplacement: string): string;
      virtual; abstract;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[Index: Integer]: Integer read GetResult;
    property Lengths[Index: Integer]: Integer read GetLength;
    property Options: TSynSearchOptions write SetOptions;
  end;

  TBetterRegistry = class(TRegistry)
    function OpenKeyReadOnly(const Key: string): Boolean;
  end;

  // ++ DPI-Aware
procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: Integer);
// -- DPI-Aware

implementation

uses
  System.Rtti,
  Vcl.GraphUtil,
  SynEditMiscProcs,
  SynEditCodeFolding,
  SynEdit,
  SynEditTextBuffer;

{$IF CompilerVersion <= 32}
procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: integer);
var
  buffer: TBitmap;
begin
  buffer := TBitmap.Create;
  try
    buffer.SetSize(NewWidth, NewHeight);
    buffer.AlphaFormat := afDefined;
    buffer.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
    Bitmap.SetSize(NewWidth, NewHeight);
    Bitmap.Canvas.Draw(0, 0, buffer);
  finally
    buffer.Free;
  end;
end;
{$ELSE}
// ++ DPI-Aware
procedure ResizeBitmap(Bitmap: TBitmap; const NewWidth, NewHeight: Integer);
var
  Factory: IWICImagingFactory;
  Scaler: IWICBitmapScaler;
  Source: TWICImage;
begin
  Bitmap.AlphaFormat := afDefined;
  Source := TWICImage.Create;
  try
    Source.Assign(Bitmap);
    Factory := TWICImage.ImagingFactory;
    Factory.CreateBitmapScaler(Scaler);
    try
      Scaler.Initialize(Source.Handle, NewWidth, NewHeight,
        WICBitmapInterpolationModeHighQualityCubic);
      Source.Handle := IWICBitmap(Scaler);
    finally
      Scaler := nil;
      Factory := nil;
    end;
    Bitmap.Assign(Source);
  finally
    Source.Free;
  end;
end;
// -- DPI-Aware
{$ENDIF}

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited Create;
  FBG := clHighLight;
  FFG := clHighLightText;
  FFillWholeLines := True;
  Alpha := 0.4;
end;

procedure TSynSelectedColor.Assign(Source: TPersistent);
var
  Src: TSynSelectedColor;
begin
  if (Source <> nil) and (Source is TSynSelectedColor) then
  begin
    Src := TSynSelectedColor(Source);
    FBG := Src.FBG;
    FFG := Src.FFG;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TSynSelectedColor.SetAlpha(Value: Single);
begin
  Value := EnsureRange(Value, 0, 1);
  if (FAlpha <> Value) then
  begin
    FAlpha := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if (FBG <> Value) then
  begin
    FBG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if (FFG <> Value) then
  begin
    FFG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFillWholeLines(const Value: Boolean);
begin
  if (FFillWholeLines <> Value) then
  begin
     FFillWholeLines := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{ TSynGutter }

procedure TSynGutter.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynGutter.ChangeScale(M, D: Integer);
begin
  FFont.Height := Round(FFont.Height * M / D);
  if Assigned(FInternalImage) then
    FInternalImage.ChangeScale(M, D);
  FCurrentPPI := M; // Vcl does the same
  Changed;
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
  FFont.Size := 8;
  FFont.Style := [];
  FUseFontStyle := True;
  FFont.OnChange := OnFontChange;
  OnFontChange(Self);

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
  if Assigned(Source) and (Source is TSynGutter) then
  begin
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
    Changed;
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
        break;
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

procedure TSynGutter.OnFontChange(Sender: TObject);
begin
  FFont.OnChange := nil;  // avoid recursion
  if Assigned(FOwner) then
    FFont.Quality := TCustomSynEdit(FOWner).FontQuality;
  // revert to default font if not monospaced or invalid
  if not IsFontMonospacedAndValid(FFont) then
    Font.Name := DefaultFontName;
  Font.OnChange := OnFontChange;
  FTextFormat.Create(FFont, 1, 0, 0);
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
    // ++ DPI-Aware
    if Assigned(FOwner) then
      FInternalImage.ChangeScale(FCurrentPPI, 96);
    // -- DPI-Aware
  end;
  Result := FInternalImage;
end;

function TSynGutter.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TSynBookMarkOpt }

// ++ DPI-Aware
procedure TSynBookMarkOpt.ChangeScale(M, D: Integer);
begin
  FLeftMargin := MulDiv(FLeftMargin, M, D);
  FXoffset := MulDiv(FXoffset, M, D);
end;
// -- DPI-Aware

constructor TSynBookMarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  FDrawBookmarksFirst := True;
  FEnableKeys := True;
  FGlyphsVisible := True;
  FLeftMargin := 2;
  FOwner := AOwner;
  FXoffset := 12;
end;

procedure TSynBookMarkOpt.Assign(Source: TPersistent);
var
  Src: TSynBookMarkOpt;
begin
  if (Source <> nil) and (Source is TSynBookMarkOpt) then
  begin
    Src := TSynBookMarkOpt(Source);
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

procedure TSynBookMarkOpt.SetBookmarkImages(const Value: TCustomImageList);
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

procedure TSynBookMarkOpt.SetDrawBookmarksFirst(Value: Boolean);
begin
  if Value <> FDrawBookmarksFirst then
  begin
    FDrawBookmarksFirst := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if FGlyphsVisible <> Value then
  begin
    FGlyphsVisible := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetLeftMargin(Value: Integer);
begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetXOffset(Value: Integer);
begin
  if FXoffset <> Value then
  begin
    FXoffset := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{ TSynGlyph }

procedure TSynGlyph.ChangeScale(M, D: Integer);
begin
  FScaledW := MulDiv(FScaledW, M, D);
  FSCaledH := MulDiv(FScaledH, M, D);
  CreateScaledBitmap;
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
  FWICImage := TWICImage.Create;
  FWICImage.Assign(FInternalGlyph);
  FWICImage.Handle.GetSize(FScaledW, FScaledH);
  FScaledBitmap := FWICImage.Handle;

  FVisible := True;
  FGlyph := TBitmap.Create;
end;

procedure TSynGlyph.CreateScaledBitmap;
var
  Factory: IWICImagingFactory;
  Scaler: IWICBitmapScaler;
begin
  Factory := FWICImage.ImagingFactory;
  Factory.CreateBitmapScaler(Scaler);
  Scaler.Initialize(FWICImage.Handle, FScaledW, FScaledH,
    WICBitmapInterpolationModeHighQualityCubic);
  FScaledBitmap := IWICBitmap(Scaler);
end;

destructor TSynGlyph.Destroy;
begin
  if Assigned(FInternalGlyph) then
    FreeAndNil(FInternalGlyph);

  FWICImage.Free;
  FGlyph.Free;
  inherited Destroy;
end;

function TSynGlyph.GetSize: TSize;
begin
  Result.Create(FScaledW, FScaledH);
end;

function TSynGlyph.GetWicBitmap: IWICBitmap;
begin
  Result := FScaledBitmap;
end;

procedure TSynGlyph.Assign(aSource: TPersistent);
var
  vSrc: TSynGlyph;
begin
  if Assigned(aSource) and (aSource is TSynGlyph) then
  begin
    vSrc := TSynGlyph(aSource);
    FInternalGlyph.Assign(vSrc.FInternalGlyph);
    FVisible := vSrc.FVisible;
    FGlyph.Assign(vSrc.FGlyph);
    if FGlyph.Empty then
      FWICImage.Assign(FInternalGlyph)
    else
    begin
      FGlyph.AlphaFormat := afDefined;
      FWICImage.Assign(FGlyph);
    end;
    FWICImage.Handle.GetSize(FScaledW, FScaledH);
    FScaledBitmap := FWICImage.Handle;
  end
  else
    inherited;
  Changed;
end;

procedure TSynGlyph.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  if FGlyph.Empty then
    FWICImage.Assign(FInternalGlyph)
  else
  begin
    FGlyph.AlphaFormat := afDefined;
    FWICImage.Assign(FGlyph);
  end;
  FWICImage.Handle.GetSize(FScaledW, FScaledH);
  FScaledBitmap := FWICImage.Handle;
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

{ TSynMethodChain }

procedure TSynMethodChain.Add(AEvent: TMethod);
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt
      ('%s.Entry : the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    Add(Code);
    Add(Data);
  end
end;

constructor TSynMethodChain.Create;
begin
  inherited;
  FNotifyProcs := TList.Create;
end;

destructor TSynMethodChain.Destroy;
begin
  FNotifyProcs.Free;
  inherited;
end;

function TSynMethodChain.DoHandleException(E: Exception): Boolean;
begin
  if not Assigned(FExceptionHandler) then
    raise E
  else
    try
      Result := True;
      FExceptionHandler(Self, E, Result);
    except
      raise ESynMethodChain.CreateFmt
        ('%s.DoHandleException : MUST NOT occur any kind of exception in ' +
        'ExceptionHandler', [ClassName]);
    end;
end;

procedure TSynMethodChain.Fire;
var
  AMethod: TMethod;
  I: Integer;
begin
  I := 0;
  with FNotifyProcs, AMethod do
    while I < Count do
      try
        repeat
          Code := Items[I];
          Inc(I);
          Data := Items[I];
          Inc(I);

          DoFire(AMethod)
        until I >= Count;
      except
        on E: Exception do
          if not DoHandleException(E) then
            I := MaxInt;
      end;
end;

procedure TSynMethodChain.Remove(AEvent: TMethod);
var
  I: Integer;
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt
      ('%s.Remove: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    I := Count - 1;
    while I > 0 do
      if Items[I] <> Data then
        Dec(I, 2)
      else
      begin
        Dec(I);
        if Items[I] = Code then
        begin
          Delete(I);
          Delete(I);
        end;
        Dec(I);
      end;
  end;
end;

{ TSynNotifyEventChain }

procedure TSynNotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  inherited Add(TMethod(AEvent));
end;

constructor TSynNotifyEventChain.CreateEx(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
end;

procedure TSynNotifyEventChain.DoFire(const AEvent: TMethod);
begin
  TNotifyEvent(AEvent)(FSender);
end;

procedure TSynNotifyEventChain.Remove(AEvent: TNotifyEvent);
begin
  inherited Remove(TMethod(AEvent));
end;

{ TSynInternalImage }

type
  TInternalResource = class(TObject)
  public
    UsageCount: Integer;
    Name: string;
    Bitmap: TBitmap;
  end;

procedure TSynInternalImage.ChangeScale(M, D: Integer);
begin
  if M = D then
    Exit;

  FWidth := MulDiv(FWidth, M, D);
  ResizeBitmap(FImages, FWidth * FCount, MulDiv(FImages.Height, M, D));
  FHeight := FImages.Height;
end;

constructor TSynInternalImage.Create(aModule: THandle; const Name: string;
  Count: Integer);
begin
  inherited Create;
  FImages := TBitmap.Create;
  FImages.LoadFromResourceName(aModule, Name);
  FWidth := (FImages.Width + Count shr 1) div Count;
  FHeight := FImages.Height;
  FCount := Count;
end;

destructor TSynInternalImage.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

procedure TSynInternalImage.Draw(aCanvas: TCanvas;
  Number, X, Y, LineHeight: Integer);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < FCount) then
  begin
    if LineHeight >= FHeight then
    begin
      rcSrc := Rect(Number * FWidth, 0, (Number + 1) * FWidth, FHeight);
      Inc(Y, (LineHeight - FHeight) div 2);
      rcDest := Rect(X, Y, X + FWidth, Y + FHeight);
    end
    else
    begin
      rcDest := Rect(X, Y, X + FWidth, Y + LineHeight);
      Y := (FHeight - LineHeight) div 2;
      rcSrc := Rect(Number * FWidth, Y, (Number + 1) * FWidth, Y + LineHeight);
    end;
    DrawTransparentBitmap(FImages, rcSrc, aCanvas, rcDest, 255);
  end;
end;

{ TSynHotKey }

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

{ TBetterRegistry }

function TBetterRegistry.OpenKeyReadOnly(const Key: string): Boolean;

  function IsRelative(const Value: string): Boolean;
  begin
    Result := not((Value <> '') and (Value[1] = '\'));
  end;

var
  TempKey: HKey;
  S: string;
  Relative: Boolean;
begin
  S := Key;
  Relative := IsRelative(S);

  if not Relative then
    Delete(S, 1, 1);
  TempKey := 0;
  Result := RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0, KEY_READ, TempKey)
    = ERROR_SUCCESS;
  if Result then
  begin
    if (CurrentKey <> 0) and Relative then
      S := CurrentPath + '\' + S;
    ChangeKey(TempKey, S);
  end;
end; { TBetterRegistry.OpenKeyReadOnly }

{ TSynEditSearchCustom }

// possibility to preprocess search expression before is send to SynEdit.SearchReplace()
function TSynEditSearchCustom.PreprocessReplaceExpression(const AReplace
  : string): string;
begin
  Result := AReplace;
end;

{ TSynGutterBand }

procedure TSynGutterBand.Assign(Source: TPersistent);
var
  Src: TSynGutterBand;
begin
  if Assigned(Source) and (Source is TSynGutterBand) then
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
  Index: Integer;
  rcFold: TRect;
begin
  if Visible and (FKind = gbkFold) then
  begin
    SynEdit := TCustomSynEdit(Editor);
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

procedure TSynGutterBand.DoPaintLines(Canvas: TCanvas; ClipR: TRect;
  const FirstRow, LastRow: Integer);
// Drawing of builtin bands
begin
  case FKind of
    gbkMarks:
      PaintMarks(Canvas, ClipR, FirstRow, LastRow);
    gbkLineNumbers:
      PaintLineNumbers(Canvas, ClipR, FirstRow, LastRow);
    gbkFold:
      PaintFoldShapes(Canvas, ClipR, FirstRow, LastRow);
    gbkTrackChanges:
      PaintTrackChanges(Canvas, ClipR, FirstRow, LastRow);
    gbkMargin:
      PaintMargin(Canvas, ClipR, FirstRow, LastRow);
  end;
end;

function TSynGutterBand.FoldShapeRect(Row, Line: Integer): TRect;
// Given that WordWrap and CodeFolding are mutally exclusive Row = Line
// But at some point this could be relaxed
Var
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

function TSynGutterBand.GetEditor: TPersistent;
begin
  if Assigned(Gutter) then
    Result := Gutter.GetOwner
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

procedure TSynGutterBand.PaintFoldShapes(Canvas: TCanvas; ClipR: TRect;
  const FirstRow, LastRow: Integer);
const
  PlusMinusMargin = 2;
var
  SynEdit: TCustomSynEdit;
  vLine: Integer;
  cRow: Integer;
  rcFold: TRect;
  X: Integer;
  FoldRange: TSynFoldRange;
  Index: Integer;
  Margin: Integer;
  PMMargin: Integer;
  ShapeSize: Integer;
  PPI: Integer;
begin
  SynEdit := TCustomSynEdit(Editor);
  Assert(Assigned(SynEdit));
  Assert(Assigned(Gutter));
  PPI := Gutter.FCurrentPPI;

  // Draw the folding lines and squares
  if SynEdit.UseCodeFolding then
  begin
    Margin := MulDiv(MarginX, PPI, 96);
    PMMargin := MulDiv(PlusMinusMargin, PPI, 96);

    ShapeSize := SynEdit.CodeFolding.ScaledGutterShapeSize(PPI);

    for cRow := FirstRow to LastRow do
    begin
      vLine := SynEdit.RowToLine(cRow);
      if (vLine > SynEdit.Lines.Count) { and not (SynEdit.Lines.Count = 0) }
      then
        break;

      rcFold.TopLeft := Point(ClipR.Left + Margin, (cRow - SynEdit.TopLine) *
        SynEdit.LineHeight + (SynEdit.LineHeight - ShapeSize) div 2);
      rcFold.BottomRight := rcFold.TopLeft;
      rcFold.BottomRight.Offset(ShapeSize, ShapeSize);

      Canvas.Pen.Color := SynEdit.CodeFolding.FolderBarLinesColor;

      // Any fold ranges beginning on this line?
      if SynEdit.AllFoldRanges.FoldStartAtLine(vLine, Index) then
      begin
        FoldRange := SynEdit.AllFoldRanges.Ranges[Index];
        Canvas.Brush.Color := SynEdit.CodeFolding.FolderBarLinesColor;
        Canvas.FrameRect(rcFold);

        // Paint minus sign
        Canvas.Pen.Color := SynEdit.CodeFolding.FolderBarLinesColor;
        Canvas.MoveTo(rcFold.Left + PMMargin, rcFold.Top + ShapeSize div 2);
        Canvas.LineTo(rcFold.Right - PMMargin, rcFold.Top + ShapeSize div 2);

        // Paint vertical line of plus sign
        if FoldRange.Collapsed then
        begin
          X := rcFold.Left + ShapeSize div 2;
          Canvas.MoveTo(X, rcFold.Top + PMMargin);
          Canvas.LineTo(X, rcFold.Bottom - PMMargin);
        end
        else
        // Draw the bottom part of a line
        begin
          X := rcFold.Left + ShapeSize div 2;
          Canvas.MoveTo(X, rcFold.Bottom);
          Canvas.LineTo(X, (cRow - SynEdit.TopLine + 1) * SynEdit.LineHeight);
        end;
      end
      else
      begin
        // Need to paint a line end?
        if SynEdit.AllFoldRanges.FoldEndAtLine(vLine, Index) then
        begin
          X := rcFold.Left + ShapeSize div 2;
          Canvas.MoveTo(X, (cRow - SynEdit.TopLine) * SynEdit.LineHeight);
          Canvas.LineTo(X, rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
          Canvas.LineTo(rcFold.Right,
            rcFold.Top + ((rcFold.Bottom - rcFold.Top) div 2));
        end;
        // Need to paint a line?
        if SynEdit.AllFoldRanges.FoldAroundLine(vLine, Index) then
        begin
          X := rcFold.Left + ShapeSize div 2;
          Canvas.MoveTo(X, (cRow - SynEdit.TopLine) * SynEdit.LineHeight);
          Canvas.LineTo(X, (cRow - SynEdit.TopLine + 1) * SynEdit.LineHeight);
        end;
      end;
    end;
  end;
end;

procedure TSynGutterBand.PaintLineNumbers(Canvas: TCanvas; ClipR: TRect;
  const FirstRow, LastRow: Integer);
var
  SynEdit: TCustomSynEdit;
  Row, Line: Integer;
  LineTop: Integer;
  LineRect: TRect;
  PPI: Integer;
  S: string;
  TextFormat: TSynTextFormat;
  RT: ISynWicRenderTarget;
  WordWrapGlyph: ID2D1Bitmap;
  RectF: TRectF;
  FontColor: TColor;
  GDIRT: ID2D1GdiInteropRenderTarget;
  SourceDC: HDC;
  BF: TBlendFunction;
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

  RT := SynWicRenderTarget(ClipR.Width, ClipR.Height);
  if SynEdit.WordWrap and SynEdit.WordWrapGlyph.Visible then
    RT.IDW.CreateBitmapFromWicBitmap(SynEdit.WordWrapGlyph.WicBitmap, nil,
     WordWrapGlyph);

  RT.IDW.BeginDraw;
  RT.IDW.Clear(D2D1ColorF(0, 0, 0, 0));
  for Row := FirstRow to LastRow do
  begin
    Line := SynEdit.RowToLine(Row);
    LineTop := (Row - SynEdit.TopLine) * SynEdit.LineHeight;
    LineRect := Rect(MulDiv(MarginX, PPI, 96), LineTop - ClipR.Top,
      ClipR.Width, LineTop - ClipR.Top + SynEdit.LineHeight);

    if SynEdit.WordWrap and SynEdit.WordWrapGlyph.Visible and
      (Row <> SynEdit.LineToRow(Line))
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
      RT.IDW.DrawBitmap(WordWrapGlyph, @RectF);
    end
    else
    begin
      // paint line numbers
      S := Gutter.FormatLineNumber(Line);
      if Assigned(SynEdit.OnGutterGetText) then
        SynEdit.OnGutterGetText(Self, Line, S);
      RT.IDW.DrawText(PChar(S), S.Length, TextFormat.IDW, LineRect,
        TSynDWrite.SolidBrush(FontColor),
        D2D1_DRAW_TEXT_OPTIONS_CLIP + D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT,
        DWRITE_MEASURING_MODE_GDI_NATURAL);
    end;
  end;

    GDIRT := RT.IDW as ID2D1GdiInteropRenderTarget;
    CheckOSError(GDIRT.GetDC(D2D1_DC_INITIALIZE_MODE_COPY, SourceDC));
    BF.BlendOp := AC_SRC_OVER;
    BF.BlendFlags := 0;
    BF.SourceConstantAlpha := 255;
    BF.AlphaFormat := AC_SRC_ALPHA;
    AlphaBlend(Canvas.Handle, ClipR.Left, ClipR.Top, ClipR.Width, ClipR.Height,
      SourceDC, 0, 0, ClipR.Width, ClipR.Height, BF);
    GDIRT.ReleaseDC(nil);

  RT.IDW.EndDraw;
//  Canvas.Draw(CLipR.Left, ClipR.Top, RT.WicImage);
  if not Gutter.UseFontStyle then
  begin
    TextFormat.IDW.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
    TextFormat.IDW.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
  end;
end;

procedure TSynGutterBand.PaintLines(Canvas: TCanvas; ClipR: TRect;
  const FirstRow, LastRow: Integer);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  if Assigned(FOnPaintLines) then
    FOnPaintLines(Canvas, ClipR, FirstRow, LastRow, DoDefault);
  if DoDefault then
    DoPaintLines(Canvas, ClipR, FirstRow, LastRow);
end;

procedure TSynGutterBand.PaintMargin(Canvas: TCanvas; ClipR: TRect;
  const FirstRow, LastRow: Integer);
Var
  Offset: Integer;
begin
  if (Gutter.BorderStyle <> gbsNone) then
    with Canvas do
    begin
      Pen.Color := Gutter.BorderColor;
      Pen.Width := 1;
      if Gutter.BorderStyle = gbsMiddle then
        Offset := Max(2, (ClipR.Right - ClipR.Left) div 2)
      else
        Offset := 1;
      MoveTo(ClipR.Right - Offset, ClipR.Top);
      LineTo(ClipR.Right - Offset, ClipR.Bottom);
    end;
end;

procedure TSynGutterBand.PaintMarks(Canvas: TCanvas; ClipR: TRect;
  const FirstRow, LastRow: Integer);
var
  SynEdit: TCustomSynEdit;

  procedure DrawMark(aMark: TSynEditMark; var aGutterOff: Integer;
    aMarkRow: Integer);
  begin
    if (not aMark.InternalImage) and
      Assigned(SynEdit.BookMarkOptions.BookmarkImages) then
    begin
      if aMark.ImageIndex <= SynEdit.BookMarkOptions.BookmarkImages.Count then
      begin
        if aMark.IsBookmark = SynEdit.BookMarkOptions.DrawBookmarksFirst then
          aGutterOff := 0
        else if aGutterOff = 0 then
          aGutterOff := SynEdit.BookMarkOptions.Xoffset;
        SynEdit.BookMarkOptions.BookmarkImages.Draw(Canvas,
          ClipR.Left + SynEdit.BookMarkOptions.LeftMargin + aGutterOff,
          (aMarkRow - SynEdit.TopLine) * SynEdit.LineHeight, aMark.ImageIndex);
        Inc(aGutterOff, SynEdit.BookMarkOptions.Xoffset);
      end;
    end
    else
    begin
      if aMark.ImageIndex in [0 .. 9] then
      begin
        if aGutterOff = 0 then
        begin
          Gutter.InternalImage.Draw(Canvas, aMark.ImageIndex,
            ClipR.Left + SynEdit.BookMarkOptions.LeftMargin + aGutterOff,
            (aMarkRow - SynEdit.TopLine) * SynEdit.LineHeight,
            SynEdit.LineHeight);
        end;
        Inc(aGutterOff, SynEdit.BookMarkOptions.Xoffset);
      end;
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
          not(SynEdit.UseCodeFolding and SynEdit.AllFoldRanges.FoldHidesLine
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
            not(SynEdit.UseCodeFolding and SynEdit.AllFoldRanges.FoldHidesLine
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

procedure TSynGutterBand.PaintTrackChanges(Canvas: TCanvas; ClipR: TRect;
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
      Canvas.Brush.Color := Color;
      Canvas.FillRect(LineRect);
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

procedure TSynGutterBand.SetOnClick(const Value: TGutterBandClickEvent);
begin
  FOnClick := Value;
  Changed(False);
end;

procedure TSynGutterBand.SetOnMouseCursor(const Value: TGutterMouseCursorEvent);
begin
  FOnMouseCursor := Value;
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

{ TSynBandsCollection }

function TSynBandsCollection.GetBands(Index: Integer): TSynGutterBand;
begin
  Result := TSynGutterBand(Items[Index]);
end;

procedure TSynBandsCollection.Update(Item: TCollectionItem);
var
  Gutter: TSynGutter;
begin
  Gutter := TSynGutter(GetOwner);
  if Assigned(Gutter) then
    Gutter.Changed;
end;

{ TTrackChanges }

procedure TSynTrackChanges.Assign(Source: TPersistent);
var
  Src: TSynTrackChanges;
begin
  if Assigned(Source) and (Source is TSynTrackChanges) then
  begin
    Src := TSynTrackChanges(Source);
    FWidth := Src.Width;
    FSavedColor := Src.SavedColor;
    FModifiedColor := Src.ModifiedColor;
    FSavedModifiedColor :=  Src.SavedModifiedColor;
    FOriginalColor := Src.OriginalColor;
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
    if FVisible then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetOriginalColor(const Value: TColor);
begin
  if FOriginalColor <> Value then
  begin
    FOriginalColor := Value;
    if FVisible then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetSavedColor(const Value: TColor);
begin
  if FSavedColor <> Value then
  begin
    FSavedColor := Value;
    if FVisible then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetSavedModifiedColor(const Value: TColor);
begin
  if FSavedModifiedColor <> Value then
  begin
    FSavedModifiedColor := Value;
    if FVisible then
      FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    FOwner.Changed;
  end;
end;

procedure TSynTrackChanges.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    if FVisible then
      FOwner.Changed;
  end;
end;

{ TSynIndentGuides }

procedure TSynIndentGuides.Assign(Source: TPersistent);
var
  Src: TSynIndentGuides;
begin
  if (Source <> nil) and (Source is TSynIndentGuides) then
  begin
    Src := TSynIndentGuides(Source);
    FVisible := Src.FVisible;
    FStyle := Src.FStyle;
    FColor := Src.FColor;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

constructor TSynIndentGuides.Create;
begin
  inherited Create;
  FVisible := True;
  FStyle := igsSolid;
  FColor := clMedGray;
end;

procedure TSynIndentGuides.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynIndentGuides.SetStyle(const Value: TSynIdentGuidesStyle);
begin
  FStyle := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynIndentGuides.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
