{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditMiscClasses;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  SynEditTypes,
  SynEditSelections,
  SynEditKeyCmds,
  SynEditKeyConst;

const
  { Cross-platform selection color defaults.
    TColors.SysHighlight/SysHighlightText are system colors (negative TColor
    values) that resolve via GetSysColor on Windows.  On non-Windows FMX
    targets TColorToAlphaColor returns TAlphaColors.Null for system colors.
    Use explicit TColor values (BGR format) that work on all platforms. }
  clDefaultSelectionBG = TColors.Dodgerblue;  // $FF901E in BGR
  clDefaultSelectionFG = TColors.White;       // $FFFFFF in BGR

type
  { Bookmark/mark for FMX SynEdit }
  TSynFMXEditMark = class
  private
    FLine: Integer;
    FChar: Integer;
    FBookmarkNum: Integer;
    FVisible: Boolean;
    function GetIsBookmark: Boolean;
  public
    constructor Create;
    property Line: Integer read FLine write FLine;
    property Char: Integer read FChar write FChar;
    property BookmarkNumber: Integer read FBookmarkNum write FBookmarkNum;
    property IsBookmark: Boolean read GetIsBookmark;
    property Visible: Boolean read FVisible write FVisible;
  end;

  { List of marks/bookmarks }
  TSynFMXEditMarkList = class(TObjectList<TSynFMXEditMark>)
  public
    function GetMarksForLine(ALine: Integer): TArray<TSynFMXEditMark>;
    procedure ClearLine(ALine: Integer);
  end;

  { Forward declarations }
  TSynFMXGutter = class;
  TSynFMXGutterBands = class;

  { Gutter band kind }
  TSynFMXGutterBandKind = (gbkCustom, gbkMarks, gbkLineNumbers, gbkFold, gbkMargin);

  { Gutter band background }
  TSynFMXGutterBandBackground = (gbbNone, gbbGutter, gbbEditor);

  { Gutter band paint event }
  TFMXGutterBandPaintEvent = procedure(Canvas: TCanvas; ClipR: TRectF;
    const FirstRow, LastRow: Integer; var DoDefaultPaint: Boolean) of object;

  { Gutter band click event }
  TFMXGutterBandClickEvent = procedure(Sender: TObject; Button: TMouseButton;
    X, Y: Single; Row, Line: Integer) of object;

  { Individual gutter band }
  TSynFMXGutterBand = class
  private
    FKind: TSynFMXGutterBandKind;
    FWidth: Single;
    FVisible: Boolean;
    FBackground: TSynFMXGutterBandBackground;
    FOnPaintLines: TFMXGutterBandPaintEvent;
    FOnClick: TFMXGutterBandClickEvent;
    FGutter: TSynFMXGutter;
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Single);
    // Built-in paint methods (implementation uses FMX.SynEdit)
    procedure PaintLineNumbers(Canvas: TCanvas; const ClipR: TRectF;
      FirstRow, LastRow: Integer);
    procedure PaintMarks(Canvas: TCanvas; const ClipR: TRectF;
      FirstRow, LastRow: Integer);
    procedure PaintFoldShapes(Canvas: TCanvas; const ClipR: TRectF;
      FirstRow, LastRow: Integer);
    procedure PaintMarginLine(Canvas: TCanvas; const ClipR: TRectF;
      FirstRow, LastRow: Integer);
  public
    constructor Create(AGutter: TSynFMXGutter; AKind: TSynFMXGutterBandKind;
      AWidth: Single; AVisible: Boolean);
    function RealWidth: Single;
    function LeftX: Single;
    procedure PaintLines(Canvas: TCanvas; const ClipR: TRectF;
      FirstRow, LastRow: Integer);
    procedure DoClick(Button: TMouseButton; X, Y: Single; Row, Line: Integer);
    property Kind: TSynFMXGutterBandKind read FKind;
    property Width: Single read FWidth write SetWidth;
    property Visible: Boolean read FVisible write SetVisible;
    property Background: TSynFMXGutterBandBackground read FBackground
      write FBackground;
    property Gutter: TSynFMXGutter read FGutter;
    property OnPaintLines: TFMXGutterBandPaintEvent read FOnPaintLines
      write FOnPaintLines;
    property OnClick: TFMXGutterBandClickEvent read FOnClick write FOnClick;
  end;

  { Collection of gutter bands }
  TSynFMXGutterBands = class(TObjectList<TSynFMXGutterBand>)
  private
    FGutter: TSynFMXGutter;
  public
    constructor Create(AGutter: TSynFMXGutter);
    function Add(AKind: TSynFMXGutterBandKind; AWidth: Single;
      AVisible: Boolean): TSynFMXGutterBand;
    function BandByKind(AKind: TSynFMXGutterBandKind): TSynFMXGutterBand;
  end;

  { Top-level gutter }
  TSynFMXGutter = class(TPersistent)
  private
    FBands: TSynFMXGutterBands;
    FColor: TColor;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FOwner: TObject; // TCustomFMXSynEdit (avoids circular ref in interface)
    procedure SetColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function RealGutterWidth: Single;
    function BandAtX(X: Single): TSynFMXGutterBand;
    procedure Changed;
    property Bands: TSynFMXGutterBands read FBands;
    property Owner: TObject read FOwner;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Color: TColor read FColor write SetColor default TColors.Whitesmoke;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { Selected text color — shared class from SynEditTypes.pas }
  TSynSelectedColor = SynEditTypes.TSynSelectedColor;

  { TSynEditSearchCustom is now in the shared SynEditTypes.pas unit }

  { FMX multi-selection support — thin subclass of shared TSynSelectionsBase }
  TSynFMXSelections = class(TSynSelectionsBase)
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
    constructor Create(Owner: TObject);
  end;

implementation

uses
  System.UIConsts,
  FMX.SynEdit,
  FMX.SynEditRenderer,
  SynEditCodeFolding;

{ TSynFMXEditMark }

constructor TSynFMXEditMark.Create;
begin
  inherited;
  FBookmarkNum := -1;
  FVisible := True;
end;

function TSynFMXEditMark.GetIsBookmark: Boolean;
begin
  Result := FBookmarkNum >= 0;
end;

{ TSynFMXEditMarkList }

function TSynFMXEditMarkList.GetMarksForLine(ALine: Integer): TArray<TSynFMXEditMark>;
var
  I, Count: Integer;
begin
  Count := 0;
  SetLength(Result, Self.Count);
  for I := 0 to Self.Count - 1 do
    if Items[I].Line = ALine then
    begin
      Result[Count] := Items[I];
      Inc(Count);
    end;
  SetLength(Result, Count);
end;

procedure TSynFMXEditMarkList.ClearLine(ALine: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if (Items[I].Line = ALine) and not Items[I].IsBookmark then
      Delete(I);
end;

{ TSynFMXGutterBand }

constructor TSynFMXGutterBand.Create(AGutter: TSynFMXGutter;
  AKind: TSynFMXGutterBandKind; AWidth: Single; AVisible: Boolean);
begin
  inherited Create;
  FGutter := AGutter;
  FKind := AKind;
  FWidth := AWidth;
  FVisible := AVisible;
  FBackground := gbbGutter;
end;

procedure TSynFMXGutterBand.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    FGutter.Changed;
  end;
end;

procedure TSynFMXGutterBand.SetWidth(Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    FGutter.Changed;
  end;
end;

function TSynFMXGutterBand.RealWidth: Single;
var
  Editor: TCustomFMXSynEdit;
  DigitCount, LC: Integer;
begin
  if not FVisible then
    Exit(0);

  Editor := TCustomFMXSynEdit(FGutter.Owner);

  case FKind of
    gbkLineNumbers:
    begin
      LC := Max(Editor.Lines.Count, 1);
      DigitCount := Max(2, Length(IntToStr(LC)));
      Result := Round((DigitCount + 1) * Editor.CharWidth) + 4;
    end;
    gbkMarks:
      Result := Round(Editor.LineHeight);
    gbkFold:
    begin
      if Editor.UseCodeFolding then
        Result := Editor.CodeFolding.GutterShapeSize + 8
      else
        Result := 0;
    end;
  else
    Result := FWidth;
  end;
end;

function TSynFMXGutterBand.LeftX: Single;
var
  I: Integer;
  Band: TSynFMXGutterBand;
begin
  Result := 0;
  for I := 0 to FGutter.Bands.Count - 1 do
  begin
    Band := FGutter.Bands[I];
    if Band = Self then
      Break;
    Result := Result + Band.RealWidth;
  end;
end;

procedure TSynFMXGutterBand.PaintLines(Canvas: TCanvas; const ClipR: TRectF;
  FirstRow, LastRow: Integer);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  if Assigned(FOnPaintLines) then
    FOnPaintLines(Canvas, ClipR, FirstRow, LastRow, DoDefault);

  if DoDefault then
    case FKind of
      gbkLineNumbers: PaintLineNumbers(Canvas, ClipR, FirstRow, LastRow);
      gbkMarks: PaintMarks(Canvas, ClipR, FirstRow, LastRow);
      gbkFold: PaintFoldShapes(Canvas, ClipR, FirstRow, LastRow);
      gbkMargin: PaintMarginLine(Canvas, ClipR, FirstRow, LastRow);
    end;
end;

procedure TSynFMXGutterBand.DoClick(Button: TMouseButton; X, Y: Single;
  Row, Line: Integer);
var
  Editor: TCustomFMXSynEdit;
  Index, I, BmkX, BmkY: Integer;
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button, X, Y, Row, Line);

  Editor := TCustomFMXSynEdit(FGutter.Owner);

  if FKind = gbkMarks then
  begin
    // Click on marks band clears bookmark on that line
    for I := 0 to 9 do
      if Editor.IsBookmarkSet(I) then
      begin
        Editor.GetBookmark(I, BmkX, BmkY);
        if BmkY = Line then
        begin
          Editor.ClearBookmark(I);
          Break;
        end;
      end;
  end
  else if FKind = gbkFold then
  begin
    if Editor.UseCodeFolding and
      Editor.AllFoldRanges.FoldStartAtLine(Line, Index) then
    begin
      if Editor.AllFoldRanges.Ranges[Index].Collapsed then
        Editor.Uncollapse(Index)
      else
        Editor.Collapse(Index);
    end;
  end;
end;

procedure TSynFMXGutterBand.PaintLineNumbers(Canvas: TCanvas;
  const ClipR: TRectF; FirstRow, LastRow: Integer);
var
  Editor: TCustomFMXSynEdit;
  Renderer: TSynFMXRenderer;
  Row, Line: Integer;
  Y: Single;
  R: TRectF;
  NumStr: string;
begin
  Editor := TCustomFMXSynEdit(FGutter.Owner);
  Renderer := TSynFMXRenderer(Editor.Renderer);

  for Row := FirstRow to LastRow do
  begin
    Line := Editor.RowToLine(Row);
    if Line > Editor.Lines.Count then Break;
    Y := (Row - Editor.TopLine) * Editor.LineHeight;
    // In word wrap mode, only show number on first display row of each line
    if Editor.WordWrap then
    begin
      if Editor.LineToRow(Line) <> Row then
        Continue;
    end;
    NumStr := IntToStr(Line);
    R := RectF(ClipR.Left + 2, Y, ClipR.Right - 4, Y + Editor.LineHeight);
    Renderer.PaintLineNumber(Canvas, R, NumStr, TAlphaColors.Gray);
  end;
end;

procedure TSynFMXGutterBand.PaintMarks(Canvas: TCanvas;
  const ClipR: TRectF; FirstRow, LastRow: Integer);
var
  Editor: TCustomFMXSynEdit;
  Renderer: TSynFMXRenderer;
  Row, Line, I: Integer;
  Y, CX, CY, Radius: Single;
  Mark: TSynFMXEditMark;
begin
  Editor := TCustomFMXSynEdit(FGutter.Owner);
  Renderer := TSynFMXRenderer(Editor.Renderer);

  if (Editor.Marks = nil) or (Editor.Marks.Count = 0) then
    Exit;

  for Row := FirstRow to LastRow do
  begin
    Line := Editor.RowToLine(Row);
    if Line > Editor.Lines.Count then Break;
    Y := (Row - Editor.TopLine) * Editor.LineHeight;

    for I := 0 to Editor.Marks.Count - 1 do
    begin
      Mark := Editor.Marks[I];
      if Mark.Visible and Mark.IsBookmark and (Mark.Line = Line) then
      begin
        CX := ClipR.Left + Editor.CharWidth;
        CY := Y + Editor.LineHeight / 2;
        Radius := Editor.LineHeight / 2 - 1;
        Canvas.Fill.Color := TAlphaColors.Dodgerblue;
        Canvas.FillEllipse(
          RectF(CX - Radius, CY - Radius, CX + Radius, CY + Radius), 1.0);
        Renderer.PaintToken(Canvas,
          CX - Editor.CharWidth / 2, Y,
          IntToStr(Mark.BookmarkNumber),
          TAlphaColors.White, TAlphaColors.Null, [fsBold]);
        Break; // one indicator per line
      end;
    end;
  end;
end;

procedure TSynFMXGutterBand.PaintFoldShapes(Canvas: TCanvas;
  const ClipR: TRectF; FirstRow, LastRow: Integer);
var
  Editor: TCustomFMXSynEdit;
  Renderer: TSynFMXRenderer;
  Row, Line, Index: Integer;
  Y, X, ShapeSize, Margin: Single;
  rcFold: TRectF;
  FoldRange: TSynFoldRange;
  LinesColor: TAlphaColor;
begin
  Editor := TCustomFMXSynEdit(FGutter.Owner);
  if not Editor.UseCodeFolding then Exit;
  Renderer := TSynFMXRenderer(Editor.Renderer);
  ShapeSize := Editor.CodeFolding.GutterShapeSize;
  Margin := 2;
  LinesColor := TColorToAlphaColor(Editor.CodeFolding.FolderBarLinesColor);

  for Row := FirstRow to LastRow do
  begin
    Line := Editor.RowToLine(Row);
    if Line > Editor.Lines.Count then Break;

    Y := (Row - Editor.TopLine) * Editor.LineHeight;
    rcFold := RectF(
      ClipR.Left + (ClipR.Width - ShapeSize) / 2,
      Y + (Editor.LineHeight - ShapeSize) / 2,
      ClipR.Left + (ClipR.Width - ShapeSize) / 2 + ShapeSize,
      Y + (Editor.LineHeight + ShapeSize) / 2);

    if Editor.AllFoldRanges.FoldStartAtLine(Line, Index) then
    begin
      FoldRange := Editor.AllFoldRanges.Ranges[Index];

      Canvas.Stroke.Color := LinesColor;
      Canvas.Stroke.Thickness := 1;
      Canvas.DrawRect(rcFold, 0, 0, AllCorners, 1.0);

      X := rcFold.Left + ShapeSize / 2;
      Renderer.DrawLine(Canvas,
        rcFold.Left + Margin, rcFold.Top + ShapeSize / 2,
        rcFold.Right - Margin, rcFold.Top + ShapeSize / 2,
        LinesColor);

      if FoldRange.Collapsed then
      begin
        Renderer.DrawLine(Canvas,
          X, rcFold.Top + Margin,
          X, rcFold.Bottom - Margin,
          LinesColor);
      end
      else
      begin
        Renderer.DrawLine(Canvas,
          X, rcFold.Bottom,
          X, Y + Editor.LineHeight,
          LinesColor);
      end;
    end
    else
    begin
      X := rcFold.Left + ShapeSize / 2;

      if Editor.AllFoldRanges.FoldEndAtLine(Line, Index) then
      begin
        Renderer.DrawLine(Canvas,
          X, Y,
          X, Y + Editor.LineHeight / 2,
          LinesColor);
        Renderer.DrawLine(Canvas,
          X, Y + Editor.LineHeight / 2,
          rcFold.Right, Y + Editor.LineHeight / 2,
          LinesColor);
      end;

      if Editor.AllFoldRanges.FoldAroundLine(Line, Index) then
      begin
        Renderer.DrawLine(Canvas,
          X, Y,
          X, Y + Editor.LineHeight,
          LinesColor);
      end;
    end;
  end;
end;

procedure TSynFMXGutterBand.PaintMarginLine(Canvas: TCanvas;
  const ClipR: TRectF; FirstRow, LastRow: Integer);
var
  Renderer: TSynFMXRenderer;
begin
  Renderer := TSynFMXRenderer(
    TCustomFMXSynEdit(FGutter.Owner).Renderer);
  Renderer.DrawLine(Canvas,
    ClipR.Right - 1, ClipR.Top,
    ClipR.Right - 1, ClipR.Bottom,
    TAlphaColors.Lightgray);
end;

{ TSynFMXGutterBands }

constructor TSynFMXGutterBands.Create(AGutter: TSynFMXGutter);
begin
  inherited Create(True); // owns objects
  FGutter := AGutter;
end;

function TSynFMXGutterBands.Add(AKind: TSynFMXGutterBandKind; AWidth: Single;
  AVisible: Boolean): TSynFMXGutterBand;
begin
  Result := TSynFMXGutterBand.Create(FGutter, AKind, AWidth, AVisible);
  inherited Add(Result);
end;

function TSynFMXGutterBands.BandByKind(
  AKind: TSynFMXGutterBandKind): TSynFMXGutterBand;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Kind = AKind then
      Exit(Items[I]);
  Result := nil;
end;

{ TSynFMXGutter }

constructor TSynFMXGutter.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := TColors.Whitesmoke;
  FVisible := True;
  FBands := TSynFMXGutterBands.Create(Self);
  // Default bands matching original hardcoded layout
  FBands.Add(gbkLineNumbers, 0, True);
  FBands.Add(gbkMarks, 0, True);
  FBands.Add(gbkFold, 0, False); // hidden until UseCodeFolding enabled
  FBands.Add(gbkMargin, 3, True).Background := gbbNone;
end;

destructor TSynFMXGutter.Destroy;
begin
  FBands.Free;
  inherited;
end;

function TSynFMXGutter.RealGutterWidth: Single;
var
  I: Integer;
begin
  if not FVisible then
    Exit(0);
  Result := 0;
  for I := 0 to FBands.Count - 1 do
    Result := Result + FBands[I].RealWidth;
end;

function TSynFMXGutter.BandAtX(X: Single): TSynFMXGutterBand;
var
  I: Integer;
  Left: Single;
  W: Single;
begin
  Left := 0;
  for I := 0 to FBands.Count - 1 do
  begin
    W := FBands[I].RealWidth;
    if (X >= Left) and (X < Left + W) then
      Exit(FBands[I]);
    Left := Left + W;
  end;
  Result := nil;
end;

procedure TSynFMXGutter.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynFMXGutter.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TSynFMXGutter.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TSynSelectedColor — now in SynEditTypes.pas }

{ TSynFMXSelections }

constructor TSynFMXSelections.Create(Owner: TObject);
begin
  inherited Create(Owner);
end;

procedure TSynFMXSelections.CaretsChanged;
begin
  TCustomFMXSynEdit(FOwner).Repaint;
end;

procedure TSynFMXSelections.DoInvalidateSelection(const Sel: TSynSelection);
begin
  TCustomFMXSynEdit(FOwner).Repaint;
end;

procedure TSynFMXSelections.DoRestoreSelection(const Sel: TSynSelection;
  EnsureVisible: Boolean);
begin
  TCustomFMXSynEdit(FOwner).SetCaretAndSelection(
    Sel.Caret, Sel.Start, Sel.Stop);
end;

function TSynFMXSelections.GetLineText(ALine: Integer): string;
begin
  Result := TCustomFMXSynEdit(FOwner).Lines[ALine - 1];
end;

function TSynFMXSelections.GetWordWrap: Boolean;
begin
  Result := TCustomFMXSynEdit(FOwner).WordWrap;
end;

function TSynFMXSelections.GetScrollPastEOL: Boolean;
begin
  Result := eoScrollPastEol in TCustomFMXSynEdit(FOwner).ScrollOptions;
end;

function TSynFMXSelections.GetRowLength(ARow: Integer): Integer;
begin
  Result := TCustomFMXSynEdit(FOwner).GetRowLength(ARow);
end;

function TSynFMXSelections.BufferToDisplayPos(
  const P: TBufferCoord): TDisplayCoord;
begin
  Result := TCustomFMXSynEdit(FOwner).BufferToDisplayPos(P);
end;

function TSynFMXSelections.DisplayToBufferPos(
  const P: TDisplayCoord): TBufferCoord;
begin
  Result := TCustomFMXSynEdit(FOwner).DisplayToBufferPos(P);
end;

function TSynFMXSelections.SelectionToDisplayRow(
  var Sel: TSynSelection): Integer;
begin
  Result := BufferToDisplayPos(Sel.Caret).Row;
end;

end.
