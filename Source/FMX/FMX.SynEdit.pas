{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEdit;

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
  FMX.Controls,
  FMX.Graphics,
  FMX.TextLayout,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditTextBuffer,
  SynEditCodeFolding,
  SynEditMiscClasses,
  SynEditKeyConst,
  SynEditKbdHandler,
  SynEditMiscProcs;

type
  TCustomFMXSynEdit = class(TControl)
  private
    FLines: TSynEditStringList;
    FHighlighter: TSynCustomHighlighter;
    FFont: TFont;
    FTabWidth: Integer;
    FReadOnly: Boolean;
    FModified: Boolean;
    FCaretX: Integer;
    FCaretY: Integer;
    FInsertMode: Boolean;
    FTopLine: Integer;
    FLeftChar: Integer;
    FRightEdge: Integer;
    FRightEdgeColor: TColor;
    FGutter: TObject; // TODO: TSynGutter
    FOptions: TSynEditorOptions;
    FScrollOptions: TSynEditorScrollOptions;
    FOnChange: TNotifyEvent;
    FOnStatusChange: TNotifyEvent;
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetTabWidth(Value: Integer);
    procedure SetReadOnly(Value: Boolean);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure SetTopLine(Value: Integer);
    procedure SetLeftChar(Value: Integer);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure SetScrollOptions(Value: TSynEditorScrollOptions);
    function GetLineCount: Integer;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure ClearAll;
    procedure Undo;
    procedure Redo;
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure ClearSelection;
    function GetTextRange(AStart, AEnd: TBufferCoord): string;
    property Lines: TSynEditStringList read FLines;
    property LineCount: Integer read GetLineCount;
    property CaretX: Integer read FCaretX write SetCaretX;
    property CaretY: Integer read FCaretY write SetCaretY;
    property TopLine: Integer read FTopLine write SetTopLine;
    property LeftChar: Integer read FLeftChar write SetLeftChar;
    property Modified: Boolean read FModified;
    property InsertMode: Boolean read FInsertMode write FInsertMode;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  published
    property Font: TFont read FFont write FFont;
    property Highlighter: TSynCustomHighlighter read FHighlighter
      write SetHighlighter;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 8;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RightEdge: Integer read FRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor read FRightEdgeColor write SetRightEdgeColor
      default clSilver;
    property Options: TSynEditorOptions read FOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    property ScrollOptions: TSynEditorScrollOptions read FScrollOptions
      write SetScrollOptions default SYNEDIT_DEFAULT_SCROLLOPTIONS;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStatusChange: TNotifyEvent read FOnStatusChange
      write FOnStatusChange;
  end;

  TFMXSynEdit = class(TCustomFMXSynEdit)
  published
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property Enabled;
    property Height;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Inherited published properties }
    property Font;
    property Highlighter;
    property TabWidth;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property Options;
    property ScrollOptions;
    property OnChange;
    property OnStatusChange;
  end;

implementation

uses
  FMX.SynEditRenderer;

{ TCustomFMXSynEdit }

constructor TCustomFMXSynEdit.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TSynEditStringList.Create(nil);
  FFont := TFont.Create;
  FFont.Family := 'Consolas';
  FFont.Size := 10;
  FTabWidth := 8;
  FInsertMode := True;
  FCaretX := 1;
  FCaretY := 1;
  FTopLine := 1;
  FLeftChar := 1;
  FRightEdge := 80;
  FRightEdgeColor := TColors.Silver;
  FOptions := SYNEDIT_DEFAULT_OPTIONS;
  FScrollOptions := SYNEDIT_DEFAULT_SCROLLOPTIONS;
  CanFocus := True;
  TabStop := True;
  SetAcceptsControls(False);
end;

destructor TCustomFMXSynEdit.Destroy;
begin
  FHighlighter := nil;
  FFont.Free;
  FLines.Free;
  inherited;
end;

procedure TCustomFMXSynEdit.Paint;
begin
  // TODO: Implement FMX painting using Canvas
  Canvas.BeginScene;
  try
    Canvas.ClearRect(LocalRect, TAlphaColors.White);
  finally
    Canvas.EndScene;
  end;
end;

procedure TCustomFMXSynEdit.Resize;
begin
  inherited;
  // TODO: Recalculate visible lines/columns
end;

procedure TCustomFMXSynEdit.DoEnter;
begin
  inherited;
  // TODO: Show caret
end;

procedure TCustomFMXSynEdit.DoExit;
begin
  inherited;
  // TODO: Hide caret
end;

procedure TCustomFMXSynEdit.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;
  // TODO: Handle key input
end;

procedure TCustomFMXSynEdit.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;
end;

procedure TCustomFMXSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  // TODO: Handle mouse down for cursor positioning
end;

procedure TCustomFMXSynEdit.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  // TODO: Handle mouse move for selection
end;

procedure TCustomFMXSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
end;

procedure TCustomFMXSynEdit.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;
  // TODO: Handle scrolling
end;

procedure TCustomFMXSynEdit.LoadFromFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomFMXSynEdit.SaveToFile(const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomFMXSynEdit.LoadFromStream(AStream: TStream);
begin
  FLines.LoadFromStream(AStream);
  FCaretX := 1;
  FCaretY := 1;
  FTopLine := 1;
  FLeftChar := 1;
  FModified := False;
  Repaint;
end;

procedure TCustomFMXSynEdit.SaveToStream(AStream: TStream);
begin
  FLines.SaveToStream(AStream);
  FModified := False;
end;

procedure TCustomFMXSynEdit.ClearAll;
begin
  FLines.Clear;
  FCaretX := 1;
  FCaretY := 1;
  FModified := False;
  Repaint;
end;

procedure TCustomFMXSynEdit.Undo;
begin
  // TODO: Implement
end;

procedure TCustomFMXSynEdit.Redo;
begin
  // TODO: Implement
end;

procedure TCustomFMXSynEdit.CutToClipboard;
begin
  // TODO: Implement
end;

procedure TCustomFMXSynEdit.CopyToClipboard;
begin
  // TODO: Implement
end;

procedure TCustomFMXSynEdit.PasteFromClipboard;
begin
  // TODO: Implement
end;

procedure TCustomFMXSynEdit.SelectAll;
begin
  // TODO: Implement
end;

procedure TCustomFMXSynEdit.ClearSelection;
begin
  // TODO: Implement
end;

function TCustomFMXSynEdit.GetTextRange(AStart, AEnd: TBufferCoord): string;
begin
  Result := ''; // TODO: Implement
end;

procedure TCustomFMXSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if FHighlighter <> Value then
  begin
    FHighlighter := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetTabWidth(Value: Integer);
begin
  if (Value > 0) and (Value <> FTabWidth) then
  begin
    FTabWidth := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TCustomFMXSynEdit.SetCaretX(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FCaretX <> Value then
  begin
    FCaretX := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetCaretY(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FCaretY <> Value then
  begin
    FCaretY := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetTopLine(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FTopLine <> Value then
  begin
    FTopLine := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetLeftChar(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FLeftChar <> Value then
  begin
    FLeftChar := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetRightEdge(Value: Integer);
begin
  if FRightEdge <> Value then
  begin
    FRightEdge := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetRightEdgeColor(Value: TColor);
begin
  if FRightEdgeColor <> Value then
  begin
    FRightEdgeColor := Value;
    Repaint;
  end;
end;

procedure TCustomFMXSynEdit.SetOptions(Value: TSynEditorOptions);
begin
  FOptions := Value;
end;

procedure TCustomFMXSynEdit.SetScrollOptions(Value: TSynEditorScrollOptions);
begin
  FScrollOptions := Value;
end;

function TCustomFMXSynEdit.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TCustomFMXSynEdit.GetCanUndo: Boolean;
begin
  Result := False; // TODO: Implement
end;

function TCustomFMXSynEdit.GetCanRedo: Boolean;
begin
  Result := False; // TODO: Implement
end;

end.
