{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynSpellCheck;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  FMX.Graphics,
  SynEditTypes,
  SynEditMiscProcs,
  SynSpellCheckTypes,
  FMX.SynEdit;

{$REGION 'TSynFMXSpellCheck Component'}

type
  TSynFMXSpellCheck = class;

  TSynFMXSpellPaintPlugin = class(TSynFMXEditPlugin)
  private
    FSpellCheck: TSynFMXSpellCheck;
  public
    constructor Create(AOwner: TCustomFMXSynEdit;
      ASpellCheck: TSynFMXSpellCheck); reintroduce;
    destructor Destroy; override;
    procedure AfterPaint(Canvas: TCanvas; const AClip: TRectF;
      FirstLine, LastLine: Integer); override;
  end;

  TSynFMXSpellCheck = class(TComponent)
  private
    FEditor: TComponent;
    FProvider: ISynSpellCheckProvider;
    FLanguage: string;
    FErrors: TList<TSynSpellError>;
    FEnabled: Boolean;
    FUnderlineColor: TAlphaColor;
    FPaintPlugin: TSynFMXSpellPaintPlugin;
    FOnCheckComplete: TNotifyEvent;
    FCheckTokens: TSynSpellCheckTokens;
    procedure SetEditor(Value: TComponent);
    procedure SetLanguage(const Value: string);
    procedure SetEnabled(Value: Boolean);
    function GetLineText(ALine: Integer): string;
    function GetLineCount: Integer;
    function GetEditorLines: TStrings;
    function GetEditorBlockBegin: TBufferCoord;
    function GetEditorBlockEnd: TBufferCoord;
    procedure DoCheckText(const AText: string; ALine: Integer;
      AStartOffset: Integer = 0);
    procedure DoCheckLine(ALine: Integer);
    procedure InvalidateEditor;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckLine(ALine: Integer);
    procedure CheckFile;
    procedure CheckSelection;
    procedure ClearErrors;
    function ErrorAtPos(ALine, AChar: Integer): Integer;
    property Errors: TList<TSynSpellError> read FErrors;
    property Provider: ISynSpellCheckProvider read FProvider write FProvider;
    property UnderlineColor: TAlphaColor read FUnderlineColor
      write FUnderlineColor;
  published
    property Editor: TComponent read FEditor write SetEditor;
    property Language: string read FLanguage write SetLanguage;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property CheckTokens: TSynSpellCheckTokens read FCheckTokens
      write FCheckTokens default [sctComment, sctString, sctIdentifier];
    property OnCheckComplete: TNotifyEvent read FOnCheckComplete
      write FOnCheckComplete;
  end;

{$ENDREGION 'TSynFMXSpellCheck Component'}

implementation

uses
  System.Character,
  SynEditHighlighter,
  SynEditTextBuffer;

type
  TSynHighlighterAccess = class(TSynCustomHighlighter);

{ ============================================================================ }
{ TSynFMXSpellPaintPlugin                                                      }
{ ============================================================================ }

constructor TSynFMXSpellPaintPlugin.Create(AOwner: TCustomFMXSynEdit;
  ASpellCheck: TSynFMXSpellCheck);
begin
  inherited Create(AOwner, [phAfterPaint]);
  FSpellCheck := ASpellCheck;
end;

destructor TSynFMXSpellPaintPlugin.Destroy;
begin
  inherited;
end;

procedure TSynFMXSpellPaintPlugin.AfterPaint(Canvas: TCanvas;
  const AClip: TRectF; FirstLine, LastLine: Integer);
var
  Error: TSynSpellError;
  Pt1, Pt2: TPointF;
  BaseY, X, Delta, NextX: Single;
  Up: Boolean;
begin
  if not FSpellCheck.Enabled then Exit;
  if FSpellCheck.Errors.Count = 0 then Exit;

  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := FSpellCheck.UnderlineColor;
  Canvas.Stroke.Thickness := 1.0;

  for Error in FSpellCheck.Errors do
  begin
    if (Error.Line < FirstLine) or (Error.Line > LastLine) then
      Continue;

    Pt1 := Owner.BufferCoordToPixel(BufferCoord(Error.StartChar, Error.Line));
    Pt2 := Owner.BufferCoordToPixel(BufferCoord(Error.EndChar, Error.Line));
    BaseY := Pt1.Y + Owner.LineHeight - 1;
    Delta := Owner.LineHeight / 6;

    // Draw zigzag wave
    X := Pt1.X;
    Up := True;
    while X < Pt2.X do
    begin
      NextX := Min(X + Delta, Pt2.X);
      if Up then
        Canvas.DrawLine(PointF(X, BaseY), PointF(NextX, BaseY - Delta), 1.0)
      else
        Canvas.DrawLine(PointF(X, BaseY - Delta), PointF(NextX, BaseY), 1.0);
      X := NextX;
      Up := not Up;
    end;
  end;
end;

{ ============================================================================ }
{ TSynFMXSpellCheck                                                            }
{ ============================================================================ }

constructor TSynFMXSpellCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FErrors := TList<TSynSpellError>.Create;
  FEnabled := True;
  FLanguage := 'en-US';
  FUnderlineColor := TAlphaColors.Red;
  FCheckTokens := [sctComment, sctString, sctIdentifier];
  FProvider := nil;
  FEditor := nil;
end;

destructor TSynFMXSpellCheck.Destroy;
begin
  FPaintPlugin.Free;
  if Assigned(FEditor) then
    FEditor.RemoveFreeNotification(Self);
  FErrors.Free;
  FProvider := nil;
  inherited;
end;

procedure TSynFMXSpellCheck.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FEditor) then
  begin
    FreeAndNil(FPaintPlugin);
    FEditor := nil;
  end;
end;

procedure TSynFMXSpellCheck.SetEditor(Value: TComponent);
begin
  if Value <> FEditor then
  begin
    FreeAndNil(FPaintPlugin);
    if Assigned(FEditor) then
      FEditor.RemoveFreeNotification(Self);
    FEditor := Value;
    if Assigned(FEditor) then
    begin
      FEditor.FreeNotification(Self);
      if FEditor is TCustomFMXSynEdit then
        FPaintPlugin := TSynFMXSpellPaintPlugin.Create(
          TCustomFMXSynEdit(FEditor), Self);
    end;
    ClearErrors;
  end;
end;

procedure TSynFMXSpellCheck.SetLanguage(const Value: string);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    if Assigned(FProvider) then
      FProvider.SetLanguage(Value);
    ClearErrors;
  end;
end;

procedure TSynFMXSpellCheck.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not FEnabled then
      ClearErrors
    else
      InvalidateEditor;
  end;
end;

function TSynFMXSpellCheck.GetEditorLines: TStrings;
begin
  if FEditor is TCustomFMXSynEdit then
    Result := TCustomFMXSynEdit(FEditor).Lines
  else
    Result := nil;
end;

function TSynFMXSpellCheck.GetEditorBlockBegin: TBufferCoord;
begin
  if FEditor is TCustomFMXSynEdit then
    Result := TCustomFMXSynEdit(FEditor).BlockBegin
  else
    Result := BufferCoord(0, 0);
end;

function TSynFMXSpellCheck.GetEditorBlockEnd: TBufferCoord;
begin
  if FEditor is TCustomFMXSynEdit then
    Result := TCustomFMXSynEdit(FEditor).BlockEnd
  else
    Result := BufferCoord(0, 0);
end;

function TSynFMXSpellCheck.GetLineText(ALine: Integer): string;
var
  Lines: TStrings;
begin
  Result := '';
  Lines := GetEditorLines;
  if Assigned(Lines) and (ALine >= 1) and (ALine <= Lines.Count) then
    Result := Lines[ALine - 1];
end;

function TSynFMXSpellCheck.GetLineCount: Integer;
var
  Lines: TStrings;
begin
  Result := 0;
  Lines := GetEditorLines;
  if Assigned(Lines) then
    Result := Lines.Count;
end;

procedure TSynFMXSpellCheck.DoCheckText(const AText: string; ALine: Integer;
  AStartOffset: Integer);
var
  Words: TArray<TWordInfo>;
  Info: TWordInfo;
  Err: TSynSpellError;
begin
  if not Assigned(FProvider) or not FProvider.IsAvailable then
    Exit;
  if AText = '' then
    Exit;

  Words := ExtractWords(AText);
  for Info in Words do
  begin
    // Skip tokens that are all digits or have no letters
    if not ContainsLetter(Info.Word) then
      Continue;

    if not FProvider.CheckWord(Info.Word) then
    begin
      Err.Line := ALine;
      Err.StartChar := Info.StartChar + AStartOffset;
      Err.EndChar := Info.EndChar + AStartOffset;
      Err.Word := Info.Word;
      FErrors.Add(Err);
    end;
  end;
end;

procedure TSynFMXSpellCheck.DoCheckLine(ALine: Integer);
var
  Ed: TCustomFMXSynEdit;
  HL: TSynCustomHighlighter;
  Attr, DefComment, DefString, DefIdent: TSynHighlighterAttributes;
  LineText, Token: string;
  TokenPos: Integer;
  ShouldCheck: Boolean;
begin
  LineText := GetLineText(ALine);
  if LineText = '' then
    Exit;

  // If the editor has a highlighter and we have token filters, use them
  if (FEditor is TCustomFMXSynEdit) then
  begin
    Ed := TCustomFMXSynEdit(FEditor);
    HL := Ed.Highlighter;
    if Assigned(HL) and (FCheckTokens <> []) then
    begin
      // Cache default attributes for comparison
      DefComment := TSynHighlighterAccess(HL).GetDefaultAttribute(SYN_ATTR_COMMENT);
      DefString := TSynHighlighterAccess(HL).GetDefaultAttribute(SYN_ATTR_STRING);
      DefIdent := TSynHighlighterAccess(HL).GetDefaultAttribute(SYN_ATTR_IDENTIFIER);

      // Set up highlighter range from previous line
      if ALine <= 1 then
        HL.ResetRange
      else
        HL.SetRange(Ed.Lines.Ranges[ALine - 2]); // 0-based index

      HL.SetLine(LineText, ALine - 1);

      while not HL.GetEol do
      begin
        Attr := HL.GetTokenAttribute;
        ShouldCheck := False;

        if Assigned(Attr) then
        begin
          if (sctComment in FCheckTokens) and (Attr = DefComment) then
            ShouldCheck := True
          else if (sctString in FCheckTokens) and (Attr = DefString) then
            ShouldCheck := True
          else if (sctIdentifier in FCheckTokens) and (Attr = DefIdent) then
            ShouldCheck := True;
        end;

        if ShouldCheck then
        begin
          Token := HL.GetToken;
          TokenPos := HL.GetTokenPos; // 0-based
          DoCheckText(Token, ALine, TokenPos);
        end;

        HL.Next;
      end;
      Exit;
    end;
  end;

  // Fallback: no highlighter — check entire line
  DoCheckText(LineText, ALine);
end;

procedure TSynFMXSpellCheck.InvalidateEditor;
begin
  if FEditor is TCustomFMXSynEdit then
    TCustomFMXSynEdit(FEditor).Repaint;
end;

procedure TSynFMXSpellCheck.CheckLine(ALine: Integer);
var
  I: Integer;
begin
  if not FEnabled or not Assigned(FEditor) or not Assigned(FProvider) then
    Exit;

  // Remove existing errors for this line
  for I := FErrors.Count - 1 downto 0 do
    if FErrors[I].Line = ALine then
      FErrors.Delete(I);

  DoCheckLine(ALine);

  InvalidateEditor;

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.CheckFile;
var
  I, LC: Integer;
begin
  if not FEnabled or not Assigned(FEditor) or not Assigned(FProvider) then
    Exit;

  ClearErrors;

  LC := GetLineCount;
  for I := 1 to LC do
    DoCheckLine(I);

  InvalidateEditor;

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.CheckSelection;
var
  BB, BE: TBufferCoord;
  Line, I: Integer;
  Err: TSynSpellError;
begin
  if not FEnabled or not Assigned(FEditor) or not Assigned(FProvider) then
    Exit;

  BB := GetEditorBlockBegin;
  BE := GetEditorBlockEnd;

  // If no valid selection, check the whole file
  if (BB.Line = 0) or (BE.Line = 0) or
     ((BB.Line = BE.Line) and (BB.Char = BE.Char)) then
  begin
    CheckFile;
    Exit;
  end;

  // Check each line in the selection using highlighter-aware logic
  for Line := BB.Line to BE.Line do
    DoCheckLine(Line);

  // Remove errors that fall outside the selection bounds
  for I := FErrors.Count - 1 downto 0 do
  begin
    Err := FErrors[I];
    if (Err.Line = BB.Line) and (Err.StartChar < BB.Char) then
      FErrors.Delete(I)
    else if (Err.Line = BE.Line) and (Err.EndChar > BE.Char) then
      FErrors.Delete(I);
  end;

  InvalidateEditor;

  if Assigned(FOnCheckComplete) then
    FOnCheckComplete(Self);
end;

procedure TSynFMXSpellCheck.ClearErrors;
begin
  FErrors.Clear;
  InvalidateEditor;
end;

function TSynFMXSpellCheck.ErrorAtPos(ALine, AChar: Integer): Integer;
var
  I: Integer;
  Err: TSynSpellError;
begin
  for I := 0 to FErrors.Count - 1 do
  begin
    Err := FErrors[I];
    if (Err.Line = ALine) and (AChar >= Err.StartChar) and
       (AChar < Err.EndChar) then
      Exit(I);
  end;
  Result := -1;
end;

end.
