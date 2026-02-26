{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynCompletionProposal.pas, released 2000-04-11.
The Original Code is based on mwCompletionProposal.pas by Cyrille de Brebisson,
part of the mwEdit component suite.

FMX port: Clean FMX implementation using TPopup instead of VCL TCustomForm.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}

unit FMX.SynCompletionProposal;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.SysUtils,
  System.Classes,
  System.Math,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Graphics,
  SynEditTypes,
  SynEditKeyCmds,
  FMX.SynEdit;

type
  TSynCompletionType = (ctCode, ctParams);

  TSynCompletionOption = (scoLimitToMatchedText, scoCaseSensitive,
    scoUseInsertList, scoUsePrettyText, scoEndCharCompletion,
    scoCompleteWithTab, scoCompleteWithEnter);
  TSynCompletionOptions = set of TSynCompletionOption;

  TCodeCompletionEvent = procedure(Sender: TObject; var Value: string;
    Shift: TShiftState; Index: Integer; EndToken: WideChar) of object;
  TAfterCodeCompletionEvent = procedure(Sender: TObject; const Value: string;
    Shift: TShiftState; Index: Integer; EndToken: WideChar) of object;
  TValidateEvent = procedure(Sender: TObject; Shift: TShiftState;
    EndToken: WideChar) of object;
  TSynCompletionExecuteEvent = procedure(Kind: TSynCompletionType;
    Sender: TObject; var CurrentInput: string; var X, Y: Integer;
    var CanExecute: Boolean) of object;

const
  DefaultProposalOptions = [scoLimitToMatchedText, scoEndCharCompletion,
    scoCompleteWithTab, scoCompleteWithEnter];
  DefaultEndOfTokenChr = '()[]. ';

type
  { TSynFMXCompletionProposalForm - dropdown popup showing the completion list.
    Uses FMX TPopup as the container. }
  TSynFMXCompletionProposalForm = class(TPopup)
  private
    FItemList: TStrings;          // full display items
    FInsertList: TStrings;        // full insert items
    FAssignedList: TStrings;      // filtered display items
    FFilteredInsertList: TStrings;// filtered insert items
    FCurrentString: string;       // what user has typed so far
    FPosition: Integer;           // selected index in FAssignedList
    FLinesInWindow: Integer;      // number of visible items
    FItemHeight: Single;          // height of one item in pixels
    FScrollBar: TScrollBar;       // vertical scrollbar
    FClSelect: TAlphaColor;       // selection background color
    FClSelectText: TAlphaColor;   // selection text color
    FClBackground: TAlphaColor;   // background color
    FClText: TAlphaColor;         // normal text color
    FFont: TFont;
    FMargin: Single;
    FCaseSensitive: Boolean;
    FUseInsertList: Boolean;
    FMatchText: Boolean;
    FCompleteWithTab: Boolean;
    FCompleteWithEnter: Boolean;
    FOnValidate: TValidateEvent;
    FOnCancel: TNotifyEvent;
    FEndOfTokenChr: string;
    FMouseWheelAccumulator: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure ScrollBarChanged(Sender: TObject);
    function GetTopIndex: Integer;
    procedure SetTopIndex(Value: Integer);
    function IsWordBreakChar(AChar: WideChar): Boolean;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MoveLine(DY: Integer);
    procedure AddItem(const ADisplay, AInsert: string);
    procedure ClearList;
    property CurrentString: string read FCurrentString write SetCurrentString;
    property Position: Integer read FPosition write SetPosition;
    property ItemList: TStrings read FItemList;
    property InsertList: TStrings read FInsertList;
    property AssignedList: TStrings read FAssignedList;
    property FilteredInsertList: TStrings read FFilteredInsertList;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property UseInsertList: Boolean read FUseInsertList write FUseInsertList;
    property MatchText: Boolean read FMatchText write FMatchText;
    property CompleteWithTab: Boolean read FCompleteWithTab write FCompleteWithTab;
    property CompleteWithEnter: Boolean read FCompleteWithEnter write FCompleteWithEnter;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property ClSelect: TAlphaColor read FClSelect write FClSelect;
    property ClSelectText: TAlphaColor read FClSelectText write FClSelectText;
    property ClBackground: TAlphaColor read FClBackground write FClBackground;
    property ClText: TAlphaColor read FClText write FClText;
  end;

  { TSynFMXCompletionProposal - main component dropped on the form. }
  TSynFMXCompletionProposal = class(TComponent)
  private
    FEditor: TComponent;  // TCustomFMXSynEdit
    FForm: TSynFMXCompletionProposalForm;
    FOptions: TSynCompletionOptions;
    FShortCut: TShortCut;
    FTriggerChars: string;
    FEndOfTokenChr: string;
    FTimerInterval: Integer;
    FTimer: TTimer;
    FItemList: TStrings;
    FInsertList: TStrings;
    FOnExecute: TSynCompletionExecuteEvent;
    FOnCodeCompletion: TCodeCompletionEvent;
    FOnAfterCodeCompletion: TAfterCodeCompletionEvent;
    FOnCancel: TNotifyEvent;
    FActive: Boolean;
    FCompletionStart: Integer;
    procedure SetEditor(Value: TComponent);
    procedure TimerExecute(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure FormValidate(Sender: TObject; Shift: TShiftState;
      EndToken: WideChar);
    procedure FormCancel(Sender: TObject);
    function GetCurrentInput: string;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(const ACurrentInput: string; X, Y: Integer);
    procedure Activate;
    procedure Deactivate;
    procedure CancelCompletion;
    function IsActive: Boolean;
    property Form: TSynFMXCompletionProposalForm read FForm;
    property CompletionStart: Integer read FCompletionStart
      write FCompletionStart;
  published
    property Editor: TComponent read FEditor write SetEditor;
    property Options: TSynCompletionOptions read FOptions write FOptions
      default DefaultProposalOptions;
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property TriggerChars: string read FTriggerChars write FTriggerChars;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property TimerInterval: Integer read FTimerInterval write FTimerInterval
      default 0;
    property ItemList: TStrings read FItemList write FItemList;
    property InsertList: TStrings read FInsertList write FInsertList;
    property OnExecute: TSynCompletionExecuteEvent read FOnExecute
      write FOnExecute;
    property OnCodeCompletion: TCodeCompletionEvent read FOnCodeCompletion
      write FOnCodeCompletion;
    property OnAfterCodeCompletion: TAfterCodeCompletionEvent
      read FOnAfterCodeCompletion write FOnAfterCodeCompletion;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

implementation

uses
  FMX.Platform,
  FMX.Forms,
  FMX.TextLayout,
  SynEditKeyConst;

{ --------------------------------------------------------------------------- }
{ TSynFMXCompletionProposalForm                                               }
{ --------------------------------------------------------------------------- }

constructor TSynFMXCompletionProposalForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemList := TStringList.Create;
  FInsertList := TStringList.Create;
  FAssignedList := TStringList.Create;
  FFilteredInsertList := TStringList.Create;
  FFont := TFont.Create;
  FFont.Family := 'Consolas';
  FFont.Size := 10;
  FItemHeight := 18;
  FLinesInWindow := 8;
  FMargin := 4;
  FPosition := 0;
  FCurrentString := '';
  FEndOfTokenChr := DefaultEndOfTokenChr;
  FCaseSensitive := False;
  FUseInsertList := False;
  FMatchText := True;
  FCompleteWithTab := True;
  FCompleteWithEnter := True;
  FMouseWheelAccumulator := 0;

  // Default FMX-friendly colors
  FClBackground := TAlphaColorRec.White;
  FClText := TAlphaColorRec.Black;
  FClSelect := $FF3399FF;     // bright blue selection
  FClSelectText := TAlphaColorRec.White;

  // Create scrollbar
  FScrollBar := TScrollBar.Create(Self);
  FScrollBar.Orientation := TOrientation.Vertical;
  FScrollBar.Align := TAlignLayout.Right;
  FScrollBar.Width := 16;
  FScrollBar.Min := 0;
  FScrollBar.Max := 0;
  FScrollBar.SmallChange := 1;
  FScrollBar.OnChange := ScrollBarChanged;
  FScrollBar.Parent := Self;

  // Popup sizing
  Width := 300;
  Height := FItemHeight * FLinesInWindow + 2;
  PlacementTarget := nil;
  Placement := TPlacement.Absolute;
end;

destructor TSynFMXCompletionProposalForm.Destroy;
begin
  FFilteredInsertList.Free;
  FAssignedList.Free;
  FInsertList.Free;
  FItemList.Free;
  FFont.Free;
  inherited;
end;

function TSynFMXCompletionProposalForm.GetTopIndex: Integer;
begin
  Result := Round(FScrollBar.Value);
end;

procedure TSynFMXCompletionProposalForm.SetTopIndex(Value: Integer);
begin
  FScrollBar.Value := EnsureRange(Value, 0, Max(0, FAssignedList.Count - FLinesInWindow));
end;

procedure TSynFMXCompletionProposalForm.ScrollBarChanged(Sender: TObject);
begin
  // If the selected position fell out of the visible area, adjust
  if FPosition < GetTopIndex then
    FPosition := GetTopIndex
  else if FPosition > GetTopIndex + FLinesInWindow - 1 then
    FPosition := GetTopIndex + FLinesInWindow - 1;
  Repaint;
end;

function TSynFMXCompletionProposalForm.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result := Pos(AChar, FEndOfTokenChr) > 0;
end;

procedure TSynFMXCompletionProposalForm.Paint;
var
  I, TopIdx, ItemIdx: Integer;
  R: TRectF;
  TextR: TRectF;
  ItemText: string;
  TextColor: TAlphaColor;
  Layout: TTextLayout;
  AvailableWidth: Single;
begin
  inherited;

  Canvas.BeginScene;
  try
    // Fill background
    R := LocalRect;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := FClBackground;
    Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);

    // Draw a border
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := TAlphaColorRec.Gray;
    Canvas.Stroke.Thickness := 1;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);

    // Calculate visible lines
    AvailableWidth := Width - FScrollBar.Width;
    FLinesInWindow := Max(1, Trunc((Height - 2) / FItemHeight));

    TopIdx := GetTopIndex;

    // Create a text layout for drawing
    Layout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      Layout.Font.Assign(FFont);
      Layout.HorizontalAlign := TTextAlign.Leading;
      Layout.VerticalAlign := TTextAlign.Center;
      Layout.WordWrap := False;
      Layout.Trimming := TTextTrimming.Character;

      for I := 0 to FLinesInWindow - 1 do
      begin
        ItemIdx := TopIdx + I;
        if ItemIdx >= FAssignedList.Count then
          Break;

        TextR := RectF(FMargin, 1 + FItemHeight * I,
          AvailableWidth - FMargin, 1 + FItemHeight * (I + 1));

        // Draw selection highlight
        if ItemIdx = FPosition then
        begin
          Canvas.Fill.Color := FClSelect;
          Canvas.FillRect(RectF(1, 1 + FItemHeight * I,
            AvailableWidth, 1 + FItemHeight * (I + 1)),
            0, 0, AllCorners, AbsoluteOpacity);
          TextColor := FClSelectText;
        end
        else
          TextColor := FClText;

        ItemText := FAssignedList[ItemIdx];

        Layout.Text := ItemText;
        Layout.Color := TextColor;
        Layout.MaxSize := PointF(TextR.Width, TextR.Height);
        Layout.TopLeft := TextR.TopLeft;
        Layout.RenderLayout(Canvas);
      end;
    finally
      Layout.Free;
    end;
  finally
    Canvas.EndScene;
  end;
end;

procedure TSynFMXCompletionProposalForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  ClickedIdx: Integer;
begin
  inherited;
  ClickedIdx := GetTopIndex + Trunc((Y - 1) / FItemHeight);
  if (ClickedIdx >= 0) and (ClickedIdx < FAssignedList.Count) then
  begin
    Position := ClickedIdx;
    // Double-click validates
    if ssDouble in Shift then
      if Assigned(FOnValidate) then
        FOnValidate(Self, [], #0);
  end;
end;

procedure TSynFMXCompletionProposalForm.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  Delta: Integer;
begin
  Inc(FMouseWheelAccumulator, WheelDelta);
  Delta := FMouseWheelAccumulator div 120;
  FMouseWheelAccumulator := FMouseWheelAccumulator mod 120;
  if Delta <> 0 then
    MoveLine(-Delta * 3);
  Handled := True;
end;

procedure TSynFMXCompletionProposalForm.MoveLine(DY: Integer);
var
  NewPos: Integer;
begin
  NewPos := EnsureRange(FPosition + DY, 0, Max(0, FAssignedList.Count - 1));
  if NewPos <> FPosition then
    Position := NewPos;
end;

procedure TSynFMXCompletionProposalForm.AddItem(const ADisplay, AInsert: string);
begin
  FItemList.Add(ADisplay);
  FInsertList.Add(AInsert);
end;

procedure TSynFMXCompletionProposalForm.ClearList;
begin
  FItemList.Clear;
  FInsertList.Clear;
  FAssignedList.Clear;
  FFilteredInsertList.Clear;
  FPosition := 0;
end;

procedure TSynFMXCompletionProposalForm.SetCurrentString(const Value: string);
var
  I: Integer;
  CompareStr: string;
  Matches: Boolean;
begin
  FCurrentString := Value;

  if FMatchText then
  begin
    // Filter items by matching prefix
    FAssignedList.Clear;
    FFilteredInsertList.Clear;

    for I := 0 to FItemList.Count - 1 do
    begin
      if FUseInsertList and (I < FInsertList.Count) then
        CompareStr := FInsertList[I]
      else
        CompareStr := FItemList[I];

      if FCaseSensitive then
        Matches := CompareStr.StartsWith(Value)
      else
        Matches := CompareStr.StartsWith(Value, True);

      if Matches then
      begin
        FAssignedList.AddObject(FItemList[I], TObject(I));
        if I < FInsertList.Count then
          FFilteredInsertList.Add(FInsertList[I])
        else
          FFilteredInsertList.Add(FItemList[I]);
      end;
    end;
  end
  else
  begin
    // No filtering: show all items, find best match
    FAssignedList.Assign(FItemList);
    FFilteredInsertList.Assign(FInsertList);
  end;

  // Update scrollbar
  FLinesInWindow := Max(1, Trunc((Height - 2) / FItemHeight));
  FScrollBar.Max := Max(0, FAssignedList.Count - FLinesInWindow);
  FScrollBar.Value := 0;

  // Reset position
  if FMatchText then
    FPosition := 0
  else
  begin
    // Find first matching item
    FPosition := 0;
    for I := 0 to FAssignedList.Count - 1 do
    begin
      if FUseInsertList and (I < FInsertList.Count) then
        CompareStr := FInsertList[I]
      else
        CompareStr := FAssignedList[I];

      if FCaseSensitive then
        Matches := CompareStr.StartsWith(Value)
      else
        Matches := CompareStr.StartsWith(Value, True);

      if Matches then
      begin
        FPosition := I;
        Break;
      end;
    end;
  end;

  Repaint;
end;

procedure TSynFMXCompletionProposalForm.SetPosition(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value >= FAssignedList.Count then
    Value := Max(0, FAssignedList.Count - 1);

  if FPosition = Value then
    Exit;

  FPosition := Value;

  // Ensure the selected item is visible
  if FPosition < GetTopIndex then
    SetTopIndex(FPosition)
  else if FPosition >= GetTopIndex + FLinesInWindow then
    SetTopIndex(FPosition - FLinesInWindow + 1);

  Repaint;
end;

{ --------------------------------------------------------------------------- }
{ TSynFMXCompletionProposal                                                   }
{ --------------------------------------------------------------------------- }

constructor TSynFMXCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := DefaultProposalOptions;
  FShortCut := Ord(' ') or scCtrl;
  FEndOfTokenChr := DefaultEndOfTokenChr;
  FTriggerChars := '.';
  FTimerInterval := 0;
  FActive := False;
  FCompletionStart := 1;

  FItemList := TStringList.Create;
  FInsertList := TStringList.Create;

  FForm := TSynFMXCompletionProposalForm.Create(Self);
  FForm.OnValidate := FormValidate;
  FForm.OnCancel := FormCancel;
  FForm.Stored := False;
end;

destructor TSynFMXCompletionProposal.Destroy;
begin
  CancelCompletion;
  Editor := nil;  // unhook
  FreeAndNil(FTimer);
  FreeAndNil(FForm);
  FInsertList.Free;
  FItemList.Free;
  inherited;
end;

procedure TSynFMXCompletionProposal.SetEditor(Value: TComponent);
var
  Ed: TCustomFMXSynEdit;
begin
  if FEditor = Value then
    Exit;

  // Unhook from old editor
  if FEditor <> nil then
  begin
    FEditor.RemoveFreeNotification(Self);
    if FEditor is TCustomFMXSynEdit then
      TCustomFMXSynEdit(FEditor).RemoveKeyDownHandler(EditorKeyDown);
  end;

  FEditor := Value;

  // Hook into new editor
  if FEditor <> nil then
  begin
    FEditor.FreeNotification(Self);
    if FEditor is TCustomFMXSynEdit then
    begin
      Ed := TCustomFMXSynEdit(FEditor);
      Ed.AddKeyDownHandler(EditorKeyDown);
      if FForm <> nil then
      begin
        FForm.Parent := Ed;
        FForm.IsOpen := False;
      end;
    end;
  end
  else if FForm <> nil then
    FForm.Parent := nil;
end;

procedure TSynFMXCompletionProposal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FEditor then
    begin
      CancelCompletion;
      FEditor := nil;
      if FForm <> nil then
        FForm.Parent := nil;
    end
    else if AComponent = FForm then
      FForm := nil;
  end;
end;

procedure TSynFMXCompletionProposal.TimerExecute(Sender: TObject);
begin
  if Assigned(FTimer) then
    FTimer.Enabled := False;
  Activate;
end;

function TSynFMXCompletionProposal.GetCurrentInput: string;
var
  Ed: TCustomFMXSynEdit;
  Line: string;
  I: Integer;
begin
  Result := '';
  if not (FEditor is TCustomFMXSynEdit) then
    Exit;

  Ed := TCustomFMXSynEdit(FEditor);
  if (Ed.CaretY < 1) or (Ed.CaretY > Ed.Lines.Count) then
    Exit;

  Line := Ed.Lines[Ed.CaretY - 1];
  I := Ed.CaretX - 1;

  while (I > 0) and (I <= Length(Line)) and (Line[I] > #32) and
    not FForm.IsWordBreakChar(Line[I]) do
    Dec(I);

  FCompletionStart := I + 1;
  Result := Copy(Line, I + 1, Ed.CaretX - I - 1);
end;

procedure TSynFMXCompletionProposal.EditorKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
var
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
  Ed: TCustomFMXSynEdit;
  C: WideChar;
begin
  if not (FEditor is TCustomFMXSynEdit) then
    Exit;
  Ed := TCustomFMXSynEdit(FEditor);

  if FForm.IsOpen then
  begin
    // The popup is active -- intercept keys for the completion list.
    // Keys that we consume are zeroed out so the editor does not process them.
    // Keys that must reach the editor (Backspace, Left, Right, character input,
    // Delete) are left intact so the editor's own KeyDown processes them, and
    // we only update our internal state here.
    case Key of
      SYNEDIT_UP:
        begin
          if ssCtrl in Shift then
            FForm.Position := 0
          else
            FForm.MoveLine(-1);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_DOWN:
        begin
          if ssCtrl in Shift then
            FForm.Position := Max(0, FForm.AssignedList.Count - 1)
          else
            FForm.MoveLine(1);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_PRIOR:
        begin
          FForm.MoveLine(-FForm.FLinesInWindow);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_NEXT:
        begin
          FForm.MoveLine(FForm.FLinesInWindow);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_HOME:
        begin
          FForm.Position := 0;
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_END:
        begin
          FForm.Position := Max(0, FForm.AssignedList.Count - 1);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_RETURN:
        begin
          if FForm.CompleteWithEnter and Assigned(FForm.OnValidate) then
            FForm.OnValidate(FForm, Shift, #0);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_TAB:
        begin
          if FForm.CompleteWithTab and Assigned(FForm.OnValidate) then
            FForm.OnValidate(FForm, Shift, #0);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_ESCAPE:
        begin
          if Assigned(FForm.OnCancel) then
            FForm.OnCancel(FForm);
          Key := 0;
          KeyChar := #0;
        end;
      SYNEDIT_BACK:
        begin
          // Let the editor handle backspace (key passes through).
          // Update our tracked string.
          if Length(FForm.CurrentString) > 0 then
            FForm.CurrentString := Copy(FForm.CurrentString, 1,
              Length(FForm.CurrentString) - 1)
          else
          begin
            if Assigned(FForm.OnCancel) then
              FForm.OnCancel(FForm);
          end;
          // Do NOT zero Key -- let editor process the backspace.
        end;
      SYNEDIT_LEFT:
        begin
          if Shift = [] then
          begin
            if Length(FForm.CurrentString) > 0 then
              FForm.CurrentString := Copy(FForm.CurrentString, 1,
                Length(FForm.CurrentString) - 1)
            else
            begin
              if Assigned(FForm.OnCancel) then
                FForm.OnCancel(FForm);
            end;
            // Do NOT zero Key -- let editor move the caret.
          end;
        end;
      SYNEDIT_RIGHT:
        begin
          if Shift = [] then
          begin
            if (Ed.CaretY >= 1) and (Ed.CaretY <= Ed.Lines.Count) and
              (Ed.CaretX <= Ed.Lines[Ed.CaretY - 1].Length) then
            begin
              C := Ed.Lines[Ed.CaretY - 1][Ed.CaretX];
              if FForm.IsWordBreakChar(C) then
              begin
                if Assigned(FForm.OnCancel) then
                  FForm.OnCancel(FForm);
              end
              else
                FForm.CurrentString := FForm.CurrentString + C;
            end;
            // Do NOT zero Key -- let editor move the caret.
          end;
        end;
      SYNEDIT_DELETE:
        begin
          // Let editor handle delete (key passes through).
        end;
    else
      // Character input while popup is open
      if (KeyChar >= #32) and (Shift * [ssCtrl, ssAlt] = []) then
      begin
        if FForm.IsWordBreakChar(KeyChar) then
        begin
          // End-of-token character: validate if option set, then
          // let the character reach the editor.
          if (scoEndCharCompletion in FOptions) and
            Assigned(FForm.OnValidate) then
            FForm.OnValidate(FForm, Shift, KeyChar);
        end
        else
        begin
          // Regular identifier character: update the filter string.
          // The character will reach the editor's own KeyDown handler.
          FForm.CurrentString := FForm.CurrentString + KeyChar;
        end;
        // Do NOT zero KeyChar -- let editor insert the character.
      end;
    end;
  end
  else
  begin
    // Popup is not open -- check for shortcut to activate
    ShortCutKey := FShortCut and not (scShift or scCtrl or scAlt);
    ShortCutShift := [];
    if FShortCut and scShift <> 0 then Include(ShortCutShift, ssShift);
    if FShortCut and scCtrl <> 0 then Include(ShortCutShift, ssCtrl);
    if FShortCut and scAlt <> 0 then Include(ShortCutShift, ssAlt);
    if (Key = ShortCutKey) and (Shift = ShortCutShift) then
    begin
      Key := 0;
      KeyChar := #0;
      Activate;
    end
    else if (KeyChar >= #32) and (Shift * [ssCtrl, ssAlt] = []) then
    begin
      // Check trigger characters for timer-based activation
      if (FTimerInterval > 0) and (Pos(KeyChar, FTriggerChars) > 0) then
      begin
        if FTimer = nil then
        begin
          FTimer := TTimer.Create(Self);
          FTimer.OnTimer := TimerExecute;
        end;
        FTimer.Interval := FTimerInterval;
        FTimer.Enabled := True;
      end;
    end;
  end;
end;

procedure TSynFMXCompletionProposal.FormValidate(Sender: TObject;
  Shift: TShiftState; EndToken: WideChar);
var
  Ed: TCustomFMXSynEdit;
  Value: string;
  Index: Integer;
  Line: string;
  LineIdx: Integer;
begin
  if not (FEditor is TCustomFMXSynEdit) then
    Exit;
  Ed := TCustomFMXSynEdit(FEditor);

  Ed.BeginUpdate;
  try
    Index := FForm.FPosition;

    // Determine what text to insert
    if (scoUseInsertList in FOptions) then
    begin
      if (scoLimitToMatchedText in FOptions) then
      begin
        if (Index >= 0) and (Index < FForm.FilteredInsertList.Count) and
          ((scoEndCharCompletion in FOptions) or (EndToken = #0)) then
          Value := FForm.FilteredInsertList[Index]
        else
          Value := FForm.CurrentString;
      end
      else
      begin
        if (Index >= 0) and (Index < FForm.InsertList.Count) and
          ((scoEndCharCompletion in FOptions) or (EndToken = #0)) then
          Value := FForm.InsertList[Index]
        else
          Value := FForm.CurrentString;
      end;
    end
    else
    begin
      if (Index >= 0) and (Index < FForm.AssignedList.Count) and
        ((scoEndCharCompletion in FOptions) or (EndToken = #0)) then
        Value := FForm.AssignedList[Index]
      else
        Value := FForm.CurrentString;
    end;

    // Fire OnCodeCompletion to allow modification
    if Assigned(FOnCodeCompletion) then
      FOnCodeCompletion(Self, Value, Shift, Index, EndToken);

    // Replace the text from CompletionStart to CaretX with the selected value
    // by directly manipulating the line text.
    LineIdx := Ed.CaretY - 1;
    if (LineIdx >= 0) and (LineIdx < Ed.Lines.Count) then
    begin
      Line := Ed.Lines[LineIdx];
      // Remove old text (from CompletionStart to CaretX-1) and insert new value
      Delete(Line, FCompletionStart, Ed.CaretX - FCompletionStart);
      Insert(Value, Line, FCompletionStart);
      Ed.Lines[LineIdx] := Line;
      // Place caret after the inserted text
      Ed.CaretXY := BufferCoord(FCompletionStart + Length(Value), Ed.CaretY);
    end;

    // Close popup
    CancelCompletion;

    // Fire after-completion event
    if Assigned(FOnAfterCodeCompletion) then
      FOnAfterCodeCompletion(Self, Value, Shift, Index, EndToken);
  finally
    Ed.EndUpdate;
  end;
end;

procedure TSynFMXCompletionProposal.FormCancel(Sender: TObject);
begin
  CancelCompletion;
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TSynFMXCompletionProposal.Execute(const ACurrentInput: string;
  X, Y: Integer);
var
  CanExecute: Boolean;
  Kind: TSynCompletionType;
  CurrentInput: string;
  PosX, PosY: Integer;
begin
  CurrentInput := ACurrentInput;
  PosX := X;
  PosY := Y;
  CanExecute := True;
  Kind := ctCode;

  // Fire OnExecute to allow the host to populate item lists or cancel
  if Assigned(FOnExecute) then
    FOnExecute(Kind, Self, CurrentInput, PosX, PosY, CanExecute);

  if not CanExecute then
    Exit;

  // Sync option flags to form
  FForm.CaseSensitive := scoCaseSensitive in FOptions;
  FForm.UseInsertList := scoUseInsertList in FOptions;
  FForm.MatchText := scoLimitToMatchedText in FOptions;
  FForm.CompleteWithTab := scoCompleteWithTab in FOptions;
  FForm.CompleteWithEnter := scoCompleteWithEnter in FOptions;
  FForm.EndOfTokenChr := FEndOfTokenChr;

  // Copy items to the form
  FForm.FItemList.Assign(FItemList);
  FForm.FInsertList.Assign(FInsertList);

  // Set popup position
  FForm.PlacementRectangle :=
    TBounds.Create(RectF(PosX, PosY, PosX + FForm.Width, PosY + FForm.Height));

  // Apply current input to filter items
  FForm.CurrentString := CurrentInput;

  // Show the popup
  FForm.IsOpen := True;
  FActive := True;
end;

procedure TSynFMXCompletionProposal.Activate;
var
  Ed: TCustomFMXSynEdit;
  Pt: TPointF;
  CurrentInput: string;
  X, Y: Integer;
begin
  if not (FEditor is TCustomFMXSynEdit) then
    Exit;

  Ed := TCustomFMXSynEdit(FEditor);
  if Ed.ReadOnly then
    Exit;

  // Get the current word fragment at the caret
  CurrentInput := GetCurrentInput;

  // BufferCoordToPixel returns editor-local coordinates.
  // Add one line height so the popup appears below the caret line,
  // then convert to screen coordinates (Placement = Absolute).
  Pt := Ed.BufferCoordToPixel(Ed.CaretXY);
  Pt.Y := Pt.Y + Ed.LineHeight;
  Pt := Ed.LocalToScreen(Pt);

  X := Round(Pt.X);
  Y := Round(Pt.Y);

  Execute(CurrentInput, X, Y);
end;

procedure TSynFMXCompletionProposal.Deactivate;
begin
  FActive := False;
  if (FForm <> nil) and FForm.IsOpen then
    FForm.IsOpen := False;
  if Assigned(FTimer) then
    FTimer.Enabled := False;
end;

procedure TSynFMXCompletionProposal.CancelCompletion;
begin
  Deactivate;
end;

function TSynFMXCompletionProposal.IsActive: Boolean;
begin
  Result := FActive and (FForm <> nil) and FForm.IsOpen;
end;

end.
