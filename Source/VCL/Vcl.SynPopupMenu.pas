unit SynPopupMenu;

interface

uses
  Classes, Menus;

type
  TSynDefaultPopupMenu = (dpmDisabled, dpmBefore, dpmAfter);

  TSynMenuEntry = (meNone, meUndo, meRedo, meCut, meCopy, mePaste,
                meDelete, meSelectAll);

  TSynPopupMenu = class(TPopupMenu)
  private
    FDefaultPopupMenu: TSynDefaultPopupMenu;
    FDefaultItems: array[TSynMenuEntry] of TMenuItem;
    FFilled: Boolean;
    procedure FillDefaultMenu;
    function GetDefaultItem(AEntry: TSynMenuEntry): TMenuItem;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup(X, Y: Integer); override;
    property DefaultItems[AEntry: TSynMenuEntry]: TMenuItem read GetDefaultItem;
  published
    property DefaultPopupMenu: TSynDefaultPopupMenu read
      FDefaultPopupMenu write FDefaultPopupMenu default dpmBefore;
  end;

implementation

uses
  Windows, ActnList, StdActns, SynEdit, SynEditMiscClasses;

{ TSynPopupMenu }

constructor TSynPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultPopupMenu := dpmBefore;
  if not (csLoading in ComponentState) then
    FillDefaultMenu;
end;

procedure TSynPopupMenu.Loaded;
begin
  inherited;
  FillDefaultMenu;
end;

function TSynPopupMenu.GetDefaultItem(AEntry: TSynMenuEntry): TMenuItem;
begin
  Result := FDefaultItems[AEntry];
end;

procedure TSynPopupMenu.FillDefaultMenu;
var
  i: Integer;

  procedure InsertItem(Item: TMenuItem);
  begin
    if FDefaultPopupMenu = dpmAfter then
      Items.Add(Item)
    else
      Items.Insert(i, Item);
    Inc(i);
  end;

  procedure AddSeparator;
  var
    Item: TMenuItem;
  begin
    Item := TMenuItem.Create(Self);
    Item.Caption := '-';
    InsertItem(Item);
  end;

  procedure AddAction(ActionClass: TBasicActionClass; Entry: TSynMenuEntry;
    const ACaption, AHint: string; AShortCut: TShortCut);
  var
    Action: TCustomAction;
    Item: TMenuItem;
  begin
    Action := TCustomAction(ActionClass.Create(Self));
    Action.Caption := ACaption;
    Action.Hint := AHint;
    Action.ShortCut := AShortCut;
    Item := TMenuItem.Create(Self);
    Item.Action := Action;
    FDefaultItems[Entry] := Item;
    InsertItem(Item);
  end;

var
  FEmpty: Boolean;
begin
  if FFilled or (FDefaultPopupMenu = dpmDisabled) or (csDesigning in ComponentState) then
    Exit;
  FFilled := True;
  i := 0;
  FEmpty := Items.Count = 0;
  if not FEmpty and (FDefaultPopupMenu = dpmAfter) then
    AddSeparator;
  AddAction(TEditUndo, meUndo, '&Undo',
    'Undo|Reverts the last action', ShortCut(Ord('Z'), [ssCtrl]));
  AddAction(TSynEditRedo, meRedo, '&Redo',
    'Redo|Redo last action', ShortCut(Ord('Z'), [ssCtrl, ssShift]));
  AddSeparator;
  AddAction(TEditCut, meCut, 'Cu&t',
    'Cut|Cuts the selection and puts it on the Clipboard', ShortCut(Ord('X'), [ssCtrl]));
  AddAction(TEditCopy, meCopy, '&Copy',
    'Copy|Copies the selection and puts it on the Clipboard', ShortCut(Ord('C'), [ssCtrl]));
  AddAction(TEditPaste, mePaste, '&Paste',
    'Paste|Inserts Clipboard contents', ShortCut(Ord('V'), [ssCtrl]));
  AddSeparator;
  AddAction(TEditDelete, meDelete, '&Delete',
    'Delete|Erases the selection', ShortCut(VK_DELETE, []));
  AddAction(TEditSelectAll, meSelectAll, 'Select &All',
    'Select All|Selects the entire document', ShortCut(Ord('A'), [ssCtrl]));
  if not FEmpty and (FDefaultPopupMenu = dpmBefore) then
    AddSeparator;
end;

procedure TSynPopupMenu.Popup(X, Y: Integer);
begin
  if PopupComponent is TCustomSynEdit then
    FillDefaultMenu;
  inherited;
end;

end.

