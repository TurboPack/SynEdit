{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition
-------------------------------------------------------------------------------}

unit FMX.SynEditScrollBars;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.Types,
  System.UITypes,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.SynEditTypes,
  SynEditTypes;

{ Factory method }
function CreateSynEditScrollBars(Editor: TControl): ISynEditScrollBars;

implementation

type
  TSynFMXScrollBars = class(TInterfacedObject, ISynEditScrollBars)
  private
    FEditor: TControl;
    FIsScrolling: Boolean;
  public
    constructor Create(AEditor: TControl);
    function UpdateScrollBars: Boolean;
    function GetIsScrolling: Boolean;
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPointF);
  end;

function CreateSynEditScrollBars(Editor: TControl): ISynEditScrollBars;
begin
  Result := TSynFMXScrollBars.Create(Editor);
end;

{ TSynFMXScrollBars }

constructor TSynFMXScrollBars.Create(AEditor: TControl);
begin
  inherited Create;
  FEditor := AEditor;
end;

function TSynFMXScrollBars.UpdateScrollBars: Boolean;
begin
  Result := False;
  // TODO: Implement FMX scrollbar update
end;

function TSynFMXScrollBars.GetIsScrolling: Boolean;
begin
  Result := FIsScrolling;
end;

procedure TSynFMXScrollBars.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPointF);
begin
  // TODO: Implement FMX mouse wheel handling
end;

end.
