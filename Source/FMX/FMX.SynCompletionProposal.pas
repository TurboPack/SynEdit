{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition
-------------------------------------------------------------------------------}

unit FMX.SynCompletionProposal;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditHighlighter;

type
  { FMX Completion Proposal - uses TPopup instead of VCL custom form }
  TSynFMXCompletionProposal = class(TComponent)
  private
    // TODO: Implement using FMX TPopup
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TSynFMXCompletionProposal.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: Implement
end;

destructor TSynFMXCompletionProposal.Destroy;
begin
  // TODO: Cleanup
  inherited;
end;

end.
