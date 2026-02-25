{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition
-------------------------------------------------------------------------------}

unit FMX.SynEditReg;

{$I SynEdit.inc}

interface

procedure Register;

implementation

uses
  System.Classes,
  FMX.SynEdit,
  FMX.SynCompletionProposal,
  FMX.SynSpellCheck,
  FMX.SynEditPrint,
  SynEditHighlighter,
  SynEditStrConst;

procedure Register;
begin
  RegisterComponents('SynEdit FMX', [TFMXSynEdit,
    TSynFMXCompletionProposal, TSynFMXSpellCheck, TSynFMXEditPrint]);
end;

end.
