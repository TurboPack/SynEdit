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
  FMX.SynMacroRecorder,
  SynEditHighlighter,
  SynEditStrConst;

procedure Register;
begin
  RegisterComponents('SynEdit FMX', [TFMXSynEdit,
    TSynFMXCompletionProposal, TSynFMXSpellCheck, TSynFMXEditPrint,
    TFMXSynMacroRecorder]);
end;

end.
