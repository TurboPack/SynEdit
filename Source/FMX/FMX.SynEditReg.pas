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
  SynEditHighlighter,
  SynEditStrConst;

procedure Register;
begin
  RegisterComponents('SynEdit FMX', [TFMXSynEdit]);
end;

end.
