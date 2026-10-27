program Project1;

uses
  DUnitX.Loggers.GUI.VCL,
  TestSynEditStringList in 'TestSynEditStringList.pas';
begin
  ReportMemoryLeaksOnShutdown := True;
  DUnitX.Loggers.GUI.VCL.Run;
end.

