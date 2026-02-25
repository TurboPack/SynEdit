{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition
-------------------------------------------------------------------------------}

unit FMX.SynEditUndo;

{$I SynEdit.inc}

interface

uses
  FMX.SynEdit,
  SynEditTypes,
  SynEditKeyCmds;

{ Factory Method }
function CreateSynEditUndo(Editor: TCustomFMXSynEdit): ISynEditUndo;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,
  SynEditMiscProcs,
  SynEditMiscClasses,
  SynEditTextBuffer;

// TODO: Port undo implementation from VCL version
// The undo logic is platform-independent; only the Editor type reference differs

function CreateSynEditUndo(Editor: TCustomFMXSynEdit): ISynEditUndo;
begin
  Result := nil; // TODO: Implement
end;

end.
