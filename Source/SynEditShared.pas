unit SynEditShared;

interface

uses
  SynEditTextBuffer;

type
  ISynEdit = interface(IInterface)
    ['{D287B3BE-B7B3-413F-9545-D43FC5B57A68}']
    function GetLines: TSynEditStringList;
    procedure SetLines(const AValue: TSynEditStringList);
    property Lines: TSynEditStringList read GetLines write SetLines;
  end;

implementation

end.
