unit TestSynEditStringList;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestSynEditStringList = class(TObject)
  strict private
    procedure InternalDeleteDuplicates(const AMode: Integer);
  public
    [Test]
    procedure TestDeleteDuplicates;
  end;

implementation

uses
  System.Classes, System.SysUtils, SynEditTextBuffer;

{ TTestSynEditStringList }

procedure TTestSynEditStringList.InternalDeleteDuplicates(const AMode: Integer);
var
  lCount: Integer;
  lExpected: string;
  //lReferences: Integer;
  lLast: string;
  lSaved: TStringList;
  lSnapshot: TArray<string>;
  lSource: TSynEditStringList;
begin
  lSaved := nil;
  lSource := TSynEditStringList.Create(nil);
  try
    lSaved := TStringList.Create;
    lLast := 'dynamic final row ' + AMode.ToString;
    for lCount := 0 to 4 do
      lSource.Add('dynamic row ' + lCount.ToString);
    lSource.Add(lLast);
    case AMode of
      0: lSource.Delete(1);
      1: lSource.DeleteLines(1, 2);
      2: lSource.Delete(0);
      3: lSource.DeleteLines(0, 3);
      4:
      begin
        lSource.Delete(1);
        lSource.Delete(1);
      end;
    end;
    lSnapshot := lSource.ToStringArray;
    lSaved.AddStrings(lSnapshot);
    lSnapshot := nil;
    lExpected := lSaved.Text;
    // Delphi UnicodeString's reference count is eight bytes before its data.
    //lReferences := PInteger(NativeUInt(Pointer(lLast)) - 8)^;
    //Assert.IsTrue(lReferences = 2, Format('Case %d: expected Last + Saved references=2, got %d', [AMode, lReferences]));
    Assert.IsTrue(lSaved.Text = lExpected, 'Destroying editor text changed saved text');
    Assert.IsTrue(lSaved[lSaved.Count - 1] = lLast, 'Final row changed');
  finally
    lSource.Free;
    lSaved.Free;
  end;
end;

procedure TTestSynEditStringList.TestDeleteDuplicates;
var
  lMode: Integer;
begin
  for lMode := 0 to 4 do
    InternalDeleteDuplicates(lMode);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestSynEditStringList);

end.

