unit TestFMXSynEditHighlighter;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditHighlighter = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestHighlighterNilByDefault;
    [Test]
    procedure TestAssignHighlighter;
    [Test]
    procedure TestHighlighterClearToNil;
    [Test]
    procedure TestHighlighterFreeNotification;
    [Test]
    procedure TestMultipleHighlighterSwitch;
  end;

implementation

uses
  System.SysUtils,
  SynHighlighterDelphi,
  SynHighlighterJSON;

procedure TTestFMXSynEditHighlighter.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
end;

procedure TTestFMXSynEditHighlighter.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditHighlighter.TestHighlighterNilByDefault;
begin
  Assert.IsNull(FEditor.Highlighter);
end;

procedure TTestFMXSynEditHighlighter.TestAssignHighlighter;
var
  HL: TSynDelphiSyn;
begin
  HL := TSynDelphiSyn.Create(nil);
  try
    FEditor.Highlighter := HL;
    Assert.AreSame(HL, FEditor.Highlighter);
  finally
    FEditor.Highlighter := nil;
    HL.Free;
  end;
end;

procedure TTestFMXSynEditHighlighter.TestHighlighterClearToNil;
var
  HL: TSynDelphiSyn;
begin
  HL := TSynDelphiSyn.Create(nil);
  try
    FEditor.Highlighter := HL;
    Assert.IsNotNull(FEditor.Highlighter);
    FEditor.Highlighter := nil;
    Assert.IsNull(FEditor.Highlighter);
  finally
    HL.Free;
  end;
end;

procedure TTestFMXSynEditHighlighter.TestHighlighterFreeNotification;
var
  HL: TSynDelphiSyn;
begin
  HL := TSynDelphiSyn.Create(nil);
  FEditor.Highlighter := HL;
  Assert.IsNotNull(FEditor.Highlighter);
  // Freeing the highlighter should trigger FreeNotification
  HL.Free;
  Assert.IsNull(FEditor.Highlighter,
    'Highlighter property should be nil after highlighter is freed');
end;

procedure TTestFMXSynEditHighlighter.TestMultipleHighlighterSwitch;
var
  HL1: TSynDelphiSyn;
  HL2: TSynJSONSyn;
begin
  HL1 := TSynDelphiSyn.Create(nil);
  HL2 := TSynJSONSyn.Create(nil);
  try
    FEditor.Highlighter := HL1;
    Assert.AreSame(HL1, FEditor.Highlighter);
    FEditor.Highlighter := HL2;
    Assert.AreSame(HL2, FEditor.Highlighter);
    FEditor.Highlighter := nil;
    Assert.IsNull(FEditor.Highlighter);
  finally
    FEditor.Highlighter := nil;
    HL2.Free;
    HL1.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditHighlighter);

end.
