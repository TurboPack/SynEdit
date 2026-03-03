unit TestFMXSynEditGutter;

interface

uses
  DUnitX.TestFramework,
  FMX.SynEdit;

type
  [TestFixture]
  TTestFMXSynEditGutter = class
  private
    FEditor: TFMXSynEdit;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestDefaultBandCount;
    [Test]
    procedure TestDefaultBandOrder;
    [Test]
    procedure TestDefaultBandVisibility;
    [Test]
    procedure TestGutterWidthPositive;
    [Test]
    procedure TestGutterWidthWithFolding;
    [Test]
    procedure TestGutterWidthWithoutFolding;
    [Test]
    procedure TestLineNumberWidthAutoSizes;
    [Test]
    procedure TestBandByKind;
    [Test]
    procedure TestBandByKindNil;
    [Test]
    procedure TestBandAtX;
    [Test]
    procedure TestBandAtXPastGutter;
    [Test]
    procedure TestHideBandReducesWidth;
    [Test]
    procedure TestGutterInvisibleZeroWidth;
    [Test]
    procedure TestCustomBand;
    [Test]
    procedure TestFoldBandVisibilitySync;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  SynEditTypes,
  FMX.SynEditMiscClasses,
  SynHighlighterPas;

const
  SampleText =
    'program Test;' + sLineBreak +
    'begin' + sLineBreak +
    '  WriteLn;' + sLineBreak +
    'end.';

procedure TTestFMXSynEditGutter.Setup;
begin
  FEditor := TFMXSynEdit.Create(nil);
  FEditor.Text := SampleText;
end;

procedure TTestFMXSynEditGutter.TearDown;
begin
  FEditor.Free;
end;

procedure TTestFMXSynEditGutter.TestDefaultBandCount;
begin
  Assert.AreEqual(4, FEditor.Gutter.Bands.Count);
end;

procedure TTestFMXSynEditGutter.TestDefaultBandOrder;
begin
  Assert.AreEqual(Ord(gbkLineNumbers), Ord(FEditor.Gutter.Bands[0].Kind));
  Assert.AreEqual(Ord(gbkMarks), Ord(FEditor.Gutter.Bands[1].Kind));
  Assert.AreEqual(Ord(gbkFold), Ord(FEditor.Gutter.Bands[2].Kind));
  Assert.AreEqual(Ord(gbkMargin), Ord(FEditor.Gutter.Bands[3].Kind));
end;

procedure TTestFMXSynEditGutter.TestDefaultBandVisibility;
begin
  Assert.IsTrue(FEditor.Gutter.Bands[0].Visible, 'LineNumbers visible');
  Assert.IsTrue(FEditor.Gutter.Bands[1].Visible, 'Marks visible');
  Assert.IsFalse(FEditor.Gutter.Bands[2].Visible, 'Fold hidden');
  Assert.IsTrue(FEditor.Gutter.Bands[3].Visible, 'Margin visible');
end;

procedure TTestFMXSynEditGutter.TestGutterWidthPositive;
begin
  Assert.IsTrue(FEditor.GutterWidth > 0, 'GutterWidth should be > 0');
end;

procedure TTestFMXSynEditGutter.TestGutterWidthWithFolding;
var
  WidthBefore, WidthAfter: Single;
  HL: TSynPasSyn;
begin
  WidthBefore := FEditor.GutterWidth;
  HL := TSynPasSyn.Create(nil);
  try
    FEditor.Highlighter := HL;
    FEditor.UseCodeFolding := True;
    WidthAfter := FEditor.GutterWidth;
    Assert.IsTrue(WidthAfter > WidthBefore, 'Width should increase with folding');
  finally
    FEditor.UseCodeFolding := False;
    FEditor.Highlighter := nil;
    HL.Free;
  end;
end;

procedure TTestFMXSynEditGutter.TestGutterWidthWithoutFolding;
var
  FoldBand: TSynFMXGutterBand;
begin
  FoldBand := FEditor.Gutter.Bands.BandByKind(gbkFold);
  Assert.IsNotNull(FoldBand);
  Assert.IsFalse(FoldBand.Visible, 'Fold band should be hidden');
  Assert.IsTrue(FoldBand.RealWidth = 0, 'Fold band width should be 0');
end;

procedure TTestFMXSynEditGutter.TestLineNumberWidthAutoSizes;
var
  WidthSmall, WidthLarge: Single;
  I: Integer;
  Band: TSynFMXGutterBand;
begin
  Band := FEditor.Gutter.Bands.BandByKind(gbkLineNumbers);
  WidthSmall := Band.RealWidth;
  // Add lines to get past 4 digits
  FEditor.BeginUpdate;
  try
    for I := 1 to 10000 do
      FEditor.Lines.Add('Line ' + IntToStr(I));
  finally
    FEditor.EndUpdate;
  end;
  WidthLarge := Band.RealWidth;
  Assert.IsTrue(WidthLarge > WidthSmall,
    'Line number band should widen with more lines');
end;

procedure TTestFMXSynEditGutter.TestBandByKind;
var
  Band: TSynFMXGutterBand;
begin
  Band := FEditor.Gutter.Bands.BandByKind(gbkMarks);
  Assert.IsNotNull(Band);
  Assert.AreEqual(Ord(gbkMarks), Ord(Band.Kind));
end;

procedure TTestFMXSynEditGutter.TestBandByKindNil;
begin
  // Remove the custom band kind - gbkCustom is not in defaults
  Assert.IsNull(FEditor.Gutter.Bands.BandByKind(gbkCustom));
end;

procedure TTestFMXSynEditGutter.TestBandAtX;
var
  Band: TSynFMXGutterBand;
begin
  // X=0 should be in the first band (LineNumbers)
  Band := FEditor.Gutter.BandAtX(0);
  Assert.IsNotNull(Band);
  Assert.AreEqual(Ord(gbkLineNumbers), Ord(Band.Kind));
end;

procedure TTestFMXSynEditGutter.TestBandAtXPastGutter;
var
  Band: TSynFMXGutterBand;
begin
  Band := FEditor.Gutter.BandAtX(FEditor.GutterWidth + 100);
  Assert.IsNull(Band, 'Should return nil past gutter edge');
end;

procedure TTestFMXSynEditGutter.TestHideBandReducesWidth;
var
  WidthBefore, WidthAfter: Single;
begin
  WidthBefore := FEditor.GutterWidth;
  FEditor.Gutter.Bands.BandByKind(gbkMarks).Visible := False;
  WidthAfter := FEditor.GutterWidth;
  Assert.IsTrue(WidthAfter < WidthBefore,
    'Hiding marks band should reduce gutter width');
  // Restore
  FEditor.Gutter.Bands.BandByKind(gbkMarks).Visible := True;
end;

procedure TTestFMXSynEditGutter.TestGutterInvisibleZeroWidth;
begin
  FEditor.Gutter.Visible := False;
  Assert.AreEqual(Single(0), FEditor.GutterWidth,
    'Gutter width should be 0 when invisible');
  FEditor.Gutter.Visible := True;
end;

procedure TTestFMXSynEditGutter.TestCustomBand;
var
  WidthBefore, WidthAfter: Single;
begin
  WidthBefore := FEditor.GutterWidth;
  FEditor.Gutter.Bands.Add(gbkCustom, 20, True);
  // Trigger width recalc
  FEditor.Gutter.Changed;
  WidthAfter := FEditor.GutterWidth;
  Assert.AreEqual(5, FEditor.Gutter.Bands.Count, 'Should have 5 bands');
  Assert.IsTrue(WidthAfter > WidthBefore,
    'Custom band should increase gutter width');
end;

procedure TTestFMXSynEditGutter.TestFoldBandVisibilitySync;
var
  FoldBand: TSynFMXGutterBand;
  HL: TSynPasSyn;
begin
  FoldBand := FEditor.Gutter.Bands.BandByKind(gbkFold);
  Assert.IsFalse(FoldBand.Visible, 'Fold band initially hidden');

  HL := TSynPasSyn.Create(nil);
  try
    FEditor.Highlighter := HL;
    FEditor.UseCodeFolding := True;
    Assert.IsTrue(FoldBand.Visible, 'Fold band visible when folding enabled');
    FEditor.UseCodeFolding := False;
    Assert.IsFalse(FoldBand.Visible, 'Fold band hidden when folding disabled');
  finally
    FEditor.Highlighter := nil;
    HL.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestFMXSynEditGutter);

end.
