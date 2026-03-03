unit TestFMXSynEditRenderer;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestTColorToAlphaColor = class
  public
    [Test]
    procedure TestBlack;
    [Test]
    procedure TestWhite;
    [Test]
    procedure TestRed;
    [Test]
    procedure TestGreen;
    [Test]
    procedure TestBlue;
    [Test]
    procedure TestMixedColorByteSwap;
    [Test]
    procedure TestSysNoneReturnsNull;
    [Test]
    procedure TestAlphaAlwaysFF;
  end;

  [TestFixture]
  TTestSynFMXRendererMetrics = class
  public
    [Test]
    procedure TestCharWidthPositive;
    [Test]
    procedure TestLineHeightPositive;
    [Test]
    procedure TestSetFontUpdatesMetrics;
  end;

implementation

uses
  System.UITypes,
  System.UIConsts,
  FMX.Graphics,
  FMX.SynEditRenderer;

{ TTestTColorToAlphaColor }

procedure TTestTColorToAlphaColor.TestBlack;
begin
  // TColor black = $00000000 -> TAlphaColor = $FF000000
  Assert.AreEqual(TAlphaColor($FF000000), TColorToAlphaColor(TColor($00000000)),
    'Black should convert to $FF000000');
end;

procedure TTestTColorToAlphaColor.TestWhite;
begin
  // TColor white = $00FFFFFF -> TAlphaColor = $FFFFFFFF
  Assert.AreEqual(TAlphaColor($FFFFFFFF), TColorToAlphaColor(TColor($00FFFFFF)),
    'White should convert to $FFFFFFFF');
end;

procedure TTestTColorToAlphaColor.TestRed;
begin
  // TColor red = $000000FF (BB=00, GG=00, RR=FF) -> TAlphaColor = $FFFF0000
  Assert.AreEqual(TAlphaColor($FFFF0000), TColorToAlphaColor(TColor($000000FF)),
    'Red TColor should become $FFFF0000');
end;

procedure TTestTColorToAlphaColor.TestGreen;
begin
  // TColor green = $0000FF00 (BB=00, GG=FF, RR=00) -> TAlphaColor = $FF00FF00
  Assert.AreEqual(TAlphaColor($FF00FF00), TColorToAlphaColor(TColor($0000FF00)),
    'Green TColor should become $FF00FF00');
end;

procedure TTestTColorToAlphaColor.TestBlue;
begin
  // TColor blue = $00FF0000 (BB=FF, GG=00, RR=00) -> TAlphaColor = $FF0000FF
  Assert.AreEqual(TAlphaColor($FF0000FF), TColorToAlphaColor(TColor($00FF0000)),
    'Blue TColor should become $FF0000FF');
end;

procedure TTestTColorToAlphaColor.TestMixedColorByteSwap;
begin
  // TColor = $00AABBCC (BB=AA, GG=BB, RR=CC) -> TAlphaColor = $FFCCBBAA
  Assert.AreEqual(TAlphaColor($FFCCBBAA), TColorToAlphaColor(TColor($00AABBCC)),
    'Mixed color should byte-swap R and B channels');
end;

procedure TTestTColorToAlphaColor.TestSysNoneReturnsNull;
begin
  Assert.AreEqual(TAlphaColors.Null, TColorToAlphaColor(TColors.SysNone),
    'SysNone should map to TAlphaColors.Null');
end;

procedure TTestTColorToAlphaColor.TestAlphaAlwaysFF;
var
  Result: TAlphaColor;
begin
  // For any non-SysNone, non-system color, alpha byte should be $FF
  Result := TColorToAlphaColor(TColor($00123456));
  Assert.AreEqual(Byte($FF), Byte(Result shr 24),
    'Alpha channel should always be $FF for regular colors');
end;

{ TTestSynFMXRendererMetrics }

procedure TTestSynFMXRendererMetrics.TestCharWidthPositive;
var
  Renderer: TSynFMXRenderer;
begin
  Renderer := TSynFMXRenderer.Create;
  try
    Assert.IsTrue(Renderer.CharWidth > 0,
      'CharWidth should be positive after creation');
  finally
    Renderer.Free;
  end;
end;

procedure TTestSynFMXRendererMetrics.TestLineHeightPositive;
var
  Renderer: TSynFMXRenderer;
begin
  Renderer := TSynFMXRenderer.Create;
  try
    Assert.IsTrue(Renderer.LineHeight > 0,
      'LineHeight should be positive after creation');
  finally
    Renderer.Free;
  end;
end;

procedure TTestSynFMXRendererMetrics.TestSetFontUpdatesMetrics;
var
  Renderer: TSynFMXRenderer;
  OldCharWidth, OldLineHeight: Single;
  NewFont: TFont;
begin
  Renderer := TSynFMXRenderer.Create;
  try
    OldCharWidth := Renderer.CharWidth;
    OldLineHeight := Renderer.LineHeight;
    NewFont := TFont.Create;
    try
      NewFont.Family := 'Consolas';
      NewFont.Size := 24; // significantly different from default 10
      Renderer.SetFont(NewFont);
    finally
      NewFont.Free;
    end;
    // Larger font should produce larger metrics
    Assert.IsTrue(Renderer.CharWidth > OldCharWidth,
      'CharWidth should increase with larger font');
    Assert.IsTrue(Renderer.LineHeight > OldLineHeight,
      'LineHeight should increase with larger font');
  finally
    Renderer.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestTColorToAlphaColor);
  TDUnitX.RegisterTestFixture(TTestSynFMXRendererMetrics);

end.
