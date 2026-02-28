unit TestSynHighlighterXMLFolding;

{ Tests for XML highlighter fold range detection.
  Uses only shared units (SynEditCodeFolding, SynHighlighterXML,
  SynEditTextBuffer) — no FMX or VCL dependency. }

interface

uses
  DUnitX.TestFramework,
  SynEditCodeFolding,
  SynHighlighterXML;

type
  [TestFixture]
  TTestXMLFolding = class
  private
    FHighlighter: TSynXMLSyn;
    FFoldRanges: TSynFoldRanges;
    procedure ScanText(const AText: string);
    function FindFoldAtLine(ALine: Integer): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Basic element folding }
    [Test]
    procedure TestSingleElementFolds;
    [Test]
    procedure TestNestedElementsFold;
    [Test]
    procedure TestDeeplyNestedElements;

    { Self-closing elements }
    [Test]
    procedure TestSelfClosingDoesNotFold;
    [Test]
    procedure TestSelfClosingWithSpaceDoesNotFold;

    { Same-line balanced elements }
    [Test]
    procedure TestBalancedOnOneLineDoesNotFold;

    { Processing instructions, comments, CDATA, DOCTYPE }
    [Test]
    procedure TestProcessingInstructionDoesNotFold;
    [Test]
    procedure TestCommentDoesNotFold;
    [Test]
    procedure TestCDATADoesNotFold;
    [Test]
    procedure TestDoctypeDoesNotFold;

    { Multi-line attributes }
    [Test]
    procedure TestMultiLineAttributesFold;

    { Namespaced elements }
    [Test]
    procedure TestNamespacedElementsFold;

    { Full sample source }
    [Test]
    procedure TestFullSampleFoldsCorrectly;

    { Quoted special characters }
    [Test]
    procedure TestQuotedGreaterThanInAttribute;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  SynEditTextBuffer;

procedure TTestXMLFolding.Setup;
begin
  FHighlighter := TSynXMLSyn.Create(nil);
  FFoldRanges := TSynFoldRanges.Create;
end;

procedure TTestXMLFolding.TearDown;
begin
  FFoldRanges.Free;
  FHighlighter.Free;
end;

procedure TTestXMLFolding.ScanText(const AText: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    FFoldRanges.StartScanning;
    FHighlighter.ScanForFoldRanges(FFoldRanges, Lines, 0, Lines.Count - 1);
    FFoldRanges.StopScanning(Lines);
  finally
    Lines.Free;
  end;
end;

function TTestXMLFolding.FindFoldAtLine(ALine: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FFoldRanges.Count - 1 do
    if FFoldRanges[I].FromLine = ALine then
      Exit(I);
  Result := -1;
end;

{ --- Basic element folding --- }

procedure TTestXMLFolding.TestSingleElementFolds;
begin
  ScanText(
    '<root>'            + sLineBreak +   // line 1
    '  content'         + sLineBreak +   // line 2
    '</root>'                             // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'root fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'root fold should close at </root>');
end;

procedure TTestXMLFolding.TestNestedElementsFold;
begin
  ScanText(
    '<outer>'           + sLineBreak +   // line 1
    '  <inner>'         + sLineBreak +   // line 2
    '    text'          + sLineBreak +   // line 3
    '  </inner>'        + sLineBreak +   // line 4
    '</outer>'                            // line 5
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'outer fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'inner fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine);
end;

procedure TTestXMLFolding.TestDeeplyNestedElements;
begin
  ScanText(
    '<a>'               + sLineBreak +   // line 1
    '  <b>'             + sLineBreak +   // line 2
    '    <c>'           + sLineBreak +   // line 3
    '      text'        + sLineBreak +   // line 4
    '    </c>'          + sLineBreak +   // line 5
    '  </b>'            + sLineBreak +   // line 6
    '</a>'                                // line 7
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'a fold should exist');
  Assert.AreEqual(7, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'b fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(2)].ToLine);
  Assert.IsTrue(FindFoldAtLine(3) >= 0, 'c fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(3)].ToLine);
end;

{ --- Self-closing elements --- }

procedure TTestXMLFolding.TestSelfClosingDoesNotFold;
begin
  ScanText(
    '<root>'            + sLineBreak +   // line 1
    '  <item/>'         + sLineBreak +   // line 2
    '  <item value="x"/>' + sLineBreak + // line 3
    '</root>'                             // line 4
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'root fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2), 'self-closing <item/> must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'self-closing <item value="x"/> must not fold');
end;

procedure TTestXMLFolding.TestSelfClosingWithSpaceDoesNotFold;
begin
  ScanText(
    '<root>'            + sLineBreak +   // line 1
    '  <br />'          + sLineBreak +   // line 2
    '</root>'                             // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'root fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'self-closing <br /> must not fold');
end;

{ --- Same-line balanced elements --- }

procedure TTestXMLFolding.TestBalancedOnOneLineDoesNotFold;
begin
  ScanText(
    '<root>'                            + sLineBreak + // line 1
    '  <name>John</name>'               + sLineBreak + // line 2
    '  <age>30</age>'                   + sLineBreak + // line 3
    '</root>'                                           // line 4
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'root fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'balanced <name>...</name> on one line must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'balanced <age>...</age> on one line must not fold');
end;

{ --- Processing instructions, comments, CDATA, DOCTYPE --- }

procedure TTestXMLFolding.TestProcessingInstructionDoesNotFold;
begin
  ScanText(
    '<?xml version="1.0"?>'  + sLineBreak + // line 1
    '<root>'                  + sLineBreak + // line 2
    '</root>'                                // line 3
  );
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'processing instruction must not fold');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'root fold should exist');
end;

procedure TTestXMLFolding.TestCommentDoesNotFold;
begin
  ScanText(
    '<!-- comment -->'    + sLineBreak +   // line 1
    '<root>'              + sLineBreak +   // line 2
    '</root>'                               // line 3
  );
  Assert.AreEqual(-1, FindFoldAtLine(1), 'comment must not fold');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'root fold should exist');
end;

procedure TTestXMLFolding.TestCDATADoesNotFold;
begin
  ScanText(
    '<root>'                            + sLineBreak + // line 1
    '  <![CDATA[ some data ]]>'         + sLineBreak + // line 2
    '</root>'                                           // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'root fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2), 'CDATA must not fold');
end;

procedure TTestXMLFolding.TestDoctypeDoesNotFold;
begin
  ScanText(
    '<!DOCTYPE root>'     + sLineBreak +   // line 1
    '<root>'              + sLineBreak +   // line 2
    '</root>'                               // line 3
  );
  Assert.AreEqual(-1, FindFoldAtLine(1), 'DOCTYPE must not fold');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'root fold should exist');
end;

{ --- Multi-line attributes --- }

procedure TTestXMLFolding.TestMultiLineAttributesFold;
begin
  // Non-void element with attributes spanning two lines should fold
  ScanText(
    '<root>'                              + sLineBreak + // line 1
    '  <connection host="localhost"'      + sLineBreak + // line 2
    '    port="5432">'                    + sLineBreak + // line 3
    '    <param name="x"/>'              + sLineBreak + // line 4
    '  </connection>'                     + sLineBreak + // line 5
    '</root>'                                            // line 6
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'root fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(1)].ToLine);
  // <connection> starts on line 2 (tag opens, no > yet but tag name detected)
  Assert.IsTrue(FindFoldAtLine(2) >= 0,
    'connection fold should exist at line 2');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine);
end;

{ --- Namespaced elements --- }

procedure TTestXMLFolding.TestNamespacedElementsFold;
begin
  ScanText(
    '<soap:Envelope>'     + sLineBreak +   // line 1
    '  <soap:Body>'       + sLineBreak +   // line 2
    '    <data/>'         + sLineBreak +   // line 3
    '  </soap:Body>'      + sLineBreak +   // line 4
    '</soap:Envelope>'                      // line 5
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'Envelope fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Body fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'self-closing <data/> must not fold');
end;

{ --- Full sample source --- }

procedure TTestXMLFolding.TestFullSampleFoldsCorrectly;
var
  SampleSource: string;
  Idx: Integer;
begin
  SampleSource :=
    '<?xml version="1.0" encoding="UTF-8"?>'#13#10 +    // line 1
    '<!-- App configuration -->'#13#10 +                  // line 2
    '<configuration>'#13#10 +                             // line 3
    '  <appSettings>'#13#10 +                             // line 4
    '    <add key="Theme" value="Dark"/>'#13#10 +         // line 5
    '    <add key="Lang" value="en"/>'#13#10 +            // line 6
    '  </appSettings>'#13#10 +                            // line 7
    '  <database>'#13#10 +                                // line 8
    '    <connection host="localhost"'#13#10 +             // line 9
    '      port="5432" name="mydb"/>'#13#10 +             // line 10
    '    <pool min="2" max="10"/>'#13#10 +                // line 11
    '  </database>'#13#10 +                               // line 12
    '  <logging level="info">'#13#10 +                    // line 13
    '    <![CDATA[ %d{yyyy-MM-dd} [%t] %m ]]>'#13#10 +   // line 14
    '    <target type="file">'#13#10 +                    // line 15
    '      <param name="path" value="/var/log"/>'#13#10 + // line 16
    '    </target>'#13#10 +                               // line 17
    '  </logging>'#13#10 +                                // line 18
    '</configuration>';                                   // line 19
  ScanText(SampleSource);

  // <?xml ...?> (line 1) — no fold
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'processing instruction must not fold');

  // <!-- comment --> (line 2) — no fold
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'comment must not fold');

  // <configuration> (line 3) folds to </configuration> (line 19)
  Idx := FindFoldAtLine(3);
  Assert.IsTrue(Idx >= 0, 'configuration fold should exist');
  Assert.AreEqual(19, FFoldRanges[Idx].ToLine);

  // <appSettings> (line 4) folds to </appSettings> (line 7)
  Idx := FindFoldAtLine(4);
  Assert.IsTrue(Idx >= 0, 'appSettings fold should exist');
  Assert.AreEqual(7, FFoldRanges[Idx].ToLine);

  // <add .../> (lines 5, 6) — self-closing, no fold
  Assert.AreEqual(-1, FindFoldAtLine(5), 'self-closing add must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(6), 'self-closing add must not fold');

  // <database> (line 8) folds to </database> (line 12)
  Idx := FindFoldAtLine(8);
  Assert.IsTrue(Idx >= 0, 'database fold should exist');
  Assert.AreEqual(12, FFoldRanges[Idx].ToLine);

  // <logging> (line 13) folds to </logging> (line 18)
  Idx := FindFoldAtLine(13);
  Assert.IsTrue(Idx >= 0, 'logging fold should exist');
  Assert.AreEqual(18, FFoldRanges[Idx].ToLine);

  // <![CDATA[...]]> (line 14) — no fold
  Assert.AreEqual(-1, FindFoldAtLine(14), 'CDATA must not fold');

  // <target> (line 15) folds to </target> (line 17)
  Idx := FindFoldAtLine(15);
  Assert.IsTrue(Idx >= 0, 'target fold should exist');
  Assert.AreEqual(17, FFoldRanges[Idx].ToLine);

  // <param .../> (line 16) — self-closing, no fold
  Assert.AreEqual(-1, FindFoldAtLine(16), 'self-closing param must not fold');
end;

{ --- Quoted special characters --- }

procedure TTestXMLFolding.TestQuotedGreaterThanInAttribute;
begin
  ScanText(
    '<item expr="a &gt; b">' + sLineBreak + // line 1
    '  content'              + sLineBreak + // line 2
    '</item>'                                // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0,
    'item fold should exist despite &gt; in attribute');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestXMLFolding);

end.
