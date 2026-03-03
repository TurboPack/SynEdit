unit TestSynHighlighterCSSFolding;

{ Tests for CSS highlighter fold range detection.
  Uses only shared units (SynEditCodeFolding, SynHighlighterCss,
  SynEditTextBuffer) — no FMX or VCL dependency. }

interface

uses
  DUnitX.TestFramework,
  SynEditCodeFolding,
  SynHighlighterCss;

type
  [TestFixture]
  TTestCSSFolding = class
  private
    FHighlighter: TSynCssSyn;
    FFoldRanges: TSynFoldRanges;
    procedure ScanText(const AText: string);
    function FindFoldAtLine(ALine: Integer): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Basic rule block folding }
    [Test]
    procedure TestSingleRuleFolds;
    [Test]
    procedure TestMultipleRulesFold;

    { Nested blocks }
    [Test]
    procedure TestNestedMediaQueryFolds;
    [Test]
    procedure TestDeeplyNestedBlocks;

    { Same-line balanced braces }
    [Test]
    procedure TestSameLineBracesDoNotFold;

    { Comments }
    [Test]
    procedure TestCommentDoesNotFold;
    [Test]
    procedure TestMultiLineCommentDoesNotFold;
    [Test]
    procedure TestBracesInCommentIgnored;

    { Strings }
    [Test]
    procedure TestBracesInDoubleQuoteStringIgnored;
    [Test]
    procedure TestBracesInSingleQuoteStringIgnored;
    [Test]
    procedure TestEscapedQuoteInString;

    { Edge cases }
    [Test]
    procedure TestEmptyRuleBlock;

    { Full sample }
    [Test]
    procedure TestFullSampleFoldsCorrectly;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  SynEditTextBuffer;

procedure TTestCSSFolding.Setup;
begin
  FHighlighter := TSynCssSyn.Create(nil);
  FFoldRanges := TSynFoldRanges.Create;
end;

procedure TTestCSSFolding.TearDown;
begin
  FFoldRanges.Free;
  FHighlighter.Free;
end;

procedure TTestCSSFolding.ScanText(const AText: string);
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

function TTestCSSFolding.FindFoldAtLine(ALine: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FFoldRanges.Count - 1 do
    if FFoldRanges[I].FromLine = ALine then
      Exit(I);
  Result := -1;
end;

{ --- Basic rule block folding --- }

procedure TTestCSSFolding.TestSingleRuleFolds;
begin
  ScanText(
    'body {'              + sLineBreak +   // line 1
    '  color: red;'       + sLineBreak +   // line 2
    '}'                                     // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'body fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'body fold should close at }');
end;

procedure TTestCSSFolding.TestMultipleRulesFold;
begin
  ScanText(
    'body {'              + sLineBreak +   // line 1
    '  color: red;'       + sLineBreak +   // line 2
    '}'                   + sLineBreak +   // line 3
    'h1 {'                + sLineBreak +   // line 4
    '  font-size: 18px;'  + sLineBreak +   // line 5
    '}'                                     // line 6
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'body fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.IsTrue(FindFoldAtLine(4) >= 0, 'h1 fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(4)].ToLine);
end;

{ --- Nested blocks --- }

procedure TTestCSSFolding.TestNestedMediaQueryFolds;
begin
  ScanText(
    '@media screen {'       + sLineBreak +   // line 1
    '  body {'              + sLineBreak +   // line 2
    '    color: red;'       + sLineBreak +   // line 3
    '  }'                   + sLineBreak +   // line 4
    '}'                                       // line 5
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, '@media fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'body fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine);
end;

procedure TTestCSSFolding.TestDeeplyNestedBlocks;
begin
  ScanText(
    '@supports (display: grid) {'  + sLineBreak +  // line 1
    '  @media screen {'            + sLineBreak +  // line 2
    '    .container {'             + sLineBreak +  // line 3
    '      display: grid;'         + sLineBreak +  // line 4
    '    }'                        + sLineBreak +  // line 5
    '  }'                          + sLineBreak +  // line 6
    '}'                                             // line 7
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, '@supports fold should exist');
  Assert.AreEqual(7, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.IsTrue(FindFoldAtLine(2) >= 0, '@media fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(2)].ToLine);
  Assert.IsTrue(FindFoldAtLine(3) >= 0, '.container fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(3)].ToLine);
end;

{ --- Same-line balanced braces --- }

procedure TTestCSSFolding.TestSameLineBracesDoNotFold;
begin
  ScanText(
    'body { color: red; }'    + sLineBreak +  // line 1
    'h1 { font-size: 18px; }'                 // line 2
  );
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'same-line body rule must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'same-line h1 rule must not fold');
end;

{ --- Comments --- }

procedure TTestCSSFolding.TestCommentDoesNotFold;
begin
  ScanText(
    '/* Single line comment */' + sLineBreak +  // line 1
    'body {'                    + sLineBreak +  // line 2
    '  color: red;'             + sLineBreak +  // line 3
    '}'                                          // line 4
  );
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'single-line comment must not fold');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'body fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine);
end;

procedure TTestCSSFolding.TestMultiLineCommentDoesNotFold;
begin
  ScanText(
    '/* Start of'              + sLineBreak +  // line 1
    '   multi-line'            + sLineBreak +  // line 2
    '   comment */'            + sLineBreak +  // line 3
    'body {'                   + sLineBreak +  // line 4
    '  color: red;'            + sLineBreak +  // line 5
    '}'                                         // line 6
  );
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'multi-line comment start must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'comment middle must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'comment end must not fold');
  Assert.IsTrue(FindFoldAtLine(4) >= 0, 'body fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(4)].ToLine);
end;

procedure TTestCSSFolding.TestBracesInCommentIgnored;
begin
  ScanText(
    '/* { not a fold } */'     + sLineBreak +  // line 1
    'body {'                   + sLineBreak +  // line 2
    '  /* } not a close */'    + sLineBreak +  // line 3
    '  color: red;'            + sLineBreak +  // line 4
    '}'                                         // line 5
  );
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'braces in comment must not fold');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'body fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'comment brace must not affect fold range');
end;

{ --- Strings --- }

procedure TTestCSSFolding.TestBracesInDoubleQuoteStringIgnored;
begin
  ScanText(
    'body {'                           + sLineBreak +  // line 1
    '  content: "brace { here";'       + sLineBreak +  // line 2
    '}'                                                 // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'body fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'brace in double-quote string must not affect fold');
end;

procedure TTestCSSFolding.TestBracesInSingleQuoteStringIgnored;
begin
  ScanText(
    'body {'                           + sLineBreak +  // line 1
    '  content: ''brace } here'';'     + sLineBreak +  // line 2
    '}'                                                 // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'body fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'brace in single-quote string must not affect fold');
end;

procedure TTestCSSFolding.TestEscapedQuoteInString;
begin
  ScanText(
    'body {'                                + sLineBreak +  // line 1
    '  content: "escaped \" brace { x";'    + sLineBreak +  // line 2
    '}'                                                      // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'body fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'escaped quote must not end string early');
end;

{ --- Edge cases --- }

procedure TTestCSSFolding.TestEmptyRuleBlock;
begin
  ScanText(
    'body {'              + sLineBreak +   // line 1
    '}'                                     // line 2
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'empty body fold should exist');
  Assert.AreEqual(2, FFoldRanges[FindFoldAtLine(1)].ToLine);
end;

{ --- Full sample --- }

procedure TTestCSSFolding.TestFullSampleFoldsCorrectly;
var
  SampleSource: string;
  Idx: Integer;
begin
  SampleSource :=
    '/* Main Stylesheet */'#13#10 +                      // line 1
    'body {'#13#10 +                                      // line 2
    '  font-family: Arial, sans-serif;'#13#10 +           // line 3
    '  font-size: 14px;'#13#10 +                          // line 4
    '  color: #333;'#13#10 +                              // line 5
    '}'#13#10 +                                           // line 6
    'h1 {'#13#10 +                                        // line 7
    '  font-size: 24px;'#13#10 +                          // line 8
    '  color: #000099;'#13#10 +                           // line 9
    '}'#13#10 +                                           // line 10
    '@media screen and (max-width: 768px) {'#13#10 +      // line 11
    '  body {'#13#10 +                                    // line 12
    '    font-size: 12px;'#13#10 +                        // line 13
    '  }'#13#10 +                                         // line 14
    '  h1 {'#13#10 +                                      // line 15
    '    font-size: 18px;'#13#10 +                        // line 16
    '  }'#13#10 +                                         // line 17
    '}';                                                   // line 18
  ScanText(SampleSource);

  // /* Main Stylesheet */ (line 1) — no fold
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'comment must not fold');

  // body (line 2) folds to } (line 6)
  Idx := FindFoldAtLine(2);
  Assert.IsTrue(Idx >= 0, 'body fold should exist');
  Assert.AreEqual(6, FFoldRanges[Idx].ToLine);

  // h1 (line 7) folds to } (line 10)
  Idx := FindFoldAtLine(7);
  Assert.IsTrue(Idx >= 0, 'h1 fold should exist');
  Assert.AreEqual(10, FFoldRanges[Idx].ToLine);

  // @media (line 11) folds to } (line 18)
  Idx := FindFoldAtLine(11);
  Assert.IsTrue(Idx >= 0, '@media fold should exist');
  Assert.AreEqual(18, FFoldRanges[Idx].ToLine);

  // nested body (line 12) folds to } (line 14)
  Idx := FindFoldAtLine(12);
  Assert.IsTrue(Idx >= 0, 'nested body fold should exist');
  Assert.AreEqual(14, FFoldRanges[Idx].ToLine);

  // nested h1 (line 15) folds to } (line 17)
  Idx := FindFoldAtLine(15);
  Assert.IsTrue(Idx >= 0, 'nested h1 fold should exist');
  Assert.AreEqual(17, FFoldRanges[Idx].ToLine);

  // property lines and closing braces — no fold
  Assert.AreEqual(-1, FindFoldAtLine(3), 'property line must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(13), 'nested property line must not fold');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCSSFolding);

end.
