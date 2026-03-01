unit TestSynHighlighterHTMLFolding;

{ Tests for HTML highlighter fold range detection.
  Uses only shared units (SynEditCodeFolding, SynHighlighterHtml,
  SynEditTextBuffer) — no FMX or VCL dependency. }

interface

uses
  DUnitX.TestFramework,
  SynEditCodeFolding,
  SynHighlighterHtml;

type
  [TestFixture]
  TTestHTMLFolding = class
  private
    FHighlighter: TSynHTMLSyn;
    FFoldRanges: TSynFoldRanges;
    procedure ScanText(const AText: string);
    function FindFoldAtLine(ALine: Integer): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Basic tag folding }
    [Test]
    procedure TestSingleTagPairFolds;
    [Test]
    procedure TestNestedTagsFold;
    [Test]
    procedure TestDeeplyNestedTags;

    { Void elements }
    [Test]
    procedure TestVoidElementsDoNotFold;
    [Test]
    procedure TestSelfClosingTagsDoNotFold;

    { Same-line balanced tags }
    [Test]
    procedure TestBalancedTagsOnOneLineDoNotFold;

    { Comments and DOCTYPE }
    [Test]
    procedure TestCommentsDoNotFold;
    [Test]
    procedure TestDoctypeDoesNotFold;

    { Multi-line attributes }
    [Test]
    procedure TestMultiLineVoidTagDoesNotFold;

    { Full sample source }
    [Test]
    procedure TestFullSampleFoldsCorrectly;

    { Attributes with special characters }
    [Test]
    procedure TestQuotedGreaterThanInAttribute;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  SynEditTextBuffer;

procedure TTestHTMLFolding.Setup;
begin
  FHighlighter := TSynHTMLSyn.Create(nil);
  FFoldRanges := TSynFoldRanges.Create;
end;

procedure TTestHTMLFolding.TearDown;
begin
  FFoldRanges.Free;
  FHighlighter.Free;
end;

procedure TTestHTMLFolding.ScanText(const AText: string);
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

function TTestHTMLFolding.FindFoldAtLine(ALine: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FFoldRanges.Count - 1 do
    if FFoldRanges[I].FromLine = ALine then
      Exit(I);
  Result := -1;
end;

{ --- Basic tag folding --- }

procedure TTestHTMLFolding.TestSingleTagPairFolds;
begin
  ScanText(
    '<div>'             + sLineBreak +   // line 1
    '  content'         + sLineBreak +   // line 2
    '</div>'                              // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'div fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'div fold should close at </div>');
end;

procedure TTestHTMLFolding.TestNestedTagsFold;
begin
  ScanText(
    '<html>'            + sLineBreak +   // line 1
    '  <body>'          + sLineBreak +   // line 2
    '    content'       + sLineBreak +   // line 3
    '  </body>'         + sLineBreak +   // line 4
    '</html>'                             // line 5
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'html fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'html fold should close at </html>');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'body fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'body fold should close at </body>');
end;

procedure TTestHTMLFolding.TestDeeplyNestedTags;
begin
  ScanText(
    '<html>'            + sLineBreak +   // line 1
    '<body>'            + sLineBreak +   // line 2
    '<div>'             + sLineBreak +   // line 3
    '  text'            + sLineBreak +   // line 4
    '</div>'            + sLineBreak +   // line 5
    '</body>'           + sLineBreak +   // line 6
    '</html>'                             // line 7
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'html fold should exist');
  Assert.AreEqual(7, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'body fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(2)].ToLine);
  Assert.IsTrue(FindFoldAtLine(3) >= 0, 'div fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(3)].ToLine);
end;

{ --- Void elements --- }

procedure TTestHTMLFolding.TestVoidElementsDoNotFold;
begin
  ScanText(
    '<div>'             + sLineBreak +   // line 1
    '  <br>'            + sLineBreak +   // line 2
    '  <hr>'            + sLineBreak +   // line 3
    '  <img src="x">'   + sLineBreak +   // line 4
    '  <input type="text">' + sLineBreak + // line 5
    '  <meta charset="utf-8">' + sLineBreak + // line 6
    '  <link rel="stylesheet" href="x">' + sLineBreak + // line 7
    '</div>'                              // line 8
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'div fold should exist');
  Assert.AreEqual(8, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2), 'br must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(3), 'hr must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(4), 'img must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(5), 'input must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(6), 'meta must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(7), 'link must not fold');
end;

procedure TTestHTMLFolding.TestSelfClosingTagsDoNotFold;
begin
  ScanText(
    '<div>'             + sLineBreak +   // line 1
    '  <br/>'           + sLineBreak +   // line 2
    '  <img src="x" />' + sLineBreak +  // line 3
    '</div>'                              // line 4
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'div fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2), 'self-closing br must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(3), 'self-closing img must not fold');
end;

{ --- Same-line balanced tags --- }

procedure TTestHTMLFolding.TestBalancedTagsOnOneLineDoNotFold;
begin
  ScanText(
    '<div>'                               + sLineBreak + // line 1
    '  <span>some text</span>'            + sLineBreak + // line 2
    '  <p>paragraph</p>'                  + sLineBreak + // line 3
    '</div>'                                              // line 4
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'div fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'balanced span on one line must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'balanced p on one line must not fold');
end;

{ --- Comments and DOCTYPE --- }

procedure TTestHTMLFolding.TestCommentsDoNotFold;
begin
  ScanText(
    '<!-- comment -->'  + sLineBreak +   // line 1
    '<div>'             + sLineBreak +   // line 2
    '  text'            + sLineBreak +   // line 3
    '</div>'                              // line 4
  );
  Assert.AreEqual(-1, FindFoldAtLine(1), 'comment must not fold');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'div fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine);
end;

procedure TTestHTMLFolding.TestDoctypeDoesNotFold;
begin
  ScanText(
    '<!DOCTYPE html>'   + sLineBreak +   // line 1
    '<html>'            + sLineBreak +   // line 2
    '</html>'                             // line 3
  );
  Assert.AreEqual(-1, FindFoldAtLine(1), 'DOCTYPE must not fold');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'html fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(2)].ToLine);
end;

{ --- Multi-line attributes --- }

procedure TTestHTMLFolding.TestMultiLineVoidTagDoesNotFold;
begin
  // <input> spans two lines but is void — should not fold
  ScanText(
    '<form>'                              + sLineBreak + // line 1
    '  <input name="user" value=''any'    + sLineBreak + // line 2
    '    value''>'                         + sLineBreak + // line 3
    '</form>'                                             // line 4
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'form fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine);
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'multi-line void input must not fold');
end;

{ --- Full sample source --- }

procedure TTestHTMLFolding.TestFullSampleFoldsCorrectly;
var
  SampleSource: string;
  Idx: Integer;
begin
  SampleSource :=
    '<!-- Syntax highlighting -->'#13#10 +              // line 1
    #13#10 +                                            // line 2
    '<html>'#13#10 +                                    // line 3
    '<body bgcolor="red">'#13#10 +                      // line 4
    '  <form name="frmLogin" action="doSomething.asp">'#13#10 + // line 5
    '    <input name="user" value=''any'#13#10 +        // line 6
    '      value''>'#13#10 +                            // line 7
    '  </form>'#13#10 +                                 // line 8
    '  <invalid>Sample HTML code &copy; 2001</invalid>'#13#10 + // line 9
    '</body>'#13#10 +                                   // line 10
    '</html>';                                          // line 11
  ScanText(SampleSource);

  // <html> (line 3) folds to </html> (line 11)
  Idx := FindFoldAtLine(3);
  Assert.IsTrue(Idx >= 0, 'html fold should exist at line 3');
  Assert.AreEqual(11, FFoldRanges[Idx].ToLine,
    'html should fold to line 11');

  // <body> (line 4) folds to </body> (line 10)
  Idx := FindFoldAtLine(4);
  Assert.IsTrue(Idx >= 0, 'body fold should exist at line 4');
  Assert.AreEqual(10, FFoldRanges[Idx].ToLine,
    'body should fold to line 10');

  // <form> (line 5) folds to </form> (line 8)
  Idx := FindFoldAtLine(5);
  Assert.IsTrue(Idx >= 0, 'form fold should exist at line 5');
  Assert.AreEqual(8, FFoldRanges[Idx].ToLine,
    'form should fold to line 8');

  // <input> (line 6) must NOT fold — void element
  Assert.AreEqual(-1, FindFoldAtLine(6),
    'input (void element) must not fold');

  // <invalid>...</invalid> on line 9 — balanced, no fold
  Assert.AreEqual(-1, FindFoldAtLine(9),
    'balanced tags on one line must not fold');

  // comment on line 1 — no fold
  Assert.AreEqual(-1, FindFoldAtLine(1),
    'comment must not fold');
end;

{ --- Attributes with special characters --- }

procedure TTestHTMLFolding.TestQuotedGreaterThanInAttribute;
begin
  // > inside a quoted attribute value should not close the tag
  ScanText(
    '<div data-expr="a > b">'  + sLineBreak + // line 1
    '  content'                + sLineBreak + // line 2
    '</div>'                                   // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0,
    'div fold should exist despite > in attribute');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'div fold should close at </div>');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestHTMLFolding);

end.
