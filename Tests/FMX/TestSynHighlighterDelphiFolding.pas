unit TestSynHighlighterDelphiFolding;

{ Tests for Delphi highlighter fold range detection.
  Uses only shared units (SynEditCodeFolding, SynHighlighterDelphi,
  SynEditTextBuffer) — no FMX or VCL dependency. }

interface

uses
  DUnitX.TestFramework,
  SynEditCodeFolding,
  SynHighlighterDelphi;

type
  [TestFixture]
  TTestDelphiFolding = class
  private
    FHighlighter: TSynDelphiSyn;
    FFoldRanges: TSynFoldRanges;
    procedure ScanText(const AText: string);
    function FindFoldAtLine(ALine: Integer): Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    { Record folding }
    [Test]
    procedure TestRecordFoldsToEnd;
    [Test]
    procedure TestRecordWithClassVarFoldsCorrectly;
    [Test]
    procedure TestRecordDoesNotSwallowFollowingCode;
    [Test]
    procedure TestNestedRecordFolds;

    { Class folding }
    [Test]
    procedure TestClassFoldsToEnd;
    [Test]
    procedure TestClassWithMethodsFoldsCorrectly;

    { Begin..end folding }
    [Test]
    procedure TestBeginEndFolds;
    [Test]
    procedure TestNestedBeginEnd;

    { Procedure/function folding }
    [Test]
    procedure TestProcedureFoldsFromHeader;
    [Test]
    procedure TestProcedureDeclarationInRecordDoesNotFold;
    [Test]
    procedure TestProcedureDeclarationInClassDoesNotFold;
    [Test]
    procedure TestProcedureWithVarFoldsFromHeader;
    [Test]
    procedure TestFunctionFoldsFromHeader;
    [Test]
    procedure TestConstructorDestructorFoldFromHeader;
    [Test]
    procedure TestFullSampleFoldsCorrectly;

    { Try..end folding }
    [Test]
    procedure TestTryEndFolds;

    { Case..end folding }
    [Test]
    procedure TestCaseEndFolds;

    { Region folding }
    [Test]
    procedure TestRegionFolds;
    [Test]
    procedure TestRegionDoesNotInterfereWithBeginEnd;

    { Interface/Implementation folding }
    [Test]
    procedure TestInterfaceSectionFolds;
    [Test]
    procedure TestImplementationSectionFolds;
    [Test]
    procedure TestProgramEndDotClosesBeginFold;

    { Mixed scenarios }
    [Test]
    procedure TestRecordThenProcedure;
    [Test]
    procedure TestMultipleRecords;
    [Test]
    procedure TestClassVarDoesNotOpenFold;
    [Test]
    procedure TestClassFunctionDoesNotOpenClassFold;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  SynEditTextBuffer;

procedure TTestDelphiFolding.Setup;
begin
  FHighlighter := TSynDelphiSyn.Create(nil);
  FFoldRanges := TSynFoldRanges.Create;
end;

procedure TTestDelphiFolding.TearDown;
begin
  FFoldRanges.Free;
  FHighlighter.Free;
end;

procedure TTestDelphiFolding.ScanText(const AText: string);
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

function TTestDelphiFolding.FindFoldAtLine(ALine: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FFoldRanges.Count - 1 do
    if FFoldRanges[I].FromLine = ALine then
      Exit(I);
  Result := -1;
end;

{ --- Record folding --- }

procedure TTestDelphiFolding.TestRecordFoldsToEnd;
begin
  ScanText(
    'type'           + sLineBreak +   // line 1
    '  TPoint = record' + sLineBreak + // line 2
    '    X: Integer;' + sLineBreak +   // line 3
    '    Y: Integer;' + sLineBreak +   // line 4
    '  end;'                            // line 5
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0,
    'Should have a fold starting at the record line');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Record fold should close at end;');
end;

procedure TTestDelphiFolding.TestRecordWithClassVarFoldsCorrectly;
begin
  // This is the key bug scenario: "class var" contains the word "class"
  // which must NOT open a new fold range
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TMyRecord = record'        + sLineBreak +   // line 2
    '    class var Count: Integer;' + sLineBreak + // line 3
    '  end;'                                        // line 4
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0,
    'Should have a fold starting at the record line');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Record fold should close at end; (line 4), not extend further');
end;

procedure TTestDelphiFolding.TestRecordDoesNotSwallowFollowingCode;
begin
  // Record fold must not extend past its end; into subsequent code
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TMyRecord = record'        + sLineBreak +   // line 2
    '    class var Count: Integer;' + sLineBreak + // line 3
    '  end;'                       + sLineBreak +  // line 4
    ''                             + sLineBreak +  // line 5
    'procedure DoSomething;'       + sLineBreak +  // line 6
    'begin'                        + sLineBreak +  // line 7
    '  WriteLn;'                   + sLineBreak +  // line 8
    'end;'                                          // line 9
  );
  // Record fold should end at line 4
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Record fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Record fold must end at line 4, not swallow the procedure');

  // Procedure fold should start at the procedure header, not begin
  Assert.IsTrue(FindFoldAtLine(6) >= 0, 'Procedure fold should exist at line 6');
  Assert.AreEqual(9, FFoldRanges[FindFoldAtLine(6)].ToLine,
    'Procedure fold should close at line 9');
end;

procedure TTestDelphiFolding.TestNestedRecordFolds;
begin
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TOuter = record'           + sLineBreak +   // line 2
    '    Inner: record'           + sLineBreak +   // line 3
    '      X: Integer;'           + sLineBreak +   // line 4
    '    end;'                    + sLineBreak +   // line 5
    '  end;'                                        // line 6
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Outer record fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Outer record should fold to line 6');
  Assert.IsTrue(FindFoldAtLine(3) >= 0, 'Inner record fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(3)].ToLine,
    'Inner record should fold to line 5');
end;

{ --- Class folding --- }

procedure TTestDelphiFolding.TestClassFoldsToEnd;
begin
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TFoo = class'              + sLineBreak +   // line 2
    '    FValue: Integer;'        + sLineBreak +   // line 3
    '  end;'                                        // line 4
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Class fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Class fold should close at end;');
end;

procedure TTestDelphiFolding.TestClassWithMethodsFoldsCorrectly;
begin
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TFoo = class'              + sLineBreak +   // line 2
    '    procedure Bar;'          + sLineBreak +   // line 3
    '  end;'                      + sLineBreak +   // line 4
    ''                            + sLineBreak +   // line 5
    'procedure TFoo.Bar;'         + sLineBreak +   // line 6
    'begin'                       + sLineBreak +   // line 7
    'end;'                                          // line 8
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Class fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Class fold should close at its end; not extend to method end');
end;

{ --- Begin..end folding --- }

procedure TTestDelphiFolding.TestBeginEndFolds;
begin
  ScanText(
    'begin'                       + sLineBreak +   // line 1
    '  DoSomething;'              + sLineBreak +   // line 2
    'end.'                                          // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'begin..end fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'begin fold should close at end.');
end;

procedure TTestDelphiFolding.TestNestedBeginEnd;
begin
  ScanText(
    'begin'                       + sLineBreak +   // line 1
    '  begin'                     + sLineBreak +   // line 2
    '    DoInner;'                + sLineBreak +   // line 3
    '  end;'                      + sLineBreak +   // line 4
    'end.'                                          // line 5
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'Outer begin fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'Outer begin should fold to line 5');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Inner begin fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Inner begin should fold to line 4');
end;

{ --- Procedure/function folding --- }

procedure TTestDelphiFolding.TestProcedureFoldsFromHeader;
begin
  ScanText(
    'procedure Foo;'              + sLineBreak +   // line 1
    'begin'                       + sLineBreak +   // line 2
    '  WriteLn;'                  + sLineBreak +   // line 3
    'end;'                                          // line 4
  );
  // Fold should start at the procedure header, not at begin
  Assert.IsTrue(FindFoldAtLine(1) >= 0,
    'Fold should start at procedure header line');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'Procedure fold should close at end;');
  // No separate fold at begin line
  Assert.AreEqual(-1, FindFoldAtLine(2),
    'begin should not have its own separate fold');
end;

procedure TTestDelphiFolding.TestProcedureDeclarationInRecordDoesNotFold;
begin
  // procedure Save; inside a record is a declaration only — no body, no fold
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TMyHelper = record helper for TMyRecord' + sLineBreak + // line 2
    '    procedure Save;'         + sLineBreak +   // line 3
    '  end;'                                        // line 4
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Record fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Record fold should close at end;');
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'procedure Save; declaration must not create a fold');
end;

procedure TTestDelphiFolding.TestProcedureDeclarationInClassDoesNotFold;
begin
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TFoo = class'              + sLineBreak +   // line 2
    '    procedure DoWork;'       + sLineBreak +   // line 3
    '    function GetValue: Integer;' + sLineBreak + // line 4
    '  end;'                                        // line 5
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Class fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Class fold should close at end;');
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'procedure declaration in class must not fold');
  Assert.AreEqual(-1, FindFoldAtLine(4),
    'function declaration in class must not fold');
end;

procedure TTestDelphiFolding.TestProcedureWithVarFoldsFromHeader;
begin
  // var block between procedure header and begin should be included in the fold
  ScanText(
    'procedure Test;'             + sLineBreak +   // line 1
    'var'                         + sLineBreak +   // line 2
    '  X: Integer;'              + sLineBreak +   // line 3
    'begin'                       + sLineBreak +   // line 4
    '  X := 1;'                  + sLineBreak +   // line 5
    'end;'                                          // line 6
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0,
    'Fold should start at procedure header');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'Procedure fold should include var block and close at end;');
  Assert.AreEqual(-1, FindFoldAtLine(4),
    'begin should not have its own separate fold');
end;

procedure TTestDelphiFolding.TestFunctionFoldsFromHeader;
begin
  ScanText(
    'function Add(A, B: Integer): Integer;' + sLineBreak + // line 1
    'begin'                       + sLineBreak +   // line 2
    '  Result := A + B;'         + sLineBreak +   // line 3
    'end;'                                          // line 4
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0,
    'Fold should start at function header');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'Function fold should close at end;');
end;

procedure TTestDelphiFolding.TestConstructorDestructorFoldFromHeader;
begin
  ScanText(
    'constructor TFoo.Create;'   + sLineBreak +   // line 1
    'begin'                       + sLineBreak +   // line 2
    '  inherited;'               + sLineBreak +   // line 3
    'end;'                        + sLineBreak +   // line 4
    ''                            + sLineBreak +   // line 5
    'destructor TFoo.Destroy;'   + sLineBreak +   // line 6
    'begin'                       + sLineBreak +   // line 7
    '  inherited;'               + sLineBreak +   // line 8
    'end;'                                          // line 9
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0,
    'Constructor fold should start at header');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'Constructor fold should close at end;');
  Assert.IsTrue(FindFoldAtLine(6) >= 0,
    'Destructor fold should start at header');
  Assert.AreEqual(9, FFoldRanges[FindFoldAtLine(6)].ToLine,
    'Destructor fold should close at end;');
end;

procedure TTestDelphiFolding.TestFullSampleFoldsCorrectly;
var
  SampleSource: string;
  Idx: Integer;
begin
  // Test using the actual GetSampleSource text from the highlighter
  SampleSource :=
    'unit ModernDelphi;'#13#10 +              // line 1
    'interface'#13#10 +                        // line 2
    'type'#13#10 +                             // line 3
    '  TMyRecord = record'#13#10 +             // line 4
    '    class var Count: Integer;'#13#10 +    // line 5
    '  end;'#13#10 +                           // line 6
    '  TMyHelper = record helper for TMyRecord'#13#10 + // line 7
    '    procedure Save;'#13#10 +              // line 8
    '  end;'#13#10 +                           // line 9
    'implementation'#13#10 +                   // line 10
    'procedure Test;'#13#10 +                  // line 11
    'var'#13#10 +                              // line 12
    '  JSON: string;'#13#10 +                  // line 13
    'begin'#13#10 +                            // line 14
    '  // Delphi 13 Multiline String'#13#10 +  // line 15
    '  JSON := '''#13#10 +                     // line 16
    '    {'#13#10 +                            // line 17
    '      "name": "Delphi",'#13#10 +          // line 18
    '      "version": 13'#13#10 +              // line 19
    '    }'#13#10 +                            // line 20
    '  '''';'#13#10 +                          // line 21
    'end;'#13#10 +                             // line 22
    'end.';                                    // line 23
  ScanText(SampleSource);

  // interface (line 2) folds to line 9 (before implementation)
  Idx := FindFoldAtLine(2);
  Assert.IsTrue(Idx >= 0, 'Interface fold should exist at line 2');
  Assert.AreEqual(9, FFoldRanges[Idx].ToLine,
    'Interface fold should close before implementation');

  // TMyRecord = record (line 4) folds to end; (line 6)
  Idx := FindFoldAtLine(4);
  Assert.IsTrue(Idx >= 0, 'TMyRecord fold should exist at line 4');
  Assert.AreEqual(6, FFoldRanges[Idx].ToLine,
    'TMyRecord should fold to line 6, not extend further');

  // TMyHelper = record helper (line 7) folds to end; (line 9)
  Idx := FindFoldAtLine(7);
  Assert.IsTrue(Idx >= 0, 'TMyHelper fold should exist at line 7');
  Assert.AreEqual(9, FFoldRanges[Idx].ToLine,
    'TMyHelper should fold to line 9');

  // procedure Save; (line 8) must NOT have a fold — it is a declaration
  Assert.AreEqual(-1, FindFoldAtLine(8),
    'procedure Save; declaration must not fold');

  // implementation (line 10) folds to end. (line 23)
  Idx := FindFoldAtLine(10);
  Assert.IsTrue(Idx >= 0, 'Implementation fold should exist at line 10');
  Assert.AreEqual(23, FFoldRanges[Idx].ToLine,
    'Implementation fold should close at end.');

  // procedure Test (line 11) should fold to end; (line 22)
  Idx := FindFoldAtLine(11);
  Assert.IsTrue(Idx >= 0, 'procedure Test fold should exist at line 11');
  Assert.AreEqual(22, FFoldRanges[Idx].ToLine,
    'procedure Test should fold to end; at line 22');

  // begin at line 14 should NOT have its own fold
  Assert.AreEqual(-1, FindFoldAtLine(14),
    'begin should not have a separate fold from procedure');
end;

{ --- Try..end folding --- }

procedure TTestDelphiFolding.TestTryEndFolds;
begin
  ScanText(
    'begin'                       + sLineBreak +   // line 1
    '  try'                       + sLineBreak +   // line 2
    '    DoRisky;'                + sLineBreak +   // line 3
    '  except'                    + sLineBreak +   // line 4
    '  end;'                      + sLineBreak +   // line 5
    'end.'                                          // line 6
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'try fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'try fold should close at its end;');
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'begin fold should exist');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'begin fold should close at end.');
end;

{ --- Case..end folding --- }

procedure TTestDelphiFolding.TestCaseEndFolds;
begin
  ScanText(
    'begin'                       + sLineBreak +   // line 1
    '  case X of'                 + sLineBreak +   // line 2
    '    1: DoA;'                 + sLineBreak +   // line 3
    '    2: DoB;'                 + sLineBreak +   // line 4
    '  end;'                      + sLineBreak +   // line 5
    'end.'                                          // line 6
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'case fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'case fold should close at its end;');
end;

{ --- Region folding --- }

procedure TTestDelphiFolding.TestRegionFolds;
begin
  ScanText(
    '{$REGION ''MyRegion''}'      + sLineBreak +   // line 1
    '// some code'                + sLineBreak +   // line 2
    '{$ENDREGION}'                                  // line 3
  );
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'Region fold should exist');
  Assert.AreEqual(3, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'Region fold should close at $ENDREGION');
  Assert.AreEqual(FoldRegionType, FFoldRanges[FindFoldAtLine(1)].FoldType,
    'Region fold should have FoldRegionType');
end;

procedure TTestDelphiFolding.TestRegionDoesNotInterfereWithBeginEnd;
begin
  ScanText(
    '{$REGION ''Block''}'         + sLineBreak +   // line 1
    'begin'                       + sLineBreak +   // line 2
    '  DoWork;'                   + sLineBreak +   // line 3
    'end;'                        + sLineBreak +   // line 4
    '{$ENDREGION}'                                  // line 5
  );
  // Region fold
  Assert.IsTrue(FindFoldAtLine(1) >= 0, 'Region fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(1)].ToLine,
    'Region fold should span lines 1-5');
  // begin..end fold inside the region
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'begin..end fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'begin..end fold should close at line 4');
end;

{ --- Interface/Implementation folding --- }

procedure TTestDelphiFolding.TestInterfaceSectionFolds;
begin
  ScanText(
    'unit Foo;'                   + sLineBreak +   // line 1
    'interface'                   + sLineBreak +   // line 2
    'type'                        + sLineBreak +   // line 3
    '  TBar = record'             + sLineBreak +   // line 4
    '    X: Integer;'            + sLineBreak +   // line 5
    '  end;'                      + sLineBreak +   // line 6
    'implementation'              + sLineBreak +   // line 7
    'end.'                                          // line 8
  );
  // Interface section folds from line 2 to line 6 (before implementation)
  Assert.IsTrue(FindFoldAtLine(2) >= 0,
    'Interface fold should exist at line 2');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Interface fold should close before implementation');
  // Implementation section folds from line 7 to line 8
  Assert.IsTrue(FindFoldAtLine(7) >= 0,
    'Implementation fold should exist at line 7');
  Assert.AreEqual(8, FFoldRanges[FindFoldAtLine(7)].ToLine,
    'Implementation fold should close at end.');
end;

procedure TTestDelphiFolding.TestImplementationSectionFolds;
begin
  ScanText(
    'unit Foo;'                   + sLineBreak +   // line 1
    'interface'                   + sLineBreak +   // line 2
    'implementation'              + sLineBreak +   // line 3
    'procedure Bar;'              + sLineBreak +   // line 4
    'begin'                       + sLineBreak +   // line 5
    'end;'                        + sLineBreak +   // line 6
    'end.'                                          // line 7
  );
  // Implementation section folds from line 3 to line 7
  Assert.IsTrue(FindFoldAtLine(3) >= 0,
    'Implementation fold should exist');
  Assert.AreEqual(7, FFoldRanges[FindFoldAtLine(3)].ToLine,
    'Implementation fold should close at end.');
  // Procedure fold still works inside implementation
  Assert.IsTrue(FindFoldAtLine(4) >= 0,
    'Procedure fold should exist at line 4');
  Assert.AreEqual(6, FFoldRanges[FindFoldAtLine(4)].ToLine,
    'Procedure fold should close at end;');
end;

procedure TTestDelphiFolding.TestProgramEndDotClosesBeginFold;
begin
  // In a program (no interface/implementation), end. closes begin
  ScanText(
    'program Foo;'                + sLineBreak +   // line 1
    'begin'                       + sLineBreak +   // line 2
    '  WriteLn;'                 + sLineBreak +   // line 3
    'end.'                                          // line 4
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0,
    'begin fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'begin fold should close at end.');
end;

{ --- Mixed scenarios --- }

procedure TTestDelphiFolding.TestRecordThenProcedure;
begin
  // Record followed by procedure — each should fold independently
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TRec = record'             + sLineBreak +   // line 2
    '    Value: Integer;'         + sLineBreak +   // line 3
    '  end;'                      + sLineBreak +   // line 4
    ''                            + sLineBreak +   // line 5
    'procedure Test;'             + sLineBreak +   // line 6
    'begin'                       + sLineBreak +   // line 7
    'end;'                                          // line 8
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Record fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Record fold should close at line 4');
  Assert.IsTrue(FindFoldAtLine(6) >= 0, 'Procedure fold should exist at line 6');
  Assert.AreEqual(8, FFoldRanges[FindFoldAtLine(6)].ToLine,
    'Procedure fold should close at line 8');
end;

procedure TTestDelphiFolding.TestMultipleRecords;
begin
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TRec1 = record'            + sLineBreak +   // line 2
    '    A: Integer;'             + sLineBreak +   // line 3
    '  end;'                      + sLineBreak +   // line 4
    ''                            + sLineBreak +   // line 5
    '  TRec2 = record'            + sLineBreak +   // line 6
    '    B: Integer;'             + sLineBreak +   // line 7
    '  end;'                                        // line 8
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'First record fold should exist');
  Assert.AreEqual(4, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'First record should fold to line 4');
  Assert.IsTrue(FindFoldAtLine(6) >= 0, 'Second record fold should exist');
  Assert.AreEqual(8, FFoldRanges[FindFoldAtLine(6)].ToLine,
    'Second record should fold to line 8');
end;

procedure TTestDelphiFolding.TestClassVarDoesNotOpenFold;
begin
  // "class var" should NOT create a fold-open for the word "class"
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  TMyRecord = record'        + sLineBreak +   // line 2
    '    class var A: Integer;'   + sLineBreak +   // line 3
    '    class var B: Integer;'   + sLineBreak +   // line 4
    '  end;'                                        // line 5
  );
  // There should be exactly one fold range starting at line 2
  Assert.AreEqual(-1, FindFoldAtLine(3),
    'class var on line 3 must not open a fold range');
  Assert.AreEqual(-1, FindFoldAtLine(4),
    'class var on line 4 must not open a fold range');
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Record fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Record fold should close at end; on line 5');
end;

procedure TTestDelphiFolding.TestClassFunctionDoesNotOpenClassFold;
begin
  // "class function" / "class procedure" should be treated as code
  // declarations, not as class block openers
  ScanText(
    'type'                        + sLineBreak +   // line 1
    '  THelper = record'          + sLineBreak +   // line 2
    '    class function Create: THelper; static;' + sLineBreak + // line 3
    '    class procedure Reset; static;' + sLineBreak + // line 4
    '  end;'                                        // line 5
  );
  Assert.IsTrue(FindFoldAtLine(2) >= 0, 'Record fold should exist');
  Assert.AreEqual(5, FFoldRanges[FindFoldAtLine(2)].ToLine,
    'Record fold should close at end; on line 5');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDelphiFolding);

end.
