unit SynSpellCheckMetaphone;

interface

function metaphone(a: PWideChar; lg: Integer): PWideChar; stdcall;

implementation

uses
  SynUnicode, {$IFDEF USE_JCL_UNICODE_SUPPORT}JclUnicode, {$ENDIF}
  Classes, SysUtils;

type
  TRuleType = (
    mrBeginningOfWord, // If FromStr is at begining of word
    mrAfter, // If FromStr is just after arg
    mrBefore, // If FromStr is just before arg
    mrNotAfterVowel, // If FromStr is not preceded by vowel
    mrNotBeforeVowel, // If FromStr is not followed by vowel
    mrBetween, // If FromStr is between two chars specified in Args
    mrNotEndAfter, // Not at end of word after string in Args
    mrNotEndBefore, // Not at end of word just before string in args
    mrBeforeNoVowel, // Before "args", but no vowel thereafter
    mrAfterVowelNotBeforeVowel, // After, but not before vowel
    mrAtEndBefore, // At end of word just bofore "Arg"
    mrNone); // Rule always applies

  TRule = record
    sFrom, sTo: UnicodeString;
    RuleType: TRuleType;
    Args: UnicodeString;
  end;

const
  RuleCount = 67;

  // Note - always make the default rule the LAST
  Rules: array[1..RuleCount] of TRule = (
    // Beginning of word rules
    (sFrom: ('AE'); sTo: ('E'); RuleType: mrBeginningOfWord; Args: ('')),
    (sFrom: ('GN'); sTo: ('N'); RuleType: mrBeginningOfWord; Args: ('')),
    (sFrom: ('KN'); sTo: ('N'); RuleType: mrBeginningOfWord; Args: ('')),
    (sFrom: ('PN'); sTo: ('N'); RuleType: mrBeginningOfWord; Args: ('')),
    (sFrom: ('WR'); sTo: ('R'); RuleType: mrBeginningOfWord; Args: ('')),
    (sFrom: ('PS'); sTo: ('S'); RuleType: mrBeginningOfWord; Args: ('')),
    (sFrom: ('WH'); sTo: ('W'); RuleType: mrBeginningOfWord; Args: ('')),
      // 8 rules
    // B
    (sFrom: ('B'); sTo: ('B'); RuleType: mrNotEndAfter; Args: ('M')),
    (sFrom: ('B'); sTo: ('B'); RuleType: mrNone; Args: ('')),
    // C
    (sFrom: ('C'); sTo: ('X'); RuleType: mrBetween; Args: ('CA')),
    (sFrom: ('C'); sTo: ('X'); RuleType: mrBefore; Args: ('H')),
    (sFrom: ('C'); sTo: ('S'); RuleType: mrBefore; Args: ('I')),
    (sFrom: ('C'); sTo: ('S'); RuleType: mrBefore; Args: ('E')),
    (sFrom: ('C'); sTo: ('S'); RuleType: mrBefore; Args: ('Y')),
    (sFrom: ('C'); sTo: (''); RuleType: mrBetween; Args: ('SE')),
    (sFrom: ('C'); sTo: (''); RuleType: mrBetween; Args: ('SI')), // 10
    (sFrom: ('C'); sTo: ('K'); RuleType: mrNone; Args: ('')),
    // D
    (sFrom: ('D'); sTo: ('J'); RuleType: mrBefore; Args: ('GE')),
    (sFrom: ('D'); sTo: ('J'); RuleType: mrBefore; Args: ('GY')),
    (sFrom: ('D'); sTo: ('J'); RuleType: mrBefore; Args: ('GI')),
    (sFrom: ('D'); sTo: ('T'); RuleType: mrNone; Args: ('')),
    //F
    (sFrom: ('F'); sTo: ('F'); RuleType: mrNone; Args: ('')),
    // GG - changed to "1" in phase 1
    (sFrom: ('1'); sTo: ('K'); RuleType: mrNone; Args: ('')),
    // G
    (sFrom: ('G'); sTo: ('G'); RuleType: mrBefore; Args: ('G')),
    (sFrom: ('G'); sTo: (''); RuleType: mrAfter; Args: ('G')),
    (sFrom: ('G'); sTo: (''); RuleType: mrBeforeNoVowel; Args: ('H')),
    (sFrom: ('G'); sTo: (''); RuleType: mrAtEndBefore; Args: ('N')),
    (sFrom: ('G'); sTo: (''); RuleType: mrAtEndBefore; Args: ('NED')),
    (sFrom: ('G'); sTo: (''); RuleType: mrBetween; Args: ('DE')),
      // Paired with D rule
    (sFrom: ('G'); sTo: ('J'); RuleType: mrBefore; Args: ('I')),
    (sFrom: ('G'); sTo: ('J'); RuleType: mrBefore; Args: ('E')),
    (sFrom: ('G'); sTo: ('J'); RuleType: mrBefore; Args: ('Y')),
    (sFrom: ('G'); sTo: ('K'); RuleType: mrNone; Args: ('')), // 16
    // H
    (sFrom: ('H'); sTo: (''); RuleType: mrAfterVowelNotBeforeVowel; Args: ('')),
    (sFrom: ('H'); sTo: (''); RuleType: mrAfter; Args: ('C')),
    (sFrom: ('H'); sTo: (''); RuleType: mrAfter; Args: ('S')),
    (sFrom: ('H'); sTo: (''); RuleType: mrAfter; Args: ('P')),
    (sFrom: ('H'); sTo: (''); RuleType: mrAfter; Args: ('T')),
    (sFrom: ('H'); sTo: (''); RuleType: mrAfter; Args: ('G')),
    (sFrom: ('H'); sTo: (''); RuleType: mrNone; Args: ('')),
    // J
    (sFrom: ('J'); sTo: ('J'); RuleType: mrNone; Args: ('')),
    // K
    (sFrom: ('K'); sTo: (''); RuleType: mrAfter; Args: ('C')),
    (sFrom: ('K'); sTo: ('K'); RuleType: mrNone; Args: ('')), // 10
    // L
    (sFrom: ('L'); sTo: ('L'); RuleType: mrNone; Args: ('')),
    // M
    (sFrom: ('M'); sTo: ('M'); RuleType: mrNone; Args: ('')),
    // N
    (sFrom: ('N'); sTo: ('N'); RuleType: mrNone; Args: ('')),
    // P
    (sFrom: ('P'); sTo: ('F'); RuleType: mrBefore; Args: ('H')),
    (sFrom: ('P'); sTo: ('P'); RuleType: mrNone; Args: ('')),
    // Q
    (sFrom: ('Q'); sTo: ('K'); RuleType: mrNone; Args: ('')),
    // R
    (sFrom: ('R'); sTo: ('R'); RuleType: mrNone; Args: ('')),
    // S
    (sFrom: ('S'); sTo: ('X'); RuleType: mrBefore; Args: ('H')),
    (sFrom: ('S'); sTo: ('X'); RuleType: mrBetween; Args: ('SO')),
    (sFrom: ('S'); sTo: ('X'); RuleType: mrBetween; Args: ('SA')),
    (sFrom: ('S'); sTo: ('S'); RuleType: mrNone; Args: ('')), //11
    // T
    (sFrom: ('T'); sTo: ('X'); RuleType: mrBefore; Args: ('IA')),
    (sFrom: ('T'); sTo: ('X'); RuleType: mrBefore; Args: ('IO')),
    (sFrom: ('T'); sTo: (''); RuleType: mrBefore; Args: ('CH')),
    (sFrom: ('T'); sTo: ('0'); RuleType: mrBefore; Args: ('H')),
    (sFrom: ('T'); sTo: ('T'); RuleType: mrNone; Args: ('')),
    // V
    (sFrom: ('V'); sTo: ('F'); RuleType: mrNone; Args: ('')),
    // W
    (sFrom: ('W'); sTo: ('W'); RuleType: mrNotBeforeVowel; Args: ('')),
    (sFrom: ('W'); sTo: (''); RuleType: mrNone; Args: ('')),
    // X
    (sFrom: ('X'); sTo: ('S'); RuleType: mrBeginningOfWord; Args: ('')),
    (sFrom: ('X'); sTo: ('KS'); RuleType: mrNone; Args: ('')),
    // Y
    (sFrom: ('Y'); sTo: ('Y'); RuleType: mrNotBeforeVowel; Args: ('')),
    (sFrom: ('Y'); sTo: (''); RuleType: mrNone; Args: ('')),
    // Z
    (sFrom: ('Z'); sTo: ('S'); RuleType: mrNone; Args: (''))
    ); // 12

  AllowChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Vowels = 'AEIOU';

var
  OutStr: TUnicodeStringList;

procedure ApplyRules(ScanStr: UnicodeString; RuleBase: array of TRule);

  function RuleApplicable(Rule: TRule; CharIndex: integer): Boolean;
  var
    RemChar, PrevChar, ArgLength, InpLength: Integer;
  begin
    InpLength := length(ScanStr);
    RemChar := InpLength - CharIndex;
    PrevChar := CharIndex - 1;
    ArgLength := length(Rule.Args);
    Result := False;
    if ((RemChar + 1) >= Length(Rule.sFrom)) and (Copy(ScanStr, CharIndex,
      Length(Rule.sFrom)) = Rule.sFrom) then
      case Rule.RuleType of
        mrBeginningOfWord: //if sFrom is at beggining of word
          if CharIndex = 1 then
            Result := True;
        mrAfter: //If sFrom is just after arg
          if (PrevChar >= ArgLength) and (Copy(ScanStr, CharIndex - ArgLength -
            1, ArgLength) = Rule.Args) then
            Result := True;
        mrBefore: //If sFrom is just before arg
          if (RemChar >= ArgLength) and
            (copy(ScanStr, CharIndex + 1, ArgLength) = Rule.Args) then
            result := true;

        mrNotAfterVowel: //If sFrom is not preceded by vowel
          if (RemChar >= 1) and (pos(copy(ScanStr, CharIndex + 1, 1), Vowels) >
            0) then
            result := true;

        mrNotBeforeVowel: //If sFrom is not followed by vowel
          if (PrevChar >= 1) and (pos(copy(ScanStr, CharIndex + 1, 1), Vowels) >
            0) then
            result := true;

        mrBetween: //If sFrom is between two chars specified in Args
          if (PrevChar >= 1) and (RemChar >= 1) and (length(rule.args) = 2) and
            (copy(ScanStr, CharIndex - 1, 1) = copy(rule.args, 1, 1)) and
            (copy(ScanStr, CharIndex + 1, 1) = copy(rule.args, 2, 1)) then
            result := true;

        mrNotEndAfter: //Not at end of word after string in Args
          if (CharIndex < InpLength) and (PrevChar >= length(rule.args)) and
            (copy(ScanStr, CharIndex - ArgLength - 1, ArgLength) = Rule.Args)
              then
            result := true;

        mrNotEndBefore: //Not at end of word just before string in args
          if (ArgLength > RemChar) and
            (copy(ScanStr, CharIndex + 1, ArgLength) = Rule.Args) then
            result := true;

        mrBeforeNoVowel: //Before "args", but no vowel thereafter
          if (ArgLength + 1 <= RemChar) and
            (copy(ScanStr, CharIndex + 1, ArgLength) = Rule.Args) and
            (pos(vowels, copy(ScanStr, CharIndex + 1 + ArgLength, 1)) > 0) then
            result := true;

        mrAfterVowelNotBeforeVowel: //after, but not before vowel
          if (PrevChar > 0) and (RemChar > 0) and
            (pos(Vowels, copy(ScanStr, CharIndex - 1, 1)) > 0) and
            (pos(Vowels, copy(ScanStr, CharIndex + 1, 1)) = 0) then
            result := true;

        mrAtEndBefore: //At end of word just before "Arg"
          if (ArgLength = RemChar) and
            (copy(ScanStr, CharIndex + 1, ArgLength) = Rule.Args) then
            result := true;

        mrNone: //Rule always applies
          Result := true;
      end; //case

  end; //function RuleApplicable

var
  iI: Integer;

  t: integer;
  SkipRule: UnicodeString;
  SkipFlag: boolean;
begin
  t := Low(RuleBase);
  while t <= High(RuleBase) do
  begin
    SkipFlag := False;
    for iI := 1 to Length(ScanStr) do
      if RuleApplicable(RuleBase[t], iI) then
      begin
        OutStr.AddObject(RuleBase[t].sTo, Pointer(iI));
        SkipFlag := True;
        SkipRule := RuleBase[t].sFrom;
      end;
    if SkipFlag then
      while RuleBase[t].sFrom = SkipRule do
        Inc(t)
    else
      Inc(t); // Normal increment
  end;
end;

function FindRel(xx: Integer): UnicodeString;
var
  iI: Integer;
begin
  for iI := 0 to OutStr.Count - 1 do
    if Integer(OutStr.Objects[iI]) = xx then
      Result := Result + Format('%3.3s %d ', [OutStr[iI], iI]);
end;

function MetaPhone(a: PWideChar; lg: integer): PWideChar; stdcall;
var
  sResult: UnicodeString;

  InStr, TempStr: UnicodeString;
  x, y, SmallestIndex, SmallestValue: integer; //for selection sort
  FirstFlag: boolean;
begin
  OutStr := TUnicodeStringList.Create;
  try
    TempStr := WideUpperCase(a);
    InStr := '';

    for x := 1 to length(TempStr) do
      if pos(copy(TempStr, x, 1), AllowChar) > 0 then
        InStr := InStr + copy(TempStr, x, 1);

    //remove doubles EXCEPT FOR G (ugly exception)
    if length(InStr) > 0 then
    begin
      TempStr := copy(instr, 1, 1);
      for x := 2 to length(InStr) do
        if (copy(instr, x, 1) = 'G') then
          TempStr := TempStr + copy(instr, x, 1)
        else if (copy(instr, x, 1) <> copy(instr, x - 1, 1)) then
          TempStr := TempStr + copy(instr, x, 1);
      InStr := TempStr;
    end;

    //scan input and create result for each rule in output array
    ApplyRules(InStr, Rules);

    //get result - order output stringlist, then translate to string
    //do selection sort - or something like that, anyway :-)
    for x := 0 to OutStr.count - 1 do
    begin
      SmallestIndex := x;
      SmallestValue := integer(OutStr.objects[x]);
      for y := x to OutStr.count - 1 do
        if integer(OutStr.objects[y]) < SmallestValue then
        begin
          SmallestIndex := y;
          SmallestValue := integer(OutStr.objects[y]);
        end;
      if SmallestIndex > x then //do swap with smallest
        OutStr.Exchange(x, SmallestIndex);
    end;

    FirstFlag := False;
    for x := 0 to OutStr.Count - 1 do
    begin
      sResult := sResult + OutStr[x];
      if Integer(OutStr.Objects[x]) = 1 then
        FirstFlag := True;
    end;

    //the following is a fix for words beginning with vowels
    //if there isn't a translation of the first character,
    //it adds whatever the first character is
    if (not FirstFlag) and (Length(Instr) > 0) then
      sResult := Copy(Instr, 1, 1) + sResult;
    Result := PWideChar(sResult);
  finally
    OutStr.Free;
  end;
end;

end.

