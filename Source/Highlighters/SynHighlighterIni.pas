{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterIni.pas, released 2000-04-21.
The Original Code is based on the izIniSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Igor P. Zenkov.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}
{
@abstract(Provides an Ini and toml files highlighter for SynEdit)
@author(Igor P. Zenkov, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999-11-02, converted to SynEdit 2000-04-21)
The SynHighlighterIni unit provides SynEdit with and Ini and toml files highlighter.
Thanks to Primoz Gabrijelcic, Martin Waldenburg and Michael Hieke.
}

unit SynHighlighterIni;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.RegularExpressions,
  Vcl.Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynEditCodeFolding,
  SynUnicode;

type
  TtkTokenKind = (tkComment, tkText, tkSection, tkKey, tkNull, tkNumber,
    tkHex, tkFloat, tkOct, tkSpace, tkString, tkSymbol, tkKeyword,
    tkTrippleQuotedString, tkUnknown);

  TIniHighlightType = (typeIni, typeToml);

  TRangeState = NativeUInt;
const
  rsUnKnown = 0;
  rsMultilineString = 1;
  rsMultilineString2 = 2;
  rsOpenBracketsBase = 1000;

type
  TSynIniSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    FTokenID: TtkTokenKind;
    fRange: TRangeState;
    FOpenSqBrackets: NativeUInt;
    fCommentAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fSectionAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fKeywordAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fIniHighlightType: TIniHighlightType;
    procedure SectionOpenProc;
    procedure KeyProc;
    procedure CRProc;
    procedure EqualProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure StringProc;  // ""
    procedure StringProc1; // ''
    procedure StringEndProc(EndChar: Char);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
    procedure AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings); override;
  published
    property IniHighlightType: TIniHighlightType read fIniHighlightType
      write fIniHighlightType default typeIni;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property SectionAttri: TSynHighlighterAttributes read fSectionAttri
      write fSectionAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri
      write fKeyAttri;
    property KeywordAttri: TSynHighlighterAttributes read fKeywordAttri
      write fKeywordAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  System.SysUtils,
  SynEditStrConst;

constructor TSynIniSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  AddAttribute(fTextAttri);
  fSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  fSectionAttri.Style := [fsBold];
  AddAttribute(fSectionAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey, SYNS_FriendlyAttrKey);
  fKeyAttri.Foreground := clTeal;
  AddAttribute(fKeyAttri);
  fKeywordAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeywordAttri.Foreground := clBlue;
  fKeywordAttri.Style := [fsBold];
  AddAttribute(fKeywordAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clPurple;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsBold];
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  fDefaultFilter := SYNS_FilterINI;
end;

procedure TSynIniSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  Line: Integer;
  SLine: string;
begin
  for Line := FromLine to ToLine do
  begin
    SLine := LinesToScan[Line];

    if (SLine <> '') and
      (GetHighlighterAttriAtRowCol(LinesToScan, Line, 1) = SectionAttri)
    then
      FoldRanges.StopStartFoldRange(Line + 1, 0)
    else
      FoldRanges.NoFoldInfo(Line + 1)
  end;
end;

{ Create }

procedure TSynIniSyn.SectionOpenProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    if FIniHighlightType = typeToml then
    begin
      Inc(FOpenSqBrackets);
      fTokenID := tkSymbol;
    end
    else
      fTokenID := tkText;

    Inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  fTokenID := tkSection;
  Inc(Run);

  if FLine[Run]  = '[' then
  begin
    // table array
    Inc(Run);
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      ']':
        begin
          Inc(Run);
          if FLine[Run]  = ']' then
          begin
            // table array
            Inc(Run);
          end;
          Break
        end;
      #10: Break;
      #13: Break;
    else Inc(Run);
    end;
end;

procedure TSynIniSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
  if fRange > rsOpenBracketsBase then
    FOpenSqBrackets := fRange - rsOpenBracketsBase
  else
    FOpenSqBrackets := 0;
end;

procedure TSynIniSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
    else Inc(Run);
  end;
end;

procedure TSynIniSyn.EqualProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynIniSyn.KeyProc;
begin
  fTokenID := tkKey;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '=': Break;
      #10: Break;
      #13: Break;
      else Inc(Run);
    end;
end;

procedure TSynIniSyn.TextProc;

begin
  if (Run = 0) and (FOpenSqBrackets <= 0) then
    KeyProc
  else
  begin
    if (fIniHighlightType = typeToml) and
      CharInSet(fLine[Run], ['[', ']', '{', '}'])  then
    begin
      if FLine[Run] = '[' then
        Inc(FOpenSqBrackets)
      else if FLine[Run] = ']' then
        Dec(FOpenSqBrackets);

      fTokenID := tkSymbol;
      Inc(Run);
    end
    else
    begin
      fTokenID := tkText;
      Inc(Run);
      while FLine[Run] <> #0 do
        if IsIdentChar(FLine[Run]) then
          Inc(Run)
        else
          Break;

      if (fIniHighlightType = typeToml) and
        ((GetToken.ToLower = 'true') or
         (GetToken.ToLower = 'false'))
      then
        fTokenId := tkKeyword;
    end;
  end;
end;

procedure TSynIniSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynIniSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynIniSyn.NumberProc;
type
  TNumberState =
    (
    nsStart,
    nsDotFound,
    nsHex,
    nsOct,
    nsBinary,
    nsExpFound
    );

var
  temp: WideChar;
  State: TNumberState;

  function CheckSpecialCases: Boolean;
  begin
    case temp of
      // Look for dot (.)
      '.': begin
        // .45
        if CharInSet(FLine[Run], ['0'..'9']) then
        begin
          Inc(Run);
          fTokenID := tkFloat;
          State := nsDotFound;
        // Non-number dot
        end else begin
          fTokenID := tkText;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      '+', '-': begin
        if not CharInSet(FLine[Run], ['.', '0'..'9']) then
        begin
          fTokenID := tkText;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      // Look for zero (0)
      '0': begin
        temp := FLine[Run];
        // 0x123ABC
        if CharInSet(temp, ['x', 'X']) then begin
          Inc (Run);
          fTokenID := tkHex;
          State := nsHex;
        // 0o123
        end else if CharInSet(temp, ['o', 'O']) then begin
          Inc (Run);
          fTokenID := tkOct;
          State := nsOct;
        // 0.45
        end else if CharInSet(temp, ['b', 'B']) then begin
          Inc (Run);
          fTokenID := tkOct;
          State := nsBinary;
        // 0.45
        end else if temp = '.' then begin
          Inc (Run);
          State := nsDotFound;
          fTokenID := tkFloat;
        end; // if
      end; // ZERO
    end; // case

    Result := True;
  end; // CheckSpecialCases

  function HandleBadNumber: Boolean;
  begin
    Result := False;
    fTokenID := tkUnknown;
    // Ignore all tokens till end of "number"
    while IsIdentChar(FLine[Run]) or (FLine[Run] = '.') do
      Inc (Run);
  end; // HandleBadNumber

  function HandleExponent: Boolean;
  begin
    State := nsExpFound;
    fTokenID := tkFloat;
    // Skip e[+/-]
    if CharInSet(FLine[Run+1], ['+', '-']) then
      Inc (Run);
    // Invalid token : 1.0e
    if not CharInSet(FLine[Run+1], ['0'..'9']) then begin
      Inc (Run);
      Result := HandleBadNumber;
      Exit;
    end; // if

    Result := True;
  end; // HandleExponent

  function CheckStart: Boolean;
  begin
    // Allow underscores inside the number
    if temp = '_' then begin
      if CharInSet(FLine[Run + 1], ['0'..'9']) then
        Result := True
      else
        Result := HandleBadNumber;
    // 1234
    end else if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    //123e4
    end else if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 123.45
    end else if temp = '.' then begin
      State := nsDotFound;
      fTokenID := tkFloat;
      Result := True;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckStart

  function CheckDotFound: Boolean;
  begin
    // Allow underscores inside the number
    if temp = '_' then begin
      if CharInSet(FLine[Run - 1], ['0'..'9']) and
        CharInSet(FLine[Run + 1], ['0'..'9'])
      then
        Result := True
      else
        Result := HandleBadNumber;
    // 1.0e4
    end else if CharInSet(temp, ['e', 'E']) then begin
      Result := HandleExponent;
    // 123.45
    end else if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 123.45j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      Result := False;
    // 123.45.45: Error!
    end else if temp = '.' then begin
      Result := False;
      HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckDotFound

  function CheckSpecialInt(ValidChars: TSysCharSet): Boolean;
  begin
    // Allow underscores inside the number
    if temp = '_' then begin
      if CharInSet(FLine[Run - 1], ValidChars) and
        CharInSet(FLine[Run + 1], ValidChars)
      then
        Result := True
      else
        Result := HandleBadNumber;
    end else if CharInSet(temp, ValidChars) then
    begin
      Result := True;
    end else if CharInSet(temp, ['l', 'L']) then begin
      Inc (Run);
      Result := False;
    end else if temp = '.' then begin
      Result := False;
      HandleBadNumber;
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    end else begin
      Result := False;
    end; // if
  end; // CheckHex

  function CheckExpFound: Boolean;
  begin
    // Allow underscores inside the number
    if temp = '_' then begin
      if CharInSet(FLine[Run - 1], ['0'..'9']) and
        CharInSet(FLine[Run + 1], ['0'..'9'])
      then
        Result := True
      else
        Result := HandleBadNumber;
    // 1e+123
    end else if CharInSet(temp, ['0'..'9']) then begin
      Result := True;
    // 1e+123j
    end else if CharInSet(temp, ['j', 'J']) then begin
      Inc (Run);
      Result := False;
    // 1e4.5: Error!
    end else if temp = '.' then begin
      Result := False;
      HandleBadNumber;
    // Error!
    end else if IsIdentChar(temp) then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckExpFound

begin
  State := nsStart;
  fTokenID := tkNumber;

  temp := FLine[Run];
  Inc (Run);

  // Special cases
  if not CheckSpecialCases then
    Exit;

  // Use a state machine to parse numbers
  while True do begin
    temp := FLine[Run];

    case State of
      nsStart:
        if not CheckStart then Exit;
      nsDotFound:
        if not CheckDotFound then Exit;
      nsHex:
        if not CheckSpecialInt(['a'..'f', 'A'..'F', '0'..'9']) then Exit;
      nsOct:
        if not CheckSpecialInt(['0'..'7']) then Exit;
      nsBinary:
        if not CheckSpecialInt(['0'..'1']) then Exit;
      nsExpFound:
        if not CheckExpFound then Exit;
    end; // case

    Inc (Run);
  end; // while
end;

procedure TSynIniSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynIniSyn.AdjustFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings);
var
  I, J: Integer;
begin
  // Last section
  if FoldRanges.Count > 0 then
    FoldRanges.Ranges.List[FoldRanges.Count - 1].ToLine := LinesToScan.Count;

  // Remove empty lines at the bottom
  for I := 0 to FoldRanges.Count - 1 do
    with FoldRanges.Ranges.List[I] do
      for J := ToLine downto FromLine + 1 do
        if LinesToScan[J - 1] = '' then
          Dec(ToLine)
        else
          Break;
end;

// ;
procedure TSynIniSyn.CommentProc;
begin
  fTokenID := tkComment;
  Inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10: Break;
      #13: Break;
      else Inc(Run);
    end;
end;

procedure TSynIniSyn.SpaceProc;
begin
  if (Run = 0) and (FOpenSqBrackets <= 0) then
    KeyProc
  else
  begin
    Inc(Run);
    fTokenID := tkSpace;
    while IsWhiteChar(FLine[Run]) and not IsLineEnd(Run) do Inc(Run);
  end;
end;

procedure TSynIniSyn.StringEndProc(EndChar: Char);
var
  fBackslashCount: Integer;
begin
  fTokenID := tkTrippleQuotedString;

  case FLine[Run] of
    #0:
      begin
        NullProc;
        EXIT;
      end;
    #10:
      begin
        LFProc;
        EXIT;
    end;
    #13:
      begin
        CRProc;
        EXIT;
      end;
  end;

  repeat
    if FLine[Run] = '\' then
    begin
       if FLine[Run + 1] = EndChar then
         begin
           fBackslashCount := 1;

           while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
             fBackslashCount := fBackslashCount + 1;

           if (fBackslashCount mod 2 = 1) then Inc(Run, 2);
       end;
     end;// if FLine[Run]...
    if (FLine[Run]=EndChar) and (FLine[Run+1]=EndChar) and (FLine[Run+2]=EndChar) then begin
      Inc(Run,3);
      fRange:=rsUnknown;
      EXIT;
    end;
    Inc(Run);
  until IsLineEnd(Run);
end;

// ""
procedure TSynIniSyn.StringProc;
var
  fBackslashCount: Integer;
begin
  if Run = 0 then
  begin
    KeyProc;
    Exit;
  end;

  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then begin
    fTokenID := tkTrippleQuotedString;
    Inc(Run, 3);

    fRange:=rsMultilineString;
    while fLine[Run] <> #0 do begin
      case fLine[Run] of

        '\': begin
             { If we're looking at a backslash, and the following character is an
             end quote, and it's preceeded by an odd number of backslashes, then
             it shouldn't mark the end of the string.  If it's preceeded by an
             even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
              if FLine[Run + 1] = #39 then
                begin
                  fBackslashCount := 1;

                  while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                    fBackslashCount := fBackslashCount + 1;

                  if (fBackslashCount mod 2 = 1) then Inc(Run)
              end;
              Inc(Run);
            end;// '\':

        #39:
          if (fLine[Run + 1] = #39) and (fLine[Run + 2] = #39) then begin
            fRange := rsUnKnown;
            Inc(Run, 3);
            EXIT;
          end else
            Inc(Run);
        #10: EXIT;
        #13: EXIT;
        else
          Inc(Run);
      end;
    end;
  end
  else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;

      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = #39 then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then Inc(Run)
             end;
             Inc(Run);
          end;// '\':

      else Inc(Run);
    end; //case
  until (FLine[Run] = #39);
  if FLine[Run] <> #0 then Inc(Run);
end;

// ''
procedure TSynIniSyn.StringProc1;
var
  fBackslashCount: Integer;
begin
  if Run = 0 then
  begin
    KeyProc;
    Exit;
  end;

  fTokenID := tkString;
  if (FLine[Run + 1] = '"') and (FLine[Run + 2] = '"') then
  begin
    fTokenID := tkTrippleQuotedString;
    Inc(Run, 3);

    fRange := rsMultilineString2;
    while fLine[Run] <> #0 do
    begin
      case fLine[Run] of

        '\':begin
               { If we're looking at a backslash, and the following character is an
               end quote, and it's preceeded by an odd number of backslashes, then
               it shouldn't mark the end of the string.  If it's preceeded by an
               even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
               if FLine[Run + 1] = '"' then
                 begin
                   fBackslashCount := 1;

                   while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                     fBackslashCount := fBackslashCount + 1;

                   if (fBackslashCount mod 2 = 1) then Inc(Run)
               end;
               Inc(Run);
            end;// '\':

        '"':
          if (fLine[Run + 1] = '"') and (fLine[Run + 2] = '"') then begin
            fRange := rsUnKnown;
            Inc(Run, 3);
            Exit;
          end else
            Inc(Run);
        #10: Exit;
        #13: Exit;
        else
          Inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
      {The same backslash stuff above...}
      '\':begin
             if FLine[Run + 1] = '"' then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (FLine[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then Inc(Run)
             end;
             Inc(Run);
          end;// '\':

      else Inc(Run);
    end; //case
  until (FLine[Run] = '"');
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynIniSyn.Next;
begin
  fTokenPos := Run;

  case fRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    else
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '''': StringProc;
        '"': StringProc1;
        '+', '-', '.', '0'..'9': NumberProc;
        ';', '#': CommentProc;
        '=' : EqualProc;
        '[': SectionOpenProc;
        else
          if IsWhiteChar(fLine[Run]) then
            SpaceProc
          else
            TextProc;
      end;
  end;

  inherited;
end;

function TSynIniSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeywordAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynIniSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynIniSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynIniSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkText: Result := fTextAttri;
    tkSection: Result := fSectionAttri;
    tkKey: Result := fKeyAttri;
    tkKeyword: Result := fKeywordAttri;
    tkNumber,
    tkHex,
    tkFloat,
    tkOct: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fTextAttri;
    tkTrippleQuotedString: Result := fStringAttri;
    else Result := nil;
  end;
end;

function TSynIniSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynIniSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterINI;
end;

class function TSynIniSyn.GetLanguageName: string;
begin
  Result := SYNS_LangINI;
end;

function TSynIniSyn.GetRange: Pointer;
begin
  if FOpenSqBrackets > 0 then
    Result := Pointer(rsOpenBracketsBase + FOpenSqBrackets)
  else
    Result := Pointer(fRange);
end;

function TSynIniSyn.GetSampleSource: string;
begin
  Result := '; Syntax highlighting'#13#10+
            '[Section]'#13#10+
            'Key=value'#13#10+
            'String="Arial"'#13#10+
            'Number=123456';
end;

class function TSynIniSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangINI;
end;

initialization
  RegisterPlaceableHighlighter(TSynIniSyn);
end.
