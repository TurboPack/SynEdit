{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
CONTENTS:
  Misc types, constants, and procedures used in FMX printing and previewing.
  Platform-independent - no VCL or FMX framework dependencies.
-------------------------------------------------------------------------------}

unit FMX.SynEditPrintTypes;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.SysUtils;

const
  { Default margins in millimeters }
  DefLeft = 25;
  DefRight = 15;
  DefTop = 25;
  DefBottom = 25;
  DefHeader = 15;
  DefFooter = 15;
  DefLeftHFTextIndent = 2;
  DefRightHFTextIndent = 2;
  DefHFInternalMargin = 0.5;
  DefGutter = 0;

type
  { Frame around header/footer }
  TFrameType = (ftLine, ftBox, ftShaded);
  TFrameTypes = set of TFrameType;

  { Margin units (internally always stored in mm) }
  TUnitSystem = (usMM, usCM, usInch, muThousandthsOfInches);

  { Print status events }
  TSynPrintStatus = (psBegin, psNewPage, psEnd);

  { Event types }
  TSynPrintStatusEvent = procedure(Sender: TObject; Status: TSynPrintStatus;
    PageNumber: Integer; var Abort: Boolean) of object;
  TSynPrintLineEvent = procedure(Sender: TObject; LineNumber,
    PageNumber: Integer) of object;

  { Line info record - maps source lines to printed pages }
  TLineInfo = record
    LineIndex: Integer;
    PageIndex: Integer;
  end;

{ Converts an integer to Roman numeral string }
function IntToRoman(Value: Integer): string;

implementation

function IntToRoman(Value: Integer): string;
begin
  Result := '';
  while Value >= 1000 do begin
    Result := Result + 'M';
    Value := Value - 1000;
  end;

  if Value >= 900 then
  begin
    Result := Result + 'CM';
    Value := Value - 900;
  end;

  while Value >= 500 do
  begin
    Result := Result + 'D';
    Value := Value - 500;
  end;

  if Value >= 400 then
  begin
    Result := Result + 'CD';
    Value := Value - 400;
  end;

  while Value >= 100 do
  begin
    Result := Result + 'C';
    Value := Value - 100;
  end;

  if Value >= 90 then
  begin
    Result := Result + 'XC';
    Value := Value - 90;
  end;

  while Value >= 50 do
  begin
    Result := Result + 'L';
    Value := Value - 50;
  end;

  if Value >= 40 then
  begin
    Result := Result + 'XL';
    Value := Value - 40;
  end;

  while Value >= 10 do
  begin
    Result := Result + 'X';
    Value := Value - 10;
  end;

  if Value >= 9 then
  begin
    Result := Result + 'IX';
    Value := Value - 9;
  end;

  while Value >= 5 do
  begin
    Result := Result + 'V';
    Value := Value - 5;
  end;

  if Value >= 4 then
  begin
    Result := Result + 'IV';
    Value := Value - 4;
  end;

  while Value > 0 do
  begin
    Result := Result + 'I';
    Dec(Value);
  end;
end;

end.
