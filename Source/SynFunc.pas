{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Description: SynEdit functions
The initial author of this file is Kassebaum Development.
Copyright (c) 2026, all rights reserved.

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

unit SynFunc;

interface

uses
  System.Classes, System.Types;

type
  TPointHelper = record helper for TPoint
  public
    constructor Create(const AX, AY : Int64); overload;
  end;

  TRectHelper = record helper for TRect
  public
    constructor Create(const AOrigin: TPoint; const AWidth, AHeight: Int64); overload;
    procedure SetBottom(const AValue: Int32); overload; inline;
    procedure SetBottom(const AValue: Int64); overload; inline;
    procedure SetLeft(const AValue: Int32); overload; inline;
    procedure SetLeft(const AValue: Int64); overload; inline;
    procedure SetRight(const AValue: Int32); overload; inline;
    procedure SetRight(const AValue: Int64); overload; inline;
    procedure SetTop(const AValue: Int32); overload; inline;
    procedure SetTop(const AValue: Int64); overload; inline;
  end;

  TStringsHelper = class helper for TStrings
  public
    procedure DeleteNative(const AIndex: NativeInt); inline;
    function GetItem(const AIndex: NativeInt): string; inline;
    function GetObjects(const AIndex: NativeInt): TObject; inline;
    procedure SetItem(const AIndex: NativeInt; const AValue: string); inline;
  end;

function Bounds(ALeft, ATop, ARight, ABottom: Int64): TRect; overload;inline;

procedure InflateRect(var ARect: TRect; const ADX, ADY: Int64); overload; inline;

function MulDiv(const ANumber, ANumerator, ADenominator: Int64): Int64; overload;
function MulDiv(const ANumber, ANumerator, ADenominator: Int32): Int32; overload; inline;

function Point(AX, AY: Int64): TPoint; overload; inline;

function PosNative(const ASubStr, AStr: string; const AOffset: NativeInt = 1): Int32; inline;

function Rect(ALeft, ATop, ARight, ABottom: Int64): TRect; overload;inline;

function RoundNative(const AValue: Real): NativeInt; inline;

function StringOfChar(AChar: Char; const ACount: Int64): string; overload; inline;

function ToByte(const AValue: Int64): Byte; overload; inline;
function ToByte(const AValue: Int32): Byte; overload; inline;

function ToInt32(const AValue: Int64): Int32; overload; inline;
function ToInt32(const AValue: Int32): Int32; overload; inline;

function ToUInt32(const AValue: Int64): UInt32; overload; inline;
function ToUInt32(const AValue: Int32): UInt32; overload; inline;

function ToWord(const AValue: Int64): Word; overload; inline;
function ToWord(const AValue: Int32): Word; overload; inline;

implementation

uses
  Winapi.Windows;

{ TPointHelper }

constructor TPointHelper.Create(const AX, AY: Int64);
begin
  X := ToInt32(AX);
  Y := ToInt32(AY);
end;

{ TRectHelper }

constructor TRectHelper.Create(const AOrigin: TPoint; const AWidth, AHeight: Int64);
begin
  Create(AOrigin, Int32(AWidth), Int32(AHeight));
end;

procedure TRectHelper.SetBottom(const AValue: Int64);
begin
  Bottom := Int32(AValue);
end;

procedure TRectHelper.SetLeft(const AValue: Int32);
begin
  Left := AValue;
end;

procedure TRectHelper.SetLeft(const AValue: Int64);
begin
  Left := Int32(AValue);
end;

procedure TRectHelper.SetRight(const AValue: Int32);
begin
  Right := AValue;
end;

procedure TRectHelper.SetRight(const AValue: Int64);
begin
  Right := Int32(AValue);
end;

procedure TRectHelper.SetBottom(const AValue: Int32);
begin
  Bottom := AValue;
end;

procedure TRectHelper.SetTop(const AValue: Int64);
begin
  Top := Int32(AValue);
end;

procedure TRectHelper.SetTop(const AValue: Int32);
begin
  Top := AValue;
end;

{ TStringsHelper }

procedure TStringsHelper.DeleteNative(const AIndex: NativeInt);
begin
  Delete(Int32(AIndex));
end;

function TStringsHelper.GetItem(const AIndex: NativeInt): string;
begin
  Result := Strings[Int32(AIndex)];
end;

function TStringsHelper.GetObjects(const AIndex: NativeInt): TObject;
begin
  Result := Objects[Int32(AIndex)];
end;

procedure TStringsHelper.SetItem(const AIndex: NativeInt; const AValue: string);
begin
  Strings[Int32(AIndex)] := AValue;
end;

function Bounds(ALeft, ATop, ARight, ABottom: Int64): TRect;
begin
  Result := System.Types.Bounds(Int32(ALeft), Int32(ATop), Int32(ARight), Int32(ABottom));
end;

procedure InflateRect(var ARect: TRect; const ADX, ADY: Int64);
begin
  System.Types.InflateRect(ARect, Int32(ADX), Int32(ADY));
end;

function MulDiv(const ANumber, ANumerator, ADenominator: Int32): Int32;
begin
  Result := Winapi.Windows.MulDiv(ANumber, ANumerator, ADenominator);
end;

function MulDiv(const ANumber, ANumerator, ADenominator: Int64): Int64;
var
  lValue: Int64;
begin
  if ADenominator <> 0 then
  begin
    lValue := ANumber * ANumerator;
    Result := Round(lValue / ADenominator);
  end
  else
    Result := ANumber;
end;

function Point(AX, AY: Int64): TPoint;
begin
  Result.X := Int32(AX);
  Result.Y := Int32(AY);
end;

function PosNative(const ASubStr, AStr: string; const AOffset: NativeInt): Int32;
begin
  Result := System.Pos(ASubStr, AStr, Int32(AOffset));
end;

function Rect(ALeft, ATop, ARight, ABottom: Int64): TRect;
begin
  Result := System.Classes.Rect(Int32(ALeft), Int32(ATop), Int32(ARight), Int32(ABottom));
end;

function RoundNative(const AValue: Real): NativeInt; inline;
begin
  Result := NativeInt(Round(AValue));
end;

function StringOfChar(AChar: Char; const ACount: Int64): string; overload; inline;
begin
  Result := System.StringOfChar(AChar, Int32(ACount));
end;

function ToByte(const AValue: Int64): Byte;
begin
  Result := Byte(AValue);
end;

function ToByte(const AValue: Int32): Byte;
begin
  Result := Byte(AValue);
end;

function ToInt32(const AValue: Int64): Int32;
begin
  Result := Int32(AValue);
end;

function ToInt32(const AValue: Int32): Int32;
begin
  Result := AValue;
end;

function ToUInt32(const AValue: Int64): UInt32;
begin
  Result := UInt32(AValue);
end;

function ToUInt32(const AValue: Int32): UInt32;
begin
  Result := AValue;
end;

function ToWord(const AValue: Int64): Word;
begin
  Result := Word(AValue);
end;

function ToWord(const AValue: Int32): Word;
begin
  Result := Word(AValue);
end;

end.
