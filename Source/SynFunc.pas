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
  System.Classes, System.Types, Winapi.Windows, Vcl.Controls, Vcl.Forms,
  Vcl.Graphics, Vcl.Themes;

type
{$IF COMPILERVERSION < 29}
  FixedInt = LongInt;
{$IFEND}

{$IF COMPILERVERSION < 36}
  TSynInt64 = Integer;
{$ELSE}
  TSynInt64 = Int64;
{$IFEND}

{$IF COMPILERVERSION < 36}
  TSynNativeInt = Integer;
{$ELSE}
  TSynNativeInt = NativeInt;
{$IFEND}

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

  TRectFHelper = record helper for TRectF
  public
    class function FromRect(const ASource: TRect): TRectF; static; inline;
  end;

  TStringsHelper = class helper for TStrings
  strict private
    {$IF COMPILERVERSION < 31}function GetUseLocale: Boolean; inline;{$IFEND}
    {$IF COMPILERVERSION < 31}function GetTrailingLineBreak: Boolean; inline;{$IFEND}
    {$IF COMPILERVERSION < 31}procedure SetUseLocale(const AValue: Boolean); inline;{$IFEND}
    {$IF COMPILERVERSION < 31}procedure SetTrailingLineBreak(const AValue: Boolean); inline;{$IFEND}
  public
    procedure DeleteNative(const AIndex: NativeInt); inline;
    function GetItemsNative(const AIndex: NativeInt): string; inline;
    function GetObjectsNative(const AIndex: NativeInt): TObject; inline;
    procedure SetItemsNative(const AIndex: NativeInt; const AValue: string); inline;
    procedure SetObjectsNative(const AIndex: NativeInt; const AValue: TObject); inline;
    {$IF COMPILERVERSION < 31}function Updating: Boolean;{$IFEND}
    {$IF COMPILERVERSION < 31}property UseLocale: Boolean read GetUseLocale write SetUseLocale;{$IFEND}
    {$IF COMPILERVERSION < 31}property TrailingLineBreak: Boolean read GetTrailingLineBreak write SetTrailingLineBreak;{$IFEND}
    property ItemsNative[const AIndex: NativeInt]: string read GetItemsNative write SetItemsNative;
    property ObjectsNative[const AIndex: NativeInt]: TObject read GetObjectsNative write SetObjectsNative;
  end;

{$IF COMPILERVERSION < 34}TControlHelper = class helper for TControl
  strict private
    {$IF COMPILERVERSION < 33}function GetCurrentPPI: Integer;{$IFEND}
  public
    {$IF COMPILERVERSION < 34}function ClientToScreen(const ARect: TRect): TRect; overload;{$IFEND}
    {$IF COMPILERVERSION < 32}function FCurrentPPI: Integer;{$IFEND}
    {$IF COMPILERVERSION < 31}procedure ScaleForPPI(ANewPPI: Integer);{$IFEND}
    {$IF COMPILERVERSION < 33}property CurrentPPI: Integer read GetCurrentPPI;{$IFEND}
  end;{$IFEND}

{$IF COMPILERVERSION < 34}TScreenHelper = class helper for TScreen
  public
    function DefaultPixelsPerInch: Integer;
  end;{$IFEND}

{$IF COMPILERVERSION < 32}TThreadHelper = class helper for TThread
  public
    class procedure ForceQueue(const AThread: TThread; const AThreadProc: TThreadProcedure; ADelay: Integer = 0); overload; static;
  end;{$IFEND}

{$IF COMPILERVERSION < 33}TCustomStyleServicesHelper = class helper for TCustomStyleServices
  public
    function DrawElement(ADC: HDC; ADetails: TThemedElementDetails; const AR: TRect; AClipRect: PRect = nil; ADPI: Integer = 0): Boolean; overload;
  end;{$IFEND}

{$IF COMPILERVERSION < 36}TFontHelper = class helper for TFont
  strict private
{$IF COMPILERVERSION < 36}function GetIsDPIRelated: Boolean;{$IFEND}
{$IF COMPILERVERSION < 36}function GetIsScreenFont: Boolean;{$IFEND}
{$IF COMPILERVERSION < 36}procedure SetIsDPIRelated(const AValue: Boolean);{$IFEND}
{$IF COMPILERVERSION < 36}procedure SetIsScreenFont(const AValue: Boolean);{$IFEND}
  public
{$IF COMPILERVERSION < 36}property IsDPIRelated: Boolean read GetIsDPIRelated write SetIsDPIRelated;{$IFEND}
{$IF COMPILERVERSION < 36}property IsScreenFont: Boolean read GetIsScreenFont write SetIsScreenFont;{$IFEND}
  end;{$IFEND}

{$IF COMPILERVERSION < 34}function StyleServices(AControl: TControl = nil): TCustomStyleServices;{$IFEND}

{$IF COMPILERVERSION < 33}function GetSystemMetricsForDpi(nIndex: Integer; dpi: UINT): Integer; stdcall;{$IFEND}

function Bounds(ALeft, ATop, ARight, ABottom: Int64): TRect; overload;inline;

procedure InflateRect(var ARect: TRect; const ADX, ADY: Int64); overload; inline;

function MulDiv(const ANumber, ANumerator, ADenominator: Int64): Int64; overload;
function MulDiv(const ANumber, ANumerator, ADenominator: Int32): Int32; overload; inline;

function Point(AX, AY: Int64): TPoint; overload; inline;

function PosNative(const ASubStr, AStr: string; const AOffset: NativeInt = 1): Int32; inline;

function Rect(ALeft, ATop, ARight, ABottom: Int64): TRect; overload; inline;

function RoundNative(const AValue: Real): NativeInt; inline;
function RoundSyn(const AValue: Real): TSynNativeInt; inline;

function StringOfChar(AChar: Char; const ACount: Int64): string; overload; inline;

function ToByte(const AValue: Int64): Byte; overload; inline;
function ToByte(const AValue: Int32): Byte; overload; inline;

function ToInt32(const AValue: Int64): Int32; overload; inline;
function ToInt32(const AValue: Int32): Int32; overload; inline;

function ToSynNativeInt(const AValue: NativeInt): TSynNativeInt; inline;

function ToUInt32(const AValue: Int64): UInt32; overload; inline;
function ToUInt32(const AValue: Int32): UInt32; overload; inline;

function ToWord(const AValue: Int64): Word; overload; inline;
function ToWord(const AValue: Int32): Word; overload; inline;

implementation

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

{ TRectFHelper }

class function TRectFHelper.FromRect(const ASource: TRect): TRectF;
begin
  Result := TRectF.Create(ASource.Left, ASource.Top, ASource.Right, ASource.Bottom);
end;

{ TStringsHelper }

procedure TStringsHelper.DeleteNative(const AIndex: NativeInt);
begin
  Delete(Int32(AIndex));
end;

function TStringsHelper.GetItemsNative(const AIndex: NativeInt): string;
begin
  Result := Strings[Int32(AIndex)];
end;

function TStringsHelper.GetObjectsNative(const AIndex: NativeInt): TObject;
begin
  Result := Objects[Int32(AIndex)];
end;

{$IF COMPILERVERSION < 31}function TStringsHelper.GetUseLocale: Boolean;
begin
  Result := False;
end;{$IFEND}

{$IF COMPILERVERSION < 31}function TStringsHelper.GetTrailingLineBreak: Boolean;
begin
  Result := False;
end;{$IFEND}

{$IF COMPILERVERSION < 31}procedure TStringsHelper.SetUseLocale(const AValue: Boolean);
begin
end;{$IFEND}

{$IF COMPILERVERSION < 31}procedure TStringsHelper.SetTrailingLineBreak(const AValue: Boolean);
begin
end;{$IFEND}

procedure TStringsHelper.SetItemsNative(const AIndex: NativeInt; const AValue: string);
begin
  Strings[Int32(AIndex)] := AValue;
end;

procedure TStringsHelper.SetObjectsNative(const AIndex: NativeInt; const AValue: TObject);
begin
  Objects[Int32(AIndex)] := AValue;
end;

{$IF COMPILERVERSION < 31}function TStringsHelper.Updating: Boolean;
begin
  Result := UpdateCount > 0;
end;{$IFEND}

{ TControlHelper }

{$IF COMPILERVERSION < 34}function TControlHelper.ClientToScreen(const ARect: TRect): TRect;
var
  lOrigin: TPoint;
begin
  Result := ARect;
  lOrigin := ClientOrigin;
  OffsetRect(Result, lOrigin.X, lOrigin.Y);
end;{$IFEND}

{$IF COMPILERVERSION < 32}function TControlHelper.FCurrentPPI: Integer;
begin
  Result := Screen.PixelsPerInch;
end;{$IFEND}

{$IF COMPILERVERSION < 33}function TControlHelper.GetCurrentPPI: Integer;
begin
  Result := Screen.PixelsPerInch;
end;{$IFEND}

{$IF COMPILERVERSION < 31}procedure TControlHelper.ScaleForPPI(ANewPPI: Integer);
begin
end;{$IFEND}

{ TScreenHelper }

{$IF COMPILERVERSION < 34}function TScreenHelper.DefaultPixelsPerInch: Integer;
begin
  Result := Winapi.Windows.USER_DEFAULT_SCREEN_DPI;
end;{$IFEND}

{$IF COMPILERVERSION < 32}class procedure TThreadHelper.ForceQueue(const AThread: TThread; const AThreadProc: TThreadProcedure; ADelay: Integer);
begin
  TThread.Queue(AThread, AThreadProc);
end;{$IFEND}

{ TCustomStyleServicesHelper }

{$IF COMPILERVERSION < 33}function TCustomStyleServicesHelper.DrawElement(ADC: HDC; ADetails: TThemedElementDetails; const AR: TRect; AClipRect: PRect; ADPI: Integer): Boolean;
begin
  Result := DrawElement(ADC, ADetails, AR, AClipRect);
end;{$IFEND}

{ TFontHelper }

{$IF COMPILERVERSION < 36}function TFontHelper.GetIsDPIRelated: Boolean;
begin
  Result := False;
end;{$IFEND}

{$IF COMPILERVERSION < 36}function TFontHelper.GetIsScreenFont: Boolean;
begin
  Result := False;
end;{$IFEND}

{$IF COMPILERVERSION < 36}procedure TFontHelper.SetIsDPIRelated(const AValue: Boolean);
begin
end;{$IFEND}

{$IF COMPILERVERSION < 36}procedure TFontHelper.SetIsScreenFont(const AValue: Boolean);
begin
end;{$IFEND}

{$IF COMPILERVERSION < 34}function StyleServices(AControl: TControl): TCustomStyleServices;
begin
  Result := Vcl.Themes.StyleServices;
end;{$IFEND}

{$IF COMPILERVERSION < 33}
{$WARN SYMBOL_PLATFORM OFF}
function GetSystemMetricsForDpi(nIndex: Integer; dpi: UINT): Integer; stdcall; external 'user32.dll' name 'GetSystemMetricsForDpi' delayed;
{$WARN SYMBOL_PLATFORM ON}
{$IFEND}

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

function RoundSyn(const AValue: Real): TSynNativeInt; inline;
begin
  Result := TSynNativeInt(Round(AValue));
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

function ToSynNativeInt(const AValue: NativeInt): TSynNativeInt;
begin
  Result := TSynNativeInt(AValue);
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
