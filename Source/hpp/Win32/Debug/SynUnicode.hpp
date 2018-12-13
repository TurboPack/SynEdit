// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynUnicode.pas' rev: 33.00 (Windows)

#ifndef SynunicodeHPP
#define SynunicodeHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Clipbrd.hpp>
#include <System.Types.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.TypInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synunicode
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSaveFormat : unsigned char { sfUTF16LSB, sfUTF16MSB, sfUTF8, sfAnsi };

typedef System::Byte TFontCharSet;

enum DECLSPEC_DENUM TSynEncoding : unsigned char { seUTF8, seUTF16LE, seUTF16BE, seAnsi };

typedef System::Set<TSynEncoding, TSynEncoding::seUTF8, TSynEncoding::seAnsi> TSynEncodings;

typedef System::Classes::TFileStream TWideFileStream;

//-- var, const, procedure ---------------------------------------------------
#define SLineBreak L"\r\n"
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 3> UTF8BOM;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 2> UTF16BOMLE;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 2> UTF16BOMBE;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 4> UTF32BOMLE;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 4> UTF32BOMBE;
static const System::WideChar PrivateUseLow = (System::WideChar)(0xe000);
static const System::WideChar PrivateUseHigh = (System::WideChar)(0xf8ff);
static const System::WideChar FillerChar = (System::WideChar)(0xe000);
static const System::WideChar WideNull = (System::WideChar)(0x0);
static const System::WideChar WideTabulator = (System::WideChar)(0x9);
static const System::WideChar WideSpace = (System::WideChar)(0x20);
static const System::WideChar WideLF = (System::WideChar)(0xa);
static const System::WideChar WideLineFeed = (System::WideChar)(0xa);
static const System::WideChar WideVerticalTab = (System::WideChar)(0xb);
static const System::WideChar WideFormFeed = (System::WideChar)(0xc);
static const System::WideChar WideCR = (System::WideChar)(0xd);
static const System::WideChar WideCarriageReturn = (System::WideChar)(0xd);
#define WideCRLF L"\r\n"
static const System::WideChar WideLineSeparator = (System::WideChar)(0x2028);
static const System::WideChar WideParagraphSeparator = (System::WideChar)(0x2029);
static const System::WideChar BOM_LSB_FIRST = (System::WideChar)(0xfeff);
static const System::WideChar BOM_MSB_FIRST = (System::WideChar)(0xfffe);
static const TSaveFormat sfUnicodeLSB = (TSaveFormat)(0);
static const TSaveFormat sfUnicodeMSB = (TSaveFormat)(1);
extern DELPHI_PACKAGE bool Win32PlatformIsUnicode;
extern DELPHI_PACKAGE System::WideChar * __fastcall WCharUpper(System::WideChar * lpsz);
extern DELPHI_PACKAGE unsigned __fastcall WCharUpperBuff(System::WideChar * lpsz, unsigned cchLength);
extern DELPHI_PACKAGE System::WideChar * __fastcall WCharLower(System::WideChar * lpsz);
extern DELPHI_PACKAGE unsigned __fastcall WCharLowerBuff(System::WideChar * lpsz, unsigned cchLength);
extern DELPHI_PACKAGE System::UnicodeString __fastcall SynWideUpperCase(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall SynWideLowerCase(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall SynIsCharAlpha(const System::WideChar C);
extern DELPHI_PACKAGE bool __fastcall SynIsCharAlphaNumeric(const System::WideChar C);
extern DELPHI_PACKAGE int __fastcall WideLastDelimiter(const System::UnicodeString Delimiters, const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall UnicodeStringReplace(const System::UnicodeString S, const System::UnicodeString OldPattern, const System::UnicodeString NewPattern, System::Sysutils::TReplaceFlags Flags);
extern DELPHI_PACKAGE int __fastcall WStrComp(System::WideChar * Str1, System::WideChar * Str2);
extern DELPHI_PACKAGE int __fastcall WStrLComp(System::WideChar * Str1, System::WideChar * Str2, unsigned MaxLen);
extern DELPHI_PACKAGE void __fastcall StrSwapByteOrder(System::WideChar * Str);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WideQuotedStr(const System::UnicodeString S, System::WideChar Quote);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WideExtractQuotedStr(System::WideChar * &Src, System::WideChar Quote);
extern DELPHI_PACKAGE System::UnicodeString __fastcall UnicodeStringOfChar(System::WideChar C, unsigned Count);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WideTrim(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WideTrimLeft(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall WideTrimRight(const System::UnicodeString S);
extern DELPHI_PACKAGE TFontCharSet __fastcall CharSetFromLocale(unsigned Language);
extern DELPHI_PACKAGE int __fastcall CodePageFromLocale(unsigned Language);
extern DELPHI_PACKAGE System::Word __fastcall KeyboardCodePage(void);
extern DELPHI_PACKAGE System::WideChar __fastcall KeyUnicode(char C);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StringToUnicodeStringEx(const System::AnsiString S, System::Word CodePage);
extern DELPHI_PACKAGE System::AnsiString __fastcall UnicodeStringToStringEx(const System::UnicodeString WS, System::Word CodePage);
extern DELPHI_PACKAGE System::Types::TSize __fastcall GetTextSize(HDC DC, System::WideChar * Str, int Count);
extern DELPHI_PACKAGE System::Types::TSize __fastcall TextExtent(Vcl::Graphics::TCanvas* ACanvas, const System::UnicodeString Text);
extern DELPHI_PACKAGE int __fastcall TextWidth(Vcl::Graphics::TCanvas* ACanvas, const System::UnicodeString Text);
extern DELPHI_PACKAGE int __fastcall TextHeight(Vcl::Graphics::TCanvas* ACanvas, const System::UnicodeString Text);
extern DELPHI_PACKAGE void __fastcall TextOut(Vcl::Graphics::TCanvas* ACanvas, int X, int Y, const System::UnicodeString Text);
extern DELPHI_PACKAGE void __fastcall TextRect(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &Rect, int X, int Y, const System::UnicodeString Text);
extern DELPHI_PACKAGE bool __fastcall IsAnsiOnly(const System::UnicodeString WS);
extern DELPHI_PACKAGE bool __fastcall IsUTF8(const System::UnicodeString FileName, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE bool __fastcall IsUTF8(System::Classes::TStream* Stream, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE TSynEncoding __fastcall GetEncoding(const System::UnicodeString FileName, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE TSynEncoding __fastcall GetEncoding(System::Classes::TStream* Stream, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE void __fastcall SaveToFile(const System::UnicodeString WS, const System::UnicodeString FileName, TSynEncoding Encoding, bool WithBom = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall SaveToFile(System::Classes::TStrings* UnicodeStrings, const System::UnicodeString FileName, TSynEncoding Encoding, bool WithBom = true)/* overload */;
extern DELPHI_PACKAGE TSynEncoding __fastcall LoadFromFile(System::Classes::TStrings* UnicodeStrings, const System::UnicodeString FileName, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE TSynEncoding __fastcall LoadFromFile(System::Classes::TStrings* UnicodeStrings, const System::UnicodeString FileName, TSynEncoding Encoding, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE void __fastcall SaveToStream(const System::UnicodeString WS, System::Classes::TStream* Stream, TSynEncoding Encoding, bool WithBom = true)/* overload */;
extern DELPHI_PACKAGE void __fastcall SaveToStream(System::Classes::TStrings* UnicodeStrings, System::Classes::TStream* Stream, TSynEncoding Encoding, bool WithBom = true)/* overload */;
extern DELPHI_PACKAGE TSynEncoding __fastcall LoadFromStream(System::Classes::TStrings* UnicodeStrings, System::Classes::TStream* Stream, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE TSynEncoding __fastcall LoadFromStream(System::Classes::TStrings* UnicodeStrings, System::Classes::TStream* Stream, TSynEncoding Encoding)/* overload */;
extern DELPHI_PACKAGE TSynEncoding __fastcall LoadFromStream(System::Classes::TStrings* UnicodeStrings, System::Classes::TStream* Stream, TSynEncoding Encoding, /* out */ bool &WithBOM)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ClipboardProvidesText(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetClipboardText(void);
extern DELPHI_PACKAGE void __fastcall SetClipboardText(const System::UnicodeString Text);
extern DELPHI_PACKAGE bool __fastcall IsWideCharMappableToAnsi(const System::WideChar WC);
extern DELPHI_PACKAGE bool __fastcall IsUnicodeStringMappableToAnsi(const System::UnicodeString WS);
}	/* namespace Synunicode */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNUNICODE)
using namespace Synunicode;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynunicodeHPP
