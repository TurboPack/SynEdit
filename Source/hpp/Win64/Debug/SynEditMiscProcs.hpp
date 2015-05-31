// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditMiscProcs.pas' rev: 29.00 (Windows)

#ifndef SyneditmiscprocsHPP
#define SyneditmiscprocsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.Math.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditmiscprocs
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<int, 134217727> TIntArray;

typedef TIntArray *PIntArray;

typedef System::UnicodeString __fastcall (*TConvertTabsProc)(const System::UnicodeString Line, int TabWidth);

typedef System::UnicodeString __fastcall (*TConvertTabsProcEx)(const System::UnicodeString Line, int TabWidth, bool &HasTabs);

typedef bool __fastcall (__closure *THighlighterAttriProc)(Synedithighlighter::TSynCustomHighlighter* Highlighter, Synedithighlighter::TSynHighlighterAttributes* Attri, System::UnicodeString UniqueAttriName, void * *Params, const int Params_High);

//-- var, const, procedure ---------------------------------------------------
static const int MaxIntArraySize = int(0x7ffffff);
extern DELPHI_PACKAGE int __fastcall MinMax(int x, int mi, int ma);
extern DELPHI_PACKAGE void __fastcall SwapInt(int &l, int &r);
extern DELPHI_PACKAGE System::Types::TPoint __fastcall MaxPoint(const System::Types::TPoint P1, const System::Types::TPoint P2);
extern DELPHI_PACKAGE System::Types::TPoint __fastcall MinPoint(const System::Types::TPoint P1, const System::Types::TPoint P2);
extern DELPHI_PACKAGE PIntArray __fastcall GetIntArray(unsigned Count, int InitialValue);
extern DELPHI_PACKAGE void __fastcall InternalFillRect(HDC dc, const System::Types::TRect &rcPaint);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConvertTabsEx(const System::UnicodeString Line, int TabWidth, bool &HasTabs);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConvertTabs(const System::UnicodeString Line, int TabWidth);
extern DELPHI_PACKAGE TConvertTabsProc __fastcall GetBestConvertTabsProc(int TabWidth);
extern DELPHI_PACKAGE TConvertTabsProcEx __fastcall GetBestConvertTabsProcEx(int TabWidth);
extern DELPHI_PACKAGE int __fastcall GetExpandedLength(const System::UnicodeString aStr, int aTabWidth);
extern DELPHI_PACKAGE int __fastcall CharIndex2CaretPos(int Index, int TabWidth, const System::UnicodeString Line);
extern DELPHI_PACKAGE int __fastcall CaretPos2CharIndex(int Position, int TabWidth, const System::UnicodeString Line, bool &InsideTabChar);
extern DELPHI_PACKAGE int __fastcall StrScanForCharInCategory(const System::UnicodeString Line, int Start, Synedittypes::TCategoryMethod IsOfCategory);
extern DELPHI_PACKAGE int __fastcall StrRScanForCharInCategory(const System::UnicodeString Line, int Start, Synedittypes::TCategoryMethod IsOfCategory);
extern DELPHI_PACKAGE System::WideChar * __fastcall GetEOL(System::WideChar * Line);
extern DELPHI_PACKAGE System::UnicodeString __fastcall EncodeString(System::UnicodeString s);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DecodeString(System::UnicodeString s);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DeleteTypePrefixAndSynSuffix(System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall EnumHighlighterAttris(Synedithighlighter::TSynCustomHighlighter* Highlighter, bool SkipDuplicates, THighlighterAttriProc HighlighterAttriProc, void * *Params, const int Params_High);
extern DELPHI_PACKAGE void __fastcall SynDrawGradient(Vcl::Graphics::TCanvas* const ACanvas, const System::Uitypes::TColor AStartColor, const System::Uitypes::TColor AEndColor, int ASteps, const System::Types::TRect &ARect, const bool AHorizontal);
}	/* namespace Syneditmiscprocs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITMISCPROCS)
using namespace Syneditmiscprocs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditmiscprocsHPP
