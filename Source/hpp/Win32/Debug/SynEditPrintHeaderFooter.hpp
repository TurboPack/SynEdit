// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPrintHeaderFooter.pas' rev: 33.00 (Windows)

#ifndef SyneditprintheaderfooterHPP
#define SyneditprintheaderfooterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <SynEditPrintTypes.hpp>
#include <SynEditPrintMargins.hpp>
#include <SynUnicode.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditprintheaderfooter
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS THeaderFooterItem;
class DELPHICLASS TLineInfo;
class DELPHICLASS THeaderFooter;
class DELPHICLASS THeader;
class DELPHICLASS TFooter;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION THeaderFooterItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FText;
	Vcl::Graphics::TFont* FFont;
	int FLineNumber;
	System::Classes::TAlignment FAlignment;
	int FIndex;
	System::UnicodeString __fastcall GetAsString();
	void __fastcall SetAsString(const System::UnicodeString Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	
public:
	__fastcall THeaderFooterItem();
	__fastcall virtual ~THeaderFooterItem();
	System::UnicodeString __fastcall GetText(int NumPages, int PageNum, bool Roman, System::UnicodeString Title, System::UnicodeString ATime, System::UnicodeString ADate);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=FAlignment, nodefault};
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property int LineNumber = {read=FLineNumber, write=FLineNumber, nodefault};
	__property System::UnicodeString Text = {read=FText, write=FText};
};

#pragma pack(pop)

enum DECLSPEC_DENUM THeaderFooterType : unsigned char { hftHeader, hftFooter };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TLineInfo : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int LineHeight;
	int MaxBaseDist;
public:
	/* TObject.Create */ inline __fastcall TLineInfo() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TLineInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THeaderFooter : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	THeaderFooterType FType;
	Syneditprinttypes::TFrameTypes FFrameTypes;
	System::Uitypes::TColor FShadedColor;
	System::Uitypes::TColor FLineColor;
	System::Classes::TList* FItems;
	Vcl::Graphics::TFont* FDefaultFont;
	System::UnicodeString FDate;
	System::UnicodeString FTime;
	int FNumPages;
	System::UnicodeString FTitle;
	Syneditprintmargins::TSynEditPrintMargins* FMargins;
	int FFrameHeight;
	Vcl::Graphics::TPen* FOldPen;
	Vcl::Graphics::TBrush* FOldBrush;
	Vcl::Graphics::TFont* FOldFont;
	bool FRomanNumbers;
	System::Classes::TList* FLineInfo;
	int FLineCount;
	bool FMirrorPosition;
	void __fastcall SetDefaultFont(Vcl::Graphics::TFont* const Value);
	void __fastcall DrawFrame(Vcl::Graphics::TCanvas* ACanvas);
	void __fastcall CalcHeight(Vcl::Graphics::TCanvas* ACanvas);
	void __fastcall SaveFontPenBrush(Vcl::Graphics::TCanvas* ACanvas);
	void __fastcall RestoreFontPenBrush(Vcl::Graphics::TCanvas* ACanvas);
	System::UnicodeString __fastcall GetAsString();
	void __fastcall SetAsString(const System::UnicodeString Value);
	
public:
	__fastcall THeaderFooter();
	__fastcall virtual ~THeaderFooter();
	int __fastcall Add(System::UnicodeString Text, Vcl::Graphics::TFont* Font, System::Classes::TAlignment Alignment, int LineNumber);
	void __fastcall Delete(int Index);
	void __fastcall Clear();
	int __fastcall Count();
	THeaderFooterItem* __fastcall Get(int Index);
	void __fastcall SetPixPrInch(int Value);
	void __fastcall InitPrint(Vcl::Graphics::TCanvas* ACanvas, int NumPages, System::UnicodeString Title, Syneditprintmargins::TSynEditPrintMargins* Margins);
	void __fastcall Print(Vcl::Graphics::TCanvas* ACanvas, int PageNum);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall FixLines();
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	
__published:
	__property Syneditprinttypes::TFrameTypes FrameTypes = {read=FFrameTypes, write=FFrameTypes, default=1};
	__property System::Uitypes::TColor ShadedColor = {read=FShadedColor, write=FShadedColor, default=12632256};
	__property System::Uitypes::TColor LineColor = {read=FLineColor, write=FLineColor, default=0};
	__property Vcl::Graphics::TFont* DefaultFont = {read=FDefaultFont, write=SetDefaultFont};
	__property bool RomanNumbers = {read=FRomanNumbers, write=FRomanNumbers, default=0};
	__property bool MirrorPosition = {read=FMirrorPosition, write=FMirrorPosition, default=0};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION THeader : public THeaderFooter
{
	typedef THeaderFooter inherited;
	
public:
	__fastcall THeader();
public:
	/* THeaderFooter.Destroy */ inline __fastcall virtual ~THeader() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFooter : public THeaderFooter
{
	typedef THeaderFooter inherited;
	
public:
	__fastcall TFooter();
public:
	/* THeaderFooter.Destroy */ inline __fastcall virtual ~TFooter() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditprintheaderfooter */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPRINTHEADERFOOTER)
using namespace Syneditprintheaderfooter;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditprintheaderfooterHPP
