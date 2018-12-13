// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPrintMargins.pas' rev: 33.00 (Windows)

#ifndef SyneditprintmarginsHPP
#define SyneditprintmarginsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditPrintTypes.hpp>
#include <SynEditPrinterInfo.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditprintmargins
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynEditPrintMargins;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSynEditPrintMargins : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	double FLeft;
	double FRight;
	double FTop;
	double FBottom;
	double FHeader;
	double FFooter;
	double FLeftHFTextIndent;
	double FRightHFTextIndent;
	double FHFInternalMargin;
	double FGutter;
	bool FMirrorMargins;
	Syneditprinttypes::TUnitSystem FUnitSystem;
	double __fastcall ConvertTo(double Value);
	double __fastcall ConvertFrom(double Value);
	double __fastcall GetBottom();
	double __fastcall GetFooter();
	double __fastcall GetGutter();
	double __fastcall GetHeader();
	double __fastcall GetLeft();
	double __fastcall GetRight();
	double __fastcall GetTop();
	double __fastcall GetLeftHFTextIndent();
	double __fastcall GetRightHFTextIndent();
	double __fastcall GetHFInternalMargin();
	void __fastcall SetBottom(const double Value);
	void __fastcall SetFooter(const double Value);
	void __fastcall SetGutter(const double Value);
	void __fastcall SetHeader(const double Value);
	void __fastcall SetLeft(const double Value);
	void __fastcall SetRight(const double Value);
	void __fastcall SetTop(const double Value);
	void __fastcall SetLeftHFTextIndent(const double Value);
	void __fastcall SetRightHFTextIndent(const double Value);
	void __fastcall SetHFInternalMargin(const double Value);
	
public:
	int PLeft;
	int PRight;
	int PTop;
	int PBottom;
	int PHeader;
	int PFooter;
	int PLeftHFTextIndent;
	int PRightHFTextIndent;
	int PHFInternalMargin;
	int PGutter;
	__fastcall TSynEditPrintMargins();
	void __fastcall InitPage(Vcl::Graphics::TCanvas* ACanvas, int PageNum, Syneditprinterinfo::TSynEditPrinterInfo* PrinterInfo, bool LineNumbers, bool LineNumbersInMargin, int MaxLineNum);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	
__published:
	__property Syneditprinttypes::TUnitSystem UnitSystem = {read=FUnitSystem, write=FUnitSystem, default=0};
	__property double Left = {read=GetLeft, write=SetLeft};
	__property double Right = {read=GetRight, write=SetRight};
	__property double Top = {read=GetTop, write=SetTop};
	__property double Bottom = {read=GetBottom, write=SetBottom};
	__property double Header = {read=GetHeader, write=SetHeader};
	__property double Footer = {read=GetFooter, write=SetFooter};
	__property double LeftHFTextIndent = {read=GetLeftHFTextIndent, write=SetLeftHFTextIndent};
	__property double RightHFTextIndent = {read=GetRightHFTextIndent, write=SetRightHFTextIndent};
	__property double HFInternalMargin = {read=GetHFInternalMargin, write=SetHFInternalMargin};
	__property double Gutter = {read=GetGutter, write=SetGutter};
	__property bool MirrorMargins = {read=FMirrorMargins, write=FMirrorMargins, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSynEditPrintMargins() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditprintmargins */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPRINTMARGINS)
using namespace Syneditprintmargins;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditprintmarginsHPP
