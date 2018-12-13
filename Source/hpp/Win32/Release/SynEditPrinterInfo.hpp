// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPrinterInfo.pas' rev: 33.00 (Windows)

#ifndef SyneditprinterinfoHPP
#define SyneditprinterinfoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Printers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditprinterinfo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynEditPrinterInfo;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditPrinterInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FPhysicalWidth;
	int FPhysicalHeight;
	int FPrintableWidth;
	int FPrintableHeight;
	int FLeftGutter;
	int FRightGutter;
	int FTopGutter;
	int FBottomGutter;
	int FXPixPrInch;
	int FYPixPrInch;
	float FXPixPrmm;
	float FYPixPrmm;
	bool FIsUpdated;
	void __fastcall FillDefault();
	int __fastcall GetBottomGutter();
	int __fastcall GetLeftGutter();
	int __fastcall GetPhysicalHeight();
	int __fastcall GetPhysicalWidth();
	int __fastcall GetPrintableHeight();
	int __fastcall GetPrintableWidth();
	int __fastcall GetRightGutter();
	int __fastcall GetTopGutter();
	int __fastcall GetXPixPrInch();
	int __fastcall GetYPixPrInch();
	float __fastcall GetXPixPrmm();
	float __fastcall GetYPixPrmm();
	
public:
	void __fastcall UpdatePrinter();
	int __fastcall PixFromLeft(double mmValue);
	int __fastcall PixFromRight(double mmValue);
	int __fastcall PixFromTop(double mmValue);
	int __fastcall PixFromBottom(double mmValue);
	__property int PhysicalWidth = {read=GetPhysicalWidth, nodefault};
	__property int PhysicalHeight = {read=GetPhysicalHeight, nodefault};
	__property int PrintableWidth = {read=GetPrintableWidth, nodefault};
	__property int PrintableHeight = {read=GetPrintableHeight, nodefault};
	__property int LeftGutter = {read=GetLeftGutter, nodefault};
	__property int RightGutter = {read=GetRightGutter, nodefault};
	__property int TopGutter = {read=GetTopGutter, nodefault};
	__property int BottomGutter = {read=GetBottomGutter, nodefault};
	__property int XPixPrInch = {read=GetXPixPrInch, nodefault};
	__property int YPixPrInch = {read=GetYPixPrInch, nodefault};
	__property float XPixPrmm = {read=GetXPixPrmm};
	__property float YPixPrmm = {read=GetYPixPrmm};
public:
	/* TObject.Create */ inline __fastcall TSynEditPrinterInfo() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSynEditPrinterInfo() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditprinterinfo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPRINTERINFO)
using namespace Syneditprinterinfo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditprinterinfoHPP
