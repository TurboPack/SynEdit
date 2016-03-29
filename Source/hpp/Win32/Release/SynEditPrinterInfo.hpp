// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPrinterInfo.pas' rev: 31.00 (Windows)

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
	void __fastcall FillDefault(void);
	int __fastcall GetBottomGutter(void);
	int __fastcall GetLeftGutter(void);
	int __fastcall GetPhysicalHeight(void);
	int __fastcall GetPhysicalWidth(void);
	int __fastcall GetPrintableHeight(void);
	int __fastcall GetPrintableWidth(void);
	int __fastcall GetRightGutter(void);
	int __fastcall GetTopGutter(void);
	int __fastcall GetXPixPrInch(void);
	int __fastcall GetYPixPrInch(void);
	float __fastcall GetXPixPrmm(void);
	float __fastcall GetYPixPrmm(void);
	
public:
	void __fastcall UpdatePrinter(void);
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
	/* TObject.Create */ inline __fastcall TSynEditPrinterInfo(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TSynEditPrinterInfo(void) { }
	
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
