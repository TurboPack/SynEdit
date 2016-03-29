// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynTextDrawer.pas' rev: 31.00 (Windows)

#ifndef SyntextdrawerHPP
#define SyntextdrawerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Math.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syntextdrawer
{
//-- forward type declarations -----------------------------------------------
struct TheFontData;
struct TheSharedFontsInfo;
class DELPHICLASS TheFontsInfoManager;
class DELPHICLASS EheFontStockException;
class DELPHICLASS TheFontStock;
class DELPHICLASS EheTextDrawerException;
class DELPHICLASS TheTextDrawer;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<int, 536870911> TIntegerArray;

typedef TIntegerArray *PIntegerArray;

typedef System::Int8 TheStockFontPatterns;

typedef TheFontData *PheFontData;

struct DECLSPEC_DRECORD TheFontData
{
public:
	System::Uitypes::TFontStyles Style;
	HFONT Handle;
	int CharAdv;
	int CharHeight;
};


typedef System::StaticArray<TheFontData, 16> TheFontsData;

typedef TheFontsData *PheFontsData;

typedef TheSharedFontsInfo *PheSharedFontsInfo;

struct DECLSPEC_DRECORD TheSharedFontsInfo
{
public:
	int RefCount;
	int LockCount;
	Vcl::Graphics::TFont* BaseFont;
	tagLOGFONTW BaseLF;
	bool IsTrueType;
	TheFontsData FontsData;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TheFontsInfoManager : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FFontsInfo;
	PheSharedFontsInfo __fastcall FindFontsInfo(const tagLOGFONTW &LF);
	PheSharedFontsInfo __fastcall CreateFontsInfo(Vcl::Graphics::TFont* ABaseFont, const tagLOGFONTW &LF);
	void __fastcall DestroyFontHandles(PheSharedFontsInfo pFontsInfo);
	void __fastcall RetrieveLogFontForComparison(Vcl::Graphics::TFont* ABaseFont, tagLOGFONTW &LF);
	
public:
	__fastcall TheFontsInfoManager(void);
	__fastcall virtual ~TheFontsInfoManager(void);
	void __fastcall LockFontsInfo(PheSharedFontsInfo pFontsInfo);
	void __fastcall UnLockFontsInfo(PheSharedFontsInfo pFontsInfo);
	PheSharedFontsInfo __fastcall GetFontsInfo(Vcl::Graphics::TFont* ABaseFont);
	void __fastcall ReleaseFontsInfo(PheSharedFontsInfo pFontsInfo);
};

#pragma pack(pop)

enum DECLSPEC_DENUM Syntextdrawer__2 : unsigned char { tooOpaque, tooClipped };

typedef System::Set<Syntextdrawer__2, Syntextdrawer__2::tooOpaque, Syntextdrawer__2::tooClipped> TTextOutOptions;

typedef void __fastcall (__closure *TheExtTextOutProc)(int X, int Y, TTextOutOptions fuOptions, const System::Types::TRect &ARect, const System::UnicodeString Text, int Length);

#pragma pack(push,4)
class PASCALIMPLEMENTATION EheFontStockException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EheFontStockException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EheFontStockException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EheFontStockException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EheFontStockException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EheFontStockException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EheFontStockException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EheFontStockException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EheFontStockException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EheFontStockException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EheFontStockException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EheFontStockException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EheFontStockException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EheFontStockException(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TheFontStock : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	HDC FDC;
	int FDCRefCount;
	TheSharedFontsInfo *FpInfo;
	bool FUsingFontHandles;
	HFONT FCrntFont;
	System::Uitypes::TFontStyles FCrntStyle;
	TheFontData *FpCrntFontData;
	tagLOGFONTW FBaseLF;
	Vcl::Graphics::TFont* __fastcall GetBaseFont(void);
	bool __fastcall GetIsTrueType(void);
	
protected:
	virtual HDC __fastcall InternalGetDC(void);
	virtual void __fastcall InternalReleaseDC(HDC Value);
	virtual HFONT __fastcall InternalCreateFont(System::Uitypes::TFontStyles Style);
	virtual int __fastcall CalcFontAdvance(HDC DC, System::PInteger pCharHeight);
	virtual int __fastcall GetCharAdvance(void);
	virtual int __fastcall GetCharHeight(void);
	virtual PheFontData __fastcall GetFontData(int idx);
	void __fastcall UseFontHandles(void);
	void __fastcall ReleaseFontsInfo(void);
	virtual void __fastcall SetBaseFont(Vcl::Graphics::TFont* Value);
	virtual void __fastcall SetStyle(System::Uitypes::TFontStyles Value);
	__property PheFontData FontData[int idx] = {read=GetFontData};
	__property PheSharedFontsInfo FontsInfo = {read=FpInfo};
	
public:
	__fastcall virtual TheFontStock(Vcl::Graphics::TFont* InitialFont);
	__fastcall virtual ~TheFontStock(void);
	virtual void __fastcall ReleaseFontHandles(void);
	__property Vcl::Graphics::TFont* BaseFont = {read=GetBaseFont};
	__property System::Uitypes::TFontStyles Style = {read=FCrntStyle, write=SetStyle, nodefault};
	__property HFONT FontHandle = {read=FCrntFont, nodefault};
	__property int CharAdvance = {read=GetCharAdvance, nodefault};
	__property int CharHeight = {read=GetCharHeight, nodefault};
	__property bool IsTrueType = {read=GetIsTrueType, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EheTextDrawerException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EheTextDrawerException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EheTextDrawerException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EheTextDrawerException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EheTextDrawerException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EheTextDrawerException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EheTextDrawerException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EheTextDrawerException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EheTextDrawerException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EheTextDrawerException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EheTextDrawerException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EheTextDrawerException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EheTextDrawerException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EheTextDrawerException(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TheTextDrawer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	HDC FDC;
	int FSaveDC;
	TheFontStock* FFontStock;
	Vcl::Graphics::TBitmap* FStockBitmap;
	System::Uitypes::TFontStyles FCalcExtentBaseStyle;
	int FBaseCharWidth;
	int FBaseCharHeight;
	HFONT FCrntFont;
	TIntegerArray *FETODist;
	System::Uitypes::TColor FColor;
	System::Uitypes::TColor FBkColor;
	int FCharExtra;
	int FDrawingCount;
	System::StaticArray<_ABC, 128> FCharABCWidthCache;
	System::StaticArray<int, 128> FCharWidthCache;
	
protected:
	virtual void __fastcall ReleaseETODist(void);
	virtual void __fastcall AfterStyleSet(void);
	virtual void __fastcall DoSetCharExtra(int Value);
	void __fastcall FlushCharABCWidthCache(void);
	bool __fastcall GetCachedABCWidth(unsigned c, _ABC &abc);
	__property HDC StockDC = {read=FDC, nodefault};
	__property int DrawingCount = {read=FDrawingCount, nodefault};
	__property TheFontStock* FontStock = {read=FFontStock};
	__property int BaseCharWidth = {read=FBaseCharWidth, nodefault};
	__property int BaseCharHeight = {read=FBaseCharHeight, nodefault};
	
public:
	__fastcall virtual TheTextDrawer(System::Uitypes::TFontStyles CalcExtentBaseStyle, Vcl::Graphics::TFont* BaseFont);
	__fastcall virtual ~TheTextDrawer(void);
	virtual int __fastcall GetCharWidth(void);
	virtual int __fastcall GetCharHeight(void);
	virtual void __fastcall BeginDrawing(HDC DC);
	virtual void __fastcall EndDrawing(void);
	virtual void __fastcall TextOut(int X, int Y, System::WideChar * Text, int Length);
	virtual void __fastcall ExtTextOut(int X, int Y, TTextOutOptions Options, const System::Types::TRect &ARect, System::WideChar * Text, int Length);
	System::Types::TSize __fastcall TextExtent(const System::UnicodeString Text)/* overload */;
	System::Types::TSize __fastcall TextExtent(System::WideChar * Text, int Count)/* overload */;
	int __fastcall TextWidth(const System::UnicodeString Text)/* overload */;
	int __fastcall TextWidth(System::WideChar * Text, int Count)/* overload */;
	virtual void __fastcall SetBaseFont(Vcl::Graphics::TFont* Value);
	virtual void __fastcall SetBaseStyle(const System::Uitypes::TFontStyles Value);
	virtual void __fastcall SetStyle(System::Uitypes::TFontStyles Value);
	virtual void __fastcall SetForeColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetBackColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetCharExtra(int Value);
	virtual void __fastcall ReleaseTemporaryResources(void);
	__property int CharWidth = {read=GetCharWidth, nodefault};
	__property int CharHeight = {read=GetCharHeight, nodefault};
	__property Vcl::Graphics::TFont* BaseFont = {write=SetBaseFont};
	__property System::Uitypes::TFontStyles BaseStyle = {write=SetBaseStyle, nodefault};
	__property System::Uitypes::TColor ForeColor = {write=SetForeColor, nodefault};
	__property System::Uitypes::TColor BackColor = {write=SetBackColor, nodefault};
	__property System::Uitypes::TFontStyles Style = {write=SetStyle, nodefault};
	__property int CharExtra = {read=FCharExtra, write=SetCharExtra, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 FontStyleCount = System::Int8(0x4);
static const System::Int8 FontStyleCombineCount = System::Int8(0x10);
extern DELPHI_PACKAGE TheFontsInfoManager* __fastcall GetFontsInfoManager(void);
extern DELPHI_PACKAGE bool __fastcall UniversalExtTextOut(HDC DC, int X, int Y, TTextOutOptions Options, const System::Types::TRect &Rect, System::WideChar * Str, int Count, PIntegerArray ETODist);
}	/* namespace Syntextdrawer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNTEXTDRAWER)
using namespace Syntextdrawer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyntextdrawerHPP
