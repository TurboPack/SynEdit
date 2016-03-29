// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditMiscClasses.pas' rev: 31.00 (Windows)

#ifndef SyneditmiscclassesHPP
#define SyneditmiscclassesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Consts.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <System.Win.Registry.hpp>
#include <SynEditTypes.hpp>
#include <SynEditKeyConst.hpp>
#include <SynUnicode.hpp>
#include <System.Math.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditmiscclasses
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynSelectedColor;
class DELPHICLASS TSynGutter;
class DELPHICLASS TSynBookMarkOpt;
class DELPHICLASS TSynGlyph;
class DELPHICLASS ESynMethodChain;
class DELPHICLASS TSynMethodChain;
class DELPHICLASS TSynNotifyEventChain;
class DELPHICLASS TSynInternalImage;
class DELPHICLASS TSynHotKey;
class DELPHICLASS TSynEditSearchCustom;
class DELPHICLASS TBetterRegistry;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSynSelectedColor : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Uitypes::TColor fBG;
	System::Uitypes::TColor fFG;
	System::Classes::TNotifyEvent fOnChange;
	void __fastcall SetBG(System::Uitypes::TColor Value);
	void __fastcall SetFG(System::Uitypes::TColor Value);
	
public:
	__fastcall TSynSelectedColor(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Uitypes::TColor Background = {read=fBG, write=SetBG, default=-16777203};
	__property System::Uitypes::TColor Foreground = {read=fFG, write=SetFG, default=-16777202};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSynSelectedColor(void) { }
	
};


enum DECLSPEC_DENUM TSynGutterBorderStyle : unsigned char { gbsNone, gbsMiddle, gbsRight };

class PASCALIMPLEMENTATION TSynGutter : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Vcl::Graphics::TFont* fFont;
	System::Uitypes::TColor fColor;
	System::Uitypes::TColor fBorderColor;
	int fWidth;
	bool fShowLineNumbers;
	int fDigitCount;
	bool fLeadingZeros;
	bool fZeroStart;
	int fLeftOffset;
	int fRightOffset;
	System::Classes::TNotifyEvent fOnChange;
	System::Uitypes::TCursor fCursor;
	bool fVisible;
	bool fUseFontStyle;
	bool fAutoSize;
	int fAutoSizeDigitCount;
	TSynGutterBorderStyle fBorderStyle;
	int fLineNumberStart;
	bool fGradient;
	System::Uitypes::TColor fGradientStartColor;
	System::Uitypes::TColor fGradientEndColor;
	int fGradientSteps;
	void __fastcall SetAutoSize(const bool Value);
	void __fastcall SetBorderColor(const System::Uitypes::TColor Value);
	void __fastcall SetColor(const System::Uitypes::TColor Value);
	void __fastcall SetDigitCount(int Value);
	void __fastcall SetLeadingZeros(const bool Value);
	void __fastcall SetLeftOffset(int Value);
	void __fastcall SetRightOffset(int Value);
	void __fastcall SetShowLineNumbers(const bool Value);
	void __fastcall SetUseFontStyle(bool Value);
	void __fastcall SetVisible(bool Value);
	void __fastcall SetWidth(int Value);
	void __fastcall SetZeroStart(const bool Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	void __fastcall OnFontChange(System::TObject* Sender);
	void __fastcall SetBorderStyle(const TSynGutterBorderStyle Value);
	void __fastcall SetLineNumberStart(const int Value);
	void __fastcall SetGradient(const bool Value);
	void __fastcall SetGradientStartColor(const System::Uitypes::TColor Value);
	void __fastcall SetGradientEndColor(const System::Uitypes::TColor Value);
	void __fastcall SetGradientSteps(const int Value);
	int __fastcall GetWidth(void);
	
public:
	__fastcall TSynGutter(void);
	__fastcall virtual ~TSynGutter(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AutoSizeDigitCount(int LinesCount);
	System::UnicodeString __fastcall FormatLineNumber(int Line);
	int __fastcall RealGutterWidth(int CharWidth);
	
__published:
	__property bool AutoSize = {read=fAutoSize, write=SetAutoSize, default=0};
	__property TSynGutterBorderStyle BorderStyle = {read=fBorderStyle, write=SetBorderStyle, default=1};
	__property System::Uitypes::TColor Color = {read=fColor, write=SetColor, default=-16777201};
	__property System::Uitypes::TColor BorderColor = {read=fBorderColor, write=SetBorderColor, default=-16777211};
	__property System::Uitypes::TCursor Cursor = {read=fCursor, write=fCursor, default=0};
	__property int DigitCount = {read=fDigitCount, write=SetDigitCount, default=4};
	__property Vcl::Graphics::TFont* Font = {read=fFont, write=SetFont};
	__property bool LeadingZeros = {read=fLeadingZeros, write=SetLeadingZeros, default=0};
	__property int LeftOffset = {read=fLeftOffset, write=SetLeftOffset, default=16};
	__property int RightOffset = {read=fRightOffset, write=SetRightOffset, default=2};
	__property bool ShowLineNumbers = {read=fShowLineNumbers, write=SetShowLineNumbers, default=0};
	__property bool UseFontStyle = {read=fUseFontStyle, write=SetUseFontStyle, default=1};
	__property bool Visible = {read=fVisible, write=SetVisible, default=1};
	__property int Width = {read=GetWidth, write=SetWidth, default=30};
	__property bool ZeroStart = {read=fZeroStart, write=SetZeroStart, default=0};
	__property int LineNumberStart = {read=fLineNumberStart, write=SetLineNumberStart, default=1};
	__property bool Gradient = {read=fGradient, write=SetGradient, default=0};
	__property System::Uitypes::TColor GradientStartColor = {read=fGradientStartColor, write=SetGradientStartColor, default=-16777211};
	__property System::Uitypes::TColor GradientEndColor = {read=fGradientEndColor, write=SetGradientEndColor, default=-16777201};
	__property int GradientSteps = {read=fGradientSteps, write=SetGradientSteps, default=48};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
};


class PASCALIMPLEMENTATION TSynBookMarkOpt : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Vcl::Controls::TImageList* fBookmarkImages;
	bool fDrawBookmarksFirst;
	bool fEnableKeys;
	bool fGlyphsVisible;
	int fLeftMargin;
	System::Classes::TComponent* fOwner;
	int fXoffset;
	System::Classes::TNotifyEvent fOnChange;
	void __fastcall SetBookmarkImages(Vcl::Controls::TImageList* const Value);
	void __fastcall SetDrawBookmarksFirst(bool Value);
	void __fastcall SetGlyphsVisible(bool Value);
	void __fastcall SetLeftMargin(int Value);
	void __fastcall SetXOffset(int Value);
	
public:
	__fastcall TSynBookMarkOpt(System::Classes::TComponent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property Vcl::Controls::TImageList* BookmarkImages = {read=fBookmarkImages, write=SetBookmarkImages};
	__property bool DrawBookmarksFirst = {read=fDrawBookmarksFirst, write=SetDrawBookmarksFirst, default=1};
	__property bool EnableKeys = {read=fEnableKeys, write=fEnableKeys, default=1};
	__property bool GlyphsVisible = {read=fGlyphsVisible, write=SetGlyphsVisible, default=1};
	__property int LeftMargin = {read=fLeftMargin, write=SetLeftMargin, default=2};
	__property int Xoffset = {read=fXoffset, write=SetXOffset, default=12};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSynBookMarkOpt(void) { }
	
};


class PASCALIMPLEMENTATION TSynGlyph : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool fVisible;
	Vcl::Graphics::TBitmap* fInternalGlyph;
	Vcl::Graphics::TBitmap* fGlyph;
	System::Uitypes::TColor fInternalMaskColor;
	System::Uitypes::TColor fMaskColor;
	System::Classes::TNotifyEvent fOnChange;
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall GlyphChange(System::TObject* Sender);
	void __fastcall SetMaskColor(System::Uitypes::TColor Value);
	void __fastcall SetVisible(bool Value);
	int __fastcall GetWidth(void);
	int __fastcall GetHeight(void);
	
public:
	__fastcall TSynGlyph(NativeUInt aModule, const System::UnicodeString aName, System::Uitypes::TColor aMaskColor);
	__fastcall virtual ~TSynGlyph(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	void __fastcall Draw(Vcl::Graphics::TCanvas* aCanvas, int aX, int aY, int aLineHeight);
	__property int Width = {read=GetWidth, nodefault};
	__property int Height = {read=GetHeight, nodefault};
	
__published:
	__property Vcl::Graphics::TBitmap* Glyph = {read=fGlyph, write=SetGlyph};
	__property System::Uitypes::TColor MaskColor = {read=fMaskColor, write=SetMaskColor, default=536870911};
	__property bool Visible = {read=fVisible, write=SetVisible, default=1};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
};


class PASCALIMPLEMENTATION ESynMethodChain : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESynMethodChain(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESynMethodChain(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESynMethodChain(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESynMethodChain(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynMethodChain(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynMethodChain(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESynMethodChain(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESynMethodChain(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynMethodChain(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynMethodChain(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynMethodChain(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynMethodChain(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESynMethodChain(void) { }
	
};


typedef void __fastcall (__closure *TSynExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E, bool &DoContinue);

class PASCALIMPLEMENTATION TSynMethodChain : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FNotifyProcs;
	TSynExceptionEvent FExceptionHandler;
	
protected:
	virtual void __fastcall DoFire(const System::TMethod &AEvent) = 0 ;
	virtual bool __fastcall DoHandleException(System::Sysutils::Exception* E);
	__property TSynExceptionEvent ExceptionHandler = {read=FExceptionHandler, write=FExceptionHandler};
	
public:
	__fastcall virtual TSynMethodChain(void);
	__fastcall virtual ~TSynMethodChain(void);
	void __fastcall Add(const System::TMethod &AEvent);
	void __fastcall Remove(const System::TMethod &AEvent);
	void __fastcall Fire(void);
};


class PASCALIMPLEMENTATION TSynNotifyEventChain : public TSynMethodChain
{
	typedef TSynMethodChain inherited;
	
private:
	System::TObject* FSender;
	
protected:
	virtual void __fastcall DoFire(const System::TMethod &AEvent);
	
public:
	__fastcall TSynNotifyEventChain(System::TObject* ASender);
	HIDESBASE void __fastcall Add(System::Classes::TNotifyEvent AEvent);
	HIDESBASE void __fastcall Remove(System::Classes::TNotifyEvent AEvent);
	__property ExceptionHandler;
	__property System::TObject* Sender = {read=FSender, write=FSender};
public:
	/* TSynMethodChain.Create */ inline __fastcall virtual TSynNotifyEventChain(void) : TSynMethodChain() { }
	/* TSynMethodChain.Destroy */ inline __fastcall virtual ~TSynNotifyEventChain(void) { }
	
};


class PASCALIMPLEMENTATION TSynInternalImage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Vcl::Graphics::TBitmap* fImages;
	int fWidth;
	int fHeight;
	int fCount;
	Vcl::Graphics::TBitmap* __fastcall CreateBitmapFromInternalList(NativeUInt aModule, const System::UnicodeString Name);
	void __fastcall FreeBitmapFromInternalList(void);
	
public:
	__fastcall TSynInternalImage(NativeUInt aModule, const System::UnicodeString Name, int Count);
	__fastcall virtual ~TSynInternalImage(void);
	void __fastcall Draw(Vcl::Graphics::TCanvas* ACanvas, int Number, int X, int Y, int LineHeight);
	void __fastcall DrawTransparent(Vcl::Graphics::TCanvas* ACanvas, int Number, int X, int Y, int LineHeight, System::Uitypes::TColor TransparentColor);
};


typedef Vcl::Forms::TFormBorderStyle TSynBorderStyle;

enum DECLSPEC_DENUM THKModifier : unsigned char { hkShift, hkCtrl, hkAlt };

typedef System::Set<THKModifier, THKModifier::hkShift, THKModifier::hkAlt> THKModifiers;

enum DECLSPEC_DENUM THKInvalidKey : unsigned char { hcNone, hcShift, hcCtrl, hcAlt, hcShiftCtrl, hcShiftAlt, hcCtrlAlt, hcShiftCtrlAlt };

typedef System::Set<THKInvalidKey, THKInvalidKey::hcNone, THKInvalidKey::hcShiftCtrlAlt> THKInvalidKeys;

class PASCALIMPLEMENTATION TSynHotKey : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	System::Classes::TShortCut FHotKey;
	THKInvalidKeys FInvalidKeys;
	THKModifiers FModifiers;
	bool FPressedOnlyModifiers;
	void __fastcall SetBorderStyle(const Vcl::Forms::TBorderStyle Value);
	void __fastcall SetHotKey(const System::Classes::TShortCut Value);
	void __fastcall SetInvalidKeys(const THKInvalidKeys Value);
	void __fastcall SetModifiers(const THKModifiers Value);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	
protected:
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	DYNAMIC void __fastcall DoExit(void);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TSynHotKey(System::Classes::TComponent* AOwner);
	
__published:
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property System::Classes::TShortCut HotKey = {read=FHotKey, write=SetHotKey, default=65};
	__property THKInvalidKeys InvalidKeys = {read=FInvalidKeys, write=SetInvalidKeys, default=3};
	__property THKModifiers Modifiers = {read=FModifiers, write=SetModifiers, default=4};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TSynHotKey(void) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSynHotKey(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TSynEditSearchCustom : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetPattern(void) = 0 ;
	virtual void __fastcall SetPattern(const System::UnicodeString Value) = 0 ;
	virtual int __fastcall GetLength(int Index) = 0 ;
	virtual int __fastcall GetResult(int Index) = 0 ;
	virtual int __fastcall GetResultCount(void) = 0 ;
	virtual void __fastcall SetOptions(const Synedittypes::TSynSearchOptions Value) = 0 ;
	
public:
	virtual int __fastcall FindAll(const System::UnicodeString NewText) = 0 ;
	virtual System::UnicodeString __fastcall Replace(const System::UnicodeString aOccurrence, const System::UnicodeString aReplacement) = 0 ;
	__property System::UnicodeString Pattern = {read=GetPattern, write=SetPattern};
	__property int ResultCount = {read=GetResultCount, nodefault};
	__property int Results[int Index] = {read=GetResult};
	__property int Lengths[int Index] = {read=GetLength};
	__property Synedittypes::TSynSearchOptions Options = {write=SetOptions, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TSynEditSearchCustom(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TSynEditSearchCustom(void) { }
	
};


class PASCALIMPLEMENTATION TBetterRegistry : public System::Win::Registry::TRegistry
{
	typedef System::Win::Registry::TRegistry inherited;
	
public:
	HIDESBASE bool __fastcall OpenKeyReadOnly(const System::UnicodeString Key);
public:
	/* TRegistry.Create */ inline __fastcall TBetterRegistry(void)/* overload */ : System::Win::Registry::TRegistry() { }
	/* TRegistry.Create */ inline __fastcall TBetterRegistry(unsigned AAccess)/* overload */ : System::Win::Registry::TRegistry(AAccess) { }
	/* TRegistry.Destroy */ inline __fastcall virtual ~TBetterRegistry(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 BorderWidth = System::Int8(0x0);
}	/* namespace Syneditmiscclasses */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITMISCCLASSES)
using namespace Syneditmiscclasses;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditmiscclassesHPP
