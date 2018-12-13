// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPrintPreview.pas' rev: 33.00 (Windows)

#ifndef SyneditprintpreviewHPP
#define SyneditprintpreviewHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Themes.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Forms.hpp>
#include <SynEditPrint.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditprintpreview
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynEditPrintPreview;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TPreviewPageEvent)(System::TObject* Sender, int PageNumber);

enum DECLSPEC_DENUM TSynPreviewScale : unsigned char { pscWholePage, pscPageWidth, pscUserScaled };

class PASCALIMPLEMENTATION TSynEditPrintPreview : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
protected:
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	Syneditprint::TSynEditPrint* FSynEditPrint;
	TSynPreviewScale FScaleMode;
	int FScalePercent;
	System::Types::TPoint FVirtualSize;
	System::Types::TPoint FVirtualOffset;
	System::Types::TPoint FPageSize;
	System::Types::TPoint FScrollPosition;
	System::Uitypes::TColor FPageBG;
	int FPageNumber;
	bool FShowScrollHint;
	TPreviewPageEvent FOnPreviewPage;
	System::Classes::TNotifyEvent FOnScaleChange;
	int FWheelAccumulator;
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall SetPageBG(System::Uitypes::TColor Value);
	void __fastcall SetSynEditPrint(Syneditprint::TSynEditPrint* Value);
	void __fastcall SetScaleMode(TSynPreviewScale Value);
	void __fastcall SetScalePercent(int Value);
	
private:
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Message);
	void __fastcall PaintPaper();
	int __fastcall GetPageCount();
	
protected:
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	int __fastcall GetPageHeightFromWidth(int AWidth);
	int __fastcall GetPageHeight100Percent();
	int __fastcall GetPageWidthFromHeight(int AHeight);
	int __fastcall GetPageWidth100Percent();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall ScrollHorzFor(int Value);
	virtual void __fastcall ScrollHorzTo(int Value);
	void __fastcall ScrollVertFor(int Value);
	virtual void __fastcall ScrollVertTo(int Value);
	virtual void __fastcall UpdateScrollbars();
	virtual void __fastcall SizeChanged();
	
public:
	__fastcall virtual TSynEditPrintPreview(System::Classes::TComponent* AOwner);
	virtual void __fastcall Paint();
	void __fastcall UpdatePreview();
	void __fastcall NextPage();
	void __fastcall PreviousPage();
	void __fastcall FirstPage();
	void __fastcall LastPage();
	void __fastcall Print();
	__property int PageNumber = {read=FPageNumber, nodefault};
	__property int PageCount = {read=GetPageCount, nodefault};
	
__published:
	__property Align = {default=5};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property Color = {default=-16777204};
	__property Cursor = {default=0};
	__property System::Uitypes::TColor PageBGColor = {read=FPageBG, write=SetPageBG, default=16777215};
	__property PopupMenu;
	__property Syneditprint::TSynEditPrint* SynEditPrint = {read=FSynEditPrint, write=SetSynEditPrint};
	__property TSynPreviewScale ScaleMode = {read=FScaleMode, write=SetScaleMode, default=2};
	__property int ScalePercent = {read=FScalePercent, write=SetScalePercent, default=100};
	__property Visible = {default=1};
	__property bool ShowScrollHint = {read=FShowScrollHint, write=FShowScrollHint, default=1};
	__property OnClick;
	__property OnMouseDown;
	__property OnMouseUp;
	__property TPreviewPageEvent OnPreviewPage = {read=FOnPreviewPage, write=FOnPreviewPage};
	__property System::Classes::TNotifyEvent OnScaleChange = {read=FOnScaleChange, write=FOnScaleChange};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TSynEditPrintPreview() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TSynEditPrintPreview(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditprintpreview */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPRINTPREVIEW)
using namespace Syneditprintpreview;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditprintpreviewHPP
