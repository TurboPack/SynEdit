// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditKbdHandler.pas' rev: 28.00 (Windows)

#ifndef SyneditkbdhandlerHPP
#define SyneditkbdhandlerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Syneditkbdhandler
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TKeyboardControl;
class PASCALIMPLEMENTATION TKeyboardControl : public Vcl::Controls::TWinControl
{
	typedef Vcl::Controls::TWinControl inherited;
	
public:
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnMouseDown;
public:
	/* TWinControl.Create */ inline __fastcall virtual TKeyboardControl(System::Classes::TComponent* AOwner) : Vcl::Controls::TWinControl(AOwner) { }
	/* TWinControl.CreateParented */ inline __fastcall TKeyboardControl(HWND ParentWindow) : Vcl::Controls::TWinControl(ParentWindow) { }
	/* TWinControl.Destroy */ inline __fastcall virtual ~TKeyboardControl(void) { }
	
};


typedef void __fastcall (__closure *TMouseCursorEvent)(System::TObject* Sender, const Synedittypes::TBufferCoord &aLineCharPos, System::Uitypes::TCursor &aCursor);

class DELPHICLASS TMethodList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TMethodList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::TMethod operator[](int Index) { return Items[Index]; }
	
private:
	System::Classes::TList* fData;
	System::TMethod __fastcall GetItem(int Index);
	int __fastcall GetCount(void);
	
public:
	__fastcall TMethodList(void);
	__fastcall virtual ~TMethodList(void);
	void __fastcall Add(const System::TMethod &aHandler);
	void __fastcall Remove(const System::TMethod &aHandler);
	__property System::TMethod Items[int Index] = {read=GetItem/*, default*/};
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TSynEditKbdHandler;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditKbdHandler : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TMethodList* fKeyPressChain;
	TMethodList* fKeyDownChain;
	TMethodList* fKeyUpChain;
	TMethodList* fMouseDownChain;
	TMethodList* fMouseUpChain;
	TMethodList* fMouseCursorChain;
	bool fInKeyPress;
	bool fInKeyDown;
	bool fInKeyUp;
	bool fInMouseDown;
	bool fInMouseUp;
	bool fInMouseCursor;
	
public:
	__fastcall TSynEditKbdHandler(void);
	__fastcall virtual ~TSynEditKbdHandler(void);
	void __fastcall ExecuteKeyPress(System::TObject* Sender, System::WideChar &Key);
	void __fastcall ExecuteKeyDown(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ExecuteKeyUp(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall ExecuteMouseDown(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ExecuteMouseUp(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall ExecuteMouseCursor(System::TObject* Sender, const Synedittypes::TBufferCoord &aLineCharPos, System::Uitypes::TCursor &aCursor);
	void __fastcall AddKeyDownHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall RemoveKeyDownHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall AddKeyUpHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall RemoveKeyUpHandler(Vcl::Controls::TKeyEvent aHandler);
	void __fastcall AddKeyPressHandler(Synedittypes::TKeyPressWEvent aHandler);
	void __fastcall RemoveKeyPressHandler(Synedittypes::TKeyPressWEvent aHandler);
	void __fastcall AddMouseDownHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall RemoveMouseDownHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall AddMouseUpHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall RemoveMouseUpHandler(Vcl::Controls::TMouseEvent aHandler);
	void __fastcall AddMouseCursorHandler(TMouseCursorEvent aHandler);
	void __fastcall RemoveMouseCursorHandler(TMouseCursorEvent aHandler);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditkbdhandler */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITKBDHANDLER)
using namespace Syneditkbdhandler;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditkbdhandlerHPP
