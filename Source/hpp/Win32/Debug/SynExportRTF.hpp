// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynExportRTF.pas' rev: 30.00 (Windows)

#ifndef SynexportrtfHPP
#define SynexportrtfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Winapi.RichEdit.hpp>
#include <SynEditExport.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>
#include <SynEditHighlighter.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synexportrtf
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynExporterRTF;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSynExporterRTF : public Syneditexport::TSynCustomExporter
{
	typedef Syneditexport::TSynCustomExporter inherited;
	
private:
	bool fAttributesChanged;
	System::Classes::TList* fListColors;
	System::UnicodeString __fastcall ColorToRTF(System::Uitypes::TColor AColor);
	int __fastcall GetColorIndex(System::Uitypes::TColor AColor);
	
protected:
	virtual void __fastcall FormatAfterLastAttribute(void);
	virtual void __fastcall FormatAttributeDone(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged);
	virtual void __fastcall FormatAttributeInit(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged);
	virtual void __fastcall FormatBeforeFirstAttribute(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged);
	virtual void __fastcall FormatNewLine(void);
	virtual System::UnicodeString __fastcall GetFooter(void);
	virtual System::UnicodeString __fastcall GetFormatName(void);
	virtual System::UnicodeString __fastcall GetHeader(void);
	virtual System::UnicodeString __fastcall ReplaceReservedChar(System::WideChar AChar);
	virtual bool __fastcall UseBom(void);
	
public:
	__fastcall virtual TSynExporterRTF(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynExporterRTF(void);
	virtual void __fastcall Clear(void);
	virtual Synunicode::TSynEncodings __fastcall SupportedEncodings(void);
	
__published:
	__property Color;
	__property DefaultFilter = {default=0};
	__property Encoding = {default=0};
	__property Font;
	__property Highlighter;
	__property Title = {default=0};
	__property UseBackground;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synexportrtf */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEXPORTRTF)
using namespace Synexportrtf;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynexportrtfHPP
