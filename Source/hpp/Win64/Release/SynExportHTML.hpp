// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynExportHTML.pas' rev: 31.00 (Windows)

#ifndef SynexporthtmlHPP
#define SynexporthtmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditExport.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synexporthtml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynExporterHTML;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSynExporterHTML : public Syneditexport::TSynCustomExporter
{
	typedef Syneditexport::TSynCustomExporter inherited;
	
private:
	System::UnicodeString __fastcall AttriToCSS(Synedithighlighter::TSynHighlighterAttributes* Attri, System::UnicodeString UniqueAttriName);
	bool __fastcall AttriToCSSCallback(Synedithighlighter::TSynCustomHighlighter* Highlighter, Synedithighlighter::TSynHighlighterAttributes* Attri, System::UnicodeString UniqueAttriName, void * *Params, const int Params_High);
	System::UnicodeString __fastcall ColorToHTML(System::Uitypes::TColor AColor);
	System::UnicodeString __fastcall GetStyleName(Synedithighlighter::TSynCustomHighlighter* Highlighter, Synedithighlighter::TSynHighlighterAttributes* Attri);
	System::UnicodeString __fastcall MakeValidName(System::UnicodeString Name);
	bool __fastcall StyleNameCallback(Synedithighlighter::TSynCustomHighlighter* Highlighter, Synedithighlighter::TSynHighlighterAttributes* Attri, System::UnicodeString UniqueAttriName, void * *Params, const int Params_High);
	
protected:
	bool fCreateHTMLFragment;
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
	__fastcall virtual TSynExporterHTML(System::Classes::TComponent* AOwner);
	virtual Synunicode::TSynEncodings __fastcall SupportedEncodings(void);
	
__published:
	__property Color;
	__property bool CreateHTMLFragment = {read=fCreateHTMLFragment, write=fCreateHTMLFragment, default=0};
	__property DefaultFilter = {default=0};
	__property Encoding = {default=0};
	__property Font;
	__property Highlighter;
	__property Title = {default=0};
	__property UseBackground;
public:
	/* TSynCustomExporter.Destroy */ inline __fastcall virtual ~TSynExporterHTML(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synexporthtml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEXPORTHTML)
using namespace Synexporthtml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynexporthtmlHPP
