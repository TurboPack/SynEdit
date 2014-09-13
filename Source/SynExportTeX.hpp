// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynExportTeX.pas' rev: 28.00 (Windows)

#ifndef SynexporttexHPP
#define SynexporttexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <SynEditExport.hpp>	// Pascal unit
#include <SynEditHighlighter.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synexporttex
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSynExporterTeX;
class PASCALIMPLEMENTATION TSynExporterTeX : public Syneditexport::TSynCustomExporter
{
	typedef Syneditexport::TSynCustomExporter inherited;
	
private:
	int fMargin;
	Synedithighlighter::TSynHighlighterAttributes* fLastAttri;
	System::UnicodeString __fastcall AttriToCommand(Synedithighlighter::TSynHighlighterAttributes* Attri, System::UnicodeString UniqueAttriName);
	bool __fastcall AttriToCommandCallback(Synedithighlighter::TSynCustomHighlighter* Highlighter, Synedithighlighter::TSynHighlighterAttributes* Attri, System::UnicodeString UniqueAttriName, void * *Params, const int Params_High);
	bool __fastcall CommandNameCallback(Synedithighlighter::TSynCustomHighlighter* Highlighter, Synedithighlighter::TSynHighlighterAttributes* Attri, System::UnicodeString UniqueAttriName, void * *Params, const int Params_High);
	System::UnicodeString __fastcall GetCommandName(Synedithighlighter::TSynCustomHighlighter* Highlighter, Synedithighlighter::TSynHighlighterAttributes* Attri);
	System::UnicodeString __fastcall GetNewCommands(void);
	System::UnicodeString __fastcall MakeValidName(System::UnicodeString Name);
	
protected:
	bool fCreateTeXFragment;
	int fTabWidth;
	bool fPageStyleEmpty;
	virtual void __fastcall FormatAfterLastAttribute(void);
	virtual void __fastcall FormatAttributeDone(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged);
	virtual void __fastcall FormatAttributeInit(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged);
	virtual void __fastcall FormatBeforeFirstAttribute(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged);
	virtual void __fastcall FormatNewLine(void);
	virtual void __fastcall FormatToken(System::UnicodeString Token);
	virtual System::UnicodeString __fastcall GetFooter(void);
	virtual System::UnicodeString __fastcall GetFormatName(void);
	virtual System::UnicodeString __fastcall GetHeader(void);
	virtual System::UnicodeString __fastcall ReplaceReservedChar(System::WideChar AChar);
	virtual void __fastcall SetTokenAttribute(Synedithighlighter::TSynHighlighterAttributes* Attri);
	virtual bool __fastcall UseBom(void);
	
public:
	__fastcall virtual TSynExporterTeX(System::Classes::TComponent* AOwner);
	virtual Synunicode::TSynEncodings __fastcall SupportedEncodings(void);
	
__published:
	__property int Margin = {read=fMargin, write=fMargin, default=2};
	__property int TabWidth = {read=fTabWidth, write=fTabWidth, default=2};
	__property Color;
	__property bool CreateTeXFragment = {read=fCreateTeXFragment, write=fCreateTeXFragment, default=0};
	__property bool PageStyleEmpty = {read=fPageStyleEmpty, write=fPageStyleEmpty, default=0};
	__property DefaultFilter = {default=0};
	__property Encoding = {default=0};
	__property Font;
	__property Highlighter;
	__property Title = {default=0};
	__property UseBackground;
public:
	/* TSynCustomExporter.Destroy */ inline __fastcall virtual ~TSynExporterTeX(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synexporttex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEXPORTTEX)
using namespace Synexporttex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynexporttexHPP
