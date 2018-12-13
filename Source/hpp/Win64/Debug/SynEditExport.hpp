// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditExport.pas' rev: 32.00 (Windows)

#ifndef SyneditexportHPP
#define SyneditexportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Clipbrd.hpp>
#include <SynEditHighlighter.hpp>
#include <SynEditTypes.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditexport
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ESynEncoding;
class DELPHICLASS TSynCustomExporter;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION ESynEncoding : public Synedittypes::ESynError
{
	typedef Synedittypes::ESynError inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESynEncoding(const System::UnicodeString Msg) : Synedittypes::ESynError(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESynEncoding(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : Synedittypes::ESynError(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESynEncoding(NativeUInt Ident)/* overload */ : Synedittypes::ESynError(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESynEncoding(System::PResStringRec ResStringRec)/* overload */ : Synedittypes::ESynError(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynEncoding(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : Synedittypes::ESynError(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynEncoding(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : Synedittypes::ESynError(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESynEncoding(const System::UnicodeString Msg, int AHelpContext) : Synedittypes::ESynError(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESynEncoding(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : Synedittypes::ESynError(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynEncoding(NativeUInt Ident, int AHelpContext)/* overload */ : Synedittypes::ESynError(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynEncoding(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : Synedittypes::ESynError(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynEncoding(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Synedittypes::ESynError(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynEncoding(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : Synedittypes::ESynError(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESynEncoding(void) { }
	
};


class PASCALIMPLEMENTATION TSynCustomExporter : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TMemoryStream* fBuffer;
	int FCharSize;
	bool fFirstAttribute;
	bool FStreaming;
	void __fastcall AssignFont(Vcl::Graphics::TFont* Value);
	void __fastcall SetEncoding(const Synunicode::TSynEncoding Value);
	void __fastcall SetExportAsText(bool Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	void __fastcall SetHighlighter(Synedithighlighter::TSynCustomHighlighter* Value);
	void __fastcall SetTitle(const System::UnicodeString Value);
	void __fastcall SetUseBackground(const bool Value);
	int __fastcall StringSize(const System::UnicodeString AText);
	void __fastcall WriteString(const System::UnicodeString AText);
	
protected:
	System::Uitypes::TColor fBackgroundColor;
	unsigned fClipboardFormat;
	System::UnicodeString fDefaultFilter;
	Synunicode::TSynEncoding FEncoding;
	bool fExportAsText;
	Vcl::Graphics::TFont* fFont;
	Synedithighlighter::TSynCustomHighlighter* fHighlighter;
	System::Uitypes::TColor fLastBG;
	System::Uitypes::TColor fLastFG;
	System::Uitypes::TFontStyles fLastStyle;
	System::UnicodeString fTitle;
	bool fUseBackground;
	void __fastcall AddData(const System::UnicodeString AText);
	void __fastcall AddDataNewLine(const System::UnicodeString AText);
	void __fastcall AddNewLine(void);
	void __fastcall CopyToClipboardFormat(unsigned AFormat);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall FormatAttributeDone(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged) = 0 ;
	virtual void __fastcall FormatAttributeInit(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged) = 0 ;
	virtual void __fastcall FormatAfterLastAttribute(void) = 0 ;
	virtual void __fastcall FormatBeforeFirstAttribute(bool BackgroundChanged, bool ForegroundChanged, System::Uitypes::TFontStyles FontStylesChanged) = 0 ;
	virtual void __fastcall FormatToken(System::UnicodeString Token);
	virtual void __fastcall FormatNewLine(void) = 0 ;
	int __fastcall GetBufferSize(void);
	virtual unsigned __fastcall GetClipboardFormat(void);
	virtual System::UnicodeString __fastcall GetFooter(void) = 0 ;
	virtual System::UnicodeString __fastcall GetFormatName(void);
	virtual System::UnicodeString __fastcall GetHeader(void) = 0 ;
	void __fastcall InsertData(int APos, const System::UnicodeString AText);
	virtual System::UnicodeString __fastcall ReplaceReservedChar(System::WideChar AChar) = 0 ;
	System::UnicodeString __fastcall ReplaceReservedChars(System::UnicodeString AToken);
	virtual void __fastcall SetTokenAttribute(Synedithighlighter::TSynHighlighterAttributes* Attri);
	virtual bool __fastcall UseBom(void) = 0 ;
	
public:
	__fastcall virtual TSynCustomExporter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynCustomExporter(void);
	virtual void __fastcall Clear(void);
	void __fastcall CopyToClipboard(void);
	void __fastcall ExportAll(System::Classes::TStrings* ALines);
	void __fastcall ExportRange(System::Classes::TStrings* ALines, Synedittypes::TBufferCoord Start, Synedittypes::TBufferCoord Stop);
	void __fastcall SaveToFile(const System::UnicodeString FileName);
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual Synunicode::TSynEncodings __fastcall SupportedEncodings(void) = 0 ;
	__property System::Uitypes::TColor Color = {read=fBackgroundColor, write=fBackgroundColor, nodefault};
	__property System::UnicodeString DefaultFilter = {read=fDefaultFilter, write=fDefaultFilter};
	__property Synunicode::TSynEncoding Encoding = {read=FEncoding, write=SetEncoding, default=0};
	__property bool ExportAsText = {read=fExportAsText, write=SetExportAsText, nodefault};
	__property Vcl::Graphics::TFont* Font = {read=fFont, write=SetFont};
	__property System::UnicodeString FormatName = {read=GetFormatName};
	__property Synedithighlighter::TSynCustomHighlighter* Highlighter = {read=fHighlighter, write=SetHighlighter};
	__property System::UnicodeString Title = {read=fTitle, write=SetTitle};
	__property bool UseBackground = {read=fUseBackground, write=SetUseBackground, nodefault};
};


typedef System::StaticArray<System::UnicodeString, 4> Syneditexport__3;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Syneditexport__3 EncodingStrs;
extern DELPHI_PACKAGE System::ResourceString _SEncodingError;
#define Syneditexport_SEncodingError System::LoadResourceString(&Syneditexport::_SEncodingError)
}	/* namespace Syneditexport */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITEXPORT)
using namespace Syneditexport;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditexportHPP
