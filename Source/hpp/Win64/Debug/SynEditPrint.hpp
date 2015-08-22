// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPrint.pas' rev: 30.00 (Windows)

#ifndef SyneditprintHPP
#define SyneditprintHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Printers.hpp>
#include <SynEdit.hpp>
#include <SynEditTypes.hpp>
#include <SynEditPrintTypes.hpp>
#include <SynEditPrintHeaderFooter.hpp>
#include <SynEditPrinterInfo.hpp>
#include <SynEditPrintMargins.hpp>
#include <SynEditMiscProcs.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditprint
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPageLine;
class DELPHICLASS TSynEditPrint;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TPageLine : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int FirstLine;
public:
	/* TObject.Create */ inline __fastcall TPageLine(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TPageLine(void) { }
	
};


class PASCALIMPLEMENTATION TSynEditPrint : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	int FCopies;
	Syneditprintheaderfooter::TFooter* FFooter;
	Syneditprintheaderfooter::THeader* FHeader;
	System::Classes::TStrings* FLines;
	Syneditprintmargins::TSynEditPrintMargins* FMargins;
	int FPageCount;
	Vcl::Graphics::TFont* FFont;
	System::UnicodeString FTitle;
	System::UnicodeString FDocTitle;
	Syneditprinterinfo::TSynEditPrinterInfo* FPrinterInfo;
	System::Classes::TList* FPages;
	Vcl::Graphics::TCanvas* FCanvas;
	int FCharWidth;
	int FMaxLeftChar;
	bool FWrap;
	Syneditprinttypes::TPrintLineEvent FOnPrintLine;
	Syneditprinttypes::TPrintStatusEvent FOnPrintStatus;
	int FYPos;
	int FLineHeight;
	bool FHighlight;
	bool FColors;
	Synedithighlighter::TSynCustomHighlighter* FHighlighter;
	Vcl::Graphics::TFont* FOldFont;
	bool FSynOK;
	bool FLineNumbers;
	int FLineNumber;
	int FLineOffset;
	bool FAbort;
	bool FPrinting;
	System::Uitypes::TColor FDefaultBG;
	int FPageOffset;
	bool FRangesOK;
	int FMaxWidth;
	int FMaxCol;
	bool FPagesCounted;
	bool FLineNumbersInMargin;
	int FTabWidth;
	System::Uitypes::TColor fFontColor;
	bool fSelectedOnly;
	bool fSelAvail;
	Synedittypes::TSynSelectionMode fSelMode;
	Synedittypes::TBufferCoord fBlockBegin;
	Synedittypes::TBufferCoord fBlockEnd;
	Syneditmiscprocs::TIntArray *FETODist;
	void __fastcall CalcPages(void);
	void __fastcall SetLines(System::Classes::TStrings* const Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetCharWidth(const int Value);
	void __fastcall SetMaxLeftChar(const int Value);
	void __fastcall PrintPage(int Num);
	void __fastcall WriteLine(const System::UnicodeString Text);
	void __fastcall WriteLineNumber(void);
	void __fastcall HandleWrap(const System::UnicodeString Text, int MaxWidth);
	void __fastcall TextOut(const System::UnicodeString Text, System::Classes::TList* AList);
	void __fastcall SetHighlighter(Synedithighlighter::TSynCustomHighlighter* const Value);
	void __fastcall RestoreCurrentFont(void);
	void __fastcall SaveCurrentFont(void);
	void __fastcall SetPixelsPrInch(void);
	void __fastcall InitPrint(void);
	void __fastcall InitRanges(void);
	int __fastcall GetPageCount(void);
	void __fastcall SetSynEdit(Synedit::TCustomSynEdit* const Value);
	void __fastcall SetFooter(Syneditprintheaderfooter::TFooter* const Value);
	void __fastcall SetHeader(Syneditprintheaderfooter::THeader* const Value);
	void __fastcall SetMargins(Syneditprintmargins::TSynEditPrintMargins* const Value);
	System::UnicodeString __fastcall ClipLineToRect(System::UnicodeString S, const System::Types::TRect &R);
	System::UnicodeString __fastcall ExpandAtWideGlyphs(const System::UnicodeString S);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	__property int MaxLeftChar = {read=FMaxLeftChar, write=SetMaxLeftChar, nodefault};
	__property int CharWidth = {read=FCharWidth, write=SetCharWidth, nodefault};
	virtual void __fastcall PrintStatus(Syneditprinttypes::TSynPrintStatus Status, int PageNumber, bool &Abort);
	virtual void __fastcall PrintLine(int LineNumber, int PageNumber);
	
public:
	__fastcall virtual TSynEditPrint(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynEditPrint(void);
	void __fastcall UpdatePages(Vcl::Graphics::TCanvas* ACanvas);
	void __fastcall PrintToCanvas(Vcl::Graphics::TCanvas* ACanvas, int PageNumber);
	void __fastcall Print(void);
	void __fastcall PrintRange(int StartPage, int EndPage);
	__property Syneditprinterinfo::TSynEditPrinterInfo* PrinterInfo = {read=FPrinterInfo};
	__property int PageCount = {read=GetPageCount, nodefault};
	__property Synedit::TCustomSynEdit* SynEdit = {write=SetSynEdit};
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	
__published:
	__property int Copies = {read=FCopies, write=FCopies, nodefault};
	__property Syneditprintheaderfooter::THeader* Header = {read=FHeader, write=SetHeader};
	__property Syneditprintheaderfooter::TFooter* Footer = {read=FFooter, write=SetFooter};
	__property Syneditprintmargins::TSynEditPrintMargins* Margins = {read=FMargins, write=SetMargins};
	__property System::Classes::TStrings* Lines = {read=FLines, write=SetLines};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
	__property System::UnicodeString DocTitle = {read=FDocTitle, write=FDocTitle};
	__property bool Wrap = {read=FWrap, write=FWrap, default=1};
	__property bool Highlight = {read=FHighlight, write=FHighlight, default=1};
	__property bool SelectedOnly = {read=fSelectedOnly, write=fSelectedOnly, default=0};
	__property bool Colors = {read=FColors, write=FColors, default=0};
	__property bool LineNumbers = {read=FLineNumbers, write=FLineNumbers, default=0};
	__property int LineOffset = {read=FLineOffset, write=FLineOffset, default=0};
	__property int PageOffset = {read=FPageOffset, write=FPageOffset, default=0};
	__property Syneditprinttypes::TPrintLineEvent OnPrintLine = {read=FOnPrintLine, write=FOnPrintLine};
	__property Syneditprinttypes::TPrintStatusEvent OnPrintStatus = {read=FOnPrintStatus, write=FOnPrintStatus};
	__property Synedithighlighter::TSynCustomHighlighter* Highlighter = {read=FHighlighter, write=SetHighlighter};
	__property bool LineNumbersInMargin = {read=FLineNumbersInMargin, write=FLineNumbersInMargin, default=0};
	__property int TabWidth = {read=FTabWidth, write=FTabWidth, nodefault};
	__property System::Uitypes::TColor Color = {read=FDefaultBG, write=FDefaultBG, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditprint */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPRINT)
using namespace Syneditprint;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditprintHPP
