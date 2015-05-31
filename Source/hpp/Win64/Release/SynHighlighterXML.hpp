// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterXML.pas' rev: 29.00 (Windows)

#ifndef SynhighlighterxmlHPP
#define SynhighlighterxmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Win.Registry.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterxml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynXMLSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkAposAttrValue, tkAposEntityRef, tkAttribute, tkCDATA, tkComment, tkElement, tkEntityRef, tkEqual, tkNull, tkProcessingInstruction, tkQuoteAttrValue, tkQuoteEntityRef, tkSpace, tkSymbol, tkText, tknsAposAttrValue, tknsAposEntityRef, tknsAttribute, tknsEqual, tknsQuoteAttrValue, tknsQuoteEntityRef, tkDocType };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsAposAttrValue, rsAPosEntityRef, rsAttribute, rsCDATA, rsComment, rsElement, rsEntityRef, rsEqual, rsProcessingInstruction, rsQuoteAttrValue, rsQuoteEntityRef, rsText, rsnsAposAttrValue, rsnsAPosEntityRef, rsnsEqual, rsnsQuoteAttrValue, rsnsQuoteEntityRef, rsDocType, rsDocTypeSquareBraces };

class PASCALIMPLEMENTATION TSynXMLSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fElementAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fTextAttri;
	Synedithighlighter::TSynHighlighterAttributes* fEntityRefAttri;
	Synedithighlighter::TSynHighlighterAttributes* fProcessingInstructionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCDATAAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDocTypeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fAttributeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fnsAttributeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fAttributeValueAttri;
	Synedithighlighter::TSynHighlighterAttributes* fnsAttributeValueAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	bool FWantBracesParsed;
	void __fastcall NullProc(void);
	void __fastcall CarriageReturnProc(void);
	void __fastcall LineFeedProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall LessThanProc(void);
	void __fastcall GreaterThanProc(void);
	void __fastcall CommentProc(void);
	void __fastcall ProcessingInstructionProc(void);
	void __fastcall DocTypeProc(void);
	void __fastcall CDATAProc(void);
	void __fastcall TextProc(void);
	void __fastcall ElementProc(void);
	void __fastcall AttributeProc(void);
	void __fastcall QAttributeValueProc(void);
	void __fastcall AAttributeValueProc(void);
	void __fastcall EqualProc(void);
	void __fastcall IdentProc(void);
	void __fastcall NextProcedure(void);
	bool __fastcall NextTokenIs(System::UnicodeString Token);
	void __fastcall EntityRefProc(void);
	void __fastcall QEntityRefProc(void);
	void __fastcall AEntityRefProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	virtual bool __fastcall IsNameChar(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynXMLSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* ElementAttri = {read=fElementAttri, write=fElementAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* AttributeAttri = {read=fAttributeAttri, write=fAttributeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NamespaceAttributeAttri = {read=fnsAttributeAttri, write=fnsAttributeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* AttributeValueAttri = {read=fAttributeValueAttri, write=fAttributeValueAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NamespaceAttributeValueAttri = {read=fnsAttributeValueAttri, write=fnsAttributeValueAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* TextAttri = {read=fTextAttri, write=fTextAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CDATAAttri = {read=fCDATAAttri, write=fCDATAAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* EntityRefAttri = {read=fEntityRefAttri, write=fEntityRefAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ProcessingInstructionAttri = {read=fProcessingInstructionAttri, write=fProcessingInstructionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DocTypeAttri = {read=fDocTypeAttri, write=fDocTypeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property bool WantBracesParsed = {read=FWantBracesParsed, write=FWantBracesParsed, default=1};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynXMLSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterxml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERXML)
using namespace Synhighlighterxml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterxmlHPP
