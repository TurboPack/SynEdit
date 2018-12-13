// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterJSON.pas' rev: 33.00 (Windows)

#ifndef SynhighlighterjsonHPP
#define SynhighlighterjsonHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Win.Registry.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterjson
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynJSONSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkString, tkReserved, tkNull, tkNumber, tkSpace, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsAttribute, rsObjectValue, rsArrayValue };

class PASCALIMPLEMENTATION TSynJSONSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState FRange;
	TtkTokenKind FTokenID;
	Synedithighlighter::TSynHighlighterAttributes* FReservedAttri;
	Synedithighlighter::TSynHighlighterAttributes* FAttributeAttri;
	Synedithighlighter::TSynHighlighterAttributes* FValueAttri;
	Synedithighlighter::TSynHighlighterAttributes* FNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* FSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* FSymbolAttri;
	void __fastcall CloseArrayProc();
	void __fastcall CloseObjectProc();
	void __fastcall ColonProc();
	void __fastcall CommaProc();
	void __fastcall CRProc();
	void __fastcall LFProc();
	void __fastcall NullProc();
	void __fastcall NumberProc();
	void __fastcall OpenArrayProc();
	void __fastcall OpenObjectProc();
	void __fastcall ReservedWordProc();
	void __fastcall SpaceProc();
	void __fastcall StringProc();
	void __fastcall SymbolProc();
	void __fastcall UnknownProc();
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource();
	virtual bool __fastcall IsFilterStored();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynJSONSyn(System::Classes::TComponent* AOwner);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	virtual void * __fastcall GetRange();
	TtkTokenKind __fastcall GetTokenID();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	virtual int __fastcall GetTokenKind();
	virtual void __fastcall Next();
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange();
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* AttributeAttri = {read=FAttributeAttri, write=FAttributeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ReservedAttri = {read=FReservedAttri, write=FReservedAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=FNumberAttri, write=FNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=FSpaceAttri, write=FSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=FSymbolAttri, write=FSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ValueAttri = {read=FValueAttri, write=FValueAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynJSONSyn() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterjson */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERJSON)
using namespace Synhighlighterjson;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterjsonHPP
