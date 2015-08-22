// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterJSON.pas' rev: 30.00 (Windows)

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
	void __fastcall CloseArrayProc(void);
	void __fastcall CloseObjectProc(void);
	void __fastcall ColonProc(void);
	void __fastcall CommaProc(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall OpenArrayProc(void);
	void __fastcall OpenObjectProc(void);
	void __fastcall ReservedWordProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall UnknownProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynJSONSyn(System::Classes::TComponent* AOwner);
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
	__property Synedithighlighter::TSynHighlighterAttributes* AttributeAttri = {read=FAttributeAttri, write=FAttributeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ReservedAttri = {read=FReservedAttri, write=FReservedAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=FNumberAttri, write=FNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=FSpaceAttri, write=FSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=FSymbolAttri, write=FSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ValueAttri = {read=FValueAttri, write=FValueAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynJSONSyn(void) { }
	
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
