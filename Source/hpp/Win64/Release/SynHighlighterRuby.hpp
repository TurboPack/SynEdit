// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterRuby.pas' rev: 30.00 (Windows)

#ifndef SynhighlighterrubyHPP
#define SynhighlighterrubyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterruby
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynRubySyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey, tkSpace, tkString, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown };

class PASCALIMPLEMENTATION TSynRubySyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind FTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSecondKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	System::Classes::TStrings* fKeyWords;
	System::Classes::TStrings* fSecondKeys;
	void __fastcall BraceOpenProc(void);
	void __fastcall PointCommaProc(void);
	void __fastcall CRProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall LowerProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall RoundOpenProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall SetSecondKeys(System::Classes::TStrings* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	void __fastcall NextProcedure(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynRubySyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynRubySyn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual bool __fastcall IsKeyword(const System::UnicodeString AKeyword);
	bool __fastcall IsSecondKeyWord(System::UnicodeString aToken);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SecondKeyAttri = {read=fSecondKeyAttri, write=fSecondKeyAttri};
	__property System::Classes::TStrings* SecondKeyWords = {read=fSecondKeys, write=SetSecondKeys};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterruby */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERRUBY)
using namespace Synhighlighterruby;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterrubyHPP
