// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterM3.pas' rev: 30.00 (Windows)

#ifndef Synhighlighterm3HPP
#define Synhighlighterm3HPP

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
#include <SynHighlighterHashEntries.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterm3
{
//-- forward type declarations -----------------------------------------------
struct TRangeState;
class DELPHICLASS TSynM3Syn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkPragma, tkReserved, tkSpace, tkString, tkSymbol, tkUnknown, tkSyntaxError };

enum DECLSPEC_DENUM TTokenRange : unsigned char { trNone, trComment, trPragma };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TRangeState
{
	
public:
	union
	{
		struct 
		{
			System::Word TokenRange;
			System::Word Level;
		};
		struct 
		{
			void *p;
		};
		
	};
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TSynM3Syn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind FTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPragmaAttri;
	Synedithighlighter::TSynHighlighterAttributes* fReservedAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSyntaxErrorAttri;
	Synhighlighterhashentries::TSynHashEntryList* fKeywords;
	void __fastcall DoAddKeyword(System::UnicodeString AKeyword, int AKind);
	int __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall SymAsciiCharProc(void);
	void __fastcall SymCommentHelpProc(void);
	void __fastcall SymCRProc(void);
	void __fastcall SymIdentProc(void);
	void __fastcall SymLFProc(void);
	void __fastcall SymNestedHelperProc(System::WideChar AOpenChar, System::WideChar ACloseChar);
	void __fastcall SymNullProc(void);
	void __fastcall SymNumberProc(void);
	void __fastcall SymPragmaProc(void);
	void __fastcall SymPragmaHelpProc(void);
	void __fastcall SymRoundOpenProc(void);
	void __fastcall SymSpaceProc(void);
	void __fastcall SymStringProc(void);
	void __fastcall SymSymbolProc(void);
	void __fastcall SymUnknownProc(void);
	
protected:
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynM3Syn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynM3Syn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PragmaAttri = {read=fPragmaAttri, write=fPragmaAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ReservedAttri = {read=fReservedAttri, write=fReservedAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SyntaxErrorAttri = {read=fSyntaxErrorAttri, write=fSyntaxErrorAttri};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterm3 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERM3)
using namespace Synhighlighterm3;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Synhighlighterm3HPP
