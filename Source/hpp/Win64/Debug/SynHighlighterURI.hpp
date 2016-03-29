// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterURI.pas' rev: 31.00 (Windows)

#ifndef SynhighlighteruriHPP
#define SynhighlighteruriHPP

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

namespace Synhighlighteruri
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynURISyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkNull, tkSpace, tkFtpLink, tkGopherLink, tkHttpLink, tkHttpsLink, tkMailtoLink, tkNewsLink, tkNntpLink, tkProsperoLink, tkTelnetLink, tkWaisLink, tkWebLink, tkUnknown, tkNullChar };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Key);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

typedef bool __fastcall (__closure *TAlreadyVisitedURIFunc)(System::UnicodeString URI);

class PASCALIMPLEMENTATION TSynURISyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	System::WideChar *fMayBeProtocol;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 16> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fURIAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVisitedURIAttri;
	TAlreadyVisitedURIFunc fAlreadyVisitedURI;
	int __fastcall HashKey(System::WideChar * Str);
	void __fastcall InitIdent(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	void __fastcall NullProc(void);
	void __fastcall ProtocolProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall UnknownProc(void);
	TtkTokenKind __fastcall AltFunc(int Key);
	TtkTokenKind __fastcall FuncFtp(int Key);
	TtkTokenKind __fastcall FuncGopher(int Key);
	TtkTokenKind __fastcall FuncHttp(int Key);
	TtkTokenKind __fastcall FuncHttps(int Key);
	TtkTokenKind __fastcall FuncMailto(int Key);
	TtkTokenKind __fastcall FuncNews(int Key);
	TtkTokenKind __fastcall FuncNntp(int Key);
	TtkTokenKind __fastcall FuncProspero(int Key);
	TtkTokenKind __fastcall FuncTelnet(int Key);
	TtkTokenKind __fastcall FuncWais(int Key);
	TtkTokenKind __fastcall FuncWeb(int Key);
	bool __fastcall IsAlphaNum(System::WideChar AChar);
	bool __fastcall IsMark(System::WideChar AChar);
	bool __fastcall IsReserved(System::WideChar AChar);
	bool __fastcall IsUnreserved(System::WideChar AChar);
	bool __fastcall IsURIChar(System::WideChar AChar);
	bool __fastcall IsNeverAtEnd(System::WideChar AChar);
	bool __fastcall IsEMailAddressChar(System::WideChar AChar);
	bool __fastcall IsNeverAtEMailAddressEnd(System::WideChar AChar);
	bool __fastcall IsValidEmailAddress(void);
	bool __fastcall IsValidURI(void);
	bool __fastcall IsValidWebLink(void);
	void __fastcall SetURIAttri(Synedithighlighter::TSynHighlighterAttributes* const Value);
	void __fastcall SetVisitedURIAttri(Synedithighlighter::TSynHighlighterAttributes* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsCurrentToken(const System::UnicodeString Token);
	virtual bool __fastcall IsFilterStored(void);
	void __fastcall SetAlreadyVisitedURIFunc(TAlreadyVisitedURIFunc Value);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynURISyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynURISyn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* URIAttri = {read=fURIAttri, write=SetURIAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VisitedURIAttri = {read=fVisitedURIAttri, write=SetVisitedURIAttri};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 SYN_ATTR_URI = System::Int8(0x6);
static const System::Int8 SYN_ATTR_VISITEDURI = System::Int8(0x7);
}	/* namespace Synhighlighteruri */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERURI)
using namespace Synhighlighteruri;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighteruriHPP
