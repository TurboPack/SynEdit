// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterRC.pas' rev: 30.00 (Windows)

#ifndef SynhighlighterrcHPP
#define SynhighlighterrcHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterrc
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynRCSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsDirective, rsComment };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynRCSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 241> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDirecAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall KeyWordFunc(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall CommentProc(void);
	void __fastcall CRProc(void);
	void __fastcall DirectiveProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall QuoteProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall UnknownProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual Synedithighlighter::TSynHighlighterCapabilities __fastcall GetCapabilities();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynRCSyn(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TSynRCSyn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * value);
	virtual void __fastcall ResetRange(void);
	virtual bool __fastcall UseUserSettings(int SettingIndex);
	virtual void __fastcall EnumUserSettings(System::Classes::TStrings* Settings);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DirecAttri = {read=fDirecAttri, write=fDirecAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterrc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERRC)
using namespace Synhighlighterrc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterrcHPP
