// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterMsg.pas' rev: 28.00 (Windows)

#ifndef SynhighlightermsgHPP
#define SynhighlightermsgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <SynEditHighlighter.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synhighlightermsg
{
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkString, tkSymbol, tkTerminator, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown, rsBraceComment, rsString };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class DELPHICLASS TSynMsgSyn;
class PASCALIMPLEMENTATION TSynMsgSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 7> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fTerminatorAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncBeginproc(int Index);
	TtkTokenKind __fastcall FuncChars(int Index);
	TtkTokenKind __fastcall FuncEnclosedby(int Index);
	TtkTokenKind __fastcall FuncEndproc(int Index);
	TtkTokenKind __fastcall FuncKeys(int Index);
	TtkTokenKind __fastcall FuncSamplesource(int Index);
	TtkTokenKind __fastcall FuncTokentypes(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall IdentProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall TerminatorProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall NullProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	void __fastcall BraceCommentOpenProc(void);
	void __fastcall BraceCommentProc(void);
	void __fastcall StringOpenProc(void);
	void __fastcall StringProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__fastcall virtual TSynMsgSyn(System::Classes::TComponent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	virtual void * __fastcall GetRange(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* TerminatorAttri = {read=fTerminatorAttri, write=fTerminatorAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynMsgSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightermsg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERMSG)
using namespace Synhighlightermsg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightermsgHPP
