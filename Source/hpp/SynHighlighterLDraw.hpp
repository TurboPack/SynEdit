// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterLDraw.pas' rev: 29.00 (Windows)

#ifndef SynhighlighterldrawHPP
#define SynhighlighterldrawHPP

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
#include <SynEditHighlighter.hpp>
#include <SynEditTypes.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterldraw
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynLDRSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkColor, tkComment, tkFirstTri, tkFourthTri, tkIdentifier, tkKey, tkLine, tkNull, tkOpLine, tkQuad, tkSecondTri, tkThirdTri, tkTriangle, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynLDRSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 2> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fColorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fFirstTriAttri;
	Synedithighlighter::TSynHighlighterAttributes* fFourthTriAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fLineAttri;
	Synedithighlighter::TSynHighlighterAttributes* fOpLineAttri;
	Synedithighlighter::TSynHighlighterAttributes* fQuadAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSecondTriAttri;
	Synedithighlighter::TSynHighlighterAttributes* fThirdTriAttri;
	Synedithighlighter::TSynHighlighterAttributes* fTriangleAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAuthor(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall IdentProc(void);
	void __fastcall Number1Proc(void);
	void __fastcall UnknownProc(void);
	void __fastcall NullProc(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	System::WideChar __fastcall FirstChar(System::WideChar * DatLine);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__fastcall virtual TSynLDRSyn(System::Classes::TComponent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	virtual void * __fastcall GetRange(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual System::UnicodeString __fastcall GetKeyWords(int TokenKind);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* ColorAttri = {read=fColorAttri, write=fColorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* FirstTriAttri = {read=fFirstTriAttri, write=fFirstTriAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* FourthTriAttri = {read=fFourthTriAttri, write=fFourthTriAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* LineAttri = {read=fLineAttri, write=fLineAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* OpLineAttri = {read=fOpLineAttri, write=fOpLineAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* QuadAttri = {read=fQuadAttri, write=fQuadAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SecondTriAttri = {read=fSecondTriAttri, write=fSecondTriAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ThirdTriAttri = {read=fThirdTriAttri, write=fThirdTriAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* TriangleAttri = {read=fTriangleAttri, write=fTriangleAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynLDRSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterldraw */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERLDRAW)
using namespace Synhighlighterldraw;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterldrawHPP
