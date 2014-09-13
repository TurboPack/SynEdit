// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterGeneral.pas' rev: 28.00 (Windows)

#ifndef SynhighlightergeneralHPP
#define SynhighlightergeneralHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <SynEditHighlighter.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synhighlightergeneral
{
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkPreprocessor, tkSpace, tkString, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TCommentStyle : unsigned char { csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csBasStyle, csCPPStyle };

typedef System::Set<TCommentStyle, TCommentStyle::csAnsiStyle, TCommentStyle::csCPPStyle> TCommentStyles;

enum DECLSPEC_DENUM TRangeState : unsigned char { rsANil, rsAnsi, rsPasStyle, rsCStyle, rsUnKnown };

enum DECLSPEC_DENUM TStringDelim : unsigned char { sdSingleQuote, sdDoubleQuote, sdSingleAndDoubleQuote };

typedef void __fastcall (__closure *TGetTokenAttributeEvent)(Synedithighlighter::TSynHighlighterAttributes* attribute);

class DELPHICLASS TSynGeneralSyn;
class PASCALIMPLEMENTATION TSynGeneralSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	System::UnicodeString fIdentChars;
	TRangeState fRange;
	TtkTokenKind fTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPreprocessorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	System::Classes::TStrings* fKeyWords;
	TCommentStyles fComments;
	TStringDelim fStringDelim;
	bool fDetectPreprocessor;
	TGetTokenAttributeEvent fOnGetTokenAttribute;
	bool FStringMultiLine;
	void __fastcall AsciiCharProc(void);
	void __fastcall BraceOpenProc(void);
	void __fastcall PointCommaProc(void);
	void __fastcall CRProc(void);
	void __fastcall IdentProc(void);
	void __fastcall IntegerProc(void);
	void __fastcall LFProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall RoundOpenProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall AnsiProc(void);
	void __fastcall PasStyleProc(void);
	void __fastcall CStyleProc(void);
	void __fastcall SetKeyWords(System::Classes::TStrings* const Value);
	void __fastcall SetComments(TCommentStyles Value);
	TStringDelim __fastcall GetStringDelim(void);
	void __fastcall SetStringDelim(const TStringDelim Value);
	System::UnicodeString __fastcall GetIdentifierChars(void);
	void __fastcall SetIdentifierChars(const System::UnicodeString Value);
	bool __fastcall StoreIdentChars(void);
	void __fastcall SetDetectPreprocessor(bool Value);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	bool __fastcall IsStringDelim(System::WideChar aChar);
	__fastcall virtual TSynGeneralSyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynGeneralSyn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	System::WideChar __fastcall GetCharBeforeToken(int offset = 0xffffffff);
	System::WideChar __fastcall GetCharAfterToken(int offset = 0x1);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual bool __fastcall IsKeyword(const System::UnicodeString AKeyword);
	virtual bool __fastcall IsWordBreakChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual bool __fastcall SaveToRegistry(HKEY RootKey, System::UnicodeString Key);
	virtual bool __fastcall LoadFromRegistry(HKEY RootKey, System::UnicodeString Key);
	__property TGetTokenAttributeEvent OnGetTokenAttribute = {read=fOnGetTokenAttribute, write=fOnGetTokenAttribute};
	__property bool StringMultiLine = {read=FStringMultiLine, write=FStringMultiLine, nodefault};
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property TCommentStyles Comments = {read=fComments, write=SetComments, default=0};
	__property bool DetectPreprocessor = {read=fDetectPreprocessor, write=SetDetectPreprocessor, nodefault};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property System::UnicodeString IdentifierChars = {read=GetIdentifierChars, write=SetIdentifierChars, stored=StoreIdentChars};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property System::Classes::TStrings* KeyWords = {read=fKeyWords, write=SetKeyWords};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PreprocessorAttri = {read=fPreprocessorAttri, write=fPreprocessorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property TStringDelim StringDelim = {read=GetStringDelim, write=SetStringDelim, default=0};
};


//-- var, const, procedure ---------------------------------------------------
#define cDefaultIdentChars L"_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstu"\
	L"vwxyz"
}	/* namespace Synhighlightergeneral */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERGENERAL)
using namespace Synhighlightergeneral;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightergeneralHPP
