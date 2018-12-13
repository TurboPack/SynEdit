// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterTclTk.pas' rev: 33.00 (Windows)

#ifndef SynhighlightertcltkHPP
#define SynhighlightertcltkHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlightertcltk
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynTclTkSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkSymbol, tkKey, tkComment, tkIdentifier, tkNull, tkNumber, tkSecondKey, tkTixKey, tkSpace, tkString, tkOptions, tkVariable, tkWidgetKey, tkPath, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsAnsi, rsPasStyle, rsCStyle };

class PASCALIMPLEMENTATION TSynTclTkSyn : public Synedithighlighter::TSynCustomHighlighter
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
	Synedithighlighter::TSynHighlighterAttributes* fOptionsAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVariableAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPathAttri;
	System::Classes::TStrings* fKeyWords;
	System::Classes::TStrings* fSecondKeys;
	System::Classes::TStrings* fTixWords;
	Synedithighlighter::TSynHighlighterAttributes* fTixKeyAttri;
	System::Classes::TStrings* fWidgetWords;
	Synedithighlighter::TSynHighlighterAttributes* fWidgetKeyAttri;
	void __fastcall BraceOpenProc();
	void __fastcall PointCommaProc();
	void __fastcall CRProc();
	void __fastcall IdentProc();
	void __fastcall LFProc();
	void __fastcall NullProc();
	void __fastcall NumberProc();
	void __fastcall RoundOpenProc();
	void __fastcall SlashProc();
	void __fastcall SpaceProc();
	void __fastcall StringProc();
	void __fastcall UnknownProc();
	void __fastcall AnsiProc();
	void __fastcall PasStyleProc();
	void __fastcall CStyleProc();
	void __fastcall VariableProc();
	void __fastcall PathProc();
	void __fastcall MinusProc();
	void __fastcall SymbolProc();
	void __fastcall SetKeyWords(System::Classes::TStrings* const Value);
	void __fastcall SetSecondKeys(System::Classes::TStrings* const Value);
	bool __fastcall IsKeywordListStored();
	bool __fastcall IsSecondKeywordListStored();
	bool __fastcall InternalIsKeyword(const System::UnicodeString AKeyword, System::Classes::TStrings* KeyWordList, bool ACaseSensitive = false);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource();
	virtual bool __fastcall IsFilterStored();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynTclTkSyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynTclTkSyn();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	virtual void * __fastcall GetRange();
	TtkTokenKind __fastcall GetTokenID();
	virtual bool __fastcall IsKeyword(const System::UnicodeString AKeyword);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	virtual int __fastcall GetTokenKind();
	virtual void __fastcall Next();
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange();
	virtual bool __fastcall SaveToRegistry(HKEY RootKey, System::UnicodeString Key);
	virtual bool __fastcall LoadFromRegistry(HKEY RootKey, System::UnicodeString Key);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property System::Classes::TStrings* KeyWords = {read=fKeyWords, write=SetKeyWords, stored=IsKeywordListStored};
	__property Synedithighlighter::TSynHighlighterAttributes* SecondKeyAttri = {read=fSecondKeyAttri, write=fSecondKeyAttri};
	__property System::Classes::TStrings* SecondKeyWords = {read=fSecondKeys, write=SetSecondKeys, stored=IsSecondKeywordListStored};
	__property Synedithighlighter::TSynHighlighterAttributes* TixKeyAttri = {read=fTixKeyAttri, write=fTixKeyAttri};
	__property System::Classes::TStrings* TixWords = {read=fTixWords};
	__property Synedithighlighter::TSynHighlighterAttributes* WidgetKeyAttri = {read=fWidgetKeyAttri, write=fWidgetKeyAttri};
	__property System::Classes::TStrings* WidgetWords = {read=fWidgetWords};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* OptionsAttri = {read=fOptionsAttri, write=fOptionsAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PathAttri = {read=fPathAttri, write=fPathAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VariableAttri = {read=fVariableAttri, write=fVariableAttri};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightertcltk */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERTCLTK)
using namespace Synhighlightertcltk;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightertcltkHPP
