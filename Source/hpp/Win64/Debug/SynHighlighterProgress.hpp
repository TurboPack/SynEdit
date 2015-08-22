// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterProgress.pas' rev: 30.00 (Windows)

#ifndef SynhighlighterprogressHPP
#define SynhighlighterprogressHPP

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
#include <SynHighlighterHashEntries.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterprogress
{
//-- forward type declarations -----------------------------------------------
struct TRangeInfo;
class DELPHICLASS TSynProgressSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkEvent, tkIdentifier, tkInclude, tkKey, tkNonReserved, tkNull, tkNumber, tkPreprocessor, tkSpace, tkDataType, tkString, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsNone, rsInclude, rsPreprocessorDef, rsPreprocessor, rsComment };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TRangeInfo
{
	
public:
	union
	{
		struct 
		{
			System::Word Range;
			System::Word Level;
		};
		struct 
		{
			void *Ptr;
		};
		
	};
};
#pragma pack(pop)


typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynProgressSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	int fCommentLevel;
	int fIncludeLevel;
	int fPreProcessorLevel;
	TtkTokenKind FTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fEventAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIncludeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNonReservedKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPreprocessorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDataTypeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synhighlighterhashentries::TSynHashEntryList* fHashList;
	void __fastcall DoAddKeyword(System::UnicodeString AKeyword, int AKind);
	int __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall AsciiCharProc(void);
	void __fastcall CommentRangeProc(void);
	void __fastcall IncludeRangeProc(void);
	void __fastcall PreprocessorRangeProc(void);
	void __fastcall PreprocessorDefinitionProc(void);
	void __fastcall PreprocessorDefinitionRangeProc(void);
	void __fastcall BraceOpenProc(void);
	void __fastcall IdentProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StringProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall SymbolProc(void);
	
protected:
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__property Synhighlighterhashentries::TSynHashEntryList* Keywords = {read=fHashList};
	__fastcall virtual TSynProgressSyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynProgressSyn(void);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* EventAttri = {read=fEventAttri, write=fEventAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IncludeAttri = {read=fIncludeAttri, write=fIncludeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NonReservedKeyAttri = {read=fNonReservedKeyAttri, write=fNonReservedKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PreprocessorAttri = {read=fPreprocessorAttri, write=fPreprocessorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DataTypeAttri = {read=fDataTypeAttri, write=fDataTypeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString DefaultKeywords;
extern DELPHI_PACKAGE System::UnicodeString DefaultNonReservedKeywords;
extern DELPHI_PACKAGE System::UnicodeString DefaultEvents;
extern DELPHI_PACKAGE System::UnicodeString DefaultDataTypes;
}	/* namespace Synhighlighterprogress */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERPROGRESS)
using namespace Synhighlighterprogress;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterprogressHPP
