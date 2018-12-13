// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterSQL.pas' rev: 33.00 (Windows)

#ifndef SynhighlightersqlHPP
#define SynhighlightersqlHPP

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

namespace Synhighlightersql
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynSQLSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkDatatype, tkDefaultPackage, tkException, tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkPLSQL, tkSQLPlus, tkString, tkSymbol, tkTableName, tkUnknown, tkVariable, tkConditionalComment, tkDelimitedIdentifier };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsComment, rsString, rsConditionalComment };

enum DECLSPEC_DENUM TSQLDialect : unsigned char { sqlStandard, sqlInterbase6, sqlMSSQL7, sqlMySQL, sqlOracle, sqlSybase, sqlIngres, sqlMSSQL2K, sqlPostgres, sqlNexus };

class PASCALIMPLEMENTATION TSynSQLSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	Synhighlighterhashentries::TSynHashEntryList* fKeywords;
	System::Classes::TStrings* fTableNames;
	System::Classes::TStrings* fFunctionNames;
	TSQLDialect fDialect;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fConditionalCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDataTypeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDefaultPackageAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDelimitedIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fExceptionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fFunctionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fPLSQLAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSQLPlusAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fTableNameAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVariableAttri;
	int __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall DoAddKeyword(System::UnicodeString AKeyword, int AKind);
	void __fastcall SetDialect(TSQLDialect Value);
	void __fastcall SetTableNames(System::Classes::TStrings* const Value);
	void __fastcall SetFunctionNames(System::Classes::TStrings* const Value);
	void __fastcall PutFunctionNamesInKeywordList();
	void __fastcall TableNamesChanged(System::TObject* Sender);
	void __fastcall InitializeKeywordLists();
	void __fastcall PutTableNamesInKeywordList();
	void __fastcall AndSymbolProc();
	void __fastcall AsciiCharProc();
	void __fastcall CRProc();
	void __fastcall EqualProc();
	void __fastcall GreaterProc();
	void __fastcall IdentProc();
	void __fastcall LFProc();
	void __fastcall LowerProc();
	void __fastcall MinusProc();
	void __fastcall HashProc();
	void __fastcall NullProc();
	void __fastcall NumberProc();
	void __fastcall OrSymbolProc();
	void __fastcall PlusProc();
	void __fastcall SlashProc();
	void __fastcall SpaceProc();
	void __fastcall QuoteProc();
	void __fastcall BacktickProc();
	void __fastcall BracketProc();
	void __fastcall SymbolProc();
	void __fastcall SymbolAssignProc();
	void __fastcall VariableProc();
	void __fastcall UnknownProc();
	void __fastcall AnsiCProc();
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource();
	virtual bool __fastcall IsFilterStored();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynSQLSyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynSQLSyn();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	virtual System::UnicodeString __fastcall GetKeyWords(int TokenKind);
	virtual void * __fastcall GetRange();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	TtkTokenKind __fastcall GetTokenID();
	virtual int __fastcall GetTokenKind();
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual bool __fastcall IsKeyword(const System::UnicodeString AKeyword);
	virtual void __fastcall Next();
	virtual void __fastcall ResetRange();
	virtual void __fastcall SetRange(void * Value);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ConditionalCommentAttri = {read=fConditionalCommentAttri, write=fConditionalCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DataTypeAttri = {read=fDataTypeAttri, write=fDataTypeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DefaultPackageAttri = {read=fDefaultPackageAttri, write=fDefaultPackageAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DelimitedIdentifierAttri = {read=fDelimitedIdentifierAttri, write=fDelimitedIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ExceptionAttri = {read=fExceptionAttri, write=fExceptionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* FunctionAttri = {read=fFunctionAttri, write=fFunctionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* PLSQLAttri = {read=fPLSQLAttri, write=fPLSQLAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SQLPlusAttri = {read=fSQLPlusAttri, write=fSQLPlusAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* TableNameAttri = {read=fTableNameAttri, write=fTableNameAttri};
	__property System::Classes::TStrings* TableNames = {read=fTableNames, write=SetTableNames};
	__property System::Classes::TStrings* FunctionNames = {read=fFunctionNames, write=SetFunctionNames};
	__property Synedithighlighter::TSynHighlighterAttributes* VariableAttri = {read=fVariableAttri, write=fVariableAttri};
	__property TSQLDialect SQLDialect = {read=fDialect, write=SetDialect, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightersql */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERSQL)
using namespace Synhighlightersql;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightersqlHPP
