// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditWildcardSearch.pas' rev: 28.00 (Windows)

#ifndef SyneditwildcardsearchHPP
#define SyneditwildcardsearchHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <SynEdit.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <SynRegExpr.hpp>	// Pascal unit
#include <SynEditRegexSearch.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Syneditwildcardsearch
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSynEditWildcardSearch;
class PASCALIMPLEMENTATION TSynEditWildcardSearch : public Syneditregexsearch::TSynEditRegexSearch
{
	typedef Syneditregexsearch::TSynEditRegexSearch inherited;
	
private:
	System::UnicodeString fPattern;
	
protected:
	virtual System::UnicodeString __fastcall GetPattern(void);
	virtual void __fastcall SetPattern(const System::UnicodeString Value);
	virtual void __fastcall SetOptions(const Synedittypes::TSynSearchOptions Value);
	virtual int __fastcall GetLength(int Index);
	virtual int __fastcall GetResult(int Index);
	virtual int __fastcall GetResultCount(void);
	System::UnicodeString __fastcall WildCardToRegExpr(System::UnicodeString AWildCard);
	
public:
	__fastcall virtual TSynEditWildcardSearch(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynEditWildcardSearch(void);
	virtual int __fastcall FindAll(const System::UnicodeString NewText);
	virtual System::UnicodeString __fastcall Replace(const System::UnicodeString aOccurrence, const System::UnicodeString aReplacement);
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditwildcardsearch */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITWILDCARDSEARCH)
using namespace Syneditwildcardsearch;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditwildcardsearchHPP
