// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditSearch.pas' rev: 28.00 (Windows)

#ifndef SyneditsearchHPP
#define SyneditsearchHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <SynEditMiscClasses.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Syneditsearch
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSynEditSearch;
class PASCALIMPLEMENTATION TSynEditSearch : public Syneditmiscclasses::TSynEditSearchCustom
{
	typedef Syneditmiscclasses::TSynEditSearchCustom inherited;
	
private:
	System::WideChar *Run;
	System::WideChar *Origin;
	System::WideChar *TheEnd;
	System::UnicodeString Pat;
	System::UnicodeString CasedPat;
	int fCount;
	int fTextLen;
	int Look_At;
	int PatLen;
	int PatLenSucc;
	System::StaticArray<int, 65536> Shift;
	bool fCaseSensitive;
	bool fWhole;
	System::Classes::TList* fResults;
	bool fShiftInitialized;
	System::UnicodeString FTextToSearch;
	bool __fastcall GetFinished(void);
	void __fastcall InitShiftTable(void);
	void __fastcall SetCaseSensitive(const bool Value);
	
protected:
	bool __fastcall TestWholeWord(void);
	virtual void __fastcall SetPattern(const System::UnicodeString Value);
	virtual System::UnicodeString __fastcall GetPattern(void);
	virtual int __fastcall GetLength(int Index);
	virtual int __fastcall GetResult(int Index);
	virtual int __fastcall GetResultCount(void);
	virtual void __fastcall SetOptions(const Synedittypes::TSynSearchOptions Value);
	
public:
	__fastcall virtual TSynEditSearch(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TSynEditSearch(void);
	virtual int __fastcall FindAll(const System::UnicodeString NewText);
	virtual System::UnicodeString __fastcall Replace(const System::UnicodeString aOccurrence, const System::UnicodeString aReplacement);
	int __fastcall FindFirst(const System::UnicodeString NewText);
	void __fastcall FixResults(int First, int Delta);
	int __fastcall Next(void);
	__property int Count = {read=fCount, write=fCount, nodefault};
	__property bool Finished = {read=GetFinished, nodefault};
	__property Pattern = {read=CasedPat, default=0};
	__property bool CaseSensitive = {read=fCaseSensitive, write=SetCaseSensitive, nodefault};
	__property bool Whole = {read=fWhole, write=fWhole, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syneditsearch */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITSEARCH)
using namespace Syneditsearch;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditsearchHPP
