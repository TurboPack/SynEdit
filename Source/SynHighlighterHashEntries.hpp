// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterHashEntries.pas' rev: 28.00 (Windows)

#ifndef SynhighlighterhashentriesHPP
#define SynhighlighterhashentriesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterhashentries
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSynHashEntry;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynHashEntry : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TSynHashEntry* fNext;
	int fKeyLen;
	System::UnicodeString fKeyword;
	int fKind;
	
public:
	virtual TSynHashEntry* __fastcall AddEntry(TSynHashEntry* NewEntry);
	__fastcall TSynHashEntry(const System::UnicodeString AKey, int AKind);
	__fastcall virtual ~TSynHashEntry(void);
	__property System::UnicodeString Keyword = {read=fKeyword};
	__property int KeywordLen = {read=fKeyLen, nodefault};
	__property int Kind = {read=fKind, nodefault};
	__property TSynHashEntry* Next = {read=fNext};
};

#pragma pack(pop)

class DELPHICLASS TSynHashEntryList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynHashEntryList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TSynHashEntry* operator[](int Index) { return Items[Index]; }
	
protected:
	HIDESBASE TSynHashEntry* __fastcall Get(int HashKey);
	HIDESBASE void __fastcall Put(int HashKey, TSynHashEntry* Entry);
	
public:
	__fastcall virtual ~TSynHashEntryList(void);
	void __fastcall DeleteEntries(void);
	__property TSynHashEntry* Items[int Index] = {read=Get, write=Put/*, default*/};
public:
	/* TObject.Create */ inline __fastcall TSynHashEntryList(void) : System::Classes::TList() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TEnumerateKeywordEvent)(System::UnicodeString AKeyword, int AKind);

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall EnumerateKeywords(int AKind, System::UnicodeString KeywordList, Synedittypes::TCategoryMethod IsIdentChar, TEnumerateKeywordEvent AKeywordProc);
}	/* namespace Synhighlighterhashentries */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERHASHENTRIES)
using namespace Synhighlighterhashentries;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterhashentriesHPP
