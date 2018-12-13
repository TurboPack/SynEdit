// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterHP48.pas' rev: 32.00 (Windows)

#ifndef Synhighlighterhp48HPP
#define Synhighlighterhp48HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterhp48
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSpeedListObject;
class DELPHICLASS TSpeedStringList;
class DELPHICLASS TSynHP48Syn;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TSpeedListObject : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::UnicodeString FName;
	TSpeedStringList* FSpeedList;
	System::TObject* FObject;
	virtual void __fastcall SetName(const System::UnicodeString Value);
	
public:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__fastcall TSpeedListObject(System::UnicodeString name);
	__fastcall virtual ~TSpeedListObject(void);
	__property TSpeedStringList* SpeedList = {read=FSpeedList, write=FSpeedList};
	__property System::TObject* Pointer = {read=FObject, write=FObject};
};


typedef System::StaticArray<TSpeedListObject*, 1> TSpeedListObjects;

typedef TSpeedListObjects *PSpeedListObjects;

class PASCALIMPLEMENTATION TSpeedStringList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString operator[](int Index) { return this->Strings[Index]; }
	
private:
	System::UnicodeString __fastcall GetText(void);
	void __fastcall SetText(const System::UnicodeString Value);
	System::TObject* __fastcall GetInObject(int Index);
	void __fastcall SetInObject(int Index, System::TObject* const Value);
	
protected:
	System::Classes::TNotifyEvent FOnChange;
	System::StaticArray<int, 128> SumOfUsed;
	System::StaticArray<int, 128> DatasUsed;
	System::StaticArray<PSpeedListObjects, 128> Datas;
	System::StaticArray<int, 128> LengthDatas;
	virtual void __fastcall Changed(void);
	virtual System::UnicodeString __fastcall Get(int Index);
	TSpeedListObject* __fastcall GetObject(int Index);
	int __fastcall GetCount(void);
	System::Classes::TStrings* __fastcall GetStringList(void);
	void __fastcall SetStringList(System::Classes::TStrings* const Value);
	
public:
	void __fastcall NameChange(TSpeedListObject* const obj, const System::UnicodeString NewName);
	void __fastcall ObjectDeleted(TSpeedListObject* const obj);
	__fastcall virtual ~TSpeedStringList(void);
	__fastcall TSpeedStringList(void);
	int __fastcall AddObj(TSpeedListObject* const Value);
	TSpeedListObject* __fastcall Add(const System::UnicodeString Value);
	void __fastcall Clear(void);
	TSpeedListObject* __fastcall Find(const System::UnicodeString Name);
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TSpeedListObject* Objects[int Index] = {read=GetObject};
	__property System::TObject* InObject[int Index] = {read=GetInObject, write=SetInObject};
	__property System::UnicodeString Strings[int Index] = {read=Get/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	__property System::Classes::TStrings* StringList = {read=GetStringList, write=SetStringList};
	__property System::UnicodeString Text = {read=GetText, write=SetText};
};


enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkNull, tkAsmKey, tkAsm, tkAsmComment, tksAsmKey, tksAsm, tksAsmComment, tkRplKey, tkRpl, tkRplComment };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsRpl, rsComRpl, rssasm1, rssasm2, rssasm3, rsAsm, rsComAsm2, rsComAsm1 };

class PASCALIMPLEMENTATION TSynHP48Syn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TtkTokenKind fTockenKind;
	TRangeState fRange;
	System::StaticArray<Synedithighlighter::TSynHighlighterAttributes*, 10> Attribs;
	TSpeedStringList* FRplKeyWords;
	TSpeedStringList* FAsmKeyWords;
	TSpeedStringList* FSAsmNoField;
	TRangeState FBaseRange;
	Synedithighlighter::TSynHighlighterAttributes* __fastcall GetAttrib(int Index);
	void __fastcall SetAttrib(int Index, Synedithighlighter::TSynHighlighterAttributes* Value);
	TtkTokenKind __fastcall NullProc(void);
	TtkTokenKind __fastcall SpaceProc(void);
	TtkTokenKind __fastcall ParOpenProc(void);
	TtkTokenKind __fastcall RplComProc(void);
	TtkTokenKind __fastcall AsmComProc(System::WideChar c);
	TtkTokenKind __fastcall PersentProc(void);
	TtkTokenKind __fastcall IdentProc(void);
	TtkTokenKind __fastcall SlashProc(void);
	TtkTokenKind __fastcall SasmProc1(void);
	TtkTokenKind __fastcall SasmProc2(void);
	TtkTokenKind __fastcall SasmProc3(void);
	void __fastcall EndOfToken(void);
	void __fastcall SetHighLightChange(void);
	TtkTokenKind __fastcall Next1(void);
	void __fastcall Next2(TtkTokenKind tkk);
	TtkTokenKind __fastcall GetTokenFromRange(void);
	TtkTokenKind __fastcall StarProc(void);
	
protected:
	virtual int __fastcall GetAttribCount(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetAttribute(int idx);
	virtual bool __fastcall IsFilterStored(void);
	virtual bool __fastcall IsLineEnd(int Run);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynHP48Syn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynHP48Syn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void __fastcall DoSetLine(const System::UnicodeString Value, int LineNumber);
	virtual void __fastcall Next(void);
	virtual System::UnicodeString __fastcall GetToken(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void * __fastcall GetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	virtual bool __fastcall SaveToRegistry(HKEY RootKey, System::UnicodeString Key);
	virtual bool __fastcall LoadFromRegistry(HKEY RootKey, System::UnicodeString Key);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TSpeedStringList* AsmKeyWords = {read=FAsmKeyWords};
	__property TSpeedStringList* SAsmFoField = {read=FSAsmNoField};
	__property TSpeedStringList* RplKeyWords = {read=FRplKeyWords};
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* AsmKey = {read=GetAttrib, write=SetAttrib, index=1};
	__property Synedithighlighter::TSynHighlighterAttributes* AsmTxt = {read=GetAttrib, write=SetAttrib, index=2};
	__property Synedithighlighter::TSynHighlighterAttributes* AsmComment = {read=GetAttrib, write=SetAttrib, index=3};
	__property Synedithighlighter::TSynHighlighterAttributes* sAsmKey = {read=GetAttrib, write=SetAttrib, index=4};
	__property Synedithighlighter::TSynHighlighterAttributes* sAsmTxt = {read=GetAttrib, write=SetAttrib, index=5};
	__property Synedithighlighter::TSynHighlighterAttributes* sAsmComment = {read=GetAttrib, write=SetAttrib, index=6};
	__property Synedithighlighter::TSynHighlighterAttributes* RplKey = {read=GetAttrib, write=SetAttrib, index=7};
	__property Synedithighlighter::TSynHighlighterAttributes* RplTxt = {read=GetAttrib, write=SetAttrib, index=8};
	__property Synedithighlighter::TSynHighlighterAttributes* RplComment = {read=GetAttrib, write=SetAttrib, index=9};
	__property TRangeState BaseRange = {read=FBaseRange, write=FBaseRange, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte NbSubList = System::Byte(0x80);
}	/* namespace Synhighlighterhp48 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERHP48)
using namespace Synhighlighterhp48;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Synhighlighterhp48HPP
