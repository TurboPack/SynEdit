// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterMulti.pas' rev: 31.00 (Windows)

#ifndef SynhighlightermultiHPP
#define SynhighlightermultiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlightermulti
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TScheme;
class DELPHICLASS TSchemes;
class DELPHICLASS TMarker;
class DELPHICLASS TSynMultiSyn;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnCheckMarker)(System::TObject* Sender, int &StartPos, int &MarkerLen, System::UnicodeString &MarkerText, int Line, const System::UnicodeString LineStr);

class PASCALIMPLEMENTATION TScheme : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString fEndExpr;
	System::UnicodeString fStartExpr;
	Synedithighlighter::TSynCustomHighlighter* fHighlighter;
	Synedithighlighter::TSynHighlighterAttributes* fMarkerAttri;
	System::Classes::TComponentName fSchemeName;
	bool fCaseSensitive;
	TOnCheckMarker fOnCheckStartMarker;
	TOnCheckMarker fOnCheckEndMarker;
	System::UnicodeString __fastcall ConvertExpression(const System::UnicodeString Value);
	void __fastcall MarkerAttriChanged(System::TObject* Sender);
	void __fastcall SetMarkerAttri(Synedithighlighter::TSynHighlighterAttributes* const Value);
	void __fastcall SetHighlighter(Synedithighlighter::TSynCustomHighlighter* const Value);
	void __fastcall SetEndExpr(const System::UnicodeString Value);
	void __fastcall SetStartExpr(const System::UnicodeString Value);
	void __fastcall SetCaseSensitive(const bool Value);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	virtual void __fastcall SetDisplayName(const System::UnicodeString Value);
	
public:
	__fastcall virtual TScheme(System::Classes::TCollection* Collection);
	__fastcall virtual ~TScheme(void);
	
__published:
	__property bool CaseSensitive = {read=fCaseSensitive, write=SetCaseSensitive, default=1};
	__property System::UnicodeString StartExpr = {read=fStartExpr, write=SetStartExpr};
	__property System::UnicodeString EndExpr = {read=fEndExpr, write=SetEndExpr};
	__property Synedithighlighter::TSynCustomHighlighter* Highlighter = {read=fHighlighter, write=SetHighlighter};
	__property Synedithighlighter::TSynHighlighterAttributes* MarkerAttri = {read=fMarkerAttri, write=SetMarkerAttri};
	__property System::Classes::TComponentName SchemeName = {read=fSchemeName, write=fSchemeName};
	__property TOnCheckMarker OnCheckStartMarker = {read=fOnCheckStartMarker, write=fOnCheckStartMarker};
	__property TOnCheckMarker OnCheckEndMarker = {read=fOnCheckEndMarker, write=fOnCheckEndMarker};
};


typedef System::TMetaClass* TgmSchemeClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSchemes : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TScheme* operator[](int aIndex) { return this->Items[aIndex]; }
	
private:
	TSynMultiSyn* fOwner;
	TScheme* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TScheme* const Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TSchemes(TSynMultiSyn* aOwner);
	__property TScheme* Items[int aIndex] = {read=GetItems, write=SetItems/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TSchemes(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMarker : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	int fScheme;
	int fStartPos;
	int fMarkerLen;
	System::UnicodeString fMarkerText;
	bool fIsOpenMarker;
	
public:
	__fastcall TMarker(int aScheme, int aStartPos, int aMarkerLen, bool aIsOpenMarker, const System::UnicodeString aMarkerText);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TMarker(void) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TRangeOperation : unsigned char { roGet, roSet };

typedef NativeUInt TRangeUNativeInt;

typedef void __fastcall (__closure *TRangeProc)(TRangeOperation Operation, NativeUInt &Range);

typedef void __fastcall (__closure *TCustomRangeEvent)(TSynMultiSyn* Sender, TRangeOperation Operation, void * &Range);

class PASCALIMPLEMENTATION TSynMultiSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeProc fRangeProc;
	System::UnicodeString fDefaultLanguageName;
	System::Classes::TList* fMarkers;
	TMarker* fMarker;
	int fNextMarker;
	int fCurrScheme;
	void *fTmpRange;
	TCustomRangeEvent fOnCustomRange;
	System::UnicodeString fLineStr;
	void __fastcall SetDefaultHighlighter(Synedithighlighter::TSynCustomHighlighter* const Value);
	TMarker* __fastcall GetMarkers(int Index);
	__property TMarker* Markers[int Index] = {read=GetMarkers};
	void __fastcall DoCheckMarker(TScheme* Scheme, int StartPos, int MarkerLen, const System::UnicodeString MarkerText, bool Start, int Line, const System::UnicodeString LineStr);
	void __fastcall SetOnCustomRange(const TCustomRangeEvent Value);
	
protected:
	TSchemes* fSchemes;
	Synedithighlighter::TSynCustomHighlighter* fDefaultHighlighter;
	int fLineNumber;
	System::UnicodeString fSampleSource;
	virtual void __fastcall Loaded(void);
	void __fastcall SetSchemes(TSchemes* const Value);
	void __fastcall ClearMarkers(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual int __fastcall GetAttribCount(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetAttribute(int Index);
	void __fastcall HookHighlighter(Synedithighlighter::TSynCustomHighlighter* aHL);
	void __fastcall UnhookHighlighter(Synedithighlighter::TSynCustomHighlighter* aHL);
	virtual void __fastcall Notification(System::Classes::TComponent* aComp, System::Classes::TOperation aOp);
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual void __fastcall SetSampleSource(System::UnicodeString Value);
	virtual void __fastcall DoSetLine(const System::UnicodeString Value, int LineNumber);
	void __fastcall OldRangeProc(TRangeOperation Operation, NativeUInt &Range);
	void __fastcall NewRangeProc(TRangeOperation Operation, NativeUInt &Range);
	void __fastcall UserRangeProc(TRangeOperation Operation, NativeUInt &Range);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynMultiSyn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynMultiSyn(void);
	virtual bool __fastcall GetEol(void);
	virtual System::UnicodeString __fastcall GetExpandedToken(void);
	virtual void * __fastcall GetRange(void);
	virtual System::UnicodeString __fastcall GetToken(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	bool __fastcall UpdateRangeProcs(void);
	__property int CurrScheme = {read=fCurrScheme, write=fCurrScheme, nodefault};
	__property System::UnicodeString CurrLine = {read=fLineStr};
	virtual bool __fastcall LoadFromRegistry(HKEY RootKey, System::UnicodeString Key);
	virtual bool __fastcall SaveToRegistry(HKEY RootKey, System::UnicodeString Key);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	
__published:
	__property TSchemes* Schemes = {read=fSchemes, write=SetSchemes};
	__property Synedithighlighter::TSynCustomHighlighter* DefaultHighlighter = {read=fDefaultHighlighter, write=SetDefaultHighlighter};
	__property System::UnicodeString DefaultLanguageName = {read=fDefaultLanguageName, write=fDefaultLanguageName};
	__property TCustomRangeEvent OnCustomRange = {read=fOnCustomRange, write=SetOnCustomRange};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightermulti */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERMULTI)
using namespace Synhighlightermulti;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlightermultiHPP
