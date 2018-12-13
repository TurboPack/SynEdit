// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynUsp10.pas' rev: 33.00 (Windows)

#ifndef Synusp10HPP
#define Synusp10HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synusp10
{
//-- forward type declarations -----------------------------------------------
struct tag_SCRIPT_VISATTR;
struct tagGOFFSET;
//-- type declarations -------------------------------------------------------
typedef void * TScriptCache;

typedef void * *PScriptCache;

enum DECLSPEC_DENUM TScriptControl_enum : unsigned char { fContextDigits, fInvertPreBoundDir, fInvertPostBoundDir, fLinkStringBefore, fLinkStringAfter, fNeutralOverride, fNumericOverride, fLegacyBidiClass };

typedef System::Set<TScriptControl_enum, TScriptControl_enum::fContextDigits, TScriptControl_enum::fLegacyBidiClass> TScriptControl_set;

typedef tag_SCRIPT_CONTROL *PScriptControl;

typedef tag_SCRIPT_CONTROL TScriptControl;

enum DECLSPEC_DENUM TScriptState_enum : unsigned char { uBidiLevel_reserved1, uBidiLevel_r2, uBidiLevel_r3, uBidiLevel_r4, uBidiLevel_r5, fOverrideDirection, fInhibitSymSwap, fCharShape, fDigitSubstitute, fInhibitLigate, fDisplayZWG, fArabicNumContext, fGcpClusters };

typedef System::Set<TScriptState_enum, TScriptState_enum::uBidiLevel_reserved1, TScriptState_enum::fGcpClusters> TScriptState_set;

typedef tag_SCRIPT_STATE *PScriptState;

typedef tag_SCRIPT_STATE TScriptState;

enum DECLSPEC_DENUM TScriptAnalysis_enum : unsigned char { eScript_r1, eScript_r2, eScript_r3, eScript_r4, eScript_r5, eScript_r6, eScript_r7, eScript_r8, eScript_r9, eScript_r10, fRTL, fLayoutRTL, fLinkBefore, fLinkAfter, fLogicalOrder, fNoGlyphIndex };

typedef System::Set<TScriptAnalysis_enum, TScriptAnalysis_enum::eScript_r1, TScriptAnalysis_enum::fNoGlyphIndex> TScriptAnalysis_set;

typedef tag_SCRIPT_ANALYSIS *PScriptAnalysis;

typedef tag_SCRIPT_ANALYSIS TScriptAnalysis;

typedef tag_SCRIPT_ITEM *PScriptItem;

typedef tag_SCRIPT_ITEM TScriptItem;

enum DECLSPEC_DENUM tag_SCRIPT_JUSTIFY : unsigned char { SCRIPT_JUSTIFY_NONE, SCRIPT_JUSTIFY_ARABIC_BLANK, SCRIPT_JUSTIFY_CHARACTER, SCRIPT_JUSTIFY_RESERVED1, SCRIPT_JUSTIFY_BLANK, SCRIPT_JUSTIFY_RESERVED2, SCRIPT_JUSTIFY_RESERVED3, SCRIPT_JUSTIFY_ARABIC_NORMAL, SCRIPT_JUSTIFY_ARABIC_KASHIDA, SCRIPT_JUSTIFY_ARABIC_ALEF, SCRIPT_JUSTIFY_ARABIC_HA, SCRIPT_JUSTIFY_ARABIC_RA, SCRIPT_JUSTIFY_ARABIC_BA, SCRIPT_JUSTIFY_ARABIC_BARA, SCRIPT_JUSTIFY_ARABIC_SEEN, SCRIPT_JUSTIFY_RESERVED4 };

typedef tag_SCRIPT_JUSTIFY *PScriptJustify;

typedef tag_SCRIPT_JUSTIFY TScriptJustify;

enum DECLSPEC_DENUM TScriptVisAttr_enum : unsigned char { uJustification_r1, uJustification_r2, uJustification_r3, uJustification_r4, fClusterStart, fDiacritic, fZeroWidth, fReserved };

typedef System::Set<TScriptVisAttr_enum, TScriptVisAttr_enum::uJustification_r1, TScriptVisAttr_enum::fReserved> TScriptVisAttr_set;

typedef tag_SCRIPT_VISATTR *PScriptVisAttr;

#pragma pack(push,1)
struct DECLSPEC_DRECORD tag_SCRIPT_VISATTR
{
	
public:
	union
	{
		struct 
		{
			TScriptVisAttr_set fFlags;
			System::Byte fShapeReserved;
		};
		struct 
		{
			System::Byte uJustification;
		};
		
	};
};
#pragma pack(pop)


typedef tag_SCRIPT_VISATTR TScriptVisAttr;

typedef tagGOFFSET *PGOffset;

struct DECLSPEC_DRECORD tagGOFFSET
{
public:
	int du;
	int dv;
};


typedef tagGOFFSET TGOffset;

enum DECLSPEC_DENUM TScriptLogAttr_enum : unsigned char { fSoftBreak, fWhiteSpace, fCharStop, fWordStop, fInvalid };

typedef tag_SCRIPT_LOGATTR *PScriptLogAttr;

typedef tag_SCRIPT_LOGATTR TScriptLogAttr;

enum DECLSPEC_DENUM TScriptProperties_enum : unsigned char { fNumeric, fComplex, fNeedsWordBreaking, fNeedsCaretInfo, bCharSet, fControl, fPrivateUseArea, fNeedsCharacterJustify, fInvalidGlyph, fInvalidLogAttr, fCDM, fAmbiguousCharSet, fClusterSizeVaries, fRejectInvalid };

typedef System::Set<TScriptProperties_enum, TScriptProperties_enum::fNumeric, TScriptProperties_enum::fRejectInvalid> TScriptProperties_set;

typedef SCRIPT_PROPERTIES *PScriptProperties;

typedef SCRIPT_PROPERTIES TScriptProperties;

typedef SCRIPT_FONTPROPERTIES *PScriptFontProperties;

typedef SCRIPT_FONTPROPERTIES TScriptFontProperties;

typedef tag_SCRIPT_TABDEF *PScriptTabDef;

typedef tag_SCRIPT_TABDEF TScriptTabDef;

typedef void * TScriptStringAnalysis;

typedef void * *PScriptStringAnalysis;

typedef tag_SCRIPT_DIGITSUBSTITUTE *PScriptDigitSubstitute;

typedef tag_SCRIPT_DIGITSUBSTITUTE TScriptDigitSubstitute;

//-- var, const, procedure ---------------------------------------------------
static const System::Word USPBUILD = System::Word(0x190);
static const System::Int8 SCRIPT_UNDEFINED = System::Int8(0x0);
static const unsigned USP_E_SCRIPT_NOT_IN_FONT = unsigned(0x80040200);
static const System::Int8 MASK_uBidiLevel = System::Int8(0x1f);
static const System::Word MASK_eScript = System::Word(0x3ff);
static const System::Int8 MASK_uJustification = System::Int8(0xf);
static const System::Int8 SGCM_RTL = System::Int8(0x1);
static const System::Int8 SSA_PASSWORD = System::Int8(0x1);
static const System::Int8 SSA_TAB = System::Int8(0x2);
static const System::Int8 SSA_CLIP = System::Int8(0x4);
static const System::Int8 SSA_FIT = System::Int8(0x8);
static const System::Int8 SSA_DZWG = System::Int8(0x10);
static const System::Int8 SSA_FALLBACK = System::Int8(0x20);
static const System::Int8 SSA_BREAK = System::Int8(0x40);
static const System::Byte SSA_GLYPHS = System::Byte(0x80);
static const System::Word SSA_RTL = System::Word(0x100);
static const System::Word SSA_GCP = System::Word(0x200);
static const System::Word SSA_HOTKEY = System::Word(0x400);
static const System::Word SSA_METAFILE = System::Word(0x800);
static const System::Word SSA_LINK = System::Word(0x1000);
static const System::Word SSA_HIDEHOTKEY = System::Word(0x2000);
static const System::Word SSA_HOTKEYONLY = System::Word(0x2400);
static const int SSA_FULLMEASURE = int(0x4000000);
static const int SSA_LPKANSIFALLBACK = int(0x8000000);
static const int SSA_PIDX = int(0x10000000);
static const int SSA_LAYOUTRTL = int(0x20000000);
static const int SSA_DONTGLYPH = int(0x40000000);
static const unsigned SSA_NOKASHIDA = unsigned(0x80000000);
static const System::Int8 SIC_COMPLEX = System::Int8(0x1);
static const System::Int8 SIC_ASCIIDIGIT = System::Int8(0x2);
static const System::Int8 SIC_NEUTRAL = System::Int8(0x4);
static const System::Int8 SCRIPT_DIGITSUBSTITUTE_CONTEXT = System::Int8(0x0);
static const System::Int8 SCRIPT_DIGITSUBSTITUTE_NONE = System::Int8(0x1);
static const System::Int8 SCRIPT_DIGITSUBSTITUTE_NATIONAL = System::Int8(0x2);
static const System::Int8 SCRIPT_DIGITSUBSTITUTE_TRADITIONAL = System::Int8(0x3);
extern DELPHI_PACKAGE bool Usp10IsInstalled;
extern DELPHI_PACKAGE HRESULT __stdcall ScriptFreeCache(PScriptCache psc);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptTextOut(const HDC hdc, PScriptCache psc, int x, int y, unsigned fuOptions, const System::Types::PRect lprc, const PScriptAnalysis psa, const System::WideChar * pwcReserved, int iReserved, const PWORD pwGlyphs, int cGlyphs, const System::PInteger piAdvance, const System::PInteger piJustify, const PGOffset pGoffset);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptJustify(const PScriptVisAttr psva, const System::PInteger piAdvance, int cGlyphs, int iDx, int iMinKashida, System::PInteger piJustify);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptCPtoX(int iCP, System::LongBool fTrailing, int cChars, int cGlyphs, const PWORD pwLogClust, const PScriptVisAttr psva, const System::PInteger piAdvance, const PScriptAnalysis psa, System::PInteger piX);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptXtoCP(int iX, int cChars, int cGlyphs, const PWORD pwLogClust, const PScriptVisAttr psva, const int piAdvance, const PScriptAnalysis psa, System::PInteger piCP, System::PInteger piTrailing);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptGetLogicalWidths(const PScriptAnalysis psa, int cChars, int cGlyphs, const System::PInteger piGlyphWidth, const PWORD pwLogClust, const PScriptVisAttr psva, System::PInteger piDx);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptApplyLogicalWidth(const System::PInteger piDx, int cChars, int cGlyphs, const PWORD pwLogClust, const PScriptVisAttr psva, const System::PInteger piAdvance, const PScriptAnalysis psa, PABC pABC, System::PInteger piJustify);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptGetCMap(HDC hdc, PScriptCache psc, const System::WideChar * pwcInChars, int cChars, unsigned dwFlags, PWORD pwOutGlyphs);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptGetGlyphABCWidth(HDC hdc, PScriptCache psc, System::Word wGlyph, PABC pABC);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptGetProperties(/* out */ PScriptProperties &ppSp, /* out */ int &piNumScripts);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptGetFontProperties(HDC hdc, PScriptCache psc, PScriptFontProperties sfp);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptCacheGetHeight(HDC hdc, PScriptCache psc, System::PInteger tmHeight);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringAnalyse(HDC hdc, const void * pString, int cString, int cGlyphs, int iCharset, unsigned dwFlags, int iReqWidth, PScriptControl psControl, PScriptState psState, const System::PInteger piDx, PScriptTabDef pTabdef, const System::PByte pbInClass, PScriptStringAnalysis pssa);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringFree(PScriptStringAnalysis pssa);
extern DELPHI_PACKAGE System::Types::PSize __stdcall ScriptString_pSize(void * ssa);
extern DELPHI_PACKAGE System::PInteger __stdcall ScriptString_pcOutChars(void * ssa);
extern DELPHI_PACKAGE PScriptLogAttr __stdcall ScriptString_pLogAttr(void * ssa);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringGetOrder(void * ssa, System::PLongWord puOrder);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringCPtoX(void * ssa, int icp, System::LongBool fTrailing, /* out */ int &pX);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringXtoCP(void * ssa, int iX, System::PInteger piCh, System::PInteger piTrailing);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringGetLogicalWidths(void * ssa, /* out */ int &piDx);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringValidate(void * ssa);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptStringOut(void * ssa, int iX, int iY, unsigned uOptions, const System::Types::PRect prc, int iMinSel, int iMaxSel, System::LongBool fDisabled);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptIsComplex(const System::WideChar * pwcInChars, int cInChars, unsigned dwFlags);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptRecordDigitSubstitution(unsigned Locale, /* out */ tag_SCRIPT_DIGITSUBSTITUTE &psds);
extern DELPHI_PACKAGE HRESULT __stdcall ScriptApplyDigitSubstitution(const PScriptDigitSubstitute psds, PScriptControl psc, PScriptState pss);
}	/* namespace Synusp10 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNUSP10)
using namespace Synusp10;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Synusp10HPP
