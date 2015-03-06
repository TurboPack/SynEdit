// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditHighlighter.pas' rev: 29.00 (Windows)

#ifndef SynedithighlighterHPP
#define SynedithighlighterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <System.IniFiles.hpp>
#include <SynEditTypes.hpp>
#include <SynEditMiscClasses.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <SynEditHighlighterOptions.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synedithighlighter
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynHighlighterAttributes;
class DELPHICLASS TSynCustomHighlighter;
class DELPHICLASS TSynHighlighterList;
//-- type declarations -------------------------------------------------------
;

class PASCALIMPLEMENTATION TSynHighlighterAttributes : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Uitypes::TColor fBackground;
	System::Uitypes::TColor fBackgroundDefault;
	System::Uitypes::TColor fForeground;
	System::Uitypes::TColor fForegroundDefault;
	System::UnicodeString fFriendlyName;
	System::UnicodeString fName;
	System::Uitypes::TFontStyles fStyle;
	System::Uitypes::TFontStyles fStyleDefault;
	System::Classes::TNotifyEvent fOnChange;
	virtual void __fastcall Changed(void);
	bool __fastcall GetBackgroundColorStored(void);
	bool __fastcall GetForegroundColorStored(void);
	bool __fastcall GetFontStyleStored(void);
	void __fastcall SetBackground(System::Uitypes::TColor Value);
	void __fastcall SetForeground(System::Uitypes::TColor Value);
	void __fastcall SetStyle(System::Uitypes::TFontStyles Value);
	int __fastcall GetStyleFromInt(void);
	void __fastcall SetStyleFromInt(const int Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AssignColorAndStyle(TSynHighlighterAttributes* Source);
	__fastcall TSynHighlighterAttributes(System::UnicodeString AName, System::UnicodeString AFriendlyName);
	void __fastcall InternalSaveDefaultValues(void);
	virtual bool __fastcall LoadFromBorlandRegistry(HKEY RootKey, System::UnicodeString AttrKey, System::UnicodeString AttrName, bool OldStyle);
	bool __fastcall LoadFromRegistry(Syneditmiscclasses::TBetterRegistry* Reg);
	bool __fastcall SaveToRegistry(Syneditmiscclasses::TBetterRegistry* Reg);
	bool __fastcall LoadFromFile(System::Inifiles::TIniFile* Ini);
	bool __fastcall SaveToFile(System::Inifiles::TIniFile* Ini);
	void __fastcall SetColors(System::Uitypes::TColor Foreground, System::Uitypes::TColor Background);
	__property System::UnicodeString FriendlyName = {read=fFriendlyName};
	__property int IntegerStyle = {read=GetStyleFromInt, write=SetStyleFromInt, nodefault};
	__property System::UnicodeString Name = {read=fName};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
	
__published:
	__property System::Uitypes::TColor Background = {read=fBackground, write=SetBackground, stored=GetBackgroundColorStored, nodefault};
	__property System::Uitypes::TColor Foreground = {read=fForeground, write=SetForeground, stored=GetForegroundColorStored, nodefault};
	__property System::Uitypes::TFontStyles Style = {read=fStyle, write=SetStyle, stored=GetFontStyleStored, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSynHighlighterAttributes(void) { }
	
};


enum DECLSPEC_DENUM TSynHighlighterCapability : unsigned char { hcUserSettings, hcRegistry };

typedef System::Set<TSynHighlighterCapability, TSynHighlighterCapability::hcUserSettings, TSynHighlighterCapability::hcRegistry> TSynHighlighterCapabilities;

class PASCALIMPLEMENTATION TSynCustomHighlighter : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TStringList* fAttributes;
	Syneditmiscclasses::TSynNotifyEventChain* fAttrChangeHooks;
	int fUpdateCount;
	bool fEnabled;
	System::Sysutils::TSysCharSet FAdditionalWordBreakChars;
	System::Sysutils::TSysCharSet FAdditionalIdentChars;
	System::UnicodeString FExportName;
	Synedithighlighteroptions::TSynEditHighlighterOptions* FOptions;
	System::UnicodeString __fastcall GetExportName(void);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetAdditionalIdentChars(const System::Sysutils::TSysCharSet &Value);
	void __fastcall SetAdditionalWordBreakChars(const System::Sysutils::TSysCharSet &Value);
	
protected:
	System::WideChar *fCasedLine;
	System::UnicodeString fCasedLineStr;
	bool fCaseSensitive;
	System::UnicodeString fDefaultFilter;
	System::WideChar *fExpandedLine;
	int fExpandedLineLen;
	System::UnicodeString fExpandedLineStr;
	int fExpandedTokenPos;
	System::WideChar *fLine;
	int fLineLen;
	System::UnicodeString fLineStr;
	int fLineNumber;
	int fStringLen;
	System::WideChar *fToIdent;
	int fTokenPos;
	bool fUpdateChange;
	int Run;
	int ExpandedRun;
	int fOldRun;
	virtual void __fastcall Loaded(void);
	void __fastcall AddAttribute(TSynHighlighterAttributes* Attri);
	void __fastcall DefHighlightChange(System::TObject* Sender);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall FreeHighlighterAttributes(void);
	virtual int __fastcall GetAttribCount(void);
	virtual TSynHighlighterAttributes* __fastcall GetAttribute(int Index);
	virtual TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index) = 0 ;
	virtual System::UnicodeString __fastcall GetDefaultFilter(void);
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual void __fastcall DoSetLine(const System::UnicodeString Value, int LineNumber);
	virtual bool __fastcall IsCurrentToken(const System::UnicodeString Token);
	virtual bool __fastcall IsFilterStored(void);
	virtual bool __fastcall IsLineEnd(int Run);
	void __fastcall SetAttributesOnChange(System::Classes::TNotifyEvent AEvent);
	virtual void __fastcall SetDefaultFilter(System::UnicodeString Value);
	virtual void __fastcall SetSampleSource(System::UnicodeString Value);
	TSynHighlighterCapabilities __fastcall GetCapabilitiesProp(void);
	System::UnicodeString __fastcall GetFriendlyLanguageNameProp(void);
	System::UnicodeString __fastcall GetLanguageNameProp(void);
	
public:
	__classmethod virtual TSynHighlighterCapabilities __fastcall GetCapabilities();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__fastcall virtual TSynCustomHighlighter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynCustomHighlighter(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	virtual bool __fastcall GetEol(void) = 0 ;
	virtual System::UnicodeString __fastcall GetExpandedToken(void);
	virtual int __fastcall GetExpandedTokenPos(void);
	virtual System::UnicodeString __fastcall GetKeyWords(int TokenKind);
	virtual void * __fastcall GetRange(void);
	virtual System::UnicodeString __fastcall GetToken(void);
	virtual TSynHighlighterAttributes* __fastcall GetTokenAttribute(void) = 0 ;
	virtual int __fastcall GetTokenKind(void) = 0 ;
	virtual int __fastcall GetTokenPos(void);
	virtual bool __fastcall IsKeyword(const System::UnicodeString AKeyword);
	virtual void __fastcall Next(void);
	void __fastcall NextToEol(void);
	int __fastcall PosToExpandedPos(int Pos);
	virtual void __fastcall SetLineExpandedAtWideGlyphs(const System::UnicodeString Line, const System::UnicodeString ExpandedLine, int LineNumber);
	virtual void __fastcall SetLine(const System::UnicodeString Value, int LineNumber);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	virtual bool __fastcall UseUserSettings(int settingIndex);
	virtual void __fastcall EnumUserSettings(System::Classes::TStrings* Settings);
	virtual bool __fastcall LoadFromRegistry(HKEY RootKey, System::UnicodeString Key);
	virtual bool __fastcall SaveToRegistry(HKEY RootKey, System::UnicodeString Key);
	bool __fastcall LoadFromFile(System::UnicodeString AFileName);
	bool __fastcall SaveToFile(System::UnicodeString AFileName);
	void __fastcall HookAttrChangeEvent(System::Classes::TNotifyEvent ANotifyEvent);
	void __fastcall UnhookAttrChangeEvent(System::Classes::TNotifyEvent ANotifyEvent);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual bool __fastcall IsWhiteChar(System::WideChar AChar);
	virtual bool __fastcall IsWordBreakChar(System::WideChar AChar);
	__property System::UnicodeString FriendlyLanguageName = {read=GetFriendlyLanguageNameProp};
	__property System::UnicodeString LanguageName = {read=GetLanguageNameProp};
	__property System::Sysutils::TSysCharSet AdditionalIdentChars = {read=FAdditionalIdentChars, write=SetAdditionalIdentChars};
	__property System::Sysutils::TSysCharSet AdditionalWordBreakChars = {read=FAdditionalWordBreakChars, write=SetAdditionalWordBreakChars};
	__property int AttrCount = {read=GetAttribCount, nodefault};
	__property TSynHighlighterAttributes* Attribute[int Index] = {read=GetAttribute};
	__property TSynHighlighterCapabilities Capabilities = {read=GetCapabilitiesProp, nodefault};
	__property System::UnicodeString SampleSource = {read=GetSampleSource, write=SetSampleSource};
	__property TSynHighlighterAttributes* CommentAttribute = {read=GetDefaultAttribute, index=0};
	__property TSynHighlighterAttributes* IdentifierAttribute = {read=GetDefaultAttribute, index=1};
	__property TSynHighlighterAttributes* KeywordAttribute = {read=GetDefaultAttribute, index=2};
	__property TSynHighlighterAttributes* StringAttribute = {read=GetDefaultAttribute, index=3};
	__property TSynHighlighterAttributes* SymbolAttribute = {read=GetDefaultAttribute, index=5};
	__property TSynHighlighterAttributes* WhitespaceAttribute = {read=GetDefaultAttribute, index=4};
	__property System::UnicodeString ExportName = {read=GetExportName};
	
__published:
	__property System::UnicodeString DefaultFilter = {read=GetDefaultFilter, write=SetDefaultFilter, stored=IsFilterStored};
	__property bool Enabled = {read=fEnabled, write=SetEnabled, default=1};
	__property Synedithighlighteroptions::TSynEditHighlighterOptions* Options = {read=FOptions, write=FOptions};
};


typedef System::TMetaClass* TSynCustomHighlighterClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynHighlighterList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	TSynCustomHighlighterClass operator[](int Index) { return Items[Index]; }
	
private:
	System::Classes::TList* hlList;
	TSynCustomHighlighterClass __fastcall GetItem(int Index);
	
public:
	__fastcall TSynHighlighterList(void);
	__fastcall virtual ~TSynHighlighterList(void);
	HIDESBASE int __fastcall Count(void);
	int __fastcall FindByFriendlyName(System::UnicodeString FriendlyName);
	int __fastcall FindByName(System::UnicodeString Name);
	int __fastcall FindByClass(System::Classes::TComponent* Comp);
	__property TSynCustomHighlighterClass Items[int Index] = {read=GetItem/*, default*/};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 SYN_ATTR_COMMENT = System::Int8(0x0);
static const System::Int8 SYN_ATTR_IDENTIFIER = System::Int8(0x1);
static const System::Int8 SYN_ATTR_KEYWORD = System::Int8(0x2);
static const System::Int8 SYN_ATTR_STRING = System::Int8(0x3);
static const System::Int8 SYN_ATTR_WHITESPACE = System::Int8(0x4);
static const System::Int8 SYN_ATTR_SYMBOL = System::Int8(0x5);
extern DELPHI_PACKAGE TSynHighlighterList* __fastcall GetPlaceableHighlighters(void);
extern DELPHI_PACKAGE void __fastcall RegisterPlaceableHighlighter(TSynCustomHighlighterClass highlighter);
}	/* namespace Synedithighlighter */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITHIGHLIGHTER)
using namespace Synedithighlighter;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynedithighlighterHPP
