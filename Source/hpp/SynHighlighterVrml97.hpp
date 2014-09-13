// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterVrml97.pas' rev: 28.00 (Windows)

#ifndef Synhighlightervrml97HPP
#define Synhighlightervrml97HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.Win.Registry.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <SynEditHighlighter.hpp>	// Pascal unit
#include <SynHighlighterHashEntries.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synhighlightervrml97
{
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown, tkNonReservedKey, tkEvent, tkVrmlAppearance, tkVrmlAttribute, tkVrmlDefinition, tkVrmlEvent, tkVrmlGrouping, tkVrmlInterpolator, tkVrmlLight, tkVrmlNode, tkVrmlParameter, tkVrmlproto, tkVrmlSensor, tkVrmlShape, tkVrmlShape_Hint, tkVrmlTime_dependent, tkVrmlViewpoint, tkVrmlWorldInfo, tkX3DDocType, tkX3DHeader };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsNormalText, rsComment, rsX3DHeader, rsX3DDocType };

class DELPHICLASS TSynVrml97Syn;
class PASCALIMPLEMENTATION TSynVrml97Syn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	bool isDoctype;
	TtkTokenKind FTokenID;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNonReservedKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fEventAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlAppearanceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlAttributeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlDefinitionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlEventAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlGroupingAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlInterpolatorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlLightAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlNodeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlParameterAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlprotoAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlSensorAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlShapeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlShape_HintAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlTime_dependentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlViewpointAttri;
	Synedithighlighter::TSynHighlighterAttributes* fVrmlWorldInfoAttri;
	Synedithighlighter::TSynHighlighterAttributes* fX3DDocTypeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fX3DHeaderAttri;
	Synhighlighterhashentries::TSynHashEntryList* fKeywords;
	void __fastcall DoAddKeyword(System::UnicodeString AKeyword, int AKind);
	int __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall AndSymbolProc(void);
	void __fastcall CommentProc(void);
	void __fastcall DiesisCommentProc(void);
	void __fastcall X3DDocTypeOpenProc(void);
	void __fastcall X3DDocTypeProc(void);
	void __fastcall X3DHeaderOpenProc(void);
	void __fastcall X3DHeaderProc(void);
	void __fastcall InCommentProc(void);
	void __fastcall CRProc(void);
	void __fastcall IdentProc(void);
	void __fastcall LFProc(void);
	void __fastcall MinusProc(void);
	void __fastcall ModSymbolProc(void);
	void __fastcall NullProc(void);
	void __fastcall NumberProc(void);
	void __fastcall OrSymbolProc(void);
	void __fastcall PlusProc(void);
	void __fastcall PointProc(void);
	void __fastcall SlashProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall StarProc(void);
	void __fastcall StringProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall UnknownProc(void);
	bool __fastcall NextTokenIs(System::UnicodeString T);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__fastcall virtual TSynVrml97Syn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TSynVrml97Syn(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual void * __fastcall GetRange(void);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual void __fastcall Next(void);
	virtual void __fastcall SetRange(void * Value);
	virtual void __fastcall ResetRange(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* NonReservedKeyAttri = {read=fNonReservedKeyAttri, write=fNonReservedKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* EcmaScriptKeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* EcmaScriptEventAttri = {read=fEventAttri, write=fEventAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlAppearanceAttri = {read=fVrmlAppearanceAttri, write=fVrmlAppearanceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlAttributeAttri = {read=fVrmlAttributeAttri, write=fVrmlAttributeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlDefinitionAttri = {read=fVrmlDefinitionAttri, write=fVrmlDefinitionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlEventAttri = {read=fVrmlEventAttri, write=fVrmlEventAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlGroupingAttri = {read=fVrmlGroupingAttri, write=fVrmlGroupingAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlInterpolatorAttri = {read=fVrmlInterpolatorAttri, write=fVrmlInterpolatorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlLightAttri = {read=fVrmlLightAttri, write=fVrmlLightAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlNodeAttri = {read=fVrmlNodeAttri, write=fVrmlNodeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlParameterAttri = {read=fVrmlParameterAttri, write=fVrmlParameterAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlprotoAttri = {read=fVrmlprotoAttri, write=fVrmlprotoAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlSensorAttri = {read=fVrmlSensorAttri, write=fVrmlSensorAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlShapeAttri = {read=fVrmlShapeAttri, write=fVrmlShapeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlShape_HintAttri = {read=fVrmlShape_HintAttri, write=fVrmlShape_HintAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlTime_dependentAttri = {read=fVrmlTime_dependentAttri, write=fVrmlTime_dependentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlViewpointAttri = {read=fVrmlViewpointAttri, write=fVrmlViewpointAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* VrmlWorldInfoAttri = {read=fVrmlWorldInfoAttri, write=fVrmlWorldInfoAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* X3DDocTypeAttri = {read=fX3DDocTypeAttri, write=fX3DDocTypeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* X3DHeaderAttri = {read=fX3DHeaderAttri, write=fX3DHeaderAttri};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlightervrml97 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERVRML97)
using namespace Synhighlightervrml97;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Synhighlightervrml97HPP
