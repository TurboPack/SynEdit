// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterLLVM.pas' rev: 28.00 (Windows)

#ifndef SynhighlighterllvmHPP
#define SynhighlighterllvmHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <SynEditTypes.hpp>	// Pascal unit
#include <SynEditHighlighter.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterllvm
{
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkBoolean, tkComment, tkConstant, tkFloat, tkHex, tkIdentifier, tkInstruction, tkKey, tkLabel, tkNumber, tkNull, tkSpace, tkString, tkSymbol, tkType, tkUnnamedIdentifier, tkUnknown };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown, rsSingleComment, rsString };

typedef void __fastcall (__closure *TProcTableProc)(void);

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class DELPHICLASS TSynLLVMIRSyn;
class PASCALIMPLEMENTATION TSynLLVMIRSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 1553> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fBooleanAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fConstantAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fInstructionAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fLabelAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fTypesAttri;
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall FuncBoolean(int Index);
	TtkTokenKind __fastcall FuncConstant(int Index);
	TtkTokenKind __fastcall FuncInstruction(int Index);
	TtkTokenKind __fastcall FuncKey(int Index);
	TtkTokenKind __fastcall FuncType(int Index);
	void __fastcall IdentProc(void);
	void __fastcall UnknownProc(void);
	TtkTokenKind __fastcall AltFunc(int Index);
	void __fastcall InitIdent(void);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall NullProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	void __fastcall IntegerTypeProc(void);
	void __fastcall SingleCommentOpenProc(void);
	void __fastcall SingleCommentProc(void);
	void __fastcall StringOpenProc(void);
	void __fastcall StringProc(void);
	void __fastcall AtTypeProc(void);
	void __fastcall PercentTypeProc(void);
	void __fastcall NumberProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__fastcall virtual TSynLLVMIRSyn(System::Classes::TComponent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	virtual void * __fastcall GetRange(void);
	virtual void __fastcall ResetRange(void);
	virtual void __fastcall SetRange(void * Value);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol(void);
	virtual System::UnicodeString __fastcall GetKeyWords(int TokenKind);
	TtkTokenKind __fastcall GetTokenID(void);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute(void);
	virtual int __fastcall GetTokenKind(void);
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next(void);
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* BooleanAttribute = {read=fBooleanAttri, write=fBooleanAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttribute = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ConstantAttribute = {read=fConstantAttri, write=fConstantAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttribute = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* InstructionAttribute = {read=fInstructionAttri, write=fInstructionAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeywordAttribute = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* LabelAttribute = {read=fLabelAttri, write=fLabelAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttribute = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttribute = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttribute = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* TypesAttribute = {read=fTypesAttri, write=fTypesAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynLLVMIRSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterllvm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERLLVM)
using namespace Synhighlighterllvm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterllvmHPP
