// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterWebIDL.pas' rev: 33.00 (Windows)

#ifndef SynhighlighterwebidlHPP
#define SynhighlighterwebidlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterwebidl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynWebIDLSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkArguments, tkComment, tkExtendedAttributes, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkString, tkTypes, tkSymbol, tkUnknown };

enum DECLSPEC_DENUM TstkSymbolTokenKind : unsigned char { stkBraceOpen, stkBraceClose, stkSquareOpen, stkSquareClose, stkQuestionMark, stkColon, stkGreater, stkLess };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnknown, rsSingleComment, rsCStyleComment, rsString, rsExtendedAttributes };

typedef void __fastcall (__closure *TProcTableProc)();

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynWebIDLSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	TstkSymbolTokenKind fSymbolTokenID;
	System::StaticArray<TIdentFuncTableFunc, 59> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fArgumentsAttri;
	Synedithighlighter::TSynHighlighterAttributes* fExtendedAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fNumberAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	Synedithighlighter::TSynHighlighterAttributes* fTypesAttri;
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall FuncAny(int Index);
	TtkTokenKind __fastcall FuncAttribute(int Index);
	TtkTokenKind __fastcall FuncBoolean(int Index);
	TtkTokenKind __fastcall FuncByte(int Index);
	TtkTokenKind __fastcall FuncBytestring(int Index);
	TtkTokenKind __fastcall FuncCallback(int Index);
	TtkTokenKind __fastcall FuncConst(int Index);
	TtkTokenKind __fastcall FuncCreator(int Index);
	TtkTokenKind __fastcall FuncDate(int Index);
	TtkTokenKind __fastcall FuncDeleter(int Index);
	TtkTokenKind __fastcall FuncDictionary(int Index);
	TtkTokenKind __fastcall FuncDomstring(int Index);
	TtkTokenKind __fastcall FuncDouble(int Index);
	TtkTokenKind __fastcall FuncEnum(int Index);
	TtkTokenKind __fastcall FuncException(int Index);
	TtkTokenKind __fastcall FuncFloat(int Index);
	TtkTokenKind __fastcall FuncGetter(int Index);
	TtkTokenKind __fastcall FuncImplements(int Index);
	TtkTokenKind __fastcall FuncInherit(int Index);
	TtkTokenKind __fastcall FuncInterface(int Index);
	TtkTokenKind __fastcall FuncLegacycaller(int Index);
	TtkTokenKind __fastcall FuncLong(int Index);
	TtkTokenKind __fastcall FuncObject(int Index);
	TtkTokenKind __fastcall FuncOctet(int Index);
	TtkTokenKind __fastcall FuncOptional(int Index);
	TtkTokenKind __fastcall FuncPartial(int Index);
	TtkTokenKind __fastcall FuncReadonly(int Index);
	TtkTokenKind __fastcall FuncRegexp(int Index);
	TtkTokenKind __fastcall FuncSequence(int Index);
	TtkTokenKind __fastcall FuncSetter(int Index);
	TtkTokenKind __fastcall FuncShort(int Index);
	TtkTokenKind __fastcall FuncStatic(int Index);
	TtkTokenKind __fastcall FuncStringifier(int Index);
	TtkTokenKind __fastcall FuncTypedef(int Index);
	TtkTokenKind __fastcall FuncUnresticted(int Index);
	TtkTokenKind __fastcall FuncUnrestricted(int Index);
	TtkTokenKind __fastcall FuncUnsigned(int Index);
	TtkTokenKind __fastcall FuncVoid(int Index);
	void __fastcall IdentProc();
	void __fastcall UnknownProc();
	TtkTokenKind __fastcall AltFunc(int Index);
	void __fastcall InitIdent();
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall BraceCloseProc();
	void __fastcall BraceOpenProc();
	void __fastcall ColonProc();
	void __fastcall CRProc();
	void __fastcall CStyleCommentProc();
	void __fastcall GreaterProc();
	void __fastcall LessProc();
	void __fastcall LFProc();
	void __fastcall NullProc();
	void __fastcall NumberProc();
	void __fastcall QuestionMarkProc();
	void __fastcall SlashProc();
	void __fastcall SpaceProc();
	void __fastcall SquareCloseProc();
	void __fastcall SquareOpenProc();
	void __fastcall StringOpenProc();
	void __fastcall StringProc();
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource();
	virtual bool __fastcall IsFilterStored();
	
public:
	__fastcall virtual TSynWebIDLSyn(System::Classes::TComponent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	virtual void * __fastcall GetRange();
	virtual void __fastcall ResetRange();
	virtual void __fastcall SetRange(void * Value);
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetDefaultAttribute(int Index);
	virtual bool __fastcall GetEol();
	virtual System::UnicodeString __fastcall GetKeyWords(int TokenKind);
	TtkTokenKind __fastcall GetTokenID();
	virtual Synedithighlighter::TSynHighlighterAttributes* __fastcall GetTokenAttribute();
	virtual int __fastcall GetTokenKind();
	virtual bool __fastcall IsIdentChar(System::WideChar AChar);
	virtual void __fastcall Next();
	
__published:
	__property Synedithighlighter::TSynHighlighterAttributes* ArgumentsAttri = {read=fArgumentsAttri, write=fArgumentsAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ExtendedAttri = {read=fExtendedAttri, write=fExtendedAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* NumberAttri = {read=fNumberAttri, write=fNumberAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* TypesAttri = {read=fTypesAttri, write=fTypesAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynWebIDLSyn() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterwebidl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERWEBIDL)
using namespace Synhighlighterwebidl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterwebidlHPP
