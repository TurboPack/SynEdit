// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynRegExpr.pas' rev: 28.00 (Windows)

#ifndef SynregexprHPP
#define SynregexprHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <SynUnicode.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synregexpr
{
//-- type declarations -------------------------------------------------------
typedef System::WideChar * PRegExprChar;

typedef System::UnicodeString RegExprString;

typedef System::WideChar REChar;

typedef System::WideChar TREOp;

typedef System::WideChar *PREOp;

typedef int TRENextOff;

typedef int *PRENextOff;

typedef int TREBracesArg;

typedef int *PREBracesArg;

typedef System::WideChar __fastcall (__closure *TRegExprInvertCaseFunction)(const System::WideChar Ch);

class DELPHICLASS TRegExpr;
typedef System::UnicodeString __fastcall (__closure *TRegExprReplaceFunction)(TRegExpr* ARegExpr);

class PASCALIMPLEMENTATION TRegExpr : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::StaticArray<System::WideChar *, 15> startp;
	System::StaticArray<System::WideChar *, 15> endp;
	System::StaticArray<int, 10> LoopStack;
	int LoopStackIdx;
	System::WideChar regstart;
	System::WideChar reganch;
	System::WideChar *regmust;
	int regmlen;
	System::WideChar *reginput;
	System::WideChar *fInputStart;
	System::WideChar *fInputEnd;
	System::WideChar *regparse;
	int regnpar;
	System::WideChar regdummy;
	System::WideChar *regcode;
	int regsize;
	System::WideChar *regexpbeg;
	bool fExprIsCompiled;
	System::WideChar *programm;
	System::WideChar *fExpression;
	System::WideChar *fInputString;
	int fLastError;
	int fModifiers;
	int fCompModifiers;
	int fProgModifiers;
	System::UnicodeString fSpaceChars;
	System::UnicodeString fWordChars;
	TRegExprInvertCaseFunction fInvertCase;
	System::UnicodeString fLineSeparators;
	bool fLinePairedSeparatorAssigned;
	System::WideChar fLinePairedSeparatorHead;
	System::WideChar fLinePairedSeparatorTail;
	void __fastcall InvalidateProgramm(void);
	bool __fastcall IsProgrammOk(void);
	System::UnicodeString __fastcall GetExpression(void);
	void __fastcall SetExpression(const System::UnicodeString s);
	System::UnicodeString __fastcall GetModifierStr(void);
	__classmethod bool __fastcall ParseModifiersStr(const System::UnicodeString AModifiers, int &AModifiersInt);
	void __fastcall SetModifierStr(const System::UnicodeString AModifiers);
	bool __fastcall GetModifier(int AIndex);
	void __fastcall SetModifier(int AIndex, bool ASet);
	virtual void __fastcall Error(int AErrorID);
	bool __fastcall CompileRegExpr(System::WideChar * exp);
	void __fastcall Tail(System::WideChar * p, System::WideChar * val);
	void __fastcall OpTail(System::WideChar * p, System::WideChar * val);
	System::WideChar * __fastcall EmitNode(System::WideChar op);
	void __fastcall EmitC(System::WideChar b);
	void __fastcall InsertOperator(System::WideChar op, System::WideChar * opnd, int sz);
	System::WideChar * __fastcall ParseReg(int paren, int &flagp);
	System::WideChar * __fastcall ParseBranch(int &flagp);
	System::WideChar * __fastcall ParsePiece(int &flagp);
	System::WideChar * __fastcall ParseAtom(int &flagp);
	int __fastcall GetCompilerErrorPos(void);
	int __fastcall regrepeat(System::WideChar * p, int AMax);
	System::WideChar * __fastcall regnext(System::WideChar * p);
	bool __fastcall MatchPrim(System::WideChar * prog);
	bool __fastcall ExecPrim(int AOffset);
	System::UnicodeString __fastcall DumpOp(System::WideChar op);
	int __fastcall GetSubExprMatchCount(void);
	int __fastcall GetMatchPos(int Idx);
	int __fastcall GetMatchLen(int Idx);
	System::UnicodeString __fastcall GetMatch(int Idx);
	System::UnicodeString __fastcall GetInputString(void);
	void __fastcall SetInputString(const System::UnicodeString AInputString);
	System::WideChar * __fastcall StrScanCI(System::WideChar * s, System::WideChar ch);
	void __fastcall SetLineSeparators(const System::UnicodeString AStr);
	void __fastcall SetLinePairedSeparator(const System::UnicodeString AStr);
	System::UnicodeString __fastcall GetLinePairedSeparator(void);
	
public:
	__fastcall TRegExpr(void);
	__fastcall virtual ~TRegExpr(void);
	__classmethod int __fastcall VersionMajor();
	__classmethod int __fastcall VersionMinor();
	__property System::UnicodeString Expression = {read=GetExpression, write=SetExpression};
	__property System::UnicodeString ModifierStr = {read=GetModifierStr, write=SetModifierStr};
	__property bool ModifierI = {read=GetModifier, write=SetModifier, index=1, nodefault};
	__property bool ModifierR = {read=GetModifier, write=SetModifier, index=2, nodefault};
	__property bool ModifierS = {read=GetModifier, write=SetModifier, index=3, nodefault};
	__property bool ModifierG = {read=GetModifier, write=SetModifier, index=4, nodefault};
	__property bool ModifierM = {read=GetModifier, write=SetModifier, index=5, nodefault};
	__property bool ModifierX = {read=GetModifier, write=SetModifier, index=6, nodefault};
	bool __fastcall Exec(const System::UnicodeString AInputString)/* overload */;
	bool __fastcall Exec(void)/* overload */;
	bool __fastcall Exec(int AOffset)/* overload */;
	bool __fastcall ExecNext(void);
	bool __fastcall ExecPos(int AOffset = 0x1);
	__property System::UnicodeString InputString = {read=GetInputString, write=SetInputString};
	System::UnicodeString __fastcall Substitute(const System::UnicodeString ATemplate);
	void __fastcall Split(System::UnicodeString AInputStr, System::Classes::TStrings* APieces);
	System::UnicodeString __fastcall Replace(System::UnicodeString AInputStr, const System::UnicodeString AReplaceStr, bool AUseSubstitution = false)/* overload */;
	System::UnicodeString __fastcall Replace(System::UnicodeString AInputStr, TRegExprReplaceFunction AReplaceFunc)/* overload */;
	System::UnicodeString __fastcall ReplaceEx(System::UnicodeString AInputStr, TRegExprReplaceFunction AReplaceFunc);
	__property int SubExprMatchCount = {read=GetSubExprMatchCount, nodefault};
	__property int MatchPos[int Idx] = {read=GetMatchPos};
	__property int MatchLen[int Idx] = {read=GetMatchLen};
	__property System::UnicodeString Match[int Idx] = {read=GetMatch};
	int __fastcall LastError(void);
	virtual System::UnicodeString __fastcall ErrorMsg(int AErrorID);
	__property int CompilerErrorPos = {read=GetCompilerErrorPos, nodefault};
	__property System::UnicodeString SpaceChars = {read=fSpaceChars, write=fSpaceChars};
	__property System::UnicodeString WordChars = {read=fWordChars, write=fWordChars};
	__property System::UnicodeString LineSeparators = {read=fLineSeparators, write=SetLineSeparators};
	__property System::UnicodeString LinePairedSeparator = {read=GetLinePairedSeparator, write=SetLinePairedSeparator};
	__classmethod System::WideChar __fastcall InvertCaseFunction(const System::WideChar Ch);
	__property TRegExprInvertCaseFunction InvertCase = {read=fInvertCase, write=fInvertCase};
	void __fastcall Compile(void);
	System::UnicodeString __fastcall Dump(void);
};


class DELPHICLASS ERegExpr;
#pragma pack(push,4)
class PASCALIMPLEMENTATION ERegExpr : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	int ErrorCode;
	int CompilerErrorPos;
public:
	/* Exception.Create */ inline __fastcall ERegExpr(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ERegExpr(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ERegExpr(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ERegExpr(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ERegExpr(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ERegExpr(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ERegExpr(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ERegExpr(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ERegExpr(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ERegExpr(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ERegExpr(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ERegExpr(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ERegExpr(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 REOpSz = System::Int8(0x1);
static const System::Int8 RENextOffSz = System::Int8(0x2);
static const System::Int8 REBracesArgSz = System::Int8(0x2);
static const System::WideChar EscChar = (System::WideChar)(0x5c);
extern DELPHI_PACKAGE bool RegExprModifierI;
extern DELPHI_PACKAGE bool RegExprModifierR;
extern DELPHI_PACKAGE bool RegExprModifierS;
extern DELPHI_PACKAGE bool RegExprModifierG;
extern DELPHI_PACKAGE bool RegExprModifierM;
extern DELPHI_PACKAGE bool RegExprModifierX;
extern DELPHI_PACKAGE System::UnicodeString RegExprSpaceChars;
extern DELPHI_PACKAGE System::UnicodeString RegExprWordChars;
extern DELPHI_PACKAGE System::UnicodeString RegExprLineSeparators;
extern DELPHI_PACKAGE System::UnicodeString RegExprLinePairedSeparator;
static const System::Int8 NSUBEXP = System::Int8(0xf);
static const System::Byte NSUBEXPMAX = System::Byte(0xff);
static const int MaxBracesArg = int(0x7ffffffe);
static const System::Int8 LoopStackMax = System::Int8(0xa);
static const System::Int8 TinySetLen = System::Int8(0x3);
extern DELPHI_PACKAGE TRegExprInvertCaseFunction RegExprInvertCaseFunction;
extern DELPHI_PACKAGE bool __fastcall ExecRegExpr(const System::UnicodeString ARegExpr, const System::UnicodeString AInputStr);
extern DELPHI_PACKAGE void __fastcall SplitRegExpr(const System::UnicodeString ARegExpr, const System::UnicodeString AInputStr, System::Classes::TStrings* APieces);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReplaceRegExpr(const System::UnicodeString ARegExpr, const System::UnicodeString AInputStr, const System::UnicodeString AReplaceStr, bool AUseSubstitution = false);
extern DELPHI_PACKAGE System::UnicodeString __fastcall QuoteRegExprMetaChars(const System::UnicodeString AStr);
extern DELPHI_PACKAGE int __fastcall RegExprSubExpressions(const System::UnicodeString ARegExpr, System::Classes::TStrings* ASubExprs, bool AExtendedSyntax = false);
}	/* namespace Synregexpr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNREGEXPR)
using namespace Synregexpr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynregexprHPP
