// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditTypes.pas' rev: 28.00 (Windows)

#ifndef SynedittypesHPP
#define SynedittypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Synedittypes
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS ESynError;
#pragma pack(push,4)
class PASCALIMPLEMENTATION ESynError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESynError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESynError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESynError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESynError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESynError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESynError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESynError(void) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TSynSearchOption : unsigned char { ssoMatchCase, ssoWholeWord, ssoBackwards, ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt };

typedef System::Set<TSynSearchOption, TSynSearchOption::ssoMatchCase, TSynSearchOption::ssoPrompt> TSynSearchOptions;

typedef bool __fastcall (__closure *TCategoryMethod)(System::WideChar AChar);

typedef void __fastcall (__closure *TKeyPressWEvent)(System::TObject* Sender, System::WideChar &Key);

enum DECLSPEC_DENUM TSynSelectionMode : unsigned char { smNormal, smLine, smColumn };

typedef TSynSelectionMode *PSynSelectionMode;

enum DECLSPEC_DENUM TBorlandSelectionMode : unsigned char { bsmInclusive, bsmLine, bsmColumn, bsmNormal };

typedef TBorlandSelectionMode *PBorlandSelectionMode;

struct DECLSPEC_DRECORD TBufferCoord
{
public:
	int Char;
	int Line;
	static bool __fastcall _op_Equality(const TBufferCoord &a, const TBufferCoord &b);
};


struct DECLSPEC_DRECORD TBufferBlock
{
public:
	int BeginLine;
	int BeginChar;
	int EndLine;
	int EndChar;
	static bool __fastcall _op_Equality(const TBufferBlock &a, const TBufferBlock &b);
};


struct DECLSPEC_DRECORD TDisplayCoord
{
public:
	int Column;
	int Row;
	static bool __fastcall _op_Equality(const TDisplayCoord &a, const TDisplayCoord &b);
};


//-- var, const, procedure ---------------------------------------------------
static const System::WideChar SynTabGlyph = (System::WideChar)(0x2192);
static const System::WideChar SynSoftBreakGlyph = (System::WideChar)(0xac);
static const System::WideChar SynLineBreakGlyph = (System::WideChar)(0xb6);
static const System::WideChar SynSpaceGlyph = (System::WideChar)(0x2219);
extern DELPHI_PACKAGE TDisplayCoord __fastcall DisplayCoord(int AColumn, int ARow);
extern DELPHI_PACKAGE TBufferCoord __fastcall BufferCoord(int AChar, int ALine);
}	/* namespace Synedittypes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITTYPES)
using namespace Synedittypes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynedittypesHPP
