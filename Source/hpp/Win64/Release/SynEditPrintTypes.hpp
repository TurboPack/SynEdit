// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPrintTypes.pas' rev: 31.00 (Windows)

#ifndef SyneditprinttypesHPP
#define SyneditprinttypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditprinttypes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TWrapPos;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TFrameType : unsigned char { ftLine, ftBox, ftShaded };

typedef System::Set<TFrameType, TFrameType::ftLine, TFrameType::ftShaded> TFrameTypes;

enum DECLSPEC_DENUM TUnitSystem : unsigned char { usMM, usCM, usInch, muThousandthsOfInches };

enum DECLSPEC_DENUM TSynPrintStatus : unsigned char { psBegin, psNewPage, psEnd };

typedef void __fastcall (__closure *TPrintStatusEvent)(System::TObject* Sender, TSynPrintStatus Status, int PageNumber, bool &Abort);

typedef void __fastcall (__closure *TPrintLineEvent)(System::TObject* Sender, int LineNumber, int PageNumber);

class PASCALIMPLEMENTATION TWrapPos : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int Index;
public:
	/* TObject.Create */ inline __fastcall TWrapPos(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TWrapPos(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 DefLeft = System::Int8(0x19);
static const System::Int8 DefRight = System::Int8(0xf);
static const System::Int8 DefTop = System::Int8(0x19);
static const System::Int8 DefBottom = System::Int8(0x19);
static const System::Int8 DefHeader = System::Int8(0xf);
static const System::Int8 DefFooter = System::Int8(0xf);
static const System::Int8 DefLeftHFTextIndent = System::Int8(0x2);
static const System::Int8 DefRightHFTextIndent = System::Int8(0x2);
#define DefHFInternalMargin  (5.000000E-01)
static const System::Int8 DefGutter = System::Int8(0x0);
extern DELPHI_PACKAGE bool __fastcall WrapTextEx(const System::UnicodeString Line, const System::Sysutils::TSysCharSet &BreakChars, int MaxCol, System::Classes::TList* AList);
extern DELPHI_PACKAGE System::UnicodeString __fastcall IntToRoman(int Value);
}	/* namespace Syneditprinttypes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPRINTTYPES)
using namespace Syneditprinttypes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditprinttypesHPP
