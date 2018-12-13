// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditHighlighterOptions.pas' rev: 32.00 (Windows)

#ifndef SynedithighlighteroptionsHPP
#define SynedithighlighteroptionsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synedithighlighteroptions
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynEditHighlighterOptions;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditHighlighterOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FAutoDetectEnabled;
	unsigned FAutoDetectLineLimit;
	System::WideString FAutoDetectMatchExpression;
	System::WideString FDefaultExtension;
	System::WideString FLineCommentarEnd;
	System::WideString FLineCommentarStart;
	System::WideString FTitle;
	bool FVisible;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	
__published:
	__property bool AutoDetectEnabled = {read=FAutoDetectEnabled, write=FAutoDetectEnabled, nodefault};
	__property unsigned AutoDetectLineLimit = {read=FAutoDetectLineLimit, write=FAutoDetectLineLimit, nodefault};
	__property System::WideString AutoDetectMatchExpression = {read=FAutoDetectMatchExpression, write=FAutoDetectMatchExpression};
	__property System::WideString DefaultExtension = {read=FDefaultExtension, write=FDefaultExtension};
	__property System::WideString LineCommentarEnd = {read=FLineCommentarEnd, write=FLineCommentarEnd};
	__property System::WideString LineCommentarStart = {read=FLineCommentarStart, write=FLineCommentarStart};
	__property System::WideString Title = {read=FTitle, write=FTitle};
	__property bool Visible = {read=FVisible, write=FVisible, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSynEditHighlighterOptions(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TSynEditHighlighterOptions(void) : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synedithighlighteroptions */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITHIGHLIGHTEROPTIONS)
using namespace Synedithighlighteroptions;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynedithighlighteroptionsHPP
