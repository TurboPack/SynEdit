// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditKeyCmds.pas' rev: 29.00 (Windows)

#ifndef SyneditkeycmdsHPP
#define SyneditkeycmdsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Menus.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditkeycmds
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS ESynKeyError;
class DELPHICLASS TSynEditKeyStroke;
class DELPHICLASS TSynEditKeyStrokes;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION ESynKeyError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESynKeyError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESynKeyError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESynKeyError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESynKeyError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynKeyError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynKeyError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESynKeyError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESynKeyError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynKeyError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynKeyError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynKeyError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynKeyError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESynKeyError(void) { }
	
};

#pragma pack(pop)

typedef System::Word TSynEditorCommand;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditKeyStroke : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::Word FKey;
	System::Classes::TShiftState FShift;
	System::Word FKey2;
	System::Classes::TShiftState FShift2;
	TSynEditorCommand FCommand;
	System::Classes::TShortCut __fastcall GetShortCut(void);
	System::Classes::TShortCut __fastcall GetShortCut2(void);
	void __fastcall SetCommand(const TSynEditorCommand Value);
	void __fastcall SetKey(const System::Word Value);
	void __fastcall SetKey2(const System::Word Value);
	void __fastcall SetShift(const System::Classes::TShiftState Value);
	void __fastcall SetShift2(const System::Classes::TShiftState Value);
	void __fastcall SetShortCut(const System::Classes::TShortCut Value);
	void __fastcall SetShortCut2(const System::Classes::TShortCut Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	__property System::Word Key = {read=FKey, write=SetKey, nodefault};
	__property System::Word Key2 = {read=FKey2, write=SetKey2, nodefault};
	__property System::Classes::TShiftState Shift = {read=FShift, write=SetShift, nodefault};
	__property System::Classes::TShiftState Shift2 = {read=FShift2, write=SetShift2, nodefault};
	
__published:
	__property TSynEditorCommand Command = {read=FCommand, write=SetCommand, nodefault};
	__property System::Classes::TShortCut ShortCut = {read=GetShortCut, write=SetShortCut, default=0};
	__property System::Classes::TShortCut ShortCut2 = {read=GetShortCut2, write=SetShortCut2, default=0};
public:
	/* TCollectionItem.Create */ inline __fastcall virtual TSynEditKeyStroke(System::Classes::TCollection* Collection) : System::Classes::TCollectionItem(Collection) { }
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TSynEditKeyStroke(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditKeyStrokes : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TSynEditKeyStroke* operator[](int Index) { return Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	HIDESBASE TSynEditKeyStroke* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TSynEditKeyStroke* Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall TSynEditKeyStrokes(System::Classes::TPersistent* AOwner);
	HIDESBASE TSynEditKeyStroke* __fastcall Add(void);
	void __fastcall AddKey(const TSynEditorCommand ACmd, const System::Word AKey, const System::Classes::TShiftState AShift);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	int __fastcall FindCommand(TSynEditorCommand Cmd);
	int __fastcall FindKeycode(System::Word Code, System::Classes::TShiftState SS);
	int __fastcall FindKeycode2(System::Word Code1, System::Classes::TShiftState SS1, System::Word Code2, System::Classes::TShiftState SS2);
	int __fastcall FindShortcut(System::Classes::TShortCut SC);
	int __fastcall FindShortcut2(System::Classes::TShortCut SC, System::Classes::TShortCut SC2);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall ResetDefaults(void);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	__property TSynEditKeyStroke* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TSynEditKeyStrokes(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 ecNone = System::Int8(0x0);
static const System::Int8 ecViewCommandFirst = System::Int8(0x0);
static const System::Word ecViewCommandLast = System::Word(0x1f4);
static const System::Word ecEditCommandFirst = System::Word(0x1f5);
static const System::Word ecEditCommandLast = System::Word(0x3e8);
static const System::Int8 ecLeft = System::Int8(0x1);
static const System::Int8 ecRight = System::Int8(0x2);
static const System::Int8 ecUp = System::Int8(0x3);
static const System::Int8 ecDown = System::Int8(0x4);
static const System::Int8 ecWordLeft = System::Int8(0x5);
static const System::Int8 ecWordRight = System::Int8(0x6);
static const System::Int8 ecLineStart = System::Int8(0x7);
static const System::Int8 ecLineEnd = System::Int8(0x8);
static const System::Int8 ecPageUp = System::Int8(0x9);
static const System::Int8 ecPageDown = System::Int8(0xa);
static const System::Int8 ecPageLeft = System::Int8(0xb);
static const System::Int8 ecPageRight = System::Int8(0xc);
static const System::Int8 ecPageTop = System::Int8(0xd);
static const System::Int8 ecPageBottom = System::Int8(0xe);
static const System::Int8 ecEditorTop = System::Int8(0xf);
static const System::Int8 ecEditorBottom = System::Int8(0x10);
static const System::Int8 ecGotoXY = System::Int8(0x11);
static const System::Int8 ecSelection = System::Int8(0x64);
static const System::Int8 ecSelLeft = System::Int8(0x65);
static const System::Int8 ecSelRight = System::Int8(0x66);
static const System::Int8 ecSelUp = System::Int8(0x67);
static const System::Int8 ecSelDown = System::Int8(0x68);
static const System::Int8 ecSelWordLeft = System::Int8(0x69);
static const System::Int8 ecSelWordRight = System::Int8(0x6a);
static const System::Int8 ecSelLineStart = System::Int8(0x6b);
static const System::Int8 ecSelLineEnd = System::Int8(0x6c);
static const System::Int8 ecSelPageUp = System::Int8(0x6d);
static const System::Int8 ecSelPageDown = System::Int8(0x6e);
static const System::Int8 ecSelPageLeft = System::Int8(0x6f);
static const System::Int8 ecSelPageRight = System::Int8(0x70);
static const System::Int8 ecSelPageTop = System::Int8(0x71);
static const System::Int8 ecSelPageBottom = System::Int8(0x72);
static const System::Int8 ecSelEditorTop = System::Int8(0x73);
static const System::Int8 ecSelEditorBottom = System::Int8(0x74);
static const System::Int8 ecSelGotoXY = System::Int8(0x75);
static const System::Byte ecSelWord = System::Byte(0xc6);
static const System::Byte ecSelectAll = System::Byte(0xc7);
static const System::Byte ecCopy = System::Byte(0xc9);
static const System::Byte ecScrollUp = System::Byte(0xd3);
static const System::Byte ecScrollDown = System::Byte(0xd4);
static const System::Byte ecScrollLeft = System::Byte(0xd5);
static const System::Byte ecScrollRight = System::Byte(0xd6);
static const System::Byte ecInsertMode = System::Byte(0xdd);
static const System::Byte ecOverwriteMode = System::Byte(0xde);
static const System::Byte ecToggleMode = System::Byte(0xdf);
static const System::Byte ecNormalSelect = System::Byte(0xe7);
static const System::Byte ecColumnSelect = System::Byte(0xe8);
static const System::Byte ecLineSelect = System::Byte(0xe9);
static const System::Byte ecMatchBracket = System::Byte(0xfa);
static const System::Byte ecCommentBlock = System::Byte(0xfb);
static const System::Word ecGotoMarker0 = System::Word(0x12d);
static const System::Word ecGotoMarker1 = System::Word(0x12e);
static const System::Word ecGotoMarker2 = System::Word(0x12f);
static const System::Word ecGotoMarker3 = System::Word(0x130);
static const System::Word ecGotoMarker4 = System::Word(0x131);
static const System::Word ecGotoMarker5 = System::Word(0x132);
static const System::Word ecGotoMarker6 = System::Word(0x133);
static const System::Word ecGotoMarker7 = System::Word(0x134);
static const System::Word ecGotoMarker8 = System::Word(0x135);
static const System::Word ecGotoMarker9 = System::Word(0x136);
static const System::Word ecSetMarker0 = System::Word(0x15f);
static const System::Word ecSetMarker1 = System::Word(0x160);
static const System::Word ecSetMarker2 = System::Word(0x161);
static const System::Word ecSetMarker3 = System::Word(0x162);
static const System::Word ecSetMarker4 = System::Word(0x163);
static const System::Word ecSetMarker5 = System::Word(0x164);
static const System::Word ecSetMarker6 = System::Word(0x165);
static const System::Word ecSetMarker7 = System::Word(0x166);
static const System::Word ecSetMarker8 = System::Word(0x167);
static const System::Word ecSetMarker9 = System::Word(0x168);
static const System::Word ecGotFocus = System::Word(0x1e0);
static const System::Word ecLostFocus = System::Word(0x1e1);
static const System::Word ecContextHelp = System::Word(0x1ea);
static const System::Word ecDeleteLastChar = System::Word(0x1f5);
static const System::Word ecDeleteChar = System::Word(0x1f6);
static const System::Word ecDeleteWord = System::Word(0x1f7);
static const System::Word ecDeleteLastWord = System::Word(0x1f8);
static const System::Word ecDeleteBOL = System::Word(0x1f9);
static const System::Word ecDeleteEOL = System::Word(0x1fa);
static const System::Word ecDeleteLine = System::Word(0x1fb);
static const System::Word ecClearAll = System::Word(0x1fc);
static const System::Word ecLineBreak = System::Word(0x1fd);
static const System::Word ecInsertLine = System::Word(0x1fe);
static const System::Word ecChar = System::Word(0x1ff);
static const System::Word ecImeStr = System::Word(0x226);
static const System::Word ecUndo = System::Word(0x259);
static const System::Word ecRedo = System::Word(0x25a);
static const System::Word ecCut = System::Word(0x25b);
static const System::Word ecPaste = System::Word(0x25c);
static const System::Word ecBlockIndent = System::Word(0x262);
static const System::Word ecBlockUnindent = System::Word(0x263);
static const System::Word ecTab = System::Word(0x264);
static const System::Word ecShiftTab = System::Word(0x265);
static const System::Word ecAutoCompletion = System::Word(0x28a);
static const System::Word ecUpperCase = System::Word(0x26c);
static const System::Word ecLowerCase = System::Word(0x26d);
static const System::Word ecToggleCase = System::Word(0x26e);
static const System::Word ecTitleCase = System::Word(0x26f);
static const System::Word ecUpperCaseBlock = System::Word(0x271);
static const System::Word ecLowerCaseBlock = System::Word(0x272);
static const System::Word ecToggleCaseBlock = System::Word(0x273);
static const System::Word ecString = System::Word(0x276);
static const System::Word ecUserFirst = System::Word(0x3e9);
extern DELPHI_PACKAGE void __fastcall GetEditorCommandValues(System::Classes::TGetStrProc Proc);
extern DELPHI_PACKAGE void __fastcall GetEditorCommandExtended(System::Classes::TGetStrProc Proc);
extern DELPHI_PACKAGE bool __fastcall IdentToEditorCommand(const System::UnicodeString Ident, int &Cmd);
extern DELPHI_PACKAGE bool __fastcall EditorCommandToIdent(int Cmd, System::UnicodeString &Ident);
extern DELPHI_PACKAGE System::UnicodeString __fastcall EditorCommandToDescrString(TSynEditorCommand Cmd);
extern DELPHI_PACKAGE System::UnicodeString __fastcall EditorCommandToCodeString(TSynEditorCommand Cmd);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConvertCodeStringToExtended(System::UnicodeString AString);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ConvertExtendedToCodeString(System::UnicodeString AString);
extern DELPHI_PACKAGE int __fastcall IndexToEditorCommand(const int AIndex);
extern DELPHI_PACKAGE TSynEditorCommand __fastcall ConvertExtendedToCommand(System::UnicodeString AString);
extern DELPHI_PACKAGE TSynEditorCommand __fastcall ConvertCodeStringToCommand(System::UnicodeString AString);
}	/* namespace Syneditkeycmds */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITKEYCMDS)
using namespace Syneditkeycmds;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditkeycmdsHPP
