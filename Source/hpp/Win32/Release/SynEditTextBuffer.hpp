// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditTextBuffer.pas' rev: 32.00 (Windows)

#ifndef SynedittextbufferHPP
#define SynedittextbufferHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <SynEditTypes.hpp>
#include <SynEditMiscProcs.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Graphics.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synedittextbuffer
{
//-- forward type declarations -----------------------------------------------
struct TSynEditStringRec;
struct TSynEditTwoWideChars;
class DELPHICLASS TSynEditStringList;
class DELPHICLASS ESynEditStringList;
class DELPHICLASS TSynEditUndoItem;
class DELPHICLASS TSynEditUndoList;
//-- type declarations -------------------------------------------------------
typedef void * TSynEditRange;

enum DECLSPEC_DENUM TSynEditStringFlag : unsigned char { sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown };

typedef System::Set<TSynEditStringFlag, TSynEditStringFlag::sfHasTabs, TSynEditStringFlag::sfExpandedLengthUnknown> TSynEditStringFlags;

typedef TSynEditStringRec *PSynEditStringRec;

struct DECLSPEC_DRECORD TSynEditStringRec
{
public:
	System::UnicodeString FString;
	System::TObject* fObject;
	void *fRange;
	int fExpandedLength;
	int fCharIndex;
	TSynEditStringFlags fFlags;
};


struct DECLSPEC_DRECORD TSynEditTwoWideChars
{
public:
	System::WideChar One;
	System::WideChar Two;
};


typedef TSynEditTwoWideChars *PSynEditTwoWideChars;

typedef System::StaticArray<TSynEditStringRec, 89478485> TSynEditStringRecList;

typedef TSynEditStringRecList *PSynEditStringRecList;

typedef void __fastcall (__closure *TStringListChangeEvent)(System::TObject* Sender, int Index, int Count);

typedef System::UnicodeString __fastcall (__closure *TExpandAtWideGlyphsFunc)(const System::UnicodeString S);

enum DECLSPEC_DENUM TSynEditFileFormat : unsigned char { sffDos, sffUnix, sffMac, sffUnicode };

class PASCALIMPLEMENTATION TSynEditStringList : public System::Classes::TStrings
{
	typedef System::Classes::TStrings inherited;
	
private:
	TSynEditStringRecList *fList;
	int fCount;
	int fCapacity;
	TSynEditFileFormat fFileFormat;
	bool fAppendNewLineAtEOF;
	Syneditmiscprocs::TConvertTabsProcEx fConvertTabsProc;
	int fIndexOfLongestLine;
	int fTabWidth;
	TExpandAtWideGlyphsFunc FExpandAtWideGlyphsFunc;
	bool FCharIndexesAreValid;
	System::Classes::TNotifyEvent fOnChange;
	System::Classes::TNotifyEvent fOnChanging;
	System::Classes::TNotifyEvent fOnCleared;
	TStringListChangeEvent fOnDeleted;
	TStringListChangeEvent fOnInserted;
	TStringListChangeEvent fOnPutted;
	System::UnicodeString __fastcall ExpandString(int Index);
	System::UnicodeString __fastcall GetExpandedString(int Index);
	int __fastcall GetExpandedStringLength(int Index);
	int __fastcall GetLengthOfLongestLine(void);
	void * __fastcall GetRange(int Index);
	void __fastcall Grow(void);
	void __fastcall InsertItem(int Index, const System::UnicodeString S);
	void __fastcall PutRange(int Index, void * ARange);
	void __fastcall SetFileFormat(const TSynEditFileFormat Value);
	
protected:
	bool FStreaming;
	virtual System::UnicodeString __fastcall Get(int Index);
	virtual int __fastcall GetCapacity(void);
	virtual int __fastcall GetCount(void);
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::UnicodeString __fastcall GetTextStr(void);
	virtual void __fastcall Put(int Index, const System::UnicodeString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	void __fastcall SetTabWidth(int Value);
	virtual void __fastcall SetUpdateState(bool Updating);
	void __fastcall UpdateCharIndexes(void);
	
public:
	__fastcall TSynEditStringList(TExpandAtWideGlyphsFunc AExpandAtWideGlyphsFunc);
	__fastcall virtual ~TSynEditStringList(void);
	virtual int __fastcall Add(const System::UnicodeString S);
	virtual void __fastcall AddStrings(System::Classes::TStrings* Strings)/* overload */;
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(int Index);
	void __fastcall DeleteLines(int Index, int NumLines);
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual void __fastcall Insert(int Index, const System::UnicodeString S);
	void __fastcall InsertLines(int Index, int NumLines);
	void __fastcall InsertStrings(int Index, System::Classes::TStrings* NewStrings);
	void __fastcall InsertText(int Index, System::UnicodeString NewText);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream, System::Sysutils::TEncoding* Encoding)/* overload */;
	System::UnicodeString __fastcall GetSeparatedText(System::UnicodeString Separators);
	virtual void __fastcall SetTextStr(const System::UnicodeString Value);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream)/* overload */;
	void __fastcall FontChanged(void);
	int __fastcall LineCharLength(int Index);
	int __fastcall LineCharIndex(int Index);
	__property bool AppendNewLineAtEOF = {read=fAppendNewLineAtEOF, write=fAppendNewLineAtEOF, nodefault};
	__property TSynEditFileFormat FileFormat = {read=fFileFormat, write=SetFileFormat, nodefault};
	__property System::UnicodeString ExpandedStrings[int Index] = {read=GetExpandedString};
	__property int ExpandedStringLengths[int Index] = {read=GetExpandedStringLength};
	__property int LengthOfLongestLine = {read=GetLengthOfLongestLine, nodefault};
	__property void * Ranges[int Index] = {read=GetRange, write=PutRange};
	__property int TabWidth = {read=fTabWidth, write=SetTabWidth, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=fOnChange, write=fOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=fOnChanging, write=fOnChanging};
	__property System::Classes::TNotifyEvent OnCleared = {read=fOnCleared, write=fOnCleared};
	__property TStringListChangeEvent OnDeleted = {read=fOnDeleted, write=fOnDeleted};
	__property TStringListChangeEvent OnInserted = {read=fOnInserted, write=fOnInserted};
	__property TStringListChangeEvent OnPutted = {read=fOnPutted, write=fOnPutted};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  AddStrings(const System::DynamicArray<System::UnicodeString> Strings){ System::Classes::TStrings::AddStrings(Strings); }
	inline void __fastcall  AddStrings(const System::DynamicArray<System::UnicodeString> Strings, const System::DynamicArray<System::TObject*> Objects){ System::Classes::TStrings::AddStrings(Strings, Objects); }
	inline void __fastcall  SaveToStream(System::Classes::TStream* Stream){ System::Classes::TStrings::SaveToStream(Stream); }
	inline void __fastcall  LoadFromStream(System::Classes::TStream* Stream, System::Sysutils::TEncoding* Encoding){ System::Classes::TStrings::LoadFromStream(Stream, Encoding); }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION ESynEditStringList : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall ESynEditStringList(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall ESynEditStringList(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall ESynEditStringList(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall ESynEditStringList(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynEditStringList(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall ESynEditStringList(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall ESynEditStringList(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall ESynEditStringList(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynEditStringList(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall ESynEditStringList(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynEditStringList(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall ESynEditStringList(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~ESynEditStringList(void) { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TSynChangeReason : unsigned char { crInsert, crPaste, crDragDropInsert, crDeleteAfterCursor, crDelete, crLineBreak, crIndent, crUnindent, crSilentDelete, crSilentDeleteAfterCursor, crAutoCompleteBegin, crAutoCompleteEnd, crPasteBegin, crPasteEnd, crSpecial1Begin, crSpecial1End, crSpecial2Begin, crSpecial2End, crCaret, crSelection, crNothing, crGroupBreak, crDeleteAll, crWhiteSpaceAdd };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynEditUndoItem : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	TSynChangeReason fChangeReason;
	Synedittypes::TSynSelectionMode fChangeSelMode;
	Synedittypes::TBufferCoord fChangeStartPos;
	Synedittypes::TBufferCoord fChangeEndPos;
	System::UnicodeString fChangeStr;
	int fChangeNumber;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TSynChangeReason ChangeReason = {read=fChangeReason, nodefault};
	__property Synedittypes::TSynSelectionMode ChangeSelMode = {read=fChangeSelMode, nodefault};
	__property Synedittypes::TBufferCoord ChangeStartPos = {read=fChangeStartPos};
	__property Synedittypes::TBufferCoord ChangeEndPos = {read=fChangeEndPos};
	__property System::UnicodeString ChangeStr = {read=fChangeStr};
	__property int ChangeNumber = {read=fChangeNumber, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSynEditUndoItem(void) { }
	
public:
	/* TObject.Create */ inline __fastcall TSynEditUndoItem(void) : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TSynEditUndoList : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	int fBlockChangeNumber;
	int fBlockCount;
	bool fFullUndoImposible;
	System::Classes::TList* fItems;
	int fLockCount;
	int fMaxUndoActions;
	int fNextChangeNumber;
	int fInitialChangeNumber;
	bool fInsideRedo;
	System::Classes::TNotifyEvent fOnAddedUndo;
	void __fastcall EnsureMaxEntries(void);
	bool __fastcall GetCanUndo(void);
	int __fastcall GetItemCount(void);
	void __fastcall SetMaxUndoActions(int Value);
	void __fastcall SetInitialState(const bool Value);
	bool __fastcall GetInitialState(void);
	TSynEditUndoItem* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TSynEditUndoItem* const Value);
	
public:
	__fastcall TSynEditUndoList(void);
	__fastcall virtual ~TSynEditUndoList(void);
	void __fastcall AddChange(TSynChangeReason AReason, const Synedittypes::TBufferCoord &AStart, const Synedittypes::TBufferCoord &AEnd, const System::UnicodeString ChangeText, Synedittypes::TSynSelectionMode SelMode);
	void __fastcall BeginBlock(void);
	void __fastcall Clear(void);
	void __fastcall EndBlock(void);
	void __fastcall Lock(void);
	TSynEditUndoItem* __fastcall PeekItem(void);
	TSynEditUndoItem* __fastcall PopItem(void);
	void __fastcall PushItem(TSynEditUndoItem* Item);
	void __fastcall Unlock(void);
	TSynChangeReason __fastcall LastChangeReason(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall AddGroupBreak(void);
	void __fastcall DeleteItem(int AIndex);
	__property int BlockChangeNumber = {read=fBlockChangeNumber, write=fBlockChangeNumber, nodefault};
	__property bool CanUndo = {read=GetCanUndo, nodefault};
	__property bool FullUndoImpossible = {read=fFullUndoImposible, nodefault};
	__property bool InitialState = {read=GetInitialState, write=SetInitialState, nodefault};
	__property TSynEditUndoItem* Items[int Index] = {read=GetItems, write=SetItems};
	__property int ItemCount = {read=GetItemCount, nodefault};
	__property int BlockCount = {read=fBlockCount, nodefault};
	__property int MaxUndoActions = {read=fMaxUndoActions, write=SetMaxUndoActions, nodefault};
	__property bool InsideRedo = {read=fInsideRedo, write=fInsideRedo, nodefault};
	__property System::Classes::TNotifyEvent OnAddedUndo = {read=fOnAddedUndo, write=fOnAddedUndo};
};


//-- var, const, procedure ---------------------------------------------------
static const int SynEditStringRecSize = int(0x18);
static const int MaxSynEditStrings = int(0x5555555);
#define NullRange (void *)(0xffffffff)
}	/* namespace Synedittextbuffer */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITTEXTBUFFER)
using namespace Synedittextbuffer;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynedittextbufferHPP
