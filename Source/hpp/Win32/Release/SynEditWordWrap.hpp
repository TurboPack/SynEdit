// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditWordWrap.pas' rev: 30.00 (Windows)

#ifndef SyneditwordwrapHPP
#define SyneditwordwrapHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <SynEditTypes.hpp>
#include <SynEditTextBuffer.hpp>
#include <SynEdit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditwordwrap
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynWordWrapPlugin;
//-- type declarations -------------------------------------------------------
typedef int TLineIndex;

typedef int TRowIndex;

typedef System::Word TRowLength;

typedef System::StaticArray<TRowIndex, 134217728> TRowIndexArray;

typedef TRowIndexArray *PRowIndexArray;

typedef System::StaticArray<System::Word, 134217728> TRowLengthArray;

typedef TRowLengthArray *PRowLengthArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynWordWrapPlugin : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
private:
	TRowIndexArray *fLineOffsets;
	TRowLengthArray *fRowLengths;
	int fLineCapacity;
	int fRowCapacity;
	int fLineCount;
	Synedit::TCustomSynEdit* fEditor;
	System::Word fMinRowLength;
	System::Word fMaxRowLength;
	void __fastcall GrowLines(int aMinSize);
	void __fastcall MoveLines(TLineIndex aStart, int aMoveBy);
	void __fastcall GrowRows(int aMinSize);
	void __fastcall MoveRows(TRowIndex aStart, int aMoveBy);
	void __fastcall SetEmpty(void);
	
protected:
	void __fastcall WrapLines(void);
	int __fastcall ReWrapLine(TLineIndex aIndex);
	void __fastcall TrimArrays(void);
	__property PRowIndexArray LineOffsets = {read=fLineOffsets};
	__property PRowLengthArray RowLengths = {read=fRowLengths};
	__property Synedit::TCustomSynEdit* Editor = {read=fEditor};
	
public:
	__fastcall TSynWordWrapPlugin(Synedit::TCustomSynEdit* aOwner);
	__fastcall virtual ~TSynWordWrapPlugin(void);
	__property int LineCount = {read=fLineCount, nodefault};
	Synedittypes::TDisplayCoord __fastcall BufferToDisplayPos(const Synedittypes::TBufferCoord &aPos);
	Synedittypes::TBufferCoord __fastcall DisplayToBufferPos(const Synedittypes::TDisplayCoord &aPos);
	int __fastcall RowCount(void);
	int __fastcall GetRowLength(int aRow);
	int __fastcall LinesInserted(int aIndex, int aCount);
	int __fastcall LinesDeleted(int aIndex, int aCount);
	int __fastcall LinesPutted(int aIndex, int aCount);
	void __fastcall Reset(void);
	void __fastcall DisplayChanged(void);
private:
	void *__ISynEditBufferPlugin;	// Synedit::ISynEditBufferPlugin 
	
public:
	operator Synedit::ISynEditBufferPlugin*(void) { return (Synedit::ISynEditBufferPlugin*)&__ISynEditBufferPlugin; }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool OldWhitespaceBehaviour;
static const int MaxIndex = int(0x7ffffff);
}	/* namespace Syneditwordwrap */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITWORDWRAP)
using namespace Syneditwordwrap;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditwordwrapHPP
