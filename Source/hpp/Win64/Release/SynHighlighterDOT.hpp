// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynHighlighterDOT.pas' rev: 31.00 (Windows)

#ifndef SynhighlighterdotHPP
#define SynhighlighterdotHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <SynEditTypes.hpp>
#include <SynEditHighlighter.hpp>
#include <SynUnicode.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synhighlighterdot
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynDOTSyn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TtkTokenKind : unsigned char { tkArrowHead, tkAttribute, tkComment, tkDirections, tkIdentifier, tkKey, tkNull, tkShape, tkSpace, tkString, tkUnknown, tkValue, tkSymbol };

enum DECLSPEC_DENUM TRangeState : unsigned char { rsUnKnown, rsCStyleComment, rsString };

typedef TtkTokenKind __fastcall (__closure *TIdentFuncTableFunc)(int Index);

typedef TIdentFuncTableFunc *PIdentFuncTableFunc;

class PASCALIMPLEMENTATION TSynDOTSyn : public Synedithighlighter::TSynCustomHighlighter
{
	typedef Synedithighlighter::TSynCustomHighlighter inherited;
	
private:
	TRangeState fRange;
	TtkTokenKind fTokenID;
	System::StaticArray<TIdentFuncTableFunc, 787> fIdentFuncTable;
	Synedithighlighter::TSynHighlighterAttributes* fArrowHeadAttri;
	Synedithighlighter::TSynHighlighterAttributes* fAttributeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fCommentAttri;
	Synedithighlighter::TSynHighlighterAttributes* fDirectionsAttri;
	Synedithighlighter::TSynHighlighterAttributes* fIdentifierAttri;
	Synedithighlighter::TSynHighlighterAttributes* fKeyAttri;
	Synedithighlighter::TSynHighlighterAttributes* fShapeAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSpaceAttri;
	Synedithighlighter::TSynHighlighterAttributes* fStringAttri;
	Synedithighlighter::TSynHighlighterAttributes* fValueAttri;
	Synedithighlighter::TSynHighlighterAttributes* fSymbolAttri;
	TtkTokenKind __fastcall AltFunc(int Index);
	TtkTokenKind __fastcall FuncAll(int Index);
	TtkTokenKind __fastcall FuncAppendix(int Index);
	TtkTokenKind __fastcall FuncArrowhead(int Index);
	TtkTokenKind __fastcall FuncArrowsize(int Index);
	TtkTokenKind __fastcall FuncArrowtail(int Index);
	TtkTokenKind __fastcall FuncAuto(int Index);
	TtkTokenKind __fastcall FuncBack(int Index);
	TtkTokenKind __fastcall FuncBgcolor(int Index);
	TtkTokenKind __fastcall FuncBold(int Index);
	TtkTokenKind __fastcall FuncBoth(int Index);
	TtkTokenKind __fastcall FuncBottomlabel(int Index);
	TtkTokenKind __fastcall FuncBox(int Index);
	TtkTokenKind __fastcall FuncCenter(int Index);
	TtkTokenKind __fastcall FuncCircle(int Index);
	TtkTokenKind __fastcall FuncClusterrank(int Index);
	TtkTokenKind __fastcall FuncColor(int Index);
	TtkTokenKind __fastcall FuncComment(int Index);
	TtkTokenKind __fastcall FuncCompound(int Index);
	TtkTokenKind __fastcall FuncConcentrate(int Index);
	TtkTokenKind __fastcall FuncConstraint(int Index);
	TtkTokenKind __fastcall FuncDecorate(int Index);
	TtkTokenKind __fastcall FuncDiamond(int Index);
	TtkTokenKind __fastcall FuncDigraph(int Index);
	TtkTokenKind __fastcall FuncDir(int Index);
	TtkTokenKind __fastcall FuncDistortion(int Index);
	TtkTokenKind __fastcall FuncDot(int Index);
	TtkTokenKind __fastcall FuncDotted(int Index);
	TtkTokenKind __fastcall FuncDoublecircle(int Index);
	TtkTokenKind __fastcall FuncDoubleoctagon(int Index);
	TtkTokenKind __fastcall FuncE(int Index);
	TtkTokenKind __fastcall FuncEdge(int Index);
	TtkTokenKind __fastcall FuncEgg(int Index);
	TtkTokenKind __fastcall FuncEllipse(int Index);
	TtkTokenKind __fastcall FuncFalse(int Index);
	TtkTokenKind __fastcall FuncFill(int Index);
	TtkTokenKind __fastcall FuncFillcolor(int Index);
	TtkTokenKind __fastcall FuncFilled(int Index);
	TtkTokenKind __fastcall FuncFixedsize(int Index);
	TtkTokenKind __fastcall FuncFontcolor(int Index);
	TtkTokenKind __fastcall FuncFontname(int Index);
	TtkTokenKind __fastcall FuncFontpath(int Index);
	TtkTokenKind __fastcall FuncFontsize(int Index);
	TtkTokenKind __fastcall FuncForward(int Index);
	TtkTokenKind __fastcall FuncGlobal(int Index);
	TtkTokenKind __fastcall FuncGraph(int Index);
	TtkTokenKind __fastcall FuncGroup(int Index);
	TtkTokenKind __fastcall FuncHeadlabel(int Index);
	TtkTokenKind __fastcall FuncHeadport(int Index);
	TtkTokenKind __fastcall FuncHeadurl(int Index);
	TtkTokenKind __fastcall FuncHeight(int Index);
	TtkTokenKind __fastcall FuncHexagon(int Index);
	TtkTokenKind __fastcall FuncHouse(int Index);
	TtkTokenKind __fastcall FuncId(int Index);
	TtkTokenKind __fastcall FuncInv(int Index);
	TtkTokenKind __fastcall FuncInvdot(int Index);
	TtkTokenKind __fastcall FuncInvhouse(int Index);
	TtkTokenKind __fastcall FuncInvodot(int Index);
	TtkTokenKind __fastcall FuncInvtrapezium(int Index);
	TtkTokenKind __fastcall FuncInvtriangle(int Index);
	TtkTokenKind __fastcall FuncLabel(int Index);
	TtkTokenKind __fastcall FuncLabelangle(int Index);
	TtkTokenKind __fastcall FuncLabeldistance(int Index);
	TtkTokenKind __fastcall FuncLabelfloat(int Index);
	TtkTokenKind __fastcall FuncLabelfontcolor(int Index);
	TtkTokenKind __fastcall FuncLabelfontname(int Index);
	TtkTokenKind __fastcall FuncLabelfontsize(int Index);
	TtkTokenKind __fastcall FuncLabeljust(int Index);
	TtkTokenKind __fastcall FuncLabelloc(int Index);
	TtkTokenKind __fastcall FuncLayer(int Index);
	TtkTokenKind __fastcall FuncLayers(int Index);
	TtkTokenKind __fastcall FuncLhead(int Index);
	TtkTokenKind __fastcall FuncLtail(int Index);
	TtkTokenKind __fastcall FuncMargin(int Index);
	TtkTokenKind __fastcall FuncMax(int Index);
	TtkTokenKind __fastcall FuncMcircle(int Index);
	TtkTokenKind __fastcall FuncMclimit(int Index);
	TtkTokenKind __fastcall FuncMdiamond(int Index);
	TtkTokenKind __fastcall FuncMerged(int Index);
	TtkTokenKind __fastcall FuncMin(int Index);
	TtkTokenKind __fastcall FuncMinimum(int Index);
	TtkTokenKind __fastcall FuncMinlen(int Index);
	TtkTokenKind __fastcall FuncMrecord(int Index);
	TtkTokenKind __fastcall FuncMsquare(int Index);
	TtkTokenKind __fastcall FuncMultiples(int Index);
	TtkTokenKind __fastcall FuncN(int Index);
	TtkTokenKind __fastcall FuncNe(int Index);
	TtkTokenKind __fastcall FuncNode(int Index);
	TtkTokenKind __fastcall FuncNodesep(int Index);
	TtkTokenKind __fastcall FuncNone(int Index);
	TtkTokenKind __fastcall FuncNormal(int Index);
	TtkTokenKind __fastcall FuncNslimit(int Index);
	TtkTokenKind __fastcall FuncNw(int Index);
	TtkTokenKind __fastcall FuncOctagon(int Index);
	TtkTokenKind __fastcall FuncOdot(int Index);
	TtkTokenKind __fastcall FuncOnto(int Index);
	TtkTokenKind __fastcall FuncOrdering(int Index);
	TtkTokenKind __fastcall FuncOrientation(int Index);
	TtkTokenKind __fastcall FuncPage(int Index);
	TtkTokenKind __fastcall FuncPagedir(int Index);
	TtkTokenKind __fastcall FuncParallelogram(int Index);
	TtkTokenKind __fastcall FuncPeripheries(int Index);
	TtkTokenKind __fastcall FuncPlaintext(int Index);
	TtkTokenKind __fastcall FuncPoint(int Index);
	TtkTokenKind __fastcall FuncPolygon(int Index);
	TtkTokenKind __fastcall FuncQuantum(int Index);
	TtkTokenKind __fastcall FuncRank(int Index);
	TtkTokenKind __fastcall FuncRankdir(int Index);
	TtkTokenKind __fastcall FuncRanksep(int Index);
	TtkTokenKind __fastcall FuncRatio(int Index);
	TtkTokenKind __fastcall FuncRecord(int Index);
	TtkTokenKind __fastcall FuncRegular(int Index);
	TtkTokenKind __fastcall FuncRemincross(int Index);
	TtkTokenKind __fastcall FuncRotate(int Index);
	TtkTokenKind __fastcall FuncS(int Index);
	TtkTokenKind __fastcall FuncSame(int Index);
	TtkTokenKind __fastcall FuncSamehead(int Index);
	TtkTokenKind __fastcall FuncSametail(int Index);
	TtkTokenKind __fastcall FuncSamplepoints(int Index);
	TtkTokenKind __fastcall FuncSe(int Index);
	TtkTokenKind __fastcall FuncSearchsize(int Index);
	TtkTokenKind __fastcall FuncSection(int Index);
	TtkTokenKind __fastcall FuncShape(int Index);
	TtkTokenKind __fastcall FuncShapefile(int Index);
	TtkTokenKind __fastcall FuncSides(int Index);
	TtkTokenKind __fastcall FuncSink(int Index);
	TtkTokenKind __fastcall FuncSize(int Index);
	TtkTokenKind __fastcall FuncSkew(int Index);
	TtkTokenKind __fastcall FuncSource(int Index);
	TtkTokenKind __fastcall FuncStrict(int Index);
	TtkTokenKind __fastcall FuncStyle(int Index);
	TtkTokenKind __fastcall FuncSubgraph(int Index);
	TtkTokenKind __fastcall FuncSw(int Index);
	TtkTokenKind __fastcall FuncTaillabel(int Index);
	TtkTokenKind __fastcall FuncTailport(int Index);
	TtkTokenKind __fastcall FuncTailurl(int Index);
	TtkTokenKind __fastcall FuncToplabel(int Index);
	TtkTokenKind __fastcall FuncTrapezium(int Index);
	TtkTokenKind __fastcall FuncTriangle(int Index);
	TtkTokenKind __fastcall FuncTripleoctagon(int Index);
	TtkTokenKind __fastcall FuncTrue(int Index);
	TtkTokenKind __fastcall FuncUrl(int Index);
	TtkTokenKind __fastcall FuncW(int Index);
	TtkTokenKind __fastcall FuncWeight(int Index);
	TtkTokenKind __fastcall FuncWhen(int Index);
	TtkTokenKind __fastcall FuncWidth(int Index);
	TtkTokenKind __fastcall FuncZ(int Index);
	unsigned __fastcall HashKey(System::WideChar * Str);
	TtkTokenKind __fastcall IdentKind(System::WideChar * MayBe);
	void __fastcall InitIdent(void);
	void __fastcall IdentProc(void);
	void __fastcall UnknownProc(void);
	void __fastcall NullProc(void);
	void __fastcall SpaceProc(void);
	void __fastcall CRProc(void);
	void __fastcall LFProc(void);
	void __fastcall CStyleCommentOpenProc(void);
	void __fastcall CStyleCommentProc(void);
	void __fastcall StringOpenProc(void);
	void __fastcall StringProc(void);
	void __fastcall SymbolProc(void);
	void __fastcall DirectionsProc(void);
	
protected:
	virtual System::UnicodeString __fastcall GetSampleSource(void);
	virtual bool __fastcall IsFilterStored(void);
	
public:
	__fastcall virtual TSynDOTSyn(System::Classes::TComponent* AOwner);
	__classmethod virtual System::UnicodeString __fastcall GetLanguageName();
	__classmethod virtual System::UnicodeString __fastcall GetFriendlyLanguageName();
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
	__property Synedithighlighter::TSynHighlighterAttributes* ArrowHeadAttri = {read=fArrowHeadAttri, write=fArrowHeadAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* AttributeAttri = {read=fAttributeAttri, write=fAttributeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* CommentAttri = {read=fCommentAttri, write=fCommentAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* DirectionsAttri = {read=fDirectionsAttri, write=fDirectionsAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* IdentifierAttri = {read=fIdentifierAttri, write=fIdentifierAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* KeyAttri = {read=fKeyAttri, write=fKeyAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ShapeAttri = {read=fShapeAttri, write=fShapeAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SpaceAttri = {read=fSpaceAttri, write=fSpaceAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* StringAttri = {read=fStringAttri, write=fStringAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* ValueAttri = {read=fValueAttri, write=fValueAttri};
	__property Synedithighlighter::TSynHighlighterAttributes* SymbolAttri = {read=fSymbolAttri, write=fSymbolAttri};
public:
	/* TSynCustomHighlighter.Destroy */ inline __fastcall virtual ~TSynDOTSyn(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Synhighlighterdot */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNHIGHLIGHTERDOT)
using namespace Synhighlighterdot;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynhighlighterdotHPP
