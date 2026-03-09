{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterDOT.pas, released 2002-11-30.
Description: DOT Syntax Parser/Highlighter
The initial author of this file is nissl (nissl@tiscali.it, nissl@mammuth.it)
Unicode translation by Maël Hörz.
Copyright (c) 2002, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}
{
@abstract(Provides a ATT DOT highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it))
@created(november 2002)
@lastmod(2002-11-30)
The SynHighlighterDOT unit provides SynEdit with a DOT Graph Drawing (.dot) highlighter.
The highlighter formats DOT source code ref.: http://www.research.att.com/sw/tools/graphviz/.
}

unit SynHighlighterDOT;

{$I SynEdit.inc}

interface

uses
  Windows,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynFunc,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkArrowHead,
    tkAttribute,
    tkComment,
    tkDirections,
    tkIdentifier,
    tkKey,
    tkNull,
    tkShape,
    tkSpace,
    tkString,
    tkUnknown,
    tkValue,
    tkSymbol);

  TRangeState = (rsUnKnown, rsCStyleComment, rsString);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: TSynNativeInt): TtkTokenKind of object;

type
  TSynDOTSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..786] of TIdentFuncTableFunc;
    fArrowHeadAttri: TSynHighlighterAttributes;
    fAttributeAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirectionsAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fShapeAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function AltFunc(Index: TSynNativeInt): TtkTokenKind;
    function FuncAll(Index: TSynNativeInt): TtkTokenKind;
    function FuncAppendix(Index: TSynNativeInt): TtkTokenKind;
    function FuncArrowhead(Index: TSynNativeInt): TtkTokenKind;
    function FuncArrowsize(Index: TSynNativeInt): TtkTokenKind;
    function FuncArrowtail(Index: TSynNativeInt): TtkTokenKind;
    function FuncAuto(Index: TSynNativeInt): TtkTokenKind;
    function FuncBack(Index: TSynNativeInt): TtkTokenKind;
    function FuncBgcolor(Index: TSynNativeInt): TtkTokenKind;
    function FuncBold(Index: TSynNativeInt): TtkTokenKind;
    function FuncBoth(Index: TSynNativeInt): TtkTokenKind;
    function FuncBottomlabel(Index: TSynNativeInt): TtkTokenKind;
    function FuncBox(Index: TSynNativeInt): TtkTokenKind;
    function FuncCenter(Index: TSynNativeInt): TtkTokenKind;
    function FuncCircle(Index: TSynNativeInt): TtkTokenKind;
    function FuncClusterrank(Index: TSynNativeInt): TtkTokenKind;
    function FuncColor(Index: TSynNativeInt): TtkTokenKind;
    function FuncComment(Index: TSynNativeInt): TtkTokenKind;
    function FuncCompound(Index: TSynNativeInt): TtkTokenKind;
    function FuncConcentrate(Index: TSynNativeInt): TtkTokenKind;
    function FuncConstraint(Index: TSynNativeInt): TtkTokenKind;
    function FuncDecorate(Index: TSynNativeInt): TtkTokenKind;
    function FuncDiamond(Index: TSynNativeInt): TtkTokenKind;
    function FuncDigraph(Index: TSynNativeInt): TtkTokenKind;
    function FuncDir(Index: TSynNativeInt): TtkTokenKind;
    function FuncDistortion(Index: TSynNativeInt): TtkTokenKind;
    function FuncDot(Index: TSynNativeInt): TtkTokenKind;
    function FuncDotted(Index: TSynNativeInt): TtkTokenKind;
    function FuncDoublecircle(Index: TSynNativeInt): TtkTokenKind;
    function FuncDoubleoctagon(Index: TSynNativeInt): TtkTokenKind;
    function FuncE(Index: TSynNativeInt): TtkTokenKind;
    function FuncEdge(Index: TSynNativeInt): TtkTokenKind;
    function FuncEgg(Index: TSynNativeInt): TtkTokenKind;
    function FuncEllipse(Index: TSynNativeInt): TtkTokenKind;
    function FuncFalse(Index: TSynNativeInt): TtkTokenKind;
    function FuncFill(Index: TSynNativeInt): TtkTokenKind;
    function FuncFillcolor(Index: TSynNativeInt): TtkTokenKind;
    function FuncFilled(Index: TSynNativeInt): TtkTokenKind;
    function FuncFixedsize(Index: TSynNativeInt): TtkTokenKind;
    function FuncFontcolor(Index: TSynNativeInt): TtkTokenKind;
    function FuncFontname(Index: TSynNativeInt): TtkTokenKind;
    function FuncFontpath(Index: TSynNativeInt): TtkTokenKind;
    function FuncFontsize(Index: TSynNativeInt): TtkTokenKind;
    function FuncForward(Index: TSynNativeInt): TtkTokenKind;
    function FuncGlobal(Index: TSynNativeInt): TtkTokenKind;
    function FuncGraph(Index: TSynNativeInt): TtkTokenKind;
    function FuncGroup(Index: TSynNativeInt): TtkTokenKind;
    function FuncHeadlabel(Index: TSynNativeInt): TtkTokenKind;
    function FuncHeadport(Index: TSynNativeInt): TtkTokenKind;
    function FuncHeadurl(Index: TSynNativeInt): TtkTokenKind;
    function FuncHeight(Index: TSynNativeInt): TtkTokenKind;
    function FuncHexagon(Index: TSynNativeInt): TtkTokenKind;
    function FuncHouse(Index: TSynNativeInt): TtkTokenKind;
    function FuncId(Index: TSynNativeInt): TtkTokenKind;
    function FuncInv(Index: TSynNativeInt): TtkTokenKind;
    function FuncInvdot(Index: TSynNativeInt): TtkTokenKind;
    function FuncInvhouse(Index: TSynNativeInt): TtkTokenKind;
    function FuncInvodot(Index: TSynNativeInt): TtkTokenKind;
    function FuncInvtrapezium(Index: TSynNativeInt): TtkTokenKind;
    function FuncInvtriangle(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabel(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabelangle(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabeldistance(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabelfloat(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabelfontcolor(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabelfontname(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabelfontsize(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabeljust(Index: TSynNativeInt): TtkTokenKind;
    function FuncLabelloc(Index: TSynNativeInt): TtkTokenKind;
    function FuncLayer(Index: TSynNativeInt): TtkTokenKind;
    function FuncLayers(Index: TSynNativeInt): TtkTokenKind;
    function FuncLhead(Index: TSynNativeInt): TtkTokenKind;
    function FuncLtail(Index: TSynNativeInt): TtkTokenKind;
    function FuncMargin(Index: TSynNativeInt): TtkTokenKind;
    function FuncMax(Index: TSynNativeInt): TtkTokenKind;
    function FuncMcircle(Index: TSynNativeInt): TtkTokenKind;
    function FuncMclimit(Index: TSynNativeInt): TtkTokenKind;
    function FuncMdiamond(Index: TSynNativeInt): TtkTokenKind;
    function FuncMerged(Index: TSynNativeInt): TtkTokenKind;
    function FuncMin(Index: TSynNativeInt): TtkTokenKind;
    function FuncMinimum(Index: TSynNativeInt): TtkTokenKind;
    function FuncMinlen(Index: TSynNativeInt): TtkTokenKind;
    function FuncMrecord(Index: TSynNativeInt): TtkTokenKind;
    function FuncMsquare(Index: TSynNativeInt): TtkTokenKind;
    function FuncMultiples(Index: TSynNativeInt): TtkTokenKind;
    function FuncN(Index: TSynNativeInt): TtkTokenKind;
    function FuncNe(Index: TSynNativeInt): TtkTokenKind;
    function FuncNode(Index: TSynNativeInt): TtkTokenKind;
    function FuncNodesep(Index: TSynNativeInt): TtkTokenKind;
    function FuncNone(Index: TSynNativeInt): TtkTokenKind;
    function FuncNormal(Index: TSynNativeInt): TtkTokenKind;
    function FuncNslimit(Index: TSynNativeInt): TtkTokenKind;
    function FuncNw(Index: TSynNativeInt): TtkTokenKind;
    function FuncOctagon(Index: TSynNativeInt): TtkTokenKind;
    function FuncOdot(Index: TSynNativeInt): TtkTokenKind;
    function FuncOnto(Index: TSynNativeInt): TtkTokenKind;
    function FuncOrdering(Index: TSynNativeInt): TtkTokenKind;
    function FuncOrientation(Index: TSynNativeInt): TtkTokenKind;
    function FuncPage(Index: TSynNativeInt): TtkTokenKind;
    function FuncPagedir(Index: TSynNativeInt): TtkTokenKind;
    function FuncParallelogram(Index: TSynNativeInt): TtkTokenKind;
    function FuncPeripheries(Index: TSynNativeInt): TtkTokenKind;
    function FuncPlaintext(Index: TSynNativeInt): TtkTokenKind;
    function FuncPoint(Index: TSynNativeInt): TtkTokenKind;
    function FuncPolygon(Index: TSynNativeInt): TtkTokenKind;
    function FuncQuantum(Index: TSynNativeInt): TtkTokenKind;
    function FuncRank(Index: TSynNativeInt): TtkTokenKind;
    function FuncRankdir(Index: TSynNativeInt): TtkTokenKind;
    function FuncRanksep(Index: TSynNativeInt): TtkTokenKind;
    function FuncRatio(Index: TSynNativeInt): TtkTokenKind;
    function FuncRecord(Index: TSynNativeInt): TtkTokenKind;
    function FuncRegular(Index: TSynNativeInt): TtkTokenKind;
    function FuncRemincross(Index: TSynNativeInt): TtkTokenKind;
    function FuncRotate(Index: TSynNativeInt): TtkTokenKind;
    function FuncS(Index: TSynNativeInt): TtkTokenKind;
    function FuncSame(Index: TSynNativeInt): TtkTokenKind;
    function FuncSamehead(Index: TSynNativeInt): TtkTokenKind;
    function FuncSametail(Index: TSynNativeInt): TtkTokenKind;
    function FuncSamplepoints(Index: TSynNativeInt): TtkTokenKind;
    function FuncSe(Index: TSynNativeInt): TtkTokenKind;
    function FuncSearchsize(Index: TSynNativeInt): TtkTokenKind;
    function FuncSection(Index: TSynNativeInt): TtkTokenKind;
    function FuncShape(Index: TSynNativeInt): TtkTokenKind;
    function FuncShapefile(Index: TSynNativeInt): TtkTokenKind;
    function FuncSides(Index: TSynNativeInt): TtkTokenKind;
    function FuncSink(Index: TSynNativeInt): TtkTokenKind;
    function FuncSize(Index: TSynNativeInt): TtkTokenKind;
    function FuncSkew(Index: TSynNativeInt): TtkTokenKind;
    function FuncSource(Index: TSynNativeInt): TtkTokenKind;
    function FuncStrict(Index: TSynNativeInt): TtkTokenKind;
    function FuncStyle(Index: TSynNativeInt): TtkTokenKind;
    function FuncSubgraph(Index: TSynNativeInt): TtkTokenKind;
    function FuncSw(Index: TSynNativeInt): TtkTokenKind;
    function FuncTaillabel(Index: TSynNativeInt): TtkTokenKind;
    function FuncTailport(Index: TSynNativeInt): TtkTokenKind;
    function FuncTailurl(Index: TSynNativeInt): TtkTokenKind;
    function FuncToplabel(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrapezium(Index: TSynNativeInt): TtkTokenKind;
    function FuncTriangle(Index: TSynNativeInt): TtkTokenKind;
    function FuncTripleoctagon(Index: TSynNativeInt): TtkTokenKind;
    function FuncTrue(Index: TSynNativeInt): TtkTokenKind;
    function FuncUrl(Index: TSynNativeInt): TtkTokenKind;
    function FuncW(Index: TSynNativeInt): TtkTokenKind;
    function FuncWeight(Index: TSynNativeInt): TtkTokenKind;
    function FuncWhen(Index: TSynNativeInt): TtkTokenKind;
    function FuncWidth(Index: TSynNativeInt): TtkTokenKind;
    function FuncZ(Index: TSynNativeInt): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure IdentProc;
    procedure UnknownProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CStyleCommentOpenProc;
    procedure CStyleCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure DirectionsProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords(TokenKind: TSynNativeInt): string; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
     function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property ArrowHeadAttri: TSynHighlighterAttributes read fArrowHeadAttri write fArrowHeadAttri;
    property AttributeAttri: TSynHighlighterAttributes read fAttributeAttri write fAttributeAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DirectionsAttri: TSynHighlighterAttributes read fDirectionsAttri write fDirectionsAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property ShapeAttri: TSynHighlighterAttributes read fShapeAttri write fShapeAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri write fValueAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  KeyWords: array[0..145] of string = (
    'all', 'appendix', 'arrowhead', 'arrowsize', 'arrowtail', 'auto', 'back', 
    'bgcolor', 'bold', 'both', 'bottomlabel', 'box', 'center', 'circle', 
    'clusterrank', 'color', 'comment', 'compound', 'concentrate', 'constraint', 
    'decorate', 'diamond', 'digraph', 'dir', 'distortion', 'dot', 'dotted', 
    'doublecircle', 'doubleoctagon', 'e', 'edge', 'egg', 'ellipse', 'false', 
    'fill', 'fillcolor', 'filled', 'fixedsize', 'fontcolor', 'fontname', 
    'fontpath', 'fontsize', 'forward', 'global', 'graph', 'group', 'headlabel', 
    'headport', 'headurl', 'height', 'hexagon', 'house', 'id', 'inv', 'invdot', 
    'invhouse', 'invodot', 'invtrapezium', 'invtriangle', 'label', 'labelangle', 
    'labeldistance', 'labelfloat', 'labelfontcolor', 'labelfontname', 
    'labelfontsize', 'labeljust', 'labelloc', 'layer', 'layers', 'lhead', 
    'ltail', 'margin', 'max', 'mcircle', 'mclimit', 'mdiamond', 'merged', 'min', 
    'minimum', 'minlen', 'mrecord', 'msquare', 'multiples', 'n', 'ne', 'node', 
    'nodesep', 'none', 'normal', 'nslimit', 'nw', 'octagon', 'odot', 'onto', 
    'ordering', 'orientation', 'page', 'pagedir', 'parallelogram', 
    'peripheries', 'plaintext', 'point', 'polygon', 'quantum', 'rank', 
    'rankdir', 'ranksep', 'ratio', 'record', 'regular', 'remincross', 'rotate', 
    's', 'same', 'samehead', 'sametail', 'samplepoints', 'se', 'searchsize', 
    'section', 'shape', 'shapefile', 'sides', 'sink', 'size', 'skew', 'source', 
    'strict', 'style', 'subgraph', 'sw', 'taillabel', 'tailport', 'tailurl', 
    'toplabel', 'trapezium', 'triangle', 'tripleoctagon', 'true', 'url', 'w', 
    'weight', 'when', 'width', 'z' 
  );

  KeyIndices: array[0..786] of TSynNativeInt = (
    -1, -1, -1, -1, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 141, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, 50, -1, -1, -1, -1, -1, 
    -1, -1, -1, 40, -1, -1, -1, -1, 4, -1, -1, -1, -1, 90, -1, 3, -1, 110, 86, 
    -1, -1, 49, 23, -1, 92, -1, -1, -1, 15, -1, 122, -1, -1, 28, -1, 78, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 85, -1, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, 140, -1, 103, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 142, -1, 7, -1, 0, 
    -1, -1, 97, -1, -1, -1, -1, -1, 43, -1, -1, -1, 131, -1, -1, -1, 5, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 101, -1, 10, -1, 
    47, 68, -1, 132, -1, -1, 52, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, 
    -1, -1, 64, -1, -1, 124, -1, -1, -1, -1, -1, -1, 87, -1, -1, -1, 12, -1, 84, 
    -1, -1, -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 42, -1, 38, -1, -1, -1, 143, -1, -1, -1, 145, 
    106, -1, 127, -1, -1, -1, 99, 75, -1, -1, 102, -1, 58, -1, -1, 56, -1, -1, 
    -1, -1, 9, -1, -1, -1, -1, -1, 22, -1, 73, -1, -1, -1, 17, -1, 54, 112, -1, 
    -1, -1, -1, -1, -1, -1, 113, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 96, -1, 
    21, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 69, 116, -1, -1, 32, -1, 
    -1, -1, -1, -1, -1, -1, 16, -1, -1, -1, -1, -1, 126, -1, -1, -1, -1, -1, -1, 
    -1, 71, -1, -1, -1, -1, -1, -1, -1, -1, -1, 137, -1, -1, 117, -1, -1, -1, 
    -1, -1, -1, -1, -1, 111, 93, -1, -1, -1, -1, 108, -1, -1, 119, -1, -1, -1, 
    -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, 89, -1, -1, -1, -1, 76, -1, -1, -1, 
    -1, -1, -1, -1, 77, -1, -1, 104, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 26, -1, -1, -1, 79, -1, 19, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, 39, -1, -1, -1, 115, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 109, 35, -1, -1, 70, -1, -1, 57, -1, 72, -1, 
    -1, 83, -1, -1, -1, -1, 130, -1, -1, -1, 18, -1, 118, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, 81, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 61, 
    37, 1, -1, -1, -1, -1, 138, -1, -1, -1, -1, -1, 129, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, 14, -1, -1, 8, -1, -1, -1, 125, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, 91, -1, -1, -1, -1, -1, 60, -1, -1, -1, -1, -1, -1, -1, 
    95, -1, -1, -1, -1, 136, -1, -1, 20, -1, 62, -1, -1, -1, -1, 134, -1, -1, 
    -1, 63, -1, -1, -1, 121, 80, -1, -1, -1, -1, -1, -1, 135, -1, -1, 120, -1, 
    -1, -1, 53, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 31, -1, -1, -1, -1, -1, 
    -1, 24, -1, -1, 139, 67, -1, -1, 59, -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, 
    -1, 128, 34, -1, -1, -1, -1, -1, -1, -1, -1, 65, -1, 114, -1, -1, -1, -1, 
    -1, -1, -1, 55, -1, -1, 94, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 41, -1, -1, -1, -1, -1, -1, -1, 44, -1, 
    -1, -1, -1, -1, 74, -1, 51, 144, -1, -1, 82, 98, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, 100, 66, -1, 25, -1, -1, -1, 45, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, 
    6, 105, -1, -1, 133, 123, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, 107, -1, -1, -1, -1, -1, -1, -1, -1, -1, 30, -1, -1, -1 
  );

{$Q-}
function TSynDOTSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 63 + Ord(Str^) * 331;
    Inc(Str);
  end;
  Result := Result mod 787;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynDOTSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynDOTSyn.InitIdent;
var
  i: TSynNativeInt;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[132] := FuncAll;
  fIdentFuncTable[509] := FuncAppendix;
  fIdentFuncTable[188] := FuncArrowhead;
  fIdentFuncTable[72] := FuncArrowsize;
  fIdentFuncTable[65] := FuncArrowtail;
  fIdentFuncTable[149] := FuncAuto;
  fIdentFuncTable[752] := FuncBack;
  fIdentFuncTable[130] := FuncBgcolor;
  fIdentFuncTable[536] := FuncBold;
  fIdentFuncTable[266] := FuncBoth;
  fIdentFuncTable[169] := FuncBottomlabel;
  fIdentFuncTable[4] := FuncBox;
  fIdentFuncTable[206] := FuncCenter;
  fIdentFuncTable[666] := FuncCircle;
  fIdentFuncTable[533] := FuncClusterrank;
  fIdentFuncTable[85] := FuncColor;
  fIdentFuncTable[327] := FuncComment;
  fIdentFuncTable[278] := FuncCompound;
  fIdentFuncTable[481] := FuncConcentrate;
  fIdentFuncTable[425] := FuncConstraint;
  fIdentFuncTable[573] := FuncDecorate;
  fIdentFuncTable[302] := FuncDiamond;
  fIdentFuncTable[272] := FuncDigraph;
  fIdentFuncTable[79] := FuncDir;
  fIdentFuncTable[621] := FuncDistortion;
  fIdentFuncTable[726] := FuncDot;
  fIdentFuncTable[419] := FuncDotted;
  fIdentFuncTable[104] := FuncDoublecircle;
  fIdentFuncTable[90] := FuncDoubleoctagon;
  fIdentFuncTable[377] := FuncE;
  fIdentFuncTable[783] := FuncEdge;
  fIdentFuncTable[614] := FuncEgg;
  fIdentFuncTable[319] := FuncEllipse;
  fIdentFuncTable[409] := FuncFalse;
  fIdentFuncTable[641] := FuncFill;
  fIdentFuncTable[461] := FuncFillcolor;
  fIdentFuncTable[631] := FuncFilled;
  fIdentFuncTable[508] := FuncFixedsize;
  fIdentFuncTable[237] := FuncFontcolor;
  fIdentFuncTable[435] := FuncFontname;
  fIdentFuncTable[60] := FuncFontpath;
  fIdentFuncTable[685] := FuncFontsize;
  fIdentFuncTable[235] := FuncForward;
  fIdentFuncTable[141] := FuncGlobal;
  fIdentFuncTable[693] := FuncGraph;
  fIdentFuncTable[730] := FuncGroup;
  fIdentFuncTable[212] := FuncHeadlabel;
  fIdentFuncTable[171] := FuncHeadport;
  fIdentFuncTable[749] := FuncHeadurl;
  fIdentFuncTable[78] := FuncHeight;
  fIdentFuncTable[51] := FuncHexagon;
  fIdentFuncTable[701] := FuncHouse;
  fIdentFuncTable[177] := FuncId;
  fIdentFuncTable[603] := FuncInv;
  fIdentFuncTable[280] := FuncInvdot;
  fIdentFuncTable[660] := FuncInvhouse;
  fIdentFuncTable[261] := FuncInvodot;
  fIdentFuncTable[467] := FuncInvtrapezium;
  fIdentFuncTable[258] := FuncInvtriangle;
  fIdentFuncTable[628] := FuncLabel;
  fIdentFuncTable[557] := FuncLabelangle;
  fIdentFuncTable[507] := FuncLabeldistance;
  fIdentFuncTable[575] := FuncLabelfloat;
  fIdentFuncTable[584] := FuncLabelfontcolor;
  fIdentFuncTable[192] := FuncLabelfontname;
  fIdentFuncTable[650] := FuncLabelfontsize;
  fIdentFuncTable[724] := FuncLabeljust;
  fIdentFuncTable[625] := FuncLabelloc;
  fIdentFuncTable[172] := FuncLayer;
  fIdentFuncTable[315] := FuncLayers;
  fIdentFuncTable[464] := FuncLhead;
  fIdentFuncTable[341] := FuncLtail;
  fIdentFuncTable[469] := FuncMargin;
  fIdentFuncTable[274] := FuncMax;
  fIdentFuncTable[699] := FuncMcircle;
  fIdentFuncTable[253] := FuncMclimit;
  fIdentFuncTable[391] := FuncMdiamond;
  fIdentFuncTable[399] := FuncMerged;
  fIdentFuncTable[92] := FuncMin;
  fIdentFuncTable[423] := FuncMinimum;
  fIdentFuncTable[589] := FuncMinlen;
  fIdentFuncTable[493] := FuncMrecord;
  fIdentFuncTable[705] := FuncMsquare;
  fIdentFuncTable[472] := FuncMultiples;
  fIdentFuncTable[208] := FuncN;
  fIdentFuncTable[102] := FuncNe;
  fIdentFuncTable[75] := FuncNode;
  fIdentFuncTable[202] := FuncNodesep;
  fIdentFuncTable[50] := FuncNone;
  fIdentFuncTable[386] := FuncNormal;
  fIdentFuncTable[70] := FuncNslimit;
  fIdentFuncTable[551] := FuncNw;
  fIdentFuncTable[81] := FuncOctagon;
  fIdentFuncTable[364] := FuncOdot;
  fIdentFuncTable[663] := FuncOnto;
  fIdentFuncTable[565] := FuncOrdering;
  fIdentFuncTable[300] := FuncOrientation;
  fIdentFuncTable[135] := FuncPage;
  fIdentFuncTable[706] := FuncPagedir;
  fIdentFuncTable[252] := FuncParallelogram;
  fIdentFuncTable[723] := FuncPeripheries;
  fIdentFuncTable[167] := FuncPlaintext;
  fIdentFuncTable[256] := FuncPoint;
  fIdentFuncTable[117] := FuncPolygon;
  fIdentFuncTable[402] := FuncQuantum;
  fIdentFuncTable[753] := FuncRank;
  fIdentFuncTable[246] := FuncRankdir;
  fIdentFuncTable[773] := FuncRanksep;
  fIdentFuncTable[369] := FuncRatio;
  fIdentFuncTable[460] := FuncRecord;
  fIdentFuncTable[74] := FuncRegular;
  fIdentFuncTable[363] := FuncRemincross;
  fIdentFuncTable[281] := FuncRotate;
  fIdentFuncTable[289] := FuncS;
  fIdentFuncTable[652] := FuncSame;
  fIdentFuncTable[439] := FuncSamehead;
  fIdentFuncTable[316] := FuncSametail;
  fIdentFuncTable[354] := FuncSamplepoints;
  fIdentFuncTable[483] := FuncSe;
  fIdentFuncTable[372] := FuncSearchsize;
  fIdentFuncTable[599] := FuncSection;
  fIdentFuncTable[588] := FuncShape;
  fIdentFuncTable[87] := FuncShapefile;
  fIdentFuncTable[757] := FuncSides;
  fIdentFuncTable[195] := FuncSink;
  fIdentFuncTable[540] := FuncSize;
  fIdentFuncTable[333] := FuncSkew;
  fIdentFuncTable[248] := FuncSource;
  fIdentFuncTable[640] := FuncStrict;
  fIdentFuncTable[520] := FuncStyle;
  fIdentFuncTable[477] := FuncSubgraph;
  fIdentFuncTable[145] := FuncSw;
  fIdentFuncTable[174] := FuncTaillabel;
  fIdentFuncTable[756] := FuncTailport;
  fIdentFuncTable[580] := FuncTailurl;
  fIdentFuncTable[596] := FuncToplabel;
  fIdentFuncTable[570] := FuncTrapezium;
  fIdentFuncTable[351] := FuncTriangle;
  fIdentFuncTable[514] := FuncTripleoctagon;
  fIdentFuncTable[624] := FuncTrue;
  fIdentFuncTable[115] := FuncUrl;
  fIdentFuncTable[39] := FuncW;
  fIdentFuncTable[128] := FuncWeight;
  fIdentFuncTable[241] := FuncWhen;
  fIdentFuncTable[702] := FuncWidth;
  fIdentFuncTable[245] := FuncZ;
end;

function TSynDOTSyn.AltFunc(Index: TSynNativeInt): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynDOTSyn.FuncAll(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncAppendix(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncArrowhead(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncArrowsize(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncArrowtail(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncAuto(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBack(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBgcolor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBold(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBoth(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBottomlabel(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncBox(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncCenter(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncCircle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncClusterrank(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncColor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncComment(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncCompound(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncConcentrate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncConstraint(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDecorate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDiamond(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDigraph(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDir(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDistortion(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDot(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDotted(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDoublecircle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncDoubleoctagon(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncE(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncEdge(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncEgg(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncEllipse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFalse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFill(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFillcolor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFilled(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue  // TODO: ANSI source isn't clear if tkValue or tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFixedsize(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontcolor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontname(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontpath(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncFontsize(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncForward(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncGlobal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncGraph(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncGroup(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeadlabel(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeadport(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeadurl(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHeight(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHexagon(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncHouse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncId(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInv(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvdot(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvhouse(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvodot(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvtrapezium(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncInvtriangle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabel(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelangle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabeldistance(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfloat(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfontcolor(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfontname(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelfontsize(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabeljust(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLabelloc(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLayer(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute  // TODO: ANSI source isn't clear if tkAttribute or tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLayers(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute  // TODO: ANSI source isn't clear if tkAttribute or tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLhead(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncLtail(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMargin(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMax(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMcircle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMclimit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMdiamond(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMerged(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMin(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMinimum(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMinlen(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMrecord(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMsquare(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncMultiples(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncN(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNe(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNode(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNodesep(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNone(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNormal(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNslimit(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncNw(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOctagon(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOdot(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkArrowHead
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOnto(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOrdering(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncOrientation(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPage(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPagedir(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncParallelogram(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPeripheries(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPlaintext(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPoint(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncPolygon(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncQuantum(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRank(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRankdir(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRanksep(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRatio(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRecord(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRegular(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRemincross(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncRotate(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncS(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSame(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSamehead(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSametail(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSamplepoints(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSe(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSearchsize(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSection(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncShape(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncShapefile(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSides(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSink(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSize(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSkew(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSource(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncStrict(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncStyle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSubgraph(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncSw(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTaillabel(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTailport(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTailurl(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncToplabel(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTrapezium(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTriangle(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTripleoctagon(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkShape
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncTrue(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncUrl(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncW(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkValue
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncWeight(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncWhen(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncWidth(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

function TSynDOTSyn.FuncZ(Index: TSynNativeInt): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkAttribute
  else
    Result := tkIdentifier;
end;

procedure TSynDOTSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynDOTSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynDOTSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynDOTSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynDOTSyn.DirectionsProc;
begin
  Inc(Run);
  if (fLine[Run] = '>') or (fLine[Run] = '-') then
  begin
    fTokenID := tkDirections;
    Inc(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynDOTSyn.CStyleCommentOpenProc;
begin
  Inc(Run);
  if fLine[Run] = '/' then
  begin
    fTokenID := tkComment;
    Inc(Run, 2);
    while not IsLineEnd(Run) do Inc(Run);
    Exit;
  end;
  if fLine[Run] = '*' then
  begin
    fRange := rsCStyleComment;
    CStyleCommentProc;
    fTokenID := tkComment;
  end
  else
    fTokenID := tkIdentifier;
end;

procedure TSynDOTSyn.CStyleCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
        begin
          Inc(Run, 2);
          fRange := rsUnKnown;
          Break;
        end;
        if not IsLineEnd(Run) then
          Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynDOTSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynDOTSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if fLine[Run] = '''' then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

constructor TSynDOTSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fArrowHeadAttri := TSynHighLighterAttributes.Create(SYNS_AttrArrowHead, SYNS_FriendlyAttrArrowHead);
  fArrowHeadAttri.Foreground := clRed;
  AddAttribute(fArrowHeadAttri);

  fAttributeAttri := TSynHighLighterAttributes.Create(SYNS_AttrAttribute, SYNS_FriendlyAttrAttribute);
  AddAttribute(fAttributeAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fDirectionsAttri := TSynHighLighterAttributes.Create(SYNS_AttrDirections, SYNS_FriendlyAttrDirections);
  fDirectionsAttri.Style := [fsBold];
  fDirectionsAttri.Foreground := clYellow;
  AddAttribute(fDirectionsAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fShapeAttri := TSynHighLighterAttributes.Create(SYNS_AttrShape, SYNS_FriendlyAttrShape);
  fShapeAttri.Style := [fsBold];
  fShapeAttri.Foreground := clRed;
  AddAttribute(fShapeAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);

  fValueAttri := TSynHighLighterAttributes.Create(SYNS_AttrValue, SYNS_FriendlyAttrValue);
  fValueAttri.Style := [fsItalic];
  fValueAttri.Foreground := clRed;
  AddAttribute(fValueAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsBold];
  fSymbolAttri.Foreground := clGreen;
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterDOT;
  fRange := rsUnknown;
end;

procedure TSynDOTSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);
end;

procedure TSynDOTSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynDOTSyn.SymbolProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynDOTSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsCStyleComment: CStyleCommentProc;
  else
    begin
      fRange := rsUnknown;
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        '/': CStyleCommentOpenProc;
        '-': DirectionsProc;
        '''': StringOpenProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '~', '{', '}', ',', '(', ')', '[', ']', '<', '>', ':', '?', ';', '!', '=': SymbolProc;
        else UnknownProc;
      end;
    end;
  end;
  inherited;
end;

function TSynDOTSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDOTSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynDOTSyn.GetKeyWords(TokenKind: TSynNativeInt): string;
begin
  Result :=
    '--,->,all,appendix,arrowhead,arrowsize,arrowtail,auto,back,bgcolor,bo' +
    'ld,both,bottomlabel,box,center,circle,clusterrank,color,comment,compou' +
    'nd,concentrate,constraint,decorate,diamond,digraph,dir,distortion,dot,' +
    'dotted,doublecircle,doubleoctagon,e,edge,egg,ellipse,false,fill,fillco' +
    'lor,filled,fixedsize,fontcolor,fontname,fontpath,fontsize,forward,glob' +
    'al,graph,group,headlabel,headport,headURL,height,hexagon,house,id,inv,' +
    'invdot,invhouse,invodot,invtrapezium,invtriangle,label,labelangle,labe' +
    'ldistance,labelfloat,labelfontcolor,labelfontname,labelfontsize,labelj' +
    'ust,labelloc,layer,layers,lhead,ltail,margin,max,mcircle,mclimit,mdiam' +
    'ond,merged,min,minimum,minlen,mrecord,msquare,multiples,n,ne,node,node' +
    'sep,none,normal,nslimit,nw,octagon,odot,onto,ordering,orientation,page' +
    ',pagedir,parallelogram,peripheries,plaintext,point,polygon,quantum,ran' +
    'k,rankdir,ranksep,ratio,record,regular,remincross,rotate,s,same,samehe' +
    'ad,sametail,samplepoints,se,searchsize,section,shape,shapefile,sides,s' +
    'ink,size,skew,source,strict,style,subgraph,sw,taillabel,tailport,tailU' +
    'RL,toplabel,trapezium,triangle,tripleoctagon,true,url,w,weight,when,wi' +
    'dth,z';
end;

function TSynDOTSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynDOTSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkArrowHead: Result := fArrowHeadAttri;
    tkAttribute: Result := fAttributeAttri;
    tkComment: Result := fCommentAttri;
    tkDirections: Result := fDirectionsAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkShape: Result := fShapeAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkValue: Result := fValueAttri;
    tkUnknown: Result := fIdentifierAttri;
    tkSymbol: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDOTSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(fTokenId);
end;

function TSynDOTSyn.GetSampleSource: string;
begin
  Result :=
    '// ATT DOT Graphic description language'#13#10 +
    'digraph asde91 {'#13#10 +
    '  ranksep=.75; size = "7.5,7.5";'#13#10 +
    '  {'#13#10 +
    '      node [shape=plaintext, fontsize=16];'#13#10 +
    '      /* the time-line graph */'#13#10 +
    '      past -> 1978 -> 1980 -> 1982 -> 1983 -> 1985 -> 1986 ->'#13#10 +
    '      1987 -> 1988 -> 1989 -> 1990 -> "future";'#13#10 +
    '      /* ancestor programs */'#13#10 +
    '      "Bourne sh"; "make"; "SCCS"; "yacc"; "cron"; "Reiser cpp";'#13#10 +
    '      "Cshell"; "emacs"; "build"; "vi"; "<curses>"; "RCS"; "C*";'#13#10 +
    '  }'#13#10 +
    '      { rank = same;'#13#10 +
    '      "Software IS"; "Configuration Mgt"; "Architecture & Libraries";'#13#10 +
    '      "Process";'#13#10 +
    '  };'#13#10 +
    '    node [shape=box];'#13#10 +
    '    { rank = same; "past"; "SCCS"; "make"; "Bourne sh"; "yacc"; "cron"; }'#13#10 +
    '    { rank = same; 1978; "Reiser cpp"; "Cshell"; }'#13#10 +
    '    { rank = same; 1980; "build"; "emacs"; "vi"; }'#13#10 +
    '    { rank = same; 1982; "RCS"; "<curses>"; "IMX"; "SYNED"; }'#13#10 +
    '    { rank = same; 1983; "ksh"; "IFS"; "TTU"; }'#13#10 +
    '    { rank = same; 1985; "nmake"; "Peggy"; }'#13#10 +
    '    { rank = same; 1986; "C*"; "ncpp"; "ksh-i"; "<curses-i>"; "PG2"; }'#13#10 +
    '    { rank = same; 1987; "Ansi cpp"; "nmake 2.0"; "3D File System"; "fdelta";'#13#10 +
    '        "DAG"; "CSAS";}'#13#10 +
    '    { rank = same; 1988; "CIA"; "SBCS"; "ksh-88"; "PEGASUS/PML"; "PAX";'#13#10 +
    '        "backtalk"; }'#13#10 +
    '    { rank = same; 1989; "CIA++"; "APP"; "SHIP"; "DataShare"; "ryacc";'#13#10 +
    '        "Mosaic"; }'#13#10 +
    '    { rank = same; 1990; "libft"; "CoShell"; "DIA"; "IFS-i"; "kyacc"; "sfio";'#13#10 +
    '        "yeast"; "ML-X"; "DOT"; }'#13#10 +
    '    { rank = same; "future"; "Adv. Software Technology"; }'#13#10 +
    '    "PEGASUS/PML" -> "ML-X";'#13#10 +
    '    "SCCS" -> "nmake";'#13#10 +
    '    "SCCS" -> "3D File System";'#13#10 +
    '    "SCCS" -> "RCS";'#13#10 +
    '    "make" -> "nmake";'#13#10 +
    '    "make" -> "build";'#13#10 +
    '}';
end;

function TSynDOTSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterDOT;
end;

function TSynDOTSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

class function TSynDOTSyn.GetLanguageName: string;
begin
  Result := SYNS_LangDOT;
end;

procedure TSynDOTSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynDOTSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynDOTSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

class function TSynDOTSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangDOT;
end;

initialization
  RegisterPlaceableHighlighter(TSynDOTSyn);
end.
