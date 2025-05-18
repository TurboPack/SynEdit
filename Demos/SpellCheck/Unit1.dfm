object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 599
  ClientWidth = 806
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object SynEdit: TSynEdit
    Left = 0
    Top = 0
    Width = 806
    Height = 599
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    PopupMenu = pmnuEditor
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Width = 3
      end>
    Highlighter = CommandsDataModule.SynPasSyn1
    Lines.Strings = (
      'unit VirtualTrees;'
      ''
      
        '// The contents of this file are subject to the Mozilla Public L' +
        'icense'
      
        '// Version 1.1 (the "License"); you may not use this file except' +
        ' in compliance'
      
        '// with the License. You may obtain a copy of the License at htt' +
        'p://www.mozilla.org/MPL/'
      '//'
      
        '// Alternatively, you may redistribute this library, use and/or ' +
        'modify it under the terms of the'
      
        '// GNU Lesser General Public License as published by the Free So' +
        'ftware Foundation;'
      
        '// either version 2.1 of the License, or (at your option) any la' +
        'ter version.'
      
        '// You may obtain a copy of the LGPL at http://www.gnu.org/copyl' +
        'eft/.'
      '//'
      
        '// Software distributed under the License is distributed on an "' +
        'AS IS" basis,'
      
        '// WITHOUT WARRANTY OF ANY KIND, either express or implied. See ' +
        'the License for the'
      
        '// specific language governing rights and limitations under the ' +
        'License.'
      '//'
      
        '// The original code is VirtualTrees.pas, released September 30,' +
        ' 2000.'
      '//'
      
        '// The initial developer of the original code is digital publish' +
        'ing AG (Munich, Germany, www.digitalpublishing.de),'
      
        '// most code was written by Mike Lischke 2000-2009 (public@soft-' +
        'gems.net, www.soft-gems.net)'
      '//'
      '// Portions created by digital publishing AG are Copyright'
      '// (C) 1999-2001 digital publishing AG. All Rights Reserved.'
      
        '//--------------------------------------------------------------' +
        '--------------------------------------------------------'
      '//'
      '// For a list of recent changes please see file CHANGES.TXT'
      '//'
      
        '// Credits for their valuable assistance and code donations go t' +
        'o:'
      
        '//   Freddy Ertl, Marian Aldenhoevel, Thomas Bogenrieder, Jim Ku' +
        'enemann, Werner Lehmann, Jens Treichler,'
      
        '//   Paul Gallagher (IBO tree), Ondrej Kelle, Ronaldo Melo Ferra' +
        'z, Heri Bender, Roland Beduerftig (BCB)'
      
        '//   Anthony Mills, Alexander Egorushkin (BCB), Mathias Torell (' +
        'BCB), Frank van den Bergh, Vadim Sedulin, Peter Evans,'
      
        '//   Milan Vandrovec (BCB), Steve Moss, Joe White, David Clark, ' +
        'Anders Thomsen, Igor Afanasyev, Eugene Programmer,'
      
        '//   Corbin Dunn, Richard Pringle, Uli Gerhardt, Azza, Igor Savk' +
        'ic, Daniel Bauten, Timo Tegtmeier, Dmitry Zegebart,'
      
        '//   Andreas Hausladen, Joachim Marder, Roman Kassebaum, Vincent' +
        ' Parrett, Dietmar Roesler, Sanjay Kanade,'
      
        '//   and everyone that sent pull requests: https://github.com/Vi' +
        'rtual-TreeView/Virtual-TreeView/pulls?q='
      '// Beta testers:'
      
        '//   Freddy Ertl, Hans-Juergen Schnorrenberg, Werner Lehmann, Ji' +
        'm Kueneman, Vadim Sedulin, Moritz Franckenstein,'
      '//   Wim van der Vegt, Franc v/d Westelaken'
      
        '// Indirect contribution (via publicly accessible work of those ' +
        'persons):'
      '//   Alex Denissov, Hiroyuki Hori (MMXAsm expert)'
      '// Documentation:'
      
        '//   Markus Spoettl and toolsfactory GbR (http://www.doc-o-matic' +
        '.com/, sponsoring Virtual TreeView development'
      
        '//   with a free copy of the Doc-O-Matic help authoring system),' +
        ' Sven H. (Step by step tutorial)'
      '// Source repository:'
      '//   https://github.com/Virtual-TreeView/Virtual-TreeView'
      '// Accessability implementation:'
      '//   Marco Zehe (with help from Sebastian Modersohn)'
      '// Port to Firemonkey:'
      '//   Karol Bieniaszewski (github user livius2)'
      
        '//--------------------------------------------------------------' +
        '--------------------------------------------------------'
      ''
      'interface'
      ''
      
        '{$if CompilerVersion < 24}{$MESSAGE FATAL '#39'This version supports' +
        ' only RAD Studio XE3 and higher. Please use V5 from  http://www.' +
        'jam-software.com/virtual-treeview/VirtualTreeViewV5.5.3.zip  or ' +
        ' https://github.com/Virtual-TreeView/Virtual-TreeView/archive/V5' +
        '_stable.zip'#39'}{$ifend}'
      ''
      '{$booleval off} // Use fastest possible boolean evaluation'
      ''
      
        '// For some things to work we need code, which is classified as ' +
        'being unsafe for .NET.'
      '{$WARN UNSAFE_TYPE OFF}'
      '{$WARN UNSAFE_CAST OFF}'
      '{$WARN UNSAFE_CODE OFF}'
      ''
      '{$LEGACYIFEND ON}'
      '{$WARN UNSUPPORTED_CONSTRUCT      OFF}'
      ''
      '{$HPPEMIT '#39'#include <objidl.h>'#39'}'
      '{$HPPEMIT '#39'#include <oleidl.h>'#39'}'
      '{$HPPEMIT '#39'#include <oleacc.h>'#39'}'
      '{$ifdef BCB}'
      '  {$HPPEMIT '#39'#pragma comment(lib, "VirtualTreesCR")'#39'}'
      '{$else}'
      '  {$HPPEMIT '#39'#pragma comment(lib, "VirtualTreesR")'#39'}'
      '{$endif}'
      '{$HPPEMIT '#39'#pragma comment(lib, "Shell32")'#39'}'
      '{$HPPEMIT '#39'#pragma comment(lib, "uxtheme")'#39'}'
      '{$HPPEMIT '#39'#pragma link "VirtualTrees.Accessibility"'#39'}'
      ''
      'uses'
      
        '  Winapi.Windows, Winapi.oleacc, Winapi.Messages, System.SysUtil' +
        's, Vcl.Graphics,'
      
        '  Vcl.Controls, Vcl.Forms, Vcl.ImgList, Winapi.ActiveX, Vcl.StdC' +
        'trls, System.Classes,'
      
        '  Vcl.Menus, Vcl.Printers, System.Types, Winapi.CommCtrl, Vcl.Th' +
        'emes, Winapi.UxTheme,'
      '  Winapi.ShlObj, System.UITypes, System.Generics.Collections,'
      '  VirtualTrees.Types,'
      '  VirtualTrees.Colors,'
      '  VirtualTrees.DragImage,'
      '  VirtualTrees.Header;'
      ''
      'const'
      '  //Aliases'
      '  NoColumn                 = VirtualTrees.Types.NoColumn;'
      '  InvalidColumn            = VirtualTrees.Types.InvalidColumn;'
      
        '  sdAscending              = VirtualTrees.Types.TSortDirection.s' +
        'dAscending;'
      
        '  sdDescending              = VirtualTrees.Types.TSortDirection.' +
        'sdDescending;'
      ''
      '  ctNone              = VirtualTrees.Types.TCheckType.ctNone;'
      
        '  ctTriStateCheckBox  = VirtualTrees.Types.TCheckType.ctTriState' +
        'CheckBox;'
      
        '  ctCheckBox          = VirtualTrees.Types.TCheckType.ctCheckBox' +
        ';'
      
        '  ctRadioButton       = VirtualTrees.Types.TCheckType.ctRadioBut' +
        'ton;'
      '  ctButton            = VirtualTrees.Types.TCheckType.ctButton;'
      ''
      
        '  csUncheckedNormal   = VirtualTrees.Types.TCheckState.csUncheck' +
        'edNormal;'
      
        '  csUncheckedPressed    = VirtualTrees.Types.TCheckState.csUnche' +
        'ckedPressed;'
      
        '  csCheckedNormal   = VirtualTrees.Types.TCheckState.csCheckedNo' +
        'rmal;'
      
        '  csCheckedPressed    = VirtualTrees.Types.TCheckState.csChecked' +
        'Pressed;'
      
        '  csMixedNormal   = VirtualTrees.Types.TCheckState.csMixedNormal' +
        ';'
      
        '  csMixedPressed    = VirtualTrees.Types.TCheckState.csMixedPres' +
        'sed;'
      
        '  csUncheckedDisabled   = VirtualTrees.Types.TCheckState.csUnche' +
        'ckedDisabled;'
      
        '  csCheckedDisabled   = VirtualTrees.Types.TCheckState.csChecked' +
        'Disabled;'
      
        '  csMixedDisable    = VirtualTrees.Types.TCheckState.csMixedDisa' +
        'bled;'
      ''
      ''
      ''
      'var'
      '  IsWinVistaOrAbove: Boolean;'
      ''
      '  {$MinEnumSize 1, make enumerations as small as possible}'
      ''
      'type'
      '  // Alias defintions for convenience'
      '  TImageIndex = System.UITypes.TImageIndex;'
      '  TCanvas = Vcl.Graphics.TCanvas;'
      ''
      '  //these were moved, aliases are for backwards compatibility.'
      
        '  //some may be removed once we sort out excactly what is needed' +
        '.'
      '  TDimension = VirtualTrees.Types.TDimension;'
      '  TColumnIndex = VirtualTrees.Types.TColumnIndex;'
      '  TColumnPosition = VirtualTrees.Types.TColumnPosition;'
      '  EVirtualTreeError = VirtualTrees.Types.EVirtualTreeError;'
      '  TAutoScrollInterval = VirtualTrees.Types.TAutoScrollInterval;'
      '  TVTScrollIncrement = VirtualTrees.Types.TVTScrollIncrement;'
      '  TFormatArray = VirtualTrees.Types.TFormatArray;'
      '  TFormatEtcArray = VirtualTrees.Types.TFormatEtcArray;'
      ''
      '  TVTPaintOption = VirtualTrees.Types.TVTPaintOption;'
      '  TVTPaintOptions = VirtualTrees.Types.TVTPaintOptions;'
      '  TVTAnimateOption = VirtualTrees.Types.TVTAnimationOption;'
      '  TVTAnimateOptions = VirtualTrees.Types.TVTAnimationOptions;'
      '  TVTAutoOption = VirtualTrees.Types.TVTAutoOption;'
      '  TVTAutoOptions = VirtualTrees.Types.TVTAutoOptions;'
      '  TVTSelectionOption = VirtualTrees.Types.TVTSelectionOption;'
      '  TVTSelectionOptions = VirtualTrees.Types.TVTSelectionOptions;'
      '  TVTEditOptions = VirtualTrees.Types.TVTEditOptions;'
      '  TVTMiscOption = VirtualTrees.Types.TVTMiscOption;'
      '  TVTMiscOptions = VirtualTrees.Types.TVTMiscOptions;'
      '  TVTExportMode  = VirtualTrees.Types.TVTExportMode;'
      '  TVTStringOption = VirtualTrees.Types.TVTStringOption;'
      '  TVTStringOptions = VirtualTrees.Types.TVTStringOptions;'
      
        '  TCustomVirtualTreeOptions = VirtualTrees.Types.TCustomVirtualT' +
        'reeOptions;'
      '  TVirtualTreeOptions = VirtualTrees.Types.TVirtualTreeOptions;'
      '  TTreeOptionsClass = VirtualTrees.Types.TTreeOptionsClass;'
      
        '  TCustomStringTreeOptions = VirtualTrees.Types.TCustomStringTre' +
        'eOptions;'
      '  TStringTreeOptions = VirtualTrees.Types.TStringTreeOptions;'
      ''
      '  TScrollBarStyle = VirtualTrees.Types.TScrollBarStyle;'
      '  TScrollBarOptions = VirtualTrees.Types.TScrollBarOptions;'
      ''
      '  TVTColumnOption = VirtualTrees.Types.TVTColumnOption;'
      '  TVTColumnOptions = VirtualTrees.Types.TVTColumnOptions;'
      
        '  TVirtualTreeColumnStyle = VirtualTrees.Types.TVirtualTreeColum' +
        'nStyle;'
      '  TSortDirection = VirtualTrees.Types.TSortDirection;'
      '  TCheckType = VirtualTrees.Types.TCheckType;'
      '  TCheckState = VirtualTrees.Types.TCheckState;'
      '  TVTDropMarkMode = VirtualTrees.Types.TVTDropMarkMode;'
      '  TScrollDirections = VirtualTrees.Types.TScrollDirections;'
      '  TVirtualTreeColumn = VirtualTrees.Header.TVirtualTreeColumn;'
      '  TVirtualTreeColumns = VirtualTrees.Header.TVirtualTreeColumns;'
      
        '  TVirtualTreeColumnClass = VirtualTrees.Header.TVirtualTreeColu' +
        'mnClass;'
      '  TColumnsArray = VirtualTrees.Header.TColumnsArray;'
      '  TCardinalArray = VirtualTrees.Header.TCardinalArray;'
      '  TIndexArray = VirtualTrees.Header.TIndexArray;'
      ''
      '  TVTHeader = VirtualTrees.Header.TVTHeader;'
      '  TVTHeaderClass = VirtualTrees.Header.TVTHeaderClass;'
      '  TVTHeaderOption = VirtualTrees.Header.TVTHeaderOption;'
      '  TVTHeaderOptions = VirtualTrees.Header.TVTHeaderOptions;'
      '  THeaderPaintInfo = VirtualTrees.Header.THeaderPaintInfo;'
      
        '  TVTHeaderColumnLayout = VirtualTrees.Types.TVTHeaderColumnLayo' +
        'ut;'
      
        '  TVTConstraintPercent = VirtualTrees.Header.TVTConstraintPercen' +
        't;'
      '  TSmartAutoFitType = VirtualTrees.Types.TSmartAutoFitType;'
      
        '  TVTFixedAreaConstraints = VirtualTrees.Header.TVTFixedAreaCons' +
        'traints;'
      '  TVTHeaderStyle = VirtualTrees.Header.TVTHeaderStyle;'
      '  THeaderState = VirtualTrees.Header.THeaderState;'
      '  THeaderStates = VirtualTrees.Header.THeaderStates;'
      ''
      '  TVTColors = VirtualTrees.Colors.TVTColors;'
      '  //'
      ''
      
        '  // Be careful when adding new states as this might change the ' +
        'size of the type which in turn'
      
        '  // changes the alignment in the node record as well as the str' +
        'eam chunks.'
      
        '  // Do not reorder the states and always add new states at the ' +
        'end of this enumeration in order to avoid'
      '  // breaking existing code.'
      '  TVirtualNodeState = ('
      
        '    vsInitialized,       // Set after the node has been initiali' +
        'zed.'
      
        '    vsChecking,          // Node'#39's check state is changing, avoi' +
        'd propagation.'
      
        '    vsCutOrCopy,         // Node is selected as cut or copy and ' +
        'paste source.'
      '    vsDisabled,          // Set if node is disabled.'
      
        '    vsDeleting,          // Set when the node is about to be fre' +
        'ed.'
      '    vsExpanded,          // Set if the node is expanded.'
      
        '    vsHasChildren,       // Indicates the presence of child node' +
        's without actually setting them.'
      
        '    vsVisible,           // Indicate whether the node is visible' +
        ' or not (independant of the expand states of its parents).'
      
        '    vsSelected,          // Set if the node is in the current se' +
        'lection.'
      
        '    vsOnFreeNodeCallRequired,   // Set if user data has been set' +
        ' which requires OnFreeNode.'
      
        '    vsAllChildrenHidden, // Set if vsHasChildren is set and no c' +
        'hild node has the vsVisible flag set.'
      
        '    vsReleaseCallOnUserDataRequired, // Indicates that the user ' +
        'data is a reference to an interface which should be released.'
      
        '    vsMultiline,         // Node text is wrapped at the cell bou' +
        'ndaries instead of being shorted.'
      
        '    vsHeightMeasured,    // Node height has been determined and ' +
        'does not need a recalculation.'
      
        '    vsToggling,          // Set when a node is expanded/collapse' +
        'd to prevent recursive calls.'
      
        '    vsFiltered,          // Indicates that the node should not b' +
        'e painted (without effecting its children).'
      
        '    vsInitializing       // Set when the node is being initializ' +
        'ed'
      '  );'
      '  TVirtualNodeStates = set of TVirtualNodeState;'
      ''
      
        '  // States used in InitNode to indicate states a node shall ini' +
        'tially have.'
      '  TVirtualNodeInitState = ('
      '    ivsDisabled,'
      '    ivsExpanded,'
      '    ivsHasChildren,'
      '    ivsMultiline,'
      '    ivsSelected,'
      '    ivsFiltered,'
      '    ivsReInit'
      '  );'
      '  TVirtualNodeInitStates = set of TVirtualNodeInitState;'
      ''
      ''
      ''
      
        '  // These flags are used to indicate where a click in the heade' +
        'r happened.'
      '  TVTHeaderHitPosition = ('
      
        '    hhiNoWhere,         // No column is involved (possible only ' +
        'if the tree is smaller than the client area).'
      '    hhiOnColumn,        // On a column.'
      
        '    hhiOnIcon,          // On the bitmap associated with a colum' +
        'n.'
      '    hhiOnCheckbox       // On the checkbox if enabled.'
      '  );'
      '  TVTHeaderHitPositions = set of TVTHeaderHitPosition;'
      ''
      '  // These flags are returned by the hit test method.'
      '  THitPosition = ('
      
        '    hiAbove,             // above the client area (if relative) ' +
        'or the absolute tree area'
      
        '    hiBelow,             // below the client area (if relative) ' +
        'or the absolute tree area'
      
        '    hiNowhere,           // no node is involved (possible only i' +
        'f the tree is not as tall as the client area)'
      
        '    hiOnItem,            // on the bitmaps/buttons or label asso' +
        'ciated with an item'
      
        '    hiOnItemButton,      // on the button associated with an ite' +
        'm'
      
        '    hiOnItemButtonExact, // exactly on the button associated wit' +
        'h an item'
      '    hiOnItemCheckbox,    // on the checkbox if enabled'
      
        '    hiOnItemIndent,      // in the indentation area in front of ' +
        'a node'
      
        '    hiOnItemLabel,       // on the normal text area associated w' +
        'ith an item'
      
        '    hiOnItemLeft,        // in the area to the left of a node'#39's ' +
        'text area (e.g. when right aligned or centered)'
      
        '    hiOnItemRight,       // in the area to the right of a node'#39's' +
        ' text area (e.g. if left aligned or centered)'
      '    hiOnNormalIcon,      // on the "normal" image'
      '    hiOnStateIcon,       // on the state image'
      
        '    hiToLeft,            // to the left of the client area (if r' +
        'elative) or the absolute tree area'
      
        '    hiToRight,           // to the right of the client area (if ' +
        'relative) or the absolute tree area'
      '    hiUpperSplitter,     // in the upper splitter area of a node'
      '    hiLowerSplitter      // in the lower splitter area of a node'
      '  );'
      '  THitPositions = set of THitPosition;'
      ''
      ''
      '  TCheckImageKind = ('
      '    ckCustom,         // application defined check images'
      
        '    ckSystemDefault   // Uses the system check images, theme awa' +
        're.'
      '  );'
      ''
      '  // mode to describe a move action'
      '  TVTNodeAttachMode = ('
      
        '    amNoWhere,        // just for simplified tests, means to ign' +
        'ore the Add/Insert command'
      
        '    amInsertBefore,   // insert node just before destination (as' +
        ' sibling of destination)'
      
        '    amInsertAfter,    // insert node just after destionation (as' +
        ' sibling of destination)'
      '    amAddChildFirst,  // add node as first child of destination'
      '    amAddChildLast    // add node as last child of destination'
      '  );'
      ''
      '  // modes to determine drop position further'
      '  TDropMode = ('
      '    dmNowhere,'
      '    dmAbove,'
      '    dmOnNode,'
      '    dmBelow'
      '  );'
      ''
      '  // operations basically allowed during drag'#39'n drop'
      '  TDragOperation = ('
      '    doCopy,'
      '    doMove,'
      '    doLink'
      '  );'
      '  TDragOperations = set of TDragOperation;'
      ''
      '  TVTImageKind = ('
      '    ikNormal,'
      '    ikSelected,'
      '    ikState,'
      '    ikOverlay'
      '  );'
      ''
      '  {'
      '    Fine points: Observed when fixing issue #623'
      
        '    -- hmHint allows multiline hints automatically if provided t' +
        'hrough OnGetHint event.'
      
        '       This is irresptive of whether node itself is multi-line o' +
        'r not.'
      ''
      
        '    -- hmToolTip shows a hint only when node text is not fully s' +
        'hown. It'#39's meant to'
      
        '       fully show node text when not visible. It will show multi' +
        '-line hint only if'
      
        '       the node itself is multi-line. If you provide a custom mu' +
        'lti-line hint then'
      
        '       you must force linebreak style to hlbForceMultiLine in th' +
        'e OnGetHint event'
      '       in order to show the complete hint.'
      '  }'
      '  TVTHintMode = ('
      '    hmDefault,            // show the hint of the control'
      
        '    hmHint,               // show node specific hint string retu' +
        'rned by the application'
      
        '    hmHintAndDefault,     // same as hmHint but show the control' +
        #39's hint if no node is concerned'
      
        '    hmTooltip             // show the text of the node if it isn' +
        #39't already fully shown'
      '  );'
      ''
      '  // Indicates how to format a tooltip.'
      '  TVTTooltipLineBreakStyle = ('
      '    hlbDefault,           // Use multi-line style of the node.'
      '    hlbForceSingleLine,   // Use single line hint.'
      '    hlbForceMultiLine     // Use multi line hint.'
      '  );'
      ''
      '  TMouseButtons = set of TMouseButton;'
      ''
      
        '  // Used to describe the action to do when using the OnBeforeIt' +
        'emErase event.'
      '  TItemEraseAction = ('
      
        '    eaColor,   // Use the provided color to erase the background' +
        ' instead the one of the tree.'
      
        '    eaDefault, // The tree should erase the item'#39's background (b' +
        'itmap or solid).'
      
        '    eaNone     // Do nothing. Let the application paint the back' +
        'ground.'
      '  );'
      ''
      ''
      '  // Kinds of operations'
      '  TVTOperationKind = ('
      '    okAutoFitColumns,'
      '    okGetMaxColumnWidth,'
      '    okSortNode,'
      '    okSortTree,'
      '    okExport,'
      '    okExpand'
      '  );'
      '  TVTOperationKinds = set of TVTOperationKind;'
      ''
      
        '  // content elements of the control from left to right, used wh' +
        'en calculatin left margins.'
      '  TVTElement = ('
      '    ofsMargin, // right of the margin'
      
        '    ofsToggleButton, // the exact x-postition of the toggle butt' +
        'on'
      '    ofsCheckBox,'
      '    ofsStateImage,'
      '    ofsImage,'
      '    ofsLabel, // where drawing a selection begins'
      '    ofsText, // includes TextMargin'
      '    ofsRightOfText, // Includes NodeWidth and ExtraNodeWidth'
      '    ofsEndOfClientArea // The end of the paint area'
      '  );'
      ''
      
        '  /// An array that can be used to calculate the offsets ofthe e' +
        'lements in the tree.'
      '  TVTOffsets = array [TVTElement] of TDimension;'
      ''
      'type'
      '  TBaseVirtualTree = class;'
      '  TVirtualTreeClass = class of TBaseVirtualTree;'
      ''
      '  PVirtualNode = ^TVirtualNode;'
      ''
      
        '  // This record must already be defined here and not later beca' +
        'use otherwise BCB users will not be able'
      '  // to compile (conversion done by BCB is wrong).'
      '  TCacheEntry = record'
      '    Node: PVirtualNode;'
      '    AbsoluteTop: Cardinal;'
      '  end;'
      ''
      '  TCache = array of TCacheEntry;'
      '  TNodeArray = array of PVirtualNode;'
      ''
      ''
      '  // Used in the CF_VTREFERENCE clipboard format.'
      '  PVTReference = ^TVTReference;'
      '  TVTReference = record'
      '    Process: Cardinal;'
      '    Tree: TBaseVirtualTree;'
      '  end;'
      ''
      '  TVirtualNode = packed record'
      
        '    Index,                   // index of node with regard to its' +
        ' parent'
      '    ChildCount: Cardinal;    // number of child nodes'
      '    NodeHeight: Word;        // height in pixels'
      
        '    States: TVirtualNodeStates; // states describing various pro' +
        'perties of the node (expanded, initialized etc.)'
      '    Align: Byte;             // line/button alignment'
      
        '    CheckState: TCheckState; // indicates the current check stat' +
        'e (e.g. checked, pressed etc.)'
      
        '    CheckType: TCheckType;   // indicates which check type shall' +
        ' be used for this node'
      
        '    Dummy: Byte;             // dummy value to fill DWORD bounda' +
        'ry       TODO: Is this still necessary?'
      
        '    TotalCount,              // sum of this node, all of its chi' +
        'ld nodes and their child nodes etc.'
      
        '    TotalHeight: Cardinal;   // height in pixels this node cover' +
        's on screen including the height of all of its'
      '                             // children'
      
        '    // Note: Some copy routines require that all pointers (as we' +
        'll as the data area) in a node are'
      
        '    //       located at the end of the node! Hence if you want t' +
        'o add new member fields (except pointers to internal'
      '    //       data) then put them before field Parent.'
      
        '    Parent,                  // reference to the node'#39's parent (' +
        'for the root this contains the treeview)'
      
        '    PrevSibling,             // link to the node'#39's previous sibl' +
        'ing or nil if it is the first node'
      
        '    NextSibling,             // link to the node'#39's next sibling ' +
        'or nil if it is the last node'
      
        '    FirstChild,              // link to the node'#39's first child..' +
        '.'
      '    LastChild: PVirtualNode; // link to the node'#39's last child...'
      '  private'
      
        '    Data: record end;        // this is a placeholder, each node' +
        ' gets extra data determined by NodeDataSize'
      '  public'
      '    function IsAssigned(): Boolean; inline;'
      '    function GetData(): Pointer; overload; inline;'
      '    function GetData<T>(): T; overload; inline;'
      '    procedure SetData(pUserData: Pointer); overload;'
      '    procedure SetData<T>(pUserData: T); overload;'
      '    procedure SetData(const pUserData: IInterface); overload;'
      '  end;'
      ''
      ''
      
        '  // Structure used when info about a certain position in the he' +
        'ader is needed.'
      '  TVTHeaderHitInfo = record'
      '    X,'
      '    Y: TDimension;'
      '    Button: TMouseButton;'
      '    Shift: TShiftState;'
      '    Column: TColumnIndex;'
      '    HitPosition: TVTHeaderHitPositions;'
      '  end;'
      ''
      
        '  // Structure used when info about a certain position in the tr' +
        'ee is needed.'
      '  THitInfo = record'
      '    HitNode: PVirtualNode;'
      '    HitPositions: THitPositions;'
      '    HitColumn: TColumnIndex;'
      '    HitPoint: TPoint;'
      '  end;'
      ''
      ''
      ''
      '  // ----- OLE drag'#39'n drop handling'
      ''
      '  IVTDragManager = interface(IUnknown)'
      '    ['#39'{C4B25559-14DA-446B-8901-0C879000EB16}'#39']'
      '    procedure ForceDragLeave; stdcall;'
      '    function GetDataObject: IDataObject; stdcall;'
      '    function GetDragSource: TBaseVirtualTree; stdcall;'
      '    function GetDropTargetHelperSupported: Boolean; stdcall;'
      '    function GetIsDropTarget: Boolean; stdcall;'
      ''
      '    property DataObject: IDataObject read GetDataObject;'
      '    property DragSource: TBaseVirtualTree read GetDragSource;'
      
        '    property DropTargetHelperSupported: Boolean read GetDropTarg' +
        'etHelperSupported;'
      '    property IsDropTarget: Boolean read GetIsDropTarget;'
      '  end;'
      ''
      ''
      ''
      '  PVTHintData = ^TVTHintData;'
      '  TVTHintData = record'
      '    Tree: TBaseVirtualTree;'
      '    Node: PVirtualNode;'
      '    Column: TColumnIndex;'
      
        '    HintRect: TRect;            // used for draw trees only, str' +
        'ing trees get the size from the hint string'
      
        '    HintText: string;    // set when size of the hint window is ' +
        'calculated'
      '    BidiMode: TBidiMode;'
      '    Alignment: TAlignment;'
      '    LineBreakStyle: TVTToolTipLineBreakStyle;'
      '  end;'
      ''
      
        '  // The trees need an own hint window class because of Unicode ' +
        'output and adjusted font.'
      '  TVirtualTreeHintWindow = class(THintWindow)'
      '  strict private'
      '    FHintData: TVTHintData;'
      '    FTextHeight: TDimension;'
      
        '    procedure CMTextChanged(var Message: TMessage); message CM_T' +
        'EXTCHANGED;'
      
        '    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message ' +
        'WM_ERASEBKGND;'
      '  strict protected'
      '    procedure CreateParams(var Params: TCreateParams); override;'
      '    procedure Paint; override;'
      
        '    // Mitigator function to use the correct style service for t' +
        'his context (either the style assigned to the control for Delphi' +
        ' > 10.4 or the application style)'
      
        '    function StyleServices(AControl: TControl = nil): TCustomSty' +
        'leServices;'
      '  public'
      
        '    function CalcHintRect(MaxWidth: TDimension; const AHint: str' +
        'ing; AData: Pointer): TRect; override;'
      '    function IsHintMsg(var Msg: TMsg): Boolean; override;'
      '  end;'
      ''
      '  TChangeReason = ('
      '    crIgnore,       // used as placeholder'
      '    crAccumulated,  // used for delayed changes'
      '    crChildAdded,   // one or more child nodes have been added'
      '    crChildDeleted, // one or more child nodes have been deleted'
      '    crNodeAdded,    // a node has been added'
      '    crNodeCopied,   // a node has been duplicated'
      '    crNodeMoved     // a node has been moved to a new place'
      '  ); // desribes what made a structure change event happen'
      ''
      
        '  // Communication interface between a tree editor and the tree ' +
        'itself (declared as using stdcall in case it'
      
        '  // is implemented in a (C/C++) DLL). The GUID is not nessecary' +
        ' in Delphi but important for BCB users'
      '  // to allow QueryInterface and _uuidof calls.'
      '  IVTEditLink = interface'
      '    ['#39'{2BE3EAFA-5ACB-45B4-9D9A-B58BCC496E17}'#39']'
      
        '    function BeginEdit: Boolean; stdcall;                  // Ca' +
        'lled when editing actually starts.'
      
        '    function CancelEdit: Boolean; stdcall;                 // Ca' +
        'lled when editing has been cancelled by the tree.'
      
        '    function EndEdit: Boolean; stdcall;                     // C' +
        'alled when editing has been finished by the tree. Returns True i' +
        'f successful, False if edit mode is still active.'
      
        '    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualN' +
        'ode; Column: TColumnIndex): Boolean; stdcall;'
      
        '                                                           // Ca' +
        'lled after creation to allow a setup.'
      '    procedure ProcessMessage(var Message: TMessage); stdcall;'
      
        '                                                           // Us' +
        'ed to forward messages to the edit window(s)-'
      
        '    procedure SetBounds(R: TRect); stdcall;                // Ca' +
        'lled to place the editor.'
      '  end;'
      ''
      
        '  // Indicates in the OnUpdating event what state the tree is cu' +
        'rrently in.'
      '  TVTUpdateState = ('
      
        '    usBegin,       // The tree just entered the update state (Be' +
        'ginUpdate call for the first time).'
      
        '    usBeginSynch,  // The tree just entered the synch update sta' +
        'te (BeginSynch call for the first time).'
      
        '    usSynch,       // Begin/EndSynch has been called but the tre' +
        'e did not change the update state.'
      
        '    usUpdate,      // Begin/EndUpdate has been called but the tr' +
        'ee did not change the update state.'
      
        '    usEnd,         // The tree just left the update state (EndUp' +
        'date called for the last level).'
      
        '    usEndSynch     // The tree just left the synch update state ' +
        '(EndSynch called for the last level).'
      '  );'
      ''
      
        '  // These elements are used both to query the application, whic' +
        'h of them it wants to draw itself and to tell it during'
      
        '  // painting, which elements must be drawn during the advanced ' +
        'custom draw events.'
      '  THeaderPaintElements = set of ('
      '    hpeBackground,'
      '    hpeDropMark,'
      '    hpeHeaderGlyph,'
      '    hpeSortGlyph,'
      '    hpeText,'
      
        '    // New in 7.0: Use this in FOnHeaderDrawQueryElements and On' +
        'AdvancedHeaderDraw'
      
        '    // for additional custom header drawing while keeping the de' +
        'fault drawing'
      '    hpeOverlay '
      '  );'
      ''
      
        '  // Various events must be handled at different places than the' +
        'y were initiated or need'
      '  // a persistent storage until they are reset.'
      '  TVirtualTreeStates = set of ('
      '    tsChangePending,          // A selection change is pending.'
      
        '    tsCheckPropagation,       // Set during automatic check stat' +
        'e propagation.'
      
        '    tsCollapsing,             // A full collapse operation is in' +
        ' progress.'
      
        '    tsToggleFocusedSelection, // Node selection was modifed usin' +
        'g Ctrl-click. Change selection state on next mouse up.'
      
        '    tsClearPending,           // Need to clear the current selec' +
        'tion on next mouse move.'
      
        '    tsClearOnNewSelection,    // Need to clear the current selec' +
        'tion before selecting a new node'
      
        '    tsClipboardFlushing,      // Set during flushing the clipboa' +
        'rd to avoid freeing the content.'
      
        '    tsCopyPending,            // Indicates a pending copy operat' +
        'ion which needs to be finished.'
      
        '    tsCutPending,             // Indicates a pending cut operati' +
        'on which needs to be finished.'
      
        '    tsDrawSelPending,         // Multiselection only. User held ' +
        'down the left mouse button on a free'
      
        '                              // area and might want to start dr' +
        'aw selection.'
      
        '    tsDrawSelecting,          // Multiselection only. Draw selec' +
        'tion has actually started.'
      
        '    tsEditing,                // Indicates that an edit operatio' +
        'n is currently in progress.'
      
        '    tsEditPending,            // An mouse up start edit if dragg' +
        'ing has not started.'
      
        '    tsExpanding,              // A full expand operation is in p' +
        'rogress.'
      
        '    tsNodeHeightTracking,     // A node height changing operatio' +
        'n is in progress.'
      
        '    tsNodeHeightTrackPending, // left button is down, user might' +
        ' want to start changing a node'#39's height.'
      
        '    tsHint,                   // Set when our hint is visible or' +
        ' soon will be.'
      
        '    tsInAnimation,            // Set if the tree is currently in' +
        ' an animation loop.'
      
        '    tsIncrementalSearching,   // Set when the user starts increm' +
        'ental search.'
      
        '    tsIncrementalSearchPending, // Set in WM_KEYDOWN to tell to ' +
        'use the char in WM_CHAR for incremental search.'
      
        '    tsIterating,              // Set when IterateSubtree is curr' +
        'ently in progress.'
      
        '    tsLeftButtonDown,         // Set when the left mouse button ' +
        'is down.'
      
        '    tsLeftDblClick,           // Set when the left mouse button ' +
        'was doubly clicked.'
      
        '    tsMiddleButtonDown,       // Set when the middle mouse butto' +
        'n is down.'
      
        '    tsMiddleDblClick,         // Set when the middle mouse butto' +
        'n was doubly clicked.'
      
        '    tsNeedRootCountUpdate,    // Set if while loading a root nod' +
        'e count is set.'
      '    tsOLEDragging,            // OLE dragging in progress.'
      
        '    tsOLEDragPending,         // User has requested to start del' +
        'ayed dragging.'
      
        '    tsPainting,               // The tree is currently painting ' +
        'itself.'
      
        '    tsRightButtonDown,        // Set when the right mouse button' +
        ' is down.'
      
        '    tsRightDblClick,          // Set when the right mouse button' +
        ' was doubly clicked.'
      
        '    tsPopupMenuShown,         // The user clicked the right mous' +
        'e button, which might cause a popup menu to appear.'
      
        '    tsScrolling,              // Set when autoscrolling is activ' +
        'e.'
      
        '    tsScrollPending,          // Set when waiting for the scroll' +
        ' delay time to elapse.'
      
        '    tsSizing,                 // Set when the tree window is bei' +
        'ng resized. This is used to prevent recursive calls'
      
        '                              // due to setting the scrollbars w' +
        'hen sizing.'
      
        '    tsStopValidation,         // Cache validation can be stopped' +
        ' (usually because a change has occured meanwhile).'
      
        '    tsStructureChangePending, // The structure of the tree has b' +
        'een changed while the update was locked.'
      
        '    tsSynchMode,              // Set when the tree is in synch m' +
        'ode, where no timer events are triggered.'
      
        '    tsThumbTracking,          // Stop updating the horizontal sc' +
        'roll bar while dragging the vertical thumb and vice versa.'
      
        '    tsToggling,               // A toggle operation (for some no' +
        'de) is in progress.'
      
        '    tsUpdateHiddenChildrenNeeded, // Pending update for the hidd' +
        'en children flag after massive visibility changes.'
      
        '    tsUpdating,               // The tree does currently not upd' +
        'ate its window because a BeginUpdate has not yet ended.'
      
        '    tsUseCache,               // The tree'#39's node caches are vali' +
        'dated and non-empty.'
      
        '    tsUserDragObject,         // Signals that the application cr' +
        'eated an own drag object in OnStartDrag.'
      
        '    tsUseThemes,              // The tree runs under WinXP+, is ' +
        'theme aware and themes are enabled.'
      
        '    tsValidating,             // The tree'#39's node caches are curr' +
        'ently validated.'
      
        '    tsPreviouslySelectedLocked,// The member FPreviouslySelected' +
        ' should not be changed'
      
        '    tsValidationNeeded,       // Something in the structure of t' +
        'he tree has changed. The cache needs validation.'
      '    tsVCLDragging,            // VCL drag'#39'n drop in progress.'
      
        '    tsVCLDragPending,         // One-shot flag to avoid clearing' +
        ' the current selection on implicit mouse up for VCL drag.'
      
        '    tsVCLDragFinished,        // Flag to avoid triggering the On' +
        'ColumnClick event twice'
      
        '    tsWheelPanning,           // Wheel mouse panning is active o' +
        'r soon will be.'
      
        '    tsWheelScrolling,         // Wheel mouse scrolling is active' +
        ' or soon will be.'
      
        '    tsWindowCreating,         // Set during window handle creati' +
        'on to avoid frequent unnecessary updates.'
      
        '    tsUseExplorerTheme        // The tree runs under WinVista+ a' +
        'nd is using the explorer theme'
      '  );'
      ''
      '  // determines whether and how the drag image is to show'
      '  TVTDragImageKind = ('
      
        '    diComplete,       // show a complete drag image with all col' +
        'umns, only visible columns are shown'
      
        '    diMainColumnOnly, // show only the main column (the tree col' +
        'umn)'
      '    diNoImage         // don'#39't show a drag image at all'
      '  );'
      ''
      
        '  // Switch for OLE and VCL drag'#39'n drop. Because it is not possi' +
        'ble to have both simultanously.'
      '  TVTDragType = ('
      '    dtOLE,'
      '    dtVCL'
      '  );'
      ''
      '  // options which determine what to draw in PaintTree'
      '  TVTInternalPaintOption = ('
      
        '    poBackground,       // draw background image if there is any' +
        ' and it is enabled'
      
        '    poColumnColor,      // erase node'#39's background with the colu' +
        'mn'#39's color'
      
        '    poDrawFocusRect,    // draw focus rectangle around the focus' +
        'ed node'
      
        '    poDrawSelection,    // draw selected nodes with the normal s' +
        'election color'
      
        '    poDrawDropMark,     // draw drop mark if a node is currently' +
        ' the drop target'
      '    poGridLines,        // draw grid lines if enabled'
      '    poMainOnly,         // draw only the main column'
      '    poSelectedOnly,     // draw only selected nodes'
      
        '    poUnbuffered        // draw directly onto the target canvas;' +
        ' especially useful when printing'
      '  );'
      '  TVTInternalPaintOptions = set of TVTInternalPaintOption;'
      ''
      '  // Determines the look of a tree'#39's lines.'
      '  TVTLineStyle = ('
      
        '    lsCustomStyle,           // application provides a line patt' +
        'ern'
      '    lsDotted,                // usual dotted lines (default)'
      '    lsSolid                  // simple solid lines'
      '  );'
      ''
      '  // TVTLineType is used during painting a tree'
      '  TVTLineType = ('
      '    ltNone,          // no line at all'
      
        '    ltBottomRight,   // a line from bottom to the center and fro' +
        'm there to the right'
      '    ltTopDown,       // a line from top to bottom'
      
        '    ltTopDownRight,  // a line from top to bottom and from cente' +
        'r to the right'
      '    ltRight,         // a line from center to the right'
      
        '    ltTopRight,      // a line from bottom to center and from th' +
        'ere to the right'
      '    // special styles for alternative drawings of tree lines'
      '    ltLeft,          // a line from top to bottom at the left'
      
        '    ltLeftBottom     // a combination of ltLeft and a line at th' +
        'e bottom from left to right'
      '  );'
      ''
      '  // Determines how to draw tree lines.'
      '  TVTLineMode = ('
      '    lmNormal,        // usual tree lines (as in TTreeview)'
      
        '    lmBands          // looks similar to a Nassi-Schneidermann d' +
        'iagram'
      '  );'
      ''
      
        '  // A collection of line type IDs which is used while painting ' +
        'a node.'
      '  TLineImage = array of TVTLineType;'
      ''
      ''
      '  // Export type'
      '  TVTExportType = ('
      '    etNone,   // No export, normal displaying on the screen'
      '    etRTF,    // contentToRTF'
      '    etHTML,   // contentToHTML'
      '    etText,   // contentToText'
      '    etExcel,  // supported by external tools'
      '    etWord,   // supported by external tools'
      '    etPDF,    // supported by external tools'
      '    etPrinter,// supported by external tools'
      '    etCSV,    // supported by external tools'
      '    etCustom  // supported by external tools'
      '  );'
      ''
      
        '  TVTNodeExportEvent   = procedure (Sender: TBaseVirtualTree; aE' +
        'xportType: TVTExportType; Node: PVirtualNode) of object;'
      
        '  TVTColumnExportEvent = procedure (Sender: TBaseVirtualTree; aE' +
        'xportType: TVTExportType; Column: TVirtualTreeColumn) of object;'
      
        '  TVTTreeExportEvent   = procedure(Sender: TBaseVirtualTree; aEx' +
        'portType: TVTExportType) of object;'
      ''
      
        '  // For painting a node and its columns/cells a lot of informat' +
        'ion must be passed frequently around.'
      '  TVTImageInfo = record'
      
        '    Index: TImageIndex;       // Index in the associated image l' +
        'ist.'
      
        '    XPos,                     // Horizontal position in the curr' +
        'ent target canvas.'
      
        '    YPos: TDimension;         // Vertical position in the curren' +
        't target canvas.'
      
        '    Ghosted: Boolean;         // Flag to indicate that the image' +
        ' must be drawn slightly lighter.'
      
        '    Images: TCustomImageList; // The image list to be used for p' +
        'ainting.'
      '    function Equals(const pImageInfo2: TVTImageInfo): Boolean;'
      '  end;'
      ''
      '  TVTImageInfoIndex = ('
      '    iiNormal,'
      '    iiState,'
      '    iiCheck,'
      '    iiOverlay'
      '  );'
      ''
      '  // Options which are used when modifying the scroll offsets.'
      '  TScrollUpdateOptions = set of ('
      
        '    suoRepaintHeader,        // if suoUpdateNCArea is also set t' +
        'hen invalidate the header'
      
        '    suoRepaintScrollBars,    // if suoUpdateNCArea is also set t' +
        'hen repaint both scrollbars after updating them'
      
        '    suoScrollClientArea,     // scroll and invalidate the proper' +
        ' part of the client area'
      
        '    suoUpdateNCArea          // update non-client area (scrollba' +
        'rs, header)'
      '  );'
      ''
      '  // Determines the look of a tree'#39's buttons.'
      '  TVTButtonStyle = ('
      
        '    bsRectangle,             // traditional Windows look (plus/m' +
        'inus buttons)'
      '    bsTriangle               // traditional Macintosh look'
      '  );'
      ''
      
        '  // TButtonFillMode is only used when the button style is bsRec' +
        'tangle and determines how to fill the interior.'
      '  TVTButtonFillMode = ('
      
        '    fmTreeColor,             // solid color, uses the tree'#39's bac' +
        'kground color'
      '    fmWindowColor,           // solid color, uses clWindow'
      
        '    fmShaded,                // color gradient, Windows XP style' +
        ' (legacy code, use toThemeAware on Windows XP instead)'
      
        '    fmTransparent            // transparent color, use the item'#39 +
        's background color'
      '  );'
      ''
      '  TVTPaintInfo = record'
      '    Canvas: TCanvas;              // the canvas to paint on'
      
        '    PaintOptions: TVTInternalPaintOptions;  // a copy of the pai' +
        'nt options passed to PaintTree'
      '    Node: PVirtualNode;           // the node to paint'
      
        '    Column: TColumnIndex;         // the node'#39's column index to ' +
        'paint'
      
        '    Position: TColumnPosition;    // the column position of the ' +
        'node'
      '    CellRect: TRect;              // the node cell'
      
        '    ContentRect: TRect;           // the area of the cell used f' +
        'or the node'#39's content'
      '    NodeWidth: TDimension;        // the actual node width'
      
        '    Alignment: TAlignment;        // how to align within the nod' +
        'e rectangle'
      
        '    CaptionAlignment: TAlignment; // how to align text within th' +
        'e caption rectangle'
      
        '    BidiMode: TBidiMode;          // directionality to be used f' +
        'or painting'
      
        '    BrushOrigin: TPoint;          // the alignment for the brush' +
        ' used to draw dotted lines'
      
        '    ImageInfo: array[TVTImageInfoIndex] of TVTImageInfo; // info' +
        ' about each possible node image'
      
        '    Offsets: TVTOffsets;          // The offsets of the various ' +
        'elements of a tree node'
      '    VAlign: TDimension;'
      '    procedure AdjustImageCoordinates();'
      '  end;'
      ''
      
        '  // Method called by the Animate routine for each animation ste' +
        'p.'
      
        '  TVTAnimationCallback = function(Step, StepSize: Integer; Data:' +
        ' Pointer): Boolean of object;'
      ''
      '  TVTIncrementalSearch = ('
      
        '    isAll,                   // search every node in tree, initi' +
        'alize if necessary'
      '    isNone,                  // disable incremental search'
      
        '    isInitializedOnly,       // search only initialized nodes, s' +
        'kip others'
      
        '    isVisibleOnly            // search only visible nodes, initi' +
        'alize if necessary'
      '  );'
      ''
      
        '  // Determines which direction to use when advancing nodes duri' +
        'ng an incremental search.'
      '  TVTSearchDirection = ('
      '    sdForward,'
      '    sdBackward'
      '  );'
      ''
      
        '  // Determines where to start incremental searching for each ke' +
        'y press.'
      '  TVTSearchStart = ('
      
        '    ssAlwaysStartOver,       // always use the first/last node (' +
        'depending on direction) to search from'
      '    ssLastHit,               // use the last found node'
      '    ssFocusedNode            // use the currently focused node'
      '  );'
      ''
      '  // Determines how to use the align member of a node.'
      '  TVTNodeAlignment = ('
      
        '    naFromBottom,            // the align member specifies amoun' +
        't of units (usually pixels) from top border of the node'
      
        '    naFromTop,               // align is to be measured from bot' +
        'tom'
      
        '    naProportional           // align is to be measure in percen' +
        't of the entire node height and relative to top'
      '  );'
      ''
      
        '  // Determines how to draw the selection rectangle used for dra' +
        'w selection.'
      '  TVTDrawSelectionMode = ('
      '    smDottedRectangle,       // same as DrawFocusRect'
      
        '    smBlendedRectangle       // alpha blending, uses special col' +
        'ors (see TVTColors)'
      '  );'
      ''
      
        '  // Determines for which purpose the cell paint event is called' +
        '.'
      '  TVTCellPaintMode = ('
      '    cpmPaint,                // painting the cell'
      '    cpmGetContentMargin      // getting cell content margin'
      '  );'
      ''
      
        '  // Determines which sides of the cell content margin should be' +
        ' considered.'
      '  TVTCellContentMarginType = ('
      '    ccmtAllSides,            // consider all sides'
      
        '    ccmtTopLeftOnly,         // consider top margin and left mar' +
        'gin only'
      
        '    ccmtBottomRightOnly      // consider bottom margin and right' +
        ' margin only'
      '  );'
      ''
      '  TClipboardFormats = class(TStringList)'
      '  private'
      '    FOwner: TBaseVirtualTree;'
      '  public'
      '    constructor Create(AOwner: TBaseVirtualTree); virtual;'
      ''
      '    function Add(const S: string): Integer; override;'
      '    procedure Insert(Index: Integer; const S: string); override;'
      '    property Owner: TBaseVirtualTree read FOwner;'
      '  end;'
      ''
      '  // ----- Event prototypes:'
      ''
      '  // node enumeration'
      
        '  TVTGetNodeProc = reference to procedure(Sender: TBaseVirtualTr' +
        'ee; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);'
      '  // node events'
      
        '  TVTChangingEvent = procedure(Sender: TBaseVirtualTree; Node: P' +
        'VirtualNode; var Allowed: Boolean) of object;'
      
        '  TVTCheckChangingEvent = procedure(Sender: TBaseVirtualTree; No' +
        'de: PVirtualNode; var NewState: TCheckState;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTChangeEvent = procedure(Sender: TBaseVirtualTree; Node: PVi' +
        'rtualNode) of object;'
      
        '  TVTStructureChangeEvent = procedure(Sender: TBaseVirtualTree; ' +
        'Node: PVirtualNode; Reason: TChangeReason) of object;'
      
        '  TVTEditCancelEvent = procedure(Sender: TBaseVirtualTree; Colum' +
        'n: TColumnIndex) of object;'
      
        '  TVTEditChangingEvent = procedure(Sender: TBaseVirtualTree; Nod' +
        'e: PVirtualNode; Column: TColumnIndex;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTEditChangeEvent = procedure(Sender: TBaseVirtualTree; Node:' +
        ' PVirtualNode; Column: TColumnIndex) of object;'
      
        '  TVTFreeNodeEvent = procedure(Sender: TBaseVirtualTree; Node: P' +
        'VirtualNode) of object;'
      
        '  TVTFocusChangingEvent = procedure(Sender: TBaseVirtualTree; Ol' +
        'dNode, NewNode: PVirtualNode; OldColumn,'
      '    NewColumn: TColumnIndex; var Allowed: Boolean) of object;'
      
        '  TVTFocusChangeEvent = procedure(Sender: TBaseVirtualTree; Node' +
        ': PVirtualNode; Column: TColumnIndex) of object;'
      
        '  TVTAddToSelectionEvent = procedure(Sender: TBaseVirtualTree; N' +
        'ode: PVirtualNode) of object;'
      
        '  TVTRemoveFromSelectionEvent = procedure(Sender: TBaseVirtualTr' +
        'ee; Node: PVirtualNode) of object;'
      
        '  TVTGetImageEvent = procedure(Sender: TBaseVirtualTree; Node: P' +
        'VirtualNode; Kind: TVTImageKind; Column: TColumnIndex;'
      
        '    var Ghosted: Boolean; var ImageIndex: TImageIndex) of object' +
        ';'
      
        '  TVTGetImageExEvent = procedure(Sender: TBaseVirtualTree; Node:' +
        ' PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;'
      
        '    var Ghosted: Boolean; var ImageIndex: TImageIndex; var Image' +
        'List: TCustomImageList) of object;'
      
        '  TVTGetImageTextEvent = procedure(Sender: TBaseVirtualTree; Nod' +
        'e: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;'
      '    var ImageText: string) of object;'
      
        '  TVTHotNodeChangeEvent = procedure(Sender: TBaseVirtualTree; Ol' +
        'dNode, NewNode: PVirtualNode) of object;'
      
        '  TVTInitChildrenEvent = procedure(Sender: TBaseVirtualTree; Nod' +
        'e: PVirtualNode; var ChildCount: Cardinal) of object;'
      
        '  TVTInitNodeEvent = procedure(Sender: TBaseVirtualTree; ParentN' +
        'ode, Node: PVirtualNode;'
      '    var InitialStates: TVirtualNodeInitStates) of object;'
      
        '  TVTPopupEvent = procedure(Sender: TBaseVirtualTree; Node: PVir' +
        'tualNode; Column: TColumnIndex; const P: TPoint;'
      
        '    var AskParent: Boolean; var PopupMenu: TPopupMenu) of object' +
        ';'
      
        '  TVTHelpContextEvent = procedure(Sender: TBaseVirtualTree; Node' +
        ': PVirtualNode; Column: TColumnIndex;'
      '    var HelpContext: Integer) of object;'
      
        '  TVTCreateEditorEvent = procedure(Sender: TBaseVirtualTree; Nod' +
        'e: PVirtualNode; Column: TColumnIndex;'
      '    out EditLink: IVTEditLink) of object;'
      
        '  TVTSaveTreeEvent = procedure(Sender: TBaseVirtualTree; Stream:' +
        ' TStream) of object;'
      
        '  TVTSaveNodeEvent = procedure(Sender: TBaseVirtualTree; Node: P' +
        'VirtualNode; Stream: TStream) of object;'
      
        '  TVTBeforeGetCheckStateEvent = procedure(Sender: TBaseVirtualTr' +
        'ee; Node: PVirtualNode) of object;'
      ''
      '  // header/column events'
      
        '  TVTHeaderAddPopupItemEvent = procedure(const Sender: TObject; ' +
        'const Column: TColumnIndex; var Cmd: TAddPopupItemType) of objec' +
        't;'
      
        '  TVTHeaderClickEvent = procedure(Sender: TVTHeader; HitInfo: TV' +
        'THeaderHitInfo) of object;'
      
        '  TVTHeaderMouseEvent = procedure(Sender: TVTHeader; Button: TMo' +
        'useButton; Shift: TShiftState; X, Y: Integer) of object;'
      
        '  TVTHeaderMouseMoveEvent = procedure(Sender: TVTHeader; Shift: ' +
        'TShiftState; X, Y: Integer) of object;'
      
        '  TVTBeforeHeaderHeightTrackingEvent = procedure(Sender: TVTHead' +
        'er; Shift: TShiftState) of object;'
      
        '  TVTAfterHeaderHeightTrackingEvent = procedure(Sender: TVTHeade' +
        'r) of object;'
      
        '  TVTHeaderHeightTrackingEvent = procedure(Sender: TVTHeader; va' +
        'r P: TPoint; Shift: TShiftState; var Allowed: Boolean) of object' +
        ';'
      
        '  TVTHeaderHeightDblClickResizeEvent = procedure(Sender: TVTHead' +
        'er; var P: TPoint; Shift: TShiftState; var Allowed: Boolean) of ' +
        'object;'
      
        '  TVTHeaderNotifyEvent = procedure(Sender: TVTHeader; Column: TC' +
        'olumnIndex) of object;'
      
        '  TVTHeaderDraggingEvent = procedure(Sender: TVTHeader; Column: ' +
        'TColumnIndex; var Allowed: Boolean) of object;'
      
        '  TVTHeaderDraggedEvent = procedure(Sender: TVTHeader; Column: T' +
        'ColumnIndex; OldPosition: Integer) of object;'
      
        '  TVTHeaderDraggedOutEvent = procedure(Sender: TVTHeader; Column' +
        ': TColumnIndex; DropPosition: TPoint) of object;'
      
        '  TVTHeaderPaintEvent = procedure(Sender: TVTHeader; HeaderCanva' +
        's: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover,'
      '    Pressed: Boolean; DropMark: TVTDropMarkMode) of object;'
      
        '  TVTHeaderPaintQueryElementsEvent = procedure(Sender: TVTHeader' +
        '; var PaintInfo: THeaderPaintInfo;'
      '    var Elements: THeaderPaintElements) of object;'
      
        '  TVTAdvancedHeaderPaintEvent = procedure(Sender: TVTHeader; var' +
        ' PaintInfo: THeaderPaintInfo;'
      '    const Elements: THeaderPaintElements) of object;'
      
        '  TVTBeforeAutoFitColumnsEvent = procedure(Sender: TVTHeader; va' +
        'r SmartAutoFitType: TSmartAutoFitType) of object;'
      
        '  TVTBeforeAutoFitColumnEvent = procedure(Sender: TVTHeader; Col' +
        'umn: TColumnIndex; var SmartAutoFitType: TSmartAutoFitType;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTAfterAutoFitColumnEvent = procedure(Sender: TVTHeader; Colu' +
        'mn: TColumnIndex) of object;'
      
        '  TVTAfterAutoFitColumnsEvent = procedure(Sender: TVTHeader) of ' +
        'object;'
      
        '  TVTColumnClickEvent = procedure (Sender: TBaseVirtualTree; Col' +
        'umn: TColumnIndex; Shift: TShiftState) of object;'
      
        '  TVTColumnDblClickEvent = procedure (Sender: TBaseVirtualTree; ' +
        'Column: TColumnIndex; Shift: TShiftState) of object;'
      
        '  TColumnChangeEvent = procedure(const Sender: TBaseVirtualTree;' +
        ' const Column: TColumnIndex; Visible: Boolean) of object;'
      
        '  TVTColumnWidthDblClickResizeEvent = procedure(Sender: TVTHeade' +
        'r; Column: TColumnIndex; Shift: TShiftState; P: TPoint;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTBeforeColumnWidthTrackingEvent = procedure(Sender: TVTHeade' +
        'r; Column: TColumnIndex; Shift: TShiftState) of object;'
      
        '  TVTAfterColumnWidthTrackingEvent = procedure(Sender: TVTHeader' +
        '; Column: TColumnIndex) of object;'
      
        '  TVTColumnWidthTrackingEvent = procedure(Sender: TVTHeader; Col' +
        'umn: TColumnIndex; Shift: TShiftState; var TrackPoint: TPoint; P' +
        ': TPoint;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTGetHeaderCursorEvent = procedure(Sender: TVTHeader; var Cur' +
        'sor: HCURSOR) of object;'
      
        '  TVTBeforeGetMaxColumnWidthEvent = procedure(Sender: TVTHeader;' +
        ' Column: TColumnIndex; var UseSmartColumnWidth: Boolean) of obje' +
        'ct;'
      
        '  TVTAfterGetMaxColumnWidthEvent = procedure(Sender: TVTHeader; ' +
        'Column: TColumnIndex; var MaxWidth: Integer) of object;'
      
        '  TVTCanSplitterResizeColumnEvent = procedure(Sender: TVTHeader;' +
        ' P: TPoint; Column: TColumnIndex; var Allowed: Boolean) of objec' +
        't;'
      
        '  TVTCanSplitterResizeHeaderEvent = procedure(Sender: TVTHeader;' +
        ' P: TPoint; var Allowed: Boolean) of object;'
      ''
      '  // move, copy and node tracking events'
      
        '  TVTNodeMovedEvent = procedure(Sender: TBaseVirtualTree; Node: ' +
        'PVirtualNode) of object;'
      
        '  TVTNodeMovingEvent = procedure(Sender: TBaseVirtualTree; Node,' +
        ' Target: PVirtualNode;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTNodeCopiedEvent = procedure(Sender: TBaseVirtualTree; Node:' +
        ' PVirtualNode) of object;'
      
        '  TVTNodeCopyingEvent = procedure(Sender: TBaseVirtualTree; Node' +
        ', Target: PVirtualNode;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTNodeClickEvent = procedure(Sender: TBaseVirtualTree; const ' +
        'HitInfo: THitInfo) of object;'
      
        '  TVTNodeHeightTrackingEvent = procedure(Sender: TBaseVirtualTre' +
        'e; Node: PVirtualNode; Column: TColumnIndex; Shift: TShiftState;'
      
        '    var TrackPoint: TPoint; P: TPoint; var Allowed: Boolean) of ' +
        'object;'
      
        '  TVTNodeHeightDblClickResizeEvent = procedure(Sender: TBaseVirt' +
        'ualTree; Node: PVirtualNode; Column: TColumnIndex;'
      
        '    Shift: TShiftState; P: TPoint; var Allowed: Boolean) of obje' +
        'ct;'
      
        '  TVTCanSplitterResizeNodeEvent = procedure(Sender: TBaseVirtual' +
        'Tree; P: TPoint; Node: PVirtualNode;'
      '    Column: TColumnIndex; var Allowed: Boolean) of object;'
      ''
      '  // drag'#39'n drop/OLE events'
      
        '  TVTCreateDragManagerEvent = procedure(Sender: TBaseVirtualTree' +
        '; out DragManager: IVTDragManager) of object;'
      
        '  TVTCreateDataObjectEvent = procedure(Sender: TBaseVirtualTree;' +
        ' out IDataObject: IDataObject) of object;'
      
        '  TVTDragAllowedEvent = procedure(Sender: TBaseVirtualTree; Node' +
        ': PVirtualNode; Column: TColumnIndex;'
      '    var Allowed: Boolean) of object;'
      
        '  TVTDragOverEvent = procedure(Sender: TBaseVirtualTree; Source:' +
        ' TObject; Shift: TShiftState; State: TDragState;'
      
        '    Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept' +
        ': Boolean) of object;'
      
        '  TVTDragDropEvent = procedure(Sender: TBaseVirtualTree; Source:' +
        ' TObject; DataObject: IDataObject;'
      
        '    Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var E' +
        'ffect: Integer; Mode: TDropMode) of object;'
      
        '  TVTRenderOLEDataEvent = procedure(Sender: TBaseVirtualTree; co' +
        'nst FormatEtcIn: TFormatEtc; out Medium: TStgMedium;'
      '    ForClipboard: Boolean; var Result: HRESULT) of object;'
      
        '  TVTGetUserClipboardFormatsEvent = procedure(Sender: TBaseVirtu' +
        'alTree; var Formats: TFormatEtcArray) of object;'
      ''
      '  // paint events'
      
        '  TVTBeforeItemEraseEvent = procedure(Sender: TBaseVirtualTree; ' +
        'TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;'
      
        '    var ItemColor: TColor; var EraseAction: TItemEraseAction) of' +
        ' object;'
      
        '  TVTAfterItemEraseEvent = procedure(Sender: TBaseVirtualTree; T' +
        'argetCanvas: TCanvas; Node: PVirtualNode;'
      '    ItemRect: TRect) of object;'
      
        '  TVTBeforeItemPaintEvent = procedure(Sender: TBaseVirtualTree; ' +
        'TargetCanvas: TCanvas; Node: PVirtualNode;'
      '    ItemRect: TRect; var CustomDraw: Boolean) of object;'
      
        '  TVTAfterItemPaintEvent = procedure(Sender: TBaseVirtualTree; T' +
        'argetCanvas: TCanvas; Node: PVirtualNode;'
      '    ItemRect: TRect) of object;'
      
        '  TVTBeforeCellPaintEvent = procedure(Sender: TBaseVirtualTree; ' +
        'TargetCanvas: TCanvas; Node: PVirtualNode;'
      
        '    Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellR' +
        'ect: TRect; var ContentRect: TRect) of object;'
      
        '  TVTAfterCellPaintEvent = procedure(Sender: TBaseVirtualTree; T' +
        'argetCanvas: TCanvas; Node: PVirtualNode;'
      '    Column: TColumnIndex; CellRect: TRect) of object;'
      
        '  TVTPaintEvent = procedure(Sender: TBaseVirtualTree; TargetCanv' +
        'as: TCanvas) of object;'
      
        '  TVTBackgroundPaintEvent = procedure(Sender: TBaseVirtualTree; ' +
        'TargetCanvas: TCanvas; R: TRect;'
      '    var Handled: Boolean) of object;'
      
        '  TVTGetLineStyleEvent = procedure(Sender: TBaseVirtualTree; var' +
        ' Bits: Pointer) of object;'
      
        '  TVTMeasureItemEvent = procedure(Sender: TBaseVirtualTree; Targ' +
        'etCanvas: TCanvas; Node: PVirtualNode;'
      '    var NodeHeight: Integer) of object;'
      ''
      
        '  TVTPrepareButtonImagesEvent = procedure(Sender: TBaseVirtualTr' +
        'ee; const APlusBM : TBitmap; const APlusHotBM :TBitmap;'
      
        '                                          const APlusSelectedHot' +
        'BM :TBitmap; const AMinusBM : TBitmap; const AMinusHotBM : TBitm' +
        'ap;'
      
        '                                          const AMinusSelectedHo' +
        'tBM :TBitmap; var ASize : TSize) of object;'
      ''
      '  // search, sort'
      
        '  TVTCompareEvent = procedure(Sender: TBaseVirtualTree; Node1, N' +
        'ode2: PVirtualNode; Column: TColumnIndex;'
      '    var Result: Integer) of object;'
      
        '  TVTIncrementalSearchEvent = procedure(Sender: TBaseVirtualTree' +
        '; Node: PVirtualNode; const SearchText: string;'
      '    var Result: Integer) of object;'
      ''
      '  // operations'
      
        '  TVTOperationEvent = procedure(Sender: TBaseVirtualTree; Operat' +
        'ionKind: TVTOperationKind) of object;'
      ''
      '  TVTHintKind = (vhkText, vhkOwnerDraw);'
      
        '  TVTHintKindEvent = procedure(Sender: TBaseVirtualTree; Node: P' +
        'VirtualNode; Column: TColumnIndex; var Kind: TVTHintKind) of obj' +
        'ect;'
      
        '  TVTDrawHintEvent = procedure(Sender: TBaseVirtualTree; HintCan' +
        'vas: TCanvas; Node: PVirtualNode; R: TRect; Column: TColumnIndex' +
        ') of object;'
      
        '  TVTGetHintSizeEvent = procedure(Sender: TBaseVirtualTree; Node' +
        ': PVirtualNode; Column: TColumnIndex; var R: TRect) of object;'
      ''
      '  // miscellaneous'
      
        '  TVTBeforeDrawLineImageEvent = procedure(Sender: TBaseVirtualTr' +
        'ee; Node: PVirtualNode; Level: Integer; var PosX: Integer) of ob' +
        'ject;'
      
        '  TVTGetNodeDataSizeEvent = procedure(Sender: TBaseVirtualTree; ' +
        'var NodeDataSize: Integer) of object;'
      
        '  TVTKeyActionEvent = procedure(Sender: TBaseVirtualTree; var Ch' +
        'arCode: Word; var Shift: TShiftState;'
      '    var DoDefault: Boolean) of object;'
      
        '  TVTScrollEvent = procedure(Sender: TBaseVirtualTree; DeltaX, D' +
        'eltaY: Integer) of object;'
      
        '  TVTUpdatingEvent = procedure(Sender: TBaseVirtualTree; State: ' +
        'TVTUpdateState) of object;'
      
        '  TVTGetCursorEvent = procedure(Sender: TBaseVirtualTree; var Cu' +
        'rsor: TCursor) of object;'
      
        '  TVTStateChangeEvent = procedure(Sender: TBaseVirtualTree; Ente' +
        'r, Leave: TVirtualTreeStates) of object;'
      
        '  TVTGetCellIsEmptyEvent = procedure(Sender: TBaseVirtualTree; N' +
        'ode: PVirtualNode; Column: TColumnIndex;'
      '    var IsEmpty: Boolean) of object;'
      
        '  TVTScrollBarShowEvent = procedure(Sender: TBaseVirtualTree; Ba' +
        'r: Integer; Show: Boolean) of object;'
      ''
      '  // Helper types for node iterations.'
      '  TGetFirstNodeProc = function: PVirtualNode of object;'
      
        '  TGetNextNodeProc = function(Node: PVirtualNode; ConsiderChildr' +
        'enAbove: Boolean = False): PVirtualNode of object;'
      ''
      '  TVZVirtualNodeEnumerationMode = ('
      '    vneAll,'
      '    vneChecked,'
      '    vneChild,'
      '    vneCutCopy,'
      '    vneInitialized,'
      '    vneLeaf,'
      '    vneLevel,'
      '    vneNoInit,'
      '    vneSelected,'
      '    vneVisible,'
      '    vneVisibleChild,'
      '    vneVisibleNoInitChild,'
      '    vneVisibleNoInit'
      '  );'
      ''
      '  PVTVirtualNodeEnumeration = ^TVTVirtualNodeEnumeration;'
      ''
      '  TVTVirtualNodeEnumerator = record'
      '  private'
      '    FNode: PVirtualNode;'
      '    FCanMoveNext: Boolean;'
      '    FEnumeration: PVTVirtualNodeEnumeration;'
      '    function GetCurrent: PVirtualNode; inline;'
      '  public'
      '    function MoveNext: Boolean; inline;'
      '    property Current: PVirtualNode read GetCurrent;'
      '  end;'
      ''
      '  TVTVirtualNodeEnumeration = record'
      '  private'
      '    FMode: TVZVirtualNodeEnumerationMode;'
      '    FTree: TBaseVirtualTree;'
      '    // GetNextXxx parameters:'
      '    FConsiderChildrenAbove: Boolean;'
      '    FNode: PVirtualNode;'
      '    FNodeLevel: Cardinal;'
      '    FState: TCheckState;'
      '    FIncludeFiltered: Boolean;'
      '  public'
      '    function GetEnumerator: TVTVirtualNodeEnumerator;'
      '  private'
      '    function GetNext(Node: PVirtualNode): PVirtualNode;'
      '  end;'
      ''
      ''
      '  // ----- TBaseVirtualTree'
      '  TBaseVirtualTree = class(TCustomControl)'
      '  private'
      
        '    FTotalInternalDataSize: Cardinal;            // Cache of the' +
        ' sum of the necessary internal data size for all tree'
      '    FBorderStyle: TBorderStyle;'
      '    FHeader: TVTHeader;'
      '    FRoot: PVirtualNode;'
      '    FDefaultNodeHeight,'
      '    FIndent: Cardinal;'
      '    FOptions: TCustomVirtualTreeOptions;'
      
        '    FUpdateCount: Cardinal;                      // update stopp' +
        'er, updates of the tree control are only done if = 0'
      
        '    FSynchUpdateCount: Cardinal;                 // synchronizer' +
        ', causes all events which are usually done via timers'
      
        '                                                 // to happen im' +
        'mediately, regardless of the normal update state'
      
        '    FNodeDataSize: Integer;                      // number of by' +
        'tes to allocate with each node (in addition to its base'
      
        '                                                 // structure an' +
        'd the internal data), if -1 then do callback'
      
        '    FStates: TVirtualTreeStates;                 // various acti' +
        've/pending states the tree needs to consider'
      '    FLastSelected,'
      '    FFocusedNode: PVirtualNode;'
      
        '    FEditColumn,                                 // column to be' +
        ' edited (focused node)'
      
        '    FFocusedColumn: TColumnIndex;                // NoColumn if ' +
        'no columns are active otherwise the last hit column of'
      
        '                                                 // the currentl' +
        'y focused node'
      
        '    FHeightTrackPoint: TPoint;                   // Starting poi' +
        'nt of a node'#39's height changing operation.'
      
        '    FHeightTrackNode: PVirtualNode;              // Node which h' +
        'eight is being changed.'
      
        '    FHeightTrackColumn: TColumnIndex;            // Initial colu' +
        'mn where the height changing operation takes place.'
      
        '    FScrollDirections: TScrollDirections;        // directions t' +
        'o scroll client area into depending on mouse position'
      
        '    FLastStructureChangeReason: TChangeReason;   // Used for del' +
        'ayed structure change event.'
      '    FLastStructureChangeNode,                    // dito'
      
        '    FLastChangedNode,                            // used for del' +
        'ayed change event'
      
        '    FCurrentHotNode: PVirtualNode;               // Node over wh' +
        'ich the mouse is hovering.'
      
        '    FCurrentHotColumn: TColumnIndex;             // Column over ' +
        'which the mouse is hovering.'
      
        '    FHotNodeButtonHit: Boolean;                  // Indicates we' +
        'ther the mouse is hovering over the hot node'#39's button.'
      '    FLastSelRect,'
      
        '    FNewSelRect: TRect;                          // used while d' +
        'oing draw selection'
      
        '    FHotCursor: TCursor;                         // can be set t' +
        'o additionally indicate the current hot node'
      
        '    FLastHitInfo: THitInfo;                      // The THitInfo' +
        ' of the last mouse-down event.'
      
        '                                                 // in Win98 (sl' +
        'ide) and Windows 2000 (fade))'
      
        '    FHintMode: TVTHintMode;                      // determines t' +
        'he kind of the hint window'
      
        '    FHintData: TVTHintData;                      // used while p' +
        'reparing the hint window'
      
        '    FChangeDelay: Cardinal;                      // used to dela' +
        'y OnChange event'
      
        '    FEditDelay: Cardinal;                        // determines t' +
        'ime to elapse before a node goes into edit mode'
      
        '    FPositionCache: TCache;                      // array which ' +
        'stores node references ordered by vertical positions'
      
        '                                                 // (see also Do' +
        'ValidateCache for more information)'
      
        '    FVisibleCount: Cardinal;                     // number of cu' +
        'rrently visible nodes'
      
        '    FStartIndex: Cardinal;                       // index to sta' +
        'rt validating cache from'
      
        '    FSelection: TNodeArray;                      // list of curr' +
        'ently selected nodes'
      
        '    FSelectionCount: Integer;                    // number of cu' +
        'rrently selected nodes (size of FSelection might differ)'
      
        '    FSelectionLocked: Boolean;                   // prevents the' +
        ' tree from changing the selection '
      
        '    FRangeAnchor: PVirtualNode;                  // anchor node ' +
        'for selection with the keyboard, determines start of a'
      
        '                                                 // selection ra' +
        'nge'
      
        '    FCheckPropagationCount: Cardinal;            // nesting leve' +
        'l of check propagation (WL, 05.02.2004)'
      
        '    FLastSelectionLevel: Integer;                // keeps the la' +
        'st node level for constrained multiselection'
      
        '    FDrawSelShiftState: TShiftState;             // keeps the in' +
        'itial shift state when the user starts selection with'
      '                                                 // the mouse'
      
        '    FEditLink: IVTEditLink;                      // used to comu' +
        'nicate with an application defined editor'
      
        '    FTempNodeCache: TNodeArray;                  // used at vari' +
        'ous places to hold temporarily a bunch of node refs.'
      
        '    FTempNodeCount: Cardinal;                    // number of no' +
        'des in FTempNodeCache'
      
        '    FBackground: TPicture;                       // A background' +
        ' image loadable at design and runtime.'
      
        '    FBackgroundImageTransparent: Boolean;        // By default, ' +
        'this is off. When switched on, will try to draw the image'
      
        '                                                 // transparent ' +
        'by using the color of the component as transparent color'
      ''
      
        '    FMargin: Integer;                            // horizontal d' +
        'istance to border and columns'
      
        '    FTextMargin: Integer;                        // space betwee' +
        'n the node'#39's text and its horizontal bounds'
      '    FBackgroundOffsetX,'
      
        '    FBackgroundOffsetY: Integer;                 // used to fine' +
        ' tune the position of the background image'
      
        '    FAnimationDuration: Cardinal;                // specifies ho' +
        'w long an animation shall take (expanding, hint)'
      
        '    FWantTabs: Boolean;                          // If True then' +
        ' the tree also consumes the tab key.'
      
        '    FNodeAlignment: TVTNodeAlignment;            // determines h' +
        'ow to interpret the align member of a node'
      
        '    FHeaderRect: TRect;                          // Space which ' +
        'the header currently uses in the control (window coords).'
      
        '    FLastHintRect: TRect;                        // Area which t' +
        'he mouse must leave to reshow a hint.'
      '    FUpdateRect: TRect;'
      
        '    FEmptyListMessage: string;            // Optional message th' +
        'at will be displayed if no nodes exist in the control.'
      ''
      '    // paint support and images'
      '    FPlusBM,'
      
        '    FMinusBM,                                    // small bitmap' +
        's used for tree buttons'
      '    FHotPlusBM,'
      '    FHotMinusBM,'
      '    FSelectedHotPlusBM,'
      
        '    FSelectedHotMinusBM: TBitmap;                // small bitmap' +
        's used for hot tree buttons'
      
        '    FImages,                                     // normal image' +
        's in the tree'
      
        '    FStateImages,                                // state images' +
        ' in the tree'
      
        '    FCustomCheckImages: TCustomImageList;        // application ' +
        'defined check images'
      
        '    FCheckImageKind: TCheckImageKind;            // light or dar' +
        'k, cross marks or tick marks'
      
        '    FCheckImages: TCustomImageList;              // Reference to' +
        ' global image list to be used for the check images.'
      '    //TODO: Use this margin for other images as well'
      
        '    FImagesMargin: Integer;                      // The margin u' +
        'sed left and right of the checkboxes.'
      '    FImageChangeLink,'
      '    FStateChangeLink,'
      
        '    FCustomCheckChangeLink: TChangeLink;         // connections ' +
        'to the image lists'
      
        '    FOldFontChange: TNotifyEvent;                // helper metho' +
        'd pointer for tracking font changes in the off screen buffer'
      
        '    FColors: TVTColors;                          // class compri' +
        'sing all customizable colors in the tree'
      
        '    FButtonStyle: TVTButtonStyle;                // style of the' +
        ' tree buttons'
      
        '    FButtonFillMode: TVTButtonFillMode;          // for rectangu' +
        'lar tree buttons only: how to fill them'
      
        '    FLineStyle: TVTLineStyle;                    // style of the' +
        ' tree lines'
      
        '    FLineMode: TVTLineMode;                      // tree lines o' +
        'r bands etc.'
      
        '    FDottedBrush: HBRUSH;                        // used to pain' +
        't dotted lines without special pens'
      
        '    FSelectionCurveRadius: Cardinal;             // radius for r' +
        'ounded selection rectangles'
      
        '    FSelectionBlendFactor: Byte;                 // Determines t' +
        'he factor by which the selection rectangle is to be'
      
        '                                                 // faded if ena' +
        'bled.'
      
        '    FDrawSelectionMode: TVTDrawSelectionMode;    // determines t' +
        'he paint mode for draw selection'
      ''
      '    // alignment and directionality support'
      
        '    FAlignment: TAlignment;                      // default alig' +
        'nment of the tree if no columns are shown'
      ''
      '    // drag'#39'n drop and clipboard support'
      
        '    FDragImageKind: TVTDragImageKind;            // determines w' +
        'hether or not and what to show in the drag image'
      
        '    FDragOperations: TDragOperations;            // determines w' +
        'hich operations are allowed during drag'#39'n drop'
      
        '    FDragThreshold: Integer;                     // used to dete' +
        'rmine when to actually start a drag'#39'n drop operation'
      
        '    FDragManager: IVTDragManager;                // drag'#39'n drop,' +
        ' cut'#39'n paste'
      
        '    FDropTargetNode: PVirtualNode;               // node current' +
        'ly selected as drop target'
      
        '    FLastDropMode: TDropMode;                    // set while dr' +
        'agging and used to track changes'
      
        '    FDragSelection: TNodeArray;                  // temporary co' +
        'py of FSelection used during drag'#39'n drop'
      
        '    FLastDragEffect: Integer;                    // The last exe' +
        'cuted drag effect'
      
        '    FDragType: TVTDragType;                      // used to swit' +
        'ch between OLE and VCL drag'#39'n drop'
      
        '    FDragImage: TVTDragImage;                    // drag image m' +
        'anagement'
      '    FDragWidth,'
      
        '    FDragHeight: Integer;                        // size of the ' +
        'drag image, the larger the more CPU power is needed'
      
        '    FClipboardFormats: TClipboardFormats;        // a list of cl' +
        'ipboard format descriptions enabled for this tree'
      
        '    FLastVCLDragTarget: PVirtualNode;            // A node cache' +
        ' for VCL drag'#39'n drop (keywords: DragLeave on DragDrop).'
      
        '    FVCLDragEffect: Integer;                     // A cache for ' +
        'VCL drag'#39'n drop to keep the current drop effect.'
      ''
      '    // scroll support'
      
        '    FScrollBarOptions: TScrollBarOptions;        // common prope' +
        'rties of horizontal and vertical scrollbar'
      
        '    FAutoScrollInterval: TAutoScrollInterval;    // determines s' +
        'peed of auto scrolling'
      
        '    FAutoScrollDelay: Cardinal;                  // amount of mi' +
        'lliseconds to wait until autoscrolling becomes active'
      
        '    FAutoExpandDelay: Cardinal;                  // amount of mi' +
        'lliseconds to wait until a node is expanded if it is the'
      '                                                 // drop target'
      '    FOffsetX: Integer;'
      
        '    FOffsetY: Integer;                           // Determines l' +
        'eft and top scroll offset.'
      
        '    FEffectiveOffsetX: Integer;                  // Actual posit' +
        'ion of the horizontal scroll bar (varies depending on bidi mode)' +
        '.'
      '    FRangeX,'
      
        '    FRangeY: Cardinal;                           // current virt' +
        'ual width and height of the tree'
      
        '    FBottomSpace: Cardinal;                      // Extra space ' +
        'below the last node.'
      ''
      
        '    FDefaultPasteMode: TVTNodeAttachMode;        // Used to dete' +
        'rmine where to add pasted nodes to.'
      
        '    FDragScrollStart: Cardinal;                  // Contains the' +
        ' start time when a tree does auto scrolling as drop target.'
      ''
      '    // search'
      
        '    FIncrementalSearch: TVTIncrementalSearch;    // Used to dete' +
        'rmine whether and how incremental search is to be used.'
      
        '    FSearchTimeout: Cardinal;                    // Number of mi' +
        'lliseconds after which to stop incremental searching.'
      
        '    FSearchBuffer: string;                 // Collects a sequenc' +
        'e of keypresses used to do incremental searching.'
      
        '    FLastSearchNode: PVirtualNode;               // Reference to' +
        ' node which was last found as search fit.'
      
        '    FSearchDirection: TVTSearchDirection;        // Direction to' +
        ' incrementally search the tree.'
      
        '    FSearchStart: TVTSearchStart;                // Where to sta' +
        'rt iteration on each key press.'
      ''
      '    // miscellanous'
      
        '    FPanningWindow: HWND;                        // Helper windo' +
        'w for wheel panning'
      
        '    FPanningCursor: HCURSOR;                     // Current whee' +
        'l panning cursor.'
      
        '    FPanningImage: TBitmap;                      // A little 32x' +
        '32 bitmap to indicate the panning reference point.'
      
        '    FLastClickPos: TPoint;                       // Used for ret' +
        'ained drag start and wheel mouse scrolling.'
      
        '    FOperationCount: Cardinal;                   // Counts how m' +
        'any nested long-running operations are in progress.'
      
        '    FOperationCanceled: Boolean;                 // Used to indi' +
        'cate that a long-running operation should be canceled.'
      
        '    FChangingTheme: Boolean;                     // Used to indi' +
        'cate that a theme change is goi ng on'
      
        '    FNextNodeToSelect: PVirtualNode;             // Next tree no' +
        'de that we would like to select if the current one gets de')
    Options = [eoAutoIndent, eoDisableScrollArrows, eoDragDropEditing, eoDropFiles, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoRightMouseMovesCursor, eoShowScrollHint, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces, eoShowLigatures]
    SelectedColor.Alpha = 0.400000005960464500
  end
  object pmnuEditor: TPopupMenu
    OnPopup = pmnuEditorPopup
    Left = 708
    Top = 36
    object pmnuSpelling: TMenuItem
      Caption = 'Spelling'
      object pmnSeparator: TMenuItem
        Caption = '-'
      end
      object pmnDelete: TMenuItem
        Action = CommandsDataModule.actSynSpellErrorDelete
      end
      object pmnAdd: TMenuItem
        Action = CommandsDataModule.actSynSpellErrorAdd
      end
      object pmnIgnore: TMenuItem
        Action = CommandsDataModule.actSynSpellErrorIgnore
      end
      object pmnIgnoreOnce: TMenuItem
        Action = CommandsDataModule.actSynSpellErrorIgnoreOnce
      end
      object pmnSeparator2: TMenuItem
        Caption = '-'
      end
      object CheckFile1: TMenuItem
        Action = CommandsDataModule.actSynSpellCheckWord
      end
      object SpellClearErros11: TMenuItem
        Action = CommandsDataModule.actSynSpellCheckLine
      end
      object CheckSelection1: TMenuItem
        Action = CommandsDataModule.actSynSpellCheckSelection
      end
      object CheckWord1: TMenuItem
        Action = CommandsDataModule.actSynSpellCheckFile
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object ClearErrors1: TMenuItem
        Action = CommandsDataModule.actSynSpellClearErrors
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object CheckAsYouType1: TMenuItem
        Action = CommandsDataModule.actSynSpellCheckAsYouType
      end
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object lmiEditUndo: TMenuItem
      Action = CommandsDataModule.EditUndo1
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object lmiEditCut: TMenuItem
      Action = CommandsDataModule.EditCut1
    end
    object lmiEditCopy: TMenuItem
      Action = CommandsDataModule.EditCopy1
    end
    object lmiEditPaste: TMenuItem
      Action = CommandsDataModule.EditPaste1
    end
    object lmiEditDelete: TMenuItem
      Action = CommandsDataModule.EditDelete1
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object lmiEditSelectAll: TMenuItem
      Action = CommandsDataModule.EditSelectAll1
    end
  end
end
