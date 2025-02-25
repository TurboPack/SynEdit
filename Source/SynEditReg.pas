{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditReg.pas, released 2000-04-07.
All Rights Reserved.

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

unit SynEditReg;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.Actions,
  // SynEdit components
  SynEdit,
  SynDBEdit,
  SynEditStrConst,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynEditPlugins,
  SynEditExport,
  SynExportHTML,
  SynExportRTF,
  SynExportTeX,
  SynHighlighterMulti,
  SynCompletionProposal,
  SynEditPythonBehaviour,
  SynEditPrint,
  SynEditPrintPreview,
  SynMacroRecorder,
  SynAutoCorrect,
  SynEditSearch,
  SynEditRegexSearch,
  SynHighlighterManager,
  SynEditOptionsDialog,
  SynHighlighterADSP21xx,
  SynHighlighterAsm,
  SynHighlighterAWK,
  SynHighlighterBaan,
  SynHighlighterBat,
  SynHighlighterCAC,
  SynHighlighterCache,
  SynHighlighterCobol,
  SynHighlighterCpp,
  SynHighlighterCS,
  SynHighlighterCss,
  SynHighlighterDfm,
  SynHighlighterDml,
  SynHighlighterDOT,
  SynHighlighterDWS,
  SynHighlighterEiffel,
  SynHighlighterFortran,
  SynHighlighterFoxpro,
  SynHighlighterGalaxy,
  SynHighlighterGeneral,
  SynHighlighterHaskell,
  SynHighlighterHC11,
  SynHighlighterHP48,
  SynHighlighterHtml,
  SynHighlighterIni,
  SynHighlighterInno,
  SynHighlighterJava,
  SynHighlighterJScript,
  SynHighlighterJSON,
  SynHighlighterKix,
  SynHighlighterModelica,
  SynHighlighterM3,   
  SynHighlighterPas,
  SynHighlighterPerl, 
  SynHighlighterPHP,
  SynHighlighterProgress, 
  SynHighlighterPython,
  SynHighlighterRC,
  SynHighlighterRuby,
  SynHighlighterSml,
  SynHighlighterSQL,
  SynHighlighterTclTk,
  SynHighlighterTeX,
  SynHighlighterUNIXShellScript,
  SynHighlighterURI,
  SynHighlighterVB,
  SynHighlighterVBScript,
  SynHighlighterVrml97,
  SynHighlighterGWS,
  SynHighlighterCPM,
  SynHighlighterSDD,
  SynHighlighterXML,
  SynHighlighterYAML,
  SynHighlighterMsg,
  SynHighlighterIDL,
  SynHighlighterUnreal,
  SynHighlighterST,
  SynHighlighterLDraw,
  SynHighlighterOmni,
  SynSpellCheck,
  SynURIOpener,
  SynEditActionsResource;

procedure Register;

implementation

procedure Register;
begin
// SynEdit main components
  RegisterComponents(SYNS_ComponentsPage, [TSynEdit]);

  RegisterComponents(SYNS_ComponentsPage, [TDBSynEdit]);

  GroupDescendentsWith(TSynCustomHighlighter, TSynEdit);
  GroupDescendentsWith(TSynEditSearchCustom, TSynEdit);
  GroupDescendentsWith(TSynCustomExporter, TSynEdit);
  GroupDescendentsWith(TSynMultiSyn, TSynEdit);
  GroupDescendentsWith(TSynBaseCompletionProposal, TSynEdit);
  GroupDescendentsWith(TSynAutoComplete, TSynEdit);
  GroupDescendentsWith(TAbstractSynPlugin, TSynEdit);
  GroupDescendentsWith(TCustomSynAutoCorrect, TSynEdit);
  GroupDescendentsWith(TSynEditPrint, TSynEdit);
  GroupDescendentsWith(TSynEditPrintPreview, TSynEdit);
  GroupDescendentsWith(TSynEditPythonBehaviour, TSynEdit);
  GroupDescendentsWith(TSynHighlighterManager, TSynEdit);
  GroupDescendentsWith(TSynEditOptionsDialog, TSynEdit);
  GroupDescendentsWith(TSynURIOpener, TSynEdit);

// SynEdit extra components
  RegisterComponents(SYNS_ComponentsPage, [TSynExporterHTML, TSynExporterRTF,
    TSynExporterTeX, TSynEditPythonBehaviour, TSynMultiSyn,
    TSynCompletionProposal, TSynAutoComplete, TSynMacroRecorder,
    TSynEditPrint, TSynEditPrintPreview, TSynAutoCorrect,
    TSynEditSearch, TSynEditRegexSearch, TSynEditOptionsDialog, TSynURIOpener]);
  RegisterComponents(SYNS_ComponentsPage, [TSynHighlighterManager]);
  RegisterComponents(SYNS_ComponentsPage, [TSynSpellCheck]);

// SynEdit highlighters
  RegisterComponents(SYNS_HighlightersPage, [
    //classic
    TSynCppSyn, TSynEiffelSyn, TSynFortranSyn, TSynGeneralSyn, TSynJavaSyn,
    TSynM3Syn, TSynPasSyn, TSynVBSyn, TSynCobolSyn, TSynCSSyn, TSynOmniSyn,
    // internet
    TSynCssSyn, TSynHTMLSyn, TSynJScriptSyn, TSynPHPSyn, TSynVBScriptSyn,
    TSynXMLSyn, TSynJSONSyn, TSynVrml97Syn,
    //interpreted
    TSynAWKSyn, TSynBATSyn,
    TSynDWSSyn,
    TSynKixSyn, TSynPerlSyn, TSynPythonSyn,
    TSynTclTkSyn, TSynGWScriptSyn, TSynRubySyn, TSynUNIXShellScriptSyn,
    //database
    TSynCACSyn, TSynCacheSyn, TSynFoxproSyn, TSynSQLSyn, TSynSDDSyn,
    //assembler
    TSynADSP21xxSyn, TSynAsmSyn, TSynHC11Syn, TSynHP48Syn, TSynSTSyn,
    //data modeling
    TSynDmlSyn, TSynModelicaSyn, TSynSMLSyn,
    //data
    TSynDfmSyn, TSynIniSyn, TSynInnoSyn,
    // other
    TSynBaanSyn, TSynGalaxySyn, TSynProgressSyn, TSynMsgSyn,
    TSynIdlSyn, TSynUnrealSyn, TSynCPMSyn, TSynTeXSyn, TSynYAMLSyn,
    TSynHaskellSyn, TSynLDRSyn, TSynURISyn, TSynDOTSyn, TSynRCSyn
  ]);

  // SynEdit Actions
  RegisterActions('SynEdit Spell Check',
    [TSynSpellCheckFile, TSynSpellCheckLine, TSynSpellCheckSelection,
    TSynSpellCheckWord, TSynSpellClearErrors, TSynSpellCheckAsYouType,
    TSynSpellErrorAdd, TSynSpellErrorIgnoreOnce,
    TSynSpellErrorIgnore, TSynSpellErrorDelete], TSynEditActions);
  RegisterActions('Edit', [TSynEditRedo], TSynEditActions);
end;

end.
