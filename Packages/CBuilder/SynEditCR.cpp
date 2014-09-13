//---------------------------------------------------------------------------

#include <System.hpp>
#pragma hdrstop
USEFORMNS("..\..\Source\SynAutoCorrectEditor.pas", Synautocorrecteditor, frmAutoCorrectEditor);
USEFORMNS("..\..\Source\SynEditKeyCmdEditor.pas", Syneditkeycmdeditor, SynEditKeystrokeEditorForm);
USEFORMNS("..\..\Source\SynEditKeyCmdsEditor.pas", Syneditkeycmdseditor, SynEditKeystrokesEditorForm);
USEFORMNS("..\..\Source\SynEditOptionsDialog.pas", Syneditoptionsdialog, fmEditorOptionsDialog);
USEFORMNS("..\..\Source\SynEditPrintMarginsDialog.pas", Syneditprintmarginsdialog, SynEditPrintMarginsDlg);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package-Quelltext.
//---------------------------------------------------------------------------


#pragma argsused
extern "C" int _libmain(unsigned long reason)
{
	return 1;
}
//---------------------------------------------------------------------------
