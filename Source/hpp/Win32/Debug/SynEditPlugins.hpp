// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynEditPlugins.pas' rev: 29.00 (Windows)

#ifndef SyneditpluginsHPP
#define SyneditpluginsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Menus.hpp>
#include <SynEdit.hpp>
#include <SynEditKeyCmds.hpp>
#include <SynUnicode.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syneditplugins
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAbstractSynPlugin;
class DELPHICLASS TAbstractSynHookerPlugin;
class DELPHICLASS TAbstractSynSingleHookPlugin;
class DELPHICLASS TAbstractSynCompletion;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAbstractSynPlugin : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	void __fastcall SetEditor(Synedit::TCustomSynEdit* const Value);
	Synedit::TCustomSynEdit* __fastcall GetEditors(int aIndex);
	Synedit::TCustomSynEdit* __fastcall GetEditor(void);
	int __fastcall GetEditorCount(void);
	
protected:
	System::Classes::TList* fEditors;
	virtual void __fastcall Notification(System::Classes::TComponent* aComponent, System::Classes::TOperation aOperation);
	virtual void __fastcall DoAddEditor(Synedit::TCustomSynEdit* aEditor);
	virtual void __fastcall DoRemoveEditor(Synedit::TCustomSynEdit* aEditor);
	int __fastcall AddEditor(Synedit::TCustomSynEdit* aEditor);
	int __fastcall RemoveEditor(Synedit::TCustomSynEdit* aEditor);
	
public:
	__fastcall virtual ~TAbstractSynPlugin(void);
	__property Synedit::TCustomSynEdit* Editors[int aIndex] = {read=GetEditors};
	__property int EditorCount = {read=GetEditorCount, nodefault};
	
__published:
	__property Synedit::TCustomSynEdit* Editor = {read=GetEditor, write=SetEditor};
public:
	/* TComponent.Create */ inline __fastcall virtual TAbstractSynPlugin(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	
};


class PASCALIMPLEMENTATION TAbstractSynHookerPlugin : public TAbstractSynPlugin
{
	typedef TAbstractSynPlugin inherited;
	
protected:
	void __fastcall HookEditor(Synedit::TCustomSynEdit* aEditor, Syneditkeycmds::TSynEditorCommand aCommandID, System::Classes::TShortCut aOldShortCut, System::Classes::TShortCut aNewShortCut);
	void __fastcall UnHookEditor(Synedit::TCustomSynEdit* aEditor, Syneditkeycmds::TSynEditorCommand aCommandID, System::Classes::TShortCut aShortCut);
	virtual void __fastcall OnCommand(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData) = 0 ;
public:
	/* TAbstractSynPlugin.Destroy */ inline __fastcall virtual ~TAbstractSynHookerPlugin(void) { }
	
public:
	/* TComponent.Create */ inline __fastcall virtual TAbstractSynHookerPlugin(System::Classes::TComponent* AOwner) : TAbstractSynPlugin(AOwner) { }
	
};


enum DECLSPEC_DENUM TPluginState : unsigned char { psNone, psExecuting, psAccepting, psCancelling };

class PASCALIMPLEMENTATION TAbstractSynSingleHookPlugin : public TAbstractSynHookerPlugin
{
	typedef TAbstractSynHookerPlugin inherited;
	
private:
	Syneditkeycmds::TSynEditorCommand fCommandID;
	bool __fastcall IsShortCutStored(void);
	void __fastcall SetShortCut(const System::Classes::TShortCut Value);
	
protected:
	TPluginState fState;
	Synedit::TCustomSynEdit* fCurrentEditor;
	System::Classes::TShortCut fShortCut;
	__classmethod virtual System::Classes::TShortCut __fastcall DefaultShortCut();
	virtual void __fastcall DoAddEditor(Synedit::TCustomSynEdit* aEditor);
	virtual void __fastcall DoRemoveEditor(Synedit::TCustomSynEdit* aEditor);
	virtual void __fastcall DoExecute(void) = 0 ;
	virtual void __fastcall DoAccept(void) = 0 ;
	virtual void __fastcall DoCancel(void) = 0 ;
	
public:
	__fastcall virtual TAbstractSynSingleHookPlugin(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAbstractSynSingleHookPlugin(void);
	__property Syneditkeycmds::TSynEditorCommand CommandID = {read=fCommandID, nodefault};
	__property Synedit::TCustomSynEdit* CurrentEditor = {read=fCurrentEditor};
	bool __fastcall Executing(void);
	void __fastcall Execute(Synedit::TCustomSynEdit* aEditor);
	void __fastcall Accept(void);
	void __fastcall Cancel(void);
	
__published:
	__property System::Classes::TShortCut ShortCut = {read=fShortCut, write=SetShortCut, stored=IsShortCutStored, nodefault};
};


class PASCALIMPLEMENTATION TAbstractSynCompletion : public TAbstractSynSingleHookPlugin
{
	typedef TAbstractSynSingleHookPlugin inherited;
	
protected:
	System::UnicodeString fCurrentString;
	virtual void __fastcall SetCurrentString(const System::UnicodeString Value);
	virtual void __fastcall OnCommand(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &AChar, void * Data, void * HandlerData);
	virtual void __fastcall DoExecute(void);
	virtual void __fastcall DoAccept(void);
	virtual void __fastcall DoCancel(void);
	virtual System::UnicodeString __fastcall GetCurrentEditorString(void);
	
public:
	HIDESBASE void __fastcall AddEditor(Synedit::TCustomSynEdit* aEditor);
	__property System::UnicodeString CurrentString = {read=fCurrentString, write=SetCurrentString};
public:
	/* TAbstractSynSingleHookPlugin.Create */ inline __fastcall virtual TAbstractSynCompletion(System::Classes::TComponent* aOwner) : TAbstractSynSingleHookPlugin(aOwner) { }
	/* TAbstractSynSingleHookPlugin.Destroy */ inline __fastcall virtual ~TAbstractSynCompletion(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Syneditkeycmds::TSynEditorCommand __fastcall NewPluginCommand(void);
extern DELPHI_PACKAGE void __fastcall ReleasePluginCommand(Syneditkeycmds::TSynEditorCommand aCmd);
}	/* namespace Syneditplugins */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNEDITPLUGINS)
using namespace Syneditplugins;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyneditpluginsHPP
