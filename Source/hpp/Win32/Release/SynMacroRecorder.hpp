// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynMacroRecorder.pas' rev: 29.00 (Windows)

#ifndef SynmacrorecorderHPP
#define SynmacrorecorderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Menus.hpp>
#include <SynEdit.hpp>
#include <SynEditKeyCmds.hpp>
#include <SynEditPlugins.hpp>
#include <SynEditTypes.hpp>
#include <SynUnicode.hpp>
#include <System.WideStrUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Synmacrorecorder
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TSynMacroEvent;
class DELPHICLASS TSynBasicEvent;
class DELPHICLASS TSynCharEvent;
class DELPHICLASS TSynStringEvent;
class DELPHICLASS TSynPositionEvent;
class DELPHICLASS TSynDataEvent;
class DELPHICLASS TCustomSynMacroRecorder;
class DELPHICLASS TSynMacroRecorder;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSynMacroState : unsigned char { msStopped, msRecording, msPlaying, msPaused };

enum DECLSPEC_DENUM TSynMacroCommand : unsigned char { mcRecord, mcPlayback };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynMacroEvent : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::Byte fRepeatCount;
	virtual System::UnicodeString __fastcall GetAsString(void) = 0 ;
	virtual void __fastcall InitEventParameters(System::UnicodeString aStr) = 0 ;
	
public:
	__fastcall virtual TSynMacroEvent(void);
	virtual void __fastcall Initialize(Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData) = 0 ;
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream) = 0 ;
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream) = 0 ;
	virtual void __fastcall Playback(Synedit::TCustomSynEdit* aEditor) = 0 ;
	__property System::UnicodeString AsString = {read=GetAsString};
	__property System::Byte RepeatCount = {read=fRepeatCount, write=fRepeatCount, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSynMacroEvent(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynBasicEvent : public TSynMacroEvent
{
	typedef TSynMacroEvent inherited;
	
protected:
	Syneditkeycmds::TSynEditorCommand fCommand;
	virtual System::UnicodeString __fastcall GetAsString(void);
	virtual void __fastcall InitEventParameters(System::UnicodeString aStr);
	
public:
	virtual void __fastcall Initialize(Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
	virtual void __fastcall Playback(Synedit::TCustomSynEdit* aEditor);
	__property Syneditkeycmds::TSynEditorCommand Command = {read=fCommand, write=fCommand, nodefault};
public:
	/* TSynMacroEvent.Create */ inline __fastcall virtual TSynBasicEvent(void) : TSynMacroEvent() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSynBasicEvent(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynCharEvent : public TSynMacroEvent
{
	typedef TSynMacroEvent inherited;
	
protected:
	System::WideChar fKey;
	virtual System::UnicodeString __fastcall GetAsString(void);
	virtual void __fastcall InitEventParameters(System::UnicodeString aStr);
	
public:
	virtual void __fastcall Initialize(Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
	virtual void __fastcall Playback(Synedit::TCustomSynEdit* aEditor);
	__property System::WideChar Key = {read=fKey, write=fKey, nodefault};
public:
	/* TSynMacroEvent.Create */ inline __fastcall virtual TSynCharEvent(void) : TSynMacroEvent() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSynCharEvent(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynStringEvent : public TSynMacroEvent
{
	typedef TSynMacroEvent inherited;
	
protected:
	System::UnicodeString fString;
	virtual System::UnicodeString __fastcall GetAsString(void);
	virtual void __fastcall InitEventParameters(System::UnicodeString aStr);
	
public:
	virtual void __fastcall Initialize(Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
	virtual void __fastcall Playback(Synedit::TCustomSynEdit* aEditor);
	__property System::UnicodeString Value = {read=fString, write=fString};
public:
	/* TSynMacroEvent.Create */ inline __fastcall virtual TSynStringEvent(void) : TSynMacroEvent() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSynStringEvent(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynPositionEvent : public TSynBasicEvent
{
	typedef TSynBasicEvent inherited;
	
protected:
	Synedittypes::TBufferCoord fPosition;
	virtual System::UnicodeString __fastcall GetAsString(void);
	virtual void __fastcall InitEventParameters(System::UnicodeString aStr);
	
public:
	virtual void __fastcall Initialize(Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
	virtual void __fastcall Playback(Synedit::TCustomSynEdit* aEditor);
	__property Synedittypes::TBufferCoord Position = {read=fPosition, write=fPosition};
public:
	/* TSynMacroEvent.Create */ inline __fastcall virtual TSynPositionEvent(void) : TSynBasicEvent() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSynPositionEvent(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSynDataEvent : public TSynBasicEvent
{
	typedef TSynBasicEvent inherited;
	
protected:
	void *fData;
	
public:
	virtual void __fastcall Initialize(Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* aStream);
	virtual void __fastcall SaveToStream(System::Classes::TStream* aStream);
	virtual void __fastcall Playback(Synedit::TCustomSynEdit* aEditor);
public:
	/* TSynMacroEvent.Create */ inline __fastcall virtual TSynDataEvent(void) : TSynBasicEvent() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSynDataEvent(void) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TSynUserCommandEvent)(TCustomSynMacroRecorder* aSender, Syneditkeycmds::TSynEditorCommand aCmd, TSynMacroEvent* &aEvent);

class PASCALIMPLEMENTATION TCustomSynMacroRecorder : public Syneditplugins::TAbstractSynHookerPlugin
{
	typedef Syneditplugins::TAbstractSynHookerPlugin inherited;
	
private:
	System::StaticArray<System::Classes::TShortCut, 2> fShortCuts;
	System::Classes::TNotifyEvent fOnStateChange;
	TSynUserCommandEvent fOnUserCommand;
	System::UnicodeString fMacroName;
	bool fSaveMarkerPos;
	TSynMacroEvent* __fastcall GetEvent(int aIndex);
	int __fastcall GetEventCount(void);
	System::UnicodeString __fastcall GetAsString(void);
	Syneditkeycmds::TSynEditorCommand __fastcall GetPlaybackCommandID(void);
	System::Classes::TShortCut __fastcall GetPlaybackShortCut(const int Index);
	Syneditkeycmds::TSynEditorCommand __fastcall GetRecordCommandID(void);
	System::Classes::TShortCut __fastcall GetRecordShortCut(const int Index);
	void __fastcall SetAsString(const System::UnicodeString Value);
	
protected:
	Synedit::TCustomSynEdit* fCurrentEditor;
	TSynMacroState fState;
	System::Classes::TList* fEvents;
	System::StaticArray<Syneditkeycmds::TSynEditorCommand, 2> fCommandIDs;
	void __fastcall SetShortCut(const int Index, const System::Classes::TShortCut Value);
	bool __fastcall GetIsEmpty(void);
	void __fastcall StateChanged(void);
	virtual void __fastcall DoAddEditor(Synedit::TCustomSynEdit* aEditor);
	virtual void __fastcall DoRemoveEditor(Synedit::TCustomSynEdit* aEditor);
	virtual void __fastcall OnCommand(System::TObject* Sender, bool AfterProcessing, bool &Handled, Syneditkeycmds::TSynEditorCommand &Command, System::WideChar &aChar, void * Data, void * HandlerData);
	TSynMacroEvent* __fastcall CreateMacroEvent(Syneditkeycmds::TSynEditorCommand aCmd);
	__property Syneditkeycmds::TSynEditorCommand RecordCommandID = {read=GetRecordCommandID, nodefault};
	__property Syneditkeycmds::TSynEditorCommand PlaybackCommandID = {read=GetPlaybackCommandID, nodefault};
	
public:
	__fastcall virtual TCustomSynMacroRecorder(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TCustomSynMacroRecorder(void);
	void __fastcall Error(const System::UnicodeString aMsg);
	HIDESBASE void __fastcall AddEditor(Synedit::TCustomSynEdit* aEditor);
	HIDESBASE void __fastcall RemoveEditor(Synedit::TCustomSynEdit* aEditor);
	void __fastcall RecordMacro(Synedit::TCustomSynEdit* aEditor);
	void __fastcall PlaybackMacro(Synedit::TCustomSynEdit* aEditor);
	void __fastcall Stop(void);
	void __fastcall Pause(void);
	void __fastcall Resume(void);
	__property bool IsEmpty = {read=GetIsEmpty, nodefault};
	__property TSynMacroState State = {read=fState, nodefault};
	void __fastcall Clear(void);
	void __fastcall AddEvent(Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData);
	void __fastcall InsertEvent(int aIndex, Syneditkeycmds::TSynEditorCommand aCmd, System::WideChar aChar, void * aData);
	void __fastcall AddCustomEvent(TSynMacroEvent* aEvent);
	void __fastcall InsertCustomEvent(int aIndex, TSynMacroEvent* aEvent);
	void __fastcall DeleteEvent(int aIndex);
	void __fastcall LoadFromStream(System::Classes::TStream* aSrc);
	void __fastcall LoadFromStreamEx(System::Classes::TStream* aSrc, bool aClear);
	void __fastcall SaveToStream(System::Classes::TStream* aDest);
	void __fastcall LoadFromFile(System::UnicodeString aFilename);
	void __fastcall SaveToFile(System::UnicodeString aFilename);
	__property int EventCount = {read=GetEventCount, nodefault};
	__property TSynMacroEvent* Events[int aIndex] = {read=GetEvent};
	__property System::Classes::TShortCut RecordShortCut = {read=GetRecordShortCut, write=SetShortCut, index=0, nodefault};
	__property System::Classes::TShortCut PlaybackShortCut = {read=GetPlaybackShortCut, write=SetShortCut, index=1, nodefault};
	__property bool SaveMarkerPos = {read=fSaveMarkerPos, write=fSaveMarkerPos, default=0};
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	__property System::UnicodeString MacroName = {read=fMacroName, write=fMacroName};
	__property System::Classes::TNotifyEvent OnStateChange = {read=fOnStateChange, write=fOnStateChange};
	__property TSynUserCommandEvent OnUserCommand = {read=fOnUserCommand, write=fOnUserCommand};
};


class PASCALIMPLEMENTATION TSynMacroRecorder : public TCustomSynMacroRecorder
{
	typedef TCustomSynMacroRecorder inherited;
	
__published:
	__property SaveMarkerPos = {default=0};
	__property RecordShortCut;
	__property PlaybackShortCut;
	__property OnStateChange;
	__property OnUserCommand;
public:
	/* TCustomSynMacroRecorder.Create */ inline __fastcall virtual TSynMacroRecorder(System::Classes::TComponent* aOwner) : TCustomSynMacroRecorder(aOwner) { }
	/* TCustomSynMacroRecorder.Destroy */ inline __fastcall virtual ~TSynMacroRecorder(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _sCannotRecord;
#define Synmacrorecorder_sCannotRecord System::LoadResourceString(&Synmacrorecorder::_sCannotRecord)
extern DELPHI_PACKAGE System::ResourceString _sCannotPlay;
#define Synmacrorecorder_sCannotPlay System::LoadResourceString(&Synmacrorecorder::_sCannotPlay)
extern DELPHI_PACKAGE System::ResourceString _sCannotPause;
#define Synmacrorecorder_sCannotPause System::LoadResourceString(&Synmacrorecorder::_sCannotPause)
extern DELPHI_PACKAGE System::ResourceString _sCannotResume;
#define Synmacrorecorder_sCannotResume System::LoadResourceString(&Synmacrorecorder::_sCannotResume)
}	/* namespace Synmacrorecorder */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNMACRORECORDER)
using namespace Synmacrorecorder;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SynmacrorecorderHPP
