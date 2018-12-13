// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'SynDBEdit.pas' rev: 33.00 (Windows)

#ifndef SyndbeditHPP
#define SyndbeditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.DBCtrls.hpp>
#include <SynEdit.hpp>
#include <SynEditKeyCmds.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Data.DB.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Menus.hpp>
#include <SynEditTypes.hpp>
#include <SynEditMiscClasses.hpp>
#include <Vcl.Forms.hpp>
#include <SynEditHighlighter.hpp>

//-- user supplied -----------------------------------------------------------

namespace Syndbedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomDBSynEdit;
class DELPHICLASS TDBSynEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomDBSynEdit : public Synedit::TCustomSynEdit
{
	typedef Synedit::TCustomSynEdit inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	bool fEditing;
	bool FBeginEdit;
	System::Classes::TNotifyEvent FLoadData;
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall EditingChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall SetEditing(bool Value);
	void __fastcall UpdateData(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Msg);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual bool __fastcall GetReadOnly();
	virtual void __fastcall Loaded();
	virtual void __fastcall DoChange();
	virtual void __fastcall SetReadOnly(bool Value);
	
public:
	__fastcall virtual TCustomDBSynEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomDBSynEdit();
	DYNAMIC void __fastcall DragDrop(System::TObject* Source, int X, int Y);
	virtual void __fastcall ExecuteCommand(Syneditkeycmds::TSynEditorCommand Command, System::WideChar AChar, void * Data);
	void __fastcall LoadMemo();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
protected:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property Data::Db::TField* Field = {read=GetField};
	__property System::Classes::TNotifyEvent OnLoadData = {read=FLoadData, write=FLoadData};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomDBSynEdit(HWND ParentWindow) : Synedit::TCustomSynEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TDBSynEdit : public TCustomDBSynEdit
{
	typedef TCustomDBSynEdit inherited;
	
__published:
	__property DataField = {default=0};
	__property DataSource;
	__property Field;
	__property OnLoadData;
	__property ActiveLineColor = {default=536870911};
	__property Align = {default=0};
	__property Anchors = {default=3};
	__property Constraints;
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property Enabled = {default=1};
	__property Font;
	__property Height;
	__property Name = {default=0};
	__property ParentColor = {default=1};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Tag = {default=0};
	__property Visible = {default=1};
	__property Width;
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDock;
	__property OnStartDrag;
	__property BookMarkOptions;
	__property BorderStyle = {default=1};
	__property ExtraLineSpacing = {default=0};
	__property Gutter;
	__property HideSelection = {default=0};
	__property Highlighter;
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property InsertCaret = {default=0};
	__property InsertMode = {default=1};
	__property Keystrokes;
	__property MaxScrollWidth = {default=1024};
	__property MaxUndo = {default=1024};
	__property Options = {default=40632722};
	__property OverwriteCaret = {default=3};
	__property ReadOnly = {default=0};
	__property RightEdge = {default=80};
	__property RightEdgeColor = {default=12632256};
	__property ScrollBars = {default=3};
	__property SearchEngine;
	__property SelectedColor;
	__property SelectionMode = {default=0};
	__property TabWidth = {default=8};
	__property WantTabs = {default=0};
	__property OnChange;
	__property OnCommandProcessed;
	__property OnDropFiles;
	__property OnGutterClick;
	__property OnGutterGetText;
	__property OnGutterPaint;
	__property OnPaint;
	__property OnPlaceBookmark;
	__property OnProcessCommand;
	__property OnProcessUserCommand;
	__property OnReplaceText;
	__property OnSpecialLineColors;
	__property OnStatusChange;
	__property OnPaintTransient;
public:
	/* TCustomDBSynEdit.Create */ inline __fastcall virtual TDBSynEdit(System::Classes::TComponent* AOwner) : TCustomDBSynEdit(AOwner) { }
	/* TCustomDBSynEdit.Destroy */ inline __fastcall virtual ~TDBSynEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TDBSynEdit(HWND ParentWindow) : TCustomDBSynEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Syndbedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYNDBEDIT)
using namespace Syndbedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SyndbeditHPP
