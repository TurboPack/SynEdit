unit Vcl.SynEditDragDrop;
{
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

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

   OLE Drag and Drop components and routines
   Based on Grahame Marsh's code for UNDO
}

interface

Uses
  Windows, ActiveX, SysUtils, Classes, Messages, Controls, Forms, ExtCtrls,
  SynEditDragDropWin;

// Re-export shared drop effect constants (API contract preserved)
const
  deNone   = SynEditDragDropWin.deNone;
  deMove   = SynEditDragDropWin.deMove;
  deCopy   = SynEditDragDropWin.deCopy;
  deLink   = SynEditDragDropWin.deLink;
  deScroll = SynEditDragDropWin.deScroll;

type
  // Re-export shared type (API contract preserved)
  TSynDragSource = SynEditDragDropWin.TSynDragSource;

  TOnDragEvent = procedure(Sender: TObject; DataObject: IDataObject; State: TShiftState; MousePt: TPoint; var Effect: LongInt; var Result: HResult) of object;
  TOnDragOverEvent = procedure(Sender: TObject; State: TShiftState; MousePt: TPoint; var Effect: LongInt; var Result: HResult) of object;
  TOnDragLeaveEvent = procedure(Sender: TObject; var Result: HResult) of Object;

  // Implementation of the IDropTarget interface
  TSynDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FOnDragEnter: TOnDragEvent;
    FOnDragOver: TOnDragOverEvent;
    FOnDragLeave: TOnDragLeaveEvent;
    FOnDrop: TOnDragEvent;
// IDropTarget
    function DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; overload; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; overload; stdcall;
    function DragLeave: HResult; overload; stdcall;
    function Drop(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; overload; stdcall;
  protected
    procedure DragEnter(DataObject: IDataObject; State: TShiftState; Pt: TPoint; var Effect: LongInt; var Result: HResult); overload;
    procedure DragOver(State: TShiftState; Pt: TPoint; var Effect: LongInt; var Result: HResult); overload;
    procedure DragLeave(var Result: HResult);overload;
    procedure Drop(DataObject: IDataObject; State: TShiftState; Pt: TPoint; var Effect: LongInt; var Result: HResult); overload;
  public
    property OnDragEnter: TOnDragEvent read FOnDragEnter write FOnDragEnter;
    property OnDragOver: TOnDragOverEvent read FOnDragOver write FOnDragOver;
    property OnDragLeave: TOnDragLeaveEvent read FOnDragLeave write FOnDragLeave;
    property OnDrop: TOnDragEvent read FOnDrop write FOnDrop;
  end;

implementation

{ TDropTarget }

function TSynDropTarget.DragEnter(const DataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: LongInt): HResult;
begin
  Result := S_OK;
  try
    DragEnter(DataObj, KeysToShiftState(grfKeyState), Pt, dwEffect, Result);
  except
    Result := E_UNEXPECTED;
  end
end;

function TSynDropTarget.DragLeave: HResult;
begin
  Result := S_OK;
  try
    DragLeave(Result)
  except
    Result := E_UNEXPECTED;
  end
end;

function TSynDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: LongInt): HResult;
begin
  Result := S_OK;
  try
    DragOver(KeysToShiftState(grfKeyState), Pt, dwEffect, Result);
  except
    Result := E_UNEXPECTED;
  end
end;

function TSynDropTarget.Drop(const DataObj: IDataObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: LongInt): HResult;
begin
  Result := S_OK;
  try
    Drop(DataObj, KeysToShiftState(grfKeyState), Pt, dwEffect, Result);
  except
    Result := E_UNEXPECTED;
  end
end;

procedure TSynDropTarget.DragEnter(DataObject: IDataObject;
  State: TShiftState; Pt: TPoint; var Effect: LongInt; var Result: HResult);
begin
  Effect := StandardEffect(State);
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Self, DataObject, State, Pt, Effect, Result);
end;

procedure TSynDropTarget.DragLeave(var Result: HResult);
begin
  if Assigned(FOnDragLeave) then
    FOnDragLeave(Self, Result)
end;

procedure TSynDropTarget.DragOver(State: TShiftState;
  Pt: TPoint; var Effect: LongInt; var Result: HResult);
begin
  Effect := StandardEffect(State);
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, State, Pt, Effect, Result)
end;

procedure TSynDropTarget.Drop(DataObject: IDataObject; State: TShiftState;
  Pt: TPoint; var Effect: LongInt; var Result: HResult);
begin
  Effect := StandardEffect(State);
  if Assigned(FOnDrop) then
    FOnDrop(Self, DataObject, State, Pt, Effect, Result)
end;


end.
