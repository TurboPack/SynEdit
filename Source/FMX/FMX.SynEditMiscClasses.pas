{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditMiscClasses;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditKeyConst;

type
  { Notification event chain - matches VCL version interface }
  TSynNotifyEventChain = class(TComponent)
  private
    FClients: TList;
    FSender: TObject;
  public
    constructor CreateEx(AOwner: TComponent);
    destructor Destroy; override;
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    procedure Fire;
    property Sender: TObject read FSender write FSender;
  end;

  { Selected text color }
  TSynSelectedColor = class(TPersistent)
  private
    FBG: TColor;
    FFG: TColor;
    FOnChange: TNotifyEvent;
    FOpacity: Byte;
    FFillWholeLines: Boolean;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
    procedure SetOpacity(Value: Byte);
    procedure SetFillWholeLines(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Background: TColor read FBG write SetBG default clHighlight;
    property Foreground: TColor read FFG write SetFG default clHighlightText;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property FillWholeLines: Boolean read FFillWholeLines write SetFillWholeLines
      default True;
  end;

  { Search engine interface - matches VCL version }
  TSynEditSearchCustom = class(TComponent)
  protected
    function GetPattern: string; virtual; abstract;
    procedure SetPattern(const Value: string); virtual; abstract;
    function GetLength(Index: Integer): Integer; virtual; abstract;
    function GetResult(Index: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure SetOptions(const Value: TSynSearchOptions); virtual; abstract;
  public
    function FindAll(const NewText: string): Integer; virtual; abstract;
    function Replace(const aOccurrence, aReplacement: string): string;
      virtual; abstract;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[Index: Integer]: Integer read GetResult;
    property Lengths[Index: Integer]: Integer read GetLength;
    property Options: TSynSearchOptions write SetOptions;
  end;

implementation

{ TSynNotifyEventChain }

constructor TSynNotifyEventChain.CreateEx(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
end;

destructor TSynNotifyEventChain.Destroy;
begin
  FClients.Free;
  inherited;
end;

procedure TSynNotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  FClients.Add(TMethod(AEvent).Code);
  FClients.Add(TMethod(AEvent).Data);
end;

procedure TSynNotifyEventChain.Remove(AEvent: TNotifyEvent);
var
  I: Integer;
begin
  I := 0;
  while I < FClients.Count - 1 do
  begin
    if (FClients[I] = TMethod(AEvent).Code) and
       (FClients[I + 1] = TMethod(AEvent).Data) then
    begin
      FClients.Delete(I);
      FClients.Delete(I);
      Break;
    end;
    Inc(I, 2);
  end;
end;

procedure TSynNotifyEventChain.Fire;
var
  I: Integer;
  Event: TNotifyEvent;
begin
  I := 0;
  while I < FClients.Count - 1 do
  begin
    TMethod(Event).Code := FClients[I];
    TMethod(Event).Data := FClients[I + 1];
    Event(FSender);
    Inc(I, 2);
  end;
end;

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited;
  FBG := clHighlight;
  FFG := clHighlightText;
  FOpacity := 255;
  FFillWholeLines := True;
end;

procedure TSynSelectedColor.Assign(Source: TPersistent);
begin
  if Source is TSynSelectedColor then
  begin
    FBG := TSynSelectedColor(Source).FBG;
    FFG := TSynSelectedColor(Source).FFG;
    FOpacity := TSynSelectedColor(Source).FOpacity;
    FFillWholeLines := TSynSelectedColor(Source).FFillWholeLines;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end
  else
    inherited;
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if FBG <> Value then
  begin
    FBG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if FFG <> Value then
  begin
    FFG := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetOpacity(Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFillWholeLines(const Value: Boolean);
begin
  if FFillWholeLines <> Value then
  begin
    FFillWholeLines := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

end.
