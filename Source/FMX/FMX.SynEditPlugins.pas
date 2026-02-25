{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditPlugins;

{$I SynEdit.inc}

interface

uses
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  FMX.Graphics;

type
  TCustomFMXSynEdit = class;

  TPlugInHandler = (phLinesInserted, phLinesDeleted, phLinePut, phAfterPaint);
  TPlugInHandlers = set of TPlugInHandler;

  TSynFMXEditPlugin = class(TObject)
  private
    FOwner: TCustomFMXSynEdit;
    FHandlers: TPlugInHandlers;
  public
    constructor Create(AOwner: TCustomFMXSynEdit); virtual;
    destructor Destroy; override;
    procedure AfterPaint(Canvas: TCanvas; const AClip: TRectF;
      FirstLine, LastLine: Integer); virtual;
    procedure LinesInserted(FirstLine, Count: Integer); virtual;
    procedure LinesDeleted(FirstLine, Count: Integer); virtual;
    procedure LinePut(aIndex: Integer; const OldLine: string); virtual;
    property Owner: TCustomFMXSynEdit read FOwner;
    property Handlers: TPlugInHandlers read FHandlers;
  end;

implementation

uses
  FMX.SynEdit;

{ TSynFMXEditPlugin }

constructor TSynFMXEditPlugin.Create(AOwner: TCustomFMXSynEdit);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TSynFMXEditPlugin.Destroy;
begin
  inherited;
end;

procedure TSynFMXEditPlugin.AfterPaint(Canvas: TCanvas; const AClip: TRectF;
  FirstLine, LastLine: Integer);
begin
end;

procedure TSynFMXEditPlugin.LinesInserted(FirstLine, Count: Integer);
begin
end;

procedure TSynFMXEditPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
end;

procedure TSynFMXEditPlugin.LinePut(aIndex: Integer; const OldLine: string);
begin
end;

end.
