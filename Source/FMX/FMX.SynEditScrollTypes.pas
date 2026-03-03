{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditScrollTypes;

{ FMX scrollbar interface, formerly FMX.SynEditTypes.pas.

  Renamed to FMX.SynEditScrollTypes for the same reason as the VCL
  counterpart: a bare 'SynEditTypes' in a uses clause shadows any
  dotted-prefix variant under Delphi's scope resolution rules.
  Contains only ISynEditScrollBars, so 'ScrollTypes' better describes
  the actual content. }

{$I SynEdit.inc}

interface

uses
  System.Classes,
  System.Types,
  System.UITypes,
  FMX.Types,
  SynEditTypes;

type
  { FMX scrollbar interface - uses FMX events instead of WM_SCROLL }
  ISynEditScrollBars = interface
    ['{F3A7B2C1-D4E5-4F60-A1B2-C3D4E5F67890}']
    function UpdateScrollBars: Boolean;
    function GetIsScrolling: Boolean;
    function GetVisibleVScrollBarWidth: Single;
    function GetVisibleHScrollBarHeight: Single;
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPointF);
    property IsScrolling: Boolean read GetIsScrolling;
  end;

implementation

end.
