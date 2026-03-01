{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditKeyConst;

{ FMX key-constant facade.  Re-exports constants from
  SynEditKeyConstShared so that FMX code using 'FMX.SynEditKeyConst'
  continues to compile unchanged.  The actual definitions live in the
  shared unit; this wrapper exists only to keep the FMX.* namespace
  populated for discoverability and IDE code-completion. }

{$I SynEdit.inc}

interface

uses
  SynEditKeyConstShared;

const
  SYNEDIT_RETURN    = SynEditKeyConstShared.SYNEDIT_RETURN;
  SYNEDIT_ESCAPE    = SynEditKeyConstShared.SYNEDIT_ESCAPE;
  SYNEDIT_SPACE     = SynEditKeyConstShared.SYNEDIT_SPACE;
  SYNEDIT_PRIOR     = SynEditKeyConstShared.SYNEDIT_PRIOR;
  SYNEDIT_NEXT      = SynEditKeyConstShared.SYNEDIT_NEXT;
  SYNEDIT_END       = SynEditKeyConstShared.SYNEDIT_END;
  SYNEDIT_HOME      = SynEditKeyConstShared.SYNEDIT_HOME;
  SYNEDIT_UP        = SynEditKeyConstShared.SYNEDIT_UP;
  SYNEDIT_DOWN      = SynEditKeyConstShared.SYNEDIT_DOWN;
  SYNEDIT_BACK      = SynEditKeyConstShared.SYNEDIT_BACK;
  SYNEDIT_LEFT      = SynEditKeyConstShared.SYNEDIT_LEFT;
  SYNEDIT_RIGHT     = SynEditKeyConstShared.SYNEDIT_RIGHT;
  SYNEDIT_MENU      = SynEditKeyConstShared.SYNEDIT_MENU;
  SYNEDIT_CONTROL   = SynEditKeyConstShared.SYNEDIT_CONTROL;
  SYNEDIT_SHIFT     = SynEditKeyConstShared.SYNEDIT_SHIFT;
  SYNEDIT_F1        = SynEditKeyConstShared.SYNEDIT_F1;
  SYNEDIT_F2        = SynEditKeyConstShared.SYNEDIT_F2;
  SYNEDIT_F3        = SynEditKeyConstShared.SYNEDIT_F3;
  SYNEDIT_F4        = SynEditKeyConstShared.SYNEDIT_F4;
  SYNEDIT_F5        = SynEditKeyConstShared.SYNEDIT_F5;
  SYNEDIT_F6        = SynEditKeyConstShared.SYNEDIT_F6;
  SYNEDIT_F7        = SynEditKeyConstShared.SYNEDIT_F7;
  SYNEDIT_F8        = SynEditKeyConstShared.SYNEDIT_F8;
  SYNEDIT_F9        = SynEditKeyConstShared.SYNEDIT_F9;
  SYNEDIT_F10       = SynEditKeyConstShared.SYNEDIT_F10;
  SYNEDIT_F11       = SynEditKeyConstShared.SYNEDIT_F11;
  SYNEDIT_F12       = SynEditKeyConstShared.SYNEDIT_F12;
  SYNEDIT_INSERT    = SynEditKeyConstShared.SYNEDIT_INSERT;
  SYNEDIT_DELETE    = SynEditKeyConstShared.SYNEDIT_DELETE;
  SYNEDIT_NUMPAD0   = SynEditKeyConstShared.SYNEDIT_NUMPAD0;
  SYNEDIT_ADD       = SynEditKeyConstShared.SYNEDIT_ADD;
  SYNEDIT_SUBTRACT  = SynEditKeyConstShared.SYNEDIT_SUBTRACT;
  SYNEDIT_TAB       = SynEditKeyConstShared.SYNEDIT_TAB;
  SYNEDIT_CLEAR     = SynEditKeyConstShared.SYNEDIT_CLEAR;
  SYNEDIT_PAUSE     = SynEditKeyConstShared.SYNEDIT_PAUSE;
  SYNEDIT_CAPITAL   = SynEditKeyConstShared.SYNEDIT_CAPITAL;

implementation

end.
