{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

unit FMX.SynEditKeyConst;

{ FMX key constant mappings - maps FMX virtual key codes to SynEdit constants.
  FMX uses the same virtual key code values as Windows. }

{$I SynEdit.inc}

interface

uses
  System.UITypes,
  FMX.Types;

const
  SYNEDIT_RETURN    = vkReturn;
  SYNEDIT_ESCAPE    = vkEscape;
  SYNEDIT_SPACE     = vkSpace;
  SYNEDIT_PRIOR     = vkPrior;
  SYNEDIT_NEXT      = vkNext;
  SYNEDIT_END       = vkEnd;
  SYNEDIT_HOME      = vkHome;
  SYNEDIT_UP        = vkUp;
  SYNEDIT_DOWN      = vkDown;
  SYNEDIT_BACK      = vkBack;
  SYNEDIT_LEFT      = vkLeft;
  SYNEDIT_RIGHT     = vkRight;
  SYNEDIT_MENU      = vkMenu;
  SYNEDIT_CONTROL   = vkControl;
  SYNEDIT_SHIFT     = vkShift;
  SYNEDIT_F1        = vkF1;
  SYNEDIT_F2        = vkF2;
  SYNEDIT_F3        = vkF3;
  SYNEDIT_F4        = vkF4;
  SYNEDIT_F5        = vkF5;
  SYNEDIT_F6        = vkF6;
  SYNEDIT_F7        = vkF7;
  SYNEDIT_F8        = vkF8;
  SYNEDIT_F9        = vkF9;
  SYNEDIT_F10       = vkF10;
  SYNEDIT_F11       = vkF11;
  SYNEDIT_F12       = vkF12;
  SYNEDIT_INSERT    = vkInsert;
  SYNEDIT_DELETE    = vkDelete;
  SYNEDIT_NUMPAD0   = vkNumpad0;
  SYNEDIT_ADD       = vkAdd;
  SYNEDIT_SUBTRACT  = vkSubtract;
  SYNEDIT_TAB       = vkTab;
  SYNEDIT_CLEAR     = vkClear;
  SYNEDIT_PAUSE     = vkPause;
  SYNEDIT_CAPITAL   = vkCapital;

implementation

end.
