{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
All Rights Reserved.

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
-------------------------------------------------------------------------------}

unit Vcl.SynEditKeyConst;

{ VCL key-constant facade.  Re-exports every constant from
  SynEditKeyConstShared so that VCL code using 'Vcl.SynEditKeyConst'
  continues to compile unchanged.  The actual definitions live in the
  shared unit; this wrapper exists only to keep the Vcl.* namespace
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
  SYNEDIT_F13       = SynEditKeyConstShared.SYNEDIT_F13;
  SYNEDIT_F14       = SynEditKeyConstShared.SYNEDIT_F14;
  SYNEDIT_F15       = SynEditKeyConstShared.SYNEDIT_F15;
  SYNEDIT_F16       = SynEditKeyConstShared.SYNEDIT_F16;
  SYNEDIT_F17       = SynEditKeyConstShared.SYNEDIT_F17;
  SYNEDIT_F18       = SynEditKeyConstShared.SYNEDIT_F18;
  SYNEDIT_F19       = SynEditKeyConstShared.SYNEDIT_F19;
  SYNEDIT_F20       = SynEditKeyConstShared.SYNEDIT_F20;
  SYNEDIT_F21       = SynEditKeyConstShared.SYNEDIT_F21;
  SYNEDIT_F22       = SynEditKeyConstShared.SYNEDIT_F22;
  SYNEDIT_F23       = SynEditKeyConstShared.SYNEDIT_F23;
  SYNEDIT_F24       = SynEditKeyConstShared.SYNEDIT_F24;
  SYNEDIT_PRINT     = SynEditKeyConstShared.SYNEDIT_PRINT;
  SYNEDIT_INSERT    = SynEditKeyConstShared.SYNEDIT_INSERT;
  SYNEDIT_DELETE    = SynEditKeyConstShared.SYNEDIT_DELETE;
  SYNEDIT_NUMPAD0   = SynEditKeyConstShared.SYNEDIT_NUMPAD0;
  SYNEDIT_NUMPAD1   = SynEditKeyConstShared.SYNEDIT_NUMPAD1;
  SYNEDIT_NUMPAD2   = SynEditKeyConstShared.SYNEDIT_NUMPAD2;
  SYNEDIT_NUMPAD3   = SynEditKeyConstShared.SYNEDIT_NUMPAD3;
  SYNEDIT_NUMPAD4   = SynEditKeyConstShared.SYNEDIT_NUMPAD4;
  SYNEDIT_NUMPAD5   = SynEditKeyConstShared.SYNEDIT_NUMPAD5;
  SYNEDIT_NUMPAD6   = SynEditKeyConstShared.SYNEDIT_NUMPAD6;
  SYNEDIT_NUMPAD7   = SynEditKeyConstShared.SYNEDIT_NUMPAD7;
  SYNEDIT_NUMPAD8   = SynEditKeyConstShared.SYNEDIT_NUMPAD8;
  SYNEDIT_NUMPAD9   = SynEditKeyConstShared.SYNEDIT_NUMPAD9;
  SYNEDIT_MULTIPLY  = SynEditKeyConstShared.SYNEDIT_MULTIPLY;
  SYNEDIT_ADD       = SynEditKeyConstShared.SYNEDIT_ADD;
  SYNEDIT_SEPARATOR = SynEditKeyConstShared.SYNEDIT_SEPARATOR;
  SYNEDIT_SUBTRACT  = SynEditKeyConstShared.SYNEDIT_SUBTRACT;
  SYNEDIT_DECIMAL   = SynEditKeyConstShared.SYNEDIT_DECIMAL;
  SYNEDIT_DIVIDE    = SynEditKeyConstShared.SYNEDIT_DIVIDE;
  SYNEDIT_NUMLOCK   = SynEditKeyConstShared.SYNEDIT_NUMLOCK;
  SYNEDIT_SCROLL    = SynEditKeyConstShared.SYNEDIT_SCROLL;
  SYNEDIT_TAB       = SynEditKeyConstShared.SYNEDIT_TAB;
  SYNEDIT_CLEAR     = SynEditKeyConstShared.SYNEDIT_CLEAR;
  SYNEDIT_PAUSE     = SynEditKeyConstShared.SYNEDIT_PAUSE;
  SYNEDIT_CAPITAL   = SynEditKeyConstShared.SYNEDIT_CAPITAL;

implementation

end.
