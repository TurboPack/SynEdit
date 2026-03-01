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

unit SynEditKeyConstShared;

{ Platform-independent key constants for SynEdit.
  Values match standard virtual key codes.

  Named with the 'Shared' suffix because Delphi's scope resolution rules
  make a bare 'SynEditKeyConst' shadow the prefixed 'Vcl.SynEditKeyConst'
  and 'FMX.SynEditKeyConst' units.  The suffix ensures that shared code
  references this unit unambiguously, while VCL/FMX code can still use
  the prefixed wrappers. }

{$I SynEdit.inc}

interface

const
  SYNEDIT_RETURN    = $0D;
  SYNEDIT_ESCAPE    = $1B;
  SYNEDIT_SPACE     = $20;
  SYNEDIT_PRIOR     = $21;
  SYNEDIT_NEXT      = $22;
  SYNEDIT_END       = $23;
  SYNEDIT_HOME      = $24;
  SYNEDIT_UP        = $26;
  SYNEDIT_DOWN      = $28;
  SYNEDIT_BACK      = $08;
  SYNEDIT_LEFT      = $25;
  SYNEDIT_RIGHT     = $27;
  SYNEDIT_MENU      = $12;
  SYNEDIT_CONTROL   = $11;
  SYNEDIT_SHIFT     = $10;
  SYNEDIT_F1        = $70;
  SYNEDIT_F2        = $71;
  SYNEDIT_F3        = $72;
  SYNEDIT_F4        = $73;
  SYNEDIT_F5        = $74;
  SYNEDIT_F6        = $75;
  SYNEDIT_F7        = $76;
  SYNEDIT_F8        = $77;
  SYNEDIT_F9        = $78;
  SYNEDIT_F10       = $79;
  SYNEDIT_F11       = $7A;
  SYNEDIT_F12       = $7B;
  SYNEDIT_F13       = $7C;
  SYNEDIT_F14       = $7D;
  SYNEDIT_F15       = $7E;
  SYNEDIT_F16       = $7F;
  SYNEDIT_F17       = $80;
  SYNEDIT_F18       = $81;
  SYNEDIT_F19       = $82;
  SYNEDIT_F20       = $83;
  SYNEDIT_F21       = $84;
  SYNEDIT_F22       = $85;
  SYNEDIT_F23       = $86;
  SYNEDIT_F24       = $87;
  SYNEDIT_PRINT     = $2A;
  SYNEDIT_INSERT    = $2D;
  SYNEDIT_DELETE    = $2E;
  SYNEDIT_NUMPAD0   = $60;
  SYNEDIT_NUMPAD1   = $61;
  SYNEDIT_NUMPAD2   = $62;
  SYNEDIT_NUMPAD3   = $63;
  SYNEDIT_NUMPAD4   = $64;
  SYNEDIT_NUMPAD5   = $65;
  SYNEDIT_NUMPAD6   = $66;
  SYNEDIT_NUMPAD7   = $67;
  SYNEDIT_NUMPAD8   = $68;
  SYNEDIT_NUMPAD9   = $69;
  SYNEDIT_MULTIPLY  = $6A;
  SYNEDIT_ADD       = $6B;
  SYNEDIT_SEPARATOR = $6C;
  SYNEDIT_SUBTRACT  = $6D;
  SYNEDIT_DECIMAL   = $6E;
  SYNEDIT_DIVIDE    = $6F;
  SYNEDIT_NUMLOCK   = $90;
  SYNEDIT_SCROLL    = $91;
  SYNEDIT_TAB       = $09;
  SYNEDIT_CLEAR     = $0C;
  SYNEDIT_PAUSE     = $13;
  SYNEDIT_CAPITAL   = $14;

implementation

end.
