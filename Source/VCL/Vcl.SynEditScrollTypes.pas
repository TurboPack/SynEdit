{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTypes.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Unicode translation by Ma�l H�rz.
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

VCL scrollbar interface, formerly Vcl.SynEditTypes.pas.

Renamed to Vcl.SynEditScrollTypes to resolve a name collision: a bare
'SynEditTypes' in a uses clause matches the shared SynEditTypes.pas
(exact match) and shadows this Vcl-prefixed unit.  Renaming the shared
side would require updating 110+ files and re-aliasing enum types (which
Delphi does not support), so the VCL/FMX side was renamed instead.
The unit contains only ISynEditScrollBars, so 'ScrollTypes' is a more
accurate name anyway.
-------------------------------------------------------------------------------}

unit Vcl.SynEditScrollTypes;

{$I SynEdit.inc}

interface

uses
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Controls,
  SynEditTypes;

type
  ISynEditScrollBars = interface
    function UpdateScrollBars: Boolean;
    function GetIsScrolling: Boolean;
    procedure WMHScroll(var AMsg: TWMScroll);
    procedure WMVScroll(var AMsg: TWMScroll);
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint);
    property IsScrolling: Boolean read GetIsScrolling;
  end;

implementation

end.
