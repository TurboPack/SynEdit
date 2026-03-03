{-------------------------------------------------------------------------------
TurboPack SynEdit - FMX Edition

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Re-exports plugin types from FMX.SynEdit for backward compatibility.
-------------------------------------------------------------------------------}

unit FMX.SynEditPlugins;

{$I SynEdit.inc}

interface

uses
  FMX.SynEdit;

type
  TFMXEditPlugin = TSynFMXEditPlugin;

implementation

end.
