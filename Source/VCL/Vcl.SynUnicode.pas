{-------------------------------------------------------------------------------
VCL-specific clipboard operations for SynEdit.

These functions were extracted from SynUnicode.pas to keep the shared
package free of VCL dependencies.
-------------------------------------------------------------------------------}

unit Vcl.SynUnicode;

{$I SynEdit.inc}

interface

function ClipboardProvidesText: Boolean;
function GetClipboardText: string;
procedure SetClipboardText(const Text: string);

implementation

uses
  Winapi.Windows,
  Vcl.Clipbrd;

function ClipboardProvidesText: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_UNICODETEXT);
end;

function GetClipboardText: string;
begin
  Result := Clipboard.AsText;
end;

procedure SetClipboardText(const Text: string);
begin
  Clipboard.AsText := Text;
end;

end.
