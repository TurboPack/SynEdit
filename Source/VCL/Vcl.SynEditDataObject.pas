unit Vcl.SynEditDataObject;
{
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

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

  This unit implements the IDataObject interface for Clipboard and Drag & Drop
  operations.
  Code based on Grahame Marsh's articles on OLE Drag & Drop
  published in UNDO (www.undo.com)
}

interface
uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  SynEditDragDropWin;

Type
  // Re-export shared type (API contract preserved)
  TSynEnumFormatEtc = SynEditDragDropWin.TSynEnumFormatEtc;

  TSynEditDataObject = class (TInterfacedObject, IDataObject)
  private
    FText: string;
    FInternalText: string;
    FFormatEtc: TList<TClipFormat>;
    HtmlStream: TMemoryStream;
    procedure StreamHTML(Editor: TObject; Stream: TStream);
  protected
    function GetData (const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; overload; stdcall;
    function GetDataHere (const formatetc: TFormatEtc; out medium: TStgMedium): HResult; overload; stdcall;
    function QueryGetData (const formatetc: TFormatEtc): HResult; overload; stdcall;
    function GetCanonicalFormatEtc (const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult; overload; stdcall;
    function SetData (const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult; overload; stdcall;
    function EnumFormatEtc (dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult; overload; stdcall;
    function DAdvise (const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult; overload; stdcall;
    function DUnadvise (dwConnection: Longint): HResult; overload; stdcall;
    function EnumDAdvise (out enumAdvise: IEnumStatData): HResult; overload; stdcall;
  public
    constructor Create(ASynEdit: TObject);
    destructor Destroy; override;
  end;

// Re-export shared functions (API contract preserved)
function MakeGlobal(const S: string): HGLOBAL; overload;
function MakeGlobal(var P; Size: Integer): HGLOBAL; overload;
function HasFormat(DataObject: IDataObject; Format: TClipFormat): Boolean;
function GetInternalClipText: TArray<string>;

// Re-export shared constant (API contract preserved)
const
  IntClipFormatDelimiter = SynEditDragDropWin.IntClipFormatDelimiter;

// Note: SynEditClipboardFormat and HTMLClipboardFormat vars are accessible
// directly via the SynEditDragDropWin unit (which is in the interface uses).

implementation

uses
  SynEdit,
  SynEditTypes,
  SynExportHTML;

// Re-export thin wrappers that delegate to shared unit
function MakeGlobal(const S: string): HGLOBAL;
begin
  Result := SynEditDragDropWin.MakeGlobal(S);
end;

function MakeGlobal(var P; Size: Integer): HGLOBAL;
begin
  Result := SynEditDragDropWin.MakeGlobal(P, Size);
end;

function HasFormat(DataObject: IDataObject; Format: TClipFormat): Boolean;
begin
  Result := SynEditDragDropWin.HasFormat(DataObject, Format);
end;

function GetInternalClipText: TArray<string>;
begin
  Result := SynEditDragDropWin.GetInternalClipText;
end;


constructor TSynEditDataObject.Create(ASynEdit: TObject);
var
  Ed: TCustomSynEdit;

  function DelimitedText(Delimiter: string): string;
  var
    Index: Integer;
    Sel: TSynSelection;
  begin
    Result := '';
    for Index := 0 to Ed.Selections.Count - 1 do
    begin
      Sel := Ed.Selections[Index];
      if Sel.IsEmpty then
        Result := Result + Ed.Lines[Sel.Caret.Line - 1]
      else
        Result := Result + Ed.SelectionText(Sel);

      if Index < Ed.Selections.Count - 1 then
        Result := Result + Delimiter;
    end;
  end;

begin
  inherited Create;

  Ed := ASynEdit as TCustomSynEdit;

  FFormatEtc := TList<TClipFormat>.Create;

  FFormatEtc.Add(CF_UNICODETEXT);
  FText := DelimitedText(SLineBreak);

  if Ed.Selections.Count > 1 then
  begin
    FFormatEtc.Add(SynEditClipboardFormat); // InternalFormat
    FInternalText := DelimitedText(IntClipFormatDelimiter);
  end;

  if not (eoCopyPlainText in TCustomSynEdit(ASynEdit).Options) and
    Assigned(TCustomSynEdit(ASynEdit).Highlighter) and (FText <> '')
  then
  begin
    FFormatEtc.Add(HTMLClipboardFormat); // HTMLFormat
    HtmlStream := TMemoryStream.Create;
    StreamHtml(ASynEdit, HtmlStream);
  end;
end;

destructor TSynEditDataObject.Destroy;
begin
  FFormatEtc.Free;
  HtmlStream.Free;
  inherited Destroy
end;

function TSynEditDataObject.GetData (const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult;
begin
  ZeroMemory (@Medium, sizeof (TStgMedium));
  Result := QueryGetData(formatetcIn);
  if Result = S_OK then
  try
    Medium.tymed := TYMED_HGLOBAL;
    if FormatEtcIn.cfFormat = CF_UNICODETEXT then
      Medium.hGlobal := SynEditDragDropWin.MakeGlobal(FText)
    else if FormatEtcIn.cfFormat = SynEditClipboardFormat then
      Medium.hGlobal := SynEditDragDropWin.MakeGlobal(FInternalText)
    else if (FormatEtcIn.cfFormat = HTMLClipboardFormat) then
      Medium.hGlobal := SynEditDragDropWin.MakeGlobal(HtmlStream.Memory^, HtmlStream.Position);
  except
    Result := E_UNEXPECTED;
  end
end;

function TSynEditDataObject.GetDataHere (const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TSynEditDataObject.StreamHTML(Editor: TObject; Stream: TStream);
var
  HTMLExport: TSynExporterHTML;
  SL: TStringList;
  Ed: TCustomSynEdit;
begin
  Ed := Editor as TCustomSynEdit;
  HTMLExport := TSynExporterHTML.Create(nil);
  try
    HTMLExport.Font := Ed.Font;
    HTMLExport.CreateHTMLFragment := True;
    HTMLExport.UseBackground := not (eoNoHTMLBackground in TCustomSynEdit(Editor).Options);
    HTMLExport.Highlighter := Ed.Highlighter;
    SL := TStringList.Create;
    try
      SL.Text := FText;
      HTMLExport.ExportAll(SL);
      HTMLExport.SaveToStream(Stream);
    finally
      SL.Free;
    end;
    // Adding a terminating null byte to the Stream.
    Stream.WriteData(0, 1);
  finally
    HTMLExport.Free;
  end;
end;

function TSynEditDataObject.QueryGetData (const formatetc: TFormatEtc): HResult;
begin
  if (formatetc.tymed and TYMED_HGLOBAL = TYMED_HGLOBAL) and
    FFormatEtc.Contains(formatetc.cfFormat)
  then
    Result := S_OK
  else
    Result := DV_E_FORMATETC;
end;

function TSynEditDataObject.GetCanonicalFormatEtc (const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult;
begin
  FormatEtcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
end;

function TSynEditDataObject.SetData (const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSynEditDataObject.EnumFormatEtc (dwDirection: Longint; out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  try
    if dwDirection = DATADIR_GET then
    begin
      EnumFormatEtc := SynEditDragDropWin.TSynEnumFormatEtc.Create(FFormatEtc.ToArray);
      Result := S_OK
    end else
      Result := E_NOTIMPL;
  except
    Result := E_UNEXPECTED;
  end
end;

function TSynEditDataObject.DAdvise (const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSynEditDataObject.DUnadvise (dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TSynEditDataObject.EnumDAdvise (out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;


end.
