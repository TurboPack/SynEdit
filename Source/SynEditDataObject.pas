unit SynEditDataObject;
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
  System.Generics.Collections;

Type

  TSynEnumFormatEtc = class (TInterfacedObject, IEnumFORMATETC)
  private
    FList: TArray<TClipFormat>;
    FIndex: Integer;
  protected
    function GetFormatEtc(ClipFormat: TClipFormat): TFormatEtc;
    {IEnumFORMATETC}
    function Next (celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip (celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone (out Enum: IEnumFormatEtc): HResult; stdcall;
  public
    constructor Create (FormatList: TArray<TClipFormat>; Index: Integer = 0);
  end;

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

function MakeGlobal (const S: string): hGlobal; overload;
function MakeGlobal (var P; Size: Integer): hGlobal; overload;
function HasFormat(DataObject: IDataObject; Format: TClipFormat): Boolean;
function GetInternalClipText: TArray<string>;

const
  IntClipFormatDelimiter = #$EEFF;   // from private unicode area

var
  SynEditClipboardFormat: UINT;
  HTMLClipboardFormat: UINT;

implementation

uses
  Vcl.Clipbrd,
  SynEdit,
  SynEditTypes,
  SynExportHTML;

function MakeGlobal (const S: string): hGlobal;
var
  P: PChar;
  Size: Integer;
begin
  Size := ByteLength(S) + SizeOf(Char);
  Result := GlobalAlloc (GHND, Size);
  if Result = 0 then
    OutOfMemoryError;
  P := GlobalLock (Result);
  try
    Move(PChar(S)^, P^, Size)
  finally
    GlobalUnlock (Result)
  end
end;

function MakeGlobal (var P; Size: Integer): hGlobal;
var
  D: pointer;
begin
  Result := GlobalAlloc (GHND, Size);
  if Result = 0 then
    OutOfMemoryError;
  D := GlobalLock (Result);
  try
    Move (P, D^, Size)
  finally
    GlobalUnlock (Result)
  end
end;

function HasFormat(DataObject: IDataObject; Format: TClipFormat):Boolean;
var
  FormatEnumerator: IEnumFormatEtc;
  FormatEtc: TFormatEtc;
  Returned: Integer;
begin
  Result := False;
  if (DataObject.EnumFormatEtc (DATADIR_GET, FormatEnumerator) = S_OK) then begin
    FormatEnumerator.Reset;
    while FormatEnumerator.Next (1, FormatEtc, @Returned) = S_OK do
      if FormatEtc.cfFormat = Format then
        Exit(True);
  end;
end;

function GetInternalClipText: TArray<string>;
var
  Data: THandle;
  TempS: string;
begin
  Result := [];
  if not Clipboard.HasFormat(SynEditClipboardFormat) then Exit;
  Data := Clipboard.GetAsHandle(SynEditClipboardFormat);

  if Data <> 0 then
    try
      TempS := PChar(GlobalLock(Data));
      Result := TempS.Split([IntClipFormatDelimiter]);
    finally
      GlobalUnlock(Data);
    end;
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
      Medium.hGlobal := MakeGlobal(FText)
    else if FormatEtcIn.cfFormat = SynEditClipboardFormat then
      Medium.hGlobal := MakeGlobal(FInternalText)
    else if (FormatEtcIn.cfFormat = HTMLClipboardFormat) then
      Medium.hGlobal := MakeGlobal(HtmlStream.Memory^, HtmlStream.Position);
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
      EnumFormatEtc := TSynEnumFormatEtc.Create(FFormatEtc.ToArray);
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


//=== BASE ENUM FORMATETC CLASS ================================================

constructor TSynEnumFormatEtc.Create(FormatList: TArray<TClipFormat>;
  Index: Integer);
begin
  inherited Create;
  FList := FormatList;
  FIndex := Index;
end;

function TSynEnumFormatEtc.GetFormatEtc(ClipFormat: TClipFormat): TFormatEtc;
begin
  with Result do
  begin
    cfFormat := ClipFormat;
    dwAspect := DVASPECT_CONTENT;
    ptd := nil;
    tymed := TYMED_HGLOBAL;
    lindex := -1;
  end;
end;

function TSynEnumFormatEtc.Next (celt: Longint; out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
  FormatEtc: PFormatEtc;
begin
  I := 0;
  FormatEtc:= PFormatEtc(@Elt);
  while (I < Celt) and (FIndex < Length(FList)) do
  begin
    FormatEtc^ := GetFormatEtc(FList[FIndex]);
    Inc(FormatEtc);
    Inc (FIndex);
    Inc (I)
  end;

  if (pCeltFetched <> nil) then pCeltFetched^:= i;

  if (I = Celt) then
    Result:= S_OK
  else
    Result:= S_FALSE;
end;

function TSynEnumFormatEtc.Skip (celt: Longint): HResult;
begin
  Result := S_OK;
  if Celt <= Length(FList) - FIndex then
    FIndex := FIndex + Celt
  else begin
    FIndex := Length(FList);
    Result := S_FALSE
  end
end;

function TSynEnumFormatEtc.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TSynEnumFormatEtc.Clone (out Enum: IEnumFormatEtc): HResult;
begin
  Result := S_OK;
  Enum := TSynEnumFormatEtc.Create (FList, FIndex);
end;


const
  CF_HTML = 'HTML Format';
initialization
  OleInitialize(nil);
  SynEditClipboardFormat := RegisterClipboardFormat ('Internal SynEdit clipboard format');
  HTMLClipboardFormat := RegisterClipboardFormat(CF_HTML);
finalization
  OleFlushClipboard;
  OleUninitialize;
end.
