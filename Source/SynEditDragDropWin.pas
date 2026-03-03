{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.

Windows OLE drag-and-drop building blocks shared by VCL and FMX editors.
Includes IDropSource, IEnumFORMATETC, HGLOBAL helpers, clipboard format
variables, and standalone HTML fragment generation.
-------------------------------------------------------------------------------}

unit SynEditDragDropWin;

{$I SynEdit.inc}

{$IFDEF MSWINDOWS}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  SynEditHighlighter;

// Drop effects as Delphi style constants (originals in ActiveX)
const
  deNone   = DROPEFFECT_NONE;
  deMove   = DROPEFFECT_MOVE;
  deCopy   = DROPEFFECT_COPY;
  deLink   = DROPEFFECT_LINK;
  deScroll = DROPEFFECT_SCROLL;
  IntClipFormatDelimiter = #$EEFF;   // from private unicode area

var
  SynEditClipboardFormat: UINT;
  HTMLClipboardFormat: UINT;

type
  // Implementation of the IDropSource interface
  TSynDragSource = class(TInterfacedObject, IDropSource)
  private
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  end;

  // IEnumFORMATETC implementation for enumerating clipboard formats
  TSynEnumFormatEtc = class(TInterfacedObject, IEnumFORMATETC)
  private
    FList: TArray<TClipFormat>;
    FIndex: Integer;
  protected
    function GetFormatEtc(ClipFormat: TClipFormat): TFormatEtc;
    {IEnumFORMATETC}
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
  public
    constructor Create(FormatList: TArray<TClipFormat>; Index: Integer = 0);
  end;

/// Returns the normal response for a wanted effect:
///   no keys       = "move"
///   control only  = "copy"
function StandardEffect(Keys: TShiftState): Integer;

/// Allocate an HGLOBAL and copy a string into it (with null terminator).
function MakeGlobal(const S: string): HGLOBAL; overload;

/// Allocate an HGLOBAL and copy a raw buffer into it.
function MakeGlobal(var P; Size: Integer): HGLOBAL; overload;

/// Check whether an IDataObject supports a given clipboard format.
function HasFormat(DataObject: IDataObject; Format: TClipFormat): Boolean;

/// Read internal multi-caret clipboard text from the system clipboard.
function GetInternalClipText: TArray<string>;

/// Generate a CF_HTML fragment from highlighted text.
///  Walks the highlighter token-by-token and produces inline CSS spans
///  wrapped in the standard CF_HTML header format.
///  Returns UTF-8 encoded bytes suitable for HGLOBAL allocation.
function GenerateHTMLFragment(const AText: string;
  AHighlighter: TSynCustomHighlighter;
  const AFontFamily: string; AFontSize: Integer;
  ABackColor: TAlphaColor; AUseBackground: Boolean): TBytes;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{ no implementation uses — all imports in interface }

//=== UTILITY FUNCTIONS ========================================================

function StandardEffect(Keys: TShiftState): Integer;
begin
  Result := deMove;
  if ssCtrl in Keys then
    Result := deCopy;
end;

function MakeGlobal(const S: string): HGLOBAL;
var
  P: PChar;
  Size: Integer;
begin
  Size := ByteLength(S) + SizeOf(Char);
  Result := GlobalAlloc(GHND, Size);
  if Result = 0 then
    OutOfMemoryError;
  P := GlobalLock(Result);
  try
    Move(PChar(S)^, P^, Size);
  finally
    GlobalUnlock(Result);
  end;
end;

function MakeGlobal(var P; Size: Integer): HGLOBAL;
var
  D: Pointer;
begin
  Result := GlobalAlloc(GHND, Size);
  if Result = 0 then
    OutOfMemoryError;
  D := GlobalLock(Result);
  try
    Move(P, D^, Size);
  finally
    GlobalUnlock(Result);
  end;
end;

function HasFormat(DataObject: IDataObject; Format: TClipFormat): Boolean;
var
  FormatEnumerator: IEnumFormatEtc;
  FormatEtc: TFormatEtc;
  Returned: Integer;
begin
  Result := False;
  if DataObject.EnumFormatEtc(DATADIR_GET, FormatEnumerator) = S_OK then
  begin
    FormatEnumerator.Reset;
    while FormatEnumerator.Next(1, FormatEtc, @Returned) = S_OK do
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
  if not IsClipboardFormatAvailable(SynEditClipboardFormat) then Exit;
  if not OpenClipboard(0) then Exit;
  try
    Data := GetClipboardData(SynEditClipboardFormat);
    if Data <> 0 then
      try
        TempS := PChar(GlobalLock(Data));
        Result := TempS.Split([IntClipFormatDelimiter]);
      finally
        GlobalUnlock(Data);
      end;
  finally
    CloseClipboard;
  end;
end;

//=== HTML FRAGMENT GENERATION ================================================

function ColorToHtmlHex(AColor: TAlphaColor): string;
begin
  Result := '#' +
    IntToHex(TAlphaColorRec(AColor).R, 2) +
    IntToHex(TAlphaColorRec(AColor).G, 2) +
    IntToHex(TAlphaColorRec(AColor).B, 2);
end;

function HtmlEncode(const S: string): string;
var
  I: Integer;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create(Length(S));
  try
    for I := 1 to Length(S) do
      case S[I] of
        '&': SB.Append('&amp;');
        '<': SB.Append('&lt;');
        '>': SB.Append('&gt;');
        '"': SB.Append('&quot;');
      else
        SB.Append(S[I]);
      end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function FontStylesToCSS(Styles: TFontStyles): string;
begin
  Result := '';
  if TFontStyle.fsBold in Styles then
    Result := Result + 'font-weight: bold; ';
  if TFontStyle.fsItalic in Styles then
    Result := Result + 'font-style: italic; ';
  if TFontStyle.fsUnderline in Styles then
    Result := Result + 'text-decoration: underline; ';
  if TFontStyle.fsStrikeOut in Styles then
    Result := Result + 'text-decoration: line-through; ';
end;

function GenerateHTMLFragment(const AText: string;
  AHighlighter: TSynCustomHighlighter;
  const AFontFamily: string; AFontSize: Integer;
  ABackColor: TAlphaColor; AUseBackground: Boolean): TBytes;
var
  Lines: TArray<string>;
  HTML: TStringBuilder;
  I: Integer;
  Token: string;
  Attr: TSynHighlighterAttributes;
  SpanStyle: string;
  FragmentStart, FragmentEnd: Integer;
  Header: string;
  UTF8Fragment: TBytes;
const
  CrLf = #13#10;
  HeaderTemplate =
    'Version:0.9' + CrLf +
    'StartHTML:%.10d' + CrLf +
    'EndHTML:%.10d' + CrLf +
    'StartFragment:%.10d' + CrLf +
    'EndFragment:%.10d' + CrLf;
  HTMLStart = '<html>' + CrLf + '<body>' + CrLf;
  HTMLEnd = CrLf + '</body>' + CrLf + '</html>';
  FragStart = '<!--StartFragment-->';
  FragEnd = '<!--EndFragment-->';
begin
  Result := nil;
  if not Assigned(AHighlighter) or (AText = '') then
    Exit;

  Lines := AText.Split([#13#10, #10, #13]);

  HTML := TStringBuilder.Create;
  try
    // Open the styled container
    HTML.Append('<pre style="font-family: ''');
    HTML.Append(HtmlEncode(AFontFamily));
    HTML.Append(''', monospace; font-size: ');
    HTML.Append(AFontSize);
    HTML.Append('pt;');
    if AUseBackground then
    begin
      HTML.Append(' background-color: ');
      HTML.Append(ColorToHtmlHex(ABackColor));
      HTML.Append(';');
    end;
    HTML.Append('">');

    for I := 0 to Length(Lines) - 1 do
    begin
      if I > 0 then
        HTML.Append(CrLf);

      AHighlighter.SetLine(Lines[I], I);
      while not AHighlighter.GetEol do
      begin
        Token := AHighlighter.GetToken;
        Attr := AHighlighter.GetTokenAttribute;

        if (Attr = nil) or ((Attr.Foreground = 0) and (Attr.Style = [])) then
          HTML.Append(HtmlEncode(Token))
        else
        begin
          SpanStyle := '';
          if Attr.Foreground <> 0 then
            SpanStyle := SpanStyle + 'color: ' +
              ColorToHtmlHex(TAlphaColor(Attr.Foreground)) + '; ';
          SpanStyle := SpanStyle + FontStylesToCSS(Attr.Style);
          SpanStyle := SpanStyle.Trim;

          if SpanStyle <> '' then
          begin
            HTML.Append('<span style="');
            HTML.Append(SpanStyle);
            HTML.Append('">');
            HTML.Append(HtmlEncode(Token));
            HTML.Append('</span>');
          end
          else
            HTML.Append(HtmlEncode(Token));
        end;

        AHighlighter.Next;
      end;
    end;

    HTML.Append('</pre>');

    // Build the CF_HTML envelope
    UTF8Fragment := TEncoding.UTF8.GetBytes(HTML.ToString);

    // Calculate CF_HTML offsets
    // Header has fixed-length placeholders (%.10d = 10 digits)
    var HeaderLen := Length(TEncoding.UTF8.GetBytes(
      Format(HeaderTemplate, [0, 0, 0, 0])));
    var HTMLStartBytes := TEncoding.UTF8.GetBytes(HTMLStart + FragStart);
    var HTMLEndBytes := TEncoding.UTF8.GetBytes(FragEnd + HTMLEnd);
    var StartHTMLOfs := HeaderLen;

    FragmentStart := StartHTMLOfs + Length(TEncoding.UTF8.GetBytes(HTMLStart))
      + Length(TEncoding.UTF8.GetBytes(FragStart));
    FragmentEnd := FragmentStart + Length(UTF8Fragment);
    var EndHTMLOfs := FragmentEnd + Length(TEncoding.UTF8.GetBytes(FragEnd))
      + Length(TEncoding.UTF8.GetBytes(HTMLEnd));

    Header := Format(HeaderTemplate, [StartHTMLOfs, EndHTMLOfs,
      FragmentStart, FragmentEnd]);

    // Assemble final bytes
    var HeaderBytes := TEncoding.UTF8.GetBytes(Header);
    SetLength(Result, Length(HeaderBytes) + Length(HTMLStartBytes)
      + Length(UTF8Fragment) + Length(HTMLEndBytes) + 1); // +1 for null

    var Pos := 0;
    Move(HeaderBytes[0], Result[Pos], Length(HeaderBytes));
    Inc(Pos, Length(HeaderBytes));
    Move(HTMLStartBytes[0], Result[Pos], Length(HTMLStartBytes));
    Inc(Pos, Length(HTMLStartBytes));
    Move(UTF8Fragment[0], Result[Pos], Length(UTF8Fragment));
    Inc(Pos, Length(UTF8Fragment));
    Move(HTMLEndBytes[0], Result[Pos], Length(HTMLEndBytes));
    Inc(Pos, Length(HTMLEndBytes));
    Result[Pos] := 0; // null terminator
  finally
    HTML.Free;
  end;
end;

//=== DRAG SOURCE ==============================================================

function TSynDragSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Longint): HResult;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and MK_LBUTTON) = 0 then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;

function TSynDragSource.GiveFeedback(dwEffect: Longint): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

//=== ENUM FORMATETC ===========================================================

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

function TSynEnumFormatEtc.Next(celt: Longint; out elt;
  pceltFetched: PLongint): HResult;
var
  I: Integer;
  FormatEtc: PFormatEtc;
begin
  I := 0;
  FormatEtc := PFormatEtc(@Elt);
  while (I < Celt) and (FIndex < Length(FList)) do
  begin
    FormatEtc^ := GetFormatEtc(FList[FIndex]);
    Inc(FormatEtc);
    Inc(FIndex);
    Inc(I);
  end;

  if pCeltFetched <> nil then
    pCeltFetched^ := I;

  if I = Celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TSynEnumFormatEtc.Skip(celt: Longint): HResult;
begin
  Result := S_OK;
  if Celt <= Length(FList) - FIndex then
    FIndex := FIndex + Celt
  else
  begin
    FIndex := Length(FList);
    Result := S_FALSE;
  end;
end;

function TSynEnumFormatEtc.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TSynEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;
begin
  Result := S_OK;
  Enum := TSynEnumFormatEtc.Create(FList, FIndex);
end;

//=== INITIALIZATION ===========================================================

const
  CF_HTML = 'HTML Format';

initialization
  OleInitialize(nil);
  SynEditClipboardFormat := RegisterClipboardFormat('Internal SynEdit clipboard format');
  HTMLClipboardFormat := RegisterClipboardFormat(CF_HTML);

finalization
  OleFlushClipboard;
  OleUninitialize;

{$ENDIF}

end.
