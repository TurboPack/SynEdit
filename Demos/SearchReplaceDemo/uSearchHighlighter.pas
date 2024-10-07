{-----------------------------------------------------------------------------
 Unit Name: uSearchHighlighter
 Author:    Kiriakos Vlahos
 Date:      24-May-2007
 Purpose:   Classes and support routints for highlighting a search term
-----------------------------------------------------------------------------}

unit uSearchHighlighter;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Synedit,
  SynEditTypes,
  SynEditMiscClasses;

  procedure RegisterSearchHighlightIndicatorSpec(Editor: TCustomSynEdit);
  procedure HighligthtSearchTerm(ATerm : string; Editor: TCustomSynEdit;
    SearchEngine : TSynEditSearchCustom; SearchOptions : TSynSearchOptions);
  procedure ClearSearchHighlight(Editor: TCustomSynEdit);

implementation

uses
  System.UITypes,
  SynDWrite;

const SearchHighlightIndicatorId: TGUID  = '{A59BCD6A-02A6-4B34-B28C-D9EACA0C9F09}';


procedure RegisterSearchHighlightIndicatorSpec(Editor: TCustomSynEdit);
const
  Alpha = 0.3;  // could allow customization
  BackColor: TColor = $0078AAFF;
begin
  var Spec := TSynIndicatorSpec.New(sisRoundedFilledRectangle, clNoneF,
    D2D1ColorF(BackColor, Alpha), []);

  Editor.Indicators.RegisterSpec(SearchHighlightIndicatorId, Spec);
end;

procedure ClearSearchHighlight(Editor: TCustomSynEdit);
begin
  Editor.Indicators.Clear(SearchHighlightIndicatorId);
end;

procedure HighligthtSearchTerm(ATerm : string; Editor: TCustomSynEdit;
  SearchEngine : TSynEditSearchCustom; SearchOptions : TSynSearchOptions);
var
  I: Integer;
  J: Integer;
  Indicator: TSynIndicator;
begin
  ClearSearchHighlight(Editor);
  if ATerm = '' then Exit;

  Indicator.Id := SearchHighlightIndicatorId;
  for I := 0 to Editor.Lines.Count - 1 do begin
    SearchEngine.Options := SearchOptions;
    SearchEngine.Pattern := ATerm;
    SearchEngine.FindAll(Editor.Lines[i]);

    for J := 0 to SearchEngine.ResultCount - 1 do begin
      Indicator.CharStart := SearchEngine.Results[j];
      Indicator.CharEnd := Indicator.CharStart + SearchEngine.Lengths[j];
      Editor.Indicators.Add(I + 1, Indicator);
    end;
  end;
end;

end.
