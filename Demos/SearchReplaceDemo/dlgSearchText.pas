{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dlgSearchText.pas, released 2000-06-23.

The Original Code is part of the SearchReplaceDemo project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

Known Issues:
-------------------------------------------------------------------------------}

unit dlgSearchText;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TTextSearchDialog = class(TForm)
    Label1: TLabel;
    cbSearchText: TComboBox;
    rgSearchDirection: TRadioGroup;
    gbSearchOptions: TGroupBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbRegularExpression: TCheckBox;
    procedure cbRegularExpressionClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetSearchBackwards: Boolean;
    function GetSearchCaseSensitive: Boolean;
    function GetSearchFromCursor: Boolean;
    function GetSearchInSelection: Boolean;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    function GetSearchWholeWords: Boolean;
    procedure SetSearchBackwards(Value: Boolean);
    procedure SetSearchCaseSensitive(Value: Boolean);
    procedure SetSearchFromCursor(Value: Boolean);
    procedure SetSearchInSelection(Value: Boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(Value: string);
    procedure SetSearchWholeWords(Value: Boolean);
    procedure SetSearchRegularExpression(const Value: Boolean);
    function GetSearchRegularExpression: Boolean;
  public
    property SearchBackwards: Boolean read GetSearchBackwards
      write SetSearchBackwards;
    property SearchCaseSensitive: Boolean read GetSearchCaseSensitive
      write SetSearchCaseSensitive;
    property SearchFromCursor: Boolean read GetSearchFromCursor
      write SetSearchFromCursor;
    property SearchInSelectionOnly: Boolean read GetSearchInSelection
      write SetSearchInSelection;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchTextHistory: string read GetSearchTextHistory
      write SetSearchTextHistory;
    property SearchWholeWords: Boolean read GetSearchWholeWords
      write SetSearchWholeWords;
    property SearchRegularExpression: Boolean read GetSearchRegularExpression
      write SetSearchRegularExpression;
  end;

implementation

{$R *.DFM}

procedure TTextSearchDialog.cbRegularExpressionClick(Sender: TObject);
begin
  // The regexp search engine does not support the whole word option
  cbSearchWholeWords.Enabled := not cbRegularExpression.Checked;
end;

{ TTextSearchDialog }

function TTextSearchDialog.GetSearchBackwards: Boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TTextSearchDialog.GetSearchCaseSensitive: Boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TTextSearchDialog.GetSearchFromCursor: Boolean;
begin
  Result := cbSearchFromCursor.Checked;
end;

function TTextSearchDialog.GetSearchInSelection: Boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TTextSearchDialog.GetSearchRegularExpression: Boolean;
begin
  Result := cbRegularExpression.Checked;
end;

function TTextSearchDialog.GetSearchText: string;
begin
  Result := cbSearchText.Text;
end;

function TTextSearchDialog.GetSearchTextHistory: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to cbSearchText.Items.Count - 1 do begin
    if I >= 10 then
      Break;
    if I > 0 then
      Result := Result + #13#10;
    Result := Result + cbSearchText.Items[I];
  end;
end;

function TTextSearchDialog.GetSearchWholeWords: Boolean;
begin
  Result := not cbRegularExpression.Checked and cbSearchWholeWords.Checked;
end;

procedure TTextSearchDialog.SetSearchBackwards(Value: Boolean);
begin
  rgSearchDirection.ItemIndex := Ord(Value);
end;

procedure TTextSearchDialog.SetSearchCaseSensitive(Value: Boolean);
begin
  cbSearchCaseSensitive.Checked := Value;
end;

procedure TTextSearchDialog.SetSearchFromCursor(Value: Boolean);
begin
  cbSearchFromCursor.Checked := Value;
end;

procedure TTextSearchDialog.SetSearchInSelection(Value: Boolean);
begin
  cbSearchSelectedOnly.Checked := Value;
end;

procedure TTextSearchDialog.SetSearchText(Value: string);
begin
  cbSearchText.Text := Value;
end;

procedure TTextSearchDialog.SetSearchTextHistory(Value: string);
begin
  cbSearchText.Items.Text := Value;
end;

procedure TTextSearchDialog.SetSearchWholeWords(Value: Boolean);
begin
  cbSearchWholeWords.Checked := Value;
end;

procedure TTextSearchDialog.SetSearchRegularExpression(
  const Value: Boolean);
begin
  cbRegularExpression.Checked := Value;
end;

{ event handlers }

procedure TTextSearchDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  s: string;
  I: Integer;
begin
  if ModalResult = mrOK then begin
    s := cbSearchText.Text;
    if s <> '' then begin
      I := cbSearchText.Items.IndexOf(s);
      if I > -1 then begin
        cbSearchText.Items.Delete(I);
        cbSearchText.Items.Insert(0, s);
        cbSearchText.Text := s;
      end else
        cbSearchText.Items.Insert(0, s);
    end;
  end;
end;

end.

 