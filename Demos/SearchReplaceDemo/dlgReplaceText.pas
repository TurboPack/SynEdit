{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dlgReplaceText.pas, released 2000-06-23.

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

unit dlgReplaceText;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  dlgSearchText, StdCtrls, ExtCtrls;

type
  TTextReplaceDialog = class(TTextSearchDialog)
    Label2: TLabel;
    cbReplaceText: TComboBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    procedure SetReplaceText(Value: string);
    procedure SetReplaceTextHistory(Value: string);
  public
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory
      write SetReplaceTextHistory;
  end;

implementation

{$R *.DFM}

{ TTextReplaceDialog }

function TTextReplaceDialog.GetReplaceText: string;
begin
  Result := cbReplaceText.Text;
end;

function TTextReplaceDialog.GetReplaceTextHistory: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to cbReplaceText.Items.Count - 1 do begin
    if I >= 10 then
      Break;
    if I > 0 then
      Result := Result + #13#10;
    Result := Result + cbReplaceText.Items[I];
  end;
end;

procedure TTextReplaceDialog.SetReplaceText(Value: string);
begin
  cbReplaceText.Text := Value;
end;

procedure TTextReplaceDialog.SetReplaceTextHistory(Value: string);
begin
  cbReplaceText.Items.Text := Value;
end;

procedure TTextReplaceDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  S: string;
  I: Integer;
begin
  inherited;
  if ModalResult = mrOK then begin
    S := cbReplaceText.Text;
    if S <> '' then begin
      I := cbReplaceText.Items.IndexOf(S);
      if I > -1 then begin
        cbReplaceText.Items.Delete(I);
        cbReplaceText.Items.Insert(0, S);
        cbReplaceText.Text := S;
      end else
        cbReplaceText.Items.Insert(0, S);
    end;
  end;
end;

end.


