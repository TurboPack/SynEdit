object ConfirmReplaceDialog: TConfirmReplaceDialog
  Left = 176
  Top = 158
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsDialog
  Caption = 'Confirm replace'
  ClientHeight = 147
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 20
  object lblConfirmation: TLabel
    Left = 90
    Top = 18
    Width = 392
    Height = 66
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    WordWrap = True
  end
  object Image1: TImage
    Left = 24
    Top = 24
    Width = 48
    Height = 48
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
  end
  object btnReplace: TButton
    Left = 12
    Top = 101
    Width = 113
    Height = 34
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Yes'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object btnSkip: TButton
    Left = 131
    Top = 101
    Width = 112
    Height = 34
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&No'
    ModalResult = 7
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 249
    Top = 101
    Width = 113
    Height = 34
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnReplaceAll: TButton
    Left = 368
    Top = 101
    Width = 112
    Height = 34
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Yes to &all'
    ModalResult = 14
    TabOrder = 3
  end
end
