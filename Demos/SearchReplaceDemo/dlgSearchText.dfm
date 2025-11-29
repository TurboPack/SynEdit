object TextSearchDialog: TTextSearchDialog
  Left = 132
  Top = 168
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsDialog
  Caption = 'Search Text'
  ClientHeight = 253
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 144
  TextHeight = 20
  object Label1: TLabel
    Left = 12
    Top = 18
    Width = 78
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Search for:'
  end
  object cbSearchText: TComboBox
    Left = 144
    Top = 12
    Width = 342
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 0
  end
  object gbSearchOptions: TGroupBox
    Left = 12
    Top = 60
    Width = 231
    Height = 191
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Options'
    TabOrder = 1
    object cbSearchCaseSensitive: TCheckBox
      Left = 12
      Top = 26
      Width = 210
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'C&ase sensitivity'
      TabOrder = 0
    end
    object cbSearchWholeWords: TCheckBox
      Left = 12
      Top = 59
      Width = 210
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Whole words only'
      TabOrder = 1
    end
    object cbSearchFromCursor: TCheckBox
      Left = 12
      Top = 92
      Width = 210
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search from &caret'
      TabOrder = 2
    end
    object cbSearchSelectedOnly: TCheckBox
      Left = 12
      Top = 125
      Width = 210
      Height = 25
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Selected text only'
      TabOrder = 3
    end
    object cbRegularExpression: TCheckBox
      Left = 12
      Top = 156
      Width = 210
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Regular expression'
      TabOrder = 4
      OnClick = cbRegularExpressionClick
    end
  end
  object rgSearchDirection: TRadioGroup
    Left = 255
    Top = 60
    Width = 231
    Height = 98
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Direction'
    ItemIndex = 0
    Items.Strings = (
      '&Forward'
      '&Backward')
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 255
    Top = 224
    Width = 113
    Height = 34
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 374
    Top = 224
    Width = 112
    Height = 34
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
