inherited TextReplaceDialog: TTextReplaceDialog
  Caption = 'Replace text'
  ClientHeight = 210
  StyleElements = [seFont, seClient, seBorder]
  TextHeight = 13
  inherited Label1: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  object Label2: TLabel [1]
    Left = 8
    Top = 41
    Width = 65
    Height = 13
    Caption = '&Replace with:'
  end
  inherited cbSearchText: TComboBox
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited gbSearchOptions: TGroupBox
    Top = 70
    TabOrder = 2
  end
  inherited rgSearchDirection: TRadioGroup
    Top = 70
    TabOrder = 3
  end
  inherited btnOK: TButton
    Top = 179
    TabOrder = 4
  end
  inherited btnCancel: TButton
    Top = 179
    TabOrder = 5
  end
  object cbReplaceText: TComboBox
    Left = 96
    Top = 37
    Width = 228
    Height = 21
    TabOrder = 1
  end
end
