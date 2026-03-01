inherited WorkbookMainForm: TWorkbookMainForm
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Workbook Edit Demo'
  ClientHeight = 647
  ClientWidth = 1134
  Font.Height = -26
  StyleElements = [seFont, seClient, seBorder]
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 144
  TextHeight = 32
  object pctrlMain: TPageControl [0]
    Left = 0
    Top = 0
    Width = 1134
    Height = 603
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 0
    TabStop = False
  end
  inherited StatusBar: TStatusBar
    Top = 603
    Width = 1134
    Height = 44
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Panels = <
      item
        Alignment = taCenter
        Width = 189
      end
      item
        Alignment = taCenter
        Width = 162
      end
      item
        Alignment = taCenter
        Width = 189
      end
      item
        Width = 113
      end>
  end
end
