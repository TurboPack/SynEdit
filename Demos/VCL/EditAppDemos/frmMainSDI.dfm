inherited SDIMainForm: TSDIMainForm
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Single Document Edit Demo'
  ClientHeight = 647
  ClientWidth = 1143
  Font.Height = -26
  StyleElements = [seFont, seClient, seBorder]
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 144
  TextHeight = 32
  inherited StatusBar: TStatusBar
    Top = 603
    Width = 1143
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
  inherited mnuMain: TMainMenu
    inherited mFile: TMenuItem
      inherited miFileClose: TMenuItem
        Visible = False
      end
      inherited miFileCloseAll: TMenuItem
        Visible = False
      end
    end
  end
end
