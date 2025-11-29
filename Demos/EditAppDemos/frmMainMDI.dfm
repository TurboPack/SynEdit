inherited MDIMainForm: TMDIMainForm
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Multiple Document Edit Demo'
  ClientHeight = 621
  ClientWidth = 1134
  Font.Height = -26
  FormStyle = fsMDIForm
  WindowMenu = mWindow
  StyleElements = [seFont, seClient, seBorder]
  PixelsPerInch = 144
  TextHeight = 32
  inherited StatusBar: TStatusBar
    Top = 578
    Width = 1134
    Height = 43
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
    object mWindow: TMenuItem
      Caption = '&Window'
      object miWindowCascade: TMenuItem
        Action = actWindowCascade
      end
      object miWindowTile: TMenuItem
        Action = actWindowTileHorz
      end
      object miWindowTileVert: TMenuItem
        Action = actWindowTileVert
      end
      object miWindowArrange: TMenuItem
        Action = actWindowArrange
      end
    end
  end
  object actlWindow: TActionList
    Left = 28
    Top = 72
    object actWindowTileHorz: TWindowTileHorizontal
      Category = 'Window'
      Caption = '&Tile'
      Enabled = False
      ImageIndex = 15
    end
    object actWindowTileVert: TWindowTileVertical
      Category = 'Window'
      Caption = 'Tile &Vertically'
      Enabled = False
      ImageIndex = 16
    end
    object actWindowArrange: TWindowArrange
      Category = 'Window'
      Caption = '&Arrange Icons'
      Enabled = False
    end
    object actWindowCascade: TWindowCascade
      Category = 'Window'
      Caption = '&Cascade'
      Enabled = False
      ImageIndex = 17
    end
  end
end
