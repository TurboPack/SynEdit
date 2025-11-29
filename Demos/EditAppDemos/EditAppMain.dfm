object MainForm: TMainForm
  Left = 186
  Top = 133
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ClientHeight = 431
  ClientWidth = 756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 20
  object StatusBar: TStatusBar
    Left = 0
    Top = 402
    Width = 756
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Action = actUpdateStatusBarPanels
    Panels = <
      item
        Alignment = taCenter
        Width = 126
      end
      item
        Alignment = taCenter
        Width = 108
      end
      item
        Alignment = taCenter
        Width = 126
      end
      item
        Width = 75
      end>
  end
  object mnuMain: TMainMenu
    Left = 28
    Top = 32
    object mFile: TMenuItem
      Caption = '&File'
      OnClick = mFileClick
      object miFileNew: TMenuItem
        Action = actFileNew
      end
      object miFileOpen: TMenuItem
        Action = actFileOpen
      end
      object mRecentFiles: TMenuItem
        Caption = '&Recent Files'
        OnClick = mRecentFilesClick
        object miFileMRU1: TMenuItem
          Caption = '[MRU1]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU2: TMenuItem
          Caption = '[MRU2]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU3: TMenuItem
          Caption = '[MRU3]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU4: TMenuItem
          Caption = '[MRU4]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU5: TMenuItem
          Caption = '[MRU5]'
          OnClick = OnOpenMRUFile
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miFileSave: TMenuItem
      end
      object miFileSaveAs: TMenuItem
      end
      object miFileClose: TMenuItem
      end
      object miFileCloseAll: TMenuItem
        Action = actFileCloseAll
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miFilePrint: TMenuItem
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object mEdit: TMenuItem
      Caption = '&Edit'
      object miEditUndo: TMenuItem
      end
      object miEditRedo: TMenuItem
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miEditCut: TMenuItem
      end
      object miEditCopy: TMenuItem
      end
      object miEditPaste: TMenuItem
      end
      object miEditDelete: TMenuItem
      end
      object miEditSelectAll: TMenuItem
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miEditFind: TMenuItem
      end
      object miEditFindNext: TMenuItem
      end
      object miEditFindPrev: TMenuItem
      end
      object miEditReplace: TMenuItem
      end
    end
    object mView: TMenuItem
      Caption = '&View'
      object miViewStatusbar: TMenuItem
        Action = actViewStatusbar
      end
    end
  end
  object actlStandard: TActionList
    Left = 28
    Top = 108
    object actFileNew: TAction
      Category = 'File'
      Caption = '&New'
      ShortCut = 16462
      OnExecute = actFileNewExecute
      OnUpdate = actFileNewOrOpenUpdate
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = actFileOpenExecute
      OnUpdate = actFileNewOrOpenUpdate
    end
    object actFileCloseAll: TAction
      Category = 'File'
      Caption = 'Close All Fi&les'
      Enabled = False
      ShortCut = 24691
      OnExecute = actFileCloseAllExecute
      OnUpdate = actFileCloseAllUpdate
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actViewStatusbar: TAction
      Category = 'View'
      Caption = '&Status Bar'
      OnExecute = actViewStatusbarExecute
      OnUpdate = actViewStatusbarUpdate
    end
    object actUpdateStatusBarPanels: TAction
      Caption = 'actUpdateStatusBarPanels'
      OnUpdate = actUpdateStatusBarPanelsUpdate
    end
  end
end
