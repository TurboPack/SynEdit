object EditorForm: TEditorForm
  Left = 338
  Top = 199
  ActiveControl = SynEditor
  Caption = 'Editor'
  ClientHeight = 287
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SynEditor: TSynEdit
    Left = 0
    Top = 0
    Width = 448
    Height = 287
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    PopupMenu = pmnuEditor
    TabOrder = 0
    OnEnter = SynEditorEnter
    OnExit = SynEditorExit
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 15
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkMargin
        Width = 2
      end>
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces, eoShowLigatures]
    SearchEngine = SynEditSearch1
    SelectedColor.Alpha = 0.400000005960464500
    OnChange = SynEditorChange
    OnReplaceText = SynEditorReplaceText
    OnStatusChange = SynEditorStatusChange
    RemovedKeystrokes = <
      item
        Command = ecLineBreak
        ShortCut = 8205
      end
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <>
  end
  object pmnuEditor: TPopupMenu
    Left = 92
    Top = 28
    object lmiEditUndo: TMenuItem
      Action = CommandsDataModule.actEditUndo
    end
    object lmiEditRedo: TMenuItem
      Action = CommandsDataModule.actEditRedo
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object lmiEditCut: TMenuItem
      Action = CommandsDataModule.actEditCut
    end
    object lmiEditCopy: TMenuItem
      Action = CommandsDataModule.actEditCopy
    end
    object lmiEditPaste: TMenuItem
      Action = CommandsDataModule.actEditPaste
    end
    object lmiEditDelete: TMenuItem
      Action = CommandsDataModule.actEditDelete
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object lmiEditSelectAll: TMenuItem
      Action = CommandsDataModule.actEditSelectAll
    end
  end
  object SynEditSearch1: TSynEditSearch
    Left = 92
    Top = 64
  end
end
