object EditorForm: TEditorForm
  Left = 338
  Top = 199
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  ActiveControl = SynEditor
  Caption = 'Editor'
  ClientHeight = 431
  ClientWidth = 681
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 20
  object SynEditor: TSynEdit
    Left = 0
    Top = 0
    Width = 681
    Height = 431
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    PopupMenu = pmnuEditor
    TabOrder = 0
    OnEnter = SynEditorEnter
    OnExit = SynEditorExit
    UseCodeFolding = False
    BookMarkOptions.LeftMargin = 3
    BookMarkOptions.Xoffset = 18
    ExtraLineSpacing = 3
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
    ScrollbarAnnotations = <>
    SearchEngine = SynEditSearch1
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
    end
    object lmiEditRedo: TMenuItem
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object lmiEditCut: TMenuItem
    end
    object lmiEditCopy: TMenuItem
    end
    object lmiEditPaste: TMenuItem
    end
    object lmiEditDelete: TMenuItem
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object lmiEditSelectAll: TMenuItem
    end
  end
  object SynEditSearch1: TSynEditSearch
    Left = 92
    Top = 64
  end
end
