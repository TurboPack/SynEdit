object SearchReplaceDemoForm: TSearchReplaceDemoForm
  Left = 100
  Top = 122
  Caption = 'Search and replace demo'
  ClientHeight = 557
  ClientWidth = 830
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object SynEditor: TSynEdit
    Left = 0
    Top = 26
    Width = 830
    Height = 512
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    OnMouseDown = SynEditorMouseDown
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.ShowLineNumbers = True
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
    ScrollbarAnnotations = <>
    SelectedColor.Alpha = 0.400000005960464500
    VisibleSpecialChars = []
    OnProcessCommand = SynEditorProcessCommand
    OnReplaceText = SynEditorReplaceText
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
  object ToolBarMain: TToolBar
    Left = 0
    Top = 0
    Width = 830
    Height = 26
    AutoSize = True
    BorderWidth = 1
    Caption = 'Standard'
    Images = VirtualImageList
    TabOrder = 1
    object ToolButtonFileOpen: TToolButton
      Left = 0
      Top = 0
      Action = ActionFileOpen
    end
    object ToolButtonSeparator1: TToolButton
      Left = 23
      Top = 0
      Width = 8
      ImageIndex = 1
      ImageName = 'Item2'
      Style = tbsSeparator
    end
    object ToolButtonSearch: TToolButton
      Left = 31
      Top = 0
      Action = ActionSearch
    end
    object ToolButtonSearchNext: TToolButton
      Left = 54
      Top = 0
      Action = ActionSearchNext
    end
    object ToolButtonSearchPrev: TToolButton
      Left = 77
      Top = 0
      Action = ActionSearchPrev
    end
    object ToolButtonSeparator2: TToolButton
      Left = 100
      Top = 0
      Width = 8
      Caption = 'ToolButtonSeparator2'
      ImageIndex = 4
      ImageName = 'Item5'
      Style = tbsSeparator
    end
    object ToolButtonSearchReplace: TToolButton
      Left = 108
      Top = 0
      Action = ActionSearchReplace
    end
  end
  object Statusbar: TStatusBar
    Left = 0
    Top = 538
    Width = 830
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object OpenDialogFile: TOpenDialog
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 49
    Top = 144
  end
  object ActionListMain: TActionList
    Images = VirtualImageList
    Left = 48
    Top = 240
    object ActionFileOpen: TAction
      Caption = '&Open...'
      ImageIndex = 0
      ImageName = 'Item1'
      ShortCut = 16463
      OnExecute = ActionFileOpenExecute
    end
    object ActionSearch: TAction
      Caption = '&Find...'
      ImageIndex = 1
      ImageName = 'Item2'
      ShortCut = 16454
      OnExecute = ActionSearchExecute
    end
    object ActionSearchNext: TAction
      Caption = 'Find &next'
      Enabled = False
      ImageIndex = 2
      ImageName = 'Item3'
      ShortCut = 114
      OnExecute = ActionSearchNextExecute
      OnUpdate = actSearchUpdate
    end
    object ActionSearchPrev: TAction
      Caption = 'Find &previous'
      Enabled = False
      ImageIndex = 3
      ImageName = 'Item4'
      ShortCut = 8306
      OnExecute = ActionSearchPrevExecute
      OnUpdate = actSearchUpdate
    end
    object ActionSearchReplace: TAction
      Caption = '&Replace...'
      Enabled = False
      ImageIndex = 4
      ImageName = 'Item5'
      ShortCut = 16456
      OnExecute = ActionSearchReplaceExecute
      OnUpdate = ActionSearchReplaceUpdate
    end
  end
  object SynEditSearch: TSynEditSearch
    Left = 212
    Top = 136
  end
  object SynEditRegexSearch: TSynEditRegexSearch
    Left = 60
    Top = 48
  end
  object ImageCollection: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000734944415478DABD93D10AC0200845F5BF7BECBF5D0E9509A5CEC17C
              A9C873B8152101C197C28A00B94D6AF5E32B01C34F48D736768EA0894C1045CC
              8EC3CD44E2BBB7037C271701E9F438E232070208614D564E80E12D2409189E33
              07C77097D8835D820E6C029E746027A8D4F6157EF98D515D796A5FF2F182CCB2
              0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000754944415478DAB5935D0E80200C83D76BEDFED9B5108C10C00E8CCE
              3E1028CBB71F029224F922F40094E3A5EC63E753409600B801981F5B411FD407
              7B3E05D4326700F30700CBB25385E0DC9765CA6266A2AAC2EEC60AFF020C814F
              005E9FABF934C06ACA6DDAEE2B4500BC279A01B485D0DFF84607D5A283F1113C
              A4540000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000824944415478DAB592C11280200844D9EFD6237E37458DA584C4A1F6
              E42CF25C182124E4A971935A674FA8C0DE8307E8CD6383F5404DF4EC027AD14B
              A6B531D13F006F04DB1C0222C8EB12A1D6411744DE3F807E51C5CC546AC139CA
              FD212CE40268F32E029E235B7F4A17013485BE9E065052A9047689CBDAA78065
              CCA8B6FA89596DB51B96F1147EDCA70000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000834944415478DAB592C10EC0200843CB77EB51BF9BD92D264E197870
              3D9922CF4214856296A0BECC52809493C09010C006C57381E7B9C1F21640373A
              C84A65D5CE032C48388227AFF91320B4EF242A9EF70FA05FA44ACBCEE8B554CD
              390F3B1AC02380CD4D1059C79DFD0E09014CC1D7B701D8D45682798956ED3CC0
              8AE9D5C29F18E902EC1C91F17E1EF80D0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000008A4944415478DAA5924B0EC0200844E1DCB2C4735B6932893F50239B
              1235330FA65CA890151397DA333965F7E8DB776C025973111152554A92B62293
              00DC230AEF0D57D7DF1DB5A398852B51A7B8A088C90E044202A410630604A731
              3E11B4061639CE6CD95D8C2742268094ACDF128CE2AE804731FE7DCB11568EED
              1777D8C1F50823C1F30EC2116EEB03431B962543C18EBE0000000049454E44AE
              426082}
          end>
      end>
    Left = 208
    Top = 240
  end
  object VirtualImageList: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Item1'
        Name = 'Item1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Item2'
        Name = 'Item2'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Item3'
        Name = 'Item3'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Item4'
        Name = 'Item4'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Item5'
        Name = 'Item5'
      end>
    ImageCollection = ImageCollection
    Left = 216
    Top = 48
  end
end
