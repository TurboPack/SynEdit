object Form1: TForm1
  Left = 142
  Top = 175
  Caption = 'Print example'
  ClientHeight = 438
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit: TSynEdit
    Left = 0
    Top = 23
    Width = 637
    Height = 415
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Visible = True
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Visible = True
        Width = 3
      end>
    Highlighter = SynPasSyn
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 637
    Height = 23
    AutoSize = True
    ButtonHeight = 23
    ButtonWidth = 24
    Caption = 'ToolBar1'
    Images = MenuImages
    Indent = 5
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 5
      Top = 0
      Action = FileOpenCmd
      ImageName = 'Item1'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton2: TToolButton
      Left = 29
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      ImageName = 'Item2'
      Style = tbsSeparator
    end
    object ToolButton8: TToolButton
      Left = 37
      Top = 0
      Action = FilePageSetup
      ImageName = 'Item2'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton3: TToolButton
      Left = 61
      Top = 0
      Action = FilePrinterSetupCmd
      ImageName = 'Item3'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton4: TToolButton
      Left = 85
      Top = 0
      Action = FilePrintPreviewCmd
      ImageName = 'Item4'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton5: TToolButton
      Left = 109
      Top = 0
      Action = FilePrintCmd
      ImageName = 'Item5'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton6: TToolButton
      Left = 133
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 4
      ImageName = 'Item5'
      Style = tbsSeparator
    end
    object ToolButton7: TToolButton
      Left = 141
      Top = 0
      Action = FileExitCmd
      ImageName = 'Item6'
      ParentShowHint = False
      ShowHint = True
    end
  end
  object SynPasSyn: TSynPasSyn
    DefaultFilter = 'Pascal files (*.pas,*.dpr,*.dpk,*.inc)|*.pas;*.dpr;*.dpk;*.inc'
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clMaroon
    Left = 106
    Top = 189
  end
  object OpenDialog: TOpenDialog
    Left = 27
    Top = 119
  end
  object PrintDialog: TPrintDialog
    Left = 201
    Top = 119
  end
  object SynEditPrint: TSynEditPrint
    Copies = 1
    Header.FrameTypes = [ftBox, ftShaded]
    Header.DefaultFont.Charset = DEFAULT_CHARSET
    Header.DefaultFont.Color = clBlack
    Header.DefaultFont.Height = -13
    Header.DefaultFont.Name = 'Arial'
    Header.DefaultFont.Style = []
    Footer.DefaultFont.Charset = DEFAULT_CHARSET
    Footer.DefaultFont.Color = clBlack
    Footer.DefaultFont.Height = -13
    Footer.DefaultFont.Name = 'Arial'
    Footer.DefaultFont.Style = []
    Margins.Left = 25.000000000000000000
    Margins.Right = 15.000000000000000000
    Margins.Top = 25.000000000000000000
    Margins.Bottom = 25.000000000000000000
    Margins.Header = 15.000000000000000000
    Margins.Footer = 15.000000000000000000
    Margins.LeftHFTextIndent = 2.000000000000000000
    Margins.RightHFTextIndent = 2.000000000000000000
    Margins.HFInternalMargin = 0.500000000000000000
    Margins.MirrorMargins = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Colors = True
    Highlighter = SynPasSyn
    TabWidth = 8
    Color = clWhite
    Left = 22
    Top = 189
  end
  object ActionList1: TActionList
    Images = MenuImages
    Left = 102
    Top = 59
    object FileOpenCmd: TAction
      Caption = '&Open'
      Hint = 'Open'
      ImageIndex = 0
      ShortCut = 16463
      OnExecute = FileOpenCmdExecute
    end
    object FilePageSetup: TAction
      Caption = 'Page Setup'
      Hint = 'Page setup'
      ImageIndex = 1
      OnExecute = FilePageSetupExecute
    end
    object FilePrinterSetupCmd: TAction
      Caption = 'Printer &Setup'
      Hint = 'Printer setup'
      ImageIndex = 2
      OnExecute = FilePrinterSetupCmdExecute
    end
    object FilePrintPreviewCmd: TAction
      Caption = 'Print Preview'
      Hint = 'Print preview'
      ImageIndex = 3
      OnExecute = FilePrintPreviewCmdExecute
    end
    object FilePrintCmd: TAction
      Caption = '&Print'
      Hint = 'Print'
      ImageIndex = 4
      ShortCut = 16464
      OnExecute = FilePrintCmdExecute
    end
    object FileExitCmd: TAction
      Caption = '&Exit'
      Hint = 'Exit'
      ImageIndex = 5
      OnExecute = FileExitCmdExecute
    end
  end
  object PrinterSetupDialog: TPrinterSetupDialog
    Left = 114
    Top = 120
  end
  object MainMenu1: TMainMenu
    Images = MenuImages
    Left = 26
    Top = 59
    object File1: TMenuItem
      Caption = '&File'
      object FileOpenCmd1: TMenuItem
        Action = FileOpenCmd
        ImageName = 'Item1'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object PageSetup1: TMenuItem
        Action = FilePageSetup
        ImageName = 'Item2'
      end
      object PrinterSetup1: TMenuItem
        Action = FilePrinterSetupCmd
        ImageName = 'Item3'
      end
      object PrintPreview1: TMenuItem
        Action = FilePrintPreviewCmd
        ImageName = 'Item4'
      end
      object Print1: TMenuItem
        Action = FilePrintCmd
        ImageName = 'Item5'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = FileExitCmd
        ImageName = 'Item6'
      end
    end
  end
  object ImageCollection: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000794944415478DABD944B0EC0200844E1DE2EBD3795066849A858FA61
              8346E731482212103C0DFC15827C556268F036840167A1EE2D57DB5167069959
              5D698D0544C2DC8F1344544420A4CB20DBB551006710B800A0D557975F43BC30
              7B97D409037A5F13B7E6A65307382755C0313BA8031C6435C2E9BCF1156C9837
              68F19BEE23240000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000604944415478DA63FCCFF09F0104181918210C0200A89E115D8C11D9
              1098825686D6FFD50CD5988AA116A11B8462480B430B5E57D40021C800640BC9
              7609BAD7483204590E59FDA821A386906C08A1648F0C605980362E21DA195080
              6108250000D1F8BEF0A20852260000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000BC4944415478DAA5924112C3200845F9E7D6A59C9B8A838A9126A665
              918C4E783C2010127A1B5C20290BFA194710813013F5C401B1FB1002BDEEF964
              151DA8415285D9798328A0C6465548B3D0440708DBE910009BA15A2D16299849
              68E107CA200F08DBB9B328C52AC36CA2C13E59B46F2A7CB494E7C01BC46FE376
              D33543BF55AB9CE7E6F421638D0FD16C0DB4BEE9FD2F3B0D1CE49B09171E0552
              4E9744C8593BE26685597D37FAB19DC5A46FE764B8512B0372D53C1DEC06F927
              3ED732B3006DC21F9C0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000964944415478DABD93E10E80200884E1B9E5273EB7818EC54CC56C8B
              2D672C3FCFBBC40205BE16CA135264230C2152F30F1043D0164401085866A06D
              25ABA3859091AAFF952880992151AA0B33E742440FD01462004AA929106715A2
              F31EB484D4546450802951557D5221C4F78E20DE0F55C439C3ABE3AC8CF5BDA3
              88ABD902F2711FFD6CEA89F5DAFBC62DF675EFDE4096E01BC6B02EEEF293F046
              65B5AF0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA0000008C4944415478DAAD92CB1280200845E5BB6D99DF6DE283804C4963D3
              8CE9B90714A28B6EB7A007015C36543A0B5D080252CDD301E610DCD44DAFFF1A
              E00159B17885CC2CF45C08F2D522EFAFDF0CB1DE0625334011404BD6DFD48401
              C8E4378815A0DB6AD0A149388308F087A783D266D44E5403873B5D0E7AA31D32
              69576C1DAE6EA508B2C7B6325801D9A90B995387F52211AA1C0000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA0000005C4944415478DAEDD3B10EC0200804D0BBEF9611BF9BB2D8361663B4
              3176281B5C78DBD160783BAC11227755436217519573175198A52B67FE916F22
              74C3F379C48192CF21B7A761A406869108682280865D8980823C6E4B5ABC0D39
              006F928C7520C474900000000049454E44AE426082}
          end>
      end>
    Left = 32
    Top = 272
  end
  object MenuImages: TVirtualImageList
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
      end
      item
        CollectionIndex = 5
        CollectionName = 'Item6'
        Name = 'Item6'
      end>
    ImageCollection = ImageCollection
    Width = 17
    Height = 17
    Left = 120
    Top = 272
  end
end
