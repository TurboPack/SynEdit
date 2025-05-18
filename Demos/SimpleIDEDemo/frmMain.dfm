object SimpleIDEMainForm: TSimpleIDEMainForm
  Left = 108
  Top = 107
  Caption = 'Simple IDE Demo'
  ClientHeight = 582
  ClientWidth = 877
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBarDebug: TToolBar
    Left = 0
    Top = 0
    Width = 877
    Height = 30
    AutoSize = True
    BorderWidth = 2
    Caption = 'Debug'
    Images = vilActions
    TabOrder = 0
    object ToolButtonRun: TToolButton
      Left = 0
      Top = 0
      Action = ActionDebugRun
      ImageName = 'Item1'
    end
    object ToolButtonStep: TToolButton
      Left = 23
      Top = 0
      Action = ActionDebugStep
      ImageName = 'Item2'
    end
    object ToolButtonGotoCursor: TToolButton
      Left = 46
      Top = 0
      Action = ActionDebugGotoCursor
      ImageName = 'Item3'
    end
    object ToolButtonPause: TToolButton
      Left = 69
      Top = 0
      Action = ActionDebugPause
      ImageName = 'Item4'
    end
    object ToolButtonStop: TToolButton
      Left = 92
      Top = 0
      Action = ActionDebugStop
      ImageName = 'Item5'
    end
    object ToolButtonSeparator: TToolButton
      Left = 115
      Top = 0
      Width = 8
      Caption = 'ToolButtonSeparator'
      ImageIndex = 5
      ImageName = 'Item6'
      Style = tbsSeparator
    end
    object ToolButtonToggleBreakpoint: TToolButton
      Left = 123
      Top = 0
      Action = ActionToggleBreakpoint
      ImageName = 'Item6'
    end
    object ToolButtonClearAllBreakpoints: TToolButton
      Left = 146
      Top = 0
      Action = ActionClearAllBreakpoints
      ImageName = 'Item7'
    end
  end
  object SynEditor: TSynEdit
    Left = 0
    Top = 30
    Width = 877
    Height = 533
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 1
    CodeFolding.ShowCollapsedLine = True
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
        Kind = gbkCustom
        Width = 13
        OnPaintLines = SynEditorTSynGutterBands1PaintLines
        OnCLick = ClickDebugBand
        OnMouseCursor = SynEditorTSynGutterBands1MouseCursor
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
    Highlighter = SynPasSyn
    Options = [eoAutoIndent, eoKeepCaretX, eoScrollByOneLess, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
    ReadOnly = True
    SelectedColor.Alpha = 0.400000005960464500
    OnSpecialLineColors = SynEditorSpecialLineColors
  end
  object Statusbar: TStatusBar
    Left = 0
    Top = 563
    Width = 877
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = ' Ready'
  end
  object ActionListMain: TActionList
    Left = 104
    Top = 100
    object ActionDebugRun: TAction
      Category = 'Debug'
      Caption = 'Run'
      ImageIndex = 0
      ShortCut = 120
      OnExecute = ActionDebugRunExecute
      OnUpdate = ActionDebugRunUpdate
    end
    object ActionDebugStep: TAction
      Category = 'Debug'
      Caption = 'Step'
      ImageIndex = 1
      ShortCut = 119
      OnExecute = ActionDebugStepExecute
      OnUpdate = ActionDebugStepUpdate
    end
    object ActionDebugGotoCursor: TAction
      Category = 'Debug'
      Caption = 'Goto Cursor'
      ImageIndex = 2
      ShortCut = 115
      OnExecute = ActionDebugGotoCursorExecute
      OnUpdate = ActionDebugGotoCursorUpdate
    end
    object ActionDebugPause: TAction
      Category = 'Debug'
      Caption = 'Pause'
      ImageIndex = 3
      ShortCut = 27
      OnExecute = ActionDebugPauseExecute
      OnUpdate = ActionDebugPauseUpdate
    end
    object ActionDebugStop: TAction
      Category = 'Debug'
      Caption = 'Stop'
      ImageIndex = 4
      ShortCut = 16497
      OnExecute = ActionDebugStopExecute
      OnUpdate = ActionDebugStopUpdate
    end
    object ActionToggleBreakpoint: TAction
      Category = 'Debug'
      Caption = 'Toggle Breakpoint'
      ImageIndex = 5
      ShortCut = 16503
      OnExecute = ActionToggleBreakpointExecute
      OnUpdate = ActionToggleBreakpointUpdate
    end
    object ActionClearAllBreakpoints: TAction
      Category = 'Debug'
      Caption = 'Clear All Breakpoints'
      ImageIndex = 6
      ShortCut = 24695
      OnExecute = ActionClearAllBreakpointsExecute
      OnUpdate = ActionClearAllBreakpointsUpdate
    end
  end
  object SynPasSyn: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 172
    Top = 100
  end
  object MainMenu: TMainMenu
    Left = 36
    Top = 100
    object MenuItemDebug: TMenuItem
      Caption = '&Debug'
      object miDebugRun: TMenuItem
        Action = ActionDebugRun
      end
      object miDebugStep: TMenuItem
        Action = ActionDebugStep
      end
      object miDebugGotoCursor: TMenuItem
        Action = ActionDebugGotoCursor
      end
      object miDebugPause: TMenuItem
        Action = ActionDebugPause
      end
      object miDebugStop: TMenuItem
        Action = ActionDebugStop
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miToggleBreakpoint: TMenuItem
        Action = ActionToggleBreakpoint
      end
      object miClearBreakpoints: TMenuItem
        Action = ActionClearAllBreakpoints
      end
    end
  end
  object icActions: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000664944415478DAE5934B0E80200C443BE76E97F4DCA335608C360195
              85895DF1C9BC3E20A09442192C35C5790D0150D56ED8DDE701204E4A1D37000E
              6CF201E0B5C136411E0EA3BF186416ED45BE710766D7BDDB06D17D07AF16738E
              D04DD74A3F1365389FD602330B8F76A59538490000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000006C4944415478DACD913B0EC0200C43ED739311CE1D48A5B691F80894
              A1F504167E044385A2E4A222D852CE4092C47B4F03104515AFB9929DF5900B70
              2A7F6107A0594DCDA75F6F014681B1F715C0825D1FA71378884DF1006685CD20
              A10E424FF839A0FFB6757808385518500132378276F47493C90000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000564944415478DADD923B0E00200843DB73EBA8E746A38383D618598C
              6C5078E14783C1637C0B406433040E9F4DAC39323601BA18B82A5E41AA98C40C
              11470077076A075780DDF2548E3CA3AB834F000AB2FDC41B73030A5BE76AF209
              7A17900000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000006B4944415478DACD92DB0E80300843C7779747F86EB4461363D04D83
              C63E1146CFBA8B9859B4414121C79E1000A06B76F79701B25B8A780028398248
              6E66A29204AAACD062D9CB83F5AD04AA9CF319008EB25B9C204BB1BDC83709FE
              71075780D37FD075AF4A01D186FDA92625AE8580FF220A750000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000A94944415478DA8D524B028420088573E312CECD880E86A6099B8A7C
              1F9E20332B248B0AA13D05B06398018D8088AE6011E92FA580838DF04820885D
              55F5210860773408FEE75B19E6A5E855C1D652E8E35C4798946F04D1C5380FEF
              66A9DD9483DDDCFE4DA07707433D8416AFF0FB160EA9372110AD49ECF7C05DAC
              EA6982690F5CB9FE374791207D0B712FD69548ED41CCA48556D507317C847822
              D88E70452F214E2328A4F1DBFA01C4C3941DAC8511210000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              610000006A4944415478DAB591510EC0200843E9FD0FCDA64B9785018A6EFC68
              80BE14808A4A1410F4E2D983B02702DC6255012EBD0772014D4CE1EB359032C0
              3AA93B304EBE017061B4B70468859EE09CD5112C847FF7C4D10E9E902CFE03CC
              42A6CF3812A780CCC11050896DC001D5C9A6F182C9CF7F0000000049454E44AE
              426082}
          end>
      end
      item
        Name = 'Item7'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000B54944415478DA95934112C4200804E1DD70D47713240B8568DC848B
              A5C534032A0A08E440403BD0738417811910621140BCF5FF4001186217D6F504
              3A023C4EA0570E9A67B706CC3C410C507BCF6B77272A26A2C509DEBA94982A2D
              956B018504C0AC6AA266CE534A958F80E829F7ACC10FAD05C067E090DEFBE282
              647E6C5E2800F916EAD07C5F215B401EDA104F8ED2D91660C9BFEB241BCEDA96
              BB70F1F2170C302A31A1BF8DA7D8035A9721860F81F53B7F8D0B459BC6F1E7AB
              C3690000000049454E44AE426082}
          end>
      end>
    Left = 24
    Top = 40
  end
  object vilActions: TVirtualImageList
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
      end
      item
        CollectionIndex = 6
        CollectionName = 'Item7'
        Name = 'Item7'
      end>
    ImageCollection = icActions
    Left = 40
    Top = 248
  end
  object icGutterGlyphs: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000000374944415478DA63FCCFF09F8158C0385C148305195B814435235EC5
              1085301EAA064CC50CAD40816AE2142334301076C6200A3AAA28060024E22AF3
              E531F6900000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              950000005B4944415478DAAD91C10AC0300843F5BB739DDF9DB5DB5C0B8A085B
              A0B487F772489542E9463FC12A460AB40DCF3B137298180F0BC2057BDB9B631C
              4461C10E983DA0234BD89A51EC700B0D386DDE830016D32180FFECDCFEC12A27
              74D448F3FFFD10100000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000000724944415478DAA591DB1280200844E1BBE111BE9B40CB4B56EAB42F
              3A725898150D0C5685BF60043503C229ACA2C69CEF22F924AE8D050E909C4470
              CAC82BEA15F677290D098ED1DDBC70A5B1A1C217A07A8265B9B4520F037DE410
              1396E047E756D438DE761ED248F04B1ADB396FFFE04C07A15F62F3DDB4913500
              00000049454E44AE426082}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000000584944415478DAAD91C10DC0300803F1FE4393405462D15AE923FEFA
              3824809BDBDFE01A8CA867268317FC9415AF22070A0EB0EFC85134B304438EE4
              B759C1CB7780D9FA091BAF25AB34F32D24CCD750D6F39D09BCFBEE9E01F0504F
              F37FFCC2070000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              950000004B4944415478DA63FCCFF09F8158C0481FC58C202E1A00CA33622806
              29C46607239206B062B8C2FF505984B1603E4C03A6628471703676C5C81A1810
              B650C16462DC4C5268901CCE548F6E006B5846F37E3D33BE0000000049454E44
              AE426082}
          end>
      end
      item
        Name = 'Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D494844520000000B0000000E0806000000F961E6
              95000000504944415478DA63FCCFF09F8158C0481FC58C202E1A00CA33622806
              296C69696170727262D8B76F1F9CAEA9A9816B002BC6A5105D035CF1B163C7B0
              2A04D1565656A88A0781C9248506C9E14CF5E806008ACA84F3BD56245C000000
              0049454E44AE426082}
          end>
      end>
    Left = 136
    Top = 36
  end
  object vilGutterGlyphs: TVirtualImageList
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
    ImageCollection = icGutterGlyphs
    Width = 11
    Height = 14
    Left = 160
    Top = 244
  end
end
