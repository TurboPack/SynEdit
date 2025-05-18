object TestPrintPreviewDlg: TTestPrintPreviewDlg
  Left = 192
  Top = 148
  Caption = 'Print Preview'
  ClientHeight = 449
  ClientWidth = 633
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  WindowState = wsMaximized
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 633
    Height = 23
    AutoSize = True
    ButtonHeight = 23
    ButtonWidth = 24
    Caption = 'ToolBar1'
    Images = PreviewImages
    Indent = 5
    TabOrder = 0
    object FirstBtn: TToolButton
      Left = 5
      Top = 0
      Action = FirstCmd
      ImageName = 'Item1'
      ParentShowHint = False
      ShowHint = True
    end
    object PrevBtn: TToolButton
      Left = 29
      Top = 0
      Action = PrevCmd
      ImageName = 'Item2'
      ParentShowHint = False
      ShowHint = True
    end
    object NextBtn: TToolButton
      Left = 53
      Top = 0
      Action = NextCmd
      ImageName = 'Item3'
      ParentShowHint = False
      ShowHint = True
    end
    object LastBtn: TToolButton
      Left = 77
      Top = 0
      Action = LastCmd
      ImageName = 'Item4'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton1: TToolButton
      Left = 101
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 2
      ImageName = 'Item3'
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 109
      Top = 0
      Action = ZoomCmd
      DropdownMenu = PopupMenu1
      ImageName = 'Item5'
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
    end
    object ToolButton5: TToolButton
      Left = 148
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 4
      ImageName = 'Item5'
      Style = tbsSeparator
    end
    object PrintBtn: TToolButton
      Left = 156
      Top = 0
      Action = PrintCmd
      ImageName = 'Item6'
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton4: TToolButton
      Left = 180
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 4
      ImageName = 'Item5'
      Style = tbsSeparator
    end
    object CloseBtn: TToolButton
      Left = 188
      Top = 0
      Action = CloseCmd
      ImageName = 'Item7'
      ParentShowHint = False
      ShowHint = True
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 430
    Width = 633
    Height = 19
    Panels = <
      item
        Width = 400
      end
      item
        Width = 100
      end>
  end
  object SynEditPrintPreview: TSynEditPrintPreview
    Left = 0
    Top = 23
    Width = 633
    Height = 407
    ScaleMode = pscWholePage
    OnMouseDown = SynEditPrintPreviewMouseDown
    OnPreviewPage = SynEditPrintPreviewPreviewPage
  end
  object ActionList: TActionList
    Images = PreviewImages
    Left = 80
    Top = 39
    object FirstCmd: TAction
      Caption = 'FirstCmd'
      Hint = 'First|Go to first page'
      ImageIndex = 0
      ShortCut = 32838
      OnExecute = FirstCmdExecute
    end
    object PrevCmd: TAction
      Caption = 'PrevCmd'
      Hint = 'Previous|Go to previous page'
      ImageIndex = 1
      ShortCut = 32848
      OnExecute = PrevCmdExecute
    end
    object NextCmd: TAction
      Caption = 'NextCmd'
      Hint = 'Next|Go to next page'
      ImageIndex = 2
      ShortCut = 32846
      OnExecute = NextCmdExecute
    end
    object LastCmd: TAction
      Caption = 'LastCmd'
      Hint = 'Last|Go to last page'
      ImageIndex = 3
      ShortCut = 32844
      OnExecute = LastCmdExecute
    end
    object ZoomCmd: TAction
      Caption = 'FitCmd'
      Hint = 'Zoom|Zoom In/Out'
      ImageIndex = 4
      ShortCut = 32858
      OnExecute = ZoomCmdExecute
    end
    object PrintCmd: TAction
      Caption = 'PrintCmd'
      Hint = 'Print|Print the document'
      ImageIndex = 5
      ShortCut = 16464
      OnExecute = PrintCmdExecute
    end
    object CloseCmd: TAction
      Caption = 'CloseCmd'
      Hint = 'Close|Close Print Preview'
      ImageIndex = 6
      ShortCut = 32835
      OnExecute = CloseCmdExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 172
    Top = 41
    object Fitto1: TMenuItem
      Tag = -1
      Caption = 'Whole page'
      OnClick = Fitto1Click
    end
    object Pagewidth1: TMenuItem
      Tag = -2
      Caption = 'Page width'
      OnClick = Fitto1Click
    end
    object N1: TMenuItem
      Caption = '-'
      OnClick = Fitto1Click
    end
    object N251: TMenuItem
      Tag = 25
      Caption = '25%'
      OnClick = Fitto1Click
    end
    object N501: TMenuItem
      Tag = 50
      Caption = '50%'
      OnClick = Fitto1Click
    end
    object N1001: TMenuItem
      Tag = 100
      Caption = '100%'
      OnClick = Fitto1Click
    end
    object N2001: TMenuItem
      Tag = 200
      Caption = '200%'
      OnClick = Fitto1Click
    end
    object N4001: TMenuItem
      Tag = 400
      Caption = '400%'
      OnClick = Fitto1Click
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnHint = ApplicationEvents1Hint
    Left = 81
    Top = 105
  end
  object PreviewImgCollection: TImageCollection
    Images = <
      item
        Name = 'Item1'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000494944415478DA63FCCFF09F8152C038B80D616468FDFF9FA19A9168
              71620D01898168B20D811940B621C806906508BA01241B822B26E8EF12AA8509
              36EF0D6C3A21283EB83320A90000678660F02335AF590000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'Item2'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000494944415478DA63FCCFF09F8152C038F40C616468FDFF9FA19A916C
              4340068068B20D811940B621C806906508BA01241B82CD80817109D5C2049B41
              039B4E900DA2D810BC160C1A4300FE9042F099E962A80000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'Item3'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000474944415478DACDD2310A00200C03C0E4DDCEBE3BE203B42901314B
              B7A3B4A120A4E1FF0831250CC6C89E0E54220E642115642337A8859CA0B79BC4
              3789BF13F7246E6C27FF200BE29F42F02A410B900000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item4'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA000000494944415478DA63FCCFF09F8152C038B80D616468FDFF9FA19A9168
              715C868068740D6419826E10D986201B44912130832836049BF7E8EF128AC384
              E2D8A1389D509C62490583C71000777760F0365675BF0000000049454E44AE42
              6082}
          end>
      end
      item
        Name = 'Item5'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA0000009A4944415478DAAD92590EC0200844E1E4C0C9A9638A5A53B58BF3A3
              10F2322CECE414323557D512E32F2A4C0B7140387D5B400B4A35BC8404404432
              8DCE245E335B823815642B2D80D9C8BDC600E59A416B487AE9FD74C064EE54E3
              98D5C84D85242770D00B8E562DED71B265265BB6F3E44EE29D3A098D2E16F9D9
              E15D2033C169D958077A0C99815E4146A0D7903BD0274880B003D11F90560729
              C6ACF01B1D0EAE0000000049454E44AE426082}
          end>
      end
      item
        Name = 'Item6'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA0000008C4944415478DAAD924B0EC0200844E5E4EAC9A934152988F5533626
              04DF0CA38001C369410F02D4FEA8720F5C08014A8D9501E6203468D49FBE0418
              C88E0B173272D1CB85212B2EEE59791264E635AAB204B41E39553BBA4EB4033E
              C3FA6FE32C24C4739253668198A2BA08D860A3755064054DDD3ADA5CE7E5A4BE
              CE4CB8BD5518A26DCE066B2027F50BE402108487F055E65F6E0000000049454E
              44AE426082}
          end>
      end
      item
        Name = 'Item7'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000110000001108060000003B6D47
              FA0000005B4944415478DAEDD3B10EC020080450EECBC52FBFD2416D2C8D5163
              74A81372E131010A65F6A14620B1A952029A88AAE6FF5D93A1E4883F72260233
              2C1F470C48F918F218EA466AA01BF1804FC4F6B9B7E2010979F5965CF136E402
              654E8C718A22DE430000000049454E44AE426082}
          end>
      end>
    Left = 80
    Top = 176
  end
  object PreviewImages: TVirtualImageList
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
    ImageCollection = PreviewImgCollection
    Width = 17
    Height = 17
    Left = 192
    Top = 176
  end
end
