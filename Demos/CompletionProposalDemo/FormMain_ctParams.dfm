object Form1: TForm1
  Left = 256
  Top = 122
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Form1'
  ClientHeight = 753
  ClientWidth = 1116
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 20
  object SynTest: TSynEdit
    Left = 624
    Top = 12
    Width = 482
    Height = 746
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    BookMarkOptions.LeftMargin = 3
    BookMarkOptions.Xoffset = 18
    ExtraLineSpacing = 3
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.Visible = False
    Gutter.Bands = <>
    Lines.Strings = (
      'This is a Demo to show you how the '
      'Code Completion component works '
      'when the default kind is ctParams.'
      ''
      'Everything really depends on the'
      'code you put in the execute event.'
      'This determines what the parameters'
      'are and what index should be'
      'highlighted.'
      ''
      'See the source file execute event'
      'for more information.')
    ScrollbarAnnotations = <>
    RemovedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 16496
      end>
  end
  object SynEdit1: TSynEdit
    Left = 9
    Top = 144
    Width = 602
    Height = 602
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 1
    UseCodeFolding = False
    BookMarkOptions.LeftMargin = 3
    BookMarkOptions.Xoffset = 18
    ExtraLineSpacing = 3
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.Bands = <>
    Lines.Strings = (
      'Use Shift+Ctrl+Space to activate Parameter'
      'completion, or type the function name'
      'and the '#39'('#39' (open paren) to start it with'
      'the timer.'
      ''
      'The valid functions for this example are'
      ''
      'TestFunction'
      'Min'
      'Max'
      ''
      'Below is an example using paren Counting:'
      'Max(a + b(1 + 2), (3 + 4) * c)'
      ''
      'Here is an example of embeded functions'
      ''
      'TestFunction(Min(a, b), SomeVar, Another)')
    ScrollbarAnnotations = <>
    RemovedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 16496
      end>
  end
  object Button3: TButton
    Left = 12
    Top = 60
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Font'
    TabOrder = 2
    OnClick = Button3Click
  end
  object scpParams: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoLimitToMatchedText, scoUsePrettyText, scoUseBuiltInTimer]
    ClBackground = clInfoBk
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    GripperFont.Charset = DEFAULT_CHARSET
    GripperFont.Color = clBtnText
    GripperFont.Height = -12
    GripperFont.Name = 'Segoe UI'
    GripperFont.Style = []
    Columns = <>
    OnExecute = scpParamsExecute
    ShortCut = 24608
    Editor = SynEdit1
    TimerInterval = 1200
    Left = 8
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 44
  end
end
