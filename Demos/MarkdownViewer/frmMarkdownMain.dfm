object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Form1'
  ClientHeight = 605
  ClientWidth = 1164
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 25
  object SynEdit: TSynEdit
    Left = 621
    Top = 0
    Width = 543
    Height = 605
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -23
    Font.Name = 'Consolas'
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
    Gutter.Bands = <
      item
        Kind = gbkMarks
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
        Width = 3
      end>
    ScrollbarAnnotations = <>
  end
  object btnRender: TButton
    Left = 566
    Top = 240
    Width = 37
    Height = 38
    Hint = 'Click to render the markdown'
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '>'
    TabOrder = 2
    OnClick = btnRenderClick
  end
  object reMarkdown: TRichEdit
    Left = 0
    Top = 0
    Width = 557
    Height = 605
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alLeft
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -23
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      '# Demo of the Markdown Viewer component'
      'This component is a subclass of TSynEdit.'
      'Press the button on the right, to render '
      'this markdown document.'
      ''
      '## Title Level 2'
      '### Links'
      '---'
      'Some text with a link to '
      '[Google](https://www.google.com).'
      'See what happens when you hover over '
      'the link or click it.'
      ''
      '### Emphasis and code blocks'
      'The `Component` supports *italic*, '
      '**bold** and ***bold italic*** text.'
      ''
      '#### Bullets'
      '- Bullet 1'
      '* Bullet 2'
      '+ Bullet 3'
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    StyleElements = [seBorder]
  end
end
