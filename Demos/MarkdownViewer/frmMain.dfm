object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 403
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object SynEdit: TSynEdit
    Left = 408
    Top = 0
    Width = 362
    Height = 403
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
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
    SelectedColor.Alpha = 0.400000005960464500
    VisibleSpecialChars = []
  end
  object btnRender: TButton
    Left = 377
    Top = 160
    Width = 25
    Height = 25
    Hint = 'Click to render the markdown'
    Caption = '>'
    TabOrder = 2
    OnClick = btnRenderClick
  end
  object reMarkdown: TRichEdit
    Left = 0
    Top = 0
    Width = 371
    Height = 403
    Align = alLeft
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
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
