object SynOmniSynSetup: TSynOmniSynSetup
  Left = 917
  Top = 105
  HelpContext = 115
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Omni Highlighter Setup'
  ClientHeight = 561
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    563
    561)
  TextHeight = 15
  object lGeneralName: TLabel
    Left = 8
    Top = 8
    Width = 35
    Height = 15
    Caption = 'Name:'
  end
  object lMasks: TLabel
    Left = 178
    Top = 6
    Width = 182
    Height = 15
    Caption = 'File extensions, comma-separated:'
  end
  object edLanguagelName: TEdit
    Left = 8
    Top = 24
    Width = 164
    Height = 23
    TabOrder = 0
    OnChange = edLanguagelNameChange
  end
  object gbComment: TGroupBox
    Left = 8
    Top = 150
    Width = 346
    Height = 221
    Caption = 'Comment styles:'
    TabOrder = 2
    object Bevel1: TBevel
      Left = 170
      Top = 16
      Width = 9
      Height = 194
      Shape = bsLeftLine
    end
    object cCommentAnsi: TCheckBox
      Left = 8
      Top = 16
      Width = 57
      Height = 17
      Hint = 'Pascal'
      Caption = '(*'#8230'*)'
      TabOrder = 0
      OnClick = edLanguagelNameChange
    end
    object cCommentPas: TCheckBox
      Left = 8
      Top = 32
      Width = 57
      Height = 17
      Hint = 'Pascal'
      Caption = '{'#8230'}'
      TabOrder = 1
      OnClick = edLanguagelNameChange
    end
    object cCommentC: TCheckBox
      Left = 8
      Top = 96
      Width = 57
      Height = 17
      Hint = 'Ansi'
      Caption = '/*'#8230'*/'
      TabOrder = 5
      OnClick = edLanguagelNameChange
    end
    object cCommentSpec: TCheckBox
      Left = 284
      Top = 16
      Width = 49
      Height = 17
      Caption = '/'
      TabOrder = 43
      OnClick = edLanguagelNameChange
    end
    object cCommentBas: TCheckBox
      Left = 183
      Top = 160
      Width = 33
      Height = 17
      Hint = 'Basic based'
      Caption = #8230' ;'
      TabOrder = 31
      OnClick = edLanguagelNameChange
    end
    object cCommentFox: TCheckBox
      Left = 231
      Top = 112
      Width = 41
      Height = 17
      Caption = '*  &&&&'
      TabOrder = 38
      OnClick = edLanguagelNameChange
    end
    object cCommentREM: TCheckBox
      Left = 284
      Top = 80
      Width = 57
      Height = 17
      Hint = 'MS-Dos Bat'
      Caption = 'REM  ::'
      TabOrder = 47
      OnClick = edLanguagelNameChange
    end
    object cCommentExcl: TCheckBox
      Left = 183
      Top = 32
      Width = 33
      Height = 17
      Caption = '!'
      TabOrder = 24
      OnClick = edLanguagelNameChange
    end
    object cCommentBy: TCheckBox
      Left = 183
      Top = 112
      Width = 33
      Height = 17
      Caption = '@'
      TabOrder = 28
      OnClick = edLanguagelNameChange
    end
    object cCommentSharp: TCheckBox
      Left = 183
      Top = 16
      Width = 33
      Height = 17
      Caption = '#'
      TabOrder = 23
      OnClick = edLanguagelNameChange
    end
    object cCommentSlash: TCheckBox
      Left = 284
      Top = 48
      Width = 41
      Height = 17
      Caption = '//'
      TabOrder = 45
      OnClick = edLanguagelNameChange
    end
    object cCommentPercent: TCheckBox
      Left = 183
      Top = 48
      Width = 33
      Height = 17
      Caption = '%'
      TabOrder = 25
      OnClick = edLanguagelNameChange
    end
    object cCommentSinglQ: TCheckBox
      Left = 183
      Top = 80
      Width = 33
      Height = 17
      Caption = #39
      TabOrder = 26
      OnClick = edLanguagelNameChange
    end
    object cCommentSQL: TCheckBox
      Left = 231
      Top = 80
      Width = 33
      Height = 17
      Hint = 'SQL'
      Caption = '--'
      TabOrder = 36
      OnClick = edLanguagelNameChange
    end
    object cCommentDblQ: TCheckBox
      Left = 183
      Top = 96
      Width = 33
      Height = 17
      Caption = '"'
      TabOrder = 27
      OnClick = edLanguagelNameChange
    end
    object cCommentFortran: TCheckBox
      Left = 231
      Top = 48
      Width = 33
      Height = 17
      Hint = 'Fortran'
      Caption = 'C'
      TabOrder = 34
      OnClick = edLanguagelNameChange
    end
    object cCommentCStar: TCheckBox
      Left = 231
      Top = 64
      Width = 33
      Height = 17
      Caption = 'C*'
      TabOrder = 35
      OnClick = edLanguagelNameChange
    end
    object cCommentDollar: TCheckBox
      Left = 8
      Top = 176
      Width = 65
      Height = 17
      Caption = '$('#8230'$)  $*'
      TabOrder = 10
      OnClick = edLanguagelNameChange
    end
    object cCommentLBracket: TCheckBox
      Left = 183
      Top = 144
      Width = 33
      Height = 17
      Caption = '{'
      TabOrder = 30
      OnClick = edLanguagelNameChange
    end
    object cCommentPoco: TCheckBox
      Left = 231
      Top = 128
      Width = 41
      Height = 17
      Hint = 'Poco'
      Caption = '* ;'
      TabOrder = 39
      OnClick = edLanguagelNameChange
    end
    object cCommentSmart: TCheckBox
      Left = 8
      Top = 64
      Width = 57
      Height = 17
      Hint = 'Smarty'
      Caption = '{*'#8230'*}'
      TabOrder = 3
      OnClick = edLanguagelNameChange
    end
    object cCommentHaskell: TCheckBox
      Left = 8
      Top = 48
      Width = 57
      Height = 17
      Hint = 'Haskell'
      Caption = '{-'#8230'-}'
      TabOrder = 2
      OnClick = edLanguagelNameChange
    end
    object cCommentPipe: TCheckBox
      Left = 183
      Top = 128
      Width = 33
      Height = 17
      Hint = 'Baan4GL'
      Caption = '|'
      TabOrder = 29
      OnClick = edLanguagelNameChange
    end
    object cCommentWebFocus: TCheckBox
      Left = 231
      Top = 96
      Width = 41
      Height = 17
      Hint = 'WebFocus'
      Caption = '-*'
      TabOrder = 37
      OnClick = edLanguagelNameChange
    end
    object cCommentD: TCheckBox
      Left = 8
      Top = 80
      Width = 57
      Height = 17
      Hint = 'D language'
      Caption = '/+'#8230'+/'
      TabOrder = 4
      OnClick = edLanguagelNameChange
    end
    object cCommentJCL: TCheckBox
      Left = 8
      Top = 112
      Width = 57
      Height = 17
      Hint = 'IBM mainframe JCL'
      Caption = '//*'#8230'*//'
      TabOrder = 6
      OnClick = edLanguagelNameChange
    end
    object cCommentDMIS: TCheckBox
      Left = 231
      Top = 32
      Width = 33
      Height = 17
      Hint = 'DMIS - Dimensional Measuring Interface Specification'
      Caption = '$$'
      TabOrder = 33
      OnClick = edLanguagelNameChange
    end
    object cCommentVLisp: TCheckBox
      Left = 8
      Top = 160
      Width = 49
      Height = 17
      Hint = 'VLisp (AutoCad)'
      Caption = ';|'#8230'|;'
      TabOrder = 9
      OnClick = edLanguagelNameChange
    end
    object cCommentDead: TCheckBox
      Left = 88
      Top = 64
      Width = 65
      Height = 17
      Hint = 'Jovial language (Algol derivative)'
      Caption = '"'#8230'"'
      TabOrder = 15
      OnClick = edLanguagelNameChange
    end
    object cCommentCLisp: TCheckBox
      Left = 88
      Top = 16
      Width = 65
      Height = 17
      Hint = 'CLisp (AutoCad)'
      Caption = '#|'#8230'|#'
      TabOrder = 12
      OnClick = edLanguagelNameChange
    end
    object cComment2Excl: TCheckBox
      Left = 88
      Top = 48
      Width = 65
      Height = 17
      Hint = 'Mynx'
      Caption = '!!'#8230' !!'
      TabOrder = 14
      OnClick = edLanguagelNameChange
    end
    object cCommentCPL: TCheckBox
      Left = 231
      Top = 16
      Width = 33
      Height = 17
      Hint = 'CPL'
      Caption = '$'
      TabOrder = 32
      OnClick = edLanguagelNameChange
    end
    object cCommentDollarMulti: TCheckBox
      Left = 8
      Top = 144
      Width = 57
      Height = 17
      Hint = 'Crossing, Vissim'
      Caption = '$'#8230'$'
      TabOrder = 8
      OnClick = edLanguagelNameChange
    end
    object cCommentForth: TCheckBox
      Left = 88
      Top = 80
      Width = 57
      Height = 17
      Hint = 'Forth'
      Caption = '( '#8230' )  \'
      TabOrder = 16
      OnClick = edLanguagelNameChange
    end
    object cCommentHTML: TCheckBox
      Left = 8
      Top = 128
      Width = 57
      Height = 17
      Hint = 'HTML'
      Caption = '<!--'#8230'-->'
      TabOrder = 7
    end
    object cCommentTab: TCheckBox
      Left = 284
      Top = 96
      Width = 59
      Height = 17
      Hint = 'TAB key'
      Caption = 'Tab'
      TabOrder = 48
      OnClick = edLanguagelNameChange
    end
    object cCommentStars: TCheckBox
      Left = 231
      Top = 144
      Width = 33
      Height = 17
      Hint = 'Odin, Abaqus'
      Caption = '**'
      TabOrder = 40
      OnClick = edLanguagelNameChange
    end
    object cCommentLua: TCheckBox
      Left = 88
      Top = 96
      Width = 57
      Height = 17
      Hint = 'LUA'
      Caption = '--[[ '#8230' ]]'
      TabOrder = 17
      OnClick = edLanguagelNameChange
    end
    object cCommentPCL: TCheckBox
      Left = 231
      Top = 160
      Width = 33
      Height = 17
      Hint = 'PCL extension'
      Caption = '!*'
      TabOrder = 41
      OnClick = edLanguagelNameChange
    end
    object cCommentLilypond: TCheckBox
      Left = 88
      Top = 112
      Width = 65
      Height = 17
      Hint = 'Lilypond'
      Caption = '%('#8230'%)'
      TabOrder = 18
      OnClick = edLanguagelNameChange
    end
    object cCommentSpace: TCheckBox
      Left = 284
      Top = 112
      Width = 59
      Height = 17
      Hint = 'Descript.ion'
      Caption = 'Space'
      TabOrder = 49
      OnClick = edLanguagelNameChange
    end
    object cCommentJCL2: TCheckBox
      Left = 284
      Top = 64
      Width = 41
      Height = 17
      Caption = '//*'
      TabOrder = 46
      OnClick = edLanguagelNameChange
    end
    object cCommentAutomaton: TCheckBox
      Left = 183
      Top = 64
      Width = 41
      Height = 17
      Hint = 'Automaton'
      Caption = '%%'
      TabOrder = 50
      OnClick = edLanguagelNameChange
    end
    object cCommentLineC: TCheckBox
      Left = 284
      Top = 32
      Width = 49
      Height = 17
      Caption = '/*'
      TabOrder = 44
      OnClick = edLanguagelNameChange
    end
    object cCommentOkuma: TCheckBox
      Left = 88
      Top = 128
      Width = 57
      Height = 17
      Hint = 'OKUMA, OSP'
      Caption = '('#8230')'
      TabOrder = 19
      OnClick = edLanguagelNameChange
    end
    object cCommentHeller: TCheckBox
      Left = 88
      Top = 144
      Width = 65
      Height = 17
      Hint = 'HELLER, UniPro CNC-90'
      Caption = '(*'#8230')  (/'#8230')'
      TabOrder = 20
      OnClick = edLanguagelNameChange
    end
    object cCommentPwShell: TCheckBox
      Left = 88
      Top = 160
      Width = 57
      Height = 17
      Hint = 'Power Shell'
      Caption = '<#'#8230'#>'
      TabOrder = 21
      OnClick = edLanguagelNameChange
    end
    object cCommentDash: TCheckBox
      Left = 284
      Top = 128
      Width = 49
      Height = 17
      Hint = 'MEGA'
      Caption = '-'
      TabOrder = 51
      OnClick = edLanguagelNameChange
    end
    object cCommentSS: TCheckBox
      Left = 88
      Top = 176
      Width = 57
      Height = 17
      Hint = 'From * to ;'
      Caption = '*'#8230' ;'
      TabOrder = 22
      OnClick = edLanguagelNameChange
    end
    object cCommentBackSlash: TCheckBox
      Left = 284
      Top = 144
      Width = 49
      Height = 17
      Hint = '4680 Basic'
      Caption = '\'
      TabOrder = 52
      OnClick = edLanguagelNameChange
    end
    object cCommentAngleBracket: TCheckBox
      Left = 284
      Top = 160
      Width = 49
      Height = 17
      Caption = '<<<'
      TabOrder = 53
      OnClick = edLanguagelNameChange
    end
    object cCommentINI: TCheckBox
      Left = 183
      Top = 176
      Width = 33
      Height = 17
      Hint = 'INI file based'
      Caption = ';'
      TabOrder = 54
      OnClick = edLanguagelNameChange
    end
    object cCommentTexInfo: TCheckBox
      Left = 8
      Top = 192
      Width = 95
      Height = 17
      Hint = 'TexInfo comment @c. @ignore ... @end ignore, @bye'
      Caption = '@c, @ignore'
      TabOrder = 11
      OnClick = edLanguagelNameChange
    end
    object cCommentEuklid: TCheckBox
      Left = 231
      Top = 176
      Width = 33
      Height = 17
      Hint = 'Euklid+'
      Caption = '..'
      TabOrder = 42
      OnClick = edLanguagelNameChange
    end
    object cCommentAutoIt: TCheckBox
      Left = 88
      Top = 32
      Width = 75
      Height = 17
      Hint = 'AutoIt3'
      Caption = '#cs .. #ce'
      TabOrder = 13
      OnClick = edLanguagelNameChange
    end
  end
  object gbStrings: TGroupBox
    Left = 8
    Top = 380
    Width = 166
    Height = 137
    Caption = 'Strings:'
    TabOrder = 3
    object rSingleQuote: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 156
      Height = 17
      Align = alTop
      Caption = 'Single quotation marks'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = edLanguagelNameChange
    end
    object rDoubleQuote: TCheckBox
      Left = 5
      Top = 39
      Width = 145
      Height = 17
      Caption = 'Double quotes'
      TabOrder = 1
      OnClick = edLanguagelNameChange
    end
    object rApostropheQuote: TCheckBox
      Left = 5
      Top = 58
      Width = 145
      Height = 17
      Caption = 'Inverted apostrophes'
      TabOrder = 2
      OnClick = edLanguagelNameChange
    end
    object eEscChar: TEdit
      Left = 128
      Top = 81
      Width = 25
      Height = 23
      MaxLength = 1
      TabOrder = 3
      Text = '\'
    end
    object cEscString: TCheckBox
      Left = 5
      Top = 84
      Width = 98
      Height = 17
      Caption = 'String escapes'
      TabOrder = 4
      OnClick = edLanguagelNameChange
    end
  end
  object bLoad: TButton
    Left = 16
    Top = 527
    Width = 65
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Load'
    TabOrder = 5
    OnClick = bLoadClick
  end
  object bSave: TButton
    Left = 88
    Top = 527
    Width = 65
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Save'
    Enabled = False
    TabOrder = 6
    OnClick = bSaveClick
  end
  object btnCancel: TButton
    Left = 460
    Top = 527
    Width = 75
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object edFileType: TEdit
    Left = 178
    Top = 25
    Width = 182
    Height = 23
    CharCase = ecLowerCase
    TabOrder = 1
    OnChange = edLanguagelNameChange
    OnKeyPress = edFileTypeKeyPress
  end
  object gbMisc: TGroupBox
    Left = 180
    Top = 380
    Width = 174
    Height = 137
    Caption = 'Other Options:'
    TabOrder = 4
    object cRubySymbols: TCheckBox
      Left = 8
      Top = 74
      Width = 137
      Height = 17
      Caption = 'Ruby symbols'
      TabOrder = 0
      OnClick = edLanguagelNameChange
    end
    object cPHPVariable: TCheckBox
      Left = 8
      Top = 56
      Width = 137
      Height = 17
      Caption = 'PHP style variables'
      TabOrder = 1
      OnClick = edLanguagelNameChange
    end
    object cVectors: TCheckBox
      Left = 8
      Top = 38
      Width = 137
      Height = 17
      Caption = 'Vectors'
      TabOrder = 2
      OnClick = edLanguagelNameChange
    end
    object cHTML: TCheckBox
      Left = 8
      Top = 20
      Width = 164
      Height = 17
      Caption = 'HTML'
      TabOrder = 3
      OnClick = edLanguagelNameChange
    end
    object cPreprocessors: TCheckBox
      Left = 8
      Top = 92
      Width = 137
      Height = 17
      Caption = 'Preprocesor'
      TabOrder = 4
      OnClick = edLanguagelNameChange
    end
    object cLabel: TCheckBox
      Left = 8
      Top = 114
      Width = 137
      Height = 17
      Caption = 'Label'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = edLanguagelNameChange
    end
  end
  object btnOK: TButton
    Left = 373
    Top = 528
    Width = 75
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object gProperties: TGroupBox
    Left = 8
    Top = 53
    Width = 352
    Height = 84
    TabOrder = 9
    object lCodeFoldingType: TLabel
      Left = 14
      Top = 37
      Width = 98
      Height = 15
      Caption = 'Code folding type:'
    end
    object lKeyWordChars: TLabel
      Left = 14
      Top = 58
      Width = 169
      Height = 15
      Caption = 'Allowed characters in keywords:'
    end
    object cCaseSensitive: TCheckBox
      Left = 14
      Top = 14
      Width = 185
      Height = 17
      Caption = 'Case sensitive'
      TabOrder = 0
      OnClick = edLanguagelNameChange
    end
    object cbCodeFolding: TComboBox
      Left = 224
      Top = 29
      Width = 109
      Height = 23
      Style = csDropDownList
      TabOrder = 1
      OnChange = edLanguagelNameChange
    end
    object eKeyWordChars: TEdit
      Left = 224
      Top = 56
      Width = 109
      Height = 22
      Font.Charset = EASTEUROPE_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Text = '_'
      OnChange = edLanguagelNameChange
    end
  end
  object gbxKeywords: TGroupBox
    Left = 366
    Top = 8
    Width = 194
    Height = 509
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Keywords:'
    TabOrder = 10
    object pageKeyWords: TPageControl
      Left = 2
      Top = 17
      Width = 190
      Height = 490
      ActivePage = tsKeyWords1
      Align = alClient
      TabOrder = 0
      TabWidth = 30
      object tsKeyWords1: TTabSheet
        Caption = '1'
        object mKeyWords: TMemo
          Left = 0
          Top = 0
          Width = 182
          Height = 434
          Align = alTop
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
          OnChange = edLanguagelNameChange
        end
        object cKW1StartWith: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 440
          Width = 176
          Height = 17
          Align = alBottom
          Caption = 'Keyword prefixes'
          TabOrder = 1
        end
      end
      object tsKeyWords2: TTabSheet
        Caption = '2'
        ImageIndex = 1
        object mResWords: TMemo
          Left = 0
          Top = 0
          Width = 182
          Height = 433
          Align = alTop
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
          OnChange = edLanguagelNameChange
        end
        object cKW2StartWith: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 440
          Width = 176
          Height = 17
          Align = alBottom
          Caption = 'Keyword prefixes'
          TabOrder = 1
        end
      end
      object tsKeyWords3: TTabSheet
        Caption = '3'
        ImageIndex = 3
        object mKeyWords2: TMemo
          Left = 0
          Top = 0
          Width = 182
          Height = 434
          Align = alTop
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
          OnChange = edLanguagelNameChange
        end
        object cKW3StartWith: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 440
          Width = 176
          Height = 17
          Align = alBottom
          Caption = 'Keyword prefixes'
          TabOrder = 1
        end
      end
      object tsKeyWords4: TTabSheet
        Caption = '4'
        ImageIndex = 2
        object mKeyWords3: TMemo
          Left = 0
          Top = 0
          Width = 182
          Height = 433
          Align = alTop
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
          OnChange = edLanguagelNameChange
        end
        object cKW4StartWith: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 440
          Width = 176
          Height = 17
          Align = alBottom
          Caption = 'Keyword prefixes'
          TabOrder = 1
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.ini'
    Filter = 'HighLighters (*.ini)|*.ini'
    Left = 408
    Top = 40
  end
end
