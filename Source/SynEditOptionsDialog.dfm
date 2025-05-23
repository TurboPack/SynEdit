object fmEditorOptionsDialog: TfmEditorOptionsDialog
  Left = 580
  Top = 154
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 415
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    388
    415)
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 6
    Top = 8
    Width = 380
    Height = 363
    ActivePage = Display
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Display: TTabSheet
      BorderWidth = 6
      Caption = 'Display'
      object gbGutter: TGroupBox
        Left = 0
        Top = 0
        Width = 360
        Height = 121
        Align = alTop
        Caption = 'Gutter'
        TabOrder = 0
        object Label1: TLabel
          Left = 176
          Top = 96
          Width = 66
          Height = 15
          Caption = 'Gutter color:'
        end
        object ckGutterAutosize: TCheckBox
          Left = 9
          Top = 37
          Width = 120
          Height = 17
          Caption = 'Autosize'
          TabOrder = 1
        end
        object ckGutterShowLineNumbers: TCheckBox
          Left = 9
          Top = 56
          Width = 120
          Height = 17
          Caption = 'Show line numbers'
          TabOrder = 2
        end
        object ckGutterShowLeaderZeros: TCheckBox
          Left = 9
          Top = 94
          Width = 120
          Height = 17
          Caption = 'Show leading zeros'
          TabOrder = 4
        end
        object ckGutterStartAtZero: TCheckBox
          Left = 9
          Top = 75
          Width = 120
          Height = 17
          Caption = 'Start at zero'
          TabOrder = 3
        end
        object ckGutterVisible: TCheckBox
          Left = 9
          Top = 18
          Width = 120
          Height = 17
          Caption = 'Visible'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbGutterFont: TCheckBox
          Left = 176
          Top = 18
          Width = 120
          Height = 17
          Caption = 'Use Gutter Font'
          TabOrder = 5
          OnClick = cbGutterFontClick
        end
        object btnGutterFont: TButton
          Left = 292
          Top = 14
          Width = 40
          Height = 25
          Caption = 'Font'
          TabOrder = 6
          OnClick = btnGutterFontClick
        end
        object pGutterBack: TPanel
          Left = 261
          Top = 92
          Width = 52
          Height = 21
          BorderWidth = 1
          TabOrder = 8
          object pGutterColor: TPanel
            Left = 2
            Top = 2
            Width = 38
            Height = 17
            Align = alClient
            BevelOuter = bvLowered
            Color = clGray
            TabOrder = 0
            OnClick = pGutterColorClick
          end
          object btnGutterColor: TPanel
            Left = 40
            Top = 2
            Width = 10
            Height = 17
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            OnMouseDown = btnGutterColorMouseDown
            object Image2: TImage
              Left = 3
              Top = 6
              Width = 5
              Height = 5
              Picture.Data = {
                07544269746D61708A000000424D8A0000000000000076000000280000000500
                0000050000000100040000000000140000000000000000000000100000001000
                0000000000000000800000800000008080008000000080008000808000008080
                8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
                FF00DDDDD000DD0DD000D000D00000000000DDDDD000}
              Transparent = True
              OnMouseDown = btnGutterColorMouseDown
            end
          end
        end
        object pnlGutterFontDisplay: TPanel
          Left = 176
          Top = 40
          Width = 156
          Height = 33
          BevelOuter = bvNone
          TabOrder = 7
          object lblGutterFont: TLabel
            Left = 0
            Top = 0
            Width = 156
            Height = 33
            Align = alClient
            Alignment = taCenter
            AutoSize = False
            Caption = 'Courier New 8pt'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
          end
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 121
        Width = 360
        Height = 96
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object gbRightEdge: TGroupBox
          Left = 0
          Top = 4
          Width = 172
          Height = 88
          Caption = 'Right Edge'
          TabOrder = 0
          object Label3: TLabel
            Left = 9
            Top = 56
            Width = 59
            Height = 15
            Caption = 'Edge color:'
          end
          object Label10: TLabel
            Left = 9
            Top = 26
            Width = 75
            Height = 15
            Caption = 'Edge Column:'
          end
          object pRightEdgeBack: TPanel
            Left = 97
            Top = 54
            Width = 52
            Height = 21
            BorderWidth = 1
            TabOrder = 1
            object pRightEdgeColor: TPanel
              Left = 2
              Top = 2
              Width = 38
              Height = 17
              Align = alClient
              BevelOuter = bvLowered
              Color = clGray
              TabOrder = 0
              OnClick = pRightEdgeColorClick
            end
            object btnRightEdge: TPanel
              Left = 40
              Top = 2
              Width = 10
              Height = 17
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 1
              OnMouseDown = btnRightEdgeMouseDown
              object Image1: TImage
                Left = 3
                Top = 6
                Width = 5
                Height = 5
                Picture.Data = {
                  07544269746D61708A000000424D8A0000000000000076000000280000000500
                  0000050000000100040000000000140000000000000000000000100000001000
                  0000000000000000800000800000008080008000000080008000808000008080
                  8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
                  FF00DDDDD000DD0DD000D000D00000000000DDDDD000}
                Transparent = True
                OnMouseDown = btnRightEdgeMouseDown
              end
            end
          end
          object eRightEdge: TEdit
            Left = 98
            Top = 23
            Width = 51
            Height = 23
            TabOrder = 0
            Text = '0'
          end
        end
        object gbLineSpacing: TGroupBox
          Left = 186
          Top = 5
          Width = 174
          Height = 88
          Caption = 'Line spacing / Tab spacing'
          TabOrder = 1
          object Label8: TLabel
            Left = 9
            Top = 27
            Width = 59
            Height = 15
            Caption = 'Extra Lines:'
          end
          object Label9: TLabel
            Left = 9
            Top = 56
            Width = 56
            Height = 15
            Caption = 'Tab Width:'
          end
          object eLineSpacing: TEdit
            Left = 80
            Top = 23
            Width = 52
            Height = 23
            TabOrder = 0
            Text = '0'
          end
          object eTabWidth: TEdit
            Left = 80
            Top = 53
            Width = 52
            Height = 23
            TabOrder = 1
            Text = '8'
          end
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 217
        Width = 360
        Height = 88
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object gbBookmarks: TGroupBox
          Left = 0
          Top = 4
          Width = 170
          Height = 79
          Caption = 'Bookmarks'
          TabOrder = 0
          object ckBookmarkKeys: TCheckBox
            Left = 9
            Top = 25
            Width = 101
            Height = 17
            Caption = 'Bookmark keys'
            TabOrder = 0
          end
          object ckBookmarkVisible: TCheckBox
            Left = 9
            Top = 51
            Width = 121
            Height = 17
            Caption = 'Bookmarks visible'
            TabOrder = 1
          end
        end
        object gbEditorFont: TGroupBox
          Left = 186
          Top = 5
          Width = 174
          Height = 79
          Caption = 'Editor Font'
          TabOrder = 1
          object btnFont: TButton
            Left = 67
            Top = 47
            Width = 84
            Height = 25
            Caption = 'Font'
            TabOrder = 0
            OnClick = btnFontClick
          end
          object Panel3: TPanel
            Left = 8
            Top = 19
            Width = 143
            Height = 30
            BevelOuter = bvNone
            TabOrder = 1
            object labFont: TLabel
              Left = 0
              Top = 0
              Width = 143
              Height = 30
              Align = alClient
              Alignment = taCenter
              AutoSize = False
              Caption = 'Courier New 10pt'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Courier New'
              Font.Style = []
              ParentFont = False
            end
          end
        end
      end
    end
    object Options: TTabSheet
      BorderWidth = 6
      Caption = 'Options'
      object gbOptions: TGroupBox
        Left = 0
        Top = 0
        Width = 360
        Height = 250
        Align = alClient
        Caption = 'Options'
        TabOrder = 0
        object ckAutoIndent: TCheckBox
          Left = 9
          Top = 15
          Width = 170
          Height = 17
          Hint = 
            'Will indent the caret on new lines with the same amount of leadi' +
            'ng white space as the preceding line'
          Caption = 'Auto indent'
          TabOrder = 0
        end
        object ckDragAndDropEditing: TCheckBox
          Left = 9
          Top = 34
          Width = 170
          Height = 17
          Hint = 
            'Allows you to select a block of text and drag it within the docu' +
            'ment to another location'
          Caption = 'Drag and drop editing'
          TabOrder = 1
        end
        object ckHalfPageScroll: TCheckBox
          Left = 182
          Top = 15
          Width = 170
          Height = 17
          Hint = 
            'When scrolling with page-up and page-down commands, only scroll ' +
            'a half page at a time'
          Caption = 'Half page scroll'
          TabOrder = 11
        end
        object ckEnhanceEndKey: TCheckBox
          Left = 9
          Top = 167
          Width = 170
          Height = 17
          Hint = 'Makes it so the caret is never visible'
          Caption = 'Enhance End Key'
          TabOrder = 8
        end
        object ckScrollByOneLess: TCheckBox
          Left = 182
          Top = 34
          Width = 170
          Height = 17
          Hint = 'Forces scrolling to be one less'
          Caption = 'Scroll by one less'
          TabOrder = 12
        end
        object ckScrollPastEOF: TCheckBox
          Left = 182
          Top = 53
          Width = 170
          Height = 17
          Hint = 'Allows the cursor to go past the end of file marker'
          Caption = 'Scroll past end of file'
          TabOrder = 13
        end
        object ckScrollPastEOL: TCheckBox
          Left = 182
          Top = 72
          Width = 170
          Height = 17
          Hint = 
            'Allows the cursor to go past the last character into the white s' +
            'pace at the end of a line'
          Caption = 'Scroll past end of line'
          TabOrder = 14
        end
        object ckShowScrollHint: TCheckBox
          Left = 182
          Top = 91
          Width = 170
          Height = 17
          Hint = 
            'Shows a hint of the visible line numbers when scrolling vertical' +
            'ly'
          Caption = 'Show scroll hint'
          TabOrder = 15
        end
        object ckSmartTabs: TCheckBox
          Left = 9
          Top = 110
          Width = 170
          Height = 17
          Hint = 
            'When tabbing, the cursor will go to the next non-white space cha' +
            'racter of the previous line'
          Caption = 'Smart tabs'
          TabOrder = 5
        end
        object ckTabsToSpaces: TCheckBox
          Left = 182
          Top = 129
          Width = 170
          Height = 17
          Hint = 'Converts a tab character to the number of spaces in Tab Width'
          Caption = 'Tabs to spaces'
          TabOrder = 17
        end
        object ckTrimTrailingSpaces: TCheckBox
          Left = 182
          Top = 148
          Width = 170
          Height = 17
          Hint = 'Spaces at the end of lines will be trimmed and not saved'
          Caption = 'Trim trailing spaces'
          TabOrder = 18
        end
        object ckWantTabs: TCheckBox
          Left = 9
          Top = 91
          Width = 170
          Height = 17
          Hint = 
            'Let the editor accept tab characters instead of going to the nex' +
            't control'
          Caption = 'Want tabs'
          TabOrder = 4
        end
        object ckBracketsHiglight: TCheckBox
          Left = 9
          Top = 53
          Width = 170
          Height = 17
          Hint = 'When the cursor moves to a bracket highlight the matching one'
          Caption = 'Higlight matching brackets'
          TabOrder = 2
        end
        object ckKeepCaretX: TCheckBox
          Left = 9
          Top = 72
          Width = 170
          Height = 17
          Hint = 
            'When moving through lines the X position will always stay the sa' +
            'me'
          Caption = 'Maintain caret column'
          TabOrder = 3
        end
        object ckScrollHintFollows: TCheckBox
          Left = 182
          Top = 110
          Width = 170
          Height = 17
          Hint = 'The scroll hint follows the mouse when scrolling vertically'
          Caption = 'Scroll hint follows mouse'
          TabOrder = 16
        end
        object ckGroupUndo: TCheckBox
          Left = 182
          Top = 167
          Width = 170
          Height = 17
          Hint = 
            'When undoing/redoing actions, handle all continous changes of th' +
            'e same kind in one call instead undoing/redoing each command sep' +
            'arately'
          Caption = 'Group undo'
          TabOrder = 19
        end
        object ckSmartTabDelete: TCheckBox
          Left = 9
          Top = 129
          Width = 170
          Height = 17
          Hint = 'similar to Smart Tabs, but when you delete characters'
          Caption = 'Smart tab delete'
          TabOrder = 6
        end
        object ckRightMouseMoves: TCheckBox
          Left = 182
          Top = 186
          Width = 170
          Height = 17
          Hint = 
            'When clicking with the right mouse for a popup menu, move the cu' +
            'rsor to that location'
          Caption = 'Right mouse moves cursor'
          TabOrder = 20
        end
        object ckEnhanceHomeKey: TCheckBox
          Left = 9
          Top = 148
          Width = 170
          Height = 17
          Hint = 'enhances home key positioning, similar to visual studio'
          Caption = 'Enhance Home Key'
          TabOrder = 7
        end
        object ckHideShowScrollbars: TCheckBox
          Left = 9
          Top = 186
          Width = 170
          Height = 17
          Hint = 
            'if enabled, then the scrollbars will only show when necessary.  ' +
            'If you have ScrollPastEOL, then it the horizontal bar will alway' +
            's be there (it uses MaxLength instead)'
          Caption = 'Hide scrollbars as necessary'
          TabOrder = 9
        end
        object ckDisableScrollArrows: TCheckBox
          Left = 9
          Top = 205
          Width = 170
          Height = 17
          Hint = 
            'Disables the scroll bar arrow buttons when you can'#39't scroll in t' +
            'hat direction any more'
          Caption = 'Disable scroll arrows'
          TabOrder = 10
        end
        object ckShowSpecialChars: TCheckBox
          Left = 182
          Top = 205
          Width = 170
          Height = 17
          Hint = 'Shows linebreaks, spaces and tabs using special symbols'
          Caption = 'Show special chars'
          TabOrder = 21
        end
        object ckCompleteBrackets: TCheckBox
          Left = 9
          Top = 224
          Width = 170
          Height = 17
          Hint = 'Auto complete brackets'
          Caption = 'Complete brackets'
          TabOrder = 22
        end
        object ckCompleteQuotes: TCheckBox
          Left = 182
          Top = 224
          Width = 170
          Height = 17
          Hint = 'Auto complete quotes'
          Caption = 'Complete quotes'
          TabOrder = 23
        end
      end
      object gbCaret: TGroupBox
        Left = 0
        Top = 250
        Width = 360
        Height = 71
        Align = alBottom
        Caption = 'Caret'
        TabOrder = 1
        DesignSize = (
          360
          71)
        object Label2: TLabel
          Left = 16
          Top = 18
          Width = 61
          Height = 15
          Caption = 'Insert caret:'
        end
        object Label4: TLabel
          Left = 16
          Top = 45
          Width = 83
          Height = 15
          Caption = 'Overwrite caret:'
        end
        object cInsertCaret: TComboBox
          Left = 120
          Top = 14
          Width = 219
          Height = 23
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cOverwriteCaret: TComboBox
          Left = 120
          Top = 41
          Width = 219
          Height = 23
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
      end
    end
    object Keystrokes: TTabSheet
      BorderWidth = 6
      Caption = 'Keystrokes'
      object gbKeyStrokes: TGroupBox
        Left = 0
        Top = 203
        Width = 360
        Height = 118
        Align = alBottom
        Caption = 'Keystroke Options'
        TabOrder = 1
        object Label5: TLabel
          Left = 16
          Top = 28
          Width = 60
          Height = 15
          Caption = 'Command:'
        end
        object Label6: TLabel
          Left = 16
          Top = 91
          Width = 54
          Height = 15
          Caption = 'Keystroke:'
        end
        object Label7: TLabel
          Left = 16
          Top = 59
          Width = 54
          Height = 15
          Caption = 'Keystroke:'
        end
        object cKeyCommand: TComboBox
          Left = 120
          Top = 23
          Width = 186
          Height = 23
          Style = csDropDownList
          TabOrder = 0
          OnKeyUp = cKeyCommandKeyUp
        end
      end
      object pnlCommands: TPanel
        Left = 0
        Top = 0
        Width = 360
        Height = 159
        Align = alClient
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'pnlCommands'
        TabOrder = 0
        object KeyList: TListView
          Left = 2
          Top = 2
          Width = 356
          Height = 155
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Command'
              Width = 167
            end
            item
              Caption = 'Keystroke'
              Width = 142
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnChanging = KeyListChanging
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 159
        Width = 360
        Height = 44
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object btnRemKey: TButton
          Left = 162
          Top = 5
          Width = 75
          Height = 25
          Caption = '&Remove'
          TabOrder = 0
          OnClick = btnRemKeyClick
        end
        object btnAddKey: TButton
          Left = 81
          Top = 5
          Width = 75
          Height = 25
          Caption = '&Add'
          TabOrder = 1
          OnClick = btnAddKeyClick
        end
        object btnUpdateKey: TButton
          Left = 0
          Top = 5
          Width = 75
          Height = 25
          Caption = '&Update'
          TabOrder = 2
          OnClick = btnUpdateKeyClick
        end
      end
    end
  end
  object btnOk: TButton
    Left = 226
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 306
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ColorDialog: TColorDialog
    Left = 24
    Top = 369
  end
  object ColorPopup: TPopupMenu
    Left = 56
    Top = 369
    object None1: TMenuItem
      Tag = -1
      Caption = 'None'
      OnClick = PopupMenuClick
    end
    object Scrollbar1: TMenuItem
      Caption = 'Scrollbar'
      OnClick = PopupMenuClick
    end
    object Background1: TMenuItem
      Tag = 1
      Caption = 'Background'
      OnClick = PopupMenuClick
    end
    object ActiveCaption1: TMenuItem
      Tag = 2
      Caption = 'Active Caption'
      OnClick = PopupMenuClick
    end
    object InactiveCaption1: TMenuItem
      Tag = 3
      Caption = 'Inactive Caption'
      OnClick = PopupMenuClick
    end
    object Menu1: TMenuItem
      Tag = 4
      Caption = 'Menu'
      OnClick = PopupMenuClick
    end
    object Window1: TMenuItem
      Tag = 5
      Caption = 'Window'
      OnClick = PopupMenuClick
    end
    object WindowFrame1: TMenuItem
      Tag = 6
      Caption = 'Window Frame'
      OnClick = PopupMenuClick
    end
    object MEnu2: TMenuItem
      Tag = 7
      Caption = 'Menu Text'
      OnClick = PopupMenuClick
    end
    object WindowText1: TMenuItem
      Tag = 8
      Caption = 'Window Text'
      OnClick = PopupMenuClick
    end
    object CaptionText1: TMenuItem
      Tag = 9
      Caption = 'Caption Text'
      OnClick = PopupMenuClick
    end
    object ActiveBorder1: TMenuItem
      Tag = 10
      Caption = 'Active Border'
      OnClick = PopupMenuClick
    end
    object InactiveBorder1: TMenuItem
      Tag = 11
      Caption = 'Inactive Border'
      OnClick = PopupMenuClick
    end
    object ApplicationWorkspace1: TMenuItem
      Tag = 12
      Caption = 'Application Workspace'
      OnClick = PopupMenuClick
    end
    object Highlight1: TMenuItem
      Tag = 13
      Caption = 'Highlight'
      OnClick = PopupMenuClick
    end
    object HighlightText1: TMenuItem
      Tag = 14
      Caption = 'Highlight Text'
      OnClick = PopupMenuClick
    end
    object ButtonFace1: TMenuItem
      Tag = 15
      Caption = 'Button Face'
      OnClick = PopupMenuClick
    end
    object ButtonShadow1: TMenuItem
      Tag = 16
      Caption = 'Button Shadow'
      OnClick = PopupMenuClick
    end
    object GrayText1: TMenuItem
      Tag = 17
      Caption = 'Gray Text'
      OnClick = PopupMenuClick
    end
    object ButtonText1: TMenuItem
      Tag = 18
      Caption = 'Button Text'
      OnClick = PopupMenuClick
    end
    object InactiveCaptionText1: TMenuItem
      Tag = 19
      Caption = 'Inactive Caption Text'
      OnClick = PopupMenuClick
    end
    object Highlight2: TMenuItem
      Tag = 20
      Caption = 'Highlight'
      OnClick = PopupMenuClick
    end
    object N3dDarkShadow1: TMenuItem
      Tag = 21
      Caption = '3D Dark Shadow'
      OnClick = PopupMenuClick
    end
    object N3DLight1: TMenuItem
      Tag = 22
      Caption = '3D Light'
      OnClick = PopupMenuClick
    end
    object InfoTipText1: TMenuItem
      Tag = 23
      Caption = 'Info Tip Text'
      OnClick = PopupMenuClick
    end
    object InfoTipBackground1: TMenuItem
      Tag = 24
      Caption = 'Info Tip Background'
      OnClick = PopupMenuClick
    end
  end
  object ImageList1: TImageList
    Left = 88
    Top = 369
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 120
    Top = 369
  end
end
