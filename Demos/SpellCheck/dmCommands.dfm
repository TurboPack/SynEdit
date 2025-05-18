object CommandsDataModule: TCommandsDataModule
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object SynPasSyn1: TSynPasSyn
    Left = 32
    Top = 24
  end
  object SynSpellCheck: TSynSpellCheck
    CheckAsYouType = False
    Left = 120
    Top = 24
  end
  object ActionList1: TActionList
    Left = 32
    Top = 112
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object EditDelete1: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
    object actSynSpellCheckFile: TSynSpellCheckFile
      Category = 'SynEdit Spell Check'
      Caption = 'Check File'
    end
    object actSynSpellCheckLine: TSynSpellCheckLine
      Category = 'SynEdit Spell Check'
      Caption = 'Check Line'
    end
    object actSynSpellCheckSelection: TSynSpellCheckSelection
      Category = 'SynEdit Spell Check'
      Caption = 'Check Selection'
    end
    object actSynSpellCheckWord: TSynSpellCheckWord
      Category = 'SynEdit Spell Check'
      Caption = 'Check Word'
    end
    object actSynSpellClearErrors: TSynSpellClearErrors
      Category = 'SynEdit Spell Check'
      Caption = 'Clear Errors'
    end
    object actSynSpellCheckAsYouType: TSynSpellCheckAsYouType
      Category = 'SynEdit Spell Check'
      Caption = 'Check As You Type'
    end
    object actSynSpellErrorAdd: TSynSpellErrorAdd
      Category = 'SynEdit Spell Check'
      Caption = 'Add'
    end
    object actSynSpellErrorIgnoreOnce: TSynSpellErrorIgnoreOnce
      Category = 'SynEdit Spell Check'
      Caption = 'Ignore Once'
    end
    object actSynSpellErrorIgnore: TSynSpellErrorIgnore
      Category = 'SynEdit Spell Check'
      Caption = 'Ignore'
    end
    object actSynSpellErrorDelete: TSynSpellErrorDelete
      Category = 'SynEdit Spell Check'
      Caption = 'Delete'
    end
  end
end
