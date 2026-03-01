object SynEditActions: TSynEditActions
  Height = 270
  Width = 474
  object ActionList: TActionList
    Left = 24
    Top = 32
    object SynEditRedo: TSynEditRedo
      Category = 'Edit'
      Caption = '&Redo'
      Hint = 'Redo| Redo last action'
      ImageName = 'Redo'
      ShortCut = 24666
    end
    object SynSpellCheckFile: TSynSpellCheckFile
      Category = 'Spell Checking'
      Caption = 'Check File'
    end
    object SynSpellCheckLine: TSynSpellCheckLine
      Category = 'Spell Checking'
      Caption = 'Check Line'
    end
    object SynSpellCheckSelection: TSynSpellCheckSelection
      Category = 'Spell Checking'
      Caption = 'Check Selection'
    end
    object SynSpellCheckWord: TSynSpellCheckWord
      Category = 'Spell Checking'
      Caption = 'Check Word'
    end
    object SynSpellClearErrors: TSynSpellClearErrors
      Category = 'Spell Checking'
      Caption = 'Clear Errors'
    end
    object SynSpellCheckAsYouType: TSynSpellCheckAsYouType
      Category = 'Spell Checking'
      Caption = 'Check As You Type'
    end
    object SynSpellErrorAdd: TSynSpellErrorAdd
      Category = 'Spell Checking'
      Caption = 'Add'
    end
    object SynSpellErrorIgnoreOnce: TSynSpellErrorIgnoreOnce
      Category = 'Spell Checking'
      Caption = 'Ignore Once'
    end
    object SynSpellErrorIgnore: TSynSpellErrorIgnore
      Category = 'Spell Checking'
      Caption = 'Ignore'
    end
    object SynSpellErrorDelete: TSynSpellErrorDelete
      Category = 'Spell Checking'
      Caption = 'Delete'
    end
  end
end
