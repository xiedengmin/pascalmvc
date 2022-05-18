inherited DASQLGeneratorFrame: TDASQLGeneratorFrame
  Height = 258
  object pnSQLGenerator: TPanel
    Left = 8
    Top = 6
    Width = 465
    Height = 242
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnResize = pnSQLGeneratorResize
    object lbTableName: TLabel
      Left = 13
      Top = 13
      Width = 61
      Height = 13
      Caption = 'Table Name:'
    end
    object lbKeyFieldsLabel: TLabel
      Left = 182
      Top = 12
      Width = 51
      Height = 13
      Caption = 'Key Fields:'
    end
    object lbUpdateFieldsLabel: TLabel
      Left = 321
      Top = 11
      Width = 68
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Update Fields:'
    end
    object lbKeyFields: TListBox
      Left = 180
      Top = 28
      Width = 133
      Height = 204
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 10
      OnClick = lbUpdateFieldsClick
    end
    object lbUpdateFields: TListBox
      Left = 321
      Top = 28
      Width = 133
      Height = 204
      Anchors = [akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 11
      OnClick = lbUpdateFieldsClick
    end
    object cbTables: TComboBox
      Left = 13
      Top = 29
      Width = 156
      Height = 21
      TabOrder = 0
      OnChange = cbTablesChange
      OnDropDown = cbTablesDropDown
    end
    object btGetFields: TButton
      Left = 13
      Top = 56
      Width = 155
      Height = 22
      Caption = 'Get Fields'
      Enabled = False
      TabOrder = 1
      OnClick = btGetFieldsClick
    end
    object btGenerate: TButton
      Left = 13
      Top = 82
      Width = 155
      Height = 22
      Caption = 'Generate SQL'
      TabOrder = 2
      OnClick = btGenerateClick
    end
    object cbQuoteFields: TCheckBox
      Left = 15
      Top = 109
      Width = 146
      Height = 17
      Caption = 'Quote names'
      TabOrder = 3
    end
    object bvl: TBevel
      Left = 15
      Top = 130
      Width = 146
      Height = 2
      Shape = bsBottomLine
    end
    object cbInsert: TCheckBox
      Left = 15
      Top = 135
      Width = 146
      Height = 17
      Caption = 'Insert'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = cbIUDRClick
    end
    object cbUpdate: TCheckBox
      Left = 15
      Top = 151
      Width = 146
      Height = 17
      Caption = 'Update'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = cbIUDRClick
    end
    object cbDelete: TCheckBox
      Left = 15
      Top = 167
      Width = 146
      Height = 17
      Caption = 'Delete'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = cbIUDRClick
    end
    object cbLock: TCheckBox
      Left = 15
      Top = 183
      Width = 146
      Height = 17
      Caption = 'Lock'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = cbIUDRClick
    end
    object cbRefresh: TCheckBox
      Left = 15
      Top = 199
      Width = 146
      Height = 17
      Caption = 'Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 8
      OnClick = cbIUDRClick
    end
    object cbRecCount: TCheckBox
      Left = 15
      Top = 215
      Width = 146
      Height = 17
      Caption = 'Record count'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = cbIUDRClick
    end
  end
end
