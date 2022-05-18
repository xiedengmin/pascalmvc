object DADataTypeMapFrame: TDADataTypeMapFrame
  object btnAddRule: TButton
    Left = 192
    Top = 14
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = btnAddRuleClick
  end
  object btnRemoveRule: TButton
    Left = 192
    Top = 45
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 2
    OnClick = btnRemoveRuleClick
  end
  object btnMoveUp: TButton
    Left = 192
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Move Up'
    TabOrder = 3
    OnClick = btnMoveUpClick
  end
  object btnMoveDown: TButton
    Left = 192
    Top = 107
    Width = 75
    Height = 25
    Caption = 'Move Down'
    TabOrder = 4
    OnClick = btnMoveDownClick
  end
  object chkIgnoreError: TCheckBox
    Left = 16
    Top = 131
    Width = 15
    Height = 15
    TabOrder = 5
    Visible = False
  end
end
