inherited DAConditionsFrame: TDAConditionsFrame
  Width = 496
  Height = 248
  object lbCName: TLabel
    Left = 7
    Top = 8
    Width = 75
    Height = 13
    Caption = 'Condition Name'
  end
  object lbItemName: TListBox
    Left = 3
    Top = 27
    Width = 153
    Height = 174
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbItemNameClick
  end
  object PanelItem: TPanel
    Left = 176
    Top = 8
    Width = 311
    Height = 224
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object lbCValue: TLabel
      Left = 16
      Top = 12
      Width = 26
      Height = 13
      Caption = 'Value'
    end
    object lbConditionEnable: TLabel
      Left = 33
      Top = 204
      Width = 32
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Enable'
    end
    object meConditionValue: TMemo
      Left = 14
      Top = 28
      Width = 285
      Height = 170
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      OnExit = meConditionValueExit
    end
    object cbConditionEnable: TCheckBox
      Left = 14
      Top = 202
      Width = 15
      Height = 17
      Anchors = [akLeft, akBottom]
      TabOrder = 1
      OnClick = cbConditionEnableClick
    end
  end
  object btnAdd: TButton
    Left = 3
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 81
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Remove'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
end
