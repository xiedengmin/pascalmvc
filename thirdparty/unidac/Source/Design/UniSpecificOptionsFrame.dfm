object UniSpecificOptionsFrame: TUniSpecificOptionsFrame
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  object pnProvider: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 29
    BevelOuter = bvNone
    TabOrder = 0
    object lbProvider: TLabel
      Left = 0
      Top = 3
      Width = 48
      Height = 13
      Caption = 'Provider:'
    end
    object edProvider: TComboBox
      Left = 52
      Top = 0
      Width = 153
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = edProviderChange
    end
  end
end
