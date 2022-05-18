inherited DAQueryEditorForm: TDAQueryEditorForm
  Width = 600
  Height = 400
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    inherited btOk: TBitBtn
      TabOrder = 3
    end
    inherited btCancel: TBitBtn
      TabOrder = 4
    end
    object btnDataEditor: TBitBtn
      Left = 180
      Top = 8
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Data Editor...'
      TabOrder = 1
      OnClick = btnDataEditorClick
    end
    object btnCodeEditor: TBitBtn
      Left = 94
      Top = 8
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Code Editor ...'
      TabOrder = 2
      OnClick = btnCodeEditorClick
    end
  end
  inherited PageControl: TPageControl
    object shEditSQL: TTabSheet [1]
      Caption = '&Update SQLs'
    end
    object shGenerator: TTabSheet [2]
      Caption = 'SQL &Generator'
    end
    object shDataTypeMap: TTabSheet [5]
      Caption = '&Data Type Mapping'
    end
    object shConditions: TTabSheet
      Caption = '&Conditions'
    end
  end
end
