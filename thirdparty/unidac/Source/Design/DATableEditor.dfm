inherited DATableEditorForm: TDATableEditorForm
  Left = 294
  Top = 203
  Width = 600
  Height = 400
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 265
    inherited btOk: TBitBtn
      TabOrder = 1
    end
    inherited btCancel: TBitBtn
      TabOrder = 2
    end
    object btnDataEditor: TBitBtn
      Left = 8
      Top = 8
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Data Editor...'
      TabOrder = 0
      OnClick = btnDataEditorClick
    end
  end
  inherited PageControl: TPageControl
    ActivePage = shSQL
    object shSQL: TTabSheet
      Caption = 'S&QL'
    end
    object shDataTypeMap: TTabSheet
      Caption = '&Data Type Mapping'
      ImageIndex = 4
    end
    object shConditions: TTabSheet
      Caption = '&Conditions'
      ImageIndex = 2
    end
  end
end
