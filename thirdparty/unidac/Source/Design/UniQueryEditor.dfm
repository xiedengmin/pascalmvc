inherited UniQueryEditorForm: TUniQueryEditorForm
  Left = 435
  Top = 196
  Width = 600
  Height = 400
  Caption = 'UniQuery Editor'
  Constraints.MinHeight = 400	
  Constraints.MinWidth = 600
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 316
    Width = 552
    inherited imCorner: TImage
      Left = 539
    end
    inherited btOk: TBitBtn
      Left = 387
    end
    inherited btCancel: TBitBtn
      Left = 468
      TabOrder = 5
    end
    object btMacros: TButton
      Left = 265
      Top = 8
      Width = 117
      Height = 25
      Caption = 'Connection Macros...'
      TabOrder = 4
      OnClick = btMacrosClick
    end
  end
  inherited PageControl: TPageControl
    Width = 535
    Height = 301
    object shOptions: TTabSheet [5]
      Caption = '&Options'
      ImageIndex = 6
    end
  end
end
