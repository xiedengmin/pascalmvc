object fmRandom: TfmRandom
  Left = 192
  Top = 114
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Random'
  ClientHeight = 309
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object lbInform: TLabel
    Left = 12
    Top = 32
    Width = 415
    Height = 32
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Move the mouse pointer over the form to generate random data for' +
      ' the random generator initialization to increase its reliability' +
      '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    OnMouseMove = FormMouseMove
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 0
    Width = 442
    Height = 17
    Align = alTop
    Step = 2
    TabOrder = 0
  end
  object btClose: TButton
    Left = 182
    Top = 272
    Width = 85
    Height = 25
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 1
  end
end
