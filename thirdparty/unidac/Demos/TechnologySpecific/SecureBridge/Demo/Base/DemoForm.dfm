object DemoForm: TDemoForm
  Left = 77
  Top = 70
  Width = 927
  Height = 525
  Caption = 'SecureBridge demos'
  Color = clSilver
  Constraints.MinHeight = 450
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 74
    Width = 919
    Height = 417
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 919
      Height = 3
      Align = alTop
      Brush.Color = 48127
      Pen.Color = clBtnFace
    end
    object pnDemo: TPanel
      Left = 0
      Top = 3
      Width = 919
      Height = 414
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnSource: TPanel
      Left = 0
      Top = 3
      Width = 919
      Height = 414
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      object Panel6: TPanel
        Left = 0
        Top = 379
        Width = 919
        Height = 35
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object Panel3: TPanel
          Left = 16
          Top = 3
          Width = 127
          Height = 25
          Anchors = [akLeft, akBottom]
          BevelOuter = bvNone
          Color = 48127
          TabOrder = 0
          object sbOpenDemoDir: TSpeedButton
            Left = 1
            Top = 1
            Width = 125
            Height = 23
            Caption = 'Open demo folder'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = sbOpenDemoDirClick
          end
        end
      end
    end
  end
  object pnTopLabel: TPanel
    Left = 0
    Top = 0
    Width = 919
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lbTitle: TLabel
      Left = 0
      Top = 0
      Width = 919
      Height = 45
      Cursor = crArrow
      Align = alTop
      AutoSize = False
      Caption = ' SecureBridge'
      Color = 48127
      Constraints.MinWidth = 130
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWhite
      Font.Height = -35
      Font.Name = 'Verdana'
      Font.Style = [fsBold, fsItalic]
      ParentColor = False
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 45
    Width = 919
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object pnShowSource: TPanel
      Left = 774
      Top = 3
      Width = 125
      Height = 24
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      Color = 48127
      TabOrder = 0
      object sbDemo: TSpeedButton
        Left = 1
        Top = 1
        Width = 123
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'Show source'
        Enabled = False
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Pitch = fpVariable
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
        OnClick = sbDemoClick
      end
    end
    object Panel1: TPanel
      Left = 2
      Top = 3
      Width = 125
      Height = 24
      BevelOuter = bvNone
      Color = 48127
      TabOrder = 1
      object btRandom: TSpeedButton
        Left = 1
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Randomize'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
        OnClick = btRandomClick
      end
    end
  end
end
