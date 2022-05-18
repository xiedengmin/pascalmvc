inherited SSHClientFrame: TSSHClientFrame
  Width = 614
  Height = 520
  Align = alClient
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 680
    Height = 348
    Align = alTop
    BevelOuter = bvNone
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    object Panel4: TPanel
      Left = 296
      Top = 3
      Width = 249
      Height = 24
      BevelOuter = bvNone
      Color = 48127
      TabOrder = 1
      object btConnectDB: TSpeedButton
        Left = 1
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Connect DB'
        Enabled = False
        Flat = True
        Transparent = False
        OnClick = btConnectDBClick
      end
      object btDisconnectDB: TSpeedButton
        Left = 125
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Disconnect DB'
        Enabled = False
        Flat = True
        Transparent = False
        OnClick = btDisconnectDBClick
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 3
      Width = 249
      Height = 24
      BevelOuter = bvNone
      Color = 48127
      TabOrder = 0
      object btConnectSSH: TSpeedButton
        Left = 1
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Connect SSH'
        Flat = True
        Transparent = False
        OnClick = btConnectSSHClick
      end
      object btDisconnectSSH: TSpeedButton
        Left = 125
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Disconnect SSH'
        Enabled = False
        Flat = True
        Transparent = False
        OnClick = btDisconnectSSHClick
      end
    end
    object Panel5: TPanel
      Tag = 1
      Left = 1
      Top = 30
      Width = 730
      Height = 313
      BevelOuter = bvNone
      Color = 48127
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 389
        Height = 260
        BevelOuter = bvNone
        TabOrder = 0
        object lbSSHUserName: TLabel
          Left = 16
          Top = 106
          Width = 51
          Height = 13
          Caption = 'User name'
        end
        object lbSSHConnection: TLabel
          Left = 8
          Top = 4
          Width = 91
          Height = 13
          Caption = 'SSH Connection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbSSHServer: TLabel
          Left = 16
          Top = 54
          Width = 54
          Height = 13
          Caption = 'SSH Server'
        end
        object lbSSHPort: TLabel
          Left = 16
          Top = 80
          Width = 42
          Height = 13
          Caption = 'SSH Port'
        end
        object lbAuthenticationKind: TLabel
          Left = 81
          Top = 26
          Width = 94
          Height = 13
          Caption = 'Authentication kind :'
        end
        object pnPassword: TPanel
          Left = 10
          Top = 124
          Width = 375
          Height = 31
          Color = clHotLight
          BevelOuter = bvNone
          TabOrder = 5
          object lbSShPassword: TLabel
            Left = 6
            Top = 8
            Width = 46
            Height = 13
            Caption = 'Password'
          end
          object edSSHPassword: TEdit
            Left = 70
            Top = 4
            Width = 293
            Height = 21
            PasswordChar = '*'
            TabOrder = 0
            OnChange = edSSHUserNameChange
          end
        end
        object edSSHUserName: TEdit
          Left = 80
          Top = 102
          Width = 293
          Height = 21
          TabOrder = 4
          OnChange = edSSHUserNameChange
        end
        object edSSHHost: TEdit
          Left = 80
          Top = 50
          Width = 293
          Height = 21
          TabOrder = 2
          OnChange = edSSHUserNameChange
        end
        object edSSHPort: TEdit
          Left = 80
          Top = 76
          Width = 100
          Height = 21
          TabOrder = 3
          OnChange = edSSHUserNameChange
        end
        object rbPassword: TRadioButton
          Left = 203
          Top = 24
          Width = 69
          Height = 17
          Caption = 'Password'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = rbPasswordClick
        end
        object rbPublicKey: TRadioButton
          Left = 286
          Top = 24
          Width = 68
          Height = 17
          Caption = 'Public key'
          TabOrder = 1
          OnClick = rbPublicKeyClick
        end
        object pnPrivateKey: TPanel
          Left = 10
          Top = 124
          Width = 373
          Height = 103
          BevelOuter = bvNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          Visible = False
          object lbKeyPath: TLabel
            Left = 6
            Top = 8
            Width = 53
            Height = 13
            Caption = 'Key path'
          end
          object edSSHKeyPath: TEdit
            Left = 70
            Top = 4
            Width = 268
            Height = 21
            TabOrder = 0
            OnChange = edSSHKeyPathChange
          end
          object sbKeyPath: TSpeedButton
            Left = 342
            Top = 3
            Width = 23
            Height = 23
            Caption = '...'
            Flat = True
            Transparent = False
            OnClick = sbKeyPathClick
          end
          object lbPrivateKey: TLabel
            Left = 6
            Top = 34
            Width = 53
            Height = 13
            Caption = 'Private key'
          end
          object cbSSHPrivateKey: TComboBox
            Left = 70
            Top = 30
            Width = 293
            Height = 21
            ItemHeight = 13
            TabOrder = 1
            OnChange = cbSSHPrivateKeyChange
            OnDropDown = cbSSHPrivateKeyDropDown
          end
          object pnGenerateKey: TPanel
            Left = 240
            Top = 58
            Width = 125
            Height = 24
            BevelOuter = bvNone
            Color = 48127
            TabOrder = 1
            object btKeyGen: TSpeedButton
              Left = 1
              Top = 1
              Width = 123
              Height = 22
              Caption = 'Generate key'
              Flat = True
              Transparent = False
              OnClick = btKeyGenClick
            end
          end
        end
        object cbRandomization: TCheckBox
          Left = 16
          Top = 160
          Width = 119
          Height = 17
          Hint = 'Generation random data increase connection reliability'
          Caption = 'Silent randomization'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
        end
      end
      object Panel7: TPanel
        Left = 391
        Top = 1
        Width = 338
        Height = 260
        BevelOuter = bvNone
        TabOrder = 1
        object lbDBConnection: TLabel
          Left = 8
          Top = 4
          Width = 84
          Height = 13
          Caption = 'DB Connection '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbListenPort: TLabel
          Left = 16
          Top = 54
          Width = 51
          Height = 13
          Caption = 'Listen Port'
        end
        object lbDBServer: TLabel
          Left = 16
          Top = 106
          Width = 32
          Height = 13
          Caption = 'Server'
        end
        object lbDBPort: TLabel
          Left = 16
          Top = 132
          Width = 20
          Height = 13
          Caption = 'Port'
        end
        object lbDBUserName: TLabel
          Left = 16
          Top = 158
          Width = 51
          Height = 13
          Caption = 'User name'
        end
        object lbDBPassword: TLabel
          Left = 16
          Top = 184
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object lbSID: TLabel
          Left = 16
          Top = 158
          Width = 65
          Height = 13
          Caption = 'Service Name'
        end
        object lbDatabase: TLabel
          Left = 16
          Top = 210
          Width = 46
          Height = 13
          Caption = 'Database'
        end
        object lbProvider: TLabel
          Left = 16
          Top = 80
          Width = 40
          Height = 13
          Caption = 'Provider'
        end
        object rbLocalPF: TRadioButton
          Left = 200
          Top = 24
          Width = 120
          Height = 17
          Caption = 'LocalPortForwarding'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = rbLocalPFClick
        end
        object rbDirect: TRadioButton
          Left = 100
          Top = 24
          Width = 89
          Height = 17
          Caption = 'Direct SSH'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TabStop = True
          OnClick = rbDirectClick
        end
        object edDBHost: TEdit
          Left = 98
          Top = 102
          Width = 220
          Height = 21
          TabOrder = 4
          OnChange = edListenPortChange
        end
        object edDBUserName: TEdit
          Left = 98
          Top = 154
          Width = 220
          Height = 21
          TabOrder = 8
          OnChange = edListenPortChange
        end
        object edDBPassword: TEdit
          Left = 98
          Top = 180
          Width = 220
          Height = 21
          PasswordChar = '*'
          TabOrder = 9
          OnChange = edListenPortChange
        end
        object cbSID: TCheckBox
          Left = 263
          Top = 130
          Width = 89
          Height = 17
          Caption = 'Use SID'
          TabOrder = 6
          OnClick = cbSIDClick
        end
        object seDBPort: TSpinEdit
          Left = 98
          Top = 128
          Width = 94
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 3306
          OnChange = edListenPortChange
        end
        object edDBServiceName: TEdit
          Left = 98
          Top = 154
          Width = 220
          Height = 21
          TabOrder = 7
          OnChange = edListenPortChange
        end
        object cbDBDatabase: TComboBox
          Left = 98
          Top = 206
          Width = 220
          Height = 21
          ItemHeight = 13
          TabOrder = 10
          OnChange = cbDBDatabaseChange
          OnDropDown = cbDBDatabaseDropDown
        end
        object seListenPort: TSpinEdit
          Left = 98
          Top = 50
          Width = 90
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 5001
          OnChange = edListenPortChange
        end
        object cbProvider: TComboBox
          Left = 98
          Top = 76
          Width = 220
          Height = 21
          ItemHeight = 13
          TabOrder = 3
          OnChange = cbProviderChange
        end
        object lbConnectMode: TLabel
          Left = 16
          Top = 236
          Width = 69
          Height = 13
          Caption = 'Connect Mode'
        end
        object cbConnectMode: TComboBox
          Left = 98
          Top = 232
          Width = 100
          Height = 21
          ItemIndex = 0
          TabOrder = 11
          Text = 'Normal'
          Items.Strings = (
            'Normal'
            'SysOper'
            'SysDBA'
            'SysASM'
            'SysBackup'
            'SysDG'
            'SysKM')
          end
        end
      end
      object Panel10: TPanel
        Left = 2
        Top = 292
        Width = 728
        Height = 25
        BevelOuter = bvNone
        TabOrder = 2
        object lbTableName: TLabel
          Left = 8
          Top = 6
          Width = 55
          Height = 13
          Caption = 'Table name'
        end
        object cbTableName: TComboBox
          Left = 83
          Top = 2
          Width = 304
          Height = 21
          DropDownCount = 16
          Enabled = False
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbTableNameChange
          OnDropDown = cbTableNameDropDown
        end
      end
      object Panel9: TPanel
        Left = 2
        Top = 318        
        Width = 728
        Height = 24
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 3
        object btOpen: TSpeedButton
          Left = 1
          Top = 1
          Width = 82
          Height = 22
          Caption = 'Open'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btOpenClick
        end
        object btClose: TSpeedButton
          Left = 84
          Top = 1
          Width = 82
          Height = 22
          Caption = 'Close'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btCloseClick
        end
        object DBNavigator: TDBNavigator
          Left = 167
          Top = 1
          Width = 220
          Height = 22
          DataSource = DataSource
          Flat = True
          TabOrder = 0
        end
        object Panel8: TPanel
          Left = 388
          Top = 1
          Width = 190
          Height = 22
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
    end
  object Panel2: TPanel
    Left = 0
    Top = 315
    Width = 699
    Height = 80
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object DBGrid: TDBGrid
      Left = 0
      Top = 0
      Width = 699
      Height = 80
      Align = alClient
      DataSource = DataSource
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object ScSSHClient: TScSSHClient
    HostKeyAlgorithms = <
      item
        Algorithm = aaRSA
      end
      item
        Algorithm = aaDSA
      end>
    KeyStorage = ScFileStorage
    AfterConnect = ScSSHClientAfterConnect
    BeforeConnect = ScSSHClientBeforeConnect
    AfterDisconnect = ScSSHClientAfterDisconnect
    OnServerKeyValidate = ScSSHClientServerKeyValidate
    Left = 12
    Top = 216
  end
  object UniConnection: TUniConnection
    LoginPrompt = False
    AfterConnect = UniConnectionAfterConnect
    BeforeConnect = UniConnectionBeforeConnect
    AfterDisconnect = UniConnectionAfterDisconnect
    Left = 416
    Top = 257
  end
  object UniTable: TUniTable
    Connection = UniConnection
    AfterOpen = UniTableAfterOpen
    AfterClose = UniTableAfterClose
    Left = 448
    Top = 257
  end
  object DataSource: TDataSource
    DataSet = UniTable
    Left = 480
    Top = 257
  end
  object CRSSHIOHandler: TCRSSHIOHandler
    Client = ScSSHClient
    Left = 84
    Top = 216
  end
  object ScFileStorage: TScFileStorage
    Left = 112
    Top = 216
  end
  object ScSSHChannel: TScSSHChannel
    Client = ScSSHClient
    Left = 40
    Top = 216
  end
  object OpenDialog: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 60
    Top = 220
  end
end
