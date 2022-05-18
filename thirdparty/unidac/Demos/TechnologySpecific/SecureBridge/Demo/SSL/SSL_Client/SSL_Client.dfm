inherited SSLClientFrame: TSSLClientFrame
  Width = 506
  Height = 270
  Align = alClient
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 323
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
      Left = 1
      Top = 3
      Width = 249
      Height = 24
      BevelOuter = bvNone
      Color = 48127
      TabOrder = 0
      object btConnectDB: TSpeedButton
        Left = 1
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Connect DB'
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
    object Panel5: TPanel
      Tag = 1
      Left = 1
      Top = 30
      Width = 785
      Height = 289
      BevelOuter = bvNone
      Color = 48127
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Panel6: TPanel
        Left = 450
        Top = 1
        Width = 334
        Height = 236
        BevelOuter = bvNone
        TabOrder = 3
        object lbDBConnection: TLabel
          Left = 8
          Top = 5
          Width = 105
          Height = 13
          Caption = 'DB Connection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbDBServer: TLabel
          Left = 16
          Top = 56
          Width = 32
          Height = 13
          Caption = 'Server'
        end
        object lbDBPort: TLabel
          Left = 16
          Top = 82
          Width = 20
          Height = 13
          Caption = 'Port'
        end
        object lbSID: TLabel
          Left = 16
          Top = 108
          Width = 65
          Height = 13
          Caption = 'Service Name'
        end
        object lbDBUserName: TLabel
          Left = 16
          Top = 134
          Width = 51
          Height = 13
          Caption = 'User name'
        end
        object lbDBPassword: TLabel
          Left = 16
          Top = 160
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object lbConnectMode: TLabel
          Left = 16
          Top = 186
          Width = 69
          Height = 13
          Caption = 'Connect Mode'
        end
        object lbDBDatabase: TLabel
          Left = 16
          Top = 186
          Width = 46
          Height = 13
          Caption = 'Database'
        end
        object lbProvider: TLabel
          Left = 16
          Top = 30
          Width = 40
          Height = 13
          Caption = 'Provider'
        end
        object cbSID: TCheckBox
          Left = 265
          Top = 80
          Width = 89
          Height = 17
          Caption = 'Use SID'
          TabOrder = 3
          OnClick = cbSIDClick
        end
        object cbConnectMode: TComboBox
          Left = 98
          Top = 182
          Width = 100
          Height = 21
          ItemIndex = 0
          TabOrder = 8
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
        object edDBHost: TEdit
          Left = 98
          Top = 52
          Width = 220
          Height = 21
          TabOrder = 1
          OnChange = edDBHostChange
        end
        object edDBUserName: TEdit
          Left = 98
          Top = 130
          Width = 220
          Height = 21
          TabOrder = 5
          OnChange = edDBHostChange
        end
        object edDBPassword: TEdit
          Left = 98
          Top = 156
          Width = 220
          Height = 21
          PasswordChar = '*'
          TabOrder = 6
          OnChange = edDBHostChange
        end
        object seDBPort: TSpinEdit
          Left = 98
          Top = 78
          Width = 95
          Height = 22
          MaxValue = 65536
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = edDBHostChange
        end
        object edDBServiceName: TEdit
          Left = 98
          Top = 104
          Width = 220
          Height = 21
          TabOrder = 4
          OnChange = edDBHostChange
        end
        object cbDBDatabase: TComboBox
          Left = 98
          Top = 182
          Width = 220
          Height = 21
          TabOrder = 7
          OnChange = cbDBDatabaseChange
          OnDropDown = cbDBDatabaseDropDown
        end
        object cbProvider: TComboBox
          Left = 98
          Top = 26
          Width = 220
          Height = 21
          TabOrder = 0
          OnChange = cbProviderChange
        end
      end
      object Panel7: TPanel
        Left = 1
        Top = 238
        Width = 783
        Height = 25
        BevelOuter = bvNone
        TabOrder = 0
        object lbTableName: TLabel
          Left = 12
          Top = 6
          Width = 55
          Height = 13
          Caption = 'Table name'
        end
        object cbTableName: TComboBox
          Left = 120
          Top = 2
          Width = 290
          Height = 21
          DropDownCount = 16
          Enabled = False
          TabOrder = 0
          OnChange = cbTableNameChange
          OnDropDown = cbTableNameDropDown
        end
      end
      object Panel9: TPanel
        Left = 1
        Top = 264
        Width = 404
        Height = 24
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 2
        object btOpen: TSpeedButton
          Left = 1
          Top = 1
          Width = 90
          Height = 22
          Caption = 'Open'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btOpenClick
        end
        object btClose: TSpeedButton
          Left = 92
          Top = 1
          Width = 90
          Height = 22
          Caption = 'Close'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btCloseClick
        end
        object DBNavigator: TDBNavigator
          Left = 183
          Top = 1
          Width = 220
          Height = 22
          DataSource = DataSource
          Flat = True
          TabOrder = 0
        end
      end
      object Panel8: TPanel
        Left = 405
        Top = 264
        Width = 379
        Height = 24
        BevelOuter = bvNone
        TabOrder = 4
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 448
        Height = 236
        BevelOuter = bvNone
        TabOrder = 1
        TabStop = True
        object lbSSLConnection: TLabel
          Left = 10
          Top = 5
          Width = 89
          Height = 13
          Caption = 'SSL Connection '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbCAcertificate: TLabel
          Left = 10
          Top = 30
          Width = 65
          Height = 13
          Caption = 'CA certificate'
        end
        object lbClientCertificate: TLabel
          Left = 10
          Top = 56
          Width = 78
          Height = 13
          Caption = 'Client certificate'
        end
        object lbClientPrivateKey: TLabel
          Left = 10
          Top = 82
          Width = 84
          Height = 13
          Caption = 'Client private key'
        end
        object sbCACertName: TSpeedButton
          Left = 411
          Top = 25
          Width = 23
          Height = 23
          Caption = '...'
          Flat = True
          Transparent = False
          OnClick = sbCACertNameClick
        end
        object sbCertName: TSpeedButton
          Left = 411
          Top = 51
          Width = 23
          Height = 23
          Caption = '...'
          Flat = True
          Transparent = False
          OnClick = sbCertNameClick
        end
        object sbKeyName: TSpeedButton
          Left = 411
          Top = 77
          Width = 23
          Height = 23
          Caption = '...'
          Flat = True
          Transparent = False
          OnClick = sbKeyNameClick
        end
        object edCACertName: TEdit
          Left = 120
          Top = 26
          Width = 290
          Height = 21
          TabOrder = 0
          Text = '.\ca-cert.pem'
          OnChange = edDBHostChange
        end
        object edCertName: TEdit
          Left = 120
          Top = 52
          Width = 290
          Height = 21
          TabOrder = 1
          Text = '.\client-cert.pem'
          OnChange = edDBHostChange
        end
        object edKeyName: TEdit
          Left = 120
          Top = 78
          Width = 290
          Height = 21
          TabOrder = 2
          Text = '.\client.key'
          OnChange = edDBHostChange
        end
        object pnOracleOptions: TPanel
          Left = 1
          Top = 103
          Width = 435
          Height = 80
          BevelOuter = bvNone
          Caption = 'pnOracleOption'
          ParentBackground = False
          TabOrder = 3
          object lbServerCertDN: TLabel
            Left = 10
            Top = 57
            Width = 102
            Height = 13
            Caption = 'Server Certificate DN'
          end
          object sbWallet: TSpeedButton
            Left = 410
            Top = 25
            Width = 23
            Height = 23
            Caption = '...'
            Flat = True
            Transparent = False
            OnClick = sbWalletClick
          end
          object lbWallet: TLabel
            Left = 10
            Top = 31
            Width = 83
            Height = 13
            Caption = 'Wallet'
          end
          object lbStorageKind: TLabel
            Left = 10
            Top = 5
            Width = 77
            Height = 13
            Caption = 'Storage Kind'
          end
          object rbWallet: TRadioButton
            Left = 160
            Top = 3
            Width = 80
            Height = 17
            Caption = 'Wallet'
            TabOrder = 0
            TabStop = True
            OnClick = rbWalletClick
          end
          object rbCertificate: TRadioButton
            Left = 293
            Top = 3
            Width = 84
            Height = 17
            Caption = 'Certificate'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = rbCertificateClick
          end
          object edWallet: TEdit
            Left = 119
            Top = 26
            Width = 290
            Height = 21
            TabOrder = 2
            OnChange = edDBHostChange
          end
          object edServerCertDN: TEdit
            Left = 119
            Top = 53
            Width = 290
            Height = 21
            TabOrder = 3
            OnChange = edDBHostChange
          end
        end
        object pnAdvancedOptions: TPanel
          Left = 1
          Top = 184
          Width = 435
          Height = 80
          BevelOuter = bvNone
          ParentBackground = False
          TabOrder = 4
          object cbTrustServerCertificate: TCheckBox
            Left = 120
            Top = 2
            Width = 256
            Height = 17
            Hint = 'Specifies whether to verify the server certificate during an SSL handshake.'
            Caption = 'Trust Server Certificate'
            Checked = True
            ParentShowHint = False
            ShowHint = True
            State = cbChecked
            TabOrder = 0
          end
          object cbRandomization: TCheckBox
            Left = 120
            Top = 28
            Width = 118
            Height = 17
            Hint = 'Generation random data increase connection reliability'
            Caption = 'Silent randomization'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 267
    Width = 506
    Height = 3
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object DBGrid: TDBGrid
      Left = 0
      Top = 0
      Width = 506
      Height = 3
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
  object UniConnection: TUniConnection
    LoginPrompt = False
    AfterConnect = UniConnectionAfterConnect
    BeforeConnect = UniConnectionBeforeConnect
    AfterDisconnect = UniConnectionAfterConnect
    Left = 120
    Top = 169
  end
  object UniTable: TUniTable
    Connection = UniConnection
    AfterOpen = UniTableAfterOpen
    AfterClose = UniTableAfterClose
    Left = 152
    Top = 169
  end
  object DataSource: TDataSource
    DataSet = UniTable
    Left = 184
    Top = 169
  end
  object CRSSLIOHandler: TCRSSLIOHandler
    Storage = ScCryptoAPIStorage
    Left = 88
    Top = 168
  end
  object ScCryptoAPIStorage: TScCryptoAPIStorage
    CertProviderType = ptMemory
    Left = 56
    Top = 168
  end
  object OpenDialog: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 152
  end
end
