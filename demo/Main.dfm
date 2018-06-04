object ShellDemoForm: TShellDemoForm
  Left = 207
  Top = 166
  Width = 578
  Height = 467
  Caption = 'Shell Path Edit Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    570
    433)
  PixelsPerInch = 96
  TextHeight = 13
  object NameLabel: TLabel
    Left = -2
    Top = 370
    Width = 89
    Height = 21
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'File names:'
    Layout = tlCenter
  end
  object TypeLabel: TLabel
    Left = -2
    Top = 343
    Width = 89
    Height = 21
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Files of type:'
    Layout = tlCenter
  end
  object ShellView: TShellView
    Left = 280
    Top = 122
    Width = 282
    Height = 218
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentColor = False
    TabOrder = 3
    AllowContextMenu = True
    SpecialFolder = sfDesktop
    ViewMode = vmIcon
    OnDefaultAction = ShelNodeDefaultAction
    OnIncludeItem = ShellViewIncludeItem
    OnSelectionChanged = ShellViewSelectionChanged
  end
  object ShellTree: TShellTree
    Left = 94
    Top = 122
    Width = 180
    Height = 218
    Anchors = [akLeft, akTop, akBottom]
    ParentColor = False
    TabOrder = 2
    AllowContextMenu = True
    AutoExpand = False
    HideSelection = True
    HotTrack = False
    Indent = 19
    RowSelect = False
    ShowButtons = True
    ShowLines = True
    ShowRoot = False
    ToolTips = True
  end
  object ShellBubbles: TShellBubbles
    Left = 8
    Top = 95
    Width = 80
    Height = 244
    Anchors = [akLeft, akTop, akBottom]
    ParentColor = False
    TabOrder = 0
    Items = <
      item
        SpecialFolder = sfDesktop
      end
      item
        SpecialFolder = sfDrives
      end
      item
        SpecialFolder = sfControls
      end
      item
        SpecialFolder = sfPersonal
      end
      item
        SpecialFolder = sfPrinters
      end>
    NavigateRoot = True
  end
  object FilterCombo: TComboBox
    Left = 93
    Top = 373
    Width = 469
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = 'Show any and all files or folders (*.*) in icon mode'
    OnChange = FilterComboChange
    Items.Strings = (
      'Show any and all files or folders (*.*) in icon mode'
      'Show text files or any folders (*.txt) in detail mode'
      'Show only image files in thumb nails mode'
      'Show only folders in list mode'
      'Show only images or folders in tile mode')
  end
  object FileNameEdit: TEdit
    Left = 93
    Top = 346
    Width = 469
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
  end
  object ShellEdit: TShellEdit
    Left = 96
    Top = 95
    Width = 466
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ParentColor = False
    TabOrder = 1
    Visible = False
    TabStop = True
    Options = [soDefaultAction, soNavigate, soSuggest]
    HotTrack = False
    AutoHeight = False
    ButtonVisible = True
    AutoSelect = False
    Flat = False
    ImageIndex = 4
    Modified = False
    SelLength = 0
    SelStart = 0
    Sizeable = True
    Style = beStandard
    Text = 'Desktop'
    WantTabs = False
    DesignSize = (
      462
      17)
  end
  object GroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 557
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options   '
    TabOrder = 7
    object Panel: TPanel
      Left = 16
      Top = 23
      Width = 538
      Height = 49
      BevelOuter = bvNone
      TabOrder = 0
      object UsePathEditCheckBox: TCheckBox
        Left = 0
        Top = 0
        Width = 97
        Height = 17
        TabStop = False
        Caption = 'Use Path Edit'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = UsePathEditCheckBoxClick
      end
      object ShowIconsCheckBox: TCheckBox
        Left = 144
        Top = 24
        Width = 97
        Height = 17
        TabStop = False
        Caption = 'Show Icons'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = ShowIconsCheckBoxClick
      end
      object ShowButtonsCheckBox: TCheckBox
        Left = 288
        Top = 23
        Width = 105
        Height = 17
        TabStop = False
        Caption = 'Show Buttons'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = ShowButtonsCheckBoxClick
      end
      object ShowFilesBox: TCheckBox
        Left = 416
        Top = 24
        Width = 97
        Height = 17
        TabStop = False
        Caption = 'Show Files'
        TabOrder = 7
        OnClick = ShowFilesBoxClick
      end
      object DefaultActionCheckBox: TCheckBox
        Left = 416
        Top = 1
        Width = 121
        Height = 17
        TabStop = False
        Caption = 'Use Default Action'
        TabOrder = 3
      end
      object SuggestCheckBox: TCheckBox
        Left = 0
        Top = 24
        Width = 105
        Height = 17
        TabStop = False
        Caption = 'Suggest Names'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = SuggestCheckBoxClick
      end
      object UseThemesCheckBox: TCheckBox
        Left = 288
        Top = 0
        Width = 97
        Height = 17
        TabStop = False
        Caption = 'Use Themes'
        Checked = True
        Ctl3D = True
        ParentCtl3D = False
        State = cbChecked
        TabOrder = 2
        OnClick = UseThemesCheckBoxClick
      end
      object AllowContextMenusCheckBox: TCheckBox
        Left = 144
        Top = 1
        Width = 129
        Height = 17
        TabStop = False
        Caption = 'Use Context Menus'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = AllowContextMenusCheckBoxClick
      end
    end
  end
  object CloseButton: TButton
    Left = 487
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 6
    OnClick = CloseButtonClick
  end
  object ShellPathEdit: TShellPathEditBar
    Left = 94
    Top = 95
    Width = 468
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ParentColor = False
    TabOrder = 8
    OnDefaultAction = ShelNodeDefaultAction
    ShowButtons = True
    ShowIcons = True
    DesignSize = (
      464
      17)
  end
  object ShellBinding: TShellBinding
    SpecialFolder = sfDesktop
    Bindings = (
      'ShellBubbles'
      'ShellEdit'
      'ShellPathEdit'
      'ShellTree'
      'ShellView')
  end
end
