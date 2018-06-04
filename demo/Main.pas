
(********************************************************)
(*                                                      *)
(*  Codebot Shell Controls Demo                         *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShlTools, ShlCtrls, ProviderTools, FormTools,
  BtnEdit, ScrollCtrls, GraphTools;

type
  TShellDemoForm = class(TForm)
    ShellView: TShellView;
    ShellTree: TShellTree;
    ShellBubbles: TShellBubbles;
    FilterCombo: TComboBox;
    FileNameEdit: TEdit;
    ShellEdit: TShellEdit;
    GroupBox: TGroupBox;
    NameLabel: TLabel;
    TypeLabel: TLabel;
    CloseButton: TButton;
    Panel: TPanel;
    UsePathEditCheckBox: TCheckBox;
    ShowIconsCheckBox: TCheckBox;
    ShowButtonsCheckBox: TCheckBox;
    ShowFilesBox: TCheckBox;
    DefaultActionCheckBox: TCheckBox;
    SuggestCheckBox: TCheckBox;
    UseThemesCheckBox: TCheckBox;
    AllowContextMenusCheckBox: TCheckBox;
    ShellBinding: TShellBinding;
    ShellPathEdit: TShellPathEditBar;
    procedure FormCreate(Sender: TObject);
    procedure ShelNodeDefaultAction(Sender: TObject; Node: TShellNode;
      var AllowAction: Boolean);
    procedure CloseButtonClick(Sender: TObject);
    procedure UsePathEditCheckBoxClick(Sender: TObject);
    procedure SuggestCheckBoxClick(Sender: TObject);
    procedure ShowIconsCheckBoxClick(Sender: TObject);
    procedure ShowButtonsCheckBoxClick(Sender: TObject);
    procedure ShowFilesBoxClick(Sender: TObject);
    procedure UseThemesCheckBoxClick(Sender: TObject);
    procedure ShellViewIncludeItem(Sender: TObject; Node: TShellNode;
      var AllowAction: Boolean);
    procedure FilterComboChange(Sender: TObject);
    procedure AllowContextMenusCheckBoxClick(Sender: TObject);
    procedure ShellViewSelectionChanged(Sender: TObject);
  end;

var
  ShellDemoForm: TShellDemoForm;

implementation

{$R *.dfm}

procedure TShellDemoForm.FormCreate(Sender: TObject);
begin
  DesktopFont := True;
  { Becase we are using desktop font we need to align labels }
  AlignCenter(FileNameEdit, NameLabel);
  AlignCenter(FilterCombo, TypeLabel);
end;

procedure TShellDemoForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TShellDemoForm.ShelNodeDefaultAction(Sender: TObject; Node: TShellNode;
  var AllowAction: Boolean);
begin
  { If we are working with a file }
  if Node.ShellFolder = nil then
    { The default action is to open it, we can stop that here }
    if not DefaultActionCheckBox.Checked then
      AllowAction := MessageDlg('Do you really want to open "' + Node.Name + '"', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TShellDemoForm.UsePathEditCheckBoxClick(Sender: TObject);
begin
  ShellEdit.Visible := not UsePathEditCheckBox.Checked;
  ShellPathEdit.Visible := UsePathEditCheckBox.Checked;
end;

procedure TShellDemoForm.SuggestCheckBoxClick(Sender: TObject);
begin
  if SuggestCheckBox.Checked then
  begin
    ShellPathEdit.ShowSuggest := True;
    ShellEdit.Options := ShellEdit.Options + [soSuggest];
  end
  else
  begin
    ShellPathEdit.ShowSuggest := False;
    ShellEdit.Options := ShellEdit.Options - [soSuggest];
  end;
end;

procedure TShellDemoForm.ShowIconsCheckBoxClick(Sender: TObject);
begin
  ShellPathEdit.ShowIcons := ShowIconsCheckBox.Checked;
end;

procedure TShellDemoForm.ShowButtonsCheckBoxClick(Sender: TObject);
begin
  ShellPathEdit.ShowButtons := ShowButtonsCheckBox.Checked;
end;

procedure TShellDemoForm.ShowFilesBoxClick(Sender: TObject);
begin
  ShellPathEdit.ShowFiles := ShowFilesBox.Checked;
end;

procedure TShellDemoForm.UseThemesCheckBoxClick(Sender: TObject);
begin
  ThemePainter.Enabled := UseThemesCheckBox.Checked;
  { work around for bug in TCheckBox with theme changes }
  UseThemesCheckBox.Visible := False;
  UseThemesCheckBox.Visible := True;
  SendMessage(WM_THEMECHANGED, UseThemesCheckBox.Handle, 0, 0);
end;

procedure TShellDemoForm.AllowContextMenusCheckBoxClick(Sender: TObject);
begin
  ShellTree.AllowContextMenu := AllowContextMenusCheckBox.Checked;
  ShellView.AllowContextMenu := AllowContextMenusCheckBox.Checked;
end;

procedure TShellDemoForm.ShellViewSelectionChanged(Sender: TObject);
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ShellView.GetItems(S, True);
    FileNameEdit.Text := S.CommaText;
  finally
    S.Free;
  end;
end;

procedure TShellDemoForm.ShellViewIncludeItem(Sender: TObject;
  Node: TShellNode; var AllowAction: Boolean);
const
  ImageExtensions: array[0..8] of string = (
   '.JPG', '.JPEG', '.GIF', '.PNG', '.BMP',  '.TIF',
   '.PSD', '.PSPIMAGE', '.SVG');
var
  Ext: string;
  I: Integer;
begin
  { Handle filtering items in the shell view }
  case  FilterCombo.ItemIndex of
    { Allow folders or text files }
    1: AllowAction := (Node.ShellFolder <> nil) or (ExtractFilePath(UpperCase(Node.Path)) = '.TXT');
    { Allow only images }
    2:
      begin
        AllowAction := Node.ShellFolder = nil;
        if AllowAction then
        begin
          Ext := ExtractFileExt(UpperCase(Node.Path));
          AllowAction := False;
          for I := Low(ImageExtensions) to High(ImageExtensions) do
            if ImageExtensions[I] = Ext then
            begin
              AllowAction := True;
              Break;
            end;
        end;
      end;
    { Allow only folders }
    3: AllowAction := Node.ShellFolder <> nil;
    { Allow folders or images }
    4:
      begin
        AllowAction := Node.ShellFolder <> nil;
        if not AllowAction then
        begin
          Ext := ExtractFileExt(UpperCase(Node.Path));
          for I := Low(ImageExtensions) to High(ImageExtensions) do
            if ImageExtensions[I] = Ext then
            begin
              AllowAction := True;
              Break;
            end;
        end;
      end;
  end;
end;

procedure TShellDemoForm.FilterComboChange(Sender: TObject);
begin
  case  FilterCombo.ItemIndex of
    0: ShellView.ViewMode := vmIcon;
    1: ShellView.ViewMode := vmDetails;
    2: ShellView.ViewMode := vmThumbnail;
    3: ShellView.ViewMode := vmList;
    4: ShellView.ViewMode := vmTile;
  end;
  { Reload items to reflect the new filter }
  ShellView.Refresh;
end;

end.
