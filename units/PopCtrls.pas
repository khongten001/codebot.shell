
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit PopCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  Dialogs, BaseTypes, GraphTools, ExtCtrls, StdCtrls, Math, CheckLst,
  ScrollCtrls, WinTools, BlendTools, ProviderTools
  {$IFDEF GDIPLUS}, GdiPlus, GdiIntf{$ENDIF};

{ TCustomPopupForm class }

type
  TCaptureKey = (ckNone, ckUp, ckDown, ckLeft, ckRight, ckPrior, ckNext,
    ckReturn, ckEscape, ckTab);

  TCaptureKeys = set of TCaptureKey;

  TTrackSize = record
    Min: TPoint;
    Max: TPoint;
  end;
  PTrackSize = ^TTrackSize;

  TCustomPopupForm = class(TCustomForm)
  private
    FAssociate: TWinControl;
    FAssociatePoint: TPoint;
    FCaptureKeys: TCaptureKeys;
    FDefAssociateWindowProc: TWndMethod;
    FForwardControl: TWinControl;
    FForwardFocus: Boolean;
    FHorzOffset: Integer;
    FSendKeys: Boolean;
    FSizeable: Boolean;
    FStatusText: string;
    FShadow: THotTracker;
    FUseFade: Boolean;
    FUseShadow: Boolean;
    FTimer: TPerformanceTimer;
    FOnCancel: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    procedure AssociateWindowProc(var Message: TMessage);
    procedure SetAssociate(Value: TWinControl);
    procedure SetForwardControl(Value: TWinControl);
    procedure SetSizeable(Value: Boolean);
    procedure SetStatusText(const Value: string);
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure AdjustControlSize; virtual;
    procedure Cancel; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ForwardChange(NewControl: TWinControl); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure QueryTrackSize(var Size: TTrackSize); virtual;
    procedure Select; virtual;
    procedure WndProc(var Message: TMessage); override;
    property CaptureKeys: TCaptureKeys read FCaptureKeys write FCaptureKeys;
    property ForwardControl: TWinControl read FForwardControl write
      SetForwardControl;
    property StatusText: string read FStatusText write SetStatusText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup; dynamic;
    property Associate: TWinControl read FAssociate write SetAssociate;
    property ForwardFocus: Boolean read FForwardFocus write FForwardFocus default True;
    property HorzOffset: Integer read FHorzOffset write FHorzOffset;
    property SendKeys: Boolean read FSendKeys write FSendKeys;
    property Sizeable: Boolean read FSizeable write SetSizeable default True;
    property UseFade: Boolean read FUseFade write FUseFade;
    property UseShadow: Boolean read FUseShadow write FUseShadow;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TPopupFormClass = class of TCustomPopupForm;

{ TScrollingMenuItem }

type
  TScrollingMenuItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FDefaulted: Boolean;
    FImageIndex: Integer;
    procedure SetCaption(Value: TCaption);
    procedure SetImageIndex(Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Defaulted: Boolean read FDefaulted write FDefaulted;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

{ TScrollingMenuItems }

  TScrollingMenuItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TScrollingMenuItem;
    procedure Put(Index: Integer; Value: TScrollingMenuItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TScrollingMenuItem;
    function FindItemID(ID: Integer): TScrollingMenuItem;
    function Insert(Index: Integer): TScrollingMenuItem;
    property Items[Index: Integer]: TScrollingMenuItem read Get write Put; default;
  end;

{ TScrollingMenu }

  TScrollingMenu = class(TCustomDrawList)
  private
    FItems: TScrollingMenuItems;
    procedure ItemsChanged;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; var Rect: TRect; State: TDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TScrollingMenuItems read FItems;
    property Images;
    property OnClick;
  end;

{ TPopupScrollingMenu }

  TPopupScrollingMenu = class(TCustomPopupForm)
  private
    FDisplayCount: Integer;
    FMenu: TScrollingMenu;
    FMenuIndex: Integer;
    FMinWidth: Integer;
    procedure MenuClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
  published
    property DisplayCount: Integer read FDisplayCount write FDisplayCount;
    property MinWidth: Integer read FMinWidth write FMinWidth;
    property Menu: TScrollingMenu read FMenu;
    property MenuIndex: Integer read FMenuIndex;
    property Associate;
    property OnCancel;
    property OnSelect;
  end;

implementation

uses
  StrConst;

function KeyToCaptureKey(Key: Word): TCaptureKey;
begin
  case Key of
    VK_UP:
      Result := ckUp;
    VK_DOWN:
      Result := ckDown;
    VK_LEFT:
      Result := ckLeft;
    VK_RIGHT:
      Result := ckRight;
    VK_PRIOR:
      Result := ckPrior;
    VK_NEXT:
      Result := ckNext;
    VK_RETURN:
      Result := ckReturn;
    VK_ESCAPE:
      Result := ckEscape;
    VK_TAB:
      Result := ckTab;
  else
    Result := ckNone;
  end;
end;

{ TCustomPopupForm }

constructor TCustomPopupForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  FCaptureKeys := [ckUp, ckDown, ckReturn, ckEscape];
  FTimer := TPerformanceTimer.Create;
  FUseFade := True;
  FUseShadow := False;
  Align := alNone;
  BorderStyle := bsNone;
  DesktopFont := True;
  Color := clWindow;
  Visible := False;
  ParentColor := False;
  ParentFont := False;
  Ctl3D := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  WindowState := wsNormal;
  DefaultMonitor := dmActiveForm;
  FForwardFocus := True;
  FSizeable := True;
  FShadow := THotTracker.Create;
  FShadow.StandardBlur := True;
  FShadow.Border := 2;
  FShadow.Clipped := True;
  FShadow.Opacity := High(Byte); 
  FShadow.X := 3;
  FShadow.Y := 3;
end;

destructor TCustomPopupForm.Destroy;
begin
  FTimer.Free;
  if FAssociate <> nil then
  begin
    FAssociate.RemoveFreeNotification(Self);
    FAssociate.WindowProc := FDefAssociateWindowProc;
  end;
  inherited Destroy;
end;

procedure TCustomPopupForm.AdjustControlSize;
begin
  if FForwardControl <> nil then
    with FForwardControl do
    begin
      Parent := Self;
      BoundsRect := Rect(0, 0, Self.ClientWidth, Self.ClientHeight);
      if FSizeable then
        Height := Height - GetSystemMetrics(SM_CYHSCROLL);
      Anchors := [akLeft, akTop, akRight, akBottom];
    end;
end;

procedure TCustomPopupForm.Cancel;
begin
  KillTimer(Handle, 20);
  Hide;
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TCustomPopupForm.CreateParams(var Params: TCreateParams);
const
  ExStyle = WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  Style = WS_BORDER or WS_CHILD or WS_CLIPCHILDREN;
begin
  inherited CreateParams(Params);
  Params.ExStyle := ExStyle;
  Params.Style := Style;
  Params.WndParent := GetDesktopWindow;
end;

procedure TCustomPopupForm.AssociateWindowProc(var Message: TMessage);
begin
  with Message do
    if IsWindowVisible(Handle) then
      case Msg of
        WM_CANCELMODE, CM_CANCELMODE:
          begin
            Cancel;
            FDefAssociateWindowProc(Message);
          end;
        WM_CHAR:
          begin
            if FSendKeys and (FForwardControl <> nil) then
              SendMessage(FForwardControl.Handle, Msg, WParam, LParam);
            FDefAssociateWindowProc(Message);
          end;
        WM_KEYDOWN:
          case WParam of
            VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_SPACE:
              begin
                if FForwardControl <> nil then
                begin
                  SendMessage(FForwardControl.Handle, Msg, WParam, LParam);
                  if KeyToCaptureKey(LongRec(WParam).Lo) in FCaptureKeys then
                    WParam := 0;
                end;
                Result := 0;
              end;
            VK_RETURN:
              begin
                Select;
                Result := 0;
              end;
            VK_ESCAPE:
              begin
                Cancel;
                Result := 0;
              end;
            else
              FDefAssociateWindowProc(Message);
            end;
        WM_KILLFOCUS:
          begin
            Cancel;
            FDefAssociateWindowProc(Message);
          end;
        WM_MOUSEWHEEL:
          begin
            if FForwardControl <> nil then
              PostMessage(FForwardControl.Handle, Msg, WParam, LParam);
            Result := 0;
          end;
      else
        FDefAssociateWindowProc(Message);
      end
    else
      FDefAssociateWindowProc(Message);
end;

procedure TCustomPopupForm.ForwardChange(NewControl: TWinControl);
begin
  if FForwardFocus then
  begin
    if (FForwardControl <> nil) and (FForwardControl.HandleAllocated) then
      SendMessage(FForwardControl.Handle, WM_KILLFOCUS, 0, 0);
    if (NewControl <> nil) and (NewControl.HandleAllocated) then
      SendMessage(NewControl.Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TCustomPopupForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAssociate) then
  begin
    Cancel;
    FAssociate.WindowProc := FDefAssociateWindowProc;
    FAssociate := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TCustomPopupForm.Paint;
var
  DC: HDC;
  Rect: TRect;
  Brush: HBRUSH;
begin
  DC := Canvas.Handle;
  Windows.GetClientRect(Handle, Rect);
  Brush := CreateSolidBrush(ColorToRGB(Color));
  if FSizeable then
  begin
    Dec(Rect.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    FillRect(DC, Rect, Brush);
    Rect.Top := Rect.Bottom;
    Inc(Rect.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    DrawThemeStatus(DC, FStatusText, Rect);
  end
  else
    FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
end;

procedure TCustomPopupForm.QueryTrackSize(var Size: TTrackSize);
begin
  Size.Min.x := GetSystemMetrics(SM_CXVSCROLL) + 2;
  Size.Min.y := 75;
end;

procedure TCustomPopupForm.WMTimer(var Message: TWMTimer);
const
  FadeDuration = 200;
var
  Delta: Int64;
  Moved: Boolean;
  P: TPoint;
begin
  inherited;
  if Message.TimerID = 10 then
  begin
    Delta :=  FTimer.ElapsedTime;
    if Delta > FadeDuration then
    begin
      if FUseShadow then
      begin
        FShadow.Opacity := High(Byte);
        FShadow.Update;
      end;
      AlphaBlend := False;
      AlphaBlendValue := $FF;
      KillTimer(Handle, 10);
      FTimer.Stop;
    end
    else
    begin
      AlphaBlendValue := Round((FTimer.ElapsedTime / FadeDuration) * $FF);
      if FUseShadow then
      begin
        FShadow.Opacity := AlphaBlendValue;
        FShadow.Update;
      end;
    end;
  end
  else if Message.TimerID = 20 then
  begin
    if FAssociate <> nil then
    begin
      with FAssociate do
        P := Parent.ClientToScreen(BoundsRect.TopLeft);
      Moved := (P.X <> FAssociatePoint.X) or (P.Y <> FAssociatePoint.Y);
      if Moved then
        Cancel;
    end;
  end;
end;

procedure TCustomPopupForm.Popup;
var
  R: TRect;
  P: TPoint;
begin
  if FAssociate = nil then Exit;
  with FAssociate do
  begin
    P := Parent.ClientToScreen(BoundsRect.TopLeft);
    R := Rect(P.X + FHorzOffset, P.Y + Height + 1, P.X + Self.Width + FHorzOffset,
      P.Y + Height + 1 + Self.Height);
  end;
  FAssociatePoint := P;
  SetTimer(Handle, 20, 10, nil);
  if Screen.MonitorCount < 2 then
  begin
    if R.Right > Screen.Width then
      OffsetRect(R, Screen.Width - R.Right, 0);
    if R.Bottom > Screen.Height then
      OffsetRect(R, 0, -HeightOf(R) - FAssociate.Height - 2);
  end;
  BoundsRect := R;
  if FUseFade then
  begin
    AlphaBlendValue := 0;
    AlphaBlend := True;
    if FUseShadow then
      FShadow.Opacity := 0;
    FTimer.Stop;
    FTimer.Start;
    SetTimer(Handle, 10, 10, nil);
  end;
  Show;
  if (FForwardControl <> nil) and FForwardFocus then
    SendMessage(FForwardControl.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TCustomPopupForm.Select;
begin
  Hide;
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TCustomPopupForm.WndProc(var Message: TMessage);
var
  Point: TPoint;
  Rect: TRect;
begin
  with Message do
    case Msg of
      WM_ERASEBKGND:
        Result := 0;
      WM_GETMINMAXINFO:
        with PMinMaxInfo(lParam)^ do
        begin
          QueryTrackSize(PTrackSize(@ptMinTrackSize)^);
          Result := 0;
        end;
      WM_NCHITTEST:
        begin
          Point.x := SmallInt(LongRec(LParam).Lo);
          Point.y := SmallInt(LongRec(LParam).Hi);
          Windows.ScreenToClient(Handle, Point);
          Windows.GetClientRect(Handle, Rect);
          if FSizeable and (Point.x > Rect.Right - 16) and
            (Point.y > Rect.Bottom - 16) then
            Result := HTBOTTOMRIGHT
          else
            Result := HTCLIENT;
        end;
      WM_SETCURSOR:
        begin
          case LongRec(LParam).Lo of
            HTBOTTOMRIGHT:
              SetCursor(LoadCursor(0, IDC_SIZENWSE));
            HTCLIENT:
              SetCursor(LoadCursor(0, IDC_ARROW));
          end;
          Result := 1;
        end;
      WM_SIZE:
        begin
          InvalidateRect(Handle, nil, False);
          inherited WndProc(Message);
        end;
    else
      inherited WndProc(Message);
    end;
end;

procedure TCustomPopupForm.SetAssociate(Value: TWinControl);
begin
  if Value <> FAssociate then
  begin
    Cancel;
    if FAssociate <> nil then
    begin
      FAssociate.RemoveFreeNotification(Self);
      FAssociate.WindowProc := FDefAssociateWindowProc;
    end;
    FAssociate := Value;
    if FAssociate <> nil then
    begin
      FAssociate.FreeNotification(Self);
      FDefAssociateWindowProc :=  FAssociate.WindowProc;
      FAssociate.WindowProc := AssociateWindowProc;
      Width := FAssociate.Width;
      Height := 175;
    end;
  end;
end;

procedure TCustomPopupForm.SetForwardControl(Value: TWinControl);
begin
  if Value <> FForwardControl then
  begin
    ForwardChange(Value);
    FForwardControl := Value;
  end;
end;

procedure TCustomPopupForm.SetSizeable(Value: Boolean);
begin
  if Value <> FSizeable then
  begin
    Cancel;
    FSizeable := Value;
    AdjustControlSize;
  end;
end;

procedure TCustomPopupForm.SetStatusText(const Value: string);
begin
  if Value <> FStatusText then
  begin
    FStatusText := Value;
    Invalidate;
  end;
end;

procedure TCustomPopupForm.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  if FUseShadow then
    DrawFrame(DC, R, dfRaised)
  else
    FillRectColor(DC, R, clBtnFace);
  ReleaseDC(Handle, DC);
  Message.Result := 0;
end;

procedure TCustomPopupForm.WMShowWindow(var Message: TWMShowWindow);
begin
  inherited;
  if FUseShadow and (Message.Status = 0) then
    if Message.Show  then
    begin
      FShadow.Associate := Handle;
      FShadow.Update;
    end
    else
    begin
      FShadow.Visible := False;
      FShadow.Associate := 0;
    end;
end;

procedure TCustomPopupForm.WMSize(var Message: TWMSize);
begin
  inherited;
  if Visible and FUseShadow then
    FShadow.Update;
end;

{ TScrollingMenuItem }

constructor TScrollingMenuItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
end;

procedure TScrollingMenuItem.Assign(Source: TPersistent);
var
  EditItem: TScrollingMenuItem absolute Source;
begin
  if Source is TScrollingMenuItem then
  begin
    FCaption := EditItem.Caption;
    FDefaulted := EditItem.Defaulted;
    FImageIndex := EditItem.ImageIndex;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TScrollingMenuItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TScrollingMenuItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

{ TScrollingMenuItems }

constructor TScrollingMenuItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScrollingMenuItem);
end;

function TScrollingMenuItems.Add: TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(inherited Add);
end;

function TScrollingMenuItems.FindItemID(ID: Integer): TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(inherited FindItemID(ID));
end;

function TScrollingMenuItems.Insert(Index: Integer): TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(inherited Insert(Index));
end;

procedure TScrollingMenuItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TScrollingMenu then
    TScrollingMenu(GetOwner).ItemsChanged;
end;

function TScrollingMenuItems.Get(Index: Integer): TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(GetItem(Index));
end;

procedure TScrollingMenuItems.Put(Index: Integer; Value: TScrollingMenuItem);
begin
  SetItem(Index, Value);
end;

{ TScrollingMenu }

constructor TScrollingMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TScrollingMenuItems.Create(Self);
  ItemHeight := 22;
  Color := clMenu;
  DoubleBuffered := True;
  BorderStyle := bsNone;
  HotTrack := True;
end;

destructor TScrollingMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TScrollingMenu.ItemsChanged;
begin
  Count := FItems.Count;
  Invalidate;
end;

const
  GutterWidth = 32;

procedure TScrollingMenu.DrawBackground;
var
  DC: HDC;
  R: TRect;
begin
  DC := Canvas.Handle;
  R := ClientRect;
  R.Right := R.Left + GutterWidth;
  FillRectColor(DC, R, clMenuBar);
  R.Left := R.Right;
  R.Right := ClientWidth;
  FillRectColor(DC, R, Color);
end;

procedure TScrollingMenu.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  Item: TScrollingMenuItem;
  DC: HDC;
  F: HFONT;
  R: TRect;
begin
  Item := Items[Index];
  DC := Canvas.Handle;
  R := Rect;
  R.Right := GutterWidth;
  DrawRectEdge(DC, R, clBtnHighlight, drRight);
  Dec(R.Right);
  DrawRectEdge(DC, R, Blend(clBtnShadow, clMenu, 30), drRight);
  R := Rect;
  Dec(R.Right);
  if dsHot in State then
    DrawFancyMenuHighlightRect(DC, R);
  R := Rect;
  R.Right := R.Left + GutterWidth;
  ImageListDraw(Images, Canvas, R, Item.ImageIndex, []);
  R.Left := R.Right;
  R.Right := Rect.Right;
  InflateRect(R, -4, 0);
  if Item.Defaulted then
  begin
    F := SelectObject(DC, GetFont(DC, [fsBold]));
    DrawCaption(DC, Item.Caption, R, drLeft);
    OverwriteObject(DC, F);
  end
  else
    DrawCaption(DC, Item.Caption, R, drLeft);
end;

procedure TScrollingMenu.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if Message.WheelDelta < 0 then
    TopIndex := TopIndex + 3
  else
    TopIndex := TopIndex - 3;
  Message.Result := 1;
end;

{ TPopupScrollingMenu }

constructor TPopupScrollingMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisplayCount := 15;
  FMenu := TScrollingMenu.Create(Self);
  FMenu.Parent := Self;
  FMenu.Align := alClient;
  FMenu.OnClick := MenuClick;
  ForwardControl := FMenu;
  FMinWidth := 120;
end;

procedure TPopupScrollingMenu.Popup;

  function ComputeWidth: Integer;
  var
    DC: HDC;
    F: HFont;
    Extra: Integer;
    W: Integer;
    I: Integer;
  begin
    Result := FMinWidth;
    DC := Canvas.Handle;
    Extra := 52;
    if FMenu.Items.Count > FDisplayCount then
      Inc(Extra, GetSystemMetrics(SM_CXVSCROLL));
    F := 0;
    for I := 0 to FMenu.Items.Count - 1 do
    begin
      if FMenu.Items[I].Defaulted then
        F := SelectObject(DC, GetFont(DC, [fsBold]));
      W := FontWidth(DC, FMenu.Items[I].Caption) + Extra;
      if FMenu.Items[I].Defaulted then
        OverwriteObject(DC, F);
      if W > Result then
        Result := W;
    end;
  end;

var
  I: Integer;
begin
  I := FMenu.Count;
  if I = 0 then Exit;
  FMenu.ItemIndex := 0;
  if I > FDisplayCount then
    I := FDisplayCount;
  Width := ComputeWidth;
  Height := FMenu.ItemHeight * I + 2;
  inherited Popup;
end;

procedure TPopupScrollingMenu.MenuClick(Sender: TObject);
begin
  FMenuIndex := FMenu.ItemIndex;
  Select;
end;

end.
