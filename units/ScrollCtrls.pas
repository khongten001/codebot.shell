
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit ScrollCtrls;

interface

{$I CODEBOT.INC}

uses
  Classes, Controls, Forms, Graphics, Messages, SysUtils, Windows, StdCtrls,
  ImgList, BaseTypes, GraphTools, ProviderTools, FormTools;

{ TControlHintWindow }

type
  TControlHintWindow = class(THintWindow)
  private
    FActive: Boolean;
    FControl: TControl;
    FPoint: TPoint;
    procedure SetActive(Value: Boolean);
    procedure SetControl(Value: TControl);
    procedure SetPoint(const Value: TPoint);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    property Active: Boolean read FActive write SetActive;
    property Control: TControl read FControl write SetControl;
    property Point: TPoint read FPoint write SetPoint;
  end;

{ TScrollList }

  TScrollDir = (sdNone, sdUp, sdDown);
  TSelectItems = array of Boolean;

  TScrollList = class(TFramedImagesWindow)
  private
    FCount: Integer;
    FDownIndex: Integer;
    FHotTrack: Boolean;
    FHotIndex: Integer;
    FHintWindow: TControlHintWindow;
    FInsideRect: Boolean;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FLocked: Boolean;
    FLockedIndex: Integer;
    FTopIndex: Integer;
    FScrolling: Boolean;
    FMouseDisabled: Boolean;
    FMultiSelect: Boolean;
    FSelectCount: Integer;
    FSelectItems: TSelectItems;
    FShift: TShiftState;
    FShiftIndex: Integer;
    FOnSelectItem: TNotifyEvent;
    procedure SetCount(Value: Integer);
    procedure SetHotTrack(Value: Boolean);
    procedure SetMouseDisabled(Value: Boolean);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetScrollIndex(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetScrolling(Value: Boolean);
    function GetSelected(Index: Integer): Boolean;
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetTopIndex(Value: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMCaptureChanged(var Message: TWMNoParams); message WM_CAPTURECHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure CreateHandle; override;
    procedure CaptureMove(X, Y: Integer); virtual;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawItem(Index: Integer; var Rect: TRect; State: TDrawState); virtual;
    procedure InvalidateItem(Item: Integer);
    procedure UpdateScrollRange;
    procedure Scroll(Delta: Integer); virtual;
    procedure ScrollMove(Distance: Integer; Direction: TScrollDir); virtual;
    procedure SelectItem(PriorIndex: Integer; NewIndex: Integer; var CanSelect: Boolean); virtual;
    property DownIndex: Integer read FDownIndex;
    property Count: Integer read FCount write SetCount;
    property HintWindow: TControlHintWindow read FHintWindow;
    property HotIndex: Integer read FHotIndex;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property MouseDisabled: Boolean read FMouseDisabled write SetMouseDisabled;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property InsideRect: Boolean read FInsideRect write FInsideRect;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 16;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Locked: Boolean read FLocked write FLocked;
    property Scrolling: Boolean read FScrolling write SetScrolling;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property SelectCount: Integer read FSelectCount;
    property TopIndex: Integer read FTopIndex write SetTopIndex;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  public
    constructor Create(AOwner: TComponent); override;
    function ItemRect(Item: Integer): TRect;
    function ItemAtPos(const Pos: TPoint; Existing: Boolean = False): Integer;
    procedure ScrollToSelection;
    procedure InsureItemVisible;
  end;

{ TCustomDrawList }

  TCustomDrawList = class(TScrollList)
  private
    FAutoScroll: Boolean;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawItem: TDrawIndexEvent;
    procedure SetAutoScroll(Value: Boolean);
  protected
    procedure Paint; override;
    procedure DrawBackground; virtual;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure Scroll(Delta: Integer); override;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawItem: TDrawIndexEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TDrawList }

  TDrawList = class(TCustomDrawList)
  public
    property Canvas;
    property Count;
    property MouseDisabled;
    property TopIndex;
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TDrawTextList }

  TDrawTextList = class(TCustomDrawList)
  private
    FItems: TStrings;
    procedure SetItems(Value: TStrings);
    procedure ItemsChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property MouseDisabled;
    property TopIndex;
    property ItemIndex;
    property SelectCount;
    property Selected;
  published
    property Items: TStrings read FItems write SetItems;
    property Align;
    property Anchors;
    property AutoScroll;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawBackground;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TCustomBubbleList }

  TCustomBubbleList = class(TCustomDrawList)
  private
    FClicked: Boolean;
    FKeyDown: Boolean;
    FDownIndex: Integer;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SelectItem(PriorIndex, NewIndex: Integer;
      var CanSelect: Boolean); override;
    property Clicked: Boolean read FClicked;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBubbleList }

  TBubbleList = class(TCustomBubbleList)
  public
    property Canvas;
    property Count;
    property TopIndex;
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnDblClick;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TControlHintWindow }

constructor TControlHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWindow;
end;

procedure TControlHintWindow.ActivateHint(Rect: TRect; const AHint: string);
begin
  FActive := True;
  if IsRectEmpty(Rect) then
  begin
    Rect := CalcHintRect(High(Integer), AHint, nil);
    if FControl <> nil then
      with FControl.ClientToScreen(FPoint) do
        OffsetRect(Rect, x - 4, y - 3)
    else
      OffsetRect(Rect, FPoint.x, FPoint.y);
  end;
  inherited ActivateHint(Rect, AHint);
end;

procedure TControlHintWindow.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
      ActivateHint(Rect(0, 0, 0, 0), Caption)
    else
      ShowWindow(Handle, SW_HIDE);
  end;
end;

procedure TControlHintWindow.SetControl(Value: TControl);
begin
  FControl := Value;
  Active := False;
end;

procedure TControlHintWindow.SetPoint(const Value: TPoint);
begin
  if (Value.x <> FPoint.x) or (Value.y <> FPoint.y) then
  begin
    FPoint := Value;
    Active := False;
  end;
end;

{ TScrollList }

const
  ScrollTimer = $100;

constructor TScrollList.Create(AOwner: TComponent);
begin
  FDownIndex := -1;
  FHotIndex := -1;
  FShiftIndex := -1;
  FHintWindow := TControlHintWindow.Create(Self);
  FHintWindow.Control := Self;
  FItemHeight := 15;
  FItemIndex := -1;
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csOpaque];
  ParentColor := False;
  ParentFont := True;
  Width := 100;
  Height := 200;
end;

procedure TScrollList.CreateHandle;
begin
  inherited CreateHandle;
  UpdateScrollRange;
end;

procedure TScrollList.CMMouseLeave(var Message: TMessage);
var
  R: TRect;
  I: Integer;
begin
  FHintWindow.Active := False;
  if FHotIndex > -1 then
  begin
    I := FHotIndex;
    FHotIndex := -1;
    R := ItemRect(I);
    InvalidateRect(Handle, @R, False);
  end;
  inherited;
end;

procedure TScrollList.WMCaptureChanged(var Message: TWMNoParams);
begin
  inherited;
  FScrolling := GetCapture = Handle;
end;

procedure TScrollList.WMMouseWheel(var Message: TWMMouseWheel);
var
  Delta: Integer;
begin
  if FMultiSelect then
    FShift := KeyboardStateToShiftState - [ssCtrl];
  HintWindow.Active := False;
  Delta := -Message.WheelDelta div 120;
  if ItemIndex > -1 then
    if ItemIndex + Delta < 0 then
      ItemIndex := 0
    else if ItemIndex + Delta > Count - 1 then
      ItemIndex := Count - 1
    else
      ItemIndex := ItemIndex + Delta;
  if ItemIndex = Count - 1 then
    SetScrollIndex(ItemIndex + 1);
  InsureItemVisible;
  inherited;
end;

procedure TScrollList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TScrollList.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateScrollRange;
end;

procedure TScrollList.WMTimer(var Message: TWMTimer);
var
  Point: TPoint;
  ScrollDir: TScrollDir;
  Distance: Integer;
begin
  Message.Result := 0;
  if (Message.TimerID <> ScrollTimer) or FMouseDisabled then Exit;
  if FScrolling then
  begin
    GetCursorPos(Point);
    Windows.ScreenToClient(Handle, Point);
    ScrollDir := sdNone;
    Distance := 0;
    with Point do
      if Y < 0 then
      begin
        Distance := -Y div FItemHeight + 1;
        ScrollDir := sdUp;
      end
      else if Y > ClientHeight then
      begin
        Distance := (Y - ClientHeight) div FItemHeight + 1;
        ScrollDir := sdDown;
      end;
    if ScrollDir <> sdNone then
      ScrollMove(Distance, ScrollDir)
    else
    begin
      FScrolling := False;
      KillTimer(Handle, ScrollTimer);
    end;
  end
  else
    KillTimer(Handle, ScrollTimer);
end;

procedure TScrollList.WMVScroll(var Message: TWMScroll);
begin
  with Message do
    case ScrollCode of
      SB_BOTTOM: SetTopIndex(FCount - 1);
      SB_LINEDOWN: SetTopIndex(FTopIndex + 1);
      SB_LINEUP: SetTopIndex(FTopIndex - 1);
      SB_PAGEDOWN: SetTopIndex(FTopIndex + ClientHeight div FItemHeight);
      SB_PAGEUP: SetTopIndex(FTopIndex - ClientHeight div FItemHeight);
      SB_THUMBTRACK:
        begin
          {if Pos < 0 then
            Pos := Count;}
          SetTopIndex(Pos);
        end;
      SB_TOP: SetTopIndex(0);
    end;
end;

procedure TScrollList.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TScrollList.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TScrollList.CaptureMove(X, Y: Integer);
var
  I: Integer;
begin
  if MouseCapture then
  begin
    I := ItemAtPos(Point(X, Y));
    if I < 0 then Exit;
    if (Y < 0) or (Y > ClientHeight) then
      Scrolling := True
    else
      SetScrollIndex(I);
  end;
end;

procedure TScrollList.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ScrollToSelection;
end;

procedure TScrollList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FShift := Shift;
  case Key of
    VK_HOME: ItemIndex := 0;
    VK_END: ItemIndex := Count - 1;
    VK_NEXT: SetScrollIndex(ItemIndex + ClientHeight div FItemHeight);
    VK_PRIOR: SetScrollIndex(ItemIndex - ClientHeight div FItemHeight);
    VK_UP: SetScrollIndex(ItemIndex - 1);
    VK_DOWN: SetScrollIndex(ItemIndex + 1);
  end;
  InsureItemVisible;
end;

procedure TScrollList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FShift := Shift;
  FHintWindow.Active := False;
  if Button = mbLeft then
  begin
    CaptureMove(X, Y);
    if CanFocus then
      SetFocus;
    FDownIndex := ItemAtPos(Point(X, Y), True);
    if FDownIndex > -1 then
    begin
      if (FItemIndex > -1) and (FItemIndex <> FDownIndex) then
        InvalidateItem(FItemIndex);
      FItemIndex := FDownIndex;
      FHotIndex := FDownIndex;
      InvalidateItem(FDownIndex);
    end;
  end;
end;

procedure TScrollList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PriorIndex: Integer;
  P: TPoint;
  I: Integer;
begin
  if Button = mbLeft then
  begin
    if FDownIndex > -1 then
    begin
      PriorIndex := FDownIndex;
      FDownIndex := -1;
      if PriorIndex > -1 then
        InvalidateItem(PriorIndex);
    end;
    if FHotTrack then
    begin
      P := Point(X, Y);
      I := ItemAtPos(P, False);
      if I <> FHotIndex then
        InvalidateItem(FHotIndex);
      FHotIndex := I;
      if FHotIndex > -1 then
      begin
        InvalidateItem(FHotIndex);
        if not PtInRect(ItemRect(FHotIndex), P) then
          FHotIndex := -1;
      end;;
    end;
  end;
  if Button = mbLeft then
    InsureItemVisible;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TScrollList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FMultiSelect then
    FShift := KeyboardStateToShiftState - [ssCtrl];
  if FHotTrack and (not FMouseDisabled) then
  begin
    I := ItemAtPos(Point(X, Y), True);
    if I <> FHotIndex then
    begin
      InvalidateItem(FHotIndex);
      FHotIndex := I;
      InvalidateItem(FHotIndex);
    end;
  end;
  if MouseCapture and (not FMouseDisabled) then
    CaptureMove(X, Y);
end;

procedure TScrollList.Paint;
var
  UpdateRect, R: TRect;
  DrawState: TDrawState;
  I: Integer;
begin
  UpdateRect := Canvas.ClipRect;
  with UpdateRect do
  begin
    Left := 0;
    Right := ClientWidth;
    Top := (Top div FItemHeight) * FItemHeight;
    if Bottom mod FItemHeight > 0 then
      Bottom := Bottom + FItemHeight;
    Bottom := (Bottom div FItemHeight) * FItemHeight;
    for I := Top div FItemHeight to Bottom div FItemHeight do
    begin
      if I + FTopIndex > FCount - 1 then
        Break;
      Top := I * FItemHeight;
      Bottom := Top + FItemHeight;
      DrawState := [];
      if Focused then
        Include(DrawState, dsFocused);
      if FTopIndex + I = ItemIndex then
      begin
        if FMultiSelect then
          Include(DrawState, dsDefaulted)
        else
          Include(DrawState, dsSelected);
      end;
      if FMultiSelect and FSelectItems[FTopIndex + I] then
        Include(DrawState, dsSelected);
      if FTopIndex + I = FHotIndex then
        Include(DrawState, dsHot);
      if FTopIndex + I = FDownIndex then
        Include(DrawState, dsPressed);
      R := UpdateRect;
      DrawItem(FTopIndex + I, R, DrawState);
    end;
    Top := Bottom;
  end;
  inherited Paint;
end;

procedure TScrollList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
begin
end;

procedure TScrollList.InvalidateItem(Item: Integer);
var
  Rect: TRect;
begin
  if Item > -1 then
    if HandleAllocated then
    begin
      Rect := ItemRect(Item);
      InvalidateRect(Handle, @Rect, True);
    end;
end;

function TScrollList.ItemRect(Item: Integer): TRect;
begin
  Result := Classes.Rect(0, (Item - FTopIndex) * FItemHeight, ClientWidth,
    (Item - FTopIndex + 1) * FItemHeight);
end;

procedure TScrollList.Scroll(Delta: Integer);
begin
  ScrollBy(0, Delta);
end;

procedure TScrollList.ScrollMove(Distance: Integer; Direction: TScrollDir);
const
  Movement: array[TScrollDir] of Integer = (0, -1, 1);
begin
  if Distance > 0 then
    SetScrollIndex(ItemIndex + Distance * Movement[Direction]);
end;

procedure TScrollList.SelectItem(PriorIndex: Integer; NewIndex: Integer;
  var CanSelect: Boolean);
begin
  if CanSelect then
  begin
    FItemIndex := NewIndex;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self);
  end;
end;

procedure TScrollList.ScrollToSelection;
begin
  if FItemIndex < FTopIndex then
    SetTopIndex(FItemIndex)
  else if FItemIndex >= FTopIndex + (ClientHeight + 1) div FItemHeight  then
    SetTopIndex(FItemIndex - (ClientHeight - 1) div FItemHeight);
end;

procedure TScrollList.InsureItemVisible;
begin
  if (ItemIndex > -1) and (ItemRect(ItemIndex).Bottom > ClientHeight) and
    (TopIndex < ItemIndex) then
    TopIndex := TopIndex + 1;
end;

function TScrollList.ItemAtPos(const Pos: TPoint;
  Existing: Boolean = False): Integer;
begin
  Result := FTopIndex + Pos.Y div FItemHeight;
  if Result > FCount - 1 then
    if Existing then Result := -1 else Result := FCount - 1;
  if FInsideRect and ((Pos.X < 0) or (Pos.X > ClientWidth - 1)) then
    Result := -1;
  if FLocked and (Result <> FLockedIndex) then
    Result := -1;
end;

procedure TScrollList.UpdateScrollRange;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then
    with ScrollInfo do
    begin
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
      nMin := 0;
      nMax := FCount - 1;
      nPage := ClientHeight div FItemHeight;
      nPos := FTopIndex;
      SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      if FCount - FTopIndex < Integer(nPage) then SetTopIndex(FCount -
        Integer(nPage));
    end;
end;

procedure TScrollList.SetCount(Value: Integer);
begin
  if Value <> FCount then
  begin
    if Value < 0 then
      Value := 0;
    FCount := Value;
    FSelectItems := nil;
    if FMultiSelect and (FCount > 0) then
      SetLength(FSelectItems, FCount);
    FItemIndex := -1;
    FHotIndex := -1;
    if FCount > 0 then
      ItemIndex := 0;
    UpdateScrollRange;
    Invalidate;
  end;
end;

procedure TScrollList.SetItemHeight(Value: Integer);
var
  WasVisible: Boolean;
begin
  if Value < 1 then
    Value := 1;
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    WasVisible := Visible;
    Visible := False;
    UpdateScrollRange;
    Visible := WasVisible;
    Repaint;
  end;
end;

procedure TScrollList.SetHotTrack(Value: Boolean);
begin
  if Value <> FHotTrack then
  begin
    FHotTrack := Value;
    InvalidateItem(FHotIndex);
    FHotIndex := -1;
  end;
end;

procedure TScrollList.SetMouseDisabled(Value: Boolean);
begin
  if Value <> FMouseDisabled then
  begin
    FMouseDisabled := Value;
    InvalidateItem(FHotIndex);
    FHotIndex := -1;
    KillTimer(Handle, ScrollTimer);
  end;
end;

procedure TScrollList.SetMultiSelect(Value: Boolean);
begin
  if Value <> FMultiSelect then
  begin
    FMultiSelect := Value;
    FSelectItems := nil;
    if FMultiSelect and (FCount > 0) then
    begin
      SetLength(FSelectItems, FCount);
      if FItemIndex > -1 then
        FSelectItems[FItemIndex] := True;
    end
    else
      FSelectCount := 0;
    Invalidate;
    FHotIndex := -1;
    KillTimer(Handle, ScrollTimer);
  end;
end;

procedure TScrollList.SetScrollIndex(Value: Integer);
begin
  if Count = 0 then
    SetItemIndex(-1)
  else if Value > Count - 1 then
  begin
    SetItemIndex(Count - 1);
    SetTopIndex(FTopIndex + 1);
  end
  else if Value < 0 then
    SetItemIndex(0)
  else
    SetItemIndex(Value);
end;

procedure TScrollList.SetItemIndex(Value: Integer);
var
  PriorIndex: Integer;
  CanSelect: Boolean;
  WasSelected: Boolean;
  I: Integer;
begin
  if FLocked then
    if Value > -1 then
      Value := FLockedIndex;
  // FDownIndex := -1;
  if Value > Count - 1 then
    Value := Count - 1;
  if Value > -1 then
    FLockedIndex := Value;
  if Value <> FItemIndex then
  begin
    PriorIndex := FItemIndex;
    if not HandleAllocated then
    begin
      FItemIndex := Value;
      Exit;
    end;
    CanSelect := True;
    SelectItem(FItemIndex, Value, CanSelect);
    if CanSelect then
    begin
      if PriorIndex > -1 then
        InvalidateItem(PriorIndex);
      FItemIndex := Value;
      ScrollToSelection;
      if PriorIndex <> FItemIndex then
      begin
        InvalidateItem(FItemIndex);
        if FMultiSelect and (FItemIndex > -1) then
          if ssShift in FShift then
          begin
            if FShiftIndex > -1 then
              WasSelected := FSelectItems[FShiftIndex]
            else
              WasSelected := False;
            FillChar(Pointer(@FSelectItems[0])^, FCount, #0);
            FSelectCount := 0;
            if FShiftIndex > -1 then
            begin
              if FItemIndex < FShiftIndex then
                for I := FShiftIndex - 1 downto FItemIndex do
                begin
                  FSelectItems[I] := True;
                  Inc(FSelectCount);
                end
                else for I := FShiftIndex + 1 to FItemIndex do
                begin
                  FSelectItems[I] := True;
                  Inc(FSelectCount);
                end;
              if WasSelected then
              begin
                FSelectItems[FShiftIndex] := True;
                Inc(FSelectCount);
              end;
              Invalidate;
            end
            else
            begin
              FSelectItems[FItemIndex] := True;
              FSelectCount := 1;
              FShiftIndex := FItemIndex;
              Invalidate;
            end;
          end
          else if ssCtrl in FShift then
          begin
            if FSelectItems[FItemIndex] then
              Dec(FSelectCount);
            FSelectItems[FItemIndex] := not FSelectItems[FItemIndex];
            if FSelectItems[FItemIndex] then
              Inc(FSelectCount);
            if FSelectCount > 1 then
              Invalidate;
          end
          else if (PriorIndex > -1) and (FSelectCount = 1) and FSelectItems[FItemIndex] then
          begin
            FSelectItems[PriorIndex] := False;
            FSelectItems[FItemIndex] := True;
          end
          else
          begin
            FillChar(Pointer(@FSelectItems[0])^, FCount, #0);
            FSelectCount := 1;
            FSelectItems[FItemIndex] := True;
            Invalidate;
          end;
      end;
      if not (ssShift in FShift) then
        FShiftIndex := FItemIndex;
    end;
  end
  else if FMultiSelect and (FItemIndex > -1) and (ssCtrl in FShift) then
  begin
    if FSelectItems[FItemIndex] then
      Dec(FSelectCount);
    FSelectItems[FItemIndex] := not FSelectItems[FItemIndex];
    if FSelectItems[FItemIndex] then
      Inc(FSelectCount);
    if FSelectCount > 1 then
      Invalidate
    else
      InvalidateItem(FItemIndex);
  end;
  FShift := FShift - [ssCtrl];
end;

procedure TScrollList.SetScrolling(Value: Boolean);
begin
  if Value <> FScrolling then
  begin
    FScrolling := Value;
    if FScrolling then
      SetTimer(Handle, ScrollTimer, 60, nil);
  end;
end;

function TScrollList.GetSelected(Index: Integer): Boolean;
begin
  Result := False;
  if (not FMultiSelect) or (Index < 0) or (Index > FCount -1) then
    Exit;
  Result := FSelectItems[Index];
end;

procedure TScrollList.SetSelected(Index: Integer; Value: Boolean);
begin
  if (not FMultiSelect) or (Index < 0) or (Index > FCount -1) then
    Exit;
  if FSelectItems[Index] <> Value then
  begin
    FShift := [];
    if FSelectItems[Index] then
      Dec(FSelectCount);
    FSelectItems[Index] := Value;
    if FSelectItems[Index] then
      Inc(FSelectCount);
    Invalidate;
  end;
end;

procedure TScrollList.SetTopIndex(Value: Integer);
var
  ScrollInfo: TScrollInfo;
  Delta: Integer;
  P: TPoint;
begin
  if Value > FCount - ClientHeight div FItemHeight then
    Value := FCount - ClientHeight div FItemHeight;
  if Value < 0 then
    Value := 0;
  if Value <> FTopIndex then
  begin
    Delta := (FTopIndex - Value) * FItemHeight;
    FTopIndex := Value;
    if FHotTrack then
      if FHotIndex > - 1 then
        InvalidateItem(FHotIndex);
    FHotIndex := -1;
    ScrollInfo.cbSize := Sizeof(TScrollInfo);
    ScrollInfo.fMask := SIF_POS;
    ScrollInfo.nPos := FTopIndex;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    Scroll(Delta);
    InvalidateItem(FItemIndex);
    if FHotTrack then
    begin
      P := ScreenToClient(Mouse.CursorPos);
      FHotIndex := ItemAtPos(P, False);
      if FHotIndex > -1 then
        if PtInRect(ItemRect(FHotIndex), P) then
          InvalidateItem(FHotIndex)
        else
          FHotIndex := -1;
    end;
  end;
end;

{ TCustomDrawList }

constructor TCustomDrawList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoScroll := True;
end;

procedure TCustomDrawList.Paint;
begin
  DrawBackground;
  inherited Paint;
end;

procedure TCustomDrawList.DrawBackground;
begin
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self);
end;

procedure TCustomDrawList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Canvas, Index, Rect, State);
end;

procedure TCustomDrawList.Scroll(Delta: Integer);
var
  Rect: TRect;
begin
  if FAutoScroll then
    inherited Scroll(Delta)
  else
  begin
    Rect := ClientRect;
    InvalidateRect(Handle, @Rect, False);
  end;
end;

procedure TCustomDrawList.SetAutoScroll(Value: Boolean);
begin
  if Value <> FAutoScroll then
  begin
    FAutoScroll := Value;
    DoubleBuffered := not Value;
  end;
end;

{ TDrawTextList }

constructor TDrawTextList.Create(AOwner: TComponent);
var
  S: TStringList;
begin
  inherited Create(AOwner);
  S := TStringList.Create;
  S.OnChange := ItemsChange;
  FAutoScroll := False;
  FItems := S;
end;

destructor TDrawTextList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TDrawTextList.ItemsChange(Sender: TObject);
begin
  Count := FItems.Count;
  Invalidate;
end;

procedure TDrawTextList.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
  Invalidate;
end;

{ TCustomBubbleList }

constructor TCustomBubbleList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  HotTrack := True;
  InsideRect := True;
  FDownIndex := -1;
end;

procedure TCustomBubbleList.Click;
begin
  if FClicked then inherited Click;
end;

procedure TCustomBubbleList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style or CS_HREDRAW;
end;

procedure TCustomBubbleList.DrawBackground;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
end;

procedure TCustomBubbleList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  Brush: HBRUSH;
  DC: HDC;
begin
  DC := Canvas.Handle;
  Brush := GetBrush(Color);
  FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
  State := State + [dsFlat];
  if not Enabled then
    State := State + [dsDisabled];
  if Index = HotIndex then
  begin
    State := State + [dsHot];
    if Index = DownIndex then
      State := State + [dsPressed];
  end;
  DrawThemeThinButton(DC, Rect, State);
  if [dsHot, dsPressed] * State = [dsHot, dsPressed] then
    OffsetRect(Rect, 1, 1);
  inherited DrawItem(Index, Rect, State);
end;

procedure TCustomBubbleList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ItemIndex > -1 then
    case Key of
      VK_RETURN:
        try
          FClicked := True;
          Click;
        finally
          FClicked := False;
        end;
      VK_SPACE:
        if not FKeyDown then
        begin
          FKeyDown := True;
          FDownIndex := ItemIndex;
          if FDownIndex > -1 then
            InvalidateItem(ItemIndex);
        end;
    end;
end;

procedure TCustomBubbleList.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if ItemIndex > -1 then
    case Key of
      VK_SPACE:
        if FKeyDown then
        try
          FKeyDown := False;
          FDownIndex := -1;
          ReleaseCapture;
          InvalidateItem(ItemIndex);
          FClicked := True;
          Click;
        finally
          FClicked := False;
        end;
    end;
end;

procedure TCustomBubbleList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    FLocked := True;
end;

procedure TCustomBubbleList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PriorIndex: Integer;
begin
  {FHotIndex := ItemAtPos(Point(X, Y), True, True);
  if (Button = mbLeft) and (FDownIndex > -1)  then
  begin
    FMouseDisabled := False;
    PriorIndex := FDownIndex;
    FDownIndex := -1;
    InvalidateItem(PriorIndex);
    UpdateWindow(Handle);
    if PriorIndex = FHotIndex then
    try
      FClicked := True;
      Click;
    finally
      FClicked := False;
    end;
  end;}
  PriorIndex := DownIndex;
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FLocked := False;
    if (PriorIndex > -1) and (PriorIndex = HotIndex) then
    try
      InvalidateItem(PriorIndex);
      UpdateWindow(Handle);
      FClicked := True;
      Click;
    finally
      FClicked := False;
    end;
  end;
end;

procedure TCustomBubbleList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomBubbleList.SelectItem(PriorIndex: Integer; NewIndex: Integer;
  var CanSelect: Boolean);
begin
  inherited SelectItem(PriorIndex, NewIndex, CanSelect);
end;

procedure TCustomBubbleList.WMLButtonUp(var Message: TWMLButtonUp);
var
  Point: TPoint;
begin
  Point := SmallPointToPoint(Message.Pos);
  inherited;
end;

procedure TCustomBubbleList.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

end.
