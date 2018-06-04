
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit BtnCtrls;

interface

{$I CODEBOT.INC}

uses
  Classes, ImgList, Controls, Windows, Menus, Forms, Messages, SysUtils,
  StdCtrls, ActnList, Graphics, BaseTypes, GraphTools, ProviderTools,
  {$IFNDEF LITE}ScrollCtrls, PopCtrls, {$ENDIF} FormTools;

{ TImageSpeedButton class }

type
  TDrawEvent = procedure(Control: TControl; Rect: TRect;
    DrawState: TDrawState; var DefaultDraw: Boolean) of object;

  TButtonStyle = (bsFramed, bsBeveled, bsFlat, bsTransparent);
  TPressedState = (psNone, psButtonDown, psMenuButtonDown);

{ Button options described

    boAutoFocus: Button will focus itself when pressed
    boAutoPopup: Popumenu will be automatically shown when the menu was clicked
    boAutoSize: Button will size itself based on caption, image, and padding
    boClean: Button will not display 3D elemments on its face
    boToggle: Clicking the button toggles its down state
    boGroup: Button uses the parent control to toggle down state
    boMenu: Button has a clickable down arrow icon,
    boSideways: When boAutoPopup is set the popup menu will appear to the side
    boLocked: Clicking the menu arrow clicks the entire button
    boWide: Button is the same width as if boMenu were set
    boOpaque: Images are drawn fully opaque }

  TButtonOption = (boAutoFocus, boAutoPopup, boAutoSize, boClean, boToggle, boGrouped, boMenu,
    boSideways, boLocked, boWide, boOpaque);

  TButtonOptions = set of TButtonOption;

  TImageSpeedButton = class(TProviderGraphicControl)
  private
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageChangeLink: TChangeLink;
    FPressed: Boolean;
    FHot: Boolean;
    FDown: Boolean;
    FToggle: Boolean;
    FOnDrawButton: TDrawEvent;
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
    procedure ImageListChange(Sender: TObject);
    procedure SetDown(Value: Boolean);
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DrawButton(const Rect: TRect; State: TDrawState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Align;
    property Anchors;
    property Color;
    property Cursor;
    property Visible;
    property Enabled;
    property Hint;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Down: Boolean read FDown write SetDown;
    property Toggle: Boolean read FToggle write FToggle;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnDrawButton: TDrawEvent read FOnDrawButton write FOnDrawButton;
  end;

implementation

{ TImageSpeedButton }

constructor TImageSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csParentBackground, csDoubleClicks]; //  
  Width := 24;
  Height := 24;
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TImageSpeedButton.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TImageSpeedButton.ImageListChange(Sender: TObject);
begin
  Repaint;
end;

procedure TImageSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FPressed := True;
    Repaint;
  end;
end;

procedure TImageSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    P := FPressed;
    FPressed := False;
    if P and PtInRect(ClientRect, Point(X, Y)) then
    begin
      if FToggle then
        FDown := not FDown;
      Repaint;
      Click;
    end
    else
      Repaint;
  end;
end;

procedure TImageSpeedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

procedure TImageSpeedButton.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    AdjustSize;
    Repaint;
  end;
end;

procedure TImageSpeedButton.SetImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Repaint;
  end;
end;

procedure TImageSpeedButton.SetDown(Value: Boolean);
begin
  if Value <> FDown then
  begin
    FDown := Value;
    Repaint;
  end;
end;

procedure TImageSpeedButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FHot := True;
  Repaint;
end;

procedure TImageSpeedButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FHot := False;
  Repaint;
end;

procedure TImageSpeedButton.DrawButton(const Rect: TRect; State: TDrawState);
var
  DefaultDraw: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    if dsChecked in State then
      DrawRectOutline(Canvas.Handle, Rect, clBlack)
    else
      DrawRectOutline(Canvas.Handle, Rect, cl3DDkShadow);
  end
  else
  begin
    DefaultDraw := True;
    if Assigned(FOnDrawButton) then
      FOnDrawButton(Self, Rect, State, DefaultDraw);
    if DefaultDraw then
      DrawThemeThinButton(Canvas.Handle, Rect, State);
  end;
end;

procedure TImageSpeedButton.Paint;
var
  S: TDrawState;
  I, W, H: Integer;
begin
  inherited Paint;
  S := [];
  if not Enabled then
    Include(S, dsDisabled);
  if FDown then
    Include(S, dsChecked);
  if FHot then
  begin
    Include(S, dsHot);
    if FPressed then
      Include(S, dsPressed);
  end;
  DrawButton(ClientRect, S);
  if FImages <> nil then
  begin
    I := FImages.Height;
    W := Width;
    H := Height;
    ImageListDraw(FImages, Canvas, (W - I) div 2, (H - I) div 2, FImageIndex, Enabled, FHot);
  end;
end;

end.
