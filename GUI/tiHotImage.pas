unit tiHotImage;

{$I tiDefines.inc}

interface
uses
   Classes
  ,StdCtrls
  ,Graphics
  ,Messages
  ,Controls
  ,ExtCtrls
  {$IFDEF FPC}
  ,LMessages
  {$ENDIF}
 ;

type

  TtiHotImage = class(TImage)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    property  OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property  OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

{ TtiHotImage }

procedure TtiHotImage.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TtiHotImage.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

end.

