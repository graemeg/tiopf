unit tiAnimatedGIF;

{
A customised TImage for specific animated GIF support.
Features:
- AnimationEnabled property:
  Turns the animation on or off (swaps between displaying an
  "enabled" animated GIF image and a static "disabled" GIF image).
- ResourceName property:
  Two GIF images are loaded from the application resources, one
  for the animation disabled image (a static GIF image) and one
  for the animation enabled image (an animated GIF image). The
  GIF images can be added to the application by creating an RC
  file with the two GIF files added in the RCDATA format and then
  adding the RC file to the project.
  NOTE: "_16D" is appended to the specified ResourceName to find
  the disabled image and "_16N" is appended to find the enabled image.
- OnHint property:
  Event handler is called when the hint is about to be shown. The
  event handler can change the hint message to be displayed.
- GIF and animated GIF support comes from the third-party TGifImage.
}

interface

uses
  // Delphi units
  Windows, Messages, SysUtils, Classes, ExtCtrls, Forms, Controls,
  // TGIFImage units
  GIFImage
 ;

type

  TtiHintShowEvent = procedure(Sender: TObject; var HintMessage: string) of object;

  TtiAnimatedGIF = class(TImage)
  private
    FResourceName: string;
    FAnimationEnabled: Boolean;
    FAnimationSpeed: Integer;
    FOnHint: TtiHintShowEvent;
    procedure UpdateImage;
    procedure SetResourceName(const AValue: string);
    procedure SetAnimationEnabled(const AValue: Boolean);
    procedure SetAnimationSpeed(const AValue: Integer);
  protected
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  public
    constructor Create(AOwner: TComponent); override;
    property ResourceName: string read FResourceName write SetResourceName;
    property AnimationEnabled: Boolean read FAnimationEnabled write SetAnimationEnabled;
    property AnimationSpeed: Integer read FAnimationSpeed write SetAnimationSpeed; // % speed 0-1000
    property OnHint: TtiHintShowEvent read FOnHint write FOnHint;
  end;

implementation

uses
   tiImageMgr
 ;

{ TtiAnimatedGIF }

constructor TtiAnimatedGIF.Create(AOwner: TComponent);
begin
  inherited;
  FResourceName := '';
  FAnimationEnabled := False;
  FAnimationSpeed := 100;
  FOnHint := nil;
end;

procedure TtiAnimatedGIF.SetAnimationEnabled(const AValue: Boolean);
begin
  if AValue <> FAnimationEnabled then begin
    FAnimationEnabled := AValue;
    UpdateImage;
  end;
end;

procedure TtiAnimatedGIF.SetAnimationSpeed(const AValue: Integer);
begin
  if AValue <> FAnimationSpeed then begin
    FAnimationSpeed := AValue;
    if Self.Picture.Graphic is TGIFImage then
      (Self.Picture.Graphic as TGIFImage).AnimationSpeed := FAnimationSpeed;
  end;
end;

procedure TtiAnimatedGIF.SetResourceName(const AValue: string);
begin
  if AValue <> FResourceName then begin
    FResourceName := AValue;
    UpdateImage;
  end;
end;

procedure TtiAnimatedGIF.UpdateImage;
var
  GIFImage: TGIFImage;
  ResourceNameSuffix: string;
  ResourceStream: TResourceStream;
begin
  if FResourceName <> '' then begin
    // Get the appropriate (animation enabled / disabled) GIF image from resource.
    GifImage := TGifImage.Create;
    try
      ResourceNameSuffix := '_' + ctiImageSizes[tiIS16];
      if FAnimationEnabled then
        ResourceNameSuffix := ResourceNameSuffix + ctiImageStates[tiISNormal]
      else
        ResourceNameSuffix := ResourceNameSuffix + ctiImageStates[tiISDisabled];

      //GIFImage.LoadFromResourceName(MainInstance, FResourceName + ResourceNameSuffix)
      ResourceStream := TResourceStream.Create(MainInstance, FResourceName + ResourceNameSuffix, RT_RCDATA);
      try
        GIFImage.LoadFromStream(ResourceStream);
      finally
        FreeAndNil(ResourceStream);
      end;

      Self.Picture.Assign(GIFImage);
      (Self.Picture.Graphic as TGIFImage).AnimationSpeed := FAnimationSpeed;
    finally
      FreeAndNil(GIFImage);
    end;
  end;
end;

procedure TtiAnimatedGIF.CMHintShow(var Message: TCMHintShow);
var
  HintMessage: string;
begin
  if Assigned(FOnHint) then begin
    // User can change the hint message.
    HintMessage := Hint;
    FOnHint(Self, HintMessage);
    Message.HintInfo.HintStr := HintMessage;
    // Disable hint if blank
    if HintMessage = '' then
      Message.Msg := 0;
  end;
end;

end.
