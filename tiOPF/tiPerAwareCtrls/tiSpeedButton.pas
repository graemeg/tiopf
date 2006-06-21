{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSpeedButton.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

2000-02-28: Last Modified
2004-01-20: Peter Hinrichsen added DisabledGlyph

-----------------------------------------------------------------------------}

unit tiSpeedButton;

interface

uses
  Classes
  ,Messages
  ,Graphics
  ,Controls
  ,Buttons
  ,tiImageMgr
  ,tiResources
  ;

type

  TtiSpeedButton = class(TSpeedButton)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOver: Boolean;
    FGlyphHot: TBitmap;
    FGlyphNormal: TBitmap;
    FGlyphDisabled: TBitmap;
    FImageResName: TtiImageRes;
    procedure SetGlyphHot(Value: TBitmap);
    procedure SetGlyphDisabled(const Value: TBitmap);
    procedure SetImageResName(const Value: TtiImageRes);
    procedure ClearGlyph;
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure SetEnabled(Value: Boolean); override ;
    procedure ShowCorrectGlyph ;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    ImageRes : TtiImageRes read FImageResName write SetImageResName ;
    property    GlyphHot: TBitmap read FGlyphHot write SetGlyphHot;
    property    GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
    property    OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property    OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

constructor TtiSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOver          := False;
  FGlyphHot      := TBitmap.Create;
  FGlyphNormal   := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
  Flat           := True ;
end;

destructor TtiSpeedButton.Destroy;
begin
  FGlyphHot.Free;
  FGlyphNormal.Free;
  FGlyphDisabled.Free;
  inherited Destroy;
end;

procedure TtiSpeedButton.SetGlyphHot(Value: TBitmap);
begin
  FGlyphHot.Assign(Value);
end;

procedure TtiSpeedButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FOver := True;
    ShowCorrectGlyph ;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TtiSpeedButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if FOver then
  begin
    FOver := False;
    ShowCorrectGlyph ;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TtiSpeedButton.SetEnabled(Value: Boolean);
begin
  inherited;
  // for D7...
  if csDesigning in ComponentState then
    Exit;

  FOver := True;
  ShowCorrectGlyph ;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);

end;

procedure TtiSpeedButton.ShowCorrectGlyph;
begin
  if FGlyphNormal.Empty and (not Glyph.Empty) then
    FGlyphNormal.Assign(Glyph) ;

  if ( Not Enabled ) and ( not FGlyphDisabled.Empty ) then
  begin
    Glyph.FreeImage ;
    Glyph.Height := FGlyphDisabled.Height ;
    Glyph.Width  := FGlyphDisabled.Width * 2 ;
    Glyph.Canvas.Draw(FGlyphDisabled.Width,0,FGlyphDisabled);
    NumGlyphs := 2 ;
  end
  else if FOver then
  begin
    Glyph.Assign(FGlyphHot);
    NumGlyphs := 1 ;
  end else
  begin
    Glyph.Assign(FGlyphNormal);
    NumGlyphs := 1 ;
  end ;
end;               

procedure TtiSpeedButton.SetGlyphDisabled(const Value: TBitmap);
begin
  FGlyphDisabled.Assign(Value);
end;

procedure TtiSpeedButton.SetImageResName(const Value: TtiImageRes);
var
  lResName : string ;
  lImageIndex : integer ;
begin
  FImageResName := Value;
  lResName := cImageRes[FImageResName];
  if FImageResName = tiRINone then
    ClearGlyph 
  else
  begin
    lImageIndex := gTIImageListMgr.ImageIndex16(lResName);
    if lImageIndex = -1 then
      SetImageResName(tiRINone)
    else
    begin
      ClearGlyph ;
      gTIImageListMgr.ILNormal16.GetBitmap(lImageIndex, Glyph ) ;
      gTIImageListMgr.ILHot16.GetBitmap(lImageIndex, GlyphHot ) ;
      gTIImageListMgr.ILDisabled16.GetBitmap(lImageIndex, GlyphDisabled ) ;
    end ;
  end;
end;

procedure TtiSpeedButton.ClearGlyph;
begin
  Glyph.Assign(nil);
  GlyphHot.Assign(nil);
  GlyphDisabled.Assign(nil);
end ;

end.

