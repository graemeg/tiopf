unit contact_views;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  tiBaseMediator,
  fpg_panel;

type
  { A specialized mediator for TContact.Photo property }
  TContactPhotoMediator = class(TtiMediatorView)
  protected
    procedure   DoObjectToGUI; override;
    procedure   SetupGUIandObject; override;
  public
    constructor Create; override;
    function    View: TfpgImagePanel; reintroduce;
    class function ComponentClass: TClass; override;
  end;


implementation

uses
  fpg_main,
  fpg_imgfmt_png,
  model;


{ TContactPhotoMediator }

procedure TContactPhotoMediator.DoObjectToGUI;
var
  img: TfpgImage;
  c: TContact;
begin
  // Don't call inherited DoObjectToGUI
  CheckFieldNames;
  c := Subject as TContact;
  img := LoadImage_PNG(c.Photo);
  View.Image := img;
end;

procedure TContactPhotoMediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.OwnsImage := True;
end;

constructor TContactPhotoMediator.Create;
begin
  inherited Create;
  GuiFieldName := 'Image';
end;

function TContactPhotoMediator.View: TfpgImagePanel;
begin
  Result := TfpgImagePanel(inherited View);
end;

class function TContactPhotoMediator.ComponentClass: TClass;
begin
  Result := TfpgImagePanel;
end;

end.

