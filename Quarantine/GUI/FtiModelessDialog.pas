unit FtiModelessDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TtiFormModelessDialog = class(TForm)
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormDestroy(Sender: TObject); virtual;
    procedure FormShow(Sender: TObject); virtual;
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Paint; override;
  end;

implementation
uses
  tiApplicationMenuSystem;
  
const
  cPopupFormHeight = 320;
  cPopupFormWidth  = 350;

{$R *.dfm}

procedure TtiFormModelessDialog.FormActivate(Sender: TObject);
begin
  gAMS.FormMgr.SetEscapeKeyEnabled(False);
end;

procedure TtiFormModelessDialog.FormCreate(Sender: TObject);
begin
  BorderStyle := bsNone;
  BorderIcons := [];
  BorderStyle := bsNone;
  OnActivate:= FormActivate;
  OnDeActivate := FormDeactivate;
  OnShow := FormShow;
  Color := clWhite;
  Height := cPopupFormHeight;
  Width  := cPopupFormWidth;
end;

procedure TtiFormModelessDialog.FormDeactivate(Sender: TObject);
begin
  gAMS.FormMgr.SetEscapeKeyEnabled(True);
  Close;
end;

procedure TtiFormModelessDialog.FormDestroy(Sender: TObject);
begin
  // Override in inherited if needed
end;

procedure TtiFormModelessDialog.FormShow(Sender: TObject);
begin
  // Override in inherited if needed
end;

procedure TtiFormModelessDialog.Paint;
var
  lColor: TColor;
begin
  inherited;
  lColor := Canvas.Pen.Color;
  try
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  finally
    Canvas.Pen.Color := lColor;
  end;
end;

end.
