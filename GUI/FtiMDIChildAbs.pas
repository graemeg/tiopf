unit FtiMDIChildAbs;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TFormMDIChildAbs = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses
  tiUtils
  ,tiRegINI
 ;

{$R *.DFM}

procedure TFormMDIChildAbs.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormMDIChildAbs.FormCreate(Sender: TObject);
begin
  gReg.ReadFormState(self);
end;

procedure TFormMDIChildAbs.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState(self);
end;

end.
