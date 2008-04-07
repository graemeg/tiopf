unit FAdrsAbs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons;

type
  TFormAdrsAbs = class(TFormTIPerEditDialog)
    lblErrors: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    function  FormIsValid : boolean; override;
  public
    { Public declarations }
  end;

implementation
uses
  tiGUIINI;

{$R *.dfm}

procedure TFormAdrsAbs.FormCreate(Sender: TObject);
begin
  inherited;
  GGUIINI.ReadFormState(Self);
end;

procedure TFormAdrsAbs.FormDestroy(Sender: TObject);
begin
  inherited;
  GGUIINI.WriteFormState(Self);
end;

function TFormAdrsAbs.FormIsValid: boolean;
var
  LS: string;
begin
  result:= DataBuffer.IsValid(LS);
  lblErrors.Caption:= LS;
  lblErrors.Visible:= LS <> '';
end;

end.
