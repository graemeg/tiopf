unit FClientCompanyEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FClientAbsEdit, tiReadOnly, StdCtrls, tiMemoReadOnly, ExtCtrls,
  tiFocusPanel, tiPerAwareCtrls, Buttons, Variants, tiObject;

type
  TFormClientCompanyEdit = class(TFormClientAbsEdit)
    paeCompanyName: TtiPerAwareEdit;
  private
    { Private declarations }
  public
    procedure SetData(const Value: TtiObject); override;
  end;

implementation
uses
  Client_BOM
 ;
  
{$R *.dfm}

{ TFormClientCompanyEdit }

procedure TFormClientCompanyEdit.SetData(const Value: TtiObject);
begin
  inherited;
  paeCompanyName.LinkToData(DataBuffer,'CompanyName');
end;

end.
