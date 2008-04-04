unit FEAdrsEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons, tiPerAwareCtrls, ExtCtrls,
  tiFocusPanel, tiObject, FAdrsAbs;

type
  TFormEAdrsEdit = class(TFormAdrsAbs)
    paeEAdrsText: TtiPerAwareEdit;
    paeEAdrsType: TtiPerAwareComboBoxStatic;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetData(const AValue: TtiObject); override;
  public
    { Public declarations }
  end;

implementation
uses
  Adrs_Singleton;

{$R *.dfm}

procedure TFormEAdrsEdit.FormCreate(Sender: TObject);
begin
  inherited;
  GAdrsBook.EAdrsTypeList.AssignCaptions(paeEAdrsType.Items);
end;

procedure TFormEAdrsEdit.SetData(const AValue: TtiObject);
begin
  inherited;
  paeEAdrsType.LinkToData(DataBuffer, 'AdrsTypeAsString');
  paeEAdrsText.LinkToData(DataBuffer, 'Text');
end;

end.
