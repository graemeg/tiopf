unit FAdrsEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, StdCtrls, Buttons, tiPerAwareCtrls, ExtCtrls,
  tiFocusPanel, tiObject, FAdrsAbs;

type
  TFormAdrsEdit = class(TFormAdrsAbs)
    paeAdrsType: TtiPerAwareComboBoxStatic;
    paeAdrsLines: TtiPerAwareMemo;
    paeSuburb: TtiPerAwareEdit;
    paeState: TtiPerAwareEdit;
    paePCode: TtiPerAwareEdit;
    paeCountry: TtiPerAwareEdit;
    procedure FormCreate(Sender: TObject);
  private

  protected
    procedure SetData(const AValue: TtiObject); override;
  public
    { Public declarations }
  end;

implementation
uses
  Adrs_Singleton;

{$R *.dfm}

procedure TFormAdrsEdit.FormCreate(Sender: TObject);
begin
  inherited;
  GAdrsBook.AdrsTypeList.AssignCaptions(paeAdrsType.Items);
end;

procedure TFormAdrsEdit.SetData(const AValue: TtiObject);
begin
  inherited;
  paeAdrsType.LinkToData(DataBuffer, 'AdrsTypeAsString');
  paeAdrsLines.LinkToData(DataBuffer, 'Lines');
  paeSuburb.LinkToData(DataBuffer, 'Suburb');
  paeState.LinkToData(DataBuffer, 'State');
  paePCode.LinkToData(DataBuffer, 'PCode');
  paeCountry.LinkToData(DataBuffer, 'Country');
end;

end.

