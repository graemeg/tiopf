unit FPerson;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FtiFormMgrForm, StdCtrls, ExtCtrls, tiObject, tiPerAwareCtrls,
  tiFocusPanel, tiVTListView;

type
  TFormPerson = class(TFormTIFormMgrForm)
    paeFirstName: TtiPerAwareEdit;
    paeLastName: TtiPerAwareEdit;
    paeTitle: TtiPerAwareEdit;
    paeInitials: TtiPerAwareEdit;
    paeNotes: TtiPerAwareMemo;
    LV: TtiVTListView;
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure SetData(const AValue: TtiObject); override;
  public
    { Public declarations }
  end;

implementation
uses
  tiApplicationMenuSystem
  ,Adrs_BOM
 ;

{$R *.dfm}

{ TFormPerson }

procedure TFormPerson.FormCreate(Sender: TObject);
begin
  inherited;
  ButtonsVisible:= btnVisReadWrite;
  LV.AddColumn('AdrsType', vttkString, 'Address type', 80);
  LV.AddColumn('AdrsText', vttkString, 'Address type', 80);

end;

procedure TFormPerson.SetData(const AValue: TtiObject);
begin
  inherited;
  paeFirstName.LinkToData(DataBuffer, 'FirstName');
  paeLastName.LinkToData(DataBuffer, 'LastName');
  paeTitle.LinkToData(DataBuffer, 'Title');
  paeInitials.LinkToData(DataBuffer, 'Initials');
  paeNotes.LinkToData(DataBuffer, 'Notes');
  LV.Data:= (DataBuffer as TPerson).EAdrsList;

end;

end.
