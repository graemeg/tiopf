unit FClientCompanyEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FClientAbsEdit, tiReadOnly, StdCtrls, tiMemoReadOnly, ExtCtrls,
  tiFocusPanel, tiPerAwareCtrls, Buttons
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiPtnVisPerObj
  ;

type
  TFormClientCompanyEdit = class(TFormClientAbsEdit)
    paeCompanyName: TtiPerAwareEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetData(const Value: TPerObjAbs); override ;
  end;

implementation
uses
  Client_BOM
  ;
  
{$R *.dfm}

{ TFormClientCompanyEdit }

procedure TFormClientCompanyEdit.SetData(const Value: TPerObjAbs);
begin
  inherited;
  paeCompanyName.LinkToData(DataBuffer,'CompanyName');
end;

procedure TFormClientCompanyEdit.FormCreate(Sender: TObject);
begin
  inherited;
  paeCompanyName.MaxLength := High( TCompanyName );
end;

end.
