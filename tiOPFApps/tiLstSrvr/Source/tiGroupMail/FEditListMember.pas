unit FEditListMember;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FtiPerEditDialog, tiReadOnly, StdCtrls, Buttons, ExtCtrls,
  tiPerAwareCtrls, tiPtnVisPerObj, tiFocusPanel;

type
  TFormEditListMember = class(TFormTIPerEditDialog)
    paeEMailAddress: TtiPerAwareEdit;
    procedure FormShow(Sender: TObject);
  private
  protected
    procedure SetData(const Value: TPerObjAbs); override ;
    function  FormIsValid : boolean ; override ;
  public
  end;

implementation
uses
  tiUtils
  ;

{$R *.DFM}

{ TFormEditListMember }

procedure TFormEditListMember.SetData(const Value: TPerObjAbs);
begin
  inherited SetData( Value ) ;
  paeEMailAddress.LinkToData( Databuffer, 'EMailAddress' ) ;
end;

procedure TFormEditListMember.FormShow(Sender: TObject);
begin
  inherited;
  paeEMailAddress.SetFocus ;
end;

function TFormEditListMember.FormIsValid: boolean;
begin
  result := tiIsEMailAddressValid( paeEMailAddress.Value ) ;
end;

end.
