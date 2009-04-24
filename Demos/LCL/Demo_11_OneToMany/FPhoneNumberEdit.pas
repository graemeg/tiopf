unit FPhoneNumberEdit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LResources,
  Dialogs, StdCtrls, Buttons, ExtCtrls, tiObject;

type
  TFormPhoneNumberEdit = class(TForm)
    paeOID: TEdit;
    paeNumberType: TEdit;
    paeNumberText: TEdit;
    memoErrors: TMemo;
  private
  protected
    procedure SetData(const Value: TtiObject);
    function  FormIsValid: boolean;
  public
  end;

implementation

{ TFormClientEdit }

function TFormPhoneNumberEdit.FormIsValid: boolean;
begin
  //result:= Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TFormPhoneNumberEdit.SetData(const Value: TtiObject);
begin
  inherited;
  //paeOID.Value:= DataBuffer.OID.AsString;
  //paeNumberType.LinkToData(DataBuffer, 'NumberType');
  //paeNumberText.LinkToData(DataBuffer,'NumberText');
end;

initialization
  {$I FPhoneNumberEdit.lrs}

end.
 
