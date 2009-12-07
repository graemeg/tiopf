unit FClientEdit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LResources,
  Dialogs, StdCtrls, Buttons, ExtCtrls, tiObject;

type
  TFormClientEdit = class(TForm)
    paeOID: TEdit;
    paeClientName: TEdit;
    paeClientID: TEdit;
    memoErrors: TMemo;
  private
  protected
    procedure SetData(const Value: TtiObject);
    function  FormIsValid: boolean;
  public
  end;

implementation


{ TFormClientEdit }

function TFormClientEdit.FormIsValid: boolean;
begin
  //result:= Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TFormClientEdit.SetData(const Value: TtiObject);
begin
  inherited;
  //paeOID.Value:= DataBuffer.OID.AsString;
  //paeClientName.LinkToData(DataBuffer, 'ClientName');
  //paeClientID.LinkToData(DataBuffer,'ClientID');
end;

initialization
  {$I FClientEdit.lrs}

end.
 
