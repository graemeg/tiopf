unit FPhoneNumberEdit;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, tiReadOnly, StdCtrls, Buttons, tiMemoReadOnly,
  ExtCtrls, tiFocusPanel, tiPerAwareCtrls, tiObject
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
 ;

type
  TFormPhoneNumberEdit = class(TFormTIPerEditDialog)
    paeOID: TtiPerAwareEdit;
    paeNumberType: TtiPerAwareEdit;
    paeNumberText: TtiPerAwareEdit;
    memoErrors: TtiMemoReadOnly;
  private
  protected
    procedure SetData(const Value: TtiObject); override;
    function  FormIsValid: boolean; override;
  public
  end;

implementation

{$R *.dfm}

{ TFormClientEdit }

function TFormPhoneNumberEdit.FormIsValid: boolean;
begin
  result:= Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TFormPhoneNumberEdit.SetData(const Value: TtiObject);
begin
  inherited;
  paeOID.Value:= DataBuffer.OID.AsString;
  paeNumberType.LinkToData(DataBuffer, 'NumberType');
  paeNumberText.LinkToData(DataBuffer,'NumberText');
end;

end.
 
