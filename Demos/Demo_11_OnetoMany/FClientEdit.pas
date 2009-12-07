unit FClientEdit;

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
  TFormClientEdit = class(TFormTIPerEditDialog)
    paeOID: TtiPerAwareEdit;
    paeClientName: TtiPerAwareEdit;
    paeClientID: TtiPerAwareEdit;
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

function TFormClientEdit.FormIsValid: boolean;
begin
  result:= Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TFormClientEdit.SetData(const Value: TtiObject);
begin
  inherited;
  paeOID.Value:= DataBuffer.OID.AsString;
  paeClientName.LinkToData(DataBuffer, 'ClientName');
  paeClientID.LinkToData(DataBuffer,'ClientID');
end;

end.
 
