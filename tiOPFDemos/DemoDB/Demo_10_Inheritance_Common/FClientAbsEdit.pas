unit FClientAbsEdit;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, tiReadOnly, StdCtrls, Buttons, tiMemoReadOnly,
  ExtCtrls, tiFocusPanel, tiPerAwareCtrls, tiPtnVisPerObj
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,Client_BOM
  ;

type
  TFormClientAbsEdit = class(TFormTIPerEditDialog)
    paeOID: TtiPerAwareEdit;
    paeClientID: TtiPerAwareEdit;
    memoErrors: TtiMemoReadOnly;
    Bevel1: TBevel;
  private
  protected
    procedure SetData(const Value: TPerObjAbs); override ;
    function  FormIsValid : boolean ; override ;
  public
  end;

function EditClient( const pClient : TClientAbs ) : boolean ;

implementation
uses
   FClientCompanyEdit
  ,FClientPersonEdit
  ,tiUtils
  ;

{$R *.dfm}

// This would be better as a factory with the registration code in each
// of the units that contain the concrete classes
function EditClient( const pClient : TClientAbs ) : boolean ;
begin
  result := false ;
  if pClient is TClientCompany then
    result := TFormClientCompanyEdit.Execute(pClient)
  else if pClient is TClientPerson then
    result := TFormClientPersonEdit.Execute(pClient)
  else
    tiFmtException('Invalid class type < ' +
                   pClient.ClassName + '>',
                   '', 'EditClient');
end ;

{ TFormClientEdit }

function TFormClientAbsEdit.FormIsValid: boolean;
begin
  result := Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TFormClientAbsEdit.SetData(const Value: TPerObjAbs);
begin
  inherited;
  paeOID.Value := DataBuffer.OID.AsString ;
  paeClientID.LinkToData(DataBuffer,'ClientID');
end;

end.
 
