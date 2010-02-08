unit FClientEdit;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, tiReadOnly, StdCtrls, Buttons, tiMemoReadOnly,
  ExtCtrls, tiFocusPanel, tiPerAwareCtrls, tiObject, Variants;

type
  TFormClientEdit = class(TFormTIPerEditDialog)
    paeOID: TtiPerAwareEdit;
    paeClientName: TtiPerAwareEdit;
    memoErrors: TtiMemoReadOnly;
    paeClientSource: TtiPerAwareComboBoxDynamic;
    procedure FormCreate(Sender: TObject);
    procedure paeClientSourceChange(Sender: TObject);
  private
  protected
    procedure SetData(const Value: TtiObject); override;
    function  FormIsValid: boolean; override;
  public
  end;

implementation
uses
  Client_BOM
 ;

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
  paeClientSource.Value:= TClient(DataBuffer).ClientSource;
end;

procedure TFormClientEdit.FormCreate(Sender: TObject);
begin
  inherited;
  paeClientSource.List:= gClientSources.List;
end;

procedure TFormClientEdit.paeClientSourceChange(Sender: TObject);
begin
  inherited;
  TClient(DataBuffer).ClientSource:= paeClientSource.Value as TClientSource;
end;

end.

