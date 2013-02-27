unit FClientEdit;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FtiPerEditDialog, tiReadOnly, StdCtrls, Buttons, tiMemoReadOnly,
  ExtCtrls, tiFocusPanel, tiPerAwareCtrls, tiPtnVisPerObj
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ;

type
  TFormClientEdit = class(TFormTIPerEditDialog)
    gbClient: TGroupBox;
    gbAddress: TGroupBox;
    paeOID: TtiPerAwareEdit;
    paeClientName: TtiPerAwareEdit;
    paeAdrsText: TtiPerAwareMemo;
    paeLocality: TtiPerAwareEdit;
    paeState: TtiPerAwareEdit;
    paePostCode: TtiPerAwareEdit;
    memoErrors: TtiMemoReadOnly;
    procedure paeAdrsTextKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure SetData(const Value: TPerObjAbs); override ;
    function  FormIsValid : boolean ; override ;
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
  result := Databuffer.IsValid(MemoErrors.Lines);
end;

procedure TFormClientEdit.SetData(const Value: TPerObjAbs);
var
  lAdrs : TAdrs ;
begin
  inherited;
  // The Client object
  paeOID.Value := DataBuffer.OID.AsString ;
  paeClientName.LinkToData(DataBuffer, 'ClientName');

  // The Adrs object, owned by the client
  lAdrs := ( DataBuffer as TClient ).Adrs ;
  paeAdrsText.LinkToData( lAdrs, 'AdrsText' ) ;
  paeLocality.LinkToData( lAdrs, 'Locality' ) ;
  paeState.LinkToData( lAdrs, 'State'    ) ;
  paePostCode.LinkToData( lAdrs, 'PostCode' ) ;
end;

procedure TFormClientEdit.paeAdrsTextKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  Key := UpperCase(Key)[1];
end;

procedure TFormClientEdit.FormCreate(Sender: TObject);
begin
  inherited;
  paeAdrsText.MaxLength := High(TAdrsText);
  paeLocality.MaxLength := High(TAdrsLocality);
  paeState.MaxLength    := High(TAdrsState);
  paePostCode.MaxLength := High(TAdrsPostCode);
end;

end.
 
