unit FFileNameFilterEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FtiPerEditDialog, tiReadOnly, StdCtrls, Buttons, tiPerAwareCtrls,
  ExtCtrls, tiPtnVisPerObj, tiFocusPanel ;

type
  TFormFileNameFilterEdit = class(TFormTIPerEditDialog)
    paeFilterType: TtiPerAwareComboBoxStatic;
    paeWildCard: TtiPerAwareEdit;
    procedure FormCreate(Sender: TObject);
  private
  protected
    function  FormIsValid : boolean ; override;
  public
    procedure SetData(const Value: TPerObjAbs); override ;
  end;


implementation
uses
  tiFileSyncSetup_BOM
  ;

{$R *.DFM}

{ TFormTIPerEditDialog1 }

procedure TFormFileNameFilterEdit.SetData(const Value: TPerObjAbs);
begin
  inherited SetData( Value ) ;
  paeFilterType.LinkToData( DataBuffer, 'FilterTypeAsString'     ) ;
  paeWildCard.LinkToData(   DataBuffer, 'WildCard'    ) ;
end;

procedure TFormFileNameFilterEdit.FormCreate(Sender: TObject);
var
  i : TtiFileFilterType ;
begin
  inherited;
  paeFilterType.Items.Clear ;
  for i := Low( TtiFileFilterType ) to High( TtiFileFilterType ) do
    paeFilterType.Items.Add( caFilterTypes[i] )
end;

function TFormFileNameFilterEdit.FormIsValid: boolean;
begin
  result :=
    ( not SameText( paeFilterType.Value, caFilterTypes[fftUnassigned] )) and
    ( paeWildCard.Value <> '' ) ;
end;

end.
