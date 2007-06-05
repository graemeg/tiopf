unit FClientPersonEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FClientAbsEdit, tiReadOnly, StdCtrls, tiMemoReadOnly, ExtCtrls,
  tiFocusPanel, tiPerAwareCtrls, Buttons
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiObject
  ;
  

type
  TFormClientPersonEdit = class(TFormClientAbsEdit)
    paeGivenName: TtiPerAwareEdit;
    paeFamilyName: TtiPerAwareEdit;
  private
    { Private declarations }
  public
    procedure SetData(const Value: TtiObject); override ;
  end;

implementation
uses
  Client_BOM
  ;
  
{$R *.dfm}

{ TFormClientPersonEdit }

procedure TFormClientPersonEdit.SetData(const Value: TtiObject);
begin
  inherited;
  paeGivenName.LinkToData( DataBuffer, 'GivenName' ) ;
  paeFamilyName.LinkToData( DataBuffer, 'FamilyName' ) ;
end;

end.
