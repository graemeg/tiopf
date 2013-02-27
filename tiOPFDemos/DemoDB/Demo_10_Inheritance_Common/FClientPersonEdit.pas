unit FClientPersonEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FClientAbsEdit, tiReadOnly, StdCtrls, tiMemoReadOnly, ExtCtrls,
  tiFocusPanel, tiPerAwareCtrls, Buttons
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiPtnVisPerObj
  ;
  

type
  TFormClientPersonEdit = class(TFormClientAbsEdit)
    paeGivenName: TtiPerAwareEdit;
    paeFamilyName: TtiPerAwareEdit;
    paeNameTitle: TtiPerAwareComboBoxStatic;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetData(const Value: TPerObjAbs); override ;
  end;

implementation
uses
  Client_BOM
  ;
  
{$R *.dfm}

{ TFormClientPersonEdit }

procedure TFormClientPersonEdit.SetData(const Value: TPerObjAbs);
begin
  inherited;
  paeNameTitle.LinkToData( DataBuffer, 'NameTitle' ) ;
  paeGivenName.LinkToData( DataBuffer, 'GivenName' ) ;
  paeFamilyName.LinkToData( DataBuffer, 'FamilyName' ) ;
end;

procedure TFormClientPersonEdit.FormCreate(Sender: TObject);
begin
  inherited;
  AssignNameTitles(paeNameTitle.Items);
end;

end.
