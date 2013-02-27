
{$I tiDefines.inc}

unit FtiListViewDif;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tiListViewDif, Contnrs, ExtCtrls, tiListView, tiListViewPlus,
  tiSplitter ;

type

  TLVDifTestDataRow = class( TPersistent )
  private
    FOID: integer;
    FStrProp: string;
  published
    property OID : integer read FOID write FOID ;
    property StrProp : string read FStrProp write FStrProp ;
  end ;

  TFormTIListViewDif = class(TForm)
    LVD: TtiListViewDif;
    lblLHS: TLabel;
    lblRHS: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVDMoveSplitter(pTISplitterPanel: TtiSplitterPanel);
  private
    FDataLHS : TObjectList ;
    FDataRHS : TObjectList ;
  public
    { Public declarations }
  end;

var
  FormTIListViewDif: TFormTIListViewDif;

implementation

{$R *.DFM}

procedure TFormTIListViewDif.FormCreate(Sender: TObject);
var
  i : integer ;
  lData : TLVDifTestDataRow ;
begin

  FDataLHS := TObjectList.Create ;
  FDataRHS := TObjectList.Create ;

  for i := 1 to 1000 do
  begin
    if i mod 7 <> 0 then
    begin
      lData := TLVDifTestDataRow.Create ;
      lData.OID := i ;
      lData.StrProp := IntToStr( i ) ;
      FDataLHS.Add( lData ) ;
    end ;

    if i mod 5 <> 0 then
    begin
      lData := TLVDifTestDataRow.Create ;
      lData.OID := i ;
      lData.StrProp := IntToStr( i ) ;
      FDataRHS.Add( lData ) ;
      if i mod 3 = 0 then
        lData.StrProp := IntToStr( i+1 ) ;
    end ;

  end ;

  LVD.PKProperties.Add.PropName := 'OID' ;
  LVD.DataLHS := FDataLHS ;
  LVD.DataRHS := FDataRHS ;

end;

procedure TFormTIListViewDif.FormDestroy(Sender: TObject);
begin
  FDataLHS.Free ;
  FDataRHS.Free ;
end;

procedure TFormTIListViewDif.LVDMoveSplitter( pTISplitterPanel: TtiSplitterPanel);
begin
  lblRHS.Left :=
    LVD.Left + LVD.SplitterPos + 28 ;
end;

end.
