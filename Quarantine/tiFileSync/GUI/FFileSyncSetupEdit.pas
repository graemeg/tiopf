unit FFileSyncSetupEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FtiPerEditDialog, StdCtrls, Buttons, tiPerAwareCtrls, ExtCtrls, tiObject,
  tiReadOnly, tiFocusPanel;

type
  TFormFileSyncSetupEdit = class(TFormTIPerEditDialog)
    GroupBox1: TGroupBox;
    paeLocalDir: TtiPerAwareEdit;
    paeSourceReader: TtiPerAwareComboBoxStatic;
    gbTarget: TGroupBox;
    paeTargetLocation: TtiPerAwareEdit;
    paeTargetReader: TtiPerAwareComboBoxStatic;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
  protected
    procedure SetData(const Value: TtiObject); override ;
  end;

implementation
uses
  tiFileSyncReader_Abs
  ,cFileSync
  ;

{$R *.DFM}

{ TFormTIPerEditDialog1 }

procedure TFormFileSyncSetupEdit.SetData(const Value: TtiObject);
begin
  inherited SetData( Value ) ;
  paeLocalDir.LinkToData(       DataBuffer, 'LocalDir'     ) ;
  paeTargetLocation.LinkToData( DataBuffer, 'TargetLocation'    ) ;
  paeSourceReader.LinkToData(   DataBuffer, 'SourceReader' ) ;
  paeTargetReader.LinkToData(   DataBuffer, 'TargetReader' ) ;
end;

procedure TFormFileSyncSetupEdit.FormCreate(Sender: TObject);
var
  i : integer ;
begin
  inherited;
  paeSourceReader.Items.Add( cgsDiskFiles ) ;
  for i := 0 to gFileSyncReaderFactory.Count - 1 do
    paeTargetReader.Items.Add( gFileSyncReaderFactory.Items[i].PerObjAbsClassName ) ;
end;

end.
