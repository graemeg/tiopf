
{$I tiDefines.inc}

unit FDUnitTestingWhichPersistenceLayers;

interface                     

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, tiButtons, StdCtrls;

type
  TFormWhichPersistenceLayers = class(TForm)
    btnPnl: TtiButtonPanel;
    Tmr: TTimer;                 
    pnlCheckBoxes: TPanel;                                 
    cbTestNonPersistentClasses: TCheckBox;
    Bevel1: TBevel;
    rgTestPersistenceLayers: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure btnPnlBtn1Click(Sender: TObject);
    procedure btnPnlBtn2Click(Sender: TObject);
    procedure TmrTimer(Sender: TObject);
    procedure cbTestNonPersistentClassesClick(Sender: TObject);
    procedure rgTestPersistenceLayersClick(Sender: TObject);
  private
    procedure DoCheckBoxClick( Sender : TObject ) ;
    procedure Save;
    procedure EnableRGTestPersistenceLayers;
    function CountSelectedPersistenceLayers: integer;
  public
    class function Execute : boolean ;
  end;

implementation
uses
  tiDBConnectionSetupAbs_TST
  ,tiUtils
  ,tiDUnitINI
  ,tiDUnitDependencies
  ;

{$R *.DFM}

{ TFormWhichPersistenceLayers }

class function TFormWhichPersistenceLayers.Execute : boolean ;
var
  lForm : TFormWhichPersistenceLayers ;
begin
  lForm := TFormWhichPersistenceLayers.Create( nil ) ;
  try
    result := lForm.ShowModal = mrOK ;
  finally
    lForm.Free ;
  end ;
end;

procedure TFormWhichPersistenceLayers.FormCreate(Sender: TObject);
var
  i : integer ;
  lCheckBox : TCheckBox ;
  lPerLayerName : string ;
  lPerFrameworkSetup : TPerFrameworkSetup ;
const
  cBorder = 8 ;
begin
  cbTestNonPersistentClasses.Checked := gPerFrameworkSetupFactory.TestNonPersistentClasses ;
  rgTestPersistenceLayers.ItemIndex  := Ord( gPerFrameworkSetupFactory.TestPersistenceLayers );
  lCheckBox := nil ;
  for i := 0 to gPerFrameworkSetupFactory.Count - 1 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lPerFrameworkSetup := gPerFrameworkSetupFactory.FindByClassID( lPerLayerName ) ;
    lCheckBox := TCheckBox.Create( self ) ;
    lCheckBox.Top := ( i ) * ( cBorder + lCheckBox.Height ) + cBorder ;
    lCheckBox.Left := cBorder ;
    lCheckBox.Caption := lPerLayerName ;
    lCheckBox.Name := 'cb' + lPerLayerName ;
    lCheckBox.Tag := i ;
    lCheckBox.Parent := pnlCheckBoxes ;
    lCheckBox.Checked := lPerFrameworkSetup.Enabled and lPerFrameworkSetup.Selected ;
    lCheckBox.OnClick := DoCheckBoxClick ;
    lCheckBox.Enabled := lPerFrameworkSetup.Enabled ;
  end ;
  if lCheckBox <> nil then
  begin
    pnlCheckBoxes.ClientHeight := lCheckBox.Top + lCheckBox.Height ;
    ClientHeight := pnlCheckBoxes.Height + btnPnl.Height + cBorder ;
  end ;
  Width := 269 ;
  ClientHeight := pnlCheckBoxes.Height + btnPnl.Height +
                  pnlCheckBoxes.Top + 8;
  EnableRGTestPersistenceLayers;
  Tmr.Enabled := true ;
end;

procedure TFormWhichPersistenceLayers.Save;
var
  i : integer ;
  lCheckBox : TCheckBox ;
  lPerLayerName : string ;
  lPerFrameworkSetup : TPerFrameworkSetup ;
begin
  Tmr.Enabled := false ;
  gPerFrameworkSetupFactory.TestNonPersistentClasses := cbTestNonPersistentClasses.Checked ;
  gPerFrameworkSetupFactory.TestPersistenceLayers    := TTestPersistenceLayers(rgTestPersistenceLayers.ItemIndex);
  for i := gPerFrameworkSetupFactory.Count - 1 downto 0 do
  begin
    lPerLayerName := gPerFrameworkSetupFactory.Items[i].PerLayerName ;
    lCheckBox := TCheckBox( FindComponent( 'cb' + lPerLayerName )) ;
    lPerFrameworkSetup := gPerFrameworkSetupFactory.FindByClassID(lPerLayerName);
    lPerFrameworkSetup.Selected := lCheckBox.Checked ;
    if not lPerFrameworkSetup.Selected then
      gPerFrameworkSetupFactory.DeleteByClassID( lPerLayerName ) ;
  end ;
end ;

procedure TFormWhichPersistenceLayers.btnPnlBtn1Click( Sender: TObject);
begin
  Save ;
  ModalResult := mrOK ;
end;

procedure TFormWhichPersistenceLayers.btnPnlBtn2Click( Sender: TObject);
begin
  Save ;
  ModalResult := mrCancel ;
end;

procedure TFormWhichPersistenceLayers.TmrTimer(Sender: TObject);
begin
  btnPnlBtn1Click( nil ) ;
end;

procedure TFormWhichPersistenceLayers.DoCheckBoxClick(Sender: TObject);
begin
  Tmr.Enabled := false ;
  if ( CountSelectedPersistenceLayers <= 1 ) and
     ( TTestPersistenceLayers(rgTestPersistenceLayers.ItemIndex ) = tplAllAtSameTime)
   then
     rgTestPersistenceLayers.ItemIndex := Ord( tplOneAtTheTime);
  EnableRGTestPersistenceLayers;
end;

function TFormWhichPersistenceLayers.CountSelectedPersistenceLayers : integer;
var
  i : integer ;
begin
  result := 0 ;
  for i := 0 to pnlCheckBoxes.ControlCount - 1 do
    if ( pnlCheckBoxes.Controls[i] is TCheckBox ) and
       (( pnlCheckBoxes.Controls[i] as TCheckBox ).Checked ) then
     Inc(result);
end;

procedure TFormWhichPersistenceLayers.EnableRGTestPersistenceLayers;
begin
  rgTestPersistenceLayers.Enabled := CountSelectedPersistenceLayers > 1 ;
end;

procedure TFormWhichPersistenceLayers.cbTestNonPersistentClassesClick(Sender: TObject);
begin
  DoCheckBoxClick( nil ) ;
end;

procedure TFormWhichPersistenceLayers.rgTestPersistenceLayersClick(Sender: TObject);
begin
  Tmr.Enabled := false ;
end;

end.
