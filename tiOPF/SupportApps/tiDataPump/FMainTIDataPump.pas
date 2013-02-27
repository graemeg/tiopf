unit FMainTIDataPump;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    gbSource: TGroupBox;
    gbTarget: TGroupBox;
    BitBtn1: TBitBtn;
    memoLog: TMemo;
    paeSourcePL: TtiPerAwareComboBoxHistory;
    paeTargetPL: TtiPerAwareComboBoxHistory;
    paeSourceDatabaseName: TtiPerAwareComboBoxHistory;
    paeSourceUserName: TtiPerAwareComboBoxHistory;
    paeSourcePassword: TtiPerAwareComboBoxHistory;
    paeTargetDatabaseName: TtiPerAwareComboBoxHistory;
    paeTargetUserName: TtiPerAwareComboBoxHistory;
    paeTargetPassword: TtiPerAwareComboBoxHistory;
    memoTables: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    procedure UpdateProgress(const pMessage: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  tiRegINI
  ,tiDataPump_BOM
  ;
  
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  gINI.ReadFormState(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState(Self);
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  lDP : TtiDataPump ;
begin
  lDP := TtiDataPump.Create ;
  try
    lDP.SourcePerLayerName := paeSourcePL.Value;
    lDP.SourceDatabaseName := paeSourceDatabaseName.Value;
    lDP.SourceUserName := paeSourceUserName.Value;
    lDP.SourcePassword := paeSourcePassword.Value;

    lDP.TargetPerLayerName := paeTargetPL.Value;
    lDP.TargetDatabaseName := paeTargetDatabaseName.Value;
    lDP.TargetUserName := paeTargetUserName.Value;
    lDP.TargetPassword := paeTargetPassword.Value;

    lDP.Tables.Assign(memoTables.Lines);

    lDP.OnUpdateProgress := UpdateProgress;
    
    lDP.Execute;

  finally
    lDP.Free;
  end;

end;

procedure TForm1.UpdateProgress(const pMessage: string);
begin
  memoLog.Lines.Add(pMessage);
end;

end.
