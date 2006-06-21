unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  tiPersist
  ,tiUtils
  ;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  gTIPerMgr.LoadPersistenceFramework ;
  memo1.Lines.Text :=
    'Take a look at the command line parameter for connection details' + Cr(2) +
    'You have connected as follows:' + Cr +
    gTIPerMgr.DefaultPerLayer.DBConnectionPools.DetailsAsString ;
end;

end.
