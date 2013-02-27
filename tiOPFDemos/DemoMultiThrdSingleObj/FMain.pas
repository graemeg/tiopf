
{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, tiPerAwareCtrls, tiMemoReadOnly, tiFocusPanel;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    paeNoOfThreads: TtiPerAwareFloatEdit;
    tiMemoReadOnly1: TtiMemoReadOnly;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  Form1: TForm1;

implementation
uses
  TestPerObjThreadList
  ,tiPtnVisPerObj_Cli
  ,tiPerObjOIDGUID
  ;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TestPerObjThreadList.ExecuteMultiThreadTest( 1 ) ;
end ;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TestPerObjThreadList.ExecuteMultiThreadTest( Trunc( paeNoOfThreads.Value ) ) ;
end;

end.
