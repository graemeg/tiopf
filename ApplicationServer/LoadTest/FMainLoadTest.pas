unit FMainLoadTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls
  ,Contnrs
  ;

type

  TThrdHTTPServerLoadTest = class( TThread )
  private
    FCycles : integer;
    FTargetCycles : integer ;
    FThreadID : integer ;
  public
    constructor CreateExt(pThreadID : Integer ; pTargetCycles : integer) ;
    procedure   Execute ; override ;
  end ;

  TForm2 = class(TForm)
    paeThreadCount: TtiPerAwareFloatEdit;
    btnStart: TButton;
    btnStop: TButton;
    paeCycles: TtiPerAwareFloatEdit;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    FList : TObjectList ;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  tiHTTP
  ,tiLog
  ;

{$R *.dfm}

{ TThrdHTTPServerLoadTest }

constructor TThrdHTTPServerLoadTest.CreateExt(pThreadID : integer ; pTargetCycles : integer);
begin
  Create(true);
  FreeOnTerminate := false ;
  FTargetCycles := pTargetCycles;
  FThreadID := pThreadID ;
  Resume ;
end;

procedure TThrdHTTPServerLoadTest.Execute ;
var
  lHTTPClient : TtiHTTPIndy ;
  ls : string;
  lDoc : string ;
begin
  FCycles := 0 ;
  while (FCycles <= FTargetCycles) and (not Terminated )do
  begin
    lHTTPClient := TtiHTTPIndy.Create ;
    try
      lDoc := IntToStr(GetCurrentThreadID) + '_' + IntToStr(FCycles);
      lHTTPClient.Get( 'http:\\preprodopdms\' + lDoc) ;
      ls := Copy( lHTTPClient.Output.DataString, 13, 50 ) ;
      LogArray([FThreadID, FCycles, ls]);
    finally
      lHTTPClient.Free;
    end;
    Inc(FCycles);
  end ;
end;

procedure TForm2.btnStartClick(Sender: TObject);
var
  i : integer ;
begin
  FList.Clear;
  for i := 1 to Trunc(paeThreadCount.Value) do
  FList.Add(TThrdHTTPServerLoadTest.CreateExt(i, Trunc(paeCycles.Value))) ;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FList :=TObjectList.Create(true);
  paeThreadCount.Value := 5 ;
  paeCycles.Value := 100 ;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

procedure TForm2.btnStopClick(Sender: TObject);
var
  i : integer ;
begin
  for i := 0 to FList.Count - 1 do
    TThrdHTTPServerLoadTest(FList.Items[i]).Terminate;
  for i := 0 to FList.Count - 1 do
    TThrdHTTPServerLoadTest(FList.Items[i]).WaitFor;
  Log('Cancel');
end;

end.
