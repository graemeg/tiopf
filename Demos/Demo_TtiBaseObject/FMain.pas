unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    NoReferenceCountingPerformance: TButton;
    btnReferenceCountingPerformance: TButton;
    GroupBox2: TGroupBox;
    btnTestValid: TButton;
    Memo1: TMemo;
    memoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure PerformanceTestNoReferenceCounting(Sender: TObject);
    procedure PerformanceTestReferenceCounting(Sender: TObject);
    procedure btnTestValidClick(Sender: TObject);
  private
    procedure Log(const AMessage: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
   tiBaseObject
  ,tiUtils
  ;

const
  CTestRunTime = 5; // Seconds

{$R *.dfm}

procedure TForm1.PerformanceTestNoReferenceCounting(Sender: TObject);
var
  LO: TtiBaseObject;
  LStart: Cardinal;
  LCount: Cardinal;
  i: integer;
begin
  LCount:= 0;
  LStart:= tiGetTickCount;
  while tiGetTickCount - LStart < CTestRunTime * 1000 do
  begin
    for i := 1 to 10000 do
    begin
      LO:= TtiBaseObject.Create;
      LO.Free;
    end;
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d seconds (no reference counting)',
    [tiIntToCommaStr(LCount), CTestRunTime]));
end;

procedure TForm1.PerformanceTestReferenceCounting(Sender: TObject);
{$ifdef reference_counting}
var
  LO: IInterface;
  LStart: Cardinal;
  LCount: Cardinal;
  i: integer;
begin
  LCount:= 0;
  LStart:= tiGetTickCount;
  while tiGetTickCount - LStart < CTestRunTime * 1000 do
  begin
    for i := 1 to 10000 do
      LO:= TtiBaseObject.CreateWithRefCounting;
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d seconds (reference counting)',
    [tiIntToCommaStr(LCount), CTestRunTime]));
{$else}
begin
  Assert(False, 'reference_counting not enabled');
{$endif}
end;

procedure TForm1.btnTestValidClick(Sender: TObject);
var
  LO: TtiBaseObject;
begin
  LO:= TtiBaseObject.Create;
  Assert(LO.TestValid);
  LO.Free;
  Assert(not LO.TestValid);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef reference_counting}
    Log('reference_counting is ON');
  {$else}
    Log('reference_counting is OFF');
    btnReferenceCountingPerformance.Enabled:= False;
  {$endif}
  Log('---');
end;

procedure TForm1.Log(const AMessage: string);
begin
  memoLog.Lines.Add(AMessage);
end;

end.
