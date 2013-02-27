
{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, tiPerAwareCtrls, Adrs_BOM, tiThreadProgress,
  tiFocusPanel ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    paeThreadCount: TtiPerAwareFloatEdit;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public declarations }
  end;

  TThrdReadAdrsBook = class( TThread )
  private
    FData : TAdrsBook ;
    procedure DoOnTerminate( Sender : TObject ) ;
  public
    constructor CreateExt ;
    destructor  Destroy ; override ;
    procedure   Execute ; override ;
  end ;

  TThrdProgressReadAdrsBook = class( TtiThreadProgress )
  private
    FData : TAdrsBook ;
  protected
    procedure DoOnTerminate( Sender : TObject ) ; override ;
  public
    constructor Create(CreateSuspended: Boolean); override ;
    destructor  Destroy ; override ;
    procedure   Execute ; override ;
  end ;

var
  Form1: TForm1;

implementation
uses
  tiPtnVisPerObj_Cli
  ,tiUtils
  ,tiDialogs
  ;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  lAdrsBook : TAdrsBook ;
begin
  lAdrsBook := TAdrsBook.Create ;
  try
    lAdrsBook.Read ;
    tiShowPerObjAbs( lAdrsBook ) ;
  finally
    lAdrsBook.Free ;
  end ;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i : integer ;
begin
  for i := 1 to Trunc( paeThreadCount.Value ) do
  begin
    TThrdReadAdrsBook.CreateExt ;
    Sleep( 1000 ) ;
  end ;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i : integer ;
begin
  for i := 1 to Trunc( paeThreadCount.Value ) do
  begin
    TThrdProgressReadAdrsBook.Create(True) ;
    Sleep( 1000 ) ;
  end ;
end;

{ TThrdReadAdrsBook }

constructor TThrdReadAdrsBook.CreateExt;
begin
  Create( true ) ;
  FData := TAdrsBook.Create ;
  FreeOnTerminate := true ;
  OnTerminate := DoOnTerminate ;
  Resume ;
end;

destructor TThrdReadAdrsBook.Destroy;
begin
  FData.Free ;
  inherited;
end;

procedure TThrdReadAdrsBook.DoOnTerminate(Sender: TObject);
var
  lFileName : TFileName ;
begin
  lFileName := tiGetTempFile( 'TXT' ) ;
  tiStringToFile( tiPerObjAbsAsString( FData ), lFileName ) ;
  tiEditFile( lFileName ) ;
end;

procedure TThrdReadAdrsBook.Execute;
begin
  FData.Read ;
  // To make the process look longer than it really is
  Sleep( 1000 ) ;
end;

{ TThrdProgressReadAdrsBook }

constructor TThrdProgressReadAdrsBook.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FData := TAdrsBook.Create ;
  Text := 'Reading TAdrsBook' ;
  Application.ProcessMessages ;
  Resume ;
end;

destructor TThrdProgressReadAdrsBook.Destroy;
begin
  FData.Free ;
  inherited;
end;

procedure TThrdProgressReadAdrsBook.DoOnTerminate(Sender: TObject);
var
  lFileName : TFileName ;
begin
  inherited ;
  lFileName := tiGetTempFile( 'TXT' ) ;
  tiStringToFile( tiPerObjAbsAsString( FData ), lFileName ) ;
  tiEditFile( lFileName ) ;
end;

procedure TThrdProgressReadAdrsBook.Execute;
begin
  FData.Read ;
  // To make the process look longer than it really is
  Sleep( 15000 ) ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  tiShowMessage(
    'Before running this demo, you will have to populate the '+
    'database with some test data.' + Cr(2) +
    'You can use DemoTIPerFramework ' +
    'to do this.' ) ;
end;

end.
