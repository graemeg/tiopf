unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, tiMemoReadOnly, jpeg, ExtCtrls, ComCtrls
  ,tiFileSync_Mgr
  ;

const
  cErrorCanNotFindOPDMSEXE = 'Can not find OPMDS application to run:'#13#13'%s';
  cError = ' * * * Error * * * ';
  
type
  TForm1 = class(TForm)
    memoLog: TtiMemoReadOnly;
    PB: TProgressBar;
    Button1: TButton;
    procedure tmrExecuteTimer(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure Execute ;
    procedure DoLog( const pMessage : string; pAppendToPrevRow: boolean);
    procedure DoUpdateProgress( pMax, pPos : integer );
    procedure DoTerminate(Sender: TObject ) ;
    procedure DoTerminateWithError(Sender: TObject ) ;
    procedure ShowError(const pMessage: string);
  public
  end;

var
  Form1: TForm1;

implementation
uses
   tiFileSyncReader_Abs
  ,tiFileSyncReader_DiskFiles
  ,tiFileSyncReader_Remote
  ,tiCompressZLib
  ,tiOIDGUID
  ,cFileSync
  ,tiUtils
  ,tiConstants
  , Math;

  //

{$R *.dfm}

procedure TForm1.tmrExecuteTimer(Sender: TObject);
begin
end;

// Move this lot to a thread...
procedure TForm1.Execute;
var
  lParams: string;
begin
  lParams := cHTTPURL + '=' + 'http://cartopddb/OPDMSLauncher.exe';
  TthrdTIFileSyncOneWayCopy.Create(
    cgsRemote,
    lParams,
    '..\OPDMSClient\Remote',
    cgsDiskFiles,
    '',
    tiGetEXEPath,
    DoLog,
    DoUpdateProgress,
    DoTerminate,
    DoTerminateWithError,
    Self);
end;

procedure TForm1.DoLog(const pMessage: string; pAppendToPrevRow: boolean);
begin
  MemoLog.Lines.Add(pMessage)
end;

procedure TForm1.DoUpdateProgress(pMax, pPos: integer);
begin
  PB.Max:= pMax;
  PB.Position:= pPos;
end;


procedure TForm1.DoTerminate(Sender: TObject);
begin
end;

procedure TForm1.DoTerminateWithError(Sender: TObject);
begin
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close ;
end;

procedure TForm1.ShowError(const pMessage: string);
begin
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Execute;
end;

initialization

end.

