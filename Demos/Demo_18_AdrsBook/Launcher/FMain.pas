unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, tiMemoReadOnly, jpeg, ExtCtrls, ComCtrls
  ,tiFileSync_Mgr, XPMan, tiFileName_BOM;

const
  cError = ' * * * Error * * * ';
  CEXENameToRun = 'AdrsBookUIModalForms.exe';
  
const
  EXECUTE_FILESYNC                = WM_USER + 1;

type

  TForm1 = class(TForm)
    bvlBottom: TBevel;
    memoLog: TtiMemoReadOnly;
    pbMajor: TProgressBar;
    btnClose: TButton;
    XPManifest1: TXPManifest;
    lblProgressMajor: TLabel;
    lblProgressMinor: TLabel;
    pbMinor: TProgressBar;
    Animate: TAnimate;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrExecuteTimer(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    FThread: TthrdTIFileSyncOneWayCopy;
    FTerminated: Boolean;
    procedure DoExecute(var Message: TMessage); message EXECUTE_FILESYNC;
    procedure Execute ;
    procedure DoLog( const pMessage : string; pAppendToPrevRow: boolean);
    procedure DoUpdateProgressMajor(AFileName: TtiFileName; AMax, APos : integer);
    procedure DoUpdateProgressMinor(AFileName: TtiFileName; AMax, APos : integer);
    procedure DoTerminate(Sender: TObject ) ;
    procedure DoTerminateWithError(Sender: TObject ) ;
    procedure DoFile( const pFileName: string; pAction: TtiFileSyncAction );
    procedure ShowError(const pMessage: string);
    procedure ShowCloseButton;
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
  ,tiUtils
  ,tiConstants
  ,tiHTTP
  ,tiLog
  ,tiLogToFile
  ,Math
  ,StrUtils

  ,cFileSync
  ;


{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLog.RegisterLog(TtiLogToFile);
  FTerminated:= False;
  btnCancel.Left:= btnClose.Left;
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;
  Constraints.MaxWidth  := Width;
  Constraints.MinWidth  := Width;
  SendMessage(Handle, EXECUTE_FILESYNC, 0, 0);
end;

procedure TForm1.tmrExecuteTimer(Sender: TObject);
begin
end;

// Move this lot to a thread...
procedure TForm1.Execute;
var
  lsl : TStringList;
  lParams: string;
begin
  tiHTTP.gTIOPFHTTPDefaultBlockSize:= 256000;
  Show;

  lsl := TStringList.Create;
  try
    lsl.Values[cHTTPURL] := lsl.Values[cHTTPURL] + 'http://localhost/LauncherServer.exe';
    lParams := lsl.CommaText;
  finally
    lsl.Free;
  end;

  FThread:=
    TthrdTIFileSyncOneWayCopy.Create(
      cgsRemote,                                                // SourceReader
      lParams,                                                  // SourceReaderParams
      'AddressBook',                                            // SourceStartDir
      cgsDiskFiles,                                             // TargetReader
      '',                                                       // TargetReaderParams
      ExpandFileName(tiGetEXEPath),                             // TargetStartDir
      DoLog,                                                    // OnLog
      DoUpdateProgressMajor,                                    // OnProgressMajor
      DoUpdateProgressMinor,                                    // OnProgressMinor
      DoFile,                                                   // OnFile
      DoTerminate,                                              // OnTerminate
      DoTerminateWithError,                                     // OnTerminateWithError
      Self);                                                    // FormToShow
end;

procedure TForm1.DoLog(const pMessage: string; pAppendToPrevRow: boolean);
var
  lIndex: Integer ;
  lLine: string;
begin
  if pAppendToPrevRow and ( MemoLog.Lines.Count > 0 ) then
  begin
    lIndex := memoLog.Lines.Count-1 ;
    lLine := MemoLog.Lines[lIndex] + ' ' + pMessage;
    MemoLog.Lines[lIndex] := lLine;
  end else
    MemoLog.Lines.Add(pMessage)
end;

procedure TForm1.DoUpdateProgressMajor(AFileName: TtiFileName; AMax, APos : integer);
begin
  if Assigned(AFileName) then
  begin
    lblProgressMajor.Caption:=
      Format('Copying file %d/%d %s',
             [APos, AMax,
              AFileName.NameFull]);
    pbMajor.Visible:= True;;
    lblProgressMajor.Visible:= True;
    DoUpdateProgressMinor(AFileName, 0, 0);
  end else
    lblProgressMajor.Visible:= False;
  pbMajor.Max:= AMax;
  pbMajor.Position:= APos;
end;


procedure TForm1.DoTerminate(Sender: TObject);
var
  lFileName: string;
begin
  OnCloseQuery:= nil;
  try
    if not FTerminated then
    begin
      lFileName:= tiAddEXEPath(CEXENameToRun) ;
      Hide;
      tiShellExecute(lFileName);
    end;
    Close;
  except
    on e:Exception do
      ShowError(e.message);
  end;
end;

procedure TForm1.DoTerminateWithError(Sender: TObject);
begin
  ShowError('');
  ShowCloseButton;
end;

procedure TForm1.ShowCloseButton;
begin
  Animate.Visible:= False;
  lblProgressMajor.Visible:= False;
  lblProgressMinor.Visible:= False;
  pbMajor.Visible:= False;
  pbMinor.Visible:= False;
  btnCancel.Visible:= False;
  btnClose.Visible := True ;
end;

procedure TForm1.DoFile(const pFilename: string; pAction: TtiFileSyncAction);
begin
  //
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  OnCloseQuery:= nil;
  Close ;
end;

procedure TForm1.ShowError(const pMessage: string);
begin
  Animate.Active := False ;
  Animate.Visible := False ;
  memoLog.Height := bvlBottom.Top - memoLog.Top - 8;
  pbMajor.Visible := False ;
  ShowCloseButton;
  if pMessage <> '' then
    memolog.Lines.Text :=
      memoLog.Lines.Text + Cr +
      cError + Cr +
      pMessage ;
  Show;
end;

procedure TForm1.DoExecute(var Message: TMessage);
begin
  Execute;
end;

function ExtractNamePart(const AFileName: string): string;
begin
  Result := Copy(AFileName, 1, Length(AFileName) - Length(ExtractFileExt(AFileName)));
end;

procedure TForm1.DoUpdateProgressMinor(AFileName: TtiFileName; AMax, APos: integer);
begin
  if Assigned(AFileName) then
  begin
    if AMax = 0 then
    begin
      lblProgressMinor.Caption:= 'Reading files size';
      pbMinor.Visible:= False;
    end else
    begin
      lblProgressMinor.Caption:=
        Format('%skb of %skb copied.',
               [tiIntToCommaStr(APos div 1000), tiIntToCommaStr(AMax div 1000)]);
      pbMinor.Visible:= True;
    end;
    lblProgressMinor.Visible:= True;
    pbMinor.Max:= AMax;
    pbMinor.Position:= APos;
  end else
  begin
    lblProgressMinor.Visible:= False;
    pbMinor.Visible:= False;
  end;
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  OnCloseQuery:= nil;
  BorderIcons:= [];
  btnCancel.Visible:= False;
  FTerminated:= True;
  if Assigned(FThread) then
    FThread.Terminate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(FThread) then
  begin
    FThread.WaitFor;
    FThread.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Animate.Active := True ;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= False;
  btnCancelClick(nil);
end;

end.

