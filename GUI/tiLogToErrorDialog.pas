unit tiLogToErrorDialog;

{$I tiDefines.inc}

interface

uses
  Classes
  ,SysUtils
  ,Graphics, Controls, Forms
  ,Dialogs, ExtCtrls, StdCtrls, Buttons
  {$IFDEF MSWINDOWS}
  ,Windows, Messages
  {$ENDIF MSWINDOWS}
  ,tiLog,tiLogToFile
  {$IFNDEF VER130}
  ,Variants
  {$ENDIF}
 ;

type
  TLogErrorForm = class(TForm)
    Image: TImage;
    MemoLog: TMemo;
    CopyButton: TSpeedButton;
    OKButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  // Log to an error form
  TLogToError = class(TtiLogToCacheAbs)
  private
    FForm             : TLogErrorForm;

    procedure ShowError;
  protected
    function  AcceptEvent(const ADateTime : string;
                           const AMessage : string;
                           ASeverity : TtiLogSeverity): boolean; override;
    procedure   WriteToOutput; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Log(const ADateTime : string;
                     const AThreadID : string;
                     const AMessage : string;
                     ASeverity : TtiLogSeverity); override;
  end;

implementation
uses
  tiUtils
  {$IFDEF FPC}
  ,LResources
  {$ENDIF}
 ;

var
  lLog : TtiLogToFile;



{$IFNDEF FPC}
  {$R *.dfm}
{$ENDIF}

const
  // Some constants for displaying the error dialog
  cuiBorder     = 8;
  cuiBtnHeight  = 25;
  cuiBtnWidth   = 85;
  cuiImageWidth = 32;

procedure TLogErrorForm.FormCreate(Sender: TObject);
begin
  Height     := Screen.Height div 4 * 3;
  Width      := Screen.Width  div 4 * 3;
  Caption    := ' Application error log - ' + Application.Title;
  Constraints.MinHeight := 300;
  Constraints.MinWidth := 550;
  {$IFNDEF FPC}
  Image.Picture.Icon.Handle := LoadIcon(0, IDI_ERROR);
  {$ENDIF}
  CopyButton.Glyph.LoadFromResourceName(HInstance, 'tiLogCopyToClip');
end;

procedure TLogErrorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MemoLog.Lines.Clear;
end;

procedure TLogErrorForm.FormResize(Sender: TObject);
begin
  Image.SetBounds(cuiBorder,
                    (ClientHeight - cuiImageWidth) div 2,
                    cuiImageWidth,
                    cuiImageWidth);
  OKButton.SetBounds((ClientWidth  - cuiBorder*2 - cuiBtnWidth) div 2,
                      ClientHeight - cuiBorder - cuiBtnHeight,
                      cuiBtnWidth,
                      cuiBtnHeight);
end;

procedure TLogErrorForm.CopyButtonClick(Sender: TObject);
begin
  MemoLog.SelectAll;
  MemoLog.CopyToClipboard;
  MemoLog.SelStart := 0;
  MemoLog.SelLength := 0;
  MessageDlg('The error message has been copied to the clipboard.'#13#13 +
              'You can now paste into an email to forward to ' +
              Application.Title + ' support.',
              mtInformation,
              [mbOK],
              0);
end;

procedure TLogErrorForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;


constructor TLogToError.Create;
begin
  // GUI output must be synchronized with the main thread.
  inherited CreateSynchronized;
  FForm := TLogErrorForm.Create(nil);
  ThrdLog.Resume;
end;

destructor TLogToError.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

function TLogToError.AcceptEvent(const ADateTime,
                                  AMessage: string;
                                  ASeverity: TtiLogSeverity): boolean;
begin
  result := (ASeverity = lsError);
end;



procedure TLogToError.WriteToOutput;
begin
  inherited WriteToOutput;

  if ListWorking.Count = 0 then
    Exit; //==>

  ThrdLog.tiSynchronize(ShowError);

end;

procedure TLogToError.ShowError;
var
  i : integer;
  lLogEvent : TtiLogEvent;
  lsMessage : string;
begin

  if FForm.MemoLog.Lines.Count > 0 then begin
    FForm.MemoLog.Lines.Add('');
    FForm.MemoLog.Lines.Add('* * * * * * * * * * * * * * * * * * * * *');
    FForm.MemoLog.Lines.Add('');
  end;

  for i := 0 to ListWorking.Count - 1 do begin
    lLogEvent := TtiLogEvent(ListWorking.Items[i]);
    FForm.MemoLog.Lines.Add('An error occurred at: ' + lLogEvent.DateTime);
    FForm.MemoLog.Lines.Add('');
    lsMessage := tiStrTran(lLogEvent.LogMessage, #10, '');
    lsMessage := tiStrTran(lsMessage, #13, #13+#10);
    FForm.MemoLog.Lines.Add(lsMessage);
  end;
  ListWorking.Clear;

  FForm.MemoLog.Selstart := 0;
  {$IFNDEF FPC}
  SendMessage(FForm.MemoLog.handle, em_scrollcaret, 0, 0);
  {$ENDIF}

  if not FForm.Visible then begin
    FForm.Top        := (Screen.Height - FForm.Height) div 2;
    FForm.Left       := (Screen.Width  - FForm.Width ) div 2;
    FForm.Visible    := true;
    FForm.OKButton.SetFocus;
  end;

end;


procedure TLogToError.Log(const ADateTime, AThreadID, AMessage: string; ASeverity: TtiLogSeverity);
begin
  if Terminated then
    Exit; //==>
  inherited log(ADateTime, AThreadID, AMessage, ASeverity);
end;

initialization
  lLog := TtiLogToFile.CreateWithDateInFileName;
  gLog.RegisterLog(LLog);
  Log('Application <' + ParamStr(0) + '> started');
  gLog.RegisterLog(TLogToError);
  
{$IFDEF FPC}
{$I tiLogToErrorDialog.lrs}
{$ENDIF}

end.
