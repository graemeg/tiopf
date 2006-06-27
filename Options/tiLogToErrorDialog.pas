{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiLogToErrorDialog;

{$I tiDefines.inc}

interface

uses
  Classes
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows, Messages, Graphics, Controls, Forms
  ,Dialogs, ExtCtrls, StdCtrls, Buttons
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QGraphics, QControls, QForms, QDialogs
  ,QExtCtrls, QStdCtrls, QButtons
  {$ENDIF LINUX}
  ,tiLog
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
  TLogToError = class( TtiLogToCacheAbs )
  private
    FForm              : TLogErrorForm ;

    procedure ShowError;
  protected
    function  AcceptEvent( const psDateTime : string ;
                           const psMessage  : string;
                           pSeverity  : TtiLogSeverity ) : boolean ; override ;
    procedure   WriteToOutput ; override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Log( const psDateTime : string ;
                     const psThreadID : string ;
                     const psMessage  : string;
                     pSeverity  : TtiLogSeverity ) ; override ;
  end;

implementation
uses
  tiUtils
  ;
  
{$IFDEF MSWINDOWS}
  {$R *.dfm}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  {$R *.xfm}
{$ENDIF LINUX}

const
  // Some constants for displaying the error dialog
  cuiBorder     = 8 ;
  cuiBtnHeight  = 25 ;
  cuiBtnWidth   = 85 ;
  cuiImageWidth = 32 ;

procedure TLogErrorForm.FormCreate(Sender: TObject);
begin
  Height      := Screen.Height div 4 * 3 ;
  Width       := Screen.Width  div 4 * 3 ;
  Caption     := ' Application error log - ' + Application.Title ;
  Constraints.MinHeight := 300 ;
  Constraints.MinWidth  := 550 ;
  {$IFDEF MSWINDOWS}
  Image.Picture.Icon.Handle := LoadIcon(0, IDI_ERROR);
  {$ENDIF MSWINDOWS}
  CopyButton.Glyph.LoadFromResourceName( HInstance, 'tiLogCopyToClip' ) ;
end;

procedure TLogErrorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MemoLog.Lines.Clear ;
end;

procedure TLogErrorForm.FormResize(Sender: TObject);
begin
  Image.SetBounds( cuiBorder,
                    (ClientHeight - cuiImageWidth ) div 2,
                    cuiImageWidth,
                    cuiImageWidth );
  OKButton.SetBounds( (ClientWidth  - cuiBorder*2 - cuiBtnWidth ) div 2,
                      ClientHeight - cuiBorder - cuiBtnHeight,
                      cuiBtnWidth,
                      cuiBtnHeight ) ;
end;

procedure TLogErrorForm.CopyButtonClick(Sender: TObject);
begin
  MemoLog.SelectAll ;
  MemoLog.CopyToClipboard ;
  MemoLog.SelStart := 0 ;
  MemoLog.SelLength := 0 ;
  MessageDlg( 'The error message has been copied to the clipboard.'#13#13 +
              'You can now paste into an email to forward to ' +
              Application.Title + ' support.',
              mtInformation,
              [mbOK],
              0 ) ;
end;

procedure TLogErrorForm.OKButtonClick(Sender: TObject);
begin
  Close ;
end;


constructor TLogToError.Create;
begin
  inherited;
  FForm := TLogErrorForm.Create( nil );
  ThrdLog.Resume;
end;

destructor TLogToError.Destroy;
begin
  FreeAndNil( FForm ) ;
  inherited;
end;

function TLogToError.AcceptEvent( const psDateTime,
                                  psMessage: string;
                                  pSeverity: TtiLogSeverity): boolean;
begin
  result := ( pSeverity = lsError ) ;
end;



procedure TLogToError.WriteToOutput;
begin
  inherited WriteToOutput ;

  if ListWorking.Count = 0 then
    Exit ; //==>

  ThrdLog.tiSynchronize( ShowError ) ;

end ;

procedure TLogToError.ShowError ;
var
  i : integer ;
  lLogEvent : TtiLogEvent ;
  lsMessage : string ;
begin

  if FForm.MemoLog.Lines.Count > 0 then begin
    FForm.MemoLog.Lines.Add( '' ) ;
    FForm.MemoLog.Lines.Add( '* * * * * * * * * * * * * * * * * * * * *' ) ;
    FForm.MemoLog.Lines.Add( '' ) ;
  end ;

  for i := 0 to ListWorking.Count - 1 do begin
    lLogEvent := TtiLogEvent( ListWorking.Items[i] ) ;
    FForm.MemoLog.Lines.Add( 'An error occurred at: ' + lLogEvent.DateTime ) ;
    FForm.MemoLog.Lines.Add( '' ) ;
    lsMessage := tiStrTran( lLogEvent.LogMessage, #10, '' ) ;
    lsMessage := tiStrTran( lsMessage, #13, #13+#10 ) ;
    FForm.MemoLog.Lines.Add( lsMessage ) ;
  end ;
  ListWorking.Clear ;

  FForm.MemoLog.Selstart := 0;
  {$IFDEF MSWINDOWS}
  SendMessage( FForm.MemoLog.handle, em_scrollcaret, 0, 0 );
  {$ENDIF MSWINDOWS}

  if not FForm.Visible then begin
    FForm.Top         := ( Screen.Height - FForm.Height ) div 2 ;
    FForm.Left        := ( Screen.Width  - FForm.Width  ) div 2 ;
    FForm.Visible     := true ;
    FForm.OKButton.SetFocus ;
  end ;

end;


procedure TLogToError.Log(const psDateTime, psThreadID, psMessage: string; pSeverity: TtiLogSeverity);
begin
  if Terminated then
    Exit ; //==>
  inherited log(psDateTime, psThreadID, psMessage, pSeverity);
end;

initialization
var
  lLog : TtiLogToFile ;
begin
  lLog := TtiLogToFile(gLog.RegisterLog( TtiLogToFile )) ;
  lLog.OverwriteOldFile := false ;
  lLog.DateInFileName := true ;
  Log( 'Application <' + ParamStr( 0 ) + '> started' ) ;
  gLog.RegisterLog(TLogToError);

end.
