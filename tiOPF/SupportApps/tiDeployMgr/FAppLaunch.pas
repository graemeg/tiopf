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

  Revision history:
    November 2000, Peter Hinrichsen, Made open source

  Purpose:


  Classes:


  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FAppLaunch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, tiDeployMgr_BOM, ComCtrls, ExtCtrls, tiDeployMgr_Cli ;


type

  // ---------------------------------------------------------------------------
  TFormAppLaunch = class(TForm)
    Timer1: TTimer;
    Animate1: TAnimate;
    mLog: TMemo;
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThread : TThrdRunDownload ;
    procedure DoCloseForm( Sender : TObject ) ;
    procedure SetCommandLineParams(const Value: string);
    procedure SetDatabaseName(const Value: string);
    procedure SetFileGroup(const Value: string);
  public
    property  DatabaseName : string write SetDatabaseName ;
    property  FileGroup    : string write SetFileGroup ;
    property  CommandLineParams : string write SetCommandLineParams ;
  end;

var
  FormAppLaunch: TFormAppLaunch;

implementation
uses
  tiUtils
  ,tiPtnVisMgr
  ,tiDBConnectionPool
  ,tiCompressNone
  ,tiCompressZLib
  ,tiDeployMgr_Srv
  ,tiCommandLineParams
  ;

{$R *.DFM}

// -----------------------------------------------------------------------------
procedure TFormAppLaunch.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true ;
end;

// -----------------------------------------------------------------------------
procedure TFormAppLaunch.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled   := false ;
  Animate1.Active := true ;
  FThread.Resume ;
end;

procedure TFormAppLaunch.DoCloseForm(Sender: TObject);
begin
  Close ;
end;

procedure TFormAppLaunch.FormCreate(Sender: TObject);
begin
  FThread                   := TThrdRunDownload.CreateExt ;
  FThread.MemoLog           := mLog ;
  FThread.DatabaseName      := gCommandLineParams.GetParam( 'd' ) ;
  FThread.DeployApp.AppName := gCommandLineParams.GetParam( 'run' ) ;
  FThread.CommandLineParams := gCommandLineParams.AsString ;
  FThread.OnTerminate       := DoCloseForm ;
end;

procedure TFormAppLaunch.FormDestroy(Sender: TObject);
begin
  FThread.Free ;
end;

procedure TFormAppLaunch.SetCommandLineParams(const Value: string);
begin
  FThread.CommandLineParams := Value ;
end;

procedure TFormAppLaunch.SetDatabaseName(const Value: string);
begin
  FThread.DatabaseName := Value ;
end;

procedure TFormAppLaunch.SetFileGroup(const Value: string);
begin
  FThread.DeployApp.AppName    := Value ;
end;

end.
