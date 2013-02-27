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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiLog, StdCtrls, Spin;

type

  TthrdLogTest = class( TThread )
  private
    FiTarget   : integer ;
    FiThreadNo : integer ;
  public
    constructor CreateExt( piThreadNo, piTarget : integer ) ;
    procedure   Execute ; override ;
  end ;

  TthrdLogError = class( TThread )
  public
    constructor CreateExt ;
    procedure   Execute ; override ;
  end ;


  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    seLog: TSpinEdit;
    seThreads: TSpinEdit;
    Memo1: TMemo;
    btnLogError: TButton;
    btnViewLogFile: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnLogErrorClick(Sender: TObject);
    procedure btnViewLogFileClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  tiUtils
  ;
  
{$R *.DFM}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TthrdLogTest
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TthrdLogTest.CreateExt( piThreadNo, piTarget: integer);
begin
  Create( true ) ;
  FreeOnTerminate := true ;
  FiThreadNo := piThreadNo ;
  FiTarget   := piTarget ;
  Resume ;
end;

procedure TthrdLogTest.Execute;
var
  i : integer ;
begin

  for i := 1 to FiTarget do begin
    Log( 'Thread: ' + IntToStr( FiThreadNo ) +
         ' Iteration: ' + IntToStr( i )) ;
    Sleep( 10 ) ;
    if Terminated then
      Exit ; //==>
  end ;
  if not Terminated then
    Log( 'Thread: ' +
         IntToStr( FiThreadNo ) +
         ' has finished' ) ;

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i : integer ;
begin
  for i := 1 to seThreads.Value do
  begin
    TthrdLogTest.CreateExt( i, seLog.Value ) ;
    sleep( 150 ) ;
  end ;
  Log( 'Finished creating ' + IntToStr( seThreads.Value ) +
       ' threads' ) ;

end;

procedure TForm1.btnLogErrorClick(Sender: TObject);
begin
  TthrdLogError.CreateExt ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TthrdLogError
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TthrdLogError.CreateExt;
begin
  Create( true ) ;
  FreeOnTerminate := true ;
  resume ;
end;

procedure TthrdLogError.Execute;
begin
  LogError( 'An error was logged.' ) ;
  LogError( 'Another error was %s.', ['logged']) ;
end;

procedure TForm1.btnViewLogFileClick(Sender: TObject);
begin
  tiEditFile( gLog.LogFileName ) ;
end;

end.



