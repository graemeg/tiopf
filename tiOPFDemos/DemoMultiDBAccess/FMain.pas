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
    April 2002, Peter Hinrichsen, Created

  Purpose:
    To demonstrate
      a) How to connect to two (or n) databases at the same time
      b) How to copy data from one database to another


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Adrs_BOM, tiMemoReadOnly ;

type
  TForm2 = class(TForm)
    btnConnectToDB_1: TButton;
    btnConnectToDB_2: TButton;
    btnUnLoadDBConnections: TButton;
    btnViewFAdrs_1: TButton;
    btnViewFAdrs_2: TButton;
    btnReadFromDB_1: TButton;
    btnReadFromDB_2: TButton;
    btnClearDB_2: TButton;
    btnSaveAdrsBook_1ToDB_2: TButton;
    tiMemoReadOnly1: TtiMemoReadOnly;
    procedure FormCreate(Sender: TObject);
    procedure btnViewFAdrs_1Click(Sender: TObject);
    procedure btnViewFAdrs_2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnReadFromDB_1Click(Sender: TObject);
    procedure btnReadFromDB_2Click(Sender: TObject);
    procedure btnConnectToDB_1Click(Sender: TObject);
    procedure btnConnectToDB_2Click(Sender: TObject);
    procedure btnUnLoadDBConnectionsClick(Sender: TObject);
    procedure btnSaveAdrsBook_1ToDB_2Click(Sender: TObject);
    procedure btnClearDB_2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FAdrs1 : TAdrsBook ;
    FAdrs2 : TAdrsBook ;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  tiPersist
  ,cTIPersist
  ,tiPtnVisPerObj_Cli
  ,tiPtnVisPerObj
  ,Adrs_Cli
  ,tiDialogs
  ,tiUtils
  ;

{$R *.DFM}

const
  cDB1 = 'C:\TechInsite\Demos\Data\Adrs.gdb' ;
  cDB2 = 'C:\TechInsite\Demos\Data\Adrs1.gdb' ;
  cUserName = 'SYSDBA' ;
  cPassword = 'masterkey' ;

procedure TForm2.FormCreate(Sender: TObject);
begin
  tiShowMessage(
    'Before using this demo, you will have to:' + Cr(2) +
    '1. Populate the database: ' + cDB1 + Cr +
    ' with test data. You can use the DemoTIPerFramework application to do this.' + Cr(2) +
    '2. Create a duplicate copy of ' + cDB1 + ' named ' + cDB2 + Cr(2) +
    'See the constants defined in FMain if you want to change file name or locations' ) ;

  FAdrs1 := TAdrsBook.Create ;
  FAdrs2 := TAdrsBook.Create ;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FAdrs1.Free ;
  FAdrs2.Free ;
end;

// Connect to DB #1
procedure TForm2.btnConnectToDB_1Click(Sender: TObject);
begin
  gTIPerMgr.LoadDatabaseLayer( cTIPersistIBX,
                               cDB1,
                               cUserName,
                               cPassword );
end;

// Connect to DB #2
procedure TForm2.btnConnectToDB_2Click(Sender: TObject);
begin
  gTIPerMgr.LoadDatabaseLayer( cTIPersistIBX,
                               cDB2,
                               cUserName,
                               cPassword );
end;

// Load DB #1 into FAdrs1
procedure TForm2.btnReadFromDB_1Click(Sender: TObject);
begin
  FAdrs1.Clear ;
  FAdrs1.Read( cDB1 ) ;
end;

// Load DB #2 into FAdrs2
procedure TForm2.btnReadFromDB_2Click(Sender: TObject);
begin
  FAdrs2.Clear ;
  FAdrs2.Read( cDB2 ) ;
end;

// View the contents of FAdrs1
procedure TForm2.btnViewFAdrs_1Click(Sender: TObject);
begin
  tiShowPerObjAbs( FAdrs1, false ) ;
end;

// View the contents of FAdrs2
procedure TForm2.btnViewFAdrs_2Click(Sender: TObject);
begin
  tiShowPerObjAbs( FAdrs2, false ) ;
end;

// Clear DB #2
procedure TForm2.btnClearDB_2Click(Sender: TObject);
begin
  FAdrs2.Deleted := true ;
  FAdrs2.Save( cDB2 ) ;
  FAdrs2.Clear ;
end;

// Save FAdrs1 to DB #2
procedure TForm2.btnSaveAdrsBook_1ToDB_2Click(Sender: TObject);
begin
  // Set the object state of all persisted objects to posCreate
  Adrs_Cli.SetAllToCreate( FAdrs1 ) ;
  FAdrs1.Save( cDB2 ) ;
end;

// Unload the databases
procedure TForm2.btnUnLoadDBConnectionsClick(Sender: TObject);
begin
  gTIPerMgr.UnLoadDatabaseLayer( cTIPersistIBX, cDB1 ) ;
  gTIPerMgr.UnLoadDatabaseLayer( cTIPersistIBX, cDB2 ) ;
end;

// Confirm the databases have been closed because you will get an
// AV on shutdown if they are not.
procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false ;
  if gTIPerMgr.DefaultPerLayer.DBConnectionPools.IsConnected( cDB1 ) then
  begin
    tiAppError( 'You can not close the application because ' + Cr + '<' +
                cDB1 + '>' + Cr + 'is still connected.' ) ;
    Exit ; //==>
  end ;

  if gTIPerMgr.DefaultPerLayer.DBConnectionPools.IsConnected( cDB2 ) then
  begin
    tiAppError( 'You can not close the application because ' + Cr + '<' +
                cDB2 + '>' + Cr + 'is still connected.' ) ;
    Exit ; //==>
  end ;

  CanClose := true ;

end;

end.
