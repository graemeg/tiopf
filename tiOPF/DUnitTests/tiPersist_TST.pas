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

unit tiPersist_TST;

interface
uses
  Classes
  ,tstPerFramework_BOM
  ,tiPersistAbs_TST
  ;

const
  cRepeatCount = 10 ;

type

  TTestTIPerMgrAbs = class( TtiPerTestCase )
  protected
    procedure   CreateDBIfNotExists ; override ;
    procedure   TearDown ; override ;
  published
    constructor Create(MethodName: string); override ;
    procedure   LoadPersistenceLayer ;
    procedure   LoadPerFramework ;
    procedure   LoadDatabaseLayer ;
  end ;

procedure RegisterTests ;

implementation
uses
  tiCommandLineParams
  ,tiDBConnectionPool
  ,SysUtils
  ,tiPersist
  ,Windows
  ,Contnrs
  ,tiLog
  ,tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,Forms
  ,TestFrameWork
  ,ctiPersist
  ,tiQuery
  ,tiDBConnectionSetupAbs_TST
  ,tiDUnitDependencies
  ;

procedure RegisterTests ;
begin
  tiDUnitDependencies.RegisterDBTests( 'TIPerMgr', TTestTIPerMgrAbs );
end ;

{ TTestTIPerMgrAbs }

constructor TTestTIPerMgrAbs.Create(MethodName: string);
begin
  inherited;
  // Dont want any SetupTasks to run
end;

procedure TTestTIPerMgrAbs.CreateDBIfNotExists;
begin
  Assert( PerFrameworkSetup <> nil, 'PerFrameworkSetup not assigned' ) ;
  gTIPerMgr.LoadPersistenceFramework(PerFrameworkSetup.PerLayerName) ;
  try
    inherited;
  finally
    gTIPerMgr.UnLoadPersistenceFramework ;
  end;
end;

procedure TTestTIPerMgrAbs.LoadDatabaseLayer;
var
  i : integer ;
begin
  Assert( PerFrameworkSetup <> nil, 'PerFrameworkSetup not assigned' ) ;
  if PerFrameworkSetup.CanCreateDatabase then
    CreateDBIfNotExists;
  for i := 1 to cRepeatCount do                     
  begin                                                   
    gTIPerMgr.LoadPersistenceFramework(PerFrameworkSetup.PerLayerName) ;
    try
      CheckNotNull( gTIPerMgr.DefaultPerLayer, 'DefaultPerLayer' ) ;
      CheckEquals(  PerFrameworkSetup.PerLayerName, gTIPerMgr.DefaultPerLayerName, 'DefaultPerLayerName' ) ;
      CheckNull(    gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
      CheckEquals(  '', gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
      gTIPerMgr.LoadDatabaseLayer(
        PerFrameworkSetup.PerLayerName,
        PerFrameworkSetup.DBName,
        PerFrameworkSetup.Username,
        PerFrameworkSetup.Password ) ;
      try
        CheckNotNull( gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
        CheckEquals( PerFrameworkSetup.DBName, gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
      finally
        gTIPerMgr.UnLoadDatabaseLayer(
          PerFrameworkSetup.PerLayerName,
          PerFrameworkSetup.DBName ) ;
      end;
      CheckNull(   gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
      CheckEquals( '', gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
    finally
      gTIPerMgr.UnLoadPersistenceFramework(PerFrameworkSetup.PerLayerName) ;
    end;
    CheckNull(   gTIPerMgr.DefaultPerLayer, 'DefaultPerLayer' ) ;
    CheckEquals( '', gTIPerMgr.DefaultPerLayerName, 'DefaultPerLayerName' ) ;
    CheckNull(   gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
    CheckEquals( '', gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
  end ;
end;

procedure TTestTIPerMgrAbs.LoadPerFramework;
var
  i : integer ;
begin
  Assert( PerFrameworkSetup <> nil, 'PerFrameworkSetup not assigned' ) ;
  if PerFrameworkSetup.CanCreateDatabase then
    CreateDBIfNotExists;

  for i := 1 to cRepeatCount do
  begin
    gTIPerMgr.LoadPersistenceFramework(
      PerFrameworkSetup.PerLayerName,
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;
    try
      CheckNotNull( gTIPerMgr.DefaultPerLayer, 'DefaultPerLayer' ) ;
      CheckEquals( PerFrameworkSetup.PerLayerName, gTIPerMgr.DefaultPerLayerName, 'DefaultPerLayerName' ) ;
      CheckNotNull( gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
      CheckEquals( PerFrameworkSetup.DBName, gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
    finally
      gTIPerMgr.UnLoadPersistenceFramework(PerFrameworkSetup.PerLayerName) ;
    end;
    CheckNull( gTIPerMgr.DefaultPerLayer, 'DefaultPerLayer' ) ;
    CheckEquals( '', gTIPerMgr.DefaultPerLayerName, 'DefaultPerLayerName' ) ;
    CheckNull( gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
    CheckEquals( '', gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
  end; 
end;

procedure TTestTIPerMgrAbs.LoadPersistenceLayer;
var
  i : integer ;
begin
  Assert( PerFrameworkSetup <> nil, 'PerFrameworkSetup not assigned' ) ;
  for i := 1 to cRepeatCount do
  begin
    Log( 'LoadPersistenceLayer test #' + IntToStr( i ));
    gTIPerMgr.LoadPersistenceFramework(PerFrameworkSetup.PerLayerName) ;
    try
      CheckNotNull( gTIPerMgr.DefaultPerLayer, 'DefaultPerLayer' ) ;
      CheckEquals(  PerFrameworkSetup.PerLayerName, gTIPerMgr.DefaultPerLayerName, 'DefaultPerLayerName' ) ;
      CheckNull(    gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
      CheckEquals(  '', gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
    finally
      gTIPerMgr.UnLoadPersistenceFramework(PerFrameworkSetup.PerLayerName) ;     
    end;
    CheckNull(   gTIPerMgr.DefaultPerLayer, 'DefaultPerLayer' ) ;
    CheckEquals( '', gTIPerMgr.DefaultPerLayerName, 'DefaultPerLayerName' ) ;
    CheckNull(   gTIPerMgr.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
    CheckEquals( '', gTIPerMgr.DefaultDBConnectionName, 'DefaultDBConnectionName' ) ;
  end;
end;

procedure TTestTIPerMgrAbs.TearDown;
var
  i : integer ;
begin
  for i := gPerFrameworkSetupFactory.Count - 1 downto 0 do
  begin
    if gTIPerMgr.RegPerLayers.IsLoaded(gPerFrameworkSetupFactory.Items[i].PerLayerName) then
      gTIPerMgr.RegPerLayers.UnLoadPersistenceLayer(gPerFrameworkSetupFactory.Items[i].PerLayerName);
  end ;
end;

end.
