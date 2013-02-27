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

unit tiDBConnectionSetupAbs_TST;

interface                         
uses
  Classes  // needed for TStringList
  ,tstPerFramework_BOM
  ,tiPtnVisPerObj
  ,TestFramework
  ,INIFiles
  ;

type

  TTestPerFrameworkSetupFactory = class ;
  TPerFrameworkSetup            = class ;

  TPerFrameworkSetup = class( TPerObjAbs )
  private
    FPerLayerName : string ;
    FDBName       : string ;
    FUsername     : string ;
    FPassword     : string ;
    FCanCreateDatabase: boolean;
    function  GetSelected: boolean;
    function  GetToRun: boolean;
    procedure SetSelected(const Value: boolean);
    function  GetEnabled: boolean;
  protected
    // Gives you the chance to override default database, username
    // and password values for the unit tests.
    function  ReadFromReg( const pPerLayer, pProp, pDefault : string ) : string ;
  public
    destructor  destroy ; override ;
    property    PerLayerName  : string read FPerLayerName  write FPerLayerName  ;
    property    DBName        : string read FDBName        write FDBName        ;
    property    Username      : string read FUsername      write FUsername      ;
    property    Password      : string read FPassword      write FPassword      ;
    property    CanCreateDatabase : boolean read FCanCreateDatabase write FCanCreateDatabase ;
    procedure   ForceTestDataDirectory ;
    property    Enabled       : boolean read GetEnabled                         ;
    property    Selected      : boolean read GetSelected   write SetSelected    ;
    property    ToRun         : boolean read GetToRun;
    procedure   DoRunOnce ; virtual ;
  end ;

  TTestPersistenceLayers = ( tplOneAtTheTime, tplAllAtSameTime );

  TTestPerFrameworkSetupFactory = class( TPerObjList )
  private
    FTestAll: boolean;
    function  GetTestNonPersistentClasses: boolean;
    procedure SetTestNonPersistentClasses(const Value: boolean);
    function  GetTestPersistenceLayers: TTestPersistenceLayers;
    procedure SetTestPersistenceLayers( const Value: TTestPersistenceLayers);
  protected
    function    GetItems(i: integer):TPerFrameworkSetup ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TPerFrameworkSetup); reintroduce ;
  public
    constructor Create ; override ;
    property    TestNonPersistentClasses : boolean read GetTestNonPersistentClasses write SetTestNonPersistentClasses ;
    property    TestPersistenceLayers    : TTestPersistenceLayers read GetTestPersistenceLayers write SetTestPersistenceLayers ;
    function    ToRun(const pClassID : string):boolean;
    property    TestAll : boolean read FTestAll  ;
    property    Items[i:integer] : TPerFrameworkSetup read GetItems write SetItems ;
    procedure   Add( pObject : TPerFrameworkSetup   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    function    IsRegistered( const pClassID : string ) : boolean ;
    function    FindByClassID(const pClassID : string) : TPerFrameworkSetup ;
    procedure   DeleteByClassID(const pClassID : string);
    procedure   RunAllOnce ; 
  end;

var
  gTestDataRoot : string ;

implementation
uses
  tiCommandLineParams
  ,tiDBConnectionPool
  ,SysUtils
  ,tiPersist
  ,Windows
  ,Contnrs
  ,tiLog
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,Forms
  ,ctiPersist
  ,tiQuery
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiDUnitINI
  ;

{ TTestPerFrameworkSetupFactory }

procedure TTestPerFrameworkSetupFactory.Add(pObject: TPerFrameworkSetup;pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

constructor TTestPerFrameworkSetupFactory.Create;
begin
  inherited;
  FTestAll := FindCmdLineSwitch('All', ['-', '/'], true) ;
end;

procedure TTestPerFrameworkSetupFactory.DeleteByClassID(const pClassID: string);
var
  lData : TPerFrameworkSetup;
begin
  lData := FindByClassID(pClassID);
  Remove(lData);
end;

function TTestPerFrameworkSetupFactory.FindByClassID(
  const pClassID: string): TPerFrameworkSetup;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].PerLayerName, pClassID ) then
    begin
      result := Items[i];
      Exit ; //==>
    end; 
end;

function TTestPerFrameworkSetupFactory.GetItems(i: integer): TPerFrameworkSetup;
begin
  result := TPerFrameworkSetup( inherited GetItems( i )) ;
end;

function TTestPerFrameworkSetupFactory.GetTestNonPersistentClasses: boolean;
begin
  result := gDUnitINI.ReadBool( 'PerLayersToTest', 'NonPersistent', True ) ;
  result := result or FTestAll ;
end;

function TTestPerFrameworkSetupFactory.GetTestPersistenceLayers: TTestPersistenceLayers;
begin
  result := TTestPersistenceLayers(gDUnitINI.ReadInteger(
     'PerLayersToTest', 'TestPersistenceLayers', 0 )) ;
end;

function TTestPerFrameworkSetupFactory.IsRegistered( const pClassID : string ): boolean;
begin
  result := FindByClassID(pClassID) <> nil ;
end;

procedure TTestPerFrameworkSetupFactory.RunAllOnce;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    Items[i].DoRunOnce;
end;

procedure TTestPerFrameworkSetupFactory.SetItems(i: integer;const Value: TPerFrameworkSetup);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TTestPerFrameworkSetupFactory.SetTestNonPersistentClasses(const Value: boolean);
begin
  gDUnitINI.WriteBool( 'PerLayersToTest', 'NonPersistent', Value ) ;
end;

procedure TTestPerFrameworkSetupFactory.SetTestPersistenceLayers(
  const Value: TTestPersistenceLayers);
begin
  gDUnitINI.WriteInteger( 'PerLayersToTest', 'TestPersistenceLayers', Ord(Value)) ;
end;

function TTestPerFrameworkSetupFactory.ToRun(const pClassID: string): boolean;
var
  lSetup : TPerFrameworkSetup ;
begin
  result := IsRegistered(pClassID);
  result := result or FTestAll ;
  if not result then
    Exit ; //==>
  lSetup := FindByClassID(pClassID) ;
  result := lSetup.Enabled and ( lSetup.Selected or FTestAll ) ;
end;

{ TPerFrameworkSetup }

destructor TPerFrameworkSetup.destroy;
begin
  inherited;
end;

procedure TPerFrameworkSetup.DoRunOnce;
begin
  // Implement in concrete
end;

procedure TPerFrameworkSetup.ForceTestDataDirectory;
var
  lDir : string ;
begin
  if not CanCreateDatabase then
    Exit ; //==>
  lDir := ExtractFilePath( DBName ) ;
  if not DirectoryExists( lDir ) then
    ForceDirectories( lDir ) ;
  if not DirectoryExists( lDir ) then
    raise exception.create( 'Can not create directory <' +
                            lDir +
                            '> called in ' +
                            ClassName + '.Create' ) ;
end;

function TPerFrameworkSetup.GetEnabled: boolean;
begin
  result := gDUnitINI.ReadBool( 'PerLayersEnabled', PerLayerName, true ) ;
end;

function TPerFrameworkSetup.GetSelected: boolean;
begin
  result := gDUnitINI.ReadBool( 'PerLayersToTest', PerLayerName, true ) ;
end;

function TPerFrameworkSetup.GetToRun: boolean;
begin
  result := Selected and Enabled ;
end;

function TPerFrameworkSetup.ReadFromReg(const pPerLayer, pProp, pDefault: string): string;
begin
  result := gDUnitReg.ReadString('DB_' + pPerLayer, pProp, 'Unknown') ;
  if result = 'Unknown' then
  begin
    result := pDefault ;
    gDUnitReg.WriteString('DB_' + pPerlayer, pProp, '');
  end
  else if result = '' then
    result := pDefault ;
end;

procedure TPerFrameworkSetup.SetSelected(const Value: boolean);
begin
  gDUnitINI.WriteBool( 'PerLayersToTest', PerLayerName, Value ) ;
end;

initialization
  gTestDataRoot := '..\Data\Demo' ;

end.
