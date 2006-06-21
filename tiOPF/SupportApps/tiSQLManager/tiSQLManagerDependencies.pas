unit tiSQLManagerDependencies;

interface
uses
   tiQueryXMLLight
  ,tiPerObjOIDInteger
  ;

procedure ConnectToDatabase ;
function  GetSQLMgrFileName: string;

implementation
uses
   tiLog
  ,tiPersist
  ,tiSQLMgr_BOM
  ,tiSQLMgr_Svr
  ,FSplash
  ,tiRegINI
  ,tiDialogs
  ,Forms
  ,SysUtils
  ,tiCommandLineParams
  ,cTIPersist
  ;

// ToDo: Move the SQLManager file name to a global
function GetSQLMgrFileName: string;
begin
  Result := gCommandLineParams.GetParam('f');
end ;

procedure ConnectToDatabase ;
var
  lFileName     : string ;
  lDatabaseName : string ;
  lUserName     : string ;
  lPassword     : string ;
  lPerLayerName : string ;
  i : Integer ;
begin
  // The false parameter will supress the error dialog box. This is necessary
  // because the SQLManager has its own error handling when a query fails.
  // If there are problems in the SQLManager, this param should be set to true
  // to help with the debugging.
  SetupLogForClient( true ) ;

  // Uncomment this line for 'legacy' SQLManager databases
  // This line should be commented out if you have just downloaded the framework
  // and can not get the SQLManager working. If this constant is not commented
  // out, and you are using a 'new' style sql mananer schema, then there will
  // be no sql data loaded because the read query will fail. There will be
  // an entry in the error log descrbing this, but no error dialog which is
  // described in the comment below.
  // If it fails to compile here, turn Assignable Typed Constants ON
  // ToDo: Turn typed constants that are assigned to global vars
  cFieldNameSQLSQL := 'SQL' ;

  tiSQLMgr_BOM.RegisterMappings;

  // Get the persistence layer name (that's not XMLLight)
  for i := 0 to gTIPerMgr.RegPerLayers.Count - 1 do
    if ( gTIPerMgr.RegPerLayers.Items[i].PerLayerName <> cTIPersistXMLLight ) then
    begin
      lPerLayerName := gTIPerMgr.RegPerLayers.Items[i].PerLayerName;
      Break ; //==>
    end ;

  lDatabaseName := gCommandLineParams.GetParam('d');
  lUserName     := gCommandLineParams.GetParam('u');
  lPassword     := gCommandLineParams.GetParam('p');

  gTIPerMgr.DefaultPerLayerName := lPerLayerName ;
  // Connect to the appropriate database using -d -u and -p command line params.
  gTIPerMgr.LoadDatabaseLayer(lPerLayerName,
                              lDatabaseName, lUserName, lPassword);

  // Connect to the SQLManager XML data source
  lFileName := GetSQLMgrFileName ;
  if lFileName = '' then
  begin
    tiAppError('Please specify the SQLManager file name in on the commandline with the -f parameter') ;
    Halt;
  end;

  if not (FileExists(lFileName)) then
  begin
    tiAppError('Can not find the file <' + lFileName + '>') ;
    Halt;
  end ;

  try
    gTIPerMgr.LoadDatabaseLayer(cTIPersistXMLLight,
                                lFileName, 'null', 'null',
                                'readonly=false,optxmldbsize=True')
  except
    on e:Exception do
    begin
      tiAppError('Unable to load SQLManager data <' +
                 lFileName + '> Error message: ' + e.message ) ;
      Halt;
    end;
  end;

  FSplash.ShowSplash ;

end ;

end.
