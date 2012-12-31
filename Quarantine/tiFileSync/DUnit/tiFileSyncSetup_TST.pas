unit tiFileSyncSetup_TST;

interface
uses
   TestFramework
  ,tiFileSyncSetup_BOM
  ;

type

  TTestFileSyncSetup = class( TTestCase )
  private
    function  CreateTestFileSyncDir( const pValue : string ) : TtiFileSyncDir ;
    procedure UpdateTestFileSyncDir( pData : TtiFileSyncDir ; const pValue : string ) ;
    procedure InsertTestFileSyncDir( const pValue : string ) ;
    procedure CheckTestFileSyncDir( pData : TtiFileSyncDir ; const pValue : string ) ;

    function  CreateTestFileNameFilter( const pValue : string ) : TtiFileNameFilter ;
    procedure UpdateTestFileNameFilter( pData : TtiFileNameFilter ; const pValue : string ) ;
    procedure InsertTestFileNameFilter( const pValue : string ) ;
    procedure CheckTestFileNameFilter( pData : TtiFileNameFilter ; const pValue : string ) ;

  protected
    procedure Setup ; override ;
    procedure TearDown ; override ;
  published
    procedure FileSyncDir_Clone ;
    // procedure FileSyncDir_ReadOne; // Requires 1.098 or higher
    procedure FileSyncDir_ReadList;
    procedure FileSyncDir_Create;
    procedure FileSyncDir_Update;
    procedure FileSyncDir_Delete;

    procedure FileNameFilter_Clone ;
    // procedure FileNameFilter_ReadList ; // Requires 1.098 or higher
    procedure FileNameFilter_ReadList;
    procedure FileNameFilter_Create;
    procedure FileNameFilter_Update;
    procedure FileNameFilter_Delete;

    procedure FileSyncSetup ;
  end ;

procedure RegisterTests ;

const
  cTableNameSyncDir        = 'sync_dir' ;
  cTableNameFileNameFilter = 'file_name_filter' ;

  cOIDFileSyncDir = '1' ;
  cOIDFileNameFilter = '2' ;

implementation
uses
  tiOPFManager
  ,tiObject
  ,SysUtils
  ,tiQuery
  ;

procedure RegisterTests ;
begin
  RegisterTest( TTestFileSyncSetup.Suite );
end ;

procedure TTestFileSyncSetup.CheckTestFileNameFilter(
  pData: TtiFileNameFilter; const pValue: string);
var
  lData : TtiFileNameFilter ;
begin
  lData := CreateTestFileNameFilter(pValue) ;
  try
    CheckEquals( lData.FilterTypeAsString, pData.FilterTypeAsString, 'FilterTypeAsString' );
    Check(       lData.FilterType = pData.FilterType, 'FilterType' );
    CheckEquals( lData.Wildcard, pData.Wildcard, 'Wildcard' );
  finally
    lData.Free;
  end ;
end;

procedure TTestFileSyncSetup.CheckTestFileSyncDir(pData: TtiFileSyncDir;  const pValue: string);
var
  lData : TtiFileSyncDir ;
begin
  lData := CreateTestFileSyncDir(pValue) ;
  try
    CheckEquals( lData.LocalDir, pData.LocalDir);
    CheckEquals( lData.TargetLocation, pData.TargetLocation);
    CheckEquals( lData.SourceReader, pData.SourceReader);
    CheckEquals( lData.TargetReader, pData.TargetReader);
  finally
    lData.Free;
  end ;
end;

function TTestFileSyncSetup.CreateTestFileNameFilter(
  const pValue: string): TtiFileNameFilter;
begin
  result := TtiFileNameFilter.Create ;
  result.OID.AsString := pValue ;
  UpdateTestFileNameFilter( result, pValue ) ;
end;

function TTestFileSyncSetup.CreateTestFileSyncDir( const pValue : string ) : TtiFileSyncDir;
begin
  result := TtiFileSyncDir.Create ;
  result.OID.AsString := pValue ;
  UpdateTestFileSyncDir( result, pValue ) ;
end;

procedure TTestFileSyncSetup.FileNameFilter_Clone;
var
  lFNF1 : TtiFileNameFilter ;
  lFNF2 : TtiFileNameFilter ;
begin
  lFNF1 := TtiFileNameFilter.Create ;
  try
    lFNF1.FilterType := fftInclude ;
    lFNF1.WildCard   := '*.*' ;

    lFNF2 := lFNF1.Clone ;
    try
      Check( lFNF1.Equals( lFNF2 ), 'Clone failed' ) ;
      Check( lFNF2.FilterType = fftInclude ) ;
      CheckEquals( lFNF1.FilterTypeAsString, lFNF2.FilterTypeAsString ) ;
      CheckEquals( '*.*', lFNF2.WildCard  ) ;
    finally
      lFNF2.Free ;
    end;
  finally
    lFNF1.Free ;
  end;
end;

procedure TTestFileSyncSetup.FileNameFilter_Create;
var
  lList  : TtiFileNameFilters ;
  lData  : TtiFileNameFilter ;
begin
  lData := CreateTestFileNameFilter(cOIDFileNameFilter);
  try
    lData.ObjectState := posCreate ;
    lData.Save ;
    Check( lData.ObjectState = posClean ) ;
  finally
    lData.Free ;
  end;
  lList := TtiFileNameFilters.Create ;
  try
    lList.Read ;
    Check( lList.ObjectState = posClean ) ;
    CheckEquals( 1, lList.Count ) ;
    CheckTestFileNameFilter( lList.Items[0], cOIDFileNameFilter );
  finally
    lList.Free ;
  end;
end;

procedure TTestFileSyncSetup.FileNameFilter_Delete;
var
  lList  : TtiFileNameFilters ;
begin
  InsertTestFileNameFilter( cOIDFileNameFilter ) ;
  lList := TtiFileNameFilters.Create ;
  try
    lList.Read ;
    Check( lList.ObjectState = posClean ) ;
    CheckEquals( 1, lList.Count ) ;
    lList.Items[0].Deleted := true ;
    lList.Save ;
    Check( lList.Items[0].ObjectState = posDeleted ) ;
  finally
    lList.Free ;
  end;
  lList := TtiFileNameFilters.Create ;
  try
    lList.Read ;
    Check( lList.ObjectState = posClean ) ;
    CheckEquals( 0, lList.Count ) ;
  finally
    lList.Free ;
  end;
end;

procedure TTestFileSyncSetup.FileNameFilter_ReadList;
var
  lList  : TtiFileNameFilters ;
begin
  InsertTestFileNameFilter( cOIDFileNameFilter ) ;
  lList := TtiFileNameFilters.Create ;
  try
    lList.Read ;
    Check( lList.ObjectState = posClean ) ;
    CheckEquals( 1, lList.Count ) ;
    CheckTestFileNameFilter( lList.Items[0], cOIDFileNameFilter );
  finally
    lList.Free ;
  end;
end;

procedure TTestFileSyncSetup.FileNameFilter_Update;
var
  lList  : TtiFileNameFilters ;
begin
  InsertTestFileNameFilter( cOIDFileNameFilter ) ;
  lList := TtiFileNameFilters.Create ;
  try
    lList.Read ;
    Check( lList.ObjectState = posClean ) ;
    CheckEquals( 1, lList.Count ) ;
    UpdateTestFileNameFilter(lList.Items[0],cOIDFileNameFilter+'1');
    lList.Items[0].Dirty := true ;
    lList.Save ;
    Check( lList.Items[0].ObjectState = posClean ) ;
  finally
    lList.Free ;
  end;
  lList := TtiFileNameFilters.Create ;
  try
    lList.Read ;
    Check( lList.ObjectState = posClean ) ;
    CheckEquals( 1, lList.Count ) ;
    CheckTestFileNameFilter( lList.Items[0], cOIDFileNameFilter+'1' );
  finally
    lList.Free ;
  end;
end;

procedure TTestFileSyncSetup.FileSyncDir_Clone;
var
  lFSD1 : TtiFileSyncDir ;
  lFSD2 : TtiFileSyncDir ;
begin
  lFSD1 := TtiFileSyncDir.Create ;
  try
    lFSD1.LocalDir       := 'c:\temp' ;
    lFSD1.TargetLocation := 'c:\sync' ;
    lFSD1.SourceReader   := 'Disk files' ;
    lFSD1.TargetReader   := 'Disk files' ;

    lFSD2 := lFSD1.Clone ;
    try
      Check( lFSD1.Equals( lFSD2 ), 'Clone failed' ) ;
      CheckEquals( lFSD2.LocalDir,       'c:\temp' ) ;
      CheckEquals( lFSD2.TargetLocation, 'c:\sync' ) ;
      CheckEquals( lFSD2.SourceReader,   'Disk files' ) ;
      CheckEquals( lFSD2.TargetReader,   'Disk files' ) ;
    finally
      lFSD2.Free ;
    end ;
  finally
    lFSD1.Free ;
  end ;
end;

procedure TTestFileSyncSetup.FileSyncDir_Create;
var
  lList   : TtiFileSyncDirs ;
  lData   : TtiFileSyncDir ;
begin
  lList := TtiFileSyncDirs.Create ;
  try
    lData   := CreateTestFileSyncDir(cOIDFileSyncDir);
    lData.ObjectState := posCreate ;
    lList.Add(lData);
    lList.Save ;
    Check( posClean = lData.ObjectState, 'posClean' ) ;
  finally
    lList.Free ;
  end ;

  lList := TtiFileSyncDirs.Create ;
  try
    lList.Read ;
    Check( posClean = lList.ObjectState, 'posClean' ) ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    CheckTestFileSyncDir( lList.Items[0], cOIDFileSyncDir ) ;
  finally
    lList.Free ;
  end ;

end;

procedure TTestFileSyncSetup.FileSyncDir_Delete;
var
  lList   : TtiFileSyncDirs ;
begin
  InsertTestFileSyncDir( cOIDFileSyncDir ) ;
  lList := TtiFileSyncDirs.Create ;
  try
    lList.Read ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    UpdateTestFileSyncDir(lList.Items[0], cOIDFileSyncDir+'1');
    lList.Items[0].Deleted := true ;
    lList.Save ;
    Check( posDeleted = lList.Items[0].ObjectState, 'ObjectState');
  finally
    lList.Free ;
  end ;
  lList := TtiFileSyncDirs.Create ;
  try
    lList.Read ;
    Check( posClean = lList.ObjectState, 'posClean' ) ;
    CheckEquals( 0, lList.Count, 'Count' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestFileSyncSetup.FileSyncDir_ReadList;
var
  lList   : TtiFileSyncDirs ;
begin
  InsertTestFileSyncDir( cOIDFileSyncDir ) ;
  lList := TtiFileSyncDirs.Create ;
  try
    lList.Read ;
    Check( posClean = lList.ObjectState, 'posClean' ) ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    CheckTestFileSyncDir( lList.Items[0], cOIDFileSyncDir ) ;
  finally
    lList.Free ;
  end ;
end;

{
procedure TTestFileSyncSetup.FileSyncDir_ReadOne;
var
  lData   : TtiFileSyncDir ;
begin
  InsertTestFileSyncDir( cOIDFileSyncDir ) ;
  lData := TtiFileSyncDir.Create ;
  try
    lData.Read ;
    Check( posClean = lData.ObjectState, 'posClean' ) ;
    CheckTestFileSyncDir( lData, cOIDFileSyncDir ) ;
  finally
    lData.Free ;
  end ;
end;
}

procedure TTestFileSyncSetup.FileSyncDir_Update;
var
  lList   : TtiFileSyncDirs ;
begin
  InsertTestFileSyncDir( cOIDFileSyncDir ) ;
  lList := TtiFileSyncDirs.Create ;
  try
    lList.Read ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    UpdateTestFileSyncDir(lList.Items[0], cOIDFileSyncDir+'1');
    lList.Items[0].Dirty := true ;
    lList.Save ;
    Check( posClean = lList.Items[0].ObjectState, 'ObjectState');
  finally
    lList.Free ;
  end ;
  lList := TtiFileSyncDirs.Create ;
  try
    lList.Read ;
    Check( posClean = lList.ObjectState, 'posClean' ) ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    CheckTestFileSyncDir( lList.Items[0], cOIDFileSyncDir+'1' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestFileSyncSetup.FileSyncSetup;
var
  lFSS : TtiFileSyncSetup ;
  lFSD : TtiFileSyncDir ;
  lFNF : TtiFileNameFilter ;
begin
  lFSS := TtiFileSyncSetup.Create ;
  try
    lFSD := TtiFileSyncDir.CreateNew ;
    lFSS.FileSyncDirs.Add( lFSD ) ;
    lFSD.LocalDir := 'c:\temp' ;
    lFSD.TargetLocation := 'c:\syncto' ;
    lFSD.SourceReader := 'Disk files' ;
    lFSD.TargetReader := 'Disk files' ;

    lFNF := TtiFileNameFilter.CreateNew ;
    lFSS.FileNameFilters.Add( lFNF ) ;
    lFNF.WildCard := '*.*' ;
    lFNF.FilterType := fftInclude ;
    lFSS.Save ;

  finally
    lFSS.Free ;
  end ;

  lFSS := TtiFileSyncSetup.Create ;
  try
    lFSS.Read ;
    CheckEquals( 1, lFSS.FileSyncDirs.Count ) ;
    CheckEquals( 1, lFSS.FileNameFilters.Count ) ;

    CheckEquals( 'c:\temp',    lFSS.FileSyncDirs.Items[0].LocalDir ) ;
    CheckEquals( 'c:\syncto',  lFSS.FileSyncDirs.Items[0].TargetLocation ) ;
    CheckEquals( 'Disk files', lFSS.FileSyncDirs.Items[0].SourceReader ) ;
    CheckEquals( 'Disk files', lFSS.FileSyncDirs.Items[0].TargetReader ) ;
    CheckEquals( '*.*',    lFSS.FileNameFilters.Items[0].WildCard ) ;
    Check( fftInclude = lFSS.FileNameFilters.Items[0].FilterType ) ;

  finally
    lFSS.Free ;
  end ;

end;

procedure TTestFileSyncSetup.InsertTestFileNameFilter(const pValue: string);
var
  lData : TtiFileNameFilter ;
  lParams : TtiQueryParams ;
begin
  lData := CreateTestFileNameFilter(pValue);
  try
    lParams := TtiQueryParams.Create ;
    try
      lParams.SetValueAsString( 'OID',         lData.OID.AsString ) ;
      lParams.SetValueAsString( 'Filter_Type', lData.FilterTypeAsString ) ;
      lParams.SetValueAsString( 'Wild_Card',   lData.WildCard );
      gTIOPFManager.InsertRow( cTableNameFileNameFilter, lParams ) ;
    finally
      lParams.Free ;
    end;
  finally
    lData.Free ;
  end;
end;

procedure TTestFileSyncSetup.InsertTestFileSyncDir(const pValue: string);
var
  lData : TtiFileSyncDir ;
  lParams : TtiQueryParams ;
begin
  lData := CreateTestFileSyncDir(pValue);
  try
    lParams := TtiQueryParams.Create ;
    try
      lParams.SetValueAsString( 'OID',             lData.OID.AsString)   ;
      lParams.SetValueAsString( 'Local_Dir',       lData.LocalDir)       ;
      lParams.SetValueAsString( 'Target_Location', lData.TargetLocation) ;
      lParams.SetValueAsString( 'Source_Reader',   lData.SourceReader)   ;
      lParams.SetValueAsString( 'Target_Reader',   lData.TargetReader)   ;
      gTIOPFManager.InsertRow( cTableNameSyncDir, lParams ) ;
    finally
      lParams.Free ;
    end;
  finally
    lData.Free ;
  end;
end;

procedure TTestFileSyncSetup.Setup;
begin
  inherited;
  TearDown ;
end;

procedure TTestFileSyncSetup.TearDown;
begin
  inherited;
  gTIOPFManager.DeleteRow( cTableNameSyncDir, nil ) ;
  gTIOPFManager.DeleteRow( cTableNameFileNameFilter, nil ) ;
end;

procedure TTestFileSyncSetup.UpdateTestFileNameFilter(
  pData: TtiFileNameFilter; const pValue: string);
begin
  pData.FilterType := fftInclude ;
  pData.WildCard   := pValue ;
end;

procedure TTestFileSyncSetup.UpdateTestFileSyncDir(pData: TtiFileSyncDir;  const pValue: string);
begin
  pData.LocalDir       := pValue ;
  pData.TargetLocation := pValue ;
  pData.SourceReader   := pValue ;
  pData.TargetReader   := pValue ;
end;

end.
