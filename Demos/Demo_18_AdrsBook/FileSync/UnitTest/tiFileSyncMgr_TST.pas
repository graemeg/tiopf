unit tiFileSyncMgr_TST;

interface
uses
  TestFramework
  ,tiFileSync_Mgr
  ;

type

  TtiFileSyncMgr_TST = class( TtiFileSyncMgr )
  public
    procedure BuildCopyLists; override ;
    procedure ReadIndexes; override;
  end ;

  TTestFileSyncMgr = class( TTestCase )
  published
    procedure Clear;
    procedure AssignFromFileSyncDir;
  end ;

  TTestFileSyncMgr_Abs = class( TTestCase )
  protected
    FFSM : TtiFileSyncMgr_TST ;
    FSourceReader: string;
    FTargetReader: string;
    FSourceRootDir: string;
    FTargetRootDir: string;
    procedure Setup; override ;
    procedure TearDown; override ;
  published
    procedure Source1_Target_Dir0_File0;   virtual ; // Create the dir, copy the file
    procedure Source1_Target_Dir1_File1;   // Copy the file
    procedure Source1_Target_File1;        // Do nothing
    procedure Source1_Target_File1_Update; // Copy the file - handle file locks
    procedure Source0_Target_File1;        // Delete the file
  end ;

  TTestFileSyncMgr_File2File = class( TTestFileSyncMgr_Abs )
  protected
    procedure Setup; override ;
  end ;

  TTestFileSyncMgr_File2Remote = class( TTestFileSyncMgr_Abs )
  protected
    procedure Setup; override ;
  end ;

  TTestFileSyncMgr_Remote2File = class( TTestFileSyncMgr_Abs )
  protected
    procedure Setup; override ;
  end ;


procedure RegisterTests ;

implementation
uses
  tiFileName_BOM
  ,cFileSync
  ;

procedure RegisterTests ;
begin
  RegisterTest( TTestFileSyncMgr.Suite ) ;
  RegisterTest( TTestFileSyncMgr_File2File.Suite );
  RegisterTest( TTestFileSyncMgr_File2Remote.Suite );
  RegisterTest( TTestFileSyncMgr_Remote2File.Suite );
end ;

{ TtiFileSyncMgr_TST }

procedure TtiFileSyncMgr_TST.BuildCopyLists;
begin
  inherited;
end;


procedure TtiFileSyncMgr_TST.ReadIndexes;
begin
  Assert(false, 'Dont call ReadIndexes while testing from here');
end;

{ TTestFileSyncMgr_Abs }

procedure TTestFileSyncMgr_Abs.Setup;
begin
  inherited;
  FFSM := TtiFileSyncMgr_TST.Create ;
  Assert(FSourceReader<>'', 'FSourceReader=''''');
  Assert(FTargetReader<>'', 'FTargetReader=''''');
  Assert(FSourceRootDir<>'', 'FSourceRootDir=''''');
  Assert(FTargetRootDir<>'', 'FTargetRootDir=''''');
end;

procedure TTestFileSyncMgr_Abs.Source0_Target_File1;
begin

end;

procedure TTestFileSyncMgr_Abs.Source1_Target_Dir0_File0;
var
  lFileName : TtiFileName;
begin
  // Setup Source
  FFSM.SourceFileNames.StartDir := FSourceRootDir;
  lFileName:= TtiFileName.Create ;
  lFileName.PathAndName := FSourceRootDir + '\file.txt';
  FFSM.SourceFileNames.Add(lFileName);

  // Setup target
  FFSM.TargetFileNames.StartDir := FTargetRootDir;

  // Build copy lists
  FFSM.BuildCopyLists;

  // Check results
  CheckEquals( 1, FFSM.CopyFileNames.Count,   'CopyFileNames.Count');
  lFileName := FFSM.CopyFileNames.Items[0].SourceFileName ;
  CheckEquals(FSourceRootDir + '\file.txt', lFileName.PathAndName, 'PathAndName');


  CheckEquals( 0, FFSM.UpdateFileNames.Count, 'UpdateFileNames.Count');
  CheckEquals( 0, FFSM.DeleteFileNames.Count, 'DeleteFileNames.Count');

end;

procedure TTestFileSyncMgr_Abs.Source1_Target_Dir1_File1;
begin

end;

procedure TTestFileSyncMgr_Abs.Source1_Target_File1;
begin

end;

procedure TTestFileSyncMgr_Abs.Source1_Target_File1_Update;
begin

end;

procedure TTestFileSyncMgr_Abs.TearDown;
begin
  FFSM.Free;
  inherited;
end;

{ TTestFileSyncMgr }

procedure TTestFileSyncMgr.AssignFromFileSyncDir;
begin

end;

procedure TTestFileSyncMgr.Clear;
begin

end;

{ TTestFileSyncMgr_File2File }

procedure TTestFileSyncMgr_File2File.Setup;
begin
  FSourceReader  := cgsDiskFiles;
  FTargetReader  := cgsDiskFiles;
  FSourceRootDir := 'c:\temp\source';
  FTargetRootDir := 'c:\temp\target';
  inherited;
end;

{ TTestFileSyncMgr_File2Remote }

procedure TTestFileSyncMgr_File2Remote.Setup;
begin
  FSourceReader  := cgsDiskFiles;
  FTargetReader  := cgsRemote;
  FSourceRootDir := 'c:\temp\source';
  FTargetRootDir := 'target';
  inherited;
end;

{ TTestFileSyncMgr_Remote2File }

procedure TTestFileSyncMgr_Remote2File.Setup;
begin
  FSourceReader  := cgsRemote;
  FTargetReader  := cgsDiskFiles;
  FSourceRootDir := 'source';
  FTargetRootDir := 'c:\temp\target';
  inherited;
end;

end.
