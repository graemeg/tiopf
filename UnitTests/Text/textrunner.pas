{
  My custom text test runner application. This one actually works even if I
  have the DB persistance layers enabled.

  Author: Graeme Geldenhuys (2007-06-20)
}
program textrunner;

{$mode objfpc}
{$h+}

uses
  cthreads, custapp, Classes, SysUtils, fpcunit, testutils, testregistry,
  testdecorator, tiTestDependencies, tiOPFTestManager, xmlwrite,
  xmltestreport, plaintestreport, tiDUnitINI, tiOPFManager;


const
  ShortOpts = 'alhp';
  Longopts: Array[1..6] of String = (
    'all','list','format:','suite:','help', 'progress');
  Version = 'Version 0.3';


type
  TProgressWriter= class(TNoRefCountObject, ITestListener)
  private
    FSuccess: boolean;
  protected
    { ITestListener interface requirements }
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  public
    destructor Destroy; override;
  end;



  TTestRunner = Class(TCustomApplication)
  private
    FXMLResultsWriter: TXMLResultsWriter;
    FPlainResultsWriter: TPlainResultsWriter;
    progressWriter: TProgressWriter;
    FShowProgress: Boolean;
    procedure   DisplayTests(ATests: TTestSuite);
    procedure   DisplayTestSuites(ATests: TTestSuite);
    procedure   RunTestSuite(ASuiteName: string; ATests: TTestSuite);
  protected
    procedure   DoRun ; Override;
    procedure   doTestRun(aTest: TTest); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


destructor TProgressWriter.Destroy;
begin
  // on descruction, just write the missing line ending
  writeln;
  inherited Destroy;
end;

procedure TProgressWriter.AddFailure(ATest: TTest; AFailure: TTestFailure);
begin
  FSuccess := false;
  writeln('  FAILED');
end;

procedure TProgressWriter.AddError(ATest: TTest; AError: TTestFailure);
begin
  FSuccess := false;
  writeln('  ERROR');
end;

procedure TProgressWriter.StartTest(ATest: TTest);
begin
  FSuccess := true; // assume success, until proven otherwise
  Write(ATest.TestName + ': ...');
end;

procedure TProgressWriter.EndTest(ATest: TTest);
begin
  if FSuccess then
    writeln('  DONE');
end;

procedure TProgressWriter.StartTestSuite(ATestSuite: TTestSuite);
begin
  writeln('');
  writeln(ATestSuite.TestName + ':');
  // do nothing
end;

procedure TProgressWriter.EndTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;


constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLResultsWriter := TXMLResultsWriter.Create(nil);
  FPlainResultsWriter := TPlainResultsWriter.Create(nil);
  FShowProgress := False;
end;


destructor TTestRunner.Destroy;
begin
  FXMLResultsWriter.Free;
  FPlainResultsWriter.Free;
  inherited Destroy;
end;

procedure TTestRunner.doTestRun(aTest: TTest);
var
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  try
    if FShowProgress then
    begin
      progressWriter := TProgressWriter.Create;
      testResult.AddListener(progressWriter);
    end;

    if HasOption('format') and (GetOptionValue('format') = 'plain') then
      testResult.AddListener(FPlainResultsWriter)
    else if HasOption('format') and (GetOptionValue('format') = 'xml') then
      testResult.AddListener(FXMLResultsWriter);

    aTest.Run(testResult);
    if HasOption('format') and (GetOptionValue('format') = 'plain') then
      FPlainResultsWriter.WriteResult(testResult)
    else if HasOption('format') and (GetOptionValue('format') = 'xml') then
    begin
      FXMLResultsWriter.WriteResult(testResult);
      WriteXMLFile(FXMLResultsWriter.Document, 'results.xml');
    end;
  finally
    if FShowProgress then
      progressWriter.Free;
    testResult.Free;
  end;
end;

procedure TTestRunner.DisplayTests(ATests: TTestSuite);
var
  i: integer;
begin
  for i := 0 to ATests.Tests.Count-1 do
  begin
    if (ATests[i] is TTest) and not (ATests[i] is TTestSuite) then
      Writeln(ATests[i].TestSuiteName + '.' + ATests[i].TestName);
    if ATests[i] is TTestSuite then
      DisplayTests(TTestSuite(ATests[i]));
  end;
end;

procedure TTestRunner.DisplayTestSuites(ATests: TTestSuite);
var
  i: integer;
begin
  for i := 0 to ATests.Tests.Count-1 do
  begin
    if not (ATests[i] is TTestDecorator) and (ATests[i] is TTestSuite) then
      Writeln(ATests[i].TestName);
    if ATests[i] is TTestSuite then
      DisplayTests(TTestSuite(ATests[i]));
  end;
end;

procedure TTestRunner.RunTestSuite(ASuiteName: string; ATests: TTestSuite);
var
  i: integer;
begin
  for i := 0 to ATests.Tests.Count-1 do
  begin
    if ATests[i].TestName = ASuiteName then
    begin
      doTestRun(ATests[i]);
      break;
    end
    else
    begin
      if ATests[i] is TTestSuite then
        RunTestSuite(ASuiteName, TTestSuite(ATests[i]));
    end;
  end;
end;

procedure TTestRunner.DoRun;
var
  i: Integer;
  S: String;
begin
  S:=CheckOptions(ShortOpts,LongOpts);
  If (S<>'') then
    Writeln(S);
  if HasOption('h', 'help') or (ParamCount = 0) then
  begin
    writeln(Title);
    writeln(Version);
    writeln('Usage: ');
    writeln('-l or --list to show a list of registered tests');
    writeln('default format is xml, add --format=[latex,plain] to output in other formats');
    writeln('-a or --all to run all the tests and show the results in xml format');
    writeln('The results can be redirected to an xml file,');
    writeln('for example: ./testrunner --all > results.xml');
    writeln('use --suite=MyTestSuiteName to run only the tests in a single test suite class');
    writeln('-p or --progress   Shows progress as tests are run.');
    writeln('');
    Terminate;
  end;

  if HasOption('l', 'list') then
  begin
    DisplayTests(GetTestRegistry);
  end;

  if HasOption('p', 'progress') then
  begin
    FShowProgress := True;
  end;

  { show what persistence layers will be tested}
  writeln(' ');
  writeln('Persistence layers to be tested:');
  for i := 0 to GTIOPFManager.PersistenceLayers.Count - 1 do
    writeln(#9 + GTIOPFManager.PersistenceLayers.Items[i].PersistenceLayerName);

  if HasOption('a', 'all') then
  begin
    doTestRun(GetTestRegistry)
  end
  else
    if HasOption('suite') then
    begin
      S := '';
      S := GetOptionValue('suite');
      if S = '' then
        DisplayTestSuites(GetTestRegistry)
//        for I := 0 to GetTestRegistry.Tests.count - 1 do
//          writeln(GetTestRegistry[i].TestName)
      else
        RunTestSuite(S, GetTestRegistry);
      //for I := 0 to GetTestRegistry.Tests.count - 1 do
        //if GetTestRegistry[i].TestName = S then
        //begin
          //doTestRun(GetTestRegistry[i]);
        //end;
    end;
    
  writeln('');
  Terminate;
end;


var
  App: TTestRunner;

begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'tiOPF Console Test runner.';

  GTIOPFTestManager.Read;
  GTIOPFTestManager.UnloadPersistenceLayersNotSelected;
  GTIOPFTestManager.DeleteDatabaseFiles;
  tiTestDependencies.tiRegisterTests;

  App.Run;
  App.Free;
end.

