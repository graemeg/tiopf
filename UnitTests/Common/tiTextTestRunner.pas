{
    TODO:  Add a -h or --help parameter to output all available parameters
           the Text Test Runner can accept.
}
unit tiTextTestRunner;

{$I tiDefines.inc}
{.$I DUnit.inc}  // there is no DUnit.inc

interface
uses
  {$IFDEF DUNIT2}
   TestFrameworkProxyIfaces
  {$ELSE}
   TestFramework
  {$ENDIF}
  ,TextTestRunner
 ;

const
  CCommandLineParamLogTo     = 'logto';   // -logto <Directory to write logs to>
  CCommandLineSummaryINIFile = 'ini';     // -ini <File name to write summary info to>
  CCommandLineParamsNoTests  = 'notests'; // -notests If this param is included, don't run any tests, just write the logs
  CFileNameShort             = 'DUnitReportShort%s.txt';
  CFileNameLong              = 'DUnitReportLong%s.htm';
  CCommandLineParamProgress  = 'progress';  { very basic progress output }
  CCommandLineParamXML       = 'xml';       { outputs test results to a XML file for further processing with XSLT }
  CCommandLineParamSilent    = 'silent';    { doesn't register the summary . (dot) progress output }


type
  {$IFDEF DUNIT2}
    ITest = ITestProxy;
  {$ENDIF}

  TtiTextListenerWriteItems = (tlwtFile, tlwtConsole);
  TtiTextListenerWriteTo = set of TtiTextListenerWriteItems;

  TtiTextTestListener = class(TTextTestListener)
  private
    FDelphiVersion: string;
    FPos : integer;
    FFileNameShort : string;
    FFileNameLong : string;
    FFileNameINI: string;
  protected
    procedure Write2File(const AFileName: string; const AStr : string);
    procedure Write2Short(const AStr : string; pWriteTo: TtiTextListenerWriteTo);
    procedure WriteLn2Short(const AStr : string; pWriteTo: TtiTextListenerWriteTo );
    procedure Write2Long(const AStr : string = '');
    procedure Write2Table(const pCell1, pCell2, pCell3, pCell4, pCell5 : string); overload;
    procedure Write2Table(const pCell1 : string); overload;
    procedure IncPos;
    function  PrintHeader(r: ITestResult): string; override;
    procedure WriteSummaryToINIFile(testResult: ITestResult);
    function  FormatTestName(ATest: ITest): string;
    function  ShouldRunTest(const ATest :ITest):boolean; override;
    procedure TestingStarts; override;
    procedure StartTest(test: ITest); override;

    procedure AddSuccess(test: ITest); override;
    procedure AddError(error: TTestFailure); override;
    procedure AddFailure(failure: TTestFailure); override;
    procedure AddWarning(AWarning: TTestFailure); override;

    procedure TestingEnds(testResult: ITestResult);override;
    procedure StartSuite(suite: ITest); override;
    procedure EndSuite(suite: ITest); override;
    procedure EndTest(test: ITest); override;
  public
    constructor Create; virtual;
  end;

function  tiRunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): ITestResult; overload;
function  RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): ITestResult; overload;
procedure WriteEmptyLogs(AExitBehavior: TRunnerExitBehavior);

// ToDo: We must be able to do better than this...
{.$UNDEF XMLLISTENER}
{$IFNDEF VER130}
  {$IFNDEF VER140}
    {$IFNDEF CLR}
      {$IFNDEF VER210}
        {$DEFINE XMLLISTENER}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

implementation
uses
  {$IFDEF DUNIT2}
  TestFrameworkProxy,
    {$IFDEF XMLLISTENER}
      XMLListener,
    {$ENDIF}
  {$ENDIF}
  tiUtils
  ,tiConstants
  ,tiCommandLineParams
  ,tiTestFramework
  ,tiOPFManager
  ,tiOPFTestManager
  // Delphi
  ,Classes
  ,SysUtils
  ,INIFiles
  ,textprogressrunner
 ;

const
  cMaxTextWidth = 70;   //Well that gives 69 dots per line


procedure Pause;
begin
  WriteLn('Press <RETURN> to continue.');
  ReadLn;
end;

function tiRunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): ITestResult;
var
  aListeners: array of ITestListener;
  i: integer;
begin
  Result := nil;
  try
    if Suite = nil then
      Writeln('No tests registered')
    else
    begin
      Suite.LoadConfiguration(ExtractFilePath(ParamStr(0)) + 'dunit.ini', False, True);
      Suite.FailsOnNoChecksExecuted:= false; // Suppress empty tests in the TextTestRunner
      i := 0;
      { more verbose progress output }
      if gCommandLineParams.IsParam(CCommandLineParamProgress) then
      begin
        inc(i);
        SetLength(aListeners, i);
        aListeners[i-1] := TTextProgressTestListener.Create;
      end;
      { We have this default listener, unless told to be silent }
      if not gCommandLineParams.IsParam(CCommandLineParamSilent) then
      begin
        inc(i);
        SetLength(aListeners, i);
        aListeners[i-1] := TtiTextTestListener.Create;
      end;
      { output test report to XML }
      if gCommandLineParams.IsParam(CCommandLineParamXML) then
      begin
        inc(i);
        SetLength(aListeners, i);
        aListeners[i-1] := TXMLListener.Create(ParamStr(0))
      end;
      try
        Result := RunTest(Suite, aListeners);
      finally
        {$IFDEF DUNIT2}
          Result.ReleaseListeners; // We need the XMLListener to close now
        {$ENDIF}
      end;
    end;
  finally
    {$IFDEF DUNIT2}
      if Assigned(Suite) then
        Suite.ReleaseTests;
    {$ENDIF}

    case exitBehavior of
      rxbPause:
        try
          Pause;
        except
        end;
      rxbHaltOnFailures:
        with Result do
        begin
          result:= nil;
          if not WasSuccessful then
            System.Halt(ErrorCount+FailureCount);
       end;
     end;
  end;
end;

function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): ITestResult;
begin
  Result := tiRunTest(RegisteredTests, exitBehavior);
end;

procedure WriteEmptyLogs(AExitBehavior: TRunnerExitBehavior);
var
  LTestListner: TtiTextTestListener;
  LTestResults: ITestResult;
begin
  WriteLn('The command line parameter -notests was passed, so no tests where run.');
  {$IFDEF DUNIT2}
    LTestResults:= GetTestResult;
  {$ELSE}
   LTestResults:= TTestResult.Create;
  {$ENDIF}
  LTestListner:= TtiTextTestListener.Create;
  try
    LTestListner.TestingStarts;
    LTestListner.TestingEnds(LTestResults);
  finally
    {$IFNDEF DUNIT2} LTestListner.Free; {$ENDIF}
    LTestListner := nil;
  end;
  Assert(Assigned(LTestListner), 'LTestListener should be nil');
  if AExitBehavior = rxbPause then
    Pause;
end;

{ TtiTextTestListener }

procedure TtiTextTestListener.AddError(error: TTestFailure);
begin
  inherited;
  Write2Short('E', [tlwtFile]);
  IncPos;
  Write2Table(
    FormatTestName(error.FailedTest)
   ,'<font color="#FF0000">ERROR</font>'
   ,error.ThrownExceptionName
   ,error.ThrownExceptionMessage
   ,'');

end;

procedure TtiTextTestListener.AddFailure(failure: TTestFailure);
begin
  inherited;
  Write2Short('F', [tlwtFile]);
  IncPos;
  Write2Table(
     FormatTestName(failure.FailedTest)
    ,'<font color="#FF00FF">FAILS</font>'
    ,failure.ThrownExceptionName
    ,failure.ThrownExceptionMessage
    ,'');

end;

procedure TtiTextTestListener.AddWarning(AWarning: TTestFailure);
begin
  Warnings.Add(AWarning); // ToDo: Convert to a warning
  Write2Short('W', [tlwtFile]);
  IncPos;
  Write2Table(
     FormatTestName(AWarning.FailedTest)
    ,'<font color="#FF00FF">WARN</font>'
    ,AWarning.ThrownExceptionName
    ,AWarning.ThrownExceptionMessage
    ,'');
end;

constructor TtiTextTestListener.Create;
var
  LReportDir : string;
begin
  inherited;
  FDelphiVersion := cPackageSuffix;
  if FDelphiVersion = '' then
    FDelphiVersion := '50';

  LReportDir := gCommandLineParams.GetParam(CCommandLineParamLogTo);
  if LReportDir = '' then
    LReportDir := tiGetTempDir;
  LReportDir:= ExpandFileName(LReportDir);

  if Pos('.', LReportDir) <> 0 then
    LReportDir := ExtractFilePath(LReportDir);
  LReportDir := tiAddTrailingSlash(LReportDir);
  FFileNameShort := LReportDir + Format(CFileNameShort, [FDelphiVersion]);
  FFileNameLong := LReportDir + Format(CFileNameLong, [FDelphiVersion]);
  if gCommandLineParams.IsParam(CCommandLineSummaryINIFile) then
  begin
    FFileNameINI := gCommandLineParams.GetParam(CCommandLineSummaryINIFile);
    if ExtractFilePath(FFileNameINI) = '' then
      FFileNameINI := LReportDir + FFileNameINI;
  end
  else
    FFileNameINI  := LReportDir + 'DUnitReportSummary.ini';
  FFileNameINI:= ExpandFileName(FFileNameINI);

  if not DirectoryExists(LReportDir) then
    tiForceDirectories(LReportDir);
  if not DirectoryExists(ExtractFilePath(FFileNameINI)) then
    tiForceDirectories(ExtractFilePath(FFileNameINI));

  if FileExists(FFileNameShort) then
  begin
    tiDeleteFile(FFileNameShort);
    if FileExists(FFileNameShort) then
      raise exception.Create('Unable to delete old log file <' + FFileNameShort + '>');
  end;

  if FileExists(FFileNameLong) then
  begin
    tiDeleteFile(FFileNameLong);
    if FileExists(FFileNameLong) then
      raise exception.Create('Unable to delete old log file <' + FFileNameLong + '>');
  end;

end;

procedure TtiTextTestListener.IncPos;
begin
  Inc(FPos);
  if FPos = cMaxTextWidth then
  begin
    FPos := 1;
    WriteLn2Short('', [tlwtFile, tlwtConsole]);
  end;
end;

function TtiTextTestListener.PrintHeader(r: ITestResult): string;
begin
  result := '';
  if r.wasSuccessful then
  begin
    result := result + tiLineEnd(2);
    Result := Result + PrintSettings(r);
    result := result + format('OK: %d tests'+ tiLineEnd, [r.runCount]);
  end
  else
  begin
    result := result + tiLineEnd(2);
    Result := Result + PrintSettings(r);
    result := result + 'Test Results:'+ tiLineEnd(2);
    result := result + format(
      '  Run:      %8d'+tiLineEnd+
      '  Failures: %8d'+tiLineEnd+
      '  Errors:   %8d'+tiLineEnd+
      '  Warnings: %8d'+tiLineEnd(2),
      [r.runCount, r.failureCount, r.errorCount, r.WarningCount]);
  end
end;

procedure TtiTextTestListener.StartTest(test: ITest);
begin
  write2Short('.', [tlwtFile, tlwtConsole]);
  IncPos;
end;

procedure TtiTextTestListener.TestingStarts;
  procedure _WriteShort(const pDelphiVersion: string);
  var
    i: Integer;
    LSelected: Boolean;
  begin
    writeln2Short('', [tlwtConsole]);
    {$IFDEF DUNIT2}
      writeln2Short('DUnit2 testing of tiOPF ', [tlwtFile, tlwtConsole] );
    {$ELSE}
      writeln2Short('DUnit testing of tiOPF ', [tlwtFile, tlwtConsole]);
    {$ENDIF}
    writeln2Short('Compiler name "' + cCompilerName + '"', [tlwtFile, tlwtConsole]);
    writeln2Short('Compiler version "' + FDelphiVersion + '"', [tlwtFile, tlwtConsole]);
    writeln2Short('Testing started at ' + FormatDateTime(cIntlDateTimeDisp, Now), [tlwtFile, tlwtConsole]);
    writeln2Short('(E = Exception, F = Test failure, W = Warning, x = Test Excluded)', [tlwtFile, tlwtConsole]);
    writeln2Short('', [tlwtFile, tlwtConsole]);
    writeln2Short('Persistence layers to be tested:', [tlwtFile, tlwtConsole]);
    LSelected := False;
    for i:= 0 to GTIOPFTestManager.Count - 1 do
    begin
      if GTIOPFTestManager.Items[i].Selected then
      begin
        LSelected := True;
        writeln2Short('  ' + GTIOPFTestManager.Items[i].PersistenceLayerName, [tlwtFile, tlwtConsole]);
      end;
    end;
    if not LSelected then
      writeln2Short('  None selected',[tlwtFile, tlwtConsole]);
  end;

  procedure _WriteLong(const pDelphiVersion: string);
  var
    i: Integer;
  begin
    write2Long('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">');
    write2Long('<html>');
    write2Long('<head>');
    write2Long('<meta content="text/html; charset=utf-8" http-equiv="content-type">');
    write2Long('<title>tiOPF2 Unit Tests</title>');
    write2Long('</head> <body>');
    {$IFDEF DUNIT2}
      write2Long('<h2>DUnit2 testing of tiOPF (' + cCompilerName + ' ' + pDelphiVersion + ')</h2>');
    {$ELSE}
      write2Long('<h2>DUnit testing of tiOPF (' + cCompilerName + ' ' + pDelphiVersion + ')</h2>');
    {$ENDIF}
    write2Long('<h3>Persistence layers to be tested</h3>' + tiLineEnd);
    write2Long('<table border="1" cellpadding="4" style="font-family: Courier New; font-size: 12px" >' + tiLineEnd);
    for i:= 0 to GTIOPFManager.PersistenceLayers.Count - 1 do
    begin
      write2Long('<tr><td>' + GTIOPFManager.PersistenceLayers.Items[i].PersistenceLayerName + '</td></tr>' + tiLineEnd);
    end;
    Write2Long('</table>');

    write2Long('<h3>Test results</h3>' + tiLineEnd);
    write2Long('<table border="1" cellpadding="4" style="font-family: Courier New; font-size: 12px" >' + tiLineEnd);
    write2Long('  <tr>' + tiLineEnd);
    write2Long('    <td>Test name</td>' + tiLineEnd);
    write2Long('    <td>Failure type</td>' + tiLineEnd);
    write2Long('    <td>Exception</td>' + tiLineEnd);
    write2Long('    <td>Message</td>' + tiLineEnd);
    write2Long('    <td>Run time</td>' + tiLineEnd);
    write2Long('  </tr>' + tiLineEnd);
  end;
begin
  FPos := 1;
  _WriteShort(FDelphiVersion);
  _WriteLong(FDelphiVersion);
  startTime := now;
end;

procedure TtiTextTestListener.Write2Short(const AStr: string; pWriteTo: TtiTextListenerWriteTo);
begin
  if tlwtFile in pWriteTo then
    Write2File(FFileNameShort, AStr);
  if tlwtConsole in pWriteTo then
    System.Write(AStr);
end;

procedure TtiTextTestListener.WriteLn2Short(const AStr: string; pWriteTo: TtiTextListenerWriteTo);
begin
  Write2Short(AStr+ tiLineEnd, pWriteTo);
end;

procedure TtiTextTestListener.TestingEnds(testResult: ITestResult);
var
  h, m, s, l :Word;
begin
  endTime := now;
  runTime := endTime-startTime;
  writeln2Short('', [tlwtFile, tlwtConsole]);
  writeln2Short('', [tlwtFile, tlwtConsole]);
  DecodeTime(runTime, h,  m, s, l);
  writeln2Short(Format('Time to run tests: %d:%2.2d:%2.2d.%3.3d', [h, m, s, l]), [tlwtFile, tlwtConsole]);
  writeln2Short(Report(testResult), [tlwtFile, tlwtConsole]);
  WriteSummaryToINIFile(testResult);

  Write2Long('</table>');
  Write2Long('<p>');

  Write2Long('<h2>Statistics</h2>');

  Write2Long('<table border="1" cellpadding="4" >' + tiLineEnd);
  Write2Long('  <tr>' + tiLineEnd);
  Write2Long('    <td>Tests</td>' + tiLineEnd);
  Write2Long('    <td>'+intToStr(testResult.runCount)+'</td>' + tiLineEnd);
  Write2Long('  </tr>' + tiLineEnd);
  Write2Long('  <tr>' + tiLineEnd);
  Write2Long('    <td>Failures</td>' + tiLineEnd);
  Write2Long('    <td>'+intToStr(testResult.failureCount)+'</td>' + tiLineEnd);
  Write2Long('  </tr>' + tiLineEnd);
  Write2Long('  <tr>' + tiLineEnd);
  Write2Long('    <td>Errors</td>' + tiLineEnd);
  Write2Long('    <td>'+intToStr(testResult.errorCount)+'</td>' + tiLineEnd);
  Write2Long('  </tr>' + tiLineEnd);
  Write2Long('  <tr>' + tiLineEnd);
  Write2Long('    <td>Finished At</td>' + tiLineEnd);
  Write2Long('    <td>'+DateTimeToStr(now)+'</td>' + tiLineEnd);
  Write2Long('  </tr>' + tiLineEnd);
  Write2Long('  <tr>' + tiLineEnd);
  Write2Long('    <td>Runtime:</td>' + tiLineEnd);
  Write2Long(Format('%s%d:%2.2d:%2.2d.%3.3d%s', ['    <td>', h, m, s, l,'</td>' + tiLineEnd]));
  Write2Long('  </tr>' + tiLineEnd);
  Write2Long('  </table>' + tiLineEnd);
  Write2Long('</body> </html>');

end;

procedure TtiTextTestListener.WriteSummaryToINIFile(testResult: ITestResult);
var
  lINIFile : TINIFile;
  LLong : string;
  LShort: string;
  LIdentLong : string;
  LIdentShort : string;
begin
  lINIFile := TINIFile.Create(FFileNameINI);
  try
    LLong :=
       'Run: ' + IntToStr(testResult.RunCount) + ', ' +
       'Warn: ' + IntToStr(testResult.WarningCount) + ', ' +
       'Fail: ' + IntToStr(testResult.FailureCount) + ', ' +
       'Error: ' + IntToStr(testResult.ErrorCount);

    LShort :=
      IntToStr(testResult.FailureCount +
               testResult.ErrorCount +
               testResult.ErrorCount) + '/' +
      IntToStr(testResult.RunCount);

    {$IFDEF DELPHI5}
      LIdentLong := 'report_long_d5';
      LIdentShort := 'report_short_d5';
    {$ENDIF}
    {$IFDEF DELPHI6}
      LIdentLong := 'report_long_d6';
      LIdentShort := 'report_short_d6';
    {$ENDIF}
    {$IFDEF DELPHI7}
      LIdentLong := 'report_long_d7';
      LIdentShort := 'report_short_d7';
    {$ENDIF}
    {$IFDEF DELPHI9}
      LIdentLong := 'report_long_d2005';
      LIdentShort := 'report_short_d2005';
    {$ENDIF}
    {$IFDEF DELPHI10}
      LIdentLong := 'report_long_d2006';
      LIdentShort := 'report_short_d2006';
    {$ENDIF}
    {$IFDEF DELPHI11}
      LIdentLong := 'report_long_d2007';
      LIdentShort := 'report_short_d2007';
    {$ENDIF}
    {$IFDEF FPC}
      LIdentLong := 'report_long_fpc';
      LIdentShort := 'report_short_fpc';
    {$ENDIF}

    if (LIdentLong = '') or (LIdentShort = '') then
      EtiOPFDUnitException.Create('Unknown Delphi or FPC version');
    lINIFile.WriteString('Report_Long',  lIdentLong, LLong);
    lINIFile.WriteString('Report_Short', lIdentShort, LShort);
  finally
    lINIFile.Free;
  end;
end;

procedure TtiTextTestListener.AddSuccess(test: ITest);
begin
  inherited;
  if test.tests.Count<=0 then
  begin
    Write2Table(
       FormatTestName(test)
      ,'<font color="#008000">PASS</font>'
      ,''
      ,''
      ,tiElapsedDHMS(test.ElapsedTestTime));
  end;
end;

procedure TtiTextTestListener.EndSuite(suite: ITest);
begin
  inherited;
  //Write2Long('<p>');
end;

procedure TtiTextTestListener.EndTest(test: ITest);
begin
  inherited;
  // Nothing in XMLTestRunner

end;

function TtiTextTestListener.ShouldRunTest(const ATest: ITest): boolean;
begin
  Result := inherited ShouldRunTest(ATest);
  if not Result then
  begin
    write2Short('x', [tlwtFile]);
    IncPos;
  end;
end;

procedure TtiTextTestListener.StartSuite(suite: ITest);
begin
  inherited;
  Write2Table(suite.getName);
end;

procedure TtiTextTestListener.Write2File(const AFileName, AStr: string);
var
  lBuffer  : PChar;
  lLen    : integer;
  lFileStream : TFileStream;
begin
  if FileExists(AFileName) then
    lFileStream := TFileStream.Create(AFileName,
                                       fmOpenReadWrite or fmShareDenyNone)
  else
    lFileStream := TFileStream.Create(AFileName,
                                       fmCreate or fmShareDenyNone);
  try
    lBuffer := PChar(AStr);
    lLen := length(AStr);
    lFileStream.Seek(0, soFromEnd);
    lFileStream.write(lBuffer^, lLen);
  finally
    lFileStream.Free;
  end;
end;

procedure TtiTextTestListener.Write2Long(const AStr: string);
begin
  Write2File(FFileNameLong, AStr + tiLineEnd);
end;

procedure TtiTextTestListener.Write2Table(const pCell1, pCell2, pCell3, pCell4, pCell5 : string);
var
  lCell1, lCell2, lCell3, lCell4, lCell5 : string;
begin
  if Trim(pCell1) <> '' then lCell1 := pCell1 else lCell1 := '&nbsp;';
  if Trim(pCell2) <> '' then lCell2 := pCell2 else lCell2 := '&nbsp;';
  if Trim(pCell3) <> '' then lCell3 := pCell3 else lCell3 := '&nbsp;';
  if Trim(pCell4) <> '' then lCell4 := pCell4 else lCell4 := '&nbsp;';
  if Trim(pCell5) <> '' then lCell5 := pCell5 else lCell5 := '&nbsp;';

  Write2File(FFileNameLong, '  <tr>' + tiLineEnd);
  Write2File(FFileNameLong, '    <td>' + lCell1 + '</td>' + tiLineEnd);
  Write2File(FFileNameLong, '    <td>' + lCell2 + '</td>' + tiLineEnd);
  Write2File(FFileNameLong, '    <td>' + lCell3 + '</td>' + tiLineEnd);
  Write2File(FFileNameLong, '    <td>' + lCell4 + '</td>' + tiLineEnd);
  Write2File(FFileNameLong, '    <td>' + lCell5 + '</td>' + tiLineEnd);
  Write2File(FFileNameLong, '  </tr>' + tiLineEnd);
end;

procedure TtiTextTestListener.Write2Table(const pCell1 : string);
begin
  Write2File(FFileNameLong, '  <tr>' + tiLineEnd);
  Write2File(FFileNameLong, '    <td  colspan="5">' + pCell1 + '</td>' + tiLineEnd);
  Write2File(FFileNameLong, '  </tr>' + tiLineEnd);
end;

function TtiTextTestListener.FormatTestName(ATest: ITest): string;
begin
  Result:= '&nbsp;&nbsp;' + ATest.GetName;
end;

end.
