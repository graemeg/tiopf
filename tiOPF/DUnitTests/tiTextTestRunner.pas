unit tiTextTestRunner;

{$I tiDefines.inc}

interface
uses
  TestFramework
  ,TextTestRunner
  ;

const
  cMaxTextWidth = 70 ;

type
  TtiTextListenerWriteItems = (tlwtFile, tlwtConsole);
  TtiTextListenerWriteTo = set of TtiTextListenerWriteItems;

  TtiTextTestListener = class( TTextTestListener )
  private
    FDelphiVersion: string;
    FPos : integer ;
    FFileNameShort : string ;
    FFileNameLong : string ;
  protected
    procedure Write2File( const pFileName: string ; const pStr : string ) ;
    procedure Write2Short( const pStr : string; pWriteTo: TtiTextListenerWriteTo ) ;
    procedure WriteLn2Short( const pStr : string; pWriteTo: TtiTextListenerWriteTo  ) ;
    procedure Write2Long( const pStr : string = '' ) ;
    procedure Write2Table( const pCell1, pCell2, pCell3, pCell4 : string ) ; overload ;
    procedure Write2Table( const pCell1 : string ) ; overload ;
    procedure IncPos ;
    function  PrintHeader(r: TTestResult): string; override;
    function  PrintFailureItems(r :TTestResult): string; override;
    function  PrintErrorItems(r :TTestResult): string; override;
    procedure WriteSummaryToINIFile(testResult: TTestResult);
  public
    constructor Create ; virtual ;
    procedure   TestingStarts; override ;
    procedure   StartTest(test: ITest); override ;

    procedure   AddSuccess(test: ITest); override;
    procedure   AddError(error: TTestFailure); override;
    procedure   AddFailure(failure: TTestFailure); override;

    procedure   TestingEnds(testResult: TTestResult);override;
    procedure   StartSuite(suite: ITest); override;
    procedure   EndSuite(suite: ITest); override;
    procedure   EndTest(test: ITest); override;
    //procedure   Status(test :ITest; const Msg :string);override;
    //procedure   Warning(test :ITest; const Msg :string);override;

  end;

function tiRunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult; overload;
function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult; overload;

implementation
uses
  SysUtils
  ,tiUtils
  ,Classes
  ,cTIPersist
  ,tiCommandLineParams
  ,INIFiles
  ,tiPersistAbs_TST
  ,tiDialogs
  ;

function tiRunTest(suite: ITest; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := RunTest(suite, [TtiTextTestListener.Create]);
  case exitBehavior of
    rxbPause:
      try
        writeln('Press <RETURN> to continue.');
        readln
      except
      end;
    rxbHaltOnFailures:
      with Result do
      begin
        if not WasSuccessful then
          System.Halt(ErrorCount+FailureCount);
      end
    // else fall through
  end;
end;

function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
   Result := tiRunTest(registeredTests, exitBehavior);
end;

{ TtiTextTestListener }

procedure TtiTextTestListener.AddError(error: TTestFailure);
begin
  Write2Short('E', [tlwtFile, tlwtConsole]);
  IncPos ;
  Write2Table(
    error.FailedTest.GetName
   ,'<font color="#FF0000">ERROR</font>'
   ,error.ThrownExceptionName
   //,error.LocationInfo
   ,error.ThrownExceptionMessage);

end;

procedure TtiTextTestListener.AddFailure(failure: TTestFailure);
begin
  Write2Short('F', [tlwtFile, tlwtConsole]);
  IncPos ;
  Write2Table(
     failure.FailedTest.GetName
    ,'<font color="#FF00FF">FAILS</font>'
    ,failure.ThrownExceptionName
    //,failure.LocationInfo
    ,failure.ThrownExceptionMessage);

end;

constructor TtiTextTestListener.Create;
var
  lReportDir : string ;
begin
  inherited;
  FDelphiVersion := cPackageSuffix ;
  if FDelphiVersion = '' then
    FDelphiVersion := '50' ;

  lReportDir := gCommandLineParams.GetParam('logto') ;
  if lReportDir = '' then
    lReportDir := ParamStr(0);

  if Pos('.', lReportDir) <> 0 then
    lReportDir := ExtractFilePath(lReportDir) ;
  lReportDir := tiAddTrailingSlash(lReportDir) ;
  FFileNameShort := lReportDir + 'DUnitReportShort' + FDelphiVersion + '.htm';
  FFileNameLong  := lReportDir + 'DUnitReportLong' + FDelphiVersion + '.htm';

  if FileExists(FFileNameShort) then           
  begin
    DeleteFile(FFileNameShort);
    if FileExists(FFileNameShort) then
      raise exception.create('Unable to delete old log file <' + FFileNameShort + '>' ) ;
  end;

  if FileExists(FFileNameLong) then
  begin
    DeleteFile(FFileNameLong);
    if FileExists(FFileNameLong) then
      raise exception.create('Unable to delete old log file <' + FFileNameLong + '>' ) ;
  end;

end;

procedure TtiTextTestListener.IncPos;
begin
  Inc( FPos ) ;
  if FPos = cMaxTextWidth then
  begin
    FPos := 1 ;
    WriteLn2Short('', [tlwtFile, tlwtConsole]);
  end;
end;

function TtiTextTestListener.PrintErrorItems(r: TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.ErrorCount-1 do begin
    failure := r.Errors[i];
    result := result + format('%3d) %s  %s'#13#10'          "%s"',
                               [
                               i+1,
                               tiPadR( failure.failedTest.name, 20 ),
                               tiPadR( failure.thrownExceptionName, 20 ),
                               failure.thrownExceptionMessage
                               ]) + CRLF + CRLF ;
  end;
end;

function TtiTextTestListener.PrintFailureItems(r: TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.FailureCount-1 do begin
    failure := r.Failures[i];
    result := result + format('%3d) %s  %s'#13#10'          "%s"',
                               [
                               i+1,
                               tiPadR( failure.failedTest.name, 20 ),
                               tiPadR( failure.thrownExceptionName, 20 ),
                               failure.thrownExceptionMessage
                               ]) + CRLF + CRLF ;
  end;
end;

function TtiTextTestListener.PrintHeader(r: TTestResult): string;
begin
  result := '';
  if r.wasSuccessful then
  begin
    result := result + CRLF + CrLf ;
    result := result + format('OK: %d tests'+ CRLF, [r.runCount]);
  end
  else
  begin
    result := result + CRLF + CRLF ;
    result := result + 'Test Results:'+CRLF + CrLf ;
    result := result + format(
      '  Run:      %8d'+CRLF+
      '  Failures: %8d'+CRLF+
      '  Errors:   %8d'+CRLF+CRLF,
      [r.runCount, r.failureCount, r.errorCount]);
  end
end;

procedure TtiTextTestListener.StartTest(test: ITest);
begin
  write2Short('.', [tlwtFile, tlwtConsole]);
  // Nothing in XMLTestRunner
  IncPos ;
end;

procedure TtiTextTestListener.TestingStarts;
  procedure _WriteShort(const pDelphiVersion: string);
  begin
    writeln2Short('', [tlwtConsole]);
    writeln2Short('<html>', [tlwtFile] );
    writeln2Short('<pre>', [tlwtFile] );
    write2Short('DUnit testing of tiOPF ', [tlwtFile, tlwtConsole] );
    writeln2Short('(Delphi version ' + pDelphiVersion + ')', [tlwtFile, tlwtConsole]);
    writeln2Short('Testing started at ' + DateTimeToStr( Now ), [tlwtFile, tlwtConsole]) ;
    writeln2Short('(E = Exception, F = Test failure)', [tlwtFile, tlwtConsole]);
    writeln2Short('', [tlwtFile, tlwtConsole]);
  end ;
  procedure _WriteLong(const pDelphiVersion: string);
  begin
    write2Long('<html>' );
    write2Long('<h2>DUnit testing of tiOPF</h2>' );
    write2Long('<h3>(Delphi version ' + pDelphiVersion + ')</h3>');
    write2Long('<table border="1" cellpadding="4" style="font-family: Courier New; font-size: 12px" >' + CrLf );
    write2Long('  <tr>' + CrLf );
    write2Long('    <td>Test name</td>' + CrLf );
    write2Long('    <td>Failure type</td>' + CrLf );
    write2Long('    <td>Exception</td>' + CrLf );
    //write2Long('    <td>Location</td>' + CrLf );
    write2Long('    <td>Message</td>' + CrLf );
    write2Long('  </tr>' + CrLf );
  end;
begin
  FPos := 1 ;
  _WriteShort(FDelphiVersion);
  _WriteLong(FDelphiVersion);
  startTime := now;
end;

procedure TtiTextTestListener.Write2Short(const pStr: string; pWriteTo: TtiTextListenerWriteTo );
begin
  if tlwtFile in pWriteTo then
    Write2File(FFileNameShort, pStr);
  if tlwtConsole in pWriteTo then
    System.Write(pStr);
end;

procedure TtiTextTestListener.WriteLn2Short(const pStr: string; pWriteTo: TtiTextListenerWriteTo );
begin
  Write2Short(pStr+ #13 + #10, pWriteTo);
end;

procedure TtiTextTestListener.TestingEnds(testResult: TTestResult);
var
  h, m, s, l :Word;
begin
  endTime := now;
  runTime := endTime-startTime;
  writeln2Short('', [tlwtFile, tlwtConsole]);
  writeln2Short('', [tlwtFile, tlwtConsole]);
  DecodeTime(runTime, h,  m, s, l);
  writeln2Short(Format('Time to run tests: %d:%2.2d:%2.2d.%d', [h, m, s, l]), [tlwtFile, tlwtConsole]);
  writeln2Short(Report(testResult), [tlwtFile, tlwtConsole]);
  writeln2Short('</pre>', [tlwtFile]);
  writeln2Short('</html>', [tlwtFile]);
  WriteSummaryToINIFile(testResult);


  Write2Long('</table>');
  Write2Long('<p>');

  Write2Long('<h2>Statistics</h2>');

  Write2Long('<table border="1" cellpadding="4" >' + CrLf );
  Write2Long('  <tr>' + CrLf );
  Write2Long('    <td>Tests</td>' + CrLf );
  Write2Long('    <td>'+intToStr(testResult.runCount)+'</td>' + CrLf );
  Write2Long('  </tr>' + CrLf );
  Write2Long('  <tr>' + CrLf );
  Write2Long('    <td>Failures</td>' + CrLf );
  Write2Long('    <td>'+intToStr(testResult.failureCount)+'</td>' + CrLf );
  Write2Long('  </tr>' + CrLf );
  Write2Long('  <tr>' + CrLf );
  Write2Long('    <td>Errors</td>' + CrLf );
  Write2Long('    <td>'+intToStr(testResult.errorCount)+'</td>' + CrLf );
  Write2Long('  </tr>' + CrLf );
  Write2Long('  <tr>' + CrLf );
  Write2Long('    <td>Finished At</td>' + CrLf );
  Write2Long('    <td>'+DateTimeToStr(now)+'</td>' + CrLf );
  Write2Long('  </tr>' + CrLf );
  Write2Long('  <tr>' + CrLf );
  Write2Long('    <td>Runtime:</td>' + CrLf );
  Write2Long('    <td>'+timeToStr(runTime)+'</td>' + CrLf );
  Write2Long('  </tr>' + CrLf );
  Write2Long('  </table>' + CrLf );
  Write2Long('</html>' );

end;

procedure TtiTextTestListener.WriteSummaryToINIFile(testResult: TTestResult);
var
  lFileName : string ;
  lINIFile : TINIFile ;
  ls : string ;
  lIndent : string ;
begin
  lFileName := gCommandLineParams.GetParam('ini');
  if lFileName = '' then
    Exit ; //==>
  lINIFile := TINIFile.Create(lFileName);
  try
    ls := 'Tests run: ' + IntToStr(testResult.RunCount) + ', ' +
          'Failures: ' + IntToStr(testResult.FailureCount) + ', ' +
          'Errors: ' + IntToStr(testResult.ErrorCount);
    {$IFDEF DELPHI5}
      lIndent := 'd5_error_count' ;
    {$ENDIF}
    {$IFDEF DELPHI6}
      lIndent := 'd6_error_count' ;
    {$ENDIF}
    {$IFDEF DELPHI7}
      lIndent := 'd7_error_count' ;
    {$ENDIF}
    if lIndent = '' then
      tiFmtException('Unknown Delphi version', ClassName, 'WriteSummaryToINIFile' ) ;
    lINIFile.WriteString('Report_Summary', lIndent, ls ) ;
  finally
    lINIFile.Free;
  end ;
end;

procedure TtiTextTestListener.AddSuccess(test: ITest);
begin
  inherited;
  if test.tests.Count<=0 then
  begin
    Write2Table(
       test.GetName
      ,'<font color="#008000">PASS</font>'
      ,''
      //,''
      ,'');
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

procedure TtiTextTestListener.StartSuite(suite: ITest);
begin
  inherited;
  Write2Table(suite.GetName);
end;

procedure TtiTextTestListener.Write2File(const pFileName, pStr: string);
var
  lBuffer   : PChar ;
  lLen     : integer ;
  lFileStream : TFileStream ;
begin
  if FileExists( pFileName ) then
    lFileStream := TFileStream.Create( pFileName,
                                       fmOpenReadWrite or fmShareDenyNone )
  else
    lFileStream := TFileStream.Create( pFileName,
                                       fmCreate or fmShareDenyNone ) ;
  try
    lBuffer := PChar( pStr ) ;
    lLen := length( pStr ) ;
    lFileStream.Seek( 0, soFromEnd ) ;
    lFileStream.write( lBuffer^, lLen ) ;
  finally
    lFileStream.Free ;
  end ;
end;

procedure TtiTextTestListener.Write2Long(const pStr: string);
begin
  Write2File(FFileNameLong, pStr + CrLf );
end;

procedure TtiTextTestListener.Write2Table(const pCell1, pCell2, pCell3, pCell4 : string );
var
  lCell1, lCell2, lCell3, lCell4 : string ;
begin
  if pCell1 <> '' then lCell1 := pCell1 else lCell1 := '&nbsp;';
  if pCell2 <> '' then lCell2 := pCell2 else lCell2 := '&nbsp;';
  if pCell3 <> '' then lCell3 := pCell3 else lCell3 := '&nbsp;';
  if pCell4 <> '' then lCell4 := pCell4 else lCell4 := '&nbsp;';

  Write2File(FFileNameLong, '  <tr>' + CrLf );
  Write2File(FFileNameLong, '    <td>' + lCell1 + '</td>' + CrLf );
  Write2File(FFileNameLong, '    <td>' + lCell2 + '</td>' + CrLf );
  Write2File(FFileNameLong, '    <td>' + lCell3 + '</td>' + CrLf );
  Write2File(FFileNameLong, '    <td>' + lCell4 + '</td>' + CrLf );
  Write2File(FFileNameLong, '  </tr>' + CrLf );
end;

procedure TtiTextTestListener.Write2Table( const pCell1 : string ) ;
begin
  Write2File(FFileNameLong, '  <tr>' + CrLf );
  Write2File(FFileNameLong, '    <td  colspan="4">' + pCell1 + '</td>' + CrLf );
  Write2File(FFileNameLong, '  </tr>' + CrLf );
end;

end.
