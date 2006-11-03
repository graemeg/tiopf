program fpcunittiopftext;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,ConsoleTestRunner
  ,tiDUnitDependencies
  ;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  App: TMyTestRunner;

begin
  App := TMyTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console test runner';
  
  tiDUnitDependencies.RegisterTests;
  
  App.Run;
  App.Free;
end.
