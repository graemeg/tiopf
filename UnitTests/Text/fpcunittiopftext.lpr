program fpcunittiopftext;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,ConsoleTestRunner
  ,tiTestDependencies
  ,tiOPFTestManager
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
  try
    App.Initialize;
    App.Title := 'FPCUnit Console test runner';

    GTIOPFTestManager.Read;
    GTIOPFTestManager.DeleteDatabaseFiles;
    tiTestDependencies.tiRegisterTests;

//   tiTestDependencies.RemoveUnSelectedPersistenceLayerSetups;

    App.Run;
  finally
    App.Free;
  end
end.
