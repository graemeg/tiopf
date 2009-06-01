program Demo_OneToMany;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  FMainOneToMany, FClientEdit, FPhoneNumberEdit, tiOPFManager,
  tiConstants, Client_HardCodedVisitors_Svr, Client_DBIndependentVisitors_Svr,
  Client_AutoMap_Svr, Client_BOM;


var
  i: integer;
begin
  Application.Initialize;

  gTIOPFManager.DefaultPersistenceLayerName := cTIPersistXMLLight;
  writeln('========[ available ]=========');
  for i := 0 to gTIOPFManager.PersistenceLayers.Count-1 do
    writeln(gTIOPFManager.PersistenceLayers.Items[i].PersistenceLayerName);
  writeln('========[ default ]===========');
  writeln(gTIOPFManager.DefaultPersistenceLayerName);
  writeln('==============================');

  Client_AutoMap_Svr.RegisterMappings;

  Application.CreateForm(TFormMainOneToMany, FormMainOneToMany);
  Application.Run;

  gTIOPFManager.Terminate;
end.

