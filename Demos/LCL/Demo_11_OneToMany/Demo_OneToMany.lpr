program Demo_OneToMany;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  FMainOneToMany,
  FClientEdit,
  FPhoneNumberEdit,
  tiOIDGUID, // To force linking GUID OIDs. Must be included in application at least once.
  tiOPFManager,
  tiConstants,
  Client_HardCodedVisitors_Svr,
  Client_DBIndependentVisitors_Svr,
  Client_AutoMap_Svr,
  Client_BOM;


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

  { Uncomment the persistence mechanism to use }
//  Client_AutoMap_Svr.RegisterMappings;
  Client_DBIndependentVisitors_Svr.RegisterVisitors;
//  Client_HardCodedVisitors_Svr.RegisterVisitors;

  Application.CreateForm(TFormMainOneToMany, FormMainOneToMany);
  Application.Run;

  gTIOPFManager.Terminate;
end.

