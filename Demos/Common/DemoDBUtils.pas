unit DemoDBUtils;

interface

procedure ShowConnectedDatabases;

implementation
uses
  tiOPFManager
  ,tiUtils
  ,tiDialogs
  ,SysUtils
 ;

procedure ShowConnectedDatabases;
var
  i: integer;
  LS: string;
begin
  LS:= '';
  for i:= 0 to gTIOPFManager.PersistenceLayers.Count - 1 do
  begin
    if LS <> '' then
      LS:= LS + Cr;
    if Trim(gTIOPFManager.PersistenceLayers.Items[i].DBConnectionPools.DetailsAsString) = '' then
      LS:= LS + 'Persistence layer: "'+ gTIOPFManager.PersistenceLayers.Items[i].PersistenceLayerName +
            '" loaded, but not connected to a database.' + Cr
    else
      LS:= LS + gTIOPFManager.PersistenceLayers.Items[i].DBConnectionPools.DetailsAsString + Cr
  end;

  if LS <> '' then
    tiAppMessage(LS)
  else
    tiAppMessage('No persistence layers loaded');
end;
end.
