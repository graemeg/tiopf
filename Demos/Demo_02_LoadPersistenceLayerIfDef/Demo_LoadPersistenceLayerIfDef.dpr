program Demo_LoadPersistenceLayerIfDef;

// Adding a {$DEFINE LINK_???} to your project, either in the DPR
// or the Project | Options - Conditional Defines dialog,
// the specified persistence layers will be linked and loaded.
//
// When you run this demo, a dialog will show listing
// the loaded persistence layers. (Note, the dialog will
// say the database is not connected - which is correct.)

// Take a look at tiOPFManager about line 1131 you will see how
// this the defines are implemented.

{$R *.res}

uses
  DemoDBUtils in '..\Common\DemoDBUtils.pas';

begin
  ShowConnectedDatabases;
end.
