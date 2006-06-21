program DemoTIPerFramework;

uses
  Forms,
  tiLog,
  tiPersist,
  cQueryNames in 'cquerynames.pas',
  Adrs_Dependencies in 'Adrs_Dependencies.pas',
  cAdrs in 'cAdrs.pas',
  Adrs_BOM in 'Adrs_Bom.pas',
  FormMain in 'FormMain.pas' {Main},
  FormPerson in 'FormPerson.pas' {FormPerson},
  FormCompany in 'FormCompany.pas' {FormCompany},
  FormContact in 'FormContact.pas' {FormContact},
  FormAddress in 'FormAddress.pas' {FormAddress};

{$R *.RES}

begin
  SetupLogForClient ;
  Application.Initialize;
  gTIPerMgr.LoadPersistenceFramework ;
  Application.CreateForm(TMain, Main);
  Application.Run;
  gTIPerMgr.UnLoadPersistenceFramework ;
end.



program DemoTIPerFramework;

uses
  Forms,
  tiLog,
  tiPersist,
  tiQueryIbx,
  cQueryNames in 'cquerynames.pas',
  Adrs_Dependencies in 'Adrs_Dependencies.pas',
  cAdrs in 'cAdrs.pas',
  Adrs_BOM in 'Adrs_Bom.pas',
  FormMain in 'FormMain.pas' {Main},
  FormPerson in 'FormPerson.pas' {FormPerson},
  FormCompany in 'FormCompany.pas' {FormCompany},
  FormContact in 'FormContact.pas' {FormContact},
  FormAddress in 'FormAddress.pas' {FormAddress};

{$R *.RES}


begin
  SetupLogForClient;
  Application.Initialize;

  // This code is a bit of a hack but will get you going with the compile time
  // determination of persistence layer.
  gTIPerMgr.DefaultDBConnectionName := gTIPerMgr.ReadConfigValue('d', 'DatabaseName');
  gTIPerMgr.DBConnectionPools.Connect( gTIPerMgr.DefaultDBConnectionName,
    gTIPerMgr.ReadConfigValue('u', 'UserName'), gTIPerMgr.ReadConfigValue('p', 'UserPassword'));
  gTIPerMgr.SQLMgrDatabaseMappings.RegisterMapping(gTIPerMgr.DefaultDBConnectionName, gTIPerMgr.DefaultDBConnectionName);
  //gTIPerMgr.CreateDefaultQueryFactory;

  Application.CreateForm(TMain, Main);
  Application.Run;
end.







// This block of code will use packages loaded at runtime to determine
// which persistence layer is used

// Make sure tiPersistCore is included in the Project|Options
// build with runtime packages option.


// This block of code will use packages loaded at runtime to determine
// which persistence layer is used

// Make sure tiPersistCore is included in the Project|Options
// build with runtime packages option.

program DemoTIPerFramework;

uses
  Forms,
  tiLog,
  tiPersist,
  cQueryNames in 'cquerynames.pas',
  Adrs_Dependencies in 'Adrs_Dependencies.pas',
  cAdrs in 'cAdrs.pas',
  Adrs_BOM in 'Adrs_Bom.pas',
  FormMain in 'FormMain.pas' {Main},
  FormPerson in 'FormPerson.pas' {FormPerson},
  FormCompany in 'FormCompany.pas' {FormCompany},
  FormContact in 'FormContact.pas' {FormContact},
  FormAddress in 'FormAddress.pas' {FormAddress};

{$R *.RES}

begin
  SetupLogForClient ;
  Application.Initialize;
  gTIPerMgr.LoadPersistenceFramework ;
  Application.CreateForm(TMain, Main);
  Application.Run;
  gTIPerMgr.UnLoadPersistenceFramework ;
end.

