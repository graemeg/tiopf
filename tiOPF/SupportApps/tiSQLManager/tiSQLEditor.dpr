{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Originally developed by TechInsite Pty. Ltd.
  23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
  PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au

  This code is made available on the TechInsite web site as open source.
  You may use this code in any way you like, except to sell it to other
  developers. Please be sure to leave the file header and list of
  contributors unchanged.

  If you make any changes or enhancements, which you think will benefit other
  developers and will not break any existing code, please forward your changes
  (well commented) to TechInsite and I will make them available in the next
  version.

  Revision history:
    November 2000, Peter Hinrichsen, Made open source

  Purpose:
    SQLManager project - A GUI tool for managing SQL stored in the database

  Classes:


  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program tiSQLEditor;

{%ToDo 'tiSQLEditor.todo'}

uses
  tiLog,
  tiPersist,
  Forms,
  FSplash,
  tiPerObjOIDGUID,
  tiCOmmandLineParams,
  FMainSQLEditor in 'FMainSQLEditor.pas' {FormMain},
  FPickDatabaseObject in 'FPickDatabaseObject.pas' {FormPickDatabaseObject};

{$R *.RES}

begin

  FSplash.ShowSplash ;
  SetupLogForClient( false ) ;
  gTIPerMgr.LoadDatabaseLayer(gTIPerMgr.DefaultPerLayerName,
                              gCommandLineParams.GetParam('d'),
                              gCommandLineParams.GetParam('u'),
                              gCommandLineParams.GetParam('p'));
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.


