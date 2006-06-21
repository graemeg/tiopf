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
program tiSQLManager;

{%ToDo 'tiSQLManager.todo'}

uses
  Forms,
  FMainSQLManager in 'FMainSQLManager.pas' {FormMain},
  FChildDatabase in 'FChildDatabase.pas' {FormChildDatabase},
  FChildQuery in 'FChildQuery.pas' {FormChildQuery},
  FChildGroup in 'FChildGroup.pas' {FormChildGroup},
  FSQLMgrBrowse in 'FSQLMgrBrowse.pas' {FormSQLMgrBrowse},
  FAdhocQuery in 'FAdhocQuery.pas' {FormAdHocQuery},
  FRunAsScript in 'FRunAsScript.pas' {FormRunAsScript},
  FFindQuery in 'FFindQuery.pas' {FormFindQuery},
  FEditParam in 'FEditParam.pas' {FormEditParam},
  FAbout in 'FAbout.pas' {FormAbout},
  FtiPerEditDialog in '..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FtiDialogAbs in '..\..\tiPerAwareCtrls\FtiDialogAbs.pas' {FormTiDialogAbs},
  FSQLEditor in 'FSQLEditor.pas' {FormSQLEditor},
  FPickDatabaseObject in 'FPickDatabaseObject.pas' {FormPickDatabaseObject},
  tiSQLManagerDependencies in 'tiSQLManagerDependencies.pas';

{$I tiDefines.inc}

{$R *.RES}

begin
  Application.Initialize;
  ConnectToDatabase;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.


