{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Purpose:
    Use the Adapter Pattern [GoF 139] to wrapper the TIBQuery
    component to allow a standard interface to be presented to the
    application for all data access APIs.

  Classes:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQueryUIB_EB;

interface

uses
  tiQueryUIBAbs;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseUIB_EB = class (TtiDatabaseUIBAbs)
  public
    constructor create; override;
    class procedure CreateDatabase(const pDatabaseName, pUserName,
      pPassword: string); override;
    class function DatabaseExists(const pDatabaseName, pUserName,
      pPassword: string): Boolean; override;
  end;

  // ---------------------------------------------------------------------------
  TtiQueryUIB_EB = class (TtiQueryUIBAbs)
  end;

  // ---------------------------------------------------------------------------
  {TtiExtractUIB_EB = class (TtiExtractUIBAbs)
  end;}

implementation

uses
  tiPersist,
  tiDBConnectionPool,
  ctiPersist;

{ TtiDatabaseUIB_IB }

constructor TtiDatabaseUIB_EB.create;
begin
  inherited;
  LayerName := cTIPersistUIB_EB;
  UIBDatabase.LibraryName := 'FbEmbed.dll';
end;

class procedure TtiDatabaseUIB_EB.CreateDatabase(const pDatabaseName, pUserName,
        pPassword : string );
var
  lDatabase: TtiDatabaseUIB_EB;
begin
  lDatabase := TtiDatabaseUIB_EB.Create ;
  try
    with lDatabase.UIBDatabase do begin
      DatabaseName := pDatabaseName;
      UserName := pUserName;
      PassWord := pPassword;
      CreateDatabase ;
    end;
  finally
    lDatabase.Free;
  end ;
end;

class function TtiDatabaseUIB_EB.DatabaseExists(const pDatabaseName, pUserName,
        pPassword : string ): Boolean;
var
  lDatabase: TtiDatabaseUIB_EB;
begin
  lDatabase := TtiDatabaseUIB_EB.Create ;
  try
    with lDatabase.UIBDatabase do begin
      DatabaseName := pDatabaseName;
      UserName := pUserName;
      PassWord := pPassword;
      try
        Connected := true ;
        Result := true ;
      except
        Result := false ;
      end ;
      Connected := false ;
    end;
  finally
    lDatabase.Free;
  end ;
end;

initialization

  gtiPerMgr.RegPerLayers.RegisterPersistenceLayer(
    cTIPersistUIB_EB,
    TtiDBConnectionPoolDataAbs,
    TtiQueryUIB_EB,
    TtiDatabaseUIB_EB);

finalization
  gtiPerMgr.RegPerLayers.UnRegisterPersistenceLayer(cTIPersistUIB_EB);

end.

