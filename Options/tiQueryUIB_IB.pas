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

unit tiQueryUIB_IB;

interface

uses
  tiQueryUIBAbs,
  tiPersistenceLayers;

type

  TtiPersistenceLayerUIBIB = class(TtiPersistenceLayer)
  protected
    function  GetPersistenceLayerName: string; override;
    function  GetDatabaseClass: TtiDatabaseClass; override;
    function  GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseUIB_IB = class (TtiDatabaseUIBAbs)
  public
    constructor create; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string); override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: string): Boolean; override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword : string); override;
  end;

  TtiQueryUIB_IB = class (TtiQueryUIBAbs)
  end;



implementation

uses
  tiOPFManager,
  tiDBConnectionPool,
  tiConstants;

{ TtiDatabaseUIB_IB }

constructor TtiDatabaseUIB_IB.create;
begin
  inherited;
  LayerName := cTIPersistUIB_IB;
  UIBDatabase.LibraryName := 'GDS32.dll';
end;

class procedure TtiDatabaseUIB_IB.CreateDatabase(const ADatabaseName, AUserName,
        APassword : string );
var
  lDatabase: TtiDatabaseUIB_IB;
begin
  lDatabase := TtiDatabaseUIB_IB.Create ;
  try
    with lDatabase.UIBDatabase do begin
      DatabaseName := ADatabaseName;
      UserName := AUserName;
      PassWord := APassword;
      CreateDatabase ;
    end;
  finally
    lDatabase.Free;
  end ;
end;

class function TtiDatabaseUIB_IB.DatabaseExists(const ADatabaseName, AUserName,
        APassword : string ): Boolean;
var
  lDatabase: TtiDatabaseUIB_IB;
begin
  lDatabase := TtiDatabaseUIB_IB.Create ;
  try
    with lDatabase.UIBDatabase do begin
      DatabaseName := ADatabaseName;
      UserName := AUserName;
      PassWord := APassword;
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

class procedure TtiDatabaseUIB_IB.DropDatabase(const ADatabaseName, AUserName,
  APassword: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

{ TtiPersistenceLayerUIBIB }

function TtiPersistenceLayerUIBIB.GetPersistenceLayerName: string;
begin
  Result := cTIPersistUIB_IB;
end;

function TtiPersistenceLayerUIBIB.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseUIB_IB;
end;

function TtiPersistenceLayerUIBIB.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQueryUIB_IB;
end;

procedure TtiPersistenceLayerUIBIB.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTIPersistUIB_IB;
  APersistenceLayerDefaults.DatabaseName := CDefaultDatabaseDirectory + CDefaultDatabaseName + '.gdb';
  APersistenceLayerDefaults.Username := 'SYSDBA';
  APersistenceLayerDefaults.Password := 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase := False;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL := True;
end;


initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerUIBIB);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistUIB_IB);

end.

