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

Unit tiQueryUIB_FB;

Interface

Uses
  SysUtils,
  tiQuery,
  tiQueryUIBAbs,
  tiPersistenceLayers;

Type

  TtiPersistenceLayerUIB_FB = Class(TtiPersistenceLayer)
  Protected
    Function  GetPersistenceLayerName : String; Override;
    Function  GetDatabaseClass : TtiDatabaseClass; Override;
    Function  GetQueryClass : TtiQueryClass; Override;
  Public
    Procedure AssignPersistenceLayerDefaults(Const APersistenceLayerDefaults : TtiPersistenceLayerDefaults); Override;
  End;


  TtiDatabaseUIB_FB = Class(TtiDatabaseUIBAbs)
  Public
    Constructor Create; Override;

    Class Function DatabaseExists(Const ADatabaseName, AUserName, APassword: String; const AParams: string = ''): Boolean; Override;
    Class Procedure CreateDatabase(Const ADatabaseName, AUserName, APassword: String; const AParams: string = ''); Override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    Function TIQueryClass : TtiQueryClass; Override;

  End;


  TtiQueryUIB_FB = Class(TtiQueryUIBAbs)

  End;


Implementation

Uses
  uiblib,
  tiOPFManager,
  tiDBConnectionPool,
  tiConstants;

{ TtiDatabaseUIB_FB }

Constructor TtiDatabaseUIB_FB.create;
Begin
  Inherited Create;
  LayerName := cTIPersistUIB_FB;
End;

Class Procedure TtiDatabaseUIB_FB.CreateDatabase(Const ADatabaseName, AUserName, APassword: String; const AParams: string);
Var
  lDatabase : TtiDatabaseUIB_FB;
Begin
  lDatabase := TtiDatabaseUIB_FB.Create;
  Try
    lDatabase.UIBDatabase.DatabaseName := ADatabaseName;
    lDatabase.UIBDatabase.UserName := AUserName;
    lDatabase.UIBDatabase.PassWord := APassword;
    lDatabase.UIBDatabase.CreateDatabase(GetSystemCharacterset);
  Finally
    lDatabase.Free;
  End;
End;

class procedure TtiDatabaseUIB_FB.DropDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

Class Function TtiDatabaseUIB_FB.DatabaseExists(Const ADatabaseName, AUserName, APassword: String; const AParams: string): Boolean;
Var
  lDatabase : TtiDatabaseUIB_FB;
Begin
  lDatabase := TtiDatabaseUIB_FB.Create;
  Try
    lDatabase.UIBDatabase.DatabaseName := ADatabaseName;
    lDatabase.UIBDatabase.UserName := AUserName;
    lDatabase.UIBDatabase.PassWord := APassword;
    Try
      lDatabase.UIBDatabase.Connected := true;
      Result := true;
    Except
      On e : exception Do
        result := false;
    End;
    lDatabase.UIBDatabase.Connected := false;
  Finally
    lDatabase.Free;
  End;
End;

Function TtiDatabaseUIB_FB.TIQueryClass : TtiQueryClass;
Begin
  Result := TtiQueryUIB_FB;
End;

{ TtiPersistenceLayerUIB_FB }

Procedure TtiPersistenceLayerUIB_FB.AssignPersistenceLayerDefaults(
  Const APersistenceLayerDefaults : TtiPersistenceLayerDefaults);
Begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTiPersistUIB_FB;
  APersistenceLayerDefaults.DatabaseName := CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username := 'SYSDBA';
  APersistenceLayerDefaults.Password := 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase := True;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
End;

Function TtiPersistenceLayerUIB_FB.GetDatabaseClass : TtiDatabaseClass;
Begin
  Result := TtiDatabaseUIB_FB;
End;

Function TtiPersistenceLayerUIB_FB.GetPersistenceLayerName : String;
Begin
  Result := cTiPersistUIB_FB;
End;

Function TtiPersistenceLayerUIB_FB.GetQueryClass : TtiQueryClass;
Begin
  Result := TtiQueryUIB_FB;
End;

initialization

  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerUIB_FB);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistUIB_FB);

end.

