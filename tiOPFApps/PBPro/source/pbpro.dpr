{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program pbpro;



uses
  Forms,
  JclAppInst,
  tiLog,
  tiPersist,
  tiQueryIBX,
  pbpBusinessClasses in 'pbpBusinessClasses.pas',
  pbpPersistence in 'pbpPersistence.pas',
  pbpMainForm in 'pbpMainForm.pas' {MainForm},
  pbpSplashForm in 'pbpSplashForm.pas' {SplashForm},
  pbpContractListForm in 'pbpContractListForm.pas' {ContractListForm},
  pbpClientListForm in 'pbpClientListForm.pas' {ClientListForm},
  pbpContractSearchForm in 'pbpContractSearchForm.pas' {ContractSearchForm},
  pbpTasksForm in 'pbpTasksForm.pas' {TasksForm},
  pbpPawnbrokerForm in 'pbpPawnbrokerForm.pas' {PawnbrokerForm},
  pbpPoliceReport in 'pbpPoliceReport.pas' {PoliceReportDataModule: TDataModule},
  pbpClientFrame in 'pbpClientFrame.pas' {ClientFrame: TFrame},
  pbpClientIdentityRecordsFrame in 'pbpClientIdentityRecordsFrame.pas' {ClientIdentityRecordsFrame: TFrame},
  pbpClientCreationWizardForm in 'pbpClientCreationWizardForm.pas' {Form1},
  pbpResources in 'pbpResources.pas',
  pbpContractCreationWizardForm in 'pbpContractCreationWizardForm.pas' {ContractCreationWizardForm};

{$R *.RES}

begin
  JclAppInstances.CheckSingleInstance;
  SplashForm := TSplashForm.Create(Application);
  SplashForm.Show;
  SplashForm.Update;
  SetupLogForClient;
  Application.Initialize;
  pbpPersistence.LoadPersistenceFramework;
  Application.Title := 'pbpExplorer';
  Application.CreateForm(TMainForm, MainForm);
  SplashForm.Hide;
  SplashForm.Free;
  Application.Run;
  pbpPersistence.UnLoadPersistenceFramework;
end.
