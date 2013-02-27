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

  Change history:
    Created: Jan 2000

  Notes:
      Address book application's main form

  ToDo:
    1. Build a TtiPerAwareComboBox
    2. BDE Version
    3. Paradox version

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiAdrsListMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ComCtrls, tiListView,
  tiTreeView, ExtCtrls, ToolWin
  ,Adrs_BOM, ActnList, Menus, ImgList
  ;

type
  TFormMainOld = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    aNew: TAction;
    aDelete: TAction;
    aSave: TAction;
    aCancel: TAction;
    aClose: TAction;
    aNew1: TMenuItem;
    aSave1: TMenuItem;
    aDelete1: TMenuItem;
    N3: TMenuItem;
    aClose1: TMenuItem;
    ilButtons: TImageList;
    TV: TtiTreeView;
    aShowObjects: TAction;
    Utils1: TMenuItem;
    ShowObjects1: TMenuItem;
    StatusBar1: TStatusBar;
    aShowDatabaseConnectionDetails: TAction;
    About1: TMenuItem;
    Showdatabaseconnectiondetails1: TMenuItem;
    ilTreeView: TImageList;
    aCloneCurrentObject: TAction;
    N4: TMenuItem;
    Clonethecurrentobject1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TVClose(Sender: TObject);
    procedure TVSave(Sender: TObject);
    procedure TVCancel(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;var Handled: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TVSelectNode(ptiTreeView: TtiTreeView; pNode: TTreeNode;
      pData: TObject; pParentNode: TTreeNode; pParentData: TObject);
    procedure aShowObjectsExecute(Sender: TObject);
    procedure tiTVMappingPersonOnDelete(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingPersonOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingPersonListOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure TVFilterData(pData: TObject; var pbInclude: Boolean);
    procedure aShowDatabaseConnectionDetailsExecute(Sender: TObject);
    procedure tiTVMappingCompanyListOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure tiTVMappingCompanyOnInsert(ptiTreeView: TtiTreeView;
      pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
      pParentData: TObject);
    procedure aCloneCurrentObjectExecute(Sender: TObject);
  private
    procedure Read ;
    procedure Save ;
    procedure AddPerson(pPersonList: TPeople);
    procedure AddCompany(pCompanyList: TCompanies);
  public
  end;

var
  FormMainOld: TFormMainOld;

implementation
uses
   FtiAdrsListChild_Person
  ,FtiAdrsListChild_Company
  ,tiPtnVisMgr
  ,cAdrs
  ,tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,tiPersist
  ,tiDialogs
  ,tiRegINI
  ;

{$R *.DFM}

// Form's OnCreate event
//------------------------------------------------------------------------------
procedure TFormMainOld.FormCreate(Sender: TObject);
begin
  TV.Align := alClient ;
  Caption := 'TI Persistence Framework - ' + gTIPerMgr.DefaultDBConnectionName ;
  gReg.ReadFormState( Self ) ;
  TV.RegisterChildForm( TPerson,  TFormEditPerson ) ;
  TV.RegisterChildForm( TCompany, TFormEditCompany ) ;
  Read ;
  TV.SelectedAddress := gReg.ReadString( Name, 'SelectedAddress', '' ) ;
end;

// Form's OnDestroy event
//------------------------------------------------------------------------------
procedure TFormMainOld.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState( Self ) ;
  gReg.WriteString( Name, 'SelectedAddress', TV.SelectedAddress ) ;
end;

// Create a TAddressBook object and read it's primary key data
//------------------------------------------------------------------------------
procedure TFormMainOld.Read ;
begin
  TV.Data := nil ;
  gAdrsBook.Clear ;
  gAdrsBook.Read ;
  if ( gAdrsBook.AdrsTypes.Count <> 2 ) then
    PopulateAdrsBook;
  TV.Data := gAdrsBook ;
end;

// Form's CloseQuery event
//------------------------------------------------------------------------------
procedure TFormMainOld.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if tiPerObjAbsSaveAndClose( gAdrsBook, CanClose ) then
    Save ;
end;

// TtiTreeViewPlus close action
//------------------------------------------------------------------------------
procedure TFormMainOld.TVClose(Sender: TObject);
begin
  Close ;
end;

// TtiTreeViewPlus save event
//------------------------------------------------------------------------------
procedure TFormMainOld.TVSave(Sender: TObject);
begin
  if not TV.IsCurrentChildFormValid then
    exit ; //==>

  if not gAdrsBook.Dirty then
    exit ; //==>

  Save ;

end;

// TtiTreeViewPlus Cancel event
//------------------------------------------------------------------------------
procedure TFormMainOld.TVCancel(Sender: TObject);
begin
  Read ;
end;

// ActionList's New event
//------------------------------------------------------------------------------
procedure TFormMainOld.aNewExecute(Sender: TObject);
begin
  TV.DoInsert ;
end;

// ActionList's Delete event
//------------------------------------------------------------------------------
procedure TFormMainOld.aDeleteExecute(Sender: TObject);
begin
  TV.DoDelete
end;

// ActionList's OnUpdate event
//------------------------------------------------------------------------------
procedure TFormMainOld.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  aNew.Enabled    := TV.CanInsertSelected ;
  aDelete.Enabled := TV.CanDeleteSelected ;

  aSave.Enabled   := gAdrsBook.Dirty ;
  aCancel.Enabled := aSave.Enabled ;
  aClose.Enabled  := true ;
  aShowObjects.Enabled := ( TV.SelectedData <> nil ) ;
  aCloneCurrentObject.Enabled :=
    aShowObjects.Enabled and
    ( not ( TV.SelectedData is TAdrsBook )) ;

end;

// When a node on the TtiTreeViewPlus is selected, this event is triggered.
// Read the detailed data associated with the node's data.
//------------------------------------------------------------------------------
procedure TFormMainOld.TVSelectNode(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
{
  if pData is TPerson then
    gTIPerMgr.Read( TPerson( pData ))
  else
  if pData is TCompany then
    gTIPerMgr.Read( TCompany( pData )) ;
}
end;

procedure TFormMainOld.aShowObjectsExecute(Sender: TObject);
begin
  tiShowPerObjAbs( TPerObjAbs( TV.SelectedData ), true ) ;
end;

procedure TFormMainOld.tiTVMappingPersonOnDelete(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  if not (( pData is TPerson ) or
          ( pData is TCompany )) then
    Exit ; //==>
  if tiAppConfirmation( 'Are you sure you want to delete <%s> ?',
                        [TPerObjAbs( pData ).Caption] ) then
    TPerObjAbs( pData ).Deleted := true ;
end;

procedure TFormMainOld.tiTVMappingPersonOnInsert( ptiTreeView: TtiTreeView;
                                               pNode: TTreeNode;
                                               pData: TObject;
                                               pParentNode: TTreeNode;
                                               pParentData: TObject);
begin
  if TObject( pParentNode.Data ) is TPeople then
    AddPerson( TPeople( pParentNode.Data ))
  else
    AddPerson( TCompany( pParentNode.Data ).People ) ;
end ;

procedure TFormMainOld.AddPerson( pPersonList : TPeople ) ;
var
  lData       : TPerson ;
begin
  lData := TPerson.CreateNew;
  lData.FirstName := 'Enter' ;
  lData.LastName  := 'Enter' ;
  pPersonList.Add( lData ) ;
end;

procedure TFormMainOld.tiTVMappingPersonListOnInsert(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  AddPerson( pData as TPeople ) ;
end;

procedure TFormMainOld.TVFilterData(pData: TObject; var pbInclude: Boolean);
begin
  pbInclude := not TPerObjAbs( pData ).Deleted ;
end;


procedure TFormMainOld.aShowDatabaseConnectionDetailsExecute(Sender: TObject);
var
  ls : string ;
begin
  ls := gTIPerMgr.DefaultPerLayer.DBConnectionPools.DetailsAsString ;
  tiShowMessage( ls ) ;
end;

procedure TFormMainOld.AddCompany( pCompanyList : TCompanies ) ;
var
  lData       : TCompany ;
begin
  lData := TCompany.CreateNew;
  lData.CompanyName := 'Enter' ;
  pCompanyList.Add( lData ) ;
end;

procedure TFormMainOld.tiTVMappingCompanyListOnInsert(
  ptiTreeView: TtiTreeView; pNode: TTreeNode; pData: TObject;
  pParentNode: TTreeNode; pParentData: TObject);
begin
  AddCompany( pData as TCompanies ) ;
end;

procedure TFormMainOld.tiTVMappingCompanyOnInsert(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
const
  cEmployee = '&Employee' ;
  cCompany  = '&Company' ;
  cCancel   = 'C&ancel' ;
var
  ls : string ;
begin
  ls := tiMessageDlg( 'Do you want to add an employee of ' +
                      ( pData as TCompany ).CompanyName + ' or a new company?',
                      [cEmployee, cCompany, cCancel],
                      mtConfirmation,
                      'What do you want to add?' ) ;
  if ls = cEmployee then
    AddPerson( ( pData as TCompany ).People )
  else if ls = cCompany then
    AddCompany( ( pParentData as TCompanies )) ;

end;

procedure TFormMainOld.Save;
begin
  gAdrsBook.Save ;
end;

procedure TFormMainOld.aCloneCurrentObjectExecute(Sender: TObject);
var
  lPerson : TPerObjAbs ;
  ls : string ;
begin
  lPerson := TPerObjAbs( TV.SelectedData ).Clone ;
  ls :=
        '* * * CLONE FROM * * *' + Cr( 2 ) +
        tiPerObjAbsAsString( TPerObjAbs( TV.SelectedData )) + Cr( 2 ) +
        '* * * CLONE TO * * *' + Cr( 2 ) +
        tiPerObjAbsAsString( lPerson );
  tiShowString( ls ) ;
  // There is some code in the TtiTreeView which makes it easy to use
  // drag-and-drop to clone objects and this can be seen in use in the
  // tiSQLManager application.
  // Before these objects can be saved, they must be given new OIDs and
  // their ObjectState must be set to posCreate. This can be done with a
  // visitor.
end;

end.

