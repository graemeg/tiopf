unit FWorkList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FtiFormMgrForm, tiReadOnly, StdCtrls, Buttons, ExtCtrls,
  tiHyperlink, tiFocusPanel, tiListView, Adrs_BOM, ComCtrls, Contnrs;

type
  TFormWorkList = class(TFormTIFormMgrForm)
    pnlMain: TPanel;
    Label1: TLabel;
    editSearch: TEdit;
    bbSearch: TBitBtn;
    tiHyperLink1: TtiHyperLink;
    tiHyperLink2: TtiHyperLink;
    Bevel1: TBevel;
    Bevel2: TBevel;
    lvSearchResult: TtiListView;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bbSearchClick(Sender: TObject);
    procedure lvSearchResultItemEdit(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvSearchResultItemDelete(pLV: TtiCustomListView;
      pData: TPersistent; pItem: TListItem);
    procedure lvSearchResultFilterData(pData: TPersistent;
      var pbInclude: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure tiHyperLink2Click(Sender: TObject);
    procedure tiHyperLink1Click(Sender: TObject);
  private
    FSearchResult : TObjectList;
  public
    { Public declarations }
  end;

var
  FormWorkList: TFormWorkList;

implementation
uses
  tiFormMgr
  ,FtiAdrsListChild_Person
  ,FtiAdrsListChild_Company
  ,tiVisitorDB
  ,tiVisitorDB_Cli
  ,FMain
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ;

{$R *.dfm}

procedure TFormWorkList.FormCreate(Sender: TObject);
begin
  inherited;
  FSearchResult := TObjectList.Create(False);
  lvSearchResult.AddColumn('Caption',lvtkString,'Person or company name', 350);
  ButtonsVisible := false ;
  FormCaption := 'Worklist for ' + FormatDateTime( 'DDDD DD-MMM-YYYY', Date );
  FormResize(nil);
end;

procedure TFormWorkList.FormDestroy(Sender: TObject);
begin
  FSearchResult.Free;
  inherited;
end;

procedure TFormWorkList.FormResize(Sender: TObject);
begin
  inherited;
  pnlMain.Height  := ClientHeight - pnlMain.Top - 16 ;
  pnlMain.Left := ( ClientWidth - pnlMain.Width ) div 2 ;
end;

procedure TFormWorkList.FormShow(Sender: TObject);
begin
  inherited;
  editSearch.SetFocus;
end;

procedure TFormWorkList.bbSearchClick(Sender: TObject);
begin
  inherited;
  lvSearchResult.Data := nil ;
  gAdrsBook.FindAllLike( editSearch.Text, FSearchResult ) ;
  lvSearchResult.Data := FSearchResult ;
  lvSearchResult.Visible := FSearchResult.Count > 0 ;
end;

procedure TFormWorkList.lvSearchResultItemEdit(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  inherited;
  lvSearchResult.Data := nil;
  lvSearchResult.Visible := false ;
  // But in .Read This check should not be required.
  if (pData as TtiObject ).ObjectState <> posClean then
    (pData as TtiObject ).Read ;
  editSearch.Text := '' ;
  if pData is TPerson then
    gFormMgr.ShowForm(TFormEditPerson, pData as TPerson, false)
  else if pData is TCompany then
    gFormMgr.ShowForm(TFormEditCompany, pData as TCompany, false );
end;

procedure TFormWorkList.lvSearchResultItemDelete(pLV: TtiCustomListView;
  pData: TPersistent; pItem: TListItem);
begin
  if tiPerObjAbsConfirmAndDelete( pData as TtiObject ) then
    ( pData as TtiObject ).Save ;
end;

procedure TFormWorkList.lvSearchResultFilterData(pData: TPersistent;
  var pbInclude: Boolean);
begin
  inherited;
  pbInclude := not ( pData as TtiObject ).Deleted ;
end;

procedure TFormWorkList.tiHyperLink2Click(Sender: TObject);
begin
  inherited;
  EditNewPerson;
end;

procedure TFormWorkList.tiHyperLink1Click(Sender: TObject);
begin
  inherited;
  EditNewCompany;
end;

end.
