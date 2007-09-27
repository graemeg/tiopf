unit PartLookupU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  ExtCtrls, ActnList, tiFocusPanel, tiVTListView, tiObject, tiVirtualTrees,
  StdCtrls, Buttons;

type
  TfrmPartLookup = class(TForm)
    LV: TtiVTListView;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure LVFilterData(AData: TtiObject; var pInclude: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  protected

  public
    { Public declarations }
    class function Execute(var APartNo: integer): boolean;
  end;

implementation

{$R *.dfm}

uses
  tiOPFManager
  ,tiConstants
  ,tiGUIUtils
, modSharedU, MastApp_BOM;

class function TfrmPartLookup.Execute(var APartNo: integer): boolean;
var form: TfrmPartLookup;
begin
  result:= false;

  form:= self.create(Application);

  form.LV.AddColumn('PartNo', vttkInt, 'Part No', 80);
  form.LV.AddColumn('Description', vttkString, 'Description', 150);

  form.lv.Data:= modShared.Parts;
  form.LV.SelectedData:= form.lv.Data.Find(IntToStr(APartNo));

  if form.ShowModal = mrOk then
  begin
    APartNo:= TPart(form.lv.SelectedData).PartNo;
    result:= true;
  end;
end;

procedure TfrmPartLookup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TfrmPartLookup.FormShow(Sender: TObject);
begin
  lv.SP.Showing:= true;
end;

procedure TfrmPartLookup.LVFilterData(AData: TtiObject; var pInclude: Boolean);
begin
  pInclude := not aData.Deleted ;
end;

end.
