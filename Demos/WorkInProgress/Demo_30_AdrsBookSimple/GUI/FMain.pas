unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FtiFormMgrForm, ActnList;

type



  TFormMain = class(TForm)
    tmrCloseForm: TTimer;
    AL: TActionList;
    aViewAddressList: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrCloseFormTimer(Sender: TObject);
    procedure aViewAddressListExecute(Sender: TObject);
  private
    procedure CMCloseActiveForm(var Message: TMessage); message TI_CLOSEACTIVEFORM;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation
uses
  tiGUIINI
  ,tiApplicationMenuSystem
  ,FWorkList
  ,FPersonList
 ;

{$R *.DFM}


procedure TFormMain.aViewAddressListExecute(Sender: TObject);
begin
  FPersonList.Execute;
end;

procedure TFormMain.CMCloseActiveForm(var Message: TMessage);
begin
  tmrCloseForm.Enabled:= true;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  gINI.ReadFormState(Self);
  CreateAMS(Self,
            TFormWorkList,
            '',
            0,
            nil);

  gAMS.AddMainMenuItem('&Address list', 1);
  gAMS.AddMenuItem(aViewAddressList);

  gAMS.AddMenuSidebarGroup('Address list');
  gAMS.AddMenuSidebarItem(aViewAddressList);

  gAMS.FormMgr.ShowForm(TFormWorkList);
            
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState(Self);
end;

procedure TFormMain.tmrCloseFormTimer(Sender: TObject);
begin
  tmrCloseForm.Enabled:= false;
  gAMS.FormMgr.CloseForm(gAMS.FormMgr.ActiveForm);
end;

end.
