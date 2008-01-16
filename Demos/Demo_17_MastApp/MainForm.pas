unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Buttons, ExtCtrls;

type
  TfrmMain = class(TForm)
    MainPanel: TPanel;
    OrderBtn: TSpeedButton;
    BrowseBtn: TSpeedButton;
    PartsBtn: TSpeedButton;
    CloseBtn: TSpeedButton;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewOrder: TMenuItem;
    N5: TMenuItem;
    FilePrintReport: TMenuItem;
    PrintCustList: TMenuItem;
    PrintOrders: TMenuItem;
    PrintInvoice: TMenuItem;
    FilePrinterSetup: TMenuItem;
    N4: TMenuItem;
    FileExit: TMenuItem;
    ViewMenu: TMenuItem;
    ViewOrders: TMenuItem;
    ViewPartsInventory: TMenuItem;
    N7: TMenuItem;
    ViewStayOnTop: TMenuItem;
    HelpMenu: TMenuItem;
    HelpAbout: TMenuItem;
    PrinterSetup: TPrinterSetupDialog;
    N2: TMenuItem;
    Vendors1: TMenuItem;
    procedure Customers1Click(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Vendors1Click(Sender: TObject);
    procedure PartsBtnClick(Sender: TObject);
    procedure ViewOrdersClick(Sender: TObject);
    procedure OrderBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses CustomerListU, modSharedU;

{$R *.dfm}

procedure TfrmMain.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.Customers1Click(Sender: TObject);
begin
  modShared.EditCustomers();
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ClientWidth := CloseBtn.Left + CloseBtn.Width + 1;
  ClientHeight := CloseBtn.Top + CloseBtn.Height;
  MainPanel.Align := alClient;
  { position the form at the top of display }

end;

procedure TfrmMain.OrderBtnClick(Sender: TObject);
begin
  modShared.CreateNewOrder;
end;

procedure TfrmMain.PartsBtnClick(Sender: TObject);
begin
  modShared.EditParts;
end;

procedure TfrmMain.Vendors1Click(Sender: TObject);
begin
  modShared.EditVendors;
end;

procedure TfrmMain.ViewOrdersClick(Sender: TObject);
begin
  modShared.EditCustomers;
end;

end.
