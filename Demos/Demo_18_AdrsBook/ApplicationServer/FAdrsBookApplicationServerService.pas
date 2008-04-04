unit FAdrsBookApplicationServerService;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FMainTIDBProxyService;

type
  TFormAdrsBookApplicationServer = class(TtiDBProxyServer)
  private
  protected
    procedure ConnectToDatabase; override;
  public
    { Public declarations }
  end;

var
  FormAdrsBookApplicationServer: TFormAdrsBookApplicationServer;

implementation
uses
  tiOPFManager;

{$R *.dfm}

{ TFormAdrsBookApplicationServer }

procedure TFormAdrsBookApplicationServer.ConnectToDatabase;
begin
  GTIOPFManager.ConnectDatabase(
    'adrs', 'adrs.fdb', 'SYSDBA', 'masterkey', '');
end;

end.
