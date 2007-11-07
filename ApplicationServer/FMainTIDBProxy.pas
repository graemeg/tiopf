unit FMainTIDBProxy;

{$I tiDefines.Inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls ;

type
  TFormMainTIDBProxyServer = class(TForm)
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
  end;

var
  FormMainTIDBProxyServer: TFormMainTIDBProxyServer;

{$R *.DFM}

implementation

uses
  tiINI,
  tiDBProxyServerDependencies;

procedure TFormMainTIDBProxyServer.FormCreate(Sender: TObject);
begin
  gINI.ReadFormState(Self);
  ConnectToDatabase;
  gTIDBProxy.Start;
  memoLog.Lines.Text := GetDBConnectionMessage ;
end;

procedure TFormMainTIDBProxyServer.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState(Self);
end;

end.
