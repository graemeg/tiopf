unit FPopulateDatabase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiButtons, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, StdCtrls,
  tiMemoReadOnly;

type
  TFormPopulateDatabase = class(TForm)
    tiMemoReadOnly1: TtiMemoReadOnly;
    paeCount: TtiPerAwareFloatEdit;
    tiButtonPanel1: TtiButtonPanel;
    procedure tiButtonPanel1Btn1Click(Sender: TObject);
    procedure tiButtonPanel1Btn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    class function Execute: boolean;
    procedure CreateTable;
  end;


implementation
uses
  Client_BOM
  ,tiOPFManager
  ,tiQuery
  ,tiDBConnectionPool
 ;

{$R *.DFM}

{ TFormPopulateDatabase }

class function TFormPopulateDatabase.Execute: boolean;
var
  lForm: TFormPopulateDatabase;
begin
  lForm:= TFormPopulateDatabase.Create(nil);
  try
    result:= lForm.ShowModal = mrOK;
  finally
    lForm.Free;
  end;
end;

procedure TFormPopulateDatabase.tiButtonPanel1Btn1Click(Sender: TObject);
  function _GetClientName: string;
  var
    lLen: integer;
    lChar: integer;
  const
    cLetters = 'ABCDEF GHIJKL MNOPQR STUVWX YZ';
  begin
    result:= '';
    lLen:= Trunc(Random * 50) + 4;
    repeat
      lChar:= Trunc(Random * 29)+1;
      Result:= Result + cLetters[lChar];
    until length(trim(result)) >= lLen;
    result:= Trim(Result);
  end;
var
  lClients: TClients;
  lClient : TClient;
  i       : integer;
begin
  Screen.Cursor:= crHourGlass;
  try
    Randomize;
    lClients:= TClients.Create;
    try
      for i:= 1 to Trunc(paeCount.Value)do
      begin
        lClient:= TClient.CreateNew;
        lClient.ClientID:= Copy(IntToStr(GetTickCount), 1, 9);
        lCLient.ClientName:= _GetClientName;
        lClients.Add(lClient);
      end;
      lClients.Save;
    finally
      lClients.Free;
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
  ModalResult:= mrOK;
end;

procedure TFormPopulateDatabase.tiButtonPanel1Btn2Click(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TFormPopulateDatabase.FormCreate(Sender: TObject);
begin
  paeCount.Value:= 5000;
  if GTIOPFManager.TableExists('Client') then
    GTIOPFManager.DropTable('Client');
  CreateTable;
end;

// Create table
procedure TFormPopulateDatabase.CreateTable;
var
  lTableMetaData: TtiDBMetaDataTable;
begin
  lTableMetaData:= TtiDBMetaDataTable.Create;
  try
    lTableMetaData.Name:= 'Client';
    lTableMetaData.AddInstance('OID',               qfkString,  36); // Using GUID OIDs
    lTableMetaData.AddInstance('Client_Name',       qfkString, 200);
    lTableMetaData.AddInstance('Client_ID',         qfkString,  9);
    GTIOPFManager.CreateTable(lTableMetaData);
  finally
    lTableMetaData.Free;
  end;
end;

end.
