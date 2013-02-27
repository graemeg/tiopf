unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, tiCriteria, StdCtrls, ExtCtrls,
  tiPerObjOIDInteger;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public
    FCriteria: TPerCriteria;
    FOutroCriteria: TPerCriteria;
    FOrCriteria: TPerCriteria;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  tiPtnVisPerObj_Cli, tiPtnVisCriteria;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCriteria := TPerCriteria.Create('Geral');
  FOutroCriteria := TPerCriteria.Create('Outro');
  FOrCriteria := TPerCriteria.Create('Or');

  { first criteria }
  with FOutroCriteria do
    begin
      AddBetween('TIPO_CLIENTE', '1', '2');
      AddEqualTo('NOME', '"OUTRO"');
      AddBetween('CODIGO', '"00"', '"10"');
      AddOrderBy('ATIVO');
    end;
    
  { second criteria }
  with FOrCriteria do
    begin
      AddEqualTo('ATIVO', '"N"');
      AddGroupBy('ATIVO');
    end;

  { third and principal criteria }
  with FCriteria do
    begin
      AddEqualTo('CODIGO', '"TESTE"');
      { criteria "anded" with first criteria }
      AddAndCriteria(FOutroCriteria);
      AddGreaterOrEqualThan('CLIENTE_ID', '100');
      { criteria "ored" with second criteria }
      AddOrCriteria(FOrCriteria);
      AddGroupBy('TIPO_CLIENTE');
      AddOrderBy(['CODIGO', 'NOME', 'TIPO_CLIENTE']);
    end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FCriteria.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Text := tiPerCriteriaAsSQL(FCriteria);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 Memo1.Text := tiPerObjAbsAsString(FCriteria);
end;

end.

