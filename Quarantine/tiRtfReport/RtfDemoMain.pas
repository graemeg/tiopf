unit RtfDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, TypInfo, ContNrs, tiRtfParser, Db,
  SqlExpr, tiPtnVisPerObj, ShellAPI, ActnList, DBXpress, DBTables;


type
  //Add extra functions in your rtfreport
  TRtfPrivateParser = class(TtiRtfParser)
  protected
    procedure AddFunctions; override;
    procedure UdfBla(AArgument: TRtfArgument);
  end;


  TFrmRtfDemoMain = class(TForm)
    ActionList1: TActionList;
    ActionParse: TAction;
    ActionShow: TAction;
    tbBioLife: TTable;
    DatabaseFishAct: TDatabase;
    Label1: TLabel;
    ButtonParse: TButton;
    ButtonShow: TButton;
    DatabaseMastSql: TSQLConnection;
    ActionEdit: TAction;
    Button1: TButton;
    procedure ActionParseExecute(Sender: TObject);
    procedure ActionShowExecute(Sender: TObject);
    procedure ActionEditExecute(Sender: TObject);
  protected
    procedure OnPictureAttr(APictureAttr: TRtfPictureAttr);
    procedure OnCreateDataset(ADatabase, AAliasName, ASql: string; AArgument: TRtfArgument);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


var
  FrmRtfDemoMain: TFrmRtfDemoMain;


implementation

{$R *.dfm}

type
  //Simple bom structure, nothing fancy
  TtiNestedDemoItem = class(TPerObjAbs)
  private
    FId: integer;
    FName: string;
  published
    property Id: integer read FId write FId;
    property Name: string read FName write FName;
  end;


  TtiNestedDemoItems = class(TPerObjList)
  protected
    function GetItem(Index: integer): TtiNestedDemoItem;
  public
    property Items[Index: integer]: TtiNestedDemoItem read GetItem;
  end;


  TtiDemoItem = class(TPerObjAbs)
  private
    FId: integer;
    FName: string;
    FData: TtiNestedDemoItem;
    FList: TtiNestedDemoItems;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Id: integer read FId write FId;
    property Name: string read FName write FName;
    property oData: TtiNestedDemoItem read FData;
    property oList: TtiNestedDemoItems read FList;
  end;


  TtiDemoItems = class(TPerObjList)
  protected
    function GetItem(Index: integer): TtiDemoItem;
  public
    procedure Populate;
    property Items[Index: integer]: TtiDemoItem read GetItem;
  end;

  { TtiNestedDemoItems }

function TtiNestedDemoItems.GetItem(Index: integer): TtiNestedDemoItem;
begin
  Result := TtiNestedDemoItem(inherited GetItems(Index));
end;


{ TtiDemoItem }

constructor TtiDemoItem.Create;
begin
  inherited;
  FData := TtiNestedDemoItem.Create;
  FList := TtiNestedDemoItems.Create;
end;

destructor TtiDemoItem.Destroy;
begin
  FData.Free;
  FList.Free;
  inherited;
end;


{ TtiDemoItems }

function TtiDemoItems.GetItem(Index: integer): TtiDemoItem;
begin
  Result := TtiDemoItem(inherited GetItems(Index));
end;

procedure TtiDemoItems.Populate;
var i, j: integer;
  ADemoItem: TtiDemoItem;
  ANestedDemoItem: TtiNestedDemoItem;
begin
  for i := 1 to 10 do begin
    ADemoItem := TtiDemoItem.Create;
    ADemoItem.Id := i;
    ADemoItem.Name := Format('This is demo item %d',[i]);
    ADemoItem.oData.Id := i * 1000;
    ADemoItem.oData.Name := Format('This is demo data item %d',[i * 1000]);
    Add(ADemoItem);

    for j := 1 to 5 do begin
      ANestedDemoItem := TtiNestedDemoItem.Create;
      ANestedDemoItem.Id := j;
      ANestedDemoItem.Name := Format('This is nested demo item %d',[j]);
      ADemoItem.oList.Add(ANestedDemoItem);
    end;
  end;
end;

{ TForm1 }

constructor TFrmRtfDemoMain.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TFrmRtfDemoMain.Destroy;
begin
  inherited;
end;

procedure TFrmRtfDemoMain.OnCreateDataset(ADatabase, AAliasName, ASql: string; AArgument: TRtfArgument);
//Return a TDataset descendant or a TPerObjAbs(List). It will be automaticly
//be freed (can imagine this would be a future 4th parameter of this event)
var AQuery: TSQLQuery;
  i: integer;
begin
  if SameText(ADatabase, 'MastSql')then try
    //I can imagine you would use one of the tiQuery descendants, but
    //the parser is not prepared for those kind of descendants yet.
    //TRtfDataset can be easely adapted for tiQuery descendants though.
    AQuery := TSQLQuery.Create(nil);
    AQuery.SqlConnection := DatabaseMastSql;
    AQuery.Sql.Text := ASql;
    AQuery.Prepared := true;
    if AQuery.Params.Count > 0 then begin
      if AQuery.Params.Count > AArgument.Count
      then raise Exception.CreateFmt('Invalid amount of params in query %s',[AAliasName]);
      for i := 0 to AQuery.Params.Count - 1 do begin
        AQuery.Params[i].Value := AArgument[i].Value;
      end;
    end;
    AArgument.Token := etDataset;
    AArgument.Value := integer(AQuery);
  except
    FreeAndNil(AQuery);
    raise;
  end;
end;

procedure TFrmRtfDemoMain.ActionParseExecute(Sender: TObject);
var AStart: TDateTime;
  FParser: TRtfPrivateParser;
  ADemoItems: TtiDemoItems;
begin
  ADemoItems := TtiDemoItems.Create;
  try
    ADemoItems.Populate;

    try
      AStart := Now;

      Label1.Caption := 'working';
      ActionShow.Enabled := false;
      ActionParse.Enabled := false;
      Screen.Cursor := crHourGlass;
      FParser := TRtfPrivateParser.Create;
      try
        FParser.OnPictureAttr := OnPictureAttr;
        FParser.OnCreateDataset := OnCreateDataset;
        FParser.Datasets.Add(ADemoItems, 'DemoItems');
        FParser.Datasets.Add(tbBioLife, 'BioLife');
        FParser.LoadFromFile('Demo.Rtf');
        FParser.Execute;
        FParser.SaveToFile('Results.rtf');
      finally
        FParser.Free;
        Screen.Cursor := crDefault;
        ActionShow.Enabled := true;
        ActionParse.Enabled := true;
      end;

      Label1.Caption := Format('Session completed in %s',[FormatDateTime('hh:nn:ss:zzz', Now - AStart)]);
      ActionShow.Execute;
    except
      on E: Exception do begin
        MessageDlg(E.Message, mtError,[mbOk], 0);
      end;
    end;

  finally
    ADemoItems.Free;
  end;
end;

procedure TFrmRtfDemoMain.OnPictureAttr(APictureAttr: TRtfPictureAttr);
begin
  APictureAttr.BorderColor := clRed;
  APictureAttr.BorderType := brDot;
  APictureAttr.BorderWidth := 2;
end;


{ TRtfPrivateParser }

procedure TRtfPrivateParser.AddFunctions;
begin
  inherited;
  Functions.Add(etFunction, 'Bla', 0, 0, UdfBla);
end;

procedure TRtfPrivateParser.UdfBla(AArgument: TRtfArgument);
begin
  AArgument.Token := etLitString;
  AArgument.Value := 'Best value is 1.5e+400';
end;

procedure TFrmRtfDemoMain.ActionShowExecute(Sender: TObject);
begin
  try
    ShellExecute(0, nil, PChar('Results.rtf'), nil, nil, SW_SHOW);
  except
    on E: Exception do begin
      MessageDlg(E.Message, mtError,[mbOk], 0);
    end;
  end;
end;

procedure TFrmRtfDemoMain.ActionEditExecute(Sender: TObject);
begin
  try
    ShellExecute(0, nil, PChar('Demo.rtf'), nil, nil, SW_SHOW);
  except
    on E: Exception do begin
      MessageDlg(E.Message, mtError,[mbOk], 0);
    end;
  end;
end;

end.





