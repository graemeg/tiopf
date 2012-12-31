unit tiCGIParams;

interface
uses
  tiBaseObject
  ,Classes
  ;

type
  TtiCGIParams = class;

  // ToDo: TtiCGIParams will create a param string based on a TStringList.Text
  //       and compress/encode the string. Might be better to create a param
  //       string like ?param_name1=param_value1?param_name2=param_value2
  TtiCGIParams = class( TtiBaseObject )
  private
    FStrings : TStringList;
    function    GetAsString: string;
    procedure   SetAsString(const Value: string);
    function    GetValues(const pName: string): string;
    procedure   SetValues(const pName, Value: string);
    function    GetCompressedEncodedAsString: string;
    procedure   SetCompressedEncodedAsString(const Value: string);
    function    GetCount: integer;
    function    GetParam(const Index: integer): string;
    function    GetParamName(const Index: integer): string;
    function    GetParamValue(const Index: integer): string;
  public
    constructor Create;
    destructor  Destroy; override ;
    property    Values[const pName: string]: string read GetValues Write SetValues;
    property    Params[const Index: integer]: string read GetParam; default;
    property    ParamNames[const Index: integer]: string read GetParamName;
    property    ParamValues[const Index: integer]: string read GetParamValue;
    property    AsString: string read GetAsString Write SetAsString;
    property    AsCompressedEncodedString: string read GetCompressedEncodedAsString Write SetCompressedEncodedAsString;
    property    Count: integer read GetCount;
    procedure   Assign(const ASource: TtiCGIParams);
    procedure   Clear;
  end;

implementation
uses
  tiXML
  ;

{ TtiCGIParams }

constructor TtiCGIParams.Create;
begin
  inherited;
  FStrings := TStringList.Create;
end;

destructor TtiCGIParams.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TtiCGIParams.Assign(const ASource: TtiCGIParams);
begin
  AsString:= ASource.AsString;
end;

procedure TtiCGIParams.Clear;
begin
  FStrings.Clear;
end;

function TtiCGIParams.GetAsString: string;
begin
  Result := FStrings.Text;
end;

function TtiCGIParams.GetCompressedEncodedAsString: string;
var
  ls : string;
begin
  ls := FStrings.Text ;
  if ls <> '' then
    Result := tiCompressEncode(ls)
  else
    Result := '' ;
end;

function TtiCGIParams.GetCount: integer;
begin
  result:= FStrings.Count;
end;

function TtiCGIParams.GetParam(const Index: integer): string;
begin
 Result := FStrings[Index];
end;

function TtiCGIParams.GetParamName(const Index: integer): string;
begin
  Result := FStrings.Names[Index];
end;

function TtiCGIParams.GetParamValue(const Index: integer): string;
begin
  Result := FStrings.Values[GetParamName(Index)];
end;

function TtiCGIParams.GetValues(const pName: string): string;
begin
  Result := FStrings.Values[pName];
end;

procedure TtiCGIParams.SetAsString(const Value: string);
begin
  FStrings.Text := Value;
end;

procedure TtiCGIParams.SetCompressedEncodedAsString(const Value: string);
var
  ls: string;
begin
  if Value <> '' then
    ls := tiDeCompressDecode(Value)
  else
    ls := '' ;
  FStrings.Text := ls;
end;

procedure TtiCGIParams.SetValues(const pName, Value: string);
begin
  FStrings.Values[pName] := Value;
end;

end.
