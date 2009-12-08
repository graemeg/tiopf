unit tiStructuredCSVWriter;

interface

uses
  tiBaseObject,
  tiStreams;

type

  TtiStructuredCSVWriter = class(TtiBaseObject)
  private
    FStream: TtiPreSizedStream;
    function GetAsString: string;
  protected
    property Stream: TtiPreSizedStream read FStream;
    property AsString: string read GetAsString;
    procedure Write(const AValue: string);
    procedure WriteLn(const AValue: string); overload;
    procedure WriteLn; overload;
    procedure WriteI(const ADataBlockName: string); overload;
    procedure WriteI(const ADataBlockName: string; const AFieldNames: array of String); overload;
    procedure WriteD(const ADataBlockName: string); overload;
    procedure WriteD(const ADataBlockName: string; const AValues: array of String); overload;
    procedure SaveToFile(const AFileName: string);
    procedure DoExecute; virtual; abstract;

  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  tiOPFManager,
  tiConstants;

{ TtiStructuredCSVWriter }

constructor TtiStructuredCSVWriter.Create;
begin
  inherited;
  FStream := TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
end;

destructor TtiStructuredCSVWriter.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TtiStructuredCSVWriter.GetAsString: string;
begin
  Result := FStream.AsString;
end;

procedure TtiStructuredCSVWriter.SaveToFile(const AFileName: string);
begin
  FStream.SaveToFile(AFileName);
end;

procedure TtiStructuredCSVWriter.Write(const AValue: string);
begin
  FStream.Write(AValue);
end;

procedure TtiStructuredCSVWriter.WriteD(const ADataBlockName: string;
  const AValues: array of String);
var
  i: Integer;
begin
  WriteD(ADataBlockName);
  for i:= Low(AValues) to High(AValues) do
    Write(',' + AValues[i]);
  WriteLn;
end;

procedure TtiStructuredCSVWriter.WriteD(const ADataBlockName: string);
begin
  Assert(ADataBlockName <> '', 'ADataBlockName not assigned');
  Write(CStructCSVPrefixD + ',' + ADataBlockName);
end;

procedure TtiStructuredCSVWriter.WriteI(const ADataBlockName: string);
begin
  Assert(ADataBlockName <> '', 'ADataBlockName not assigned');
  Write(CStructCSVPrefixI + ',' + ADataBlockName);
end;

procedure TtiStructuredCSVWriter.WriteI(const ADataBlockName: string;
  const AFieldNames: array of String);
var
  i: Integer;
begin
  WriteI(ADataBlockName);
  for i:= Low(AFieldNames) to High(AFieldNames) do
    Write(',' + AFieldNames[i]);
  WriteLn;
end;

procedure TtiStructuredCSVWriter.WriteLn;
begin
  WriteLn('');
end;

procedure TtiStructuredCSVWriter.WriteLn(const AValue: string);
begin
  FStream.WriteLn(AValue);
end;

end.
