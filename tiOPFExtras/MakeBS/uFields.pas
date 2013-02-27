unit uFields;

interface

uses
		Classes
	, SysUtils
	, TypInfo
	, tiUtils
	;

type

	TbsVisibility = (bsPrivate, bsProtected, bsPublic, bsPublished);

	TbsField = class(TObject)
		FieldName: String;
		FieldType: TtiTypeKind;
		Visibility: TbsVisibility;
		IsOverride: Boolean;
		IsReintroduce: Boolean;
		UseGetter: Boolean;
		UseSetter: Boolean;
	end;

	TbsFields = class(TObject)
	private
		FList: TList;
		function GetItems(Index: Integer): TbsField;
		procedure SetItems(Index: Integer; const Value: TbsField);
    function GetList: TList;
	public
		constructor Create();
		destructor Destroy; override;
		property Items[Index: Integer]: TbsField read GetItems write SetItems; default;
		property List: TList read GetList;
		function Add: TbsField;
		procedure Delete(const Index: Integer);
		function Count: Integer;
		function IsField(const Name: String): Boolean;
	end;

implementation

uses
		tiPersist
	, tiPtnVisPerObj
	;


{ TFieldList }

function TbsFields.Add: TbsField;
begin
	Result := TbsField.Create;
	FList.Add(Result);
end;

function TbsFields.Count: Integer;
begin
	Result := FList.Count;
end;

constructor TbsFields.Create;
begin
	FList := TList.Create;
end;

procedure TbsFields.Delete(const Index: Integer);
begin
	if Index < FList.Count then
		FList.Delete(Index);
end;

destructor TbsFields.Destroy;
var
	i: Integer;
begin
	while FList.Count > 0 do
		TbsField(FList[0]).Free;
	FList.Free;
	
	inherited;
end;

function TbsFields.GetItems(Index: Integer): TbsField;
begin
	Result := TbsField(FList[Index]);
end;

function TbsFields.GetList: TList;
begin
	Result := FList;
end;

function TbsFields.IsField(const Name: String): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i := 0 to FList.Count -1 do
	begin
		Result := SameText(Items[i].FieldName, Name);
		if Result then Exit;
	end;
end;

procedure TbsFields.SetItems(Index: Integer; const Value: TbsField);
begin
	FList[Index] := Value;
end;

end.
