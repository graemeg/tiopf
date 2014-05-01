unit BreadCrumbs;

interface

uses
  StdCtrls; // TLabel

type
  IBreadCrumbs = interface
    ['{235CF78E-9B50-4A54-9635-647AFF4FB0D8}']
    procedure Push(const Value: string);
    function Peek: string;
    procedure Pop;
    procedure Clear;
  end;

function CreateBreadCrumbs(const ALabel: TLabel): IBreadCrumbs;

implementation

uses
  Classes,  // TStringXXX
  sysUtils;   // Trim

const
  cBreadCrumbDelimiter: Char = '>';

type
  TBreadCrumbs = class(TInterfacedObject, IBreadCrumbs)
  private
    FBreadCrumbs: TStrings;
    FLabel: TLabel;
  protected
    procedure Push(const Value: string);
    function Peek: string;
    procedure Pop;
    procedure Clear;
  public
    constructor Create(const ALabel: TLabel);
    destructor Destroy; override;
  end;

function CreateBreadCrumbs(const ALabel: TLabel): IBreadCrumbs;
begin
  Result := TBreadCrumbs.Create(ALabel);
end;

{ TBreadCrumbs }

procedure TBreadCrumbs.Clear;
begin
  FBreadCrumbs.Clear;
  FLabel.Caption := FBreadCrumbs.DelimitedText;
end;

constructor TBreadCrumbs.Create(const ALabel: TLabel);
begin
  FLabel := ALabel;
  FBreadCrumbs := TStringList.Create;
  FBreadCrumbs.StrictDelimiter := true;
  FBreadCrumbs.Delimiter := cBreadCrumbDelimiter;
end;

destructor TBreadCrumbs.Destroy;
begin
  FBreadCrumbs.Free;
  inherited;
end;

function TBreadCrumbs.Peek: string;
begin
  if FBreadCrumbs.Count > 0 then
    Result := Trim(FBreadCrumbs[FBreadCrumbs.Count - 1])
  else
    Result := '';
end;

procedure TBreadCrumbs.Pop;
begin
  if FBreadCrumbs.Count > 0 then
  begin
    FBreadCrumbs.Delete(FBreadCrumbs.Count - 1);
    FLabel.Caption := FBreadCrumbs.DelimitedText;
  end;
end;

procedure TBreadCrumbs.Push(const Value: string);
begin
  FBreadCrumbs.Add(Format(' %s ', [Trim(Value)]));
  FLabel.Caption := FBreadCrumbs.DelimitedText;
end;

end.
