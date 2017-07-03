{ This unit defines the bare minimum type helpers to reduce IFDEF statements
  in the framework code.

  - The TRect record helper is for Delphi XE compatibility with later
    Delphi versions.
}

unit TypeHelpers;

interface

uses
  Types;

type
  TRectHelper = record helper for TRect
  private
    function  GetWidth: Integer;
    procedure SetWidth(const AWidth: Integer);
    function  GetHeight: Integer;
    procedure SetHeight(const AHeight: Integer);
  public
    procedure Inflate(const DX, DY: Integer); overload;
    procedure Inflate(const DL, DT, DR, DB: Integer); overload;
    property  Width: Integer read GetWidth write SetWidth;
    property  Height: Integer read GetHeight write SetHeight;
  end;

implementation

{ TRectHelper }

function TRectHelper.GetHeight: Integer;
begin
  result := bottom - top;
end;

function TRectHelper.GetWidth: Integer;
begin
  result := Right - Left;
end;

procedure TRectHelper.Inflate(const DX, DY: Integer);
begin
  left   := left - DX;
  right  := right + DX;
  top    := top - DY;
  bottom := bottom + DY;
end;

procedure TRectHelper.Inflate(const DL, DT, DR, DB: Integer);
begin
  left   := left - DL;
  right  := right + DR;
  top    := top - DT;
  bottom := bottom + DB;
end;

procedure TRectHelper.SetHeight(const AHeight: Integer);
begin
  bottom := top + AHeight;
end;

procedure TRectHelper.SetWidth(const AWidth: Integer);
begin
  right := left + AWidth;
end;

end.
