{--------------------------------------------------------------------}
{ QmwFastTime V 1.0                                                  }
{ Written by Martin Waldenburg 2001.                                 }
{ Copyright by Martin Waldenburg 2001. All rights reserved.          }
{ This is FreeWare.                                                  }
{ It's provided as is, without a warranty of any kind.               }
{ You use it at your own risc.                                       }
{ You may use and distribute it freely.                              }
{ But You may not say it's your work                                 }
{ If you distribute it you must provide the File.                    }
{--------------------------------------------------------------------}
(*
Some changes by:
Uwe Fechner <ufechner@csi.com>
*)


unit FastTime;

interface

uses
{$IFDEF LINUX}
  SysUtils, Classes, Libc;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, SysUtils, Classes;
{$ENDIF}

type
  TQmwFastTime = class
  private
    StartValue, StopValue: Int64;
    FFrequency: Int64;
    FCalibrate: Int64;
    {$IFDEF LINUX}
      FCalibrateUSleep: Int64;
    {$ENDIF}
    function GetElapsedTime: ShortString;
    function GetElapsed: Extended;
    {$IFDEF LINUX}
      procedure MeasureFrequency;
      procedure CalibrateLinux;
      procedure CalibrateUSleep;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
      procedure CalibrateWindows;
    {$ENDIF}
  protected
    property Calibrate: Int64 read FCalibrate;
  public
    constructor Create;
    function GetFrequency: Int64;
    procedure Start;
    procedure Stop;
    property Elapsed: Extended read GetElapsed;
    property ElapsedTime: ShortString read GetElapsedTime;
  end;

implementation

function RDTSC:Int64;
asm
  dw 310Fh
end;

constructor TQmwFastTime.Create;
begin
  {$IFDEF LINUX}
    CalibrateLinux;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    CalibrateWindows;
  {$ENDIF}
  GetFrequency;
end;

function TQmwFastTime.GetElapsed: Extended;
begin
  Result := (StopValue-StartValue- FCalibrate)/fFrequency;
end;

function TQmwFastTime.GetElapsedTime: ShortString;
begin
  Result := format('Seconds: %g', [GetElapsed]);
end;

function TQmwFastTime.GetFrequency: Int64;
begin
{$IFDEF LINUX}
  MeasureFrequency;
{$ENDIF}
{$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(FFrequency);
{$ENDIF}
  Result:= FFrequency;
 {on Linux this is the real Frequency, but not on Windows}
end;

{$IFDEF LINUX}
procedure TQmwFastTime.CalibrateLinux;
var
  Val1, Val2, Val3, Val4, Val5: Int64;
begin
  USleep(1);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val1:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val2:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val3:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val4:= (StopValue - StartValue);

  StartValue:= RDTSC;
  StopValue:=  RDTSC;
  Val5:= (StopValue - StartValue);

  FCalibrate:=  (Val1 + Val2 + Val3 + Val4 + Val5)div 5;
end;

procedure TQmwFastTime.CalibrateUSleep;
var
  Val1, Val2, Val3, Val4, Val5: Int64;
begin
  USleep(1);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val1:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val2:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val3:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val4:= (StopValue - StartValue);

  StartValue:= RDTSC;
  USleep(0);
  StopValue:=  RDTSC;
  Val5:= (StopValue - StartValue);

  FCalibrateUSleep:=  (Val1 + Val2 + Val3 + Val4 + Val5)div 5;
end;

procedure  TQmwFastTime.MeasureFrequency;
var
  Val1, Val2, Val3, Val4, Val5: Int64;
begin
  CalibrateUSleep;

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val1:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val2:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val3:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val4:= (StopValue - StartValue -FCalibrateUSleep);

  StartValue:= RDTSC;
  USleep(1);
  StopValue:=  RDTSC;
  Val5:= (StopValue - StartValue -FCalibrateUSleep);

  FFrequency:= (Val1 + Val2 + Val3 + Val4 + Val5)div 5 * 100;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure TQmwFastTime.CalibrateWindows;
begin
  QueryPerformanceCounter(StartValue);
  QueryPerformanceCounter(StopValue);
  FCalibrate:= StopValue - StartValue;
end;
{$ENDIF}

procedure TQmwFastTime.Start;
begin
{$IFDEF LINUX}
  StartValue:= RDTSC;
{$ENDIF}
{$IFDEF MSWINDOWS}
  QueryPerformanceCounter(StartValue);
{$ENDIF}
end;

procedure TQmwFastTime.Stop;
begin
{$IFDEF LINUX}
  StopValue:= RDTSC;
{$ENDIF}
{$IFDEF MSWINDOWS}
  QueryPerformanceCounter(StopValue);
{$ENDIF}
end;

end.

