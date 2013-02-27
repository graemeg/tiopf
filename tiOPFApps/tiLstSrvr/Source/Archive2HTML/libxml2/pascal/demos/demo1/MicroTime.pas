UNIT MicroTime;

(*
    Provides time values with a resolution of better than
    a microsecond.

    Works on Pentium and later. Reported to work with 486.
*)



interface

  FUNCTION CurrentTime : DOUBLE;
(* returns the elapsed time in seconds since the unit was started.

   To time some event, call this before the event starts, and
   again after the event ends. Subtract the times to get the elapsed
   time in seconds between the 2 events.

   Returns a negative value if there is a problem or if the
   time can't be read on the current system.
*)

  procedure StartTimer;

  function EndTime: double;

implementation

USES
  FastTime;

VAR
  Timer: TQmwFastTime;


FUNCTION CurrentTime : DOUBLE;
(* returns the elapsed time in seconds since the unit was started.

   To time some event, call this before the event starts, and
   again after the event ends. Subtract the times to get the elapsed
   time in seconds between the 2 events.

   Returns a negative value if there is a problem or if the
   time can't be read on the current system.
*)

BEGIN  (* CurrentTime  *)
  result:=Timer.Elapsed;
END;   (* CurrentTime  *)

procedure StartTimer;
begin
  Timer.Start
end;

function EndTime: double;
begin
  Timer.Stop;
  EndTime:=CurrentTime;
end;


INITIALIZATION

  BEGIN
    Timer:=TQmwFastTime.Create;
  END;

FINALIZATION

  BEGIN
    Timer.Free;
  END;

end.




