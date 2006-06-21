unit tiDUnitINI;

interface
uses
  INIFiles
  ,Registry
  ;

const
  cErrorCanNotSetINIFileToReadWrite = 'INI file <%s> is read-only and can not be set to read-write.' ;

function gDUnitINI : TINIFile ;
function gDUnitReg : TRegINIFile ;


implementation
uses
  tiUtils
  ,tiExcept
  ,SysUtils
  ;

var
  uINI : TINIFile ;
  uReg : TRegINIFile ;


function gDUnitINI : TINIFile ;
var
  lFileName : string ;
begin
  if uINI = nil then
  begin
    lFileName := tiGetEXEPath + PathDelim + 'DUnitTIOPF.ini';
    if FileExists(lFileName) then
    begin
      if tiIsFileReadOnly(lFileName) then
        tiSetFileReadOnly(lFileName,false);
      if tiIsFileReadOnly(lFileName) then
        raise EtiOPFUserException.CreateFmt(cErrorCanNotSetINIFileToReadWrite,
                   [lFileName]);
    end;
    uINI := TINIFile.Create( lFileName ) ;
  end ;
  result := uINI;
end;


function gDUnitReg : TRegINIFile ;
begin                      
  if uReg = nil then
    uReg := TRegINIFile.Create( 'DUnitTIOPF' ) ;
  result := uReg;
end ;


initialization

finalization
  uINI.Free ;
  uReg.Free ;
  
end.

