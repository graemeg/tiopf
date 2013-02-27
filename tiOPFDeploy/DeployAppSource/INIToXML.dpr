program INIToXML;

{$APPTYPE CONSOLE}

uses
  SysUtils
  ,tiCommandLineParams
  ,INIFiles
  ,Classes
  ;

procedure _INIToXML(const pINIFile : TINIFile ; const pXMLFile : TStringList ;
                    const pIndent : string );
var
  ls : string ;
begin
  WriteLn('  Processing ' + pIndent);
  ls := '        <' + pIndent + '>' +
        pINIFile.ReadString('Report_Summary', pIndent, 'Unknown') +
        '</' + pIndent + '>' ;
  pXMLFile.Add(ls);
end ;


var
  lINIFileName : string ;
  lXMLFileName : string ;
  lINIFile     : TINIFile ;
  lXMLFile     : TStringList ;
begin
  try
    lINIFileName := gCommandLineParams.GetParam('ini');
    lXMLFileName := gCommandLineParams.GetParam('xml');
    WriteLn('Converting ' + lINIFileName ) ;
    WriteLn('To         ' + lXMLFileName ) ;
    lINIFile := TINIFile.Create(lINIFileName);
    try
      lXMLFile := TStringList.Create;
      try
        lXMLFile.Add('<?xml version="1.0"?>');
        lXMLFile.Add('<buildlog>');
        lXMLFile.Add('    <Item>');
        _INIToXML(lINIFile, lXMLFile, 'build_date');
        _INIToXML(lINIFile, lXMLFile, 'build_version');
        _INIToXML(lINIFile, lXMLFile, 'fb_error_count');
        _INIToXML(lINIFile, lXMLFile, 'd5_error_count');
        _INIToXML(lINIFile, lXMLFile, 'd6_error_count');
        _INIToXML(lINIFile, lXMLFile, 'd7_error_count');
        _INIToXML(lINIFile, lXMLFile, 'tiopf_install');
        _INIToXML(lINIFile, lXMLFile, 'tiopfdemos_install');
        _INIToXML(lINIFile, lXMLFile, 'tiopfextras_install');
        _INIToXML(lINIFile, lXMLFile, 'tiopfhelp_install');
        _INIToXML(lINIFile, lXMLFile, 'tiopfsupportapps_install');
        lXMLFile.Add('    </Item>');
        lXMLFile.Add('</buildlog>');
        lXMLFile.SaveToFile(lXMLFileName);
      finally
        lXMLFile.Free;
      end ;
    finally
      lINIFile.Free;
    end ;
  except
    on e:exception do
    begin
      WriteLn(e.message);
      ExitCode := 1 ;
    end ;
  end ;
end.
