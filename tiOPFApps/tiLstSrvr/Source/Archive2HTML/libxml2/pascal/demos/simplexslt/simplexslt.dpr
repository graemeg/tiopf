program simplexslt;

uses
  SysUtils,
  libxml2,
  libxslt;

{$APPTYPE CONSOLE}

const
  C_EOL = #13#10;
  XsltUsageText =
    'simplexslt (XSLT processor based on libxslt translated to delphi)' + C_EOL +
    '      see: http://sf.net/projects/libxml2-pas for more info' + C_EOL +
    'Written by Michael Pakhantsov.' + C_EOL + C_EOL +
    'Usage:'                                                     + C_EOL +
    '  simplexslt source stylesheet [options] [param=value...]'  + C_EOL +
    '                                                   '        + C_EOL +
    'Options:'                                                   + C_EOL +
    '  -h, -H, -?          Displays this help text.'             + C_EOL +
    '  -o filename         write output to named file.'          + C_EOL +
    '  -m startMode        Start transform with this mode';

var
  cur : xsltStylesheetPtr;
  doc : xmlDocPtr;
  res : xmlDocPtr;
  tc  : xsltTransformContextPtr;

  Fmode :String;
  FStyleSheetFile : String;
  FInputFile : String;
  FOutputFile : String;
  FXSLTParams : array of String;
  FXSLTParamCount : Integer;

procedure Usage;
begin
  Writeln(XsltUsageText);
end;

function ParseOption(var N :Integer; Switch: string):boolean;
var
  EqPos: Integer;
  Parameter: String;
  Value: String;
begin
  Result := True;
  if ((Switch = 'h') or (Switch = '?')) then begin
    Usage;
    Halt(2);
  end else if ((Switch = 'm') or (Switch = 'mode')) then begin
    Inc(N);
    FMode := ParamStr(N);
  end else if (Switch = 'o') then begin
    Inc(N);
    FOutputFile := ParamStr(N);
  end else if (Pos('=', Switch) > 0) then begin
    EqPos := Pos('=', Switch);
    SetLength(FXSLTParams, Length(FXSLTParams) + 2);
    Parameter := Copy(Switch, 1, EqPos-1);
    SetLength(FXSLTParams[FXSLTParamCount], Length(Parameter));
    FXSLTParams[FXSLTParamCount] := Parameter;
    Value := '"' + Copy(Switch, EqPos+1, Length(Switch)) +  '"';
    Inc(FXSLTParamCount);
    SetLength(FXSLTParams[FXSLTParamCount], Length(Value));
    FXSLTParams[FXSLTParamCount] := Value;
    Inc(FXSLTParamCount);
  end else begin
    Result := False;
  end;
end;

procedure ParseCommandLine(p : integer);
var
  Param: string;
begin
  try
    while (p <= ParamCount) do begin
      Param := ParamStr(p);
      if (Param[1] in ['-','/']) then begin
        if not ParseOption(p, Copy(Param, 2, Length(Param))) then begin
          raise Exception.Create('Unknown commandline option: ' + Param);
        end;
      end else if not ParseOption(p, Param) then begin
        raise Exception.Create('Don''t know what to do with argument : ' + Param);
      end;
      Inc(p);
    end;
  except
    on e: Exception do begin
      writeln(e.Message);
    end;
  end;
end;

begin
  FXSLTParamCount := 0;
  Fmode := '';
  SetLength(FXSLTParams, 1);
  FOutputFile := '';
  if (ParamCount < 2) then begin
    Usage;
    Halt(2);
  end;
  FInputFile := ParamStr(1);
  FStyleSheetFile := ParamStr(2);
  parseCommandLine(3);

  FXSLTParams[FXSLTParamCount] := '';

  xmlSubstituteEntitiesDefault(1);
  cur := xsltParseStylesheetFile(pchar(FStyleSheetFile));
  doc := xmlParseFile(pchar(FInputFile));
  tc := xsltNewTransformContext(cur, doc);
  if (FMode <> '') then begin
    tc.Mode := pchar(FMode);
  end;
  res := xsltApplyStylesheetUser(cur, doc, ppchar(FXSLTParams), nil, nil, tc);
  if (FOutputFile = '') then begin
    xsltSaveResultToFd(1, res, cur)
  end else begin
    xsltSaveResultToFileName(pchar(FOutputFile), res, cur, 0);
  end;
  xsltFreeTransformContext(tc);
  xsltFreeStylesheet(cur);
  xmlFreeDoc(res);
  xmlFreeDoc(doc);
  xsltCleanupGlobals();
  xmlCleanupParser();
end.

