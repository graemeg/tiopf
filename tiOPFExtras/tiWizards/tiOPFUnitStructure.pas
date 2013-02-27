unit tiOPFUnitStructure;

interface

uses
  tiOPFPlaceholders;

type
  TUnitStructure = class
  private
    CurrentClass: TPlaceholder;
  public
    Placeholders: TPlaceholders;
    constructor Create;
    destructor Destroy; override;
    function ProcessTemplate(TemplateFile: string): string;
  end;

implementation

uses
  SysUtils, INIFiles, tiOPFWizardsGlobal, tiOPFBooleanStack, tiOPFProperty, Dialogs;

{ TUnitStructure }

constructor TUnitStructure.Create;
var
  iniWiz: TIniFile;
  sTmp: string;
  NewNames: TModuleNames;
begin
  Placeholders := TPlaceholders.Create(TPlaceholder);

  // Automatically add some pre-defined Placeholders
  NewNames := TModuleNames.Create;
  Placeholders.Add('%Unit%', NewNames.UnitName);  // Module Name
  Placeholders.Add('%Form%', NewNames.FormName);  // Form Name
  NewNames.Free;
  iniWiz := TIniFile.Create(cgWizardINIFile);
  sTmp := iniWiz.ReadString('Main', 'Author', '');
  if sTmp <> '' then
    Placeholders.Add('%Author%', sTmp);           // Author name

  sTmp := iniWiz.ReadString('Main', 'Company', '');
  if sTmp <> '' then
    Placeholders.Add('%Company%', sTmp);          // Company name
  iniWiz.Free;
end;

destructor TUnitStructure.Destroy;
begin
  Placeholders.Free;

  inherited;
end;

function TUnitStructure.ProcessTemplate(TemplateFile: string): string;
var
  F: Text;
  NxtLn, NxtLnCaps: string;
  BoolStack: TtiBooleanStack;
  iniWiz: TINIFile;
  FieldPrefix, ReadPrefix, WritePrefix, WriteParam: string;

  procedure QualifyOutput;
  var
    i: Integer;
  begin
    Assert(Copy(NxtLnCaps, 1, 8) = '{$IFDEF ');

    for i := 0 to Placeholders.Count-1 do
      if Pos(TPlaceholder(Placeholders.Items[i]).SearchText, NxtLnCaps) > 0 then
      begin
        BoolStack.Push(TPlaceholder(Placeholders.Items[i]).ReplaceText <> '');
        Break;
      end;
  end;

  function OutputClassSection(Section: TPropertyVisibility): string;
  var
    i: Integer;
  begin
    Assert(CurrentClass <> nil);

    Result := '';
    if Section = pvPrivate then
    begin
      // Output the property fields
      for i := 0 to CurrentClass.Properties.Count-1 do
      begin
        if TProperty(CurrentClass.Properties.Items[i]).Visibility = pvPrivate then
          Result := Result + '    '
            + TProperty(CurrentClass.Properties.Items[i]).Name
            + ': ' + TProperty(CurrentClass.Properties.Items[i]).DataType
            + ';' + CRLF;
        if (TProperty(CurrentClass.Properties.Items[i]).Visibility >= pvProtected)
          and ((TProperty(CurrentClass.Properties.Items[i]).ReadAccess in [paField, paBoth])
          or (TProperty(CurrentClass.Properties.Items[i]).WriteAccess in [paField, paBoth]))
        then
          Result := Result + '    ' + FieldPrefix
            + TProperty(CurrentClass.Properties.Items[i]).Name
            + ': ' + TProperty(CurrentClass.Properties.Items[i]).DataType
            + ';' + CRLF;
      end;
      // Output the Get and Set property methods
      for i := 0 to CurrentClass.Properties.Count-1 do
      begin
        // private Getters
        if (TProperty(CurrentClass.Properties.Items[i]).Visibility >= pvProtected)
          and (TProperty(CurrentClass.Properties.Items[i]).ReadAccess in [paMethod, paBoth])
        then
          Result := Result + '    function ' + ReadPrefix
            + TProperty(CurrentClass.Properties.Items[i]).Name
            + ': ' + TProperty(CurrentClass.Properties.Items[i]).DataType
            + ';' + CRLF;
        // private Setters
        if (TProperty(CurrentClass.Properties.Items[i]).Visibility >= pvProtected)
          and (TProperty(CurrentClass.Properties.Items[i]).WriteAccess in [paMethod, paBoth])
        then
          Result := Result + '    procedure ' + WritePrefix
            + TProperty(CurrentClass.Properties.Items[i]).Name
            + '(' + WriteParam + ': '
            + TProperty(CurrentClass.Properties.Items[i]).DataType
            + ');' + CRLF;
      end;
    end
    else
    begin
      // Output the properties
      for i := 0 to CurrentClass.Properties.Count-1 do
      begin
        if TProperty(CurrentClass.Properties.Items[i]).Visibility = Section then
        begin
          Result := Result + '    property '
            + TProperty(CurrentClass.Properties.Items[i]).Name
            + ': '
            + TProperty(CurrentClass.Properties.Items[i]).DataType;
          if TProperty(CurrentClass.Properties.Items[i]).ReadAccess <> paNone then
          begin
            Result := Result + ' read ';
            if TProperty(CurrentClass.Properties.Items[i]).ReadAccess = paField then
              Result := Result + FieldPrefix + TProperty(CurrentClass.Properties.Items[i]).Name
            else
              Result := Result + ReadPrefix + TProperty(CurrentClass.Properties.Items[i]).Name;
          end;
          if TProperty(CurrentClass.Properties.Items[i]).WriteAccess <> paNone then
          begin
            Result := Result + ' write ';
            if TProperty(CurrentClass.Properties.Items[i]).WriteAccess = paField then
              Result := Result + FieldPrefix + TProperty(CurrentClass.Properties.Items[i]).Name
            else
              Result := Result + WritePrefix + TProperty(CurrentClass.Properties.Items[i]).Name;
          end;
          Result := Result + ';' + CRLF;
        end;
      end;
    end;
  end;

  function OutputClassMethods: string;
  var
    i: Integer;
  begin
    Assert(CurrentClass <> nil);

    Result := '';
    for i := 0 to CurrentClass.Properties.Count-1 do
    begin
      // Get Methods
      if TProperty(CurrentClass.Properties.Items[i]).ReadAccess in [paMethod, paBoth] then
      begin
        Result := Result + CRLF + 'function T' + CurrentClass.ReplaceText
          + '.' + ReadPrefix
          + TProperty(CurrentClass.Properties.Items[i]).Name + ': '
          + TProperty(CurrentClass.Properties.Items[i]).DataType
          + ';' + CRLF + 'begin' + CRLF;
        if TProperty(CurrentClass.Properties.Items[i]).ReadAccess = paBoth then
          Result := Result + '  Result := ' + FieldPrefix
            + TProperty(CurrentClass.Properties.Items[i]).Name
            + ';' + CRLF
        else
          Result := Result + CRLF;
        Result := Result + 'end;' + CRLF;
      end;
      // Set Methods
      if TProperty(CurrentClass.Properties.Items[i]).WriteAccess in [paMethod, paBoth] then
      begin
        Result := Result + CRLF + 'procedure T' + CurrentClass.ReplaceText
          + '.' + WritePrefix
          + TProperty(CurrentClass.Properties.Items[i]).Name
          + '(' + WriteParam + ': '
          + TProperty(CurrentClass.Properties.Items[i]).DataType
          + ');' + CRLF + 'begin' + CRLF;
        if TProperty(CurrentClass.Properties.Items[i]).WriteAccess = paBoth then
          Result := Result + '  ' + FieldPrefix
            + TProperty(CurrentClass.Properties.Items[i]).Name
            + ' := ' + WriteParam + ';' + CRLF;
        Result := Result + 'end;' + CRLF;
      end;
    end;
  end;

  function TransformPlaceholders: string;
  var
    Transform, DateMask: string;
    FirstSectn, LastSectn: string;
    iCtr, iPos: Integer;
  begin
    Transform := NxtLn;
    if Pos('%', Transform) > 0 then
    begin
      for iCtr := 0 to PlaceHolders.Count-1 do
        Transform := StringReplace(Transform
          , TPlaceholder(PlaceHolders.Items[iCtr]).SearchText
          , TPlaceholder(PlaceHolders.Items[iCtr]).ReplaceText
          , [rfReplaceAll, rfIgnoreCase]);
      iPos := Pos('%DATE', Uppercase(Transform));
      if iPos > 0 then
      begin
        FirstSectn := Copy(Transform, 1, iPos-1);
        LastSectn := Copy(Transform, iPos+5, 999);
        iPos := Pos('%', LastSectn);
        DateMask := Copy(LastSectn, 1, iPos-1);
        LastSectn := Copy(LastSectn, iPos+1, 999);
        if DateMask = '' then
          DateMask := 'ddddd';
        Transform := FirstSectn + FormatDateTime(DateMask, Date) + LastSectn;
      end;
    end;

    Result := Transform;
  end;

  procedure CheckforClassInterface;
  var
    i: Integer;
  begin
    Assert(CurrentClass = nil);

    if Pos('CLASS', NxtlnCaps) > 0 then
    begin
      for i := 0 to Placeholders.Count-1 do
        if (TPlaceholder(Placeholders.Items[i]).PlaceholderType = ptClass)
          and (Trim(Copy(NxtLnCaps, 1, Pos('(', NxtLn))) = 'T'
          + TPlaceholder(Placeholders.Items[i]).SearchText + ' = CLASS(') then
        begin
          CurrentClass := TPlaceholder(Placeholders.Items[i]);
          Break;
        end;
    end;
  end;

  function CheckToOutputClassMethods: string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Placeholders.Count-1 do
      if Trim(NxtLnCaps) = '{ T' + TPlaceholder(Placeholders.Items[i]).SearchText + ' }' then
      begin
        CurrentClass := TPlaceholder(Placeholders.Items[i]);
        Break;
      end;
    if CurrentClass <> nil then
      Result := OutputClassMethods;
  end;

begin
  // Set up our ini user values
  iniWiz := TIniFile.Create(cgWizardINIFile);
  FieldPrefix := iniWiz.ReadString('Property', 'FieldPrefix', 'F');
  ReadPrefix := iniWiz.ReadString('Property', 'ReadPrefix', 'Get');
  WritePrefix := iniWiz.ReadString('Property', 'WritePrefix', 'Set');
  WriteParam := iniWiz.ReadString('Property', 'WriteParameter', 'Value');
  iniWiz.Free;

  BoolStack := TtiBooleanStack.Create;
  try
    // Insert the header file
    AssignFile(F, cgPathToHeaderFile );
    Reset(F);
    Result := '';
    while not Eof(F) do
    begin
      Readln(F, NxtLn);
      NxtLnCaps := UpperCase(NxtLn);
      if Copy(NxtLnCaps, 1, 8) = '{$IFDEF ' then
        QualifyOutput
      else if Pos('{$ENDIF}', NxtLnCaps) > 0 then
        BoolStack.Pop
      else if BoolStack.Result then
        Result := Result + TransformPlaceholders + CRLF;
    end;
    Close(F);

    // Insert the template file
    AssignFile(F, TemplateFile);
    Reset(F);
    while not Eof(F) do
    begin
      Readln(F, NxtLn);
      NxtLnCaps := UpperCase(NxtLn);
      if Copy(NxtLnCaps, 1, 8) = '{$IFDEF ' then
        QualifyOutput
      else if Pos('{$ENDIF}', NxtLnCaps) > 0 then
        BoolStack.Pop
      else if BoolStack.Result then
      begin
        Result := Result + TransformPlaceholders + CRLF;
        if CurrentClass <> nil then
        begin
          if Trim(NxtLnCaps) = 'PRIVATE' then
            Result := Result + OutputClassSection(pvPrivate)
          else if Trim(NxtLnCaps) = 'PROTECTED' then
            Result := Result + OutputClassSection(pvProtected)
          else if Trim(NxtLnCaps) = 'PUBLISHED' then
            Result := Result + OutputClassSection(pvPublished)
          else if Trim(NxtLnCaps) = 'PUBLIC' then
            Result := Result + OutputClassSection(pvPublic);
          if Trim(NxtLnCaps) = 'END;' then
            CurrentClass := nil;
        end
        else
          CheckforClassInterface;
        if CurrentClass = nil then
          Result := Result + CheckToOutputClassMethods;
      end;
    end;
    Close(F);
  finally
    BoolStack.Free;
  end;
end;

end.

