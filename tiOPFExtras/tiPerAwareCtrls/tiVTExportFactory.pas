Unit tiVTExportFactory;

Interface

Uses Classes, tiVTExportAbs;

Type
  TExportClass = Class Of TtiVTExportAbs;

  TExportMapping = Class
    FExportName : String;
    FExportClass : TExportClass;
  Public
    Constructor CreateExt(Const pExportName : String; pClassRef : TExportClass);
    Property ExportName : String Read FExportName Write FExportName;
    Property ExportClass : TExportClass Read FExportClass Write FExportClass;
  End;

  TFactoryExport = Class
  Private
    FExportMappings : TStringList;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure RegisterExport(Const pExportName : String; pClassRef : TExportClass);
    Function GetExport(Const pExportName : String) : TtiVTExportAbs;
    Property RegisteredExports : TStringList Read FExportMappings;
  End;

Function FactoryExport : TFactoryExport;

Implementation

Uses SysUtils, Dialogs;

Var
  uFactoryExport : TFactoryExport;

Function FactoryExport : TFactoryExport;
Begin
  If (uFactoryExport = Nil) Then
    uFactoryExport := TFactoryExport.Create;
  Result := uFactoryExport;
End;

{ TExportMapping }

Constructor TExportMapping.CreateExt(Const pExportName : String; pClassRef : TExportClass);
Begin
  Create;
  ExportName := pExportName;
  ExportClass := pClassRef;
End;

{ TFactoryExport }

Constructor TFactoryExport.Create;
Begin
  Inherited;
  FExportMappings := TStringList.Create;
End;

Destructor TFactoryExport.Destroy;
Var
  I : Integer;
Begin
  For I := 0 To FExportMappings.Count - 1 Do
    TObject(FExportMappings.Objects[I]).Free;
  FExportMappings.Free;
  Inherited;
End;

Function TFactoryExport.GetExport(Const pExportName : String) : TtiVTExportAbs;
Var
  I : Integer;
Begin
  I := FExportMappings.IndexOf(UpperCase(pExportName));
  If (I = -1) Then
    ShowMessage('Request for invalid receipt class "' + pExportName + '"');
  Result := TExportMapping(FExportMappings.Objects[I]).ExportClass.Create;
End;

Procedure TFactoryExport.RegisterExport(Const pExportName : String;
  pClassRef : TExportClass);
Var
  I : Integer;
  lExportMapping : TExportMapping;
  lExportName : String;
Begin
  lExportName := UpperCase(pExportName);
  I := FExportMappings.IndexOf(lExportName);
  If (I <> -1) Then
  Begin
    ShowMessage('Attempt to register duplicate export "' + pExportName + '"');
    Exit;
  End;
  lExportMapping := TExportMapping.CreateExt(lExportName, pClassRef);
  FExportMappings.AddObject(lExportName, lExportMapping);
End;

Initialization
  FactoryExport;
Finalization
  uFactoryExport.Free;
End.
