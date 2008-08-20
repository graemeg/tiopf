{
  This is a GUI independent unit.
  It forms the basis for the GUI Mediators (MGM) implementation.
}

unit tiBaseMediator;

{$I tiDefines.inc}

interface

uses
  TypInfo,
  Classes,
  SysUtils,
  tiObject;

type
  // forward declaration
  TMediatorView = class;

  TObjectToGUIEvent = procedure(Sender: TMediatorView; Src: TtiObject; Dest: TComponent; var Handled: Boolean) of object;
  TGUIToObjectEvent = procedure(Sender: TMediatorView; Src: TComponent; Dest: TtiObject; var Handled: Boolean) of object;


  { Base class to inherit from to make more customised Mediator Views. }
  TMediatorView = class(TtiObject)
  private
    FListObject: TtiObjectList;
    FOnGUIToObject: TGUIToObjectEvent;
    FOnObjectToGUI: TObjectToGUIEvent;
    FSettingUp: Boolean;
    FFieldName: string;
    FSubject: TtiObject;
    FGuiFieldName: string;
    FErrorMessage: string;
    procedure SetListObject(const AValue: TtiObjectList);
    procedure TestIfValid;
  protected
    UseInternalOnChange: Boolean;
    // Must be implemented to return the GUI component.
    function GetGuiControl: TComponent; virtual; abstract;
    // Must be implemented to set the GUI component.
    procedure SetGuiControl(AValue: TComponent); virtual; abstract;
    // For use by descendents. Will call GUIChanged
    procedure DoOnChange(Sender: TObject); virtual;
    // Returns FSubject by default.
    function GetSubject: TtiObject; virtual;
    // Should be overridden. Does nothing by default.
    procedure SetupGUIandObject; virtual;
    // Do something with errors.
    procedure UpdateGuiValidStatus(pErrors: TtiObjectErrors); virtual;
    // Check whether data and GUI property are OK.
    function DataAndPropertyValid: Boolean;
    // By default, copies published FieldName to published GUIfieldName.
    procedure DoGuiToObject; virtual;
    // Copy object property to GUI By default, copies published GUIFieldName to published fieldName
    procedure DoObjectToGui; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    // Must return a minimum GUI class for which this mediator is valid.
    class function ComponentClass: TClass; virtual;
    // Copy GUI to Object. Calls OnGUIToObject if set, and then calls DoGUIToObject if needed
    procedure GuiToObject;
    // Copy GUI to Object. Calls OnObjectToGUI if set, and then calls DoGUIToObject if needed
    procedure ObjectToGui;
    // Called by NotifyObservers of subject. Calls ObjectToGUI by default.
    procedure Update(pSubject: TtiObject); override;
    // Call when GUI changed. Will call GUIToObject.
    procedure GUIChanged;
    // Access properties.
    property GUIControl: TComponent read GetGuiCOntrol write SetGUIControl;
    property Subject: TtiObject read GetSubject write FSubject;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property ValueList: TtiObjectList read FListObject write SetListObject;
  published
    // Property of subject to handle.
    property FieldName: string read FFieldName write FFieldName;
    // Property of GUI to handle.
    property GuiFieldName: string read FGuiFieldName write FGuiFieldName;
    // OnGUIToObject
    property OnGUIToObject: TGUIToObjectEvent read FOnGUIToObject write FOnGUIToObject;
    // OnObjectToGUI
    property OnObjectToGUI: TObjectToGUIEvent read FOnObjectToGUI write FOnObjectToGUI;
  end;


  TMediatorViewClass = class of TMediatorView;

  TSubjectClass = class of TtiObject;


  TMediatorDef = class(TCollectionItem)
  private
    FMC: TMediatorViewClass;
    FMSC: TSubjectClass;
    FPN: string;
    FPT: TTypeKinds;
  public
    // Return True if this definition handles the Subject,Gui,APropinfo trio
    function Handles(ASubject: TtiObject; AGui: TComponent; APropInfo: PPropInfo): Boolean;
    // Return True if this definition matches 'closer' than M.
    // Note that both current and M must have Handles() returned true for this to be useful.
    function BetterMatch(M: TMediatorDef): Boolean;
    // Definition
    property MediatorClass: TMediatorViewClass read FMC write FMC;
    property MinSubjectClass: TSubjectClass read FMSC write FMSC;
    property PropertyTypes: TTypeKinds read FPT write FPT;
    property PropertyName: string read FPN write FPN;
  end;


  TMediatorDefs = class(TCollection)
  private
    function GetDef(Index: integer): TMediatorDef;
    procedure SetDef(Index: integer; const AValue: TMediatorDef);
  public
    function AddDef: TMediatorDef;
    property Defs[Index: integer]: TMediatorDef read GetDef write SetDef; default;
  end;


  TMediatorManager = class(TObject)
  private
    FDefs: TMediatorDefs;
  public
    constructor Create;
    destructor Destroy; override;
    function FindDefFor(ASubject: TtiObject; AGui: TComponent; APropName: string): TMediatorDef; overload;
    function FindDefFor(ASubject: TtiObject; AGui: TComponent; APropInfo: PPropInfo): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyName: string): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyTypes: TTypeKinds): TMediatorDef; overload;
    property Defs: TMediatorDefs read FDefs;
  end;

  EMediator = class(Exception);

function gMediatorManager: TMediatorManager;


implementation

var
  uMediatorManager: TMediatorManager;


function gMediatorManager: TMediatorManager;
begin
  if (uMediatorManager = nil) then
    uMediatorManager := TMediatorManager.Create;
  Result := uMediatorManager;
end;


{ TMediatorView }

constructor TMediatorView.Create;
begin
  inherited;
  FSettingUp          := True;
  UseInternalOnChange := True;
end;

destructor TMediatorView.Destroy;
begin
  if Assigned(FSubject) then
    FSubject.DetachObserver(self);
  inherited Destroy;
end;

class function TMediatorView.ComponentClass: TClass;
begin

end;

procedure TMediatorView.GUIChanged;
begin
  if not FSettingUp then
  begin
    GuiToObject;
    TestIfValid;
  end;
end;

procedure TMediatorView.UpdateGuiValidStatus(pErrors: TtiObjectErrors);
begin
  // do nothing
end;

function TMediatorView.DataAndPropertyValid: Boolean;
begin
  Result := (FSubject <> nil) and (FFieldName <> '');
  if not Result then
    Exit; //==>

  Result := (IsPublishedProp(FSubject, FFieldName));

  if not Result then
    raise EMediator.CreateFmt('<%s> is not a property of <%s>',
      [FFieldName, FSubject.ClassName]);

  //  EditControl.ReadOnly := ReadOnly or IsPropReadOnly;
end;

procedure TMediatorView.DoOnChange(Sender: TObject);
begin
  GUIChanged;
end;

procedure TMediatorView.TestIfValid;
var
  Errors: TtiObjectErrors;
begin
  Errors := TtiObjectErrors.Create;
  try
    Subject.IsValid(Errors);
    UpdateGuiValidStatus(Errors); // always execute this as it also resets EditControl
  finally
    Errors.Free;
  end;
end;

procedure TMediatorView.SetListObject(const AValue: TtiObjectList);
begin
  if FListObject = AValue then
    Exit;
  FListObject := AValue;
end;

procedure TMediatorView.Update(pSubject: TtiObject);
begin
  inherited;
  ObjectToGui;
  TestIfValid;
end;

function TMediatorView.GetSubject: TtiObject;
begin
  Result := FSubject;
end;

procedure TMediatorView.DoGuiToObject;
begin
  Subject.PropValue[FieldName] := TypInfo.GetPropValue(GetGUIControl, GuiFieldName);
end;

procedure TMediatorView.GuiToObject;
var
  B: Boolean;
begin
  B := False;
  if Assigned(FOnGUIToObject) then
    FOnGUIToObject(Self, GuiControl, Subject, B);
  if not B then
    DoGuiToObject;
end;

procedure TMediatorView.ObjectToGui;
var
  B: Boolean;
begin
  B := False;
  if Assigned(FOnObjectToGUI) then
    FOnObjectToGUI(Self, Subject, GuiControl, B);
  if not B then
    DoObjectToGUI;
end;

procedure TMediatorView.DoObjectToGui;
begin
  TypInfo.SetPropValue(GetGUIControl, GuiFieldName, Subject.PropValue[FieldName]);
end;

procedure TMediatorView.SetupGUIandObject;
begin
  { do nothing here }
end;


{ TMediatorManager }

constructor TMediatorManager.Create;
begin
  FDefs := TMediatorDefs.Create(TMediatorDef);
end;

destructor TMediatorManager.Destroy;
begin
  FreeAndNil(FDefs);
  inherited Destroy;
end;

function TMediatorManager.FindDefFor(ASubject: TtiObject; AGui: TComponent; APropName: string): TMediatorDef;
begin
  Result := FindDefFor(ASubject, AGUI, FindPropInfo(ASubject, APropName));
end;

function TMediatorManager.FindDefFor(ASubject: TtiObject; AGui: TComponent; APropInfo: PPropInfo): TMediatorDef;
var
  N: string;
  D: TMediatorDef;
  I: integer;
begin
  if (APropInfo = nil) then
    raise EMediator.Create('Need property information to find mediator');
  N      := APropinfo^.Name;
  Result := nil;
  for I := 0 to FDefs.Count - 1 do
  begin
    D := FDefs[I];
    if D.Handles(ASubject, AGUI, APropInfo) then
      if (D.BetterMatch(Result)) then
        Result := D;
  end;
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass): TMediatorDef;
begin
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := '';
  Result.FPT  := tkProperties - [tkClass, tkObject, tkInterface, tkDynArray, tkInterfaceRaw];
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyName: string): TMediatorDef;
begin
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := PropertyName;
  Result.FPT  := [];
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyTypes: TTypeKinds): TMediatorDef;
begin
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := '';
  Result.FPT  := PropertyTypes;
end;


{ TMediatorDefs }

function TMediatorDefs.GetDef(Index: integer): TMediatorDef;
begin
  Result := TMediatorDef(Items[Index]);
end;

procedure TMediatorDefs.SetDef(Index: integer; const AValue: TMediatorDef);
begin
  Items[Index] := AValue;
end;

function TMediatorDefs.AddDef: TMediatorDef;
begin
  Result := Add as TMediatorDef;
end;


{ TMediatorDef }

function TMediatorDef.Handles(ASubject: TtiObject; AGui: TComponent; APropInfo: PPropInfo): Boolean;
var
  N: string;
begin
  N      := APropInfo^.Name;
  // At least the classes must match
  Result := AGui.InheritsFrom(FMC.ComponentClass) and ASubject.InheritsFrom(FMSC);
  if Result then
    if (PropertyName <> '') then
      Result := (CompareText(N, PropertyName) = 0)
    else // Property kind should match. Note that property MUST be set to something.
      Result := (APropInfo^.PropType^.Kind in PropertyTypes)// If PropertyName is set, it must match
  ;
end;

function TMediatorDef.BetterMatch(M: TMediatorDef): Boolean;
var
  CGC, CSC: Boolean;
begin
  Result := (M = nil);
  if not Result then
  begin
    // Closer property match ?
    Result := (PropertyName <> '') and (M.PropertyName = '');
    if not Result then
    begin
      // M's property matches closer
      Result := not ((M.PropertyName <> '') and (PropertyName = ''));
      if Result then
      begin
        // Properties are on equal level. Check GUI class.
        // Closer GUI class ?
        Result := FMC.ComponentClass.InheritsFrom(M.MediatorClass.ComponentClass);
        if not Result then
        begin
          // M's GUI class matches closer ?
          Result := not (M.MediatorClass.ComponentClass.InheritsFrom(FMC.ComponentClass));
          if Result then
          begin
            // GUI classes are on equal level (different branches in tree). Check subject class.
            // Closer Subject class ?
            Result := FMSC.InheritsFrom(M.FMSC);
            if not Result then
              // M's subject class matches closer ?
              Result := not M.FMSC.InheritsFrom(FMSC);
          end;
        end;
      end;
    end;
  end;
end;

initialization
  // nothing

finalization
  FreeAndNil(uMediatorManager);

end.

