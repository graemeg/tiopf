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
  contnrs,
  tiObject;

type
  // forward declaration
  TMediatorView = class;

  TObjectToGUIEvent = procedure(Sender: TMediatorView; Src: TtiObject; Dest: TComponent; var Handled: Boolean) of object;
  TGUIToObjectEvent = procedure(Sender: TMediatorView; Src: TComponent; Dest: TtiObject; var Handled: Boolean) of object;

  TObjectUpdateMoment = (ouOnChange, ouOnExit, ouCustom);

  { Base class to inherit from to make more customised Mediator Views. }
  TMediatorView = class(TtiObject)
  private
    FActive: Boolean;
    FObjectUpdateMoment: TObjectUpdateMoment;
    FListObject: TtiObjectList;
    FOnGUIToObject: TGUIToObjectEvent;
    FOnObjectToGUI: TObjectToGUIEvent;
    FSettingUp: Boolean;
    FFieldName: string;
    FSubject: TtiObject;
    FGuiFieldName: string;
    procedure CheckFieldNames;
    procedure TestIfValid;
  protected
    UseInternalOnChange: Boolean;
    // If GUI and Object and fieldnames are assigned, calls SetupGUIandObject.
    procedure CheckSetupGUIandObject;
    // Must be implemented to return the GUI component.
    function GetGuiControl: TComponent; virtual; abstract;
    // Must be implemented to set the GUI component.
    procedure SetGuiControl(const AValue: TComponent); virtual;
    // Set up GUI and Object. Does nothing by default
    procedure SetupGUIandObject; virtual;
    // For use by descendents. Will call GUIChanged
    procedure DoOnChange(Sender: TObject); virtual;
    // Returns FSubject by default.
    function GetSubject: TtiObject; virtual;
    // Do something with errors.
    procedure UpdateGuiValidStatus(pErrors: TtiObjectErrors); virtual;
    // Check whether data and GUI property are OK. Not used in this class
    function DataAndPropertyValid: Boolean;
    // By default, copies published FieldName to published GUIFieldName.
    procedure DoGuiToObject; virtual;
    // Copy object property to GUI. By default it copies published GUIFieldName to published FieldName
    procedure DoObjectToGui; virtual;
    // Set value list object. Override to provide additional handling.
    procedure SetListObject(const AValue: TtiObjectList); virtual;
    //  Set up subject, attach as observer. Override to provide additional handling.
    procedure SetSubject(const AValue: TtiObject); virtual;
    // Set active - attaches/Detaches observer. Override to provide additional handling.
    procedure SetActive(const AValue: Boolean); virtual;
    // Set fieldname. Override to provide additional handling;
    procedure SetFieldName(const AValue: string); virtual;
    // Set ObjectUpdateMoment. Override to provide additional handling;
    procedure SetObjectUpdateMoment(const AValue: TObjectUpdateMoment); virtual;
  public
    constructor Create; override;
    constructor CreateCustom(AEditControl: TComponent; ASubject: TtiObject; AFieldName: string; AGuiFieldName: string);
    destructor Destroy; override;
    // Must return a minimum GUI class for which this mediator is valid.
    class function ComponentClass: TClass; virtual;
    // Must return TRUE if the class is a composite mediator.
    // In that case, the fieldnames property will not be checked.
    // By default, it returns False.
    class function CompositeMediator: Boolean; virtual;
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
    // The subject of observation...
    property Subject: TtiObject read GetSubject write SetSubject;
    // Descendents that need a list of values can use this.
    property ValueList: TtiObjectList read FListObject write SetListObject;
  published
    // Property of subject to handle.
    property FieldName: string read FFieldName write SetFieldName;
    // Property of GUI to handle.
    property GuiFieldName: string read FGuiFieldName write FGuiFieldName;
    // Property ObjectUpdateMoment : Do action e.g. in OnExit instead of OnChange.
    // Up to the descendent class to decide this.
    property ObjectUpdateMoment: TObjectUpdateMoment read FObjectUpdateMoment write SetObjectUpdateMoment default ouOnChange;
    // OnGUIToObject
    property OnGUIToObject: TGUIToObjectEvent read FOnGUIToObject write FOnGUIToObject;
    // OnObjectToGUI
    property OnObjectToGUI: TObjectToGUIEvent read FOnObjectToGUI write FOnObjectToGUI;
    // Observing or not ?
    property Active: Boolean read FActive write SetActive;
  end;


  TMediatorViewClass = class of TMediatorView;

  TSubjectClass = class of TtiObject;


  TtiMediatorFieldInfo = class(TCollectionItem)
  private
    FWidth: integer;
    FCaption: string;
    FPropName: string;
    FAlign: TAlignment;
    FOrigStyle: Boolean;
  protected
    function GetAsString: string; virtual;
    procedure SetAsString(const AValue: string); virtual;
  public
    procedure Assign(Source: TPersistent); override;
    // Setting this will parse everything.
    property AsString: string read GetAsString write SetAsString;
  published
    property Caption: string read FCaption write FCaption;
    property PropName: string read FPropName write FPropName;
    property FieldWidth: integer read FWidth write FWidth;
    property Alignment: TAlignment read FAlign write FAlign default taLeftJustify;
  end;


  TtiMediatorFieldInfoList = class(TCollection)
  private
    function GetAsString: string;
    function GetI(Index: integer): TtiMediatorFieldInfo;
    procedure SetI(Index: integer; const AValue: TtiMediatorFieldInfo);
  public
    function AddFieldInfo: TtiMediatorFieldInfo;
    property FieldInfo[Index: integer]: TtiMediatorFieldInfo read GetI write SetI; default;
    property AsString: string read GetAsString;
  end;

  { Event object used for OnBeforeSetupField event. Is used to allow formatting
    of fields before written to listview Caption or Items. }
  TOnBeforeSetupField = procedure(AObject: TtiObject; const AFieldName: string; var AValue: string) of object;


  TListItemMediator = class(TtiObject)
  private
    Factive: Boolean;
    FOnBeforeSetupField: TOnBeforeSetupField;
    function GetDisplayNames: string;
    procedure SetActive(const AValue: Boolean);
  protected
    FModel: TtiObject;
    FFieldsInfo: TtiMediatorFieldInfoList;
  public
    destructor Destroy; override;
    property OnBeforeSetupField: TOnBeforeSetupField read FOnBeforeSetupField write FOnBeforeSetupField;
    property DisplayNames: string read GetDisplayNames;
    property FieldsInfo: TtiMediatorFieldInfoList read FFieldsInfo;
  published
    property Model: TtiObject read FModel;
    property Active: Boolean read Factive write SetActive;
  end;


  { Custom mediator that handles lists of objects.
    Should move to tiBaseMediator }
  TCustomListMediator = class(TMediatorView)
  private
    FOnBeforeSetupField: TOnBeforeSetupField;
    FShowDeleted: Boolean;
    FMediatorList: TObjectList;
//    FSelectedObject: TtiObject;
    FFieldsInfo: TtiMediatorFieldInfoList;
    function GetDisplayNames: string;
    function GetIsObserving: Boolean;
    function GetModel: TtiObjectList;
    procedure SetDisplayNames(const AValue: string);
    procedure SetFieldsInfo(const AValue: TtiMediatorFieldInfoList);
    procedure SetIsObserving(const AValue: Boolean);
    procedure SetShowDeleted(const AValue: Boolean);
    procedure SetOnBeforeSetupField(const Value: TOnBeforeSetupField);
  protected
    function GetSelectedObject: TtiObject; virtual;
    procedure SetSelectedObject(const AValue: TtiObject); virtual;
    procedure CreateColumns; virtual; abstract;
    procedure ClearList; virtual; abstract;
    procedure DoCreateItemMediator(AData: TtiObject; ARowIdx: integer); virtual; abstract;
    procedure ParseDisplayNames(const AValue: string);
    procedure CreateSubMediators; virtual;
    procedure RebuildList; virtual; abstract;
    function DataAndPropertyValid(const AData: TtiObject): Boolean;
    procedure DoGUIToObject; override;
    procedure DoObjectToGUI; override;
    procedure SetSubject(const AValue: TtiObject); override;
    procedure SetFieldName(const AValue: string); override;
    procedure SetActive(const AValue: Boolean); override;
    property MediatorList: TObjectList read FMediatorList;
  public
    constructor Create; override;
    class function CompositeMediator: Boolean; override;
    property SelectedObject: TtiObject read GetSelectedObject write SetSelectedObject;
  published
    property OnBeforeSetupField: TOnBeforeSetupField read FOnBeforeSetupField write SetOnBeforeSetupField;
    property Model: TtiObjectList read GetModel;
    property DisplayNames: string read GetDisplayNames write SetDisplayNames;
    property ShowDeleted: Boolean read FShowDeleted write SetShowDeleted;
    // For backwards compatibility.
    property IsObserving: Boolean read GetIsObserving write SetIsObserving;
    property FieldsInfo: TtiMediatorFieldInfoList read FFieldsInfo write SetFieldsInfo;
  end;


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
    // If APropName is empty or APropInfo is Nil, a composite mediator will be searched.
    function FindDefFor(ASubject: TtiObject; AGui: TComponent): TMediatorDef; overload;
    function FindDefFor(ASubject: TtiObject; AGui: TComponent; APropName: string): TMediatorDef; overload;
    function FindDefFor(ASubject: TtiObject; AGui: TComponent; APropInfo: PPropInfo): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyName: string): TMediatorDef; overload;
    function RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyTypes: TTypeKinds): TMediatorDef; overload;
    property Defs: TMediatorDefs read FDefs;
  end;

  EMediator = class(Exception);

function gMediatorManager: TMediatorManager;
function tiFieldName(const AField: string): string;
function tiFieldWidth(const AField: string): integer;
function tiFieldCaption(const AField: string): string;
function tiFieldAlignment(const AField: string): TAlignment;


implementation

uses
  tiUtils, tiLog;

var
  uMediatorManager: TMediatorManager;

resourcestring
  sErrInvalidFieldName      = 'No fieldname specified for column %d';
  sErrInvalidAlignmentChar  = 'Invalid alignment character "%s" specified for column %d';
  sErrInvalidWidthSpecifier = 'Invalid with "%s" specified for column %d';
  sErrNotListObject         = '%s is not a TTiListObject';
  sErrCompositeNeedsList    = '%s needs a TtiObjectList class but is registered with %s';

const
  DefFieldWidth = 75;   // default width
  cFieldDelimiter = ';';
  cBrackets = '()';
  

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
  UseInternalOnChange := True;
  FActive := True;
  FObjectUpdateMoment := ouOnChange;
end;

constructor TMediatorView.CreateCustom(AEditControl: TComponent; ASubject: TtiObject; AFieldName: string; AGuiFieldName: string);
begin
  FSettingUp   := True;
  Create;
  Subject      := ASubject;
  FieldName    := AFieldName;
  GuiFieldName := AGuiFieldName;
  GUIControl   := AEditControl; // At this point, SetupGuiAndObject is called
  FSettingUp   := False;
end;

destructor TMediatorView.Destroy;
begin
  Subject := nil; // Will call DetachObserver
  inherited Destroy;
end;

class function TMediatorView.ComponentClass: TClass;
begin
  Result := TComponent;
end;

class function TMediatorView.CompositeMediator: Boolean;
begin
  Result := False;
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
  Result := (FSubject <> nil) and ((not CompositeMediator) or (FFieldName <> ''));
  if not Result then
    Exit; //==>

  if not CompositeMediator then
  begin
    Result := (IsPublishedProp(FSubject, FFieldName));
    if not Result then
      raise EMediator.CreateFmt('<%s> is not a property of <%s>',
        [FFieldName, FSubject.ClassName]);
  end;

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

procedure TMediatorView.CheckSetupGUIandObject;
begin
  if Assigned(Subject) and Assigned(GUIControl) then
    SetupGUIandObject;
end;

procedure TMediatorView.SetGuiControl(const AValue: TComponent);
begin
  CheckSetupGUIAndObject;
end;

procedure TMediatorView.SetupGUIandObject;
begin

end;

procedure TMediatorView.SetListObject(const AValue: TtiObjectList);
begin
  if FListObject = AValue then
    Exit;
  FListObject := AValue;
end;

procedure TMediatorView.SetObjectUpdateMoment(const AValue: TObjectUpdateMoment);
begin
  if FObjectUpdateMoment = AValue then
    Exit;
  FObjectUpdateMoment := AValue;
end;

procedure TMediatorView.SetFieldName(const AValue: string);
begin
  FFieldName := AValue;
end;

procedure TMediatorView.SetSubject(const AValue: TtiObject);
begin
  if (FSubject = AValue) then
    Exit;
  if Assigned(FSubject) and FActive then
    FSubject.DetachObserver(Self);
  FSubject := AValue;
  if Assigned(FSubject) and FActive then
    FSubject.AttachObserver(self);
  CheckSetupGUIAndObject;
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

procedure TMediatorView.CheckFieldNames;
begin
  if (GuiFieldName = '') then
    raise EMediator.CreateFmt('%s : no gui fieldname set', [ClassName]);
  if (FieldName = '') then
    raise EMediator.CreateFmt('%s : no subject fieldname set', [ClassName]);
end;

procedure TMediatorView.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if Assigned(FSubject) then
    if Active then
    begin
      FSubject.AttachObserver(Self);
      FSubject.NotifyObservers;
    end
    else
      FSubject.DetachObserver(Self);
end;

procedure TMediatorView.DoGuiToObject;
begin
  Log(Format('> TMediatorView.DoGuiToObject for %s.%s', [Subject.ClassName, FieldName]), lsDebug);
  CheckFieldNames;
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
  Log(Format('> TMediatorView.DoObjectToGui for %s.%s', [Subject.ClassName, FieldName]), lsDebug);
  CheckFieldNames;
  TypInfo.SetPropValue(GetGUIControl, GuiFieldName, Subject.PropValue[FieldName]);
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

function TMediatorManager.FindDefFor(ASubject: TtiObject; AGui: TComponent): TMediatorDef;
begin
  Result := FindDefFor(ASubject, AGUI, PPropInfo(nil));
end;

function TMediatorManager.FindDefFor(ASubject: TtiObject; AGui: TComponent; APropName: string): TMediatorDef;
var
  propinfo: PPropInfo;
begin
  propinfo := GetPropInfo(ASubject, APropName);
  Result := FindDefFor(ASubject, AGUI, propinfo);
end;

function TMediatorManager.FindDefFor(ASubject: TtiObject; AGui: TComponent; APropInfo: PPropInfo): TMediatorDef;
var
  D: TMediatorDef;
  I: integer;
begin
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
var
  s: string;
begin
  if MediatorClass.CompositeMediator then
    s := 'composite '
  else
    s := '';
  Log(Format('Registering %smediator %s with subject %s', [s, MediatorClass.ClassName, MinSubjectClass.ClassName]), lsDebug);

  if not (MinSubjectClass.inheritsfrom(TtiObjectList)) and MediatorClass.CompositeMediator then
    raise EMediator.CreateFmt(sErrCompositeNeedsList, [MediatorClass.ClassName, MinSubjectClass.ClassName]);
  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := '';
  Result.FPT  := tkProperties - [tkClass, tkInterface, tkDynArray {$IFDEF FPC}, tkObject, tkInterfaceRaw{$ENDIF}];
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyName: string): TMediatorDef;
var
  s: string;
begin
  if MediatorClass.CompositeMediator then
    s := 'composite '
  else
    s := '';
  Log(Format('Registering %smediator %s with subject %s', [s, MediatorClass.ClassName, MinSubjectClass.ClassName]), lsDebug);

  Result      := FDefs.AddDef;
  Result.MediatorClass := MediatorClass;
  Result.FMSC := MinSubjectClass;
  Result.FPN  := PropertyName;
  Result.FPT  := [];
end;

function TMediatorManager.RegisterMediator(MediatorClass: TMediatorViewClass; MinSubjectClass: TSubjectClass; PropertyTypes: TTypeKinds): TMediatorDef;
var
  s: string;
begin
  if MediatorClass.CompositeMediator then
    s := 'composite '
  else
    s := '';
  Log(Format('Registering %smediator %s with subject %s', [s, MediatorClass.ClassName, MinSubjectClass.ClassName]), lsDebug);

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
  if (APropInfo = nil) then
    Result := FMC.CompositeMediator
  else
  begin
    N      := APropInfo^.Name;
    Result := True;
  end;
  if not Result then
    Exit; // ==>
  // At least the classes must match
  Result := AGui.InheritsFrom(FMC.ComponentClass) and ASubject.InheritsFrom(FMSC);
  if Result and not FMC.CompositeMediator then
    if (PropertyName <> '') then
      Result := (CompareText(N, PropertyName) = 0)
    else // Property kind should match. Note that property MUST be set to something.
      Result := (APropInfo^.PropType^.Kind in PropertyTypes)// If PropertyName is set, it must match
  ;
end;

function TMediatorDef.BetterMatch(M: TMediatorDef): Boolean;
begin
  Result := (M = nil);
  if not Result then
  begin
    Result := (FMC.CompositeMediator = M.MediatorClass.CompositeMediator);
    if Result then
    begin
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
end;


{ Helper functions }

{ Extract the field name part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")   will return: Quantity
  Width and Field Caption is optional }
function tiFieldName(const AField: string): string;
begin
  Result := tiToken(AField, cBrackets[1], 1);
end;

{ Extract the width part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")  will return: 25
  Width and Field Caption is optional }
function tiFieldWidth(const AField: string): integer;
var
  s: string;
begin
  s := tiSubStr(AField, cBrackets[1], cBrackets[2], 1);
  if trim(s) = '' then
    Result := DefFieldWidth
  else
    Result := StrToInt(tiToken(s, ',', 1));
end;

{ Extracts the alignment from the AField string which is in the format
 fieldname(width,"field caption",a)  where a is the alignment character.
 Legal values for the alignment character are :-
 < - left aligned
 > - right aligned
 | - centre aligned
  eg:  Quantity(25,"Qty",>)   will return: Qty with a width of 25 and
 the column will be right-aligned. Width, Field Caption and alignment
 are optional }
function tiFieldAlignment(const AField: string): TAlignment;
var
  s, a: string;
  lAlignChar: char;
begin
  Result := taLeftJustify;
  s      := tiSubStr(AField, cBrackets[1], cBrackets[2], 1);
  if trim(s) <> '' then
  begin
    a := tiToken(s, ',', 3);
    if a <> '' then
    begin
      lAlignChar := a[1];
      case lAlignChar of
        '<': Result := taLeftJustify;
        '>': Result := taRightJustify;
        '|': Result := taCenter;
      end; { case }
    end;
  end;
end;

{ Extract the field caption part from the AField string which is in the format
  fieldname(width,"field caption")   eg:  Quantity(25,"Qty")   will return: Qty
  Width and Field Caption is optional }
function tiFieldCaption(const AField: string): string;
var
  s: string;
  p: PChar;
begin
  s := tiSubStr(AField, cBrackets[1], cBrackets[2]);
  if (trim(s) = '') or (Pos(',', s) = 0) then
    // It's only got a width or is blank, so we default to field name
    Result := tiFieldName(AField)
  else
  begin
    s      := tiToken(s, ',', 2);
    p      := PChar(s);
    Result := AnsiExtractQuotedStr(p, '"');
  end;
end;


{ ---------------------------------------------------------------------
  General list
  --------------------------------------------------------------------- }

const
  AlignChars: array[TAlignMent] of char     = ('l', 'r', 'c');
  OrigAlignChars: array[TAlignMent] of char = ('<', '>', '|');

{ TtiMediatorFieldInfo }

function TtiMediatorFieldInfo.GetAsString: string;
begin
  if FOrigStyle then
    Result := Format('%s (%d, "%s", %s)', [PropName, origAlignChars[Alignment], FieldWidth, Caption])
  else
    Result := Format('%s|%s|%d|%s', [PropName, AlignChars[Alignment], FieldWidth, Caption]);
end;

procedure TtiMediatorFieldInfo.SetAsString(const AValue: string);
var
  S: string;
  A: TAlignment;
  I: integer;
  P1, P2: integer;
begin
  P1         := Pos('(', AVAlue);
  P2         := Pos('|', AVAlue);
  // Have ( and Not (have | and | before ()
  FOrigStyle := (P1 <> 0) and ((P2 = 0) or (P2 > P1));
  if FOrigStyle then
  begin
    PropName   := tiFieldName(AValue);
    Caption    := tiFieldCaption(AValue);
    FieldWidth := tiFieldWidth(AValue);
    Alignment  := tiFieldAlignment(AValue);
  end
  else
  begin
    PropName := tiToken(AValue, '|', 1);
    if (PropName = '') then
      raise EMediator.CreateFmt(SErrInvalidFieldName, [Index + 1]);
    Caption    := PropName;
    Alignment  := taLeftJustify;
    FieldWidth := DefFieldWidth;
    S          := tiToken(AValue, '|', 2);
    if (S <> '') then
    begin
      if (length(S) <> 1) then
        raise EMediator.CreateFmt(SErrInvalidAlignmentChar, [S, Index + 1]);
      for A := Low(Talignment) to High(TAlignment) do
        if (Upcase(AlignChars[A]) = Upcase(S[1])) then
          Alignment := A;
      S := tiToken(AValue, '|', 3);
      if (S <> '') then
      begin
        if not TryStrToInt(S, i) then
          raise EMediator.CreateFmt(SErrInvalidWidthSpecifier, [S]);
        FieldWidth := I;
        S          := tiToken(AValue, '|', 4);
        if (S <> '') then
          Caption := S;
      end;
    end;
  end;
end;

procedure TtiMediatorFieldInfo.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;


{ TtiMediatorFieldInfoList }

function TtiMediatorFieldInfoList.GetAsString: string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + ';';
    Result := Result + FieldInfo[i].AsString;
  end;
end;

function TtiMediatorFieldInfoList.GetI(Index: integer): TtiMediatorFieldInfo;
begin
  Result := TtiMediatorFieldInfo(Items[Index]);
end;

procedure TtiMediatorFieldInfoList.SetI(Index: integer; const AValue: TtiMediatorFieldInfo);
begin
  Items[Index] := AValue;
end;

function TtiMediatorFieldInfoList.AddFieldInfo: TtiMediatorFieldInfo;
begin
  Result := Add as TtiMediatorFieldInfo;
end;


{ TListItemMediator }

function TListItemMediator.GetDisplayNames: string;
begin
  Result := FFieldsInfo.AsString;
end;

procedure TListItemMediator.SetActive(const AValue: Boolean);
begin
  if Factive = AValue then
    Exit;
  Factive := AValue;
  if Assigned(FModel) then
    if Active then
      FModel.AttachObserver(Self)
    else
      FModel.DetachObserver(Self);
end;

destructor TListItemMediator.Destroy;
begin
  Active := False;
  FModel := nil;
  inherited Destroy;
end;


{ TCustomListMediator }

procedure TCustomListMediator.SetOnBeforeSetupField(const Value: TOnBeforeSetupField);
var
  I: integer;
begin
  FOnBeforeSetupField := Value;
  for I := 0 to FMediatorList.Count - 1 do
    TListItemMediator(FMediatorList[i]).OnBeforeSetupField := Value;
end;

procedure TCustomListMediator.SetSubject(const AValue: TtiObject);
begin
  if (AValue <> nil) then
    if not (AValue is TtiObjectList) then
      raise Emediator.CreateFmt(SErrNotListObject, [AValue.ClassName]);
  inherited SetSubject(AValue);
end;

procedure TCustomListMediator.SetFieldName(const AValue: string);
begin
  inherited SetFieldName(AValue);
  ParseDisplayNames(AValue);
end;

procedure TCustomListMediator.SetActive(const AValue: Boolean);
var
  I: integer;
begin
  inherited SetActive(AValue);
  for I := 0 to FMediatorList.Count - 1 do
    TListItemMediator(FMediatorList[i]).Active := AValue;
end;

function TCustomListMediator.GetModel: TtiObjectList;
begin
  Result := Subject as TtiObjectList;
end;

function TCustomListMediator.GetSelectedObject: TtiObject;
begin
  Result := nil;
end;

function TCustomListMediator.GetDisplayNames: string;
begin
  Result := FFieldsInfo.AsString;
end;

function TCustomListMediator.GetIsObserving: Boolean;
begin
  Result := Active;
end;

procedure TCustomListMediator.SetDisplayNames(const AValue: string);
begin
  FieldName := AValue;
end;

procedure TCustomListMediator.SetFieldsInfo(const AValue: TtiMediatorFieldInfoList);
begin
  FFieldsInfo.Assign(AValue);
end;

procedure TCustomListMediator.SetIsObserving(const AValue: Boolean);
begin
  Active := AValue;
end;

procedure TCustomListMediator.SetSelectedObject(const AValue: TtiObject);
begin
  // Do nothing
end;

procedure TCustomListMediator.SetShowDeleted(const AValue: Boolean);
begin
  if FShowDeleted = AValue then
    Exit; //==>
  BeginUpdate;
  try
    FShowDeleted := AValue;
    RebuildList;
  finally
    EndUpdate;
  end;
end;

procedure TCustomListMediator.CreateSubMediators;
var
  I: integer;
begin
  CreateColumns;
  for I := 0 to Model.Count - 1 do
    if (not Model.Items[i].Deleted) or FShowDeleted then
      DoCreateItemMediator(Model.Items[i], i);
end;

function TCustomListMediator.DataAndPropertyValid(const AData: TtiObject): Boolean;
var
  c: integer;
  lField: string;
begin
  Result := (Subject <> nil) and (FFieldsInfo.Count > 0);
  if not Result then
    Exit; //==>

  for c := 0 to FFieldsInfo.Count - 1 do
  begin
    lField := FFieldsInfo[c].PropName;
    { WRONG!!  We should test the items of the Model }
    Result := (IsPublishedProp(AData, lField));
    if not Result then
      raise Exception.CreateFmt('<%s> is not a property of <%s>',
        [lField, AData.ClassName]);
  end;
end;

procedure TCustomListMediator.DoGUIToObject;
begin
  // Do nothing. List is essentially read-only.
  // inherited DoGUIToObject;
end;

procedure TCustomListMediator.DoObjectToGUI;
begin
  RebuildList;
end;

constructor TCustomListMediator.Create;
begin
  inherited Create;
  FFieldsInfo   := TtiMediatorFieldInfoList.Create(TtiMediatorFieldInfo);
  FMediatorList := TObjectList.Create;
  FShowDeleted  := False;
  Active        := False;
end;

class function TCustomListMediator.CompositeMediator: Boolean;
begin
  Result := True;
end;

procedure TCustomListMediator.ParseDisplayNames(const AValue: string);
var
  I: integer;
  lField: string;
  lInfo: TtiMediatorFieldInfo;
begin
  FFieldsInfo.Clear;
  for I := 1 to tiNumToken(AValue, cFieldDelimiter) do
  begin
    lField         := tiToken(AValue, cFieldDelimiter, I);
    lInfo          := FFieldsInfo.AddFieldInfo;
    lInfo.AsString := lfield;
  end; { Loop }
end;

initialization
  // nothing

finalization
  FreeAndNil(uMediatorManager);

end.

