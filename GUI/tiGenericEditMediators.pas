
(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
The contents of this file are subject to the Mozilla Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS
IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.

If you make any changes or enhancements, which you think will
benefit other developers and will not break any existing code,
please forward your changes (well commented) to graemeg@gmail.com
and I will make them permanent.

Revision history:

  2005-08-17: First release by Graeme Geldenhuys (graemeg@gmail.com)

Purpose:
  Abstract mediating view and Mediator Factory. This allows you to use
  standard edit components and make them object-aware.  See the demo
  application for usage.

ToDo:
  * Implement container controls. eg: TTreeview and Virtual TreeView
  * Unit tests
  * More refactoring
  * Implement a View Manager class, so we can remove the View Lists
    created in each Form using mediating views.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit tiGenericEditMediators;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses
  tiObject
  ,Controls
  ,Classes
  ;

type
  TMediatingViewClass = class of TMediatorView;

  { Base class to inherit from to make more customised Mediator Views. }
  TMediatorView = class(TtiObject)
  private
    FSettingUp: Boolean;
    FFieldName: string;
    FSubject: TtiObject;
    FEditControl: TControl;
    FGuiFieldName: string;
    FErrorMessage: string;
    procedure   TestIfValid;
  protected
    function    GetSubject: TtiObject; virtual;
    { Used to setup things like the MaxLength of a edit box, etc. }
    procedure   SetupGUIandObject; virtual;
    { Used for doing validation checks and changing the color of edit controls
      in error }
    procedure   UpdateGuiValidStatus(pErrors: TtiObjectErrors); virtual;
  public
    constructor Create; override;
    constructor CreateCustom( pEditControl: TControl; pSubject: TtiObject; pFieldName: string; pGuiFieldName: string = '' );
    destructor  Destroy; override;
    { Copies values from the edit control to the Subject }
    procedure   GuiToObject; virtual;
    { Copies property values from the Subject to the edit control }
    procedure   ObjectToGui; virtual;
    procedure   Update(pSubject: TtiObject); override;
    { This is what gets called from the edit controls OnChange event, to
      trigger a update }
    procedure   GUIChanged;
    { The object being edited or observed }
    property    Subject: TtiObject read GetSubject write FSubject;
    { The edit control used for editing a property of the Subject }
    property    EditControl: TControl read FEditControl write FEditControl;
    { Not being used at the moment }
    property    ErrorMessage: string read FErrorMessage write FErrorMessage;
    class function ComponentClass: TClass; virtual; abstract;
  published
    { Property of the Subject being edited }
    property    FieldName: string read FFieldName write FFieldName;
    { Property of the edit control used to get/set the new updated value }
    property    GuiFieldName: string read FGuiFieldName write FGuiFieldName;
  end;


  { Base class to handle TEdit controls }
  TMediatorEditView = class(TMediatorView)
  public
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TSpinEdit controls }
  TMediatorSpinEditView = class(TMediatorView)
  private
    procedure OnLostFocus(Sender: TObject);
  protected
    procedure SetupGUIandObject; override;
  public
    class function ComponentClass: TClass; override;
    procedure GuiToObject; override;
  end;


  { Base class to handle TTrackBar controls }
  TMediatorTrackBarView = class(TMediatorView)
  public
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TComboBox controls }
  TMediatorComboBoxView = class(TMediatorView)
  public
    procedure   ObjectToGui; override;
    class function ComponentClass: TClass; override;
  end;


  { Base class to handle TMemo controls }
  TMediatorMemoView = class(TMediatorView)
  protected
    procedure   SetupGUIandObject; override;
  public
    class function ComponentClass: TClass; override;
    procedure   ObjectToGui; override;
    procedure   GuiToObject; override;
  end;


  { Data class for mapping a name to a class }
  TMediatorViewMapping = class(TObject)
  private
    FMediatingViewClass: TMediatingViewClass;
    FName: string;
  public
    constructor CreateExt( pName: String; pMediatingClass: TMediatingViewClass );
    property    Name: string read FName write FName;
    property    MediatingViewClass: TMediatingViewClass read FMediatingViewClass write FMediatingViewClass;
  end;


  { This is a parameter object, instead of a whole bunch of single parameters }
  TMGMEditLink = class(TObject)
  private
    FEditControl: TControl;
    FEditObject: TtiObject;
    FObjectEditProperty: string;
    FControlEditProperty: string;
  public
    property    EditControl: TControl read FEditControl write FEditControl;
    property    EditObject: TtiObject read FEditObject write FEditObject;
    property    ObjectEditProperty: string read FObjectEditProperty write FObjectEditProperty;
    property    ControlEditProperty: string read FControlEditProperty write FControlEditProperty;
  end;


  { Factory class to register and create your mediating views }

  { TMediatorFactory }

  TMediatorFactory = class(TObject)
  private
    MappingList: TStringList;
    function    FindMediatorClass(pSubject: TtiObject; pComponentClass: TClass; pFieldName: string): TMediatingViewClass;
    function    GetMediatorClass(pSubject: TtiObject; pComponentClass: TClass; pFieldName: string): TMediatingViewClass;
  public
    constructor Create;
    destructor  Destroy; override;
    function    CreateMediator(pComponent: TControl; pSubject: TtiObject; pFieldName: String; pGuiFieldName: string): TMediatorView; overload;
    function    CreateMediator(pEditLink: TMGMEditLink): TMediatorView; overload;
//    function    FindMediator(pComponent: TControl): TMediatorView;
    procedure   RegisterMediatorClass(FieldName: string; MediatorClass: TMediatingViewClass);
  end;


  { Simple singelton for the Factory }
  function gMediatorFactory: TMediatorFactory;


implementation
uses
  SysUtils
  ,TypInfo
  ,Dialogs    { MessageDlg }
  ,StdCtrls   { TEdit, TComboBox }
  ,Spin       { TSpinEdit }
  ,ComCtrls   { TTrackBar }
  ;

var
  uMediatorFactory: TMediatorFactory;


function gMediatorFactory: TMediatorFactory;
begin
  if not Assigned(uMediatorFactory) then
    uMediatorFactory := TMediatorFactory.Create;
  result := uMediatorFactory;
end;


{ TMediatorView }

constructor TMediatorView.Create;
begin
  inherited;
  FSettingUp := True;
end;


constructor TMediatorView.CreateCustom(pEditControl: TControl; pSubject: TtiObject; pFieldName: string; pGuiFieldName: string);
begin
  Create;
  FSubject        := pSubject;
  FFieldName      := pFieldName;
  FGuiFieldName   := pGuiFieldName;
  FEditControl    := pEditControl;
  FSubject.AttachObserver(self);
  SetupGUIandObject;
  
  // I prefer to do this once in the form after all mediator are created.
//  FSubject.NotifyObservers;
  FSettingUp      := False;
end;

destructor TMediatorView.Destroy;
begin
  FSubject.DetachObserver(self);
  inherited Destroy;
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
  { This will be implemented by a concrete class }
end;


procedure TMediatorView.TestIfValid;
var
  Errors: TtiObjectErrors;
begin
  Errors := TtiObjectErrors.Create;
  try
    Subject.IsValid(Errors);
    UpdateGuiValidStatus( Errors );
  finally
    Errors.Free;
  end;
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


procedure TMediatorView.GuiToObject;
begin
  Subject.PropValue[FieldName] := TypInfo.GetPropValue((FEditControl as ComponentClass), GuiFieldName);
end;


procedure TMediatorView.ObjectToGui;
begin
  TypInfo.SetPropValue( (FEditControl as ComponentClass), GuiFieldName,
      Subject.PropValue[FieldName]);
end;


procedure TMediatorView.SetupGUIandObject;
begin
  { do nothing here }
end;


{ TMediatorFactory }

constructor TMediatorFactory.Create;
begin
  MappingList := TStringList.Create;
end;


function TMediatorFactory.CreateMediator(pComponent: TControl; pSubject: TtiObject;
    pFieldName: string; pGuiFieldName: string): TMediatorView;
var
  MediatorClass: TMediatingViewClass;
begin
  if not Assigned(pComponent) then
    raise Exception.Create('TMediatorFactory.CreateMediator: pComponent is not assigned');
  if not Assigned(pSubject) then
    raise Exception.Create('TMediatorFactory.CreateMediator: pSubject is not assigned');

  MediatorClass := GetMediatorClass(
                      pSubject,
                      pComponent.ClassType,
                      pFieldName );
  result        := MediatorClass.CreateCustom(
                      pComponent,
                      pSubject,
                      pFieldName,
                      pGuiFieldName );
  pSubject.AttachObserver( result );
end;


function TMediatorFactory.CreateMediator(pEditLink: TMGMEditLink): TMediatorView;
var
  MediatorClass: TMediatingViewClass;
begin
  MediatorClass := GetMediatorClass(
                      pEditLink.EditObject,
                      pEditLink.EditControl.ClassType,
                      pEditLink.ObjectEditProperty );
  result        := MediatorClass.CreateCustom(
                      pEditLink.EditControl,
                      pEditLink.EditObject,
                      pEditLink.ObjectEditProperty,
                      pEditLink.ControlEditProperty );
  pEditLink.EditObject.AttachObserver( Result );
end;


destructor TMediatorFactory.Destroy;
var
  i: integer;
begin
  for i := 0 to MappingList.Count -1 do
    TObject(MappingList.Objects[i]).Free;
  MappingList.Free;
  inherited;
end;


function TMediatorFactory.FindMediatorClass(pSubject: TtiObject; pComponentClass: TClass; pFieldName: string): TMediatingViewClass;
const
  cName = '%s.%s.%s';    { Subject Classname, FieldName, Edit control name }
var
  i: Integer;
  lName: string;
begin
  { Get the name formatting correct }
  lName := Format(cName, [UpperCase(pSubject.ClassName), UpperCase(pFieldName), UpperCase(pComponentClass.ClassName)]);
  { Does the Type exist in the list? }
  i := MappingList.IndexOf( lName );
  if i <> -1 then
    Result := TMediatorViewMapping(MappingList.Objects[i]).MediatingViewClass
  else
    Result := nil;
end;


function TMediatorFactory.GetMediatorClass(pSubject: TtiObject; pComponentClass: TClass; pFieldName: string): TMediatingViewClass;
begin
  Result := FindMediatorClass(pSubject, pComponentClass, pFieldName);
  if not Assigned(Result) then
    raise Exception.Create('No mediator registered for:' + #13#10 +
        '  Component: ' + pComponentClass.ClassName + #13#10 +
        '  FieldName: ' + pSubject.ClassName + '.' + pFieldName);
end;


procedure TMediatorFactory.RegisterMediatorClass(FieldName: string; MediatorClass: TMediatingViewClass);
const
  cName = '%s.%s';
var
  lName: String;
  i: Integer;
  lMapping: TMediatorViewMapping;
begin
  lName := Format(cName, [UpperCase(FieldName), UpperCase(MediatorClass.ComponentClass.ClassName)]);
  { Does the Medator mapping already exist? }
  i := MappingList.IndexOf( lName );
  if i <> -1 then
  begin   { If yes, notify the user }
    { We cannot raise an exception as this will be called in the Initialization
      section of a unit. Delphi's exception handling may not have been loaded yet! }
    MessageDlg('Registering a duplicate Mediator View Type <' + FieldName + '> with ' + ClassName,
        mtInformation, [mbOK], 0);
  end
  else
  begin   { If no, then add it to the list }
    lMapping := TMediatorViewMapping.CreateExt( lName, MediatorClass );
    MappingList.AddObject( lName, lMapping );
  end;
end;


{ TMediatorEditView }

class function TMediatorEditView.ComponentClass: TClass;
begin
  Result := TEdit;
end;


{ TMediatorSpinEditView}

class function TMediatorSpinEditView.ComponentClass: TClass;
begin
  Result := TSpinEdit;
end;


procedure TMediatorSpinEditView.GuiToObject;
begin
  { Control is busy clearing the value before replacing it with what the user
    typed. }
  if (TSpinEdit(EditControl).Text = '') then
    Exit; //==>
//  try
//    if (TSpinEdit(EditControl).Text = '') and (TSpinEdit(EditControl).Value = 0) then
//      Exit; //==>
//  except
//    on EConvertError do
//      begin
//        if (TSpinEdit(EditControl).Text = '') then
//          Exit; //==>
//      end;
//  end;

  { continue as normal }
  inherited;
end;


procedure TMediatorSpinEditView.OnLostFocus(Sender: TObject);
begin
  if (TSpinEdit(EditControl).Text = '') then
  begin
    { Default the EditControl to a valid value }
    TSpinEdit(EditControl).Value := 0;
    GUIChanged;
  end;
end;


procedure TMediatorSpinEditView.SetupGUIandObject;
begin
  inherited;
  TSpinEdit(EditControl).OnExit := @OnLostFocus;
end;


{ TMediatorSpinEditView}

class function TMediatorTrackBarView.ComponentClass: TClass;
begin
  Result := TTrackBar;
end;


{ TMediatorComboBoxView }

class function TMediatorComboBoxView.ComponentClass: TClass;
begin
  Result := TComboBox;
end;


procedure TMediatorComboBoxView.ObjectToGui;
begin
  TComboBox(EditControl).ItemIndex :=
    TComboBox(EditControl).Items.IndexOf( Subject.PropValue[FieldName] );
end;


{ TMediatorMemoView }

class function TMediatorMemoView.ComponentClass: TClass;
begin
  Result := TMemo;
end;


procedure TMediatorMemoView.GuiToObject;
begin
  Subject.PropValue[ FieldName ] := TMemo(EditControl).Lines.Text;
end;


procedure TMediatorMemoView.ObjectToGui;
begin
  TMemo(EditControl).Lines.Text := Subject.PropValue[ FieldName ];
end;


procedure TMediatorMemoView.SetupGUIandObject;
begin
  inherited;
  TMemo(EditControl).ScrollBars := ssVertical;
  TMemo(EditControl).WordWrap   := True;
end;


{ TMediatorViewMapping }

constructor TMediatorViewMapping.CreateExt(pName: String; pMediatingClass: TMediatingViewClass);
begin
  self.Create;
  Name                := pName;
  MediatingViewClass  := pMediatingClass;
end;


initialization
finalization
  gMediatorFactory.Free;

end.

