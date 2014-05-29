{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    Created: 01/06/1999

  Notes: Abstract factory.
         This factory will create TObject or TComponent descendants
         Descend from this object, and write a new CreateInstance function to
         create objects or components which have been type cast correctly.

         If you are wanting to create TPerObjAbs descendants, do not use this
         factory (the overridden constructor will not be called because the
         constructor on TPersistent is not virtual). Use the TPerObjFactory which
         can be found in tiPtnVisPerObj instead.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}



{$I tiDefines.inc}

unit tiFactory ;

interface
uses
  SysUtils
  ,Classes             // For TObject
  ;
                      
type

  // Class reference for TObject descendants
  //----------------------------------------------------------------------------
  TObjectClassRef = class of TObject ;

  // Class reference for TComponent descendants
  //----------------------------------------------------------------------------
  TComponentClassRef = class of TComponent ;

  // Tell the factory to create a TObject or a TComponent.
  // This is necessary as TComponent descendents will need
  // an owner parameter.
  //----------------------------------------------------------------------------
  TCreateAs = ( caTObject, caTComponent ) ;

  // After a class is registered with the factory, a
  // TClassMappingAbstract descendant will be added to the
  // list of registered objects.
  //----------------------------------------------------------------------------
  TClassMappingAbstract = class( TObject )
  private
    FStrClassID : string ;      // A string to identify the class
    FCreateAs   : TCreateAs ;   // Create as a TObject or TComponent
    FBoolSingleton : boolean ;  // Cache this instance ?
  public
    property    ClassID  : string read  FStrClassID
                                  write FStrClassID ;
    property    CreateAs : TCreateAs read  FCreateAs
                                     write FCreateAs ;
    property    Singleton : boolean read FBoolSingleton
                                    write FBoolSingleton ;
  end ;

  // Used when a TObject descendant is registered
  //----------------------------------------------------------------------------
  TClassMappingObject = class( TClassMappingAbstract )
  private
    FClassRef     : TObjectClassRef ;  // TObject class reference
  public
    Constructor CreateExt( const pStrClassID : string ;
                  pClassRef : TObjectClassRef ;
                  const pBoolSingleton : boolean ) ;
    property    ClassRef : TObjectClassRef read FClassRef
                                           write FClassRef ;
  end ;

  // Used when a TComponent descendant is registered
  //----------------------------------------------------------------------------
  TClassMappingComponent = class( TClassMappingAbstract )
  private
    FClassRef     : TComponentClassRef ;  // TComponent class reference
  public
    Constructor CreateExt( const pStrClassID : string ;
                  pClassRef : TComponentClassRef ;
                  const pBoolSingleton : boolean ) ;
    property    ClassRef : TComponentClassRef read  FClassRef
                                              write FClassRef ;
  end ;

  // The abstract factory
  //----------------------------------------------------------------------------
  TtiFactory = class( TObject )
  private
    FClassMappings : TStringList ;  // List of registered classes
    FObjectCache   : TStringList ;  // Cache of already created objects
    function GetClassMappingIndex( const pClassID : string ) : integer ;
  protected
    Property   ClassMappings : TStringList read FClassMappings ;
    Property   ObjectCache   : TStringList read FObjectCache ;
    // Create an instance of our class, or return a pointer to the existing
    // instance if already created. This function is protected to force
    // you to create a public implementation in a concrete class.
    Function   CreateInstance( const pStrClassID : string ; owner : TComponent = nil ) : TObject ; virtual ;
  public
    Constructor Create ; virtual ;
    Destructor  Destroy ; override ;
    // Register a TObject descendant
    Procedure   RegisterObjectClass( const pStrClassID : string;
                                     pClassRef : TObjectClassRef ;
                                     const pBoolSingleton : boolean = false ) ; overload ;
    // Register a TComponent descendant
    Procedure   RegisterComponentClass( const pStrClassID : string;
                                        pClassRef : TComponentClassRef ;
                                        const pBoolSingleton : boolean = false ) ; overload ;
    Function   GetClassRef( const pStrClassID : string ) : TObjectClassRef ;
    Function   GetClassID( pData : TObject ) : string ; overload ;
    Function   GetClassID( psClassName : string ) : string ; overload ;
  end ;

implementation
uses
  Dialogs   // MessageDlg
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TClassMappingObject: Hold information about how to create a TObject
// *                      descendant.
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TClassMappingObject.CreateExt(
                  const pStrClassID : string;
                  pClassRef: TObjectClassRef ;
                  const pBoolSingleton : boolean ) ;
begin
  Create ;
  ClassID  := pStrClassID ;
  ClassRef := pClassRef  ;
  CreateAs := caTObject ;
  Singleton := pBoolSingleton ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TClassMappingComponent: Hold information about how to create a
// *                         TComponent descendant.
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TClassMappingComponent.CreateExt(
                  const pStrClassID: string;
                  pClassRef: TComponentClassRef ;
                  const pBoolSingleton : boolean ) ;
begin
  Create ;
  ClassID  := pStrClassID ;
  ClassRef := pClassRef  ;
  CreateAs := caTComponent ;
  Singleton := pBoolSingleton ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFactoryAbstract: The abstract factory.
// *                   Used to create TObject and TComponent descendants.
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiFactory.Create;
begin
  inherited ;
  FClassMappings := TStringList.Create ;
  FObjectCache   := TStringList.Create ;
end;

//------------------------------------------------------------------------------
destructor TtiFactory.Destroy;
var i : integer ;
begin

  // Scan through FClassMappings,
  // and free any associated objects
  for i := 0 to FClassMappings.Count - 1 do
    TObject( FClassMappings.Objects[i] ).Free ;
  // Free FClassMappings
  FClassMappings.Free ;

  // Free any objects in the cache
  for i := 0 to FObjectCache.Count - 1 do begin
    try
      TObject( FObjectCache.Objects[i] ).Free ;
    except end ;
  end ;
  FObjectCache.Free ;

  // Call inherited
  inherited ;

end;

// Register a class mapping for a TObject descendant.
//------------------------------------------------------------------------------
procedure TtiFactory.RegisterObjectClass(
                               const pStrClassID: string;
                               pClassRef: TObjectClassRef ;
                               const pBoolSingleton : boolean );
var i : integer ;
    lClassMapping : TClassMappingObject ;
    lStrClassID : string ;
begin
  lStrClassID := pStrClassID ;

  // Does the class mapping alread exist?
  i := GetClassMappingIndex( lStrClassID );

  // If yes, report an error.
  // We do not raise an exception here as we may be inside an
  // initialization section.
  if i <> -1 then begin
    messageDlg( 'Registering a duplicate ' +
                'class mapping <' +
                pStrClassID + '>',
                mtInformation,
                [mbOK],
                0 ) ;
    Exit ; //==>
  end ;

  // Create the class mapping object
  lClassMapping := TClassMappingObject.CreateExt(
                      lStrClassID,
                      pClassRef,
                      pBoolSingleton ) ;

  // Add the class mapping object to the list
  FClassMappings.AddObject( pStrClassID,
                             lClassMapping ) ;

end;

// Register a class mapping for a TComponent descendant.
//------------------------------------------------------------------------------
procedure TtiFactory.RegisterComponentClass(
                                             const pStrClassID: string;
                                             pClassRef: TComponentClassRef ;
                                             const pBoolSingleton : boolean = false );
var i : integer ;
    lClassMapping : TClassMappingComponent ;
    lStrClassID : string ;
begin
  lStrClassID := pStrClassID ;

  // Does the class mapping already exist?
  i := GetClassMappingIndex( lStrClassID );

  // If yes, report an error.
  // We do not raise an exception here as we may be inside an
  // initialization section.
  if i <> -1 then begin
    messageDlg( 'Registering a duplicate ' +
                'class mapping <' +
                pStrClassID + '>',
                mtInformation,
                [mbOK],
                0 ) ;
    Exit ; //==>
  end ;

  // Create a classMapping object
  lClassMapping := TClassMappingComponent.CreateExt(
                      lStrClassID,
                      pClassRef,
                      pBoolSingleton ) ;

  // Add the ClassName, and ClassMapping object to the list
  FClassMappings.AddObject( pStrClassID,
                             lClassMapping ) ;

end;

// Either look up an existing instance of the object in the cache, or
// create a new one. DoCreateInstance should only be called from a concrete
// descendant of TFactoryAbstract.
//------------------------------------------------------------------------------
function TtiFactory.CreateInstance( const pStrClassID : string ; owner : TComponent = nil ) : TObject ;
var lIntCacheIndex   : integer ;
    lIntMappingIndex : integer ;
    lStrClassID      : string ;
    lClassMapping    : TClassMappingAbstract ;
begin

  // Get a temporary copy of ClassID, in upper case
  lStrClassID := pStrClassID ;

  // Does the class mapping exist?
  lIntMappingIndex := GetClassMappingIndex( lStrClassID );

  // If not, then raise an exception
  // We can raise an exception here as we are not likely to be inside
  // initialization code
  if lIntMappingIndex = -1 then
    Raise Exception.Create( 'Request for invalid class ' +
                            'name <' +
                            pStrClassID + '>' +
                            ' Called by factory: ' +
                            ClassName ) ;

  // Is the object already in the cache?
  // Yes, then return the cahced copy
  // No, then create one
  lIntCacheIndex := FObjectCache.IndexOf( lStrClassID );

  // The object is not already in the cache
  if lIntCacheIndex = -1 then begin
    // Get a pointer to the correct class mapping
    lClassMapping := TClassMappingAbstract(
                FClassMappings.Objects[lIntMappingIndex] ) ;

    // Do we create this object as a TComponent or a TObject?
    if lClassMapping.CreateAs = caTComponent then
      result :=
        TClassMappingComponent( lClassMapping ).ClassRef.Create( owner )
    else
      result :=
        TClassMappingObject( lClassMapping ).ClassRef.Create ;

    // If this class is to be cached, then add it to the list
    if lClassMapping.Singleton then
      FObjectCache.AddObject( lStrClassID, result ) ;

  // The object is already in the cache
  end else begin
    // So return the existing copy
    result := FObjectCache.Objects[ lIntCacheIndex ] ;

  end ;

end ;

function TtiFactory.GetClassRef(const pStrClassID: string): TObjectClassRef;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to ClassMappings.Count - 1 do
    if SameText( ClassMappings.Strings[i], pStrClassID ) then begin
      result := TClassMappingObject( ClassMappings.Objects[i] ).ClassRef ;
      Break ; //==>
    end ;
end;

function TtiFactory.GetClassID(pData : TObject): string;
begin
  result := GetClassID( pData.ClassName ) ;
end;

function TtiFactory.GetClassID(psClassName: string): string;
var
  i : integer ;
begin
  result := '' ;
  for i := 0 to ClassMappings.Count - 1 do
    if SameText( TClassMappingObject( ClassMappings.Objects[i] ).ClassRef.ClassName, psClassName ) then begin
      result := ClassMappings.Strings[i] ;
      Break ; //==>
    end ;
end;

function TtiFactory.GetClassMappingIndex(const pClassID: string): integer;
var
  i : integer ;
begin
  result := -1 ;
  for i := 0 to FClassMappings.Count - 1 do
    if SameText( pClassID, FClassMappings.Strings[i] ) then
    begin
      result := i ;
      Exit ; //==>
    end ;
end;

end.

