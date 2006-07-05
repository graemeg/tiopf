unit tiGUIUtils;

{$I tiDefines.inc}

interface
uses
  tiObject
  ,Graphics      // Canvas
  ,tiDataBuffer_BOM
  ;

const
  cgsSaveAndClose = 'Do you want to save your changes before exiting?';

{: Don't use tiPerObjAbsAsString. Use TtiObject.AsDebugString}
function  tiPerObjAbsAsString( const pVisited: TtiObject; pbIncludeDeleted: boolean = false ): string;
procedure tiShowPerObjAbs( const pVisited: TtiObject; pbIncludeDeleted: boolean = false );
procedure tiShowPerObjAbsOwnership( const pData: TtiObject );

// Set canvas.font.color to crRed if pData.Dirty, crBlank if not
procedure tiPerObjAbsFormatCanvas( const pCanvas : TCanvas ; const pData : TtiObject ) ;
// If pData.Dirty, then prompt user to save
function  tiPerObjAbsSaveAndClose( const pData      : TtiObject ;
                                   var   pbCanClose : boolean ;
                                   const psMessage  : string = cgsSaveAndClose ) : boolean ;
function  tiPerObjAbsConfirmAndDelete( const pData : TtiObject ;
                                       const pMessage : string = '' ) : boolean ;
                                       
procedure ShowTIDataSet(const pDataSet: TtiDataBuffer); // For debugging


{$IFDEF MSWINDOWS}
{: Is the Ctrl key down?}
function tiIsCtrlDown : Boolean;
{: Is the Shift key down?}
function tiIsShiftDown : Boolean;
{: Is the Alt key down?}
function tiIsAltDown : Boolean;
{$ENDIF MSWINDOWS}

implementation
uses
  tiUtils
  ,tiConstants
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,Controls // mrYes
  ,tiDialogs
  {$IFDEF FPC}
  ,LCLType    // TKeyboardState under FPC is not in Windows unit
  {$ENDIF}
  ,tiDataBuffer_Cli   // used for ShowTIDataset and TIDataSetToString method
  ;


{ Global funcs and procs }


function tiPerObjAbsAsString( const pVisited: TtiObject; pbIncludeDeleted: boolean = false ): string;
begin
  Assert( pVisited.TestValid, cErrorTIPerObjAbsTestValid );
  Result := pVisited.AsDebugString;
end;


procedure tiShowPerObjAbs( const pVisited: TtiObject; pbIncludeDeleted: boolean = false );
var
  ls: string;
begin
  ls := tiPerObjAbsAsString( pVisited, pbIncludeDeleted );
  tiShowString( ls );
end;


procedure tiShowPerObjAbsOwnership( const pData: TtiObject );
  function _GetOwnership(const pData: TtiObject; const ps: string): string;
  begin
    if pData.Owner <> nil then
    begin
      result := ps + #13 + pData.Owner.ClassName;
      Result := _GetOwnership(pData.Owner, result);
    end else
      Result := ps;
    end;
var
  ls: string;
begin
  Assert( pData.TestValid(TtiObject), cTIInvalidObjectError );
  ls := _GetOwnership(pData, pData.ClassName);
  tiShowString(ls);
end;


procedure tiPerObjAbsFormatCanvas( const pCanvas: TCanvas; const pData: TtiObject ) ;
begin
  if pData.Dirty then
    pCanvas.Font.Color := clRed
  else
    pCanvas.Font.Color := clBlack;
end;


function  tiPerObjAbsSaveAndClose( const pData      : TtiObject ;
                                   var   pbCanClose : boolean ;
                                   const psMessage  : string = cgsSaveAndClose ) : boolean ;
var
  lResult: integer;
begin

  result     := false;
  pbCanClose := true;

  if not pData.Dirty then
    exit ; //==>

  lResult := tiYesNoCancel( psMessage );
  case lResult of
    mrYes    : result := true ;
    mrNo     : ; // Do nothing
    mrCancel : pbCanClose := false;
  end;

end;


function tiPerObjAbsConfirmAndDelete(const pData: TtiObject;
    const pMessage: string = ''): boolean;
var
  lMessage: string;
begin
  result := false;

  if pData = nil then
    Exit; //==>

  if pMessage = '' then
    lMessage := 'Are you sure you want to delete <' +
                pData.Caption + '>?'
  else
    lMessage := pMessage;

  result := tiAppConfirmation( lMessage );

  if result then
    pData.Deleted := true;
end;


procedure ShowTIDataSet(const pDataSet: TtiDataBuffer);
var
  ls : string ;
begin
  ls := TIDataSetToString(pDataSet);
  tiShowString(ls);
end;


{$IFDEF MSWINDOWS}
function tiIsCtrlDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Control] And 128) <> 0) ;
end;


function tiIsShiftDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Shift] and 128) <> 0) ;
end;


function tiIsAltDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Menu] and 128) <> 0) ;
end;
{$ENDIF MSWINDOWS}


end.

