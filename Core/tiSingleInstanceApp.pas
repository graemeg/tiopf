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

  Created: 01/09/1999

  Notes: To allow only one instance of an application to run on a given machine,
         use this unit, tiSingleInstanceApp towards the top of the list
         of units in the DPR file.

         The first instance of the app will load normally.
         Any subsequent instances will not load and the first
         instance will be given focus.

  Usage: 1. Add the following two lines before any other code in the
            application's DPR file:
              if not tiSingleInstanceApp.IsFirstInstance then
                exit ; //==>
         2. Add the following line in the main form's OnCreate event:
             tiSingleInstanceApp.SaveWindowHandle( self ) ;

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit tiSingleInstanceApp;

{$I tiDefines.inc}

interface
uses
  {$IFDEF MSWINDOWS}
   Windows,
  {$ENDIF MSWINDOWS}
  Forms    // For TApplication
  ;

function  tiIsFirstInstance( psDatabase : string = '' ) : boolean ;
procedure tiSaveWindowHandle( pForm : TForm ) ;
function  tiGetWindowHandle : THandle ;

implementation
uses
   tiUtils
  ,SysUtils
  ,tiRegINI
  ;

var
  uhMutex : THandle ;
  upcMutexName : PChar ;

function tiIsFirstInstance( psDatabase : string = '' ) : boolean ;
var
  lsMutexName : string ;
  lhWindow : THandle ;
begin
  lsMutexName := ParamStr( 0 ) + psDatabase ;

  lsMutexName := tiStrTran( lsMutexName, '\', '' ) ;
  lsMutexName := tiStrTran( lsMutexName, ':', '' ) ;
  lsMutexName := tiStrTran( lsMutexName, '.', '' ) ;
  lsMutexName := upperCase( lsMutexName ) ;
  upcMutexName := PChar( lsMutexName ) ;

  uhMutex := OpenMutex( MUTEX_ALL_ACCESS, false, upcMutexName );

  if uhMutex <> 0 then begin
    result := false ;
    CloseHandle( uhMutex ) ;
    lhWindow := tiGetWindowHandle ;
    if lhWindow <> 0 then begin
      // tiShowMessage( lhWindow ) ;
      // ShowWindow( lhWindow, SW_HIDE	 ) ;
      // ShowWindow( lhWindow, SW_SHOWMAXIMIZED  ) ;
      // ShowWindow( lhWindow, SW_SHOWMINIMIZED	 ) ;
      ShowWindow( lhWindow, SW_RESTORE	 ) ;
      // ShowWindow( lhWindow, SW_SHOW	 ) ;
      // ShowWindow( lhWindow, SW_SHOWDEFAULT	 ) ;
      // ShowWindow( lhWindow, SW_SHOWNA	 ) ;
      // ShowWindow( lhWindow, SW_SHOWNOACTIVATE	 ) ;
      // ShowWindow( lhWindow, SW_SHOWNORMAL	 ) ;
      SetForegroundWindow( lhWindow ) ;
    end ;

  end else begin
    result := true ;
    CreateMutex( nil, true, upcMutexName ) ;
  end ;

end ;


procedure tiSaveWindowHandle( pForm : TForm ) ;
begin
  gReg.WriteInteger( 'FMain', 'WindowHandle', pForm.Handle ) ;
end ;


function tiGetWindowHandle : THandle ;
begin
  result := THandle( gReg.ReadInteger( 'FMain', 'WindowHandle', 0 )) ;
end ;


initialization
  uhMutex := 0 ;


finalization

  if uhMutex <> 0 then begin
    ReleaseMutex( uhMutex ) ;
    CloseHandle(  uhMutex ) ;
  end ;

{$Warnings Off}

end.

(*

Notes from Graeme Chandler's presentation to ADUG, Sept 99
http://www.gajits.com/p_win32.html

Using a Mutex

There are several different kinds of kernel objects. When a kernel
object is created it exists in the address space of the process
and that process gets a handle to that object. This handle can't
be passed to another process or reused by the next process to
access the same kernel object. However a second process can obtain
its own handle to a kernel object that already exists by using an
appropriate Win32 API function.

For example, the CreateMutex() function creates a named or unnamed
mutex object and returns its handle. The OpenMutex() Win32 API
function returns the handle to an existing named mutex object.

The following code demonstrates two ways of using calls to Mutex
functions to prevent multiple instances of an application from
existing.

1. Sample code using CreateMutex and WaitForSingleObject

Code in red indicates lines added to a standard project file to
implement the mutex code that will prevent multiple instances of
an application running.

program Project1;
uses
  Windows,
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

var
  hMutex: THandle;

begin
  hMutex := CreateMutex(nil, False, 'myUniqueName');
  if WaitForSingleObject(hMutex, 200) = Wait_Object_0 then begin
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.Run;
    ReleaseMutex(hMutex);
  end;
  CloseHandle(hMutex);
end.


2. Sample code using OpenMutex


program Project1;
uses
  Windows,
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

var
  hMutex: THandle;

begin
  hMutex := OpenMutex(MUTEX_ALL_ACCESS, false, 'myUniqueName');

  if hMutex <> 0 then begin
    CloseHandle(hMutex);
    exit;
  end;

  CreateMutxe(nil, true, 'myUniqueName');

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  ReleaseMutex(hMutex);
  CloseHandle(hMutex);
end.


Why doesn't the exit code call ReleaseMutex? Because it doesn't own
the mutex object. (A ReleaseMutex is not really necessary prior to
a close but its a good habit to get into.) The first instance of
the running application owns the mutex. But any other instance
should close the handle to its reference to the mutex object.

The above code samples demonstrate that multiple processes can
access the same kernel objects. This is achieved via usage counts.

*)
