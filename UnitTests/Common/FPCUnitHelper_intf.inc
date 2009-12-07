{%MainUnit tiTestFramework.pas}


    { DUnit2 compatable interface for FPCUnit and Free Pascal }

    AllowedMemoryLeakSize: Integer;
    function    GetName: string; virtual;
    property    Name: string read GetName;

    procedure CheckTrue(condition: Boolean; msg: string = ''); virtual;
    procedure CheckFalse(condition: Boolean; msg: string = ''); virtual;
    function  EqualsErrorMessage(const expected, actual: String;
                                 const ErrorMsg: string): String; virtual;
    function  NotEqualsErrorMessage(const expected, actual: String;
                                    const ErrorMsg: string): String; virtual;
    procedure InhibitStackTrace;
    
    procedure SetAllowedLeakArray(const AllowedList: array of Integer);

    {
    *** TODO  ***
    procedure CheckEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckNotEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotNull(obj :IUnknown; msg :string = ''); overload; virtual;
    procedure CheckSame(expected, actual: IUnknown; msg: string = ''); overload; virtual;

    procedure CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string = '');
    procedure CheckInherits(expected, actual: TClass; msg: string = ''); overload; virtual;
    }



