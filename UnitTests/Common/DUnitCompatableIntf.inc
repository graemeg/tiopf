{ %MainUnit DUnitCompatable.pp}

    { Dunit compatable interface for FPCunit and FPC < 2.0.4 }
//    function  GetName: string; virtual;
//    property  Name: string read GetName;

    {$IFDEF VER2_0_2}
    procedure Check(AValue: boolean; AMessage: string = '');
    procedure CheckEquals(expected, actual: extended; msg: string = ''); overload;
    procedure CheckEquals(expected, actual: string; msg: string = ''); overload;
    procedure CheckEquals(expected, actual: extended; delta: extended; msg: string = ''); overload;
    procedure CheckEquals(expected, actual: integer; msg: string = ''); overload;
    procedure CheckEquals(expected, actual: boolean; msg: string = ''); overload;
    procedure CheckEquals(expected, actual: TClass; msg: string = ''); overload;
    procedure CheckNotEquals(expected, actual: string; msg: string = ''); overload;
    procedure CheckNull(obj: IUnknown; msg: string = ''); overload;
    procedure CheckNull(obj: TObject; msg: string = ''); overload;
    procedure CheckNotNull(obj: TObject; msg: string = ''); overload;
    procedure CheckIs(obj :TObject; AClass: TClass; msg: string = ''); overload;
    procedure CheckSame(expected, actual: TObject; msg: string = ''); overload;
    {$ENDIF}
    {$IFDEF VER2_0_4}
    procedure CheckNotEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: boolean; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected: extended; actual: extended; delta: extended = 0; msg: string = ''); overload; virtual;
    procedure FailNotEquals(expected, actual: string; msg: string = ''; errorAddr: Pointer = nil); virtual;
    {$ENDIF}

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



