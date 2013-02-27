unit tiExcept;

interface
uses
  SysUtils
  ;

type

  // Abstract tiOPF Exception
  EtiOPFException = class( Exception )
  ;

  // Abstract exception for tiOPF internal errors
  EtiOPFInternalException = class( ETIOPFException )
  ;

  // Abstract exception for tiOPF errors, caused by a programmer using the framework
  EtiOPFProgrammerException = class( ETIOPFException )
  ;

  // Abstract exception for errors caused by bad user input
  EtiOPFUserException = class( ETIOPFException )
  ;

implementation

end.
