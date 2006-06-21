
  { These are here so that FPC can compile.  FPC doesn't support
  dynamic loading. }
  function LoadPackage(const Name: string): HMODULE;
  procedure UnloadPackage(Module: HMODULE);
  
  
