unit tiCGIExtensionRequest;

interface
uses
  tiBaseObject
  ,tiWebServerClientConnectionDetails
  ;

type
  {: Abstarct class for shelling out to a CGI Extension.}
  TtiCGIExtensionRequest = class(TtiBaseObject)
  public
    class function CreateInstance: TtiCGIExtensionRequest;
    function Execute(const ACGIExeName: string ;
                     const AParams : string;
                     const AConnectionDetails: TtiWebServerClientConnectionDetails): string; virtual; abstract;
  end ;

  TtiCGIExtensionRequestClass = class of TtiCGIExtensionRequest;

var
  gCGIExtensionRequestClass: TtiCGIExtensionRequestClass;

implementation

{ TtiCGIExtensionRequest }

class function TtiCGIExtensionRequest.CreateInstance: TtiCGIExtensionRequest;
begin
  Assert(gCGIExtensionRequestClass<>nil, 'gCGIExtensionRequestClass not assigned');
  result := gCGIExtensionRequestClass.Create;
end;

end.
