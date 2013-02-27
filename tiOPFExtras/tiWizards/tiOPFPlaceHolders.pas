{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Chris Latta, Data Solutions Pty Ltd
  for inclusion in the tiOPF (TechInsite Object Persistence Framework) from
    TechInsite Pty. Ltd.
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm
    EMail:           info@techinsite.com.au

  Please submit changes to tiOPF@techinsite.com.au

  Purpose: A collection of search-and-replace text pairs for creating
  units from templates for tiOPF wizards.

  Revision History:
    April 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFPlaceHolders;

interface

uses
  Classes
  , tiOPFProperty
  ;

type
  TPlaceholder = class; // Forward

  TPlaceholders = class(TCollection)
  public
    constructor Create(ItemClass: TCollectionItemClass); reintroduce;
    function Add(SearchText, ReplaceText: string): TPlaceholder;
  end;

  TPlaceholderType = (ptSimple, ptClass);

  TPlaceholder = class(TCollectionItem)
  public
    SearchText, ReplaceText: string;
    PlaceholderType: TPlaceholderType;
    Properties: TProperties;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TPlaceholders }

function TPlaceholders.Add(SearchText, ReplaceText: string): TPlaceholder;
begin
  Result := TPlaceholder(inherited Add);
  Result.SearchText := UpperCase(SearchText);
  Result.ReplaceText := ReplaceText;
end;

constructor TPlaceholders.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(TPlaceholder);
end;

{ TPlaceholder }

constructor TPlaceholder.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  PlaceholderType := ptSimple;
  Properties := TProperties.Create(TProperty);
end;

destructor TPlaceholder.Destroy;
begin
  Properties.Free;

  inherited;
end;

end.
