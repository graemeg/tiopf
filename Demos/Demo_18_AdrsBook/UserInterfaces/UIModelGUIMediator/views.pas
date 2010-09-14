unit views;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, tiListMediators, tiMediators;

type

  { List Meditators }
  
  TContacts_ListView_Mediator = class(TtiListViewMediatorView)
  public
    procedure SetupGUIandObject; override;
  end;
  
  
  TCityList_ListView_Mediator = class(TtiListViewMediatorView)
  public
    procedure SetupGUIandObject; override;
  end;


  { Edit Mediators }
  
  TContact_FirstName_TEdit_Mediator = class(TtiEditMediatorView)
  end;

  
  TContact_LastName_TEdit_Mediator = class(TtiEditMediatorView)
  end;

  
  TContact_EMail_TEdit_Mediator = class(TtiEditMediatorView)
  end;


  TContact_Mobile_TEdit_Mediator = class(TtiEditMediatorView)
  end;


  TContact_Comments_TMemo_Mediator = class(TtiMemoMediatorView)
  end;
  
  
  TContact_City_TCombobox_Mediator = class(TtiDynamicComboBoxMediatorView)
  public
    procedure SetupGUIandObject; override;
  end;
  

  TCity_Name_TEdit_Mediator = class(TtiEditMediatorView)
  end;
  
  
  TCity_Zip_TEdit_Mediator = class(TtiEditMediatorView)
  end;


  TCity_Country_TCombobox_Mediator = class(TtiDynamicComboBoxMediatorView)
  public
    procedure SetupGUIandObject; override;
  end;


implementation

uses
  ComCtrls, StdCtrls;

{ TContacts_ListView_Mediator }

procedure TContacts_ListView_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
{$IFDEF FPC}
  View.ScrollBars:= ssAutoBoth;
{$ENDIF}
  View.RowSelect:= True;
  View.ViewStyle:= vsReport;
end;

{ TContact_City_TCombobox_Mediator }

procedure TContact_City_TCombobox_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.Style:= csDropDownList;
end;

{ TCityList_ListView_Mediator }

procedure TCityList_ListView_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
{$IFDEF FPC}
  View.ScrollBars:= ssAutoBoth;
{$ENDIF}
  View.RowSelect:= True;
  View.ViewStyle:= vsReport;
end;

{ TCity_Country_TCombobox_Mediator }

procedure TCity_Country_TCombobox_Mediator.SetupGUIandObject;
begin
  inherited SetupGUIandObject;
  View.Style:= csDropDownList;
end;

end.