I have ported Marius work to tiOpf 2.

Installing:
-Compile and install tiPersistDataset.Dpk. The components TtiRecordDataset, TtiDataset, and TtiNestedDataset will be installed on the Data Access pallette.
Run Demo_OneToMany to see how it works.

Usage
To use TiDataset, set ObjectClass and ObjectList at runtime.
  MyTiDataset.ObjectClass:= TMytiObject;
  MyTiDataset.ObjectList:= MyList;

or

DatasetClients_.LinkObject(FClients, TClient);

You can create the dataset fields at design time using the Dataset fields editor and New.

Do not use the Add option as it will give an error.

For a class
  TClient = class( TtiObject )
...
  published
    property    ClientName : string read FClientName write FClientName ;
    property    ClientID   : string   read FClientID   write FClientID ;
    property    PhoneNumbers : TPhoneNumbers read FPhoneNumbers ;
  end ;

  ClientName and ClientId would be created as TStringFields
  PhoneNumbers should be created as TDataSetField

To use TiNestedDataset, set   DataSetField and ObjectClass at runtime

  NestedDataset_PhoneNumbers.DataSetField:= DatasetClients_PhoneNumbers;
  NestedDataset_PhoneNumbers.ObjectClass:= TPhoneNumber;

DataSetField is a field containing a TiObjectList

See the Demo app for an example

Sean
---------------------------------------
Sean Cross
mailto:sean@sourceitsoftware.com

Pics Print - The photo printing solution for Windows.
http://www.picsprint.com

Rental Property Manager - Rental management made easy http://www.sourceitsoftware.com




....................................
Original ReadMe

I used Peters DemoTIPerFramework as a demo for the dataset(s). Its nothing fancy, but it will show you how to use the TTiRecordDataset and TTiNestedDataset. I used the Interbase database for testing, but other layers shoud work as well i guess.

In order to get it running i had to make a change to Adr_Bom (the business classes used in this demo). I marked the changes with "TTiDataset demo". I added an ID to the LoopupListItem in order to make the TDBLookupComboBox working.

Some units and components have been renamed:
TOpfDataset is replaced by tiDataset.
TOpfNestedDataset is replaced by tiNestedDataset.
The unit OpfDataset.Pas is replaced by tiDataset.Pas.
The package OpfDataset.Dpk has been replaced by tiPersistDataset.Dpk.

Installing:
-Compile and install tiPersistDataset.Dpk. The components TtiRecordDataset, TtiDataset, and TtiNestedDataset will be installed on the Data Access pallette.
-Install DemoTIPerFrameworkClasses.dpk (this will register the ADR_BOM classes via the initialization. This step is only needed if you want the dataset to work on design time.
-Compile & run DemoTIPerFramework. Note that everything except the listview is build with the new dataset(s).


Please email any questions to finalist@home.nl or use the TechInsite forum..

Goodluck,
Marius

