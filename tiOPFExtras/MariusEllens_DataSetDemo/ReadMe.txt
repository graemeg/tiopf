
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

