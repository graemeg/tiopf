Unit tiVTReg;

Interface

Uses Classes, tiVTListView, tiVTEditors;

Procedure Register;

Implementation

Uses {$IFDEF VER130}   DsgnIntf; {$ELSE}  DesignIntf, DesignEditors;{$ENDIF VER130}

Procedure Register;
Begin
  RegisterComponents('TechInsite', [TtiVTListView]);
  RegisterComponentEditor(TtiVTListView, TCEAbout);
  RegisterComponentEditor(TtiVTListView, TCEVTColumnsEditor);
End;

End.

