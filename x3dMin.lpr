program x3dMin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMinX3d, CastleInternalNodeInterpolator,
  CastleInternalX3DLexer,
  CastleMaterialProperties, X3DFields, X3DLoadInternalUtils,
  X3DNodes, X3DTime, CastleClassUtils,
  CastleInterfaces,
  CastleLog,
  CastleRectangles, CastleStringUtils,
  CastleTimeUtils, CastleUtils, CastleVectors, CastleVectorsInternalDouble,
  CastleVectorsInternalSingle, CastleUnicode;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

