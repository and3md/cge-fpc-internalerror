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
  Forms, UMinX3d,
  CastleInternalNodeInterpolator, CastleInternalNormals,
  CastleInternalShadowMaps, CastleInternalShapeOctree,
  CastleInternalTriangleOctree, CastleInternalX3DLexer,
  CastleMaterialProperties,
  X3DCameraUtils,
  X3DCastleScript, X3DFields, X3DLoadInternalUtils, X3DNodes, X3DTime, X3DTriangles
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

