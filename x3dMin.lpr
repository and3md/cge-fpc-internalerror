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
  Forms, UMinX3d, CastleInternalNodeInterpolator, CastleInternalNormals,
  CastleInternalShadowMaps, CastleInternalShapeOctree,
  CastleInternalTriangleOctree, CastleInternalX3DLexer,
  CastleMaterialProperties, X3DFields, X3DLoadInternalUtils,
  X3DNodes, X3DTime, X3DTriangles, CastleClassUtils, CastleColors, CastleDynLib,
  CastleInterfaces, CastleInternalDoubleLists,
  CastleInternalGzio, CastleLog, CastleProgress, CastleProjection,
  CastleRectangles, CastleRendererBaseTypes, CastleStringUtils,
  CastleTimeUtils, CastleUtils, CastleVectors, CastleVectorsInternalDouble,
  CastleVectorsInternalSingle, CastleUnicode, CastleBoxes, CastleFrustum,
  CastleInternalOctree, CastleNURBS, CastleQuaternions, CastleTriangles,
  CastleTriangulate,
  CastleDownload, CastleFilesUtils,
  CastleFindFiles, CastleInternalDirectoryInformation, CastleURIUtils,
  CastleXMLUtils, CastleDataURI
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

