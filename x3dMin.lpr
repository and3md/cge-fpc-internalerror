program x3dMin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  CastleInternalNodeInterpolator,
  CastleInternalX3DLexer,
  CastleMaterialProperties, X3DFields,
  X3DNodes;

{$R *.res}

begin
  Writeln('test');
end.

