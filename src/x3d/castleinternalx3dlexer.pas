{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML/X3D classic lexer (TX3DLexer). }
unit CastleInternalX3DLexer;

{$I ../base/castleconf.inc}

interface

uses SysUtils, Classes, Math;

type
  TX3DEncoding = (xeClassic, xeXML);

  TX3DVersion = object
    Major, Minor: Integer;
    function FileExtension(const Encoding: TX3DEncoding;
      const ForceConvertingToX3D: boolean = false): string;
    { File filters if you want to save a file using Save3D. }
    function FileFilters(const Encoding: TX3DEncoding;
      const ForceConvertingToX3D: boolean = false): string;
  end;

  TX3DLexer = class
  end;

implementation

uses X3DFields;// remove this X3DFields (X3DLoadInternalUtils was here before) include to get rid of internal error


{ TX3DVersion --------------------------------------------------------------- }

function TX3DVersion.FileExtension(const Encoding: TX3DEncoding;
  const ForceConvertingToX3D: boolean): string;
begin
  if Encoding = xeXML then
    Result := '.x3d' else
  if (Major >= 3) or ForceConvertingToX3D then
    Result := '.x3dv' else
    Result := '.wrl';
end;

function TX3DVersion.FileFilters(const Encoding: TX3DEncoding;
  const ForceConvertingToX3D: boolean): string;
const
  SaveVRMLClassic_FileFilters =
  'All files|*|' +
  '*VRML (*.wrl)|*.wrl|' +
  'VRML (compressed) (*.wrz, *.wrl.gz)|*.wrz;*.wrl.gz';
  SaveX3DClassic_FileFilters =
  'All files|*|' +
  '*X3D classic (*.x3dv)|*.x3dv|' +
  'X3D classic (compressed) (*.x3dvz, *.x3dv.gz)|*.x3dvz;*.x3dv.gz';
  SaveX3DXml_FileFilters =
  'All files|*|' +
  '*X3D XML (*.x3d)|*.x3d|' +
  'X3D XML (compressed) (*.x3dz, *.x3d.gz)|*.x3dz;*.x3d.gz';
begin
  if Encoding = xeXML then
    Result := SaveX3DXml_FileFilters else
  if (Major >= 3) or ForceConvertingToX3D then
    Result := SaveX3DClassic_FileFilters else
    Result := SaveVRMLClassic_FileFilters;
end;

end.
