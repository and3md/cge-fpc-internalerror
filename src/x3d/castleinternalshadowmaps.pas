{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shadow maps internal utilities. }
unit CastleInternalShadowMaps;

{$I castleconf.inc}
{$modeswitch nestedprocvars}{$H+}

interface

uses X3DNodes;

{ Automatically handle VRML/X3D "receiveShadows" field
  by inserting appropriate lower-level nodes.

  If Enable is @true, the appropriate lower-level nodes are added,
  or replaced (if they already existed, because you call
  ProcessShadowMapsReceivers again).
  If Enable is @false, the appropriate nodes (added by previous calls to
  ProcessShadowMapsReceivers) will be removed instead.

  For each shape with "receiveShadows", we:
  @orderedList(
    @item(extend it's "texture" field with appropriate GeneratedShadowMap,)
    @item(extend it's "texCoord" field with appropriate
      ProjectedTextureCoordinate,)
  ) }
procedure ProcessShadowMapsReceivers(Model: TX3DNode; Shapes: TObject;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);

implementation

uses SysUtils,
  CastleUtils,
  CastleLog, CastleVectors, CastleRectangles;

const
  { Suffix of VRML node names created by ProcessShadowMapsReceivers
    transformation. }
  X3DNameSuffix = '_generated_by_ProcessShadowMapsReceivers';

procedure ProcessShadowMapsReceivers(Model: TX3DNode; Shapes: TObject;
  const Enable: boolean;
  const DefaultShadowMapSize: Cardinal);
begin

end;

end.
