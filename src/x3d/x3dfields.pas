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

{ X3D fields (TX3DField and many descendants). }
unit X3DFields;

{$I ../base/castleconf.inc}

interface

uses Classes, SysUtils, DOM, Generics.Collections,
  CastleVectors, CastleInternalX3DLexer, CastleUtils, CastleClassUtils,
  CastleStringUtils, CastleInterfaces,
  X3DTime;

{$define read_interface}

type
  {$I castlefields_misctypes.inc}
  {$I castlefields_x3dwriter.inc}
  {$I castlefields_x3dreader.inc}
  {$I castlefields_x3dfileitem.inc}
  {$I castlefields_x3dfieldorevent.inc}
  {$I castlefields_x3dfield.inc}
  {$I castlefields_x3devent.inc}
  {$I castlefields_x3devent_descendants.inc}
  {$I castlefields_x3dsinglefield.inc}
  {$I castlefields_x3dsinglefield_descendants.inc}
  {$I castlefields_x3dmultfield.inc}
  {$I castlefields_x3dsimplemultfield.inc}
  {$I castlefields_x3dsimplemultfield_descendants.inc}
  {$I castlefields_x3dfieldsmanager.inc}

{$undef read_interface}

implementation

uses Math, Generics.Defaults,
  X3DNodes;

{$define read_implementation}

{$I castlefields_misctypes.inc}
{$I castlefields_x3dwriter.inc}
{$I castlefields_x3dreader.inc}
{$I castlefields_x3dfileitem.inc}
{$I castlefields_x3dfieldorevent.inc}
{$I castlefields_x3dfield.inc}
{$I castlefields_x3devent.inc}
{$I castlefields_x3devent_descendants.inc}
{$I castlefields_x3dsinglefield.inc}
{$I castlefields_x3dsinglefield_descendants.inc}
{$I castlefields_x3dmultfield.inc}
{$I castlefields_x3dsimplemultfield.inc}
{$I castlefields_x3dsimplemultfield_descendants.inc}
{$I castlefields_x3dfieldsmanager.inc}

finalization
  FreeAndNil(FX3DFieldsManager);
end.
