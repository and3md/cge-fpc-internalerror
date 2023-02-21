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

uses Classes, SysUtils, Generics.Collections,
  CastleInternalX3DLexer;

{$define read_interface}

type
  {$I castlefields_x3dfileitem.inc}
  {$I castlefields_x3dfield.inc}
  {$I castlefields_x3devent.inc}
  {$I castlefields_x3devent_descendants.inc}
  {$I castlefields_x3dsinglefield_descendants.inc}

{$undef read_interface}

implementation

uses Generics.Defaults,
  X3DNodes;

{$define read_implementation}

{$I castlefields_x3dfileitem.inc}
{$I castlefields_x3dfield.inc}
{$I castlefields_x3devent.inc}
{$I castlefields_x3devent_descendants.inc}
{$I castlefields_x3dsinglefield_descendants.inc}

finalization

end.
