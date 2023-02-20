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

{$ifdef read_interface}

  { A top-level VRML/X3D node. This is what you get by loading 3D model from file.

    It is declared as a descendant of VRML/X3D >= 2.0 Group node,
    but it's used with all VRML/X3D versions (including VRML 1.0 and Inventor).
    This makes things simple (previously we had two separate TX3DRootNode_1
    and TX3DRootNode, which was complicating stuff).
    Children (for all VRML/X3D versions) are inside FdChildren field.

    This way VRML/X3D files may have many nodes at the top level
    (which is a standard feature of VRML/X3D >= 2.0, but we also allow it for
    VRML 1.0 as a commonly used extension). It may also have prototypes,
    routes etc. at the root level.

    This also allows us to record in one place some information that
    is returned by the parser. Like parsed VRML/X3D version, X3D profile,
    some namespace information (exported names and such). }
  TX3DRootNode = class(TX3DNode)
  strict private
    FHasForceVersion: boolean;
  public
    ForceVersion: TX3DVersion;

    property HasForceVersion: boolean
      read FHasForceVersion write FHasForceVersion default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
