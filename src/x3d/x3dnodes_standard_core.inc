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

  { Base X3D node that can have some metadata.

    Almost all X3D nodes inherit from this, with the only exception being
    the X3DMetadataObject (expressed only as an interface in Pascal,
    @link(IAbstractMetadataNode)).
    This means that you cannot have metadata inside a metadata. }
  TAbstractNode = class(TX3DNode)
  public
    procedure CreateNode; override;

    strict private FFdMetadata: TSFNode;
    public property FdMetadata: TSFNode read FFdMetadata;
  end;

  { Abstract node type that indicates that the node may be used as
    a child of a grouping node,
    e.g. inside @link(TAbstractX3DGroupingNode.FdChildren). }
  TAbstractChildNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TAbstractNode.CreateNode;
begin
  inherited;

  FFdMetadata := TSFNode.Create(Self, true, 'metadata');
  AddField(FFdMetadata);
end;

procedure TAbstractChildNode.CreateNode;
begin
  inherited;

  { It's natural that X3DChildNode goes into "children" node of parent.
    Actually, this makes many other
      DefaultContainerField := 'children'
    lines in our x3d_*.inc include files useless. }
  DefaultContainerField := 'children';
end;

{$endif read_implementation}
