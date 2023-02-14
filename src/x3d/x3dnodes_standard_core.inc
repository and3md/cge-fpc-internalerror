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

  { Base class for all nodes that contain only information without visual
    semantics. }
  TAbstractInfoNode = class(TAbstractChildNode)
  end;

  { Base abstract class for all sensors. }
  TAbstractSensorNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Event out } { }
    strict private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;
  end;


  { A metadata (extra, custom information at a node) with a string type. }
  TMetadataStringNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;

  end;

  { Information about the world.
    This node is strictly for documentation purposes
    and has no effect on the visual appearance or behaviour of the world. }
  TWorldInfoNode = class(TAbstractInfoNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdTitle: TSFString;
    public property FdTitle: TSFString read FFdTitle;
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

procedure TAbstractSensorNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);
end;

procedure TMetadataStringNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  DefaultContainerField := 'metadata';
end;

class function TMetadataStringNode.ClassX3DType: string;
begin
  Result := 'MetadataString';
end;

class function TMetadataStringNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = ClassX3DType);
end;

procedure TWorldInfoNode.CreateNode;
begin
  inherited;

  FFdTitle := TSFString.Create(Self, false, 'title', '');
  AddField(FFdTitle);
end;

class function TWorldInfoNode.ClassX3DType: string;
begin
  Result := 'WorldInfo';
end;

class function TWorldInfoNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = ClassX3DType) or
    (URN = ClassX3DType);
end;

{$endif read_implementation}
