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

  { Abstract base type for all bindable children nodes. }
  TAbstractBindableNode = class(TAbstractChildNode)
  strict private
    FBound: Boolean;
    FTransformScale: Single;
    procedure EventIsBoundReceive(Event: TX3DEvent; AValue: TX3DField);
    procedure SetBound(const Value: Boolean);
  public
    procedure CreateNode; override;

    { Event in } { }
    strict private FEventSet_bind: TSFBoolEvent;
    public property EventSet_bind: TSFBoolEvent read FEventSet_bind;

    { Event out } { }
    strict private FEventIsBound: TSFBoolEvent;
    public property EventIsBound: TSFBoolEvent read FEventIsBound;

    { "Bound" node is the node currently used, for example the "bound" background
      determines the current background look.
      Setting this to @true means that the node becomes the current node
      of the given type in the scene.

      In every scene (TCastleScene), there are four "stacks" of bindable nodes:

      @orderedList(
        @item(Viewpoints determine the camera position/orientation,
          field of view, orthographic/perspective projection etc.)
        @item(NavigationInfo determines the current navigation type,
          avatar size etc.)
        @item(Background determines the dispayed background (like a skybox
          or a simple solid color).)
        @item(Fog determines the current fog.)
      )

      The top-most node of each stack is called "bound".
      If the scene is set as TCastleSceneManager.MainScene,
      then the "bound" node determines the camera, background or fog settings.

      Setting this property to @true sends an X3D event "set_bind := true".
      This adds the node to the stack, and makes sure it is at the top.
      Setting this to @false pops the node from the stack.
      See the X3D specification about the "bindable nodes" foe details:
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/core.html#BindableChildrenNodes
    }
    property Bound: Boolean read FBound write SetBound;

    { Transformation of this bindable node.

      Bound nodes cannot be instantiated multiple number of times
      (this would make simple event like "set_bind" not possible,
      as it would not be known in what coordinate space the node is bound),
      so it's perfectly safe and comfortable to just keep their transformation
      here, a their property.

      It is gathered during traversing. Last BeforeTraverse call for this
      node sets Transform properties. By default, these represent identity
      transformation.

      Note that using TransformScale for bindable nodes like fog
      is a little simplification. Theoretically, the scale can be non-uniform,
      and around an arbitrary axis. So to apply e.g. fog, we should
      transform the 3D world back into local fog coordinate system,
      and calculate the distances there. Instead right now we do
      the opposite: transform stuff like fog VisibilityRange by
      TransformScale, and calculate distances in world coordinate
      system. This is a simplification, but in practice it's perfect
      (who uses non-uniform fog scale?) and it can be expressed for renderers
      (OpenGL) without any problems.

      @groupBegin }
    property TransformScale: Single read FTransformScale;
    { @groupEnd }

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

  { A metadata (extra, custom information at a node) with an integer type. }
  TMetadataIntegerNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;
  end;

  { A metadata (extra, custom information at a node) set,
    to reference of collection of other metadata nodes. }
  TMetadataSetNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;
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

procedure TAbstractBindableNode.CreateNode;
begin
  inherited;

  FEventSet_bind := TSFBoolEvent.Create(Self, 'set_bind', true);
  AddEvent(FEventSet_bind);

  FEventIsBound := TSFBoolEvent.Create(Self, 'isBound', false);
   EventIsBound.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventIsBoundReceive);
  AddEvent(FEventIsBound);

  FTransformScale := 1;
end;

procedure TAbstractBindableNode.EventIsBoundReceive(
  Event: TX3DEvent; AValue: TX3DField);
begin
  FBound := (AValue as TSFBool).Value;
end;

procedure TAbstractBindableNode.SetBound(const Value: Boolean);
begin
  EventSet_bind.Send(Value);
end;

procedure TAbstractSensorNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);
end;

procedure TMetadataIntegerNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  DefaultContainerField := 'metadata';
end;

class function TMetadataIntegerNode.ClassX3DType: string;
begin
  Result := 'MetadataInteger';
end;

class function TMetadataIntegerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = ClassX3DType);
end;

procedure TMetadataSetNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  DefaultContainerField := 'metadata';
end;

class function TMetadataSetNode.ClassX3DType: string;
begin
  Result := 'MetadataSet';
end;

class function TMetadataSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = ClassX3DType);
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
