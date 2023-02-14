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
  { Base node that may contain children nodes.
    Note that this descends from TAbstractChildNode,
    so it can be inserted as child of another grouping node.
    So, you can create a hierarchy of nodes with any depth. }
  TAbstractGroupingNode = class(TAbstractChildNode)
  strict private
    BeforeTraversePushedState: boolean;
  protected
    { If @true (default implementation in this class), then this really
      behaves like a grouping node. If @false, this allows everything
      to "leak out" (transform changes, VRML 1.0 state changes etc.).
      Possibility useful for VRML 1.0 extension
      https://castle-engine.io/x3d_extensions.php#section_ext_wwwinline_separate }
    function SeparateGroup: boolean; virtual;
  end;

  { Basis for all node types that have bounds specified as part of the definition. }
  IAbstractBoundedObject = interface(IX3DNode)
  ['{0EF47711-754B-4BA8-97E5-0A9601E9C542}']
  end;

  { Base node that contains children nodes, in X3D and VRML 2.0. }
  TAbstractX3DGroupingNode = class(TAbstractGroupingNode, IAbstractBoundedObject)
  strict private
    type
      TChildrenArray = array of TAbstractChildNode;
  public
    procedure CreateNode; override;

    strict private FFdRender: TSFBool;
    { This is an extension from InstantReality, see
      [http://instant-reality.com/documentation/nodetype/ChildGroup/].
      We simply ignore it for now. }
    public property FdRender: TSFBool read FFdRender;

    { Add the nodes as children.

      By default, adding the same node multiple times makes
      the node present on the list multiple times.
      This AddChildren behavior is consistent with how
      @code(FdChildren.Add(Child)) behaves,
      and consistent with how @link(RemoveChildren) works
      (it removes only a single copy in case of duplicates).
      Our engine allows duplicates on the children list ---
      you can specify them in the X3D file,
      or you can add them using @link(FdChildren).Add.

      If you specify AllowDuplicates = @false,
      then adding a node that already exists on the children list is ignored.
      This has a tiny performance hit (we need to check for existence first),
      but it is consistent with how X3D event "addChildren" should behave
      according to the spec.
      The X3D specification doesn't allow duplicates on the children list.

      This propagates the changes appropriately to the parent TCastleSceneCore,
      calling TCastleSceneCore.ChangedAll or something similar. }
    procedure AddChildren(const Children: array of TAbstractChildNode;
      const AllowDuplicates: Boolean = true); overload;
    procedure AddChildren(const Child: TAbstractChildNode;
      const AllowDuplicates: Boolean = true); overload;

    { Remove the nodes from the children list.
      Removing a node that does not exist on the children list is ignored.

      If a node exists on the current children list multiple times,
      then a single occurrence of this node is removed,
      for each occurrence of this node in the parameters list.

      This propagates the changes appropriately to the parent TCastleSceneCore,
      calling TCastleSceneCore.ChangedAll or something similar. }
    procedure RemoveChildren(const Children: array of TAbstractChildNode); overload;
    procedure RemoveChildren(const Child: TAbstractChildNode); overload;

    { Clear all children from the list.
      This automatically propagates the changes appropriately
      to the parent TCastleSceneCore. }
    procedure ClearChildren;
  end;

  { Contains children nodes without introducing a new transformation.
    It is equivalent to a TTransformNode containing an identity transform. }
  TGroupNode = class(TAbstractX3DGroupingNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    class function URNMatching(const URN: string): boolean; override;
  end;
  TGroupNode_2 = TGroupNode;

  TStringLongIntMap = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<string, LongInt>)
  strict private
    function GetItems(const AKey: string): LongInt;
    procedure SetItems(const AKey: string; const AValue: LongInt);
  public
    { Assign contents (all keys, values) of another TStringLongIntMap instance. }
    procedure Assign(const Source: TStringLongIntMap);

    { Access dictionary items.
      Setting this is allowed regardless if the key previously existed or not,
      in other words: setting this does AddOrSetValue, contrary to the ancestor TDictionary
      that only allows setting when the key already exists. }
    property Items [const AKey: string]: LongInt read GetItems write SetItems; default;
  end;

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
  TX3DRootNode = class(TGroupNode)
  strict private
    FHasForceVersion: boolean;
    FSaveAsRootNode: boolean;
    FProfile: string;
    FComponents: TStringLongIntMap;
    FScale: Single;
  private
    procedure TopLevelCheckChild(const Child: TX3DNode);
  protected
    function DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;

    { Root node never saves/restores the traversing state.
      This means that all state changes "leak out" from a root node.
      This is a good thing: if root node isn't used as a final TCastleSceneCore.RootNode,
      then it is placed inside Inline node, which will do save/restore anyway.
      And this way VRML 1.0 WWWInline with separate=FALSE may also work too. }
    function SeparateGroup: boolean; override;
  public
    ForceVersion: TX3DVersion;

    procedure CreateNode; override;
    destructor Destroy; override;

    { Should SaveToStream take care to nicely save us, treating as
      a root node of whole VRML file or prototype.

      If this node isn't a root node, you should set this to @false
      (or saving this node may generate dumb contents).
      Although, generally, you should avoid using the TX3DRootNode
      class at all for non-root nodes. }
    property SaveAsRootNode: boolean read FSaveAsRootNode write FSaveAsRootNode
      default true;

    { Save contents to the stream.

      If SaveAsRootNode then this is saved in a special way,
      such that only the contents are written, without surrounding
      braces (for classic encoding) or XML element (for xml encoding).
      This way, when saving, we hide the fact that everything was wrapped
      in an artificial TX3DRootNode. }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Set HasForceVersion to @true to force saving this model with
      given ForceVersion. }
    property HasForceVersion: boolean
      read FHasForceVersion write FHasForceVersion default false;

    { Profile required for this X3D file. See X3D spec about profiles.
      Every X3D file must always define a profile.
      (However, we use this class also for VRML 1.0 and 2.0 root nodes,
      where profile is empty.)
      Relevant only if this node is the root of X3D file. }
    property Profile: string read FProfile write FProfile;

    { Components and levels required for this X3D file.
      Relevant if this node is the root of X3D file. }
    property Components: TStringLongIntMap read FComponents;

    { Set properties to force saving this node graph as X3D.
      If we're already configured to save as X3D (major version >= 3)
      then do nothing. Otherwise, sets major/minor versions
      (by default: for X3D 3.2; make sure AMajor is >= 3)
      and sets X3D profile (default: Interchange). }
    procedure ForceSaveAsX3D(const AMajor: Integer = 3;
      const AMinor: Integer = 2;
      const AProfile: string = 'Interchange');

    { Scale of the model.
      When loading, this is set to honor @code("UNIT length ...")
      declaration in X3D 3.3.
      It scales model such that 1 unit = 1 meter. When one model is inlined
      inside another (e.g. using X3D Inline node), this is adjusted to
      the existing scale of the outer model (always to make the resulting
      complete node graph keep the assumption 1 unit = 1 meter). }
    property Scale: Single read FScale write FScale default 1.0;

    { Export node without using X3D parser. Use if you build X3D graph by hand,
      and you want do add the EXPORT X3D clause (see X3D specification about EXPORT
      to know what it does), such that it's not only serializable, but also
      works (is recognized by models that Inline this one).

      @groupBegin }
    procedure ManuallyExportNode(const Node: TX3DNode; const ExportedAlias: string); overload;
    procedure ManuallyExportNode(const Node: TX3DNode); overload;
    { @groupEnd }
  end;

  { Children nodes which cannot be modified.
    StaticGroup children are guaranteed to not change,
    send events, receive events or contain any USE
    references outside the StaticGroup.
    This allows to optimize this content for faster rendering and less memory usage. }
  TStaticGroupNode = class(TAbstractGroupingNode, IAbstractBoundedObject)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  { Interface for all VRML 2.0 / X3D transformation nodes.
    These nodes transform all their children (the ones enumerated
    by traversing, so all active children).

    The transformation change may be signalled by TX3DField.Changes
    including chTransform flag. For some special nodes, like Billboard,
    the transformation changes are automatically managed. }
  ITransformNode = interface(IX3DNode)
  ['{6991EC7D-0821-4393-B5B3-08DD967CFE05}']
  end;

  { Grouping node that transforms (moves, rotates, scales) it's children. }
  TTransformNode = class(TAbstractX3DGroupingNode, ITransformNode)
  strict private
    //WarningNegativeScaleDone: boolean;
  protected
    { }
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function TransformationChange: TNodeTransformationChange; override;
  end;
  TTransformNode_2 = TTransformNode;

{$endif read_interface}

{$ifdef read_implementation}

function TAbstractGroupingNode.SeparateGroup: boolean;
begin
  Result := true;
end;

procedure TAbstractX3DGroupingNode.CreateNode;
begin
  inherited;

  FFdRender := TSFBool.Create(Self, true, 'render', true);
  AddField(FFdRender);

  DefaultContainerField := 'children';
end;

procedure TAbstractX3DGroupingNode.AddChildren(
  const Children: array of TAbstractChildNode;
  const AllowDuplicates: Boolean);
begin
  { ignore empty list, in particular from ToChildrenArray failure }
  if High(Children) = -1 then Exit;

  { Note: it would be slightly more correct to implement this inside
    EventAddChildrenReceive, and from AddChildren only create an event
    "addChildren". This would allow other receivers of EventAddChildren
    to react to this.

    But it is not necessary in practice,
    and it would be a little wasteful (creating a temporary TMFNode to send
    for each AddChildren call). So we don't do it now. }

//  if Scene <> nil then
//    Scene.ChangedAll;
end;

procedure TAbstractX3DGroupingNode.RemoveChildren(
  const Children: array of TAbstractChildNode);
var
  N: TAbstractChildNode;
begin
  { ignore empty list, in particular from ToChildrenArray failure }
  if High(Children) = -1 then Exit;
end;

procedure TAbstractX3DGroupingNode.AddChildren(
  const Child: TAbstractChildNode;
  const AllowDuplicates: Boolean);
begin
  AddChildren([Child], AllowDuplicates);
end;

procedure TAbstractX3DGroupingNode.RemoveChildren(
  const Child: TAbstractChildNode);
begin
  RemoveChildren([Child]);
end;

procedure TAbstractX3DGroupingNode.ClearChildren;
begin
end;

procedure TGroupNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TGroupNode.ClassX3DType: string;
begin
  Result := 'Group';
end;

class function TGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = ClassX3DType) or
    (URN = ClassX3DType);
end;

class function TGroupNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TGroupNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;
end;

{ TStringLongIntMap ---------------------------------------------------------- }

procedure TStringLongIntMap.Assign(const Source: TStringLongIntMap);
var
  Pair: TDictionaryPair;
begin
  Clear;
  for Pair in Source do
    Items[Pair.Key] := Pair.Value;
end;

function TStringLongIntMap.GetItems(const AKey: string): LongInt;
begin
  Result := inherited Items[AKey];
end;

procedure TStringLongIntMap.SetItems(const AKey: string; const AValue: LongInt);
begin
  AddOrSetValue(AKey, AValue);
end;

{ TX3DRootNode --------------------------------------------------------------- }

procedure TX3DRootNode.CreateNode;
begin
  inherited;
  FComponents := TStringLongIntMap.Create;
  FSaveAsRootNode := true;
  FScale := 1;
end;

destructor TX3DRootNode.Destroy;
begin
  FreeAndNil(FComponents);
  inherited;
end;

procedure TX3DRootNode.SaveToStream(Writer: TX3DWriter);
var
  I: integer;
  FileItems: TX3DFileItemList;
begin
  if SaveAsRootNode then
  begin
    { Special things for root node saving:
      - Name is ignored (should be '').
      - Fields are ignored, except "children", that holds children
        (for any VRML version, including VRML 1).
        It's output in a special way, without writing "children [...]" around.
        Other fields are ignored (should never have any meaning). }

    FileItems := TX3DFileItemList.Create(false);
    try
      Assert(VRML1ChildrenCount = 0);

      FileItems.SaveToStream(Writer);
    finally FreeAndNil(FileItems) end;
  end else
    inherited;
end;

function TX3DRootNode.DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode;
var
  Res: TX3DRootNode;
begin
  Result := inherited;

  Res := Result as TX3DRootNode;

  Res.HasForceVersion := HasForceVersion;
  Res.ForceVersion := ForceVersion;
  Res.Scale := Scale;
  Res.Profile := Profile;
  Res.Components.Assign(Components);
end;

procedure TX3DRootNode.ForceSaveAsX3D(const AMajor: Integer = 3;
  const AMinor: Integer = 2;
  const AProfile: string = 'Interchange');
begin
  if (not HasForceVersion) or
     (ForceVersion.Major < 3) then
  begin
    HasForceVersion := true;
    ForceVersion.Major := AMajor;
    ForceVersion.Minor := AMinor;
    Profile := AProfile;
  end;
end;

function TX3DRootNode.SeparateGroup: boolean;
begin
  Result := false;
end;

procedure TX3DRootNode.TopLevelCheckChild(const Child: TX3DNode);
begin
  { We could just do here FdChildren.WarningIfChildNotAllowed(Child),
    but the warning message of it is a little unfriendly: it uncovers
    to user that we have a special TX3DRootNode that descends from Group node.
    So instead, do it by hand.

    Also, add LayerSet (not allowed in Group.FdChildren).
    This is special (see http://www.web3d.org/x3d/specifications/ISO-IEC-19775-1.2-X3D-AbstractSpecification/Part01/concepts.html#Rootnodes),
    "4.3.2 Root nodes" from X3D 3.2 spec.
  }
end;

procedure TX3DRootNode.ManuallyExportNode(const Node: TX3DNode; const ExportedAlias: string);
begin
  if Node.X3DName = '' then
    raise Exception.Create('You cannot export nodes with empty names');
end;

procedure TX3DRootNode.ManuallyExportNode(const Node: TX3DNode);
begin
  ManuallyExportNode(Node, Node.X3DName);
end;

procedure TStaticGroupNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TStaticGroupNode.ClassX3DType: string;
begin
  Result := 'StaticGroup';
end;

class function TStaticGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = ClassX3DType);
end;

function TStaticGroupNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
end;

procedure TTransformNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TTransformNode.ClassX3DType: string;
begin
  Result := 'Transform';
end;

class function TTransformNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = ClassX3DType) or
    (URN = ClassX3DType);
end;

class function TTransformNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TTransformNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
end;

function TTransformNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

{$endif read_implementation}
