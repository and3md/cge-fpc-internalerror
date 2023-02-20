{
  Copyright 2006-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Powerful automatic interpolation for any X3D node hierarchies,
  able to animate a sequence of any X3D scenes. }
unit CastleInternalNodeInterpolator;

{$I ../base/castleconf.inc}

interface

uses Classes, Generics.Collections,
  X3DNodes;

type
  TGetKeyNodeWithTime = procedure (const Index: Cardinal;
    out KeyNode: TX3DRootNode; out Time: Single) of object;

  TNodeInterpolator = class
  strict private
    { Helpers for Load implementation. }
    LoadToX3D_KeyNodes: TX3DNodeList; static;
    class procedure LoadToX3D_GetKeyNodeWithTime(const Index: Cardinal;
      out KeyNode: TX3DRootNode; out Time: Single);
  public
    const
      DefaultScenesPerTime = 30;
      DefaultEpsilon = 0.001;
      DefaultAnimationName = 'animation';

    type
      { Key X3D nodes read from castle-anim-frames. }
      TAnimation = class
        { The nodes and associated times read from castle-anim-frames.
          The list objects are owned (created, destroyed) by this TAnimation instance.
          The nodes inside KeyNodes are not -- it's the caller responsibility
          to free or pass them as appropriate (or just call
          @link(FreeKeyNodesContents) to easily free them).
          @groupBegin }
        KeyNodes: TX3DNodeList;
        { @groupEnd }
        Name: string;
        ScenesPerTime: Cardinal;
        Epsilon: Single;
        Loop, Backwards: boolean;

        constructor Create;
        destructor Destroy; override;
        procedure FreeKeyNodesContents;
      end;

      TAnimationList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TAnimation>)
        { Call TAnimation.FreeKeyNodesContents on all the items. }
        procedure FreeKeyNodesContents;
      end;

      { X3D nodes series that make an animation.
        These are like TAnimation, but sometimes with additional
        interpolated nodes added in between. }
      TBakedAnimation = class
        { The nodes for every time step.
          This is usually more than just "key" nodes, it's filled with intermediate
          interpolated nodes created by BakeToSequence.

          Just like TAnimation.KeyNodes, the nodes list instance is owned by
          this TBakedAnimation instance, but it's contents (actual X3D nodes) is not.
          That is, Nodes.OwnsObjects is @false.
          Make sure to free (or pass along) the nodes as necessary.
          In the simplest case, you can call FreeNodesContents,
          but usually you will pass the nodes furher to LoadToX3D.

          This list is never empty after BakeToSequence call,
          as you cannot create a sequence from an empty list.
          The list contains only TX3DRootNode instances.
        }
        Nodes: TX3DNodeList;

        TimeBegin, TimeEnd: Single;
        Name: string;
        Loop, Backwards: boolean;

        constructor Create;
        destructor Destroy; override;
        procedure FreeNodesContents;

        { Animation duration in seconds. Just TimeEnd - TimeBegin. }
        function Duration: Single;
      end;

      TBakedAnimationList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TBakedAnimation>)
        procedure FreeNodesContents;
      end;

    { Load animation data from castle-anim-frames file (in a given URL) to a set of variables.
      See [https://castle-engine.io/castle_animation_frames.php]
      for a specification of the file format.

      If you want a comfortable way to load castle-anim-frames from a file,
      you will usually just use the @link(TCastleScene.Load) method.
      The @name is more flexible --- it returns the nodes list,
      which allows you to modify the animation before passing it to @link(LoadToX3D)
      or other methods.

      Returns a TAnimationList instance.
      It's the caller responsibility to free this instance (along with it's contents;
      the list items will be freed automatically with the list,
      as the list has OwnsObjects = true;
      but you must manually take care to free (or pass elsewhere)
      the TAnimation.KeyNodes contents, or just call @link(TAnimationList.FreeKeyNodesContents).)
    }
    class function LoadAnimFramesToKeyNodes(const URL: string): TAnimationList;

    { From key nodes, create a series of baked nodes (with final
      animation pose already calculated) representing an animation.

      KeyNodesCount must always be at least one,
      you cannot create animation from an empty list.

      The returned animation must be freed by the caller.
      You should take care to free the contained @link(TBakedAnimation.Nodes)
      (they are @italic(not) automatically freed by freeing TBakedAnimation).

      It always contains the "key" nodes returned by GetKeyNodeWithTime,
      so there's no need to free the "key" nodes if you already take care of freeing
      the resulting nodes.

      We do not set the Name, Loop, Backwards, BoundingBox properties of the TBakedAnimation.
      Caller should set them, to finalize the initialization of TBakedAnimation. }
    class function BakeToSequence(const GetKeyNodeWithTime: TGetKeyNodeWithTime;
      const KeyNodesCount: Cardinal;
      ScenesPerTime: Cardinal;
      const Epsilon: Single): TBakedAnimation;

    { Load animations (read by @link(LoadAnimFramesToKeyNodes) to a series
      of key nodes) into a X3D animation.

      This is a combined BakeToSequence (that interpolates and creates
      intermediate nodes from a list of key nodes) and LoadSequenceToX3D
      (that converts a sequence of nodes into an animation, using TimeSensor,
      Switch etc.)

      After calling this, the "key" nodes inside the @link(TAnimation.KeyNodes)
      become owned by the resulting large X3D node. So there's no need to worry
      about freeing them anymore, just make sure to free the resulting large node.
      @bold(If this exits with exception, the @link(TAnimation.KeyNodes) are already freed.)
      This is weird, but necessary (otherwise we would have to suffer memory leaks,
      as internal BakeToSequence could already create some intermediate nodes).
      So, after calling this method, you should only free the TAnimationList instance. }
    class function LoadToX3D(const Animations: TAnimationList): TX3DRootNode;
  end;

implementation

uses SysUtils, XMLRead, DOM, Math,
  X3DFields;

{ EModelsStructureDifferent -------------------------------------------------- }

type
  EModelsStructureDifferent = class(Exception)
    constructor CreateFmt(const S: string; const Args: array of const);
  end;

constructor EModelsStructureDifferent.CreateFmt(const S: string;
  const Args: array of const);
begin
  inherited CreateFmt('Models are structurally different: ' + S, Args);
end;

{ utilities for TNodeInterpolator.BakeToSequence ----------------------------- }

{ Check that Model1 and Model2 are exactly equal,
  or that at least interpolating (see NodesLerp) is possible.

  If models are structurally different (which means that even
  interpolating between Model1 and Model2 is not possible),
  it will raise EModelsStructureDifferent. }
procedure CheckNodesStructurallyEqual(Model1, Model2: TX3DNode;
  const Epsilon: Single);

  procedure CheckSFNodesStructurallyEqual(Field1, Field2: TSFNode);
  begin
    if Field1.WeakLink and Field2.WeakLink then
    begin
      { assume equal }
    end else
    if (Field1.Value <> nil) and (not Field1.WeakLink) and
       (Field2.Value <> nil) and (not Field2.WeakLink) then
    begin
      CheckNodesStructurallyEqual(Field1.Value, Field2.Value, Epsilon);
    end else
    if not ((Field1.Value = nil) and (Field2.Value = nil)) then
      raise EModelsStructureDifferent.CreateFmt('Field "%s" of type SFNode ' +
        'is once NULL and once not-NULL', ['Field1.X3DName']);
  end;

var
  I: Integer;
begin
  { Yes, Model1 and Model2 must have *exactly* the same classes. }
  if Model1.ClassType <> Model2.ClassType then
    raise EModelsStructureDifferent.CreateFmt(
      'Different nodes classes: "%s" and "%s"',
      [Model1.ClassName, Model2.ClassName]);

  if Model1.X3DName <> Model2.X3DName then
    raise EModelsStructureDifferent.CreateFmt(
      'Different names of nodes: "%s" and "%s"',
      [Model1.X3DName, Model2.X3DName]);

  { We are interested whether Model1.BaseUrl and Model2.BaseUrl will
    give different results when using them to resolve relative URLs.
    Simply comparing them is not good --- they may contain filenames
    at the end. Stripping these filenames with ExtractURIPath
    is dirty. So we just test CombineURI with a test name. }
  if Model1.PathFromBaseUrl('test') <> Model2.PathFromBaseUrl('test') then
    raise EModelsStructureDifferent.CreateFmt(
      'BaseUrl of nodes different (will resolve relative URLs to different things): "%s" and "%s"',
      [Model1.BaseUrl, Model2.BaseUrl]);

  if Model1.VRML1ChildrenCount <> Model2.VRML1ChildrenCount then
    raise EModelsStructureDifferent.CreateFmt(
      'Different number of Inventor / VRML 1.0 children in nodes: %d vs %d',
      [Model1.VRML1ChildrenCount, Model2.VRML1ChildrenCount]);

  for I := 0 to Model1.VRML1ChildrenCount - 1 do
    CheckNodesStructurallyEqual(Model1.VRML1Children[I], Model2.VRML1Children[I], Epsilon);

  { Yes, the situation below can happen. *Usually* when we know
    that Model1 and Model2 are equal classes then we know that
    they have the same number of fields of the same type.
    However, for TX3DUnknownNode, it's not that easy. Two different instances
    of TX3DUnknownNode class may have completely different fields,
    so we must safeguard against this. }
  if Model1.FieldsCount <> Model2.FieldsCount then
    raise EModelsStructureDifferent.CreateFmt(
      'Different number of fields in nodes: "%d" and "%d"',
      [Model1.FieldsCount, Model2.FieldsCount]);

  for I := 0 to Model1.FieldsCount - 1 do
  begin
    if Model1.Fields[I].ClassType <> Model2.Fields[I].ClassType then
      raise EModelsStructureDifferent.CreateFmt(
        'Different type of field number %d in nodes: "%s" and "%s"',
        [I, Model1.Fields[I].ClassName, Model2.Fields[I].ClassName]);

    if Model1.Fields[I] is TSFNode then
      CheckSFNodesStructurallyEqual(
        TSFNode(Model1.Fields[I]), TSFNode(Model2.Fields[I])) else
    if Model1.Fields[I].CanAssignLerp then
    begin
      { Else we have single-value field that can lerp.
        No need to check anything in this case,
        it's ready to go (that is, to lerp). }
    end else
    begin
      { Check fields for equality.

        Some special fields like TInlineNode.FdUrl do not
        have to be equal, as they don't have any role for the
        "real" meaning of the model. I mean, if TInlineNode.Inlined
        contents (loaded from pointed file) have the same structure,
        then we're happy. And it's handy to allow this --- see e.g.
        examples/models/gus_1_final.wrl and
        examples/models/gus_2_final.wrl trick. }
    end;
  end;
end;

{ Merge equal children of Model1 and Model2,
  and check that Model1 and Model2 are exactly equal.

  It assumes that models are structurally equal, i.e. that you
  already did run CheckNodesStructurallyEqual over them.

  It works recursively: first it checks for every children
  are they equal. For each pair that is equal, it replaces
  given children in Model2 with appropriate children of Model1.
  At the end, if every children pair was equal and additionally
  if all fields are equal, then it returns true.

  Such copying of references is useful, because then we simply copy given
  node's reference instead of duplicating this object.
  This way Model1 and Model2 and all models interpolated along the way
  can share the same object reference. This is very good, because:

  1. If nodes are equal then creating new object each
     time would mean that I create a lot of objects with exactly the
     same contents. So memory is wasted, without any good reason.

  2. For nodes like ImageTexture, this is good because then the image
     is loaded from the file only once. This means that memory is saved,
     once again. This also means that in case when texture file doesn't
     exist, user gets only 1 warning/error message (instead of getting
     warning/error message for each duplicated TImageTextureNode instance).

  3. Also for nodes like ImageTexture, this means that if we use the same
     GLRenderer to render every model of the animation,
     then GLRenderer will recognize this and given texture
     will be loaded only once for OpenGL. So loading time and
     memory are saved *once again*  (otherwise OpenGL would allocate
     internal copy of texture for each duplicated node, once again
     wasting a lot of memory).

     Although 2. and 3. are actually somewhat void right now,
     as we have a more general cache that caches texture resources
     even across different nodes right now (the only need is to have
     equal URLs).

  4. And later the Shape cache of TGLRenderer can speed
     up loading time and conserve memory use, if it sees the same
     reference to given GeometryNode twice. }
function NodesMerge(Model1, Model2: TX3DNode;
  const Epsilon: Single): boolean;

  function SFNodesMerge(Field1, Field2: TSFNode): boolean;
  begin
    Result := true;

    { Equality was already checked by CheckNodesStructurallyEqual,
      so now if one SFNode value is not nil, we know that the other
      one is not nil too. }
    if (Field1.Value <> nil) and
      (not Field1.WeakLink) and
      (not Field2.WeakLink) then
    begin
      if NodesMerge(Field1.Value, Field2.Value, Epsilon) then
        Field1.Value := Field2.Value else
        Result := false;
    end;
  end;

var
  I: Integer;
begin
  Result := true;

  { Note that this loop will iterate over every Children,
    even if somewhere along the way we will already set Result to false.
    Even if we already know that Result is false, we stil want to
    merge Model1 and Model2 children as much as we can. }
  for I := 0 to Model1.VRML1ChildrenCount - 1 do
  begin
    if NodesMerge(Model1.VRML1Children[I], Model2.VRML1Children[I], Epsilon) then
    begin
      { Tests: Writeln('merged child ', I, ' of class ',
        Model1.VRML1Children[I].X3DType); }
      Model1.VRML1Children[I] := Model2.VRML1Children[I];
    end else
      Result := false;
  end;

  if not Result then Exit;

  for I := 0 to Model1.FieldsCount - 1 do
  begin
    if Model1.Fields[I] is TSFNode then
    begin
      if not SFNodesMerge(TSFNode(Model1.Fields[I]),
                          TSFNode(Model2.Fields[I])) then
        Result := false;
    end else
    if Model1.Fields[I].CanAssignLerp then
    begin
      if not Model1.Fields[I].Equals(Model2.Fields[I]
        { TODO: ignored for now, and maybe for ever: , Epsilon }) then
        Result := false;
    end;

    { Other fields were already checked by CheckNodesStructurallyEqual }
  end;
end;

{ Linear interpolation between Model1 and Model2.
  A = 0 means Model1, A = 1 means Model2, A between 0 and 1 is lerp
  between Model1 and Model2.

  If Model1 and Model2 are the same object (the same references),
  then this will return just Model1. This way it keeps memory optimization
  described by NodesMerge. This is also true if both Model1 and Model2
  are nil: then you can safely call this and it will return also nil. }
function NodesLerp(const A: Single; Model1, Model2: TX3DNode): TX3DNode;

  procedure SFNodeLerp(Target, Field1, Field2: TSFNode);
  begin
    if (not Field1.WeakLink) and
       (not Field2.WeakLink) then
      Target.Value := NodesLerp(A, Field1.Value, Field2.Value)
    else
      Target.Value := Field1.Value;
  end;

var
  I: Integer;
begin
  if Model1 = Model2 then
    Exit(Model1);

  Result := TX3DNodeClass(Model1.ClassType).Create(Model1.X3DName,
    Model1.BaseUrl);
  try
    if Result is TX3DRootNode then
    begin
      { copy TX3DRootNode special fields, like TX3DRootNode.DeepCopyCore.
        This is necessary for WrapRootNode working Ok lower in this file. }
      TX3DRootNode(Result).HasForceVersion := (Model1 as TX3DRootNode).HasForceVersion;
      TX3DRootNode(Result).ForceVersion := (Model1 as TX3DRootNode).ForceVersion;
      TX3DRootNode(Result).Scale := (Model1 as TX3DRootNode).Scale;
      TX3DRootNode(Result).Profile := (Model1 as TX3DRootNode).Profile;
      TX3DRootNode(Result).Components.Assign((Model1 as TX3DRootNode).Components);
    end;

    { TODO: the code below doesn't deal efficiently with the situation when single
      TX3DNode is used as a child many times in one of the nodes.
      (through "USE" keyword). Code below will then unnecessarily
      create copies of such things (wasting construction time and memory),
      instead of reusing the same object reference. }
    for I := 0 to Model1.VRML1ChildrenCount - 1 do
      Result.VRML1ChildAdd(NodesLerp(A, Model1.VRML1Children[I], Model2.VRML1Children[I]));

    { TODO: for TX3DUnknownNode, we should fill here Result.Fields.
      Also for TX3DPrototypeNode. }

    for I := 0 to Model1.FieldsCount - 1 do
    begin
      if Model1.Fields[I] is TSFNode then
      begin
        SFNodeLerp(
          (Result.Fields[I] as TSFNode),
          (Model1.Fields[I] as TSFNode),
          (Model2.Fields[I] as TSFNode));
      end else
      if Model1.Fields[I].CanAssignLerp then
      begin
        Result.Fields[I].AssignLerp(A, Model1.Fields[I], Model2.Fields[I]);
      end else
      begin
        { These fields cannot be interpolated.
          So just copy to Result.Fields[I]. }
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TAnimation ----------------------------------------------------------------- }

constructor TNodeInterpolator.TAnimation.Create;
begin
  inherited;
  KeyNodes := TX3DNodeList.Create(false);
end;

destructor TNodeInterpolator.TAnimation.Destroy;
begin
  FreeAndNil(KeyNodes);
  inherited;
end;

procedure TNodeInterpolator.TAnimation.FreeKeyNodesContents;
var
  I: Integer;
begin
  for I := 0 to KeyNodes.Count - 1 do
  begin
    KeyNodes[I].Free;
    KeyNodes[I] := nil;
  end;
  KeyNodes.Clear;
end;

{ TAnimationList ------------------------------------------------------------- }

procedure TNodeInterpolator.TAnimationList.FreeKeyNodesContents;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FreeKeyNodesContents;
end;

{ TBakedAnimation ----------------------------------------------------------------- }

constructor TNodeInterpolator.TBakedAnimation.Create;
begin
  inherited;
  Nodes := TX3DNodeList.Create(false);
end;

destructor TNodeInterpolator.TBakedAnimation.Destroy;
begin
  FreeAndNil(Nodes);
  inherited;
end;

function TNodeInterpolator.TBakedAnimation.Duration: Single;
begin
  Result := TimeEnd - TimeBegin;
end;

procedure TNodeInterpolator.TBakedAnimation.FreeNodesContents;
var
  I: Integer;
begin
  for I := 0 to Nodes.Count - 1 do
  begin
    Nodes[I].Free;
    Nodes[I] := nil;
  end;
  Nodes.Clear;
end;

{ TBakedAnimationList ------------------------------------------------------------- }

procedure TNodeInterpolator.TBakedAnimationList.FreeNodesContents;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FreeNodesContents;
end;

{ TNodeInterpolator ---------------------------------------------------------- }

class function TNodeInterpolator.BakeToSequence(
  const GetKeyNodeWithTime: TGetKeyNodeWithTime;
  const KeyNodesCount: Cardinal;
  ScenesPerTime: Cardinal;
  const Epsilon: Single): TBakedAnimation;
var
  I: Integer;
  StructurallyEqual, KeyNodesEqual: boolean;
  LastNodesIndex: Integer;
  LastKeyNode, NewKeyNode: TX3DRootNode;
  LastTime, NewTime: Single;
  NodesIndex: Integer;
  Nodes: TX3DNodeList;
begin
  ScenesPerTime := Round(ScenesPerTime * 1);

  Result := TBakedAnimation.Create;
  try
    Assert(KeyNodesCount > 0);

    { KeyNodes[0] goes to Nodes[0], that's easy }
    GetKeyNodeWithTime(0, NewKeyNode, NewTime);

    Nodes := Result.Nodes;

    Nodes.Count := 1;
    Nodes[0] := NewKeyNode;
    LastNodesIndex := 0;
    LastTime := NewTime;
    LastKeyNode := NewKeyNode;

    { calculate TimeBegin at this point }
    Result.TimeBegin := NewTime;

    for I := 1 to KeyNodesCount - 1 do
    begin
      { Now add KeyNodes[I] }
      GetKeyNodeWithTime(I, NewKeyNode, NewTime);

      StructurallyEqual := false;

      try
        CheckNodesStructurallyEqual(LastKeyNode, NewKeyNode, Epsilon);
        StructurallyEqual := true;
      except
        on E: EModelsStructureDifferent do
        begin
          ;
        end;
      end;

      Nodes.Count := Nodes.Count +
        Max(1, Round((NewTime - LastTime) * ScenesPerTime));

      if StructurallyEqual then
      begin
        { Try to merge it with LastKeyNode.
          Then initialize Nodes[LastNodesIndex + 1 to Nodes.Count - 1]. }
        KeyNodesEqual := NodesMerge(NewKeyNode, LastKeyNode, Epsilon);
        if KeyNodesEqual then
        begin
          { In this case don't waste memory, simply reuse Nodes[LastNodesIndex]. }
          FreeAndNil(NewKeyNode);
          for NodesIndex := LastNodesIndex + 1 to Nodes.Count - 1 do
            Nodes[NodesIndex] := Nodes[LastNodesIndex];
        end else
        begin
          ;
        end;
      end else
      begin
        { We cannot interpolate between last and new node.
          So just duplicate last node until Nodes.Count - 2,
          and at Nodes.Last insert new node. }
        for NodesIndex := LastNodesIndex + 1 to Nodes.Count - 2 do
          Nodes[NodesIndex] := Nodes[LastNodesIndex];
        Nodes[Nodes.Count - 1] := NewKeyNode;
        LastKeyNode := NewKeyNode;
      end;

      LastTime := NewTime;
      LastNodesIndex := Nodes.Count - 1;
    end;

    { calculate TimeEnd at this point }
    Result.TimeEnd := NewTime;

  except FreeAndNil(Result); raise end;
end;

class function TNodeInterpolator.LoadAnimFramesToKeyNodes(const URL: string): TAnimationList;

  { Load <animation> data from a given XML element to a set of variables.

    @param(BaseUrl The URL from which relative
      URLs inside Element will be resolved. It doesn't
      have to be absolute, we will expand it to make it absolute
      if necessary.) }
  function LoadOneAnimation(Element: TDOMElement): TAnimation;
  const
    DefaultLoop = false;
    DefaultBackwards = false;
  var
    I: Integer;
    Attr: TDOMAttr;
  begin
    Result := TAnimation.Create;
    try
      { Assign default values for optional attributes }
      Result.Name := DefaultAnimationName;
      Result.ScenesPerTime := DefaultScenesPerTime;
      Result.Epsilon := DefaultEpsilon;
      Result.Loop := DefaultLoop;
      Result.Backwards := DefaultBackwards;

      for I := 0 to Integer(Element.Attributes.Length) - 1 do
      begin
        Attr := Element.Attributes.Item[I] as TDOMAttr;
        if Attr.Name = 'name' then
          Result.Name := Attr.NodeValue
        else
        if Attr.Name = 'scenes_per_time' then
          Result.ScenesPerTime := StrToInt(Attr.NodeValue)
        else
        if Attr.Name = 'optimization' then
          { ignore, for backward compatibility }
        else
        if Attr.Name = 'equality_epsilon' then
          Result.Epsilon := StrToFloat(Attr.NodeValue)
        else
        if Attr.Name = 'loop' then
          Result.Loop := StrToBool(Attr.NodeValue)
        else
        if Attr.Name = 'backwards' then
          Result.Backwards := StrToBool(Attr.NodeValue)
        else
          raise Exception.CreateFmt('Unknown attribute of <animation> element: "%s"',
            [Attr.Name]);
      end;

      if Result.KeyNodes.Count = 0 then
        raise Exception.Create('At least one <frame> is required within <animation> element');
    except
      { in case of trouble, clear the partial KeyNodes contents }
      Result.FreeKeyNodesContents;
      FreeAndNil(Result);
      raise;
    end;
  end;

begin
    Result := TAnimationList.Create;
    try
    except
      Result.FreeKeyNodesContents;
      FreeAndNil(Result);
      raise;
    end;
end;

class procedure TNodeInterpolator.LoadToX3D_GetKeyNodeWithTime(const Index: Cardinal;
  out KeyNode: TX3DRootNode; out Time: Single);
begin
  KeyNode := LoadToX3D_KeyNodes[Index] as TX3DRootNode;
end;

class function TNodeInterpolator.LoadToX3D(const Animations: TAnimationList): TX3DRootNode;
var
  Animation: TAnimation;
  BakedAnimations: TBakedAnimationList;
  BakedAnimation: TBakedAnimation;
  I: Integer;
begin
  BakedAnimations := TBakedAnimationList.Create(true);
  try
    try
      for I := 0 to Animations.Count - 1 do
      begin
        Animation := Animations[I];
        LoadToX3D_KeyNodes := Animation.KeyNodes;
        BakedAnimation := BakeToSequence(
          {$ifdef CASTLE_OBJFPC}@{$endif} LoadToX3D_GetKeyNodeWithTime,
          Animation.KeyNodes.Count, Animation.ScenesPerTime, Animation.Epsilon);
        BakedAnimation.Name := Animation.Name;
        BakedAnimation.Loop := Animation.Loop;
        BakedAnimation.Backwards := Animation.Backwards;
        BakedAnimations.Add(BakedAnimation);
      end;
    except
      { make sure to free the TBakedAnimation.Nodes inside.
        No other way to do it, as BakeToSequence already created some intermediate nodes,
        and we have no way of returning them to the caller. }
      BakedAnimations.FreeNodesContents;
      raise;
    end;
    Result := nil;
  finally FreeAndNil(BakedAnimations) end;
end;

end.
