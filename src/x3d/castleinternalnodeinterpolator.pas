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

  Result := TX3DNodeClass(Model1.ClassType).Create;
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
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TAnimation ----------------------------------------------------------------- }

constructor TNodeInterpolator.TAnimation.Create;
begin
  inherited;
end;

destructor TNodeInterpolator.TAnimation.Destroy;
begin
  inherited;
end;

procedure TNodeInterpolator.TAnimation.FreeKeyNodesContents;
begin
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
end;

destructor TNodeInterpolator.TBakedAnimation.Destroy;
begin
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
  KeyNode := nil;
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
