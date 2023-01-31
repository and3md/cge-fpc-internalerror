{
  Copyright 2005-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Frustum object (TFrustum) and helpers. }
unit CastleFrustum;

{$I castleconf.inc}

interface

uses CastleVectors, CastleBoxes;

type
  { Order of planes of TFrustum.

    (This order is the same as the order of params to
    procedure FrustumProjectionMatrix and OpenGL's glFrustum routine.
    Article [http://www2.ravensoft.com/users/ggribb/plane%20extraction.pdf]
    has swapped bottom and top positions). }
  TFrustumPlane = (fpLeft, fpRight, fpBottom, fpTop, fpNear, fpFar);

  TFrustumPoints = packed array [0..7] of packed record
    case Integer of
      0: (XYZ: TVector3; W: Single);
      1: (XYZW: TVector4);
  end;

  TFrustumPointsSingle = TFrustumPoints deprecated 'use TFrustumPoints';

  TFrustumPointsDouble = packed array [0..7] of packed record
    case Integer of
      0: (XYZ: TVector3Double; W: Double);
      1: (XYZW: TVector4Double);
  end deprecated 'use Single-precision TFrustumPoints';

const
  FrustumPointsQuadsIndexes: array[TFrustumPlane, 0..3]of LongWord =
  ( (0, 3, 7, 4),
    (1, 2, 6, 5),
    (2, 3, 7, 6),
    (0, 1, 5, 4),
    (0, 1, 2, 3),
    (4, 5, 6, 7) );

  { Useful if you want to draw frustum obtained from
    TFrustum.CalculatePoints.

    It's guaranteed that the first 4 items
    touch only the first 4 (near plane) points of the frustum --- useful
    if you want to draw only the near plane rect. For ZFarInfinity,
    other points may be in infinity. }
  FrustumPointsLinesIndexes: array[0..11, 0..1]of LongWord =
  ( (0, 1), (1, 2), (2, 3), (3, 0),
    (4, 5), (5, 6), (6, 7), (7, 4),
    (0, 4), (1, 5), (2, 6), (3, 7)
  );

type
  { See @link(TFrustum.SphereCollisionPossible) for description
    what each value of this type means. }
  TFrustumCollisionPossible =
  ( fcNoCollision,
    fcSomeCollisionPossible,
    fcInsideFrustum );

  { Viewing frustum, defined as 6 plane equations.
    Calculating and operating on such frustum.
    Frustums with far plane in infinity (typically used in conjunction
    with shadow volumes) are fully supported by all methods (we just have
    5 frustum planes then).

    We define this using ObjectPascal old-style "object", to have comfort
    and low-overhead at the same time. }
  TFrustum = object
  private
    procedure NormalizePlanes;
  public
    { Calculate frustum, knowing the combined matrix (modelview * projection). }
    constructor Init(const Matrix: TMatrix4); overload;

    { Calculate frustum, knowing projection and modelview matrices.
      This is equivalent to 1-parameter Init
      with Matrix = ModelviewMatrix * ProjectionMatrix.
      This way you can get from OpenGL your two matrices (modelview
      and projection) (or you can calculate them using routines
      like @link(FrustumProjectionMatrix)), then pass them to this routine
      and you get your current viewing frustum. }
    constructor Init(const ProjectionMatrix, ModelviewMatrix: TMatrix4); overload;
  public
    { Six planes defining the frustum.
      Direction vectors of these planes must point to the inside of the frustum.
      Currently, they are always normalized.

      Note that if projection has far plane in infinity (indicated by
      ZFarInfinity) then the far plane will be invalid ---
      first three values of it's equation will be 0. }
    Planes: array [TFrustumPlane] of TVector4;

    ZFarInfinity: boolean;

    { Calculate 8 points of frustum. These points are simply
      calculated doing ThreePlanesIntersectionPoint on appropriate planes.

      Using these points you can easily draw given frustum on screen.
      Use FrustumPointsQuadsIndexes to obtain indexes to FrustumPoints.

      Note that when the far plane is in infinity,
      the 4 points of the far plane will be "in infinity",
      that is will have 4th component set to zero. Or, equivalently,
      they will be directions. Homogeneous coordinates allow us for this,
      and in fact you can just render such points without any problems
      in OpenGL.

      @raises(EPlanesParallel If Frustum doesn't have planes of any
        valid frustum.)
    }
    procedure CalculatePoints(out FrustumPoints: TFrustumPoints); overload;

    procedure CalculatePoints(out FrustumPoints: TFrustumPointsDouble); overload;
      deprecated 'use the overload on TFrustumPoints (Single-precision); it may calculate parts using Double-precision internally, to be correct';

    { Checks for collision between frustum and sphere.

      Check is done fast, but is not accurate, that's why this function's
      name contains "CollisionPossible". It returns:

      fcNoCollision when it's sure that there no collision,

      fcSomeCollisionPossible when some collision is possible,
      but nothing is sure. There *probably* is some collision,
      but it's not difficult to find some special situations where there
      is no collision but this function answers fcSomeCollisionPossible.
      There actually may be either no collision,
      or only part of sphere may be inside frustum.

      Note that it's guaranteed that if the whole sphere
      (or the whole box in case of Box3DCollisionPossible)
      is inside the frustum that fcInsideFrustum will be returned,
      not fcSomeCollisionPossible.

      fcInsideFrustum if sphere is for sure inside the frustum.

      So this function usually cannot be used for some precise collision
      detection, but it can be used for e.g. optimizing your graphic engine
      by doing frustum culling. Note that fcInsideFrustum result
      is often useful when you're comparing your frustum with
      bounding volume of some tree (e.g. octree) node: fcInsideFrustum
      tells you that not only this node collides with frustum,
      but also all it's children nodes collide for sure with frustum.
      This allows you to save some time instead of doing useless
      recursion down the tree.

      Many useful optimization ideas used in implementing this function
      were found at
      [http://www.flipcode.com/articles/article_frustumculling.shtml].

      @seealso TFrustum.Box3DCollisionPossible
    }
    function SphereCollisionPossible(
      const SphereCenter: TVector3; const SphereRadiusSqr: Single):
      TFrustumCollisionPossible;

    { Checks for collision between frustum and sphere, faster.
      Like @link(TFrustum.SphereCollisionPossible),
      but this only returns true (when TFrustum.SphereCollisionPossible
      would return fcSomeCollisionPossible or fcInsideFrustum)
      or false (when TFrustum.SphereCollisionPossible
      would return fcNoCollision).

      Consequently, it runs (very slightly) faster.
      Use this if you don't need to differentiate between
      fcSomeCollisionPossible or fcInsideFrustum cases. }
    function SphereCollisionPossibleSimple(
      const SphereCenter: TVector3; const SphereRadiusSqr: Single):
      boolean;

    { Checks for collision between frustum and box.
      Meaning of return value like @link(SphereCollisionPossible). }
    function Box3DCollisionPossible(
      const Box: TBox3D): TFrustumCollisionPossible;

    { Checks for collision between frustum and box, faster.
      Meaning of return value like @link(SphereCollisionPossibleSimple). }
    function Box3DCollisionPossibleSimple(
      const Box: TBox3D): boolean;

    function Move(const M: TVector3): TFrustum;
    procedure MoveVar(const M: TVector3);

    { Is Direction within a frustum. You can think of
      direction it as a "point infinitely away in given Direction",
      like the direction to the sun. Note that this ignores
      near/far planes of the frustum, only checking the 4 side planes. }
    function DirectionInside(const Direction: TVector3): boolean;

    { Transform frustum by a matrix.

      @param(M Transformation matrix.
        This matrix must be "sane": it must transform homogeneous positions
        into homogeneous positions, and homogeneous directions into
        homogeneous directions. Also, it must not invert or do other weird stuff
        (like shear) with directions. To be safe just make sure it's a matrix
        composed only from translations, rotations, and scale (with all
        scale parameters being > 0).)

      @raises(ETransformedResultInvalid In some cases when matrix is not sane.) }
    function Transform(const M: TMatrix4): TFrustum;

    function ToNiceStr(const Indent: string): string; deprecated 'use ToString';
    function ToString(const Indent: string): string;
  end;
  PFrustum = ^TFrustum;

implementation

constructor TFrustum.Init(const Matrix: TMatrix4);
begin
  { Based on [http://www2.ravensoft.com/users/ggribb/plane%20extraction.pdf].
    Note that position of bottom and top planes in array Frustum is swapped
    in my code. }

  Planes[fpLeft].Data[0] := Matrix.Data[0, 3] + Matrix.Data[0, 0];
  Planes[fpLeft].Data[1] := Matrix.Data[1, 3] + Matrix.Data[1, 0];
  Planes[fpLeft].Data[2] := Matrix.Data[2, 3] + Matrix.Data[2, 0];
  Planes[fpLeft].Data[3] := Matrix.Data[3, 3] + Matrix.Data[3, 0];

  Planes[fpRight].Data[0] := Matrix.Data[0, 3] - Matrix.Data[0, 0];
  Planes[fpRight].Data[1] := Matrix.Data[1, 3] - Matrix.Data[1, 0];
  Planes[fpRight].Data[2] := Matrix.Data[2, 3] - Matrix.Data[2, 0];
  Planes[fpRight].Data[3] := Matrix.Data[3, 3] - Matrix.Data[3, 0];

  Planes[fpBottom].Data[0] := Matrix.Data[0, 3] + Matrix.Data[0, 1];
  Planes[fpBottom].Data[1] := Matrix.Data[1, 3] + Matrix.Data[1, 1];
  Planes[fpBottom].Data[2] := Matrix.Data[2, 3] + Matrix.Data[2, 1];
  Planes[fpBottom].Data[3] := Matrix.Data[3, 3] + Matrix.Data[3, 1];

  Planes[fpTop].Data[0] := Matrix.Data[0, 3] - Matrix.Data[0, 1];
  Planes[fpTop].Data[1] := Matrix.Data[1, 3] - Matrix.Data[1, 1];
  Planes[fpTop].Data[2] := Matrix.Data[2, 3] - Matrix.Data[2, 1];
  Planes[fpTop].Data[3] := Matrix.Data[3, 3] - Matrix.Data[3, 1];

  Planes[fpNear].Data[0] := Matrix.Data[0, 3] + Matrix.Data[0, 2];
  Planes[fpNear].Data[1] := Matrix.Data[1, 3] + Matrix.Data[1, 2];
  Planes[fpNear].Data[2] := Matrix.Data[2, 3] + Matrix.Data[2, 2];
  Planes[fpNear].Data[3] := Matrix.Data[3, 3] + Matrix.Data[3, 2];

  Planes[fpFar].Data[0] := Matrix.Data[0, 3] - Matrix.Data[0, 2];
  Planes[fpFar].Data[1] := Matrix.Data[1, 3] - Matrix.Data[1, 2];
  Planes[fpFar].Data[2] := Matrix.Data[2, 3] - Matrix.Data[2, 2];
  Planes[fpFar].Data[3] := Matrix.Data[3, 3] - Matrix.Data[3, 2];

  { If Planes[fpFar] has really exactly zero vector,
    then far plane is in infinity. }
  ZFarInfinity :=
    (Planes[fpFar].Data[0] = 0) and
    (Planes[fpFar].Data[1] = 0) and
    (Planes[fpFar].Data[2] = 0);

  NormalizePlanes;
end;

procedure TFrustum.NormalizePlanes;
var
  fp, LastPlane: TFrustumPlane;
begin
  LastPlane := High(fp);
  if ZFarInfinity then
    LastPlane := Pred(LastPlane);

  for fp := Low(fp) to LastPlane do
  begin
    NormalizePlaneVar(Planes[fp]);

    { Previously we used this hack:

      We know that every plane Planes[fp] is correct, i.e. it's direction
      vector has non-zero length. But sometimes algorithm above calculates
      such vector with very small length, especially for fpFar plane.
      This causes problems when I'm later processing this plane,
      errors cumulate and suddenly something thinks that it has
      a zero-vector, while actually it is (or was) a vector with
      very small (but non-zero) length.

      I could do here
        NormalizePlaneVar(Planes[fp]);
      instead, but that would be slow (NormalizePlaneVar costs me
      calculating 1 Sqrt).
    if PVector3(@Planes[fp].LengthSqr^) < 0.001 then
      Planes[fp] := Planes[fp] * 100000;
    }
  end;
end;

constructor TFrustum.Init(
  const ProjectionMatrix, ModelviewMatrix: TMatrix4);
begin
  Init(ProjectionMatrix * ModelviewMatrix);
end;

procedure TFrustum.CalculatePoints(out FrustumPoints: TFrustumPointsSingle);
{ It's better to make these calculations using Double precision. }
// This deliberately uses TFrustumPointsDouble
// and CalculatePoints based on TFrustumPointsDouble.
// They should be internal (private) in this unit in the future.
{$warnings off}
var
  FrustumPointsDouble: TFrustumPointsDouble;
  I: Integer;
begin
  CalculatePoints(FrustumPointsDouble);
  for I := 0 to High(FrustumPoints) do
    FrustumPoints[I].XYZW := Vector4(FrustumPointsDouble[I].XYZW);
end;
{$warnings on}

procedure TFrustum.CalculatePoints(out FrustumPoints: TFrustumPointsDouble);
var
  Camera: TVector3Double;
begin
  FrustumPoints[0].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpNear]), Vector4Double(Planes[fpLeft]),  Vector4Double(Planes[fpTop]));
  FrustumPoints[1].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpNear]), Vector4Double(Planes[fpRight]), Vector4Double(Planes[fpTop]));
  FrustumPoints[2].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpNear]), Vector4Double(Planes[fpRight]), Vector4Double(Planes[fpBottom]));
  FrustumPoints[3].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpNear]), Vector4Double(Planes[fpLeft]),  Vector4Double(Planes[fpBottom]));

  FrustumPoints[0].W := 1;
  FrustumPoints[1].W := 1;
  FrustumPoints[2].W := 1;
  FrustumPoints[3].W := 1;

  if not ZFarInfinity then
  begin
    FrustumPoints[4].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpFar]), Vector4Double(Planes[fpLeft]),  Vector4Double(Planes[fpTop]));
    FrustumPoints[5].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpFar]), Vector4Double(Planes[fpRight]), Vector4Double(Planes[fpTop]));
    FrustumPoints[6].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpFar]), Vector4Double(Planes[fpRight]), Vector4Double(Planes[fpBottom]));
    FrustumPoints[7].XYZ := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpFar]), Vector4Double(Planes[fpLeft]),  Vector4Double(Planes[fpBottom]));

    FrustumPoints[4].W := 1;
    FrustumPoints[5].W := 1;
    FrustumPoints[6].W := 1;
    FrustumPoints[7].W := 1;
  end else
  begin
    Camera := ThreePlanesIntersectionPointDouble(Vector4Double(Planes[fpRight]), Vector4Double(Planes[fpLeft]),  Vector4Double(Planes[fpTop]));

    FrustumPoints[4].XYZ := FrustumPoints[0].XYZ - Camera;
    FrustumPoints[5].XYZ := FrustumPoints[1].XYZ - Camera;
    FrustumPoints[6].XYZ := FrustumPoints[2].XYZ - Camera;
    FrustumPoints[7].XYZ := FrustumPoints[3].XYZ - Camera;

    FrustumPoints[4].W := 0;
    FrustumPoints[5].W := 0;
    FrustumPoints[6].W := 0;
    FrustumPoints[7].W := 0;
  end;
end;

function TFrustum.SphereCollisionPossible(
  const SphereCenter: TVector3; const SphereRadiusSqr: Single):
  TFrustumCollisionPossible;
var
  fp, LastPlane: TFrustumPlane;
  Distance, SqrRealDistance: Single;
  InsidePlanesCount: Cardinal;
begin
  InsidePlanesCount := 0;

  LastPlane := High(FP);
  Assert(LastPlane = fpFar);

  { If the frustum has far plane in infinity, then ignore this plane.
    Inc InsidePlanesCount, since the sphere is inside this infinite plane. }
  if ZFarInfinity then
  begin
    LastPlane := Pred(LastPlane);
    Inc(InsidePlanesCount);
  end;

  { The logic goes like this:
      if sphere is on the "outside" of *any* of 6 planes, result is NoCollision
      if sphere is on the "inside" of *all* 6 planes, result is InsideFrustum
      else SomeCollisionPossible.

    Ideas based on
    [http://www.flipcode.com/articles/article_frustumculling.shtml]
    Version below is even better optimized: in case sphere
    intersects with one plane, but is outside another plane,
    their version may answer "intersection" (equivalent to my
    SomeCollisionPossible), without realizing that actually a better
    answer, NoCollision, exists. }

  { For the sake of maximum speed, I'm not using here things like
    TVector3.DotProduct or PointToPlaneDistanceSqr }
  for fp := Low(fp) to LastPlane do
  begin
   { This is not a true distance since this is signed }
   Distance := Planes[fp].Data[0] * SphereCenter.Data[0] +
               Planes[fp].Data[1] * SphereCenter.Data[1] +
               Planes[fp].Data[2] * SphereCenter.Data[2] +
               Planes[fp].Data[3];

   SqrRealDistance := Sqr(Distance);

   if (Distance < 0) and (SqrRealDistance > SphereRadiusSqr) then
   begin
    Result := fcNoCollision;
    Exit;
   end else
   if SqrRealDistance >= SphereRadiusSqr then
    Inc(InsidePlanesCount);
  end;

  if InsidePlanesCount = 6 then
    Result := fcInsideFrustum else
    Result := fcSomeCollisionPossible;
end;

function TFrustum.SphereCollisionPossibleSimple(
  const SphereCenter: TVector3; const SphereRadiusSqr: Single):
  boolean;
var
  fp: TFrustumPlane;
  Distance, SqrRealDistance: Single;
  LastPlane: TFrustumPlane;
begin
  LastPlane := High(FP);
  Assert(LastPlane = fpFar);

  { If the frustum has far plane in infinity, then ignore this plane. }
  if ZFarInfinity then
    LastPlane := Pred(LastPlane);

  for fp := Low(fp) to LastPlane do
  begin
   { This is not a true distance since this is signed }
   Distance := Planes[fp].Data[0] * SphereCenter.Data[0] +
               Planes[fp].Data[1] * SphereCenter.Data[1] +
               Planes[fp].Data[2] * SphereCenter.Data[2] +
               Planes[fp].Data[3];

   SqrRealDistance := Sqr(Distance);

   if (Distance < 0) and (SqrRealDistance > SphereRadiusSqr) then
   begin
    Result := false;
    Exit;
   end;
  end;

  Result := true;
end;

function TFrustum.Box3DCollisionPossible(
  const Box: TBox3D): TFrustumCollisionPossible;

{ Note: I tried to optimize this function,
  since it's crucial for TOctree.EnumerateCollidingOctreeItems,
  and this is crucial for TCastleScene.RenderFrustumOctree,
  and this is crucial for overall speed of rendering. }

var
  fp: TFrustumPlane;
  InsidePlanesCount: Cardinal;
  LastPlane: TFrustumPlane;
begin
  if Box.IsEmpty then
    Exit(fcNoCollision);

  InsidePlanesCount := 0;

  LastPlane := High(FP);
  Assert(LastPlane = fpFar);

  { If the frustum has far plane in infinity, then ignore this plane.
    Inc InsidePlanesCount, since the box is inside this infinite plane. }
  if ZFarInfinity then
  begin
    LastPlane := Pred(LastPlane);
    Inc(InsidePlanesCount);
  end;

  { The logic goes like this:
      if box is on the "outside" of *any* of 6 planes, result is NoCollision
      if box is on the "inside" of *all* 6 planes, result is InsideFrustum
      else SomeCollisionPossible. }

  for fp := Low(fp) to LastPlane do
  begin
    { Don't be confused by names below: pcOutside means that box
      is where Planes[fp] normal points, which means *inside* the frustum... }
    case Box.PlaneCollision(Planes[fp]) of
      pcInside: Exit(fcNoCollision);
      pcOutside: Inc(InsidePlanesCount);
    end;
  end;

  if InsidePlanesCount = 6 then
    Result := fcInsideFrustum else
    Result := fcSomeCollisionPossible;
end;

function TFrustum.Box3DCollisionPossibleSimple(
  const Box: TBox3D): boolean;

{ Implementation is obviously based on
  TFrustum.Box3DCollisionPossible above, see there for more comments. }

var
  fp: TFrustumPlane;
  LastPlane: TFrustumPlane;
begin
  if Box.IsEmpty then
    Exit(false);

  LastPlane := High(FP);
  Assert(LastPlane = fpFar);

  { If the frustum has far plane in infinity, then ignore this plane. }
  if ZFarInfinity then
    LastPlane := Pred(LastPlane);

  for fp := Low(fp) to LastPlane do
    { Again, don't be confused by name "Inside" below: pcInside
      means that box is where Planes[fp] inverted normal points,
      which means *outside* the frustum... }
    if Box.PlaneCollisionInside(Planes[fp]) then
      Exit(false);

  Result := true;
end;

function TFrustum.Move(const M: TVector3): TFrustum;
begin
  Result.Planes[fpLeft  ] := PlaneMove(Planes[fpLeft]  , M);
  Result.Planes[fpRight ] := PlaneMove(Planes[fpRight] , M);
  Result.Planes[fpBottom] := PlaneMove(Planes[fpBottom], M);
  Result.Planes[fpTop   ] := PlaneMove(Planes[fpTop]   , M);
  Result.Planes[fpNear  ] := PlaneMove(Planes[fpNear]  , M);
  { This is Ok for frustum with infinite far plane, since
    PlaneMove will simply keep the far plane invalid }
  Result.Planes[fpFar   ] := PlaneMove(Planes[fpFar]   , M);
  Result.ZFarInfinity := ZFarInfinity;
  Result.NormalizePlanes;
end;

procedure TFrustum.MoveVar(const M: TVector3);
begin
  PlaneMoveVar(Planes[fpLeft]  , M);
  PlaneMoveVar(Planes[fpRight] , M);
  PlaneMoveVar(Planes[fpBottom], M);
  PlaneMoveVar(Planes[fpTop]   , M);
  PlaneMoveVar(Planes[fpNear]  , M);
  { This is Ok for frustum with infinite far plane, since
    PlaneMove will simply keep the far plane invalid }
  PlaneMoveVar(Planes[fpFar]   , M);
  NormalizePlanes;
end;

function TFrustum.DirectionInside(const Direction: TVector3): boolean;
begin
  { First we check fpTop, since this (usually?) has the highest chance
    of failing (when Direction is direction of sun high in the sky) }
  Result := ( Planes[fpTop].Data[0] * Direction.Data[0] +
              Planes[fpTop].Data[1] * Direction.Data[1] +
              Planes[fpTop].Data[2] * Direction.Data[2] >= 0 ) and
            ( Planes[fpLeft].Data[0] * Direction.Data[0] +
              Planes[fpLeft].Data[1] * Direction.Data[1] +
              Planes[fpLeft].Data[2] * Direction.Data[2] >= 0 ) and
            ( Planes[fpRight].Data[0] * Direction.Data[0] +
              Planes[fpRight].Data[1] * Direction.Data[1] +
              Planes[fpRight].Data[2] * Direction.Data[2] >= 0 ) and
            ( Planes[fpBottom].Data[0] * Direction.Data[0] +
              Planes[fpBottom].Data[1] * Direction.Data[1] +
              Planes[fpBottom].Data[2] * Direction.Data[2] >= 0 );
end;

function TFrustum.Transform(const M: TMatrix4): TFrustum;
var
  I: TFrustumPlane;
begin
  if ZFarInfinity then
  begin
    Assert(High(I) = fpFar);
    for I := Low(I) to Pred(High(I)) do
      Result.Planes[I] := PlaneTransform(Planes[I], M);
    Result.Planes[fpFar] := TVector4.Zero;
  end else
    for I := Low(I) to High(I) do
      Result.Planes[I] := PlaneTransform(Planes[I], M);

  Result.ZFarInfinity := ZFarInfinity;
end;

function TFrustum.ToNiceStr(const Indent: string): string;
begin
  Result := ToString(Indent);
end;

function TFrustum.ToString(const Indent: string): string;
var
  I: TFrustumPlane;
begin
  Result := '';
  if ZFarInfinity then
  begin
    Assert(High(I) = fpFar);
    for I := Low(I) to Pred(High(I)) do
      Result := Result + Indent + Planes[I].ToString + LineEnding;
    Result := Result + Indent + '(no far plane, frustum goes to infinity)' + LineEnding;
  end else
    for I := Low(I) to High(I) do
      Result := Result + Indent + Planes[I].ToString + LineEnding;
end;

end.
