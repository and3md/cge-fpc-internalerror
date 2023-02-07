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

{ Make a sphere. }
procedure Sphere_Proxy(CoordIndex: TLongIntList;
  Coord: TVector3List; Normal: TVector3List;
  TexCoord: TVector2List;
  OverTriangulate: boolean; const Radius: Single;
  const QuadricSlices, QuadricStacks: Cardinal);

  { We have rings numbered 0 ... QuadricStacks.
    The rings nr 0 and QuadricStacks have 0 radius (bottom and top of the sphere). }

  { For stack number calculate it's radius and height (in y). }
  procedure StackCircle(StackNum: integer; out StackRadius, Height: Single);
  var
    Alpha, S, C: Float;
  begin
    { Calculate correct StackRadius and Height for sphere of radius 1,
      and then scale by Radius. }

    { Divide the vertical ring into QuadricStacks equal pieces.
      This is much better idea than simply dividing along the Height,
      because it allows the top/bottom of the sphere to look much better.
      This is also what OpenGL quadrics do.
      You can compare by using below

        StackRadius := Sqrt( 1 - Sqr(1 - 2*StackNum/QuadricStacks) ) * Radius;
        Height := (-1 + StackNum * 2/QuadricStacks) * Radius;
    }
    Alpha := Pi * StackNum / QuadricStacks;
    SinCos(Alpha, S, C);
    StackRadius :=  S * Radius;
    Height      := -C * Radius;
  end;

var
  StackRadius, StackHeight: Single;
  StackFaceIndex, BottomIndex, TopIndex, BottomCircleIndex, CircleIndex,
    StackNum, I: Integer;
begin
  { For VRML 1.0, some of these MF fields by default have non-empty content.
    It's safest to just clean them. }
  CoordIndex.Count := 0;
  Coord.Count := 0;
  Normal.Count := 0;
  if TexCoord <> nil then TexCoord.Count := 0;

  { We generate QuadricSlices + 1 points, not just QuadricSlices for each stack.
    Reason: texture coordinates must be different at the last point
    (1.0) than the first (0.0).
    Otherwise texture seam would not be correctly closed. }

  { Even for the bottom/top point, we create many points, because
    they need different tex coords. }
  BottomIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices + 1;
  Normal.Count := Normal.Count + QuadricSlices + 1;
  for I := 0 to QuadricSlices do
  begin
    Coord.List^[BottomIndex + I] := Vector3(0, -Radius, 0);
    Normal.List^[BottomIndex + I] := Vector3(0, -1, 0);
  end;
  if TexCoord <> nil then
  begin
    TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices do
      TexCoord.List^[BottomIndex + I] := Vector2(I / QuadricSlices, 0);
  end;

  BottomCircleIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices + 1;
  StackCircle(1, StackRadius, StackHeight);
  GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(BottomCircleIndex));
  Coord[Coord.Count - 1] := Coord.List^[BottomCircleIndex];
  Normal.Count := Normal.Count + QuadricSlices + 1;
  for I := 0 to QuadricSlices - 1 do
    Normal.List^[BottomCircleIndex + I] := (Coord.List^[BottomCircleIndex + I]).Normalize;
  Normal[Normal.Count - 1] := Normal.List^[BottomCircleIndex];
  if TexCoord <> nil then
  begin
    TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices  do
      TexCoord.List^[BottomCircleIndex + I] := Vector2(I / QuadricSlices, 1 / QuadricStacks);
  end;

  { Bottom stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    CoordIndex.List^[StackFaceIndex + I * 4    ] := BottomCircleIndex + I + 1;
    CoordIndex.List^[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + I;
    CoordIndex.List^[StackFaceIndex + I * 4 + 2] := BottomIndex + I;
    CoordIndex.List^[StackFaceIndex + I * 4 + 3] := -1;
  end;

  for StackNum := 2 to QuadricStacks - 1 do
  begin
    StackCircle(StackNum, StackRadius, StackHeight);

    CircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices + 1;
    StackCircle(StackNum, StackRadius, StackHeight);
    GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(CircleIndex));
    Coord[Coord.Count - 1] := Coord.List^[CircleIndex];
    Normal.Count := Normal.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices - 1 do
      Normal.List^[CircleIndex + I] := (Coord.List^[CircleIndex + I]).Normalize;
    Normal[Normal.Count - 1] := Normal.List^[CircleIndex];
    if TexCoord <> nil then
    begin
      TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
      for I := 0 to QuadricSlices  do
        TexCoord.List^[CircleIndex + I] := Vector2(I / QuadricSlices, StackNum / QuadricStacks);
    end;

    { Next stack, from quads }
    StackFaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + QuadricSlices * 5;
    for I := 0 to QuadricSlices - 1 do
    begin
      CoordIndex.List^[StackFaceIndex + I * 5    ] := BottomCircleIndex + I;
      CoordIndex.List^[StackFaceIndex + I * 5 + 1] := BottomCircleIndex + I + 1;
      CoordIndex.List^[StackFaceIndex + I * 5 + 2] := CircleIndex + I + 1;
      CoordIndex.List^[StackFaceIndex + I * 5 + 3] := CircleIndex + I;
      CoordIndex.List^[StackFaceIndex + I * 5 + 4] := -1;
    end;

    BottomCircleIndex := CircleIndex;
  end;

  TopIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices + 1;
  Normal.Count := Normal.Count + QuadricSlices + 1;
  for I := 0 to QuadricSlices do
  begin
    Coord.List^[TopIndex + I] := Vector3(0, Radius, 0);
    Normal.List^[TopIndex + I] := Vector3(0, 1, 0);
  end;
  if TexCoord <> nil then
  begin
    TexCoord.Count := TexCoord.Count + QuadricSlices + 1;
    for I := 0 to QuadricSlices do
      TexCoord.List^[TopIndex + I] := Vector2(I / QuadricSlices, 1);
  end;

  { Top stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    CoordIndex.List^[StackFaceIndex + I * 4    ] := BottomCircleIndex + I;
    CoordIndex.List^[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + I + 1;
    CoordIndex.List^[StackFaceIndex + I * 4 + 2] := TopIndex + I;
    CoordIndex.List^[StackFaceIndex + I * 4 + 3] := -1;
  end;
end;

{ Make a wireframe sphere. }
procedure SphereWireframe_Proxy(CoordIndex: TLongIntList;
  Coord: TVector3List; const Radius: Single;
  const QuadricSlices, QuadricStacks: Cardinal);

  { We have rings numbered 0 ... QuadricStacks.
    The rings nr 0 and QuadricStacks have 0 radius (bottom and top of the sphere). }

  { For stack number calculate it's radius and height (in y). }
  procedure StackCircle(StackNum: integer; out StackRadius, Height: Single);
  var
    Alpha, S, C: Float;
  begin
    { Calculate correct StackRadius and Height for sphere of radius 1,
      and then scale by Radius. }

    { Divide the vertical ring into QuadricStacks equal pieces.
      This is much better idea than simply dividing along the Height,
      because it allows the top/bottom of the sphere to look much better.
      This is also what OpenGL quadrics do.
      You can compare by using below

        StackRadius := Sqrt( 1 - Sqr(1 - 2*StackNum/QuadricStacks) ) * Radius;
        Height := (-1 + StackNum * 2/QuadricStacks) * Radius;
    }
    Alpha := Pi * StackNum / QuadricStacks;
    SinCos(Alpha, S, C);
    StackRadius :=  S * Radius;
    Height      := -C * Radius;
  end;

var
  StackRadius, StackHeight: Single;
  StackFaceIndex, BottomIndex, TopIndex, BottomCircleIndex, CircleIndex,
    StackNum, I, NextI: Integer;
begin
  CoordIndex.Count := 0;
  Coord.Count := 0;

  BottomIndex := Coord.Count;
  Coord.Count := Coord.Count + 1;
  Coord.List^[BottomIndex] := Vector3(0, -Radius, 0);

  BottomCircleIndex := Coord.Count;
  Coord.Count := Coord.Count + QuadricSlices;
  StackCircle(1, StackRadius, StackHeight);
  GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(BottomCircleIndex));

  { Bottom stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    NextI := (I + 1) mod QuadricSlices;
    CoordIndex.List^[StackFaceIndex + I * 4    ] := BottomCircleIndex + NextI;
    CoordIndex.List^[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + I;
    CoordIndex.List^[StackFaceIndex + I * 4 + 2] := BottomIndex;
    CoordIndex.List^[StackFaceIndex + I * 4 + 3] := -1;
  end;

  for StackNum := 2 to QuadricStacks - 1 do
  begin
    StackCircle(StackNum, StackRadius, StackHeight);

    CircleIndex := Coord.Count;
    Coord.Count := Coord.Count + QuadricSlices;
    StackCircle(StackNum, StackRadius, StackHeight);
    GenerateCircle(QuadricSlices, StackRadius, StackHeight, Coord.Ptr(CircleIndex));

    { Next stack, from quads (but we only need to add 3 lines) }
    StackFaceIndex := CoordIndex.Count;
    CoordIndex.Count := CoordIndex.Count + QuadricSlices * 5;
    for I := 0 to QuadricSlices - 1 do
    begin
      NextI := (I + 1) mod QuadricSlices;
      CoordIndex.List^[StackFaceIndex + I * 5    ] := BottomCircleIndex + NextI;
      CoordIndex.List^[StackFaceIndex + I * 5 + 1] := CircleIndex + NextI;
      CoordIndex.List^[StackFaceIndex + I * 5 + 2] := CircleIndex + I;
      CoordIndex.List^[StackFaceIndex + I * 5 + 3] := BottomCircleIndex + I;
      CoordIndex.List^[StackFaceIndex + I * 5 + 4] := -1;
    end;

    BottomCircleIndex := CircleIndex;
  end;

  TopIndex := Coord.Count;
  Coord.Count := Coord.Count + 1;
  Coord.List^[TopIndex] := Vector3(0, Radius, 0);

  { Top stack, from triangles }
  StackFaceIndex := CoordIndex.Count;
  CoordIndex.Count := CoordIndex.Count + QuadricSlices * 4;
  for I := 0 to QuadricSlices - 1 do
  begin
    NextI := (I + 1) mod QuadricSlices;
    CoordIndex.List^[StackFaceIndex + I * 4    ] := BottomCircleIndex + I;
    CoordIndex.List^[StackFaceIndex + I * 4 + 1] := BottomCircleIndex + NextI;
    CoordIndex.List^[StackFaceIndex + I * 4 + 2] := TopIndex;
    CoordIndex.List^[StackFaceIndex + I * 4 + 3] := -1;
  end;
end;


