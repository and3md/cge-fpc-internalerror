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

{ Rectangle representation (TRectangle, TFloatRectangle). }
unit CastleRectangles;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleVectors, CastleUtils;

type
  { Horizontal position of one control/rectangle
    with respect to another.

    This is used by @link(TCastleUserInterface.Anchor),
    @link(TRectangle.Align), @link(TFloatRectangle.Align) and other methods
    to specify the alignment of one control/rectangle with respect to another.

    Note that @link(TCastleUserInterface.Anchor) has various overloaded
    versions. E.g. you can align the left side of the control to the left side
    of the parent (most common situation), or you can align left side
    of the control to the middle of the parent...

    @seealso TVerticalPosition
  }
  THorizontalPosition = (
    hpLeft,
    hpMiddle,
    hpRight
  );

  { Vertical position of one control/rectangle with respect to another.
    @seealso THorizontalPosition }
  TVerticalPosition = (
    vpBottom,
    vpMiddle,
    vpTop
  );

  { 2D rectangle with @bold(integer) coordinates.
    Useful for various 2D GUI operations.

    The area covered by the rectangle starts in (Left,Bottom)
    pixel and spans (Width,Height) pixels. This means that the right-top pixel
    covered by the rectangle is (Left + Width - 1,Bottom + Height - 1).
    The rectangle is empty (@link(Contains) will always answer @false)
    when either Width or Height are zero. Neither Width nor Height can ever
    be negative. }
  TRectangle = record
  public
    Left, Bottom: Integer;
    Width, Height: Cardinal;
  end;

  { 2D rectangle with @bold(float) coordinates.
    Useful for various 2D GUI operations, and for bounding boxes for 2D objects.

    The area covered by the rectangle starts at (Left,Bottom) position
    and spans (Width,Height) units.
    The rectangle is empty (@link(Contains) will always answer @false)
    when either Width or Height are less than zero.
    @bold(This is consistent with it's 3D equivalent, @link(TBox3D),
    and different from it's integer counterpart @link(TRectangle).)
    In case of float bounding box (@link(TBox3D)) or float rectangle
    (@name), having a zero size makes sense, and it still is something non-empty
    (a single 2D or 3D point has zero size, but it also still has a position). }
  TFloatRectangle = record
  public
    Left, Bottom: Single;
    Width, Height: Single;
   end;

  PFloatRectangle = ^TFloatRectangle;
  TFloatRectangleArray = packed array [0..MaxInt div SizeOf(TFloatRectangle) - 1] of TFloatRectangle;
  PFloatRectangleArray = ^TFloatRectangleArray;

  TRectangleList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TRectangle>)
  public
    { Index of the first rectangle that contains point (X, Y).
      Returns -1 if not found. }
    function FindRectangle(const X, Y: Integer): Integer; overload;
    function FindRectangle(const Point: TVector2): Integer; overload;
  end;

  TFloatRectangleList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TFloatRectangle>;

implementation

uses SysUtils, Math;

{ TRectangleList -------------------------------------------------------------- }

function TRectangleList.FindRectangle(const X, Y: Integer): Integer;
begin
//  for Result := 0 to Count - 1 do
//    if List^[Result].Contains(X, Y) then
//      Exit;
  Result := -1;
end;

function TRectangleList.FindRectangle(const Point: TVector2): Integer;
begin
//  for Result := 0 to Count - 1 do
//    if List^[Result].Contains(Point) then
//      Exit;
  Result := -1;
end;

end.
