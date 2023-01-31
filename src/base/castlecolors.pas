{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Color utilities, including HSV <-> RGB conversion. }
unit CastleColors;

{$include castleconf.inc}

interface

uses Math, Classes,
  CastleVectors;

type
  TCastleColor = TVector4;
  TCastleColorRGB = TVector3;

const
  { Common color constants, for comfort.
    They follow the CSS colors constants
    [http://www.w3.org/TR/CSS21/syndata.html#color-units].
    @groupBegin }
  Maroon : TCastleColor = (Data: ( 0.5 , 0.0 , 0.0 , 1.0));
  Red    : TCastleColor = (Data: ( 1.0 , 0.0 , 0.0 , 1.0));
  Orange : TCastleColor = (Data: ( 1.0 , 0.65, 0.0 , 1.0));
  Yellow : TCastleColor = (Data: ( 1.0 , 1.0 , 0.0 , 1.0));
  Olive  : TCastleColor = (Data: ( 0.5 , 0.5 , 0.0 , 1.0));
  Purple : TCastleColor = (Data: ( 0.5 , 0.0 , 0.5 , 1.0));
  Fuchsia: TCastleColor = (Data: ( 1.0 , 0.0 , 1.0 , 1.0));
  White  : TCastleColor = (Data: ( 1.0 , 1.0 , 1.0 , 1.0));
  Lime   : TCastleColor = (Data: ( 0.0 , 1.0 , 0.0 , 1.0));
  Green  : TCastleColor = (Data: ( 0.0 , 0.5 , 0.0 , 1.0));
  Navy   : TCastleColor = (Data: ( 0.0 , 0.0 , 0.5 , 1.0));
  Blue   : TCastleColor = (Data: ( 0.0 , 0.0 , 1.0 , 1.0));
  Aqua   : TCastleColor = (Data: ( 0.0 , 1.0 , 1.0 , 1.0));
  Teal   : TCastleColor = (Data: ( 0.0 , 0.5 , 0.5 , 1.0));
  Black  : TCastleColor = (Data: ( 0.0 , 0.0 , 0.0 , 1.0));
  Silver : TCastleColor = (Data: ( 0.75, 0.75, 0.75, 1.0));
  Gray   : TCastleColor = (Data: ( 0.5 , 0.5 , 0.5 , 1.0));
  { @groupEnd }

  { Additional color constants. } { }
  LightRed  : TCastleColor = (Data: ( 1.0 , 0.33, 0.33, 1.0));
  LightGreen: TCastleColor = (Data: ( 0.33, 1.0 , 0.33, 1.0));
  LightBlue : TCastleColor = (Data: ( 0.33, 0.33, 1.0 , 1.0));

  WhiteRGB  : TCastleColorRGB = (Data: ( 1.0 , 1.0 , 1.0));
  BlackRGB  : TCastleColorRGB = (Data: ( 0.0 , 0.0 , 0.0));
  RedRGB    : TCastleColorRGB = (Data: ( 1.0 , 0.0 , 0.0));
  YellowRGB : TCastleColorRGB = (Data: ( 1.0 , 1.0 , 0.0));
  GreenRGB  : TCastleColorRGB = (Data: ( 0.0 , 0.5 , 0.0));
  BlueRGB   : TCastleColorRGB = (Data: ( 0.0 , 0.0 , 1.0));
  GrayRGB   : TCastleColorRGB = (Data: ( 0.5 , 0.5 , 0.5));

  { Deprecated, use WhiteRGB. @deprecated }
  White3Single  : TCastleColorRGB = (Data: ( 1.0 , 1.0 , 1.0)) deprecated;
  { Deprecated, use BlackRGB. @deprecated }
  Black3Single  : TCastleColorRGB = (Data: ( 0.0 , 0.0 , 0.0)) deprecated;
  { Deprecated, use RedRGB. @deprecated }
  Red3Single    : TCastleColorRGB = (Data: ( 1.0 , 0.0 , 0.0)) deprecated;
  { Deprecated, use GreenRGB. @deprecated }
  Green3Single  : TCastleColorRGB = (Data: ( 0.0 , 0.5 , 0.0)) deprecated;
  { Deprecated, use BlueRGB. @deprecated }
  Blue3Single   : TCastleColorRGB = (Data: ( 0.0 , 0.0 , 1.0)) deprecated;

{ Calculate color intensity, for converting color to grayscale.
  @groupBegin }
function GrayscaleValue(const v: TCastleColorRGB): Single; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function GrayscaleValue(const v: TCastleColor): Single; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function GrayscaleValue(const v: TVector3Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function GrayscaleValue(const v: TVector4Byte): Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
{ @groupEnd }

function Grayscale(const v: TCastleColorRGB): TCastleColorRGB; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function Grayscale(const v: TVector3Byte): TVector3Byte; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;
function Grayscale(const v: TCastleColor): TCastleColor; {$ifdef SUPPORTS_INLINE} inline; {$endif} overload;

type
  { Function that processes RGB colors, used by TCastleImage.ModulateRGB. }
  TColorModulatorByteFunc = function (const Color: TVector3Byte): TVector3Byte;

{ Some functions matching TColorModulatorByteFunc type. }

{ Convert color to grayscale.
  @groupBegin }
function ColorGrayscaleByte(const Color: TVector3Byte): TVector3Byte;
{ @groupEnd }

{ Place color intensity (calculated like for grayscale)
  into the given color component. Set the other components zero.
  @groupBegin }
function ColorRedConvertByte(const Color: TVector3Byte): TVector3Byte;
function ColorGreenConvertByte(const Color: TVector3Byte): TVector3Byte;
function ColorBlueConvertByte(const Color: TVector3Byte): TVector3Byte;
{ @groupEnd }

{ Set color values for two other channels to 0.
  Note that it's something entirely different than
  ImageConvertToChannelVar: here we preserve original channel values,
  and remove values on two other channels.

  @groupBegin }
function ColorRedStripByte(const Color: TVector3Byte): TVector3Byte;
function ColorGreenStripByte(const Color: TVector3Byte): TVector3Byte;
function ColorBlueStripByte(const Color: TVector3Byte): TVector3Byte;
{ @groupEnd }

{ Converting between RGB and HSV.
  For HSV, we keep components as floating-point values,
  with hue in 0..6 range, saturation and value in 0..1.
  For RGB, one version keeps components as bytes (0..255 range),
  and the other as floating-point values (0..1 range).
  @groupBegin }
function HsvToRgb(const Value: TVector3): TVector3;
function RgbToHsv(const Value: TVector3): TVector3; overload;
function RgbToHsv(const Value: TVector3Byte): TVector3; overload;
function HsvToRgbByte(const Value: TVector3): TVector3Byte;
{ @groupEnd }

{ Given two colors in RGB, interpolate them in HSV space. }
function LerpRgbInHsv(const A: Single; const V1, V2: TVector3): TVector3;

{ Change color into a hexadecimal notation of it (like in HTML).
  This color includes an alpha channel (as 4th component),
  and so the output contains the alpha value at the end (so it's 8 hex digits),
  unless alpha is opaque in which case it's not written (and result is 6
  hex digits). }
function ColorToHex(const V: TCastleColor): string;

{ Change color into a hexadecimal notation of it (like in HTML).
  This color has no alpha channel,
  so it's always 6 hex digits. }
function ColorRGBToHex(const V: TCastleColorRGB): string;

{ Convert hexadecimal color notation (like in HTML) into an RGBA color.
  Handles 8 or 6 digit color (RGB or RGBA with 2 letters per component;
  for 6 digits, alpha is assumed to be 1.0 (opaque)).

  @raises EConvertError In case of invalid color as string. }
function HexToColor(const S: string): TCastleColor;

{ Convert hexadecimal color notation (like in HTML) into an RGB color.
  Handles 8 or 6 digit color (RGB or RGBA with 2 letters per component;
  for 8 digits, alpha is ignored).

  @raises EConvertError In case of invalid color as string. }
function HexToColorRGB(const S: string): TCastleColorRGB;

{ Change color opacity (alpha). }
function ColorOpacity(const Color: TCastleColor; const Opacity: Single): TCastleColor;

function FadeDarkColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;
function FadeColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;

{$define read_interface}
{$I castlecolors_persistent.inc}
{$undef read_interface}

implementation

uses SysUtils, CastleUtils, CastleStringUtils;

{$define read_implementation}
{$I castlecolors_persistent.inc}
{$undef read_implementation}

{ grayscale ------------------------------------------------------------------ }

function GrayscaleValue(const v: TCastleColor): Single;
begin
  { Weights to change RGB color to grayscale.

    Grayscale color is just a color with red = green = blue.
    So the simplest conversion of RGB to grayscale is just to set
    all three R, G, B components to the average (R + G + B) / 3.
    But, since human eye is most sensitive to green, then to red,
    and least sensitive to blue, it's better to calculate this
    with some non-uniform weights.

    These weights are copied from libpng manual. }

  Result := (0.212671 * V.Data[0]+
             0.715160 * V.Data[1]+
             0.072169 * V.Data[2]);
end;

function GrayscaleValue(const v: TCastleColorRGB): Single;
begin
  Result := 0.212671 * V.Data[0]+
            0.715160 * V.Data[1]+
            0.072169 * V.Data[2];
end;

function GrayscaleValue(const v: TVector3Byte): Byte;
begin
  // force multiplication as Word
  Result := (Word(54 ) * V.Data[0]+
             Word(183) * V.Data[1]+
             Word(19 ) * V.Data[2]) shr 8; //div 256;
end;

function GrayscaleValue(const v: TVector4Byte): Byte;
begin
  Result := (Word(54 ) * V.Data[0]+
             Word(183) * V.Data[1]+
             Word(19 ) * V.Data[2]) shr 8; //div 256;
end;

function Grayscale(const v: TCastleColor): TCastleColor;
var
  V3: TVector3 absolute V;
begin
  Result.Data[0] := GrayscaleValue(V3);
  Result.Data[1] := Result.Data[0];
  Result.Data[2] := Result.Data[0];
  Result.Data[3] := V.Data[3];
end;

function Grayscale(const v: TCastleColorRGB): TCastleColorRGB;
begin
  Result.Data[0] := GrayscaleValue(V);
  Result.Data[1] := Result.Data[0];
  Result.Data[2] := Result.Data[0];
end;

function Grayscale(const v: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := GrayscaleValue(V);
  Result.Data[1] := Result.Data[0];
  Result.Data[2] := Result.Data[0];
end;

{ color changing ------------------------------------------------------------ }

function ColorGrayscaleByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result := Grayscale(Color);
end;

function ColorRedConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := GrayscaleValue(Color);
  Result.Data[1] := 0;
  Result.Data[2] := 0;
end;

function ColorGreenConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Data[1] := GrayscaleValue(Color);
  Result.Data[0] := 0;
  Result.Data[2] := 0;
end;

function ColorBlueConvertByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Data[2] := GrayscaleValue(Color);
  Result.Data[0] := 0;
  Result.Data[1] := 0;
end;

function ColorRedStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := Color.Data[0];
  Result.Data[1] := 0;
  Result.Data[2] := 0;
end;

function ColorGreenStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := 0;
  Result.Data[1] := Color.Data[1];
  Result.Data[2] := 0;
end;

function ColorBlueStripByte(const Color: TVector3Byte): TVector3Byte;
begin
  Result.Data[0] := 0;
  Result.Data[1] := 0;
  Result.Data[2] := Color.Data[2];
end;

{ HSV stuff ------------------------------------------------------------------ }

function RgbToHsv(const Value: TVector3): TVector3;
var
  Chroma, V: Single;
begin
  V := Value.Max;
  Result.Data[2] := V;
  Chroma := V - Value.Min;

  { Chroma and V are now in the same range as RGB components.
    Which means 0..1 right now, so already Ok. }

  if Chroma = 0 then
  begin
    Result.Data[0] := 0;
    Result.Data[1] := 0;
  end else
  begin
    { calculate hue }
    if V = Value.Data[0] then
    begin
      Result.Data[0] := (Value.Data[1] - Value.Data[2]) / Chroma;
      if Result.Data[0] < 0 then
        Result.Data[0] := Result.Data[0] + 6.0;
    end else
    if V = Value.Data[1] then
      Result.Data[0] := (Value.Data[2] - Value.Data[0]) / Chroma + 2.0 else
      Result.Data[0] := (Value.Data[0] - Value.Data[1]) / Chroma + 4.0;

    { calculate saturation }
    Result.Data[1] := Chroma / V;
  end;
end;

function HsvToRgb(const Value: TVector3): TVector3;
var
  F, P, Q, T, V: Single;
begin
  F := Frac(Value.Data[0]);

  { RGB component candidates }
  V := Value.Data[2];
  P := V * (1 -  Value.Data[1]);
  Q := V * (1 - (Value.Data[1] * F));
  T := V * (1 - (Value.Data[1] * (1 - F)));

  case Floor(Value.Data[0]) of
    0, 6:begin Result.Data[0] := V; Result.Data[1] := T; Result.Data[2] := P; end;
    1:   begin Result.Data[0] := Q; Result.Data[1] := V; Result.Data[2] := P; end;
    2:   begin Result.Data[0] := P; Result.Data[1] := V; Result.Data[2] := T; end;
    3:   begin Result.Data[0] := P; Result.Data[1] := Q; Result.Data[2] := V; end;
    4:   begin Result.Data[0] := T; Result.Data[1] := P; Result.Data[2] := V; end;
    else begin Result.Data[0] := V; Result.Data[1] := P; Result.Data[2] := Q; end;
  end;
end;

function RgbToHsv(const Value: TVector3Byte): TVector3;
var
  ValueFloat: TVector3;
begin
  ValueFloat.Data[0] := Value.Data[0] / 255.0;
  ValueFloat.Data[1] := Value.Data[1] / 255.0;
  ValueFloat.Data[2] := Value.Data[2] / 255.0;
  Result := RgbToHsv(ValueFloat);
end;

function HsvToRgbByte(const Value: TVector3): TVector3Byte;
var
  ResultFloat: TVector3;
begin
  ResultFloat := HsvToRgb(Value);
  Result.Data[0] := RoundClamp255(ResultFloat.Data[0] * 255.0);
  Result.Data[1] := RoundClamp255(ResultFloat.Data[1] * 255.0);
  Result.Data[2] := RoundClamp255(ResultFloat.Data[2] * 255.0);
end;

function LerpRgbInHsv(const A: Single; const V1, V2: TVector3): TVector3;
var
  H1, H2, HOut: TVector3;
  HueDiff: Single;
begin
  H1 := RgbToHsv(V1);
  H2 := RgbToHsv(V2);

  { if one of the colors has saturation = 0, then resulting hue is copied
    from the other color, not interpolated. Otherwise,
    colors with saturation = 0 get hue = 0, which causes
    interpolation from something colorful (like blue) to black go through
    weird hue. }
  if H1.Data[1] = 0 then
    HOut.Data[0] := H2.Data[0] else
  if H2.Data[1] = 0 then
    HOut.Data[0] := H1.Data[0] else
  begin
    HueDiff := H2.Data[0] - H1.Data[0];
    if HueDiff > 3 then
    begin
      { from hue 1 to hue 2 go down through 0.0 }
      H2.Data[0] := H2.Data[0] - 6;
      HOut.Data[0] := H1.Data[0] + A * (H2.Data[0] - H1.Data[0]);
      if HOut.Data[0] < 0 then HOut.Data[0] := HOut.Data[0] + 6;
    end else
    if HueDiff < -3 then
    begin
      { from hue 1 to hue 2 go up through 6.0 }
      H2.Data[0] := H2.Data[0] + 6;
      HOut.Data[0] := H1.Data[0] + A * (H2.Data[0] - H1.Data[0]);
      if HOut.Data[0] > 6 then HOut.Data[0] := HOut.Data[0] - 6;
    end else
      { normal lerp when HueDiff inside [-3, 3] }
      HOut.Data[0] := H1.Data[0] + A * (H2.Data[0] - H1.Data[0]);
  end;

  { lerp on saturation and value is normal }
  HOut.Data[1] := H1.Data[1] + A * (H2.Data[1] - H1.Data[1]);
  HOut.Data[2] := H1.Data[2] + A * (H2.Data[2] - H1.Data[2]);

  Result := HsvToRgb(HOut);
end;

function ColorToHex(const V: TCastleColor): string;
var
  A: Byte;
begin
  Result := IntToHex(RoundClamp255(V.Data[0] * 255), 2) +
            IntToHex(RoundClamp255(V.Data[1] * 255), 2) +
            IntToHex(RoundClamp255(V.Data[2] * 255), 2);
  A := RoundClamp255(V.Data[3] * 255);
  if A <> 255 then
    Result := Result + IntToHex(A, 2);
end;

function ColorRGBToHex(const V: TCastleColorRGB): string;
begin
  Result := IntToHex(RoundClamp255(V.Data[0] * 255), 2) +
            IntToHex(RoundClamp255(V.Data[1] * 255), 2) +
            IntToHex(RoundClamp255(V.Data[2] * 255), 2);
end;

function HexToColor(const S: string): TCastleColor;
begin
  if Length(S) = 8 then
    Result := Vector4(
      StrHexToInt(Copy(S, 1, 2)) / 255,
      StrHexToInt(Copy(S, 3, 2)) / 255,
      StrHexToInt(Copy(S, 5, 2)) / 255,
      StrHexToInt(Copy(S, 7, 2)) / 255) else
  if Length(S) = 6 then
    Result := Vector4(
      StrHexToInt(Copy(S, 1, 2)) / 255,
      StrHexToInt(Copy(S, 3, 2)) / 255,
      StrHexToInt(Copy(S, 5, 2)) / 255,
      1.0) else
    raise EConvertError.CreateFmt('Invalid color hex string: "%s"', [S]);
end;

function HexToColorRGB(const S: string): TCastleColorRGB;
begin
  if (Length(S) = 8) or
     (Length(S) = 6) then
    Result := Vector3(
      StrHexToInt(Copy(S, 1, 2)) / 255,
      StrHexToInt(Copy(S, 3, 2)) / 255,
      StrHexToInt(Copy(S, 5, 2)) / 255) else
    raise EConvertError.CreateFmt('Invalid color hex string: "%s"', [S]);
end;

function ColorOpacity(const Color: TCastleColor; const Opacity: Single): TCastleColor;
begin
  Result := Color;
  Result.Data[3] := Opacity;
end;

function FadeDarkColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;
const
  FullWhiteEnd = 0.9;
  FullBlack = 0.3;
  { We assume that MinScale is small enough that difference between
    "Color * MinScale * screen color" and
    "MinScale * screen color" is not noticeable. }
  MinScale = 0.1;
begin
  if FadeIntensity > 0 then
  begin
    { for FadeIntensity in 1...FullWhiteEnd (going down):
      screen color := Color * original screen color }
    if FadeIntensity > FullWhiteEnd then
      Result := Color else
    { for FadeIntensity in FullWhiteEnd...FullBlack (going down):
      final screen color changes:
      - from screen color := Color * original screen color
      - to   screen color := Color * MinScale * original screen color }
    if FadeIntensity > FullBlack then
      Result := Color * MapRange(FadeIntensity, FullWhiteEnd, FullBlack, 1, MinScale) else
    { for FadeIntensity in FullBlack...0 (going down):
      final screen color changes:
      - from screen color := MinScale * original screen color
      - to   screen color := original screen color }
      Result := White * MapRange(FadeIntensity, FullBlack, 0, MinScale, 1);

    Result.Data[3] := 1.0; { alpha always 1.0 in this case }
  end else
    Result := TVector4.Zero;
end;

function FadeColor(const Color: TCastleColor;
  const FadeIntensity: Single): TCastleColor;
const
  FullTime = 0.9;
var
  Intensity: Single;
begin
  if FadeIntensity > 0 then
  begin
    if FadeIntensity < FullTime then
      Intensity := MapRange(FadeIntensity, 0, FullTime, 0, 1) else
      Intensity := MapRange(FadeIntensity, FullTime, 1, 1, 0);
    Result := Color;
    Result.Data[3] := Intensity;
  end else
    Result := TVector4.Zero;
end;

end.
