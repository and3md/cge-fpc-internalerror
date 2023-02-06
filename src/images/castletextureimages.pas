{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handling of images for textures.
  This unit is not OpenGL-specific, it should be suitable for all 3D libraries.
  See CastleGLImage for OpenGL-specific handling of textures and other images.

  Texture is any TEncodedImage instance. This includes not only
  a traditional 2D/3D matrix of pixels represented as TCastleImage,
  but also a texture compressed for GPU (TGPUCompressedImage). Moreover, a texture
  may have mipmaps defined --- they are stored inside TCompositeImage
  instance (that contains a list of TEncodedImage).

  Since not everything can really deal with such flexible definition
  of a texture, we decided to separate some routines specifically
  for textures. For example, you have LoadTextureImage to load full texture
  information --- contrast this with LoadImage routine in CastleImages unit,
  that only returns TCastleImage (a "normal" way to deal with image data). }
unit CastleTextureImages;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleImages, CastleUtils;

const
  { Image classes that are handled by absolutely all OpenGL versions. }
  TextureImageClasses: array [0..3] of TEncodedImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage);

  { All image classes that may be handled by OpenGL.
    Some of them may require specific OpenGL extensions or versions
    (like GPU-compressed or float textures). }
  TextureImageClassesAll: array [0..5] of TEncodedImageClass = (
    TRGBImage,
    TRGBAlphaImage,
    TGrayscaleImage,
    TGrayscaleAlphaImage,
    TGPUCompressedImage,
    TRGBFloatImage);

{ Load image suitable for a texture.
  This will load image to memory formats supported by common
  3D libraries (like OpenGL), for example it will never return TRGBFloatImage
  (although OpenGL may support it, but we cannot be sure at this point).
  It may return texture compressed using one of the GPU compression algorithms
  (see TTextureCompression).

  If the image comes from a TCompositeImage file (DDS, KTX...), it will also return it
  (if not, Composite returned will be @nil). This allows you to e.g. use
  texture mipmaps recorded there. Note that Composite.OwnsFirstImage is set
  to @false, so you can always safely free everything by simple
  @code(FreeAndNil(Image); FreeAndNil(Composite);).

  Overloaded version without Composite parameter assumes you're
  not interested in this information (still it handles Composite files of course,
  it just doesn't return Composite object instance).

  @groupBegin }
function LoadTextureImage(const URL: string;
  const LoadOptions: TLoadImageOptions = []): TEncodedImage; overload;
{ @groupEnd }

type

  { Texture minification filter (what happens when many texture pixels
    are squeezed in one screen pixel). }
  TAutoMinificationFilter = (
    minNearest,
    minLinear,
    minNearestMipmapNearest,
    minNearestMipmapLinear,
    minLinearMipmapNearest,
    minLinearMipmapLinear,

    { Interpretation of this filter depends on current
      @link(TRenderingAttributes.MinificationFilter Scene.Attributes.MinificationFilter).
      If that is also minDefault, it depends on current
      @link(TRenderingAttributes.DefaultMinificationFilter). }
    minDefault,
    { Alias for minNearest. }
    minFastest,
    { Alias for minLinearMipmapLinear. }
    minNicest
  );
  TMinificationFilter = minNearest..minLinearMipmapLinear;

  { Texture magnification filter (what happens when a single texture pixel
    in stretched over many screen pixels). }
  TAutoMagnificationFilter = (
    magNearest,
    magLinear,

    { Interpretation of this filter depends on current
      @link(TRenderingAttributes.MagnificationFilter Scene.Attributes.MagnificationFilter).
      If that is also magDefault, it depends on current
      @link(TRenderingAttributes.DefaultMagnificationFilter). }
    magDefault,
    { Alias for magnNearest. }
    magFastest,
    { Alias for magLinear. }
    magNicest
  );
  TMagnificationFilter = magNearest..magLinear;

var
  { Log texture cache events. Allows to see how the cache performs,
    and also how alpha channel is detected.
    A @italic(lot) of log messages.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogTextureCache: boolean = false;

  { Cache of texture images, equal to X3DCache
    and automatically initialized / finalized if you use X3DNodes unit. }

implementation

uses SysUtils, CastleStringUtils, CastleLog, CastleURIUtils;

function LoadTextureImage(const URL: string;
  const LoadOptions: TLoadImageOptions): TEncodedImage;
begin
  Result := nil;
end;

end.
