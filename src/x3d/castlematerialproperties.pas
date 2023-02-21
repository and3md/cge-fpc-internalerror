{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Material and texture properties from external files (TMaterialProperty,
  global MaterialProperties collection). }
unit CastleMaterialProperties;

{$I castleconf.inc}

interface

uses Classes, DOM, Generics.Collections;

type

  TTextureCompressionsToGenerate = record
    { In addition to Compressions,
      generate also the most suitable variant of DXTn compression. }
    DxtAutoDetect: boolean;
  end;

  { Store information that is naturally associated with a given material
    or texture in an external file. Documentation and example of such
    file is on  https://castle-engine.io/creating_data_material_properties.php .
    Right now this allows to define things like:

    @unorderedList(
      @itemSpacing compact
      @item footsteps,
      @item toxic ground (hurts player),
      @item bump mapping (normal maps and height maps for given texture),
      @item texture GPU-compressed and downscaled alternatives.
    )

    In the future, it should be possible to express all these properties
    in pure VRML/X3D (inside Appearance / Material / ImageTexture nodes).
    Right now, you can do this with bump mapping, see
    https://castle-engine.io/x3d_extensions.php#section_ext_bump_mapping ,
    but not footsteps or toxic ground.
    In the future it should also be possible to express these properties
    in 3D authoring software (like Blender), and easily export them
    to appropriate VRML/X3D nodes.
    For now, this TMaterialProperty allows us to easily customize materials
    in a way that is not possible in Blender.

    Using an external file for material properties has also long-term
    advantages: it can be shared across many 3D models, for example
    you can define footsteps sound for all grounds using the @code(grass.png)
    textures, in all levels, at once.

    You have to load an XML file by setting
    @link(TMaterialProperties.URL MaterialProperties.URL) property.
  }
  TMaterialProperties = class
  strict private
    type
      TAutoGeneratedTextures = class
      strict private
        const
          PathsIgnoreCase = true;
        var
        FAutoProcessImageURLs: boolean;
        { necessary for Exclude with relative dirs, like "entites/*", to work }
        FBaseURL: string;
        FCompressedFormatsToGenerate: TTextureCompressionsToGenerate;
        FSmallestScale: Cardinal;
        procedure LoadImageEvent(var URL: string);
        function IsAbsoluteURLMatchingRelativeMask(const URL, Mask: string): boolean;
      public
        constructor Create(const Element: TDOMElement; const BaseURL: string; const AnAutoProcessImageURLs: boolean);
        destructor Destroy; override;
        function TextureURLMatches(const URL: string): boolean;
        property CompressedFormatsToGenerate: TTextureCompressionsToGenerate
          read FCompressedFormatsToGenerate;
        property SmallestScale: Cardinal read FSmallestScale;
      end;

      TAutoGeneratedTexturesList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TAutoGeneratedTextures>;
    var
    FAutoGeneratedTexturesList: TAutoGeneratedTexturesList;
    FURL: string;
    FAutoProcessImageURLs: boolean;
    procedure SetURL(const Value: string);
  public
    constructor Create(const AnAutoProcessImageURLs: boolean);
    destructor Destroy; override;

    { Load material properties from given XML file.
      Set this to empty string to unload previously loaded properties.
      See Castle1 and fps_game data for examples how this looks like,
      in @code(material_properties.xml). }
    property URL: string read FURL write SetURL;

    property FileName: string read FURL write SetURL; deprecated 'use URL';

    { Automatic compression formats generated for this texture.
      @param(TextureURL An absolute texture URL. Usually should just be taken
        from AutoGeneratedTextures returned list.) }
    function AutoCompressedTextureFormats(const TextureURL: string):
      TTextureCompressionsToGenerate;

    { Automatically generated smallest scale, for this texture.
      Return value should be intepreted just like TextureLoadingScale.
      See https://castle-engine.io/creating_data_material_properties.php#section_texture_scale
      @param(TextureURL An absolute texture URL. Usually should just be taken
        from AutoGeneratedTextures returned list.) }
    function AutoScale(const TextureURL: string): Cardinal;
  end;

{ Material and texture properties, see @link(TMaterialProperties).
  Set the @link(TMaterialProperties.URL URL) property
  to load material properties from XML file. }
function MaterialProperties: TMaterialProperties;

var
  { Use the auto-generated alternative downscaled images.
    This allows to conserve both GPU memory and loading time
    by using a downscaled images versions.

    The subset of your images which are affected by this must be declared inside
    the material_properties.xml file, which is loaded to @link(MaterialProperties).
    And the image files must be prepared earlier by the build tool call
    @code("castle-engine auto-generate-textures").
    See the https://castle-engine.io/creating_data_material_properties.php#section_texture_scale .

    Each size (width, height, and (for 3D images) depth) is scaled
    by 1 / 2^TextureLoadingScale.
    So value = 1 means no scaling, value = 2 means that each size is 1/2
    (texture area is 1/4), value = 3 means that each size is 1/4 and so on.

    This mechanism will @italic(not)
    automatically downscale textures at runtime. If the downscaled texture version
    should exist, according to the material_properties.xml file,
    but it doesn't, then texture loading will simply fail.
    If you want to scale the texture at runtime, use the similar @link(GLTextureScale)
    instead.

    This mechanism is independent from GLTextureScale:

    @unorderedList(
      @item(Scaling indicated by GLTextureScale is performed at runtime,
        after loading. It happens @bold(after) the results of
        TextureLoadingScale have already been applied.)

      @item(The GLTextureScale works on a different subset of textures.

        For GLTextureScale, the usage of a texture determines if it's a GUI texture
        (which cannot be scaled) or not.
        So textures loaded through TDrawableImage, or declared as guiTexture in X3D,
        are not affected by GLTextureScale. All other textures are affected.
        It doesn't matter from where they are loaded -- so it affects also
        texture contents created by code, or downloaded from the Internet.

        In contrast, the TextureLoadingScale works (only) on all the images
        declared as having a downscaled version in material_properties.xml.
        It is not affected by how the texture will be used.)

      @item(The GLTextureScale works only on texture formats that can be scaled.
        In particular, it cannot scale textures compressed with a GPU compression
        (S3TC and such). It silently ignores them.

        In contrast, the TextureLoadingScale can cooperate with GPU-compressed textures,
        if you also compress them automatically using the material_properties.xml
        and the build tool call @code("castle-engine auto-generate-textures").
        The downscaled image versions are generated from original (uncompressed,
        unscaled) images, and are then compressed.)

      @item(The GLTextureScale scaling is usually of worse quality, since it's
        done at runtime.

        In contrast, the downscaled textures used by TextureLoadingScale
        are generated as a preprocessing step.
        The build tool @code("castle-engine auto-generate-textures") may use
        a slower but higher-quality scaling.)
    )
  }
  TextureLoadingScale: Cardinal = 1;

implementation

uses SysUtils, StrUtils, Math,
  X3DNodes;

{ TMaterialProperties.TAutoGeneratedTextures -------------------------------- }

constructor TMaterialProperties.TAutoGeneratedTextures.Create(
  const Element: TDOMElement; const BaseURL: string;
  const AnAutoProcessImageURLs: boolean);
begin
  inherited Create;
  FAutoProcessImageURLs := AnAutoProcessImageURLs;
  FBaseURL := BaseURL;

  { read from XML }
end;

destructor TMaterialProperties.TAutoGeneratedTextures.Destroy;
begin
  inherited;
end;

function TMaterialProperties.TAutoGeneratedTextures.IsAbsoluteURLMatchingRelativeMask(
  const URL, Mask: string): boolean;
var
  U: string;
begin
  U := URL;
  Result := IsWild(U, Mask, PathsIgnoreCase);
end;

function TMaterialProperties.TAutoGeneratedTextures.TextureURLMatches(const URL: string): boolean;

  { Check is URL not excluded. }
  function CheckNotExcluded: boolean;
  begin
    Result := true;
  end;

begin
  Result := false;
end;

procedure TMaterialProperties.TAutoGeneratedTextures.LoadImageEvent(
  var URL: string);

  { Texture has GPU-compressed and/or downscaled counterpart, according to include/exclude
    variables. So try to replace URL with something compressed and downscaled. }
  procedure ReplaceURL;
  var
    Scale: Cardinal;
  begin
    { Do not warn about it, just as we don't warn when TextureLoadingScale = 2
      but we're loading image not mentioned in <auto_generated_textures>.
    if TextureLoadingScale > SmallestScale then
      raise Exception.CreateFmt('Invalid TextureLoadingScale %d, we do not have such downscaled images. You should add or modify the <scale smallest=".." /> declaration in "material_properties.xml", and make sure thar you load the "material_properties.xml" early enough.',
        [TextureLoadingScale]); }
    Scale := Min(SmallestScale, TextureLoadingScale);

  end;

begin
  if TextureURLMatches(URL) then
    ReplaceURL;
end;

{ TMaterialProperties ---------------------------------------------------------- }

constructor TMaterialProperties.Create(const AnAutoProcessImageURLs: boolean);
begin
  inherited Create;
  FAutoProcessImageURLs := AnAutoProcessImageURLs;
  FAutoGeneratedTexturesList := TAutoGeneratedTexturesList.Create({ owns objects } true);
end;

destructor TMaterialProperties.Destroy;
begin
  FreeAndNil(FAutoGeneratedTexturesList);
  inherited;
end;

procedure TMaterialProperties.SetURL(const Value: string);
var
  Config: TXMLDocument;
  Stream: TStream;
begin
  FURL := Value;

  FAutoGeneratedTexturesList.Clear;

  if URL = '' then Exit;
end;


function TMaterialProperties.AutoCompressedTextureFormats(const TextureURL: string):
  TTextureCompressionsToGenerate;
var
  I: Integer;
begin
  // initialize Result to empty
  FillChar(Result, SizeOf(Result), 0);

  for I := 0 to FAutoGeneratedTexturesList.Count - 1 do
    if FAutoGeneratedTexturesList[I].TextureURLMatches(TextureURL) then
      Exit(FAutoGeneratedTexturesList[I].CompressedFormatsToGenerate);
end;

function TMaterialProperties.AutoScale(const TextureURL: string): Cardinal;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to FAutoGeneratedTexturesList.Count - 1 do
    if FAutoGeneratedTexturesList[I].TextureURLMatches(TextureURL) then
      Exit(FAutoGeneratedTexturesList[I].SmallestScale);
end;

{ globals -------------------------------------------------------------------- }

var
  FMaterialProperties: TMaterialProperties;

function MaterialProperties: TMaterialProperties;
begin
  if FMaterialProperties = nil then
    FMaterialProperties := TMaterialProperties.Create(true);
  Result := FMaterialProperties;
end;

finalization
  FreeAndNil(FMaterialProperties);
end.
