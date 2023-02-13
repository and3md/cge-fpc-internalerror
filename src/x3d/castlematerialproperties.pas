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

uses Classes, DOM, Generics.Collections,
  CastleUtils, CastleClassUtils, CastleStringUtils;

type

        TSoundInfo = class;

        { Unique sound type identifier for sounds used within TRepoSoundEngine. }
        TSoundType = record
        private
          { Just an index to TRepoSoundEngine.SoundNames array. }
          Index: Cardinal;
        public
          function InternalInfo: TSoundInfo;
          class operator {$ifdef FPC}={$else}Equals{$endif} (const SoundType1, SoundType2: TSoundType): boolean;
        end;

        { List of TSoundInfo.

          @exclude
          @bold(This is an internal class, and in the future will not be publicly available). }
        TSoundInfoList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundInfo>)
          { Index of sound with given TSoundInfo.Name, or -1 if not found. }
          function IndexOfName(const SoundName: String): Integer;
        end;

        { Sound information.
          Most fields of this classs correspond to appropriate attributes in
          the XML file loaded by setting @link(TRepoSoundEngine.RepositoryURL).

          @exclude
          @bold(This is an internal class, and in the future will not be publicly available). }
        TSoundInfo = class
        private
          { OpenAL buffer of this sound. @nil if buffer is not yet loaded,
            which may happen only if TRepoSoundEngine.ALContextOpen was not yet
            called or when sound has URL = ''. }
        public
          { Unique sound name (including parent group names). Empty for the special sound stNone. }
          Name: string;

          { URL from which to load sound data.
            Absolute (including parent group URL parts).

            Empty means that the sound data is not defined,
            so the OpenAL buffer will not be initialized and trying to play
            this sound (with methods like TSoundEngine.Sound or TSoundEngine.Sound3D)
            will do nothing. This is useful if you want to use a sound name
            in code, but you do not have the actual sound file for this yet. }
          URL: string;

          { Gain (how loud the sound is).
            They are mapped directly to respective OpenAL source properties,
            so see OpenAL specification for exact details what they mean.
            In short:

            @unorderedList(
              @item(Gain scales the sound loudness. Use this to indicate that
                e.g. a plane engine is louder than a mouse squeak (when heard
                from the same distance).

                Do @italic(not) make the actual sound data (in wav, ogg and such files)
                louder/more silent for this purpose.
                This is usually bad for sound quality. Instead, keep your sound data
                at max loudness (normalized), and use this @link(Gain) property
                to scale sound.

                It can be antything from 0 to +infinity. The default is 1.)

              @item(MinGain and MaxGain force a minimum/maximum sound loudness.
                These can be used to "cheat" around default distance attenuation
                calculation.

                These must be in [0, 1] range. By default MinGain is 0 and MaxGain is 1.)
            )

            Note that Gain value > 1 is allowed.
            Although OpenAL may clip the resulting sound (after all
            calculations taking into account 3D position will be done).
            The resulting sound is also clamped by MaxGain
            (that generally must be in [0, 1], although some OpenAL implementations
            allow values > 1).

            When this sound is used for @link(TLoopingChannel.Sound):
            @orderedList(
              @item(MinGain, MaxGain are ignored.)
              @item(Effective Gain (passed to OpenAL sound source) is the
                @link(TLoopingChannel.Volume) multiplied by our @link(Gain).)
            ) }
          Gain, MinGain, MaxGain: Single;

          { How important the sound is. Influences what happens when we have a lot
            of sounds playing at once. See TSound.Importance.

            Ignored when this sound is used for @link(TLoopingChannel.Sound). }
          DefaultImportance: Cardinal;

          { A group (one among FSoundGroups, or @nil if not in any group). }
          ParentGroup: TSoundInfoList;
        end;


        TSoundBuffer = class
        private
          ALBuffer: UIntPtr;
          { Absolute URL.
            Never empty (do not create TSoundBuffer instances for invalid / empty URL,
            like the ones that can be created by TRepoSoundEngine for not defined sounds.) }
          URL: string;
          FDuration: single;
          References: Cardinal;
        public

          { Duration of the sound, in seconds. Zero if not loaded yet. }
          property Duration: single read FDuration;
        end;



  { Information for a particular material. }
  TMaterialProperty = class
  strict private
    FTextureBaseName: string;
    FFootstepsSound: TSoundType;
    FToxic: boolean;
    FToxicDamageConst, FToxicDamageRandom, FToxicDamageTime: Single;
    FNormalMap: string;
    FAlphaChannel: string;
  private
    procedure LoadFromDOMElement(Element: TDOMElement; const BaseURL: string);
  public
    { Texture basename to associate this property will all appearances
      using given texture. For now, this is the only way to associate
      property, but more are possible in the future (like MaterialNodeName). }
    property TextureBaseName: string read FTextureBaseName write FTextureBaseName;

    { Footsteps sound to make when player is walking on this material.
      stNone is no information is available. }
    property FootstepsSound: TSoundType read FFootstepsSound write FFootstepsSound;

    { Is the floor toxic when walking on it.
      Taken into account only if you assign @link(TCastleSceneManager.Player).
      @groupBegin }
    property Toxic: boolean read FToxic write FToxic;
    property ToxicDamageConst: Single read FToxicDamageConst write FToxicDamageConst;
    property ToxicDamageRandom: Single read FToxicDamageRandom write FToxicDamageRandom;
    property ToxicDamageTime: Single read FToxicDamageTime write FToxicDamageTime;
    { @groupEnd }

    { Normal map texture URL. This is a simple method to activate bump mapping,
      equivalent to using normalMap field in an Appearance node of VRML/X3D, see
      https://castle-engine.io/x3d_extensions.php#section_ext_bump_mapping .

      In case both VRML/X3D Appearance specifies normalMap and we have
      NormalMap defined here, the VRML/X3D Appearance is used. }
    property NormalMap: string read FNormalMap write FNormalMap;

    { Override alpha channel type for diffuse texture.
      The meaning and allowed values for this are the same as for
      alphaChannel field for texture nodes, see
      https://castle-engine.io/x3d_extensions.php#section_ext_alpha_channel_detection .
      Empty value (default) doesn't change the alpha channel type
      (set in VRML/X3D or auto-detected). }
    property AlphaChannel: string read FAlphaChannel write FAlphaChannel;
  end;

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
        IncludePaths: TCastleStringList; // absolute URLs
        ExcludePaths: TCastleStringList;
        { necessary for Exclude with relative dirs, like "entites/*", to work }
        FBaseURL: string;
        FCompressedFormatsToGenerate: TTextureCompressionsToGenerate;
        GatheringResult: TCastleStringList;
        FSmallestScale: Cardinal;
        procedure LoadImageEvent(var URL: string);
        function IsAbsoluteURLMatchingRelativeMask(const URL, Mask: string): boolean;
      public
        constructor Create(const Element: TDOMElement; const BaseURL: string; const AnAutoProcessImageURLs: boolean);
        destructor Destroy; override;
        function TextureURLMatches(const URL: string): boolean;
        function AutoGeneratedTextures: TCastleStringList;
        property CompressedFormatsToGenerate: TTextureCompressionsToGenerate
          read FCompressedFormatsToGenerate;
        property SmallestScale: Cardinal read FSmallestScale;
      end;

      TAutoGeneratedTexturesList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TAutoGeneratedTextures>;
      TMaterialPropertyList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TMaterialProperty>;
    var
    FAutoGeneratedTexturesList: TAutoGeneratedTexturesList;
    FMaterialPropertyList: TMaterialPropertyList;
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

    { Find material properties for given texture basename.
      Returns @nil if no material properties are found
      (in particular, if @link(URL) was not set yet). }
    function FindTextureBaseName(const TextureBaseName: string): TMaterialProperty;

    { Get the URLs of all textures that should have automatically
      generated GPU-compressed and downscaled counterparts.
      Returns a list of absolute URLs.
      This actually searches on disk, right now, to find the texture list,
      applying the include/exclude rules specified in material_properties.xml file.

      This is to be used by "castle-engine auto-generate-textures"
      tool, or similar tools.

      Caller is responsible for freeing the returned TCastleStringList list. }
    function AutoGeneratedTextures: TCastleStringList;

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

uses SysUtils, XMLRead, StrUtils, Math,
  X3DNodes;

{ TSoundInfoList }

function TSoundInfoList.IndexOfName(const SoundName: String): Integer;
begin

end;

{ TSoundType }

function TSoundType.InternalInfo: TSoundInfo;
begin

end;

class operator TSoundType. = (const SoundType1, SoundType2: TSoundType
  ): boolean;
begin

end;

{ TMaterialProperty --------------------------------------------------------- }

procedure TMaterialProperty.LoadFromDOMElement(Element: TDOMElement; const BaseURL: string);
begin
end;

{ TMaterialProperties.TAutoGeneratedTextures -------------------------------- }

constructor TMaterialProperties.TAutoGeneratedTextures.Create(
  const Element: TDOMElement; const BaseURL: string;
  const AnAutoProcessImageURLs: boolean);
var
  ChildElement, CompressElement, ScaleElement: TDOMElement;
  TextureCompressionName: string;
begin
  inherited Create;
  FAutoProcessImageURLs := AnAutoProcessImageURLs;
  IncludePaths := TCastleStringList.Create;
  ExcludePaths := TCastleStringList.Create;
  FBaseURL := BaseURL;

  { read from XML }
end;

destructor TMaterialProperties.TAutoGeneratedTextures.Destroy;
begin
  FreeAndNil(IncludePaths);
  FreeAndNil(ExcludePaths);
  inherited;
end;

function TMaterialProperties.TAutoGeneratedTextures.IsAbsoluteURLMatchingRelativeMask(
  const URL, Mask: string): boolean;
var
  U: string;
begin
  U := PrefixRemove(FBaseURL, URL, PathsIgnoreCase);
  Result := IsWild(U, Mask, PathsIgnoreCase);
end;

function TMaterialProperties.TAutoGeneratedTextures.
  AutoGeneratedTextures: TCastleStringList;

  procedure Exclude(const ExcludePathMask: string; const URLs: TCastleStringList);
  var
    I: Integer;
  begin
    I := 0;
    while I < URLs.Count do
    begin
      // Writeln('Excluding ExcludePathMask ' + ExcludePathMask +
      //   ' from ' + PrefixRemove(ExtractURIPath(FBaseURL), URLs[I], PathsIgnoreCase));
      if IsAbsoluteURLMatchingRelativeMask(URLs[I], ExcludePathMask) then
        URLs.Delete(I) else
        Inc(I);
    end;
  end;

var
  I: Integer;
begin
  Result := TCastleStringList.Create;
  GatheringResult := Result;

  GatheringResult := nil;

  for I := 0 to ExcludePaths.Count - 1 do
    Exclude(ExcludePaths[I], Result);
end;

function TMaterialProperties.TAutoGeneratedTextures.TextureURLMatches(const URL: string): boolean;

  { Check is URL not excluded. }
  function CheckNotExcluded: boolean;
  var
    I: Integer;
  begin
    for I := 0 to ExcludePaths.Count - 1 do
      if IsAbsoluteURLMatchingRelativeMask(URL, ExcludePaths[I]) then
        Exit(false);
    Result := true;
  end;

var
  URLName, URLPath: string;
  I: Integer;
  IncludePath, IncludeMask: string;
  PathMatches: boolean;
begin
  Result := false;
  URLPath := URL;
  URLName := URL;
  for I := 0 to IncludePaths.Count - 1 do
  begin
    IncludePath := IncludePaths[I];
    IncludeMask := IncludePaths[I];
    PathMatches := AnsiSameText(IncludePath, URLPath); { assume PathsIgnoreCase=true }
    if PathMatches and IsWild(URLName, IncludeMask, PathsIgnoreCase) then
    begin
      Result := CheckNotExcluded;
      Exit;
    end;
  end;
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
  FMaterialPropertyList := TMaterialPropertyList.Create({ owns objects } true);
  FAutoGeneratedTexturesList := TAutoGeneratedTexturesList.Create({ owns objects } true);
end;

destructor TMaterialProperties.Destroy;
begin
  FreeAndNil(FAutoGeneratedTexturesList);
  FreeAndNil(FMaterialPropertyList);
  inherited;
end;

procedure TMaterialProperties.SetURL(const Value: string);
var
  Config: TXMLDocument;
  MaterialProperty: TMaterialProperty;
  Stream: TStream;
begin
  FURL := Value;

  FMaterialPropertyList.Clear;
  FAutoGeneratedTexturesList.Clear;

  if URL = '' then Exit;
end;

function TMaterialProperties.FindTextureBaseName(const TextureBaseName: string): TMaterialProperty;
var
  I: Integer;
begin
  for I := 0 to FMaterialPropertyList.Count - 1 do
    if SameText(FMaterialPropertyList[I].TextureBaseName, TextureBaseName) then
      Exit(FMaterialPropertyList[I]);
  Result := nil;
end;

function TMaterialProperties.AutoGeneratedTextures: TCastleStringList;
var
  S: TCastleStringList;
  I, J: Integer;
begin
  Result := TCastleStringList.Create;
  try
    for I := 0 to FAutoGeneratedTexturesList.Count - 1 do
    begin
      S := FAutoGeneratedTexturesList[I].AutoGeneratedTextures;
      try
        for J := 0 to S.Count - 1 do
        begin
          Result.Add(S[J]);
        end;
      finally FreeAndNil(S) end;
    end;
  except FreeAndNil(Result); raise end;
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
