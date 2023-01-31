{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Data for a 2D font initialized from a FreeType font file (TTextureFontData). }
unit CastleTextureFontData;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleVectors, CastleUnicode, CastleStringUtils, CastleImages,
  CastleInternalFreeType;

type
  { Raised by
    @link(TTextureFontData.Create) or
    @link(TTextureFont.Create TTextureFont.Create(URL, ...)) or
    @link(TTextureFont.Load TTextureFont.Load(URL, ...)) when
    the freetype library cannot be found, and thus font files cannot be read. }
  EFreeTypeLibraryNotFound = CastleInternalFreeType.EFreeTypeLibraryNotFound;

  { Data for a 2D font initialized from a FreeType font file, like ttf. }
  TTextureFontData = class
  public
    type
      { Information about a particular font glyph. }
      TGlyph = class
      public
        { How to shift the glyph with respect
          to the starting position when drawing. }
        X, Y: Integer;
        { How to advance the position for next glyph. }
        AdvanceX, AdvanceY: Integer;
        { Size of the glyph.
          Always Width and Height >= 0 (they are Cardinal type after all),
          but note that it is possible that Width = Height = 0
          (it commonly happens for space ' ' character). }
        Width, Height: Cardinal;
        { Position of the glyph on the image in TTextureFontData.Image. }
        ImageX, ImageY: Cardinal;
      end;
      { Map Unicode code to a TGlyph representation. }
      TGlyphDictionary = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<TUnicodeChar, TGlyph>)
      strict private
        FOwnsGlyphs: boolean;
        function GetItems(const AKey: TUnicodeChar): TGlyph;
        procedure SetItems(const AKey: TUnicodeChar; const AValue: TGlyph);
      public
        property OwnsGlyphs: boolean read FOwnsGlyphs write FOwnsGlyphs default true;
        { Access dictionary items.
          Setting this is allowed regardless if the key previously existed or not,
          in other words: setting this does AddOrSetValue, contrary to the ancestor TDictionary
          that only allows setting when the key already exists. }
        property Items [const AKey: TUnicodeChar]: TGlyph read GetItems write SetItems; default;
        constructor Create; reintroduce;
        destructor Destroy; override;
      end;
  private
    type
      TGlyphCharDictionary = array [Byte] of TGlyph;
    const
      MaxFallbackGlyphWarnings = 10;
    var
      FAntiAliased: boolean;
      FSize: Integer;
      { For optimization of rendering normal 8-bit fonts (like standard ASCII
        text), we keep glyphs with index < 256 listed in TGlyphCharDictionary.
        Only the glyphs with index >= 256 are kept on extra TGlyphDictionary. }
      { Non-nil only for filled glyphs. }
      FGlyphsByte: TGlyphCharDictionary;
      FGlyphsExtra: TGlyphDictionary;
      FImage: TGrayscaleImage;
      MeasureDone: boolean;
      FRowHeight, FRowHeightBase, FDescend: Integer;
      FFirstExistingGlyph: TGlyph;
      FFirstExistingGlyphChar: TUnicodeChar;
      { If the requested glyph doesn't exit, @link(Glyph) will use this one
        as a fallback. }
      FFallbackGlyph: TGlyph;
      FFallbackGlyphChar: TUnicodeChar;
      FUseFallbackGlyph: Boolean;
      FallbackGlyphWarnings: Integer;

    procedure Measure(out ARowHeight, ARowHeightBase, ADescend: Integer);
    procedure CalculateFallbackGlyph;
    procedure MakeFallbackWarning(const C:TUnicodeChar);
  public
    { Create by reading a FreeType font file, like ttf.

      Providing charaters list as @nil means that we only create glyphs
      for SimpleAsciiCharacters, which includes only the basic ASCII characters.
      The ACharacters instance @italic(does not) become owned by this object,
      so remember to free it after calling this constructor.

      @raises EFreeTypeLibraryNotFound If the freetype library is not installed. }
    constructor Create(const URL: string;
      const ASize: Integer; const AnAntiAliased: boolean;
      ACharacters: TUnicodeCharList = nil);

    { Create from a ready data for glyphs and image.
      Useful when font data is embedded inside the Pascal source code.
      AGlyphs instance, and AImage instance, become owned by this class. }
    constructor CreateFromData(const AGlyphs: TGlyphDictionary;
      const AImage: TGrayscaleImage;
      const ASize: Integer; const AnAntiAliased: boolean);
    destructor Destroy; override;

    property AntiAliased: boolean read FAntiAliased;
    property Size: Integer read FSize;

    { Read-only information about a glyph for given character.

      When AllowUsingFallbackGlyph and UseFallbackGlyph (both are @true
      by default) then we always return non-nil glyph.
      If the desired glyph was not really present, we make a warning
      (using WritelnWarning) and return a fallback glyph.

      When not (AllowUsingFallbackGlyph and UseFallbackGlyph)
      then we return @nil for a missing glyph.
      Glyph may be missing because it was not requested at constructor,
      or because it doesn't exist in the font data. }
    function Glyph(const C: TUnicodeChar;
      const AllowUsingFallbackGlyph: Boolean = true): TGlyph;

    { When a glyph (picture of a particular character) in a font doesn't exist,
      by default we make a warning (using WritelnWarning) and use a fallback
      glyph, like "?". This lets user know that some character is there.

      Set this to @false to just silently omit a missing glyph.
      The @link(Glyph) method will just return (silently) @nil in this case. }
    property UseFallbackGlyph: Boolean
      read FUseFallbackGlyph write FUseFallbackGlyph default true;

    property Image: TGrayscaleImage read FImage;

    { List all characters for which glyphs are actually loaded.
      @link(Glyph) will answer non-nil exactly for these characters.
      The resulting list instance is owned by caller, so take care to free it. }
    function LoadedGlyphs: TUnicodeCharList;

    function TextWidth(const S: string): Integer;
    function TextHeight(const S: string): Integer;
    { The height (above the baseline) of the text.
      This doesn't take into account height of the text below the baseline
      (for example letter "y" has the tail below the baseline in most fonts). }
    function TextHeightBase(const S: string): Integer;
    function TextMove(const S: string): TVector2Integer;

    { Height of a row of text in this font.
      This may be calculated as simply @code(TextHeight('Wy')) for most
      normal fonts. }
    function RowHeight: Integer;

    { Height (above the baseline) of a row of text in this font.
      Similar to TextHeightBase and TextHeight,
      note that RowHeightBase is generally smaller than RowHeight,
      because RowHeightBase doesn't care how low the letter may go below
      the baseline. }
    function RowHeightBase: Integer;

    { How low the text may go below the baseline. }
    function Descend: Integer;
  end;

implementation

uses Classes, SysUtils, CastleInternalFtFont,
  CastleLog, CastleUtils, CastleURIUtils, CastleFilesUtils, CastleDownload;

{ TTextureFontData.TGlyphDictionary ------------------------------------------ }

constructor TTextureFontData.TGlyphDictionary.Create;
begin
  inherited;
  FOwnsGlyphs := true;
end;

destructor TTextureFontData.TGlyphDictionary.Destroy;
var
  G: TGlyph;
begin
  if OwnsGlyphs then
    for G in Values do
      G.Free;
  Clear;
  inherited;
end;

function TTextureFontData.TGlyphDictionary.GetItems(const AKey: TUnicodeChar): TGlyph;
begin
  Result := inherited Items[AKey];
end;

procedure TTextureFontData.TGlyphDictionary.SetItems(const AKey: TUnicodeChar; const AValue: TGlyph);
begin
  AddOrSetValue(AKey, AValue);
end;

{ TTextureFontData ----------------------------------------------------------------- }

constructor TTextureFontData.Create(const URL: string;
  const ASize: Integer; const AnAntiAliased: boolean;
  ACharacters: TUnicodeCharList);
var
  FontId: Integer;

  function GetGlyphInfo(const C: TUnicodeChar): TGlyph;
  var
    Bitmaps: TStringBitmaps;
    Bitmap: PFontBitmap;
  begin
    if AntiAliased then
      Bitmaps := FontMgr.GetStringGray(FontId, UnicodeToUTF8(C), Size) else
      Bitmaps := FontMgr.GetString(FontId, UnicodeToUTF8(C), Size);

    try
      if Bitmaps.Count = 0 then
      begin
        WritelnWarning('Font', Format('Font "%s" does not contain glyph for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit(nil);
      end;

      Bitmap := Bitmaps.Bitmaps[0];
      if Bitmaps.Count > 1 then
        WritelnWarning('Font', Format('Font "%s" contains a sequence of glyphs (more than a single glyph) for a single character "%s" (index %d)',
          [URL, C, Ord(C)]));
      if (Bitmap^.Width < 0) or (Bitmap^.Height < 0) then
      begin
        WritelnWarning('Font', Format('Font "%s" contains a glyphs with Width or Height < 0 for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit(nil);
      end;

      Result := TGlyph.Create;
      Result.Width    := Bitmap^.Width;
      Result.Height   := Bitmap^.Height;
      Result.X        := -Bitmap^.X;
      Result.Y        := Bitmap^.Height - 1 + Bitmap^.Y;
      Result.AdvanceX := Bitmap^.AdvanceX shr 10; // 64 * 16, looks like this is just magic for freetype
      Result.AdvanceY := Bitmap^.AdvanceY shr 10; // 64 * 16, looks like this is just magic for freetype
    finally FreeAndNil(Bitmaps) end;
  end;

  { Copy glyph data for character C (assuming it is Ok, that is GetGlyphInfo
    returned non-nil for this) to the Image (at position ImageX, ImageY). }
  procedure GetGlyphData(const C: TUnicodeChar; const ImageX, ImageY: Cardinal);
  var
    Bitmaps: TStringBitmaps;
    Bitmap: PFontBitmap;

    { Extracting data from glyph with Pitch, like in TFreeTypeFont.DrawChar. }
    procedure DrawChar;
    var
      B, RX, RY: Integer;
    begin
      B := 0;
      for RY := 0 to Bitmap^.Height - 1 do
      begin
        for RX := 0 to Bitmap^.Width - 1 do
          Image.PixelPtr(ImageX + RX, ImageY + Bitmap^.Height - 1 - RY)^ := Bitmap^.Data^[B + RX];
        Inc(B, Bitmap^.Pitch);
      end;
    end;

    { Extracting data with Pitch, like in TFreeTypeFont.DrawCharBW. }
    procedure DrawCharBW;
    const
      Bits: array [0..7] of Byte = (128,64,32,16,8,4,2,1);
    var
      RB: Byte;
      RX, RY, B, L: Integer;
    begin
      B := 0;
      for RY := 0 to Bitmap^.Height - 1 do
      begin
        L := 0;
        for RX := 0 to Bitmap^.Width - 1 do
        begin
          RB := RX mod 8;
          if (Bitmap^.Data^[B + L] and Bits[RB]) <> 0 then
            Image.PixelPtr(ImageX + RX, ImageY + Bitmap^.Height - 1 - RY)^ := 255;
          if RB = 7 then
            Inc(L);
        end;
        Inc(B, Bitmap^.Pitch);
      end;
    end;

  begin
    if AntiAliased then
      Bitmaps := FontMgr.GetStringGray(FontId, UnicodeToUTF8(C), Size) else
      Bitmaps := FontMgr.GetString(FontId, UnicodeToUTF8(C), Size);
    try
      Bitmap := Bitmaps.Bitmaps[0];
      if (Bitmap^.Pitch < 0) then
      begin
        WritelnWarning('Font', Format('Font "%s" contains a glyphs with Pitch < 0 for character "%s" (index %d)',
          [URL, C, Ord(C)]));
        Exit;
      end;
      if AntiAliased then
        DrawChar else
        DrawCharBW;
    finally FreeAndNil(Bitmaps) end;
  end;

const
  { Separate the glyphs for safety, to avoid pulling in colors
    from neighboring letters when drawing (floating point errors could in theory
    make small errors moving us outside of the desired pixel). }
  GlyphPadding = 2;

var
  FileName: string;
  GlyphInfo: TGlyph;
  GlyphsCount, ImageSize: Cardinal;
  MaxWidth, MaxHeight, ImageX, ImageY: Cardinal;
  C: TUnicodeChar;
  TemporaryCharacters: boolean;
  Cache: TStream;
  CacheURL: String;
  IsCachedFile: Boolean;
begin
  inherited Create;
  FSize := ASize;
  FAntiAliased := AnAntiAliased;
  FUseFallbackGlyph := true;

  CastleInternalFtFont.InitEngine;
  { By default TFontManager uses DefaultResolution that is OS-dependent
    and does not really have any good reasoninig?
    We set 0, letting FreeType library use good default,
    http://www.freetype.org/freetype2/docs/tutorial/step1.html ,
    and in effect Size is in nice pixels by default. }
  FontMgr.Resolution := 0;
  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
  begin
    Cache := Download(URL);
    try
      CacheURL := ApplicationConfig('cache_' + ExtractURIName(URL));
      StreamSaveToFile(Cache, CacheURL);
      FileName := URIToFilenameSafe(CacheURL);
      IsCachedFile := true;
    finally
      Cache.Free;
    end;
  end
  else IsCachedFile := false;

  FontId := FontMgr.RequestFont(FileName);

  if IsCachedFile then
    CheckDeleteFile(FileName, true);

  TemporaryCharacters := ACharacters = nil;
  if TemporaryCharacters then
  begin
    ACharacters := TUnicodeCharList.Create;
    ACharacters.Add(SimpleAsciiCharacters);
  end;

  try
    FGlyphsExtra := TGlyphDictionary.Create;

    GlyphsCount := 0;
    MaxWidth    := 0;
    MaxHeight   := 0;
    for C in ACharacters do
    begin
      GlyphInfo := GetGlyphInfo(C);
      if C <= High(FGlyphsByte) then
        FGlyphsByte[C] := GlyphInfo
      else
        FGlyphsExtra[C] := GlyphInfo;
      if GlyphInfo <> nil then
      begin
        if FFirstExistingGlyph = nil then
        begin
          FFirstExistingGlyph := GlyphInfo;
          FFirstExistingGlyphChar := C;
        end;
        Inc(GlyphsCount);
        MaxVar(MaxWidth , GlyphInfo.Width);
        MaxVar(MaxHeight, GlyphInfo.Height);
      end;
    end;

    if GlyphsCount = 0 then
      raise Exception.Create('Cannot create a font with no glyphs');

    MaxWidth := MaxWidth + GlyphPadding;
    MaxHeight := MaxHeight + GlyphPadding;

    ImageSize := 8;
    while (ImageSize div MaxHeight) * (ImageSize div MaxWidth) < GlyphsCount do
      ImageSize := ImageSize * 2;

    WritelnLog('Font', 'Creating image %dx%d to store glyphs of font "%s" (%d glyphs, max glyph size (including %d pixel padding) is %dx%d)',
      [ImageSize, ImageSize, URL, GlyphsCount, GlyphPadding, MaxWidth, MaxHeight]);

    FImage := TGrayscaleImage.Create(ImageSize, ImageSize);
    Image.Clear(0);
    Image.TreatAsAlpha := true;
    Image.URL := URL;

    ImageX := 0;
    ImageY := 0;
    for C in ACharacters do
    begin
      GlyphInfo := Glyph(C, false);
      if GlyphInfo <> nil then
      begin
        GlyphInfo.ImageX := ImageX;
        GlyphInfo.ImageY := ImageY;

        GetGlyphData(C, ImageX, ImageY);

        ImageX := ImageX + MaxWidth;
        if ImageX + MaxWidth >= ImageSize then
        begin
          ImageX := 0;
          ImageY := ImageY + MaxHeight;
        end;
      end;
    end;

    // Debug: SaveImage(Image, '/tmp/a.png');
  finally
    if TemporaryCharacters then
      FreeAndNil(ACharacters);
  end;

  CalculateFallbackGlyph;
end;

constructor TTextureFontData.CreateFromData(const AGlyphs: TGlyphDictionary;
  const AImage: TGrayscaleImage;
  const ASize: Integer; const AnAntiAliased: boolean);
var
  C: TUnicodeChar;
  GlyphPair: TGlyphDictionary.TDictionaryPair;
begin
  inherited Create;
  FSize := ASize;
  FAntiAliased := AnAntiAliased;
  FUseFallbackGlyph := true;

  { split AGlyphs into FGlyphsByte and FGlyphsExtra }
  FGlyphsExtra := TGlyphDictionary.Create;
  for GlyphPair in AGlyphs do
  begin
    C := GlyphPair.Key;
    if C <= High(FGlyphsByte) then
      FGlyphsByte[C] := GlyphPair.Value
    else
      FGlyphsExtra[C] := GlyphPair.Value;

    if FFirstExistingGlyph = nil then
    begin
      FFirstExistingGlyph := GlyphPair.Value;
      FFirstExistingGlyphChar := C;
    end;
  end;
  AGlyphs.OwnsGlyphs := false;
  AGlyphs.Free; // we own AGlyphs, for now we just free them

  FImage := AImage;

  CalculateFallbackGlyph;
end;

destructor TTextureFontData.Destroy;
var
  C: Byte;
begin
  FreeAndNil(FGlyphsExtra);
  for C in Byte do
    FreeAndNil(FGlyphsByte[C]);
  FreeAndNil(FImage);
  inherited;
end;

procedure TTextureFontData.CalculateFallbackGlyph;

  function TryFallback(const C: TUnicodeChar): Boolean;
  begin
    FFallbackGlyph := Glyph(C, false);
    FFallbackGlyphChar := C;
    Result := FFallbackGlyph <> nil;
  end;

begin
  if not TryFallback(Ord('?')) then
    if not TryFallback(Ord('_')) then
      if not TryFallback(Ord('.')) then
      begin
        FFallbackGlyph := FFirstExistingGlyph;
        FFallbackGlyphChar := FFirstExistingGlyphChar;
      end;
end;

function TTextureFontData.Glyph(const C: TUnicodeChar;
  const AllowUsingFallbackGlyph: Boolean): TGlyph;
begin
  if C <= High(FGlyphsByte) then
    Result := FGlyphsByte[C]
  else
  if not FGlyphsExtra.TryGetValue(C, Result) then
    Result := nil;

  if (Result = nil) and AllowUsingFallbackGlyph and UseFallbackGlyph then
  begin
    Result := FFallbackGlyph;
    MakeFallbackWarning(C);
  end;
end;

procedure TTextureFontData.MakeFallbackWarning(const C:TUnicodeChar);
begin
  if FallbackGlyphWarnings < MaxFallbackGlyphWarnings then
  begin
    Inc(FallbackGlyphWarnings);
    WritelnWarning('Font is missing glyph for character %s (UTF-8 number %d)',
      [UnicodeToUTF8(C), C]);
    if FallbackGlyphWarnings = MaxFallbackGlyphWarnings then
      WritelnWarning('No further warnings about missing glyphs will be reported for this font (to avoid slowing down the application by flooding the log with warnings)');
  end;
end;

function TTextureFontData.LoadedGlyphs: TUnicodeCharList;
var
  C: TUnicodeChar;
begin
  Result := TUnicodeCharList.Create;
  for C := 0 to High(FGlyphsByte) do
    if FGlyphsByte[C] <> nil then
      Result.Add(C);
  for C in FGlyphsExtra.Keys do
    Result.Add(C);
end;

function TTextureFontData.TextWidth(const S: string): Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
      Result := Result + G.AdvanceX;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

function TTextureFontData.TextHeight(const S: string): Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  MinY, MaxY, YOrigin: Integer;
  G: TTextureFontData.TGlyph;
begin
  MinY := 0;
  MaxY := 0;

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
    begin
      YOrigin := G.Y;
      MinVar(MinY, -YOrigin);
      MaxVar(MaxY, G.Height - YOrigin);
    end;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
  Result := MaxY - MinY;
end;

function TTextureFontData.TextMove(const S: string): TVector2Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  G: TTextureFontData.TGlyph;
begin
  Result := TVector2Integer.Zero;

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
    begin
      Result.Data[0] := Result.Data[0] + G.AdvanceX;
      Result.Data[1] := Result.Data[1] + G.AdvanceY;
    end;

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

function TTextureFontData.TextHeightBase(const S: string): Integer;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
  G: TTextureFontData.TGlyph;
begin
  Result := 0;
  { This is just like TextHeight implementation, except we only
    calculate (as Result) the MaxY value (assuming that MinY is zero). }

  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    G := Glyph(C);
    if G <> nil then
      MaxVar(Result, G.Height - G.Y);

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

procedure TTextureFontData.Measure(out ARowHeight, ARowHeightBase, ADescend: Integer);
begin
  ARowHeight := TextHeight('Wy');
  ARowHeightBase := TextHeightBase('W');
  ADescend := TextHeight('y') - TextHeight('a');
end;

function TTextureFontData.RowHeight: Integer;
begin
  if not MeasureDone then
  begin
    Measure(FRowHeight, FRowHeightBase, FDescend);
    MeasureDone := true;
  end;
  Result := FRowHeight;
end;

function TTextureFontData.RowHeightBase: Integer;
begin
  if not MeasureDone then
  begin
    Measure(FRowHeight, FRowHeightBase, FDescend);
    MeasureDone := true;
  end;
  Result := FRowHeightBase;
end;

function TTextureFontData.Descend: Integer;
begin
  if not MeasureDone then
  begin
    Measure(FRowHeight, FRowHeightBase, FDescend);
    MeasureDone := true;
  end;
  Result := FDescend;
end;

end.
