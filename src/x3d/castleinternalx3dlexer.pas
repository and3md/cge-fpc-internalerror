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

{ VRML/X3D classic lexer (TX3DLexer). }
unit CastleInternalX3DLexer;

{$I ../base/castleconf.inc}

interface

uses SysUtils, Classes, Math;

type
  { Valid keywords for all VRML / X3D versions. }
  TX3DKeyword = (vkDEF, vkEXTERNPROTO, vkFALSE, vkIS, vkNULL, vkPROTO, vkROUTE,
    vkTO, vkTRUE, vkUSE, vkEventIn, vkEventOut, vkExposedField, vkField,
    { Below keywords are X3D-only as far as specification is concerned.
      However, we decide to support IMPORT/EXPORT for older VRML versions
      too (the downside is that you cannot name your nodes like this,
      but the upside is that you can use these features in all VRML versions.) } { }
    vkAS, vkEXPORT, vkIMPORT,
    { X3D-only keywords below } { }
    vkCOMPONENT, vkMETA, vkPROFILE, vkUNIT,
    vkInputOnly, vkOutputOnly, vkInputOutput, vkInitializeOnly);

  TX3DKeywords = set of TX3DKeyword;

const
  VRML10Keywords = [vkDEF, vkUSE, vkFALSE, vkTRUE];
  VRML20Keywords = [vkDEF .. vkIMPORT] +
    { META accepted as Titania demos use it } [vkMETA];
  X3DKeywords = [Low(TX3DKeyword) .. High(TX3DKeyword)] -
    [vkEventIn, vkEventOut, vkExposedField, vkField];

type
  { Lexer token. }
  TX3DToken = (
    vtKeyword,
    vtName,

    { Symbols for all VRML versions.
      @groupBegin }
    vtOpenCurlyBracket, vtCloseCurlyBracket,
    vtOpenSqBracket, vtCloseSqBracket,
    { @groupEnd }

    { Symbols below are only for VRML <= 1.0.
      In VRML 2.0, they are no longer valid symbols
      (comma is even considered a whitespace).
      They will never be returned by lexer when reading VRML >= 2.0 files.

      @groupBegin }
    vtOpenBracket, vtCloseBracket, vtBar, vtComma,
    { @groupEnd }

    { Symbols below are only for VRML >= 2.0.
      They will never be returned by lexer when reading VRML < 2.0 files.
      @groupBegin }
    vtPeriod,
    { @groupEnd }

    { Symbols below are only for VRML >= 3.0, that is X3D.
      They will never be returned by lexer when reading VRML < 3.0 files.
      @groupBegin }
    vtColon,
    { @groupEnd }

    { Back to version-neutral tokens, suitable for all VRML / X3D versions.
      @groupBegin }
    vtFloat, vtInteger, vtString,
    { @groupEnd }

    { vtEnd means that we're standing at the end of stream, no more tokens.
      From this point, further reads using NextToken from stream will
      always result in vtEnd (they will not raise an error). }
    vtEnd);
  TX3DTokens = set of TX3DToken;

  { Any error related to VRML/X3D. }
  EX3DError = class(Exception);

  EX3DGzipCompressed = class(EX3DError);

const
  TokenNumbers : TX3DTokens = [vtFloat, vtInteger];

type
  TX3DEncoding = (xeClassic, xeXML);

  TX3DVersion = object
    Major, Minor: Integer;
    function FileExtension(const Encoding: TX3DEncoding;
      const ForceConvertingToX3D: boolean = false): string;
    { File filters if you want to save a file using Save3D. }
    function FileFilters(const Encoding: TX3DEncoding;
      const ForceConvertingToX3D: boolean = false): string;
  end;

  { VRML/X3D (classic encoding) lexer.

    The lexer always "looks" (i.e. contains in Token and TokenXxx fields)
    at the next not yet interpreted token.

    Remember that VRML is case-sensitive, so TokenName and TokenString
    should be compared in case-sensitive manner. Also note that
    for VRML/X3D >= 2.0 these fields contain UTF-8 encoded strings.

    Note that this lexer can read only from @link(TPeekCharStream), not just
    from any TStream. You may have to wrap your stream in some
    @link(TPeekCharStream) descendant (for example create TFileStream
    and then wrap it inside @link(TBufferedReadStream)). }
  TX3DLexer = class
  public
    { Standard constructor.
      After constructor call, @link(Version) is already set,
      it's checked that file is not compressed by gzip, and the first
      Token is already read.
      @raises(EX3DGzipCompressed If the Stream starts with gzip file header.) }
    constructor Create(AOwnsStream: boolean);

    destructor Destroy; override;
  end;

  { Error when reading VRML/X3D classic encoding. }
  EX3DClassicReadError = class(EX3DError)
  public
    { Standard constructor.

      Lexer instance must be valid for this call, but not longer.
      That is, you can free the lexer after this constructor finished,
      it doesn't need to be valid for the whole lifetime of this object. }
    constructor Create(Lexer: TX3DLexer; const s: string);
  end;

  { Error when reading VRML/X3D. For now, just equal to EX3DClassicReadError,
    later may be an ancestor to EX3DClassicReadError.
    Problems in other encodings (XML) are for now always turned into warnings. }
  EX3DReadError = EX3DClassicReadError;

  EX3DLexerError = class(EX3DClassicReadError);
  EX3DParserError = class(EX3DClassicReadError);

const
  X3DKeywordsName: array [TX3DKeyword] of string = (
    'DEF', 'EXTERNPROTO', 'FALSE', 'IS', 'NULL', 'PROTO', 'ROUTE',
    'TO', 'TRUE', 'USE', 'eventIn', 'eventOut', 'exposedField', 'field',
    'AS', 'EXPORT', 'IMPORT',
    'COMPONENT', 'META', 'PROFILE', 'UNIT',
    'inputOnly', 'outputOnly', 'inputOutput', 'initializeOnly'
    );

implementation

uses X3DFields;// remove this X3DFields (X3DLoadInternalUtils was here before) include to get rid of internal error

const
  X3DTokenNames: array [TX3DToken] of string = (
    'keyword', 'name',
    '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","', '"."', '":"',
    'float', 'integer', 'string', 'end of stream');

function ArrayPosX3DKeywords(const s: string; out Index: TX3DKeyword): boolean;
var
  I: TX3DKeyword;
begin
  for I := Low(X3DKeywords) to High(X3DKeywords) do
    if X3DKeywordsName[I] = s then
    begin
      Index := I;
      Result := true;
      Exit;
    end;
  Result := false;
end;

{ TX3DVersion --------------------------------------------------------------- }

function TX3DVersion.FileExtension(const Encoding: TX3DEncoding;
  const ForceConvertingToX3D: boolean): string;
begin
  if Encoding = xeXML then
    Result := '.x3d' else
  if (Major >= 3) or ForceConvertingToX3D then
    Result := '.x3dv' else
    Result := '.wrl';
end;

function TX3DVersion.FileFilters(const Encoding: TX3DEncoding;
  const ForceConvertingToX3D: boolean): string;
const
  SaveVRMLClassic_FileFilters =
  'All files|*|' +
  '*VRML (*.wrl)|*.wrl|' +
  'VRML (compressed) (*.wrz, *.wrl.gz)|*.wrz;*.wrl.gz';
  SaveX3DClassic_FileFilters =
  'All files|*|' +
  '*X3D classic (*.x3dv)|*.x3dv|' +
  'X3D classic (compressed) (*.x3dvz, *.x3dv.gz)|*.x3dvz;*.x3dv.gz';
  SaveX3DXml_FileFilters =
  'All files|*|' +
  '*X3D XML (*.x3d)|*.x3d|' +
  'X3D XML (compressed) (*.x3dz, *.x3d.gz)|*.x3dz;*.x3d.gz';
begin
  if Encoding = xeXML then
    Result := SaveX3DXml_FileFilters else
  if (Major >= 3) or ForceConvertingToX3D then
    Result := SaveX3DClassic_FileFilters else
    Result := SaveVRMLClassic_FileFilters;
end;

{ TX3DLexer ------------------------------------------------------------- }

constructor TX3DLexer.Create(AOwnsStream: boolean);
begin
end;

destructor TX3DLexer.Destroy;
begin
  inherited;
end;

{ Exceptions ----------------------------------------------------------------- }

constructor EX3DClassicReadError.Create(Lexer: TX3DLexer; const s: string);
begin
  inherited Create(Format('Error at line %d column %d: ',
    [111, 1111]) + S);
end;

end.
