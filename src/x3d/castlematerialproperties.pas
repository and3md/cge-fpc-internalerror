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

  TAutoGeneratedTextures = class
  strict private
    FAutoProcessImageURLs: boolean;
    { necessary for Exclude with relative dirs, like "entites/*", to work }
    FBaseURL: string;
    procedure LoadImageEvent(var URL: string);
  public
    constructor Create(const Element: TDOMElement; const BaseURL: string; const AnAutoProcessImageURLs: boolean);
    function TextureURLMatches(const URL: string): boolean;
  end;

  TAutoGeneratedTexturesList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TAutoGeneratedTextures>;

implementation

uses SysUtils, StrUtils, Math,
  X3DNodes;

{ TAutoGeneratedTextures -------------------------------- }

constructor TAutoGeneratedTextures.Create(
  const Element: TDOMElement; const BaseURL: string;
  const AnAutoProcessImageURLs: boolean);
begin
  inherited Create;
  FAutoProcessImageURLs := AnAutoProcessImageURLs;
  FBaseURL := BaseURL;

  { read from XML }
end;

function TAutoGeneratedTextures.TextureURLMatches(const URL: string): boolean;

  { Check is URL not excluded. }
  function CheckNotExcluded: boolean;
  begin
    Result := true;
  end;

begin
  Result := false;
end;

procedure TAutoGeneratedTextures.LoadImageEvent(
  var URL: string);

  procedure ReplaceURL;
  begin
  end;

begin
  if TextureURLMatches(URL) then
    ReplaceURL;
end;


end.
