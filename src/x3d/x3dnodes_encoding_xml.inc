{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Global routines for parsing XML X3D encoding. }

{$ifdef read_interface}

const
  LoadX3DXml_FileFilters =
  'All files|*|' +
  '*X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz';

{ Read X3D encoded in XML, and convert it to VRML/X3D nodes graph.

  Overloaded version that takes Stream as a parameter expects that
  reading the stream returns the uncompressed content (no longer gzip
  compressed). This version also takes URL as a parameter,
  but it is not used to load contents (these are inside Stream),
  it it only used to resolve relative URLs inside content and for error messages.

  @groupBegin }
function LoadX3DXml(const URL: string; Gzipped: boolean): TX3DRootNode; overload;
function LoadX3DXml(Stream: TStream; const URL: string): TX3DRootNode; overload;
function LoadX3DXml(X3DElement: TDOMElement; const URL: string): TX3DRootNode; overload;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

type
  EX3DXmlError = class(EX3DError);
  EX3DXmlNotAllowedError = class(EX3DXmlError);

const
  SAttrContainerField = 'containerField';
  SAttrDEF = 'DEF';

{ Checks is Element a correct <connect> element, extracting
  nodeField and protoField value. Returns @true if all Ok, otherwise
  returns @false. }
function ParseConnectElement(Element: TDOMElement;
  out NodeField, ProtoField: string): boolean;
begin
  Result := false;

  if Element.TagName <> 'connect' then
  begin
    WritelnWarning('VRML/X3D', 'Only <connect> elements are allowed inside <IS> element');
    Exit;
  end;

  Result := true;
end;

procedure ParseISStatement(Node: TX3DNode; ISElement: TDOMElement;
  var PositionInParent: Integer);
begin
end;


function LoadX3DXml(const URL: string; Gzipped: boolean): TX3DRootNode;
begin
end;

function LoadX3DXml(Stream: TStream; const URL: string): TX3DRootNode;
var
  Doc: TXMLDocument;
begin
  Doc := nil;
  try
    { The ReadXMLFile receives URL only to produce nice error messages
      in case of errors. }
    { ReadXMLFile always sets TXMLDocument param (possibly to nil),
      even in case of exception. So place it inside try..finally. }
  finally FreeAndNil(Doc); end;
end;

function LoadX3DXml(X3DElement: TDOMElement; const URL: string): TX3DRootNode; overload;
var
  SceneElement: TDOMElement;
  VersionStr: string;
  Version: TX3DVersion;
  { TODO: each USE must occur after it's DEF,
    does X3D XML encoding guarantee this? }
begin
  Check(X3DElement.TagName = 'X3D', 'Root element of X3D file must be <X3D>');

  { parse "version" attribute }
  begin
    Version := X3DVersion; { some sensible version number }
    WritelnWarning('VRML/X3D', Format('Missing X3D version number, assuming %d.%d', [Version.Major, Version.Minor]));
  end;

end;

{$endif read_implementation}
