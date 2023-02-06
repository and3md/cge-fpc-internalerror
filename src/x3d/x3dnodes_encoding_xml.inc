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

function ParseXMLNode(Element: TDOMElement;
  out ContainerField: string; Reader: TX3DReaderNames;
  NilIfUnresolvedUSE: boolean): TX3DNode; forward;
function ParseStatements(Element: TDOMElement;
  FileTopLevel: boolean;
  X3DHeaderElement: TDOMElement; Reader: TX3DReaderNames): TX3DRootNode; overload; forward;

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

(*
  Parse VRML node. This parses normal node (with optional DEF),
  or node with USE attribute.

  It's somewhat similar to classic VRML ParseNode.
  (Admittedly, it was even implemented by copying and modifying
  classic ParseNode :) ).

  If we will find USE clause but node name will be unknown, the normal
  behavior (when NilIfUnresolvedUSE = @false, default) is to raise
  EX3DXmlNotAllowedError (just like in case of many other errors).
  However, this is a particular parsing error, because we can probably
  pretty safely continue parsing, ignoring this error.
  So if you pass NilIfUnresolvedUSE = @true, this function will do
  WritelnWarning and simply return @nil.

  @raises(EX3DXmlNotAllowedError On various not-allowed errors.)

  @raises(EX3DXmlUnknownNodeNotAllowed On a special parsing error:
    we got unknown node name, and AllowedNodes was @false.

    We have a special error class for this, because in some cases
    it means that actually the unknown node name could be also
    unknown field / proto etc. name, so error message for the user should
    be better.)
*)
function ParseXMLNode(Element: TDOMElement;
  out ContainerField: string; Reader: TX3DReaderNames;
  NilIfUnresolvedUSE: boolean): TX3DNode;

begin
  Result := nil;
end;

{ This parses a sequence of X3D statements: any number of nodes,
  (external) protypes, routes.
  This is good to use to parse whole VRML file (when FileTopLevel = true),
  or a (non-external) prototype content (when FileTopLevel = false).

  It's somewhat similar to classic ParseStatements.
  (Admittedly, it was even implemented by copying and modifying
  classic ParseStatements :) ). }
function ParseStatements(Element: TDOMElement;
  FileTopLevel: boolean;
  X3DHeaderElement: TDOMElement; Reader: TX3DReaderNames): TX3DRootNode; overload;
var
  PositionInParent: Integer;

  { Create root group node. }
  function CreateRootNode: TX3DRootNode;
  begin
    Result := TX3DRootNode.Create('', Reader.BaseUrl);
    Result.HasForceVersion := true;
    Result.ForceVersion := Reader.Version;
  end;

  procedure ParseProfile;
  begin
    { We allow PROFILE to be omitted.
      Actually, we do not use profile for anything right now. }
    WritelnWarning('VRML/X3D', 'X3D "profile" attribute missing');
  end;

  procedure ParseHead;

    procedure ParseMeta(Element: TDOMElement);
    var
      MetaName, MetaContent: string;
    begin
      MetaName := '';
      MetaContent := '';
      Result.Meta[MetaName] := MetaContent;
    end;

    procedure ParseComponent(Element: TDOMElement);
    var
      ComponentName: string;
      ComponentLevel: Integer;
    begin
      WritelnWarning('VRML/X3D', Format('X3D XML: <component> element without required "name" attribute',
          [Element.TagName]));
    end;

    procedure ParseUnit(Element: TDOMElement);
    var
      Category, Name: string;
      ConversionFactor: Float;
    begin
    end;

  var
    Head: TDOMElement;
  begin
    Head := nil;
    if Head = nil then Exit;

    Result.Scale := Reader.LengthConversionFactor;
  end;

  procedure ParseStatement(Element: TDOMElement);

    { You can safely assume that Element.TagName
      indicates proto or externproto. }
    procedure ParseProtoStatement;
    var
      Proto: TX3DPrototypeBase;
    begin
      if Element.TagName = 'ProtoDeclare' then
        Proto := TX3DPrototype.Create else
        Proto := TX3DExternalPrototype.Create;

      Proto.PositionInParent := PositionInParent;

      Result.AddPrototype(Proto);

      Proto.ParseXML(Element, Reader);
    end;

    procedure ParseRouteStatement;
    var
      Route: TX3DRoute;
    begin
      Route := TX3DRoute.Create;
      Route.PositionInParent := PositionInParent;
      Result.AddRoute(Route);
      Route.ParseXML(Element, Reader);
    end;

    procedure ParseImportStatement;
    var
      Import: TX3DImport;
    begin
      Import := TX3DImport.Create;
      Import.PositionInParent := PositionInParent;
      Result.AddImport(Import);
      Import.ParseXML(Element, Reader);
    end;

    procedure ParseExportStatement;
    var
      ExportItem: TX3DExport;
    begin
      ExportItem := TX3DExport.Create;
      ExportItem.PositionInParent := PositionInParent;
      Result.AddExport(ExportItem);
      ExportItem.ParseXML(Element, Reader);
    end;

    procedure ParseNodeStatement;
    var
      NewNode: TX3DNode;
      ContainerFieldDummy: string;
    begin
      NewNode := ParseXMLNode(Element, ContainerFieldDummy, Reader, false);
      NewNode.PositionInParent := PositionInParent;
      Result.FdChildren.Add(NewNode);
      { for prototypes, do not check NewNode class, as anything is valid. }
      if FileTopLevel then
        Result.TopLevelCheckChild(NewNode);
    end;

  begin
    if (Element.TagName = 'ProtoDeclare') or
       (Element.TagName = 'ExternProtoDeclare') then
      ParseProtoStatement else
    if Element.TagName = 'ROUTE' then
      ParseRouteStatement else
    if Element.TagName = 'IMPORT' then
      ParseImportStatement else
    if Element.TagName = 'EXPORT' then
      ParseExportStatement else
      ParseNodeStatement;
  end;

begin
  Result := CreateRootNode;
  try
    if FileTopLevel then
    begin
      ParseProfile;
      ParseHead;
    end;

  except FreeAndNil(Result); raise end;
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
  Reader: TX3DReaderNames;
begin
  Check(X3DElement.TagName = 'X3D', 'Root element of X3D file must be <X3D>');

  { parse "version" attribute }
  begin
    Version := X3DVersion; { some sensible version number }
    WritelnWarning('VRML/X3D', Format('Missing X3D version number, assuming %d.%d', [Version.Major, Version.Minor]));
  end;

end;

{$endif read_implementation}
