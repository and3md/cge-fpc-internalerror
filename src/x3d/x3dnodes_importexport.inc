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

{$ifdef read_interface}

  TX3DImport = class(TX3DFileItem)
  public
    InlineNodeName, ImportedNodeName, ImportedNodeAlias: string;

    procedure SaveToStream(Writer: TX3DWriter); override;
    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DImport;
  end;

  TX3DExport = class(TX3DFileItem)
  public
    ExportedNodeName, ExportedNodeAlias: string;

    procedure SaveToStream(Writer: TX3DWriter); override;
    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DExport;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DImport ---------------------------------------------------------------- }

procedure TX3DImport.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('IMPORT ' + InlineNodeName + '.' + ImportedNodeName);
        if ImportedNodeName <> ImportedNodeAlias then
          Writer.Write(' AS ' + ImportedNodeAlias);
        Writer.Writeln('');
      end;
    xeXML:
      begin
        Writer.WriteIndent(Format('<IMPORT inlineDEF=%s importedDEF=%s',
          [ StringToX3DXml(InlineNodeName),
            StringToX3DXml(ImportedNodeName) ]));
        if ImportedNodeName <> ImportedNodeAlias then
          Writer.Write(' AS=' + StringToX3DXml(ImportedNodeAlias));
        Writer.Writeln(' />');
      end;
    else raise EInternalError.Create('TX3DImport.SaveToStream Encoding?');
  end;
end;

function TX3DImport.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DImport;
begin
  Result := TX3DImport.Create;
  Result.InlineNodeName := InlineNodeName;
  Result.ImportedNodeName := ImportedNodeName;
  Result.ImportedNodeAlias := ImportedNodeAlias;
end;

{ TX3DExport ---------------------------------------------------------------- }

procedure TX3DExport.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('EXPORT ' + ExportedNodeName);
        if ExportedNodeName <> ExportedNodeAlias then
          Writer.Write(' AS ' + ExportedNodeAlias);
        Writer.Writeln('');
      end;
    xeXML:
      begin
        Writer.WriteIndent('<EXPORT localDEF=' + StringToX3DXml(ExportedNodeName));
        if ExportedNodeName <> ExportedNodeAlias then
          Writer.Write(' AS=' + StringToX3DXml(ExportedNodeAlias));
        Writer.Writeln(' />');
      end;
    else raise EInternalError.Create('TX3DExport.SaveToStream Encoding?');
  end;
end;

function TX3DExport.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DExport;
begin
  Result := TX3DExport.Create;
  Result.ExportedNodeName := ExportedNodeName;
  Result.ExportedNodeAlias := ExportedNodeAlias;
end;

{$endif read_implementation}
