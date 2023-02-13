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

  { Multiple value (MF) fields ----------------------------------------------- }

  { }

  TMFString = class(specialize TX3DSimpleMultField<
    string,
    TSFString,
    TCastleStringList,
    TMFStringEvent>)
  strict protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    class function X3DType: string; override;
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TMFBool ------------------------------------------------------------------ }

{ TMFLong -------------------------------------------------------------------- }

{ TMFInt32 ------------------------------------------------------------------- }

{ TMFMatrix3f ------------------------------------------------------------------- }


{ TMFMatrix3d ------------------------------------------------------------------- }


{ TMFMatrix4f ------------------------------------------------------------------- }


{ TMFMatrix4d ------------------------------------------------------------------- }

{ TMFVec2f ------------------------------------------------------------------- }

{ TMFVec3f ------------------------------------------------------------------- }

{ TMFColor ------------------------------------------------------------------- }

{ TMFVec4f ------------------------------------------------------------------- }


{ TMFColorRGBA --------------------------------------------------------------- }


{ TMFVec2d ------------------------------------------------------------------- }


{ TMFVec3d ------------------------------------------------------------------- }

{ TMFVec4d ------------------------------------------------------------------- }

{ TMFRotation ---------------------------------------------------------------- }

{ TMFFloat ------------------------------------------------------------------- }

{ TMFDouble -------------------------------------------------------------------- }

{ TMFTime -------------------------------------------------------------------- }

{ TMFString ------------------------------------------------------------------ }

function TMFString.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  case Encoding of
    xeClassic: Result := StringToX3DClassic(Items[ItemNum]);
    xeXML    : Result := StringToX3DXmlMulti(Items[ItemNum]);
    else raise Exception.Create('TMFString.RawItemToString Encoding?');
  end;
end;

class function TMFString.X3DType: string;
begin
  Result := 'MFString';
end;

procedure TMFString.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
var
  Lexer: TX3DLexer;
begin
  { For MFString, it's very common that normal parsing fails because
    of missing double quotes, even in models from
    http://www.web3d.org/x3d/content/examples/Basic/
    Although specification clearly says that MFString
    components should always be enclosed within double
    quotes. We just do what Xj3D seems to do, that is
    we handle this as a single string (producing a warning). }

  try
    Lexer := TX3DLexer.CreateForPartialStream(AttributeValue, Reader.Version);
    try
      ParseXMLAttributeLexer(Lexer, Reader);
    finally FreeAndNil(Lexer) end;

    { Surround in try..except both CreateForPartialStream and ParseXMLAttributeLexer,
      as CreateForPartialStream can already cause exception in case of
      demo-models/x3d/test_single_quotes_mfstring.x3d . }
  except
    on E: Exception do
    begin
      Items.Count := 0;
      Items.Add(AttributeValue);
    end;
  end;
end;

function TMFString.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxAttributeCustomQuotes;
end;

procedure TMFString.SaveToStreamValue(Writer: TX3DWriter);
begin
  { MFString in XML encoding is surrounded by single quotes }
  if Writer.Encoding = xeXML then Writer.Write('''');
  inherited;
  if Writer.Encoding = xeXML then Writer.Write('''');
end;

{$endif read_implementation}
