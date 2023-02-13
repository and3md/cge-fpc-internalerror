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
  TMFBool = class(specialize TX3DSimpleMultField<
    boolean,
    TSFBool,
    TBooleanList,
    TMFBoolEvent>)
  strict protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
  public
    class function X3DType: string; override;
  end;

  TMFFloat = class(specialize TX3DSimpleMultField<
    Single,
    TSFFloat,
    TSingleList,
    TMFFloatEvent>)
  strict private
    FAngle: boolean;
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
    function CreateItemBeforeParse: TSFFloat; override;
  public
    { Value represents an angle. When reading from X3D 3.3 file, we will
      make sure it's expressed in radians, honoring optional "UNIT angle ..."
      declaration in X3D file. }
    property Angle: boolean read FAngle write FAngle default false;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFDouble = class(specialize TX3DSimpleMultField<
    Double,
    TSFDouble,
    TDoubleList,
    TMFDoubleEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFTime = class(TMFDouble)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

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

function TMFBool.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := BoolKeywords[Encoding][Items[ItemNum]];
end;

class function TMFBool.X3DType: string;
begin
  Result := 'MFBool';
end;

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

function TMFFloat.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := false;
end;

function TMFFloat.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := Format('%g', [Items[ItemNum]]);
end;

procedure TMFFloat.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFFloat).Items, (Value2 as TMFFloat).Items);
end;

function TMFFloat.CanAssignLerp: boolean;
begin
  Result := true;
end;

class function TMFFloat.X3DType: string;
begin
  Result := 'MFFloat';
end;

function TMFFloat.CreateItemBeforeParse: TSFFloat;
begin
  Result := inherited;
  { Assign our Angle to single item used for parsing.
    This way float values on MFFloat fields will be correctly converted to radians,
    important e.g. for Background.skyAngle,groundAngle. }
  (Result as TSFFloat).Angle := Angle;
end;

{ TMFDouble -------------------------------------------------------------------- }

function TMFDouble.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := false;
end;

function TMFDouble.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := Format('%g', [Items[ItemNum]]);
end;

procedure TMFDouble.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFDouble).Items, (Value2 as TMFDouble).Items);
end;

function TMFDouble.CanAssignLerp: boolean;
begin
  Result := true;
end;

class function TMFDouble.X3DType: string;
begin
  Result := 'MFDouble';
end;

{ TMFTime -------------------------------------------------------------------- }

class function TMFTime.X3DType: string;
begin
  Result := 'MFTime';
end;

class function TMFTime.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFTimeEvent.Create(AParentNode, AName, AInEvent);
end;

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
