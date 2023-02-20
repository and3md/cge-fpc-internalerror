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

{ Single-value X3D fields. }
{$ifdef read_interface}


  { X3D field containing a vector. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TSFGenericVector<
    TItem,
    TEvent> = class(TX3DField)
  strict protected
    procedure SaveToStreamValue; override;
  public
    Value: TItem;

    DefaultValue: TItem;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: TItem);

    procedure ParseValue(Lexer: TX3DLexer); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;
    procedure Send(const AValue: TItem); overload;

    class function CreateEvent: TX3DEvent; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSFBool -------------------------------------------------------------------- }

{ TSFFloat ------------------------------------------------------------------- }

{ TSFTime -------------------------------------------------------------------- }

{ TSFLong -------------------------------------------------------------------- }

{ TSFInt32 ------------------------------------------------------------------- }

{ TSFMatrix3f ------------------------------------------------------------------ }

{ TSFMatrix3d ------------------------------------------------------------------ }


{ TSFMatrix4f ------------------------------------------------------------------ }


{ TSFMatrix4d ------------------------------------------------------------------ }


{ TSFMatrix ------------------------------------------------------------------ }


{ TSFRotation ---------------------------------------------------------------- }


{ TSFString ------------------------------------------------------------------ }


{ TSFStringEnum -------------------------------------------------------------- }

{ TSFGenericVector ----------------------------------------------------------- }

constructor TSFGenericVector.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: TItem);
begin
  inherited Create(AParentNode, AExposed, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFGenericVector.ParseValue(Lexer: TX3DLexer);
var
  I: Integer;
begin
  for I := 0 to High(Value.Data) do
  begin
    Lexer.CheckTokenIs(TokenNumbers, 'float number');
    Value.Data[I] := Lexer.TokenFloat;
    Lexer.NextToken;
  end;

  // Calling ParseVector or ParseFloat here causes FPC 3.0.2 error
  // Error: Global Generic template references static symtable
  // TODO: check on other FPC versions and report.
  // ParseVector(Value.Data, Lexer);
end;

procedure TSFGenericVector.SaveToStreamValue;
begin
end;

function TSFGenericVector.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and TItem.PerfectlyEquals(DefaultValue, Value);
end;

function TSFGenericVector.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFGenericVector) and
    TItem.Equals(TSFGenericVector(SecondValue).Value, Value);
end;

function TSFGenericVector.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFGenericVector) and
    TItem.PerfectlyEquals(TSFGenericVector(SecondValue).Value, Value);
end;

procedure TSFGenericVector.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := TItem.Lerp(A, (Value1 as TSFGenericVector).Value, (Value2 as TSFGenericVector).Value);
end;

function TSFGenericVector.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFGenericVector.AssignValue(Source: TX3DField);
begin
  if Source is TSFGenericVector then
  begin
    inherited;
    Value := TSFGenericVector(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFGenericVector.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFGenericVector.CreateEvent: TX3DEvent;
begin
  Result := TEvent.Create;
end;

procedure TSFGenericVector.Send(const AValue: TItem);
var
  FieldValue: TSFGenericVector;
begin
  FieldValue := TSFGenericVector(
    TX3DFieldClass(ClassType).CreateUndefined(nil, false, 'X3DName'));
  try
    FieldValue.Value := AValue;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{ TSFVec2f ------------------------------------------------------------------- }


{ TSFVec3f ------------------------------------------------------------------- }


{ TSFColor ------------------------------------------------------------------- }

{ TSFVec4f ------------------------------------------------------------------- }


{ TSFColorRGBA --------------------------------------------------------------- }

{ TSFVec2d ------------------------------------------------------------------- }


{ TSFVec3d ------------------------------------------------------------------- }


{ TSFVec4d ------------------------------------------------------------------- }

{ TSFBitMask ------------------------------------------------------------ }

{ TSFEnum ----------------------------------------------------------------- }

{$endif read_implementation}
