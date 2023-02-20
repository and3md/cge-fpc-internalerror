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
  public
    Value: TItem;

    DefaultValue: TItem;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: TItem);

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure AssignValue(Source: TX3DField); override;
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
  end;
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
    TX3DFieldClass(ClassType).Create(nil, false, 'X3DName'));
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
