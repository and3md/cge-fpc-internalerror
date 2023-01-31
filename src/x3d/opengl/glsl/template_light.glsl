/* Shader code used for adding light source contribution.
   This is used by both desktop OpenGL and OpenGLES.
   This is used in both Gouraud and Phong shading,
   so it may go either in vertex or fragment shader.
*/

/* Light source position (or direction, if not LIGHT_TYPE_POSITIONAL)
   in eye coordinates. */
uniform vec3 castle_LightSource<Light>Position;
#ifdef LIGHT_TYPE_SPOT
uniform vec3 castle_LightSource<Light>SpotDirection;
uniform float castle_LightSource<Light>SpotCosCutoff;
#ifdef LIGHT_HAS_BEAM_WIDTH
/* In radians. Note that this differs from gl_LightSource[<Light>].spotCutoff
   that is in degrees. */
uniform float castle_LightSource<Light>SpotCutoff;
uniform float castle_LightSource<Light>BeamWidth;
#endif
#ifdef LIGHT_HAS_SPOT_EXPONENT
uniform float castle_LightSource<Light>SpotExponent;
#endif
#endif
/* Multiplied colors of light source and material. */
#ifdef LIGHT_HAS_AMBIENT
uniform vec4 castle_SideLightProduct<Light>Ambient;
#endif
#ifdef COLOR_PER_VERTEX
uniform vec4 castle_LightSource<Light>Diffuse;
#else
uniform vec4 castle_SideLightProduct<Light>Diffuse;
#endif
#ifdef LIGHT_HAS_SPECULAR
uniform vec4 castle_SideLightProduct<Light>Specular;
#endif
#ifdef LIGHT_HAS_ATTENUATION
/* Attenuation: constant, linear, quadratic. */
uniform vec3 castle_LightSource<Light>Attenuation;
#endif

#ifdef LIGHT_HAS_RADIUS
uniform float castle_LightSource<Light>Radius;
#endif

#ifdef CASTLE_SEPARATE_DIFFUSE_TEXTURE
#ifndef GL_ES
void separate_diffuse_apply_texture(inout vec4 fragment_color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye);
#endif
#endif

void PLUG_add_light_contribution(inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye,
  in float material_shininess,
  in vec4 color_per_vertex)
{
  vec3 light_dir;

/* Calculate light_dir */
#ifdef LIGHT_TYPE_POSITIONAL
  light_dir = castle_LightSource<Light>Position - vec3(vertex_eye);
  float distance_to_light = length(light_dir);
  light_dir /= distance_to_light;
#else
  light_dir = normalize(castle_LightSource<Light>Position);
#endif

#ifdef LIGHT_TYPE_SPOT
  /* Check SpotCosCutoff first, as we want to add nothing
     (not even ambient term) when were outside of spot light cone. */

  float spot_cos = dot(normalize(castle_LightSource<Light>SpotDirection), -light_dir);
  if (spot_cos < castle_LightSource<Light>SpotCosCutoff)
    return;
#endif

  float scale = 1.0;
  /* PLUG: light_scale (scale, normal_eye, light_dir) */

#ifdef LIGHT_TYPE_SPOT
#ifdef LIGHT_HAS_BEAM_WIDTH
  /* calculate spot following VRML 2.0/X3D idea of beamWidth */
  float cutOffAngle = castle_LightSource<Light>SpotCutoff;
  scale *= clamp(
    (                    acos(spot_cos) - cutOffAngle) /
    (castle_LightSource<Light>BeamWidth - cutOffAngle),
    0.0, 1.0);
#endif

#ifdef LIGHT_HAS_SPOT_EXPONENT
  /* calculate spot like fixed-function pipeline, using exponent */
  scale *= pow(spot_cos, castle_LightSource<Light>SpotExponent);
#endif
#endif

#ifdef LIGHT_HAS_ATTENUATION
  scale /= max(1.0,
           castle_LightSource<Light>Attenuation.x +
           castle_LightSource<Light>Attenuation.y * distance_to_light +
           castle_LightSource<Light>Attenuation.z * distance_to_light * distance_to_light);
#endif

#ifdef LIGHT_HAS_RADIUS
  if (distance_to_light >= castle_LightSource<Light>Radius)
    scale = 0.0;
#endif

  /* add ambient term */
  vec4 light_color =
#ifdef LIGHT_HAS_AMBIENT
  castle_SideLightProduct<Light>Ambient;
  /* PLUG: material_light_ambient (light_color) */
#else
  vec4(0.0);
#endif

  /* add diffuse term */
  vec4 diffuse =
#ifdef COLOR_PER_VERTEX
  castle_LightSource<Light>Diffuse * color_per_vertex;
#else
  castle_SideLightProduct<Light>Diffuse;
#endif

  #ifdef CASTLE_SEPARATE_DIFFUSE_TEXTURE
  separate_diffuse_apply_texture(diffuse, vertex_eye, normal_eye);
  #endif

  /* PLUG: material_light_diffuse (diffuse, vertex_eye, normal_eye) */
  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);
  light_color += diffuse * diffuse_factor;

#ifdef LIGHT_HAS_SPECULAR
  /* add specular term */
  /* halfVector is an average of
     - normalize(light position - vertex_eye) (we already have this
       in light_dir) and
     - normalize(camera position - vertex_eye)
       (and camera position == zero in camera space). */
  vec3 halfVector = normalize(light_dir - normalize(vec3(vertex_eye)));
  if (diffuse_factor != 0.0) {
    vec4 specular_color = castle_SideLightProduct<Light>Specular;
    /* PLUG: material_light_specular (specular_color) */
    /* PLUG: material_shininess (material_shininess) */
    light_color += specular_color *
      pow(max(dot(halfVector, normal_eye),
        0.0), material_shininess);
  }
#endif

  color += light_color * scale;
}

/* Undefine lights symbols, since for OpenGL ES all the shader parts
   are concatenated into a single string. */
#undef LIGHT_TYPE_POSITIONAL
#undef LIGHT_TYPE_SPOT
#undef LIGHT_HAS_AMBIENT
#undef LIGHT_HAS_SPECULAR
#undef LIGHT_HAS_ATTENUATION
#undef LIGHT_HAS_RADIUS
#undef LIGHT_HAS_BEAM_WIDTH
#undef LIGHT_HAS_SPOT_EXPONENT
