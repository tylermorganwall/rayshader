rglwidgetClass.rgl_vertex_shader = function() {
return  "#line 2 1\n"+
"// File 1 is the vertex shader\n"+
"#ifdef GL_ES\n"+
"#ifdef GL_FRAGMENT_PRECISION_HIGH\n"+
"precision highp float;\n"+
"#else\n"+
"precision mediump float;\n"+
"#endif\n"+
"#endif\n"+
"\n"+
"attribute vec3 aPos;\n"+
"attribute vec4 aCol;\n"+
"uniform mat4 mvMatrix;\n"+
"uniform mat4 prMatrix;\n"+
"varying vec4 vCol;\n"+
"varying vec4 vPosition;\n"+
"\n"+
"#ifdef NEEDS_VNORMAL\n"+
"attribute vec3 aNorm;\n"+
"uniform mat4 normMatrix;\n"+
"varying vec4 vNormal;\n"+
"#endif\n"+
"\n"+
"#if defined(HAS_TEXTURE) || defined (IS_TEXT)\n"+
"attribute vec2 aTexcoord;\n"+
"varying vec2 vTexcoord;\n"+
"#endif\n"+
"\n"+
"#ifdef FIXED_SIZE\n"+
"uniform vec3 textScale;\n"+
"#endif\n"+
"\n"+
"#ifdef FIXED_QUADS\n"+
"attribute vec3 aOfs;\n"+
"#endif\n"+
"\n"+
"#ifdef IS_TWOSIDED\n"+
"#ifdef HAS_NORMALS\n"+
"varying float normz;\n"+
"uniform mat4 invPrMatrix;\n"+
"#else\n"+
"attribute vec3 aPos1;\n"+
"attribute vec3 aPos2;\n"+
"varying float normz;\n"+
"#endif\n"+
"#endif // IS_TWOSIDED\n"+
"\n"+
"#ifdef FAT_LINES\n"+
"attribute vec3 aNext;\n"+
"attribute vec2 aPoint;\n"+
"varying vec2 vPoint;\n"+
"varying float vLength;\n"+
"uniform float uAspect;\n"+
"uniform float uLwd;\n"+
"#endif\n"+
"\n"+
"#ifdef USE_ENVMAP\n"+
"varying vec3 vReflection;\n"+
"#endif\n"+
"\n"+
"void main(void) {\n"+
"  \n"+
"#ifndef IS_BRUSH\n"+
"#if defined(NCLIPPLANES) || !defined(FIXED_QUADS) || defined(HAS_FOG) || defined(USE_ENVMAP)\n"+
"  vPosition = mvMatrix * vec4(aPos, 1.);\n"+
"#endif\n"+
"  \n"+
"#ifndef FIXED_QUADS\n"+
"  gl_Position = prMatrix * vPosition;\n"+
"#endif\n"+
"#endif // !IS_BRUSH\n"+
"  \n"+
"#ifdef IS_POINTS\n"+
"  gl_PointSize = POINTSIZE;\n"+
"#endif\n"+
"  \n"+
"  vCol = aCol;\n"+
"  \n"+
"// USE_ENVMAP implies NEEDS_VNORMAL\n"+
"\n"+
"#ifdef NEEDS_VNORMAL\n"+
"  vNormal = normMatrix * vec4(-aNorm, dot(aNorm, aPos));\n"+
"#endif\n"+
"\n"+
"#ifdef USE_ENVMAP\n"+
"  vReflection = normalize(reflect(vPosition.xyz/vPosition.w, \n"+
"                        normalize(vNormal.xyz/vNormal.w)));\n"+
"#endif\n"+
"  \n"+
"#ifdef IS_TWOSIDED\n"+
"#ifdef HAS_NORMALS\n"+
"  /* normz should be calculated *after* projection */\n"+
"  normz = (invPrMatrix*vNormal).z;\n"+
"#else\n"+
"  vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n"+
"  pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n"+
"  vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n"+
"  pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n"+
"  normz = pos1.x*pos2.y - pos1.y*pos2.x;\n"+
"#endif\n"+
"#endif // IS_TWOSIDED\n"+
"  \n"+
"#ifdef NEEDS_VNORMAL\n"+
"  vNormal = vec4(normalize(vNormal.xyz), 1);\n"+
"#endif\n"+
"  \n"+
"#if defined(HAS_TEXTURE) || defined(IS_TEXT)\n"+
"  vTexcoord = aTexcoord;\n"+
"#endif\n"+
"  \n"+
"#if defined(FIXED_SIZE) && !defined(ROTATING)\n"+
"  vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n"+
"  pos = pos/pos.w;\n"+
"  gl_Position = pos + vec4(aOfs*textScale, 0.);\n"+
"#endif\n"+
"  \n"+
"#if defined(IS_SPRITES) && !defined(FIXED_SIZE)\n"+
"  vec4 pos = mvMatrix * vec4(aPos, 1.);\n"+
"  pos = pos/pos.w + vec4(aOfs,  0.);\n"+
"  gl_Position = prMatrix*pos;\n"+
"#endif\n"+
"  \n"+
"#ifdef FAT_LINES\n"+
"  /* This code was inspired by Matt Deslauriers' code in \n"+
"   https://mattdesl.svbtle.com/drawing-lines-is-hard */\n"+
"  vec2 aspectVec = vec2(uAspect, 1.0);\n"+
"  mat4 projViewModel = prMatrix * mvMatrix;\n"+
"  vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n"+
"  currentProjected = currentProjected/currentProjected.w;\n"+
"  vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n"+
"  vec2 currentScreen = currentProjected.xy * aspectVec;\n"+
"  vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n"+
"  float len = uLwd;\n"+
"  vec2 dir = vec2(1.0, 0.0);\n"+
"  vPoint = aPoint;\n"+
"  vLength = length(nextScreen - currentScreen)/2.0;\n"+
"  vLength = vLength/(vLength + len);\n"+
"  if (vLength > 0.0) {\n"+
"    dir = normalize(nextScreen - currentScreen);\n"+
"  }\n"+
"  vec2 normal = vec2(-dir.y, dir.x);\n"+
"  dir.x /= uAspect;\n"+
"  normal.x /= uAspect;\n"+
"  vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n"+
"  gl_Position = currentProjected + offset;\n"+
"#endif\n"+
"  \n"+
"#ifdef IS_BRUSH\n"+
"  gl_Position = vec4(aPos, 1.);\n"+
"#endif\n"+
"}\n" ;};
rglwidgetClass.rgl_fragment_shader = function() {
return  "#line 2 2\n"+
"// File 2 is the fragment shader\n"+
"#ifdef GL_ES\n"+
"#ifdef GL_FRAGMENT_PRECISION_HIGH\n"+
"precision highp float;\n"+
"#else\n"+
"precision mediump float;\n"+
"#endif\n"+
"#endif\n"+
"varying vec4 vCol; // carries alpha\n"+
"varying vec4 vPosition;\n"+
"#if defined(HAS_TEXTURE) || defined (IS_TEXT)\n"+
"varying vec2 vTexcoord;\n"+
"uniform sampler2D uSampler;\n"+
"#endif\n"+
"\n"+
"#ifdef HAS_FOG\n"+
"uniform int uFogMode;\n"+
"uniform vec3 uFogColor;\n"+
"uniform vec4 uFogParms;\n"+
"#endif\n"+
"\n"+
"#if defined(IS_LIT) && !defined(FIXED_QUADS)\n"+
"varying vec4 vNormal;\n"+
"#endif\n"+
"\n"+
"#if NCLIPPLANES > 0\n"+
"uniform vec4 vClipplane[NCLIPPLANES];\n"+
"#endif\n"+
"\n"+
"#if NLIGHTS > 0\n"+
"uniform mat4 mvMatrix;\n"+
"#endif\n"+
"\n"+
"#ifdef IS_LIT\n"+
"uniform vec3 emission;\n"+
"uniform float shininess;\n"+
"#if NLIGHTS > 0\n"+
"uniform vec3 ambient[NLIGHTS];\n"+
"uniform vec3 specular[NLIGHTS]; // light*material\n"+
"uniform vec3 diffuse[NLIGHTS];\n"+
"uniform vec3 lightDir[NLIGHTS];\n"+
"uniform bool viewpoint[NLIGHTS];\n"+
"uniform bool finite[NLIGHTS];\n"+
"#endif\n"+
"#endif // IS_LIT\n"+
"\n"+
"#ifdef IS_TWOSIDED\n"+
"uniform bool front;\n"+
"varying float normz;\n"+
"#endif\n"+
"\n"+
"#ifdef FAT_LINES\n"+
"varying vec2 vPoint;\n"+
"varying float vLength;\n"+
"#endif\n"+
"\n"+
"#ifdef USE_ENVMAP\n"+
"varying vec3 vReflection;\n"+
"#endif\n"+
"\n"+
"void main(void) {\n"+
"  vec4 fragColor;\n"+
"#ifdef FAT_LINES\n"+
"  vec2 point = vPoint;\n"+
"  bool neg = point.y < 0.0;\n"+
"  point.y = neg ? (point.y + vLength)/(1.0 - vLength) :\n"+
"                 -(point.y - vLength)/(1.0 - vLength);\n"+
"#if defined(IS_TRANSPARENT) && defined(IS_LINESTRIP)\n"+
"  if (neg && length(point) <= 1.0) discard;\n"+
"#endif\n"+
"  point.y = min(point.y, 0.0);\n"+
"  if (length(point) > 1.0) discard;\n"+
"#endif // FAT_LINES\n"+
"  \n"+
"#ifdef ROUND_POINTS\n"+
"  vec2 coord = gl_PointCoord - vec2(0.5);\n"+
"  if (length(coord) > 0.5) discard;\n"+
"#endif\n"+
"  \n"+
"#if NCLIPPLANES > 0\n"+
"  for (int i = 0; i < NCLIPPLANES; i++)\n"+
"    if (dot(vPosition, vClipplane[i]) < 0.0) discard;\n"+
"#endif\n"+
"    \n"+
"#ifdef FIXED_QUADS\n"+
"    vec3 n = vec3(0., 0., 1.);\n"+
"#elif defined(IS_LIT)\n"+
"    vec3 n = normalize(vNormal.xyz);\n"+
"#endif\n"+
"    \n"+
"#ifdef IS_TWOSIDED\n"+
"    if ((normz <= 0.) != front) discard;\n"+
"#endif\n"+
"\n"+
"#ifdef IS_LIT\n"+
"    vec3 eye = normalize(-vPosition.xyz/vPosition.w);\n"+
"    vec3 lightdir;\n"+
"    vec4 colDiff;\n"+
"    vec3 halfVec;\n"+
"    vec4 lighteffect = vec4(emission, 0.);\n"+
"    vec3 col;\n"+
"    float nDotL;\n"+
"#ifdef FIXED_QUADS\n"+
"    n = -faceforward(n, n, eye);\n"+
"#endif\n"+
"    \n"+
"#if NLIGHTS > 0\n"+
"    // Simulate two-sided lighting\n"+
"    if (n.z < 0.0)\n"+
"      n = -n;\n"+
"    for (int i=0;i<NLIGHTS;i++) {\n"+
"      colDiff = vec4(vCol.rgb * diffuse[i], vCol.a);\n"+
"      lightdir = lightDir[i];\n"+
"      if (!viewpoint[i]) {\n"+
"        if (finite[i]) {\n"+
"          lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n"+
"        } else {\n"+
"          lightdir = (mvMatrix * vec4(lightdir, 0.)).xyz;\n"+
"        }\n"+
"      }\n"+
"      if (!finite[i]) {\n"+
"        halfVec = normalize(lightdir + eye);\n"+
"      } else {\n"+
"        lightdir = normalize(lightdir - vPosition.xyz/vPosition.w);\n"+
"        halfVec = normalize(lightdir + eye);\n"+
"      }\n"+
"      col = ambient[i];\n"+
"      nDotL = dot(n, lightdir);\n"+
"      col = col + max(nDotL, 0.) * colDiff.rgb;\n"+
"      col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular[i];\n"+
"      lighteffect = lighteffect + vec4(col, colDiff.a);\n"+
"    }\n"+
"#endif\n"+
"    \n"+
"#else // not IS_LIT\n"+
"    vec4 colDiff = vCol;\n"+
"    vec4 lighteffect = colDiff;\n"+
"#endif\n"+
"    \n"+
"#ifdef IS_TEXT\n"+
"    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n"+
"#endif\n"+
"    \n"+
"#ifdef HAS_TEXTURE\n"+
"\n"+
"// These calculations use the definitions from \n"+
"// https://docs.gl/gl3/glTexEnv\n"+
"\n"+
"#ifdef USE_ENVMAP\n"+
"    float m = 2.0 * sqrt(dot(vReflection, vReflection) + 2.0*vReflection.z + 1.0);\n"+
"    vec4 textureColor = texture2D(uSampler, vReflection.xy / m + vec2(0.5, 0.5));\n"+
"#else\n"+
"    vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXTURE_rgb\n"+
"\n"+
"#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n"+
"    textureColor = vec4(textureColor.rgb, lighteffect.a);\n"+
"#endif \n"+
"\n"+
"#ifdef TEXMODE_modulate\n"+
"    textureColor = lighteffect*vec4(textureColor.rgb, 1.);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_blend\n"+
"    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb, lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_add\n"+
"    textureColor = vec4(lighteffect.rgb + textureColor.rgb, lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#endif //TEXTURE_rgb\n"+
"        \n"+
"#ifdef TEXTURE_rgba\n"+
"\n"+
"#ifdef TEXMODE_replace\n"+
"// already done\n"+
"#endif \n"+
"\n"+
"#ifdef TEXMODE_modulate\n"+
"    textureColor = lighteffect*textureColor;\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_decal\n"+
"    textureColor = vec4((1. - textureColor.a)*lighteffect.rgb) +\n"+
"                     textureColor.a*textureColor.rgb, \n"+
"                     lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_blend\n"+
"    textureColor = vec4((1. - textureColor.rgb) * lighteffect.rgb,\n"+
"                    lighteffect.a*textureColor.a);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_add\n"+
"    textureColor = vec4(lighteffect.rgb + textureColor.rgb,\n"+
"                    lighteffect.a*textureColor.a);\n"+
"#endif\n"+
"    \n"+
"#endif //TEXTURE_rgba\n"+
"    \n"+
"#ifdef TEXTURE_alpha\n"+
"    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n"+
"\n"+
"#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n"+
"    textureColor = vec4(lighteffect.rgb, luminance);\n"+
"#endif \n"+
"\n"+
"#if defined(TEXMODE_modulate) || defined(TEXMODE_blend) || defined(TEXMODE_add)\n"+
"    textureColor = vec4(lighteffect.rgb, lighteffect.a*luminance);\n"+
"#endif\n"+
" \n"+
"#endif // TEXTURE_alpha\n"+
"    \n"+
"// The TEXTURE_luminance values are not from that reference    \n"+
"#ifdef TEXTURE_luminance\n"+
"    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n"+
"\n"+
"#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n"+
"    textureColor = vec4(luminance, luminance, luminance, lighteffect.a);\n"+
"#endif \n"+
"\n"+
"#ifdef TEXMODE_modulate\n"+
"    textureColor = vec4(luminance*lighteffect.rgb, lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_blend\n"+
"    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n"+
"                        lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_add\n"+
"    textureColor = vec4(luminance + lighteffect.rgb, lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#endif // TEXTURE_luminance\n"+
" \n"+
"    \n"+
"#ifdef TEXTURE_luminance_alpha\n"+
"    float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n"+
"\n"+
"#if defined(TEXMODE_replace) || defined(TEXMODE_decal)\n"+
"    textureColor = vec4(luminance, luminance, luminance, textureColor.a);\n"+
"#endif \n"+
"\n"+
"#ifdef TEXMODE_modulate\n"+
"    textureColor = vec4(luminance*lighteffect.rgb, \n"+
"                        textureColor.a*lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_blend\n"+
"    textureColor = vec4((1. - luminance)*lighteffect.rgb,\n"+
"                        textureColor.a*lighteffect.a);\n"+
"#endif\n"+
"\n"+
"#ifdef TEXMODE_add\n"+
"    textureColor = vec4(luminance + lighteffect.rgb, \n"+
"                        textureColor.a*lighteffect.a);\n"+
"\n"+
"#endif\n"+
"\n"+
"#endif // TEXTURE_luminance_alpha\n"+
"    \n"+
"    fragColor = textureColor;\n"+
"\n"+
"#elif defined(IS_TEXT)\n"+
"    if (textureColor.a < 0.1)\n"+
"      discard;\n"+
"    else\n"+
"      fragColor = textureColor;\n"+
"#else\n"+
"    fragColor = lighteffect;\n"+
"#endif // HAS_TEXTURE\n"+
"    \n"+
"#ifdef HAS_FOG\n"+
"    // uFogParms elements: x = near, y = far, z = fogscale, w = (1-sin(FOV/2))/(1+sin(FOV/2))\n"+
"    // In Exp and Exp2: use density = density/far\n"+
"    // fogF will be the proportion of fog\n"+
"    // Initialize it to the linear value\n"+
"    float fogF;\n"+
"    if (uFogMode > 0) {\n"+
"      fogF = (uFogParms.y - vPosition.z/vPosition.w)/(uFogParms.y - uFogParms.x);\n"+
"      if (uFogMode > 1)\n"+
"        fogF = mix(uFogParms.w, 1.0, fogF);\n"+
"      fogF = fogF*uFogParms.z;\n"+
"      if (uFogMode == 2)\n"+
"        fogF = 1.0 - exp(-fogF);\n"+
"      // Docs are wrong: use (density*c)^2, not density*c^2\n"+
"      // https://gitlab.freedesktop.org/mesa/mesa/-/blob/master/src/mesa/swrast/s_fog.c#L58\n"+
"      else if (uFogMode == 3)\n"+
"        fogF = 1.0 - exp(-fogF*fogF);\n"+
"      fogF = clamp(fogF, 0.0, 1.0);\n"+
"      gl_FragColor = vec4(mix(fragColor.rgb, uFogColor, fogF), fragColor.a);\n"+
"    } else gl_FragColor = fragColor;\n"+
"#else\n"+
"    gl_FragColor = fragColor;\n"+
"#endif // HAS_FOG\n"+
"    \n"+
"}\n" ;};
