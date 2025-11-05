    /**
     * Methods related to drawing
     * @name ___METHODS_FOR_DRAWING___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */
     
    /**
     * Start drawing
     * @returns { boolean } Previous state
     */
    rglwidgetClass.prototype.startDrawing = function() {
    	var value = this.drawing;
    	this.drawing = true;
    	return value;
    };

    /**
     * Stop drawing and check for context loss
     * @param { boolean } saved - Previous state
     */
    rglwidgetClass.prototype.stopDrawing = function(saved) {
      this.drawing = saved;
      if (!saved && this.gl && this.gl.isContextLost())
        this.restartCanvas();
    };

    /**
     * Update the triangles used to display a plane
     * @param { number } id - id of the plane
     * @param { Object } bbox - bounding box in which to display the plane
     */
    rglwidgetClass.prototype.planeUpdateTriangles = function(obj, bbox) {
      var perms = [[0,0,1], [1,2,2], [2,1,0]],
          x, xrow, elem, A, d, nhits, i, j, k, u, v, w, intersect, which, v0, v2, vx, reverse,
          face1 = [], face2 = [], normals = [],
          nPlanes = obj.normals.length, idx, center;
      obj.bbox = bbox;
      obj.vertices = [];
      obj.centers = [];
      obj.initialized = false;
      for (elem = 0; elem < nPlanes; elem++) {
//    Vertex Av = normal.getRecycled(elem);
        x = [];
        A = obj.normals[elem];
        d = obj.offsets[elem][0];
        nhits = 0;
        for (i=0; i<3; i++)
          for (j=0; j<2; j++)
            for (k=0; k<2; k++) {
              u = perms[0][i];
              v = perms[1][i];
              w = perms[2][i];
              if (A[w] !== 0.0) {
                intersect = -(d + A[u]*bbox[j+2*u] + A[v]*bbox[k+2*v])/A[w];
                if (bbox[2*w] < intersect && intersect < bbox[1+2*w]) {
                  xrow = [];
                  xrow[u] = bbox[j+2*u];
                  xrow[v] = bbox[k+2*v];
                  xrow[w] = intersect;
                  x.push(xrow);
                  face1[nhits] = j + 2*u;
                  face2[nhits] = k + 2*v;
                  nhits++;
                }
              }
            }

            if (nhits > 3) {
            /* Re-order the intersections so the triangles work */
              for (i=0; i<nhits-2; i++) {
                which = 0; /* initialize to suppress warning */
                for (j=i+1; j<nhits; j++) {
                  if (face1[i] === face1[j] || face1[i] === face2[j] ||
                      face2[i] === face1[j] || face2[i] === face2[j] ) {
                    which = j;
                    break;
                  }
                }
                if (which > i+1) {
                  rglwidgetClass.swap(x, i+1, which);
                  rglwidgetClass.swap(face1, i+1, which);
                  rglwidgetClass.swap(face2, i+1, which);
                }
              }
            }
            if (nhits >= 3) {
      /* Put in order so that the normal points out the FRONT of the faces */
              v0 = [x[0][0] - x[1][0] , x[0][1] - x[1][1], x[0][2] - x[1][2]];
              v2 = [x[2][0] - x[1][0] , x[2][1] - x[1][1], x[2][2] - x[1][2]];
              /* cross-product */
              vx = rglwidgetClass.xprod(v0, v2);
              reverse = rglwidgetClass.dotprod(vx, A) > 0;

              for (i=0; i<nhits-2; i++) {
                obj.vertices.push(x[0]);
                center = [];
                for (k = 0; k<3; k++)
                  center.push(x[0][k]/3);
                normals.push(A);
                for (j=1; j<3; j++) {
                  idx = i + (reverse ? 3-j : j);
                  obj.vertices.push(x[idx]);
                  for (k=0; k<3; k++)
                    center[k] += x[idx][k]/3;
                  normals.push(A);
                }
                obj.centers.push(center);
              }
            }
      }
      obj.pnormals = normals;
    };
    
    rglwidgetClass.prototype.mode4type = {points : "POINTS",
                     linestrip : "LINE_STRIP",
                     abclines : "LINES",
                     lines : "LINES",
                     sprites : "TRIANGLES",
                     planes : "TRIANGLES",
                     text : "TRIANGLES",
                     quads : "TRIANGLES",
                     surface : "TRIANGLES",
                     triangles : "TRIANGLES",
                     sphere : "TRIANGLES"
    };
    
    /**
     * Disable unused arrays
     * @param { Object } obj - Object to work with
     * @param { Array } enabled - Array indicating which are enabled
     */
    rglwidgetClass.prototype.disableArrays = function(obj, enabled) {
      var gl = this.gl || this.initGL(),
          objLocs = ["normLoc", "texLoc", "ofsLoc", "pointLoc", "nextLoc"],
          thisLocs = ["posLoc", "colLoc"], i, attr;
      for (i = 0; i < objLocs.length; i++) 
        if (enabled[objLocs[i]]) gl.disableVertexAttribArray(obj[objLocs[i]]);
      for (i = 0; i < thisLocs.length; i++)
        if (enabled[thisLocs[i]]) gl.disableVertexAttribArray(this[objLocs[i]]);
      if (typeof obj.userAttributes !== "undefined") {
      	for (attr in obj.userAttribSizes) {  // Not all attributes may have been used
      	  gl.disableVertexAttribArray( obj.userAttribLocations[attr] );
      	}
      }
    };

    /**
     * Start drawing the scene
     */    
    rglwidgetClass.prototype.doStartScene = function() {
      var gl = this.gl || this.initGL();
      gl.enable(gl.DEPTH_TEST);
      gl.depthFunc(gl.LEQUAL);
      gl.clearDepth(1.0);
      gl.clearColor(1,1,1,1);
      gl.depthMask(true); // Must be true before clearing depth buffer
      /* jshint bitwise: false */
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      /* jshint bitwise: true */
    };
    
    /**
     * Set gl depth test based on object's material
     * @param { Object } obj - object to use
     */
    rglwidgetClass.prototype.doDepthTest = function(obj) {
      var gl = this.gl,
          tests = {never: gl.NEVER,
                   less:  gl.LESS,
                   equal: gl.EQUAL,
                   lequal:gl.LEQUAL,
                   greater: gl.GREATER,
                   notequal: gl.NOTEQUAL,
                   gequal: gl.GEQUAL,
                   always: gl.ALWAYS},
           test = tests[this.getMaterial(obj, "depth_test")];
      gl.depthFunc(test);
    };    
    
    /**
     * Set polygon offset for an obj
     * @param { object } obj - object to use
     */
    rglwidgetClass.prototype.doPolygonOffset = function(obj) { 
      var gl = this.gl;
      if (typeof obj.polygon_offset !== "undefined") {
        gl.polygonOffset(obj.polygon_offset[0],
                          obj.polygon_offset[1]);
        gl.enable(gl.POLYGON_OFFSET_FILL);
      } else
        gl.disable(gl.POLYGON_OFFSET_FILL);
    };
    
    /**
     * Do code for clipping
     * @param { object } obj - Object to work with
     * @param { object } subscene - Subscene to work with
     */
    rglwidgetClass.prototype.doClipping = function(obj, subscene) {
      var gl = this.gl,
          clipcheck = 0,
          clipplaneids = subscene.clipplanes,
          clip, i,j, n = this.countClipplanes(),
          clipplanedata; 
          
      if (n > 0) {
        clipplanedata = new Float32Array(4*n);
        for (i=0; i < clipplaneids.length; i++) {
          clip = this.getObj(clipplaneids[i]);
          for (j=0; j < clip.offsets.length; j++) {
            clipplanedata.set(clip.IMVClip[j], clipcheck);
            clipcheck += 4;
          }
        }
      
        // Leftovers are initialized to zero, which is fine
        gl.uniform4fv(obj.clipLoc, clipplanedata);
      }
    };
    
    /**
     * Do code for lighting
     * @param { object } obj - Object to work with
     * @param { object } subscene - Subscene to work with
     */
    rglwidgetClass.prototype.doLighting = function(obj, subscene) {
    var gl = this.gl, i, j, n, light,
      ambient, specular, diffuse, lightDir, viewpoint, finite,
      ambient0, specular0;

      gl.uniform3fv( obj.emissionLoc, obj.emission);
      gl.uniform1f( obj.shininessLoc, obj.shininess);
      while ((typeof subscene.lights === "undefined" ||
              subscene.lights.length === 0) && 
             typeof subscene.parent !== "undefined")
        subscene = this.getObj(subscene.parent);

      if (typeof subscene.lights === "undefined")
        return;
        
      n = subscene.lights.length;
        
      ambient = new Float32Array(3*n);
      specular = new Float32Array(3*n);
      diffuse = new Float32Array(3*n);
      lightDir = new Float32Array(3*n);
      viewpoint = new Int32Array(n);
      finite = new Int32Array(n);
          
      for (i=0; i < n; i++) {
        light = this.getObj(subscene.lights[i]);
        if (!light.initialized) this.initObj(light);
        ambient0 = this.componentProduct(light.ambient, obj.ambient);
        specular0 = this.componentProduct(light.specular, obj.specular);
        for (j=0; j < 3; j++) {
          ambient[3*i + j] = ambient0[j];
          specular[3*i + j] = specular0[j];
          diffuse[3*i + j] = light.diffuse[j];
          lightDir[3*i + j] = light.lightDir[j];
        }
        viewpoint[i] = light.viewpoint;
        finite[i] = light.finite;
      }
        
      for (i = n; i < obj.nlights; i++) {
        for (j = 0; j < 3; j++) {
          ambient[3*i + j] = 0.0;
          specular[3*i + j] = 0.0;
          diffuse[3*i + j] = 0.0;
        }
      }
        
      gl.uniform3fv( obj.ambientLoc, ambient);
      gl.uniform3fv( obj.specularLoc, specular);
      gl.uniform3fv( obj.diffuseLoc, diffuse);
      gl.uniform3fv( obj.lightDirLoc, lightDir);
      gl.uniform1iv( obj.viewpointLoc, viewpoint);
      gl.uniform1iv( obj.finiteLoc, finite);
    };
    
    /**
     * Do code for colors
     * @param { object } obj - Object to work with
     */
    rglwidgetClass.prototype.doColors = function(obj) {
      var gl = this.gl;
      if (obj.colorCount === 1) {
        gl.disableVertexAttribArray( this.colLoc );
        gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
        return false;
      } else {
        gl.enableVertexAttribArray( this.colLoc );
        gl.vertexAttribPointer(this.colLoc, 4, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.cofs);
        return true;
      }
    };
    
    /**
     * Do code for normals
     * @param { object } obj - Object to work with
     */
    rglwidgetClass.prototype.doNormals = function(obj) {
      var gl = this.gl;
      if (obj.vOffsets.nofs >= 0) {
        gl.enableVertexAttribArray( obj.normLoc );
        gl.vertexAttribPointer(obj.normLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nofs);
        return true;
      } else
        return false;
    };
    
    /**
     * Do code for vNormal
     * @param { object } obj - Object to work with
     */
    rglwidgetClass.prototype.doNormMat = function(obj) {
      var gl = this.gl;
        
      gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(this.normMatrix.getAsArray()) );
    };
    
    /**
     * Do code for textures
     * @param { object } obj - Object to work with
     */    
    rglwidgetClass.prototype.doTexture = function(obj) {
      var gl = this.gl, 
          is_sphere = obj.type === "sphere";
        gl.enableVertexAttribArray( obj.texLoc );
        if (is_sphere)
          gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*this.sphere.vOffsets.stride, 4*this.sphere.vOffsets.tofs);
        else
          gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.tofs);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, obj.texture);
        gl.uniform1i( obj.sampler, 0);
        return true;
    };
    
    /**
     * Do code for user attributes
     * @param { object } obj - Object to work with
     */    
    rglwidgetClass.prototype.doUserAttributes = function(obj) {
      if (typeof obj.userAttributes !== "undefined") {
        var gl = this.gl;
      	for (var attr in obj.userAttribSizes) {  // Not all attributes may have been used
      	  gl.enableVertexAttribArray( obj.userAttribLocations[attr] );
      	  gl.vertexAttribPointer( obj.userAttribLocations[attr], obj.userAttribSizes[attr],
      	  			  gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.userAttribOffsets[attr]);
      	}
      }
    };

    /**
     * Do code for user uniforms
     * @param { object } obj - Object to work with
     */    
    rglwidgetClass.prototype.doUserUniforms = function(obj) {
      var gl = this.gl, attr;
      if (typeof obj.userUniforms !== "undefined") {
      	for (attr in obj.userUniformLocations) {
      	  var loc = obj.userUniformLocations[attr];
      	  if (loc !== null) {
      	    var uniform = obj.userUniforms[attr];
      	    if (typeof uniform !== "undefined") {
      	      var dim = rglwidgetClass.arrayDim(uniform);
      	      if (dim.length === 0)
      	        gl.uniform1f(loc, uniform);
      	      else if (dim.length === 1) {
      	        uniform = new Float32Array(uniform);
      	        switch(uniform.length) {
      	      	  case 2: gl.uniform2fv(loc, uniform); break;
      	      	  case 3: gl.uniform3fv(loc, uniform); break;
      	      	  case 4: gl.uniform4fv(loc, uniform); break;
      	      	  default: console.warn("bad uniform length");
      	        }
      	      } else if (dim.length === 2 && dim[0] === 4 && dim[1] === 4)
      	        gl.uniformMatrix4fv(loc, false, new Float32Array(rglwidgetClass.flatten(uniform)));
      	      else if (dim.length === 2) {
      	        uniform = new Float32Array(rglwidgetClass.flatten(uniform));
      	        switch(dim[[1]]) {
      	          case 1: gl.uniform1fv(loc, uniform); break;
      	          case 2: gl.uniform2fv(loc, uniform); break;
      	          case 3: gl.uniform3fv(loc, uniform); break;
      	          case 4: gl.uniform4fv(loc, uniform); break;
      	          default: console.warn("bad uniform column count");
      	        }
      	      } else
      	        console.warn("unsupported uniform shape");
      	    }
      	  }
      	}
      }
      if (typeof obj.userTextures !== "undefined") {
        var has_texture = rglwidgetClass.isSet(obj.flags, rglwidgetClass.f_has_texture),
              texnum = has_texture - 1;
        for (attr in obj.userTextures) {
      	  var texture = obj.userTextures[attr];
      	  if (texture.sampler !== null) {
      	    texnum += 1;
      	    gl.activeTexture(gl.TEXTURE0 + texnum);
            gl.bindTexture(gl.TEXTURE_2D, texture.texture);
            gl.uniform1i( texture.sampler, texnum);
      	  }
      	}
      }
    };

    /**
     * Load indices for complex drawing
     * @param { object } obj - Object to work with
     * @param { numeric } pass - Which pass of drawing?
     * @param { array } indices - Indices to draw
     */    
    rglwidgetClass.prototype.doLoadIndices = function(obj, pass, indices) {
      var gl = this.gl,
          f = obj.f[pass],
          type = obj.type,
          fat_lines = rglwidgetClass.isSet(obj.flags, rglwidgetClass.f_fat_lines),
          fnew, step;
      switch(type){
        case "points":
          step = 1;
          break;
        case "abclines":
        case "lines":
          if (fat_lines)
            step = 6;
          else
            step = 2;
          break;
        case "linestrip":
          if (fat_lines)
            step = 6;
          else
            step = 1;
          break;
        case "sphere":
        case "planes":
        case "triangles":
          step = 3;
          break;
        case "text":
        case "sprites":
        case "quads":
        case "surface":
          step = 6;
          break;
        default:
          console.error("loadIndices for "+type);
          return 0;
      }
      if (obj.index_uint)
        fnew = new Uint32Array(step * indices.length);
      else
        fnew = new Uint16Array(step * indices.length);
      for (var i = 0; i < indices.length; i++) {
        for (var j = 0; j < step; j++) {
          fnew[step*i + j] = f[step*indices[i] + j];
        }
      }
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, fnew, gl.DYNAMIC_DRAW);
      return fnew.length;
    };

    /**
     * Do code for depth masking
     * @param { boolean } mask - whether to mask
     */
    rglwidgetClass.prototype.doMasking = function(mask) {
      var gl = this.gl;
      gl.depthMask(mask);
    };

    /**
     * Do code for alpha blending
     * @param { boolean }  blend - Whether to blend.
     * @param { integer }  objid - Object id
     */    
    rglwidgetClass.prototype.doBlending = function(blend, objid) {
      var gl = this.gl, blendfunc, obj, 
        blends =  {zero: gl.ZERO,
                   one:  gl.ONE,
                   src_color: gl.SRC_COLOR,
                   one_minus_src_color: gl.ONE_MINUS_SRC_COLOR,
                   dst_color: gl.DST_COLOR,
                   one_minus_dst_color: gl.ONE_MINUS_DST_COLOR,
                   src_alpha: gl.SRC_ALPHA,
                   one_minus_src_alpha: gl.ONE_MINUS_SRC_ALPHA,
                   dst_alpha: gl.DST_ALPHA,
                   one_minus_dst_alpha: gl.ONE_MINUS_DST_ALPHA,
                   constant_color: gl.CONSTANT_COLOR,
                   one_minus_constant_color: gl.ONE_MINUS_CONSTANT_COLOR,
                   constant_alpha: gl.CONSTANT_ALPHA,
                   one_minus_constant_alpha: gl.ONE_MINUS_CONSTANT_ALPHA,
                   src_alpha_saturate: gl.SRC_ALPHA_SATURATE};
      if (blend) {
        obj = this.getObj(objid);
        blendfunc = this.getMaterial(obj, "blend");
        gl.blendFuncSeparate(blends[blendfunc[0]],
                             blends[blendfunc[1]],
                             gl.ONE, gl.ONE);
        gl.enable(gl.BLEND);
      } else {
        gl.disable(gl.BLEND);
      }
    };
    
    /**
     * Set up for fog in the subscene
     * @param { object } obj - background object
     * @param { object } subscene - which subscene
     */
    rglwidgetClass.prototype.doFog = function(obj, subscene) {
      var gl = this.gl, fogmode, color, 
          observer = subscene.par3d.observer[2],
          sintheta = Math.sin(subscene.par3d.FOV*Math.PI/180/2),
          parms = [this.frustum.near - 2*observer,
                   this.frustum.far - 2*observer,
                   this.fogScale,
                   (1-sintheta)/(1+sintheta)];
      if (typeof this.fogType === "undefined")
        this.fogType = "none";
      if (typeof this.fogScale === "undefined")
        parms[2] = 1;
      if (sintheta === 0)
        parms[3] = 1/3;
      switch(this.fogType){
        case "none": fogmode = 0; break;
        case "linear": 
          fogmode = 1; break;
        case "exp":  
          fogmode = 2; break;
        case "exp2": 
          fogmode = 3;
          break;
        default: console.error("Unknown fogtype "+this.fogType);
      }
      gl.uniform1i(obj.uFogMode, fogmode);
      color = this.fogColor;
      gl.uniform3f(obj.uFogColor, color[0], color[1], color[2]);
      gl.uniform4f(obj.uFogParms, parms[0], parms[1], parms[2], parms[3]);
    };

    /* The draw methods are called twice.  When 
       this.opaquePass is true, they should draw opaque parts
       of the scene, and return the list of transparent
       pieces.  Here context is the context array on input,
       modified when the matrices are changed.
       When this.opaquePass is false, the context argument
       contains a "piece", i.e. an ordered list of parts
       of the object to draw. */

    /**
     * Draw simple object
     * @param { object } obj - Object to draw
     * @param { object } subscene - which subscene
     * @param { array } context - Which context are we in?
     */       
    rglwidgetClass.prototype.drawSimple = function(obj, subscene, context) {
      var 
          fl,
          is_transparent,
          type = obj.type,
          gl = this.gl || this.initGL(),
          count,
          pass, mode, pmode,
          enabled = {};
        
      if (!obj.initialized)
        this.initObj(obj);
        
      if (this.texturesLoading)
        return[];

      count = obj.vertexCount;
      if (!count)
        return [];
    
      fl = obj.defFlags;
      is_transparent = fl.is_transparent || obj.someHidden;
      
      if (is_transparent && this.opaquePass)
        return this.getPieces(context, obj.id, 0, obj);

      this.doDepthTest(obj);
      
      this.doMasking(this.getMaterial(obj, "depth_mask"));
            
      gl.useProgram(obj.prog);

      this.doPolygonOffset(obj);

      gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);

      gl.uniformMatrix4fv( obj.prMatLoc, false, new Float32Array(this.prMatrix.getAsArray()) );
      gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(this.mvMatrix.getAsArray()) );

      this.doClipping(obj, subscene);

      if (fl.needs_vnormal)
        this.doNormMat(obj);
        
      if (fl.is_lit)
        this.doLighting(obj, subscene);

      if (fl.has_fog)
        this.doFog(obj, subscene);

      this.doUserAttributes(obj);

      this.doUserUniforms(obj);
 
      gl.enableVertexAttribArray( this.posLoc );
      enabled.posLoc = true;
        
      if (fl.has_texture || obj.type === "text")
        enabled.texLoc = this.doTexture(obj);

      enabled.colLoc = this.doColors(obj);
      enabled.normLoc = this.doNormals(obj);

      if (fl.fixed_size) {
        gl.uniform3f( obj.textScaleLoc, 0.75/this.vp.width, 0.75/this.vp.height, 1.0);
      }
      
      if (fl.fixed_quads) {
        gl.enableVertexAttribArray( obj.ofsLoc );
        enabled.ofsLoc = true;
        gl.vertexAttribPointer(obj.ofsLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.oofs);
      }

      for (pass = 0; pass < obj.passes; pass++) {
      	pmode = obj.pmode[pass];
        if (pmode === "culled")
          continue;

      	mode = fl.fat_lines && (fl.is_lines || pmode === "lines") ? "TRIANGLES" : this.mode4type[type];

      	if (fl.is_twosided) {
      	  gl.uniform1i(obj.frontLoc, pass !== 0);
      	  if (fl.has_normals) {
      	    gl.uniformMatrix4fv(obj.invPrMatLoc, false, new Float32Array(this.invPrMatrix.getAsArray()));
      	  }
      	}

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);
        if (!this.opaquePass) {
          if (type === "sphere" && obj.fastTransparency)
            count = this.doLoadIndices(obj, pass, this.sphere.fastpieces[0].indices);
          else
            count = this.doLoadIndices(obj, pass, context.indices);
        } else {
          count = obj.f[pass].length;
          gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[pass], gl.STATIC_DRAW);
        }
      	if (!fl.is_lines && pmode === "lines" && !fl.fat_lines) {
          mode = "LINES";
        } else if (pmode === "points") {
          mode = "POINTS";
        }
                          
        if ((fl.is_lines || pmode === "lines") && fl.fat_lines) {
          gl.enableVertexAttribArray(obj.pointLoc);
          enabled.pointLoc = true;
          gl.vertexAttribPointer(obj.pointLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.pointofs);
          gl.enableVertexAttribArray(obj.nextLoc );
          enabled.nextLoc = true;
          gl.vertexAttribPointer(obj.nextLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nextofs);
          gl.uniform1f(obj.aspectLoc, this.vp.width/this.vp.height);
          gl.uniform1f(obj.lwdLoc, this.getMaterial(obj, "lwd")/this.vp.height);
        }

        gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*obj.vOffsets.stride,  4*obj.vOffsets.vofs);

        gl.drawElements(gl[mode], count, obj.index_uint ? gl.UNSIGNED_INT : gl.UNSIGNED_SHORT, 0);
      }
      this.disableArrays(obj, enabled);
      return [];
    };

    /**
     * Draw planes object
     * @param { object } obj - Object to draw
     * @param { object } subscene - which subscene
     * @param { array } context - Which context are we in?
     */      
    rglwidgetClass.prototype.drawPlanes = function(obj, subscene, context) {
      if (this.opaquePass && (obj.bbox !== subscene.par3d.bbox || !obj.initialized)) {
          this.planeUpdateTriangles(obj, subscene.par3d.bbox);
      }
      return this.drawSimple(obj, subscene, context);
   };

    /**
     * @param { object } obj - object to draw
     * @param { object } subscene 
     * @param { array } context 
     * @description
     * Draw spheres in a subscene<br>
     * 
     * Drawing spheres happens in six ways:<br>
     * 1 opaquepass, not transparent:  transform and draw this.sphere count times<br>
     * 2 opaquepass, transparent, not fast: transform & collect sphere pieces count times<br>
     * 3 opaquepass, transparent, fast:  order the centres into separate pieces, order this.sphere once<br>
     * 4 not opaquepass, not transparent:  do nothing<br>
     * 5 not opaquepass, transparent, not fast:  transform for one sphere, draw one merged piece<br>
     * 6 not opaquepass, transparent, fast:  transform for one sphere, draw this.sphere in fixed order.<br>
     **/

    rglwidgetClass.prototype.drawSpheres = function(obj, subscene, context) {
      var flags = obj.flags,
          is_transparent = rglwidgetClass.isSet(flags, rglwidgetClass.f_is_transparent),
          sphereMV, baseofs, ofs, sscale, i,
          count, nc, scount, scale, indices, sphereNorm,
          enabled = {}, drawing,
          saveNorm = new CanvasMatrix4(this.normMatrix),
          saveMV = new CanvasMatrix4(this.mvMatrix),
          savePRMV = null,
          result = [], idx, margin = obj.material.margin;
 
      if (typeof margin !== "undefined")
        if (!this.marginVecToDataVec(obj, subscene))
          return [];

      if (!obj.initialized)
        this.initObj(obj);

      count = obj.vertexCount;
      if (!count) 
        return [];
        
      is_transparent = is_transparent || obj.someHidden;

      if (!this.opaquePass && !is_transparent)
        return [];
        
      if (this.prmvMatrix !== null)
        savePRMV = new CanvasMatrix4(this.prmvMatrix);
      
      scale = subscene.par3d.scale;        
      sphereNorm = new CanvasMatrix4();
      sphereNorm.scale(scale[0], scale[1], scale[2]);
      sphereNorm.multRight(saveNorm);
      this.normMatrix = sphereNorm;

      if (this.opaquePass) {
        context = context.slice();
        context.push(obj.id);
      } 
      
      drawing = this.opaquePass !== is_transparent;
      if (drawing) {
        nc = obj.colorCount;
        if (nc === 1) {
          this.sphere.onecolor = obj.onecolor;
        }
      }
      
      this.initShapeFromObj(this.sphere, obj);

      if (!this.opaquePass && obj.fastTransparency && typeof this.sphere.fastpieces === "undefined") {
        this.sphere.fastpieces = this.getPieces(context.context, obj.id, 0, this.sphere);
        this.sphere.fastpieces = this.sortPieces(this.sphere.fastpieces);
        this.sphere.fastpieces = this.mergePieces(this.sphere.fastpieces);
      }

      if (this.opaquePass)
        scount = count;
      else {
        indices = context.indices;
        if (obj.fastTransparency)
          scount = indices.length;  /* Each item gives the center of a whole sphere */
        else
          scount = 1;               /* Each item is a fragment of the sphere, at location subid */
      }
      for (i = 0; i < scount; i++) {
        sphereMV = new CanvasMatrix4();
        if (this.opaquePass)
          idx = i;
        else if (obj.fastTransparency)
          idx = indices[i];
        else
          idx = context.subid;
        if (typeof idx === "undefined")
          console.error("idx is undefined");
        baseofs = idx*obj.vOffsets.stride;
        ofs = baseofs + obj.vOffsets.radofs;
        sscale = obj.values[ofs];

        sphereMV.scale(sscale/scale[0], sscale/scale[1], sscale/scale[2]);
        sphereMV.translate(obj.values[baseofs],
                             obj.values[baseofs+1],
                             obj.values[baseofs+2]);
        sphereMV.multRight(saveMV);
        this.mvMatrix = sphereMV;
        this.setnormMatrix2();
        this.setprmvMatrix();
        if (drawing) {
          if (nc > 1) {
            this.sphere.onecolor = obj.values.slice(baseofs + obj.vOffsets.cofs, baseofs + obj.vOffsets.cofs + 4);
          }
          this.drawSimple(this.sphere, subscene, context);
        } else 
          result = result.concat(this.getSpherePieces(context, i, obj));
      }
      if (drawing)
        this.disableArrays(obj, enabled);
      this.normMatrix = saveNorm;
      this.mvMatrix = saveMV;
      this.prmvMatrix = savePRMV;
        
      return result;
    };
    
    /**
     * Prepare clipplanes for drawing
     * @param { object } obj - clip planes object
     */
    rglwidgetClass.prototype.drawClipplanes = function(obj) {
      var count = obj.offsets.length,
        IMVClip = [];
      for (var i=0; i < count; i++) {
        IMVClip[i] = rglwidgetClass.multMV(this.invMatrix, obj.vClipplane.slice(4*i, 4*(i+1)));
      }
      obj.IMVClip = IMVClip;
      return [];
    };

    /**
     * Prepare linestrip for drawing
     * @param { object } obj - line strip object
     * @param { object } subscene 
     * @param { array } context 
     */    
    rglwidgetClass.prototype.drawLinestrip = function(obj, subscene, context) {
      var origIndices, i, j, margin = obj.material.margin;
 
      if (typeof margin !== "undefined") 
        if (!this.marginVecToDataVec(obj, subscene))
          return [];
          
      if (this.opaquePass)
        return this.drawSimple(obj, subscene, context);
      origIndices = context.indices.slice();
      for (i=0; i < origIndices.length; i++) {
        j = origIndices[i];
        if (j < obj.centers.length - 1) {
          context.indices = [j, j+1];
          this.drawSimple(obj, subscene, context);
        }
      }
      context.indices = origIndices;
      return [];
    };
          
    /**
     * Draw a sprites object in a subscene
     * @param { object } obj - object to draw
     * @param { object } subscene
     * @param { object } context
     */
    rglwidgetClass.prototype.drawSprites = function(obj, subscene, context) {
      var flags = obj.flags,
          is_transparent = rglwidgetClass.isSet(flags, rglwidgetClass.f_is_transparent),
          sprites3d = rglwidgetClass.isSet(flags, rglwidgetClass.f_sprites_3d),
          fixed_size = rglwidgetClass.isSet(flags, rglwidgetClass.f_fixed_size),
          rotating = rglwidgetClass.isSet(flags, rglwidgetClass.f_rotating),
          i,j,
          origMV = new CanvasMatrix4( this.mvMatrix ),
          origPRMV = null,
          origPR,
          pos, radius, userMatrix,
          result = [], margin = obj.material.margin;
 
      if (typeof margin !== "undefined")
        if (!this.marginVecToDataVec(obj, subscene))
          return [];

      if (!sprites3d) 
        return this.drawSimple(obj, subscene, context);
      
      if (!obj.initialized)
        this.initObj(obj);

      if (!obj.vertexCount)
        return [];
    
      is_transparent = is_transparent || obj.someHidden;
      
      var norigs = obj.vertices.length,
          savenorm = new CanvasMatrix4(this.normMatrix),
          iOrig, adj, offset;

      userMatrix = obj.userMatrix;
                   
      if (this.opaquePass) {
        context = context.slice();
        context.push(obj.id);
      } else
        norigs = 1;
          
      if (this.prmvMatrix !== null)
         origPRMV = new CanvasMatrix4( this.prmvMatrix );

      offset = obj.offset;
      
      if (fixed_size && !rotating) {
        origPR = this.prMatrix;
        this.prMatrix = new CanvasMatrix4();
      }
        
      for (iOrig=0; iOrig < norigs; iOrig++) {
        if (this.opaquePass)
          j = iOrig;
        else
          j = context.subid;
        pos = [].concat(obj.vertices[j]).concat(1.0);
        radius = obj.radii.length > 1 ? obj.radii[j][0] : obj.radii[0][0];
        this.mvMatrix = new CanvasMatrix4(userMatrix);
        adj = this.getAdj(obj, j, offset);
        this.mvMatrix.translate(1 - 2*adj[0], 1 - 2*adj[1], 1 - 2*adj[2]);
        this.mvMatrix.scale(radius, radius, radius);
        
        if (fixed_size) {
          var viewport = subscene.par3d.viewport,
            winwidth = viewport.width*this.canvas.width,
            winheight = viewport.height*this.canvas.height,
            scalex = 27/winwidth, scaley = 27/winheight,
              scale = Math.sqrt(scalex * scaley);
          if (!rotating) {
            pos = rglwidgetClass.multVM(pos, origMV);
            pos = rglwidgetClass.multVM(pos, origPR);
            this.mvMatrix.scale(scalex, scaley, scale);
          } else {
            scale = 4.0 * scale * subscene.par3d.zoom;
            this.mvMatrix.scale(scale, scale, scale);
          }
          this.mvMatrix.translate(pos[0]/pos[3], pos[1]/pos[3], pos[2]/pos[3]);
          if (rotating)
            this.mvMatrix.multRight(origMV);
        } else {
          if (!rotating) {
            pos = rglwidgetClass.multVM(pos, origMV);
            this.mvMatrix.translate(pos[0]/pos[3], pos[1]/pos[3], pos[2]/pos[3]);
          } else {
            this.mvMatrix.translate(pos[0]/pos[3], pos[1]/pos[3], pos[2]/pos[3]);
            this.mvMatrix.multRight(origMV);
          }
        }
        this.setnormMatrix2();
        this.setprmvMatrix();
      
        j = iOrig % obj.shapefirst.length;
        var first = obj.shapefirst[j];
        
        for (i=0; i < obj.shapelens[j]; i++)
          if (this.opaquePass)
            result = result.concat(this.drawObjId(obj.objects[first + i], subscene.id, context.concat(j)));
          else
            this.drawObjId(obj.objects[i], subscene.id, context);
      }
      this.normMatrix = savenorm;
      this.mvMatrix = origMV;
      if (fixed_size && !rotating)
        this.prMatrix = origPR;
      if (origPRMV !== null)
        this.prmvMatrix = origPRMV;
      return result;
    };
    
    /**
     * Draw object that might be in margin
     * @param { Object } obj - text object to draw
     * @param { Object } subscene - subscene holding it
     * @param { Object } context - context for drawing
     */
    rglwidgetClass.prototype.drawMarginal = function(obj, subscene, context) {
      var margin = obj.material.margin;
 
      if (typeof margin !== "undefined") 
        if (!this.marginVecToDataVec(obj, subscene))
          return [];
          
      return this.drawSimple(obj, subscene, context);
    };
    
    /**
     * Draw bounding box and decorations
     * @param { Object } obj - bboxdeco to draw
     * @param { Object } subscene - subscene holding it
     * @param { Object } context - context for drawing
     */
    rglwidgetClass.prototype.drawBBox = function(obj, subscene, context) {
      var flags = obj.flags,
          is_transparent = rglwidgetClass.isSet(flags, rglwidgetClass.f_is_transparent),
          scale, bbox, indices,
          enabled = {}, drawing,
          result = [], idx, center, edges,
          saved;

      if (!obj.initialized)
        this.initBBox(obj);
      
      is_transparent = is_transparent || obj.someHidden;

      if (!this.opaquePass && !is_transparent)
        return result;
      
      this.setBbox(obj, subscene);
      
      saved = this.setBBoxMatrices(obj);
      
      bbox = obj.bbox;
      center = obj.center;

      scale = [bbox[1]-bbox[0], bbox[3]-bbox[2], bbox[5]-bbox[4]];

      if (!obj.cube.initialized) {
        this.initObj(obj.cube);
      }

      if (this.opaquePass) {
        context = context.slice();
        context.push(obj.id);
      } 
      
      drawing = this.opaquePass !== is_transparent;
      this.cube.onecolor = obj.cube.onecolor;
      this.initShapeFromObj(this.cube, obj.cube);

      if (!this.opaquePass)
        indices = context.indices;

      if (this.opaquePass)
        idx = 0;
      else
        idx = context.subid;
      if (typeof idx === "undefined")
        console.error("idx is undefined");

      if (drawing) {
        this.drawSimple(this.cube, subscene, context);
      } else 
        result = result.concat(this.getCubePieces(context, obj));

      if (!obj.ticks.initialized) {
        obj.ticks.locations = this.getTickLocations(obj);
        obj.ticks.edges = undefined;
      }
      edges = this.getTickEdges(this.prmvMatrix);
      if (obj.needsAxisCallback) 
        this.doAxisCallback(obj, edges);
      if (!obj.ticks.edges || edges.toString() !== obj.ticks.edges.toString()) {
        obj.ticks.edges = edges;
        this.getTickVertices(obj.ticks);
        this.placeTickLabels(obj);
        this.setTickLabels(obj);
      }
      if (!obj.ticks.initialized) {
        this.initObj(obj.ticks);
        this.initObj(obj.labels);
      }
        
      if (drawing) {
        this.drawSimple(obj.ticks, subscene, context);
        this.drawSimple(obj.labels, subscene, context);

        this.disableArrays(obj, enabled);
      } else {
        result = result.concat(this.drawSimple(obj.ticks, subscene, context));
        result = result.concat(this.drawSimple(obj.labels, subscene, context));
      }

      this.restoreBBoxMatrices(saved);
        
      return result;
    };
    
    /**
     * Use ids to choose object to draw
     * @param { numeric } id - object to draw
     * @param { numeric } subscene
     * @param { array } context
     */   
    rglwidgetClass.prototype.drawObjId = function(id, subsceneid, context) {
      if (typeof id !== "number")
        this.alertOnce("drawObjId id is "+typeof id);

      return this.drawObj(this.getObj(id), this.getObj(subsceneid), context);
   };
   
    /**
     * Draw an object in a subscene
     * @param { object } obj - object to draw
     * @param { object } subscene
     * @param { array } context
     */
    rglwidgetClass.prototype.drawObj = function(obj, subscene, context) {
      switch(obj.type) {
        case "abclines":
        case "surface":
          return this.drawSimple(obj, subscene, context);
        case "points":
        case "lines":  
        case "triangles":
        case "quads":
        case "text":
          return this.drawMarginal(obj, subscene, context);
        case "linestrip":
          return this.drawLinestrip(obj, subscene, context);
        case "planes":
          return this.drawPlanes(obj, subscene, context);
        case "spheres":
          return this.drawSpheres(obj, subscene, context);
        case "clipplanes":
          return this.drawClipplanes(obj);
        case "sprites":
          return this.drawSprites(obj, subscene, context);
        case "light":
          return [];
        case "bboxdeco":
          return this.drawBBox(obj, subscene, context);
      }
      
      console.error("drawObj for type = "+obj.type);
    };

    /**
     * Draw the background for a subscene
     * @param { number } id - id of background object
     * @param { number } subsceneid - id of subscene
     */
    rglwidgetClass.prototype.drawBackground = function(id, subsceneid, context) {
      var gl = this.gl || this.initGL(),
          obj = this.getObj(id),
          subscene,
          bg, i, savepr, saveinvpr, savemv, savenorm, m, bbox, result = [], 
          savedm = gl.getParameter(gl.DEPTH_WRITEMASK),
          savedt = gl.isEnabled(gl.DEPTH_TEST),
          saveblend = gl.isEnabled(gl.BLEND);

      if (!obj.initialized)
        this.initObj(obj);

      if (obj.colors.length) {
        bg = obj.colors[0];
        gl.depthMask(true);
        gl.clear(gl.DEPTH_BUFFER_BIT);
        gl.clearColor(bg[0], bg[1], bg[2], bg[3]);
        gl.clear(gl.COLOR_BUFFER_BIT);
        this.fogColor = bg;
      } else {
        this.fogColor = [0,0,0,0];
        obj.colors = [[0,0,0,0]];
      }
  
      this.fogType = obj.fogtype;
      this.fogScale = obj.fogscale;
      gl.disable(gl.BLEND);
      gl.disable(gl.DEPTH_TEST);
      gl.depthMask(false);
      if (typeof obj.quad !== "undefined") {
        savepr = this.prMatrix;
        saveinvpr = this.invPrMatrix;
        savemv = this.mvMatrix;
        savenorm = this.normMatrix;
        this.prMatrix = new CanvasMatrix4();
        this.invPrMatrix = new CanvasMatrix4();
        this.mvMatrix = new CanvasMatrix4();
        this.normMatrix = new CanvasMatrix4();
        for (i=0; i < obj.quad.length; i++)
          result = result.concat(this.drawObjId(obj.quad[i], subsceneid));
        this.prMatrix = savepr;
        this.invPrMatrix = saveinvpr;
        this.mvMatrix = savemv;
        this.normMatrix = savenorm;

      } else if (obj.sphere) {
        subscene = this.getObj(subsceneid);
        savemv = this.mvMatrix;
        savenorm = this.normMatrix;
        bbox = subscene.par3d.bbox;
        var center = [(bbox[0] + bbox[1])/2, 
                  (bbox[2] + bbox[3])/2, 
                  (bbox[4] + bbox[5])/2, 1],
            scale = subscene.par3d.scale,
            ranges = [bbox[1] - bbox[0], 
                  bbox[3] - bbox[2],
                  bbox[5] - bbox[4]],
            avgscale = rglwidgetClass.vlen(ranges)/Math.sqrt(3),
            aspect = [ranges[0]*scale[0]/avgscale,
                      ranges[1]*scale[1]/avgscale,
                      ranges[2]*scale[2]/avgscale],
            maxaspect = Math.max(aspect[0], aspect[1], aspect[2]),
            zoom = subscene.par3d.zoom;
        m = new CanvasMatrix4();
        m.rotate(90, 1, 0, 0);
        m.scale(zoom*2.0*maxaspect*ranges[0]/aspect[0], 
                zoom*2.0*maxaspect*ranges[1]/aspect[1],
                zoom*2.0*maxaspect*ranges[2]/aspect[2]);
        m.translate(center[0], center[1], center[2]);
        m.multRight(savemv);
        center = rglwidgetClass.multVM(center, savemv);
        m.translate(-center[0], -center[1], -center[2]);
        m.scale(1, 1, 0.25/zoom);
        m.translate(center[0], center[1], center[2]);
        this.mvMatrix = m;
        this.initShapeFromObj(this.sphere, obj);
        this.sphere.onecolor = obj.colors.length > 1 ? obj.colors[1] : obj.colors[0];
        
        this.normMatrix = new CanvasMatrix4();
        
        this.setnormMatrix2();
        this.setprmvMatrix();
        
        result = result.concat(this.drawSimple(this.sphere, subscene, context));
        this.mvMatrix = savemv;
        this.normMatrix = savenorm;
      }
      gl.depthMask(savedm);
      if (savedt)
        gl.enable(gl.DEPTH_TEST);
      if (saveblend)
        gl.enable(gl.BLEND);
      return result;
    };

    /**
     * Draw a subscene
     * @param { number } subsceneid - id of subscene
     * @param { array } context 
     */
    rglwidgetClass.prototype.drawSubscene = function(subsceneid, context) {
      var sub = this.getObj(subsceneid),
          objects = this.scene.objects,
          clipids = sub.clipplanes,
          subids = sub.objects,
          subscene_has_faces = false,
          subscene_needs_sorting = false,
          flags, i, obj, result = [];
          
      if (sub.par3d.skipRedraw)
        return result;
      
      if (this.opaquePass) {
        for (i=0; i < subids.length; i++) {
      	  obj = objects[subids[i]];
          flags = obj.flags;
          if (typeof flags !== "undefined") {
            subscene_has_faces = subscene_has_faces || 
                            (rglwidgetClass.isSet(flags, rglwidgetClass.f_is_lit) &&
                            !rglwidgetClass.isSet(flags, rglwidgetClass.f_fixed_quads));
            obj.is_transparent = obj.someHidden || 
              rglwidgetClass.isSet(flags, rglwidgetClass.f_is_transparent);
            subscene_needs_sorting = subscene_needs_sorting || 
              obj.is_transparent ||
              rglwidgetClass.isSet(flags, rglwidgetClass.f_depth_sort);
          }
        }
      }

      this.setViewport(subsceneid);

      this.setprMatrix(subsceneid);
      this.setInvPrMatrix();
      this.setmvMatrix(subsceneid);
      this.setnormMatrix2();
      this.setprmvMatrix();
      this.invMatrix = new CanvasMatrix4(this.mvMatrix);
      this.invMatrix.invert();
      
      if (this.opaquePass) {
        context = context.slice();
        context.push(subsceneid);
        
        this.doBlending(false);
        this.subsceneid = subsceneid;
        if (typeof this.sphere !== "undefined") // reset this.sphere.fastpieces; it will be recreated if needed
          this.sphere.fastpieces = undefined;
        if (typeof sub.backgroundId !== "undefined")
          result = result.concat(this.drawBackground(sub.backgroundId, subsceneid, context));
      }

      if (subids.length) {
            
        if (clipids.length > 0) {
          for (i = 0; i < clipids.length; i++)
            this.drawObjId(clipids[i], subsceneid);
        }
        
        subids = sub.opaque.concat(sub.transparent);
        if (this.opaquePass) {
          for (i = 0; i < subids.length; i++)
            result = result.concat(this.drawObjId(subids[i], subsceneid, context));
          subids = sub.subscenes;
          for (i = 0; i < subids.length; i++)
            result = result.concat(this.drawSubscene(subids[i], context));
        }
      }
      return result;
    };
    
    /**
     * Set the context for drawing transparently
     * @param { array } context
     */
    rglwidgetClass.prototype.setContext = function(context) {
      var result = [], objid, obj, type;
      context = context.slice();
      context.reverse();
      while (context.length > 0) {
        objid = context.pop();
        obj = this.getObj(objid);
        type = obj.type;
        switch (type) {
          case "subscene":
            this.drawSubscene(objid, false);
            break;
          case "sprites":
            result = result.concat(context.pop());
            break;
          case "spheres":
            // this.initSphereFromObj(obj);  // FIXME:  not needed?
            break;
          case "bboxdeco":
            result = result.concat(context.pop());
            break;
          default:
            console.error("bad type '", type, "' in setContext");
        }
      }
      return result;
    };
    
    /**
     * Draw the transparent pieces of a scene
     * @param {object} pieces
     */
    rglwidgetClass.prototype.drawPieces = function(pieces) {
      var i, prevcontext = [], context;
      for (i = 0; i < pieces.length; i++) {
        context = pieces[i].context.slice();
        if (context !== prevcontext) {
          prevcontext = context.slice();
          context = this.setContext(context);
          this.doBlending(true, pieces[i].objid);
        }
        this.drawObjId(pieces[i].objid, this.subsceneid, 
                       pieces[i]);
      }
    };
 
    /**
     * Draw the whole scene
     */
    rglwidgetClass.prototype.drawScene = function() {
      var wasDrawing = this.startDrawing(),
          pieces;
      if (!wasDrawing) {
        if (this.select.state !== "inactive")
          this.selectionChanged();

        this.doStartScene();
        this.opaquePass = true;
        pieces = this.drawSubscene(this.scene.rootSubscene, []);
        this.opaquePass = false;
        pieces = this.sortPieces(pieces);
        pieces = this.mergePieces(pieces);
        this.drawPieces(pieces);
      }
      this.stopDrawing(wasDrawing);
    };
