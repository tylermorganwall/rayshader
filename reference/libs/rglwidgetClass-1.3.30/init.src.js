    /**
     * Methods related to initialization
     * @name ___METHODS_FOR_INITIALIZATION___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */
     
    /**
     * Initial test for WebGL
     */
    rglwidgetClass.prototype.initGL0 = function() {
      if (!window.WebGLRenderingContext){
        this.alertOnce("Your browser does not support WebGL. See http://get.webgl.org");
        return;
      }
    };

    /**
     * Initialize WebGL
     * @returns { Object } the WebGL context
     */
    rglwidgetClass.prototype.initGL = function() {
      var self = this, success = false;
      if (this.gl) {
      	if (!this.drawing && this.gl.isContextLost())
          this.restartCanvas();
        else
          return this.gl;
      }
      // if (!this.isInBrowserViewport()) return; Return what??? At this point we know this.gl is null.
      this.canvas.addEventListener("webglcontextrestored",
        this.onContextRestored, false);
      this.canvas.addEventListener("webglcontextlost",
        this.onContextLost, false);
      this.gl = this.canvas.getContext("webgl", this.webGLoptions) ||
               this.canvas.getContext("experimental-webgl", this.webGLoptions);
      success = !!(this.gl && this.gl instanceof WebGLRenderingContext);
      if (!success)
        this.alertOnce("Your browser does not support WebGL. See http://get.webgl.org"); 
      this.index_uint = this.gl.getExtension("OES_element_index_uint");
      var save = this.startDrawing();
      Object.keys(this.scene.objects).forEach(function(key){
        self.initObjId(parseInt(key, 10));
        });
      this.stopDrawing(save);
      return this.gl;
    };

    /**
     * Resize the display to match element
     * @param { Object } el - DOM element to match
     */
    rglwidgetClass.prototype.resize = function(el) {
      this.canvas.width = el.width;
      this.canvas.height = el.height;
    };

    /**
     * Initialize the sphere object
     */
    rglwidgetClass.prototype.initSphere = function(sections, segments) {
      var v = [], phi = [], theta = [], it = [], centers = [],
           i, j, k, ind, result = {};
       
      for (i = 0; i <= sections; i++) {
        phi.push(i/sections - 0.5);
      }

      for (j = 0; j <= segments; j++) {
        theta.push(2*j/segments);
        for (i = 0; i <= sections; i++) {
          /* These are [x,y,z,s,t]: */
          v.push([Math.sin(Math.PI*theta[j]) * Math.cos(Math.PI*phi[i]),
                  Math.sin(Math.PI*phi[i]),
                  Math.cos(Math.PI*theta[j]) * Math.cos(Math.PI*phi[i]),                               
                  theta[j]/2,
                  phi[i] + 0.5]);
           // console.log("xyzst="+v[v.length-1]);
        }
      }
      result.values = new Float32Array(rglwidgetClass.flatten(v));
      result.vertexCount = v.length;
      
      for (j = 0; j < segments; j++) {
        for (i = 0; i < sections; i++) {
          ind = i + (sections + 1)*j;
          if (i > 0)                       // Not south pole
            it.push([ind, 
                     ind + sections + 1,
                     ind + 1]);
          if (i < sections - 1)             // Not north pole
            it.push([ind + sections + 1, 
                     ind + sections + 2,
                     ind + 1]);
        }
      }
      result.it = new Uint16Array(rglwidgetClass.flatten(it));
      
      for (i = 0; i < it.length; i++) {
        centers.push([0,0,0]);
        for (j = 0; j < 3; j++) { // x,y,z
          for (k = 0; k < 3; k++) {// vertices
            centers[i][j] += v[it[i][k]][j]/3;
          }
        }
      }
      result.centers = centers;
      
      result.vOffsets = {vofs:0, cofs:-1, nofs:0, radofs:-1, oofs:-1,
                         tofs:3, nextofs:-1, pointofs:-1, stride:5};

      result.f = [];
      result.indices = {};

      result.colorCount = 1;
      result.type = "sphere";
      this.sphere = result;
      this.initShapeGL(this.sphere);
    };

    /**
     * Initialize the cube object
     */
    rglwidgetClass.prototype.initCube = function() {
   var v = [[0, 0, 0], [1, 0, 0], 
            [0, 1, 0], [1, 1, 0], 
            [0, 0, 1], [1, 0, 1],
            [0, 1, 1], [1, 1, 1]],
          ib = [[0, 2, 3, 1], 
                [2, 6, 7, 3], 
                [1, 3, 7, 5], 
                [0, 4, 6, 2], 
                [0, 1, 5, 4], 
                [4, 5, 7, 6]], 
          centers = [], i, j, k, 
          i0, i1, i2,
          normal, result = {};
       
      for (i = 0; i < ib.length; i++) {
        centers.push([0,0,0]);
        for (j = 0; j < 3; j++) { // x,y,z
          for (k = 0; k < 4; k++) {// vertices
            centers[i][j] += v[ib[i][k]][j]/4;
          }
        }
      }
      result.centers = centers; 
      result.values = new Float32Array(6*4*3*2);
      result.vertexCount = 24;
      result.vertices = new Array(24);
      result.normals = new Array(24);
      for (i=0; i < 6; i++) {
        for (j=0; j < 4; j++) {
          i0 = ib[i][j];
          result.vertices[4*i + j] = v[i0];
          i1 = ib[i][(j + 1) % 4];
          i2 = ib[i][(j + 2) % 4];
          if (j === 0)
            normal = rglwidgetClass.normalize(rglwidgetClass.xprod(rglwidgetClass.vdiff(v[i1], v[i0]),
                                  rglwidgetClass.vdiff(v[i2], v[i0])));
          result.normals[4*i + j] = normal;
          for (k=0; k < 3; k++) {
            result.values[i*24 + j*6 + k] = v[i0][k];
            result.values[i*24 + j*6 + 3 + k] = normal[k];
          }
        }
        for (j=0; j<4; j++)
          ib[i][j] = 4*i + j;
      }
      result.ib = new Uint16Array(rglwidgetClass.flatten(ib));
      
      result.vOffsets = {vofs:0, cofs:-1, nofs:3, radofs:-1, oofs:-1,
                         tofs:-1, nextofs:-1, pointofs:-1, stride:6};

      result.f = [];
      result.indices = {};

      result.colorCount = 1;
      result.type = "quads";
      this.cube = result;
      this.initShapeGL(this.cube);
    };
    

    /**
     * Do the gl part of initializing the sphere and cube
     */
    rglwidgetClass.prototype.initShapeGL = function(shape) {
      var gl = this.gl || this.initGL();
      if (gl.isContextLost()) return;
      shape.buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, shape.buf);
      gl.bufferData(gl.ARRAY_BUFFER, shape.values, gl.STATIC_DRAW);
      shape.ibuf = [gl.createBuffer(), gl.createBuffer()];
      return;
    };

    /* Initialize common sphere object from spheres object
    */
    rglwidgetClass.prototype.initShapeFromObj = function(shape, obj) {
      var i, pass, f, mode, self = this,
        /* This function selects things that would be
           the back, ignoring perspective -- this is what 
           we want for the bounding box decoration. */
        is_back = function(i) {
                var normal = [].concat(shape.normals[i]),
                  pt = shape.vertices[i];
                normal.push(-rglwidgetClass.dotprod(normal, pt));
                normal = rglwidgetClass.multVM(normal, self.normMatrix);
                return normal[2] < 0 || (normal[2] === 0 && normal[0] < 0);
              }; 
      shape.ofsLoc = obj.ofsLoc;
      shape.texLoc = obj.texLoc;
      shape.texture = obj.texture;
      shape.sampler = obj.sampler;
      shape.uFogMode = obj.uFogMode;
      shape.uFogColor = obj.uFogColor;
      shape.uFogParms = obj.uFogParms;
      shape.userAttribLocations = obj.userAttribLocations;
      shape.userUniformLocations = obj.userUniformLocations;
      shape.normLoc = obj.normLoc;
      shape.invPrMatLoc = obj.invPrMatLoc;
      shape.clipLoc = obj.clipLoc;
      shape.nextLoc = obj.nextLoc;
      shape.pointLoc = obj.pointLoc;
      shape.aspectLoc = obj.aspectLoc;
      shape.lwdLoc = obj.lwdLoc;
      shape.prog = obj.prog;
      shape.material = obj.material;
      shape.flags = obj.flags;
      shape.defFlags = obj.defFlags;
      shape.someHidden = obj.someHidden;
      shape.fastTransparency = obj.fastTransparency;
      shape.nlights = obj.nlights;
      shape.emission = obj.emission;
      shape.emissionLoc = obj.emissionLoc;
      shape.shininess = obj.shininess;
      shape.shininessLoc = obj.shininessLoc;
      shape.ambient = obj.ambient;
      shape.ambientLoc = obj.ambientLoc;
      shape.specular = obj.specular;
      shape.specularLoc = obj.specularLoc;
      shape.diffuse = obj.diffuse;
      shape.diffuseLoc = obj.diffuseLoc;
      shape.lightDir = obj.lightDir;
      shape.lightDirLoc = obj.lightDirLoc;
      shape.viewpoint = obj.viewpoint;
      shape.viewpointLoc = obj.viewpointLoc;
      shape.finite = obj.finite;
      shape.finiteLoc = obj.finiteLoc;
      shape.prMatLoc = obj.prMatLoc;
      shape.mvMatLoc = obj.mvMatLoc;
      shape.normMatLoc = obj.normMatLoc;
      shape.frontLoc = obj.frontLoc;
      shape.index_uint = false;
      shape.is_transparent = obj.is_transparent;
      shape.ignoreExtent = obj.ignoreExtent;
      if (shape.passes !== obj.passes ||
          JSON.stringify( shape.pmode) !== JSON.stringify(obj.pmode)) {
        shape.passes = obj.passes;
        shape.pmode = obj.pmode;
        for (pass = 0; pass < obj.passes; pass++) {
          mode =  shape.pmode[pass];
          if (typeof  shape.indices[mode] === "undefined") {
            f = [];
            switch (mode) {
            case "culled": break;
            case "points":
              f.length =  shape.vertexCount;
              for (i=0; i < f.length; i++)
                f[i] = i;
              break;
            case "lines":
              if (typeof shape.it !== "undefined") {
                f.length = 2* shape.it.length;
      	        for (i=0; i <  shape.it.length/3; i++) {
      	          f[6*i] =  shape.it[3*i];
      	          f[6*i + 1] =  shape.it[3*i + 1];
      	          f[6*i + 2] =  shape.it[3*i + 1];
      	          f[6*i + 3] =  shape.it[3*i + 2];
      	          f[6*i + 4] =  shape.it[3*i + 2];
      	          f[6*i + 5] =  shape.it[3*i];
      	        }
              } else {
                f.length = 2*shape.ib.length;
                for (i=0; i < shape.ib.length/4; i++) {
                  f[8*i] = shape.ib[4*i];
                  f[8*i + 1] = shape.ib[4*i + 1];
                  f[8*i + 2] = shape.ib[4*i + 1];
                  f[8*i + 3] = shape.ib[4*i + 2];
                  f[8*i + 4] = shape.ib[4*i + 2];
                  f[8*i + 5] = shape.ib[4*i + 3];
                  f[8*i + 6] = shape.ib[4*i + 3];
                  f[8*i + 7] = shape.ib[4*i];
                }
              }
      	      break;
      	    case "filled":
      	      if (typeof shape.it !== "undefined")
      	        f =  shape.it;
      	      else if (typeof shape.ib !== "undefined") {
      	        f.length = 1.5*shape.ib.length;
                for (i=0; i < shape.ib.length/4; i++) {
                  f[6*i] = shape.ib[4*i];
                  f[6*i+1] = shape.ib[4*i + 1];
                  f[6*i+2] = shape.ib[4*i + 2];
                  f[6*i+3] = shape.ib[4*i];
                  f[6*i+4] = shape.ib[4*i + 2];
                  f[6*i+5] = shape.ib[4*i + 3];
                }      	        
      	      }
      	      break;
      	    }              
            shape.indices[mode] = new Uint16Array(f);
          }
        }
      }       
      for (pass = 0; pass < obj.passes; pass++) {
        mode =  shape.pmode[pass];
        shape.f[pass] =  shape.indices[mode];
        if (typeof obj.draw_front !== "undefined" &&
            !obj.draw_front) {
          shape.f[pass] = shape.f[pass].filter(is_back);   
        }
      }
      // console.log("Names in  shapes not in  shape:"+JSON.stringify(rglwidgetClass.keydiff(obj,  shape)));
       shape.initialized = true;
    };

    /**
     * Initialize a subscene
     * @param { number } id - id of subscene.
     */
    rglwidgetClass.prototype.initSubscene = function(id) {
      var sub = this.getObj(id),
          i, obj;

      if (sub.type !== "subscene")
        return;

      sub.par3d.userMatrix = this.toCanvasMatrix4(sub.par3d.userMatrix);
      sub.par3d.userProjection = this.toCanvasMatrix4(sub.par3d.userProjection);
      sub.par3d.userProjection.transpose();
      sub.par3d.listeners = [].concat(sub.par3d.listeners);
      sub.backgroundId = undefined;
      sub.subscenes = [];
      sub.clipplanes = [];
      sub.transparent = [];
      sub.opaque = [];
      sub.lights = [];
      sub.needsBegin = true;
      if (typeof sub.objects !== "undefined")
        sub.objects = [].concat(sub.objects); /* make sure it's an array */
      for (i=0; i < sub.objects.length; i++) {
        obj = this.getObj(sub.objects[i]);
        if (typeof obj === "undefined") {
          sub.objects.splice(i, 1);
          i--;
        } else if (obj.type === "background")
          sub.backgroundId = obj.id;
        else
          sub[this.whichList(obj.id)].push(obj.id);
      }
    };
    
    rglwidgetClass.prototype.initBBox = function(obj) {
      if (!this.cube)
        this.initCube();
      obj.cube = {id: obj.id + 0.1,
                    type: "quads",
                    flags: obj.flags,
                    material: obj.material,
                    colors: [obj.colors[0]],
                    vertices: this.cube.vertices,
                    normals: this.cube.normals,
                    draw_front: obj.draw_front,
                    initialized: false
        };
      if (this.getMaterial(obj.cube, "front") !==
          this.getMaterial(obj.cube, "back"))
        /* jshint bitwise: false */  
        obj.cube.flags |= rglwidgetClass.f_is_twosided;
        /* jshint bitwise: true */
      this.scene.objects[obj.cube.id] = obj.cube;
      obj.ticks = {id: obj.id + 0.2,
                     type: "lines",
                     flags: rglwidgetClass.f_has_fog,
                     material: obj.material,
                     colors: (obj.colors.length > 1 ? [obj.colors[1]] : [obj.colors[0]]),
                     axes: obj.axes,
                     initialized: false
      };
      this.scene.objects[obj.ticks.id] = obj.ticks;
      obj.labels = {id: obj.id + 0.3,
                     type: "text",
                     flags: rglwidgetClass.f_has_fog + 
                            rglwidgetClass.f_fixed_size + 
                            rglwidgetClass.f_fixed_quads,
                     material: {lit: false},
                     colors: (obj.colors.length > 1 ? [obj.colors[1]] : [obj.colors[0]]),
                     cex: [[1]],
                     family: [["sans"]],
                     font: [[1]],
                     adj: [[0.5, 0.5, 0.5]],
                     ignoreExtent: true,
                     initialized: false
      };
      this.scene.objects[obj.labels.id] = obj.labels;
      obj.initialized = true;
    };
    
    rglwidgetClass.prototype.initBackground = function(obj) {
      var material, fl = obj.defFlags;
      if (typeof obj.ids !== "undefined")
        obj.quad = rglwidgetClass.flatten([].concat(obj.ids));
      else if (obj.sphere) {
        fl.has_normals = true;
        fl.needs_vnormal = true;
        obj.defFlags = fl;
        material = obj.material;
        material.front = "culled";
        obj.vertices = [[0,0,0]];
        obj.texcoords = [[0,0]];
      }  
    };

    /**
     * Initialize object for display
     * @param { number } id - id of object to initialize
     */
    rglwidgetClass.prototype.initObjId = function(id) {
      if (typeof id !== "number") {
        this.alertOnce("initObj id is "+typeof id);
      }
      return this.initObj(this.getObj(id));
    };

    /**
     * Initialize object for display
     * @param { Object } obj - object to initialize
     */
    rglwidgetClass.prototype.initObj = function(obj) {
      var type = obj.type, 
          flags = obj.flags,
          normals = obj.normals,
          round_points = (typeof obj.material === "undefined") ?
            false : this.getMaterial(obj, "point_antialias"),
          has_indices = typeof obj.indices !== "undefined",
          has_spheres = type === "spheres" || 
                        (type === "background" && obj.sphere),
          sprites_3d = rglwidgetClass.isSet(flags, rglwidgetClass.f_sprites_3d),
          depth_sort = rglwidgetClass.isSet(flags, rglwidgetClass.f_depth_sort),
          gl = this.gl || this.initGL(),
          fl, polygon_offset,
          texinfo, drawtype, nclipplanes, f, nrows, oldrows,
          i,j,v,v1,v2, mat, uri, matobj, pass, pmode,
          dim, nx, nz, nrow, shaders;

    obj.initialized = true;
    
    obj.someHidden = false; // used in selection
    
    this.expandBufferedFields(obj);
    
    if (type === "subscene")
      return;
      
    obj.defFlags = fl = rglwidgetClass.getDefFlags(flags, type, normals, round_points);
  
    obj.is_transparent = fl.is_transparent;
  
    if (type === "bboxdeco")
      return this.initBBox(obj);
      
    if (has_spheres && typeof this.sphere === "undefined")
      this.initSphere(16, 16);

    if (type === "light") {
      obj.ambient = new Float32Array(obj.colors[0].slice(0,3));
      obj.diffuse = new Float32Array(obj.colors[1].slice(0,3));
      obj.specular = new Float32Array(obj.colors[2].slice(0,3));
      obj.lightDir = new Float32Array(obj.vertices[0]);
      return;
    }

    if (type === "clipplanes") {
      obj.vClipplane = rglwidgetClass.flatten(rglwidgetClass.cbind(obj.normals, obj.offsets));
      return;
    }

    if (type === "background") {
      this.initBackground(obj);
      if (!obj.sphere)
        return;
    }

    polygon_offset = this.getMaterial(obj, "polygon_offset");
    if (polygon_offset[0] !== 0 || polygon_offset[1] !== 0)
      obj.polygon_offset = polygon_offset;

    if (fl.is_transparent) {
      depth_sort = ["triangles", "quads", "surface",
                    "spheres", "sprites", "text",
                    "planes"].indexOf(type) >= 0;
    }
    
    if (fl.is_brush)
      this.initSelection(obj.id);

    if (typeof obj.vertices === "undefined")
      obj.vertices = [];

    v = obj.vertices;
    if (has_indices)
      obj.vertexCount = obj.indices.length;
    else
      obj.vertexCount = v.length;
      
    if (!obj.vertexCount) return;

    if (fl.is_twosided && !fl.has_normals && type !== "background") {
      if (typeof obj.userAttributes === "undefined")
        obj.userAttributes = {};
      v1 = Array(v.length);
      v2 = Array(v.length);
      if (obj.type === "triangles" || obj.type === "quads") {
      	if (obj.type === "triangles")
      	  nrow = 3;
      	else
      	  nrow = 4;
        for (i=0; i<Math.floor(v.length/nrow); i++)
          for (j=0; j<nrow; j++) {
            v1[nrow*i + j] = v[nrow*i + ((j+1) % nrow)];
            v2[nrow*i + j] = v[nrow*i + ((j+2) % nrow)];
          }
      } else if (obj.type === "surface") {
        dim = obj.dim[0];
        nx = dim[0];
        nz = dim[1];
        for (j=0; j<nx; j++) {
          for (i=0; i<nz; i++) {
            if (i+1 < nz && j+1 < nx) {
              v2[j + nx*i] = v[j + nx*(i+1)];
              v1[j + nx*i] = v[j+1 + nx*(i+1)];
            } else if (i+1 < nz) {
              v2[j + nx*i] = v[j-1 + nx*i];
              v1[j + nx*i] = v[j + nx*(i+1)];
            } else {
              v2[j + nx*i] = v[j + nx*(i-1)];
              v1[j + nx*i] = v[j-1 + nx*(i-1)];
            }
          }
        }
      }
      obj.userAttributes.aPos1 = v1;
      obj.userAttributes.aPos2 = v2;
    }

    if (!sprites_3d) {
      if (gl.isContextLost()) return;
      if (typeof obj.prog !== "undefined") {
        gl.deleteProgram(obj.prog);
        obj.prog = undefined;
      }
      
      shaders = this.getShaders(obj);
      
      obj.prog = gl.createProgram();
      gl.attachShader(obj.prog, this.getShader( gl.VERTEX_SHADER,
                      shaders.vertex ));
      gl.attachShader(obj.prog, this.getShader( gl.FRAGMENT_SHADER,
                      shaders.fragment ));
      //  Force aPos to location 0, aCol to location 1
      gl.bindAttribLocation(obj.prog, 0, "aPos");
      gl.bindAttribLocation(obj.prog, 1, "aCol");
      gl.linkProgram(obj.prog);
      var linked = gl.getProgramParameter(obj.prog, gl.LINK_STATUS);
      if (!linked) {

        // An error occurred while linking
        var lastError = gl.getProgramInfoLog(obj.prog);
        console.warn("Error in program linking:" + lastError);

        gl.deleteProgram(obj.prog);
        return;
      }
    }

    if (type === "text") {
      texinfo = this.drawTextToCanvas(obj.texts,
                                      rglwidgetClass.flatten(obj.cex),
                                      rglwidgetClass.flatten(obj.family),
                                      rglwidgetClass.flatten(obj.family));
    }

    if (fl.fixed_quads && !sprites_3d) {
      obj.ofsLoc = gl.getAttribLocation(obj.prog, "aOfs");
    }

    if (fl.has_texture || type === "text") {
      if (!obj.texture) {
        obj.texture = gl.createTexture();
        // This is a trick from https://stackoverflow.com/a/19748905/2554330 to avoid warnings
        gl.bindTexture(gl.TEXTURE_2D, obj.texture);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1, 1, 0, gl.RGBA, gl.UNSIGNED_BYTE,
              new Uint8Array([255,255,255, 255])); // white
      }
      obj.texLoc = gl.getAttribLocation(obj.prog, "aTexcoord");
      obj.sampler = gl.getUniformLocation(obj.prog, "uSampler");
    }
    
    if (fl.has_fog && !sprites_3d) {
      obj.uFogMode = gl.getUniformLocation(obj.prog, "uFogMode");
      obj.uFogColor = gl.getUniformLocation(obj.prog, "uFogColor");
      obj.uFogParms = gl.getUniformLocation(obj.prog, "uFogParms");
    }

    if (fl.has_texture) {
      mat = obj.material;
      if (typeof mat.uri !== "undefined")
        uri = mat.uri;
      else if (typeof mat.uriElementId === "undefined") {
        matobj = this.getObj(mat.uriId);
        if (typeof matobj !== "undefined") {
          uri = matobj.material.uri;
        } else {
          uri = "";
        }
      } else
        uri = document.getElementById(mat.uriElementId).rglinstance.getObj(mat.uriId).material.uri;

      this.loadImageToTexture(uri, obj.texture);
    }

    if (type === "text") {
      this.handleLoadedTexture(obj.texture, this.textureCanvas);
    }

    var stride = 3, nc, cofs, nofs, radofs, oofs, tofs, vnew, fnew,
        nextofs = -1, pointofs = -1, alias, colors, key, selection,
        filter, adj, offset, attr, last, options, 
        len, current;

    obj.alias = undefined;
    
    colors = obj.colors;

    j = this.scene.crosstalk.id.indexOf(obj.id);
    if (j >= 0) {
      key = this.scene.crosstalk.key[j];
      options = this.scene.crosstalk.options[j];
      colors = colors.slice(0); 
      for (i = 0; i < v.length; i++)
        colors[i] = obj.colors[i % obj.colors.length].slice(0);
      if ( (selection = this.scene.crosstalk.selection) &&
           (selection.length || !options.selectedIgnoreNone) )
        for (i = 0; i < v.length; i++) {
          if (!selection.includes(key[i])) {
            if (options.deselectedColor)
              colors[i] = options.deselectedColor.slice(0);
            colors[i][3] = colors[i][3]*options.deselectedFade;   /* default: mostly transparent if not selected */
          } else if (options.selectedColor)
            colors[i] = options.selectedColor.slice(0);
        }
      if ( (filter = this.scene.crosstalk.filter) )
        for (i = 0; i < v.length; i++) 
          if (!filter.includes(key[i])) {
            if (options.filteredColor)
              colors[i] = options.filteredColor.slice(0);
            colors[i][3] = colors[i][3]*options.filteredFade;   /* default: completely hidden if filtered */
          }
    }  
    
    nc = obj.colorCount = colors.length;
    if (nc > 1) {
      cofs = stride;
      stride = stride + 4;
      v = rglwidgetClass.cbind(v, colors);
    } else {
      cofs = -1;
      obj.onecolor = rglwidgetClass.flatten(colors);
    }

    if (fl.has_normals && !has_spheres) {
      nofs = stride;
      stride = stride + 3;
      v = rglwidgetClass.cbind(v, typeof obj.pnormals !== "undefined" ? obj.pnormals : obj.normals);
    } else
      nofs = -1;

    if (typeof obj.radii !== "undefined") {
      radofs = stride;
      stride = stride + 1;
      // FIXME:  always concat the radii?
      if (obj.radii.length === v.length) {
        v = rglwidgetClass.cbind(v, obj.radii);
      } else if (obj.radii.length === 1) {
        v = v.map(function(row) { return row.concat(obj.radii[0]);});
      }
    } else
      radofs = -1;
      
    // Add default indices
    if (has_indices) {
      f = Array(obj.indices.length);
      for (i = 0; i < f.length; i++)
        f[i] = obj.indices[i] - 1;
    } else {
      f = Array(v.length);
      for (i = 0; i < v.length; i++)
        f[i] = i;
    }
    obj.f = [f,f];

    if (type === "sprites" && !sprites_3d) {
      tofs = stride;
      stride += 2;
      oofs = stride;
      stride += 3;
      vnew = new Array(4*v.length);
      fnew = new Array(4*v.length);
      alias = new Array(v.length);
      var rescale = fl.fixed_size ? 72 : 1,
          size = obj.radii, s = rescale*size[0]/2;
      last = v.length;
      f = obj.f[0];
      obj.adj = rglwidgetClass.flatten(obj.adj);
      if (typeof obj.pos !== "undefined") {
        obj.pos = rglwidgetClass.flatten(obj.pos);
        offset = obj.adj[0];
      } else
        offset = 0;
      for (i=0; i < v.length; i++) {
        adj = this.getAdj(obj, i, offset);
        if (size.length > 1)
          s = rescale*size[i]/2;
        adj[0] = 2*s*(adj[0] - 0.5);
        adj[1] = 2*s*(adj[1] - 0.5);
        adj[2] = 2*s*(adj[2] - 0.5);
        vnew[i]  = v[i].concat([0,0]).concat([-s-adj[0],
                                              -s-adj[1],
                                              -adj[2]]);
        fnew[4*i] = f[i];
        vnew[last]= v[i].concat([1,0]).concat([s-adj[0],
                                              -s-adj[1],
                                              -adj[2]]);
        fnew[4*i+1] = last++;
        vnew[last]= v[i].concat([1,1]).concat([s-adj[0],
                                               s-adj[1],
                                               -adj[2]]);
        fnew[4*i+2] = last++;
        vnew[last]= v[i].concat([0,1]).concat([-s-adj[0],
                                                s-adj[1],
                                                -adj[2]]);
        fnew[4*i+3] = last++;
        alias[i] = [last-3, last-2, last-1];
      }
      v = vnew;
      obj.vertexCount = v.length;
      obj.f = [fnew, fnew];
    } else if (type === "text") {
      tofs = stride;
      stride += 2;
      oofs = stride;
      stride += 3;
      vnew = new Array(4*v.length);
      f = obj.f[0];
      fnew = new Array(4*f.length);
      alias = new Array(v.length);
      last = v.length;
      adj = rglwidgetClass.flatten(obj.adj);
      if (typeof obj.pos !== "undefined") {
        obj.pos = rglwidgetClass.flatten(obj.pos);
        offset = adj[0];
      } else
        offset = 0;
      for (i=0; i < v.length; i++) {
        adj = this.getAdj(obj, i, offset, obj.texts[i]);
        vnew[i]  = v[i].concat([0,-0.5]).concat(adj);
        fnew[4*i] = f[i];
        vnew[last] = v[i].concat([1,-0.5]).concat(adj);
        fnew[4*i+1] = last++;
        vnew[last] = v[i].concat([1, 1.5]).concat(adj);
        fnew[4*i+2] = last++;
        vnew[last] = v[i].concat([0, 1.5]).concat(adj);
        fnew[4*i+3] = last++;
        alias[i] = [last-3, last-2, last-1];
        for (j=0; j < 4; j++) {
          v1 = vnew[fnew[4*i+j]];
          v1[oofs] = 2*(v1[tofs]-v1[oofs])*texinfo.widths[i];
          v1[oofs+1] = 2*(v1[tofs+1]-v1[oofs+1])*texinfo.textHeights[i];
          v1[oofs+2] = 2*(0.5-v1[oofs+2])*texinfo.textHeights[i]/1000.0;
          v1[tofs] = (texinfo.offsetsx[i] + v1[tofs]*texinfo.widths[i])/texinfo.canvasX;
          v1[tofs+1] = 1.0-(texinfo.offsetsy[i] -
              v1[tofs+1]*texinfo.textHeights[i])/texinfo.canvasY;
          vnew[fnew[4*i+j]] = v1;
        }
      }
      v = vnew;
      obj.vertexCount = v.length;
      obj.f = [fnew, fnew];
    } else if (typeof obj.texcoords !== "undefined") {
      tofs = stride;
      stride += 2;
      oofs = -1;
      v = rglwidgetClass.cbind(v, obj.texcoords);
    } else {
      tofs = -1;
      oofs = -1;
    }
    
    obj.alias = alias;
                          
    if (typeof obj.userAttributes !== "undefined") {
      obj.userAttribOffsets = {};
      obj.userAttribLocations = {};
      obj.userAttribSizes = {};
      for (attr in obj.userAttributes) {
      	obj.userAttribLocations[attr] = gl.getAttribLocation(obj.prog, attr);
      	if (obj.userAttribLocations[attr] >= 0) { // Attribute may not have been used
      	  obj.userAttribOffsets[attr] = stride;
      	  v = rglwidgetClass.cbind(v, obj.userAttributes[attr]);
      	  stride = v[0].length;
      	  obj.userAttribSizes[attr] = stride - obj.userAttribOffsets[attr];
      	} else
      	  console.warn("attribute '"+attr+"' not found in object "+obj.id+".");
      }
    }

    if (typeof obj.userUniforms !== "undefined" ||
        typeof obj.userTextures !== "undefined") {
      obj.userUniformLocations = {};
      for (attr in obj.userUniforms) {
        obj.userUniformLocations[attr] = gl.getUniformLocation(obj.prog, attr);
        if (obj.userUniformLocations[attr] === null)
          console.warn("uniform '"+attr+"' not found in object "+obj.id+".");
      }
      for (attr in obj.userTextures) {
        var texture = obj.userTextures[attr];
        texture.texture = gl.createTexture();
        // This is a trick from https://stackoverflow.com/a/19748905/2554330 to avoid warnings
        gl.bindTexture(gl.TEXTURE_2D, texture.texture);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1, 1, 0, gl.RGBA, gl.UNSIGNED_BYTE,
              new Uint8Array([255,255,255, 255])); // white
        texture.sampler = gl.getUniformLocation(obj.prog, attr);
        if (texture.sampler === null)
          console.warn("sampler '"+attr+"' not found in object "+obj.id+".");
        uri = texture.uri;
        this.loadImageToTexture(uri, texture.texture);
      }
    }

    if (sprites_3d) {
      obj.userMatrix = new CanvasMatrix4();
      obj.userMatrix.load(rglwidgetClass.flatten(obj.usermatrix));
      obj.objects = rglwidgetClass.flatten([].concat(obj.ids));
      fl.is_lit = false;
      obj.adj = rglwidgetClass.flatten(obj.adj);
      
      if (typeof obj.pos !== "undefined") {
        obj.pos = rglwidgetClass.flatten(obj.pos);
        obj.offset = obj.adj[0];
      } else
        obj.offset = 0;
        
      var shapenum = rglwidgetClass.flatten(obj.shapenum);
      obj.shapelens = [];
      obj.shapefirst = [];
      obj.shapefirst.push(0);
      len = 0;
      current = 0;
      for (i = 0; i < shapenum.length; i++) {
        if (shapenum[i] === shapenum[current]) {
          len++;
        } else {
          obj.shapelens.push(len);
          len = 1;
          current = i;
          obj.shapefirst.push(i);
        }
      }
      obj.shapelens.push(len);
        
      for (i=0; i < obj.objects.length; i++)
        this.initObjId(obj.objects[i]);
    }

    nclipplanes = this.countClipplanes();
    if (nclipplanes && !sprites_3d) {
      obj.clipLoc = gl.getUniformLocation(obj.prog,"vClipplane");
    }

    if (fl.is_lit) {
      obj.emissionLoc = gl.getUniformLocation(obj.prog, "emission");
      obj.emission = new Float32Array(this.stringToRgb(this.getMaterial(obj, "emission")));
      obj.shininessLoc = gl.getUniformLocation(obj.prog, "shininess");
      obj.shininess = this.getMaterial(obj, "shininess");
      obj.nlights = this.countLights();
      if (obj.nlights > 0) {
        obj.ambient = new Float32Array(this.stringToRgb(this.getMaterial(obj, "ambient")));
        obj.specular = new Float32Array(this.stringToRgb(this.getMaterial(obj, "specular")));
        obj.ambientLoc = gl.getUniformLocation(obj.prog, "ambient");
        obj.specularLoc = gl.getUniformLocation(obj.prog, "specular");
        obj.diffuseLoc = gl.getUniformLocation(obj.prog, "diffuse" );
        obj.lightDirLoc = gl.getUniformLocation(obj.prog, "lightDir");
        obj.viewpointLoc = gl.getUniformLocation(obj.prog, "viewpoint");
        obj.finiteLoc = gl.getUniformLocation(obj.prog, "finite" );
      }
    }
    
    obj.passes = fl.is_twosided + 1;
    obj.pmode = new Array(obj.passes);
    for (pass = 0; pass < obj.passes; pass++) {
      if (type === "triangles" || type === "quads" || type === "surface" || has_spheres)
      	pmode = this.getMaterial(obj, (pass === 0) ? "front" : "back");
      else pmode = "filled";
      obj.pmode[pass] = pmode;
    }
    if (!has_spheres) {
      obj.f.length = obj.passes;
      for (pass = 0; pass < obj.passes; pass++) {
      	f = fnew = obj.f[pass];
        pmode = obj.pmode[pass];
      	if (pmode === "culled")
      	  fnew = [];
        else if (pmode === "points") {
          // stay with default
        } else if ((type === "quads" || type === "text" ||
             type === "sprites") && !sprites_3d) {
          nrows = Math.floor(obj.vertexCount/4);
          if (pmode === "filled") {
            fnew = Array(6*nrows);
            for (i=0; i < nrows; i++) {
              fnew[6*i] = f[4*i];
              fnew[6*i+1] = f[4*i + 1];
              fnew[6*i+2] = f[4*i + 2];
              fnew[6*i+3] = f[4*i];
              fnew[6*i+4] = f[4*i + 2];
              fnew[6*i+5] = f[4*i + 3];
            }
          } else {
            fnew = Array(8*nrows);
            for (i=0; i < nrows; i++) {
              fnew[8*i] = f[4*i];
              fnew[8*i+1] = f[4*i + 1];
              fnew[8*i+2] = f[4*i + 1];
              fnew[8*i+3] = f[4*i + 2];
              fnew[8*i+4] = f[4*i + 2];
              fnew[8*i+5] = f[4*i + 3];
              fnew[8*i+6] = f[4*i + 3];
              fnew[8*i+7] = f[4*i];
            }
          }
        } else if (type === "triangles") {
          nrows = Math.floor(obj.vertexCount/3);
          if (pmode === "filled") {
            fnew = Array(3*nrows);
            for (i=0; i < fnew.length; i++) {
              fnew[i] = f[i];
            }
          } else if (pmode === "lines") {
            fnew = Array(6*nrows);
      	    for (i=0; i < nrows; i++) {
      	      fnew[6*i] = f[3*i];
      	      fnew[6*i + 1] = f[3*i + 1];
      	      fnew[6*i + 2] = f[3*i + 1];
      	      fnew[6*i + 3] = f[3*i + 2];
      	      fnew[6*i + 4] = f[3*i + 2];
      	      fnew[6*i + 5] = f[3*i];
      	    }
          }
        } else if (has_spheres) {
          // default
        } else if (type === "surface") {
          dim = obj.dim[0];
          nx = dim[0];
          nz = dim[1];
          if (pmode === "filled") {
            fnew = [];
            for (j=0; j<nx-1; j++) {
              for (i=0; i<nz-1; i++) {
                fnew.push(f[j + nx*i],
                       f[j + nx*(i+1)],
                       f[j + 1 + nx*(i+1)],
                       f[j + nx*i],
                       f[j + 1 + nx*(i+1)],
                       f[j + 1 + nx*i]);
              }
            }
          } else if (pmode === "lines") {
            fnew = [];
            for (j=0; j<nx; j++) {
              for (i=0; i<nz; i++) {
                if (i+1 < nz)
                  fnew.push(f[j + nx*i],
                         f[j + nx*(i+1)]);
                if (j+1 < nx)
                  fnew.push(f[j + nx*i],
                         f[j+1 + nx*i]);
              }
            }
          }
        }
        obj.f[pass] = fnew;
        if (depth_sort) {
          drawtype = "DYNAMIC_DRAW";
        } else {
          drawtype = "STATIC_DRAW";
        }
      }
    }
    
    if (fl.fat_lines) {
      alias = undefined;
      obj.nextLoc = gl.getAttribLocation(obj.prog, "aNext");
      obj.pointLoc = gl.getAttribLocation(obj.prog, "aPoint");
      obj.aspectLoc = gl.getUniformLocation(obj.prog, "uAspect");
      obj.lwdLoc = gl.getUniformLocation(obj.prog, "uLwd");
      // Expand vertices to turn each segment into a pair of triangles
        
      	for (pass = 0; pass < obj.passes; pass++) {
      	  f = obj.f[pass];	
          oldrows = f.length;
      	  if (obj.pmode[pass] === "lines") 
      	    break;
      	}
      
      if (type === "linestrip") 
        nrows = 4*(oldrows - 1); 
      else
        nrows = 2*oldrows;
      vnew = new Array(nrows);
      fnew = new Array(1.5*nrows);
      
      // We're going to turn each pair of vertices into 4 new ones, with the "next" and "pt" attributes
      // added.
      // We do this by copying the originals in the first pass, adding the new attributes, then in a 
      // second pass add new vertices at the end.

      for (i = 0; i < v.length; i++) {
        vnew[i] = v[i].concat([0,0,0,0,0]); 
      }

      nextofs = stride;
      pointofs = stride + 3;
      stride = stride + 5;
            
      // Now add the extras
      var ind, k;
      last = v.length - 1;
      ind = 0;
      alias = new Array(f.length);
      for (i = 0; i < f.length; i++)
        alias[i] = [];
      for (i = 0; i < f.length - 1; i++) {
      	if (type !== "linestrip" && i % 2 === 1)
      	  continue;
      	k = ++last;
      	vnew[k] = vnew[f[i]].slice();
      	for (j=0; j<3; j++)
      	  vnew[k][nextofs + j] = vnew[f[i+1]][j];
      	vnew[k][pointofs] = -1;
      	vnew[k][pointofs+1] = -1;
      	fnew[ind] = k;
      	last++;
      	vnew[last] = vnew[k].slice();
      	vnew[last][pointofs] = 1;
      	fnew[ind+1] = last;
      	alias[f[i]].push(last-1, last);
      	last++;
      	k = last;
      	vnew[k] = vnew[f[i+1]].slice();
      	for (j=0; j<3; j++)
      	  vnew[k][nextofs + j] = vnew[f[i]][j];
      	vnew[k][pointofs] = -1;
      	vnew[k][pointofs+1] = 1;
      	fnew[ind+2] = k;
      	fnew[ind+3] = fnew[ind+1];
      	last++;
      	vnew[last] = vnew[k].slice();
      	vnew[last][pointofs] = 1;
      	fnew[ind+4] = last;
      	fnew[ind+5] = fnew[ind+2];
      	ind += 6;
      	alias[f[i+1]].push(last-1, last);
      }
      vnew.length = last+1;
      v = vnew;
      obj.vertexCount = v.length;
      if (typeof alias !== "undefined" && typeof obj.alias !== "undefined") {  // Already have aliases from previous section?
        var oldalias = obj.alias, newalias = Array(obj.alias.length);
        for (i = 0; i < newalias.length; i++) {
          newalias[i] = oldalias[i].slice();
          for (j = 0; j < oldalias[i].length; j++)
            Array.prototype.push.apply(newalias[i], alias[oldalias[j]]); // pushes each element 
        }
        obj.alias = newalias;
      } else
        obj.alias = alias;
      
      for (pass = 0; pass < obj.passes; pass++)
      	if (type === "lines" || type === "linestrip" || obj.pmode[pass] === "lines") {
          obj.f[pass] = fnew;
        }
      
      if (depth_sort) 
        drawtype = "DYNAMIC_DRAW";
      else
        drawtype = "STATIC_DRAW";
    }
    
      for (pass = 0; pass < obj.passes; pass++) {
        if (obj.vertexCount > 65535) {
          if (this.index_uint) {
            obj.f[pass] = new Uint32Array(obj.f[pass]);
            obj.index_uint = true;
          } else
            this.alertOnce("Object has "+obj.vertexCount+" vertices, not supported in this browser.");
        } else {
          obj.f[pass] = new Uint16Array(obj.f[pass]);
          obj.index_uint = false;
        }
      }
    
    if (stride !== v[0].length) {
      this.alertOnce("problem in stride calculation");
    }

    obj.vOffsets = {vofs:0, cofs:cofs, nofs:nofs, radofs:radofs, oofs:oofs, tofs:tofs,
                    nextofs:nextofs, pointofs:pointofs, stride:stride};

    obj.values = new Float32Array(rglwidgetClass.flatten(v));

    if (!has_spheres && !sprites_3d) {
      obj.buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
      gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW); //
      obj.ibuf = Array(obj.passes);
      obj.ibuf[0] = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[0]);
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[0], gl[drawtype]);
      if (fl.is_twosided) {
      	obj.ibuf[1] = gl.createBuffer();
      	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[1]);
      	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[1], gl[drawtype]);
      }
    }

    if (!sprites_3d) {
      obj.mvMatLoc = gl.getUniformLocation(obj.prog, "mvMatrix");
      obj.prMatLoc = gl.getUniformLocation(obj.prog, "prMatrix");

      if (fl.fixed_size) {
        obj.textScaleLoc = gl.getUniformLocation(obj.prog, "textScale");
      }
    }

    if (fl.needs_vnormal) {
      obj.normLoc = gl.getAttribLocation(obj.prog, "aNorm");
      obj.normMatLoc = gl.getUniformLocation(obj.prog, "normMatrix");
    }

    if (fl.is_twosided) {
      obj.frontLoc = gl.getUniformLocation(obj.prog, "front");
      if (fl.has_normals)
        obj.invPrMatLoc = gl.getUniformLocation(obj.prog, "invPrMatrix");
    }
  };
    
    /**
     * Initialize the DOM object
     * @param { Object } el - the DOM object
     * @param { Object } x - the scene data sent by JSON from R
     */
    rglwidgetClass.prototype.initialize = function(el, x) {
      this.textureCanvas = document.createElement("canvas");
      this.textureCanvas.style.display = "block";
      this.scene = x;
      this.normMatrix = new CanvasMatrix4();
      this.invPrMatrix = new CanvasMatrix4();
      this.saveMat = {};
      this.distance = null;
      this.posLoc = 0;
      this.colLoc = 1;
      if (el) {
        el.rglinstance = this;
        this.el = el;
        this.webGLoptions = el.rglinstance.scene.webGLoptions;
        this.initCanvas();
      }
      if (typeof Shiny !== "undefined") {
        var self = this;
        Shiny.addCustomMessageHandler("shinyGetPar3d",
          function(message) {
            var i, param, 
                subscene = self.getObj(message.subscene),
                parameters = [].concat(message.parameters),
                result = {tag: message.tag, subscene: message.subscene};
            if (typeof subscene !== "undefined") {
              for (i = 0; i < parameters.length; i++) {
                param = parameters[i];
                result[param] = subscene.par3d[param];
              }
            } else {
              console.log("subscene "+message.subscene+" undefined.");
            }
            Shiny.setInputValue("par3d:shinyPar3d", result, {priority: "event"});
          });
          
        Shiny.addCustomMessageHandler("shinySetPar3d",
          function(message) {
            var param = message.parameter, 
                subscene = self.getObj(message.subscene);
            if (typeof subscene !== "undefined") {
              subscene.par3d[param] = message.value;
              subscene.initialized = false;
              self.drawScene();
            } else {
              console.log("subscene "+message.subscene+" undefined.");
            }
          });
          
        Shiny.addCustomMessageHandler("resetBrush",
          function(message) {
            if (message === self.scene.selectionInput) {
              self.clearBrush(null);
              self.recordSelection(0);
            }
          });
      }
    };
    
    /**
     * Restart the WebGL canvas
     */
    rglwidgetClass.prototype.restartCanvas = function() {
      var newcanvas = document.createElement("canvas"),
          self = this, 
          labelid = this.el.getAttribute("aria-labelledby");
      newcanvas.width = this.el.width;
      newcanvas.height = this.el.height;
      newcanvas.setAttribute("aria-labelledby", 
        labelid);
        
      if (typeof this.scene.altText !== "undefined") {
        // We're in Shiny, so alter the label
        var label = document.getElementById(labelid);
        if (label)
          label.innerHTML = this.scene.altText;
      }
      newcanvas.addEventListener("webglcontextrestored",
        this.onContextRestored, false);
      newcanvas.addEventListener("webglcontextlost",
        this.onContextLost, false);
      while (this.el.firstChild) {
        this.el.removeChild(this.el.firstChild);
      }
      this.el.appendChild(newcanvas);
      this.canvas = newcanvas;
      if (this.scene.javascript) {
        /* jshint evil:true */
        Function('"use strict";' + this.scene.javascript)();
        /* jshint evil:false */
      }
      this.setMouseHandlers();
      if (this.gl) 
        Object.keys(this.scene.objects).forEach(function(key){
          self.getObj(parseInt(key, 10)).texture = undefined; 
          });
      this.gl = null;
    };

    /**
     * Initialize the WebGL canvas
     */
    rglwidgetClass.prototype.initCanvas = function() {
      this.restartCanvas();
      var objs = this.scene.objects,
          self = this;
          
      /* These hold context specific data.  In Shiny, they   
         need to be deleted.  Elsewhere, they don't exist
         and these are no-ops. */
         
      delete this.cube;
      delete this.sphere;
      
      Object.keys(objs).forEach(function(key){
        self.initSubscene(parseInt(key, 10));
      });

      this.onContextRestored = function() {
        self.initGL();
        self.drawScene();
      };

      this.onContextLost = function(event) {
        if (!self.drawing)
          this.gl = null;
        event.preventDefault();
      };

      this.initGL0();
      this.lazyLoadScene = function() {
      	if (typeof self.slide === "undefined")
      	  self.slide = self.getSlide();
      	if (self.isInBrowserViewport()) {
      	  if (!self.gl || self.gl.isContextLost())
      	    self.initGL();
      	  self.drawScene();
      	}
      };
      window.addEventListener("DOMContentLoaded", this.lazyLoadScene, false);
      window.addEventListener("load", this.lazyLoadScene, false);
      window.addEventListener("resize", this.lazyLoadScene, false);
      window.addEventListener("scroll", this.lazyLoadScene, false);
      this.slide = this.getSlide();
      if (this.slide) {
        if (typeof this.slide.rgl === "undefined")
          this.slide.rgl = [this];
        else
          this.slide.rgl.push(this);
        if (this.scene.context.rmarkdown) 
          if (this.scene.context.rmarkdown === "ioslides_presentation") {
            this.slide.setAttribute("slideenter", "this.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window);})");
          } else if (this.scene.context.rmarkdown === "slidy_presentation") {
            // This method would also work in ioslides, but it gets triggered
            // something like 5 times per slide for every slide change, so
            // you'd need a quicker function than lazyLoadScene.
            var MutationObserver = window.MutationObserver || window.WebKitMutationObserver || window.MozMutationObserver,
            observer = new MutationObserver(function(mutations) {
              mutations.forEach(function() {
                self.slide.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window); });});});
            observer.observe(this.slide, { attributes: true, attributeFilter:["class"] });
          }
      }
    };
