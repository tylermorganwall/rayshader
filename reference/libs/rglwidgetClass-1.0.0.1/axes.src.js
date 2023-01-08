    /**
     * Methods related to axes
     * @name ___METHODS_FOR_AXES___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */
     
    /**
     * Choose edges for ticks
     * @param { Matrix } prmv - projection-model-view matrix
     */
    rglwidgetClass.prototype.getTickEdges = function(prmv){
      var vertices = [[0,0,0,1], [0,0,1,1],
                      [0,1,0,1], [0,1,1,1],
                      [1,0,0,1], [1,0,1,1],
                      [1,1,0,1], [1,1,1,1]], 
           dim, i, j, k, edges, hull, step, result = [], proj = [],

        // Filter to edges that are on sides that would
        // be shown with a filled backing.
        
      has_back = function(edge) {
          var normals = [[], []],
              verts = [vertices[edge[0]], 
                       vertices[edge[1]]], 
              normal, m, n;
          n = 0;
          for (m=0; m<3; m++) {
            if (verts[0][m] === verts[1][m]) {
              normals[n] = [0,0,0,1];
              normals[n][m] = 2*verts[0][m] - 1;
              n++;
            }
          }
          for (n=0; n<2; n++) {
            normal = rglwidgetClass.multVM(normals[n], self.normMatrix);
            if (normal[2] < 0 ||
                (normal[2] === 0 && normal[0] < 0))
              return true;
          }
          return false;
        }, self = this;
        
      for (i = 0; i < vertices.length; i++) {
        proj[i] = rglwidgetClass.multVM(vertices[i], prmv);
        proj[i][0] = proj[i][0]/proj[i][3];
        proj[i][1] = proj[i][1]/proj[i][3];
        proj[i][2] = i;
      }
      hull = rglwidgetClass.chull(proj.slice());  
      for (i = 0; i < hull.length; i++)
        hull[i] = hull[i][2];
      hull.push(hull[0]);
      for (dim = 0; dim < 3; dim++) { 
        edges = [];
        step = Math.pow(2, 2-dim);
        for (i = 0; i < 4; i++) {
          j = (dim === 0) ? i : (dim === 1) ? i + 2*(i>1) : 2*i;
          for (k = 0; k < hull.length - 1; k++) {
            if ((hull[k] === j && hull[k+1] === j + step) ||
                (hull[k] === j+step && hull[k+1] === j))
          
              edges.push([j, j+step], [j+step, j]);
          }
        }

        edges = edges.filter(has_back);
        
        // Find the edge with a vertex closest
        // to the bottom left corner
        if (edges.length) {
          var best, best2, val = Infinity, newval;
          for (i = 0; i < edges.length; i++) {
            j = edges[i][0];
            newval = proj[j][0] + proj[j][1];
            if (newval < val) {
              best = j;
              best2 = edges[i][1];
              val = newval;
            }
          }
          if (typeof best !== "undefined") {
            result[dim] = vertices[best].slice(0,3);
            result[dim][dim] = undefined;
          } else
            result[dim] = undefined;
        }
      }
      return result;
    };
    
    /**
     * Choose tick locations
     * @param { Object } obj - The bboxdeco
    */
    rglwidgetClass.prototype.getTickLocations = function(obj){
      var dim, i, limits, locations = [], result = [[],[],[]], value,
          len, delta, range, bbox = obj.bbox;
      obj.needsAxisCallback = false;
      for (dim = 0; dim < 3; dim++) {
        limits = bbox.slice(2*dim, 2*dim + 2);
        range = limits[1] - limits[0];
        switch(obj.axes.mode[dim]) {
        case "custom":
          for (i=0; i < obj.vertices.length; i++) {
            value = (obj.vertices[i][dim] - limits[0])/range;
            if (typeof value !== "undefined" &&
                !isNaN(value))
              result[dim].push(value);
          }
          break;
        case "fixedstep":
          len = Math.floor(range/obj.axes.step[dim]);
          delta = obj.axes.step[dim];
          for (i = 0; i < len; i++)
            result[dim].push(i*delta);          
          break;
        case "fixednum":
          len = obj.axes.nticks[dim];
          delta = (len > 1) ? range/(len-1) : 0;
          for (i = 0; i < len; i++)
            result[dim].push(i*delta/range);
          break;
        case "pretty":
          locations = this.R_pretty(limits[0], limits[1], 5,
                                  2, // min_n
                                  0.75, // shrink_sml
                                  [1.5, 2.75], // high_u_fact
                                  0, // eps_correction
                                  0); // return_bounds)  
          for (i = locations.lo; i <= locations.up; i++) {
            value = (i*locations.unit - limits[0])/range;
            if (0 < value && value < 1)
              result[dim].push(value);
          }
          break;
        case "user":
          obj.needsAxisCallback = true;
          break;
        }
      }
      return result;
    };
    
    /**
     * Set tick vertices
     * @param { Object } ticks - the tick object
     * @param { Array }  edges - Which edges get the ticks?
    */
    rglwidgetClass.prototype.getTickVertices = function(ticks) {
      var dim, i, j, vertices = [], locations, 
          edges = ticks.edges, edge;
      for (dim = 0; dim < 3; dim++) {
        locations = ticks.locations[dim];
        if (locations.length)
          for (i = 0; i < locations.length; i++) 
            if (typeof edges[dim] !== "undefined") {
              edge = edges[dim].slice();
              edge[dim] = locations[i];
              vertices.push(edge);
              edge = edge.slice();
              for (j = 0; j < 3; j++)       
                if ((dim < 2 && j === 1 - dim) || 
                    (dim === 2 && j === 0))
                  edge[j] += 2*(edge[j] - 0.5)/ticks.axes.marklen[dim];
              vertices.push(edge);
            }
        }
      ticks.vertices = vertices;
      ticks.vertexCount = vertices.length;
      ticks.values = new Float32Array(rglwidgetClass.flatten(vertices));
      ticks.initialized = false;
    };
    
    /**
     * Set tick label positions
     * @param { Object } obj - the bbox object
    */
    rglwidgetClass.prototype.placeTickLabels = function(obj) {
      var ticks = obj.ticks, labels = obj.labels, i,j,k,
          vertices = [], tickvertices = ticks.vertices, 
          vertex, locations, dim, edges = obj.ticks.edges;
      j = 0;
      for (dim = 0; dim < 3; dim++) {
        if (typeof edges[dim] === "undefined") 
          continue;
        locations = ticks.locations[dim];
        if (locations.length)
          for (i = 0; i < locations.length; i++) {
            if (isNaN(locations[i]))
              continue;
            while (j < tickvertices.length && 
                   tickvertices[j][dim] !== locations[i]) j++;
            if (j >= tickvertices.length)
              break;
            vertex = tickvertices[j].slice();
            for (k = 0; k < 3; k++)
              vertex[k] += 2*(tickvertices[j+1][k] - vertex[k]);
            vertices.push(vertex);
            j += 2;
          }
        }
      labels.vertices = vertices;
      labels.centers = labels.vertices;
      labels.initialized = false;
    };  
     
    /**
     * Set tick labels
     * @param { Object } obj - the bbox object
     */ 
    rglwidgetClass.prototype.setTickLabels = function(obj) {
      var ticks = obj.ticks, mode, locations, labels = [],
      start = 0, nticks, dim, i, limits, range, values, max,
      edges = obj.ticks.edges;
      for (dim = 0; dim < 3; dim++) {
        if (typeof edges[dim] === "undefined") 
          continue;
        mode = obj.axes.mode[dim];
        nticks = obj.axes.nticks[dim]; // used on input only for custom!
        if (mode === "custom") 
          labels = labels.concat(obj.texts.slice(start, start + nticks));
        else {
          limits = obj.bbox.slice(2*dim, 2*(dim+1));
          range = limits[1] - limits[0];
          locations = ticks.locations[dim];
          max = -Infinity;
          values = [];
          for (i = 0; i < locations.length; i++) {
            values.push(limits[0] + range*locations[i]);
            max = Math.max(max, Math.abs(values[i]));
          }
          for (i = 0; i < locations.length; i++) {
            if (Math.abs(values[i])/max < Math.pow(10, -5))
              values[i] = 0;
            labels.push(rglwidgetClass.signif(values[i], 4).toString());
          }
          obj.axes.nticks[dim] = locations.length;  
        }
        start += nticks;
      }
      obj.labels.texts = labels;
    };

    /**
     * Set bboxdeco bbox and center vector 
     * @param { Object } obj - the bbox object
     */ 
    rglwidgetClass.prototype.setBbox = function(obj, subscene) {
      var i, expand, center = [], bbox;
      if (!obj.initialized)
        this.initBBox(obj);
        
      bbox = [].concat(subscene.par3d.bbox);
      for (i = 0; i < 3; i++) {
        expand = obj.axes.expand[i];
        center[i] = (bbox[2*i] + bbox[2*i + 1])/2;
        bbox[2*i] = center[i] - expand*(bbox[2*i + 1] - center[i]);
        bbox[2*i+1] = center[i] + expand*(bbox[2*i + 1] - center[i]);
      }
      obj.bbox = bbox;
      obj.center = center;
    };

    rglwidgetClass.prototype.setBBoxMatrices = function(obj) {
      var saved = {normMatrix: new CanvasMatrix4(this.normMatrix),
                   mvMatrix: new CanvasMatrix4(this.mvMatrix)},
          bboxNorm, bboxMV, bbox = obj.bbox, scale;
          
      bboxNorm = new CanvasMatrix4();
      scale = [bbox[1]-bbox[0], bbox[3]-bbox[2], bbox[5]-bbox[4]];
      bboxNorm.scale(1/scale[0], 1/scale[1], 1/scale[2]);
      bboxNorm.multRight(saved.normMatrix);
      this.normMatrix = bboxNorm;

      bboxMV = new CanvasMatrix4();
      bboxMV.scale(scale[0], scale[1], scale[2]);
      bboxMV.translate(bbox[0], bbox[2], bbox[4]);
      bboxMV.multRight(saved.mvMatrix);
      this.mvMatrix = obj.mvMatrix = bboxMV;
      
      if (this.prmvMatrix === null)
        saved.prmvMatrix = null;
      else
        saved.prmvMatrix = new CanvasMatrix4(this.prmvMatrix);
        
      this.setprmvMatrix();
      obj.prmvMatrix = this.prmvMatrix;
      
      return saved;
    };
    
    rglwidgetClass.prototype.restoreBBoxMatrices = function(saved) {
      this.normMatrix = saved.normMatrix;
      this.mvMatrix   = saved.mvMatrix;
      this.prmvMatrix = saved.prmvMatrix;
    };
    
    rglwidgetClass.prototype.getMarginParameters = function(bboxdeco, material) {
      // Assume we've run this.setBbox(bboxdeco, subscene);
      var bbox = bboxdeco.bbox,
          edge = [].concat(material.edge),
          saved, edges, i, 
          at = material.margin, line, level, trans, scale;

      if (material.floating) {
        saved = this.setBBoxMatrices(bboxdeco);
        edges = this.getTickEdges(this.prmvMatrix)[at];
        this.restoreBBoxMatrices(saved);
        if (typeof edges !== "undefined")
          for (i = 0; i < 3; i++) {
            if (edges[i] < 1) edges[i] = -1;
              edge[i] = edge[i]*edges[i];
        } else
          return undefined;
      }
      switch(at) {
      case 0: line = 1;
              level = 2;
              break;
      case 1: line = 0;
              level = 2;
              break;
      case 2: line = 0;
              level = 1;
              break;
      }
      scale = [edge[0]*(bbox[1]-bbox[0])/bboxdeco.axes.marklen[0], 
               edge[1]*(bbox[3]-bbox[2])/bboxdeco.axes.marklen[1], 
               edge[2]*(bbox[5]-bbox[4])/bboxdeco.axes.marklen[2]];
      trans = [edge[0] === 1 ? bbox[1] : bbox[0],
               edge[1] === 1 ? bbox[3] : bbox[2],
               edge[2] === 1 ? bbox[5] : bbox[4]];
      return {at: at, line: line, level: level, trans: trans, scale: scale};        
    };
    
    rglwidgetClass.prototype.fixVertex = function(orig, parms, center, bbox) {
      var vertex = [0,0,0];
      if (rglwidgetClass.missing(orig[0]))
        vertex[parms.at] = center[parms.at];
      else if (orig[0] === "-Inf")
        vertex[parms.at] = bbox[2*parms.at];
      else if (orig[0] === "Inf")
        vertex[parms.at] = bbox[2*parms.at + 1];
      else
        vertex[parms.at] = orig[0];
      vertex[parms.line] = parms.scale[parms.line]*orig[1] + 
          parms.trans[parms.line];
      vertex[parms.level] = parms.scale[parms.level]*orig[2] + 
          parms.trans[parms.level];
      return vertex;
    };
    
    rglwidgetClass.prototype.fixNormal = function(orig, parms) {
      var vertex = [0,0,0];
      vertex[parms.at] = orig[0];
      vertex[parms.line] = orig[1]/parms.scale[parms.line];
      vertex[parms.level] = orig[2]/parms.scale[parms.level];
      return vertex;
    };

    rglwidgetClass.prototype.marginVecToDataVec = function(obj, subscene) {
      var bboxdeco = this.getBBoxDeco(subscene),
          center, bbox, parms, parmsjson,
          orig = obj.orig, 
          vertices = [], normals = [],
          centers = [], i, vertex;
      if (typeof orig === "undefined") {
        orig = {vert: obj.vertices,
                norm: obj.normals,
                cent: obj.centers,
                doNormals: typeof obj.normals !== "undefined",
                doCenters: typeof obj.centers !== "undefined",
                parms: ""
               };
        obj.orig = orig;
      }

      if (typeof bboxdeco !== "undefined") {
        this.setBbox(bboxdeco, subscene);
        center = bboxdeco.center;
        bbox = bboxdeco.bbox;
        parms = this.getMarginParameters(bboxdeco, obj.material);
        if (typeof parms === "undefined")
          return false;  /* axis is not currently shown */
        
        parmsjson = JSON.stringify(parms);
        if (parmsjson === orig.parms)
          return true;  /* nothing has changed */
    
        orig.parms = parmsjson;
        
        for (i=0; i < orig.vert.length; i++) {
          vertex = this.fixVertex(orig.vert[i], parms, center, bbox);
          vertices.push(vertex);
        }
        obj.vertices = vertices;
        if (orig.doNormals) {
          for (i=0; i < orig.norm.length; i++) {
            vertex = this.fixNormal(orig.norm[i], parms);
            normals.push(vertex);
          }
          obj.normals = normals;
        }
        if (orig.doCenters) {
          for (i=0; i < orig.cent.length; i++) {
            vertex = this.fixVertex(orig.cent[i], parms, center, bbox);
            centers.push(vertex);
          }
          obj.centers = centers;
        }
        
        obj.initialized = false;
        return true;
      } else {
        console.warn("bboxdeco not found");
        return false;
      }
    };

    rglwidgetClass.prototype.doAxisCallback = function(obj, edges) {
      var i, j, code, axis, fn;
      for (i = 0; i < 3; i++) {
        if (obj.axes.mode[i] === "user") {
          axis = ["x", "y", "z"][i];
          if (typeof obj.callbacks !== "undefined" &&
              typeof (code = obj.callbacks[axis]) !== "undefined") {
            if (typeof edges[i] !== "undefined")
              for (j = 0; j < 3; j++)
                if (typeof edges[i][j] !== "undefined")
                  axis = axis + (edges[i][j] > 0 ? "+" : "-");
            
          /* jshint evil:true */
            fn = Function('"use strict";return (' + code + ')')();
          /* jshint evil:false */
            fn.call(this, axis);
          }
        }
      }
    };
