/**
 * Methods related to drawing transparent objects
 * @name ___METHODS_FOR_TRANSPARENCY___
 * @memberof rglwidgetClass
 * @kind function
 * @instance

 * These functions order the centers of displayed objects so they
 * can be drawn using the painters algorithm, necessary to support
 * transparency.  

 * Note that objid is not obj.id when drawing spheres.
 */

/**
 * Break objects into pieces
 * @returns { array } Array of pieces
 */
    rglwidgetClass.prototype.getPieces = function(context, objid, subid, obj) {
      var n = obj.centers.length,
          depth,
          result = new Array(n),
          z, w, i;
      context = context.slice();
          
      for(i=0; i<n; i++) {
        z = this.prmvMatrix.m13*obj.centers[i][0] +
            this.prmvMatrix.m23*obj.centers[i][1] +
            this.prmvMatrix.m33*obj.centers[i][2] +
            this.prmvMatrix.m43;
        w = this.prmvMatrix.m14*obj.centers[i][0] +
            this.prmvMatrix.m24*obj.centers[i][1] +
            this.prmvMatrix.m34*obj.centers[i][2] +
            this.prmvMatrix.m44;
        depth = z/w;
        result[i] = {context: context, 
                     objid: objid,
                     subid: subid,
                     index: i, 
                     depth: depth};
      }
      return result;    
    };
    
    /**
     * Get pieces from sphere
     * @returns { object }
     * @param { array } context - 
     * @param { numeric } subid - subscene
     * @param { object } obj - spheres object
     */
    rglwidgetClass.prototype.getSpherePieces = function(context, subid, obj)
    {
      if (obj.fastTransparency) 
        if (subid === 0) // Only compute pieces once
          return this.getPieces(context, obj.id, -1, obj);
        else
          return [];
      else
        return this.getPieces(context, obj.id, subid, this.sphere);
    };
    
   /**
     * Get pieces from cube
     * @returns { object }
     * @param { array } context - 
     * @param { numeric } subid - subscene
     * @param { object } obj - spheres object
     */
    rglwidgetClass.prototype.getCubePieces = function(context, obj)
    {
      return this.getPieces(context, obj.id, 0, this.cube);
    };
    
    /**
     * Merge pieces that can be drawn in one call
     * @returns { object }
     * @param { array } pieces - The pieces to merge
     */
    rglwidgetClass.prototype.mergePieces = function(pieces) {
      var result = [];
      if (pieces.length > 0) {
        var i,
          thiscontext = pieces[0].context, 
          thisobjid = pieces[0].objid, 
          thissubid = pieces[0].subid,
          indices = [];
        for (i= 0; i < pieces.length; i++) {
          if (pieces[i].context !== thiscontext || 
              pieces[i].objid !== thisobjid ||
              pieces[i].subid !== thissubid) {
            result.push({context: thiscontext, objid: thisobjid,
                         subid: thissubid, indices: indices});
            thiscontext = pieces[i].context;
            thisobjid = pieces[i].objid;
            thissubid = pieces[i].subid;
            indices = [];
          }
          indices.push(pieces[i].index);
        }
        result.push({context: thiscontext, objid: thisobjid,
                                subid: thissubid,
                                indices: indices});
      }
      return result;
    };

    /**
     * Sort pieces by depth
     * @returns { array }
     * @param { array } pieces - array of pieces 
     */
    rglwidgetClass.prototype.sortPieces = function(pieces) {
      var compare = function(i,j) {
        var diff = j.depth - i.depth;
        // We want to avoid context or obj changes,
        // so sort on those next.
        if (diff === 0) {
          var c1 = j.context.slice(),
              c2 = i.context.slice();
          diff = c1.length - c2.length; 
          while (diff === 0 && c1.length > 0) {
            diff = c1.pop() - c2.pop();
          }
          if (diff === 0)
            diff = j.objid - i.objid;
          if (diff === 0)
            diff = j.subid - i.subid;
        }
        return diff;
      }, result = [];
      if (pieces.length) 
        result = pieces.sort(compare);
      return result;
    };
