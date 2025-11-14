    /**
     * Methods related to subscenes
     * @name ___METHODS_FOR_SUBSCENES___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */

    /**
     * Is a particular id in a subscene?
     * @returns { boolean }
     * @param {number} id Which id?
     * @param {number} subscene Which subscene id?
     */
    rglwidgetClass.prototype.inSubscene = function(id, subscene) {
      return this.getObj(subscene).objects.indexOf(id) > -1;
    };

    /**
     * Translate from window coordinates to viewport coordinates
     * @returns { Object } translated coordinates
     * @param { number } subsceneid - which subscene to use?
     * @param { Object } coords - point to translate
     */
    rglwidgetClass.prototype.translateCoords = function(subsceneid, coords) {
      var viewport = this.getObj(subsceneid).par3d.viewport;
      return {x: coords.x - viewport.x*this.canvas.width,
              y: coords.y - viewport.y*this.canvas.height};
    };

    /**
     * Check whether point is in viewport of subscene
     * @returns {boolean}
     * @param { Object } coords - screen coordinates of point
     * @param { number } subsceneid - subscene to check
     */
    rglwidgetClass.prototype.inViewport = function(coords, subsceneid) {
      var viewport = this.getObj(subsceneid).par3d.viewport,
        x0 = coords.x - viewport.x*this.canvas.width,
        y0 = coords.y - viewport.y*this.canvas.height;
      return 0 <= x0 && x0 <= viewport.width*this.canvas.width &&
             0 <= y0 && y0 <= viewport.height*this.canvas.height;
    };

    /**
     * Find which subscene contains a point
     * @returns { number } subscene id
     * @param { Object } coords - coordinates of point
     */
    rglwidgetClass.prototype.whichSubscene = function(coords) {
      var self = this,
          recurse = function(subsceneid) {
            var subscenes = self.getChildSubscenes(subsceneid), i, id;
            for (i=0; i < subscenes.length; i++) {
              id = recurse(subscenes[i]);
              if (typeof(id) !== "undefined")
                return(id);
            }
            if (self.inViewport(coords, subsceneid))
              return(subsceneid);
            else
              return undefined;
          },
          rootid = this.scene.rootSubscene,
          result = recurse(rootid);
      if (typeof(result) === "undefined")
        result = rootid;
      return result;
    };

    /**
     * Add an id to a subscene.
     * @param {number} id Which id?
     * @param {number} subscene Which subscene id?
     */
    rglwidgetClass.prototype.addToSubscene = function(id, subscene) {
      var thelist,
          thesub = this.getObj(subscene),
          ids = [id],
          obj = this.getObj(id), i;
      if (typeof obj !== "undefined" && typeof (obj.newIds) !== "undefined") {
        ids = ids.concat(obj.newIds);
      }
      thesub.objects = [].concat(thesub.objects);
      for (i = 0; i < ids.length; i++) {
        id = ids[i];
        if (thesub.objects.indexOf(id) === -1) {
          thelist = this.whichList(id);
          thesub.objects.push(id);
          thesub[thelist].push(id);
        }
      }
    };

    /**
     * Delete an id from a subscene
     * @param { number } id - the id to add
     * @param { number } subscene - the id of the subscene
     */
    rglwidgetClass.prototype.delFromSubscene = function(id, subscene) {
      var thelist,
          thesub = this.getObj(subscene),
          obj = this.getObj(id),
          ids = [id], i, j;
      if (typeof obj !== "undefined" && typeof (obj.newIds) !== "undefined")
        ids = ids.concat(obj.newIds);
      thesub.objects = [].concat(thesub.objects); // It might be a scalar
      for (j=0; j<ids.length;j++) {
        id = ids[j];
        i = thesub.objects.indexOf(id);
        if (i > -1) {
          thesub.objects.splice(i, 1);
          thelist = this.whichList(id);
          i = thesub[thelist].indexOf(id);
          thesub[thelist].splice(i, 1);
        }
      }
    };

    /**
     * Set the ids in a subscene
     * @param { number[] } ids - the ids to set
     * @param { number } subsceneid - the id of the subscene
     */
    rglwidgetClass.prototype.setSubsceneEntries = function(ids, subsceneid) {
      var sub = this.getObj(subsceneid);
      sub.objects = ids;
      this.initSubscene(subsceneid);
    };

    /**
     * Get the ids in a subscene
     * @returns {number[]}
     * @param { number } subscene - the id of the subscene
     */
    rglwidgetClass.prototype.getSubsceneEntries = function(subscene) {
      return this.getObj(subscene).objects;
    };

    /**
     * Get the ids of the subscenes within a subscene
     * @returns { number[] }
     * @param { number } subscene - the id of the subscene
     */
    rglwidgetClass.prototype.getChildSubscenes = function(subscene) {
      return this.getObj(subscene).subscenes;
    };

    /**
     * Find a particular subscene by inheritance
     * @returns { number } id of subscene to use
     * @param { number } subsceneid - child subscene
     * @param { string } type - type of inheritance:  "projection" or "model"
     */
    rglwidgetClass.prototype.useid = function(subsceneid, type) {
      var sub = this.getObj(subsceneid);
      if (sub.embeddings[type] === "inherit")
        return(this.useid(sub.parent, type));
      else
        return subsceneid;
    };

    /**
     * Find bboxdeco for a subscene
     * @returns { number } id of bboxdeco, or undefined if none
     * @param { number } sub- subscene
     */
    rglwidgetClass.prototype.getBBoxDeco = function(sub) {
      var objects = sub.objects, i, obj;
      for (i = 0; i < objects.length; i++) {
        obj = this.getObj(objects[i]);
        if (obj.type === "bboxdeco")
          return obj;
      }
      if (sub.parent) 
        return this.getBBoxDeco(this.getObj(sub.parent));
      else
        return undefined;
    };
