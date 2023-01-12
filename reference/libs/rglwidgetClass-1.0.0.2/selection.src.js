    /**
     * Methods related to selection
     * @name ___METHODS_FOR_SELECTION___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */
     
    /**
     * Respond to brush change
     */
    rglwidgetClass.prototype.selectionChanged = function() {
      var i, j, k, id, subid = this.select.subscene, subscene,
          objids, obj,
          p1 = this.select.region.p1, p2 = this.select.region.p2,
          filter, selection = [], handle, keys, xmin, x, xmax, ymin, y, ymax, z, v,
          someHidden;
      if (!subid)
        return;
      subscene = this.getObj(subid);
      objids = subscene.objects;
      filter = this.scene.crosstalk.filter;
      this.setmvMatrix(subid);
      this.setprMatrix(subid);
      this.setprmvMatrix();
      xmin = Math.min(p1.x, p2.x);
      xmax = Math.max(p1.x, p2.x);
      ymin = Math.min(p1.y, p2.y);
      ymax = Math.max(p1.y, p2.y);
      for (i = 0; i < objids.length; i++) {
      	id = objids[i];
      	j = this.scene.crosstalk.id.indexOf(id);
      	if (j >= 0) {
      	  keys = this.scene.crosstalk.key[j];
      	  obj = this.getObj(id);
      	  someHidden = false;
      	  for (k = 0; k < keys.length; k++) {
      	    if (filter && filter.indexOf(keys[k]) < 0) {
      	      someHidden = true;
      	      continue;
      	    }
      	    v = [].concat(obj.vertices[k]).concat(1.0);
            v = rglwidgetClass.multVM(v, this.prmvMatrix);
            x = v[0]/v[3];
            y = v[1]/v[3];
            z = v[2]/v[3];
            if (xmin <= x && x <= xmax && ymin <= y && y <= ymax && -1.0 <= z && z <= 1.0) {
              selection.push(keys[k]);
            } else
              someHidden = true;
      	  }
      	  obj.someHidden = someHidden && (filter || selection.length);
      	  obj.initialized = false;
      	  /* Who should we notify?  Only shared data in the current subscene, or everyone? */
      	  if (!this.equalArrays(selection, this.scene.crosstalk.selection)) {
      	    handle = this.scene.crosstalk.sel_handle[j];
      	    handle.set(selection, {rglSubsceneId: this.select.subscene});
      	  }
      	}
      }
    };
    
    /**
     * Respond to selection or filter change from crosstalk
     * @param { Object } event - crosstalk event
     * @param { boolean } filter - filter or selection?
     */
    rglwidgetClass.prototype.selection = function(event, filter) {
      	var i, j, ids, obj, keys, crosstalk = this.scene.crosstalk,
      	    selection, someHidden;

      	// Record the message and find out if this event makes some objects have mixed values:
      	
      	crosstalk = this.scene.crosstalk;
      	
      	if (filter) {
      	  filter = crosstalk.filter = event.value;
      	  selection = crosstalk.selection;
      	} else {  
          selection = crosstalk.selection = event.value;
          filter = crosstalk.filter;
      	}
        ids = crosstalk.id;
        for (i = 0; i < ids.length ; i++) {
          obj = this.getObj(ids[i]);
          obj.initialized = false;
          keys = crosstalk.key[i];
          someHidden = false;
          for (j = 0; j < keys.length && !someHidden; j++) {
            if ((filter && filter.indexOf(keys[j]) < 0) ||
                (selection.length && selection.indexOf(keys[j]) < 0))
                someHidden = true;
          }
          obj.someHidden = someHidden;
        }
        this.drawScene();
    };
    
    /**
     * Clear the selection brush
     * @param { number } except - Subscene that should ignore this request
     */
    rglwidgetClass.prototype.clearBrush = function(except) {
      if (this.select.subscene !== except) {
        this.select.region = {p1: {x:Infinity, y:Infinity}, 
                              p2: {x:Infinity, y:Infinity}};
        this.selectionChanged();
        this.select.state = "inactive";
        this.delFromSubscene(this.scene.brushId, this.select.subscene);
      }
      this.drawScene();
    };
    
    /**
     * Set the vertices in the selection box object
     */
    rglwidgetClass.prototype.initSelection = function(id) {
      if (typeof this.select.region === "undefined")
        return;
      var obj = this.getObj(id),
          p1 = this.select.region.p1,
          p2 = this.select.region.p2;
          
      obj.vertices = [[p1.x, p1.y, 0.0],
                      [p2.x, p1.y, 0.0],
                      [p2.x, p2.y, 0.0],
                      [p1.x, p2.y, 0.0],
                      [p1.x, p1.y, 0.0]];
    };
