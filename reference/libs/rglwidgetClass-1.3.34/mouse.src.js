    /**
     * Methods related to mouse handling
     * @name ___METHODS_FOR_MOUSE_HANDLING___
     * @memberof rglwidgetClass
     * @kind function
     * @instance
     */
     
    rglwidgetClass.prototype.getCursor = function(mode) {
      switch(mode) {
        case "none": 
          return "none";
        case "trackball":
        case "xAxis":
        case "yAxis":
        case "zAxis":
        case "polar":
          return "grab";
        case "selecting":
          return "crosshair";
        case "fov":
        case "zoom":
          return "zoom-in";
        case "user":
          return "default";
      }
      return "dragging";
    };
    
    /**
     * Set mouse mode for a subscene
     * @param { string } mode - name of mode
     * @param { number } button - button number (0 to 4)
     * @param { number } subscene - subscene id number
     * @param { number } stayActive - if truthy, don't clear brush
     */
    rglwidgetClass.prototype.setMouseMode = function(mode, button, subscene, stayActive) {
      var sub = this.getObj(subscene),
          which = ["none", "left", "right", "middle", "wheel"][button];
      if (!stayActive && sub.par3d.mouseMode[which] === "selecting")
        this.clearBrush(null);
      sub.par3d.mouseMode[which] = mode;
      if (button === 1 || (button === 0 && mode !== "none"))
        this.canvas.style.cursor = this.getCursor(mode);
      if (button === 0 && mode !== "none")
        sub.needsBegin = mode;
    };

    /**
     * Compute mouse coordinates relative to current canvas
     * @returns { Object }
     * @param { Object } event - event object from mouse click
     */
    rglwidgetClass.prototype.relMouseCoords = function(event) {
      var rect = this.canvas.getBoundingClientRect();
      return {x:event.clientX-rect.left, y:event.clientY-rect.top};
    };
    
    /**
     * Send mouse selection to Shiny
     */
    rglwidgetClass.prototype.recordSelection = function(subid) {
      var result = {};
      if (typeof this.select !== "undefined" && 
          typeof this.select.state !== "undefined" &&
          this.select.state !== "inactive") {
        result = { subscene: subid,
                   state: this.select.state,
                   region: this.select.region
                 };
        this.setmvMatrix(subid);
        result.model = this.mvMatrix;
        this.setprMatrix(subid);
        result.proj = this.prMatrix;
        this.getViewport(subid);
        result.view = this.vp;
      } else
        result.state = "inactive";
      Shiny.setInputValue(this.scene.selectionInput + ":shinyMouse3d", result);
    }; 

    /**
     * Set mouse handlers for the scene
     */
    rglwidgetClass.prototype.setMouseHandlers = function() {
      var self = this, activeSubscene, handler,
          handlers = {}, drag = 0;

      handlers.rotBase = 0;

      self.screenToVector = function(x, y) {
        var viewport = self.getObj(activeSubscene).par3d.viewport,
          width = viewport.width*self.canvas.width,
          height = viewport.height*self.canvas.height,
          radius = Math.max(width, height)/2.0,
          cx = width/2.0,
          cy = height/2.0,
          px = (x-cx)/radius,
          py = (y-cy)/radius,
          plen = Math.sqrt(px*px+py*py);
        if (plen > 1.e-6) {
          px = px/plen;
          py = py/plen;
        }
        var angle = (Math.SQRT2 - plen)/Math.SQRT2*Math.PI/2,
          z = Math.sin(angle),
          zlen = Math.sqrt(1.0 - z*z);
        px = px * zlen;
        py = py * zlen;
        return [px, py, z];
      };

      handlers.trackballdown = function(x,y) {
        var activeSub = self.getObj(activeSubscene),
            activeModel = self.getObj(self.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        handlers.rotBase = self.screenToVector(x, y);
        self.saveMat = [];
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
        }
        self.canvas.style.cursor = "grabbing";
      };

      handlers.trackballmove = function(x,y) {
        var rotCurrent = self.screenToVector(x,y),
            rotBase = handlers.rotBase,
            dot = rotBase[0]*rotCurrent[0] +
                  rotBase[1]*rotCurrent[1] +
                  rotBase[2]*rotCurrent[2],
            angle = Math.acos( dot/rglwidgetClass.vlen(rotBase)/rglwidgetClass.vlen(rotCurrent) )*180.0/Math.PI,
            axis = rglwidgetClass.xprod(rotBase, rotCurrent),
            objects = self.scene.objects,
            activeSub = self.getObj(activeSubscene),
            activeModel = self.getObj(self.useid(activeSub.id, "model")),
            l = activeModel.par3d.listeners,
            i;
        if (angle === 0.0)
          return;    
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.par3d.userMatrix.load(objects[l[i]].saveMat);
          activeSub.par3d.userMatrix.rotate(angle, axis[0], axis[1], axis[2]);
        }
        self.drawScene();
      };
      handlers.trackballend = 0;

      self.clamp = function(x, lo, hi) {
      	return Math.max(lo, Math.min(x, hi));
      };

      self.screenToPolar = function(x,y) {
        var viewport = self.getObj(activeSubscene).par3d.viewport,
          width = viewport.width*self.canvas.width,
          height = viewport.height*self.canvas.height,
    	  r = Math.min(width, height)/2,
    	  dx = self.clamp(x - width/2, -r, r),
    	  dy = self.clamp(y - height/2, -r, r);
    	  return [Math.asin(dx/r), Math.asin(-dy/r)];
      };

      handlers.polardown = function(x,y) {
        var activeSub = self.getObj(activeSubscene),
            activeModel = self.getObj(self.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        handlers.dragBase = self.screenToPolar(x, y);
        self.saveMat = [];
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
          activeSub.camBase = [-Math.atan2(activeSub.saveMat.m13, activeSub.saveMat.m11),
                               Math.atan2(activeSub.saveMat.m32, activeSub.saveMat.m22)];
        }
        self.canvas.style.cursor = "grabbing";
      };

      handlers.polarmove = function(x,y) {
        var dragCurrent = self.screenToPolar(x,y),
            activeSub = self.getObj(activeSubscene),
            activeModel = self.getObj(self.useid(activeSub.id, "model")),
            objects = self.scene.objects,
            l = activeModel.par3d.listeners,
            i, j, changepos = [];
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          for (j=0; j<2; j++)
            changepos[j] = -(dragCurrent[j] - handlers.dragBase[j]);
          activeSub.par3d.userMatrix.makeIdentity();
          activeSub.par3d.userMatrix.rotate(changepos[0]*180/Math.PI, 0,-1,0);
          activeSub.par3d.userMatrix.multRight(objects[l[i]].saveMat);
          activeSub.par3d.userMatrix.rotate(changepos[1]*180/Math.PI, -1,0,0);
        }
        self.drawScene();
      };
      handlers.polarend = 0;

      handlers.axisdown = function(x) {
        handlers.rotBase = self.screenToVector(x, self.canvas.height/2);
        var activeSub = self.getObj(activeSubscene),
            activeModel = self.getObj(self.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
        }
        self.canvas.style.cursor = "grabbing";
      };

      handlers.axismove = function(x) {
        var rotCurrent = self.screenToVector(x, self.canvas.height/2),
            rotBase = handlers.rotBase,
            angle = (rotCurrent[0] - rotBase[0])*180/Math.PI,
            rotMat = new CanvasMatrix4();
        rotMat.rotate(angle, handlers.axis[0], handlers.axis[1], handlers.axis[2]);
        var activeSub = self.getObj(activeSubscene),
            activeModel = self.getObj(self.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.par3d.userMatrix.load(activeSub.saveMat);
          activeSub.par3d.userMatrix.multLeft(rotMat);
        }
        self.drawScene();
      };
      handlers.axisend = 0;

      handlers.y0zoom = 0;
      handlers.zoomdown = function(x, y) {
        var activeSub = self.getObj(activeSubscene),
          activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
          i, l = activeProjection.par3d.listeners;
        handlers.y0zoom = y;
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.zoom0 = Math.log(activeSub.par3d.zoom);
        }
        self.canvas.style.cursor = "zoom-in";
      };
      handlers.zoommove = function(x, y) {
        var activeSub = self.getObj(activeSubscene),
            activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
            i, l = activeProjection.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.par3d.zoom = Math.exp(activeSub.zoom0 + (y-handlers.y0zoom)/self.canvas.height);
        }
        self.drawScene();
      };
      handlers.zoomend = 0;

      handlers.y0fov = 0;
      handlers.fovdown = function(x, y) {
        handlers.y0fov = y;
        var activeSub = self.getObj(activeSubscene),
          activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
          i, l = activeProjection.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.fov0 = activeSub.par3d.FOV;
        }
        self.canvas.style.cursor = "zoom-in";
      };
      handlers.fovmove = function(x, y) {
        var activeSub = self.getObj(activeSubscene),
            activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
            i, l = activeProjection.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.par3d.FOV = Math.max(1, Math.min(179, activeSub.fov0 +
             180*(y-handlers.y0fov)/self.canvas.height));
        }
        self.drawScene();
      };
      handlers.fovend = 0;
      
      handlers.selectingdown = function(x, y) {
      	var viewport = self.getObj(activeSubscene).par3d.viewport,
      	  width = viewport.width*self.canvas.width,
      	  height = viewport.height*self.canvas.height, 
          p = {x: 2.0*x/width - 1.0, y: 2.0*y/height - 1.0};
      	self.select.region = {p1: p, p2: p};
      	if (self.select.subscene && self.select.subscene !== activeSubscene)
      	  self.delFromSubscene(self.scene.brushId, self.select.subscene);
      	self.select.subscene = activeSubscene;
      	self.addToSubscene(self.scene.brushId, activeSubscene);
      	self.select.state = "changing";
      	if (typeof self.scene.brushId !== "undefined")
      	  self.getObj(self.scene.brushId).initialized = false;
      	if (typeof self.scene.selectionInput !== "undefined")
      	  self.recordSelection(activeSubscene); 
      	self.drawScene();
      	self.canvas.style.cursor = "crosshair";
      };
      
      handlers.selectingmove = function(x, y) {
      	var viewport = self.getObj(activeSubscene).par3d.viewport,
      	  width = viewport.width*self.canvas.width,
      	  height = viewport.height*self.canvas.height;
      	if (self.select.state === "inactive") 
      	  return;
      	self.select.region.p2 = {x: 2.0*x/width - 1.0, y: 2.0*y/height - 1.0};
      	if (typeof self.scene.brushId !== "undefined")
      	  self.getObj(self.scene.brushId).initialized = false;
      	if (typeof self.scene.selectionInput !== "undefined")
      	  self.recordSelection(activeSubscene);
      	self.drawScene();
      };
      
      handlers.selectingend = 0;
      /* jshint evil:true */
      handlers.userdown = function(x, y) {
        var sub = self.getObj(activeSubscene),
            code = sub.callbacks[drag].begin;
        if (code) {
          var fn = Function('"use strict";return (' + code + ')')();
          fn.call(self, x, y);
        }
      };
      
      handlers.usermove = function(x, y) {
        var sub = self.getObj(activeSubscene),
            code = sub.callbacks[drag].update;
        if (code) {
          var fn = Function('"use strict";return (' + code + ')')();
          fn.call(self, x, y);
        }
      };
      
      handlers.userend = function() {
        var sub = self.getObj(activeSubscene),
            code = sub.callbacks[drag].end;
        if (code) {
          var fn = Function('"use strict";return (' + code + ')')();
          fn.call(self);
        }
      };

      self.canvas.onpointerdown = function ( ev ){
        // pointers and mice differ in capture rules; 
        // act like a mouse.
        if (ev.target.hasPointerCapture(ev.pointerId))
          ev.target.releasePointerCapture(ev.pointerId);
          
        if (!ev.which) // Use w3c defns in preference to MS
        switch (ev.button) {
          case 0: ev.which = 1; break;
          case 1:
          case 4: ev.which = 2; break;
          case 2: ev.which = 3;
        }
        drag = ["none", "left", "middle", "right", "wheel"][ev.which];
        var coords = self.relMouseCoords(ev);
        coords.y = self.canvas.height-coords.y;
        activeSubscene = self.whichSubscene(coords);
        var sub = self.getObj(activeSubscene), f;
        handler = sub.par3d.mouseMode[drag];
        switch (handler) {
        case "xAxis":
          handler = "axis";
          handlers.axis = [1.0, 0.0, 0.0];
          break;
        case "yAxis":
          handler = "axis";
          handlers.axis = [0.0, 1.0, 0.0];
          break;
        case "zAxis":
          handler = "axis";
          handlers.axis = [0.0, 0.0, 1.0];
          break;
        }
        f = handlers[handler + "down"];
        if (f) {
          coords = self.translateCoords(activeSubscene, coords);
          f.call(self, coords.x, coords.y);
          ev.preventDefault();
        } else
          console.warn("Mouse handler '" + handler + "' is not implemented.");

      };

      self.canvas.onpointerup = function ( ev ){
        if ( !drag ) return;
        var f = handlers[handler + "end"];
        if (f) {
          f.call(self);
          ev.preventDefault();
        }
        drag = 0;
        handlers.onpointermove( ev );
      };

      self.canvas.onpointerout = self.canvas.onpointerup;

      handlers.onpointermove = function ( ev ) {
        var coords = self.relMouseCoords(ev), sub, f;
        coords.y = self.canvas.height - coords.y;
        if (ev.buttons === 0) {
          activeSubscene = self.whichSubscene(coords);
          drag = "none";
          sub = self.getObj(activeSubscene);
          handler = sub.par3d.mouseMode.none;
          if (handler !== "none") {
            if (sub.needsBegin) {
              f = handlers[handler + "down"];
              if (f) {
                coords = self.translateCoords(activeSubscene, coords);
                f.call(self, coords.x, coords.y);
              }
              sub.needsBegin = 0;
            }
            self.canvas.style.cursor = self.getCursor(sub.par3d.mouseMode.none);
          } else {
            self.canvas.style.cursor = self.getCursor(sub.par3d.mouseMode.left);  
            return;
          }
        }
        f = handlers[handler + "move"];
        if (f) {
          coords = self.translateCoords(activeSubscene, coords);
          f.call(self, coords.x, coords.y);
        }
      };
      

      self.canvas.onpointerenter = function() {
        self.canvas.addEventListener("pointermove",               handlers.onpointermove);
      };
      
      self.canvas.onpointerleave = function() {
        self.canvas.removeEventListener("pointermove",
          handlers.onpointermove);
      };

      handlers.setZoom = function(ds) {
        var i;
        if (typeof activeSubscene === "undefined")
          activeSubscene = self.scene.rootSubscene;
        var activeSub = self.getObj(activeSubscene),
            activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
            l = activeProjection.par3d.listeners;

        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.par3d.zoom *= ds;
        }
        self.drawScene();
      };
      
      handlers.pushwheel = function(ev) {
        ev.deltaY = -ev.deltaY;
        handlers.pullwheel(ev);
      };
      
      handlers.pullwheel = function(ev) {
        var del = 1.05;
        if (ev.shiftKey) del = 1.005;
        var ds = ev.deltaY < 0 ? del : (1 / del);
        handlers.setZoom(ds);
      };
      
      handlers.user2wheel = function(ev) {
        var sub = self.getObj(activeSubscene),
            code = sub.callbacks.wheel.rotate;
        if (code) {
          var fn = Function('"use strict";return (' + code + ')')();
          /* jshint evil:false */
          fn.call(self, ev.deltaY < 0 ? 1 : 2);
        }        
      };
        
      handlers.wheelHandler = function(ev) {
        var coords = self.relMouseCoords(ev);
        coords.y = self.canvas.height - coords.y;
        activeSubscene = self.whichSubscene(coords);
        var sub = self.getObj(activeSubscene), f,
            handler = sub.par3d.mouseMode.wheel,
            evlocal;
            
        ev.deltaY = ev.deltaY || ev.detail || ev.deltaX || ev.wheelDelta;
                      
        switch(handler) {
          case "none": break;
          case "push":
          case "pull":
          case "user2":
            f = handlers[handler + "wheel"];
            if (f) {
              evlocal = {};
              evlocal.deltaY = ev.deltaY;
              evlocal.shiftKey = ev.shiftKey;
              evlocal.preventDefault = function() { ev.preventDefault(); };
              f.call(self, evlocal);
            }
            break;
          default: 
            evlocal = {};
            evlocal.preventDefault = function() { ev.preventDefault(); };
            evlocal.which = 4;
            evlocal.clientX = self.canvas.width/2;
            evlocal.clientY = self.canvas.height/2;
            self.canvas.onpointerdown(evlocal);
            evlocal.clientX += ev.deltaX;
            evlocal.clientY += ev.deltaY;
            handlers.onpointermove(evlocal);
            self.canvas.onpointerup(evlocal);
        }
        ev.preventDefault();
      };
      
      handlers.get_finger_dist = function(ev) {
        var diffX = ev.touches[0].clientX - ev.touches[1].clientX,
            diffY = ev.touches[0].clientY - ev.touches[1].clientY;
        return Math.sqrt(diffX * diffX + diffY * diffY); 
      };
      
      handlers.touchstart = function(ev) {
        var touch = ev.touches[0],
          mouseEvent = new MouseEvent("pointerdown",
            {
              clientX: touch.clientX,
              clientY: touch.clientY
            });
        ev.preventDefault();
        if (ev.touches.length === 2) {
          var coords = self.relMouseCoords(touch);
          coords.y = self.canvas.height-coords.y;
          activeSubscene = self.whichSubscene(coords);
          handlers.finger_dist0 = handlers.get_finger_dist(ev);
          handlers.zoomdown(coords.x, coords.y);
        }
        self.dispatchEvent(mouseEvent);
      };
      
      handlers.touchend = function(ev) {
        var mouseEvent;
        ev.preventDefault();
        if (ev.touches.length === 1) {
          mouseEvent = new MouseEvent("pointerup", {});
          self.dispatchEvent(mouseEvent);
        }
      };
      
      handlers.touchmove = function(ev) {
        var touch = ev.touches[0],
          mouseEvent;
        ev.preventDefault();
        if (ev.touches.length > 1) {
          var coords = self.relMouseCoords(touch),
              new_dist = handlers.get_finger_dist(ev);
          coords.y = self.canvas.height*Math.log(handlers.finger_dist0/new_dist) + handlers.y0zoom;
          handlers.zoommove(coords.x, coords.y);
        } else {
          mouseEvent = new MouseEvent("pointermove",
          {
            clientX: touch.clientX,
            clientY: touch.clientY
          });
          self.dispatchEvent(mouseEvent);
        }
      };

      self.canvas.addEventListener("DOMMouseScroll", handlers.wheelHandler, false);
      self.canvas.addEventListener("mousewheel", handlers.wheelHandler, false);
      self.canvas.addEventListener("touchstart", handlers.touchstart, {passive: false});
      self.canvas.addEventListener("touchend", handlers.touchend, {passive: false});
      self.canvas.addEventListener("touchmove", handlers.touchmove, {passive: false});
	  };
