/* el is the div, holding the rgl object as el.rglinstance,
     which holds x as el.rglinstance.scene
   x is the JSON encoded rglwidget.
*/


HTMLWidgets.widget({

  name: 'rglWebGL',

  type: 'output',

  factory: function(el, width, height) {
    el.width = width;
    el.height = height;
    var rgl = new rglwidgetClass(),
        onchangeselection = function(e) {
          for (var i = 0; i < rgl.scene.crosstalk.sel_handle.length; i++)
            rgl.clearBrush(except = e.rglSubsceneId);
          rgl.selection(e, false);
        },
        onchangefilter = function(e) {
          rgl.selection(e, true);
        };
    
    return { 
      renderValue: function(x) {
        var i, pel, player, groups,
            inShiny = (typeof Shiny !== "undefined");
      
        x.crosstalk.group = groups = [].concat(x.crosstalk.group);
        x.crosstalk.id = [].concat(x.crosstalk.id);
        x.crosstalk.key = [].concat(x.crosstalk.key);
        x.crosstalk.sel_handle = new Array(groups.length);
        x.crosstalk.fil_handle = new Array(groups.length);
        x.crosstalk.selection = [];
        for (i = 0; i < groups.length; i++) {
          x.crosstalk.sel_handle[i] = new crosstalk.SelectionHandle(groups[i], {sharedId: x.crosstalk.id[i]});
          x.crosstalk.sel_handle[i].on("change", onchangeselection);
          x.crosstalk.fil_handle[i] = new crosstalk.FilterHandle(groups[i], {sharedId: x.crosstalk.id[i]});
          x.crosstalk.fil_handle[i].on("change", onchangefilter);
        }
        if (inShiny) {
          // Shiny calls this multiple times, so we need extra cleanup
          // between
          rgl.sphere = undefined;
        }
        rgl.initialize(el, x);
        rgl.initGL();
  
  /* We might have been called after (some of) the players were rendered.
     We need to make sure we respond to their initial values. */

        if (typeof x.players !== "undefined") {
          var players = [].concat(x.players);
          for (i = 0; i < players.length; i++) {
            pel = document.getElementById(players[i]);
            if (pel) {
              player = pel.rglPlayer;
              if (player && (!player.initialized || inShiny)) {
                rgl.Player(pel, player);
                player.initialized = true;
              }
            }
          }
        }
        rgl.drag = 0;
        rgl.drawScene();
      },

      resize: function(width, height) {
        el.width = width;
        el.height = height;
        el.rglinstance.resize(el);
        el.rglinstance.drawScene();
      }
    };
  }
});
