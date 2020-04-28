var dataFile = null;

addPort = function() {
  
}

drawDiagram = function() {
  var graph = new joint.dia.Graph();

  var paper = new joint.dia.Paper({
    el: document.getElementById('laydown'),
    model: graph,
    width: 1600,
    height: 1000,
    gridSize: 1
  });
  

  //Declare an array of rectangles - assign icon, name, 
  //for each asset in datafile.assets add a rectangle
  var row=0;
  var col=0;
  var space=" ";
  var port = {
    // id: 'abc', // generated if `id` value is not present
    group: 'a',
    args: {}, // extra arguments for the port layout function, see `layout.Port` section
    label: {
        position: {
            name: 'right',
            args: { y: 6 } // extra arguments for the label layout function, see `layout.PortLabel` section
        },
        markup: '<text class="label-text" fill="blue"/>'
    },
    attrs: { text: { text: 'port1' } },
    //markup: '<rect width="16" height="16" x="-8" strokegit ="red" fill="gray"/>'
  };

  var assetRects = [];
  
  dataFile.assets.id.forEach(function(n) {
    var rect = new joint.shapes.standard.Rectangle({
      position: {x:  (160*col+120), y: (240*row +120) },
      size: {width: 120, height: 80},
      ports: {
        groups: {
          'a': { position: 'top'},
          'b': { position: 'bottom'}
        },
        items: [port]
      }
    });
    
    name = dataFile.assets.stencil[n-1].concat(space, dataFile.assets.testRole[n-1]);
    //rect.position(240*row, 160*col);
	  //rect.resize(120, 80);
	  rect.attr({
	    body: {
		    fill: 'blue'
	    },
	    label: {
		    text: name,
		    fill: 'white'
	    }
	  });
	  assetRects.push(rect);
    rect.addTo(graph);
    col++;
    if (col==5) {
      col=0;
      row++;
    }
  });
  
  var linkArray = [];
  
  console.log(dataFile.logConns);
  thisObj="Links ";
  //Declare an array of links - assign names
  dataFile.logConns.id.forEach(function(lc) {
  //for (var id in dataFile.logConns) {
    fromAsset = dataFile.logConns.fromId[lc-1];
    toAsset= dataFile.logConns.toId[lc-1];
    //console.log(thisObj.concat(lc, " ; ", fromAsset, " : ",toAsset, " : ", fromAsset, " ; ", assetRects[toAsset]));
    var link = new joint.shapes.standard.Link();
    link.source(assetRects[fromAsset-1]);
	  link.target(assetRects[toAsset-1]);
	  link.router('manhattan');
    link.connector('jumpover');
	  linkArray.push(link);
	  link.addTo(graph);
  });
  
  paper.options.defaultRouter = {
    name: 'manhattan',
    args: {
        elementPadding: 10
    }
  };

  paper.on('cell:pointerclick', function(cellView) {
    cellView.highlight();
    console.log(cellView);
  });
	
};

Shiny.addCustomMessageHandler("render-laydown",
    function(message){
      try {
        console.log("Draw diagram");
        dataFile = message;
        drawDiagram();
      } catch(e) {
        console.log("error object:");
        console.log(e);
      }
    }
);


$(document).on('shiny:inputchanged', function(event){
   //TO BE INTEGRATED
});
