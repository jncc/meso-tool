
var avsdev = {
  pivot: {
    dataFile: null,
    rowKey: 25,
    colKey: 400,
    active: false
  },
  geoView: {
    active: false,
    firstRun: true
  },
  cc: {
"DZA":"DZA - Algeria","AGO":"AGO - Angola","BEN":"BEN - Benin","BWA":"BWA - Botswana","BFA":"BFA - Burkina Faso","BDI":"BDI - Burundi","CMR":"CMR - Cameroon","CPV":"CPV - Cape Verde","CAF":"CAF - Central African Republic","TCD":"TCD - Chad","COM":"COM - Comoros","COD":"COD - Democratic Republic of the Congo","DJI":"DJI - Djibouti","EGY":"EGY - Egypt","GNQ":"GNQ - Equatorial Guinea","ERI":"ERI - Eritrea","ETH":"ETH - Ethiopia","GAB":"GAB - Gabon","GMB":"GMB - Gambia","GHA":"GHA - Ghana","GIN":"GIN - Guinea","GNB":"GNB - Guinea-Bissau","CIV":"CIV - Côte d'Ivoire","KEN":"KEN - Kenya","LSO":"LSO - Lesotho","LBR":"LBR - Liberia","LBY":"LBY - Libya","MDG":"MDG - Madagascar","MWI":"MWI - Malawi","MLI":"MLI - Mali","MRT":"MRT - Mauritania","MUS":"MUS - Mauritius","MAR":"MAR - Morocco","MOZ":"MOZ - Mozambique","NAM":"NAM - Namibia","NER":"NER - Niger","NGA":"NGA - Nigeria","COG":"COG - Republic of the Congo","RWA":"RWA - Rwanda","SHN":"SHN - Saint Helena","STP":"STP - Sao Tome and Principe","SEN":"SEN - Senegal","SYC":"SYC - Seychelles","SLE":"SLE - Sierra Leone","SOM":"SOM - Somalia","ZAF":"ZAF - South Africa","SSD":"SSD - South Sudan","SDN":"SDN - Sudan","SWZ":"SWZ - Swaziland","TZA":"TZA - Tanzania","TGO":"TGO - Togo","TUN":"TUN - Tunisia","UGA":"UGA - Uganda","ESH":"ESH - Western Sahara","ZMB":"ZMB - Zambia","ZWE":"ZWE - Zimbabwe"
  }
};

function debounce(func, wait, immediate) {
	var timeout;
	return function() {
		var context = this, args = arguments;
		clearTimeout(timeout);
		timeout = setTimeout(function() {
			timeout = null;
			if (!immediate) func.apply(context, args);
		}, wait);
		if (immediate && !timeout) func.apply(context, args);
	};
}


$(document).on('shiny:inputchanged', function(event){

  if (event.name == 'selectView') {
    avsdev.pivot.active = false;
    avsdev.geoView.active = false;

    if (event.value == "Geo Image") {
      avsdev.geoView.active = true;
      if (avsdev.geoView.firstRun) {
        zoomGeoView();
        avsdev.geoView.firstRun = false;
      }
    }
    if (event.value == "Pivot Table") {
      avsdev.pivot.active = true;
      drawPivot();
    }
  }

  if (event.name == '.clientdata_output_geoView-mapImage_hidden') {
    avsdev.geoView.active = !event.value;
    if (avsdev.geoView.firstRun) {
      zoomGeoView();
      avsdev.geoView.firstRun = false;
    }
  }
});



//  GEO


_zoomGeoView = function(){
  if (avsdev.geoView.active === false) {
    return;
  }

  var img = $($("#geoView-mapImage").children("img")[0]);

  if (img.hasClass("zoomed")) {

    if (document.querySelector('#geoView-mapImage img')) {
      document.querySelector('#geoView-mapImage img').dispatchEvent(new CustomEvent('wheelzoom.destroy'));
      img.removeClass("zoomed");
    }
  }

  if (!img.hasClass("zoomed")) {
    wheelzoom(img);
    img.addClass("zoomed");
  }
};
zoomGeoView = debounce(function(){ _zoomGeoView() }, 500);

/*
$(document).on('shiny:sessioninitialized', function(event){
  console.log("shiny:sessioninitialized")
  zoomGeoView()
});
*/

$(document).on('shiny:value', function(event){
  if (event.name == 'geoView-mapImage') {
    zoomGeoView();
  }
});


// PIVOT

_drawPivot = function() {
  if (avsdev.pivot.active === false) {
    return;
  }

  $.getJSON(avsdev.pivot.dataFile, function(data) {
    data = $(data).each(function(id, v){
      v.Country = avsdev.cc[v.Country];
    });
    data = $.makeArray(data);

    var renderers = $.extend($.pivotUtilities.renderers, $.pivotUtilities.plotly_renderers);
    renderers = $.extend(renderers, $.pivotUtilities.avsdev_renderers);

    $("#pivot-pivotRender").pivotUI(data, {
      rows: ["Grid Dist."],
      cols: ["Pop Density"],
      aggregatorName: "Sum",
      vals: ["Population"],
      renderers: renderers,
      rendererName: "Coloured Pivot Table",
      hiddenFromDragDrop: ["Population"],
      hiddenFromAggregators: ["Grid Dist.", "Country", "Pop Density"],
      onRefresh: function(config) {
        var config_copy = JSON.parse(JSON.stringify(config));
        //delete some values which are functions
        delete config_copy.aggregators;
        delete config_copy.renderers;
        //delete some bulky default values
        delete config_copy.rendererOptions;
        delete config_copy.localeStrings;
        //console.log(JSON.stringify(config_copy, undefined, 2));

        var grid = $("td[data-rowkey]").filter(function() {
            return $(this).data("rowkey") <= avsdev.pivot.rowKey;
        });
        var solar = $("td[data-rowkey]").filter(function() {
            return $(this).data("rowkey") > avsdev.pivot.rowKey;
        }).filter(function() {
            return $(this).data("colkey") <= avsdev.pivot.colKey;
        });
        var target = $("td[data-rowkey]").filter(function() {
            return $(this).data("rowkey") > avsdev.pivot.rowKey;
        }).filter(function() {
            return $(this).data("colkey") > avsdev.pivot.colKey;
        });

        grid.addClass("power-grid");
        solar.addClass("power-solar");
        target.addClass("power-target");

        var gridVal = grid.map(function() {
          return $(this).data("value");
        }).get().reduce(function(a, b) { return a + b; }, 0);

        var solarVal = solar.map(function() {
          return $(this).data("value");
        }).get().reduce(function(a, b) { return a + b; }, 0);

        var miniVal = target.map(function() {
          return $(this).data("value");
        }).get().reduce(function(a, b) { return a + b; }, 0);

        var totalVal = gridVal + solarVal + miniVal;
        var offGrid = parseInt($($("#summary-summaryTable table td")[6]).text())

        $($("#summary-summaryTable table td")[1]).text(Math.round(totalVal - offGrid).toLocaleString('en'));
        $($("#summary-summaryTable table td")[2]).text(Math.round(offGrid - solarVal - miniVal).toLocaleString('en'));
        $($("#summary-summaryTable table td")[3]).text(Math.round(solarVal).toLocaleString('en'));
        $($("#summary-summaryTable table td")[4]).text(Math.round(miniVal).toLocaleString('en'));
        $($("#summary-summaryTable table td")[5]).text(Math.round(totalVal).toLocaleString('en'));
      }
    });
  });

  //console.log($("td").find(`[data-colKey > 500]`))
  //$("#loading-pivot img").toggleClass("collapse");
  //$("#pivot").toggleClass("collapse");
};
drawPivot = debounce(function(){ _drawPivot() }, 750);

Shiny.addCustomMessageHandler("render-pivot",
  function(message) {
    avsdev.pivot.dataFile = message.countryTotPath[0] + ".json";
    drawPivot();
  }
);


$(document).on('shiny:inputchanged', function(event){
  if (event.name == 'control-selectCountry') {
    drawPivot();
  } else if (event.name == 'control-selectGrid') {
    avsdev.pivot.rowKey = event.value;
    drawPivot();
  } else if (event.name == 'control-selectPopDensity') {
    avsdev.pivot.colKey = event.value;
    drawPivot();
  } else if (event.name == 'control-selectData') {
    drawPivot();
  }
});
