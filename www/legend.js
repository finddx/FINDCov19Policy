// !preview r2d3 data=list(keys = c('A', 'B'), color = c('#b4a7d6', '#00ff00'))
//
// r2d3: https://rstudio.github.io/r2d3
//

// Handmade legend

// create a list of keys
var keys = data.keys;

// Color scale
var color = d3.scaleOrdinal() // D3 Version 4
  .domain(keys)
  .range(data.color);

// Add one dot in the legend for each name.
var size = 20;
svg.selectAll("mydots")
  .data(keys)
  .enter()
  .append("rect")
    .attr("x", function(d,i){ return 5 + i * 105})
    .attr("y", 5)
    .attr("width", size)
    .attr("height", size)
    .style("fill", function(d){ return color(d)});

// Add one dot in the legend for each name.
svg.selectAll("mylabels")
  .data(keys)
  .enter()
  .append("text")
    .attr("x", function(d,i){ return 5 + i * 105 + 25})
    .attr("y", 15)
    //.style("fill", function(d){ return color(d)})
    .text(function(d){ return d})
    .attr("text-anchor", "left")
    .style("alignment-baseline", "middle");
