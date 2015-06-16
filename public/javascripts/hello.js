if (window.console) {
  console.log("Welcome to your Play application's JavaScript!");
}

function plot(dimensions, clusters) {
  //Width and height
  var w = 700;
  var h = w;

  var linearScale = d3.scale.linear()
                            .domain([0, 1])
                            .range([0, w]);
  var dataset = [];

  for (var i in clusters) {
    points = clusters[i].points;
    for (var j in points) {
        data = points[j].map(function(p) { return linearScale(p) });
        data.push(clusters[i].number);
        dataset.push(data);
    }
  }

  //Create SVG element
  var svg = d3.select("body")
              .append("svg")
              .attr("width", w)
              .attr("height", h);

  // make a circle for each data point
  svg.selectAll("circle")
     .data(dataset)
     .enter()
     .append("circle")
     .attr("r", 5)
     .attr("cx", function(d) {
          return d[0];
     })
     .attr("cy", function(d) {
          return d[1];
     })
     .attr("fill", function(d) {
          return 'hsl(' + d[dimensions]*489 + ',100%,50%)'; // d[dimensions];
     });

}