//------------------------------------------------------------------------------------------------------------------------
r2d3.onRender(function(data, svg, width, height, options){

   // console.log("--- multiplot r2d3.onRender")
   r2d3.svg.selectAll("g").remove()

   var text = r2d3.svg.selectAll("text")
                      .data("placeholder")
                      .enter()
                      .append("text");

   var divID = data.divID;
   var namespace = divID.split("-")[0]   

   //console.log("divID: " + divID)
   var rnaData = data.rna;
   var srmData = data.srm;
   var xMax = data.xMax;
   var yMax = data.yMax;

   var lineDrawingScheme = d3.curveLinear;
   if(data.smoothing=="Yes")
       lineDrawingScheme = d3.curveMonotoneX
  
   var d3Div = document.getElementById(divID);
   var actual_width = d3Div.clientWidth;
   var actual_height = d3Div.clientHeight;
  
   var sideMargin = 170;
   var bottomMargin = 50;
   var topMargin = 20;

    var width = actual_width - (2 * sideMargin);
    var height = actual_height - (1 * (bottomMargin + topMargin)); //* 0.90;

   var vectorNames = Object.keys(data.vectors)

   var xScalingFunction = d3.scaleLinear()
       .domain([0, xMax * 1.01])  // the range of the values to plot
       .range([0, width]);             // the pixel range of the x-axis
    
   var yScalingFunction = d3.scaleLinear()
       .domain([0, yMax * 1.01])
       .range([height, 0]);
    
   var lineGenerator = d3.line()
       .x(function(d, i) {return xScalingFunction(d.x);})
       .y(function(d)    {return yScalingFunction(d.y);})
       .defined(function(d){return d.y !== null;})
       .curve(lineDrawingScheme)

   var xAxis = d3.axisBottom()
       .scale(xScalingFunction);
   
   var yAxis = d3.axisLeft()
        .scale(yScalingFunction);
    
    //------------------------------
    // axes
    //------------------------------
    
    xShift = sideMargin;
    yShift = height;
    translationString = `translate(${xShift}, ${yShift})`
    r2d3.svg.append('g')
        .attr("class", "axis")
        .attr('transform', translationString)
        .call(xAxis);
    
    xShift = sideMargin;
    yShift = 0
    translationString = `translate(${xShift}, ${yShift})`
    
    r2d3.svg.append('g')
        .attr("class", "axis")
        .attr('transform', translationString)
        .call(yAxis);
    
    //------------------------------
    // axis labels
    //------------------------------
    let fontWeight = 400;
    let xPos = 45;
    let yPos = height/2;
    translationString = `translate(${xPos}, ${yPos})`
  /*************************************
   * r2d3.svg.append("text")             
   *   .style("font-size", 14)
   *   .style("font-weight", fontWeight)
   *   .attr("transform", translationString)
   *   .style("text-anchor", "middle")
   *   .style("stroke", "blue")
   *   .text("PROTEIN");
   *
   * yPos = 20 + height/2;
   * translationString = `translate(${xPos}, ${yPos})`
   * r2d3.svg.append("text")             
   *   .style("font-size", 12)
   *   .style("font-weight", fontWeight)
   *   .attr("transform", translationString)
   *   .style("text-anchor", "middle")
   *   .style("stroke", "blue")
   *   .text("copy number");
   ******************************/
    xPos = (width/2) + sideMargin;
    yPos = height + (0.8 * bottomMargin);
    translationString = `translate(${xPos}, ${yPos})`
    r2d3.svg.append("text")             
      .style("font-size", 18)
      .attr("transform", translationString)
      .style("text-anchor", "middle")
      .text("Day");

      // Day0: MPP
      // Day2 to Day4: MEP
      // Day4 to Day11.5: CFU-E
      // Day11.5 : ProEB
      // Day14: BasoEB
    
    let developmentalStage_fontSize = 24;
    let developmentalStage_fontStyle = "normal";
    let developmentalStage_fontColor = "red";
    let developmentalStage_textAnchor = "middle"

   xPos = sideMargin;
    yPos = height + (bottomMargin) + 20;
    translationString = `translate(${xPos}, ${yPos})`
    r2d3.svg.append("text")             
      .style("font-size", developmentalStage_fontSize)
      .style("font-style", developmentalStage_fontStyle)
      .style("font-color", developmentalStage_fontColor)
      .style("text-anchor", developmentalStage_textAnchor)
      .attr("transform", translationString)
      .text("MPP");

    xPos = sideMargin + (3 * (width/14));
    yPos = height + (bottomMargin) + 20;
    translationString = `translate(${xPos}, ${yPos})`
    r2d3.svg.append("text")             
      .style("font-size", developmentalStage_fontSize)
      .style("font-style", developmentalStage_fontStyle)
      .style("font-color", developmentalStage_fontColor)
      .style("text-anchor", developmentalStage_textAnchor)
      .attr("transform", translationString)
      .text("MEP");

    xPos = sideMargin + (7.75 * (width/14));
    yPos = height + (bottomMargin) + 20;
    translationString = `translate(${xPos}, ${yPos})`
    r2d3.svg.append("text")             
      .style("font-size", developmentalStage_fontSize)
      .style("font-style", developmentalStage_fontStyle)
      .style("font-color", developmentalStage_fontColor)
      .style("text-anchor", developmentalStage_textAnchor)
      .attr("transform", translationString)
      .text("CFU-E");

    xPos = sideMargin + (11.5 * (width/14));
    yPos = height + (bottomMargin) + 20;
    translationString = `translate(${xPos}, ${yPos})`
    r2d3.svg.append("text")             
      .style("font-size", developmentalStage_fontSize)
      .style("font-style", developmentalStage_fontStyle)
      .style("font-color", developmentalStage_fontColor)
      .style("text-anchor", developmentalStage_textAnchor)
      .attr("transform", translationString)
      .text("ProEB");

    xPos = sideMargin + (14 * (width/14));
    yPos = height + (bottomMargin) + 20;
    translationString = `translate(${xPos}, ${yPos})`
    r2d3.svg.append("text")             
      .style("font-size", developmentalStage_fontSize)
      .style("font-style", developmentalStage_fontStyle)
      .style("font-color", developmentalStage_fontColor)
      .style("text-anchor", developmentalStage_textAnchor)
      .attr("transform", translationString)
      .text("BasoEB");


    //------------------------------
    // the plotting surface
    //------------------------------
    
    xShift = sideMargin;
    yShift = 0;
    translationString = `translate(${xShift}, ${yShift})`

    var plottingSurface = r2d3.svg.append('g')
        .attr('transform', translationString)
        .attr('width', width)
        .attr('height', height)
        .attr('class', 'plottingSurface')   
    
    var margin = {top: 50, right: 50, bottom: 50, left: 50}
    var width = window.innerWidth - margin.left - margin.right // Use the window's width 
    var height = window.innerHeight - margin.top - margin.bottom; // Use the window's height
       
    var colorNumber = 0;
    var colors = d3.schemeCategory10;
    var colorCount = colors.length;
    
    for(vectorName of vectorNames){
       colorNumber = colorNumber + 1
       // console.log("adding " + vectorName);
       var oneDataset = data.vectors[vectorName];

       plottingSurface.append("path")
         .datum(oneDataset)
         .attr("d", lineGenerator)
         .attr("fill", "none")
         .attr("stroke-width", 2)
         .attr("vName", vectorName)
         .attr("stroke", colors[(colorNumber % colorCount)])
         .on("mouseover", function(d, i){
             // console.log("mouse over: " + d3.select(this).attr("vName"));
             d3.select(this).attr('stroke-width', 10);
             var targetName = namespace + "-" + "currentlySelectedVector"
             //console.log("about to send message to " + targetName);
             Shiny.setInputValue(targetName, d3.select(this).attr("vName"));
             //Shiny.setInputValue("d3.foo-currentlySelectedVector", d3.select(this).attr("vName"));
             })
           .on("mouseout", function(d, i) {
              d3.select(this).attr('stroke-width', 2)
              Shiny.setInputValue("currentlySelectedVector", " ");
              })

       plottingSurface.selectAll("dot")
         .data(oneDataset)
         .enter().append("circle") // Uses the enter().append() method
           .filter(function(d) {return d.y != null;})
           .attr("class", "dot") // Assign a class for styling
           .attr("cx", function(d) {return xScalingFunction(d.x)})
           .attr("cy", function(d) {return yScalingFunction(d.y)})
           .attr("vName", vectorName)
           .attr("r", 5)
       } // for vectorName

}) // onRender
//------------------------------------------------------------------------------------------------------------------------
