window.onload = function() {
  var currentObject,
      mode;
  
  if (document.getElementById("move").checked == "checked") {
    mode = "moving";
  } else {
    mode = "resizing";
  }
  
  document.onmousedown = mousedownHandler;
  document.onmouseup = mouseupHandler;
  
  function mousedownHandler(event) {
    currentObject = event.target;
    
    // Start action, based on the current mode.
    if (mode == "moving") {
      startMove(event);
    } else if (mode == "resizing") {
      startResize(event);
    }
    
    // Set mouse movement handler.
    if (mode == "moving") {
      document.onmousemove = moving;
    } else if (mode == "resizing") {
      document.onmousemove = resizing;
    }
    
    return false;
  }
  
  function mouseupHandler(event) {
    // End action, based on the current mode.
    if (mode == "moving") {
      endMove(event);
    } else if (mode == "resizing") {
      endResize(event);
    }
    currentObject = null;
    return false;
  }
  
  
  // Moving
  
  var startLeft,
      startTop,
      startX,
      startY;
  
  function startMove(event) {
    if (currentObject.style.left == "") {
      startLeft = 0;
    } else {
      startLeft = parseInt(currentObject.style.left);
    }
    if (currentObject.style.top == "") {
      startTop = 0;
    } else {
      startTop = parseInt(currentObject.style.top);
    }
    
    startX = event.clientX;
    startY = event.clientY;
    
    return false;
  }
  
  function moving(event) {
    currentObject.style.left = startLeft + event.clientX - startX + "px";
    currentObject.style.top =  startTop + event.clientY - startY + "px";
    
    return false;
  }
  
  function endMove(event) {
    document.onmousemove = null;
    
    return false;
  }
  
  
  // Resizing
  
  var startWidth,
      startHeight,
      startX,
      startY;
  
  function startResize(event) {
    if (currentObject.width == "") {
      startWidth = 0;
    } else {
      startWidth = parseInt(currentObject.width);
    }
    if (currentObject.height == "") {
      startHeight = 0;
    } else {
      startHeight = parseInt(currentObject.height);
    }
    
    startX = event.clientX;
    startY = event.clientY;
    
    document.onmousemove = endResize;
    
    return false;
  }
  
  function resizing(event) {
    currentObject.style.width = startWidth + event.clientX - startX + 'px';
    currentObject.style.height = startHeight + event.clientY - startY + 'px';
  }
  
  function endResize(event) {
    document.onmousemove = null;
    
    return false;
  }
  
  
  // Draw line.
  function linedraw(ax,ay,bx,by)
  {
    if (ay > by) {
      bx = ax + bx;
      ax = bx - ax;
      bx = bx - ax;
      by = ay + by;
      ay = by - ay;
      by = by - ay;
    }
    
    var calc = Math.atan((ay - by) / (bx - ax));
    calc = calc * 180 / Math.PI;
    var length = Math.sqrt((ax - bx) * (ax - bx) + (ay - by) * (ay - by));
    
    document.body.innerHTML +=
        "<div id='line' style='height:" + length +
        "px;width:1px;background-color:black;position:absolute;top:" + (ay) +
        "px;left:" + (ax) + "px;transform:rotate(" + calc +
        "deg);-ms-transform:rotate(" + calc +
        "deg);transform-origin:0% 0%;-moz-transform:rotate(" + calc +
        "deg);-moz-transform-origin:0% 0%;-webkit-transform:rotate(" + calc  +
        "deg);-webkit-transform-origin:0% 0%;-o-transform:rotate(" + calc +
        "deg);-o-transform-origin:0% 0%;'></div>"
  }
  
  linedraw(200, 400, 500, 900);
}

