/**
  * Retrieve the coordinates of the given event relative to the center
* of the widget.
*
  * @param event
*   A mouse-related DOM event.
* @param reference
*   A DOM element whose position we want to transform the mouse coordinates to.
* @return
*    A hash containing keys 'x' and 'y'.
*/
function getRelativeCoordinates(event, reference) {
    var x, y;
    event = event || window.event;
    var el = event.target || event.srcElement;
    console.log(el);
    if (!window.opera && typeof event.offsetX != 'undefined') {
      // Use offset coordinates and find common offsetParent
      var pos = { x: event.offsetX, y: event.offsetY };
      
      // Send the coordinates upwards through the offsetParent chain.
      var e = el;
      while (e) {
        e.mouseX = pos.x;
        e.mouseY = pos.y;
        pos.x += e.offsetLeft;
        pos.y += e.offsetTop;
        e = e.offsetParent;
      }
      
      // Look for the coordinates starting from the reference element.
      var ref = reference;
      var offset = { x: 0, y: 0 };
      while (ref) {
        if (typeof ref.mouseX != 'undefined') {
          x = ref.mouseX - offset.x;
          y = ref.mouseY - offset.y;
          break;
        }
        offset.x += ref.offsetLeft;
        offset.y += ref.offsetTop;
        ref = ref.offsetParent;
      }
      
      // Reset stored coordinates
      ref = el;
      while (ref) {
        ref.mouseX = undefined;
        reference.mouseY = undefined;
        ref = ref.offsetParent;
      }
    }
    else {
      // Use absolute coordinates
      var pos = getAbsolutePosition(reference);
      x = event.pageX  - pos.x;
      y = event.pageY - pos.y;
    }
    // Subtract distance to middle
    return { x: x, y: y };
  }
