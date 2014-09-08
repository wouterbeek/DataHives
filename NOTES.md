# Notes to self on JavaScript


## 1

Mix jQuery keyword `this` with jQuery selectors
~~~{.js}
$(this).find("#some-id")
~~~


## 2

Selected options:
~~~{.js}
$("select option:selected")
~~~


## 3

The following appends an HTML element:
~~~{.js}
$("#container").append("<select></select>);
~~~
Sometimes we want to return the newly created element:
~~~
var select = $("<select></select>").appendTo("#container");
~~~


## 4

Add events to dynamically created `<select>` elements
that appear under root `#container`:
~~~{.js}
$("#container").on("change", "select", function() { ...});
~~~
