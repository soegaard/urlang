"use strict";
var model={name:"Racket",color:"purple",font:"Georgia",clicks:0,size:60,keep_pressing:false,false:43},ractive=new Ractive({el:"#container",template:"#template",data:model});
function inc_by(s,amount){return (ractive.set(s,((ractive.get(s))+amount)));};
function resize(n){(inc_by("size",n));(inc_by("clicks",1));return (ractive.set("keep_pressing",true));};
(ractive.on({grow:(function(e){return (resize(5));}),shrink:(function(e){return (resize(-5));})}));
