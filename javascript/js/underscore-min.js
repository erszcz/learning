(function(){var s=this;
var q=s._;
var b={};
var i=Array.prototype,A=Object.prototype;
var r=i.slice,v=i.unshift,u=A.toString,n=A.hasOwnProperty;
var l=i.forEach,h=i.map,y=i.reduce,e=i.reduceRight,k=i.filter,a=i.every,x=i.some,t=i.indexOf,f=i.lastIndexOf,c=Array.isArray,z=Object.keys;
var B=function(C){return new g(C)
};
if(typeof module!=="undefined"&&module.exports){module.exports=B;
B._=B
}else{s._=B
}B.VERSION="1.1.4";
var d=B.each=B.forEach=function(I,G,F){var H;
if(I==null){return
}if(l&&I.forEach===l){I.forEach(G,F)
}else{if(B.isNumber(I.length)){for(var E=0,C=I.length;
E<C;
E++){if(G.call(F,I[E],E,I)===b){return
}}}else{for(var D in I){if(n.call(I,D)){if(G.call(F,I[D],D,I)===b){return
}}}}}};
B.map=function(F,E,D){var C=[];
if(F==null){return C
}if(h&&F.map===h){return F.map(E,D)
}d(F,function(I,G,H){C[C.length]=E.call(D,I,G,H)
});
return C
};
B.reduce=B.foldl=B.inject=function(G,F,C,E){var D=C!==void 0;
if(G==null){G=[]
}if(y&&G.reduce===y){if(E){F=B.bind(F,E)
}return D?G.reduce(F,C):G.reduce(F)
}d(G,function(J,H,I){if(!D&&H===0){C=J;
D=true
}else{C=F.call(E,C,J,H,I)
}});
if(!D){throw new TypeError("Reduce of empty array with no initial value")
}return C
};
B.reduceRight=B.foldr=function(F,E,C,D){if(F==null){F=[]
}if(e&&F.reduceRight===e){if(D){E=B.bind(E,D)
}return C!==void 0?F.reduceRight(E,C):F.reduceRight(E)
}var G=(B.isArray(F)?F.slice():B.toArray(F)).reverse();
return B.reduce(G,E,C,D)
};
B.find=B.detect=function(F,E,D){var C;
o(F,function(I,G,H){if(E.call(D,I,G,H)){C=I;
return true
}});
return C
};
B.filter=B.select=function(F,E,D){var C=[];
if(F==null){return C
}if(k&&F.filter===k){return F.filter(E,D)
}d(F,function(I,G,H){if(E.call(D,I,G,H)){C[C.length]=I
}});
return C
};
B.reject=function(F,E,D){var C=[];
if(F==null){return C
}d(F,function(I,G,H){if(!E.call(D,I,G,H)){C[C.length]=I
}});
return C
};
B.every=B.all=function(F,E,D){E=E||B.identity;
var C=true;
if(F==null){return C
}if(a&&F.every===a){return F.every(E,D)
}d(F,function(I,G,H){if(!(C=C&&E.call(D,I,G,H))){return b
}});
return C
};
var o=B.some=B.any=function(F,E,D){E=E||B.identity;
var C=false;
if(F==null){return C
}if(x&&F.some===x){return F.some(E,D)
}d(F,function(I,G,H){if(C=E.call(D,I,G,H)){return b
}});
return C
};
B.include=B.contains=function(E,D){var C=false;
if(E==null){return C
}if(t&&E.indexOf===t){return E.indexOf(D)!=-1
}o(E,function(F){if(C=F===D){return true
}});
return C
};
B.invoke=function(D,E){var C=r.call(arguments,2);
return B.map(D,function(F){return(E?F[E]:F).apply(F,C)
})
};
B.pluck=function(D,C){return B.map(D,function(E){return E[C]
})
};
B.max=function(F,E,D){if(!E&&B.isArray(F)){return Math.max.apply(Math,F)
}var C={computed:-Infinity};
d(F,function(J,G,I){var H=E?E.call(D,J,G,I):J;
H>=C.computed&&(C={value:J,computed:H})
});
return C.value
};
B.min=function(F,E,D){if(!E&&B.isArray(F)){return Math.min.apply(Math,F)
}var C={computed:Infinity};
d(F,function(J,G,I){var H=E?E.call(D,J,G,I):J;
H<C.computed&&(C={value:J,computed:H})
});
return C.value
};
B.sortBy=function(E,D,C){return B.pluck(B.map(E,function(H,F,G){return{value:H,criteria:D.call(C,H,F,G)}
}).sort(function(I,H){var G=I.criteria,F=H.criteria;
return G<F?-1:G>F?1:0
}),"value")
};
B.sortedIndex=function(H,G,E){E=E||B.identity;
var C=0,F=H.length;
while(C<F){var D=(C+F)>>1;
E(H[D])<E(G)?C=D+1:F=D
}return C
};
B.toArray=function(C){if(!C){return[]
}if(C.toArray){return C.toArray()
}if(B.isArray(C)){return C
}if(B.isArguments(C)){return r.call(C)
}return B.values(C)
};
B.size=function(C){return B.toArray(C).length
};
B.first=B.head=function(E,D,C){return D&&!C?r.call(E,0,D):E[0]
};
B.rest=B.tail=function(E,C,D){return r.call(E,B.isUndefined(C)||D?1:C)
};
B.last=function(C){return C[C.length-1]
};
B.compact=function(C){return B.filter(C,function(D){return !!D
})
};
B.flatten=function(C){return B.reduce(C,function(D,E){if(B.isArray(E)){return D.concat(B.flatten(E))
}D[D.length]=E;
return D
},[])
};
B.without=function(D){var C=r.call(arguments,1);
return B.filter(D,function(E){return !B.include(C,E)
})
};
B.uniq=B.unique=function(C){return B.reduce(C,function(D,F,E){if(0==E||!B.include(D,F)){D[D.length]=F
}return D
},[])
};
B.intersect=function(D){var C=r.call(arguments,1);
return B.filter(B.uniq(D),function(E){return B.every(C,function(F){return B.indexOf(F,E)>=0
})
})
};
B.zip=function(){var C=r.call(arguments);
var F=B.max(B.pluck(C,"length"));
var E=new Array(F);
for(var D=0;
D<F;
D++){E[D]=B.pluck(C,""+D)
}return E
};
B.indexOf=function(G,E,F){if(G==null){return -1
}if(F){var D=B.sortedIndex(G,E);
return G[D]===E?D:-1
}if(t&&G.indexOf===t){return G.indexOf(E)
}for(var D=0,C=G.length;
D<C;
D++){if(G[D]===E){return D
}}return -1
};
B.lastIndexOf=function(E,D){if(E==null){return -1
}if(f&&E.lastIndexOf===f){return E.lastIndexOf(D)
}var C=E.length;
while(C--){if(E[C]===D){return C
}}return -1
};
B.range=function(J,G,H){var F=r.call(arguments),I=F.length<=1,J=I?0:F[0],G=I?F[0]:F[1],H=F[2]||1,D=Math.max(Math.ceil((G-J)/H),0),C=0,E=new Array(D);
while(C<D){E[C++]=J;
J+=H
}return E
};
B.bind=function(D,E){var C=r.call(arguments,2);
return function(){return D.apply(E||{},C.concat(r.call(arguments)))
}
};
B.bindAll=function(D){var C=r.call(arguments,1);
if(C.length==0){C=B.functions(D)
}d(C,function(E){D[E]=B.bind(D[E],D)
});
return D
};
B.memoize=function(E,D){var C={};
D=D||B.identity;
return function(){var F=D.apply(this,arguments);
return F in C?C[F]:(C[F]=E.apply(this,arguments))
}
};
B.delay=function(D,E){var C=r.call(arguments,2);
return setTimeout(function(){return D.apply(D,C)
},E)
};
B.defer=function(C){return B.delay.apply(B,[C,1].concat(r.call(arguments,1)))
};
var w=function(D,F,C){var E;
return function(){var H=this,G=arguments;
var I=function(){E=null;
D.apply(H,G)
};
if(C){clearTimeout(E)
}if(C||!E){E=setTimeout(I,F)
}}
};
B.throttle=function(C,D){return w(C,D,false)
};
B.debounce=function(C,D){return w(C,D,true)
};
B.wrap=function(C,D){return function(){var E=[C].concat(r.call(arguments));
return D.apply(this,E)
}
};
B.compose=function(){var C=r.call(arguments);
return function(){var D=r.call(arguments);
for(var E=C.length-1;
E>=0;
E--){D=[C[E].apply(this,D)]
}return D[0]
}
};
B.keys=z||function(E){if(B.isArray(E)){return B.range(0,E.length)
}var D=[];
for(var C in E){if(n.call(E,C)){D[D.length]=C
}}return D
};
B.values=function(C){return B.map(C,B.identity)
};
B.functions=B.methods=function(C){return B.filter(B.keys(C),function(D){return B.isFunction(C[D])
}).sort()
};
B.extend=function(C){d(r.call(arguments,1),function(D){for(var E in D){C[E]=D[E]
}});
return C
};
B.clone=function(C){return B.isArray(C)?C.slice():B.extend({},C)
};
B.tap=function(D,C){C(D);
return D
};
B.isEqual=function(D,C){if(D===C){return true
}var G=typeof(D),I=typeof(C);
if(G!=I){return false
}if(D==C){return true
}if((!D&&C)||(D&&!C)){return false
}if(D._chain){D=D._wrapped
}if(C._chain){C=C._wrapped
}if(D.isEqual){return D.isEqual(C)
}if(B.isDate(D)&&B.isDate(C)){return D.getTime()===C.getTime()
}if(B.isNaN(D)&&B.isNaN(C)){return false
}if(B.isRegExp(D)&&B.isRegExp(C)){return D.source===C.source&&D.global===C.global&&D.ignoreCase===C.ignoreCase&&D.multiline===C.multiline
}if(G!=="object"){return false
}if(D.length&&(D.length!==C.length)){return false
}var E=B.keys(D),H=B.keys(C);
if(E.length!=H.length){return false
}for(var F in D){if(!(F in C)||!B.isEqual(D[F],C[F])){return false
}}return true
};
B.isEmpty=function(D){if(B.isArray(D)||B.isString(D)){return D.length===0
}for(var C in D){if(n.call(D,C)){return false
}}return true
};
B.isElement=function(C){return !!(C&&C.nodeType==1)
};
B.isArray=c||function(C){return u.call(C)==="[object Array]"
};
B.isArguments=function(C){return !!(C&&n.call(C,"callee"))
};
B.isFunction=function(C){return !!(C&&C.constructor&&C.call&&C.apply)
};
B.isString=function(C){return !!(C===""||(C&&C.charCodeAt&&C.substr))
};
B.isNumber=function(C){return !!(C===0||(C&&C.toExponential&&C.toFixed))
};
B.isNaN=function(C){return C!==C
};
B.isBoolean=function(C){return C===true||C===false
};
B.isDate=function(C){return !!(C&&C.getTimezoneOffset&&C.setUTCFullYear)
};
B.isRegExp=function(C){return !!(C&&C.test&&C.exec&&(C.ignoreCase||C.ignoreCase===false))
};
B.isNull=function(C){return C===null
};
B.isUndefined=function(C){return C===void 0
};
B.noConflict=function(){s._=q;
return this
};
B.identity=function(C){return C
};
B.times=function(F,E,D){for(var C=0;
C<F;
C++){E.call(D,C)
}};
B.mixin=function(C){d(B.functions(C),function(D){p(D,B[D]=C[D])
})
};
var j=0;
B.uniqueId=function(C){var D=j++;
return C?C+D:D
};
B.templateSettings={evaluate:/<%([\s\S]+?)%>/g,interpolate:/<%=([\s\S]+?)%>/g};
B.template=function(F,E){var G=B.templateSettings;
var C="var __p=[],print=function(){__p.push.apply(__p,arguments);};with(obj||{}){__p.push('"+F.replace(/\\/g,"\\\\").replace(/'/g,"\\'").replace(G.interpolate,function(H,I){return"',"+I.replace(/\\'/g,"'")+",'"
}).replace(G.evaluate||null,function(H,I){return"');"+I.replace(/\\'/g,"'").replace(/[\r\n\t]/g," ")+"__p.push('"
}).replace(/\r/g,"\\r").replace(/\n/g,"\\n").replace(/\t/g,"\\t")+"');}return __p.join('');";
var D=new Function("obj",C);
return E?D(E):D
};
var g=function(C){this._wrapped=C
};
B.prototype=g.prototype;
var m=function(D,C){return C?B(D).chain():D
};
var p=function(C,D){g.prototype[C]=function(){var E=r.call(arguments);
v.call(E,this._wrapped);
return m(D.apply(B,E),this._chain)
}
};
B.mixin(B);
d(["pop","push","reverse","shift","sort","splice","unshift"],function(C){var D=i[C];
g.prototype[C]=function(){D.apply(this._wrapped,arguments);
return m(this._wrapped,this._chain)
}
});
d(["concat","join","slice"],function(C){var D=i[C];
g.prototype[C]=function(){return m(D.apply(this._wrapped,arguments),this._chain)
}
});
g.prototype.chain=function(){this._chain=true;
return this
};
g.prototype.value=function(){return this._wrapped
}
})();