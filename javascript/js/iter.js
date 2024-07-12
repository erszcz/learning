function Iter(data) {
  return {
    index: 0,
    next() {
      this.value = data[this.index];
      this.done = data[this.index] == undefined;
      this.index++;
      return this
    },
    [Symbol.iterator]() {
      return this
    }
  }
}

let iter1 = Iter([4,5,6]);
for (const val of iter1) {
    console.log(`Iterator value: ${ val }`);
}

let iter = Iter([4,5,6]);
console.log(iter);
let v1 = iter.next();
console.log(v1);
let v2 = iter.next();
console.log(v2);
let v3 = iter.next();
console.log(v3);

function* Iter2(data) {
  var index = 0;
  var current;
  while ((current = data[index++]) != undefined) {
    yield current
  }
}

let iter2 = Iter2([4,5,6]);
for (let val of iter2) {
    console.log(`Iterator value: ${ val }`);
}

console.log(typeof iter2.next);
// "function" — it has a next method (which returns the right result), so it's an iterator

console.log(typeof iter2[Symbol.iterator]);
// "function" — it has an @@iterator method (which returns the right iterator), so it's an iterable

console.log(iter2[Symbol.iterator]() === iter2);

// vim: ts=2 sts=2 sw=2 et
