function range(start, end, step = 1) {
    let arr = [];
    if (step > 0) {
        for (let x = start; x <= end; x += step) {
            arr.push(x);
        }
    } else if (step < 0) {
        for (let x = start; x >= end; x += step) {
            arr.push(x);
        }
    } else {
        return [];
    }
    return arr;
}
let sum = (x) => x.length == 0 ? 1 : x.pop() + sum(x);
console.log(range(1, 10));
console.log(range(5, 2, -1));
console.log(sum(range(1, 10)));

function reverseArray(arr) {
    let newarr = [];
    for (let i = arr.length - 1; i >= 0; i--) {
				console.log(arr[i]);
				newarr.push(arr[i]);
    }
    return newarr;
}
function	reverseArrayInPlace(arr) {
		for (let i = 0, j = arr.length - 1; i < j; i++,j--) {
				let tmp = arr[j];
				arr[j] = arr[i];
				arr[i] = tmp;
		}
		return arr;
}
console.log(reverseArray(["A", "B", "C"]));
console.log(reverseArrayInPlace(["A", "B", "C"]));

// Just check equality by converting JSON to a string and comparing
// that.
function deepEqual(a, b) {
		if (JSON.stringify(a) == JSON.stringify(b)) {
				return true;
		}
		return false;
}

let arrays = [[1, 2, 3], [4, 5], [6]];

function flatten(arr) {
		return arr.reduce((a, b) => a.concat(b));
}

console.log(flatten(arrays));

function	loop(val, test, update, body) {
		let	i = val;
		while	(test(i)) {
				body(i);
				i = update(i);
		}
}

loop(3, n => n > 0, n => n - 1, console.log);

function every (array, test) {
		console.log(array);
		// Empty case
		if (array.length < 1) {
				return true;
		}
		// Not empty
		return array.reduce((a, b) => test(a) && test(b));
}

console.log(every([1, 3, 5], n => n < 10));
console.log(every([2, 4, 16], n => n < 10));
console.log(every([], n => n < 10));
