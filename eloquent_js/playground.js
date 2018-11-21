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

function deepEqual(a, b) {

}
