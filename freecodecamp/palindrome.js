// Determine whether or not a string is a palindrome

function reverseStr(str) {
		let rev = "";
		for (let i = str.length - 1; i >= 0; i--) {
				rev += str[i];
		}
		return rev;
}

function stripToAlphaNumeric(str) {
		let stripstr = str.replace(/[^a-zA-Z0-9]/g, '').toLowerCase();
		return stripstr;
}

function palindrome(str) {
		let	stripped = stripToAlphaNumeric(str);
		let flipped =	reverseStr(stripped);
		if (stripped == flipped) {
				return true;
		}
		return false;
}

let p = reverseStr(stripToAlphaNumeric("A man, a plan, a canal. Panama"));

console.log(p);
if (palindrome("eye")) {
		console.log("Success.");
}
