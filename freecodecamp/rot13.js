// Run a rot13 on a string (uppercase only)

function rot13(str) {
		let out =	"";
		for (let i = 0; i < str.length; i++) {
				let	p =	str.charCodeAt(i);
				if (p >= 65 && p <= 90) {
						p += 13;
						if (p > 90) {
								p -= 26;
						}
				}
				out += String.fromCharCode(p);
		}
		return out;
}

console.log(rot13("SERR PBQR PNZC"));
