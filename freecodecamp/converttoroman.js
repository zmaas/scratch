// Convert a string to roman numerals

function convertToRoman(num) {
    let out = "";
    let decs = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
    let roms = ['M', 'CM', 'D', 'CD', 'C', 'XC',
								'L', 'XL', 'X', 'IX', 'V', 'IV', 'I'];
		for (let i = 0; i < decs.length; i++) {
				// If modulo doesn't do anything
				if (num % decs[i] != num) {
						out += roms[i];
						num -= decs[i];
						i--;
				}
		}
    return out;
}

console.log(convertToRoman(3134));
