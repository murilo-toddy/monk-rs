let boardSize = 50;
let board = []
for (let i = 0; i < boardSize; let i = i + 1) {
	if (i == boardSize - 3) {
		let board = push(board, 1);
	} else {
		let board = push(board, 0);
	}
}

for (let i = 0; i < boardSize - 2; let i = i + 1) {
	let string = "";
	for (let j = 0; j < len(board); let j = j + 1) {
		if (board[j] == 1) {
			let string = string + ".";
		} else {
			let string = string + " ";
		}
	}
	print(string);

	let pat = (board[0] << 1) | board[1];
	let newBoard = [0];
	for (let j = 1; j < boardSize - 1; let j = j + 1) {
		let pat = ((pat << 1) & 7) | board[j + 1];
		let value = ((110 >> pat) & 1);
		let newBoard = push(newBoard, value);
	}
	let newBoard = push(newBoard, 0);
	let board = newBoard;
}

