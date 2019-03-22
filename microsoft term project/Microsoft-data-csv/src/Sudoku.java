
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

import javax.swing.JFileChooser;

public class Sudoku {

	private static int boardSize = 0;
	private static int partitionSize = 0;
	private static int[][] vals = null;
	private static ArrayList<Pos> emptyPosList;

	public static void main(String[] args) {
		emptyPosList = new ArrayList<>();
		JFileChooser chooser = new JFileChooser();
		File inputFile = null;
		if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
			inputFile = chooser.getSelectedFile();
		} else {
			System.out.println("Error, file not selected");
		}
		Scanner input = null;

		int temp = 0;
		int count = 0;

		try {
			input = new Scanner(inputFile);
			temp = input.nextInt();
			boardSize = temp;
			partitionSize = (int) Math.sqrt(boardSize);
			System.out.println("Boardsize: " + temp + "x" + temp);
			vals = new int[boardSize][boardSize];

			System.out.println("Input:");
			int i = 0;
			int j = 0;
			while (input.hasNext()) {
				temp = input.nextInt();
				count++;
				System.out.printf("%3d", temp);
				vals[i][j] = temp;

				// if (temp == 0) {
				// emptyPosList.add(new Pos(i, j, boardSize));
				// }
				j++;
				if (j == boardSize) {
					j = 0;
					i++;
					System.out.println();
				}
				if (j == boardSize) {
					break;
				}
			}
			input.close();
		} catch (FileNotFoundException exception) {
			System.out.println("Input file not found");
		}
		if (count != boardSize * boardSize)
			throw new RuntimeException("Incorrect number of inputs.");

		int ind = 0;
		Pos emptyPos = null;
		for (int r = 0; r < boardSize; r++) {
			for (int c = 0; c < boardSize; c++) {
				System.out.println(r + " " + " " + c + " " + vals[r][c]);
				if (vals[r][c] == 0) {
					emptyPos = new Pos(r, c, boardSize);
					emptyPosList.add(emptyPos);
					System.out.println(emptyPos.printPos());
					System.out.println("index " + 0 + " : " + emptyPosList.get(0).printPos());
					ind++;
				}
			}
		}

		for (int i = 0; i < emptyPosList.size(); i++) {
			System.out.println("index " + i + " : " + emptyPosList.get(i).printPos());
		}

		boolean solved = solve();
		// Output
		try {
			PrintWriter writer = new PrintWriter(inputFile.getName().replaceFirst(".txt", "Solution.txt"));
			if (!solved) {
				writer.println("No solution found.");
				writer.close();
				return;
			}
			writer.print(boardSize);
			for (int i = 0; i < boardSize; i++) {
				for (int j = 0; j < boardSize; j++) {
					writer.printf("%3d", vals[i][j]);
				}
				writer.println();
				writer.close();
			}
			writer.close();
		} catch (FileNotFoundException exception) {
			System.out.println("error creating file");
		}
		if (!solved) {
			System.out.println("No solution found.");
			return;
		}
		System.out.println("\nOutput\n");
		for (int i = 0; i < boardSize; i++) {
			for (int j = 0; j < boardSize; j++) {
				System.out.printf("%3d", vals[i][j]);
			}
			System.out.println();
		}
	}

	public static boolean solve() {
		if (emptyPosList.size() == boardSize * boardSize) {
			System.out.println("Board is empty");
			return false;
		}

		int i = 0;
		while (i < emptyPosList.size() && i >= 0) {
			if (fillCell(i)) {
				i++;
			} else {
				i--;
			}
		}
		return (i != 0);
		// int row = 0, col = 0;
		//
		// while(row < boardSize && col < boardSize){
		// if(vals[row][col] == 0){
		// int num = fillCell(row, col, 0);
		// if(num != 0){
		// soln[row][col] = num;
		// if(!advancePos(row, col)){
		// return false;
		// }
		// }
		// else{
		// if(!backtrackPos(row, col)){
		// return false;
		// }
		//
		// }
		// }
		// }
		// for(int row = 0; row < boardSize; row++){
		// for(int col = 0; col < boardSize; col++){
		// if(vals[row][col] == 0){
		// soln[row][col] = fillCell(row, col, 0);
		// }
		// }
		// }
	}

	// public static boolean advancePos(int r, int c){
	// int row = r, col = c;
	//
	// col++;
	// if(col == boardSize && row < boardSize){
	// row++;
	// col = 0;
	// }
	//
	// return (row < boardSize || col < boardSize);
	//
	// }
	//
	// public static boolean backtrackPos(int r, int c){
	// int row = r, col = c;
	// while(vals[row][col] != 0 && row > -1 && col >= -1){
	// col--;
	// if(col == -1){
	// row--;
	// col = boardSize - 1;
	// }
	// }
	//
	// return(row > -1 || col > -1);
	//
	// }
	public static boolean fillCell(int ind) {
		int row = emptyPosList.get(ind).getRow();
		int col = emptyPosList.get(ind).getCol();
		int n = emptyPosList.get(ind).getVal();
		for (int num = n + 1; num < boardSize; num++) {
			if (!constraintError(row, col, num)) {
				vals[row][col] = num;
				return emptyPosList.get(ind).setVal(num);
			}
		}
		System.out.println(row + " " + col + " " + n + "error, need to backtrack");
		return false;
	}

	public static boolean constraintError(int row, int col, int num) {
		for (int i = 0; i < boardSize; i++) {
			if (vals[row][i] == num) {
				return true;
			}
		}
		for (int i = 0; i < boardSize; i++) {
			if (vals[i][col] == num) {
				return true;
			}
		}
		int boxOriginRow = partitionSize * Math.floorDiv(row, partitionSize);
		int boxOriginCol = partitionSize * Math.floorDiv(col, partitionSize);
		for (int i = 0; i < partitionSize; i++) {
			for (int j = 0; j < partitionSize; j++) {
				if (vals[boxOriginRow + i][boxOriginCol + j] == num) {
					return true;
				}
			}
		}
		return false;
	}
}
