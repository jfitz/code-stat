#include <iostream>
#include <stdio.h>
#include <string>
/* The SpelGraph Library is designed as an introduction to computer graphics programming. 
 * It has basic primitives for creating computer graphics applications.
 */
#include <SpelGraph.h>

using namespace std;


const int UPPER_LEFT_X=50;
const int UPPER_LEFT_Y=100;
const int LOWER_RIGHT_X=200;
const int LOWER_RIGHT_Y=250;
const int SQUARE_SIZE=50;

// class declaration for the Board class. There is one instance of this class. The class is designed 
// to 
class Board {
private:
	char theBoard[8][8];		// The tic-tac-toe board is a 3x3 board
	
public:
	Board();					// Constructor for the board class
	
	
	bool setMove(unsigned int p, char m);	// sets a move at position with a particular mark
	bool checkWin();						// checks for a win
	void display();							// displays the current state of the board
	void drawX(int, int);					// this function displays a graphic X
	void drawO(int, int);					// this function displays a graphic O
	int  convertMouse(int, int);			// this function is to convert x,y values to a position on the board
	void checkMouse(int&, int&);			// checks to ensure the mouse is within the area of the board
	
	
};


// class declaration for the Player class. There will be two instances of this class. One for each
// player.
class Player {
private:
	char mark;				// the type of mark used by the player i.e. X or O
	string name;			// the name of the player
	
	
public:
	Player();			// constructor for the Player class
	
	// basic access to data members
	void setName(string n);	// set name of player
	string getName();		// get the name of the player
	
	void setMark(char m);	// set the player's mark
	char getMark();			// get the mark of the player
};





/* Name of Function: main
 * Description: 
 *			This function is the driver for the tic-tac-toe program. It is responsible for controlling and validating 
 *			player moves.
 * Parmeters:
 *       argc    : number of command line arguments
 *       argv    : list of command line arguments
 */

int main(int argc, char* argv[])
{
	// data dictionary
	
    // insert code here... (remove this line when you start
	initSpelGraph();
	clearscreen(BLACK);
	// data dictionary
	Player player1, player2;
	Board Checkerboard;
	bool winner = false;
	int moves = 0;
	unsigned int move;
	string promptData;
	char* promptDataCstr;
	int playX, playY;
	
	// this shows how to use the setMark and setName methods
	player1.setMark('p');
	player2.setMark('w');
	
	player1.setName("P1");
	player2.setName("P2");
	
	
	
	// main loop for controlling the game by handling the logic between the moves of the players
	// and checking the board for wins.
	while (!winner && moves <= 64) {
		
		// display the board to start the game
		Checkerboard.display();
		
		// player 1 makes the first move
		// These two lines convert string object (used for concatentation) to char* object to
		// pass to the writedraw function.
		promptData = "Player: " + player1.getName() + " make a move: ";
		promptDataCstr = promptData.c_str();
		
		// places graphic text on the screen
		writedraw(promptDataCstr, 0, 0, PURPLE);
		
		// gets mouse position & makes sure it is in bounds
		getmouse(playX, playY);
		Checkerboard.checkMouse(playX, playY);
		
		// converts (X,Y) position of the mouse into a move value 0-8
		move = Checkerboard.convertMouse(playX, playY);
		
		// make a move and check its validity
		while (!Checkerboard.setMove(move, player1.getMark())) {
			writedraw("ERROR: Space Occupied, try again :)/>", 0, 0, PINK);
			getmouse(playX, playY);
			Checkerboard.checkMouse(playX, playY);
			move = Checkerboard.convertMouse(playX, playY);
		}
		
		moves++; // increments the number of moves
		if (Checkerboard.checkWin()) { // if player 1 wins, display and end the game
			
			// These two lines convert string object (used for concatentation) to char* object to
			// pass to the writedraw function.
			promptData = "Player: " + player1.getName() + " wins";
			promptDataCstr = promptData.c_str();
			
			// places graphic text on the screen
			writedraw(promptDataCstr, 0, 0, PURPLE);
			
			// we have a winner
			winner = true;
			
		} else {  // if player 1 does not win then player 2 plays
			
			if (moves == 64) break; // game is over!
			
			
			Checkerboard.display();
			
			// These two lines convert string object (used for concatentation) to char* object to
			// pass to the writedraw function.
			promptData = "Player: " + player2.getName() + " make a move: ";
			promptDataCstr = promptData.c_str();
			
			// places graphic text on the screen
			writedraw(promptDataCstr, 0, 0, WHITE);
			
			// gets mouse position & makes sure it is in bounds
			getmouse(playX, playY);
			Checkerboard.checkMouse(playX, playY);
			
			// converts (X,Y) position of the mouse into a move value 0-8
			move = Checkerboard.convertMouse(playX, playY);
			
			
			// make a move and check its validity
			while (!Checkerboard.setMove(move, player2.getMark())) {
				writedraw("ERROR: Space Occupied, try again :)/>", 0, 0, PINK;
				getmouse(playX, playY);
				Checkerboard.checkMouse(playX, playY);
				move = Checkerboard.convertMouse(playX, playY);
			}			
			
			moves++; // increments the number of wins
			if (Checkerboard.checkWin()) { // if player 2 wins display and end the game
				
				// These two lines convert string object (used for concatentation) to char* object to
				// pass to the writedraw function.
				promptData = "Player: " + player2.getName() + " wins";
				promptDataCstr = promptData.c_str();
				
				// places graphic text on the screen
				writedraw(promptDataCstr, 0, 0, WHITE);
				
				// we have a winner
				winner = true;
			}
		}
		
		
	}
	
	Checkerboard.display();
	
	if (winner == false)
		cout << "It's a Draw" << endl;
	
	cout << "Game Over" << endl;
	
	
	finishSpelGraph();
	
	
    return 0;
}

END_OF_MAIN()  // do not remove this line



// Begin Function Definitions


Board::Board() {
int rows;
int col;
						  
for(rows = 8; rows > 0; rows--)
{
	for(col = 0; col < 8; col++)
		{
			if(rows<=3&&rows%2==0&&col%2==0)
			cout << "b";
			else if (rows<=3&&rows%2==1&&col%2==1)
			cout << "b";
			else if (rows>=6&&rows%2==0&&col%2==0)
			cout<< "r";
			else if (rows>=6&&rows%2==1&&col%2==1)
			cout<< "r";
			else
			cout << "_";
						  }
			cout << endl;
			}	
						  
	}
// sets a move at position with a particular mark
bool Board::setMove(unsigned int p, char m) {
	int row = p / 8;
	int col = p % 8;
	
	
	if (theBoard[row][col] == 'X' || theBoard[row][col] == 'O' || p < 0 || p > 8)
		return false;
	else {
		// draw mark on the board
		if (m == 'X')
			drawX(UPPER_LEFT_X+SQUARE_SIZE*(col), UPPER_LEFT_Y+SQUARE_SIZE*(row));
		else 
			drawO(UPPER_LEFT_X+SQUARE_SIZE*(col), UPPER_LEFT_Y+SQUARE_SIZE*(row));
		
		theBoard[row][col] = m;	
		
		return true;
	}
	
	
}	


// checks for a win
bool Board::checkWin() {
	if 
						  ((theBoard[0][0] == theBoard[1][0] && theBoard[0][0] == theBoard[2][0] && theBoard[0][0] == theBoard[1][0] && theBoard[0][0] == theBoard[1][0] && theBoard[0][0] == theBoard[1][0] && theBoard[0][0] == theBoard[1][0]) || // col 1
						   (theBoard[0][1] == theBoard[1][1] && theBoard[0][1] == theBoard[2][1]) || // col 2
						   (theBoard[0][2] == theBoard[1][2] && theBoard[0][2] == theBoard[2][2]) || // col 3
						   (theBoard[0][0] == theBoard[0][1] && theBoard[0][0] == theBoard[0][2]) || // row 1
						   (theBoard[1][0] == theBoard[1][1] && theBoard[1][0] == theBoard[1][2]) || // row 2
						   (theBoard[2][0] == theBoard[2][1] && theBoard[2][0] == theBoard[2][2]) || // row 3
						  
						________________________________________________________________________________________  
((theBoard[0][0] == theBoard[1][0] && theBoard[0][0] == theBoard[2][0] && theBoard [0][0] == theBoard[3][0] && theBoard[0][0] == theBoard[4][0] && theBoard[5][0] == theBoard[0][0]  ) || // col 1
		(theBoard[0][1] == theBoard[1][1] && theBoard[0][1] == theBoard[2][1] == theBoard [3][1] == theBoard[4][1] == theBoard[5][1] == theBoard[6][1] == theBoard[7][1] )) || // col 2
		(theBoard[0][2] == theBoard[1][2] && theBoard[0][2] == theBoard[2][2] == theBoard [3][2] == theBoard[4][2] == theBoard[5][2] == theBoard[6][2] == theBoard[7][2] )) || // col 3
		
		(theBoard[0][3] == theBoard[1][3] && theBoard[0][3] == theBoard[2][3] == theBoard [3][3] == theBoard[4][3] == theBoard[5][3] == theBoard[6][3] == theBoard[7][3] )) || // col 4
		(theBoard[0][4] == theBoard[1][4] && theBoard[0][4] == theBoard[2][4] == theBoard [3][4] == theBoard[4][4] == theBoard[5][4] == theBoard[6][4] == theBoard[7][4] )) || // col 5
		(theBoard[0][5] == theBoard[1][5] && theBoard[0][5] == theBoard[2][5] == theBoard [3][5] == theBoard[4][5] == theBoard[5][5] == theBoard[6][5] == theBoard[7][5] )) || // col 6
		(theBoard[0][6] == theBoard[1][6] && theBoard[0][6] == theBoard[2][6] == theBoard [3][6] == theBoard[4][6] == theBoard[5][6] == theBoard[6][6] == theBoard[7][6] )) || // col 7
		(theBoard[0][7] == theBoard[1][7] && theBoard[0][7] == theBoard[2][7] == theBoard [3][7] == theBoard[4][7] == theBoard[5][7] == theBoard[6][7] == theBoard[7][7] )) || // col 8
	
		(theBoard[0][0] == theBoard[0][1] && theBoard[0][0] == theBoard[0][2] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 1
		(theBoard[1][0] == theBoard[1][1] && theBoard[1][0] == theBoard[1][2] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 2
		(theBoard[2][0] == theBoard[2][1] && theBoard[2][0] == theBoard[2][2] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 3
		
		(theBoard[3][0] == theBoard[1][0] && theBoard[0][0] == theBoard[2][0] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 4
		(theBoard[4][1] == theBoard[1][1] && theBoard[0][1] == theBoard[2][1] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 5
		(theBoard[5][2] == theBoard[1][2] && theBoard[0][2] == theBoard[2][2] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 6
		(theBoard[6][0] == theBoard[1][0] && theBoard[0][0] == theBoard[2][0] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 7
		(theBoard[7][1] == theBoard[1][1] && theBoard[0][1] == theBoard[2][1] == theBoard [3][0] == theBoard[4][0] == theBoard[5][0] == theBoard[6][0] == theBoard[7][0] )) || // row 8
		__________
		
		(theBoard[0][0] == theBoard[1][1] && theBoard[0][0] == theBoard[2][2]) || // diag l2r
		(theBoard[0][2] == theBoard[1][1] && theBoard[0][2] == theBoard[2][0]) )  // diag r2l
		return true; // we have a winner	
	else
		return false;
	
	
}				


void Board::checkMouse(int& x, int& y) {
	while (x < UPPER_LEFT_X || x > LOWER_RIGHT_X || 
		   y < UPPER_LEFT_Y || y > LOWER_RIGHT_Y) {
		getmouse(x, y);
	}
}

// displays the current state of the board		
void Board::display() {
	
	// draw the lines
	moveto(100, 100);
	lineto(100, 250);
	moveto(150, 100);
	lineto(150, 250);
	moveto(50, 150);
	lineto(200, 150);
	moveto(50, 200);
	lineto(200, 200);
	/*	for (int i=0; i < 8; i++) {
	 for (int j=0; j < 8; j++) {
	 cout << theBoard[i][j] << "  ";
	 
	 }
	 cout << endl;
	 }
	 */
	
}	

void Board::drawp(int x, int y) {
	setcolor(PURPLE);
	moveto(x+5, y+5);
	lineto(x+40, y+40);
	moveto(x+5, y+40);
	lineto(x+40, y+5);
	setcolor(PINK);
	
}

void Board::drawO(int x, int y) {
	setcolor(WHITE);
	moveto(x+5, y+5);
	lineto(x+40, y+5);
	lineto(x+40, y+40);
	lineto(x+5, y+40);
	lineto(x+5, y+5);
	setcolor(PINK);
	
}

int Board::convertMouse(int x, int y) {
	int row = (y-UPPER_LEFT_Y)/SQUARE_SIZE;
	int col = (x-UPPER_LEFT_X)/SQUARE_SIZE;
	
	int pos = row*8 + col;
	
	return pos;
}

// constructor for the Player class
Player::Player():  mark('P'), name("Computer") {
	// nothing here
}

// set name of player to the value of the parameter n
void Player::setName(string n) {
	name = n;
}

// returns the name of the player
string Player::getName() {
	return name;
}


// set the player's mark
void Player::setMark(char m) {
	m = toupper(m);
	if (m == 'P' || m == 'W')
		mark = m;
	else 
		mark = 'E';
	
}

// get the mark of the player
char Player::getMark() {
	return mark;
}


// End Function Definitions

