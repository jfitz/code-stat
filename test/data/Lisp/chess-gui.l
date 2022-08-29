# 12jul22 Software Lab. Alexander Burger

# Extend chess pieces
(dm (img> . +White) (Col)
   (pack "chess/" Col "w" (name> This) ".png") )

(dm (img> . +Black) (Col)
   (pack "chess/" Col "b" (name> This) ".png") )

# Piece names
(local) (*Pieces *Setup)

(de *Pieces
   ("WhiteKing" . ,"White King")
   ("WhiteCastledKing" . ,"White King")
   ("WhiteQueen" . ,"White Queen")
   ("WhiteRook" . ,"White Rook")
   ("WhiteBishop" . ,"White Bishop")
   ("WhiteKnight" . ,"White Knight")
   ("WhitePawn" . ,"White Pawn")
   ("BlackKing" . ,"Black King")
   ("BlackCastledKing" . ,"Black King")
   ("BlackQueen" . ,"Black Queen")
   ("BlackRook" . ,"Black Rook")
   ("BlackBishop" . ,"Black Bishop")
   ("BlackKnight" . ,"Black Knight")
   ("BlackPawn" . ,"Black Pawn") )

(de *Setup
   ((+Black +King) (+Black +Queen) (+Black +Rook) (+Black +Bishop) (+Black +Knight) (+Black +Pawn))
   ((+White +King) (+White +Queen) (+White +Rook) (+White +Bishop) (+White +Knight) (+White +Pawn)) )

# Board
(local) (*FieldSize *BoardSize *BlackFld *WhiteFld *StockSize *Info)

(setq
   *FieldSize 48
   *BoardSize (* 8 *FieldSize)
   *BlackFld "#b58863"
   *WhiteFld "#f0d9b5" )

(local) (field posX posY setX setY draw drawBoard)

(de field (X Y)
   (when (gt0 (- 8 (/ Y *FieldSize)))
      (get *Board (inc (/ X *FieldSize)) @) ) )

(de posX (This)
   (* *FieldSize (dec (: x))) )

(de posY (This)
   (* *FieldSize (- 8 (: y))) )

(de setX (X)
   (* *FieldSize (dec X)) )

(de setY (Y)
   (+ *BoardSize (* *FieldSize (dec Y))) )

(de draw (Piece X Y Col)
   (csDrawImage (sesId (img> Piece Col)) X Y NIL *FieldSize *FieldSize) )

(de drawBoard (Flg)
   (for Lst *Board
      (for This Lst
         (if (: piece)
            (draw (: piece)
               (posX This)
               (posY This)
               (if (: color) "b" "w") )
            (csFillStyle (if (: color) *BlackFld *WhiteFld))
            (csFillRect (posX This) (posY This) *FieldSize *FieldSize) ) ) )
   (when Flg
      (for (Y . Lst) *Setup
         (for (X . P) Lst
            (draw 'P (setX X) (setY Y)) ) ) ) )

# Game
(local) (setInfo drawGame gameGui drawSetup setupGui chessGui)

(de setInfo (Flg Busy)
   (setq *Info (cons (if Busy "@1 ..." "@1") Flg)) )

(de drawGame (Id Dly F X Y X2 Y2)
   (co 'drawGame
      (use (Piece Fld1 Fld2)
         (loop
            (cond
               ((gt0 F)  # Click
                  (let (M (moves *You)  Q (= 2 F))
                     (setInfo (xchg '*You '*Me) M)
                     (yield (make (csPost)))
                     (and Q (setq *Depth (*/ *Depth 2)))
                     (when (and M (myMove))
                        (setInfo *You)
                        (yield (make (drawBoard) (csPost))) ) ) )
               ((=0 F)  # Drag start
                  (setq Fld1 (field X Y)  Piece (; Fld1 piece))
                  (yield (make (csDef "board"))) )
               ((and (lt0 F) Piece)  # Drag move
                  (yield
                     (make
                        (csDraw "board" 0 0)
                        (when (setq Fld2 (field X2 Y2))
                           (draw Piece (- X2 `(/ *FieldSize 2)) (- Y2 `(/ *FieldSize 2)))
                           (csStrokeStyle "blue")
                           (csStrokeLine
                              (+ (posX Fld1) `(/ *FieldSize 2))
                              (+ (posY Fld1) `(/ *FieldSize 2))
                              X2 Y2 ) ) ) ) )
               (T  # Draw board
                  (when Piece
                     (off Piece)
                     (when (yourMove Fld1 Fld2)
                        (let M (moves *Me)
                           (setInfo *Me M)
                           (yield (make (drawBoard) (csDelay 0) (csPost)))
                           (when (and M (myMove))
                              (setInfo *You)
                              (yield (make (csDelay 0) (csPost))) ) ) ) )
                  (yield (make (drawBoard))) ) ) ) ) ) )

(de gameGui ()
   (setq drawCanvas drawGame)
   (form NIL
      (<div> (cons 'style (pack "width: " *BoardSize "px"))
         (<drawCanvas> "$BoardCanvas" *BoardSize *BoardSize -1 T)
         (----)
         (<spread>
            (gui '(+Style +View +TextField) "larger white"
               '(when *Undo
                  (let ((Score M . L) (car @)  (P (P1 . @) (P2 . F2)) M)
                     (pack
                        (cadr (assoc P *Pieces))
                        " "
                        (if (and (not (isa '+Castled P)) (isa '+Castled P2))
                           (if (= 7 (; F2 x)) "0-0" "0-0-0")
                           (pack
                              (cdr (asoq P L))
                              (if (== P P1) "-" "x")
                              (pick cdr (cdr M)) ) )
                        (let C (not (; P color))
                           (if2 (inCheck C) (moves C)
                              "+" "++" NIL "==" ) )
                        " (" (- Score) ")" ) ) ) )
            (gui '(+Var +NumField) '*Depth
               (mapcar format (range 1 9))
               ,"Search Depth" ) )
         (--)
         (<spread>
            (gui '(+Style +View +TextField) "larger white"
               '(text (car *Info) (if (cdr *Info) ,"Black" ,"White")) )
            (gui '(+Able +JS +Button) '*Undo ,"Undo"
               '(prog (undo) (setInfo (xchg '*Me '*You))) )
            (gui '(+Able +JS +Button) '*Redo ,"Redo"
               '(prog (redo) (setInfo (xchg '*Me '*You))) ) )
         (--)
         (<spread>
            (gui '(+Button) ,"Setup"
               '(post (setq chessGui setupGui)) )
            (gui '(+JS +Button) ,"Save"
               '(out "chess.pos" (println *Depth *You (pieces))) )
            (gui '(+Able +JS +Button) '(info "chess.pos") ,"Load"
               '(in "chess.pos"
                  (setup (read) (read) (read))
                  (setInfo *You) ) )
            (gui '(+JS +Button) ,"New"
               '(prog (setup *Depth) (setInfo)) ) ) ) ) )

# Setup
(de drawSetup (Id Dly F X Y X2 Y2)
   (co 'drawSetup
      (use (Piece X0 Y0 Fld)
         (loop
            (cond
               ((and (gt0 F) (field X Y) (; @ piece))  # Clicked piece
                  (move> @ NIL)  # Remove piece
                  (yield (make (drawBoard T))) )
               ((=0 F)  # Drag start
                  (cond
                     ((field X Y)
                        (setq
                           Piece (; @ piece)
                           X0 (+ (posX @) `(/ *FieldSize 2))
                           Y0 (+ (posY @) `(/ *FieldSize 2)) ) )
                     ((and
                           (>= 6 (setq X (inc (/ X *FieldSize))) 1)
                           (>= 2 (setq Y (inc (/ (- Y *BoardSize) *FieldSize))) 1) )
                        (setq
                           Piece (box (get *Setup Y X))
                           X0 (+ (setX X) `(/ *FieldSize 2))
                           Y0 (+ (setY Y) `(/ *FieldSize 2)) ) ) )
                  (yield (make (csDef "board"))) )
               ((and (lt0 F) Piece)  # Drag move
                  (yield
                     (make
                        (csDraw "board" 0 0)
                        (when (setq Fld (field X2 Y2))
                           (draw Piece (- X2 `(/ *FieldSize 2)) (- Y2 `(/ *FieldSize 2)))
                           (csStrokeStyle "blue")
                           (csStrokeLine X0 Y0 X2 Y2) ) ) ) )
               (T  # Draw board
                  (yield
                     (make
                        (when Piece
                           (when Fld
                              (if (box? Piece)
                                 (piece (val Piece) 1 Fld)
                                 (and (; Fld piece) (move> @ NIL))
                                 (move> Piece Fld)
                                 (put Piece 'cnt 1) ) )
                           (off Piece)
                           (csDraw "board" 0 0) )
                        (drawBoard T) ) ) ) ) ) ) ) )

(de setupGui ()
   (setq drawCanvas drawSetup)
   (form NIL
      (<div> (cons 'style (pack "width: " *BoardSize "px"))
         (<drawCanvas> "$SetupCanvas" *BoardSize `(+ *BoardSize (* 2 *FieldSize)) -1)
         (----)
         (<spread>
            (gui '(+Button) ,"Game"
               '(post (setq chessGui gameGui)) )
            (gui '(+JS +Button) ,"Clear"
               '(setup *Depth *You T) )
            (gui '(+JS +Button) ,"New"
               '(setup *Depth) ) ) ) ) )

# Start in game mode
(setq chessGui gameGui)
