# 06jun21 Software Lab. Alexander Burger

(load "@lib/simul.l")

(symbols 'chess 'simul 'pico)

### Fields/Board ###
(local) (*Board *Straight *Diagonal *DiaStraight *Moved *Pinned)

# x y color piece whAtt blAtt
(setq *Board (grid 8 8))

(for (X . Lst) *Board
   (for (Y . This) Lst
      (=: x X)
      (=: y Y)
      (=: color (not (bit? 1 (+ X Y)))) ) )

(de *Straight `west `east `south `north)

(de *Diagonal
   ((This) (: 0 1  1  0 -1  1))   # Southwest
   ((This) (: 0 1  1  0 -1 -1))   # Northwest
   ((This) (: 0 1 -1  0 -1  1))   # Southeast
   ((This) (: 0 1 -1  0 -1 -1)) ) # Northeast

(de *DiaStraight
   ((This) (: 0 1  1  0 -1  1  0 -1  1))   # South Southwest
   ((This) (: 0 1  1  0 -1  1  0  1  1))   # West Southwest
   ((This) (: 0 1  1  0 -1 -1  0  1  1))   # West Northwest
   ((This) (: 0 1  1  0 -1 -1  0 -1 -1))   # North Northwest
   ((This) (: 0 1 -1  0 -1 -1  0 -1 -1))   # North Northeast
   ((This) (: 0 1 -1  0 -1 -1  0  1 -1))   # East Northeast
   ((This) (: 0 1 -1  0 -1  1  0  1 -1))   # East Southeast
   ((This) (: 0 1 -1  0 -1  1  0 -1  1)) ) # South Southeast

### Pieces ###
(local) (*White *Black *WKPos *BKPos)
(local) (+White +Black +piece +King +Castled +Queen +Rook +Bishop +Knight +Pawn)

(local) (piece whAtt+ whAtt- blAtt+ blAtt- adjMove reAtttack)

(de piece (Typ Cnt Fld)
   (prog1
      (def
         (pack (mapcar '((Cls) (cdr (chop Cls))) Typ))
         Typ )
      (init> @ Cnt Fld) ) )

(de whAtt+ (This Pce)
   (=: whAtt (cons Pce (: whAtt))) )

(de whAtt- (This Pce)
   (=: whAtt (delq Pce (: whAtt))) )

(de blAtt+ (This Pce)
   (=: blAtt (cons Pce (: blAtt))) )

(de blAtt- (This Pce)
   (=: blAtt (delq Pce (: blAtt))) )

(de adjMove (Var KPos Att- Att+)
   (let (W (: field whAtt)  B (: field blAtt))
      (when (: field)
         (put @ 'piece NIL)
         (for F (: attacks) (Att- F This)) )
      (nond
         (Fld (set Var (delq This (val Var))))
         ((: field) (push Var This)) )
      (ifn (=: field Fld)
         (=: attacks)
         (put Fld 'piece This)
         (and (isa '+King This) (set KPos Fld))
         (for F (=: attacks (attacks> This)) (Att+ F This)) )
      (reAtttack W (: field whAtt) B (: field blAtt)) ) )

(de reAtttack (W W2 B B2)
   (for This W
      (unless (memq This W2)
         (for F (: attacks) (whAtt- F This))
         (for F (=: attacks (attacks> This)) (whAtt+ F This)) ) )
   (for This W2
      (for F (: attacks) (whAtt- F This))
      (for F (=: attacks (attacks> This)) (whAtt+ F This)) )
   (for This B
      (unless (memq This B2)
         (for F (: attacks) (blAtt- F This))
         (for F (=: attacks (attacks> This)) (blAtt+ F This)) ) )
   (for This B2
      (for F (: attacks) (blAtt- F This))
      (for F (=: attacks (attacks> This)) (blAtt+ F This)) ) )


(class +White)
# color ahead

(dm init> (Cnt Fld)
   (=: ahead north)
   (extra Cnt Fld) )

(dm move> (Fld)
   (adjMove '*White '*WKPos whAtt- whAtt+) )


(class +Black)
# color ahead

(dm init> (Cnt Fld)
   (=: color T)
   (=: ahead south)
   (extra Cnt Fld) )

(dm move> (Fld)
   (adjMove '*Black '*BKPos blAtt- blAtt+) )


(class +piece)
# cnt field attacks

(dm init> (Cnt Fld)
   (=: cnt Cnt)
   (move> This Fld) )

(dm ctl> ())


(local) (tryCastle try1Move try1Attack tryMoves tryAttacks)

(class +King +piece)

(dm name> () 'K)

(dm val> () 120)

(dm ctl> ()
   (unless (=0 (: cnt)) -10) )

(dm moves> ()
   (make
      (when (=0 (: cnt))
         (if (: color)
            (unless (; (: field) whAtt)
               (tryCastle 'd8 'c8 'a8 'whAtt 'b8)
               (tryCastle 'f8 'g8 'h8 'whAtt) )
            (unless (; (: field) blAtt)
               (tryCastle 'd1 'c1 'a1 'blAtt 'b1)
               (tryCastle 'f1 'g1 'h1 'blAtt) ) ) )
      (try1Move *Straight)
      (try1Move *Diagonal) ) )

(dm attacks> ()
   (make
      (try1Attack *Straight)
      (try1Attack *Diagonal) ) )


(class +Castled)

(dm ctl> () 30)


(class +Queen +piece)

(dm name> () 'Q)

(dm val> () 90)

(dm moves> ()
   (make
      (tryMoves *Straight)
      (tryMoves *Diagonal) ) )

(dm attacks> ()
   (make
      (tryAttacks *Straight)
      (tryAttacks *Diagonal T) ) )


(class +Rook +piece)

(dm name> () 'R)

(dm val> () 47)

(dm moves> ()
   (make (tryMoves *Straight)) )

(dm attacks> ()
   (make (tryAttacks *Straight)) )


(class +Bishop +piece)

(dm name> () 'B)

(dm val> () 33)

(dm ctl> ()
   (when (=0 (: cnt)) -10) )

(dm moves> ()
   (make (tryMoves *Diagonal)) )

(dm attacks> ()
   (make (tryAttacks *Diagonal T)) )


(class +Knight +piece)

(dm name> () 'N)

(dm val> () 28)

(dm ctl> ()
   (when (=0 (: cnt)) -10) )

(dm moves> ()
   (make (try1Move *DiaStraight)) )

(dm attacks> ()
   (make (try1Attack *DiaStraight)) )


(local) (tryPawnMove tryPawnCapt)

(class +Pawn +piece)

(dm name> () 'P)

(dm val> () 10)

(dm moves> ()
   (let (Fld1 ((: ahead) (: field))  Fld2 ((: ahead) Fld1))
      (make
         (and
            (tryPawnMove Fld1 Fld2)
            (=0 (: cnt))
            (tryPawnMove Fld2 T) )
         (tryPawnCapt (west Fld1) Fld2 (west (: field)))
         (tryPawnCapt (east Fld1) Fld2 (east (: field))) ) ) )

(dm attacks> ()
   (let Fld ((: ahead) (: field))
      (make
         (and (west Fld) (link @))
         (and (east Fld) (link @)) ) ) )


### Move Logic ###
(local) (inCheck pinned)

(de inCheck (Color)
   (if Color (; *BKPos whAtt) (; *WKPos blAtt)) )

(de try1Move (Lst)
   (for Dir Lst
      (let? Fld (Dir (: field))
         (ifn (; Fld piece)
            (link (list This (cons This Fld)))
            (unless (== (: color) (; @ color))
               (link
                  (list This
                     (cons (; Fld piece))
                     (cons This Fld) ) ) ) ) ) ) )

(de try1Attack (Lst)
   (for Dir Lst
      (and (Dir (: field)) (link @)) )  )

(de tryMoves (Lst)
   (for Dir Lst
      (let Fld (: field)
         (loop
            (NIL (setq Fld (Dir Fld)))
            (T (; Fld piece)
               (unless (== (: color) (; @ color))
                  (link
                     (list This
                        (cons (; Fld piece))
                        (cons This Fld) ) ) ) )
            (link (list This (cons This Fld))) ) ) ) )

(de tryAttacks (Lst Diag)
   (use (Pce Cls Fld2)
      (for Dir Lst
         (let Fld (: field)
            (loop
               (NIL (setq Fld (Dir Fld)))
               (link Fld)
               (T
                  (and
                     (setq Pce (; Fld piece))
                     (<> (: color) (; Pce color)) ) )
               (T (== '+Pawn (setq Cls (last (type Pce))))
                  (and
                     Diag
                     (setq Fld2 (Dir Fld))
                     (= (; Fld2 y) (; ((; Pce ahead) Fld) y))
                     (link Fld2) ) )
               (T (memq Cls '(+Knight +Queen +King)))
               (T (and Pce (xor Diag (== Cls '+Bishop)))) ) ) ) ) )

(de tryPawnMove (Fld Flg)
   (unless (; Fld piece)
      (if Flg
         (link (list This (cons This Fld)))
         (for Cls '(+Queen +Knight +Rook +Bishop)
            (link
               (list This
                  (cons This)
                  (cons
                     (piece (list (car (type This)) Cls) (: cnt))
                     Fld ) ) ) ) ) ) )

(de tryPawnCapt (Fld1 Flg Fld2)
   (if (; Fld1 piece)
      (unless (== (: color) (; @ color))
         (if Flg
            (link
               (list This
                  (cons (; Fld1 piece))
                  (cons This Fld1) ) )
            (for Cls '(+Queen +Knight +Rook +Bishop)
               (link
                  (list This
                     (cons (; Fld1 piece))
                     (cons This)
                     (cons
                        (piece (list (car (type This)) Cls) (: cnt))
                        Fld1 ) ) ) ) ) )
      (let? Pce (; Fld2 piece)
         (and
            (== Pce (car *Moved))
            (=1 (; Pce cnt))
            (memq (; Fld2 y) (4 5))
            (isa '+Pawn Pce)
            (link (list This (cons Pce) (cons This Fld1))) ) ) ) )

(de tryCastle (Fld1 Fld2 Fld3 Att Fld4)
   (or
      (; Fld1 piece)
      (get Fld1 Att)
      (; Fld2 piece)
      (get Fld2 Att)
      (; Fld4 piece)
      (let Pce (; Fld3 piece)
         (and
            (isa '+Rook Pce)
            (=0 (; Pce cnt))
            (link
               (list This
                  (cons This)
                  (cons
                     (piece (cons (car (type This)) '(+Castled +King)) 1)
                     Fld2 )
                  (cons Pce Fld1) ) ) ) ) ) )

(de pinned (Fld Lst Color)
   (use (Pce L P)
      (and
         (loop
            (NIL (setq Fld (Dir Fld)))
            (T (setq Pce (; Fld piece))
               (and
                  (= Color (; Pce color))
                  (setq L
                     (make
                        (loop
                           (NIL (setq Fld (Dir Fld)))
                           (link Fld)
                           (T (setq P (; Fld piece))) ) ) )
                  (<> Color (; P color))
                  (memq (last (type P)) Lst)
                  (cons Pce L) ) ) )
         (link @) ) ) )


### Moves ###
(local) (moves move)

# Move      ((p1 (p1 . f2))            . ((p1 . f1)))
# Capture   ((p1 (p2) (p1 . f2))       . ((p1 . f1) (p2 . f2)))
# Castle    ((K (K) (C . f2) (R . f4)) . ((R . f3) (K . f1)))
# Promote   ((P (P) (Q . f2))          . ((Q) (P . f1)))
# Capt/Prom ((P (p1) (P) (Q . f2))     . ((Q) (P . f1) (p1 . f2)))
(de moves (Color)
   (filter
      '((Lst)
         (prog2
            (move (car Lst))
            (not (inCheck Color))
            (move (cdr Lst)) ) )
      (mapcan
         '((Pce)
            (mapcar
               '((Lst)
                  (cons Lst
                     (flip
                        (mapcar
                           '((Mov) (cons (car Mov) (; Mov 1 field)))
                           (cdr Lst) ) ) ) )
               (moves> Pce) ) )
         (if Color *Black *White) ) ) )

(de move (Lst)
   (if (atom (car Lst))
      (inc (prop (push '*Moved (++ Lst)) 'cnt))
      (dec (prop (++ *Moved) 'cnt)) )
   (for Mov Lst
      (move> (car Mov) (cdr Mov)) ) )


### Evaluation ###
(local) (mate battle cost)

(de mate (Color)
   (and (inCheck Color) (not (moves Color))) )

(de battle (Fld Prey Attacker Defender)
   (use Pce
      (loop
         (NIL (setq Pce (mini 'val> Attacker)) 0)
         (setq Attacker (delq Pce Attacker))
         (NIL (and (asoq Pce *Pinned) (not (memq Fld @)))
            (max 0 (- Prey (battle Fld (val> Pce) Defender Attacker))) ) ) ) )

# Ref. Sargon, Dan and Kate Spracklen, Hayden 1978
(de cost (Color)
   (if (mate (not Color))
      -9999
      (setq *Pinned
         (make
            (for Dir *Straight
               (pinned *WKPos '(+Rook +Queen))
               (pinned *BKPos '(+Rook +Queen) T) )
            (for Dir *Diagonal
               (pinned *WKPos '(+Bishop +Queen))
               (pinned *BKPos '(+Bishop +Queen) T) ) ) )
      (let (Ctl 0  Mat 0  Lose 0  Win1 NIL  Win2 NIL  Flg NIL)
         (use (White Black Col Same B)
            (for Lst *Board
               (for This Lst
                  (setq White (: whAtt)  Black (: blAtt))
                  ((if Color inc dec) 'Ctl (- (length White) (length Black)))
                  (let? Val (and (: piece) (val> @))
                     (setq Col (: piece color)  Same (== Col Color))
                     ((if Same dec inc) 'Ctl (ctl> (: piece)))
                     (unless
                        (=0
                           (setq B
                              (if Col
                                 (battle This Val White Black)
                                 (battle This Val Black White) ) ) )
                        (dec 'Val 5)
                        (if Same
                           (setq
                              Lose (max Lose B)
                              Flg (or Flg (== (: piece) (car *Moved))) )
                           (when (> B Win1)
                              (xchg 'B 'Win1)
                              (setq Win2 (max Win2 B)) ) ) )
                     ((if Same dec inc) 'Mat Val) ) ) ) )
         (unless (=0 Lose) (dec 'Lose 5))
         (if Flg
            (* 4 (+ Mat Lose))
            (when Win2
               (dec 'Lose (>> 1 (- Win2 5))) )
            (+ Ctl (* 4 (+ Mat Lose))) ) ) ) )


### Game ###
(local) (*Depth *Undo *Redo *Me *You)
(local) (moved? bookMove myMove yourMove undo redo setup pieces)

(de moved? (Lst)
   (or
      (> 16 (length Lst))
      (find '((This) (n0 (: cnt))) Lst) ) )

(de bookMove (From To)
   (let Pce (; From piece)
      (list 0 (list (list Pce (cons Pce To)) (cons Pce From))) ) )

(de myMove ()
   (when
      (cond
         ((moved? (if *Me *Black *White))
            (game *Me *Depth moves move cost) )
         (*Me
            (if (member (; *Moved 1 field x) (1 2 3 5))
               (bookMove 'e7 'e5)
               (bookMove 'd7 'd5) ) )
         ((rand T) (bookMove 'e2 'e4))
         (T (bookMove 'd2 'd4)) )
      (let M (cons (car @) (cadr @))
         (move (cadr (push '*Undo M)))
         (off *Redo)
         (cons
            (- (car M))
            (caadr M)
            (cdr (asoq (caadr M) (cddr M)))
            (pick cdr (cdadr M)) ) ) ) )

(de yourMove (From To Cls)
   (when
      (find
         '((Mov)
            (and
               (== (caar Mov) (; From piece))
               (== To (pick cdr (cdar Mov)))
               (or
                  (not Cls)
                  (isa Cls (car (last (car Mov)))) ) ) )
         (moves *You) )
      (prog1 (cadr (push '*Undo (cons 0 @)))
         (off *Redo)
         (move @) ) ) )

(de undo ()
   (move (cddr (push '*Redo (++ *Undo)))) )

(de redo ()
   (move (cadr (push '*Undo (++ *Redo)))) )

(de setup (Depth You Init)
   (setq *Depth (or Depth 5)  *You You  *Me (not You))
   (off *White *Black *Moved *Undo *Redo)
   (for Lst *Board
      (for This Lst (=: piece) (=: whAtt) (=: blAtt)) )
   (if Init
      (for L Init
         (with (piece (cadr L) 0 (car L))
            (if (caddr L)
               (=: cnt 0)
               (=: cnt 2)
               (push '*Moved This) ) ) )
      (mapc
         '((Cls Lst)
            (piece (list '+White Cls) 0 (car Lst))
            (piece '(+White +Pawn) 0 (cadr Lst))
            (piece '(+Black +Pawn) 0 (get Lst 7))
            (piece (list '+Black Cls) 0 (get Lst 8)) )
         '(+Rook +Knight +Bishop +Queen +King +Bishop +Knight +Rook)
         *Board ) ) )

# List of all pieces
(de pieces ()
   (mapcar
      '((This)
         (list
            (: field)
            (val This)
            (not (memq This *Moved)) ) )
      (append *White *Black) ) )
