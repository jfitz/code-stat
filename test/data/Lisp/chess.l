# 13jul22 Software Lab. Alexander Burger

,"Chess"  "@lib/canvas.l"

(once (load "chess/lib.l"))

(symbols '(chess simul android pico))

(once
   (load "chess/gui.l")
   (setup) )

(start
   (seed (stamp))
   (setInfo) )

(stop
   (co 'drawGame)
   (co 'drawSetup) )

(de work ()
   (overview T)
   (pilbox
      (<h4> "fh white" "PilBox")
      (<h3> "fh white" ,"Chess") )
   (menu ,"Chess"
      (chessGui) ) )

(work)
