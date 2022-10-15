let visited = Hashtbl.create 200000

let rec walk x y =
    let addDigits number =
        let rec sumInner n soFar = 
            match n with
            | x when x<10  -> soFar+x
            | x -> sumInner (n/10) (soFar + n mod 10) in
        sumInner number 0 in
    let rec innerWalk x y totalPoints =
        match Hashtbl.mem visited (x,y) with
        | true -> totalPoints
        | _    -> begin
            Printf.printf "%d,%d\n" x y ;
            Hashtbl.add visited (x,y) 1 ;
            let digitSum = (addDigits x) + (addDigits y) in
            match digitSum with
            | n when n>25 -> 
                totalPoints
            | n -> List.fold_left
                    (fun total (dx,dy) -> total + walk (x+dx) (y+dy))
                    (totalPoints+1)
                    [(1,0);(-1,0);(0,1);(0,-1)]
        end in
    innerWalk x y 0

let _ =
    Printf.printf "Points: %d\n" (walk 1000 1000)