open System.Collections.Generic

let visited = new Dictionary<_,_>(HashIdentity.Structural)

let rec walk x y =
    let addDigits num =
        let rec sumInner n soFar = 
            match n with
            | x when x<10  -> soFar+x
            | x -> sumInner (n/10) (soFar + n % 10)
        sumInner num 0
    let rec innerWalk x y totalPoints =
        let mycell = (x,y)
        match visited.TryGetValue(mycell) with
        | true,_ -> totalPoints
        | _    -> 
            (* printfn "%d,%d" x y *)
            visited.[mycell] <- true
            let digitSum = (addDigits x) + (addDigits y)
            match digitSum with
            | n when n>25 -> 
                totalPoints
            | n -> List.fold
                    (fun total (dx,dy) -> total + walk (x+dx) (y+dy))
                    (totalPoints+1)
                    [(1,0);(-1,0);(0,1);(0,-1)]
    innerWalk x y 0

let _ =
    printfn "Points: %d\n" (walk 1000 1000)