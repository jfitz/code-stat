(*
Name   : LISTS.PAS
Date   : 14 October, 1990 (orig: 19 March, 1990)
Author : Kim Moser
System : Turbo Pascal 5.5
Descrip: Generic linked list utilities
*)

UNIT lists;

{$F+}

INTERFACE

USES Errors;

TYPE
       list = ^integer;

function listnew: list;
function listlen(p: list): word;
procedure listdel(p: list; spot: word);
function listwipe(var p: list): list;
function listinsert(p: list; data: pointer; width: word; spot: word): pointer;
function listabsorb(p: list; data: pointer; width: word; spot: word): pointer;
function listelmt(p: list; spot: word): pointer;
function elmtwidth(p: list; spot: word): word;
procedure listdispose(var p: list);

IMPLEMENTATION

CONST
     LISTSIGNATURE = 43704 (* 10*4096+10*256+12*16+12 *);  (* $AACC *)
     (* $10101010 $11001100 is a fairly unique signature to
        represent an initialized list *)

     ELMTSIGNATURE = #153;
     (* $10011001 is a fairly unique signature to
        represent an initialized element *)

TYPE
    elmtptr = ^elmt;

    elmt = record
          data: pointer;        (* Pointer to the data *)
          width: word;          (* Size of data *)
	  dispose: boolean;	(* Whether we allocated it dynamically
				   and must dispose of it when
				   deallocating list *)
          next: elmtptr;        (* Pointer to next element in list *)
          prev: elmtptr;        (* Pointer to previous element in list *)
          signature: char;      (* Whether this element has been
                                   properly initialized *)
    end;

    alist = record
          first: elmtptr;       (* Pointer to first element *)
	  last: elmtptr;        (* Pointer to last element *)
          recent: elmtptr;      (* Pointer to most recently accessed
                                   element *)
          mostrecent: word;     (* Index (offset from 0) of most recently
                                   accessed element *)
          len: word;            (* Number of elements in list *)
          signature: word;	(* SIGNATURE (hopefully) *)
    end;

    alistptr = ^alist;
    (* Pointers to 'list' are cast into alistptrs (pointers to 'alist') *)

{
procedure dump(d: pointer);
type
    strptr = ^string;
var
   i: integer;
begin
     write('DUMP: ');
     for i := 1 to length(strptr(d)^) do begin
         write(strptr(d)^[i]);
     end;
     writeln;
end;
}

function thelen(var p: list): word;
begin
	thelen := alistptr(p)^.len;
end;

function listinitialized(p: list): boolean;
begin
	listinitialized := (alistptr(p)^.signature = LISTSIGNATURE);
end;

function elmtinitialized(q: elmtptr): boolean;
begin
	elmtinitialized := (q^.signature = ELMTSIGNATURE);
end;

function eptr(VAR p: list; n: word): elmtptr;
(* Returns a pointer to the n'th elmt (offset from 0) in list 'p';
   assumes 'p' contains at least n+1 elements. *)
var
	i: word;
	q: elmtptr;
	tmp: word;
	mostrecent: word;
	distance: word;
	path: word;  (* Which "shortest path" we'll take to get to n'th elmt *)
	(* 0: start from beginning and seek forward
	   1: start from most recent and seek backward
	   2: start from most recent and seek forward
	   3: start from end and seek backward *)
begin
	(* Find "shortest path" to n'th element, i.e. do we start from
	   beginning and traverse forward, start from end and traverse
	   backward, or start from most recently accessed element and
	   traverse forward or backward? *)

	i := 0;
        mostrecent := alistptr(p)^.mostrecent;

        distance := n;
        path := 0;

	if (n <= mostrecent) then begin  (* Can we even consider path 1? *)
                tmp := mostrecent - n;
		if (tmp < distance) then begin
			path := 1;
			distance := tmp;
		end
	end else begin  (* Consider path 2 and 3 *)
                tmp := n - mostrecent;
		if (tmp < distance) then begin
			path := 2;
			distance := tmp;
		end;
                tmp := thelen(p)-1 - n;
		if (tmp < distance) then begin
			path := 3;
			distance := tmp;
		end
	end;

	(* Now we know the shortest path to the n'th elmt, so let's do it: *)

	case (path) of
		0:  (* Start at first elmt and seek forward *)
                    begin
			q := alistptr(p)^.first;  (* Start at first elmt *)
                        i := 0;
                        while ((i<n) AND (q <> NIL)) do begin
                              q := q^.next;
                              inc(i);
                        end;
                    end;
		1:  (* Start at most recent elmt and seek backward *)
                    begin
			q := alistptr(p)^.recent;  (* Start at most recent elmt *)
                        i := mostrecent;
                        while ((i>n) AND (q <> NIL)) do begin
                              q := q^.prev;
                              dec(i);
                        end;
                    end;
		2:  (* Start at most recent elmt and seek forward *)
                    begin
			q := alistptr(p)^.recent;  (* Start at most recent elmt *)
                        i := mostrecent;
			while ((i<n) AND (q <> NIL)) do begin
                              q := q^.next;
                              inc(i);
                        end;
                    end;
		3:  (* Start at end and seek backward *)
                    begin
			q := alistptr(p)^.last;  (* Start at last elmt *)
                        i := thelen(p) - 1;
                        while ((i>n) AND (q <> NIL)) do begin
                              q := q^.prev;
                              dec(i);
			end;
                    end;
(* !!!
		otherwise:
			writewarn("lists.eptr(): invalid list seek path (%d) for %d'th elmt.\n", path, n);
			return NIL;
*)
	end;  (* CASE *)

{ @@@ ***
        dump(q^.data);
        writeln('elmt width = ', q^.width);
        writeln('elmt signature = ', word(q^.signature));
}

	if (q = NIL) then begin
                writeln('lists.eptr(): NIL found before ', n, 'th elmt (path=', path, ').');
(* !!!
		writewarn("lists.eptr(): NIL found before %d'th elmt found (path=%d).\n", n, path);
*)
	end else if (NOT elmtinitialized(q)) then begin
                writeln('lists.eptr(): ', n, 'th elmt is corrupt: signature (',
                         word(q^.signature), ') <> ELMTSIGNATURE (', word(ELMTSIGNATURE), ').');
(* !!!
		writewarn("lists.eptr(): %d'th elmt is corrupt: signature (%u) <> ELMTSIGNATURE (%u).\n",
			  n, (unsigned int) (q^.signature), (unsigned int) ELMTSIGNATURE);
*)
                q := NIL;
	end;

        if (q <> NIL) then begin
        	(* Memorize most recently accessed elmt (i.e. the one we just got): *)
	        alistptr(p)^.recent := q;
                alistptr(p)^.mostrecent := n;
        end;

        eptr := q;
end;

function newelmt: elmtptr;
(* Returns a pointer to a newly allocated elmt, or NIL if unable
   to allocate memory for it. *)
var
	q: elmtptr;
begin
	new(q);
	if (q = NIL) then begin
                writeln('lists.newelmt(): new() returned NIL.');
(* !!!
		writewarn("lists.newelmt(): malloc(%u) returned NIL.\n", sizeof(struct elmt));
*)
	end else begin
		q^.signature := ELMTSIGNATURE;
	end;

	newelmt := q;
end;

function listnew: list;
var
	p: alistptr;
begin
        new(p);
	if (p = NIL) then begin
                writeln('lists.listnew(): new() returned NIL.');
(* !!!
		writewarn("lists.listnew(): malloc(%u) returned NIL.\n", sizeof(struct alist));
*)
	end else begin
		p^.first := NIL; p^.last := NIL; p^.recent := NIL;
		p^.mostrecent := 0;  (* Unnecessary *)
		p^.len := 0;
		p^.signature := LISTSIGNATURE;
	end;
	listnew := list(p);
end;

function listlen(p: list): word;
begin
	if (p = NIL) then begin
                writeln('lists.listlen(): list is NIL.');
(* !!!
		writewarn("lists.listlen(): list is NIL.\n");
*)
		listlen := 0;
	end else if (NOT listinitialized(p)) then begin
                writeln('lists.listlen(): list is NIL.');
(* !!!
		writewarn("lists.listlen(): list is NIL.\n");
*)
		listlen := 0;
	end else begin
                listlen := thelen(p);
        end;
end;

procedure listdel(p: list; spot: word);
var
	q: elmtptr;
begin
	if (p = NIL) then begin
                writeln('lists.listdel(): attempt to delete ', spot, 'th element of a NIL list.');
                exit;
(* !!!
		writewarn("lists.listdel(): attempt to delete %d'th element of a NIL list.\n", spot);
		return;
*)
	end;

	if (NOT listinitialized(p)) then begin
                writeln('lists.listdel(): attempt to delete ', spot, 'th element of an uninitialized list.');
                exit;
(* !!!
		writewarn("lists.listdel(): attempt to delete %d'th element of uninitialized list.\n", spot);
		return;
*)
	end;

{
	if (spot < 0) then begin
(* !!!
		writewarn("lists.listdel(): attemp to delete element from spot (%d) that is < 0.\n", spot);
		return;
*)
	end;
}

	if (spot >= thelen(p)) then begin
                writeln('lists.listdel(): attempt to delete ', spot,
                        'th element from list with only ', thelen(p), ' elements.');
                exit;
(*
		writewarn("lists.listdel(): attempt to delete %d'th element from list with only %d elements.\n", spot, thelen(p));
		return;
*)
	end;

	q := eptr(p, spot);

        if (q = NIL) then begin
                writeln('listdel(): eptr() returned NIL.');
        end;

	if (thelen(p) = 1) then begin  (* List len = 1 *)
		(* We must be removing the 0'th elmt: *)
		alistptr(p)^.first := NIL; alistptr(p)^.last := NIL; alistptr(p)^.recent := NIL;
		alistptr(p)^.mostrecent := 0;  (* Useless *)
	end else begin  (* List len >= 2 *)
		if (spot = 0) then begin
			(* We're removing the 0'th elmt *)
			alistptr(p)^.recent := q^.next;
                        alistptr(p)^.first := q^.next;
			q^.next^.prev := NIL;
		end else begin
			if (spot = thelen(p)-1) then begin  (* Last elmt *)
				alistptr(p)^.recent := q^.prev;
                                alistptr(p)^.last := q^.prev;
				dec(alistptr(p)^.mostrecent);
				q^.prev^.next := NIL;
			end else begin
				alistptr(p)^.recent := q^.next;
				q^.prev^.next := q^.next;
				q^.next^.prev := q^.prev;
			end;
		end;
	end;

	if (q^.dispose) then begin
		(* A list function allocated it, so we've got to dispose of it *)
                dispose(q^.data);
{		freemem(q^.data, q^.width); }
	end;
	q^.signature := chr(0);
	dispose(q);
	dec(alistptr(p)^.len);
end;

function listwipe(var p: list): list;
begin
	if (p <> NIL) then begin
               while (listlen(p) <> 0) do listdel(p, 0);
        end;
        p := listnew;
        listwipe := p;
end;

procedure elmtinsert(p: list; q: elmtptr; spot: word);
(* Adds element 'q' to spot 'spot' (offset from 0) in alist 'p'
   (or to end of list if list contains fewer than spot+1 elments) *)
var
	e: elmtptr;
begin
	if ((thelen(p) = 0) OR (spot = 0)) then begin
		(* Empty list, or spot = 0, so this becomes the first
		   (0'th) element: *)
		q^.next := alistptr(p)^.first;  (* Might be NIL, but that's fine *)
		q^.prev := NIL;  (* There is no previous element: this is the first one! *)

		if (thelen(p) <> 0) then begin
			q^.next^.prev := q;
		end else begin
			alistptr(p)^.last := q;
		end;
		alistptr(p)^.first := q; alistptr(p)^.recent := q;
		alistptr(p)^.mostrecent := 0;
	end else begin
		(* list has at least 1 elmt already, and spot > 0: *)
		if (spot >= thelen(p)) then begin
			(* Add to end of list *)
                        e := eptr(p, thelen(p)-1);
			if (e = NIL) then begin
                                writeln('lists.elmtinsert(): eptr() returned NIL (spot=',
                                        spot, ', listlen=', thelen(p), ').');
                                exit;
(* !!!
				writewarn("lists.insert(): eptr() returned NIL (spot=%d, listlen=%d)\n", spot, thelen(p));
				return;
*)
			end;
			e^.next := q;
			q^.prev := e;
			q^.next := NIL;
			alistptr(p)^.last := q; alistptr(p)^.recent := q;
			alistptr(p)^.mostrecent := thelen(p);
		end else begin
                        e := eptr(p, spot);
                	if (e = NIL) then begin
				(* This should never happen *)
                                writeln('lists.elmtinsert(): eptr() returned NIL (spot=',
                                         spot, ', listlen=', thelen(p), ').');
                                exit;
(* !!!
				writewarn("lists.insert(): eptr() returned NIL (spot=%d, listlen=%d)\n", spot, thelen(p));
				return;  { To avoid further corruption }
*)
			end;
			q^.next := e;
			q^.prev := e^.prev;
			e^.prev^.next := q;
			e^.prev := q;

			alistptr(p)^.recent := q;
			alistptr(p)^.mostrecent := spot;
		end;
	end;
	inc(alistptr(p)^.len);
end;

procedure my_move(source, dest: pointer; count: word);
type
    bigarray = array[1..65535] of char;
    arrayptr = ^bigarray;
var
     i: word;
begin
     for i := 1 to count do begin
          arrayptr(dest)^[i] := arrayptr(source)^[i];
     end;
end;

function listinsert(p: list; data: pointer; width: word; spot: word): pointer;
var
	q: elmtptr;
        src, dest: ^char;
begin
	if (p = NIL) then begin
                writeln('lists.listinsert(): attempt to insert element into NIL list.');
                q := NIL;
(* !!!
		writewarn("lists.listinsert(): attempt to insert element into NIL list.\n");
		return NIL;
*)
	end else if (NOT listinitialized(p)) then begin
                writeln('lists.listinsert(): attempt to insert element into uninitialized list.');
                q := NIL;
(* !!!
		writewarn("lists.listinsert(): attempt to insert element into uninitialized list.\n");
		return NIL;
*)
	end else begin
                q := newelmt;
               	if (q = NIL) then begin
                       writeln('lists.listinsert(): newelmt() returned NIL.');
(* !!!
                       writewarn("lists.listinsert(): newelmt() returned NIL.\n");
*)
                 end else begin
                       getmem(q^.data, width);
                       if (q^.data = NIL) then begin
                               writeln('lists.listinsert(): getmem(', width, ') returned NIL.');
(* !!!
                               writewarn("listinsert(): malloc(%d) returned NIL.\n", width);
*)
                       end else begin
                               my_move(data, q^.data, width);
                               q^.dispose := TRUE;
                               q^.width := width;
                               elmtinsert(p, q, spot);
                      end;
		end;
	end;

        if (q = NIL) then begin
              listinsert := NIL;
        end else begin
              listinsert := q^.data;
        end;
end;

function listabsorb(p: list; data: pointer; width: word; spot: word): pointer;
var
	q: elmtptr;
begin
	if (p = NIL) then begin
                writeln('lists.listabsorb(): attempt to absorb element into NIL list.');
                q := NIL;
(* !!!
		writewarn("lists.listabsorb(): attempt to absorb element into NIL list.\n");
		return NIL;
*)
	end else if (NOT listinitialized(p)) then begin
                writeln('lists.listabsorb(): attempt to absorb element into uninitialized list.');
                q := NIL;
(* !!!
		writewarn("lists.listabsorb(): attempt to absorb element into uninitialized list.\n");
		return NIL;
*)
	end else begin
                q := newelmt;
                if (q = NIL) then begin
                      writeln('lists.listabsorb(): newelmt() returned NIL.');
(* !!!
                      writewarn("lists.listinsert(): newelmt() returned NIL.\n");
*)
                end else begin
		    q^.dispose := FALSE;
		    q^.width := width;
		    q^.data := data;
		    elmtinsert(p, q, spot);
                end;
	end;

        if (q = NIL) then begin
              listabsorb := NIL;
        end else begin
              listabsorb := data;
        end;
end;

function listelmt(p: list; spot: word): pointer;
var
	q: elmtptr;
begin
	if (p = NIL) then begin
                writeln('lists.listelmt(): attempt to get ', spot, 'th element of a NIL list.');
                q := NIL;
(* !!!
		writewarn("lists.listelmt(): attempt to get %d'th element of a NIL list.\n", spot);
		return NIL;
*)
	end else if (NOT listinitialized(p)) then begin
                writeln('lists.listelmt(): attempt to get ', spot, 'th element of an uninitialized list.');
                q := NIL;
(* !!!
		writewarn("lists.listelmt(): attempt to get %d'th element of uninitialized list.\n", spot);
		return NIL;
*)
	end else if (spot >= thelen(p)) then begin
               writeln('lists.listelmt(): attempt to get ', spot,
                       'th element from list with only ', thelen(p), ' elements.');
               q := NIL;
(* !!!
		writewarn("lists.listelmt(): attempt to get %d'th element from list with only %d elements.\n", spot, thelen(p));
		return NIL;
*)
        end else begin
        	q := eptr(p, spot);
                (* Should return NIL only if list is corrupt (or, God forbid,
                   the eptr() function has a bug), but you never know *)
        end;

        if (q = NIL) then begin
               listelmt := NIL;
         end else begin
               listelmt := q^.data;
        end;
end;

function elmtwidth(p: list; spot: word): word;
var
	q: elmtptr;
begin
	if (p = NIL) then begin
                writeln('lists.elmtwidth(): attempt to get ', spot, 'th element of a NIL list.');
                q := NIL;
(* !!!
		writewarn("lists.elmtwidth(): attempt to get %d'th element of a NIL list.\n", spot);
		return 0;
*)
	end else if (NOT listinitialized(p)) then begin
                writeln('lists.elmtwidth(): attempt to get ', spot, 'th element of an uninitialized list.');
                q := NIL;
(*
		writewarn("lists.elmtwidth(): attempt to get %d'th element of uninitialized list.\n", spot);
		return 0;
*)
	end else if (spot >= thelen(p)) then begin
                writeln('lists.elmtwidth(): attempt to get ', spot,
                        'th element from a list with only ', thelen(p), ' elements.');
                q := NIL;
(* !!!
		writewarn("lists.elmtwidth(): attempt to get %d'th element from list with only %d elements.\n", spot, thelen(p));
		return 0;
*)
	end else begin
        	q := eptr(p, spot);
	        (* Should return NIL only if list is corrupt (or, God forbid,
	           the eptr() function has a bug), but you never know *)
        end;

        if (q = NIL) then begin
               elmtwidth := 0;
        end else begin
               elmtwidth := (q^.width);
        end;
end;

procedure listdispose(var p: list);
begin
	if (p = NIL) then begin
                writeln('lists.listdispose(): attempt to dispose NIL list.');
                exit;
(* !!!
		writewarn("lists.listdispose(): attempt to dispose NIL list.\n");
		return NIL;
*)
	end else if (NOT listinitialized(p)) then begin
                writeln('lists.listdispose(): attempt to dispose uninitialized list.');
                exit;
(* !!!
		writewarn("lists.listdispose(): attemp to dispose uninitialized list.\n");
		return NIL;
*)
	end;

	while (thelen(p) <> 0) do listdel(p, 0);

	if (alistptr(p)^.first <> NIL) then begin
                writeln('lists.listdispose(): listlen(p) = 0 but p^.data <> NIL.');
                exit;
(* !!!
		writewarn("lists.listdispose(): listlen(p) = 0 but p^.data <> NIL.\n");
		return NIL;
*)
	end;
	alistptr(p)^.signature := 0;
	dispose(p);
	p := NIL;
end;

{$F-}

end.

