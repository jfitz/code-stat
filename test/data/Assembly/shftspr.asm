;; Example to draw a pre-shifted masked sprite to the screen
;;
;; The sprite pixels are stored in a form that can be poked
;; direct to screen and seen.
;;
;; This example uses mode 1 sprites where pen 0 is transparent
;; leaving 3 other pens for sprites.
;;
;; A mask table/array is generated for each byte combination (mask table
;; is therefore 256 bytes long).
;;
;; Using the sprite pixel data, the appropiate mask is found in the
;; table/array.
;; 
;; The sprite is then combine with the screen pixels to the display.
;;
;; However, drawing the sprite to the display also changes it.
;; So in this example the rectangle that the sprite will cover
;; is stored into a buffer so it can be copied back to the screen when
;; the sprite is moved.
;;
;; Normally, moving a sprite byte-by-byte on a mode 1 screen will move
;; the sprite at a rate of 4 pixels a time.
;;
;; However, we use pre-shifted sprites to move 1 pixel at a time.
;; We have one image per pixel in a byte. So we have 4 images, each moved by 1
;; pixel more than the other.
;;
;; As we move we decide which to draw, and in this way the sprite appears to move
;; at 1 pixel rate.
;;
;; In addition this code also shows how to speed up KM_READ_CHAR by changing the
;; keyboard repeat rate, and also only redrawing the sprite when necessary. Both are
;; suggestions from Markus.
;;
;; This code is dedicated to Markus because he likes Smileys with Tongues.
;;
;; This source released under the GNU Library License v2 or later.
;; Code by Kev Thacker 2010
;;
;; NOTE: Sprite will flicker when you move.

;; sprite width in bytes
sprite_width equ 64/4
sprite_height equ 64

;; firmware functions we use
scr_next_line equ &bc26
scr_set_mode equ &bc0e
scr_set_border equ &bc38
scr_set_ink equ &bc32
mc_wait_flyback equ &bd19
km_read_char equ &bb09
km_set_delay equ &bb3f

;; table is aligned on a 256-byte boundary
mask_table equ &200

org &1000
nolist

start:
;; equivalent to SPEED KEY 1,1
;; tip from Markus to speed up key repeat 
;; when using km_read_char
ld h,1
ld l,1
call km_set_delay

;; set 4 colour mode 1
ld a,1
call scr_set_mode

;; set border to black
ld bc,0
call scr_set_border

;; set pens
ld hl,pens
ld b,4			;; 4 pens for mode 1
xor a			;; initial pen index
set_pens
push bc
push af
ld c,(hl)		;; B,C = inks for pen. If they are the same then no 
				;; flashing
ld b,c
inc hl
push hl
call scr_set_ink
pop hl
pop af
pop bc
inc a			;; increment pen index
djnz set_pens

ld hl,scr_pixels
ld de,&c000
ld bc,&4000
ldir



call make_scr_table
call init_mask_table

;; define initial sprite coords
ld hl,50
ld (sprite_x),hl
ld a,100
ld (sprite_y),a

call calc_spr_scr_addr

call draw_sprite

;; the main loop which draws and updates sprite
main_loop:
call mc_wait_flyback


;; check keys and update sprite position
call check_keys

;; redraw sprite?
ld a,(spr_redraw)
or a
jr z,main_loop2

;; reset for next time
xor a
ld (spr_redraw),a

;; restore background (where sprite used to be)
ld hl,(spr_scr_addr)
ld de,sprite_background
ld b,sprite_height
ld c,sprite_width
;; low byte of x coordinate
ld a,(spr_pixel_offs)
call sprite_background_restore

;; recalc screen address for new position
call calc_spr_scr_addr

call draw_sprite

;; jumps to here if sprite has not moved and no update is required
main_loop2:

jp main_loop

draw_sprite:

;; store background pixels where sprite is located
ld hl,(spr_scr_addr)
ld de,sprite_background
ld b,sprite_height
ld c,sprite_width
;; low byte of x coordinate
ld a,(spr_pixel_offs)

call sprite_background_store


;; draw sprite (writes over background pixels)
ld de,sprite_pixel_ptrs
ld hl,(spr_scr_addr)
ld b,sprite_height
ld c,sprite_width

ld a,(spr_pixel_offs)


call sprite_draw
ret

;; This function will generate a lookup table of pixel masks.
;; This table can be used to plot masked sprites onto a mode 1 display.
;; 
;;
;; call init_mask_table to initialise.
;; 
;; 
;; NOTES:
;; - This code is for mode 1.
;; - The pen which will be plotted transparent is 0.
;; - Relocate mask_table to an address which is on a 256 byte boundary. (i.e. 
;; it's low byte is 0). This will allow you to access it much quicker.

.init_mask_table
ld hl,mask_table						;; base address of the mask table
ld b,0							;; counter
ld c,0							;; initial pixel value

.mmt1
ld d,0							;; initialise initial mask

ld a,c
and &88
jr z,mmt2
ld a,d
or &88
ld d,a

.mmt2
ld a,c
and &44
jr z,mmt3
ld a,d
or &44
ld d,a

.mmt3
ld a,c
and &22
jr z,mmt4
ld a,d
or &22
ld d,a

.mmt4
ld a,c
and &11
jr z,mmt5

ld a,d
or &11
ld d,a

.mmt5
ld a,d
cpl 
ld (hl),a						;; store final mask in table
inc hl							;; increment position in table
inc c							;; increment pixel value
djnz mmt1						;; loop
ret


;; check if a key has been pressed and perform action if it has
check_keys:
call km_read_char
ret nc
;; A = ascii char of key pressed
;; we check for both upper and lower case chars
cp 'q'
jp z,sprite_up
cp 'Q'
jp z,sprite_up
cp 'a'
jp z,sprite_down
cp 'A'
jp z,sprite_down
cp 'o'
jp z,sprite_left
cp 'O'
jp z,sprite_left
cp 'p'
jp z,sprite_right
cp 'P'
jp z,sprite_right
ret

;; move sprite one byte to left
;; 2 pixels in mode 0
sprite_left:
ld hl,(sprite_x)
ld a,h
or l
ret z
dec hl
ld (sprite_x),hl
ld a,1
ld (spr_redraw),a
ret

;; mode sprite one byte to right
;; 2 pixels in mode 0
sprite_width_pixels equ sprite_width*4
right_side equ 320-sprite_width_pixels

sprite_right:
ld hl,(sprite_x)
ld bc,right_side
or a
sbc hl,bc
ret p
add hl,bc
inc hl
ld (sprite_x),hl
ld a,1
ld (spr_redraw),a

ret

;; move sprite up one scan line
sprite_up:
ld a,(sprite_y)
or a
ret z
dec a
ld (sprite_y),a
ld a,1
ld (spr_redraw),a

ret

bottom_side equ 199-sprite_height

;; move sprite down one scan line
sprite_down:
ld a,(sprite_y)
cp bottom_side
ret z
inc a
ld (sprite_y),a
ld a,1
ld (spr_redraw),a

ret

;; Y line coordinate of sprite
sprite_y:
defb 0

;; x pixel coordinate of sprite
sprite_x:			
defw 0

;; screen address of sprite
spr_scr_addr:
defw 0

spr_pixel_offs:
defb 0

;; this will be 0 for no redraw
;; 1 otherwise
;; 
;; this is one way to reduce flicker by only
;; drawing sprite if it has moved
spr_redraw:
defb 0

;; From our x,y coordinates generate
;; a screen address for top-left of sprite to be drawn at.

;; IN:
;; H = x byte coord
;; L = y line
;; OUT:
;; HL = screen address
get_scr_addr:
push bc
push de
ld c,h
ld h,0
add hl,hl
ld de,scr_addr_table
add hl,de
ld a,(hl)
inc hl
ld h,(hl)
ld l,a
ld b,0
add hl,bc
pop de
pop bc
ret

;; generate table of screen addresses
;; one address per scanline. The address
;; is for the first byte of each line.
make_scr_table:
ld ix,scr_addr_table		;; address to store table
ld hl,&c000					;; start address of first scanline
ld b,200					;; number of scanlines on screen
mst1:
ld (ix+0),l
ld (ix+1),h
inc ix
inc ix
push bc
call scr_next_line
pop bc
djnz mst1
ret


calc_spr_scr_addr:
;; X screen coord in range 0..320
;; convert to byte coordinates by dividing X by 4
;; (4 = number of pixels per byte in mode 1)
ld hl,(sprite_x)
srl h				;; HL = HL / 2
rr l
srl h				;; HL = HL / 2
rr l
					;; result = HL = HL /4
ld h,l
;; now fetch y line coordinate
ld a,(sprite_y)
ld l,a
;; calculate screen address
call get_scr_addr
;; store it
ld (spr_scr_addr),hl

ld a,(sprite_x+0)
;; find pixel offset within byte (0..3)
and &3
ld (spr_pixel_offs),a
ret

;; HL = screen address
;; DE = pointer to array of sprite data
;; A = pixel offset in sprite
;; B = height
;; C = width

sprite_draw:
or a
jr z,sprite_draw2
inc c					;; when sprite is shifted, the width is one byte larger

sprite_draw2:
ex de,hl
add a,a				;; A = A * 2
add a,l				;; add low byte of array 
ld l,a
ld a,h	
adc a,0
ld h,a
;; HL = address in array
ld a,(hl)
inc hl
ld h,(hl)
ld l,a				;; read address from array = address of sprite pixel data
ex de,hl



push bc
pop ix							;; low byte of IX = C
								;; high byte of IX = B
								
;; B = upper 8-bits (bits 15..8) of mask table memory address
ld b,mask_table/256

sprite_draw_height:
push ix
push hl


sprite_draw_width:
ld a,(de)							;; get byte of sprite pixel data
ld c,a								;; C = byte of sprite pixel data/look-up table value
									;; BC = address (in look-up table) of mask corresponding to this sprite pixel data
ld a,(bc)							;; lookup mask from table
and (hl)							;; mask pixels on screen (remove pixels which will be replaced)
or c								;; combine with sprite pixel data
ld (hl),a							;; write result to screen 
inc de
inc hl
defb &dd							;; DEC LIX
dec l
jr nz,sprite_draw_width
pop hl
call scr_next_line
pop ix
defb &dd							;; DEC HIX
dec h
jr nz,sprite_draw_height
ret

;; HL = screen address of sprite
;; DE = address to store screen data
;; B = height
;; C = width
;; A = pixel offset into byte
;; store a rectangle from the screen into a buffer
sprite_background_store:
or a
jr z,sprite_back_height
inc c					;; when sprite is shifted, the width is one byte larger

sprite_back_height:
push bc
push hl

sprite_back_width:
ld a,(hl)				;; read from screen
ld (de),a				;; store to buffer
inc hl
inc de
dec c
jr nz,sprite_back_width

pop hl
call scr_next_line
pop bc
djnz sprite_back_height
ret

;; HL = screen address of sprite
;; H = x byte coord
;; L = y line
;; DE = address to store screen data
;; B = height
;; C = width
;; A = pixel offset into byte
;; restore a rectangle to the screen
sprite_background_restore:
or a
jr z,sprite_reback_height
inc c					;; when sprite is shifted, the width is one byte larger

sprite_reback_height:
push bc
push hl

sprite_reback_width:
ld a,(de)					;; read from buffer
ld (hl),a					;; write to screen
inc hl
inc de
dec c
jr nz,sprite_reback_width

pop hl
call scr_next_line
pop bc
djnz sprite_reback_height
ret

sprite_preshifted_width equ sprite_width+1
buffer_size equ sprite_height*sprite_preshifted_width

;; a buffer to store screen behind sprite
sprite_background:
defs buffer_size

;; table/array for screen addresses for each scan line
scr_addr_table:
defs 200*2

;; the inks for the pens
pens:
defb 1
defb 20
defb 2
defb 14


sprite_pixel_ptrs:
defw smiley1			;; sprite at pixel pos 0 in byte
defw smiley2			;; sprite at pixel pos 1 in byte
defw smiley3			;; sprite at pixel pos 2 in byte
defw smiley4			;; sprite at pixle pos 3 in byte


scr_pixels:
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f8,&f0,&f2,&f0,&f0,&f0,&f0,&f0,&f8,&f0,&f0,&f0,&f4,&f1 
defb &f4,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f7,&f0,&fc,&f1 
defb &f0,&f1,&f6,&f0,&f0,&f0,&f3,&f0,&f0,&f0,&f0,&f0,&fd,&f4,&f0,&f0 
defb &f9,&f4,&e1,&60,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ee,&11,&13,&f0,&f0,&f0,&f0,&f1,&f0,&f0,&f1,&fa,&f4,&f2,&f0 
defb &f3,&f0,&f0,&f0,&f1,&f0,&f0,&f0,&f0,&f0,&fb,&d5,&f0,&f0,&f0,&f0 
defb &f0,&f2,&f9,&f8,&f8,&f3,&f0,&f0,&f0,&f0,&f0,&a8,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ee,&00,&00,&00,&00,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&72 
defb &0f,&0f,&0f,&0f,&9e,&ff,&f7,&77,&ff,&7f,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ee,&ef,&23,&e7,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&23,&f6,&0f,&0f,&0f,&0f,&6c,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&99,&8f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&23,&f6,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &3f,&3f,&87,&0e,&08,&00,&00,&00,&03,&0f,&0f,&0f,&2d,&7f,&8f,&0f 
defb &1f,&4d,&5f,&67,&bf,&2f,&8e,&2d,&9e,&2f,&8f,&cf,&0f,&0f,&0f,&1f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&4f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&9e,&4f,&af,&7f,&ca,&0f,&0f,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &0f,&0a,&00,&00,&00,&00,&00,&03,&02,&12,&81,&25,&09,&52,&8e,&0d 
defb &cf,&b7,&4f,&9f,&0f,&3f,&0f,&1b,&1f,&0f,&0e,&af,&0f,&9f,&0f,&4f 
defb &00,&07,&0f,&1f,&7f,&3f,&df,&ff,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&7e,&0f,&0f,&0f,&0e,&0a,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&01,&0f,&0f,&0f,&c2,&0f,&cb,&0f,&0f,&87,&0f,&0f,&0e,&2d 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&02,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &02,&08,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&02,&09,&f7 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f8,&f0,&f1,&f0,&f0,&f0,&f0,&f1,&fc,&f0,&f0,&f0,&f1 
defb &f0,&f4,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f8,&f2,&f0,&f0 
defb &f0,&f0,&f0,&f1,&f0,&f0,&f0,&f7,&f0,&f0,&f3,&f0,&f0,&f0,&f0,&f0 
defb &f6,&e0,&b3,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&d1,&f6,&f8,&f3,&fc,&f4,&f0,&f0,&f0,&f0,&f1,&f8 
defb &f8,&f0,&f0,&f0,&f0,&f1,&f0,&f0,&f6,&d4,&00,&fd,&30,&92,&f0,&f0 
defb &f1,&f8,&f2,&f0,&f1,&f0,&f0,&f0,&f0,&f2,&b8,&55,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&dd,&ff,&ff,&ff,&b2,&33,&33,&a7 
defb &0f,&0f,&0f,&0f,&4b,&a8,&68,&26,&75,&77,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&f5,&7b,&27,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&99,&fc,&27,&0f,&0f,&0f,&0f,&0f,&96,&f3,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&b9,&c3,&1f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ee,&77,&27,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0e,&0f,&0f,&0f,&0f,&0f,&0e,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&cf,&0f 
defb &87,&cb,&0f,&0f,&00,&00,&00,&00,&00,&01,&0f,&0f,&1e,&e3,&cf,&0f 
defb &df,&3f,&5c,&af,&e7,&0b,&0a,&0a,&3e,&1f,&da,&12,&9f,&2f,&8f,&6f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&cf,&0f,&0f,&0f,&fc,&87,&0f,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&0f,&00,&00,&03,&09,&02,&16,&6d,&86,&49 
defb &ca,&af,&1e,&2f,&2f,&0e,&0f,&0f,&0f,&0f,&4e,&0b,&0f,&2d,&0f,&0f 
defb &00,&03,&0f,&3d,&0f,&f7,&ff,&cf,&0f,&ff,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&7f,&87,&0d,&0f,&0f,&0c,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&0f,&0f,&0f,&0e,&05,&a7,&0f,&4f,&87,&00,&01,&ce,&0b 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&08,&0e 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f8,&f8,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f8 
defb &f9,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f1,&f8,&f8,&f7 
defb &f0,&f2,&f9,&f8,&f0,&f0,&f0,&f0,&f0,&f1,&f0,&f0,&fa,&fa,&f0,&f0 
defb &f9,&50,&33,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&e8,&51,&f0,&f8,&f1,&f4,&f0,&f0,&fa,&f0,&f4,&f0 
defb &f1,&f8,&f0,&f0,&fc,&fc,&f0,&f0,&b4,&51,&33,&ff,&ff,&88,&b2,&f4 
defb &f0,&f1,&f2,&f0,&f0,&f1,&f0,&f0,&f7,&28,&33,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&f1,&7a,&4f,&67,&07 
defb &0f,&0f,&0f,&0f,&7b,&7f,&2e,&2d,&9e,&ff,&ff,&ff,&ff,&ff,&ff,&55 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&dd,&6d,&2a,&8f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&fd,&87,&1f,&8f,&0f,&0f,&0f,&0f,&0f,&1f,&8f,&3f,&91,&fb,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&98,&cf,&cf,&0f,&0f,&0f 
defb &0f,&0f,&0f,&4f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&77,&cf,&78,&8f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&ff,&0f,&0f,&ef,&0f,&0d,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&1f,&3f,&ff 
defb &69,&0e,&0f,&0f,&00,&00,&00,&00,&00,&00,&01,&0f,&0f,&3c,&ff,&ff 
defb &2a,&4b,&af,&0e,&a7,&08,&07,&07,&07,&0f,&0f,&f0,&9f,&0f,&8f,&26 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &8f,&1f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&3f,&6d,&0f,&0c,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&0f,&00,&00,&06,&03,&00,&01,&06,&0a,&0d 
defb &ef,&be,&cf,&0f,&83,&5e,&0f,&0f,&0f,&0f,&27,&8f,&4f,&0f,&0f,&0f 
defb &00,&00,&0f,&0f,&0f,&0f,&0f,&3c,&ff,&cf,&ff,&ff,&ff,&ff,&3f,&cf 
defb &fe,&e1,&0d,&0e,&0a,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&04,&05,&2d,&0f,&04,&09,&04,&12,&05,&06 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&02,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f8,&f0,&f1,&f9,&f0,&f0,&f0,&38,&f0,&f0,&f0,&f1 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f1,&f0,&f0 
defb &f0,&f0,&f0,&f1,&f0,&f0,&f0,&f7,&fb,&f0,&ff,&f1,&f0,&f0,&f0,&f4 
defb &44,&71,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ee,&c0,&f0,&16,&f5,&f0,&f0,&f0,&f0,&f0,&f4,&f8 
defb &f0,&f3,&f0,&f0,&00,&01,&00,&00,&77,&ff,&ff,&ff,&ff,&ff,&b9,&f9 
defb &f8,&f0,&f9,&f8,&f0,&94,&00,&00,&40,&77,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&fe,&f6,&23,&0e,&2c,&ef,&8f,&4e 
defb &0f,&0f,&0f,&0f,&af,&5d,&db,&b9,&1b,&57,&3f,&19,&77,&ff,&f3,&f6 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &fc,&e6,&db,&5f,&0f,&8f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &bb,&c3,&0f,&0f,&0f,&8f,&0f,&0f,&0f,&0f,&8f,&0b,&e1,&4f,&7c,&dd 
defb &dd,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&a7,&97,&1f,&cf,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&98,&07,&87,&3f,&4b,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&cf,&0f,&0f,&0f,&0f,&ff,&8f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&3f,&0f,&ef,&cb 
defb &0e,&07,&0f,&08,&00,&00,&00,&00,&00,&00,&00,&0f,&0f,&0b,&0f,&e7 
defb &ff,&1f,&0d,&4f,&82,&0d,&05,&01,&07,&0f,&07,&3c,&0f,&9b,&7f,&0b 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &37,&cf,&2f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&3f,&1f,&fc,&87,&07,&03,&0c,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&0c,&00,&00,&04,&06,&01,&08,&03,&02,&0c 
defb &05,&03,&cf,&17,&2f,&0f,&3d,&3f,&7f,&4f,&8e,&5f,&0f,&0f,&0f,&0f 
defb &00,&00,&0f,&08,&0f,&0c,&0d,&0f,&07,&1e,&ff,&ff,&ff,&ff,&7c,&0f 
defb &0f,&0f,&0f,&0e,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&0c,&0a,&1e,&0e,&05,&00,&01,&00,&02 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&03,&03 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f4,&f0,&f6,&e2,&00,&00,&00,&88,&11,&c1,&78,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f1,&f9,&fe,&94,&00,&00,&12,&f0,&f0,&f2,&f0,&fc,&f5,&f0,&e3,&30 
defb &b4,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&44,&b0,&f4,&f2,&f0,&f1,&f0,&f1,&f8 
defb &f0,&f0,&00,&77,&99,&99,&00,&11,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&aa 
defb &36,&e1,&e2,&a0,&e0,&cc,&00,&00,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ee,&c7,&dc,&4f,&0f,&9d,&5d,&1f,&8f 
defb &0f,&0f,&0f,&0f,&4f,&9f,&af,&bf,&aa,&8a,&0c,&3f,&b6,&3e,&2d,&ab 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&99 
defb &f9,&33,&43,&5d,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&cc,&a5 
defb &6f,&3e,&17,&cf,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&3f,&ef,&0f,&c7,&6f 
defb &d9,&ff,&ff,&ff,&ff,&ff,&df,&ff,&ff,&73,&3f,&4f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&99,&de,&6d,&2f,&ad,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&1f,&3f,&8f,&7f,&ff,&ff,&ff,&cf,&3f,&8f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&ff,&ff,&4b,&07 
defb &0f,&0f,&0e,&08,&00,&00,&00,&00,&00,&00,&00,&00,&03,&0f,&0f,&0e 
defb &5f,&87,&cf,&42,&0e,&06,&08,&08,&05,&82,&0d,&0f,&7e,&1a,&af,&9f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ef,&7f,&9f,&2f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&3f,&ff,&fc,&0f,&0f,&0f,&0e,&01,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&04,&01,&0b,&0f,&02,&05 
defb &0c,&0f,&c9,&3e,&df,&73,&af,&47,&0f,&1f,&6f,&4f,&0f,&2f,&0f,&0f 
defb &00,&00,&00,&0e,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &08,&0f,&07,&08,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &07,&0f,&0c,&00,&00,&00,&04,&01,&0a,&1a,&0b,&05,&00,&08,&0b,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f6,&9c,&90,&f7,&ff,&ff,&ff,&ff,&ee,&98,&b2,&f7 
defb &f2,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f6,&f0,&f0 
defb &80,&7c,&bc,&e0,&77,&88,&91,&10,&d1,&f4,&f8,&f0,&e4,&58,&d1,&77 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&e8,&f9,&f0,&f0,&f5,&f0,&f0,&f6 
defb &d6,&f0,&77,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &cc,&ff,&99,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&dd,&30,&8b,&0a,&0f,&1f,&2e,&2f,&8e,&13 
defb &0f,&0f,&0f,&0f,&4f,&af,&0f,&0f,&1f,&5f,&7f,&8e,&8f,&1f,&7f,&47 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ee,&99,&2f 
defb &13,&5d,&0f,&4f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ee,&dc,&9f,&cf 
defb &84,&2e,&8f,&0f,&6f,&0f,&0f,&0f,&0f,&0f,&4f,&0e,&03,&0f,&47,&12 
defb &ff,&59,&ff,&ff,&ff,&ff,&ff,&dd,&33,&6e,&0f,&7f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&87,&13,&0f,&4f,&0f,&0f,&4f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&fe,&87,&0f,&0f,&0f,&f3,&ff,&7f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&1f,&cb,&0f,&0c 
defb &09,&08,&00,&06,&00,&00,&00,&00,&00,&00,&00,&00,&0f,&0f,&00,&0f 
defb &0b,&0d,&4b,&0e,&07,&0a,&84,&0b,&08,&8a,&0d,&0f,&04,&0f,&0e,&8f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &3d,&ff,&9f,&e7,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&ef,&3f,&0f,&0f,&0b,&0f,&0c,&06,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&01,&00,&0c,&02,&08,&0e,&07 
defb &02,&86,&8d,&63,&4f,&87,&3f,&f9,&8d,&fc,&7b,&3f,&bf,&0f,&0f,&0f 
defb &00,&00,&00,&07,&0e,&00,&00,&00,&0f,&09,&0f,&0f,&0f,&0f,&0f,&0e 
defb &08,&03,&02,&00,&00,&00,&00,&00,&02,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &0f,&0f,&0c,&00,&00,&00,&00,&01,&0b,&01,&03,&0c,&01,&0c,&12,&49 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f1,&f1,&84,&f3,&77,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&88,&b2 
defb &f2,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f4,&b8,&30,&56 
defb &10,&80,&60,&ff,&ff,&ff,&ff,&ff,&51,&20,&60,&ea,&50,&40,&ff,&ff 
defb &ff,&df,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&fe,&32,&f8,&f8,&f0,&fb,&d2,&e0 
defb &00,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ef 
defb &ff,&ee,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&fc,&e3,&0f,&2f,&3f,&8f,&1d,&1d,&8f,&cf 
defb &0f,&0f,&0f,&0f,&0f,&0b,&0f,&0f,&6f,&9f,&0f,&1f,&2f,&2f,&2a,&ae 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&44,&fd,&ff,&df,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&88,&3f,&2f 
defb &4f,&0f,&8f,&2f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&aa,&aa,&f9,&0c,&17 
defb &7f,&3f,&1f,&cf,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&4f,&cf,&3f,&0f,&ff 
defb &08,&4f,&d9,&dc,&6c,&ff,&ff,&e5,&f6,&73,&0f,&4f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &cc,&5f,&0f,&0f,&8f,&0f,&2a,&ab,&27,&1f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&4f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&9f,&ff,&0f,&0f,&0f,&0f,&0c,&0f,&3d,&ef,&8f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&fe,&86,&08,&07 
defb &00,&01,&00,&06,&0f,&0f,&0f,&0e,&00,&00,&00,&03,&0c,&00,&0f,&00 
defb &0c,&2d,&02,&09,&0d,&1a,&08,&08,&03,&0b,&07,&0a,&0f,&1e,&3d,&2d 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &08,&3f,&6b,&5f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&1f,&ef,&ef,&cb,&01,&0f,&0a,&05,&00,&0e,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&08,&00,&00,&00,&01,&0c,&0e,&0b,&06,&02 
defb &03,&0b,&03,&0f,&0e,&0f,&43,&0b,&f7,&87,&97,&4f,&ba,&af,&1f,&1f 
defb &0f,&0c,&06,&00,&00,&00,&08,&00,&00,&0f,&0f,&0f,&0f,&0f,&00,&07 
defb &00,&00,&00,&06,&00,&00,&00,&00,&0f,&0f,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &0f,&0f,&0f,&0e,&00,&00,&00,&01,&00,&00,&0d,&0d,&00,&0c,&0d,&4b 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&06 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f6,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&e2,&00,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&10 
defb &d1,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&a8,&77,&ff,&ff 
defb &ff,&ee,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ee,&66,&10,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ee,&98,&34,&f8,&fa,&44,&d0,&50 
defb &77,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &bb,&fb,&b8,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&fd,&ed,&0f,&0f,&1f,&0d,&0f,&6f,&4f,&2f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&8f,&0f,&0f,&0f,&4f,&8f,&0f,&cf,&4f,&9f,&1e,&17 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&fd,&fd,&f5,&7a,&fa,&fa,&ff,&ff,&ff,&ff,&ff,&ff 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&d9,&0f,&2f 
defb &2b,&2e,&5f,&8f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&ff,&fa,&c3,&0f,&2f,&0f 
defb &0d,&8f,&8f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&1f,&0b 
defb &2f,&0d,&b4,&bd,&5e,&5d,&fe,&8f,&1f,&cf,&2f,&1f,&8f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &69,&8f,&0f,&0f,&0f,&cf,&4f,&2f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&9e,&0f,&08,&07,&0f,&0f,&0f,&0f,&0b,&1b,&ff,&ef,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&ff,&7f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&7f,&0f,&03,&0e,&00 
defb &02,&00,&02,&00,&0f,&0f,&0f,&0e,&00,&00,&00,&03,&04,&0c,&00,&01 
defb &06,&0a,&0d,&16,&0f,&07,&07,&06,&08,&06,&05,&0a,&00,&1a,&0f,&9f 
defb &e3,&2f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &01,&0f,&27,&5f,&df,&5f,&4f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f,&0f 
defb &0f,&0f,&1f,&f1,&0f,&0f,&0f,&0e,&03,&04,&0c,&06,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&03,&00,&00,&00,&02,&02,&0a,&07,&0e 
defb &00,&05,&0c,&0d,&0f,&0d,&4e,&0a,&df,&1f,&af,&1f,&4a,&0f,&7f,&2d 
defb &0f,&0c,&07,&00,&00,&00,&00,&00,&08,&00,&00,&00,&00,&00,&0f,&00 
defb &00,&00,&00,&06,&00,&00,&00,&00,&0f,&0f,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &0f,&0f,&0f,&0e,&00,&00,&00,&01,&00,&02,&04,&08,&04,&09,&0b,&ce 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&07,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
;; scr

smiley1:
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&10,&fe,&80,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&31,&f9,&c0,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&d0,&00,&30,&80,&60,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&70,&fe,&80,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&f3,&f1,&40,&0f,&0e,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&10,&e5,&0f,&87,&0f,&e1,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&16,&87,&0f,&0f,&1e,&c3,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&01,&1e,&0f,&0f,&0f,&1e,&87,&0f,&00,&00,&00,&00,&00 
defb &00,&00,&00,&03,&0f,&0f,&0f,&0f,&1e,&87,&0f,&0c,&00,&00,&00,&00 
defb &00,&00,&01,&0f,&0f,&0f,&0f,&0f,&1f,&87,&0f,&0e,&00,&00,&00,&00 
defb &00,&00,&03,&0f,&0f,&0f,&3c,&f0,&f0,&87,&0f,&0f,&00,&00,&00,&00 
defb &00,&10,&03,&0f,&0f,&0f,&7a,&96,&f3,&87,&0f,&0f,&08,&00,&00,&00 
defb &00,&03,&07,&0f,&0f,&0f,&e5,&0f,&1e,&8f,&0f,&0f,&08,&00,&00,&00 
defb &00,&25,&0f,&0f,&0f,&1e,&e9,&0f,&1e,&87,&0f,&0f,&0c,&00,&00,&00 
defb &00,&4b,&0f,&0f,&0f,&3c,&cb,&0f,&0f,&87,&0f,&0f,&0e,&00,&00,&00 
defb &00,&c3,&0f,&0f,&0f,&3d,&87,&0f,&0f,&0f,&0f,&0f,&0f,&00,&00,&00 
defb &00,&87,&0f,&0f,&0f,&3c,&87,&0f,&78,&0f,&e5,&0f,&0f,&08,&00,&00 
defb &10,&87,&0f,&0f,&0f,&3e,&0f,&0f,&f7,&cb,&f6,&0f,&0f,&08,&00,&00 
defb &10,&0f,&0f,&0f,&0f,&7a,&0f,&1e,&ff,&cf,&f6,&0f,&0f,&0c,&00,&00 
defb &30,&0f,&0f,&0f,&0f,&78,&0f,&1e,&ff,&ed,&69,&0f,&0f,&0e,&00,&00 
defb &30,&0f,&78,&87,&0f,&69,&0f,&1e,&f7,&ed,&0f,&0f,&0f,&0e,&00,&00 
defb &61,&0f,&f0,&c3,&0f,&69,&0f,&0f,&f7,&ed,&0f,&0f,&0f,&0e,&00,&00 
defb &61,&0f,&f0,&c3,&0f,&78,&0f,&0f,&f5,&ed,&0f,&0f,&0f,&0e,&00,&00 
defb &61,&0f,&f0,&c3,&0f,&6b,&0f,&0f,&78,&c3,&4b,&0f,&0f,&0e,&00,&00 
defb &e1,&0f,&f0,&c3,&0f,&7a,&0f,&0f,&0f,&0f,&4f,&0f,&0f,&0e,&00,&00 
defb &e1,&87,&78,&c3,&0f,&7a,&0f,&0f,&0f,&0f,&e7,&0f,&0f,&0e,&00,&00 
defb &e1,&0f,&78,&87,&0f,&3d,&0f,&0f,&0f,&0f,&fd,&87,&0f,&2c,&00,&00 
defb &f0,&4b,&0f,&0f,&0f,&3d,&87,&0f,&0f,&1e,&f8,&cb,&0f,&2c,&00,&00 
defb &e1,&87,&3c,&0f,&0f,&1e,&cb,&0f,&0f,&1f,&da,&f4,&87,&68,&00,&00 
defb &f8,&4b,&78,&87,&0f,&1e,&e5,&0f,&0f,&3f,&87,&7b,&fc,&e8,&00,&00 
defb &f0,&4b,&3c,&87,&0f,&0f,&7a,&87,&0f,&f2,&0f,&3c,&f2,&e0,&00,&00 
defb &f8,&a5,&3c,&0f,&0f,&69,&3d,&fc,&f1,&e9,&1e,&4b,&d2,&e0,&00,&00 
defb &f0,&a5,&87,&0f,&0f,&e1,&0f,&f3,&f6,&87,&0f,&3c,&d2,&68,&00,&00 
defb &f8,&d2,&0f,&0f,&0f,&f0,&0f,&1e,&4b,&0f,&5a,&5a,&b4,&e0,&00,&00 
defb &74,&e1,&a5,&0f,&0f,&e1,&0f,&0f,&0f,&0f,&0f,&b4,&d2,&e0,&00,&00 
defb &74,&e1,&87,&0f,&0f,&4b,&0f,&0f,&0f,&1e,&5a,&5a,&f0,&e0,&00,&00 
defb &74,&f0,&5a,&0f,&1e,&87,&0f,&0f,&0f,&0f,&2d,&b4,&d2,&e0,&00,&00 
defb &74,&f0,&a5,&87,&3c,&e1,&0f,&0f,&0f,&5a,&5a,&5a,&f0,&c0,&00,&00 
defb &72,&f0,&d2,&5a,&b4,&e1,&0f,&0f,&0f,&0f,&a5,&b4,&f0,&c0,&00,&00 
defb &72,&f0,&e1,&87,&3c,&f0,&0f,&0f,&a5,&a5,&a5,&d2,&f0,&c0,&00,&00 
defb &32,&f0,&f0,&69,&78,&e1,&0f,&2d,&0f,&1e,&5a,&78,&f0,&f0,&e0,&00 
defb &31,&f0,&f0,&f0,&78,&e1,&a5,&1e,&5a,&5a,&78,&f0,&f1,&ff,&ff,&c0 
defb &31,&f0,&f0,&f0,&3c,&e1,&5a,&4b,&a5,&a5,&b4,&f0,&f3,&ff,&ff,&ce 
defb &12,&f8,&f0,&d2,&f0,&f0,&5a,&5a,&f0,&f0,&f0,&f0,&ff,&ff,&ff,&ef 
defb &10,&f8,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&78,&f0,&f1,&ff,&ff,&fe,&fa 
defb &01,&f8,&f0,&f0,&f0,&e1,&a5,&a5,&b4,&f0,&b4,&f3,&ff,&ff,&fa,&fb 
defb &00,&f4,&f0,&f0,&f0,&f0,&5a,&5a,&f1,&ff,&f8,&ff,&ff,&ff,&fd,&f1 
defb &00,&f6,&f0,&f0,&f0,&d2,&f0,&e1,&f3,&f1,&ff,&ff,&ff,&f4,&f4,&f1 
defb &00,&73,&f0,&f0,&f0,&f0,&f0,&f0,&f6,&f0,&f1,&f7,&fe,&fa,&f0,&f1 
defb &00,&71,&f0,&f0,&f0,&f0,&f0,&f8,&fc,&f0,&f0,&f3,&ff,&f0,&f0,&f1 
defb &00,&30,&f8,&f0,&f0,&f0,&f0,&f7,&f0,&f0,&f0,&f0,&f1,&f8,&f0,&f1 
defb &00,&10,&f6,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&e1,&10,&f6,&f0,&f2 
defb &00,&00,&73,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&c0,&00,&71,&f0,&e0 
defb &00,&00,&31,&fc,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&80,&00,&10,&f5,&c0 
defb &00,&00,&10,&fe,&f0,&f0,&f0,&f0,&f0,&f0,&f4,&00,&00,&00,&70,&80 
defb &00,&00,&00,&f7,&fe,&f0,&f0,&f0,&f0,&f1,&c0,&00,&00,&00,&00,&00 
defb &00,&00,&00,&11,&ff,&f8,&f0,&f0,&f1,&cc,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&33,&ff,&fe,&fb,&ff,&c8,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&77,&ff,&ff,&fe,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
;; smiley1

.smiley2
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&f7,&c0,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&10,&fc,&e8,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&60,&80,&10,&c0,&30,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&30,&f7,&c0,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&71,&f8,&a8,&07,&0f,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&f2,&0f,&4b,&0f,&78 
defb &08,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&03,&c3,&0f,&0f,&0f 
defb &e1,&08,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&0f,&87,&0f,&0f 
defb &0f,&c3,&0f,&08,&00,&00,&00,&00,&00,&00,&00,&00,&01,&0f,&0f,&0f 
defb &0f,&0f,&c3,&0f,&0e,&00,&00,&00,&00,&00,&00,&00,&00,&0f,&0f,&0f 
defb &0f,&0f,&0f,&cb,&0f,&0f,&00,&00,&00,&00,&00,&00,&00,&01,&0f,&0f 
defb &0f,&1e,&f0,&f0,&c3,&0f,&0f,&08,&00,&00,&00,&00,&00,&00,&81,&0f 
defb &0f,&0f,&3d,&c3,&f1,&cb,&0f,&0f,&0c,&00,&00,&00,&00,&00,&01,&0b 
defb &0f,&0f,&0f,&7a,&0f,&0f,&c7,&0f,&0f,&0c,&00,&00,&00,&00,&00,&12 
defb &0f,&0f,&0f,&0f,&f4,&0f,&0f,&c3,&0f,&0f,&0e,&00,&00,&00,&00,&00 
defb &25,&0f,&0f,&0f,&1e,&e5,&0f,&0f,&4b,&0f,&0f,&0f,&00,&00,&00,&00 
defb &00,&61,&0f,&0f,&0f,&1e,&cb,&0f,&0f,&0f,&0f,&0f,&0f,&08,&00,&00 
defb &00,&00,&43,&0f,&0f,&0f,&1e,&c3,&0f,&3c,&87,&7a,&0f,&0f,&0c,&00 
defb &00,&00,&00,&c3,&0f,&0f,&0f,&1f,&87,&0f,&7b,&ed,&7b,&87,&0f,&0c 
defb &00,&00,&00,&00,&87,&0f,&0f,&0f,&3d,&87,&0f,&f7,&ef,&7b,&87,&0f 
defb &0e,&00,&00,&00,&10,&87,&0f,&0f,&0f,&3c,&87,&0f,&f7,&fe,&3c,&0f 
defb &0f,&0f,&00,&00,&00,&10,&87,&3c,&c3,&0f,&3c,&0f,&0f,&f3,&fe,&0f 
defb &0f,&0f,&0f,&00,&00,&00,&30,&0f,&78,&e1,&0f,&3c,&0f,&0f,&7b,&fe 
defb &0f,&0f,&0f,&0f,&00,&00,&00,&30,&0f,&78,&e1,&0f,&3c,&87,&0f,&7a 
defb &fe,&0f,&0f,&0f,&0f,&00,&00,&00,&30,&0f,&78,&e1,&0f,&3d,&0f,&0f 
defb &3c,&e1,&2d,&0f,&0f,&0f,&00,&00,&00,&70,&0f,&78,&e1,&0f,&3d,&87 
defb &0f,&0f,&0f,&2f,&0f,&0f,&0f,&00,&00,&00,&70,&4b,&3c,&e1,&0f,&3d 
defb &87,&0f,&0f,&0f,&7b,&0f,&0f,&0f,&00,&00,&00,&70,&0f,&3c,&c3,&0f 
defb &1e,&8f,&0f,&0f,&0f,&7e,&cb,&0f,&1e,&00,&00,&00,&70,&a5,&0f,&0f 
defb &0f,&1e,&cb,&0f,&0f,&0f,&f4,&e5,&0f,&1e,&00,&00,&00,&70,&4b,&1e 
defb &87,&0f,&0f,&e5,&0f,&0f,&0f,&ed,&f2,&c3,&3c,&00,&00,&00,&74,&a5 
defb &3c,&c3,&0f,&0f,&f2,&0f,&0f,&1f,&cb,&3d,&fe,&f4,&00,&00,&00,&70 
defb &a5,&1e,&c3,&0f,&0f,&3d,&c3,&0f,&79,&87,&1e,&f1,&f0,&00,&00,&00 
defb &74,&d2,&1e,&87,&0f,&3c,&1e,&fe,&f0,&fc,&0f,&a5,&69,&f0,&00,&00 
defb &00,&70,&d2,&4b,&0f,&0f,&78,&0f,&79,&fb,&c3,&0f,&1e,&e1,&b4,&00 
defb &00,&00,&74,&e1,&87,&0f,&0f,&78,&87,&0f,&a5,&0f,&2d,&a5,&d2,&f0 
defb &00,&00,&00,&32,&f0,&5a,&0f,&0f,&78,&0f,&0f,&0f,&0f,&0f,&5a,&e1 
defb &f0,&00,&00,&00,&32,&f0,&4b,&0f,&0f,&2d,&0f,&0f,&0f,&0f,&a5,&a5 
defb &f0,&f0,&00,&00,&00,&32,&f0,&a5,&87,&0f,&c3,&0f,&0f,&0f,&0f,&1e 
defb &5a,&e1,&f0,&00,&00,&00,&32,&f0,&d2,&4b,&1e,&f0,&0f,&0f,&0f,&2d 
defb &a5,&a5,&f0,&e0,&00,&00,&00,&31,&f0,&e1,&a5,&d2,&f0,&0f,&0f,&0f 
defb &0f,&5a,&5a,&f0,&e0,&00,&00,&00,&31,&f0,&f0,&4b,&1e,&f0,&87,&0f 
defb &5a,&5a,&5a,&69,&f0,&e0,&00,&00,&00,&11,&f0,&f0,&b4,&3c,&f0,&0f 
defb &1e,&0f,&0f,&a5,&b4,&f0,&f0,&f0,&00,&00,&10,&f8,&f0,&f0,&b4,&f0 
defb &5a,&0f,&a5,&a5,&b4,&f0,&f0,&ff,&ff,&e8,&00,&10,&f8,&f0,&f0,&96 
defb &f0,&2d,&a5,&5a,&5a,&5a,&f0,&f1,&ff,&ff,&ef,&00,&01,&f4,&f0,&e1 
defb &f0,&f0,&a5,&a5,&f0,&f0,&f0,&f0,&f7,&ff,&ff,&ff,&08,&00,&f4,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&b4,&f0,&f0,&ff,&ff,&ff,&f5,&80,&00,&7c 
defb &f0,&f0,&f0,&f0,&5a,&5a,&5a,&f0,&d2,&f1,&ff,&ff,&fd,&f5,&88,&00 
defb &72,&f0,&f0,&f0,&f0,&a5,&a5,&f0,&ff,&fc,&f7,&ff,&ff,&fe,&f8,&88 
defb &00,&73,&f0,&f0,&f0,&e1,&f0,&f0,&79,&f8,&ff,&ff,&ff,&fa,&f2,&f0 
defb &88,&00,&31,&f8,&f0,&f0,&f0,&f0,&f0,&f3,&f0,&f0,&fb,&ff,&f5,&f0 
defb &f0,&88,&00,&30,&f8,&f0,&f0,&f0,&f0,&f4,&f6,&f0,&f0,&f1,&ff,&f8 
defb &f0,&f0,&88,&00,&10,&f4,&f0,&f0,&f0,&f0,&f3,&f8,&f0,&f0,&f0,&f0 
defb &fc,&f0,&f0,&88,&00,&00,&f3,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &08,&f3,&f0,&f1,&80,&00,&00,&31,&f8,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &e0,&00,&30,&f8,&f0,&00,&00,&00,&10,&fe,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&c0,&00,&00,&f2,&e8,&00,&00,&00,&00,&f7,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f2,&80,&00,&00,&30,&c0,&00,&00,&00,&00,&73,&ff,&f0,&f0,&f0 
defb &f0,&f0,&e8,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&ff,&fc,&f0 
defb &f0,&f0,&ee,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&11,&ff 
defb &ff,&f5,&ff,&ec,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &33,&ff,&ff,&ff,&80,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
;; smiley2

.smiley3
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&73,&e8,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&f6,&f4,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&30,&40,&00,&e0,&10,&80,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&10,&f3,&e8,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&30,&fc,&d4,&03,&0f,&08 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&71,&87,&2d,&0f,&3c 
defb &84,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&01,&69,&0f,&0f,&0f 
defb &78,&0c,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&07,&4b,&0f,&0f 
defb &0f,&69,&0f,&0c,&00,&00,&00,&00,&00,&00,&00,&00,&00,&0f,&0f,&0f 
defb &0f,&0f,&69,&0f,&0f,&00,&00,&00,&00,&00,&00,&00,&00,&07,&0f,&0f 
defb &0f,&0f,&0f,&6d,&0f,&0f,&08,&00,&00,&00,&00,&00,&00,&00,&0f,&0f 
defb &0f,&0f,&f0,&f0,&e1,&0f,&0f,&0c,&00,&00,&00,&00,&00,&00,&40,&0f 
defb &0f,&0f,&1e,&e9,&78,&ed,&0f,&0f,&0e,&00,&00,&00,&00,&00,&00,&0d 
defb &0f,&0f,&0f,&3d,&87,&0f,&6b,&0f,&0f,&0e,&00,&00,&00,&00,&00,&01 
defb &87,&0f,&0f,&0f,&7a,&87,&0f,&69,&0f,&0f,&0f,&00,&00,&00,&00,&00 
defb &12,&0f,&0f,&0f,&0f,&f2,&0f,&0f,&2d,&0f,&0f,&0f,&08,&00,&00,&00 
defb &00,&30,&0f,&0f,&0f,&0f,&e5,&0f,&0f,&0f,&0f,&0f,&0f,&0c,&00,&00 
defb &00,&00,&21,&0f,&0f,&0f,&0f,&e1,&0f,&1e,&c3,&3d,&87,&0f,&0e,&00 
defb &00,&00,&00,&61,&0f,&0f,&0f,&0f,&cb,&0f,&3d,&fe,&3d,&cb,&0f,&0e 
defb &00,&00,&00,&00,&43,&0f,&0f,&0f,&1e,&cb,&0f,&7b,&ff,&3d,&cb,&0f 
defb &0f,&00,&00,&00,&00,&c3,&0f,&0f,&0f,&1e,&c3,&0f,&7b,&ff,&96,&87 
defb &0f,&0f,&08,&00,&00,&00,&c3,&1e,&e1,&0f,&1e,&87,&0f,&79,&ff,&87 
defb &0f,&0f,&0f,&08,&00,&00,&10,&87,&3c,&f0,&0f,&1e,&87,&0f,&3d,&ff 
defb &87,&0f,&0f,&0f,&08,&00,&00,&10,&87,&3c,&f0,&0f,&1e,&c3,&0f,&3d 
defb &f7,&87,&0f,&0f,&0f,&08,&00,&00,&10,&87,&3c,&f0,&0f,&1e,&8f,&0f 
defb &1e,&f0,&1e,&0f,&0f,&0f,&08,&00,&00,&30,&87,&3c,&f0,&0f,&1e,&cb 
defb &0f,&0f,&0f,&1f,&0f,&0f,&0f,&08,&00,&00,&30,&a5,&1e,&f0,&0f,&1e 
defb &cb,&0f,&0f,&0f,&3d,&8f,&0f,&0f,&08,&00,&00,&30,&87,&1e,&e1,&0f 
defb &0f,&c7,&0f,&0f,&0f,&3f,&e5,&0f,&0f,&80,&00,&00,&30,&d2,&0f,&0f 
defb &0f,&0f,&e5,&0f,&0f,&0f,&7a,&f2,&0f,&0f,&80,&00,&00,&30,&a5,&0f 
defb &c3,&0f,&0f,&7a,&0f,&0f,&0f,&7e,&79,&e1,&1e,&80,&00,&00,&32,&d2 
defb &1e,&e1,&0f,&0f,&79,&87,&0f,&0f,&ed,&1e,&ff,&f2,&80,&00,&00,&30 
defb &d2,&0f,&e1,&0f,&0f,&1e,&e9,&0f,&3c,&cb,&0f,&f0,&f8,&80,&00,&00 
defb &32,&e1,&87,&c3,&0f,&1e,&87,&f7,&f0,&f6,&87,&5a,&3c,&78,&80,&00 
defb &00,&30,&e1,&a5,&0f,&0f,&3c,&87,&3c,&fd,&e9,&0f,&0f,&f0,&5a,&80 
defb &00,&00,&32,&f0,&4b,&0f,&0f,&3c,&c3,&0f,&5a,&0f,&1e,&5a,&69,&f0 
defb &80,&00,&00,&11,&f0,&a5,&87,&0f,&3c,&87,&0f,&0f,&0f,&0f,&2d,&f0 
defb &78,&80,&00,&00,&11,&f0,&a5,&0f,&0f,&1e,&0f,&0f,&0f,&0f,&5a,&5a 
defb &78,&f0,&80,&00,&00,&11,&f0,&d2,&4b,&0f,&69,&0f,&0f,&0f,&0f,&0f 
defb &a5,&f0,&78,&80,&00,&00,&11,&f0,&e1,&a5,&0f,&f0,&87,&0f,&0f,&1e 
defb &5a,&5a,&78,&f0,&00,&00,&00,&10,&f8,&f0,&5a,&69,&f0,&87,&0f,&0f 
defb &0f,&2d,&a5,&f0,&f0,&00,&00,&00,&10,&f8,&f0,&a5,&0f,&f0,&c3,&0f 
defb &2d,&a5,&a5,&b4,&78,&f0,&00,&00,&00,&00,&f8,&f0,&d2,&96,&f0,&87 
defb &0f,&87,&0f,&5a,&5a,&f0,&f0,&f0,&80,&00,&00,&f4,&f0,&f0,&d2,&f0 
defb &a5,&87,&5a,&5a,&5a,&f0,&f0,&f7,&ff,&fc,&00,&00,&f4,&f0,&f0,&c3 
defb &f0,&96,&5a,&2d,&a5,&a5,&f0,&f0,&ff,&ff,&ff,&08,&00,&7a,&f0,&f0 
defb &78,&f0,&d2,&5a,&78,&f0,&f0,&f0,&f3,&ff,&ff,&ff,&8c,&00,&72,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&d2,&f0,&f0,&f7,&ff,&ff,&fa,&c8,&00,&36 
defb &f0,&f0,&f0,&f0,&a5,&a5,&a5,&f0,&e1,&f0,&ff,&ff,&fe,&fa,&cc,&00 
defb &31,&f0,&f0,&f0,&f0,&d2,&5a,&78,&f7,&fe,&f3,&ff,&ff,&ff,&f4,&c4 
defb &00,&31,&f8,&f0,&f0,&f0,&78,&f0,&b4,&fc,&f7,&ff,&ff,&fd,&f1,&f0 
defb &c4,&00,&10,&fc,&f0,&f0,&f0,&f0,&f0,&f1,&f8,&f0,&f5,&ff,&fa,&f8 
defb &f0,&c4,&00,&10,&f4,&f0,&f0,&f0,&f0,&f2,&f3,&f0,&f0,&f0,&ff,&fc 
defb &f0,&f0,&c4,&00,&00,&f2,&f0,&f0,&f0,&f0,&f1,&fc,&f0,&f0,&f0,&f0 
defb &f6,&f0,&f0,&c4,&00,&00,&71,&f8,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &84,&71,&f8,&f0,&c8,&00,&00,&10,&fc,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&00,&10,&f4,&f0,&80,&00,&00,&00,&f7,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&e0,&00,&00,&71,&f4,&00,&00,&00,&00,&73,&f8,&f0,&f0,&f0,&f0 
defb &f0,&f1,&c0,&00,&00,&10,&e0,&00,&00,&00,&00,&31,&ff,&f8,&f0,&f0 
defb &f0,&f0,&f4,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&77,&fe,&f0 
defb &f0,&f0,&f7,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&ff 
defb &ff,&fa,&ff,&fe,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &11,&ff,&ff,&ff,&c8,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
;; smiley3

.smiley4
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&31,&fc,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&73,&f2,&80,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&10,&a0,&00,&70,&00,&c0,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&f1,&fc,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&10,&f6,&e2,&81,&0f,&0c 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&30,&cb,&1e,&0f,&1e 
defb &c2,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&3c,&0f,&0f,&0f 
defb &3c,&86,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&03,&2d,&0f,&0f 
defb &0f,&3c,&0f,&0e,&00,&00,&00,&00,&00,&00,&00,&00,&00,&07,&0f,&0f 
defb &0f,&0f,&3c,&0f,&0f,&08,&00,&00,&00,&00,&00,&00,&00,&03,&0f,&0f 
defb &0f,&0f,&0f,&3e,&0f,&0f,&0c,&00,&00,&00,&00,&00,&00,&00,&07,&0f 
defb &0f,&0f,&78,&f0,&f0,&0f,&0f,&0e,&00,&00,&00,&00,&00,&00,&20,&07 
defb &0f,&0f,&0f,&f4,&3c,&f6,&0f,&0f,&0f,&00,&00,&00,&00,&00,&00,&06 
defb &0f,&0f,&0f,&1e,&cb,&0f,&3d,&0f,&0f,&0f,&00,&00,&00,&00,&00,&00 
defb &4b,&0f,&0f,&0f,&3d,&c3,&0f,&3c,&0f,&0f,&0f,&08,&00,&00,&00,&00 
defb &01,&87,&0f,&0f,&0f,&79,&87,&0f,&1e,&0f,&0f,&0f,&0c,&00,&00,&00 
defb &00,&10,&87,&0f,&0f,&0f,&7a,&0f,&0f,&0f,&0f,&0f,&0f,&0e,&00,&00 
defb &00,&00,&10,&0f,&0f,&0f,&0f,&78,&0f,&0f,&e1,&1e,&cb,&0f,&0f,&00 
defb &00,&00,&00,&30,&0f,&0f,&0f,&0f,&6d,&0f,&1e,&ff,&96,&ed,&0f,&0f 
defb &00,&00,&00,&00,&21,&0f,&0f,&0f,&0f,&e5,&0f,&3d,&ff,&9e,&ed,&0f 
defb &0f,&08,&00,&00,&00,&61,&0f,&0f,&0f,&0f,&e1,&0f,&3d,&ff,&cb,&c3 
defb &0f,&0f,&0c,&00,&00,&00,&61,&0f,&f0,&0f,&0f,&c3,&0f,&3c,&ff,&cb 
defb &0f,&0f,&0f,&0c,&00,&00,&00,&c3,&1e,&f0,&87,&0f,&c3,&0f,&1e,&ff 
defb &cb,&0f,&0f,&0f,&0c,&00,&00,&00,&c3,&1e,&f0,&87,&0f,&e1,&0f,&1e 
defb &fb,&cb,&0f,&0f,&0f,&0c,&00,&00,&00,&c3,&1e,&f0,&87,&0f,&c7,&0f 
defb &0f,&f0,&87,&87,&0f,&0f,&0c,&00,&00,&10,&c3,&1e,&f0,&87,&0f,&e5 
defb &0f,&0f,&0f,&0f,&8f,&0f,&0f,&0c,&00,&00,&10,&d2,&0f,&f0,&87,&0f 
defb &e5,&0f,&0f,&0f,&1e,&cf,&0f,&0f,&0c,&00,&00,&10,&c3,&0f,&f0,&0f 
defb &0f,&6b,&0f,&0f,&0f,&1f,&fa,&0f,&0f,&48,&00,&00,&10,&e1,&87,&0f 
defb &0f,&0f,&7a,&0f,&0f,&0f,&3d,&f1,&87,&0f,&48,&00,&00,&10,&d2,&0f 
defb &69,&0f,&0f,&3d,&87,&0f,&0f,&3f,&b4,&f8,&0f,&c0,&00,&00,&11,&e1 
defb &87,&f0,&0f,&0f,&3c,&cb,&0f,&0f,&7e,&0f,&f7,&f9,&c0,&00,&00,&10 
defb &e1,&87,&78,&0f,&0f,&0f,&f4,&0f,&1e,&e5,&0f,&78,&f4,&c0,&00,&00 
defb &11,&f0,&4b,&69,&0f,&0f,&c3,&7b,&f8,&f3,&c3,&2d,&96,&b4,&c0,&00 
defb &00,&10,&f0,&5a,&0f,&0f,&1e,&c3,&1e,&f6,&fc,&0f,&0f,&78,&a5,&c0 
defb &00,&00,&11,&f0,&a5,&0f,&0f,&1e,&e1,&0f,&2d,&87,&0f,&a5,&b4,&78 
defb &c0,&00,&00,&00,&f8,&d2,&4b,&0f,&1e,&c3,&0f,&0f,&0f,&0f,&1e,&78 
defb &b4,&c0,&00,&00,&00,&f8,&d2,&0f,&0f,&0f,&87,&0f,&0f,&0f,&2d,&a5 
defb &b4,&f0,&c0,&00,&00,&00,&f8,&e1,&a5,&0f,&3c,&0f,&0f,&0f,&0f,&0f 
defb &5a,&78,&b4,&c0,&00,&00,&00,&f8,&f0,&5a,&0f,&78,&c3,&0f,&0f,&0f 
defb &a5,&a5,&b4,&f0,&80,&00,&00,&00,&f4,&f0,&a5,&b4,&78,&c3,&0f,&0f 
defb &0f,&1e,&5a,&78,&f0,&80,&00,&00,&00,&f4,&f0,&d2,&0f,&78,&e1,&0f 
defb &1e,&5a,&5a,&5a,&b4,&f0,&80,&00,&00,&00,&74,&f0,&e1,&c3,&f0,&c3 
defb &0f,&4b,&0f,&2d,&a5,&f0,&f0,&f0,&c0,&00,&00,&72,&f0,&f0,&e1,&f0 
defb &d2,&4b,&2d,&a5,&a5,&f0,&f0,&f3,&ff,&fe,&80,&00,&72,&f0,&f0,&e1 
defb &78,&c3,&a5,&96,&5a,&5a,&78,&f0,&f7,&ff,&ff,&8c,&00,&35,&f0,&f0 
defb &b4,&f0,&e1,&a5,&b4,&f0,&f0,&f0,&f1,&ff,&ff,&ff,&ce,&00,&31,&f0 
defb &f0,&f0,&f0,&f0,&f0,&f0,&e1,&f0,&f0,&f3,&ff,&ff,&fd,&e4,&00,&13 
defb &f0,&f0,&f0,&f0,&d2,&5a,&5a,&78,&f0,&78,&f7,&ff,&ff,&f5,&e6,&00 
defb &10,&f8,&f0,&f0,&f0,&e1,&a5,&b4,&f3,&ff,&f1,&ff,&ff,&ff,&fa,&e2 
defb &00,&10,&fc,&f0,&f0,&f0,&b4,&f0,&d2,&f6,&f3,&ff,&ff,&fe,&f8,&f8 
defb &e2,&00,&00,&f6,&f0,&f0,&f0,&f0,&f0,&f0,&fc,&f0,&f2,&ff,&fd,&f4 
defb &f0,&e2,&00,&00,&f2,&f0,&f0,&f0,&f0,&f1,&f1,&f8,&f0,&f0,&f7,&fe 
defb &f0,&f0,&e2,&00,&00,&71,&f0,&f0,&f0,&f0,&f0,&fe,&f0,&f0,&f0,&f0 
defb &f3,&f0,&f0,&e2,&00,&00,&30,&fc,&f0,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &c2,&30,&fc,&f0,&e4,&00,&00,&00,&f6,&f0,&f0,&f0,&f0,&f0,&f0,&f0 
defb &f0,&80,&00,&f2,&f0,&c0,&00,&00,&00,&73,&f8,&f0,&f0,&f0,&f0,&f0 
defb &f0,&f0,&00,&00,&30,&fa,&80,&00,&00,&00,&31,&fc,&f0,&f0,&f0,&f0 
defb &f0,&f0,&e8,&00,&00,&00,&f0,&00,&00,&00,&00,&10,&ff,&fc,&f0,&f0 
defb &f0,&f0,&f2,&80,&00,&00,&00,&00,&00,&00,&00,&00,&00,&33,&ff,&f0 
defb &f0,&f0,&f3,&88,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&77 
defb &ff,&fd,&f7,&ff,&80,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&ff,&ff,&ff,&ec,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00 
defb &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
;; smiley4
