SetVSRAMWrite: macro addr
	move.l  #(vdp_cmd_vsram_write)|((\addr)&$3FFF)<<16|(\addr)>>14, vdp_control
	endm

; Writes a sprite attribute structure to 4 registers, ready to write to VRAM
BuildSpriteStructure: macro x_pos,	; X pos on sprite plane
	y_pos,							; Y pos on sprite plane
	dimension_bits,					; Sprite tile dimensions (4 bits)
	next_id,						; Next sprite index in linked list
	priority_bit,					; Draw priority
	palette_id,						; Palette index
	flip_x,							; Flip horizontally
	flip_y,							; Flip vertically
	tile_id,						; First tile index
	reg1,							; Output: reg1
	reg2,							; Output: reg2
	reg3,							; Output: reg3
	reg4							; Output: reg4

