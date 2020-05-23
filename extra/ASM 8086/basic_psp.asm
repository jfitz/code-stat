This program is used to set the PSP address for a compiled BASIC program
The PSP segment is saved at 0:4F2H.


extrn	$$main:far
cseg	segment para public 'code'


; It can also be used to limit the maximum memory available to a compiled
; BASIC program. The option '/M:nnn' is used on the command line, where
; nnn is the number of K-bytes the program is limited to. If no, /M option
; is specified, no memory limitation takes place. For example, '/M:64' would
; limit the program to 64*1024 bytes. The range for nnn is 64 to 1024.

; This routine gets control before BASIC, does its handiwork, and then
; passes control to the BASIC program. It must be linked as follows:
; LINK BASMAIN+yourprog,yourprog,NUL.MAP,BASCOM

; If BASMAIN is unable to limit memory as requested, a message is displayed
; and the execution of the program is continued.

public	basmain
basmain proc far
	assume cs:cseg,ds:cseg,ss:nothing,es:nothing

	push ds 		; save ds
	xor ax,ax
	mov ds,ax		; ds=0
	mov si,4f2h		; dos communications area
	mov ax,es		; get psp seg
	mov [si],ax		; save psp in dos comm area
	pop ds			; restore ds
	mov si,80h		; point to command line
	mov ch,0
	mov cl,[si]		; get length of command line
	jcxz p025		; it's zero

p010:	inc si
	mov al,[si]		; get char from command line
	cmp al,'/'              ; is it a slash?
	jnz p020		; no
	mov ax,[si+1]		; get next 2 chars
	cmp ax,':M'             ; is it M: ?
	jz p030 		; yes
	cmp ax,':m'             ; is it m: ?
	jz p030 		; yes

p020:	loop p010		; check next char
p025:	jmp p080		; no /m: or /M: found

p030:				; found /m: or /M:
	add si,3		; point to first number
	mov ax,0
	mov bx,0
	mov cx,10
p040:	mov bl,[si]		; get character
	cmp bl,'0'              ; out of range?
	jb p050 		; yes
	cmp bl,'9'              ; out of range?
	ja p050 		; yes
	sub bl,'0'              ; convert to binary
	mul cx			; multiply ax by 10
	add ax,bx		; add new digit
	inc si			; point to next char
	jmp p040		; continue

p050:				; got value in ax
	cmp ax,64		; less than 64K?
	jb p060 		; yes - print msg
	cmp ax,1024		; greater than 1024K?
	ja p060 		; yes - print msg
	mov cl,6
	sal ax,cl		; convert from KB to paragraphs (*64)
	mov bx,es		; get psp
	add bx,ax		; new top of memory
	mov si,2		; point to top of memory in psp
	mov ax,[si]		; get current top of memory
	cmp ax,bx		; is new setting larger?
	jae p055		; no
	mov dx,offset msg2	; yes - print msg
	jmp p065

p055:	mov [si],bx		; save new top of memory
	jmp p080

p060:	mov dx,offset msg1	; print the message
p065:	add dx,100h		; fudge for the psp
	mov ah,9
	int 21h

p080:	jmp $$main		; jump to BASIC's start point

msg1	db 'Memory specification must be from 64 to 1024',7,10,13,'$'
msg2	db 'Unable to limit memory',7,10,13,'$'

basmain endp
cseg	ends
end	basmain 		; must be a main program!
