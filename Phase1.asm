
[org 0x0100]
jmp start

message: db '@' ; string to be printed
multiplier: dw 2
;--------------------------------------------------------------------
; subroutine to clear the screen
;--------------------------------------------------------------------
clrscr:		push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column
			mov ah, 0x07
			mov al, 0x20

nextloc:	mov word [es:di], ax	; clear next char on screen
			;inc AL
			add di, 2					; move to next screen location
			;cmp di, 11352				; 132x43x2
			cmp di, 10
			jne nextloc					; if no clear next position

			pop di
			pop ax
			pop es
			ret
;--------------------------------------------------------------------
; subroutine to print a side of the square
;--------------------------------------------------------------------

face:	push bp
		mov bp, sp
		push es
		push ax
		push cx
		push si
		push di
		push bx
		push dx
		
		
		mov dx, 6
		
		mov cx, 0xb800
		mov es, cx				; point es to video base
		;mov bx, [bp+4]			; controls the addition into di			
		mov bx, 0
		mov di, [bp+6]			; point di to the starting column	
		mov ax, [bp+8]			; loop control variable
		mov si, [bp+10]			; point si to string
		
		mov ch, 0x07	        ; normal attribute fixed in ah
		mov cl, [si]	        ; load value in al

next:
mov ax, bx
add ax,4
mov bx, ax
sub di, ax
sub di, ax
add di,268
;sub di, 10

nextchar:				
		mov [es:di], cx			; show this char on screen
		add di, 2				; move to next screen location	
		sub ax,1
		jnz nextchar			; repeat the operation cx times
		sub dx, 1
		jnz next
		
		pop dx
		pop bx
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 8
;--------------------------------------------------------------------
;subroutine to print a square on the screen
;--------------------------------------------------------------------
horse:
		mov ax, message
		mov bx,0               ;move dimensions of a side in bx
		
		push ax					; push address of message.. [bp+6]
		push bx
		
		mov di,4920
		push di
		mov  dx, 2
		push dx
		call face			; call the printstr subroutine
			 
		ret

;--------------------------------------------------------------------
start:	
mov AH,0x00
mov al, 0x54
int 0x10

;mov Ax,0x1003
;mov bl, 0x00
;int 0x10

call clrscr ; call the clrscr subroutine

call horse
       
		mov ax, 0x4c00 ; terminate program
		int 0x21