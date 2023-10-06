; hello world in assembly
[org 0x0100]
jmp start

message: db 'hello world' ; string to be printed
length: dw 11 ; length of the string
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
			cmp di, 11352				; 132x43x2
			jne nextloc					; if no clear next position

			pop di
			pop ax
			pop es
			ret
;--------------------------------------------------------------------
; subroutine to print a string at top left of screen
; takes address of string and its length as parameters
;--------------------------------------------------------------------
printstr:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di

			mov ax, 0xb800
			mov es, ax				; point es to video base
			mov di, 4780			; point di to top left column
									; es:di --> b800:0000
			mov si, [bp+6]			; point si to string
			mov cx, [bp+4]			; load length of string in cx
			mov ah, 0x43			; normal attribute fixed in al
			
nextchar:	mov al, [si]			; load next char of string
			mov [es:di], ax			; show this char on screen
			add di, 2				; move to next screen location
			add si, 1				; move to next char in string			
			loop nextchar			; repeat the operation cx times
			
			pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp
			ret 4

;--------------------------------------------------------------------
start:	
mov AH,0x00
mov al, 0x54
int 0x10

;mov Ax,0x1003
;mov bl, 0x00
;int 0x10

call clrscr ; call the clrscr subroutine

		mov ax, message
		push ax					; push address of message.. [bp+6]
		push word [length]		; push message length .... [bp+4]
		call printstr			; call the printstr subroutine

		mov ax, 0x4c00 ; terminate program
		int 0x21