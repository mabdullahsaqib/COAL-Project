[org 0x0100]
jmp start
RotateScreenRight:
	push bp
	mov bp,sp
	sub sp,52
	pusha
	push es
	push ds
	mov ax,[bp+6]
	xor dx,dx
	mov cx,80
	mul cx
	add ax,[bp+4]
	shl ax,1
	mov word[bp-52],ax
	mov di,bp
	sub di,50
	mov ax,ds
	mov es,ax
	mov ax,0xb800
	mov ds,ax
	mov bx,5
	scanloop:
		mov cx,5
		mov si,[bp-52]
		add si,640
		scanloopinner:
					movsw
					sub si,162
					loop scanloopinner
		add word[bp-52],2
		sub bx,1
		jnz scanloop
		mov si,bp
		sub si,50
		mov di,[bp-52]
		sub di,10
		mov ax,es
		mov ds,ax
		mov bx,5
		mov ax,0xb800
		mov es,ax
		printloop:
				mov cx,5
				rep movsw
				add di,150
				sub bx,1
				jnz printloop
		pop ds
		pop es
		popa
		mov sp,bp
		pop bp
		ret 4
clrscr:
		pusha
		push es
		mov ax,0xb800
		mov es,ax
		xor di,di
		mov ah,0x07
		mov al,'A'
		clrscrloop:
				mov [es:di],ax
				add di,2
				add al,1
				cmp al,'F'
				jne clrscrskip
				mov al,'A'
				clrscrskip:
				cmp di,4000
				jne clrscrloop
		pop es
		popa
		ret
start:
		call clrscr
		mov al,0
		int 0x16
		push 2
		push 3
		call RotateScreenRight
mov ax,0x4c00
int 0x21