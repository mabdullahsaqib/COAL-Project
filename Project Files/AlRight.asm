[org 0x0100]
jmp start
AlignRight:
			push bp
			sub sp,2
			mov word[bp-2],14
			pusha
			push es
			push ds
			mov ax,[bp-2]
			xor dx,dx
			mov cx,80
			mul cx,
			add ax,79
			shl ax,1
			mov cx,0xb800
			mov es,cx
			mov di,ax
			std
			mov ax,0x0720
			push di
			scanl:
				  scasw
				  jne scanl
				  pop cx
				  push cx
				  sub ax,di
				  mov si,di
				  pop di
				  mov ax,0xb800
				  mov ds,ax
				  mov bx,80
				  sub bx,cx
				  mov cx,bx
				  rep movsw
				  shl bx,1
				  sub si,bx
				  sub di,bx
				  shr bx,1
				  mov ax,0x0020
				  mov cx,bx
				  cld
				  rep stosw
				  pop ds
				  pop es
				  popa
				  add sp,2
				  pop bp
				  ret

start:
	call AlignRight
mov ax,0x4c00
int 0x21