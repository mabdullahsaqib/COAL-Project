[org 0x0100]
jmp start

;bp+6 x
;bp+4 y
DrawPoint:
		push bp
		mov bp,sp
		pusha
		push es
		xor dx,dx
		mov ax,[bp+4]
		mov cx,80
		mul cx
		add ax,[bp+6]
		shl ax,1
		mov di,ax
		mov ax,0xb800
		mov es,ax
		mov ax,0x0720
		mov al,'*'
		mov [es:di],ax
		pop es
		popa
		pop bp
		ret 4
LineDraw:
		push bp
		mov bp,sp
		sub sp,4
		pusha
		mov ax,[x2]
		sub ax,[x1]
		mov bx,[y2]
		sub bx,[y1]
		cmp bh,0xFF
		jne skipneg
		neg bx
		skipneg:
		cmp ax,bx
		jnc LineX
LineY:
		
		jmp LineDrawEnd
LineX:
		push ax
		xor dx,dx
		div bx
		mov cx,ax
		pop ax
		mov dx,[x1]
		mov [bp-2],dx
		mov dx,[y1]
		mov [bp-4],dx
		push cx
		linedrawloopx:
					push word[bp-2]
					push word[bp-4]
					call DrawPoint
					inc word[bp-2]
					sub cx,1
					jnz linedrawincskip
					inc word[bp-4]
					pop cx
					push cx
					linedrawincskip:
					mov dx,[x2]
					cmp dx,[bp-2]
					jnc linedrawloopx
		LineDrawEnd:
		pop cx
		popa
		add sp,4
		pop bp
		ret


clrscr:
		push cx
		push di
		push es
		xor di,di
		mov ax,0xb800
		mov es,ax
		clrscrloop:
				  mov word[es:di],0x0020
				  add di,2
				  cmp di,4000
				  jc clrscrloop
		pop es
		pop di
		pop cx
		ret
start:
call clrscr
call LineDraw



mov ax,0x4c00
int 0x21

x1:dw 5
y1:dw 5

x2:dw 35
y2:dw 10