[org 0x0100]

jmp start
RotateScreenRight:
			push bp
			mov bp,sp
			sub sp,4
			pusha
			push es
			push ds
			mov ax,0xb800
			mov es,ax
			mov ds,ax
			mov word[bp-2],0
			cld
			mov ax,80
			mul word[bp+6]
			add ax,word[bp+4]
			shl ax,1
			mov di,ax
			mov si,ax
			push di
			push si
			transposeloop1:
							mov ax,[bp-2]
							mov [bp-4],ax
							push si
							push di
							transposeloop2:
										  add si,2
										  add di,160
										  lodsw
										  sub si,2
										  movsw
										  sub si,2
										  sub di,2
										  push di
										  mov di,si
										  stosw
										  pop di
										  add word[bp-4],1
										  cmp word[bp-4],4
										  jnz transposeloop2
							pop di
							pop si
							add di,162
							add si,162
							add word[bp-2],1
							cmp word[bp-2],4
							jnz transposeloop1
				pop si
				pop di
				mov word[bp-2],0
				shiftloop1:
							push si
							push di
							mov word[bp-4],0
							shiftloop2:
										lodsw
										sub si,2
										movsw
										sub di,2
										sub si,2
										push di
										mov di,si
										stosw
										pop di
										add si,160
										add di,160
										add word[bp-4],1
										cmp word[bp-4],5
										jnz shiftloop2
							pop di
							pop si
							add di,2
							sub si,2
							add word[bp-2],1
							cmp word[bp-2],2
							jnz shiftloop1
			pop ds
			pop es
			popa
			add sp,4
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
				cmp al,'E'
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
		push 2
		push 3
		call RotateScreenRight
		
		
		mov ax,0x4c00
		int 0x21