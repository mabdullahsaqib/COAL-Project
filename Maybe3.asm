[org 0x0100]
jmp start
rabbit : db '@@@@=-:---.@@@@@@@@:---:-=]@@@@@-=.::.-+@@@@@@+:.::.+:]@@@@@@.=:==--.....:--=-:+.]@@@@@@.=-@@..@@@@@@..@.+-]@@@.:==@@@+*=+@@@@*=*-@@.*::]@@::++--.@@:.@+**+@::@@:--*:-.]@@:::+:-@@@@@.--++:-@@@@@.-=+]@+:@@@@.@@:.@@@@@@@@@@.:@@.=-]@@--::+:+-:==-@@@@@@-==:++-+]',0

ResX: dw 132
ResY: dw 43
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;print takes 5 parameters
;1.pointer to the array containing the formatted shape bp+4
;3.x displacement bp+6
;4.y displacement bp+8          
;6.color bp+10     				
print:
		    push bp
			mov bp,sp
			sub sp,6
			push si
			push di
			push dx
			push cx
			push bx
			push ax
			mov byte bh,[bp+10]
			mov si,0
			mov di,0
			mov word [bp-2],0  ;i
			mov word [bp-4],0  ;j
			;------------- find length of string
			push ds
			pop es
			mov di,[bp+4]
			mov cx,0xffff
			xor al,al
			repne scasb
			mov ax,0xffff
			sub ax,cx
			dec ax
			mov [bp-6],ax
			;------------------
			mov ax,0xb800
			mov es,ax
			mov di,0
			printloop:
				cmp word [bp-6],si
				jna printloopend
				mov di,[bp+4]
				add di,si
				mov bl,[di]  
				cmp bl,']'
				jne printloopskip
					add word [bp-2],1  ;move to next row
					mov word [bp-4],0 ;move to start of column
					add si,1
					jmp printloop
				printloopskip:
					cmp bl,'@'
					je printloopskip2
						mov ax,[bp-2]
						add ax,[bp+8]
						mov cx,[bp-4]
						add cx,[bp+6]
						mov dx,132
						mul dx
						add ax,cx
						shl ax,1
						mov di,ax
						mov word [es:di],bx
				printloopskip2:
					add word [bp-4],1
					add si,1
					cmp di,8000
				jne printloop
			printloopend:
			pop ax
			pop bx
			pop cx
			pop dx
			pop di
			pop si
			add sp,6
			pop bp
		    ret 8
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
delay:
		push cx
		mov cx,0xFFFF
		delayloop:
				 sub cx,1
				 cmp cx,0
				 jne delayloop
	     pop cx
		 ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
clrscr:
		push di
		push ax
		push cx
		push es
		mov ax,0xb800
		mov es,ax
		xor di,di
		mov ax,0x0720
		mov cx,6000
		cld
		rep stosw
		pop es
		pop cx
		pop ax
		pop di
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rabbitposx: dw 45
rabbitposy: dw 0
printrabbit:
		    push bp
			mov bp,sp
			push bx
			mov bx, [bp+4]
			mov [rabbitposy], bx
			pop bx
			pop bp
			
		push byte 0b01110111
		push word [rabbitposy]
		push word [rabbitposx]
		push word rabbit
		call print
		ret 2

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kbisr:		push ax
			push es
	
			mov ax, 0xb800
			mov es, ax						; point es to video memory


			in al, 0x60						; read a char from keyboard port, scancode
	
			cmp al, 0x48					; is the key left shift
			jne nomatch						; leave interrupt routine
            call clrscr
			mov ax, [rabbitposy]
            sub ax , 3
            push ax
            call printrabbit
            
nomatch:		mov al, 0x20
			out 0x20, al					; send EOI to PIC
			
			pop es
			pop ax

			;************************************
			; TEST YOUR CONCEPTS
			; WHAT IS EXACT FUNCTIONALITY OF IRET????

			iret
;-------------------------------------------------------


start:
	mov AH,0x00
	mov al, 0x54
	int 0x10

		mov ax,0xb800
		mov es,ax
		call clrscr
		mov bx, 30
		push bx
		call printrabbit

			xor ax, ax
			mov es, ax					; es=0, point es to IVT base
	
			cli						; disable interrupts

			mov word [es:9*4], kbisr		; store offset at n*4....... csabc:kbisr	
			mov [es:9*4+2], cs			; store segment at n*4+2

			sti						; enable interrupts 

l1:		 	jmp l1						; infinite loop 

		
mov ax,0x4c00
int 0x21

