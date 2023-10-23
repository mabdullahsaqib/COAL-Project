[org 0x0100]

jmp start
; array

;car bottoms
row: dw 14
col: dw 33
color: dw 0x40
width: dw 6
height: dw 2

; car head
length: dw 3
carTopRow: dw 12
carTopCol: dw 36
carTopColor: dw 0x40
carTopHeight: dw 2

; car window
winlength: dw 2
carwRow: dw 13
carwCol: dw 37
carwColor: dw 0x0
carwHeight: dw 1

;parameters of tyre 1
t1length: dw 1
t1row: dw 16
t1col: dw 34
t1color: dw 0x70

;parameters of tyre 2
t2length: dw 1
t2row: dw 16
t2col: dw 42
t2color: dw 0x70

; road lines
roadlines1length: dw 6
roadlines1Row: dw 13
roadlines1Col: dw 10
roadlines1Color: dw 0xFF
roadlines1Height: dw 1

; road lines
roadlines2length: dw 6
roadlines2Row: dw 13
roadlines2Col: dw 60
roadlines2Color: dw 0xFF
roadlines2Height: dw 1

;Parameters of Building1
b1length: dw 5
b1height: dw 9
b1row: dw 1
b1col: dw 1
b1color: dw 0x70

;Parameters of Building2
b2length: dw 5
b2height: dw 7
b2row: dw 3
b2col: dw 12
b2color: dw 0x70

;Parameters of Building3
b3length: dw 4
b3height: dw 8
b3row: dw 2
b3col: dw 23
b3color: dw 0x70

;Parameters of Building4
b4length: dw 6
b4height: dw 6
b4row: dw 4
b4col: dw 32
b4color: dw 0x70

;Parameters of Building5
b5length: dw 6
b5height: dw 7
b5row: dw 3
b5col: dw 45
b5color: dw 0x70

;Parameters of Building6
b6length: dw 5
b6height: dw 8
b6row: dw 2
b6col: dw 58
b6color: dw 0x70

;Parameters of Building7
b7length: dw 5
b7height: dw 6
b7row: dw 4
b7col: dw 69
b7color: dw 0x70

;parameters of window
wlength: dw 1
wrow: dw 2
wcol: dw 2
wcolor: dw 0x5A

;parameters of window2
w2length: dw 1
w2row: dw 4
w2col: dw 13
w2color: dw 0x6A

;parameters of window3
w3length: dw 1
w3row: dw 3
w3col: dw 24
w3color: dw 0x2A

;parameters of window4
w4length: dw 1
w4row: dw 5
w4col: dw 33
w4color: dw 0x3B

;parameters of window5
w5length: dw 1
w5row: dw 4
w5col: dw 46
w5color: dw 0xDA

;parameters of window6
w6length: dw 1
w6row: dw 3
w6col: dw 59
w6color: dw 0x00

;parameters of window7
w7length: dw 1
w7row: dw 5
w7col: dw 70
w7color: dw 0x2A

clrscr:
	push ax
	push es
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 0
	clrloop:
		mov word[es:di], 0x0020
		add di, 2
		cmp di, 4000
		jne clrloop
	pop di
	pop es
	pop ax
	ret

delay:
	push cx
	mov cx, 0xFFFF
	dloop1:
		  loop dloop1
	mov cx, 0xFFFF
	dloop2:
		  loop dloop2
	pop cx
	ret
	
screendivider1:
	push ax
	push es
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 1280
	loop:
		mov word[es:di], 0x075F
		add di, 2
		cmp di, 1440
		jne loop
	pop di
	pop es
	pop ax
	ret
screendivider2:
	push ax
	push es
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 2560
	loop1:
		mov word[es:di], 0x075F
		add di, 2
		cmp di, 2720
		jne loop1
	pop di
	pop es
	pop ax
	ret
printSky:
	push ax
	push es
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 0
	sloop:
		mov word[es:di], 0x1020
		add di, 2
		cmp di, 1600
		jne sloop
	pop di
	pop es
	pop ax
	ret

printroad:
	push ax
	push es
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 1600
	rloop:
		mov word[es:di], 0x0020
		add di, 2
		cmp di, 2720
		jne rloop
	bloop:
		mov word[es:di], 0x075F
		add di, 2
		cmp di, 2880
		jne bloop
	pop di
	pop es
	pop ax
	ret
Stars:
	push ax
	push es
	push di
	mov ax, 0xb800
	mov es, ax
	mov di, 0
	loope:
		mov word[es:di], 0x1E2A
		add di, 8
		cmp di, 160
		jne loope
	mov di, 320
	loop2:
		mov word[es:di], 0x1E2A
		add di, 8
		cmp di, 480
		jne loop2
	mov di, 640
	loop3:
		mov word[es:di], 0x1E2A
		add di, 8
		cmp di, 800
		jne loop3
	mov di, 960
	pop di
	pop es
	pop ax
	ret

printSquare:
	push bp
	mov bp, sp
	push ax
	push es
	push di
	push cx
	push dx
	push bx
	
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte[bp + 8]
	add ax, [bp + 6]
	shl ax, 1
	mov di, ax
	mov cx, [bp + 4]
	mov dx, [bp + 4]
	mov bx, [bp + 4]
	shl bx, 2
	shl cx, 1
	mov ah, [bp + 10]
	mov al, 0x20
	outerloop:
		innerloop:
			mov word[es:di], ax
			add di, 2
			sub cx, 1
			jnz innerloop
			
		add di, 160
		mov cx, [bp + 4]
		shl cx, 1
		sub di, bx
		sub dx, 1
		jnz outerloop
		
	pop bx
	pop dx	
	pop cx
	pop di
	pop es
	pop ax
	pop bp
	ret 8

printRectangle:
	push bp
	mov bp, sp
	push ax
	push es
	push di
	push cx
	push dx
	push bx
	
	mov ax, 0xb800
	mov es, ax
	mov al, 80
	mul byte[bp + 8]
	add ax, [bp + 6]
	shl ax, 1
	mov di, ax
	mov cx, [bp + 4]
	mov dx, [bp + 12]
	mov bx, [bp + 4]
	shl bx, 2
	shl cx, 1
	mov ah, [bp + 10]
	mov al, 0x20
	outerlooprect:
		innerlooprect:
			mov word[es:di], ax
			add di, 2
			sub cx, 1
			jnz innerlooprect
			
		add di, 160
		mov cx, [bp + 4]
		shl cx, 1
		sub di, bx
		sub dx, 1
		jnz outerlooprect
	mov si, di
	
	pop bx
	pop dx	
	pop cx
	pop di
	pop es
	pop ax
	pop bp
	ret 10

printCAR:
	push ax
	mov ax, [carTopHeight]; bp + 12
	push ax
	mov ax, [carTopColor]; bp + 10
	push ax 
	mov ax, [carTopRow]; bp + 8
	push ax
	mov ax, [carTopCol]; bp + 6
	push ax
	push word[length]; bp + 4
	call printRectangle
	
	mov ax, [height]; bp + 12
	push ax
	mov ax, [color]; bp + 10
	push ax 
	mov ax, [row]; bp + 8
	push ax
	mov ax, [col]; bp + 6
	push ax
	push word[width]; bp + 4
	call printRectangle
	
	mov ax, [carwHeight]; bp + 12
	push ax
	mov ax, [carwColor]; bp + 10
	push ax 
	mov ax, [carwRow]; bp + 8
	push ax
	mov ax, [carwCol]; bp + 6
	push ax
	push word[winlength]; bp + 4
	call printRectangle
	
	
	mov ax, [t1color];bp + 10
	push ax 
  	mov ax, [t1row]; bp + 8
	push ax
	mov ax, [t1col]; bp + 6
	push ax
	mov ax, [t1length]
	push ax; bp + 4
	call printSquare
	

	mov ax, [t2color];bp + 10
	push ax 
  	mov ax, [t2row]; bp + 8
	push ax
	mov ax, [t2col]; bp + 6
	push ax
	mov ax, [t2length]
	push ax; bp + 4
	call printSquare
	
	pop ax
	ret

Roadlines1:
    push ax
	mov ax, [roadlines1Height]; bp + 12
	push ax
	mov ax, [roadlines1Color]; bp + 10
	push ax 
	mov ax, [roadlines1Row]; bp + 8
	push ax
	mov ax, [roadlines1Col]; bp + 6
	push ax
	push word[roadlines1length]; bp + 4
	call printRectangle
	
	pop ax
	ret
	 
Roadlines2:
       push ax
	mov ax, [roadlines2Height]; bp + 12
	push ax
	mov ax, [roadlines2Color]; bp + 10
	push ax 
	mov ax, [roadlines2Row]; bp + 8
	push ax
	mov ax, [roadlines2Col]; bp + 6
	push ax
	push word[roadlines2length]; bp + 4
	call printRectangle
	
	pop ax
	ret

Building1:
	push ax
	mov ax, [b1height]
	push ax
	mov ax, [b1color];bp + 10
	push ax 
    mov ax, [b1row]; bp + 8
	push ax
	mov ax, [b1col]; bp + 6
	push ax
	mov ax, [b1length]
	push ax; bp + 4
	call printRectangle
    
	;window1
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	
	;window2
	mov ax, [wcol]
	add ax, 6
	mov [wcol], ax
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	
	;window3
	mov ax, [wcol]
	sub ax, 6
	mov [wcol], ax
	mov ax, [wrow]
	add ax, 2
	mov [wrow], ax
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	
	;window4
	mov ax, [wcol]
	add ax, 6
	mov [wcol], ax
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	
	;window5
	mov ax, [wcol]
	sub ax, 6
	mov [wcol], ax
	mov ax, [wrow]
	add ax, 2
	mov [wrow], ax
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	
	;window6
	mov ax, [wcol]
	add ax, 6
	mov [wcol], ax
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	
	;window7
	mov ax, [wcol]
	sub ax, 6
	mov [wcol], ax
	mov ax, [wrow]
	add ax, 2
	mov [wrow], ax
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	
	;window8
	mov ax, [wcol]
	add ax, 6
	mov [wcol], ax
	mov ax, [wcolor];bp + 10
	push ax 
  	mov ax, [wrow]; bp + 8
	push ax
	mov ax, [wcol]; bp + 6
	push ax
	mov ax, [wlength]
	push ax; bp + 4
	call printSquare
	pop ax
	ret	

Building2:
	push ax
	mov ax, [b2height]
	push ax
	mov ax, [b2color];bp + 10
	push ax 
    mov ax, [b2row]; bp + 8
	push ax
	mov ax, [b2col]; bp + 6
	push ax
	mov ax, [b2length]
	push ax; bp + 4
	call printRectangle	
	
	;window1
	mov ax, [w2color];bp + 10
	push ax 
  	mov ax, [w2row]; bp + 8
	push ax
	mov ax, [w2col]; bp + 6
	push ax
	mov ax, [w2length]
	push ax; bp + 4
	call printSquare
	
	;window2
	mov ax, [w2col]
	add ax, 6
	mov [w2col], ax
	mov ax, [w2color];bp + 10
	push ax 
  	mov ax, [w2row]; bp + 8
	push ax
	mov ax, [w2col]; bp + 6
	push ax
	mov ax, [w2length]
	push ax; bp + 4
	call printSquare
	
	;window3
	mov ax, [w2col]
	sub ax, 6
	mov [w2col], ax
	mov ax, [w2row]
	add ax, 2
	mov [w2row], ax
	mov ax, [w2color];bp + 10
	push ax 
  	mov ax, [w2row]; bp + 8
	push ax
	mov ax, [w2col]; bp + 6
	push ax
	mov ax, [w2length]
	push ax; bp + 4
	call printSquare
	
	;window4
	mov ax, [w2col]
	add ax, 6
	mov [w2col], ax
	mov ax, [w2color];bp + 10
	push ax 
  	mov ax, [w2row]; bp + 8
	push ax
	mov ax, [w2col]; bp + 6
	push ax
	mov ax, [w2length]
	push ax; bp + 4
	call printSquare
	
	;window5
	mov ax, [w2col]
	sub ax, 6
	mov [w2col], ax
	mov ax, [w2row]
	add ax, 2
	mov [w2row], ax
	mov ax, [w2color];bp + 10
	push ax 
  	mov ax, [w2row]; bp + 8
	push ax
	mov ax, [w2col]; bp + 6
	push ax
	mov ax, [w2length]
	push ax; bp + 4
	call printSquare
	
	;window6
	mov ax, [w2col]
	add ax, 6
	mov [w2col], ax
	mov ax, [w2color];bp + 10
	push ax 
  	mov ax, [w2row]; bp + 8
	push ax
	mov ax, [w2col]; bp + 6
	push ax
	mov ax, [w2length]
	push ax; bp + 4
	call printSquare
	
	pop ax
	ret
	
Building3:
	push ax
	mov ax, [b3height]
	push ax
	mov ax, [b3color];bp + 10
	push ax 
    mov ax, [b3row]; bp + 8
	push ax
	mov ax, [b3col]; bp + 6
	push ax
	mov ax, [b3length]
	push ax; bp + 4
	call printRectangle
    
	;window1
	mov ax, [w3color];bp + 10
	push ax 
  	mov ax, [w3row]; bp + 8
	push ax
	mov ax, [w3col]; bp + 6
	push ax
	mov ax, [w3length]
	push ax; bp + 4
	call printSquare
	
	;window2
	mov ax, [w3col]
	add ax, 4
	mov [w3col], ax
	mov ax, [w3color];bp + 10
	push ax 
  	mov ax, [w3row]; bp + 8
	push ax
	mov ax, [w3col]; bp + 6
	push ax
	mov ax, [w3length]
	push ax; bp + 4
	call printSquare
	
	;window3
	mov ax, [w3col]
	sub ax, 4
	mov [w3col], ax
	mov ax, [w3row]
	add ax, 2
	mov [w3row], ax
	mov ax, [w3color];bp + 10
	push ax 
  	mov ax, [w3row]; bp + 8
	push ax
	mov ax, [w3col]; bp + 6
	push ax
	mov ax, [w3length]
	push ax; bp + 4
	call printSquare
	
	;window4
	mov ax, [w3col]
	add ax, 4
	mov [w3col], ax
	mov ax, [w3color];bp + 10
	push ax 
  	mov ax, [w3row]; bp + 8
	push ax
	mov ax, [w3col]; bp + 6
	push ax
	mov ax, [w3length]
	push ax; bp + 4
	call printSquare
	
	;window5
	mov ax, [w3col]
	sub ax, 4
	mov [w3col], ax
	mov ax, [w3row]
	add ax, 2
	mov [w3row], ax
	mov ax, [w3color];bp + 10
	push ax 
  	mov ax, [w3row]; bp + 8
	push ax
	mov ax, [w3col]; bp + 6
	push ax
	mov ax, [w3length]
	push ax; bp + 4
	call printSquare
	
	;window6
	mov ax, [w3col]
	add ax, 4
	mov [w3col], ax
	mov ax, [w3color];bp + 10
	push ax 
  	mov ax, [w3row]; bp + 8
	push ax
	mov ax, [w3col]; bp + 6
	push ax
	mov ax, [w3length]
	push ax; bp + 4
	call printSquare
	
	pop ax
	ret	

Building4:
	push ax
	mov ax, [b4height]
	push ax
	mov ax, [b4color];bp + 10
	push ax 
    mov ax, [b4row]; bp + 8
	push ax
	mov ax, [b4col]; bp + 6
	push ax
	mov ax, [b4length]
	push ax; bp + 4
	call printRectangle
    
	;window1
	mov ax, [w4color];bp + 10
	push ax 
  	mov ax, [w4row]; bp + 8
	push ax
	mov ax, [w4col]; bp + 6
	push ax
	mov ax, [w4length]
	push ax; bp + 4
	call printSquare
	
	;window2
	mov ax, [w4col]
	add ax, 4
	mov [w4col], ax
	mov ax, [w4color];bp + 10
	push ax 
  	mov ax, [w4row]; bp + 8
	push ax
	mov ax, [w4col]; bp + 6
	push ax
	mov ax, [w4length]
	push ax; bp + 4
	call printSquare
	
	;window3
	mov ax, [w4col]
	add ax, 4
	mov [w4col], ax
	mov ax, [w4color];bp + 10
	push ax 
  	mov ax, [w4row]; bp + 8
	push ax
	mov ax, [w4col]; bp + 6
	push ax
	mov ax, [w4length]
	push ax; bp + 4
	call printSquare
	
	;window4
	mov ax, [w4col]
	sub ax, 8
	mov [w4col], ax
	mov ax, [w4row]
	add ax, 2
	mov [w4row], ax
	mov ax, [w4color];bp + 10
	push ax 
  	mov ax, [w4row]; bp + 8
	push ax
	mov ax, [w4col]; bp + 6
	push ax
	mov ax, [w4length]
	push ax; bp + 4
	call printSquare
	
	;window5
	mov ax, [w4col]
	add ax, 4
	mov [w4col], ax
	mov ax, [w4color];bp + 10
	push ax 
  	mov ax, [w4row]; bp + 8
	push ax
	mov ax, [w4col]; bp + 6
	push ax
	mov ax, [w4length]
	push ax; bp + 4
	call printSquare
	
	;window6
	mov ax, [w4col]
	add ax, 4
	mov [w4col], ax
	mov ax, [w4color];bp + 10
	push ax 
  	mov ax, [w4row]; bp + 8
	push ax
	mov ax, [w4col]; bp + 6
	push ax
	mov ax, [w4length]
	push ax; bp + 4
	call printSquare
	
	pop ax
	ret

Building5:
	push ax
	mov ax, [b5height]
	push ax
	mov ax, [b5color];bp + 10
	push ax 
    mov ax, [b5row]; bp + 8
	push ax
	mov ax, [b5col]; bp + 6
	push ax
	mov ax, [b5length]
	push ax; bp + 4
	call printRectangle
    
	;window1
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window2
	mov ax, [w5col]
	add ax, 4
	mov [w5col], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window3
	mov ax, [w5col]
	add ax, 4
	mov [w5col], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window4
	mov ax, [w5col]
	sub ax, 8
	mov [w5col], ax
	mov ax, [w5row]
	add ax, 2
	mov [w5row], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window5
	mov ax, [w5col]
	add ax, 4
	mov [w5col], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window6
	mov ax, [w5col]
	add ax, 4
	mov [w5col], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window7
	mov ax, [w5col]
	sub ax, 8
	mov [w5col], ax
	mov ax, [w5row]
	add ax, 2
	mov [w5row], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window8
	mov ax, [w5col]
	add ax, 4
	mov [w5col], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	;window9
	mov ax, [w5col]
	add ax, 4
	mov [w5col], ax
	mov ax, [w5color];bp + 10
	push ax 
  	mov ax, [w5row]; bp + 8
	push ax
	mov ax, [w5col]; bp + 6
	push ax
	mov ax, [w5length]
	push ax; bp + 4
	call printSquare
	
	pop ax
	ret

Building6:
	push ax
	mov ax, [b6height]
	push ax
	mov ax, [b6color];bp + 10
	push ax 
    mov ax, [b6row]; bp + 8
	push ax
	mov ax, [b6col]; bp + 6
	push ax
	mov ax, [b6length]
	push ax; bp + 4
	call printRectangle
    
	;window1
	mov ax, [w6color];bp + 10
	push ax 
  	mov ax, [w6row]; bp + 8
	push ax
	mov ax, [w6col]; bp + 6
	push ax
	mov ax, [w6length]
	push ax; bp + 4
	call printSquare
	
	;window2
	mov ax, [w6col]
	add ax, 6
	mov [w6col], ax
	mov ax, [w6color];bp + 10
	push ax 
  	mov ax, [w6row]; bp + 8
	push ax
	mov ax, [w6col]; bp + 6
	push ax
	mov ax, [w6length]
	push ax; bp + 4
	call printSquare
	
	;window3
	mov ax, [w6col]
	sub ax, 6
	mov [w6col], ax
	mov ax, [w6row]
	add ax, 2
	mov [w6row], ax
	mov ax, [w6color];bp + 10
	push ax 
  	mov ax, [w6row]; bp + 8
	push ax
	mov ax, [w6col]; bp + 6
	push ax
	mov ax, [w6length]
	push ax; bp + 4
	call printSquare
	
	;window4
	mov ax, [w6col]
	add ax, 6
	mov [w6col], ax
	mov ax, [w6color];bp + 10
	push ax 
  	mov ax, [w6row]; bp + 8
	push ax
	mov ax, [w6col]; bp + 6
	push ax
	mov ax, [w6length]
	push ax; bp + 4
	call printSquare
	
	;window5
	mov ax, [w6col]
	add ax, 6
	mov [w6col], ax
	mov ax, [w6color];bp + 10
	push ax 
  	mov ax, [w6row]; bp + 8
	push ax
	mov ax, [w6col]; bp + 6
	push ax
	mov ax, [w6length]
	push ax; bp + 4
	call printSquare
	
	;window6
	mov ax, [w6col]
	sub ax, 12
	mov [w6col], ax
	mov ax, [w6row]
	add ax, 2
	mov [w6row], ax
	mov ax, [w6color];bp + 10
	push ax 
  	mov ax, [w6row]; bp + 8
	push ax
	mov ax, [w6col]; bp + 6
	push ax
	mov ax, [w6length]
	push ax; bp + 4
	call printSquare
	
	;window7
	mov ax, [w6col]
	add ax, 6
	mov [w6col], ax
	mov ax, [w6color];bp + 10
	push ax 
  	mov ax, [w6row]; bp + 8
	push ax
	mov ax, [w6col]; bp + 6
	push ax
	mov ax, [w6length]
	push ax; bp + 4
	call printSquare
	
	pop ax
	ret

Building7:
	push ax
	mov ax, [b7height]
	push ax
	mov ax, [b7color];bp + 10
	push ax 
    mov ax, [b7row]; bp + 8
	push ax
	mov ax, [b7col]; bp + 6
	push ax
	mov ax, [b7length]
	push ax; bp + 4
	call printRectangle
    
	;window1
	mov ax, [w7color];bp + 10
	push ax 
  	mov ax, [w7row]; bp + 8
	push ax
	mov ax, [w7col]; bp + 6
	push ax
	mov ax, [w7length]
	push ax; bp + 4
	call printSquare
	
	;window2
	mov ax, [w7col]
	add ax, 6
	mov [w7col], ax
	mov ax, [w7color];bp + 10
	push ax 
  	mov ax, [w7row]; bp + 8
	push ax
	mov ax, [w7col]; bp + 6
	push ax
	mov ax, [w7length]
	push ax; bp + 4
	call printSquare
	
	;window3
	mov ax, [w7col]
	sub ax, 6
	mov [w7col], ax
	mov ax, [w7row]
	add ax, 2
	mov [w7row], ax
	mov ax, [w7color];bp + 10
	push ax 
  	mov ax, [w7row]; bp + 8
	push ax
	mov ax, [w7col]; bp + 6
	push ax
	mov ax, [w7length]
	push ax; bp + 4
	call printSquare
	
	;window4
	mov ax, [w7col]
	add ax, 6
	mov [w7col], ax
	mov ax, [w7color];bp + 10
	push ax 
  	mov ax, [w7row]; bp + 8
	push ax
	mov ax, [w7col]; bp + 6
	push ax
	mov ax, [w7length]
	push ax; bp + 4
	call printSquare
	
	pop ax
	ret
		
Buildings:
	call Building1
	call Building2
	call Building3
	call Building4
	call Building5
	call Building6
	call Building7
	ret

;--------------------------------------------------------------------------------------------------------------
StoresLastColumnInAnArray:
	push di
	push si
	push ax
	push es 
	mov ax, 0xb800 
	mov es , ax
	mov di, 158
	mov si , 0
	cloop:
	    mov ax , [es:di]
		mov [arr+si] , ax
		add di, 160
		add si , 2
		cmp di, 3998
		jne cloop
	pop es
	pop ax
	pop si
	pop di
	ret

MovesEveryColumnAagy:
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	push es
	
	mov ax, 0xb800
	mov es, ax
	mov cx,0
	mov di, ((79*24)*2)+2
	mov si, ((79*24)*2)
	mov cx,80*25
	moveouterloop:
				sub di,2
				sub si,2
				cmp si,0
				je rette
				mov ax,[es:si]
				mov word[es:di],ax
				loop moveouterloop
	rette:
		pop es
		pop si
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		ret
		
SettingBackArrayAtCol1:
    push di
	push si
	push bx
	push es
	mov bx,0xb800
	mov es,bx
	mov di, 0
	mov si, 0
	kloop:
		mov bx, [arr+si] 
		mov [es:di], bx
		add di, 160
		add si, 2
		cmp di, 3840
		jne kloop
	pop es
	pop bx
	pop si
	pop di
	ret
	
Phase2:
	looooop:
		call StoresLastColumnInAnArray
		call MovesEveryColumnAagy
		call SettingBackArrayAtCol1
		call delay
		jmp looooop
	ret 
;--------------------------------------------------------------------------------------------------------------
Phase1:
	call clrscr
	call printSky
	call printroad
	call Roadlines1
	call Roadlines2
	call Stars
	call Buildings
	call printCAR
	ret
	
start:
	call Phase1
	call Phase2
	mov ax, 0x4c00
	int 0x21
	
arr : dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0