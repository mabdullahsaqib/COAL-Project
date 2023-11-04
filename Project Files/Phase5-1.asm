[org 0x0100]
section .text
jmp start
;print function takes 3 parameters
;1. array to print bp+4
;2. x displacement bp+6
;3. y displacement bp+8
;4. size of array  bp+10
print:
	push bp
	mov bp,sp
	sub sp,2 ;bp-2 is just the amount of pixels in one row,
	mov word[bp-2],0x0000
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	push es
	mov bx,[buffer]
	mov es,bx
	mov si,[bp+4]
	;calculate number of pixels in row
	rownum:
			add si,1
			mov al,[si]
			cmp al,254
			jne rownum
	mov ax,si
	mov bx,[bp+4]
	sub ax,bx
	mov [bp-2],ax
	;calculate start pos of di
	mov ax,[bp+8]
	cmp word ax,[printmasky]
	jnl printskipdi
	mov ax,[printmasky]
	printskipdi:
	mov cx,320
	xor dx,dx
	mul cx
	mov cx,[bp+6]
	add ax,cx
	mov di,ax
	;calculate start pos of si
	mov cx,[bp+10] ;move length of array into cx
	mov si,[bp+4] ;move array into si
	mov ax,[printmasky] ; check if printmasky-y <= 0
	sub ax,[bp+8]
	cmp ax,0
	jng printloop ;if printmasky-y<=0 then we go to straight to printloop as no rows need to be skipped
	mov bx,[bp-2] ;if printmasky-y>0 then we need to skip some rows, first we check if (printmasky-y)*(horizontal res) < total pixels
	inc bx       
	xor dx,dx
	mul bx
	cmp ax,cx ;cx has total amount of pixels in image, if skip amount is greater than or equal to it we just end printing otherwise we go to print loop while skipping some pixels
	jae printending 
	add si,ax
	sub cx,ax
	;print every byte
	printloop:
		mov al,[si] ;mov array element into al for checking transparency/delimiters
		cmp al,255 ;transparent
		je printskip5
		cmp al,254 ;go to next row
		jne printskip 
			mov ax,di                              
			mov bx,320
			xor dx,dx 
			div bx
			push dx
			xor dx,dx
			mov ax,di
			sub ax,[bp-2]
			div bx
			pop bx
			cmp bx,dx
			ja printskip3
			add di,320
			printskip3:
			add di,320
			sub di,[bp-2]
			add si,1
			jmp printskip2
		printskip:
				movsb
				jmp printskip4
			printskip5:
			add di,1
			add si,1
				printskip4:
				mov ax,di
				mov bx,320
				xor dx,dx
				div bx
				cmp dx,0
				jne printskip2
				sub di,320
				printskip2:
				sub cx,1
				cmp cx,0
				jz printending
				cmp di,64000
				jne printloop
	printending:
	pop es
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	add sp,2
	pop bp
ret 8

delay:
	push cx
	push bx
	mov bx,0x0FFF
	delayloop:
	mov cx,0xFFFF
	delayloop1:
				loop delayloop1
				sub bx,1
				jnz delayloop
	pop bx
	pop cx
	ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
roadposx:dw 0
roadposy:dw 100
printroad:
		push ax
		push bx
		push cx
		push dx
		mov cx,7
		mov ax,[roadposx]
		roadloop:
		    push word [roadsize]
			push word [roadposy]
			push word ax
			push road
			call print
			add ax,46
			mov bx,320
			xor dx,dx
			div bx
			mov ax,dx
		loop roadloop
		pop dx
		pop cx
		pop bx
		pop ax
		ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------		
fenceposx:dw 0
fenceposy:dw 73
printfence:
			push ax
		push bx
		push cx
		push dx
		mov cx,10
		mov ax,[fenceposx]
		fenceloop:
		    push word [fencesize]
			push word [fenceposy]
			push word ax
			push fence
			call print
			add ax,32
			mov bx,320
			xor dx,dx
			div bx
			mov ax,dx
		loop fenceloop
		pop dx
		pop cx
		pop bx
		pop ax
		ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------		
mountainposx:dw 0
mountainposy:dw 20
printmountain:
			  push ax
			  mov ax,[mountainposx]
			  push word [mountainsize]
			  push word [mountainposy]
			  push word ax
			  push mountain
			  call print
			  add ax,150
			  push word [mountainsize]
			  push word [mountainposy]
			  push word ax
			  push mountain
			  call print
			  pop ax
			  ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  
horseposx:dw 255
horseposy:dw 70
horseframe:dw 0
horseinterval:dw 2
horseframerate:dw 2
printhorse:
			push word [horsesize]
			push word [horseposy]
			push word [horseposx]
			cmp word [horseframe],0
			jne horseskip
			push horse1
			horseskip:
			cmp word [horseframe],1
			jne horseskip2
			push horse2
			horseskip2:
			cmp word [horseframe],2
			jne horseskip3
			push horse3
			horseskip3:
			cmp word [horseframe],3
			jne horseskip4
			push horse4
			horseskip4:
			cmp word [horseframe],4
			jne horseskip5
			push horse5
			horseskip5:
			cmp word [horseframe],5
			jne horseskip6
			push horse6
			horseskip6:
			call print
			ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
treelineposx:dw 0
treelineposy:dw 47
;we get an offset of how much distance the trees must have on x axis
;offset can be animated to give parallax
printtrees:
			push bp
			mov bp,sp
			push ax
			push bx
			push cx
			mov cx,4
			mov ax,[treelineposx]
			mov bx,[treelineposy]
			printtreeloop:
			push word [treesize]
			push bx
			push ax
			push treeline
			call print
			add bx,8
			add ax,[bp+4]
			
			push cx
			mov cx,320
			xor dx,dx
			div cx
			mov ax,dx
			pop cx
			
			sub cx,1
			jnz printtreeloop
			pop cx
			pop bx
			pop ax
			pop bp
			ret 2
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
sunposx:dw 15
sunposy:dw 7
printsun:
		push word[sunsize]
		push word[sunposy]
		push word[sunposx]
		push sun
		call print
		ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
skyposx:dw 0
skyposy:dw 0
printsky:
		push ax
		push word[skysize]
		push word[skyposy]
		push word[skyposx]
		push sky
		call print
		mov ax,[skyposx]
		add ax,160
		push word[skysize]
		push word[skyposy]
		push ax
		push sky
		call print
		pop ax
		ret

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
carposx: dw 20
carposy: dw 90
printcar:
		push word[carsize]
		push word[carposy]
		push word[carposx]
		push car
		call print
		ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
bottomstartrow:dw 131
printbottom:
			push ax
			push cx
			push dx
			push di
			push si
			push es
			mov ax,[buffer]
			mov es,ax
			mov ax,[bottomstartrow]
			xor dx,dx
			mov cx,320
			mul cx
			mov di,ax
			mov al,[bottomcol]
			bottomloop:
					  mov byte[es:di],al
					  inc di
					  cmp di,64000
					  jne bottomloop
			pop es
			pop si
			pop di
			pop dx
			pop cx
			pop ax
			ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
carrotposx:dw 160
carrotposy:dw 100
printcarrot:
			cmp byte[carrotenabled],0
			je printcarrotskip
			push word[carrotsize]
			push word[carrotposy]
			push word[carrotposx]
			push carrot
			call print
			printcarrotskip:
			ret
printscorecarrot:
			push word[carrotsize]
			push word 135
			push word 290
			push carrot
			call print
			ret
			
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
rabbitposx: dw 150
rabbitposy: dw 180
rabbitjumpheight: dw 153
rabbitstate: db 0 ;rabbitstate is used to check current state of rabbit
				 ;0=rabbit can jump (not jumping,falling or scrolling down), this is the only state where interrupt can do anything
				 ;1=rabbit is jumping
				 ;2=bricks are scrolling down
				 ;3=rabbit is falling (dying).
				 ;4=rabbit's tail has ended
printrabbit:
			push word[rabbitsize]
			push word[rabbitposy]
			push word[rabbitposx]
			cmp byte[rabbitstate],3
			jge printrabbitskip
			push rabbit1
			jmp printrabbitend
			printrabbitskip:
			push rabbit2
			printrabbitend:
			call print
			ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
;this function is used to swap the bricks with
;eachother so the globalbrickpos returns to its original position
;and all the bricks swap places
;This is only called once whenever the platforms come down
resetbricks:
			push ax
			push bx
			push cx
			push dx
			mov word [globalbrickpos],116
			mov ax,[brick3xpos]
			mov bl,[brick3dir]
			mov bh,[brick3col]
			mov [brick4xpos],ax
			mov [brick4dir],bl
			mov [brick4col],bh
			mov byte[iscurrbrickblue],0
			cmp bh,byte[bluecol]
			jne rbbcs
			mov byte[iscurrbrickblue],1
			mov word[bluebricktimer],0
			rbbcs:
			mov ax,[brick2xpos]
			mov bl,[brick2dir]
			mov bh,[brick2col]
			mov [brick3xpos],ax
			mov [brick3dir],bl
			mov [brick3col],bh
			
			mov ax,[brick1xpos]
			mov bl,[brick1dir]
			mov bh,[brick1col]
			mov [brick2xpos],ax
			mov [brick2dir],bl
			mov [brick2col],bh
			
			;newbricks are spawned at random value between globalleft and globalright borders
			;rdstc is also used to check if brick should be blue,
			;if random value is multiple of 5 then brick is blue else not
			;only yellow bricks are spawned at random location, else it is at 135
			rdtsc                                                                        
			push ax
			mov cx,[globalrightborder]
			xor dx,dx
			sub cx,[globalleftborder]
			inc cx
			div cx
			add dx,[globalleftborder]
			mov word[brick1xpos],dx
			pop ax
			shr ax,15
			mov byte[brick1dir],al
			cmp byte[carrotenabled],1
			je rbcskip
				mov byte[carrotenabled],al
				cmp al,0
				je rbcskip
				call resetcarrot
			rbcskip:
			rdtsc
			mov cl,[yellowcol]
			mov byte[brick1col],cl
			mov cx,4
			xor dx,dx
			div cx
			cmp dx,0
			jne resetbrickskip
			mov cl,byte[bluecol]
			mov byte[brick1col],cl
			mov word[brick1xpos],135
			resetbrickskip:
			cmp dx,1
			jne resetbrickend
			mov cl,byte[skincol]
			mov byte[brick1col],cl
			mov word[brick1xpos],135
			resetbrickend:
			pop dx
			pop cx
			pop bx
			pop ax
			ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
globalbrickpos:dw 116
globalleftborder:dw 85
globalrightborder:dw 185
brick1xpos:dw 85
brick2xpos:dw 185
brick3xpos:dw 85
brick4xpos:dw 135


brick1dir:db 1 ;0 means left,1 means right
brick2dir:db 0 
brick3dir:db 1 
brick4dir:db 0 

brick1col:db 0x0E
brick2col:db 0x0E
brick3col:db 0x0E
brick4col:db 0x0E

yellowcol:db 0x0E 
bluecol:db 0x37   
skincol:db 0x5C
bottomcol:db 0x0B 

iscurrbrickblue:db 0
bluebricktimer:dw 0
bluebricktime:dw 0x2FFF
redwarningtimer:dw 0x1FFF
;bp+4 has location, bp+6 has color
brickprintloop:
				push bp
				mov bp,sp
				push bx
				push cx
				push dx
				push di
				push es
				mov bx,[buffer]
				mov es,bx
				mov di,[bp+4]
				mov bx,5
				mov dh,[bp+6]
				brickl1:
						mov cx,60
						brickl2:	
								mov [es:di],dh
								add di,1
								loop brickl2
						add di,320
						sub di,60
						sub bx,1
						jnz brickl1
				pop es
				pop di
				pop dx
				pop cx
				pop bx
				pop bp
				ret 4
printbricks:
			push ax
			push bx
			push cx
			push dx
			push di
			push si
			push es
			
			mov ax,[globalbrickpos]
			xor dx,dx
			mov cx,320
			mul cx
			add ax,[brick1xpos]
			push word[brick1col]
			push ax
			call brickprintloop
			
			mov ax,[globalbrickpos]
			add ax,27
			xor dx,dx
			mov cx,320
			mul cx
			add ax,[brick2xpos]
			push word[brick2col]
			push ax
			call brickprintloop
			
			mov ax,[globalbrickpos]
			add ax,54
			xor dx,dx
			mov cx,320
			mul cx
			add ax,[brick3xpos]
			push word[brick3col]
			push ax
			call brickprintloop
			
			mov ax,[globalbrickpos]
			add ax,81
			xor dx,dx
			mov cx,320
			mul cx
			add ax,[brick4xpos]
			push word[brick4col]
			push ax
			call brickprintloop
			
			pop es
			pop si
			pop di
			pop dx
			pop cx
			pop bx
			pop ax
			ret	
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
movebricks:
			pusha
			mov si,brick1xpos
			mov di,brick1dir
			mov bx,brick1col
			mov cx,4
			movebrickloop:
				mov ax,[si]
				mov dl,[di]
				mov dh,[bx]
				cmp dh,byte[yellowcol]
				jne bsskip
				cmp dl,0
				jne bs
				call moveleft
				jmp bsskip
				bs:
				call moveright
				bsskip:
				mov word[si],ax
				mov byte[di],dl
				add di,1
				add bx,1
				add si,2
			    loop movebrickloop
			;add or subtract 1 from rabbit pos based on dir value of brick4/brick3 based on rabbitstate
			mov dl,[brick4dir]
			mov dh,[brick4col]
			cmp byte[rabbitstate],1
			je bsend
			cmp byte[rabbitstate],2
			jne bsskip2
			mov dl,[brick3dir]
			mov dh,[brick3col]
			bsskip2:
			cmp dh,byte[yellowcol]
			jne bsend
			cmp dl,0
			je bsskip4	
				add word[rabbitposx],1
			jmp bsend
			bsskip4:
				sub word[rabbitposx],1
			bsend:
			popa
			ret
moveleft:
		sub ax,1
		cmp word ax,[globalleftborder]
		jae moveleftskip
		add ax,2
		mov dl,1
		moveleftskip:
		ret
moveright:
		add ax,1
		cmp word ax,[globalrightborder]
		jbe moverightskip
		sub ax,2
		mov dl,0
		moverightskip:
		ret 
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
checkrabbitcollision:
					push ax
					push bx
					push cx
					push dx
					mov ax,[rabbitposx]
					mov bx,[brick3xpos]
					mov cx,bx
					add cx,61
					sub bx,24
					cmp ax,bx
					jl collisionfail
					cmp ax,cx
					jg collisionfail
					jmp collisionsuccess
					collisionfail:
					mov byte[rabbitstate],3
					collisionsuccess:
					pop dx
					pop cx
					pop bx
					pop ax
					ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
;if carrot is collected we move it back up and increment score
checkcarrotcollision:
					push ax
					push bx
					push cx
					push dx
					cmp byte[carrotenabled],0
					je carrotcollisionend
					mov ax,[rabbitposy]
					mov bx,[carrotposy]
					sub ax,bx
					jns checkcarrotskip
					neg ax
					checkcarrotskip:
					cmp ax,5
					jg carrotcollisionend
					mov ax,[rabbitposx]
					mov bx,[carrotposx]
					mov cx,bx
					add bx,11
					sub cx,25
					cmp ax,cx
					jbe carrotcollisionend
					cmp ax,bx
					jae carrotcollisionend
					add word[score],1
					call resetcarrot
					mov byte[carrotenabled],0
					carrotcollisionend:
					pop dx
					pop cx
					pop bx
					pop ax
					ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
;bp+4 contains amount to move by, if the move would exceed screen bounds then we move carrot back to original position
carrotoriginalpos:dw 0
carrotlevel:db 0
movecarrot:
			push bp
			mov bp,sp
			push ax
			cmp byte[carrotenabled],0
			je movecarrotskip
			
			mov ax,[bp+4]
			add word[carrotposy],ax
			cmp word[carrotposy],200
			jl movecarrotskip
			call resetcarrot
			mov byte[carrotenabled],0
			movecarrotskip:
			pop ax
			pop bp
			ret 2
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
carrotenabled:db 0
resetcarrot:
			push ax
			mov ax,[carrotoriginalpos]
			mov word[carrotposy],ax
			mov byte[carrotlevel],0
			pop ax
			ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
setcarrot:
			push ax
			push dx
			push cx
			xor ax,ax
			mov al, byte[carrotlevel]
			xor dx,dx
			mov cx,27
			mul cx
			add ax,[carrotoriginalpos]
			mov [carrotposy],ax
			pop cx
			pop dx
			pop ax
			ret


;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
converttostring:
				pusha
				mov si,digits
				add si,1
				mov cx,2
				mov ax,[score]
				convertloop:
							xor dx,dx
							mov bx,10
							div bx
							add dl,'0'
							mov [si],dl
							dec si
							loop convertloop
				popa
				ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
scoreposx:db 38
scoreposy:db 17
score:dw 0
digits:db '00'
printscore: 
			pusha
			call converttostring
			mov ax,cs
			mov es,ax			
			mov ah,13h;service to print string in graphic mode
			mov al,0;sub-service 0 all the characters will be in the same color(bl) and cursor position is not updated after the string is written
			mov bh,0;page number=always zero
			mov bl,00001111b;color of the text (white foreground and black background)
			mov cx,2;length of string
			mov dh,[scoreposy];y coordinate
			mov dl,[scoreposx];x coordinate
			mov bp,digits;mov bp the offset of the string
			int 10h
			popa
		    ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
printbackground:
			push bp
			mov bp,sp
			call printsky
			call printsun
			call printmountain
			push word[bp+4]
		    call printtrees
			pop bp
			ret 2

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
treeanimatedoffset:dw 0
printElements:
			    add word[treeanimatedoffset],1
				push word[treeanimatedoffset]
				call printbackground
				
				call printbottom
				call printbricks
				call printrabbit
				call printcarrot
				call printscorecarrot
				
				call printfence
				call printroad
				call printcar
				call printhorse
				ret
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
animatebackground:
				push cx
				;set values for next loop
				add word[roadposx],10
				add word[fenceposx],10
				add word[treelineposx],2
				sub word[horseinterval],1 
				jnz backgroundskip0
				mov cx,[horseframerate]
				mov word[horseinterval],cx
				add word[horseframe],1
				cmp word[horseframe],6
				jne backgroundskip0
				mov word[horseframe],0
				backgroundskip0:
					cmp word[roadposx],320
					jne backgroundskip
					mov word[roadposx],0
				backgroundskip:
					cmp word[fenceposx],320
					jne backgroundskip1
					mov word[fenceposx],0
				backgroundskip1:
					cmp word[treelineposx],320
					jne backgroundskip2
					mov word[treelineposx],0
				 backgroundskip2:
				 pop cx
				 ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
GameChecks:
					pusha
					cmp byte[rabbitstate],4
					je gamecheckend
					cmp byte[rabbitstate],3
					je dying
					;rabbit is not dying or dead
					call movebricks
					call checkcarrotcollision
					cmp byte[rabbitstate],0
					je bluebrickcheck
					cmp byte[rabbitstate],1
					je rabbitjump
					cmp byte[rabbitstate],2
					je scrolldown
					jmp gamecheckend
					
					bluebrickcheck:
								cmp byte[iscurrbrickblue],1
								jne bluebrickcheckskip
								mov ax,[bluebricktime]
								cmp word[bluebricktimer],ax
								jc bluebrickcheckskip
									mov byte[iscurrbrickblue],0
									mov byte[brick4col],0x0B
									mov word[bluebricktimer],0
									mov byte[rabbitstate],4
									mov byte[gamestate], 2
									mov word[rabbitposy],184
								bluebrickcheckskip:
								jmp gamecheckend
					rabbitjump:
								sub word[rabbitposy],5
								mov bx,[rabbitjumpheight]
								cmp word[rabbitposy],bx
								ja  rabbitjumpskip  ;dont change state unless posy>=jumpheight
													;if rabbit reached height, check if x pos is within bounds of brick3
													;if not, set rabbitstate to 3 (dead)
									mov word[rabbitposy],bx
									mov byte[rabbitstate],2
									call checkrabbitcollision
									rabbitjumpskip:
								jmp gamecheckend
					scrolldown:
								add word[rabbitposy],5
								add word[globalbrickpos],5
								cmp byte[carrotenabled],0
								je scrollskip
								push 5
								call movecarrot
								scrollskip:
								cmp word[rabbitposy],180
								jl scrolldownskip
									 cmp byte[carrotenabled],0
									 je scrollskip2
										add byte[carrotlevel],1									 
										call setcarrot
									 scrollskip2:
									 mov word[rabbitposy],180
									 mov byte[rabbitstate],0
									 call resetbricks
								scrolldownskip: 
								jmp gamecheckend
								
					dying:
								add word[rabbitposy],5
								cmp word[rabbitposy],180
								jl dyingskip
									mov word[rabbitposy],180
									mov byte[rabbitstate],4
									mov byte[gamestate], 2
								dyingskip:
								jmp gamecheckend
					
					gamecheckend:
					popa
					ret

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
printbuffer:
			cli
			pusha
			push es
			push ds
			
			mov ax,[printmasky]
			mov cx,320
			xor dx,dx
			mul cx
			mov cx, [buffer]
			mov ds, cx
			mov si,ax
			mov cx, 0xA000
			mov es, cx
			xor di, di
			add di,ax
			mov cx, 64000
			sub cx,ax
			shr cx,1
			rep movsw
			
			pop ds
			pop es
			popa 
			sti
			ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
readstartimage:
			pushad
			cli
			mov ah,3dh ; open file
			mov al,0
			lea dx,startfilename
			int 21h
			mov [startfilehandle],ax
			;read file and write bytes to 
			 mov ah,3fh
			 mov cx,64000
			 mov bx,[startfilehandle]
			 mov dx,[startScreen_buffer]
			 mov ds,dx
			 xor dx,dx
			 int 21h
			 ;close file
			mov ax,cs
			mov ds,ax
			mov ah,3eh
			mov bx,[startfilehandle]
			int 21h
			sti
			popad
			ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
startText:db 'Press E to Continue'
startTextPrint:
			pusha
			mov ax,cs
			mov es,ax			
			mov ah,13h;service to print string in graphic mode
			mov al,0;sub-service 0 all the characters will be in the same color(bl) and cursor position is not updated after the string is written
			mov bh,0;page number=always zero
			mov bl,00001111b;color of the text (white foreground and black background)
			mov cx,19;length of string
			mov dh,4;y coordinate
			mov dl,9;x coordinate
			mov bp,startText;mov bp the offset of the string
			int 10h
			popa
		    ret
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
soundtask:
			;read sound data from file and store in sound_buffer segment
			cli
			mov ah,3dh ; open file
			mov al,0
			lea dx,filename
			int 21h
			mov [filehandle],ax
			;read file and write bytes to 
			mov ah,3fh
			mov cx,[sound_size]
			mov bx,[filehandle]
			mov dx,[sound_buffer]
			mov ds,dx
			xor dx,dx
			int 21h
			;close file
			mov ax,cs
			mov ds,ax
			mov ah,3eh
			mov bx,[filehandle]
			int 21h
			sti
			;loop to play audio at specific intervals
			mov dx,[sound_buffer]
			mov es,dx
			soundloop:	
				;send DSP command 10h
				mov dx, 22ch
				mov al,10h
				out dx,al
				;send byte audio sample
				mov si,[sound_index]
				mov al,[es:si]
				out dx,al
				add word[sound_index],1
				  mov cx,5500
				   sounddelay:
				   loop sounddelay
				  mov dx,[sound_size]
				cmp word[sound_index],dx
				jne soundskip
				mov word[sound_index],0
				soundskip:
			jmp soundloop
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
kbisr:		push ax
			cmp byte[gamestate],1           ;only do keyboard interrupt override when gamestate is 1
			jne nomatch
			in al, 0x60						; read a char from keyboard port, scancode
			cmp al, 0x48					; is the key up
			jne nomatch						; leave interrupt routine 
			cmp byte[rabbitstate],0
			jne nomatch
			mov byte[rabbitstate],1
            
nomatch:	mov al, 0x20
			out 0x20, al					; send EOI to PIC
			pop ax
			jmp far[cs:keyboardinterrupt]
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
timer:		
			cmp byte[gamestate],0
			je StartTimer
			cmp byte[gamestate],3
			je EndTimer
			jmp GameTimer
			
StartTimer:			
			push ax
			inc word[startScreenTick]
			mov ax,[startScreenBlinkInterval]
			cmp word[startScreenTick],ax
			jc starttimerskip
				mov word[startScreenTick],0
				call startscreen
				jmp starttimerend
			starttimerskip:
				shr ax,1
				cmp word[startScreenTick],ax
				jc starttimerend
				call startTextPrint
			starttimerend:
			pop ax
			iret 
GameTimer:
			push ax
			;check if current brick is blue and if it is then increment both timers
			cmp byte[iscurrbrickblue],1
			jne timerskip
			mov ax,[bluebricktime]
			cmp word[bluebricktimer],ax
			je timerskip
			inc word[bluebricktimer]
			mov ax,[redwarningtimer]
			cmp word[bluebricktimer],ax
			jc timerskip
			mov byte[brick4col],0x28
			jne timerskip
			timerskip:			
			pop ax
			;multitasking code
			push ds
			push bx
			push cs
			pop ds ; initialize ds to data segment
			mov bx, [currenttask] ; read index of current in bx
			shl bx,5 ; multiply by 32 for pcb start
			mov [pcb+bx+0], ax ; save ax in current pcb
			mov [pcb+bx+4], cx ; save cx in current pcb
			mov [pcb+bx+6], dx ; save dx in current pcb
			mov [pcb+bx+8], si ; save si in current pcb
			mov [pcb+bx+10], di ; save di in current pcb
			mov [pcb+bx+12], bp ; save bp in current pcb
			mov [pcb+bx+24], es ; save es in current pcb
			pop ax ; read original bx from stack
			mov [pcb+bx+2], ax ; save bx in current pcb
			pop ax ; read original ds from stack
			mov [pcb+bx+20], ax ; save ds in current pcb
			pop ax ; read original ip from stack
			mov [pcb+bx+16], ax ; save ip in current pcb
			pop ax ; read original cs from stack
			mov [pcb+bx+18], ax ; save cs in current pcb
			pop ax ; read original flags from stack
			mov [pcb+bx+26], ax ; save flags in current pcb
			mov [pcb+bx+22], ss ; save ss in current pcb
			mov [pcb+bx+14], sp ; save sp in current pcb
			mov bx, [pcb+bx+28] ; read next pcb of this pcb
			mov [currenttask], bx ; update current to new pcb
			mov cl, 5
			shl bx, cl ; multiply by 32 for pcb start
			mov cx, [pcb+bx+4] ; read cx of new process
			mov dx, [pcb+bx+6] ; read dx of new process
			mov si, [pcb+bx+8] ; read si of new process
			mov di, [pcb+bx+10] ; read di of new process
			mov bp, [pcb+bx+12] ; read bp of new process
			mov es, [pcb+bx+24] ; read es of new process
			mov ss, [pcb+bx+22] ; read ss of new process
			mov sp, [pcb+bx+14] ; read sp of new process
			push word [pcb+bx+26] ; push flags of new process
			push word [pcb+bx+18] ; push cs of new process
			push word [pcb+bx+16] ; push ip of new process
			push word [pcb+bx+20] ; push ds of new process
			mov al, 0x20
			out 0x20, al ; send EOI to PIC
			mov ax, [pcb+bx+0] ; read ax of new process
			mov bx, [pcb+bx+2] ; read bx of new process
			pop ds ; read ds of new process
			iret ; return to new process

EndTimer:
			cmp word[currenttask],1
			je GameTimer
			iret 
;--------------------------------------------------------------------
startscreen:     
            push es
            push ax
			push cx
			push dx
            push di
			push si
			push ds
			

            mov ax, 0xA000
            mov es, ax                  ; point es to video base
            xor di,di
			xor si,si
			
			mov ax,[startScreen_buffer]
			mov ds,ax
			
			mov cx,32000
			rep movsw
			

			pop ds
            pop si
            pop di
            pop dx
			pop cx
            pop ax
            pop es
            ret

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------					  			
start:
        mov ax,13h
        int 0x10
		call readstartimage    
        cli						; disable interrupts

		mov word[pcb+32+16],soundtask
		mov word[pcb+32+18],cs
		mov word[pcb+32+26],0x0200
		mov word[pcb+32+28],0
		
		mov word[pcb+16],main
		mov word[pcb+18],cs
		mov word[pcb+26],0x0200
		mov word[pcb+28],1
		
		mov word[pcb+32+12],bp
		mov word[pcb+32+14],sp
		mov word[pcb+32+20],ds
		mov word[pcb+32+22],0x6800 ;stack segment for 2nd task, 0x7000 is original stack
									;meaning it starts from 7000:FFFF
									;second stack then starts from 6800:FFFF and goes backwards
		mov word[pcb+32+24],es
		mov word[currenttask],0
		xor ax, ax
		mov es, ax					; es=0, point es to IVT base
		mov ax,[es:9*4]
		mov word[keyboardinterrupt],ax
		mov ax,[es:9*4+2]
		mov word[keyboardinterrupt+2],ax
		mov word [es:9*4], kbisr		; store offset at n*4....... csabc:kbisr	
		mov [es:9*4+2], cs			; store segment at n*4+2
		
		mov ax,[es:1ch*4]
		mov word [timerinterrupt],ax
		mov ax,[es:1ch*4+2]
		mov word [timerinterrupt+2],ax
		mov word[es:1ch*4],timer
		mov [es:1ch*4+2],cs
		
		mov ax, 100
		out 0x40, al
		mov al, ah
		out 0x40, al
		
        sti						; enable interrupts	 
main:
        mov bx,[stack_buffer]
        mov ss,bx
		
		call startscreen   ; display startscreen  
waitforstart:
        mov ah, 0		; wait for user input to start the game
        int 0x16
		cmp ah,0x12
		jne waitforstart
		mov byte[gamestate], 1
		
        mov ax,[carrotposy]
        mov word [carrotoriginalpos],ax
        mov cx,[horseframerate]; how many cycles until we switch to next horse image
        mov [horseinterval],cx
        push 60
        call printbackground;print initial background that wont get written to again
        call printbuffer
        mov word [printmasky],47
		mov ax,0
        mainloop:
        	call printElements
        	call animatebackground
        	call GameChecks
        	call printbuffer
        	call printscore
			cmp byte[gamestate],2
			jne mainloop
			inc ax
			cmp ax,0x003F
			je ending
        jmp mainloop
;unhook all buffers to return control to system
ending:
mov byte[gamestate],3
call startscreen
xor ax,ax
mov es,ax
cli
mov ax,[keyboardinterrupt]
mov word [es:9*4],ax
mov ax,[keyboardinterrupt+2]
mov word [es:9*4+2],ax
mov ax,[timerinterrupt]
mov word [es:1ch*4],ax
mov ax,[timerinterrupt+2]
mov word [es:1ch*4+2],ax
sti
mov ax,0x4c00
int 0x21
section .data
;	  ax,bx,cx,dx,di,si,bp,sp,ss,ds,es,ip,cs,flags
pcb: times 32*16 dw 0
currenttask:dw 0
nextpcb:dw 1
currentpcb:dw 0
keyboardinterrupt:dw 0,0
timerinterrupt:dw 0,0
printmasky:dw 0
gamestate: db 0
startScreen_buffer: dw 0x5000
startScreenBlinkInterval:dw 0x2FFF
startScreenTick:dw 0
stack_buffer: dw 0x7000
sound_buffer: dw 0x8000
buffer: dw 0x9000  ; buffer is stored near end of memory at 9000. 
              ; No other data is stored there as 9FFF is end of memory
			  ; and 9000 gives us exactly 64k bytes for buffer
			  ; (with some padding)
horsesize:dw 3762

horse1:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,185,185,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,185,185,185,209,209,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,43,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,185,17,208,209,210,211,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,42,43,66,67,92,67,43,255,255,255,255,43,67,43,255,255,255,255,255,255,185,185,185,186,234,148,147,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,66,67,66,65,66,66,67,67,43,66,255,43,67,92,67,255,255,255,255,255,255,186,137,137,165,147,124,148,255,255,255,255,255,255,255,255,255,255,255,255,91,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,163,65,65,138,162,66,66,66,66,67,67,67,43,67,67,43,66,255,255,255,255,255,255,6,6,66,138,171,148,124,3,255,255,255,255,255,255,255,91,91,91,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,25,25,65,12,21,161,161,137,66,67,66,67,67,67,67,67,67,92,67,67,255,255,67,67,67,67,140,165,147,124,3,3,76,255,255,255,255,255,91,90,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,26,26,25,25,65,64,162,161,234,114,66,66,66,67,67,67,67,67,67,67,92,92,67,43,43,43,255,255,168,167,146,3,3,76,76,3,255,255,91,91,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,25,25,162,8,234,21,21,21,19,19,19,113,6,65,65,67,67,67,67,66,67,67,67,92,67,43,255,43,43,255,255,145,3,3,3,3,3,29,66,66,65,255,232,20,20,255,255,255,255,255,255,42,43,43,43,67,67,67,67,43,255,255,254,255,255,22,23,22,234,19,20,234,19,19,18,233,18,18,18,19,234,6,65,66,67,66,65,66,67,67,67,66,67,67,43,255,255,3,3,123,3,3,3,28,65,162,23,24,23,25,24,161,8,22,255,255,67,67,67,66,66,67,67,67,67,67,67,43,254,255,21,21,234,18,17,18,19,19,18,18,18,209,18,18,18,18,19,234,161,137,66,67,67,67,67,67,66,66,67,67,6,113,234,3,147,171,147,3,100,28,25,25,26,26,26,25,25,25,23,22,137,65,67,66,66,66,65,66,67,67,67,67,67,67,254,255,255,18,209,255,255,255,255,255,255,209,209,18,18,18,17,17,18,18,19,234,234,65,65,66,67,66,66,65,67,92,67,162,28,28,24,25,25,26,29,29,27,25,26,25,25,25,25,163,8,21,234,137,43,43,6,42,67,67,67,67,67,66,66,65,254,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,18,18,18,19,20,161,162,25,66,66,25,26,88,67,67,26,28,7,26,26,26,65,139,115,66,162,24,23,23,23,8,21,234,234,20,234,6,43,255,255,255,67,67,67,67,42,66,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,18,19,234,23,25,25,26,26,163,25,26,26,88,26,26,25,25,25,25,162,24,162,138,161,22,21,22,22,22,234,234,234,234,234,137,137,255,255,255,255,255,255,255,255,43,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,19,234,22,162,25,25,25,162,162,162,24,162,24,24,25,25,25,24,23,23,22,235,234,20,234,21,21,234,234,234,234,234,232,137,137,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,19,20,8,24,25,162,162,24,24,23,23,8,8,23,162,162,162,8,22,22,21,234,234,20,20,234,234,234,234,234,234,21,234,234,234,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,234,21,23,162,162,23,8,8,22,21,22,22,22,22,21,21,21,21,21,234,20,20,19,20,20,234,234,234,20,234,234,234,232,232,19,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,20,20,22,8,22,22,22,22,21,21,21,21,21,21,234,234,234,20,19,19,18,18,18,19,19,19,234,234,234,234,20,20,233,233,18,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,19,19,19,234,21,234,234,234,234,234,234,234,20,234,19,19,233,18,18,18,18,18,18,18,233,232,234,234,234,20,233,209,209,18,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,19,19,234,234,234,234,19,19,19,19,233,19,18,18,18,18,18,18,18,209,209,18,18,18,19,19,234,234,19,18,18,209,18,209,233,233,18,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,18,19,234,234,234,19,18,209,18,18,209,18,209,209,209,209,209,208,209,19,18,17,18,18,209,19,20,19,232,18,18,209,136,210,8,137,208,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,18,18,19,19,19,18,18,209,209,209,209,209,209,209,136,136,209,136,136,255,255,255,255,255,137,233,18,18,17,18,20,161,137,137,210,209,114,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,19,18,18,18,18,209,209,136,114,136,136,113,136,136,210,255,255,255,255,255,255,255,255,136,18,18,18,20,22,21,234,234,8,22,234,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,232,234,20,18,18,18,209,136,136,185,113,137,136,137,255,255,255,255,255,255,255,255,255,255,255,255,136,209,209,234,233,19,233,234,8,162,162,161,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,233,19,19,19,18,18,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,209,209,209,209,136,210,234,234,137,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,233,19,18,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,137,113,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,209,18,18,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,21,209,136,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,18,209,233,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,234,209,137,255,43,43,43,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,18,209,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,234,232,209,137,67,67,92,91,30,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,18,208,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,137,234,114,43,6,66,66,67,67,30,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,234,20,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,234,65,67,67,67,67,67,67,67,89,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,21,234,19,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,209,113,67,67,67,67,67,67,67,30,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,209,136,185,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,209,66,66,67,67,43,43,67,89,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,232,136,113,113,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,43,66,67,67,255,43,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,232,136,67,67,6,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,43,67,67,67,67,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,234,113,67,92,67,6,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,67,67,43,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,66,67,92,92,92,67,67,67,67,43,43,43,43,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,67,43,67,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,67,66,67,92,92,67,67,67,67,67,92,92,92,92,92,67,67,67,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,67,67,67,67,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,43,67,67,67,67,67,67,92,92,67,43,67,92,92,92,92,92,67,67,66,91,67,67,67,67,67,43,42,255,255,255,255,255,255,255,255,255,255,255,43,67,67,67,67,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,67,67,67,66,67,92,92,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,92,67,67,67,67,255,255,255,255,255,255,255,255,255,255,255,255,43,67,67,43,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,92,67,67,91,90,67,67,67,67,67,92,92,67,67,67,67,92,92,92,67,67,67,67,67,67,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,66,66,66,89,89,67,67,67,67,92,92,67,67,67,67,67,67,67,67,67,43,6,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,67,67,91,89,255,255,42,42,255,43,67,67,67,43,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,43,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,0
horse2:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,186,114,186,185,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,185,186,186,186,186,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,186,185,185,185,185,185,186,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,12,65,43,66,67,67,42,67,255,255,6,255,255,255,255,185,113,186,17,185,185,210,234,186,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,43,66,67,66,66,65,67,67,68,92,67,66,66,42,6,255,255,255,255,113,186,186,186,138,236,146,147,148,3,255,255,255,255,255,255,255,255,255,255,255,89,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,65,67,67,67,65,64,65,66,67,67,91,67,67,67,66,42,255,255,255,255,113,113,137,137,25,172,148,148,148,148,74,255,255,255,255,255,255,28,90,90,90,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,22,163,64,25,25,8,162,66,65,65,67,92,92,68,92,67,67,255,255,255,255,67,66,65,6,164,25,24,172,148,3,74,74,255,255,255,90,66,90,90,90,90,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,24,25,25,25,64,12,21,22,22,12,66,66,67,67,91,67,91,67,66,66,66,42,43,67,67,67,67,65,25,172,148,148,3,74,74,74,255,7,90,90,90,90,66,26,255,255,255,255,255,255,255,255,255,255,255,42,65,255,255,255,254,255,255,255,8,26,26,26,25,25,8,22,8,160,234,137,65,67,65,66,67,66,66,67,91,67,43,67,67,66,67,67,66,25,25,148,148,26,74,74,74,74,90,66,89,66,65,26,255,255,255,255,255,255,255,91,6,255,255,42,66,66,67,67,67,43,254,255,255,255,23,25,26,24,21,21,22,21,235,235,209,19,20,6,12,65,66,67,65,65,67,67,67,67,67,67,67,66,255,25,25,25,26,3,3,74,74,28,90,66,164,235,18,18,19,21,255,255,42,43,43,43,67,67,66,66,67,67,67,67,67,67,254,255,255,21,21,23,8,20,18,20,21,19,19,209,18,18,18,18,233,137,65,67,67,66,67,67,67,67,67,67,66,92,255,255,255,27,7,25,26,3,74,27,26,25,8,22,21,20,19,232,232,161,65,67,66,65,67,67,66,66,67,67,65,65,67,67,254,255,255,21,235,20,18,18,17,18,19,19,18,18,17,18,18,18,18,20,21,12,67,66,66,66,67,91,91,67,67,67,66,12,22,7,3,166,26,74,98,74,26,26,25,25,25,25,23,22,21,235,12,65,65,66,66,66,12,65,66,67,66,66,67,67,254,255,255,20,19,18,18,18,18,18,17,18,210,18,18,18,18,18,18,18,19,235,137,6,65,67,91,91,67,67,67,91,92,67,29,29,171,22,24,27,98,28,27,26,25,25,25,25,23,21,21,137,65,91,67,67,66,65,42,66,67,67,67,67,67,67,254,255,255,255,19,17,18,255,255,255,255,255,185,20,18,17,18,18,18,18,18,19,234,138,25,66,67,67,67,67,91,67,91,67,29,7,24,25,25,25,27,28,7,24,23,163,23,22,21,235,235,137,12,67,67,43,66,42,65,43,66,66,66,43,43,66,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,18,19,19,19,160,24,25,65,65,65,65,65,66,91,91,28,25,24,25,25,24,137,138,6,164,22,8,22,236,20,20,235,136,137,164,255,255,255,255,255,255,255,255,255,255,255,42,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,18,18,18,18,19,19,161,25,25,25,26,25,25,25,64,66,66,26,25,25,25,24,23,21,234,136,235,21,21,21,20,20,20,234,235,136,137,160,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,17,18,18,19,21,24,25,25,25,25,25,24,163,162,25,24,24,24,23,8,22,22,20,19,19,20,20,20,20,20,236,236,235,136,136,136,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,18,19,20,22,24,25,24,24,163,24,161,22,22,22,8,22,22,21,21,21,21,20,19,19,20,19,19,20,20,235,236,235,136,234,232,17,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,19,20,21,22,24,24,8,22,22,22,21,21,21,22,21,21,236,235,20,19,19,19,19,19,19,19,19,20,20,20,234,234,233,232,17,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,19,20,21,21,21,21,21,22,21,21,21,21,21,235,20,19,19,18,18,18,18,18,18,19,19,19,20,20,19,19,209,209,209,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,19,19,19,19,19,20,235,235,235,20,234,19,19,19,19,18,18,18,18,18,208,209,18,18,19,19,19,19,19,209,209,208,209,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,19,19,19,19,19,235,235,19,19,209,19,18,18,18,18,18,18,18,209,209,209,208,208,208,18,19,19,19,18,18,210,232,138,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,19,18,18,19,20,20,19,18,18,18,17,18,18,17,209,136,210,136,136,210,208,208,18,18,18,18,18,18,18,21,21,21,21,17,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,18,18,18,18,18,18,18,18,209,208,208,18,209,136,136,137,137,136,255,209,209,210,209,209,18,18,18,236,21,21,22,21,21,255,18,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,18,18,18,18,18,18,18,18,113,209,209,136,136,136,136,136,210,255,255,255,137,136,136,136,136,209,233,21,236,234,234,21,161,24,161,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,20,19,18,19,19,18,18,18,208,209,136,136,136,137,137,136,255,255,255,255,255,255,208,136,136,208,209,210,18,209,210,209,233,8,23,161,19,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,19,18,18,18,18,209,211,234,210,255,255,255,255,255,255,255,255,255,255,255,255,255,255,207,208,208,208,209,209,209,234,236,137,19,18,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,17,18,18,19,19,209,209,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,17,18,113,113,140,161,236,136,209,136,208,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,18,17,18,18,18,18,209,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,17,208,208,136,12,162,22,232,208,136,255,255,65,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,17,17,18,18,18,18,18,208,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,208,113,136,139,22,233,19,208,137,255,255,43,67,66,255,255,91,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,17,17,17,208,18,209,209,234,255,255,42,42,255,255,255,255,255,255,255,255,255,17,17,209,136,137,161,236,233,6,43,43,66,66,67,92,14,65,255,66,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,17,18,18,18,232,136,255,66,43,66,67,67,65,255,255,255,255,255,255,208,210,42,43,43,12,21,67,67,66,67,67,67,67,92,67,67,66,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,208,17,17,19,21,137,209,137,42,66,67,30,90,30,255,43,66,255,255,6,6,66,67,67,67,42,65,67,67,91,91,91,92,92,92,67,43,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,208,17,19,20,19,18,18,42,66,67,91,91,66,43,43,43,43,43,42,42,43,67,67,67,91,30,65,66,67,30,15,92,92,92,67,67,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,17,18,209,209,18,18,210,209,136,43,67,67,67,92,67,67,67,67,67,43,42,65,67,67,67,67,30,15,67,66,67,91,90,67,92,92,43,255,66,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,208,21,18,208,210,137,66,42,43,43,67,67,66,67,67,92,91,66,65,67,67,67,67,66,66,66,66,67,67,66,67,92,91,43,255,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,233,137,255,255,255,255,66,67,67,67,67,92,67,66,6,67,67,67,67,14,42,255,65,66,14,43,66,67,66,255,255,42,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,209,233,235,136,255,255,255,255,42,43,66,67,67,66,12,255,255,255,66,65,43,65,255,255,255,43,42,255,255,66,42,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,137,233,26,42,65,255,255,43,43,43,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,21,137,66,66,43,43,66,66,43,43,66,43,43,66,42,92,42,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,235,137,6,67,67,67,67,68,67,66,43,67,67,67,67,67,14,43,43,42,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,162,42,66,67,67,67,67,92,67,43,43,67,67,68,67,67,68,67,67,67,42,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,25,65,66,67,67,68,92,92,66,255,255,66,67,92,67,67,67,91,91,67,43,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,235,163,65,66,67,67,91,92,91,89,90,255,255,43,91,67,67,67,66,66,43,43,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,138,163,66,67,91,91,67,92,30,87,255,255,255,66,66,67,42,91,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,67,67,67,67,91,90,88,255,255,255,255,42,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,42,255,42,66,65,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,0
horse3:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,43,65,255,255,255,255,255,255,255,255,185,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,92,92,67,66,66,255,255,255,186,186,114,186,186,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,92,92,92,68,68,42,255,255,255,185,186,186,186,185,186,210,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,66,66,43,65,255,67,43,67,92,67,67,68,67,67,255,255,187,186,185,185,17,185,186,186,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,66,67,67,67,67,43,67,67,92,92,67,67,43,67,255,255,113,186,185,185,186,234,244,22,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,43,66,67,65,64,65,67,67,67,66,67,67,91,91,92,92,43,42,255,185,113,113,114,136,21,147,148,148,255,255,255,255,255,255,255,255,255,7,90,90,90,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,65,66,66,65,23,163,65,64,66,67,66,67,67,91,67,92,92,67,42,255,6,114,140,6,162,172,148,148,148,148,148,255,255,255,255,255,255,90,90,90,90,66,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,159,24,25,65,65,21,21,22,65,66,67,66,66,67,67,91,67,67,67,67,66,255,67,66,66,66,65,164,24,148,148,3,74,74,255,255,255,90,90,90,90,90,66,65,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,163,163,25,25,25,12,12,22,161,235,137,65,67,67,66,67,66,67,91,91,91,92,67,67,66,67,67,66,25,26,172,148,148,74,74,74,74,255,90,90,90,89,89,89,255,255,255,255,255,255,255,255,255,91,6,91,255,255,43,254,255,255,255,255,255,255,25,26,26,25,24,8,21,21,137,209,19,234,137,65,65,12,66,66,67,67,67,67,67,67,67,43,43,255,169,24,172,25,26,74,74,74,28,90,90,66,65,139,255,255,255,255,255,255,255,43,67,67,43,43,43,67,67,67,66,254,255,255,255,255,255,22,24,25,24,21,20,20,20,19,19,18,18,18,18,19,234,137,161,65,67,66,66,65,67,91,67,65,6,255,255,171,148,3,3,3,3,74,28,66,26,138,22,136,233,136,160,25,42,43,43,66,67,67,66,67,67,67,67,67,66,254,255,255,255,255,255,21,8,8,19,18,18,19,19,18,18,17,18,18,18,18,19,235,137,12,66,67,66,67,67,67,67,67,65,42,6,230,26,26,148,172,3,74,7,25,25,25,24,24,22,21,19,231,65,65,65,67,67,66,66,67,67,65,12,66,67,254,255,255,255,255,18,20,20,19,17,18,18,18,18,18,18,18,17,18,18,17,18,19,234,234,137,65,67,67,67,66,67,91,67,66,65,25,7,172,21,171,7,98,74,26,26,25,25,24,23,21,235,136,66,67,66,66,66,64,66,67,67,66,66,67,67,254,255,255,255,255,255,19,18,18,136,186,255,255,255,255,18,17,17,17,18,17,17,18,19,19,21,163,25,64,66,66,67,67,91,67,67,90,74,25,25,25,27,29,29,25,24,24,23,8,22,21,137,137,66,92,67,66,66,65,67,67,67,67,67,66,66,254,255,255,255,255,255,255,19,246,207,255,255,255,255,255,255,255,17,18,18,18,17,18,19,19,235,162,25,25,26,26,26,65,91,92,66,28,27,25,25,25,64,164,7,25,23,23,22,21,20,234,233,136,12,67,43,255,42,12,42,66,66,43,43,66,66,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,17,17,18,18,19,21,162,25,25,25,25,25,25,66,67,65,25,25,25,25,23,161,136,6,138,22,22,235,20,19,20,233,233,136,139,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,18,18,19,21,8,24,24,24,24,25,25,24,163,162,24,25,24,23,8,22,20,18,18,236,236,20,20,20,234,232,232,136,136,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,18,20,21,22,24,24,24,162,163,163,161,161,160,160,8,22,21,21,21,20,19,18,20,20,20,20,20,234,233,232,136,234,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,20,21,160,8,161,161,22,22,22,21,21,21,22,22,21,21,235,20,19,18,18,19,234,19,20,20,20,233,232,232,233,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,19,20,234,20,20,21,21,21,21,21,21,21,21,21,235,20,19,18,18,18,17,18,20,20,20,234,19,19,209,19,209,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,19,19,19,20,20,236,236,21,20,20,20,19,19,19,19,18,18,18,17,18,19,20,20,19,19,209,209,209,18,210,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,19,19,19,20,236,21,20,19,18,19,18,18,208,18,208,209,209,209,113,185,232,20,19,19,18,209,209,209,18,17,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,19,19,19,19,19,20,19,18,18,18,18,18,208,209,208,208,136,6,136,186,209,210,18,18,208,209,136,209,18,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,21,19,19,18,18,19,18,18,18,18,136,208,18,209,136,136,136,6,6,136,209,208,209,209,18,18,208,208,208,209,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,18,19,19,18,18,209,137,210,114,136,136,136,136,137,137,255,209,209,209,209,19,20,232,209,208,233,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,18,20,19,18,18,209,136,136,136,136,136,137,138,137,255,255,255,209,209,136,234,21,235,209,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,17,18,19,19,19,19,209,210,136,136,136,255,208,255,255,255,255,255,185,209,136,234,234,233,232,208,17,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,18,18,208,136,209,18,19,19,136,209,255,255,255,255,255,255,255,255,255,255,255,136,136,137,210,209,136,136,18,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,19,18,208,233,18,18,18,209,210,18,255,255,255,255,255,255,255,255,255,255,255,255,114,114,136,136,136,210,19,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,23,21,209,210,136,234,18,18,209,209,255,255,255,255,255,255,255,255,255,255,6,255,255,255,114,6,136,136,235,234,233,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,22,8,24,137,209,255,255,236,19,19,18,208,255,255,255,255,255,255,255,255,255,42,43,66,255,209,234,6,137,236,22,235,136,255,43,42,255,255,255,42,43,42,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,20,234,233,209,209,255,255,20,20,232,137,255,255,255,255,255,255,255,43,67,67,67,67,12,161,162,65,12,162,6,136,137,255,43,67,67,67,66,67,67,67,42,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,18,19,210,233,208,255,234,22,161,67,67,67,66,65,66,66,67,67,67,67,67,66,12,22,66,66,66,65,67,90,91,67,66,67,67,67,67,67,67,67,67,66,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,17,17,18,21,22,209,233,136,234,137,66,91,30,90,89,30,91,67,67,67,67,42,65,67,67,91,91,68,91,15,92,92,92,67,68,67,68,92,91,91,67,66,66,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,20,161,136,209,43,42,42,66,12,65,65,88,30,91,67,67,67,67,12,12,66,67,92,92,92,92,15,15,92,92,92,92,92,92,92,91,67,67,14,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,209,210,43,43,66,67,66,236,21,160,12,65,66,67,67,91,91,66,66,67,91,92,92,92,91,92,92,91,68,91,91,91,92,67,67,43,43,43,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,208,136,65,66,43,65,255,137,209,210,65,67,67,67,67,91,15,15,67,91,92,92,92,67,67,67,66,66,67,67,92,67,67,66,66,43,43,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,208,17,233,42,67,67,67,67,92,92,15,92,92,92,92,67,67,43,66,66,255,43,67,67,66,42,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,91,92,92,92,15,92,92,92,67,67,67,67,67,66,255,67,42,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,66,92,67,91,92,67,92,91,67,67,67,67,66,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,67,67,67,91,68,68,67,43,66,43,65,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,67,67,92,92,92,67,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,67,92,92,92,92,67,42,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,43,67,92,92,91,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,65,66,91,67,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,14,42,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,65,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,0
horse4:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,90,255,255,255,255,255,255,255,255,255,185,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,255,255,43,67,43,91,255,255,255,255,186,186,114,186,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,42,66,66,67,92,67,66,42,43,255,255,185,186,186,186,185,186,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,26,164,67,67,42,43,66,43,66,66,92,92,91,67,43,255,255,137,186,185,185,185,17,186,185,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,65,66,67,67,43,43,67,67,92,68,92,92,67,43,43,43,255,114,185,185,185,186,209,20,210,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,66,67,66,65,67,67,67,66,66,67,67,92,91,92,92,66,66,67,67,42,113,113,186,136,243,146,147,148,255,255,255,255,255,255,255,255,255,255,89,90,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,65,66,67,65,12,66,66,65,66,66,66,67,67,67,67,92,92,67,67,67,67,65,6,6,6,25,171,148,148,148,148,149,255,255,255,255,255,255,66,90,90,90,66,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,42,66,65,25,8,6,6,64,66,66,66,66,67,67,67,67,92,91,91,67,67,43,67,66,66,42,12,64,24,148,148,3,74,3,255,255,255,255,89,90,90,90,90,65,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,25,64,65,65,21,21,21,160,65,67,66,66,67,67,66,67,67,91,92,92,67,67,14,66,67,67,66,65,25,172,148,148,26,75,74,74,255,66,90,90,90,89,66,90,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,24,24,24,12,12,22,8,21,233,136,6,65,65,65,65,65,67,66,65,66,67,67,67,67,67,66,67,66,64,171,25,148,3,74,74,74,27,90,90,90,66,89,255,255,255,255,255,255,255,255,255,42,66,42,66,43,66,43,254,255,255,255,255,255,255,20,24,25,24,8,22,21,21,233,18,18,18,209,233,234,21,161,64,67,65,65,64,66,67,91,67,66,43,66,66,25,172,3,172,3,74,74,29,66,65,65,255,255,255,255,255,255,255,255,255,255,43,43,67,67,67,67,91,67,254,255,255,255,255,255,20,25,25,24,21,20,20,19,19,209,18,18,18,18,18,19,234,21,161,65,66,66,65,67,66,67,91,67,43,43,43,30,28,74,147,3,3,74,28,25,161,233,19,18,255,255,255,255,255,67,66,66,43,66,67,67,67,67,91,91,254,255,255,255,255,255,22,24,24,22,19,18,19,19,18,18,18,18,18,18,17,18,18,19,234,21,25,66,67,67,66,67,91,67,66,67,66,25,28,172,219,148,74,75,74,23,23,163,162,161,20,136,163,42,67,66,43,66,66,67,67,67,67,67,67,67,254,255,255,255,255,255,21,21,20,18,17,18,18,17,18,18,18,17,18,18,17,17,18,19,19,21,163,25,64,26,65,65,66,64,67,91,66,28,28,21,235,172,29,29,7,25,25,25,24,24,161,137,6,67,67,67,66,66,66,67,91,91,91,67,67,67,254,255,255,255,255,17,19,19,18,18,17,17,255,255,255,255,255,18,18,18,17,17,18,18,19,21,163,25,25,26,26,26,25,26,66,67,27,28,26,24,163,163,25,7,28,26,25,25,25,24,8,21,137,67,91,67,65,65,65,67,67,67,67,67,67,66,254,255,255,255,255,255,18,18,18,18,255,255,255,255,255,255,255,255,255,19,18,18,18,18,19,21,163,25,25,25,25,24,24,25,25,25,26,26,24,24,24,24,162,139,6,164,24,23,8,21,21,137,6,67,92,66,6,42,65,67,67,67,67,66,43,43,254,255,255,255,255,255,255,19,17,255,255,255,255,255,255,255,255,255,255,255,20,18,18,18,19,21,23,25,25,24,163,24,24,163,24,162,24,25,24,24,23,8,21,19,18,137,23,8,21,20,19,232,136,66,67,66,255,42,42,43,67,67,67,43,42,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,20,21,8,24,24,8,22,8,8,22,8,8,163,24,23,8,22,22,21,19,18,136,22,20,20,19,19,19,136,136,137,255,255,255,255,255,255,66,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,20,21,21,21,21,22,22,22,22,21,21,22,8,160,22,21,21,21,20,19,18,19,19,19,19,233,19,19,209,19,19,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,20,20,235,235,21,21,21,21,236,20,21,22,8,22,21,236,20,19,19,19,18,18,20,234,234,20,19,210,208,209,19,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,235,234,20,21,21,21,20,20,19,19,20,235,20,20,19,19,18,18,18,18,18,20,20,234,234,234,19,209,209,209,19,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,19,19,20,20,20,235,21,21,19,18,18,18,19,19,19,18,18,18,18,18,18,17,19,235,20,20,19,19,18,18,209,19,19,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,19,19,20,20,19,19,19,208,208,18,18,208,18,208,208,208,208,209,18,17,19,20,234,19,19,208,18,18,208,19,21,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,19,18,18,19,22,20,18,18,18,209,209,208,18,17,18,209,136,113,209,209,209,208,18,19,209,18,18,18,208,136,210,209,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,17,18,19,21,20,18,18,208,136,136,136,114,209,136,136,136,137,136,210,210,17,19,19,18,18,209,136,136,136,209,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,18,208,233,19,19,18,18,136,136,136,136,137,137,136,137,6,137,136,138,255,18,136,234,18,208,209,136,136,209,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,18,18,18,208,209,210,234,19,19,19,209,136,209,208,137,136,136,137,137,136,255,255,255,255,18,235,20,19,18,208,210,209,209,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,18,18,208,208,209,163,20,19,19,209,210,138,255,255,255,255,255,255,255,255,255,255,255,255,19,136,233,209,208,209,114,208,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,209,208,208,209,255,20,20,19,18,209,138,255,255,255,255,255,255,255,255,255,255,255,255,255,6,136,19,209,209,136,114,113,18,18,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,17,18,209,255,255,255,20,20,19,18,209,6,255,255,255,255,255,255,255,255,255,255,255,255,255,12,6,209,208,209,136,113,113,208,209,17,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,17,19,255,255,255,21,21,19,18,136,255,255,255,255,255,255,255,255,255,255,255,42,67,65,255,136,209,18,113,136,114,114,209,208,186,255,255,255,255,42,43,43,43,42,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,209,209,255,255,255,20,8,22,136,209,17,255,255,255,255,66,43,43,42,255,66,43,92,67,65,137,210,208,136,42,6,114,136,208,208,255,255,255,65,67,91,67,66,67,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,19,209,17,255,255,255,18,20,233,234,21,137,163,163,6,65,66,43,43,67,67,66,91,92,92,67,66,163,20,12,67,67,42,136,136,210,140,255,66,67,92,91,67,43,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,21,234,22,255,255,255,137,210,18,18,234,22,22,23,65,67,66,67,67,67,90,90,15,30,92,92,67,64,235,6,67,67,67,67,67,66,43,65,67,67,92,67,66,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,22,136,136,43,67,66,43,66,65,160,42,67,66,21,6,65,67,67,67,67,66,67,91,30,91,67,67,66,12,42,67,67,67,67,68,67,14,67,91,91,91,43,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,210,65,67,67,67,66,88,87,29,66,91,91,66,66,67,67,67,67,67,66,65,66,66,66,42,66,67,67,66,67,67,67,67,67,66,91,92,92,92,91,43,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,209,66,66,67,66,90,65,86,29,255,67,67,67,14,43,43,43,67,67,12,137,65,66,42,6,43,67,67,66,67,91,92,91,67,67,67,92,68,91,68,43,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,208,209,65,66,67,67,15,65,255,255,255,42,67,67,43,66,255,42,43,65,139,12,14,67,67,67,43,43,67,67,92,92,92,92,92,92,91,92,68,67,66,66,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,67,91,30,65,255,255,255,255,43,43,42,255,255,255,255,164,66,67,67,66,67,92,67,43,91,67,92,92,92,92,92,92,92,92,92,66,42,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,66,66,255,255,255,255,255,255,255,255,255,255,255,64,163,64,66,66,66,66,90,66,67,43,43,67,67,68,67,91,92,91,92,67,66,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,161,65,65,66,66,66,65,88,255,255,255,255,91,43,42,67,91,67,67,91,43,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,6,66,67,67,67,67,66,255,255,255,255,255,43,43,65,67,91,91,91,67,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,65,43,66,43,43,255,255,255,255,43,43,65,66,91,92,91,91,66,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,65,66,67,67,67,67,66,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,67,67,66,91,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,255,255,42,42,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,0
horse5:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,43,14,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,67,43,255,255,255,255,255,255,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,92,92,43,255,255,255,255,185,114,113,186,113,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,43,67,92,92,66,66,255,255,185,185,186,186,186,186,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,67,67,92,92,67,66,43,43,255,136,186,185,185,185,185,186,185,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,6,66,65,42,43,43,43,66,43,14,67,67,67,92,92,67,43,43,67,6,113,185,185,185,185,208,210,186,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,65,67,66,66,67,67,67,67,67,67,67,67,67,92,92,67,66,67,42,6,186,186,186,209,238,146,147,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,43,42,65,66,66,66,66,65,66,67,67,67,67,67,67,67,92,92,92,67,67,65,114,114,6,139,24,148,148,148,147,149,255,255,255,255,255,255,255,29,89,90,90,89,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,7,66,66,65,64,164,66,66,65,67,67,67,66,67,67,67,67,91,67,68,67,66,255,67,65,65,65,25,164,148,148,148,148,3,255,255,255,255,255,90,90,90,90,90,66,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,25,65,65,23,8,21,64,66,67,65,65,66,66,65,66,67,67,67,91,67,66,66,66,67,67,67,66,164,26,172,172,3,74,74,74,255,255,89,90,90,90,90,66,65,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,207,24,162,12,43,12,23,22,235,6,12,6,12,12,65,64,26,65,67,67,67,67,67,67,67,67,67,66,65,165,172,148,148,3,74,74,74,74,90,90,90,66,66,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,25,24,24,163,22,21,22,20,18,18,18,18,19,19,21,22,161,66,67,66,66,66,67,67,67,67,43,43,91,25,146,172,26,3,74,74,74,29,90,89,66,65,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,24,24,24,8,21,236,20,20,19,18,18,18,18,18,18,19,19,235,138,65,66,66,67,66,66,91,91,67,6,138,6,255,26,3,148,3,3,3,28,65,26,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,21,25,25,23,19,19,20,19,18,209,18,18,18,18,18,17,18,18,19,234,161,65,65,65,65,67,91,67,66,67,66,42,28,74,148,147,3,74,75,7,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,65,66,254,255,255,255,20,21,22,22,19,18,18,18,18,18,208,18,18,18,18,18,18,17,18,18,19,22,24,65,25,25,26,26,64,65,67,67,164,28,27,146,147,148,29,29,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,66,43,14,67,254,255,255,255,21,19,19,19,208,18,18,19,18,18,17,255,17,17,18,17,17,17,18,18,19,22,25,25,26,26,25,25,25,25,66,66,7,74,25,18,19,234,12,65,66,7,24,21,21,20,19,234,210,255,255,255,255,255,43,14,67,67,14,14,14,67,254,255,255,255,255,18,18,17,18,244,255,255,255,255,255,255,255,255,255,17,17,17,17,18,19,22,24,25,25,25,24,24,24,24,25,24,25,26,147,8,23,22,234,137,6,163,162,24,24,23,23,8,22,21,160,42,66,67,67,67,67,67,67,67,67,67,254,255,255,255,255,17,17,255,17,255,255,255,255,255,255,255,255,255,255,255,255,17,18,209,235,8,24,25,24,163,163,24,163,23,162,163,24,24,8,23,23,23,22,19,209,161,25,25,25,25,24,24,23,22,137,12,66,67,66,66,67,67,67,67,67,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,19,21,22,8,161,8,22,22,22,22,22,161,162,23,24,24,23,23,23,8,235,19,22,25,25,25,25,25,24,23,8,138,12,65,66,65,65,66,67,67,67,66,66,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,20,20,21,21,21,21,21,21,22,21,21,21,22,8,23,23,23,8,22,22,22,21,20,21,23,23,23,23,162,8,22,21,137,139,67,91,67,67,67,67,67,67,66,42,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,20,21,21,21,22,21,21,21,20,20,21,22,8,22,22,21,21,21,21,21,21,20,19,21,22,8,8,22,21,20,234,234,137,66,92,91,66,66,42,43,43,42,12,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,20,20,21,21,22,21,20,19,19,19,20,21,21,21,21,21,21,21,21,20,20,20,19,20,21,21,21,20,20,19,19,233,137,12,66,42,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,18,19,233,234,22,21,21,20,19,18,18,18,19,19,234,20,20,20,19,20,19,19,19,19,19,20,20,20,20,20,19,19,19,19,209,136,140,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,19,18,234,160,23,21,19,19,18,18,18,18,18,18,19,19,209,19,18,18,18,18,18,18,18,19,20,20,20,19,19,19,19,18,209,209,208,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,18,18,18,18,234,21,21,234,18,18,18,209,210,208,209,208,18,18,208,208,18,18,18,18,18,18,17,18,20,20,20,19,19,19,19,209,208,209,208,17,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,19,18,18,18,18,233,21,21,20,19,18,18,209,136,136,209,209,209,209,209,209,209,208,208,18,17,18,18,18,18,235,235,20,19,19,19,19,209,18,209,208,18,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,19,18,18,208,210,21,21,20,19,209,210,136,136,137,137,136,136,137,136,136,136,136,136,209,209,209,209,208,208,209,234,20,20,19,19,18,18,18,18,209,209,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,18,18,208,136,160,21,233,19,209,210,136,137,136,137,163,162,138,137,137,136,136,136,136,210,234,136,233,208,208,209,19,19,19,18,18,18,18,18,18,18,210,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,19,18,18,208,209,162,24,23,233,209,208,210,6,255,255,255,255,255,255,255,255,255,163,136,136,234,136,136,138,255,185,18,234,233,18,18,18,18,208,208,208,18,18,209,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,17,19,19,208,209,137,22,23,235,18,208,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,21,21,20,18,18,208,208,208,208,208,18,18,233,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,21,21,209,208,6,231,235,233,209,208,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,209,137,21,21,19,18,208,208,208,208,208,208,18,18,18,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,230,8,162,234,208,185,255,255,19,18,18,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,66,6,235,138,6,209,209,209,208,208,208,208,18,18,208,234,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,20,8,21,209,184,255,255,255,20,19,208,208,255,255,255,255,255,255,255,255,255,67,255,42,66,42,255,255,255,255,255,255,255,43,65,234,42,66,42,42,6,136,209,209,208,18,18,209,233,136,210,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,19,137,42,42,255,255,255,255,21,20,234,164,255,255,255,255,255,255,255,255,42,43,43,67,67,66,255,255,255,255,255,255,255,66,67,65,42,67,67,67,43,6,136,210,209,209,209,209,233,234,234,235,255,255,255,254,255,255,255,255,255,255,255,255,255,255,160,66,67,67,66,43,67,42,139,235,65,66,65,44,255,255,255,255,255,43,14,67,68,92,92,67,65,255,255,255,255,255,255,43,67,42,42,67,91,67,65,12,255,136,232,208,209,136,136,233,19,19,255,255,255,254,255,255,255,255,255,255,255,255,255,255,12,67,67,67,67,43,66,67,89,161,66,67,67,43,43,42,42,66,43,14,43,66,91,92,92,68,67,255,255,255,255,255,255,12,12,234,6,66,66,65,255,255,255,255,255,17,208,136,210,136,232,20,255,255,255,254,255,255,255,255,255,255,255,255,255,255,65,66,66,67,67,66,90,30,89,137,66,91,92,67,67,43,43,43,67,14,43,43,91,92,92,14,43,255,255,255,255,255,255,160,8,234,136,137,255,255,42,43,43,42,255,255,255,255,209,209,209,19,19,255,255,254,255,255,255,255,255,255,255,255,255,114,12,67,67,67,66,65,87,87,66,67,67,67,67,91,90,66,66,43,67,43,67,43,68,91,67,43,43,255,255,255,255,255,22,138,22,234,137,42,43,43,67,91,67,43,255,255,255,255,6,137,136,19,20,255,255,254,255,255,255,255,255,255,255,255,255,255,42,66,67,43,66,255,255,255,43,67,67,65,66,91,15,88,255,66,67,43,255,67,67,91,67,43,255,255,255,255,255,255,163,160,234,209,137,43,67,67,67,67,66,42,255,255,255,255,255,139,210,209,232,137,255,254,255,255,255,255,255,255,255,255,255,255,255,66,42,255,255,255,255,255,42,67,67,65,67,30,30,255,255,66,43,66,255,255,66,91,67,255,255,255,255,255,255,162,163,6,42,6,42,67,67,67,68,43,91,255,255,255,255,255,42,43,42,136,233,65,42,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,65,43,66,67,91,66,255,255,43,43,255,255,255,43,66,255,255,255,255,255,164,164,163,67,67,66,43,43,67,67,92,68,66,255,255,255,255,255,255,43,67,43,12,160,67,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,67,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,25,66,66,12,67,67,67,65,255,42,67,67,67,42,255,255,255,255,255,43,67,67,67,67,12,66,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,162,66,67,67,67,67,91,66,42,255,255,255,42,15,255,255,255,255,255,255,67,67,67,92,91,65,67,67,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,7,64,66,67,66,67,91,67,15,255,255,255,255,255,255,255,255,255,255,255,66,67,92,92,92,67,65,67,91,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,65,66,67,67,67,67,67,43,255,255,255,255,255,255,255,255,255,255,255,255,42,68,91,67,92,66,66,66,90,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,26,66,67,66,67,67,67,67,67,42,255,255,255,255,255,255,255,255,255,255,255,255,91,67,67,67,67,67,65,66,91,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,162,12,65,65,67,67,67,67,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,66,66,43,42,43,66,65,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,42,43,43,42,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,0
horse6:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,6,15,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,66,67,91,14,65,92,42,255,255,255,114,186,185,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,66,92,92,67,67,67,42,42,6,186,113,186,186,115,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,92,91,43,66,42,43,66,113,185,186,186,185,186,186,185,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,67,68,92,67,66,43,65,15,42,113,185,185,185,185,186,186,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,66,67,91,91,92,92,67,67,66,67,43,6,185,186,209,238,145,25,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,66,66,67,67,91,92,92,92,92,91,67,67,66,6,136,138,147,148,148,148,148,255,255,255,255,255,255,255,255,66,89,66,90,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,65,43,66,66,67,67,67,67,67,92,92,92,92,92,92,67,67,66,65,42,164,164,170,148,148,3,26,255,255,255,255,255,90,90,90,90,90,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,91,42,66,67,67,67,67,67,67,67,67,67,92,92,92,92,92,91,67,66,67,67,66,65,164,24,148,148,3,74,74,255,255,255,66,90,90,90,66,66,140,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,65,66,67,67,65,66,67,67,67,67,67,67,67,67,67,91,91,67,91,92,67,14,67,66,25,25,172,148,148,74,74,74,3,90,90,90,90,90,66,65,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,42,65,65,65,65,164,12,64,66,67,65,64,66,67,67,67,67,67,67,67,91,92,91,66,92,255,27,170,26,27,26,3,74,74,7,90,90,66,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,208,25,164,65,65,163,21,21,137,65,66,66,65,66,66,64,66,67,65,65,67,67,67,67,91,67,66,65,164,172,26,28,172,3,3,74,28,66,64,6,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,24,25,26,64,65,12,8,21,21,12,65,64,6,12,161,161,64,66,67,67,67,67,67,67,67,67,67,42,137,23,74,74,172,148,3,74,74,26,255,255,255,19,255,19,20,19,19,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,24,26,25,25,163,22,21,22,234,19,209,18,18,18,18,19,234,236,22,65,66,65,67,67,66,66,66,66,67,65,26,98,148,244,169,7,29,29,28,23,22,22,21,21,22,8,23,23,8,21,162,26,255,43,43,43,43,43,43,66,43,43,254,255,255,23,25,26,24,21,20,236,20,19,18,18,18,18,18,18,18,18,18,18,19,234,8,24,65,66,65,25,65,66,67,66,28,74,170,21,23,24,164,140,65,163,163,24,24,25,25,25,25,25,24,163,8,160,65,67,67,67,67,67,67,67,67,67,254,255,21,21,24,8,21,18,19,20,19,18,18,18,18,18,18,18,18,17,18,18,18,19,22,25,25,25,26,26,25,25,26,25,26,26,25,25,25,25,24,137,6,164,25,24,25,25,25,25,25,25,25,24,160,137,12,65,66,65,66,66,67,67,67,66,254,255,21,20,21,210,209,17,209,18,18,209,209,208,18,18,18,18,17,17,17,18,18,19,21,24,25,25,25,25,24,24,25,25,25,25,25,25,25,25,24,8,21,8,24,24,25,25,24,24,24,24,23,22,137,137,137,65,67,43,66,67,67,67,67,66,254,255,20,18,18,18,208,18,137,18,18,209,209,18,209,208,209,209,208,18,18,17,18,19,236,162,25,25,24,24,24,24,24,163,163,24,24,25,24,25,24,24,23,8,8,8,8,23,8,8,8,22,21,20,234,234,137,12,66,43,43,43,66,67,66,42,254,255,255,17,18,17,255,255,255,255,255,255,255,255,255,255,255,255,209,209,209,18,18,233,21,8,24,24,23,8,162,162,8,8,8,8,163,24,24,24,24,23,8,22,22,21,236,21,21,22,21,21,20,20,20,234,137,137,137,255,255,255,255,42,42,42,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,210,18,18,19,21,22,161,161,22,22,22,21,21,21,22,22,22,22,22,22,22,22,22,21,21,20,20,20,20,236,20,20,20,20,20,234,136,137,136,18,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,232,18,19,235,20,234,21,22,22,22,21,21,21,22,22,22,21,21,21,21,21,236,20,20,19,19,20,20,20,20,19,20,20,235,234,234,234,136,18,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,20,20,20,20,21,21,236,235,20,20,235,21,21,21,235,20,20,20,19,19,19,19,19,19,19,19,19,20,19,20,20,20,234,234,232,232,18,18,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,20,234,20,20,21,21,20,19,18,18,19,19,19,19,19,19,19,19,18,18,18,18,18,18,18,18,19,19,19,20,20,20,19,209,209,209,208,209,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,21,20,234,21,21,20,20,19,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,209,208,208,209,18,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,24,23,161,23,21,18,18,18,18,208,208,209,209,209,209,208,18,18,18,18,208,18,209,209,209,208,208,209,233,19,19,19,18,18,208,210,208,18,18,18,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,24,24,23,8,22,19,18,208,208,209,209,209,209,209,209,209,208,208,208,209,209,208,18,209,234,209,208,208,210,209,19,18,18,18,208,208,209,18,208,210,234,19,210,18,209,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,23,25,24,23,21,20,234,209,208,209,136,136,136,210,136,209,209,209,209,209,209,209,19,19,18,209,255,19,18,208,209,208,18,18,18,18,208,18,17,18,208,233,21,21,235,19,209,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,8,24,24,22,19,210,136,136,136,136,137,137,136,136,137,136,234,233,234,233,234,235,234,114,255,255,255,255,255,255,255,18,18,18,18,18,18,208,18,208,18,18,18,234,22,162,21,19,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,21,233,136,136,136,209,114,136,137,163,137,208,255,255,255,17,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,233,234,19,209,18,18,208,18,18,18,19,234,233,209,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,235,210,136,137,140,209,208,209,209,136,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,18,19,232,19,18,208,234,210,18,18,209,18,19,209,18,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,22,235,136,113,255,234,21,136,209,137,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,18,209,18,18,18,18,17,255,255,255,255,255,18,17,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,8,22,136,114,6,137,65,162,136,6,66,14,66,255,6,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,17,209,210,18,18,18,20,18,255,255,255,255,255,17,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,164,24,160,136,42,255,6,91,67,6,66,67,68,14,43,66,66,65,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,232,136,18,19,235,234,21,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,23,162,137,42,43,66,66,92,91,66,67,67,67,67,67,91,67,66,67,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,210,18,235,21,234,19,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,229,137,42,67,67,91,67,92,92,67,67,91,91,91,92,92,67,67,42,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,19,234,18,18,18,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,161,65,91,91,67,91,91,92,92,91,68,91,67,67,92,92,68,14,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,18,20,234,18,17,19,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,42,65,66,67,67,67,67,67,67,67,67,67,91,67,67,67,91,91,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,22,22,210,209,17,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,43,66,65,67,67,91,91,67,67,67,67,67,67,43,43,43,67,67,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,66,43,64,22,234,18,17,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,64,67,67,67,67,66,66,67,66,255,255,255,42,255,255,43,67,43,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,43,67,65,12,42,137,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,67,67,66,66,66,66,255,255,255,255,255,255,255,42,42,91,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,65,43,66,65,164,66,67,43,42,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,66,67,66,66,67,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,66,66,65,66,67,66,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,12,66,66,67,66,90,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,66,43,67,91,67,67,67,43,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,164,66,67,67,91,15,66,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,65,67,92,67,91,67,67,67,66,43,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,65,43,66,90,87,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,65,67,92,67,67,91,67,67,67,66,15,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,43,66,67,66,66,67,67,67,91,66,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,42,43,42,67,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,0

road:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,29,29,29,29,29,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,29,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,29,30,30,30,30,15,15,15,30,30,30,30,30,30,15,15,15,15,15,15,15,15,15,30,30,29,29,29,28,28,28,28,28,28,28,28,254,28,7,7,7,7,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,7,7,7,7,7,7,7,7,7,7,7,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,7,7,7,7,7,7,7,7,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,29,29,29,29,28,28,28,29,29,29,29,28,28,28,28,29,29,29,29,29,29,29,29,29,29,29,28,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,254,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,254,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,254,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254
roadsize:dw 1551

fence:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,15,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,25,15,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,29,15,30,23,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,15,30,30,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,26,15,30,30,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,29,30,30,30,30,23,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,15,30,30,30,30,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,26,30,30,30,30,30,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,28,30,30,30,30,30,29,22,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,28,30,30,30,30,30,29,22,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,28,30,30,30,30,30,29,22,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,30,30,30,30,30,30,29,27,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,254,15,30,30,30,30,30,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,29,30,30,30,30,30,29,26,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,254,26,30,30,30,30,30,29,22,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,29,30,30,30,30,30,29,7,26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,254,30,30,30,30,30,30,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,28,30,30,30,30,30,29,25,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,26,30,30,30,30,30,29,21,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,7,30,30,30,30,30,29,25,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,30,30,30,30,30,30,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,30,30,30,30,30,30,29,7,27,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,254,26,30,30,30,30,30,28,8,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,26,29,30,30,30,30,29,22,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,27,29,30,30,30,30,28,22,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,7,30,30,30,30,29,28,21,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,26,26,26,25,25,24,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254
fencesize:dw 1023

mountain: db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,91,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,91,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,91,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,92,91,90,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,91,27,91,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,92,91,91,29,25,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,92,92,91,91,29,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,92,92,91,90,29,172,24,29,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,92,92,91,30,30,30,29,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,92,91,30,90,29,29,25,7,7,28,7,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,15,15,92,92,92,92,91,91,30,30,30,29,7,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,92,92,91,91,30,29,29,28,27,26,7,7,7,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,15,15,92,92,92,92,91,30,91,30,30,29,28,28,7,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,91,91,91,91,30,30,91,30,30,28,172,26,7,7,28,28,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,15,92,92,92,92,30,30,30,30,29,28,7,7,7,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,90,91,30,30,91,30,90,29,29,25,27,28,27,24,24,28,27,7,29,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,15,15,92,92,92,30,30,30,30,30,30,28,172,24,27,28,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,30,255,255,255,255,255,255,255,255,28,29,29,29,29,30,29,26,29,28,28,30,24,172,7,28,26,173,28,28,29,30,29,30,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,30,30,91,91,91,30,30,30,29,30,30,28,174,174,26,29,7,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,7,91,91,30,30,30,30,255,255,255,28,27,29,29,28,29,29,29,25,25,25,24,7,7,7,28,28,29,173,28,30,29,29,28,28,29,30,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,30,91,91,91,91,30,29,29,29,30,30,29,25,246,173,29,28,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,91,91,91,91,91,30,30,30,29,255,172,26,28,7,7,28,7,28,7,27,172,172,172,172,26,28,29,29,28,29,29,29,28,7,29,29,29,30,30,91,91,255,255,255,255,255,30,30,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,30,30,30,30,90,29,29,29,29,29,30,29,29,30,29,7,25,7,29,29,29,28,7,26,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,91,91,90,29,29,26,29,30,29,29,7,172,27,7,27,7,7,28,7,7,28,26,172,172,172,172,28,29,29,29,29,30,29,30,29,29,29,29,30,30,90,27,91,255,255,255,30,30,30,30,30,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,30,30,30,30,30,29,29,28,29,29,29,29,7,29,30,29,29,7,173,27,29,29,7,7,28,27,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,7,30,29,26,29,29,25,28,29,7,7,27,26,28,27,27,27,29,29,27,7,7,172,173,173,173,149,25,29,29,29,29,30,30,30,29,29,28,29,29,30,29,26,26,27,91,30,30,30,30,29,29,29,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,30,30,29,29,29,28,28,29,29,29,30,29,28,29,29,30,29,28,173,7,28,7,7,28,29,28,26,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,91,30,30,25,7,29,7,27,29,29,172,173,27,27,172,27,172,28,29,27,28,172,173,173,173,172,172,26,29,29,29,29,30,30,30,28,28,29,29,28,29,7,7,26,26,29,29,29,30,30,30,30,29,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,30,29,30,29,29,29,28,7,28,29,29,30,30,30,29,29,29,29,29,26,174,25,7,28,28,28,29,28,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,29,30,29,28,7,28,26,172,7,28,28,173,26,27,26,27,25,173,29,29,28,7,174,174,27,7,28,28,30,29,29,29,29,29,29,29,29,29,29,29,29,29,27,25,27,7,7,25,29,30,29,29,29,29,29,28,255,255,255,255,255,255,255,255,255,255,255,255,255,91,255,255,255,27,28,29,29,29,28,28,7,7,28,29,29,28,28,29,30,30,29,29,29,29,29,26,26,28,28,28,29,28,28,7,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,90,26,29,29,29,29,25,26,7,173,27,7,26,27,7,7,26,29,7,173,172,29,28,27,172,7,28,29,28,7,28,29,28,28,29,28,29,29,29,29,29,29,29,29,29,28,27,26,29,29,28,28,29,29,29,29,28,28,29,255,255,255,255,255,255,255,255,255,255,30,30,91,91,30,29,26,25,28,29,7,7,7,7,7,28,30,29,26,26,28,30,29,29,29,29,29,29,29,27,28,28,7,28,28,28,28,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,26,28,29,29,29,29,7,173,28,26,26,7,27,26,27,27,172,7,7,173,173,28,7,7,26,28,7,27,26,27,7,28,28,28,28,7,28,7,7,28,28,28,29,29,29,30,29,29,30,29,29,29,29,28,28,28,28,28,29,28,28,255,255,255,255,255,255,30,30,30,30,30,29,28,29,28,28,28,26,26,27,26,172,7,29,30,30,25,27,29,29,29,29,29,29,29,29,29,172,27,28,27,28,28,28,28,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,30,30,28,25,29,29,29,29,29,7,172,28,29,27,7,28,172,172,26,25,26,29,7,7,7,28,27,26,27,7,172,172,28,29,28,28,28,28,7,7,7,26,26,28,28,29,29,29,29,30,29,29,29,29,29,29,29,28,28,29,29,28,28,28,28,28,28,29,29,29,29,30,29,29,29,28,28,28,27,7,7,172,26,27,172,173,27,7,29,30,7,27,28,28,30,29,29,29,29,29,29,26,172,7,27,7,29,7,28,7,28,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,29,29,28,24,27,29,29,28,28,29,27,172,7,7,7,28,27,173,172,174,174,173,28,29,27,172,7,27,27,26,28,101,172,28,29,28,28,7,28,28,7,28,28,173,28,29,28,28,29,29,29,29,29,29,29,29,29,29,28,28,28,28,29,29,28,28,28,28,29,29,28,29,29,29,29,28,28,28,7,7,26,172,26,27,27,26,27,28,28,29,30,28,24,29,30,29,29,29,29,29,29,29,28,173,172,25,27,28,29,28,28,27,26,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,29,29,28,24,172,7,29,29,26,25,26,26,26,26,172,26,7,26,172,25,173,245,173,27,7,172,26,28,28,7,172,7,172,173,28,29,29,29,29,29,28,28,29,28,173,27,29,29,29,28,28,29,28,28,28,28,28,28,29,29,29,26,7,29,29,29,29,28,29,28,28,28,29,29,28,28,7,28,7,7,29,7,26,7,27,7,26,27,28,28,29,29,7,27,30,29,29,29,29,29,29,29,29,28,26,172,26,27,28,28,7,27,26,25,27,7,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,30,29,28,28,172,172,27,7,173,174,173,174,150,173,173,174,174,7,7,172,27,7,173,26,172,7,27,26,7,28,28,27,27,172,26,29,29,29,29,29,29,29,29,28,28,25,27,29,29,29,29,28,28,28,7,7,28,29,28,29,28,29,29,29,29,28,29,29,28,29,29,28,28,28,7,7,28,28,28,27,26,28,29,101,27,27,7,26,26,7,29,29,29,29,30,29,29,29,29,29,29,29,29,29,28,7,7,7,7,26,25,173,28,28,28,29,28,28,25,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,30,29,29,28,7,172,173,173,173,173,149,173,173,149,221,173,174,149,173,27,173,174,7,172,172,173,26,27,27,28,7,28,7,27,173,28,29,29,29,29,29,29,29,29,28,26,26,24,27,29,28,28,7,28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,28,29,28,28,7,7,28,28,28,28,27,25,7,28,7,27,27,27,173,7,29,28,28,29,29,29,29,29,29,29,28,28,29,29,28,7,28,29,28,7,26,173,24,28,29,29,29,29,26,25,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,7,7,29,255,255,255,255,255,255,255,255,255,255,91,91,27,26,29,28,7,7,7,173,27,174,149,172,173,221,173,173,150,174,173,172,174,173,26,173,26,172,150,174,28,7,26,28,26,173,173,27,29,30,30,29,29,29,29,28,28,29,28,173,173,25,25,28,28,28,28,28,29,28,28,28,28,28,7,28,28,29,29,29,29,29,29,29,28,29,28,28,7,28,28,7,28,28,27,7,28,7,7,7,7,27,173,27,28,28,27,7,28,29,29,29,29,29,29,29,29,7,172,26,7,29,29,28,27,28,28,28,29,29,29,28,26,27,27,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,26,26,26,92,255,255,255,255,255,255,255,255,91,27,25,26,28,28,29,7,7,27,26,29,27,26,172,7,172,172,173,149,174,173,7,26,173,7,28,7,28,173,173,7,7,28,28,26,174,172,28,29,29,29,29,28,28,28,28,28,28,28,25,26,29,26,28,29,29,28,28,29,28,7,7,7,29,29,28,28,28,29,29,29,28,28,29,28,28,28,7,7,7,7,7,28,28,28,28,7,7,7,27,7,27,172,172,27,28,28,28,29,28,29,29,29,29,28,101,101,26,149,27,28,29,29,29,26,28,28,29,29,29,28,7,26,25,27,27,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,7,92,26,7,91,91,255,255,30,30,91,91,30,30,29,26,25,28,28,172,7,7,7,7,7,28,28,173,173,172,7,27,172,173,221,26,7,7,173,173,7,172,7,28,7,27,7,29,7,28,27,26,25,28,28,27,28,7,28,28,28,28,28,29,28,28,28,28,7,28,28,28,29,29,29,28,28,28,29,29,28,28,29,29,29,29,28,28,28,28,28,7,7,7,7,7,28,28,28,7,7,7,7,7,7,7,27,26,172,26,7,27,26,28,28,28,28,28,28,28,28,28,172,172,28,29,28,29,29,27,28,28,7,28,28,28,28,7,28,28,7,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,7,92,91,26,91,91,255,91,30,30,29,30,29,29,25,25,29,29,27,26,7,27,7,7,26,7,29,172,172,26,28,27,172,27,172,7,7,7,29,7,27,172,173,7,7,7,28,29,29,7,27,26,173,173,7,26,27,7,28,28,28,7,7,29,30,28,28,29,28,26,7,29,29,29,29,29,29,29,29,28,28,29,29,7,29,29,28,28,28,28,28,7,27,7,7,7,28,28,7,7,7,7,7,7,7,28,27,172,26,7,27,27,25,29,28,28,28,28,28,7,7,27,172,27,29,29,29,29,29,29,29,29,29,28,7,26,26,29,30,29,7,28,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,91,91,30,91,27,90,29,29,30,29,29,29,29,29,24,7,29,7,7,27,7,7,7,26,173,172,7,27,174,172,28,7,28,29,7,28,7,27,29,7,101,7,172,27,7,172,28,29,29,29,27,26,173,174,26,28,28,28,28,28,28,29,26,26,29,26,29,26,7,25,27,29,29,29,29,29,29,29,29,29,28,28,29,28,28,29,28,28,7,28,7,27,27,27,7,7,28,7,7,7,7,7,7,7,28,29,27,27,7,7,26,26,25,28,29,28,7,7,7,27,27,26,172,27,29,28,28,29,29,29,7,7,28,28,29,26,172,7,29,30,29,29,28,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,91,91,91,91,29,26,29,28,29,29,28,29,28,29,28,27,28,28,28,28,28,27,27,27,27,25,149,173,26,173,173,28,7,29,28,25,28,27,28,28,7,28,101,7,26,27,27,29,29,29,29,7,174,172,172,7,29,28,28,28,29,29,29,28,28,29,28,29,7,25,173,25,29,29,28,28,28,29,29,29,7,7,28,29,29,29,28,7,28,28,28,28,7,7,27,7,28,28,7,7,7,7,7,7,7,28,29,28,27,7,7,26,28,28,29,28,7,7,7,27,27,26,27,172,7,28,7,28,28,28,29,25,26,7,29,29,28,28,29,30,30,30,29,29,25,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,91,91,29,29,28,29,28,28,24,27,7,28,7,26,28,28,7,28,28,28,7,7,26,26,7,7,28,26,174,174,27,172,29,29,29,28,25,26,26,29,29,29,29,29,7,27,26,28,29,29,29,29,29,27,172,27,27,28,28,28,29,28,28,29,29,29,30,29,29,30,28,24,172,29,29,29,29,29,29,29,29,28,26,29,29,29,28,7,7,28,7,28,7,27,28,28,28,28,7,7,28,7,7,7,7,28,28,28,28,27,7,27,28,28,28,28,7,7,27,27,27,27,27,27,172,28,28,27,28,28,7,28,29,27,28,29,29,29,29,29,30,30,91,30,29,28,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,30,91,30,30,29,28,29,7,7,27,172,7,7,26,28,7,28,29,29,27,7,28,26,26,7,27,172,173,28,28,173,173,172,7,29,29,29,29,29,26,29,29,26,29,29,29,29,28,7,29,29,30,29,29,29,29,28,26,7,28,28,30,28,28,28,28,28,29,29,29,29,29,30,27,26,28,29,29,29,30,30,30,29,29,28,29,29,7,7,28,7,28,28,29,7,7,28,7,28,28,27,28,28,7,7,27,7,28,28,28,28,7,7,27,7,28,28,28,28,7,7,27,27,27,27,26,26,28,29,7,7,29,29,7,29,28,28,29,29,28,7,29,30,30,91,30,30,29,29,28,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,30,91,29,29,29,7,24,28,27,244,172,28,28,28,25,26,29,29,28,7,28,7,7,7,172,7,26,174,172,28,28,27,172,27,29,30,29,29,30,91,30,30,27,26,29,7,7,29,29,29,29,29,29,30,29,29,30,29,28,28,7,29,29,28,29,28,26,7,29,29,29,29,29,29,28,101,29,29,29,30,30,30,29,29,29,28,28,28,28,28,28,28,28,28,29,29,28,28,27,28,28,27,7,28,7,7,7,28,28,29,28,28,28,27,7,7,28,28,28,28,28,7,7,27,27,27,7,7,29,29,29,26,29,29,29,29,29,28,29,29,28,29,26,29,30,30,30,30,27,26,29,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,30,91,26,29,28,24,172,7,7,25,246,22,28,29,29,25,174,7,29,28,26,7,29,29,28,173,172,7,7,7,7,26,27,7,28,29,29,29,30,91,30,30,27,26,28,29,28,7,7,7,28,28,29,29,30,29,27,7,29,29,28,7,28,28,29,7,28,28,28,29,28,28,28,28,101,28,29,29,30,15,30,30,91,91,29,29,29,28,28,29,29,28,29,29,29,29,29,29,29,7,28,28,26,27,7,7,7,7,28,28,29,29,28,7,27,7,28,28,28,28,28,7,7,27,27,27,27,26,28,28,29,29,29,30,29,28,28,28,26,26,25,7,29,29,29,30,30,30,30,27,8,30,29,28,28,28,27,26,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,30,30,30,90,29,28,24,27,172,27,26,25,21,24,27,29,29,28,172,24,26,7,28,26,28,30,28,172,172,27,28,28,28,7,27,29,7,29,30,30,30,91,30,91,27,26,25,29,7,7,28,28,29,29,28,28,29,7,7,25,24,28,28,28,28,29,29,28,7,29,28,28,28,29,28,29,101,28,101,30,91,91,91,91,91,91,30,29,28,28,7,28,29,29,28,29,29,29,29,29,29,29,28,7,7,26,27,27,26,7,7,28,28,29,28,28,28,7,28,28,28,28,7,7,7,27,27,27,7,27,28,29,29,91,30,91,30,28,29,7,172,173,26,28,29,29,29,30,29,30,30,91,30,91,29,29,7,28,7,26,7,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,91,30,30,30,29,24,172,26,172,25,7,173,24,25,25,7,29,30,27,25,28,29,29,25,28,30,7,8,25,7,28,7,7,28,29,29,28,29,28,29,91,91,30,7,90,29,29,28,28,29,28,29,29,29,29,28,28,7,7,27,26,26,28,27,28,30,26,26,28,28,7,28,29,29,28,101,28,101,29,91,91,91,91,30,91,91,29,29,28,7,7,28,28,28,29,28,29,29,29,29,28,29,29,7,7,27,27,27,26,7,7,28,28,28,28,28,7,7,28,7,28,28,28,28,7,7,27,7,28,7,28,29,90,7,91,30,30,29,29,26,172,7,30,29,29,28,29,29,29,30,30,91,30,91,29,29,28,29,28,27,7,26,7,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,91,30,29,29,7,172,173,172,172,26,7,26,173,7,27,7,29,29,29,29,30,30,28,22,28,30,26,8,173,27,28,7,27,29,29,28,28,29,29,30,91,91,30,27,29,29,29,29,29,28,29,29,29,29,28,7,29,7,7,28,7,27,173,26,29,30,28,7,7,7,7,28,29,28,28,28,29,29,29,91,91,91,91,91,90,29,29,29,28,7,27,7,28,28,29,28,29,29,29,29,29,28,29,28,7,7,27,28,28,28,28,29,28,28,28,28,7,7,7,28,28,28,28,7,7,7,7,28,28,28,29,29,91,91,91,30,30,28,29,26,172,29,29,29,29,29,29,29,29,29,30,91,91,91,29,28,7,29,29,29,29,28,7,7,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,30,30,26,7,28,172,24,174,26,25,28,172,25,29,29,28,29,29,28,29,29,30,26,172,29,30,7,25,173,173,28,26,28,29,29,28,29,28,30,30,30,91,91,29,29,29,29,29,29,28,29,29,29,29,29,28,29,29,7,27,28,29,172,173,7,30,29,29,28,7,28,28,28,7,28,29,29,29,91,91,91,91,91,90,29,29,28,28,28,7,26,28,28,7,29,29,28,28,29,29,29,29,28,29,28,27,7,29,28,28,28,28,28,28,28,28,7,7,28,28,7,28,28,7,7,28,28,28,29,29,29,91,91,91,30,30,29,28,28,7,26,28,29,29,29,29,29,28,29,30,30,91,91,30,29,28,28,28,29,28,28,29,7,7,28,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,30,30,30,30,28,29,7,172,7,27,7,7,29,25,27,28,29,29,29,28,28,26,27,30,29,29,29,29,28,28,173,24,28,7,27,29,29,28,29,28,30,91,91,91,91,28,28,29,29,29,29,7,28,29,29,29,28,7,28,29,7,7,29,29,173,173,7,29,29,101,7,7,28,7,28,29,101,29,29,30,91,91,91,91,91,29,29,28,7,28,7,7,7,7,28,7,7,29,28,28,28,28,29,29,28,29,29,7,7,29,29,28,28,28,28,28,28,7,27,27,7,7,28,28,7,28,28,28,29,29,29,90,91,91,7,91,30,29,28,28,7,7,7,7,7,7,28,7,7,29,29,29,29,91,91,91,29,28,27,7,29,28,7,28,28,29,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,30,30,30,30,30,29,28,172,172,26,7,7,28,29,25,27,26,29,29,29,26,27,7,26,29,29,29,29,28,7,27,172,173,26,29,27,29,29,28,29,30,30,91,91,91,91,27,28,29,28,29,29,29,29,29,29,29,28,7,28,7,7,28,29,28,173,173,27,7,29,28,7,28,28,28,28,101,29,30,90,91,91,91,91,91,91,29,29,28,7,7,7,7,28,7,7,28,7,28,29,28,28,28,29,29,29,28,29,29,28,28,28,28,28,28,28,28,28,27,27,28,7,7,28,7,28,29,29,29,29,30,91,91,91,91,7,30,30,28,28,7,7,27,27,7,27,27,7,28,7,28,7,26,91,30,91,91,29,28,24,7,29,29,29,7,28,28,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,30,30,30,30,30,30,29,28,26,25,173,26,27,7,29,27,24,173,29,29,29,25,27,7,26,29,29,29,29,29,27,173,173,25,28,29,28,30,29,27,30,30,30,30,30,91,29,25,29,29,29,28,27,27,29,30,29,28,29,7,7,7,28,29,29,28,27,25,173,27,101,28,28,28,7,28,29,15,30,91,91,91,91,92,91,91,15,30,28,28,7,27,7,7,28,7,7,28,28,7,28,28,28,29,28,29,29,28,28,29,29,28,28,7,28,28,7,28,28,27,27,28,7,7,7,29,29,29,30,30,91,91,91,91,91,91,27,29,28,28,28,7,7,26,27,7,7,7,27,28,7,7,25,29,30,30,91,91,29,28,26,29,28,29,29,29,28,29,29,28,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,92,30,30,30,30,30,29,29,28,26,26,173,26,7,25,25,29,7,7,29,29,29,29,28,28,28,29,29,29,29,29,25,173,174,26,29,28,7,29,29,29,30,30,91,30,30,91,26,28,7,172,172,173,173,246,26,29,29,28,29,28,7,7,7,28,7,29,30,28,172,28,101,7,7,28,28,29,30,91,91,91,91,91,92,15,91,91,29,29,28,28,7,26,7,7,7,27,27,28,28,101,29,28,7,29,28,28,29,29,28,28,29,29,28,7,28,28,7,28,28,7,27,7,7,28,29,29,29,30,91,91,91,91,91,91,30,91,26,26,25,7,101,28,7,27,27,27,7,7,7,28,28,28,28,30,30,30,91,91,29,25,172,7,28,28,29,30,29,28,29,28,29,29,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,92,92,30,30,30,30,29,29,29,28,26,174,173,25,27,174,174,173,28,29,29,29,29,29,30,29,29,29,29,29,29,28,25,173,26,29,29,28,28,29,29,29,101,91,91,30,91,8,26,7,244,222,174,173,245,221,26,29,28,173,7,28,7,28,7,28,26,27,28,29,28,29,28,7,28,28,29,30,91,91,91,15,91,91,91,15,91,30,29,28,28,7,27,27,7,7,172,173,27,28,28,28,28,28,27,7,28,28,28,29,29,28,29,29,29,28,28,7,28,28,28,7,7,7,28,29,29,29,30,30,91,91,91,91,91,30,30,29,25,25,172,27,28,7,7,7,27,7,7,28,28,28,28,28,30,30,30,30,91,27,29,29,28,28,28,28,29,29,29,28,29,29,29,29,28,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,91,91,30,30,30,30,29,29,29,29,29,26,246,174,172,173,246,245,173,7,29,29,28,29,29,29,28,29,29,29,29,28,172,7,26,7,27,29,28,28,29,29,101,29,30,91,91,169,146,25,25,220,221,25,25,173,245,27,28,172,173,28,7,28,28,27,25,173,173,25,29,29,28,27,28,28,28,29,90,91,91,91,91,91,30,91,91,91,29,29,29,101,28,28,7,7,27,149,173,26,101,28,7,7,7,29,28,26,7,29,28,29,29,28,29,29,29,28,28,28,7,7,7,7,28,29,29,30,30,30,91,91,91,91,91,30,30,29,28,7,7,172,149,7,28,27,27,7,28,28,28,28,28,28,29,30,30,30,91,91,29,29,29,29,29,28,28,28,29,29,29,29,29,29,28,28,29,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,91,91,91,91,30,30,30,29,29,28,29,29,29,173,246,245,172,245,174,173,173,28,29,29,28,29,29,29,7,26,173,26,29,26,173,27,28,28,26,29,28,28,29,101,29,30,30,91,7,168,29,28,24,220,244,173,26,173,174,172,28,173,28,7,7,7,7,172,172,172,173,173,28,101,28,7,28,28,28,29,29,91,30,91,91,91,91,91,30,29,29,29,28,101,28,26,27,172,173,149,172,26,7,27,27,26,7,28,28,172,26,7,29,28,28,28,29,28,29,29,28,28,7,7,28,7,28,29,30,30,30,91,91,91,91,90,30,29,29,29,28,7,26,172,149,173,172,173,27,7,28,28,28,28,28,29,30,30,30,30,30,30,29,29,28,7,28,29,29,29,28,29,29,29,29,29,29,29,29,28,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,7,30,30,27,26,29,29,29,29,29,29,29,29,29,173,246,245,173,245,25,173,173,27,29,29,29,29,29,29,27,173,173,26,27,172,173,27,26,28,7,29,28,101,28,29,29,30,91,91,146,26,30,29,172,26,244,174,26,174,173,7,173,26,28,7,7,26,172,173,173,173,245,172,28,28,28,28,27,7,7,29,29,29,29,101,29,29,29,29,26,25,28,101,101,101,27,173,172,173,172,174,173,27,172,173,149,173,101,28,28,27,26,25,28,101,28,29,28,28,29,29,28,7,7,28,28,28,28,29,30,30,30,91,91,91,30,29,29,29,29,28,7,26,27,149,173,221,149,172,7,101,28,28,7,28,28,29,30,30,30,30,30,29,29,28,26,26,28,29,29,29,29,29,29,29,29,29,29,29,28,26,27,7,255,255,255,255,255,255,255,255,254,255,255,92,255,255,255,255,91,30,91,30,30,29,28,28,29,29,29,28,28,29,29,29,173,222,246,172,222,172,173,7,28,29,29,29,28,29,29,7,172,29,29,172,245,173,27,172,28,29,28,28,28,29,29,30,30,91,27,27,29,29,29,172,26,244,221,172,245,25,28,174,7,7,7,27,25,173,173,173,173,245,7,101,28,7,7,26,7,28,29,28,28,28,29,29,7,7,28,25,24,7,101,101,28,173,172,172,172,27,26,172,173,149,26,172,173,28,7,27,29,7,26,172,7,28,28,28,28,28,28,28,28,7,28,28,28,28,29,91,30,91,91,91,29,29,29,29,28,28,28,27,26,27,149,149,149,149,27,27,28,28,28,7,28,28,29,30,30,30,90,29,29,28,7,26,7,28,29,29,29,29,29,29,29,29,29,29,29,29,25,25,28,28,255,255,255,255,255,255,255,254,255,92,15,91,29,91,30,30,29,29,30,30,29,29,29,28,28,28,7,29,29,29,29,173,222,174,25,221,174,26,29,29,29,29,29,29,29,29,28,7,29,29,7,173,173,172,26,7,28,28,7,101,29,29,29,91,91,91,90,22,25,28,244,173,222,221,173,173,27,26,173,101,26,27,26,173,172,173,174,173,27,101,28,101,172,172,101,7,7,7,28,28,28,101,101,172,172,7,7,172,28,29,101,7,7,27,172,26,27,27,7,173,173,27,28,26,7,29,7,28,29,27,172,172,173,28,101,7,28,28,28,7,27,7,101,28,28,29,30,91,91,30,29,29,29,29,28,28,28,28,28,7,26,149,149,26,172,26,28,28,28,7,7,7,28,29,101,30,30,29,25,28,172,172,26,28,29,29,29,28,28,29,29,29,29,29,29,29,29,29,27,7,7,28,7,255,255,255,255,255,254,92,92,92,91,27,91,30,30,27,28,29,29,29,29,29,29,28,29,7,29,29,29,29,25,222,25,26,246,173,172,7,29,29,29,29,29,29,29,7,25,28,29,28,7,7,172,7,7,28,28,26,7,29,29,29,91,91,30,29,22,24,7,244,244,222,222,172,221,26,173,26,7,173,26,173,26,173,245,173,173,28,28,101,27,174,7,7,173,7,28,101,101,28,28,172,149,172,172,7,28,28,28,101,28,101,27,26,172,172,173,172,172,26,173,26,26,26,29,27,7,29,7,173,245,245,173,172,27,28,28,28,28,28,7,27,7,28,29,29,29,29,29,29,29,29,7,28,28,28,28,7,7,27,172,173,172,26,172,28,28,7,7,7,7,172,29,28,29,29,7,24,28,26,172,7,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,7,26,7,27,173,255,255,255,255,254,92,92,92,7,26,29,30,29,7,28,29,29,29,29,29,29,28,28,29,29,28,29,28,7,245,173,27,246,25,173,25,29,29,29,29,29,29,29,28,172,7,7,27,26,26,28,28,27,28,101,27,7,28,7,26,27,27,30,26,21,24,172,244,221,222,222,25,26,7,173,7,172,173,173,173,27,27,173,172,172,28,101,7,172,27,27,27,27,7,101,28,29,101,172,222,172,27,150,7,101,28,101,27,7,101,172,173,173,149,149,173,173,173,173,26,173,26,28,173,29,29,7,172,246,173,172,174,172,28,7,28,29,28,7,172,26,101,29,29,29,29,28,29,29,28,7,28,28,28,7,7,28,27,26,172,172,172,172,28,28,7,7,7,26,172,28,29,29,29,28,7,28,28,27,7,101,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,7,7,7,172,28,255,255,255,254
mountainsize:dw 10050

treeline:db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,170,169,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,72,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,28,146,168,72,72,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,172,28,145,145,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,28,255,255,255,255,255,255,255,255,255,255,255,27,72,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,28,72,255,255,27,172,169,27,72,170,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,72,172,28,146,146,27,255,255,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,28,146,146,146,172,72,255,255,72,168,168,170,72,255,255,213,117,143,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,169,20,172,170,28,172,22,172,72,72,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,72,72,72,148,170,27,143,172,22,72,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,143,143,143,214,146,146,146,146,147,144,145,145,146,145,21,170,143,143,143,143,143,172,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,28,146,146,146,169,28,72,20,169,172,172,172,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,69,255,69,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,69,255,69,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,72,170,170,149,72,28,146,146,218,72,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,72,143,144,143,143,21,146,145,22,145,144,143,214,145,216,216,144,117,117,117,143,143,117,143,142,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,144,144,144,144,146,146,21,170,72,170,168,141,169,147,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,69,14,117,71,117,69,69,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,69,14,117,117,117,14,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,172,170,143,168,169,72,170,21,146,145,145,145,144,172,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,143,143,144,144,144,145,21,214,143,144,117,117,144,214,214,146,146,117,117,117,117,143,143,117,143,143,172,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,146,145,145,145,145,146,21,22,143,143,170,72,169,72,170,72,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,69,69,117,117,117,14,14,14,14,14,14,14,69,69,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,67,67,69,14,14,14,14,14,14,117,117,14,67,69,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,143,72,170,172,170,143,143,146,148,144,144,145,146,146,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,117,143,117,143,143,213,20,20,215,117,118,117,144,144,214,144,143,117,213,117,117,218,117,117,215,118,144,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,145,146,170,147,146,148,169,143,143,146,144,72,170,27,72,170,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,69,117,117,117,117,117,14,117,14,14,14,14,117,14,117,69,69,255,255,255,255,255,255,254,255,255,255,255,255,117,117,14,117,45,14,14,14,117,117,117,117,117,117,117,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,170,28,72,172,169,169,172,143,170,168,146,170,172,146,72,255,255,255,255,255,255,255,255,255,255,255,255,255,255,143,216,143,117,144,21,21,20,218,143,143,144,216,214,143,117,117,143,143,143,143,216,222,216,168,220,194,216,72,255,255,255,28,27,74,255,255,255,255,255,255,255,255,72,148,148,170,148,148,172,142,116,143,172,146,169,72,72,143,72,72,143,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,255,255,117,117,14,117,143,117,117,117,71,117,14,117,14,71,117,14,14,14,14,14,14,69,69,254,14,69,69,14,14,14,117,117,14,117,117,117,71,117,117,117,116,117,14,117,28,255,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,143,143,27,170,143,28,170,21,170,142,116,143,147,148,169,149,148,72,255,255,255,255,255,255,255,28,70,117,70,255,255,28,218,218,214,143,143,141,213,117,143,143,214,144,144,143,214,143,117,117,117,213,143,220,220,145,218,218,218,21,142,27,142,116,117,117,118,118,72,255,255,255,255,255,255,255,27,146,144,146,170,143,115,117,116,168,169,172,27,143,143,72,143,117,117,143,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,69,69,69,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,69,72,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,70,70,116,117,117,115,115,115,14,117,117,14,117,117,117,117,117,117,14,14,14,14,117,14,14,254,14,14,14,14,117,14,71,117,117,117,117,71,117,117,14,14,117,117,117,116,69,115,255,255,255,255,255,255,255,255,255,255,255,255,255,255,28,116,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,70,117,69,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,143,117,117,117,72,169,145,172,172,172,143,143,115,117,172,148,145,146,172,255,255,255,255,255,255,255,144,118,117,117,143,143,116,143,168,222,220,218,214,214,144,117,117,117,144,143,144,212,117,117,117,117,117,143,213,19,18,218,218,218,222,21,216,143,143,143,117,117,117,118,122,255,255,255,255,255,255,255,28,144,144,145,170,143,115,117,143,143,172,72,143,143,172,146,143,117,143,143,143,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,69,70,70,117,117,117,143,143,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,14,255,255,255,14,68,255,255,255,255,255,14,255,255,255,255,70,14,115,117,117,14,45,14,14,117,117,117,117,117,117,71,71,117,14,117,117,117,117,117,14,254,117,14,117,2,117,117,117,71,14,117,117,117,115,14,45,14,14,115,117,116,14,14,255,255,255,14,68,67,67,255,255,255,255,255,255,255,69,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,143,117,117,117,117,27,143,14,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,72,117,143,117,117,215,170,170,144,172,172,143,143,116,143,146,22,145,216,143,255,72,172,28,255,255,28,118,117,117,117,143,143,143,213,22,220,215,143,20,222,20,144,144,143,144,214,214,143,117,117,117,117,143,117,143,143,214,198,177,200,144,214,220,143,143,143,143,117,117,118,118,27,255,255,255,172,143,72,72,216,216,218,21,143,116,143,117,143,143,143,170,143,172,22,145,117,117,117,117,117,27,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,67,116,117,70,117,117,70,143,116,143,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,28,255,14,14,69,14,14,255,14,255,255,255,69,14,14,14,14,14,14,69,116,117,117,14,115,14,14,117,117,117,117,117,117,71,71,2,117,117,117,117,117,117,14,254,117,117,117,117,117,117,117,14,117,117,117,117,117,117,14,14,14,14,115,143,14,69,14,14,14,14,67,69,255,255,255,14,68,69,115,69,69,69,143,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,172,172,143,70,143,117,117,143,143,255,255,255,255,255,255,255,255,255,255,14,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,70,255,255,116,117,117,143,117,144,21,22,146,172,143,143,143,143,143,143,117,215,222,222,144,146,146,117,255,255,255,72,191,118,117,117,143,144,216,215,144,144,117,117,117,143,213,143,143,143,213,20,214,143,143,144,143,117,117,143,117,117,117,144,214,215,143,143,212,19,144,215,143,143,143,118,194,72,255,255,255,143,143,143,213,216,222,222,118,117,116,143,143,143,143,143,143,146,216,150,21,144,143,117,117,117,69,69,69,14,115,255,255,255,255,255,255,255,255,255,255,255,255,255,255,27,69,255,255,255,255,255,255,255,255,69,14,14,14,14,255,255,255,255,255,255,255,69,117,117,71,117,143,143,143,143,72,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,117,69,117,115,117,117,116,116,14,68,255,255,14,116,117,115,14,14,115,14,117,117,115,14,14,115,14,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,254,117,117,117,117,117,117,117,117,117,117,117,117,117,117,14,115,115,14,14,117,71,117,69,255,14,117,70,70,255,255,14,117,143,69,115,116,116,69,14,14,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,69,117,117,117,143,117,117,117,117,70,255,255,255,255,255,255,255,44,14,14,70,255,69,70,255,255,255,255,70,69,255,255,27,27,255,255,255,255,255,255,255,255,255,116,117,14,14,14,117,117,117,118,117,222,222,215,143,143,143,143,143,117,117,117,215,198,200,190,118,117,70,255,255,255,172,194,144,117,117,143,213,222,194,118,117,143,117,117,143,143,117,117,143,214,198,19,20,143,143,143,143,117,117,215,213,213,117,117,143,117,144,214,223,190,117,143,143,143,144,219,143,255,255,69,69,143,117,117,144,223,223,117,117,143,143,143,143,143,143,143,20,218,220,145,117,117,143,143,115,14,14,14,14,117,69,255,255,255,255,255,255,255,255,255,72,69,143,255,70,69,69,255,255,255,14,71,70,69,70,117,14,14,14,255,69,67,255,255,255,255,69,117,117,117,117,117,117,117,117,71,70,70,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,70,14,116,117,117,117,14,14,117,14,14,255,255,14,117,116,14,14,117,117,117,117,117,14,14,115,115,115,115,14,117,117,117,115,117,117,118,117,117,45,117,117,117,117,254,117,117,117,117,14,117,115,117,117,117,117,115,14,117,115,14,14,45,14,117,117,117,117,255,68,14,14,14,255,68,14,14,69,14,14,69,116,143,117,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,69,117,117,71,117,117,117,117,117,117,117,70,67,255,255,255,14,14,117,14,14,117,70,117,71,2,71,255,255,14,69,69,69,143,117,69,28,255,255,255,255,28,255,255,67,117,14,117,14,14,14,143,117,118,117,144,218,218,143,143,143,143,143,117,117,117,144,199,117,117,118,117,69,72,255,27,144,220,19,214,143,143,143,223,223,191,143,143,143,117,117,143,117,117,116,19,222,143,212,213,117,117,117,215,143,212,215,117,117,117,143,143,213,19,198,216,143,143,214,222,218,218,215,117,69,116,69,117,117,117,117,218,144,117,117,117,117,143,143,143,117,213,21,21,144,144,118,117,143,116,14,14,69,117,117,117,117,255,255,27,27,255,255,255,255,70,67,116,117,70,117,70,14,14,14,69,117,2,2,117,70,117,117,117,116,117,14,14,255,255,255,143,117,117,117,70,117,117,117,117,117,117,117,117,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,117,117,143,117,117,14,14,14,14,14,14,255,14,14,14,14,68,116,117,117,117,14,14,14,14,14,14,117,115,117,115,117,117,143,189,117,117,117,14,45,45,117,117,254,117,117,117,117,45,14,14,115,117,14,117,115,115,115,14,14,115,14,14,117,117,117,117,255,67,14,14,14,14,14,14,14,14,115,14,14,117,143,117,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,143,117,117,117,117,117,117,117,117,117,117,117,14,255,67,14,14,115,118,145,144,117,117,2,2,118,71,117,14,14,67,117,70,69,143,70,27,255,255,255,255,72,28,255,117,117,117,116,116,117,117,117,117,144,215,117,145,22,213,143,143,143,143,143,143,143,117,116,117,71,117,117,71,117,117,117,118,217,222,20,213,143,117,143,116,117,115,143,117,143,117,117,117,117,117,215,215,117,143,143,117,117,117,215,215,143,117,117,117,117,117,117,14,14,115,115,117,117,117,213,222,191,118,118,117,117,71,71,71,45,71,115,117,117,143,143,143,143,143,143,143,143,20,220,215,144,190,144,143,117,115,116,117,117,117,117,118,73,255,72,27,255,255,255,255,27,117,143,71,70,117,117,69,115,117,71,71,118,2,71,117,117,118,190,117,117,14,14,69,69,14,14,143,118,117,117,117,117,117,117,117,117,117,117,71,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,14,117,143,143,117,14,117,71,14,117,117,14,14,14,14,14,117,14,72,116,143,143,14,14,115,115,14,14,117,115,115,117,117,117,117,117,117,117,117,115,117,14,117,117,254,117,117,117,117,14,117,115,117,117,117,115,115,115,115,14,14,115,115,115,116,143,143,143,70,69,14,14,14,14,14,69,69,117,117,117,117,143,143,117,14,14,255,255,255,255,255,255,255,255,255,255,255,67,255,70,117,117,117,117,117,117,117,117,117,117,117,117,14,71,14,115,115,117,117,118,117,117,117,117,117,118,116,117,117,142,67,117,117,27,143,117,28,70,255,255,27,143,172,117,118,218,117,117,117,117,117,117,117,215,190,144,215,144,143,143,143,143,143,117,117,215,143,14,115,14,14,117,2,71,117,117,2,190,215,117,117,115,14,14,14,14,14,117,118,117,117,117,143,143,117,143,222,214,214,214,144,213,213,143,117,117,118,117,117,117,117,117,14,14,14,14,14,117,117,117,190,216,71,117,117,14,71,118,117,14,14,115,117,215,213,143,117,213,143,143,117,144,143,216,216,144,218,144,117,117,116,143,117,117,117,215,117,117,143,143,143,28,255,69,27,28,117,117,27,117,117,117,143,117,117,117,117,118,117,117,117,117,117,117,117,117,117,117,115,117,117,14,117,117,117,117,117,117,117,117,118,71,117,117,118,70,255,69,255,255,255,255,255,255,255,255,255,69,69,255,14,14,117,117,117,117,117,117,117,117,117,14,14,14,14,117,117,115,143,143,143,143,116,117,14,14,115,14,117,117,115,115,117,117,117,117,117,189,117,117,117,117,115,189,117,254,117,117,117,117,117,117,117,117,143,117,14,14,115,117,117,115,14,14,14,14,116,117,117,117,115,116,117,14,14,14,116,117,117,117,117,143,143,117,117,115,14,14,118,28,255,255,255,255,255,255,255,27,117,70,117,117,117,117,117,117,117,117,117,117,117,117,117,117,143,117,117,117,118,117,117,117,117,143,143,117,117,70,143,143,117,117,117,117,27,144,117,117,117,14,116,116,117,143,117,117,215,117,117,117,117,117,117,117,215,218,218,215,143,22,143,117,144,143,117,117,216,117,117,117,14,117,117,117,71,117,14,71,190,117,117,117,14,115,117,117,117,115,117,118,118,118,117,215,143,212,210,218,216,215,19,214,215,214,143,117,118,118,118,118,118,117,117,117,117,117,115,14,115,117,117,117,213,14,14,117,71,117,117,117,14,14,117,117,117,215,143,117,117,143,143,117,218,143,117,216,218,148,143,117,117,117,117,117,117,117,213,117,117,143,116,115,69,115,14,143,117,117,145,27,117,117,69,117,117,117,117,71,71,117,143,143,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,117,143,117,118,117,117,117,117,70,117,116,255,255,255,255,255,255,255,255,143,117,14,14,115,117,117,117,143,117,117,143,117,117,117,115,14,117,117,117,115,117,117,117,117,115,14,117,14,14,115,115,117,115,117,14,115,143,143,117,117,117,117,117,117,117,117,117,254,117,117,14,117,117,117,117,189,189,117,14,14,117,117,117,117,115,14,14,14,117,117,115,14,117,117,117,14,14,117,117,117,117,117,117,190,117,117,117,117,115,142,144,117,255,255,255,255,255,255,255,70,117,117,117,117,117,215,189,117,117,117,117,117,117,117,117,117,117,117,117,143,117,117,117,117,117,117,143,14,71,116,142,143,117,117,117,71,14,118,117,117,117,115,14,117,115,117,189,117,117,117,117,117,117,117,117,117,190,216,194,144,117,20,143,143,143,117,117,143,143,117,117,71,117,14,117,117,117,71,45,117,118,117,117,117,115,117,143,117,117,115,117,190,118,118,117,190,117,143,188,19,218,214,200,19,214,213,213,144,118,117,118,118,190,117,117,115,117,143,117,115,117,117,117,117,117,117,71,71,117,117,117,71,14,117,117,14,117,117,143,143,143,143,143,143,141,212,117,218,218,144,117,117,117,117,117,117,117,117,143,117,117,117,14,14,14,14,117,117,117,117,117,117,14,117,117,117,117,117,143,117,14,117,117,117,117,117,117,117,117,117,189,117,117,117,117,117,117,117,117,117,117,117,117,117,216,117,117,117,117,117,117,70,255,255,255,255,255,255,255,70,117,143,67,117,117,117,117,117,189,117,14,143,117,117,117,117,14,14,117,117,117,14,115,117,117,14,14,14,45,115,14,115,117,117,14,115,115,143,190,117,117,115,117,118,117,117,117,117,254,115,115,115,115,117,117,117,117,189,116,116,115,14,117,117,14,14,14,14,117,14,14,14,14,117,117,117,115,14,117,189,117,117,117,71,117,117,117,117,14,117,14,170,117,255,255,255,255,255,255,69,14,117,117,117,117,117,215,215,117,117,117,117,117,117,117,117,71,117,117,190,212,143,117,117,117,117,14,117,117,143,117,14,117,14,117,117,117,14,118,117,117,117,117,14,14,115,115,143,117,117,117,115,115,117,117,117,117,117,190,117,118,118,117,143,19,215,215,144,115,14,14,14,118,117,71,71,117,117,117,117,117,117,117,115,117,117,117,117,117,117,115,117,144,118,118,118,118,117,14,115,143,216,18,18,19,19,214,214,190,118,117,118,118,118,117,117,117,14,117,117,117,117,143,117,117,117,116,117,118,118,143,117,71,117,117,117,14,14,14,116,143,216,214,214,143,115,143,144,144,145,190,117,117,117,117,117,115,115,117,71,117,117,143,115,117,14,14,117,117,117,117,117,71,117,117,117,14,14,117,14,117,117,117,117,14,14,117,117,117,117,190,190,215,117,117,2,117,117,117,117,117,117,117,117,215,216,215,117,117,117,117,117,71,69,255,255,255,255,255,255,142,145,72,14,117,14,115,143,117,117,117,14,117,117,190,117,117,14,115,117,117,118,14,14,14,14,115,14,14,45,71,14,117,117,117,117,142,116,215,190,117,14,14,14,117,117,117,115,115,254,14,115,115,117,117,117,117,115,143,117,143,117,115,143,116,117,117,115,14,14,14,14,117,117,117,117,117,117,14,117,117,189,117,117,117,117,117,117,117,14,115,14,116,143,143,255,255,143,255,255,117,117,189,215,117,117,117,117,117,117,117,117,117,117,117,117,117,118,117,117,190,143,117,117,117,117,117,117,116,117,117,14,117,117,117,117,143,117,14,117,213,117,118,117,117,117,117,117,14,117,71,14,14,14,117,117,117,117,117,215,144,215,117,117,117,117,144,19,115,14,14,14,117,118,117,117,115,117,118,117,115,117,117,143,143,117,117,117,117,117,14,117,117,117,191,119,118,117,117,117,117,115,215,198,218,210,19,216,194,190,215,117,118,191,117,117,117,71,117,117,117,117,117,117,117,117,117,117,115,117,118,117,117,117,117,117,117,117,14,14,14,143,218,143,143,117,117,143,144,117,146,144,117,117,117,118,117,14,14,14,14,117,71,117,117,117,117,117,117,117,117,215,115,14,117,143,143,117,117,143,14,71,118,118,117,117,117,117,117,117,115,143,117,189,117,2,118,117,117,117,118,117,117,117,117,117,117,117,117,117,117,117,190,117,117,255,255,117,255,255,67,143,146,70,14,14,14,117,117,117,117,117,143,117,115,189,117,117,14,115,117,143,117,117,117,117,44,14,14,115,115,117,117,117,143,115,117,143,116,143,117,115,14,14,14,117,117,117,115,115,254,117,14,115,115,117,117,115,115,115,117,117,14,14,117,117,117,116,117,117,14,14,115,143,143,143,117,117,14,14,14,118,190,190,190,215,116,115,117,117,117,14,14,117,117,117,28,69,14,117,14,117,117,189,189,117,117,117,117,117,117,117,117,213,117,117,117,117,189,117,117,117,117,117,117,117,117,143,143,117,14,117,117,117,117,143,117,117,14,45,117,213,117,117,117,117,71,14,14,117,117,117,14,14,14,14,117,117,117,117,118,216,218,144,143,117,117,143,187,14,117,117,117,117,117,117,117,117,14,117,14,117,117,117,117,117,117,117,117,117,117,71,117,117,117,117,190,216,190,117,117,117,117,215,190,117,143,215,117,143,215,216,190,190,118,117,117,117,71,117,117,117,117,116,143,117,117,117,117,117,115,117,71,117,117,117,118,117,71,117,117,115,14,212,143,117,117,117,143,190,145,215,117,117,117,71,117,14,71,45,14,14,118,71,71,14,14,14,117,117,143,117,213,117,14,117,117,117,117,143,115,115,14,117,117,117,215,213,215,189,118,118,117,117,117,2,117,118,117,117,117,143,213,117,117,117,117,71,117,117,117,117,189,190,117,117,14,14,14,14,255,116,117,144,115,14,115,115,117,117,14,143,215,215,190,189,189,117,14,14,14,117,117,144,142,143,117,14,45,115,115,117,117,143,117,14,14,14,115,143,117,117,14,117,117,115,117,117,115,115,14,254,115,14,117,115,115,117,117,115,14,117,115,117,117,115,117,117,117,117,117,117,117,117,117,143,72,117,117,14,14,14,115,118,118,118,117,117,115,117,117,117,14,14,117,117,117,117,14,71,2,2,117,117,213,215,117,117,215,215,215,215,143,117,117,117,117,117,117,189,215,143,117,117,117,215,117,117,117,143,117,71,118,117,117,117,117,117,118,117,117,117,215,117,117,117,117,117,117,117,117,117,71,14,14,71,45,117,117,117,117,118,118,118,118,117,117,117,115,115,117,118,115,14,14,14,117,117,117,117,115,115,117,117,215,117,188,143,117,71,117,117,117,117,115,117,117,17,194,190,117,117,117,14,117,220,215,213,143,143,213,19,218,216,216,117,117,117,117,143,117,117,14,14,117,117,143,118,118,117,117,115,115,117,14,117,117,117,14,14,14,118,117,14,116,117,118,117,117,144,144,145,117,118,117,117,117,117,14,71,14,14,14,117,117,117,117,117,117,117,143,117,117,143,117,117,117,117,117,115,143,117,117,189,118,71,117,117,117,117,117,215,144,190,118,117,117,190,190,117,117,117,117,117,117,117,213,143,190,215,215,117,117,215,189,117,117,117,2,117,71,69,117,117,117,117,14,117,117,117,117,14,117,117,117,190,190,117,14,14,14,14,115,117,72,172,143,117,117,117,117,115,143,117,117,71,115,115,115,115,117,14,117,117,117,117,115,115,117,115,14,115,254,115,14,117,14,117,117,117,117,115,14,117,213,212,117,116,187,116,143,115,115,117,117,117,144,143,115,118,115,14,14,14,71,117,189,143,117,117,14,117,117,14,117,146,190,143,117,71,117,117,117,117,213,214,189,118,190,190,190,190,189,117,189,117,117,117,213,189,117,117,117,117,71,117,117,189,117,117,117,143,117,117,117,117,117,117,14,117,117,117,117,117,117,117,117,117,117,117,115,118,118,117,71,117,117,2,117,115,117,118,118,118,117,118,118,117,115,65,65,117,117,14,117,117,14,117,117,115,117,117,117,117,117,213,117,117,117,117,117,117,117,189,117,117,215,215,117,117,117,117,117,117,117,213,223,216,216,216,218,218,218,213,117,117,143,117,117,117,213,117,117,117,117,117,190,117,143,215,117,117,117,117,115,115,117,117,14,14,117,14,115,117,115,14,65,115,118,117,118,117,117,118,118,117,117,117,117,117,117,117,117,14,117,118,117,115,116,117,117,117,14,117,117,117,117,117,117,117,14,117,118,71,117,71,118,117,117,117,117,117,189,118,71,117,117,190,215,118,190,189,117,117,117,117,143,215,190,190,190,18,118,117,213,213,213,117,117,117,117,71,71,117,117,190,170,117,14,117,117,14,117,117,117,189,117,117,117,14,14,14,115,143,14,142,117,117,117,117,117,117,115,187,117,189,117,189,213,189,117,14,115,117,117,213,117,117,115,115,117,115,117,254,117,115,115,117,117,117,117,117,117,117,117,116,117,213,117,14,14,117,14,14,117,117,117,188,115,117,117,117,117,117,14,117,117,117,117,213,117,115,117,117,117,117,118,117,117,14,71,117,117,117,117,215,213,213,118,118,190,191,117,117,117,117,215,189,117,143,189,189,117,71,117,117,117,118,191,118,118,190,143,117,118,117,117,117,117,118,190,117,14,117,117,117,117,117,117,117,117,14,117,215,117,71,2,117,117,117,14,71,71,117,118,118,118,118,14,14,116,115,14,14,117,117,71,14,14,117,143,117,117,117,117,117,117,117,14,117,117,117,117,117,143,117,117,117,117,117,117,117,117,117,117,117,189,215,216,18,18,18,190,117,117,117,143,143,143,117,117,117,143,117,117,117,117,117,71,117,117,118,117,117,117,117,117,117,117,14,14,117,117,14,14,14,14,116,65,14,2,117,118,118,118,117,71,117,71,2,117,117,117,117,14,117,190,117,14,115,117,117,117,117,117,117,117,71,117,213,143,117,117,190,117,117,117,117,117,190,190,190,191,117,117,117,117,117,71,117,191,190,213,117,189,117,118,118,118,118,190,190,190,118,117,213,213,213,117,117,117,117,71,71,116,116,118,144,143,117,117,117,14,117,189,189,117,117,117,71,14,117,71,117,117,71,117,190,117,117,117,45,14,117,117,117,115,143,211,18,189,117,115,115,117,187,189,118,117,115,117,117,117,117,254,14,14,14,117,117,115,14,117,117,117,115,115,45,14,14,115,115,14,14,117,117,117,117,115,14,117,189,118,117,117,117,117,189,117,187,117,117,117,2,117,143,117,117,117,117,117,117,117,117,117,117,117,213,213,190,118,191,194,117,117,117,118,117,215,117,71,71,117,117,117,117,117,117,117,117,117,117,190,117,118,118,190,117,71,117,117,117,117,117,117,14,117,71,117,117,117,117,117,14,117,118,117,2,117,117,118,71,14,14,117,118,190,190,118,143,116,116,115,117,117,143,117,117,14,14,117,213,117,117,117,117,117,117,117,117,117,215,117,117,189,117,117,117,117,117,117,215,117,117,118,117,117,117,117,215,214,18,190,118,117,215,215,117,117,117,117,117,117,117,117,117,117,190,117,117,117,118,118,117,117,117,117,117,117,14,14,71,117,117,117,117,14,115,116,65,116,118,117,189,118,118,117,14,14,71,117,117,117,117,71,117,118,117,117,117,71,117,117,117,117,117,71,117,117,71,117,117,117,117,117,117,189,143,117,215,191,117,117,213,117,118,118,118,117,118,117,118,117,117,117,215,189,117,117,71,117,190,194,190,118,189,143,213,189,117,117,117,117,117,117,115,117,117,117,143,143,117,117,117,117,117,189,117,117,117,14,117,118,117,117,118,117,14,115,117,117,117,14,14,45,115,117,14,115,115,14,115,115,115,117,117,117,117,118,187,115,117,117,117,117,254,117,115,14,117,189,189,117,117,189,115,115,117,117,14,14,143,189,117,115,117,115,116,213,117,14,115,143,117,143,117,117,189,190,215,115,117,117,115,117,117,117,117,71,14,115,117,117,118,117,143,117,117,143,117,191,190,191,215,117,117,117,117,117,117,117,117,117,117,189,117,189,18,190,117,117,213,117,117,117,117,215,117,117,117,71,117,117,117,117,117,117,117,117,117,117,117,117,14,14,71,118,118,117,117,117,2,117,117,14,117,117,118,118,118,143,116,115,141,190,117,117,14,115,117,71,117,117,117,117,117,117,118,117,117,117,215,190,118,117,143,215,190,143,117,117,117,117,117,190,118,117,117,71,71,117,117,117,117,213,117,215,117,118,117,143,143,215,215,190,215,117,117,190,117,117,117,117,117,117,117,117,117,117,117,14,14,118,117,117,117,117,213,116,116,141,142,143,118,118,118,117,14,14,117,2,117,117,117,117,117,117,2,14,117,117,117,117,115,14,14,71,117,117,117,117,117,117,118,71,14,117,189,117,117,117,117,117,189,117,117,117,215,215,117,117,190,118,71,71,117,117,117,117,14,117,117,190,190,190,190,189,117,187,117,117,117,189,117,117,117,117,117,71,14,117,117,117,14,118,117,118,189,213,190,189,117,117,117,118,117,189,117,14,115,187,117,117,115,117,14,115,117,189,14,45,14,14,117,117,117,189,189,118,117,117,117,117,143,117,117,254,117,117,117,117,189,213,117,117,117,213,189,189,117,117,117,117,215,190,117,117,117,143,213,117,117,117,117,117,215,117,189,117,190,117,117,117,117,117,14,14,14,117,143,117,71,14,117,117,117,117,117,117,117,117,190,191,18,215,117,117,189,215,117,117,215,215,189,190,143,189,215,215,215,117,117,117,71,117,190,18,215,117,117,117,117,71,117,14,117,117,215,215,143,117,117,117,71,14,14,14,118,117,117,117,2,71,117,117,117,117,118,118,118,117,116,141,116,116,116,117,117,117,117,117,117,71,117,213,117,215,215,118,117,143,189,117,117,118,117,117,117,116,115,117,117,118,117,118,118,117,71,117,117,117,2,117,117,117,18,215,117,118,117,117,117,117,117,212,117,117,117,143,190,190,117,117,117,117,117,118,117,143,143,117,14,117,117,117,14,14,117,143,116,116,116,116,117,118,118,118,117,14,14,117,117,117,117,117,117,117,117,14,14,14,71,117,117,117,117,117,143,215,117,117,117,117,71,71,117,117,117,189,215,215,117,117,117,189,117,2,190,215,215,189,117,190,118,118,189,117,117,117,117,117,117,117,117,194,191,190,117,117,143,117,117,117,117,117,117,117,117,117,117,117,117,71,14,117,117,117,117,117,143,215,190,189,143,118,118,117,117,117,117,117,187,189,117,117,117,115,187,190,117,117,117,117,117,117,189,189,117,189,189,117,117,117,117,117,117,117,254,115,117,189,187,115,117,117,143,187,189,117,117,116,190,143,115,117,117,117,189,189,117,117,189,190,190,117,118,117,117,189,117,190,143,117,117,117,45,45,14,14,117,143,116,117,117,117,117,117,117,2,117,117,117,118,117,215,215,117,117,189,189,117,190,191,190,118,189,215,215,215,215,189,189,117,71,117,213,18,190,215,215,215,189,117,117,14,14,117,215,190,117,117,117,117,117,117,117,14,14,117,117,117,117,71,14,117,2,71,71,117,117,117,143,116,215,213,116,189,215,191,118,117,143,117,117,117,117,190,117,190,118,117,213,117,117,117,118,215,143,117,213,190,118,118,118,118,118,118,117,117,2,117,117,117,117,2,117,117,117,117,117,117,117,117,117,143,143,143,215,117,117,118,118,215,117,117,189,215,191,190,117,116,14,117,117,117,116,116,143,117,116,116,187,189,188,117,118,118,117,118,2,117,117,117,71,71,117,117,117,117,14,14,14,14,117,117,117,117,117,189,213,143,117,71,117,14,117,117,117,117,215,215,18,17,190,117,71,118,118,117,215,215,215,117,117,190,190,213,215,189,117,143,189,117,117,118,218,194,190,117,143,117,117,117,117,117,2,117,117,14,117,117,189,117,14,14,14,71,117,117,117,117,190,117,117,189,117,215,117,117,118,143,143,189,117,117,117,115,117,117,117,117,115,117,117,189,117,215,117,117,117,117,189,117,117,117,117,117,117,254,117,187,117,115,14,115,117,117,117,14,14,116,189,117,117,14,115,117,117,117,117,117,117,117,117,117,117,118,189,117,189,189,117,115,189,117,118,71,14,117,117,117,189,189,117,117,117,117,117,117,117,141,212,143,117,71,117,215,215,215,215,117,117,117,118,189,189,117,190,117,117,117,189,189,215,117,190,118,218,190,190,190,190,190,117,117,117,71,117,213,190,117,143,117,117,117,118,117,14,45,71,189,117,117,117,117,117,117,117,117,117,71,71,117,213,213,187,116,189,215,190,189,117,117,215,190,117,118,190,216,118,190,215,190,118,118,118,118,215,117,143,19,143,118,2,118,190,117,117,117,117,117,2,2,117,2,71,2,117,118,118,117,118,117,117,212,19,143,117,190,118,117,118,118,118,190,117,118,118,118,190,118,118,118,117,187,116,116,215,213,215,215,116,188,190,213,143,118,117,71,2,71,71,117,117,71,117,117,117,117,117,14,14,117,117,117,117,117,117,117,117,143,213,117,2,117,117,117,117,215,190,190,190,194,190,190,117,117,117,190,118,215,215,190,190,190,117,117,189,215,117,117,117,215,215,190,117,215,117,117,117,187,188,117,117,117,117,2,117,117,117,117,189,215,117,71,14,45,45,117,117,115,115,190,191,118,190,117,117,117,117,189,190,117,117,117,117,190,117,117,117,117,116,115,189,143,117,117,117,189,115,117,117,117,117,117,117,117,115,115,254,115,117,117,117,115,189,117,117,115,115,115,117,117,117,14,117,143,117,117,115,14,14,117,117,190,117,117,190,190,118,117,117,117,115,115,117,118,117,71,117,117,117,189,117,117,117,117,117,117,117,117,117,116,143,117,117,117,215,215,215,215,117,117,2,117,189,117,71,2,71,2,2,117,215,18,19,190,2,190,215,190,190,190,190,117,117,117,117,117,215,215,190,189,143,143,117,117,117,117,14,71,117,215,117,117,117,117,117,117,14,71,118,45,71,215,191,216,117,115,187,213,213,213,117,190,190,118,118,215,18,190,190,191,118,118,118,118,118,117,117,117,143,213,190,118,118,190,117,117,117,117,117,117,117,117,117,117,117,117,117,190,190,118,117,117,117,213,214,117,117,118,118,118,118,118,190,190,190,190,215,191,118,118,190,215,188,117,143,213,215,215,117,117,189,118,18,117,45,71,71,117,117,117,117,117,117,117,117,117,189,118,14,14,71,117,117,117,117,117,143,143,215,215,189,2,117,143,117,117,215,190,190,190,194,190,118,190,189,189,190,190,117,117,118,117,117,118,189,189,117,117,117,117,190,215,215,215,189,117,71,14,143,141,212,117,117,117,117,117,117,117,117,117,117,117,117,117,117,14,117,117,190,143,115,117,190,117,117,190,117,117,117,117,117,117,115,115,117,117,189,143,71,14,117,117,187,116,115,14,115,117,117,117,117,117,117,115,115,115,115,254,115,115,117,117,213,189,189,117,117,14,117,117,115,115,117,189,190,189,117,117,115,115,117,117,117,117,117,190,191,190,117,14,117,118,117,117,118,117,14,117,189,71,117,117,117,118,118,190,117,117,117,117,141,117,117,118,118,117,117,117,189,117,117,117,215,215,117,71,71,71,117,117,215,189,118,117,117,118,190,190,190,190,190,190,190,71,117,117,117,117,215,215,189,190,117,117,189,117,14,117,117,117,117,117,117,117,117,71,71,14,117,118,2,71,143,116,117,190,117,189,190,117,117,189,215,118,118,118,191,18,18,118,118,117,190,190,118,118,117,117,117,117,215,215,118,118,118,2,2,117,117,117,117,117,189,215,117,117,117,118,118,118,118,189,215,213,117,117,117,118,117,118,118,118,118,118,190,190,190,255,218,118,118,118,190,215,117,215,212,190,118,117,117,191,190,213,117,45,2,71,71,71,14,117,117,117,117,117,117,117,2,71,14,117,117,117,189,189,143,215,190,190,190,117,117,190,118,117,215,190,190,190,190,190,190,71,190,214,215,190,190,117,71,71,71,71,2,117,117,117,117,14,117,117,213,190,189,117,117,117,117,116,116,117,117,117,117,117,117,117,117,117,117,117,117,118,117,71,117,118,117,117,117,117,143,117,117,117,215,189,117,117,190,117,117,14,14,115,117,117,189,143,115,14,117,117,115,117,117,187,188,115,115,117,117,117,117,117,115,117,254,117,115,187,187,143,143,189,117,117,117,117,117,117,116,187,187,143,189,117,117,143,143,115,117,117,2,118,190,191,117,14,14,14,117,189,117,2,71,117,14,117,117,117,117,117,117,117,117,117,117,71,71,19,189,118,118,118,2,117,117,117,117,117,117,215,117,14,117,189,117,189,117,215,189,71,117,117,117,215,215,117,117,117,117,190,117,118,117,117,117,190,190,118,118,118,45,2,117,71,117,117,118,118,117,117,117,117,117,117,117,143,117,118,118,117,188,213,190,117,190,117,117,117,117,117,117,117,117,118,191,19,215,190,215,219,18,191,117,117,117,117,117,117,117,117,117,118,118,118,118,117,117,117,117,117,117,117,118,117,2,2,118,117,117,190,215,117,117,117,118,117,118,190,18,190,117,118,144,18,17,194,118,118,118,213,213,117,117,118,190,118,117,189,117,116,116,117,2,2,117,117,14,14,117,117,117,117,117,117,117,2,117,117,117,117,189,117,117,189,117,190,190,117,117,117,117,118,117,190,117,190,190,218,215,215,117,117,117,117,117,191,117,117,117,71,71,117,117,213,190,117,117,117,189,117,117,117,117,117,117,117,143,116,117,117,117,117,118,118,117,117,189,117,117,117,190,117,71,71,117,117,117,117,117,14,115,187,215,217,215,143,117,117,118,117,117,115,117,117,187,188,187,211,117,117,117,117,117,14,117,14,115,187,117,117,117,117,117,117,117,254
treesize:dw 10593

sky:db 30,30,29,30,30,29,29,101,101,101,101,30,30,30,30,30,30,30,30,30,30,15,15,30,30,30,101,77,77,77,53,53,53,53,53,77,77,77,77,77,77,77,77,77,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,101,101,101,101,101,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,29,29,29,29,29,29,29,101,101,101,101,30,30,30,30,30,30,30,30,15,15,15,15,15,30,30,30,101,77,53,53,53,53,53,53,53,53,53,78,77,77,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,101,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,102,29,29,30,29,29,29,101,101,101,101,30,30,30,30,30,30,30,30,15,15,15,15,15,30,30,30,101,101,77,53,53,53,53,53,53,53,53,77,77,77,77,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,101,101,101,101,101,101,101,101,29,29,29,101,101,29,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,102,101,29,30,29,29,29,101,101,101,101,30,30,30,30,30,30,30,30,30,15,15,15,15,15,30,30,30,101,77,53,53,53,53,53,53,53,53,77,77,77,77,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,101,29,29,29,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,101,101,29,29,29,29,29,101,101,101,101,30,30,30,30,30,30,30,30,30,15,15,15,15,30,30,30,30,101,77,53,53,53,53,53,53,53,53,78,77,77,77,77,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,101,101,29,29,29,29,29,29,29,101,101,101,101,101,101,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,30,30,29,29,29,29,29,101,101,101,29,30,30,30,30,30,30,30,30,30,30,30,15,15,30,30,30,30,30,101,77,53,53,53,53,53,53,53,53,77,77,77,77,101,101,101,101,101,101,101,101,101,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,29,29,101,101,101,29,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,30,30,30,29,29,29,29,101,30,30,30,30,30,30,30,30,30,30,30,30,30,15,15,30,30,30,30,30,30,30,101,53,53,53,53,53,53,53,53,53,78,53,53,78,102,101,29,30,101,101,77,53,77,77,102,101,101,101,102,101,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,101,101,101,101,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,102,30,30,30,30,30,30,30,30,30,30,30,30,30,254,30,30,30,29,101,29,29,101,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,53,53,53,53,53,53,53,53,78,77,77,78,102,101,29,30,30,30,30,101,77,102,77,102,101,101,101,102,101,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,29,29,101,101,101,101,101,101,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,15,30,30,30,30,30,102,30,30,30,30,30,30,30,30,30,30,30,30,30,254,30,30,30,29,101,29,101,102,101,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,53,53,53,53,53,53,53,53,78,77,77,77,77,101,30,30,30,30,30,30,101,101,77,102,101,101,101,102,102,101,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,101,29,29,29,29,29,29,101,29,101,29,101,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,29,30,30,30,30,30,30,30,30,30,30,30,30,254,101,101,101,101,101,29,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,77,53,53,53,53,53,53,53,77,77,77,77,101,29,30,30,30,30,30,101,101,101,102,102,101,101,101,102,102,101,102,101,101,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,29,29,29,101,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,101,102,102,101,101,101,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,77,53,53,53,53,53,53,78,77,101,101,29,30,30,30,30,30,30,101,101,102,102,102,101,101,101,101,101,101,101,101,102,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,101,29,29,29,29,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,101,78,102,101,101,101,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,77,53,53,53,53,53,77,77,101,102,30,30,30,30,101,101,101,101,102,102,101,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,29,29,29,101,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,101,78,102,101,101,101,101,101,101,101,101,101,101,101,101,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,53,53,53,53,53,77,102,101,101,29,30,101,101,101,101,101,101,102,102,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,77,102,102,102,102,102,102,102,101,101,101,101,101,101,101,101,101,102,102,102,102,101,101,101,101,101,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,101,101,29,29,29,101,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,254,101,102,102,101,101,101,101,102,102,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,29,101,77,53,53,53,53,77,102,102,102,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,102,102,101,102,102,102,102,102,101,101,101,101,101,101,101,101,101,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,29,101,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,30,30,30,30,30,30,30,30,30,30,30,30,254,101,102,77,102,102,102,102,102,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,29,101,77,53,53,53,53,77,102,102,101,101,101,101,102,102,102,102,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,77,77,77,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,101,101,102,101,101,102,101,101,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,101,101,30,30,30,30,30,30,30,30,30,30,30,254,101,77,78,102,102,102,101,102,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,29,101,77,53,53,53,53,77,102,102,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,102,102,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,77,77,101,30,30,30,30,30,30,30,30,30,30,254,101,78,78,78,102,102,102,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,101,102,78,53,53,53,77,102,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,102,102,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,77,102,101,30,30,30,30,30,30,30,30,30,101,254,101,77,78,77,77,102,102,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,29,30,30,30,30,30,30,30,29,101,101,102,101,77,53,53,78,77,101,102,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,102,102,102,102,102,102,101,101,101,101,102,28,28,102,101,101,101,101,101,101,101,101,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,102,102,101,29,29,30,30,30,101,101,30,30,101,254,101,101,78,77,77,102,102,102,101,101,101,101,101,101,101,101,101,30,30,30,30,30,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,77,53,78,77,102,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,101,102,102,102,102,101,101,101,101,101,101,101,101,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,77,77,101,101,101,101,101,101,101,101,30,30,101,254,101,101,102,77,77,102,102,102,101,101,101,101,101,101,101,101,101,30,101,101,101,101,101,101,101,101,101,101,101,101,101,102,102,77,101,101,101,101,53,53,53,102,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,102,102,102,102,102,102,101,101,101,101,29,101,101,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,77,77,77,101,101,101,101,101,30,30,30,101,101,254,101,101,102,102,77,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,102,102,102,101,30,30,101,77,53,53,101,30,30,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,77,102,102,102,102,77,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,102,102,102,102,101,102,28,101,29,101,29,29,29,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,77,78,77,77,101,77,77,101,30,30,101,101,101,254,101,102,102,102,77,77,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,77,101,101,101,101,101,101,101,101,30,30,30,101,77,77,77,30,30,30,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,102,77,102,77,77,102,77,77,102,77,77,77,77,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,29,101,29,101,101,29,30,30,30,30,30,30,30,30,30,30,30,30,30,101,77,53,53,77,77,77,77,101,30,30,101,101,101,254,101,102,102,77,77,77,77,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,101,101,101,30,30,30,30,30,30,30,30,101,101,101,30,30,30,30,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,29,101,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,101,101,77,53,77,77,77,77,77,102,101,101,101,101,254,102,102,77,77,77,77,77,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,101,30,30,30,30,30,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,102,102,77,77,102,77,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,102,102,101,101,101,101,101,29,101,101,101,101,29,30,30,30,30,30,30,30,30,30,101,101,101,77,77,77,77,77,78,78,101,101,101,77,254,77,77,77,77,77,77,77,77,77,77,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,77,77,102,102,102,102,102,77,102,77,102,102,102,102,77,77,78,78,77,77,77,77,78,78,78,78,78,78,77,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,29,29,30,30,30,30,30,30,30,101,101,101,101,102,77,77,77,77,102,101,101,101,102,77,254,77,77,77,77,77,77,102,77,77,77,77,77,102,102,101,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,101,101,102,102,102,102,102,102,77,77,77,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,77,77,77,77,77,77,77,77,102,102,77,77,78,77,77,77,77,77,77,77,77,77,77,77,77,78,78,78,78,78,78,78,102,102,102,102,102,102,102,77,77,77,102,102,102,102,102,102,102,102,102,102,102,102,101,102,102,101,101,101,101,101,101,101,29,29,30,30,30,30,30,30,29,101,101,101,101,101,77,77,77,77,101,101,101,77,77,77,254,77,77,77,77,77,77,77,77,77,77,102,102,77,102,102,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,101,101,101,102,102,102,102,102,102,77,77,77,77,77,77,77,77,102,102,102,102,102,102,102,102,102,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,78,78,78,78,77,78,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,102,102,102,102,102,102,102,102,102,101,102,101,101,101,101,101,101,29,29,30,30,30,30,30,29,101,101,101,101,101,101,77,53,53,77,101,101,77,78,77,77,254,77,77,77,77,77,77,77,77,77,77,102,102,102,102,102,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,77,77,77,77,102,102,102,102,102,102,102,102,77,77,77,77,77,77,77,77,77,77,77,77,77,77,78,77,77,77,77,77,77,78,77,77,77,77,77,77,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,77,77,102,102,102,102,102,102,102,102,101,101,101,101,101,101,101,29,30,30,30,30,30,101,29,101,101,101,102,77,78,53,53,53,77,101,77,77,77,77,254,77,77,78,77,77,77,77,77,77,77,77,102,102,102,102,102,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,30,101,29,29,29,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,77,77,102,77,102,102,102,102,102,102,102,77,77,77,77,77,77,77,77,77,77,77,77,78,77,78,78,77,77,78,78,77,78,78,78,78,77,77,78,78,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,77,77,102,102,102,102,102,102,102,101,101,101,101,101,101,101,101,29,29,29,101,29,30,29,29,101,102,101,101,77,53,53,53,53,77,77,77,53,77,77,254,77,77,78,77,77,77,77,77,77,77,77,102,102,102,102,102,101,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,29,29,29,29,101,101,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,77,77,77,77,77,77,77,77,78,78,77,77,78,78,78,78,78,78,78,78,78,78,78,77,77,78,78,78,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,77,77,102,102,102,102,102,102,101,101,101,101,101,101,101,101,101,29,101,101,30,30,29,101,101,101,77,77,77,53,53,53,53,53,77,77,53,53,77,254,53,77,78,77,77,77,77,77,77,77,77,102,102,102,102,102,102,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,30,30,30,29,101,29,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,77,77,77,77,77,77,77,77,77,78,78,77,78,77,78,78,78,78,78,78,78,78,78,77,77,78,78,78,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,102,77,102,102,102,102,102,102,102,101,101,101,101,101,101,101,101,29,29,29,30,30,29,101,101,77,77,77,77,78,53,53,53,53,53,53,53,53,53,254,53,77,77,78,77,77,77,77,77,77,77,102,102,102,102,102,102,101,101,101,101,101,101,101,101,30,30,30,30,30,30,30,30,30,30,29,29,29,29,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,77,77,77,77,77,77,77,77,78,78,77,78,77,78,77,77,78,78,78,78,78,77,77,77,78,78,77,78,77,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,102,102,102,102,102,102,102,102,101,101,101,101,101,101,101,29,101,101,29,30,29,29,29,101,77,77,77,77,53,53,53,53,53,53,53,53,53,53,254,53,77,77,77,77,77,77,77,77,77,77,77,77,102,102,102,102,101,101,101,101,101,101,101,101,29,30,30,30,30,30,30,30,30,30,29,29,29,29,101,101,101,101,101,101,101,101,101,102,101,101,101,101,101,101,102,102,102,101,101,102,102,102,102,102,77,77,77,77,78,102,102,102,102,77,77,77,77,78,77,78,78,77,78,77,78,77,77,78,78,78,78,78,77,77,77,78,78,78,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,102,102,102,102,102,102,102,101,101,101,101,101,101,29,29,29,29,29,29,29,29,29,101,77,53,53,53,53,53,53,53,53,78,53,53,53,53,254,77,77,77,77,77,77,77,77,77,77,77,77,102,77,77,102,102,102,102,101,101,101,101,101,101,101,30,30,30,29,101,30,30,30,30,29,101,101,101,101,29,101,101,101,101,102,102,101,101,102,102,102,102,102,102,102,101,101,101,101,101,102,102,102,102,77,77,77,77,78,102,102,102,102,102,77,77,77,78,78,77,77,77,78,77,78,77,77,78,78,78,78,78,78,77,78,78,78,78,78,78,78,77,78,78,78,78,78,78,78,78,78,78,78,78,77,77,77,77,77,77,77,102,102,102,102,77,102,102,102,101,102,101,101,101,29,29,101,101,29,101,29,29,29,101,101,77,77,53,53,53,53,53,53,53,53,53,53,53,77,254,77,77,77,77,77,77,77,77,77,77,77,77,102,77,77,102,102,102,102,102,101,101,101,101,101,29,30,30,30,101,101,29,30,30,30,29,101,101,101,101,101,101,101,101,101,102,102,102,101,102,102,102,101,102,102,101,101,101,101,101,101,102,102,102,102,77,77,77,77,78,102,102,102,102,102,102,77,77,78,78,78,77,78,78,77,78,78,78,78,78,78,78,78,78,78,78,78,78,77,78,78,78,78,78,78,78,78,78,78,78,77,78,78,78,78,77,77,77,77,77,77,77,77,77,102,102,102,102,102,102,102,102,102,101,101,29,29,29,29,29,29,29,101,29,101,101,77,53,53,53,53,53,53,53,53,53,53,53,53,77,254,77,78,78,77,77,77,77,77,77,77,77,77,77,78,102,102,102,102,102,102,101,101,101,101,101,29,30,30,30,29,101,101,29,30,30,29,101,101,101,101,101,101,101,101,101,102,102,102,102,102,102,102,101,102,102,101,101,101,101,101,102,102,102,101,102,77,77,77,77,102,102,102,102,102,102,102,77,77,77,78,77,77,77,77,77,77,77,77,78,78,78,78,78,78,78,78,78,78,77,77,77,78,78,78,78,78,78,78,78,78,78,78,78,78,78,77,77,77,77,77,77,77,77,77,102,102,102,102,102,102,102,102,102,102,101,101,29,29,101,29,101,101,101,101,101,101,77,53,53,53,53,53,53,53,53,53,53,53,53,77,254,77,78,78,77,77,77,77,77,77,77,77,78,78,78,78,102,102,102,102,102,102,101,101,101,101,101,101,29,29,101,101,101,29,30,101,29,101,101,101,101,101,101,101,102,101,102,102,102,102,102,102,102,101,102,102,102,102,101,101,101,102,102,102,101,102,77,77,78,102,102,102,102,102,102,102,102,77,77,77,78,77,78,77,77,78,77,77,78,77,78,78,78,78,78,77,78,78,78,77,77,77,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,77,77,77,77,77,77,77,78,102,78,102,77,77,102,102,102,102,102,101,101,101,101,101,101,102,102,101,102,77,77,53,53,53,53,53,53,53,53,53,53,53,53,78,254,78,77,78,77,77,77,77,77,77,77,77,78,78,78,78,77,77,77,102,102,102,102,101,101,101,101,101,101,101,101,101,101,29,29,101,101,101,101,101,101,101,101,101,101,102,101,101,101,102,102,102,102,102,102,102,102,102,101,101,102,102,102,102,77,77,102,102,102,102,102,102,102,102,102,102,102,102,77,77,78,78,78,77,77,78,78,78,78,78,78,78,78,78,78,77,78,78,78,77,78,77,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,77,77,77,77,77,77,78,78,78,78,77,77,77,102,102,102,102,102,101,101,102,102,102,102,102,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,78,254,53,78,77,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,102,102,101,101,101,101,101,101,101,101,101,101,29,101,101,29,101,101,101,101,101,101,101,101,101,102,102,102,102,102,101,102,102,101,101,102,102,101,102,102,102,102,77,77,102,102,102,78,102,102,102,102,102,102,102,102,77,78,78,77,77,77,77,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,77,77,77,77,77,77,77,102,102,102,102,102,77,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,53,77,77,77,77,77,77,78,77,78,77,78,78,77,77,77,77,77,77,77,77,77,77,102,102,101,101,101,102,77,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,102,102,101,101,102,102,101,102,102,102,102,102,102,102,77,77,77,102,102,78,78,77,77,78,78,78,78,78,77,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,77,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,53,53,53,53,53,53,53,53,78,77,53,78,53,53,53,53,254,53,53,53,78,78,78,77,77,77,78,78,78,78,78,78,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,102,101,101,102,102,102,77,77,77,102,102,102,77,77,102,102,102,102,102,78,78,77,77,77,77,77,78,78,78,78,77,77,77,77,77,77,78,78,78,78,78,78,78,78,78,78,78,53,53,78,78,77,77,78,78,78,78,77,77,77,78,78,78,78,78,78,78,78,78,78,78,78,78,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,77,77,77,53,53,53,53,254,53,53,53,53,53,53,78,78,78,78,77,77,77,78,78,78,78,77,77,78,78,77,77,77,77,77,77,77,77,77,77,77,102,102,102,102,102,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,77,77,77,77,77,77,77,77,77,77,77,78,78,78,77,77,77,78,78,77,77,77,78,78,77,77,77,77,78,77,78,78,78,78,78,78,78,78,78,78,78,78,53,53,53,53,78,78,77,78,53,53,53,77,77,77,78,78,78,78,78,78,78,77,78,78,77,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,77,77,77,77,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,78,78,102,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,78,78,77,77,77,77,77,53,53,53,77,77,77,77,77,77,77,77,77,77,77,102,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,53,53,53,77,77,77,77,77,77,77,77,77,77,77,78,78,77,77,77,78,77,77,77,77,77,77,77,77,77,78,78,78,78,78,77,78,78,78,78,53,53,53,53,53,53,77,53,53,53,53,78,78,77,77,78,78,78,77,78,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,78,77,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,78,78,78,78,102,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,78,78,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,77,77,77,77,77,53,53,78,77,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,78,77,77,77,77,77,77,77,77,77,77,77,78,53,78,78,78,77,77,77,77,78,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,78,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,78,78,78,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,78,77,77,77,77,53,53,53,53,53,53,53,53,53,77,77,77,77,77,77,78,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,77,102,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,78,77,77,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,78,78,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,78,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,78,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,53,53,53,53,53,53,53,53,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,77,53,53,53,53,53,53,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,53,53,53,53,53,53,53,53,77,77,77,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254,53,77,53,53,77,77,53,77,77,77,77,77,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,77,77,77,77,77,77,77,77,77,53,77,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,254
skysize:dw 9660

sun: db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,44,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,44,255,255,255,255,44,44,255,255,255,255,44,255,255,255,255,255,255,254,255,255,255,255,255,255,44,44,255,255,255,44,44,255,255,255,44,44,255,255,255,255,255,255,254,255,255,255,255,255,255,255,44,44,255,255,44,44,255,255,44,44,44,255,255,255,255,255,255,254,255,255,255,255,255,255,255,44,44,255,255,255,255,255,255,44,44,44,255,255,255,255,255,255,254,255,255,44,44,255,255,255,255,255,255,44,44,44,44,44,255,255,255,255,255,255,44,255,255,254,255,255,255,44,44,44,255,255,44,44,44,44,44,44,44,44,67,255,44,44,44,44,255,255,254,255,255,255,68,44,44,255,44,44,44,44,44,44,44,44,44,44,255,44,44,44,255,255,255,254,255,255,255,255,44,255,255,44,44,44,44,44,44,44,44,44,44,65,255,45,255,255,255,255,254,255,255,255,255,255,255,44,44,44,44,44,44,44,44,44,44,44,43,255,255,255,255,255,255,254,255,255,44,44,44,255,44,44,44,44,44,44,44,44,44,44,44,43,255,255,44,44,44,255,254,255,44,44,44,44,255,44,44,44,44,44,44,44,44,44,44,44,43,255,255,44,44,44,255,254,255,255,255,44,255,255,44,44,44,44,44,44,44,44,44,44,44,43,255,255,44,44,255,255,254,255,255,255,255,255,255,255,44,44,44,44,44,44,44,44,44,44,65,255,255,255,255,255,255,254,255,255,255,255,44,255,255,44,44,44,44,44,44,44,44,44,43,255,255,44,255,255,255,255,254,255,255,255,44,44,44,255,68,44,44,44,44,44,44,44,43,65,255,44,44,44,255,255,255,254,255,255,44,44,44,44,255,255,67,44,44,44,44,44,43,65,255,255,44,44,44,44,255,255,254,255,255,44,255,255,255,255,255,255,255,43,43,43,43,65,255,255,255,255,255,255,44,255,255,254,255,255,255,255,255,255,255,44,44,255,255,255,255,255,255,44,44,44,255,255,255,255,255,255,254,255,255,255,255,255,255,255,44,44,255,255,44,44,44,255,44,44,255,255,255,255,255,255,255,254,255,255,255,255,255,255,44,44,255,255,255,44,44,44,255,255,44,44,255,255,255,255,255,255,254,255,255,255,255,255,255,44,255,255,255,255,44,44,255,255,255,255,44,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,44,44,255,255,255,255,255,255,255,255,255,255,255,254
sunsize:dw 600

car: db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,23,90,163,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,162,65,65,6,162,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,90,66,66,6,65,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,90,90,91,90,89,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,162,138,90,30,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,21,24,26,8,90,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,28,29,30,29,90,90,162,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,90,90,66,90,90,66,90,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,127,178,28,255,255,255,255,28,90,30,89,66,89,255,255,255,255,255,129,129,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,129,177,177,26,255,255,255,255,255,90,90,91,90,90,90,138,27,255,255,129,177,177,177,177,127,127,129,130,154,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,129,176,153,255,255,255,255,255,255,255,91,139,139,28,30,29,234,7,15,27,129,105,177,177,178,178,177,177,177,177,105,106,127,129,129,155,7,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,155,129,129,153,255,255,255,255,255,255,255,255,255,255,255,255,255,30,18,26,15,25,129,177,177,177,177,177,178,177,177,177,178,177,177,178,177,106,106,127,129,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,129,127,129,153,255,255,255,255,255,255,255,255,255,255,255,255,255,15,29,17,26,15,25,129,177,177,177,178,127,127,127,178,178,127,127,178,178,127,127,127,106,106,106,127,130,154,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,129,106,129,201,255,255,255,255,255,255,255,255,255,255,255,255,255,255,15,7,255,27,15,24,178,177,106,127,127,127,129,129,127,127,127,127,127,127,127,129,129,129,129,129,129,129,127,127,129,129,155,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,27,155,154,154,153,154,255,255,255,255,255,255,255,255,255,255,255,177,105,129,201,225,255,255,255,255,255,255,255,255,255,255,255,255,255,26,154,153,201,7,30,129,106,127,127,127,177,177,177,177,177,177,177,177,177,178,177,177,177,177,177,177,177,178,177,177,178,177,177,178,105,106,178,255,255,254,255,255,255,255,255,255,255,255,130,177,178,177,178,177,178,106,106,127,129,103,255,255,255,255,255,255,255,255,255,127,127,127,127,129,129,129,178,129,129,129,127,127,178,178,178,127,127,127,127,127,106,127,127,129,127,178,177,177,177,177,177,178,177,177,177,177,177,177,177,177,177,177,177,177,178,178,178,177,178,177,177,177,105,106,106,105,255,255,254,255,255,255,255,255,255,30,81,130,129,129,130,130,155,155,155,155,155,155,154,154,130,129,153,129,129,129,127,127,106,178,106,106,105,105,106,177,177,177,177,178,177,178,178,178,177,177,177,177,178,177,177,177,176,177,177,177,177,177,177,177,178,177,177,177,178,177,178,178,177,177,177,178,177,178,178,177,177,177,177,177,177,177,177,178,106,255,255,254,255,255,28,103,26,25,154,130,129,129,129,129,128,129,128,177,177,177,177,177,177,177,177,177,176,177,177,177,177,177,178,178,178,178,178,178,178,178,177,177,177,178,178,178,178,178,177,177,177,178,178,178,177,177,177,177,177,178,177,177,178,178,177,177,177,177,177,177,177,177,177,105,178,178,177,177,177,177,177,177,177,177,177,177,177,177,255,255,254,28,154,129,177,177,177,177,177,178,177,177,177,177,177,177,105,105,178,178,177,178,178,178,178,177,178,178,177,177,177,105,178,178,177,178,178,178,178,178,178,178,178,178,177,177,177,178,178,178,177,178,178,177,177,178,177,178,178,178,105,177,106,106,178,201,230,234,210,18,18,18,202,177,105,177,177,177,177,177,177,177,177,177,177,177,255,255,255,254,129,127,177,177,177,177,127,177,177,106,106,178,178,105,105,106,106,106,178,177,178,178,177,177,177,177,177,177,178,178,178,178,177,177,178,178,178,178,178,178,178,178,178,178,178,106,106,106,178,178,178,177,178,178,106,178,127,106,177,105,177,129,129,234,211,18,17,17,17,17,17,211,235,201,177,177,177,106,177,106,106,177,177,177,175,255,255,255,254,27,105,178,178,177,177,177,177,130,129,177,177,199,200,200,199,177,178,177,178,178,177,178,177,178,177,178,178,178,177,177,178,178,178,177,177,178,177,177,178,177,177,178,177,105,106,106,178,177,178,106,178,178,106,106,106,106,127,177,177,129,154,211,211,17,18,210,210,137,211,210,17,211,213,21,177,106,106,106,106,106,106,106,177,255,255,255,255,254,255,106,105,177,178,178,177,129,155,177,200,234,138,138,138,210,17,200,199,177,106,178,178,177,177,177,177,178,177,178,178,177,178,178,177,178,177,177,178,178,177,177,177,178,106,106,177,177,178,106,105,106,106,106,106,106,106,106,178,106,202,236,235,18,19,137,137,137,210,210,137,138,207,213,91,80,106,106,106,106,105,105,106,255,255,255,255,255,254,255,153,178,127,106,127,127,178,200,18,138,138,210,210,210,138,138,209,255,200,106,106,178,178,178,178,177,178,177,178,178,178,178,178,178,178,178,177,178,106,106,178,105,177,178,178,178,178,177,178,106,106,178,106,105,106,106,105,106,106,21,212,18,231,138,210,22,25,26,26,21,210,138,18,26,28,129,106,106,178,106,105,177,255,255,255,255,255,254,255,255,29,153,178,106,127,177,17,138,138,21,25,27,24,20,232,137,18,255,200,106,106,178,178,178,177,177,178,177,178,105,178,178,106,106,106,106,106,106,106,105,106,178,177,178,178,178,178,178,178,178,178,177,178,177,178,105,178,177,21,21,18,137,138,23,24,25,15,26,26,175,210,137,19,237,225,178,177,177,105,105,129,255,255,255,255,255,254,255,255,255,255,177,127,127,200,210,138,21,25,30,30,20,23,22,211,138,255,17,199,105,178,177,178,178,177,178,178,177,178,178,106,106,105,105,106,106,105,106,178,106,105,105,106,178,178,106,105,106,106,105,105,105,178,178,178,106,129,235,18,234,138,235,30,23,237,30,224,237,28,227,138,211,26,7,154,128,129,255,255,255,255,255,255,255,255,254,255,255,255,255,202,105,199,17,139,234,27,234,24,25,19,29,7,19,138,209,8,24,177,177,178,178,177,106,106,106,106,106,178,106,106,105,105,106,106,106,106,105,106,106,106,106,106,106,106,106,106,105,106,106,105,105,178,177,106,153,24,226,210,138,163,30,30,7,28,25,29,30,175,137,212,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,153,25,138,8,30,29,24,21,27,7,25,21,138,209,23,29,24,178,177,177,177,106,127,177,178,178,106,106,106,105,105,105,177,105,106,105,106,127,178,106,106,178,178,178,177,177,177,177,128,128,178,177,129,25,27,21,234,138,170,226,235,7,28,30,15,30,175,137,138,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,138,23,27,23,26,29,7,18,21,22,138,233,8,7,27,201,199,201,152,226,152,175,153,154,129,177,178,127,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,26,23,234,138,8,25,7,27,24,30,15,30,21,137,138,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,138,161,25,19,28,24,27,30,24,22,139,230,23,7,27,25,25,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,20,137,209,26,15,152,211,15,30,25,160,136,26,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,163,137,24,28,30,234,21,27,22,138,138,17,18,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,174,232,137,207,24,24,25,26,174,231,136,137,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,138,138,161,8,8,8,22,162,138,17,233,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,21,209,136,137,234,21,230,136,136,137,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,161,137,136,137,137,137,210,234,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,175,233,136,136,136,136,137,211,28,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,161,137,160,21,25,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,24,24,25,24,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,254
carsize:dw 3026

rabbit1: db 255,255,255,255,255,255,161,160,160,25,255,255,255,255,255,255,255,255,255,255,255,255,25,161,161,158,255,255,255,255,254,255,255,255,255,255,158,91,91,91,91,137,26,255,255,255,255,255,255,255,255,155,65,91,91,91,66,133,255,255,255,254,255,255,255,255,255,156,91,90,87,88,91,137,255,255,255,255,255,255,255,25,65,90,88,88,91,255,155,255,255,255,254,255,255,255,255,255,255,135,91,88,88,88,91,134,255,255,255,255,255,255,136,91,87,88,88,91,134,255,255,255,255,254,255,255,255,255,255,255,255,255,90,89,87,157,135,255,255,28,28,255,26,135,157,88,88,91,160,255,255,255,255,255,254,255,255,255,255,255,255,255,157,89,133,160,90,15,15,15,15,15,15,15,15,29,134,157,89,155,255,255,255,255,255,254,255,255,255,255,255,255,255,255,134,91,15,15,15,15,15,15,15,15,15,15,15,15,90,181,255,255,255,255,255,255,254,255,255,255,255,255,255,255,160,92,15,15,28,28,15,15,15,15,15,15,7,29,15,15,91,134,255,255,255,255,255,254,255,255,255,255,255,255,160,91,15,15,26,180,180,26,15,15,15,15,156,180,181,29,15,91,91,134,255,255,255,255,254,255,255,255,255,155,133,90,15,15,15,156,7,100,156,15,15,15,15,133,100,175,27,15,15,91,88,156,155,255,255,254,255,255,255,255,255,160,158,133,29,15,15,156,27,15,29,26,26,30,30,26,26,15,15,28,133,159,135,255,255,255,254,255,255,255,155,131,161,91,15,30,15,15,15,15,30,203,180,180,180,15,15,15,15,15,30,15,91,135,255,154,255,254,255,255,255,255,157,88,156,30,15,15,15,15,30,15,30,156,156,15,15,30,15,15,15,15,7,157,137,156,255,255,254,255,156,159,255,255,91,91,15,15,15,15,15,30,156,156,156,156,156,156,30,15,15,15,15,15,91,66,155,255,255,254,156,255,15,15,91,91,91,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,91,65,255,255,255,254,157,91,15,15,91,91,91,15,29,25,7,15,15,15,15,15,15,15,15,15,15,7,26,30,15,91,137,255,255,255,254,255,135,255,91,7,88,91,157,26,90,89,133,30,15,15,15,15,15,15,30,134,89,90,161,158,91,134,255,255,255,254,255,255,255,255,255,156,65,158,90,28,158,89,156,15,15,15,15,15,15,156,90,158,90,161,159,137,255,255,255,255,254,255,255,255,255,255,255,26,26,26,158,157,28,28,90,90,90,90,90,90,7,26,133,158,133,133,156,255,255,255,255,254
rabbit2: db 255,255,255,255,255,255,161,160,160,25,255,255,255,255,255,255,255,255,255,255,255,255,25,161,161,158,255,255,255,255,254,255,255,255,255,255,158,91,91,40,91,137,26,255,255,255,255,255,255,255,255,155,65,40,91,91,66,133,255,255,255,254,255,255,255,255,255,156,91,40,87,88,91,137,255,255,255,255,255,255,255,25,65,90,40,88,91,255,155,255,255,255,254,255,255,255,255,255,255,135,91,88,88,88,91,134,255,255,255,255,255,255,136,91,87,88,40,91,134,255,255,255,255,254,255,255,255,255,255,255,255,255,90,89,87,157,135,255,255,28,28,255,26,135,157,88,88,91,160,255,255,255,255,255,254,255,255,255,255,255,255,255,157,89,133,160,90,15,15,15,40,15,15,15,15,29,134,157,89,155,255,255,255,255,255,254,255,255,255,255,255,255,255,255,134,91,15,15,15,15,15,15,15,15,15,15,40,15,90,181,255,255,255,255,255,255,254,255,255,255,255,255,255,255,160,92,15,40,40,28,40,15,15,15,40,15,7,40,15,15,91,134,255,255,255,255,255,254,255,255,255,255,255,255,160,91,15,15,40,40,40,40,15,15,15,40,40,40,40,29,15,91,91,134,255,255,255,255,254,255,255,255,255,155,133,90,15,15,15,156,40,40,156,15,15,15,15,40,40,40,27,15,15,91,88,156,155,255,255,254,255,255,255,255,255,160,158,133,29,15,40,40,27,40,40,26,26,40,40,26,40,40,15,28,133,159,135,255,255,255,254,255,255,255,155,131,161,91,15,30,15,40,40,15,30,203,180,180,180,15,15,40,40,15,30,40,91,135,255,154,255,254,255,255,255,255,157,88,40,30,15,15,40,40,30,15,30,156,156,15,15,30,40,40,15,15,40,157,137,156,255,255,254,255,156,159,255,255,91,40,15,15,15,40,40,30,156,156,156,156,156,156,30,40,40,15,15,40,91,66,155,255,255,254,156,255,15,15,91,91,40,15,15,15,15,40,15,15,15,15,15,15,15,15,40,15,15,15,40,91,65,255,255,255,254,157,91,15,15,91,91,40,15,29,25,7,15,15,15,15,15,15,15,15,15,15,7,26,30,40,91,137,255,255,255,254,255,135,255,91,7,88,40,157,26,90,89,40,30,15,15,15,15,15,15,30,134,89,90,161,158,91,134,255,255,255,254,255,255,255,255,255,156,65,158,90,28,158,40,156,15,15,15,40,40,15,156,90,158,90,161,159,137,255,255,255,255,254,255,255,255,255,255,255,26,26,26,158,157,28,28,90,90,90,90,40,90,7,26,133,158,133,133,156,255,255,255,255,254
rabbitsize: dw 570

sound_index: dw 0
filename: db 'perry.wav',0
sound_size:dw 11000
filehandle: dw 0

startfilename:db 'A.txt',0
startfilehandle:dw 0

carrot: db 255,255,255,255,255,255,255,255,255,230,255,254,255,255,255,255,255,255,190,70,70,69,255,254,255,255,255,255,255,17,117,70,70,70,255,254,255,255,255,111,6,42,186,138,138,137,255,254,255,255,6,42,42,42,66,255,255,255,255,254,255,42,42,42,42,42,255,255,255,255,255,254,42,42,42,42,42,6,255,255,255,255,255,254,42,42,42,42,6,255,255,255,255,255,255,254,42,42,6,6,255,255,255,255,255,255,255,254,6,6,255,255,255,255,255,255,255,255,255,254
carrotsize:dw 121