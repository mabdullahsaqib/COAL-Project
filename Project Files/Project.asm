[org 0x0100]
jmp start
sunarray:db '@@@@@@@@@&&&&&@]@@@@@@@&&&&&&&&&]@@@@@@&&&&&&&&&&&]@@@@@@@&&&&&&&&&]@@@@@@@@@&&&&&]',0
mountainrange: db '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.:.]@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@:-:++=-::.]@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@:::-:-:=++=---:--.]@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@::---=:-==-:--=-==-:==-@@.@@@@@@@@@@@@@@@.-+:++:]@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-@@@:@@@@@@@@@@@@@-::-===-::-=::--=-::-:-:-::=-.@@@@@@@@@@@@==-+=#**]@@@@@@@@@@@@@@@@@@@@@@@@@@@@@-.=::+=.:*+*:=+.@....=:.-=-::::--::-:--====---::=-::.@@@@@@@-+++--+==-**=]@@@@@@@@@@@@@@@@@@@@@@@@@@::-+*+=++*+==**++++--:::=::-=--:----::-::-:-----=-:-----::.@@@=###***+*##**+*-]@@@@@@@@@@@@@@@@@@@@@@@@@@-===**==++=*=+*+**+==-----::--:::----::-:-:---:---:-----:::.@:#####+*+*#*****+=]@@@@@@@@@@@@@@@@@@@@@@@:====++=****+=++=++*++=+-----:::--:--::-::---:::::--:------:-:.+######*+*##*+=++++-:-..]@@@@@@@@@@-===-==-==---=--:-===:::--=-==--=-==++=-==++==:-=::.-++++++++=+**+***#*###*####*+*++*##*+=+==....:.-**+==++==+==--]@@@@@@@@.====--=-=-=--::-:-----::::::::--:....:-=====+=+====--+++++==++***#*+**#*##********+#*+*+++=+::==....:.-**++=====-::::]@@.....:=-::==----:::-::::::::-:::::.....::---::::::--============---++++**#**++*******=+++=+++++*+==:==....:.-**+::::---:-:-==::.]..::--::.:--:.::::.:-::..........:::::::::--::::::-:::.:-:::::----+++**+++++++++**+++=====++***++==-...::::.....=+-==+=:-:-=-.::....]', 0
clouds : db '@@@@@@@@@@--.]@@@@@@@.-=-===-]@@@@.:::======--...]@@-=++*+=====++==++-]@@@@-+-=++*+=++++-]',0
groundtop: db   '---===+++++++=========+++***+++++**++=====--====++++++++++++++++++++++++++++++++---===+++++++=========+++***+++++**++=====--====++++',0
groundbottom:db '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++---===+++++++=========+++***+++++**++=====--====++++',0
horse: db '@@@@..]@@@@##=----]@@@.+#@######%%-.]@@.+######*########*+==+++==-]@.===@@@@@@@##################+%%=]@@@@@@@@@@@@#################%-+%%#]@@@@@@@@@@@@@:###%%+@@@*#####*@@@%%-]@@@@@@@@@@@@@@=##%%#@@@@.#%%###*@@@%-]@@@@@@@@@@@@@@@*#@-%+@@@@#%.@-#=]@@@@@@@@@@@@@@-#*@*%*@@@@=%#@@+#+]@@@@@@@@@@@@-==:@==:@@@-==:@@==.]',0
saddle: db '*=@@@.::.]@+*+=++*%#=]@:+##%%*+=:]@@@@**%+=-]@@@@@.:.]@@@@:...:]', 0
barn: db '@@@@@@@@@@@@@@@@@@@@@:.]@@@@@@@@@@@@@@@@@@@@.-**=:]@@@@@@@@@@@@@@@@@@@@++#*#-@@.:.]@@@@@@@@@@@@@@@@@@@@=*##*---+*#%#-]@@@@@@@@@@@@@@@..::-====-+##%####%*:]@@@@@@@@@@@.:----======+*####+****##%+]@@@@@@@@@:-===========+######=**#=###%#.]@@@@@@@.-===========+*%######=**#=####%%-]@@@@@@:===========*#%########+***+****##%%=]@@..--========+*#%%################*#****#%%%+.]@:+****################%%#########*******####.]@@+****++****++*#%##**#%%%%*###+###*%###***##]@:==+==+++-=+-=+#%%+++*%%%#=+++-+++=####-==##]@=+=++++*+==*-+*%%%*++*%%##++++-++++####=++##]@@+####*#*****##%%%##%%%%%#+++*=*+*+%%%%#####.]' ,0
doors : db '@-=+=-=+=@-=+=:+==]@-=+=-=+=@-=+=:+==]@*%-+*%-+@*%-+##%*]@=#*+=#*+@=#*+#*#%]', 0
windows : db '@@@@@@*+**+*...@@@@@@]@@@@@+:@@**:@@..@@@@]@@@@:++++**:++++:@@@]@@@@*:.@@*:.@@+.:@@@]@@@@@*+-:**--+*.@@@@]', 0
haystack : db '@@@@@@@#+..#.+]@@+#.#.#=####+]@@+#+=.#...#.+#]@@+###*#########]@@@@@@.#...#.#..]', 0
leaves : db '@@@@@@@.###.]@@@@@=+*%##*-+]@@@.=#########-:]@@-*###%####%%#+:]@@###%###%%%#####]@@*##%##%##%%%%##]@@+######%%#%%###]@@:**#%#@@##%##*-]', 0
treetrunk: db '@##]@#*]@#*]@##]@#%]@#%]@#%]@#%]@++]', 0
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
printground:
			push bp
			mov bp,sp
            push ax		
			push cx
			push dx
			;printing middle rows
			mov ax,[ResY]
			xor dx,dx
			mov cx,3
			div cx
			mov cx,[ResX]
			mul cx
			shl ax,1
			mov di,ax
			shl ax,1
				greenloop:
				mov word[es:di],0b0010000000100000
				add di,2
				cmp di,ax
				jne greenloop
			 ;printingtop
			mov ax,[ResY]
			xor dx,dx
			mov cx,3
			div cx
			push byte 0b00101111 ;push color
			push word ax  ;push y displacement
			push word 0  ;push x displacement
			push word groundtop ;push printingarray
			call print ;call function
			;printingbottom
			shl ax,1
			push byte 0b00101111 ;push color
			push word ax ;push y
			push word 0  ;push x
			push word groundbottom ;push printingarray 
			call print ;call function
			pop dx
			pop cx
			pop ax
			pop bp
			ret
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
		mov cx,2000
		cld
		rep stosw
		pop es
		pop cx
		pop ax
		pop di
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
horseposx: dw 90
horseposy: dw 17
printhorse:
		push byte 0b01100110
		push word [horseposy]
		push word [horseposx]
		push word horse
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saddleposx: dw 105
saddleposy: dw 18
printsaddle:
		push byte 0b00000000
		push word [saddleposy]
		push word [saddleposx]
		push word saddle
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
printocean:
		push di
		push ax
		push cx
		push dx
		mov ax,[ResY]
		xor dx,dx
		mov cx,3
		div cx
		mov cx,[ResX]
		mul cx
		shl ax,1
		mov di,0
		blueloop:
		mov word[es:di],0b0011000000100000
		add di,2
		cmp di,ax
		jne blueloop
		pop dx
		pop cx
		pop ax
		pop di
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mountainposx: dw 0
mountainposy: dw 1
printmountain:
		push byte 0b01110111
		push word [mountainposy]
		push word [mountainposx]
		push word mountainrange
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cloudsposx: dw 1
cloudsposy: dw 0
printclouds:
		push byte 0b11111111
		push word [cloudsposy]
		push word [cloudsposx]
		push word clouds
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sunposx: dw 106
sunposy: dw 1
printsun:
	    push byte 0b11101110
		push word [sunposy]
		push word [sunposx]
		push word sunarray
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barnposx: dw 10
barnposy: dw 11
printbarn:
		push byte 0b01000100
		push word [barnposy]
		push word [barnposx]
		push word barn
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
doorsposx: dw 27
doorsposy: dw 22
printdoors:
		push byte 0b00000000
		push word [doorsposy]
		push word [doorsposx]
		push word doors
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
windowsposx: dw 25
windowsposy: dw 15
printwindows:
		push byte 0b11111111
		push word [windowsposy]
		push word [windowsposx]
		push word windows
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
haystackposx: dw 54
haystackposy: dw 20
printhaystack:
		push byte 0b11101110
		push word [haystackposy]
		push word [haystackposx]
		push word haystack
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
treetrunkposx: dw 7
treetrunkposy: dw 17
printtreetrunk:
		push byte 0b01100110
		push word [treetrunkposy]
		push word [treetrunkposx]
		push word treetrunk
		call print
		ret
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
leavesposx: dw 0
leavesposy: dw 13
printleaves:
		push byte 0b10101010
		push word [leavesposy]
		push word [leavesposx]
		push word leaves
		call print
		ret

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

start:
	mov AH,0x00
	mov al, 0x54
	int 0x10
    mov Ax,0x1003
    mov bl, 0x00
    int 0x10

		mov ax,0xb800
		mov es,ax
		call clrscr
		call printground
		call printocean
		call printsun
		call printmountain
		call printclouds
		call printhorse
		call printsaddle
		call printhaystack
		call printbarn
		call printdoors
		call printwindows
		call printtreetrunk
		call printleaves
		
mov ax,0x4c00
int 0x21