[org 0x0100]
jmp start


seconds: dw 0 
timerflag: dw 0 
oldkb: dd 0
bool: dw 0
tim: dw 0
timerfrequency:dw 300
timerticklimit:dw 0
timertick:dw 0
sound_buffer:dw 0x9000
sound_index:dw 0
sound_size:dw 50000
filename:db 'kingsv.wav',0
filehandle:dw 0
;...................................................................
; Subroutine for kbisr case-------------------------------------
;-------------------------------------------------------------------

kbisr:

	push ax 
	push es 
	mov ax, 0xb800 
	mov es, ax   
	push word [cs:seconds] 
	call printnum ; print tick count               
	in al, 0x60                 
	cmp al, 0x2a       ;left shift key             
	jne nextcmp 
	call Initialization
    ;call baqi
    call movement
    mov word [cs:timerflag], 1
	jmp exit                   

	nextcmp:
	cmp al, 0x48 ;;;; up key              
	jne nextcmp2             
	;mov word [cs:timerflag], 1
	call movement2
		
	jmp exit                    

	nextcmp2:

	cmp al, 0x36         ;;esc key      
	jne nextcmp3              
	;mov word [cs:timerflag], 1
	
    call outbound2

	jmp exit                 

	nextcmp3:
	cmp al, 0x4d        ; right key          
	jne nomatch  
	;mov word [cs:timerflag],1 
    call outbound               
	jmp exit                     

	nomatch:	
	pop es 
	pop ax 
	jmp far [cs:oldkb]          

	exit:
	mov al, 0x20 
	out 0x20, al              
	pop es 
	pop ax 
	iret  


;...................................................................
; Subroutine for Stripes case-------------------------------------
;-------------------------------------------------------------------

stripe1:

    push 0x783d ; color
        push 2; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret

    stripe2:

        push 0x783d ; color
        push 12; x1
        push  12;y1
        push  6 ; width
        push 13 ; y2
        call mycode

        ret


    stripe3:

    push 0x783d ; color
        push 22; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret

    stripe4:

    push 0x783d ; color
        push 32; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret

    stripe5:

    push 0x783d ; color
        push 42; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret


    stripe6:
        push 0x783d ; color
        push 52; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret

    stripe7:

    push 0x783d ; color
        push 62; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret

    stripe8:

    push 0x783d ; color
        push 72; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret

    stripe9:

    push 0x783d ; color
        push 80; x1
        push  12;y1
        push  6; width
        push 13 ; y2
        call mycode

        ret








movement2:

    call Print_last
    call tile2
    call tile1
    call tile3
    call tileup
    call obj2
     mov word [cs:timerflag], 1
    call delays

     mov word [cs:timerflag], 1

    call Print_last
    call tile1
    call tile2
    call tile3
    call obj
     mov word [cs:timerflag], 1


    ret



movement:

    call tile12
    call tile22
    call tile3
    call stripe1
    call obj
    call building1
    call carrot3
    mov word [cs:timerflag], 1
    call delays
 


     mov word [cs:timerflag], 1
    call Print_last
    call tile11
    call tile21
    call tile3
    call stripe2
    call carrot2
    call obj
    call building2
    mov word [cs:timerflag], 1
    call delays
    
 
     mov word [cs:timerflag], 1
    call Print_last
    call tile1
    call tile2
    call tile3
    call stripe3
    call obj
    call building3
    call carrot
    mov word [cs:timerflag], 1
    call delays
    




    ;.................................left movememnt...............................;
     mov word [cs:timerflag], 1
    call tile1
    call tile2
    call tile3
    call obj
    call building4 
    call stripe4
    call carrot
    mov word [cs:timerflag], 1
    call delays
    



     mov word [cs:timerflag], 1
    call Print_last
    call tile11
    call tile21
    call tile3
    call obj
    call building5
    call carrot2
    call stripe5
    mov word [cs:timerflag], 1
    call delays
   
 
     mov word [cs:timerflag], 1
    call Print_last
    call tile12
    call tile22
    call tile3
    call obj
    call building6
    call stripe6
    call carrot3
    mov word [cs:timerflag], 1
    call delays
  
 

    mov word [cs:timerflag], 1
    call tile12
    call tile22
    call tile3
    call obj
    call building7
    call carrot3
    call stripe7
    mov word [cs:timerflag], 1
    call delays
   



    mov word [cs:timerflag], 1
    call Print_last
    call tile11
    call tile21
    call tile3
    call obj
    call building8
    call stripe8
    call carrot2
    mov word [cs:timerflag], 1
    call delays
   
 

    call Print_last
    call tile1
    call tile2
    call tile3
    call obj
    ;call stripe9
    call carrot
    mov word [cs:timerflag], 1
    call delays



    mov word [cs:timerflag], 1
   ret

carrot:

	push 0x073c ; color
    push 32; x1
	push  20;y1
	push  2 ; width
    push 21 ; y2
	call mycode

    ret

carrot2:

	push 0x073c ; color
    push 28; x1
	push  20;y1
	push  2 ; width
    push 21 ; y2
	call mycode

    ret



carrot3:

	push 0x073c ; color
    push 24; x1
	push  20;y1
	push  2 ; width
    push 21 ; y2
	call mycode

    ret


tile1:

  	push 0x4720 ; color
    push 30; x1
	push  18;y1
	push  6 ; width
    push 19 ; y2
	call mycode

    ret
tile2:

  	push 0x22db ; color
    push 30; x1
	push  21;y1
	push  6 ; width
    push 22 ; y2
	call mycode

    ret

tile3:

  	push 0x4720 ; color
    push 30; x1
	push  24;y1
	push  6 ; width
    push 25 ; y2
	call mycode   

    ret

tile11:

	push 0x4720 ; color
    push 26; x1
	push  18;y1
	push  6 ; width
    push 19 ; y2
	call mycode


    ret

tile12:

    push 0x4720 ; color
    push 22; x1
	push  18;y1
	push  6 ; width
    push 19 ; y2
	call mycode

    ret
tile21:   

    push 0x22db ; color
    push 26; x1
	push  21;y1
	push  6 ; width
    push 22 ; y2
	call mycode

    ret

tile22:   

    push 0x22db ; color
    push 22; x1
	push  21;y1
	push  6 ; width
    push 22 ; y2
	call mycode

    ret
obj:

  	push 0x472e ; color
    push 32; x1
	push  23;y1
	push  2 ; width
    push 24; y2
	call mycode
    ret

tileup:

  	push 0x22db ; color
    push 30; x1
	push  21;y1
	push  6 ; width
    push 22 ; y2
	call mycode

    ret    
obj2:

  	push 0x472e ; color
    push 32; x1
	push  20;y1
	push  2 ; width
    push 21; y2
	call mycode
    ret    
;...................................................................
; Subroutine for outbound case-------------------------------------
;-------------------------------------------------------------------

score:
	push es
	push ax
	push di
	push cx
	mov ax,0xb800
	mov es,ax


    mov byte[es:1282] ,'N'
    mov byte[es:1284] ,'a'
    mov byte[es:1286] ,'m'
    mov byte[es:1288] ,'e'
    mov byte[es:1310] ,'S'
    mov byte[es:1312] ,'c'
    mov byte[es:1314] ,'o'
    mov byte[es:1316] ,'r'
    mov byte[es:1318] ,'e'
    

    mov byte[es:1442] ,'R'
    mov byte[es:1444] ,'o'
    mov byte[es:1446] ,'a'
    mov byte[es:1448] ,'i'
    mov byte[es:1450] ,'l'
    mov byte[es:1474] ,'7'

        mov byte[es:3080] ,'T'
        mov byte[es:3082] ,'h'
        mov byte[es:3084] ,'e'
        mov byte[es:3086] ,''
        mov byte[es:3088] ,''
        mov byte[es:3090] ,''
        mov byte[es:3092] ,'E'
        mov byte[es:3094] ,'n'
        mov byte[es:3096] ,'d'
        mov byte[es:3098] ,'.'
        mov byte[es:3100] ,'.'
        mov byte[es:3102] ,'.'
        mov byte[es:3104] ,'.'
        mov byte[es:3106] ,'.'
        mov byte[es:3108] ,'.'
        mov byte[es:3110] ,'.'
        mov byte[es:3112] ,'.'
        mov byte[es:3114] ,'.'


     pop cx
     pop di
     pop ax
     pop es
     ret

restart:
	push es
	push ax
	push cx 
	push di

	mov ax,0xb800
	mov es,ax
	mov di,0
	mov cx,560

	rest1:
 	mov word[es:di],0x4720
 	add di,2
 	sub cx,1
 	cmp cx,0
 	jne rest1

	pop di
	pop cx
	pop ax
	pop es
	ret

finalsheet:
    call clrscr
    call restart
    call score
    ret


outbound:

        call clrscr
        call restart

        push es
        push ax
        push di
        push cx
        mov ax,0xb800
        mov es,ax

        mov byte[es:2920] ,'O'
        mov byte[es:2922] ,'U'
        mov byte[es:2924] ,'T'
        mov byte[es:2926] ,''

        mov byte[es:2928] ,'O'
        mov byte[es:2930] ,'f'
        mov byte[es:2932] ,''
        mov byte[es:2934] ,'B'
        mov byte[es:2936] ,'o'

        mov byte[es:2938] ,'u'
        mov byte[es:2940] ,'n'
        mov byte[es:2942] ,'d'
        mov byte[es:2944] ,''

        mov byte[es:3080] ,'P'
        mov byte[es:3082] ,'r'
        mov byte[es:3084] ,'e'
        mov byte[es:3086] ,'s'
        mov byte[es:3088] ,'s'
        mov byte[es:3090] ,''
        mov byte[es:3092] ,'P'
        mov byte[es:3094] ,''
        mov byte[es:3096] ,'t'
        mov byte[es:3098] ,'o'
        mov byte[es:3100] ,''
        mov byte[es:3102] ,'e'
        mov byte[es:3104] ,'x'
        mov byte[es:3106] ,'i'
        mov byte[es:3108] ,'t'


	TakIn1_:
				int 0x16   
				in al ,0x60  
				cmp al,0x19   ;;; interupt for p
				
				jne TakIn1_
       
            call finalsheet
        


 mov word [cs:timerflag], 0
        pop cx
        pop di
        pop ax
        pop es
        ret


outbound2:

        call clrscr
        call restart

        push es
        push ax
        push di
        push cx
        mov ax,0xb800
        mov es,ax

        mov byte[es:2920] ,'O'
        mov byte[es:2922] ,'U'
        mov byte[es:2924] ,'T'
        mov byte[es:2926] ,''

        mov byte[es:2928] ,'O'
        mov byte[es:2930] ,'f'
        mov byte[es:2932] ,''
        mov byte[es:2934] ,'B'
        mov byte[es:2936] ,'o'

        mov byte[es:2938] ,'u'
        mov byte[es:2940] ,'n'
        mov byte[es:2942] ,'d'
        mov byte[es:2944] ,''

        mov byte[es:3080] ,'P'
        mov byte[es:3082] ,'r'
        mov byte[es:3084] ,'e'
        mov byte[es:3086] ,'s'
        mov byte[es:3088] ,'s'
        mov byte[es:3090] ,''
        mov byte[es:3092] ,'P'
        mov byte[es:3094] ,''
        mov byte[es:3096] ,'t'
        mov byte[es:3098] ,'o'
        mov byte[es:3100] ,''
        mov byte[es:3102] ,'P'
        mov byte[es:3104] ,'l'
        mov byte[es:3106] ,'a'
        mov byte[es:3108] ,'y'


	TakIn2_:
				int 0x16   
				in al ,0x60  
				cmp al,0x19   ;;; interupt for p
				
				jne TakIn2_
       
        
        call walapart321

 mov word [cs:timerflag], 0
        pop cx
        pop di
        pop ax
        pop es
        ret


;...................................................................
; Subroutine for Timer case-----------------------------------------
;-------------------------------------------------------------------

printnum: 
	push bp 
    mov bp, sp 
	push es 
	push ax 
	push bx 
	push cx 
	push dx 
	push di 
	mov ax, 0xb800 
	mov es, ax ; point es to video base 
	mov ax, [bp+4] ; load number in ax 
	mov bx, 10 ; use base 10 for division 
	mov cx, 0 ; initialize count of digits 
   
	nextdigit:
    mov dx, 0 ; zero upper half of dividend 
	div bx ; divide by 10 
	add dl, 0x30 ; convert digit into ascii value 
	push dx ; save ascii value on stack 
	inc cx ; increment count of values 
	cmp ax, 0 ; is the quotient zero 
	jnz nextdigit ; if no divide it again 
	mov di, 140 ; point di to 70th column 
 
    nextposa: 
        pop dx ; remove a digit from the stack 
        mov dh, 0x07 ; use normal attribute 
        mov [es:di], dx ; print char on screen 
        add di, 2 ; move to next screen location 
        loop nextposa ; repeat for all digits on stack 
        pop di 
        pop dx 
        pop cx 
        pop bx 
        pop ax
        pop es 
        pop bp 
        ret 2

timer: 
	push ax
	inc word[cs:timertick]
	mov ax,[cs:timertick]
	cmp ax,word[cs:timerticklimit]
	pop ax
	jc playsound
	mov word[cs:timertick],0
	cmp word[cs:tim],10
	jbe lo
	cmp word[cs:tim],11
	je loo
	loo:
	mov word[cs:tim],0
		inc word [cs:seconds] ; increment tick count 
	lo:
	cmp word [cs:seconds],10
	jae skip2
	mov word[cs:bool],1
	push ax 
	cmp word [cs:timerflag], 1 ; is the printing flag set 
	jne playsound
	inc word[cs:tim]
	push word [cs:seconds] 
	call printnum ; print tick count 

skip2:
	;call outbound
playsound:
		pusha
			push es
			push ds
			push cs
			pop ds
			mov dx,[sound_buffer]
			mov es,dx	
			;send DSP command 10h
			mov dx, 22ch
			mov al,10h
			out dx,al
			;send byte audio sample
			mov si,[sound_index]
			mov al,[es:si]
			out dx,al
			add word[sound_index],1
			mov dx,[sound_size]
			cmp word[sound_index],dx
			jne soundskip
			mov word[sound_index],0
			soundskip:
			pop ds
			pop es
			popa
			mov al, 0x20 
			out 0x20, al ; send EOI to PIC 
			pop ax 
			iret
;--------------------------------------------------------------------
; subroutine to clear the screen
;--------------------------------------------------------------------
clrscr:			
	push es
	push ax
	push di
	push cx

	mov ax, 0xb800
	mov es, ax				

	mov di, 0
	mov cx, 2000				

	mov word [es:di], 0x0720		
	cld					;; add di, 2
	rep stosw				;;repeat cx times 
					
	pop cx
	pop di
	pop ax
	pop es

	ret
;--------------------------------------------------------------------
; subroutine for delay
;--------------------------------------------------------------------
delay: 		
	push ax
	push cx
	mov ax, 0

	d2:
		loop d2

	d1:			
		add ax, 1
		mov cx, 0xffff
		cmp ax, 0xffff
		jne d1

		mov cx, 0xFFFF
	d4:
		loop d4
	
	pop cx
	pop ax
	ret
;--------------------------------------------------------------------
; subroutine for Sky_Printing
;--------------------------------------------------------------------
Print_Sky:		
	push es
	push ax
	push di
	push cx

	mov ax, 0xb800
	mov es, ax				
	mov di, 0				
	mov cx, 0

	back:			
		mov word [es:di], 0x0BDB		
		add di, 2				
		add cx, 2
		cmp cx, 1440			
		jne back				

		pop cx
		pop di
		pop ax
		pop es

        push word [cs:seconds] 
		call printnum ; print tick count 
	ret

Print_last:		
	push es
	push ax
	push di
	push cx

	mov ax, 0xb800
	mov es, ax				
	mov di, 2880				
	mov cx, 0

	back1:			
		mov word [es:di], 0x30b2		
		add di, 2				
		add cx, 2
		cmp cx, 4000			
		jne back1				

		pop cx
		pop di
		pop ax
		pop es 
	ret
;--------    
;--------------------------------------------------------------------
; subroutine for Greenry_Printing
;--------------------------------------------------------------------

Print_Greenry:	
	push es
	push ax
	push di
	push cx
		
	
	mov ax, 0xb800
	mov es, ax	
	
	mov di, 1440
	mov ax, 0x2000
	mov cx, 80

	cld 
	rep stosw		

	pop cx
	pop di
	pop ax
	pop es
	ret

;--------------------------------------------------------------------
; subroutine for MIDBackGround_Printing
;--------------------------------------------------------------------
Print_mid_Background:	
	push es
	push ax
	push di
	push cx
					
	mov ax, 0xb800
	mov es, ax		
		
	mov di, 1600
	mov ax, 0x0720	
	mov cx, 1440
	
	cld
	rep stosw			

	pop cx
	pop di
	pop ax
	pop es
	ret


;--------------------------------------------------------------------
; subroutine for River Printing
;--------------------------------------------------------------------

Print_River:		
	push es
	push ax
	push di
	push cx
		
	
	mov ax, 0xb800
	mov es, ax	
		
	mov di, 2880				
	mov ax, 0x1000	
	mov cx, 1440
	
	cld
	rep stosw				

	pop cx
	pop di
	pop ax
	pop es
	ret


;--------------------------------------------------------------------
; subroutine for Sun Printing
;--------------------------------------------------------------------

Print_Sun:		
	push bp
	mov bp, sp
	push es
	push ax
	push si

	mov di, [bp+4]
	mov ax, 0xb800
	mov es, ax				
	mov ax, di
	mov cx, 4
			
	sun: 			
		mov word [es:di], 0x0EDB
		add di, 2
		mov word [es:di], 0x0EDB
		sub di, 4
		mov word [es:di], 0x0EDB

		mov di, ax
		add di, 160
		sub cx, 2

		jne sun
		
		pop si
		pop ax
		pop es
		pop bp
		
	ret 2


;-------------------------------------------------------------------
; -------------------------1st Portion------------------------------			    
;-------------------------------------------------------------------							

building1:
        push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0

        ;----------------building1------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,5 ;height jitni
        push ax
        mov ax, 4 ;width jistnihai rect ki
        push ax
        push 10 ;col num where the drawing starts
        call draw

        ;------------------building2-------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,1 ;height jitni
        push ax
        mov ax, 1 ;width jistnihai rect ki
        push ax
        push 16 ;col num where the drawing starts
        call draw
        ;--------------building3-----------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,9 ;height jitni
        push ax
        mov ax, 3 ;width jistnihai rect ki
        push ax
        push 16 ;col num where the drawing starts
        call draw

            pop bx
        pop dx
        pop ax
        ret

        building2:

        push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0


        ;-----------------------building4------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,3 ;height jitni
        push ax
        mov ax, 5 ;width jistnihai rect ki
        push ax
        push 20 ;col num where the drawing starts
        call draw

        ;----------------------building5----------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,9 ;height jitni
        push ax
        mov ax, 3 ;width jistnihai rect ki
        push ax
        push 40 ;col num where the drawing starts
        call draw

        pop bx
        pop dx
        pop ax
        ret

        building3:

        push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0

        ;-----------------building6----------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,7 ;height jitni
        push ax
        mov ax, 3 ;width jistnihai rect ki
        push ax
        push 46 ;col num where the drawing starts
        call draw

        ;-------------------building7--------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,8 ;height jitni
        push ax
        mov ax, 2 ;width jistnihai rect ki
        push ax
        push 56 ;col num where the drawing starts
        call draw
        pop bx
        pop dx
        pop ax
        ret


        building4:

            push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0

        ;---------------------building8---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,5 ;height jitni
        push ax
        mov ax, 3 ;width jistnihai rect ki
        push ax
        push 60 ;col num where the drawing starts
        call draw

        ;----------------------building9---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,5 ;height jitni
        push ax
        mov ax, 7 ;width jistnihai rect ki
        push ax
        push 68 ;col num where the drawing starts
        call draw

        ;----------------------building10---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,7 ;height jitni
        push ax
        mov ax, 5 ;width jistnihai rect ki
        push ax
        push 76 ;col num where the drawing starts
        call draw

        ;----------------------building11---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,9 ;height jitni
        push ax
        mov ax, 3 ;width jistnihai rect ki
        push ax
        push 82 ;col num where the drawing starts
        call draw

        ;----------------------building11---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,5 ;height jitni
        push ax
        mov ax, 5 ;width jistnihai rect ki
        push ax
        push 86 ;col num where the drawing starts
        call draw


        pop bx
        pop dx
        pop ax
        ret

        building5:

            push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0

        ;----------------------building11---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,9 ;height jitni
        push ax
        mov ax, 1 ;width jistnihai rect ki
        push ax
        push 92 ;col num where the drawing starts
        call draw
        ;----------------------building12---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,7 ;height jitni
        push ax
        mov ax, 6 ;width jistnihai rect ki
        push ax
        push 100 ;col num where the drawing starts
        call draw

        ;----------------------building12---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,5 ;height jitni
        push ax
        mov ax, 5 ;width jistnihai rect ki
        push ax
        push 106 ;col num where the drawing starts
        call draw

        pop bx
        pop dx
        pop ax
        ret


        building6:

                push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0


        ;----------------------building13---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,3 ;height jitni
        push ax
        mov ax, 6 ;width jistnihai rect ki
        push ax
        push 112 ;col num where the drawing starts
        call draw

        ;----------------------building14---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,3 ;height jitni
        push ax
        mov ax,4 ;width jistnihai rect ki
        push ax
        push 118 ;col num where the drawing starts
        call draw


        pop bx
        pop dx
        pop ax
        ret

    building7:

                push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0

        ;----------------------building14---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,7 ;height jitni
        push ax
        mov ax, 3 ;width jistnihai rect ki
        push ax
        push 128 ;col num where the drawing starts
        call draw
        ;----------------------building15---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,9 ;height jitni
        push ax
        mov ax, 1 ;width jistnihai rect ki
        push ax
        push 132 ;col num where the drawing starts
        call draw

        pop bx
        pop dx
        pop ax
        ret

        building8:
                push ax
        push dx
        push bx
        mov dx,0x783D
        mov bx, 0

        ;----------------------building16---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,7 ;height jitni
        push ax
        mov ax, 2 ;width jistnihai rect ki
        push ax
        push 140 ;col num where the drawing starts
        call draw
        ;----------------------building17---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,8 ;height jitni
        push ax
        mov ax, 2 ;width jistnihai rect ki
        push ax
        push 144 ;col num where the drawing starts
        call draw
        ;----------------------building18---------------------------
        push dx
        push 1280 ;marks where the row is
        mov ax,9 ;height jitni
        push ax
        mov ax, 2 ;width jistnihai rect ki
        push ax
        push 148 ;col num where the drawing starts
        call draw

        pop bx
        pop dx
        pop ax
        ret


draw: ;-------------------------------------drawsrtart
    push bp                                    
    mov bp,sp                                  
    push ax
    push es
    push di
    push cx
    push dx
    push bx

    mov ax,0xb800
    mov es,ax
    mov di,[bp+10]
    add di,[bp+4]
    mov dx,[bp+8] ;height
    mov bx,[bp+12]

    heightloop:
    mov cx,[bp+6] ;width
    inner:
    mov word [es:di],bx
    add di,2
    loop inner
    mov ax,[bp+6]
    shl ax,1
    add ax,160
    sub di,ax
    dec dx
    cmp dx,0
    jne heightloop

    pop bx
    pop dx
    pop cx
    pop di
    pop es
    pop ax
    pop bp
    ret 10 ;------------------------------------drawend

;--------------------------------------------------------------------


;-------------------------------------------------------------------
; -------------------------2nd Portion------------------------------			    
;-------------------------------------------------------------------
 
make_stripes:
    push ax
    push cx
    mov cx,0
    r:
    mov ah,0xf7;color
    mov al,0x20 ;ascii for space is 20, hex ascii dalte ismein, jo print krani
    push ax
    push 1920 ;marks where the row is
    mov ax,1 ;height jitni
    push ax
    mov ax,6 ;width jistnihai rect ki
    push ax
    push cx ;col num where the drawing starts
    call draw
    add cx,20
    cmp cx,160
    jne r
    pop cx
    pop ax
    ret

;--------------------------------------------------------_------------

;--------------------------------------------------------------------
; subroutine for SkyMovement
;--------------------------------------------------------------------

Sky_Movements:		

	push bp
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov dx, 0
	mov bx, 0
	mov ax, 0xb800
	mov es, ax
	mov ds, ax

	left_movement:

		mov di, dx
		mov si, di
		add si, 2
		mov cx, 80     

		mov bp, [es:di] 

		cld
		rep movsw
		sub si, 4         
		mov [es:si] , bp

		add dx, 160
	    inc bx
	    cmp bx, 9
		jne left_movement

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
    
    ret		
;--------------------------------------------------------------------
; subroutine for Ship_Movement
;--------------------------------------------------------------------

mid_Movements:		
	
	push bp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
    
	mov ax, 0xb800
    mov bx, 0
    mov es, ax
    mov ds, ax
    mov ax, 0
    mov dx, 1758	

    right_movement:
        
    	mov cx, 80     
    	mov di, dx
    	mov si, di
    	sub si, 2

    	mov bp, [es:di] 
	
    	std
    	rep movsw

    	add si, 4      
    	mov [es:si] , bp

    	    add dx, 160
    	    add bx, 1
    	    cmp bx, 9

    	jne right_movement
    
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp

    ret	





    push bp
    mov bp,sp
    push ax
    push bx

    mov ax,160
    mul word [bp+4]
    mov bx,ax

    ;obj
    mov ah,0x0C ;color
    mov al,0x3C ;ascii for space is 20, hex ascii dalte ismein, jo print krani
    push ax
    push bx ;marks where the row is
    mov ax,1 ;height jitni
    push ax
    mov ax, 1 ;width jistnihai rect ki
    push ax
    push 78 ;col num where the drawing starts
    call draw

    mov ah,0x0C ;color
    mov al,0x3E ;ascii for space is 20, hex ascii dalte ismein, jo print krani
    push ax
    push bx ;marks where the row is
    mov ax,1 ;height jitni
    push ax
    mov ax, 1 ;width jistnihai rect ki
    push ax
    push 82 ;col num where the drawing starts
    call draw

    mov ah,0x0C ;color
    mov al,0x5E ;ascii for space is 20, hex ascii dalte ismein, jo print krani
    push ax
    push bx ;marks where the row is
    mov ax,1 ;height jitni
    push ax
    mov ax, 1 ;width jistnihai rect ki
    push ax
    push 80 ;col num where the drawing starts
    call draw


    pop bx
    pop ax
    pop bp
    ret 2









;-------------------------------------------------------------
;for game----------------------------------------------------
;-------------------------------------------------------==--===


; -------------------------------------------------------------------
; subroutine for mycode
;--------------------------------------------------------------------

mycode:
   
	push bp
	mov bp,sp
	pusha
	
	mov ax,0xb800
	mov es,ax
    mov dl,0
    add dl,byte[bp+8]
    pb:
	    mov al,80
	    mul dl 
	    add ax, [bp+10] 
	    shl ax,1
	    mov di,ax
	    mov ax,[bp+12] 
	    mov cx,[bp+6] 
	
	    cld
	    rep stosw

        add dl,1
        cmp dl, byte[bp+4] 
        jne pb
	popa
	pop bp 
	ret 10







;.......................................................Play Screen.......................................................;	
Intro:
    mov ax,0xb800
    mov es,ax
    mov ds,ax
    mov di,0
    mov cx,560
UperPart: ;uper Border
    mov word[es:di],0x01B1
    add di,2
    sub cx,1
    cmp cx,0
    jne UperPart
    mov cx,880
Playbutton:  ;Play Section
    mov word[es:di],0x0020
    add di,2
    sub cx,1
    cmp cx,0
    jne Playbutton
    mov cx,560 
DownPart: ;Down Section
    mov word[es:di],0x01B1
    add di,2
    sub cx,1
    cmp cx,0
    jne DownPart


  ;.......................................................play.......................................................;	
    mov di,1990
    mov word[es:di],0x8750
    add di,4
    mov word[es:di],0x8210
    add di,6
    mov word[es:di],0x8550
    add di,2
    mov word[es:di],0x854C
    add di,2
    mov word[es:di],0x8541
    add di,2
    mov word[es:di],0x8559
    add di,144
    add di,320
    ;Print LEft
    mov word[es:di],0x0711
    add di,4
    mov word[es:di],0x023A
    add di,6
    mov word[es:di],0x054C
    add di,2
    mov word[es:di],0x0545
    add di,2
    mov word[es:di],0x0546
    add di,2
    mov word[es:di],0x0554
    add di,144
    ;Print Right
    mov word[es:di],0x0710
    add di,4
    mov word[es:di],0x023A
    add di,6
    mov word[es:di],0x0552 
    add di,2
    mov word[es:di],0x0549
    add di,2
    mov word[es:di],0x0547
    add di,2
    mov word[es:di],0x0548
    add di,2
    mov word[es:di],0x0554

    ret
;subroutine to print three on the screen
three:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 544 ; (80*3+32)*2
		mov cx, 18
			horizontal_lines_for3:
				mov word [es:di], ax ; (80*3+32)*2
				mov word [es:di + 1280], ax ; (80*11+32)*2
				mov word [es:di + 2560], ax ; (80*19+32)*2
				add di, 2
			loop horizontal_lines_for3
	
		mov di, 738 ; (80*4+49)*2
		mov cx, 7
			vertical_lines_for3:
				mov word [es:di], ax ; (80*4+49)*2
				mov word [es:di + 1280], ax ; (80*12+49)*2
				add di, 2
				mov word [es:di], ax ; (80*4+50)*2 
				mov word [es:di + 1280], ax ; (80*12+50)*2
				add di, 158
			loop vertical_lines_for3
	
	pop di
	pop cx
	pop ax
	
	ret
;subroutine to print two on the screen	
two:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 544 ; (80*3+32)*2
		mov cx, 18
			horizontal_lines_for2:
				mov word [es:di], ax ; (80*3+32)*2
				mov word [es:di + 1280], ax ; (80*11+32)*2
				mov word [es:di + 2560], ax ; (80*19+32)*2
				add di, 2
			loop horizontal_lines_for2
		
		mov di, 738 ; (80*4+49)*2
		mov cx, 7
			vertical_lines_for2:
				mov word [es:di], ax ; (80*4+49)*2
				mov word [es:di + 1244], ax ; (80*12+31)*2
				add di, 2
				mov word [es:di], ax ; (80*4+50)*2 
				mov word [es:di + 1244], ax ; (80*12+32)*2
				add di, 158
			loop vertical_lines_for2
	
	pop di
	pop cx
	pop ax
	
	ret

;subroutine to print one on the screen
one:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 558 ; (80*3+39)*2
		mov cx, 17
			vertical_line_for1:
				mov word [es:di], ax ; (80*3+39)*2
				add di, 2
				mov word [es:di], ax ; (80*3+40)*2
				add di, 158
			loop vertical_line_for1
	
	pop di
	pop cx
	pop ax
	
	ret

;subroutine to print GO alphabets on the screen
go:
	push ax
	push cx
	push di
	
	mov ax, 0xb800
	mov es, ax
	mov ax, 0x06db
	
		mov di, 516 ; (80*3+18)*2
		mov cx, 16
			horizontal_lines_forGO:
				mov word [es:di], ax ; (80*3+18)*2
				mov word [es:di + 56], ax ; (80*3+46)*2
				mov word [es:di + 2560], ax ; (80*19+18)*2
				mov word [es:di + 2560 + 56], ax ; (80*19+46)*2
				add di, 2
			loop horizontal_lines_forGO
		
		mov di, 674 ; (80*4+17)*2 
		mov cx, 15
			vertical_lines_forGO:
				mov word [es:di], ax ; (80*4+17)*2
				mov word [es:di + 56], ax ; (80*4+45)*2
				mov word [es:di + 88], ax ; (80*4+61)*2
				add di, 2
				mov word [es:di], ax ; (80*4+18)*2 
				mov word [es:di + 56], ax ; (80*4+46)*2
				mov word [es:di + 88], ax ; (80*4+62)*2
				add di, 158
			loop vertical_lines_forGO
			
		mov di, 1986 ; (80*12+33)*2
		mov cx, 7
			small_vertical_lines_forGO:
				mov word [es:di], ax ; (80*12+33)*2
				add di, 2
				mov word [es:di], ax ; (80*12+34)*2 
				add di, 158
			loop small_vertical_lines_forGO
	
	pop di
	pop cx
	pop ax

	ret

;subroutine to delay 20 in the program
; delay:
	; push cx
	; mov cx, 20 ; change the values  to increase delay time
     ; mov word [cs:timerflag], 1
	; _delay_loop1:
	; push cx
     ; mov word [cs:timerflag], 1
	; mov cx, 0xFFFF
	; _delay_loop2:
	; loop _delay_loop2
	; pop cx

	; loop _delay_loop1
	; pop cx
     ; mov word [cs:timerflag], 1
	; ret

delays:
	push cx
     mov word [cs:timerflag], 1
	mov cx, 10 ; change the values  to increase delay time
     mov word [cs:timerflag], 1
	_delay_loop11:
	push cx
	mov cx, 0xFFFF
    mov word [cs:timerflag], 1
	_delay_loop21:
	loop _delay_loop21
    mov word [cs:timerflag], 1
	pop cx
	loop _delay_loop11
    mov word [cs:timerflag], 1
	pop cx
	ret


; -------------------------------------------------------------------
; subroutine for walapart321
;--------------------------------------------------------------------

walapart321:
	push ax
	push bx
	push cx
	push es
	push di
	push ds
	mov ax,0xb800
				mov es,ax
				mov ds,ax
				push ax
				push es
				push ds
				call Intro
				pop ds
				pop es
				pop ax
				mov di,0
				mov bx,0 
				mov cx,25
				TakIn:
				int 0x16   
				in al ,0x60  
				cmp al,0x19   ;;; interupt for p
				jne TakIn
				call clrscr
				call clrscr

	call three
		call delay
		call clrscr
		call two
		call delay
		call clrscr
		call one
		call delay
		call clrscr
		call go
		call delay

        
		call clrscr
		call Initialization
		call baqi
	
		pop ds
		pop di
		pop es
		pop cx
		pop bx
		pop ax
		
		
		ret

; -------------------------------------------------------------------
; subroutine for Initializing
;--------------------------------------------------------------------

Initialization:
	call Print_Sky
	call Print_mid_Background
	call Print_River
	call Print_Greenry
    call Print_last
	ret




make_buildings:

    call building1
    call building2
    call building3
    call building4
    call building5
    call building6
    call building7

    call building8


    ret
; -------------------------------------------------------------------
; subroutine for call maindisplay
;--------------------------------------------------------------------



baqi:

	call make_buildings
	

    ret
;--------------------------------------------------------------------
; subroutine for Movements
;--------------------------------------------------------------------
Movements:		
	mainloop:

		call delay
		;call Sky_Movements
 		call mid_Movements
		jmp mainloop
		
	ret

			
initializesound:
		pusha
		push ds
		push es
		push cs
		pop ds
			;read sound data from file and store in sound_buffer segment
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
		pop es
		pop ds
		popa
		ret
;--------------------------------------------------------------------
; Driver
;--------------------------------------------------------------------
start:		
	
    call walapart321
    call baqi
	call initializesound
	
koko:
cli 
	xor ax, ax 
	mov es, ax  
	mov ax, [es:9*4] 
	mov [oldkb], ax 
	mov ax, [es:9*4+2] 
	mov [oldkb+2], ax 
	mov word [es:9*4], kbisr
	mov [es:9*4+2], cs 	
	mov word [es:8*4], timer 
	mov [es:8*4+2], cs 
	mov ax,[timerfrequency]
	mov bx,300
	xor dx,dx
	div bx
	mov word[timerticklimit],ax
	mov dx, 0x43           ; Set up the control register
	mov al, 0B11000000     ; Channel 1, Access mode lobyte/hibyte, Mode 3 (square wave generator)
	out dx, al             ; Send the control word

	mov dx, 0x40           ; Set up the data register for Channel 1
	mov ax, 300 ; Your desired frequency value
	out dx, al             ; Send the low byte
	mov al, ah             ; Send the high byte
	out dx, al             ; Send the high byte	
sti 
	mov dx, start  
	add dx, 15  
	mov cl, 4 
	shr dx, cl 
	
	mov ax, 0x3100  
	int 0x21