[org 0x0100]
jmp start
timer:		 push si
			 push ax
			 push dx
			 ;send DSP command 10h
			 mov dx, 22ch
			 mov al, 10h
			 out dx,al
			 ;send byte audio sample
			 mov si, [sound_index]
			 mov al,[sound_data+si]
			 out dx,al
			 inc word[sound_index]
			 cmp word[sound_index],51529
			 jne timerskip
			 mov word[sound_index],0
			 timerskip:
			 pop dx
			 pop ax
			 pop si
			 jmp far[cs:timerinterrupt]
start:
mov ax,13h
 int 0x10
cli
		mov ax,[es:1ch*4]
		mov word [timerinterrupt],ax
		mov ax,[es:1ch*4+2]
		mov word [timerinterrupt+2],ax
		mov word[es:1ch*4],timer
		mov [es:1ch*4+2],cs
		mov al, 0x36    ; Set the command byte for Channel 0, 16-bit binary, square wave
		out 0x43, al    ; Send the command byte
		mov ax, 20     ; Set the desired frequency
		out 0x40, al    ; Send the low byte of the divisor
		mov al, ah      ; Get the high byte of the divisor
		out 0x40, al
sti
loop1:
     jmp loop1

cli	
		mov ax,[timerinterrupt]
		mov word [es:1ch*4],ax
		mov ax,[timerinterrupt+2]
		mov word [es:1ch*4+2],ax
sti
mov ax,0x4c00
int 0x21
	
section .data
timerinterrupt:dw 0,0
sound_index: dw 0
sound_data: 
		incbin "kingsv.wav" ;51,529 bytes
