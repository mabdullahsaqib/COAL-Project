[org 0x0100]
section .text
jmp start
timer:		push si
			push ax
			push dx
			push es
			mov dx,[sound_buffer]
			mov es,dx
			;send DSP command 10h
			mov dx, 22ch
			mov al,10h
			out dx,al
			;send byte audio sample
			mov si, [sound_index]
			mov al,[es:si]
			out dx,al
			add word[sound_index],1
			cmp word[sound_index], 50000
			jne timerskip
			mov word[sound_index],0
			timerskip:
			pop es
			pop dx
			pop ax
			pop si
			jmp far [cs:timerinterrupt]
start:
cli
			;read sound data from file and store in sound_buffer segment
		mov ah,3dh ; open file
		mov al,0
		lea dx,filename
		int 21h
		mov [filehandle],ax
		;read file and write bytes to 
		mov ah,3fh
		mov cx,50000
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
		xor ax, ax
		mov es, ax					; es=0, point es to IVT base
		mov ax,[es:1ch*4]
		mov word [timerinterrupt],ax
		mov ax,[es:1ch*4+2]
		mov word [timerinterrupt+2],ax
		mov word[es:1ch*4],timer
		mov [es:1ch*4+2],cs
		mov al, 0x36    ; Set the command byte for Channel 0, 16-bit binary, square wave
		out 0x43, al    ; Send the command byte
		mov ax, 340    ; Set the desired frequency
		out 0x40, al    ; Send the low byte of the divisor
		mov al, ah      ; Get the high byte of the divisor
		out 0x40, al    ; Send the high byte of the divisor
		xor di,di
		mov ax,0xA000
		mov es,ax
		mov ax,13h
		int 0x10
sti
loop1:
	 mov ax,5
	 add bx,ax
     jmp loop1

cli	
		xor ax,ax
		mov es,ax
		mov ax,[timerinterrupt]
		mov word [es:1ch*4],ax
		mov ax,[timerinterrupt+2]
		mov word [es:1ch*4+2],ax
sti
mov ax,0x4c00
int 0x21
	
section .data
timerinterrupt:dw 0,0
sound_buffer: dw 8000
sound_index: dw 0
filename: db 'kingsv.wav',0
filehandle: dw 0