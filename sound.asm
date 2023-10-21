[org 0x0100]
section .text

start:
mov ax,13h
int 0x10
loop:
	; send DSP command 10h
	mov dx, 22ch
	mov al,10h
	out dx,al
	;send byte audio sample
	mov si, [sound_index]
	mov al,[sound_data+si]
	out dx,al
	inc word[sound_index]
	mov cx,0x2FFF
	delay:
		loop delay
	cmp word[sound_index], 51529
	jb loop
	mov word [sound_index],0
	jmp start
	mov ax,0x4c00
	int 0x21
	
section .data
sound_index: dw 0
sound_data: 
		incbin "kingsv.wav" ;51,529 bytes
