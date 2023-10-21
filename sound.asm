[org 0x0100]
section .text
start:
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
	cmp word[sound_index], 51529
	mov cx,0x2AFF
	delay:
		loop delay
	jb loop
	mov word [sound_index],0
	jmp start
	mov ax,0x4c00
	int 0x21
	
section .data
sound_index: dw 0
sound_data: 
		incbin "kingsv.wav" ;51,529 bytes
