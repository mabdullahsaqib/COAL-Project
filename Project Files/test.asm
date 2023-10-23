section .data
    ; Define constants for frequency and duration
    freq equ 3000    ; Frequency (440 Hz)
    duration equ 10000 ; Duration (in milliseconds)

section .text
global _start

_start:
    ; Initialize the timer
    mov al, 10111110b  ; Set the command byte for timer 2
    out 43h, al       ; Send the command byte to port 43h
    mov ax, 11932     ; Set the reload value for the timer
    out 42h, al       ; Send the low byte of the reload value to port 42h
    mov al, ah        ; Send the high byte of the reload value to port 42h
    out 42h, al

    ; Play the tone
    in al, 61h       ; Read the current value of the control register
    or al, 00000011b ; Set the least significant two bits to 1
    out 61h, al      ; Write the modified value back to the control register

    ; Delay for the specified duration
    call delay

    ; Stop the tone
    in al, 61h       ; Read the current value of the control register
    and al, 11111100b ; Set the least significant two bits to 0
    out 61h, al      ; Write the modified value back to the control register

    ; Exit the program
    mov ah, 4Ch       ; Set the exit function code
    int 21h           ; Call DOS to exit

delay:
    ; Delay function for the specified duration
    push cx
    mov cx, duration
delay_loop:
    loop delay_loop
    pop cx
    ret