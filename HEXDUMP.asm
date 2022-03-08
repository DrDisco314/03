title hexdump

include Irvine16.inc
include cs240.inc

getcmdtail proto

.8086

.data
stri BYTE 16 dup('0'),0                   ;stores string to print in third column
totalbytes BYTE 0                      ;counter for number of bytes
hexdigs BYTE "0123456789abcdef"

.code

;; Prints out a single space.
printspace PROC
    pushf

    popf
    ret
printspace ENDP

;; Prints the first column: the offset of each line from the beginning of the file.
printcount PROC
    pushf
    push ax

    ;mov ax,totalbytes
    ;add ax,16

    ;call WriteDec240

    pop ax
    popf
    ret
printcount ENDP

;; Prints the end column: the chars represented by hex
printend PROC
    pushf
    push dx

    ;; print line

    mov dx,OFFSET stri
    call WriteString240

    ;; print line

    call newline

    pop dx
    popf
    ret
printend ENDP

;; Stores a char in string in memory to print at end of line.
;; IN: al, a char to store
;; IN: CX, index to store char in
storechar PROC
    pushf

    popf
    ret
storechar ENDP

;; Prints a char in it's hex form
;; IN: al, a char to print
printchar PROC
    pushf
    push ax
    push cx
    push dx
    push si

    push ax
    mov dx,ax
    mov cx,4
shift:  shr dl,1                             ;shift dl to get next nybble
    loop shift

    mov dh,0	                             ;clear the high bits of dx
	mov si,dx                                ;index dx
	mov al,hexdigs[si]                       ;indexing to get hex digit
	call WriteChar240                        ;print second hex dig
    pop ax

    mov dx,ax
    and dx,00001111b
    mov dh,0	                             ;clear the high bits of dx
	mov si,dx                                ;index dx
	mov al,hexdigs[si]                       ;indexing to get hex digit
	call WriteChar240                        ;print first hex dig

    pop si
    pop dx
    pop cx
    pop ax
    popf
    ret
printchar ENDP

;; Prints the output line by line
;; IN: al, a char
;; IN: CX, a count (0-15)
;; OUT: CX, the next count
print PROC
    pushf
    push ax

    cmp cx,0
    jnz next
    call printcount                         ;prints the first column
    call printspace
    call printspace                         ;print spacing

next:    call printchar                     ;prints the char in hex
    call printspace                         ;prints space after char
    call storechar                          ;stores the char in string to print at end
    cmp cx,7
    jnz incr
    call printspace                         ;at index 7 print an extra space

incr:    inc cx                             ;increment count

    cmp cx,16                               ;if count is 16, that was the last char on line
    jnz done
    call printspace                         ;print extra space
    call printend                           ;prints the last column
    mov cx,0                                ;reset count

done:    pop ax
    popf
    ret
print ENDP


.data

cmdtail BYTE ?
filename BYTE "                    ",0
errfnf BYTE "Error: File not found",0
errnocmd BYTE "Usage: cat filename",0


.code

main proc
    mov ax,@data
    mov ds,ax


    cmp byte ptr es:[80h],0         ;; if we have no tail at all
    jnz cont

    mov dx,offset errnocmd
    call writeString240
    call newline
    jmp quit

cont:
    mov dx,offset cmdtail
    call getcmdtail

    mov dx,offset filename

    call writestring240
    call newline

    call OpenInputFile240   ;; ax will have a file handle
    jnc top                 ;; carry will be 1 if bad

    ;; bad open
    mov dx,offset errfnf
    call writestring240
    call newline
    jmp quit


top:
    call ReadFileChar240
    cmp dx,-1
    jz done
    push ax             ;; save filehandle
    mov al,dl
    call WriteChar240   ;; write character
    pop ax              ;; restore filehandle
    jmp top

done:
    call CloseFile240

quit:
    mov ax,4c00h
    int 21h
main endp

END main
