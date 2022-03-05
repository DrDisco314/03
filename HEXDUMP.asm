title hexdump

include Irvine16.inc
include cs240.inc

.8086

.data
str BYTE 16 dup(0),0                   ;stores string to print in third column
totalbytes BYTE 0                      ;counter for number of bytes

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

    mov ax,totalbytes
    add ax,16

    call WriteDec240

    pop ax
    popf
    ret
printcount ENDP

;; Prints the end column: the chars represented by hex
printend PROC
    pushf
    push dx

    ;; print line

    mov dx,OFFSET str
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

main PROC
	mov ax,@data
	mov ds,ax

	mov ax,4C00h
	int 21h

main ENDP

END main
