title hexdump

include Irvine16.inc
include cs240.inc

getcmdtail proto
_writehex_nybble proto

.8086

.data

stri BYTE 16 dup('0'),0             ;; Stores string to print in third column
totalbytes BYTE 0                   ;; Counter for number of bytes
totalbytestring BYTE "00000000",0
hexdigs BYTE "0123456789ABCDEF"
hexMem BYTE 8 dup(0)                ;; 8 Bytes all initialized to 0 to store mem

.code


;; Prints out a single space.
printspace PROC
    pushf
    push ax

    mov al,' '
    call WriteChar240

    pop ax
    popf
    ret
printspace ENDP


;; Prints the first column: the offset of each line from the beginning of the file.
printcount PROC
    pushf
    push ax
    push dx

    ;mov ax,offset totalbytestring
    ;add ax,16

    ;call WriteDec240

    mov dx,offset totalbytestring
    call WriteString240

    pop dx
    pop ax
    popf
    ret
printcount ENDP


;; Prints the end column: the chars represented by hex
printend PROC
    pushf
    push ax
    push dx

    mov al,'|'
    call WriteChar240

    mov dx,OFFSET stri
    call WriteString240

    mov al,'|'
    call WriteChar240

    call newline

    pop dx
    pop ax
    popf
    ret
printend ENDP


;; Stores a char in string in memory to print at end of line.
;; IN: al, a char to store
;; IN: CX, index to store char in
storechar PROC
    pushf
    push ax
    push di

    cmp al,32                           ;checks to see if al is a printable char
    jae next
    mov al,'.'                          ;if not make it a period

next:    mov di,cx
    mov stri[di],al                     ;store char in string in memory

    pop di
    pop ax
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
shift:  
    shr dl,1                                 ;shift dl to get next nybble
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
    call printMem
    ;;call printcount                         ;prints the first column
    call printspace
    call printspace                         ;print spacing

next:    
    call printchar                          ;prints the char in hex
    call printspace                         ;prints space after char
    call storechar                          ;stores the char in string to print at end
    cmp cx,7
    jnz incr
    call printspace                         ;at index 7 print an extra space

incr:    
    inc cx                                  ;increment count

    cmp cx,16                               ;if count is 16, that was the last char on line
    jnz done
    call printspace                         ;print extra space
    call printend                           ;prints the last column
    call setMem
    mov cx,0                                ;reset count

done:    
    pop ax
    popf
    ret
print ENDP

.code

;; IN: lineOffset, an integer representing how much memory to add to total (lineOffset < 16)
;; Prints out the total memory used by a file thus far
setMem proc
    pushf
    push bx
    push si

    mov bx,offset hexMem    ;; bx = array to place into
    mov si,offset hexMem
    add si,lengthof hexmem
    dec si                  ;; start at right hand side
    clc

    add [si],cx
    cmp byte ptr [si],16
    jb nc
    sub byte ptr [si],16
    stc
    jmp cont

nc:
    clc
    jmp done

cont:
    dec si
    inc byte ptr [si]
    cmp byte ptr [si],16
    jb nc
    sub byte ptr [si],16
    stc
    jmp cont

done:
    pop si
    pop bx
    popf
    ret
setMem endp


;; Prints out the total memory used by a file thus far
printMem proc
    pushf
    push ax
    push bx
    push cx

    mov bx,offset hexMem   ;; bx = array to place mem into
    mov cx,lengthof hexMem
top:
    mov dl,[bx]
    call _writehex_nybble
    inc bx
    loop top

    pop cx
    pop bx
    pop ax
    popf
    ret
printMem endp


.data

cmdtail BYTE ?
filename BYTE "                    ",0
errfnf BYTE "Error: File not found",0
errnocmd BYTE "Usage: cat filename",0


.code

main proc
    mov ax,@data
    mov ds,ax

    mov cx,0
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
    ;call WriteChar240   ;; write character
    call print
    pop ax              ;; restore filehandle
    jmp top

done:
    call CloseFile240

quit:
    mov ax,4c00h
    int 21h
main endp

END main
