title hexdump

include Irvine16.inc
include cs240.inc

_writehex_nybble proto

.8086

.data

stri BYTE 16 dup('0'),0             ;; Stores string to print in third column
hexdigs BYTE "0123456789ABCDEF"
hexMem BYTE 8 dup(0)                ;; 8 Bytes all initialized to 0 to store mem
readbuf byte "    "
badr BYTE "File could not be opened"

.code

;; Gets the end of the user input on the command line
;; IN: DX, offset of buffer for the command line tail.
;; USES: AX, CX, SI, DI, DX - all preserved
getcmdtail proc
    pushf
    push ax
    push cx
    push si
    push di

    mov di,dx

    mov ch,0
    mov cl,es:[80h] ;; es has start of program segment
    jcxz done
    mov si,81h      ;; point to first byte of cmd tail
                    ;; in PSP.

top:
    mov al,es:[si]  ;; get a byte from cmd tail in PSP
    mov [di],al     ;; store into our data segment string
    inc di          ;; move along both strings
    inc si
    loop top

done:
    mov byte ptr [di],0 ;; null terminate our buffer

    pop di
    pop si
    pop cx
    pop ax
    popf
    ret
getcmdtail endp


;; Prints out a single space.
;; USES: AX - preserved
printspace PROC
    pushf
    push ax

    mov al,' '
    call WriteChar240

    pop ax
    popf
    ret
printspace ENDP


;; Prints the end column: the chars represented by hex
;; USES: AX, DX - all preserved
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
;; USES: AX, CX, DI - all preserved
storechar PROC
    pushf
    push ax
    push cx
    push di

    cmp al,32           ;; Checks to see if al is a printable char
    jae next
    mov al,'.'          ;; If not make it a period

next:
    mov di,cx
    mov stri[di],al     ;; Store char in string in memory

    pop di
    pop cx
    pop ax
    popf
    ret
storechar ENDP


;; Prints a char in it's hex form
;; IN: al, a char to print
;; USES: AX, CX, DX, SI - all preserved
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
    shr dl,1             ;; shift dl to get next nybble
    loop shift

    mov dh,0	         ;; clear the high bits of dx
	mov si,dx            ;; index dx
	mov al,hexdigs[si]   ;; indexing to get hex digit
	call WriteChar240    ;; print second hex dig
    pop ax

    mov dx,ax
    and dx,00001111b
    mov dh,0	         ;; clear the high bits of dx
	mov si,dx            ;; index dx
	mov al,hexdigs[si]   ;; indexing to get hex digit
	call WriteChar240    ;; print first hex dig

    pop si
    pop dx
    pop cx
    pop ax
    popf
    ret
printchar ENDP


;; Prints the output by taking a single char and count. If count is 0, print out memory
;; count and then print the hex nybble. If count is 15, print hex nybble then the last
;; column of corresponding characters and reset count. Otherwise, just print char as a
;; hex nybble with correct spacing
;; IN: al, a char
;; IN: CX, a count (0-15)
;; OUT: CX, the next count
;; USES: AX - preserved, CX - updated
print PROC
    pushf
    push ax

    cmp cx,0
    jnz next
    call printMem       ;; prints the first column
    call printspace
    call printspace     ;; print spacing

next:
    call printchar      ;; prints the char in hex
    call printspace     ;; prints space after char
    call storechar      ;; stores the char in string to print at end
    cmp cx,7
    jnz incr
    call printspace     ;; at index 7 print an extra space

incr:
    inc cx              ;; increment count

    cmp cx,16           ;; if count is 16, that was the last char on line
    jnz done
    call printspace     ;; print extra space
    call printend       ;; prints the last column
    call setMem         ;; updates the memory
    mov cx,0            ;; reset count

done:
    pop ax
    popf
    ret
print ENDP


;; Prints the very last string at the end, when it is not a length of 16
;; IN: CX, length of string to print
;; USES: AX, BX, CX, DX, DI - all preserved
printfinal PROC
    pushf
    push ax
    push bx
    push cx
    push dx
    push di

    push cx
    mov ah,03h
    mov bh,0
    int 10h                 ;; interrupt to get the cursor position and store in dh and dl
    mov dl,60               ;; move cursor column
    call JumpCursor
    pop cx

    mov di,0

    mov al,'|'
    call WriteChar240

top:    mov al,stri[di]     ;; store a char of the string
    call WriteChar240       ;; print char
    inc di
    loop top

    mov al,'|'
    call WriteChar240

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    ret
printfinal ENDP


;; Updates the total memory used by a file so far
;; IN: cx, an integer representing how much memory to add to total (cx < 16)
;; USES: BX, CX, SI - all preserved
setMem proc
    pushf
    push bx
    push cx
    push si

    mov bx,offset hexMem    ;; bx = array to place into
    mov si,offset hexMem
    add si,lengthof hexmem
    dec si                  ;; start at right hand side
    clc

    add [si],cx             ;; hexMem(size) + cx
    cmp byte ptr [si],16    ;; Check for carry
    jb nc                   ;; If no carry, done
    sub byte ptr [si],16
    stc
    jmp cont

nc:
    clc
    jmp done

cont:
    dec si                  ;; Move back in the memArray
    inc byte ptr [si]       ;; Inc from the carry
    cmp byte ptr [si],16    ;; Check for ripple carry
    jb nc                   ;; If no carry, done
    sub byte ptr [si],16
    stc
    jmp cont

done:
    pop si
    pop cx
    pop bx
    popf
    ret
setMem endp


;; Prints out the total memory used by a file thus far
;; USES: BX, CX, DX - all preserved
printMem proc
    pushf
    push bx
    push cx
    push dx

    mov bx,offset hexMem    ;; bx = array to place mem into
    mov cx,lengthof hexMem  ;; Iterate through all elements of the array
top:
    mov dx,0
    mov dl,[bx]
    call _writehex_nybble   ;; Writes a hex nybble
    inc bx
    loop top

    pop dx
    pop cx
    pop bx
    popf
    ret
printMem endp


.data

cmdtail BYTE ?
filename BYTE "                    ",0
errfnf BYTE "Error: File not found",0
errnocmd BYTE "Usage: cat filename",0

.code


;; Program Description:
;; We begin by getting a filename from the command line using getcmdtail.
;; With the file offset we then use ReadFileChar240 to read through the file
;; character by character. Each character is sent to a print function which
;; converts the character to hexadecimal and controls the general printing.
;; Using a series of compares in this aforementioned compare function we
;; are able to properly format each row of our output. We store the characters
;; to be printed in a 16 character array that we fill up with each char sent to
;; our print function. Once we've read 16 chars (or the file is closed)
;; we then print the characters in our array that we've read in.
;; To do the memory we use an array with 8 elements that we increment
;; on each row of our output. The memory is incremented by keeping each
;; element in hexadecimal and when there is a carry the element to the left
;; in the array is incremented as long as needed.

main proc
    mov ax,@data
    mov ds,ax

    mov cx,0
    cmp byte ptr es:[80h],0   ;; if we have no tail at all
    jnz cont

    mov dx,offset errnocmd
    call writeString240       ;; Print("Usage: cat filename")
    call newline
    jmp quit

cont:
    mov dx,offset cmdtail
    call getcmdtail           ;; Get filename to be read

    mov dx,offset filename

    call OpenInputFile240     ;; ax will have a file handle
    jnc top                   ;; carry will be 1 if bad

    ;; bad open
    mov dx,offset errfnf
    call writestring240       ;; Print("Error: File not found")
    call newline
    jmp quit


top:
    call ReadFileChar240      ;; Read character from file
    cmp dx,-1
    jz done
    push ax                   ;; save filehandle
    mov al,dl
    call print                ;; Print char that we just read
    pop ax                    ;; restore filehandle
    jmp top

done:
    call CloseFile240

    cmp cx,0
    jz after
    call printfinal           ;; prints the last string if cx is not 0
    call newline

after:
    call setMem               ;; set memory for if line didn't end on multple of 16
    call printMem

quit:
    mov ax,4c00h
    int 21h
main endp

END main
