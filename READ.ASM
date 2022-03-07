title read

include Irvine16.inc
include cs240.inc

.8086

.data

readbuf byte "    "
badr BYTE "File could not be opened"

.code

;; DX: offset of buffer for the command line tail.
getcmdtail proc
	pushf
	push ax
	push cx
	push si
	push di

	mov di,dx
	
	mov ch,0
	;;mov cl,[80h]
	;;mov cl,ds:[80h]  
	mov cl,es:[80h]		; es has start of program segment
	jcxz done
	mov si,81h		; point to first byte of cmd tail
				; in PSP.

top:	mov al,es:[si]		; get a byte from cmd tail in PSP
	mov [di],al		; store into our data segment string
	inc di			; move along both
	inc si			; strings
	loop top		; 

done:	mov byte ptr [di],0	; null terminate our buffer

	pop di
	pop si
	pop cx
	pop ax
	popf
	ret

getcmdtail endp

;; IN: AX file handle
;; IN: CX number of chars to read
;; IN: DX buffer to write
;; OUT: CX number of chars actually read
ReadFileN proc
	pushf
	push ax
	push bx	
	push dx

	mov bx,ax
	mov ah,3Fh
	int 21h
	jc bad
	mov cx,ax	
	jmp coda
bad: 	
	mov dx, offset badr
	call writestring240
	call newline
	
coda:	pop dx
	pop bx
	pop ax
	popf
	ret
ReadFileN endp

END