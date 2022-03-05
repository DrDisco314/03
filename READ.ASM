title read

include Irvine16.inc
include cs240.inc

.8086

.data

readbuf byte "    "

.code

;; read a single character from a file handle (ax)
;; return char in dx, -1 if end of file
ReadFileChar240 proc
	push ax
	push bx
	push cx


	mov bx,ax		;; put filehandle in bx
	mov ah,3fh		;; read function
	mov dx, offset readbuf  ;; store in memory here
	mov cx,1		;; read only 1 char
	int 21h			;; READ!!

	jc coda		;; error, exit	

	cmp ax,0
	jz EndOfFile

	mov bx,offset readbuf
	mov dh,0
	mov dl,[bx]
	jmp coda

EndOfFile:
	mov dx,-1


coda:	pop cx
	pop bx
	pop ax
	ret
ReadFileChar240 endp

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
bad: 	mov dx, offset badr
	call writestring240
	call newline
	
coda:	pop dx
	pop bx
	pop ax
	popf
	ret
ReadFileN endp




;;------------------------------
.data

cmdtail BYTE ?
filename BYTE "                    ",0
errfnf BYTE "Error: File not found",0
errnocmd BYTE "Usage: cat filename",0

.code
main proc
	mov ax,@data
	mov ds,ax


	cmp byte ptr es:[80h],0			;; if we have no tail at all
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

	call OpenInputFile240	;; ax will have a file handle
	jnc top					;; carry will be 1 if bad

	;; bad open
	mov dx,offset errfnf
	call writestring240
	call newline
	jmp quit

		
top:	
	call ReadFileChar240
	cmp dx,-1
	jz done
	push ax				;; save filehandle
	mov al,dl	
	call WriteChar240	;; write character
	pop ax				;; restore filehandle
	jmp top

done:	call CloseFile240
	

quit:	mov ax,4c00h
	int 21h
main endp
;;----------------------------------------
END