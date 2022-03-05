title hexdump

include Irvine16.inc
include cs240.inc

.8086

.code

main PROC
	mov ax,@data
	mov ds,ax

	mov ax,4C00h
	int 21h

main ENDP

END main