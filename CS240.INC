;.model small, STDCALL
;.stack 200h

;; These functions are defined here

;; "Public"
StrCmp	proto	;; definitely want this
OpenInputFile240 proto
ReadFileChar240 proto
CloseFile240 proto
clrscr240	proto
JumpCursor	proto	;; dh: 0 <= row <= 24, 0 <=  dl <= 79
WriteDec240    proto
WriteChar240   proto
WriteString240 proto
NewLine        proto
DumpRegs240    proto
ReadChar240    proto
ReadString240  proto
BiosWriteChar  proto  ;; allows the use of color

;; "Private"

_writehex proto
_writehex_nybble proto
_drhelp proto
