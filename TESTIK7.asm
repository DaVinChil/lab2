code_seg segment
        ASSUME  CS:CODE_SEG,DS:code_seg,ES:code_seg
	org 100h
start:
    jmp begin

;----------------------------------------------------------------------------
int_2Fh_vector  DD  ?
old_09h         DD  ?
;----------------------------------------------------------------------------

flag          DB  0
keys_pressed  DB  0
flag_counting DB  0
handler       DW  ?
last_code     DB  ?
file_to_write DB 'C:\out.txt', 0

;============================================================================

parse_int proc far
	push ax
	push bx
	push cx
	
	xor cx, cx
	xor bx, bx
	xor ax, ax
	mov Cl, CS:keys_pressed
	mov AL, CS:keys_pressed
	mov bl, keys_pressed
	call print_reg_BX
	push CX	

	diving:
		mov bl, 10
		div bl
		add ah, 30h
		mov CS:keys_pressed, ah
		mov dx, offset CS:keys_pressed
		mov bx, handler
		push ax
		mov ah, 40h
		mov CX, 1
		int 21h
		pop ax
		mov ah, 0
		cmp al, 0
	ja diving
	
	pop CX
	mov CS:keys_pressed, Cl
	pop CX
	pop BX
	pop AX

	ret
parse_int endp      

print_hex proc near
    and DL, 0Fh
    add DL, 30h
    cmp DL, 3Ah
    jl _print
    add DL, 07h
    _print:
        int 21H
    ret
print_hex endp
;=====================================================================
print_reg_AX proc near
    push AX
    push BX
    push CX
    push DX

    mov BX, AX
    mov AH, 02h
    mov DL, BH
    rcr DL, 04h   ; recursive shift
    call print_hex
    mov DL, BH
    call print_hex
    mov DL, BL
    rcr DL, 04h
    call print_hex
    mov DL, BL
    call print_hex

    pop DX
    pop CX
    pop BX
    pop AX
    ret
print_reg_AX endp
;=====================================================================
print_reg_BX proc near
    push AX
    
    mov AX, BX
    call print_reg_AX
    
    pop AX
    ret
print_reg_BX endp     
 
print_mes macro message
local msg, nxt
    push AX
    push DX

    mov DX, offset msg
    mov AH, 09h
    int 21h

    pop DX
    pop AX

    jmp nxt
    msg DB message,'$'
    nxt:
endm

new_09h proc far
;
    pushf
	push    AX
    in      AL,60h                  ; ‚??¤?¬ scan-code
    cmp     AL, CS:last_code
    je      dont_count
    cmp     AL, 80h
    jnb     cont_obr
    mov     CS:last_code, AL
    add     CS:last_code, 80h
	cont_obr:
    cmp     AL,58h                  ; ?a® a??­-?®¤ <F12>
    je      hotkey_start_count      ; Yes
        cmp     CS:flag_counting, 1                        
        jne     dont_count
        cmp     AL,57h              ; <F11>
        je      print_count
        inc     CS:keys_pressed

    dont_count:
    pop     AX                      ; No. ‚®aaa ­®??¬ AX
	popf
    jmp     dword ptr CS:[old_09h]  ; ‚ a?aa?¬­e© ®?a ?®ac?? ??§ ?®§?a a 

print_count:
    sti                 ; ?? ?a¤?¬ ¬?e ai a ©¬?aa
    in      AL,61h      ; ‚??¤?¬ a®¤?a¦?¬®? ?®aa  B
    or      AL,80h      ; “aa ­®??¬ aa ae?© ??a
    out     61h,AL      ; ? ??a­?¬ ? ?®aa B.
    and     AL, 7Fh
    out     61h, AL

;---------------------------------------------------------------------------

            push    BX	; a®aa ­?­?? ?a?®«i§a?¬ea a???aaa®? ? aa???
            push    CX	; a®aa ­?­?? ?a?®«i§a?¬ea a???aaa®? ? aa???
            push    DX	; a®aa ­?­?? ?a?®«i§a?¬ea a???aaa®? ? aa???
		push DS
		push CS
			pop DS

;---------------------------------------------------------------------------
        mov DX, offset file_to_write
        mov AX, 3D01h
        int 21h 

	clc
        mov handler, AX  
        
        call parse_int
        
        jnc m1
        jmp write_error 
        
        m1:
		    mov ah, 3Eh
		    mov BX, CS:handler
		    int 21h
		    jnc m2
        write_error:
            print_mes ' * WriteError * '
        m2:

;---------------------------------------------------------------------------
		pop DS
            pop     DX
            pop     CX
            pop     BX
;---------------------------------------------------------------------------
    cli
    mov     AL, 20h      ; ?®e«?¬
    out     20h,AL       ; ?a?? § EOI
;
    pop     AX
	popf
    iret

    
hotkey_start_count:	
;---------------------------------------------------------------------------
        mov CS:keys_pressed, 0h
        mov CS:flag_counting, 1
;---------------------------------------------------------------------------
    cli
    mov     AL, 20h      ; ?®e«?¬
    out     20h,AL       ; ?a?? § EOI
;
    pop     AX
	popf
    iret
new_09h     endp
;===========================================================================
;============================================================================
int_2Fh proc far
    cmp     AH,0C7h         ; ? e ­®¬?a?
    jne     Pass_2Fh        ; ??a, ­  ?ea®¤
    cmp     AL,00h          ; ?®¤aa­???i ?a®??a?? ­  ?®?a®a­ai aaa ­®??a?
    je      inst            ; ?a®?a ¬¬  a¦? aaa ­®?«?­ 
    cmp     AL,01h          ; ?®¤aa­???i ?e?aa§???
    je      unins           ; „ , ­  ?e?aa§?a
    jmp     short Pass_2Fh  ; ???§??aa­ i ?®¤aa­???i - ­  ?ea®¤
inst:
    mov     AL,0FFh         ; ‘®®?e?¬ ® ­??®§¬®¦­®aa? ?®?a®a­®© aaa ­®???
    iret
Pass_2Fh:
    jmp dword PTR CS:[int_2Fh_vector]
;
; -------------- ?a®??a?  - ?®§¬®¦­  «? ?e?aa§?  ?a®?a ¬¬e ?§ ? ¬ia? ? ------
unins:
    push    BX
    push    CX
    push    DX
    push    ES
;
    mov     CX,CS   ; ?a??®¤?aai ¤«i aa ?­?­?i, a.?. a CS aa ?­?? ai ­?«i§i
    mov     AX,3509h    ; ?a®??a?ai ???a®a 09h
    int     21h ; ”a­???i 35h ? AL - ­®¬?a ?a?ae? ­?i. ‚®§?a a-???a®a ? ES:BX
;
    mov     DX,ES
    cmp     CX,DX
    jne     Not_remove
;
    cmp     BX, offset CS:new_09h
    jne     Not_remove
;
    mov     AX,352Fh    ; ?a®??a?ai ???a®a 2Fh
    int     21h ; ”a­???i 35h ? AL - ­®¬?a ?a?ae? ­?i. ‚®§?a a-???a®a ? ES:BX
;
    mov     DX,ES
    cmp     CX,DX
    jne     Not_remove
;
    cmp     BX, offset CS:int_2Fh
    jne     Not_remove
; ---------------------- ‚e?aa§?  ?a®?a ¬¬e ?§ ? ¬ia? ---------------------
;
    push    DS
;
    lds     DX, CS:old_09h   ; ?a  ?®¬ ­¤  i???? «?­a­  a«?¤aie?¬ ¤?a¬
;    mov     DX, word ptr old_09h
;    mov     DS, word ptr old_09h+2
    mov     AX,2509h        ; ‡ ?®«­?­?? ???a®a  aa ae¬ a®¤?a¦?¬e¬
    int     21h
;
    lds     DX, CS:int_2Fh_vector   ; ?a  ?®¬ ­¤  i???? «?­a­  a«?¤aie?¬ ¤?a¬
;    mov     DX, word ptr int_2Fh_vector
;    mov     DS, word ptr int_2Fh_vector+2
    mov     AX,252Fh
    int     21h
;
    pop     DS
;
    mov     ES,CS:2Ch       ; ES -> ®?aa¦?­??
    mov     AH, 49h         ; ”a­???i ®a?®?®¦¤?­?i ?«®?  ? ¬ia?
    int     21h
;
    mov     AX, CS
    mov     ES, AX          ; ES -> PSP ?e?aa§?¬ a ¬a ?a®?a ¬¬a
    mov     AH, 49h         ; ”a­???i ®a?®?®¦¤?­?i ?«®?  ? ¬ia?
    int     21h
;
    mov     AL,0Fh          ; ?a?§­ ? aa??e­®© ?e?aa§??
    jmp     short pop_ret
Not_remove:
    mov     AL,0F0h          ; ?a?§­ ? - ?e?aa¦ ai ­?«i§i
pop_ret:
    pop     ES
    pop     DX
    pop     CX
    pop     BX
;
    iret
int_2Fh endp
;============================================================================
begin:
        mov CL,ES:80h      
        cmp CL,0           
        je  check_install   
                            
        xor CH,CH       
        cld            
        mov DI, 81h     
        mov SI,offset key   
        mov AL,' '         
repe    scasb   
                
                
        dec DI         
        mov CX, 4       
repe    cmpsb   
                
        jne check_install
        inc flag_off

check_install:
        mov AX,0C700h  
                        
        int 2Fh        
        cmp AL,0FFh
        je  already_ins 
;----------------------------------------------------------------------------
    cmp flag_off,1
    je  xm_stranno
;----------------------------------------------------------------------------
    mov AX,352Fh                      
                                     
    int 21h                           
    mov word ptr int_2Fh_vector,BX   
    mov word ptr int_2Fh_vector+2,ES  ;
;
    mov DX,offset int_2Fh           
                                    
    mov AX,252Fh                    
                                    
    int 21h 
;============================================================================
    mov AX,3509h                        ;   ?®«ac?ai
                                        ;   ???a®a
    int 21h                             ;   ?a?ae? ­?i  09h
    mov word ptr old_09h,BX    ;   ES:BX - ???a®a
    mov word ptr old_09h+2,ES  ;
    mov DX,offset new_09h           ;   ?®«ac?ai a¬?e?­?? a®c?? ?a®¤  ? ­®?e©
;                                   ;   ®?a ?®ac?? ­  DX
    mov AX,2509h                        ;   aa­???i aaa ­®??? ?a?ae? ­?i
                                        ;   ?§¬?­?ai ???a®a 09h
    int 21h ;   AL - ­®¬?a ?a?ae?. DS:DX - a? § a?«i ?a®?a ¬¬e ®?a ?®a?? ?a?a.
;
        mov DX,offset msg1  ; ‘®®?e?­?? ®? aaa ­®???
        call    print
;----------------------------------------------------------------------------
    mov DX,offset   begin           ;   ®aa ??ai ?a®?a ¬¬a ...
    int 27h                         ;   ... a?§?¤?­a­®© ? ?e©a?
;============================================================================
already_ins:
        cmp flag_off,1      ; ‡ ?a®a ­  ?e?aa§?a aaa ­®?«?­?
        je  uninstall       ; „ , ­  ?e?aa§?a
        lea DX,msg          ; ‚e?®¤ ­  i?a ­ a®®?e?­?i: already installed!
        call    print
        int 20h
; ------------------ ‚e?aa§?  -----------------------------------------------
 uninstall:
        mov AX,0C701h  ; AH=0C7h ­®¬?a ?a®??aa  C7h, ?®¤aa­???i 01h-?e?aa§? 
        int 2Fh             ; ¬a«ia??«??a­®? ?a?ae? ­??
        cmp AL,0F0h
        je  not_sucsess
        cmp AL,0Fh
        jne not_sucsess
        mov DX,offset msg2  ; ‘®®?e?­?? ® ?e?aa§??
        call    print
        int 20h
not_sucsess:
        mov DX,offset msg3  ; ‘®®?e?­??, ca® ?e?aa§?  ­??®§¬®¦­ 
        call    print
        int 20h
xm_stranno:
        mov DX,offset msg4  ; ‘®®?e?­??, ?a®?a ¬¬e ­?a,   ?®«i§®? a?«i
        call    print       ; ¤ ?a ?®¬ ­¤a ?e?aa§??
        int 20h
;----------------------------------------------------------------------------
key         DB  '/off'
flag_off    DB  0
msg         DB  'already '
msg1        DB  'installed',13,10,'$'
msg4        DB  'just '
msg3        DB  'not '
msg2        DB  'uninstalled',13,10,'$'
;============================================================================
PRINT       PROC NEAR
    MOV AH,09H
    INT 21H
    RET
PRINT       ENDP
;;============================================================================
code_seg ends
         end start
