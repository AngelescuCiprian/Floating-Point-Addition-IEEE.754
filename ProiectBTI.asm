.model small
.stack 100h
.data
    msj1 db "Introduceti primul numar:",10,13,"$"
    msj2 db "Introduceti al doilea numar:",10,13,"$"
    msj3 db "Suma celor 2 numere este:",10,13,"$"
    msjEroare db 10,13,"Caracter invalid, va rog introduceti cifre de la 0-9 si semnul minus o singura data la inceputul numarului!",10,13,"$"
    semn db ?
    arrayExponent dw 10 dup(0)  ; in acest array vom avea convertirea exponentului in b2
    nrCifreExponent dw ?
    copienr1 dw ?
    copienr2 dw ?
    msjExponent1 db 10,13,"Exponentul primului nr este:",10,13,"$"
    msjExponent2 db 10,13,"Exponentul celui de al doilea nr este:",10,13,"$"
    msjVirgulaMobila1 db 10,13,"Primul numar in virgula mobila este",10,13,"$"
    msjVirgulaMobila2 db 10,13,"Al doilea  numar in virgula mobila este",10,13,"$"
    msjSumaVirgulaMobila db 10,13,"Suma lor in virgula mobila este",10,13,"$"
    bitsemnnr db ?
    virgulaMobilanr dw 32 dup(0)
    arrayMantisa dw 10 dup(0)
    copiecx dw ?
    nrCifreMantisa dw ?
    copieSuma dw ?
.code

;-----------------
; Procedura citireNumar
; Citeste un numar de la tastatura
;-----------------
citireNumar PROC
    mov ax,@data          ; Initializam segmentul de date
    mov ds,ax
inceput:
    mov bitsemnnr,0
    mov semn,0            ; Resetam semnul (0 pentru pozitiv, 1 pentru negativ)
    mov cx,10             ; Setam baza 10 pentru conversie
    mov ah,01h
    int 21h               ; Citim un caracter de la tastatura

    cmp al, '-'           ; Verificam daca primul caracter este semnul minus
    je negativ            ; Daca este minus, sarim la eticheta negativ
    jne pozitiv           ; Daca nu este minus, sarim la eticheta pozitiv

negativ:
    xor bx, bx            ; Initializam bx cu 0 (pentru a construi numarul)
    push bx               ; Salvam bx pe stiva
    mov semn,1            ; Setam semnul ca fiind negativ
    mov bitsemnnr,1
    jmp citireCifra       ; Sarim la citirea cifrei urmatoare

pozitiv:
    cmp al, '9'           ; Verificam daca caracterul este mai mare ca '9'
    ja eroare             ; Daca este mai mare, este invalid (eroare)
    cmp al, '0'           ; Verificam daca caracterul este mai mic ca '0'
    jb eroare             ; Daca este mai mic, este invalid (eroare)
    sub al, 48            ; Convertim caracterul ASCII in numar (de exemplu '0' -> 0, '1' -> 1, ...)
    mov bl, al            ; Salvam cifra in bx
    push bx               ; Salvam cifra pe stiva
    jmp citireCifra       ; Sarim la citirea urmatoarei cifre

citireCifra:
    mov ah,01h
    int 21h               ; Citim urmatorul caracter de la tastatura
    cmp al, 13            ; Verificam daca este Enter (caracterul 13)
    je numarCitit         ; Daca da, am citit intregul numar si iesim din bucla
    cmp al, '-'           ; Verificam daca este semnul minus
    je eroare             ; Daca da, afisam eroare
    cmp al, '9'           ; Verificam daca este mai mare ca '9'
    ja eroare             ; Daca da, afisam eroare
    cmp al, '0'           ; Verificam daca este mai mic ca '0'
    jb eroare             ; Daca da, afisam eroare
    sub al, 48            ; Convertim caracterul ASCII in numar
    mov bl, al            ; Salvam cifra in bx
    pop ax                ; Scoatem cifra anterioara din stiva
    mul cx                ; Multiplicam numarul curent cu 10 (pentru a-l muta la locul potrivit)
    add ax, bx            ; Adaugam cifra curenta la numar
    push ax               ; Salvam numarul pe stiva
    jmp citireCifra       ; Continuam sa citim cifrele

eroare:
    mov ah,09h
    lea dx,msjEroare      ; Incarcam mesajul de eroare
    int 21h               ; Afisam mesajul de eroare
    jmp inceput           ; Revenim la inceput pentru a cere din nou numarul

numarCitit:
    pop ax                ; Scoatem valoarea numarului din stiva
    cmp semn, 1           ; Daca semnul este negativ
    jne return            ; Daca nu este negativ, trecem mai departe
    neg ax                ; Daca este negativ, inversam semnul numarului

return:
    ret                   ; Intoarcem controlul in programul principal
citireNumar ENDP

aflareNrCifreb2 PROC
        ;in aceasta procedura aflam cate cifre are numarul nostru in binar , precum si mantisa
        mov bx,ax ; copie pentru ax
        xor cx,cx
        mov bx,2
        lea di,arrayMantisa 
        ;impartim nr la 2 si punem resturile pe sitva
        contorCifreb2:
            xor dx,dx
            div bx
            push dx
            inc cx
            cmp ax,0
                je aflareMantisab2
            jmp contorCifreb2
        aflareMantisab2:
            mov copiecx,cx ; retinem nr de cifre 
            pop ax ; scapam de prima cifra, mantisa incepe de dupa virgula
            dec cx ; cum am scapat de o cifra de pe stiva , inseamna ca scadem si nr cifrelor din sitva
            mov nrCifreMantisa, cx; nrcifreMantisa=nrCifrenr-1
            buclaaflareMantisa:
                pop dx
                mov [di],dx ; copiem vectorul nostru arrayMantisa valorea din stiva
                inc di ; crestem indexul vectorului
            loop buclaaflareMantisa
        returnare:
            mov ax,bx ; ii dam lui ax valoarea inapoi
            mov cx,copiecx  ; lui cx numarul de cifre
            dec cx; pentru exponent avem nevoie de n-1 cifre(exponent)
            ret
    aflareNrCifreb2 ENDP

convertireBaza2 PROC
        ;in aceasta procedura conveertim exponentul in baza2
        mov bx,2
        mov ax,cx ; ax=nr cifre al numarului in baza 2 -1 (aflat anterior)
        add ax,127 ; adunam nr cifre-1 cu 127  pt a afla exponent (simpla precizie =>+127)
        xor cx,cx
        lea di,arrayExponent ; pregatim vectorul care va retine exponentul
        descompunereb2:
            xor dx, dx
            div bx
            push dx
            inc cx
            cmp ax, 0
                jne descompunereb2
        mov nrCifreExponent,cx ; retinem cate cifre are exponentul
        incarcareCfrb2: ; aici incarcam in vectorul nostru , cifrele din exponent
            pop dx
            mov [di],dx
            inc di
        loop incarcareCfrb2
        ret
    convertireBaza2 ENDP
    
    ; Aici incepe distractia , transformam in virgula Mobila
    transformareVirgulaMobila PROC
        lea di,virgulaMobilanr ; vectorul in care se va afla numarul reprezentat in virgula mobila
        lea si,arrayExponent ; vectorul nostru exponent , calculat anterior
        ;verificam bitii de semn
        cmp bitsemnnr,0
            je bitPozitiv
            jne bitNegativ
        ;daca este pozitiv , incarcam pe prima pozitie 0 si trecem indexul cu 1 mai departe(inc di)
        ;daca este negativ , facem invers , dar la fel trecem mai departe pe pozitia viitoare( incrementam di)
        bitPozitiv:
            mov [di],0
            inc di
            jmp continua
        bitNegativ:
            mov [di],1
            inc di
            jmp continua
        continua:
        mov cx,nrCifreExponent ; incarcam cifrele din exponent
        ;dupa ce am incarcat bitul de semn , incarcam bitii din exponent
        forExponent:
            mov ax,[si]
            mov [di],ax
            inc si
            inc di
        loop forExponent
        mov cx,nrCifreMantisa; acum , din cei 23 de biti din mantisa , ii trecem pe cei care provin din numarul nostru
        ; de exemplu pt 6 avem 1.10 , deci avem 3-1=2 cifre in mantisa, asadar trecem pe 10 deocamdata 
        lea si,arrayMantisa
            forMantisa1:
                mov ax,[si]
                mov [di],ax
                inc si
                inc di
            loop forMantisa1
        ;dupa ce am trecut cifrele noastre pe care le stim ,trebuie sa umplem restul cu 0
        mov cx,23
        sub cx,nrCifreMantisa
        ;din 23 de biti cati are mantisa , am scazut cifrele pe care le avem dupa virgula si apoi umplem tot cu 0
        forMantisa2:
            mov [di],0
            inc di
        loop forMantisa2
            ret
    transformareVirgulaMobila ENDP
    ;Aici vom afisa transformarile efectuate mai sus
    afisareVirgulaMobila PROC
        lea si,virgulaMobilanr ; incarcam vectorul
        xor cx,cx
        mov cx,32 ; avme 32 de biti de afisat in total
        forafisareVM:
            mov ah,02h 
            mov dx,[si] 
            add dx,48 ; transformam in cifra pentru a afisa
            int 21h
            inc si
        loop forafisareVM
        ret
    afisareVirgulaMobila ENDP

    ;aici afisam exponentul
    afisareVectorExponent PROC
         lea si,arrayExponent
        mov cx,nrCifreExponent
        forAfisare:
            mov ah,02h
            mov dx,[si]
            add dx,48
            int 21h
            inc si
        loop forAfisare
        ret
    afisareVectorExponent ENDP
;-----------------
; Procedura afisareNumar
; Afiseaza un numar
;-----------------
afisareNumar PROC
    mov bx, 10            ; Baza 10 pentru divizare
    xor cx, cx            ; Resetam numarul de cifre (counter)
    cmp ax, 0             ; Verificam daca numarul este 0
    jge descompunere      ; Daca numarul este pozitiv, continuam cu descompunerea
    neg ax                ; Daca numarul este negativ, il facem pozitiv
    push ax               ; Salvam numarul pe stiva
    mov ah,02h
    mov dl, '-'           ; Afisam semnul minus
    int 21h
    pop ax

descompunere:
    xor dx, dx            ; Resetam restul
    div bx                ; Impartim numarul la 10 (pentru a obtine ultima cifra)
    push dx               ; Salvam restul (cifra curenta)
    inc cx                ; Incrementam numarul de cifre
    cmp ax, 0
    je afisareCifre       ; Daca nu mai sunt cifre, afisam rezultatul
    jmp descompunere      ; Continuam descompunerea

afisareCifre:
    pop dx                ; Scoatem cifra din stiva
    add dx, 48            ; Convertim cifra inapoi in caracter ASCII
    mov ah, 02h
    int 21h               ; Afisam cifra
    loop afisareCifre     ; Continuam sa afisam cifrele ramase
    ret                   ; Intoarcem controlul in programul principal
afisareNumar ENDP

;-----------------
; Main
;-----------------
main:
    mov ax,@data          ; Initializam segmentul de date
    mov ds,ax

    mesaj MACRO msj
        mov ah,09h
        lea dx,msj
        int 21h           ; Afisam mesajul
    ENDM

    mesaj msj1            ; Afisam mesajul pentru primul numar
    call citireNumar      ; Citim primul numar
    mov copienr1,ax
    push ax               ; Salvam primul numar pe stiva

    mesaj msj2            ; Afisam mesajul pentru al doilea numar
    call citireNumar      ; Citim al doilea numar
    mov copienr2,ax
    push ax               ; Salvam al doilea numar pe stiva

    mesaj msj3            ; Afisam mesajul pentru suma numerelor
    pop ax                ; Scoatem al doilea numar din stiva
    pop bx                ; Scoatem primul numar din stiva
    add ax, bx            ; Calculam suma celor doua numere
    mov copieSuma,ax      ; Retinem suma celor 2 numere
    call afisareNumar     ; Afisam suma numerelor
    mesaj msjExponent1
    mov ax,copienr1 
    call aflareNrCifreb2
    call convertireBaza2
    call afisareVectorExponent
    mesaj msjExponent2
    mov ax,copienr2
    call aflareNrCifreb2
    call convertireBaza2
    call afisareVectorExponent

    mesaj msjVirgulaMobila1
    mov ax,copienr1
    call aflareNrCifreb2
    call convertireBaza2
    call transformareVirgulaMobila
    call afisareVirgulaMobila

    mesaj msjVirgulaMobila2
    mov ax,copienr2
    call aflareNrCifreb2
    call convertireBaza2
    call transformareVirgulaMobila
    call afisareVirgulaMobila

    mesaj msjSumaVirgulaMobila
    mov ax,copieSuma
    call aflareNrCifreb2
    call convertireBaza2
    call transformareVirgulaMobila
    call afisareVirgulaMobila
    mov ah, 4Ch           ; Terminam programul
    int 21h               ; Iesim din program
end main
