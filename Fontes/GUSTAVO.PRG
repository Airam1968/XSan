***************************************************************************
*** Funcao Ajuda
FUNCTION AJUDA()
PROCEDURE HELP(p,l,v)
SET KEY 28 TO
J_ANTERIOR := SAVESCREEN(0,0,24,79)
CORVELHA   =  SETCOLOR()
SETCOLOR("R/W,W/R+")
ARQ_ATUAL:=ALIAS()
IF .NOT. FILE("HELP1.DBF")
        RUN SPLAY BLIP2.WAV
        IF CONFIRMA("ARQUIVO DE HELP INEXISTENTE. CRIA UM NOVO ?")=2
                RESTSCREEN(0,0,24,79,J_ANTERIOR)
                SET KEY 28 TO AJUDA
                RETURN nil
        ELSE
                MATHELP={}
                AADD(MATHELP,{"CHAVE","C",24,0})
                AADD(MATHELP,{"TOPO","N",2,0})
                AADD(MATHELP,{"ESQUERDA","N",2,0})
                AADD(MATHELP,{"BAIXO","N",2,0})
                AADD(MATHELP,{"DIREITA","N",2,0})
                AADD(MATHELP,{"TEXTOAJUDA","M",10,0})
                DBCREATE("HELP1",MATHELP)
                USE HELP1 NEW
                INDEX ON CHAVE TO HELP1
                USE
                IF LEN(ARQ_ATUAL) <>0
                        SELECT( ARQ_ATUAL)
                ENDIF
        ENDIF
ENDIF
USE HELP1 INDEX HELP1 NEW
CHAVEHELP=SUBSTR(P,1,10)+SUBSTR(V,1,10)+STRZERO(L,4)
SEEK CHAVEHELP
IF FOUND()
        tl=savescreen(topo,esquerda,topo+2,direita+30)
        moldura(topo,esquerda,baixo,direita,.t.," Esc para encerrar ")
        memoedit(textoajuda,topo+1,esquerda+1,baixo-1,direita-1,.f.)
        restscreen(topo,esquerda,topo+2,direita+30,tl)
ELSE
        H_conf=confirma("NAO EXISTE NOTA PARA ESSE CAMPO/TELA.CRIA UM NOVO ? ")
        If H_CONF=2
           RESTSCREEN(0,0,24,79,J_ANTERIOR)
           SET KEY 28 TO HELP
           USE
           IF LEN(ARQ_ATUAL)<>0
              SELECT(ARQ_ATUAL)
           ENDIF
           RETURN nil
        ENDIF
        SAVE SCREEN TO TELABAS
        DO WHILE .T.
        **STORE 0 TO TTOP,TESQ,TBAI,TDIR 
        DO WHILE .T.
                SETCURSOR(0)
                RESTORE SCREEN FROM TELABAS
                CANTO=CHR(218)
                TELATEMP=SAVESCREEN(0,0,24,79)
                TROW=INT(MAXROW() /2)
                TCOL=INT(MAXCOL() /2)
                @ TROW,TCOL SAY CANTO
                MENSAGEM(23,10,"POSICIONE O CURSOR ONDE SERA O CANTO SUPERIOR ",2)
                DO WHILE .T.
                        CONTINUA=MONTATELA(INKEY(0))
                        IF .NOT. CONTINUA
                                EXIT
                        ENDIF
                        RESTORE SCREEN FROM TELABAS
                        @ TROW,TCOL SAY CANTO
                ENDDO
                SAVE SCREEN TO TELAHELP
                TTOP=TROW
                TESQ=TCOL
                CANTO=CHR(188)
                TROW=TROW+2
                TCOL=TCOL+5
                @ TROW,TCOL SAY CANTO
                MENSAGEM(23,10,"POSICIONE O CURSOR ONDE SERA O CANTO inferior ")
                DO WHILE .T.
                        CONTINUA=MONTATELA(INKEY(0))
                        IF .NOT. CONTINUA
                                EXIT
                        ENDIF
                        RESTORE SCREEN FROM TELATEMP
                        QUADRO(TTOP,TESQ,TROW,TCOL,6)
                ENDDO
                SETCURSOR(1)
                STORE TROW TO TBAI
                STORE TCOL TO TDIR
                RESTORE SCREEN FROM TELAHELP
                QUADRO(TTOP,TESQ,TBAI,TCOL,6,"GR+/B")
                OK=CONFIRMA("A POSICAO ESTA CORRETA ?")
                IF OK=2
                        RESTORE SCREEN FROM TELAHELP
                        LOOP
                ELSE
                        EXIT
                ENDIF
        ENDDO
        @ 23,10 SAY " PRESSIONE CRTL W AO TERMINAR"
        @ TOPO,ESQUERDA TO BAIXO,DIREITA
        HELPTEMP=MEMOEDIT(TEXTOAJUDA,TTOP+1,TESQ+1,TBAI-1,TDIR-1,.T.)
        IF LASTKEY()=27
                MENSAGEM(23,10,"CRIACAO DA NOTA CANCELADA POR <ESC>",2)
                EXIT
        ENDIF
        OK=CONFIRMA(" O TEXTO ESTA CORRETO ?")
        IF OK=2
                OKK=CONFIRMA("CANCELA A CRIACAO DA NOTA S/N ?")
                IF OKK=1
                        EXIT
                ELSE 
                        LOOP
                ENDIF
        ELSE
                APPEND BLANK
                REPLACE CHAVE WITH CHAVEHELP
                REPLACE TOPO WITH TTOP,BAIXO WITH TBAI
                REPLACE ESQUERDA WITH TESQ,DIREITA WITH TDIR
                REPLACE TEXTOAJUDA WITH HELPTEMP
                USE
        ENDIF  
        EXIT
        ENDDO
ENDIF
RESTSCREEN(0,0,24,79,J_ANTERIOR)
USE
IF LEN(ARQ_ATUAL) <>0
        SELECT(ARQ_ATUAL)
ENDIF
SETCOLOR(CORVELHA)
SET KEY 28 TO HELP
RETURN nil

FUNCTION MONTATELA(CURSOR)
DO CASE
        CASE CURSOR=5
                IF TROW-1 >0
                        TROW=TROW-1
                 ENDIF
        CASE CURSOR=24
                IF TROW+1 <24
                        TROW=TROW+1
                 ENDIF
        CASE CURSOR=4
                IF TCOL+1 <79
                        TCOL=TCOL+1
                 ENDIF
        CASE CURSOR=19
                IF TCOL-1 >0
                        TCOL=TCOL-1
                 ENDIF
        CASE CURSOR = 13 .OR. CURSOR = 27
                RETURN nil(.F.)
ENDCASE
RETURN nil (.T.)

FUNCTION QUADRO(LS,CS,LI,CI,TIPO,COR,CHEIO)
LOCAL Q[9]
CORBOX=COR
IF TIPO =NIL
        TIPO=1
ENDIF
IF CHEIO=NIL
        CHEIO=.F.
ENDIF
Q[1]:=CHR(201)+CHR(205)+CHR(187)+CHR(186)+CHR(188)+CHR(205)+CHR(200)+CHR(186)
Q[2]:=CHR(218)+CHR(196)+CHR(191)+CHR(179)+CHR(217)+CHR(196)+CHR(192)+CHR(179)
Q[3]:=CHR(213)+CHR(205)+CHR(184)+CHR(179)+CHR(190)+CHR(205)+CHR(212)+CHR(179)
Q[4]:=CHR(214)+CHR(196)+CHR(183)+CHR(186)+CHR(189)+CHR(196)+CHR(211)+CHR(186)
Q[5]:=CHR(220)+CHR(220)+CHR(220)+CHR(219)+CHR(219)+CHR(220)+CHR(219)+CHR(219)
Q[6]:=CHR(218)+CHR(196)+CHR(183)+CHR(186)+CHR(188)+CHR(205)+CHR(212)+CHR(179)
Q[7]:=CHR(201)+CHR(205)+CHR(184)+CHR(179)+CHR(217)+CHR(196)+CHR(211)+CHR(186)
IF VALTYPE(TIPO)="C"
        Q[9]=REPL(TIPO,8)
        TIPO=9
ENDIF
IF (TIPO >=176 .AND. TIPO <=178) .OR. TIPO=219
        IF CHEIO
        Q[8]=REPLICATE(CHR(TIPO),9)
        ELSE
        Q[8]=REPLICATE(CHR(TIPO),8)
        ENDIF
        TIPO=8
ENDIF
IF TIPO <>8 .AND. CHEIO
        Q[TIPO]=Q[TIPO]=CHR(32)
ENDIF
        IF COR<>NIL
                @LS,CS,LI,CI BOX Q[TIPO]
ELSE
                @LS,CS,LI,CI BOX Q[TIPO]
ENDIF

FUNCTION MOLDA(TOPO,ESQ,DIR,BAIXO,DUBLO,TEXTO)
IF PCOUNT() <6
        @ 23,10 SAY "NUMERO DE PARAMETROS INVALIDO"
        RETURN nil(NIL)
ENDIF
IF DUBLO
        @ TOPO,ESQ CLEAR TO DIR,BAIXO
        @ TOPO,ESQ TO DIR,BAIXO DOUBLE
ELSE
        @ TOPO,ESQ CLEAR TO DIR,BAIXO
        @ TOPO,ESQ TO DIR,BAIXO
endif
IF LEN(TEXTO)>0
        TAM=BAIXO-ESQ
        NOVAPOS=(TAM-LEN(TEXTO))/2
        @TOPO,ESQ+NOVAPOS SAY TEXTO
ENDIF
RETURN nil(NIL)


FUNCTION MENSAGEM(LS,CS,TEXTO,TEMPO)
IF CS>19
        CS=19
ENDIF
TELAM=SAVESCREEN(LS,CS,LS,CS+60)
@ LS,CS SAY SPACE(60)
@ LS,CS SAY TEXTO 
IF TEMPO<>NIL
        INKEY(TEMPO)
ELSE
        INKEY(0)
ENDIF
RESTSCREEN(LS,CS,LS,CS+60,TELAM)
RETURN nil NIL
***
********************************** fim ************************************
***************************************************************************
*** Funcao Ajuda
FUNCTION RELACAD()
LOCAL PAGINA:=0,LINHA:=1,SAIDA:=.T.,FOLHA,LFLAG:=.T.
LOCAL CAB:="*"+REPLICATE("-",78)+"*"
USE  SICATEL
        SAIDA:=.F.
        FOLHA:=56
        VERIMPR()
        IF LASTKEY()==27
                RETURN nil NIL
        END
        @ 21,20 SAY "AGUARDE ESTOU IMPRIMINDO!!!          "
        SETCOLOR("")
        SET PRINT ON
        SET CONSOLE OFF
SICATEL->(DBGOTOP())
WHILE SICATEL->(!EOF())
        IF LFLAG
                CABE80("** SICATEL **","COELHO - PRODU‡‡ES","RELATORIO DE TELEFONES CADASTRADOS",SAIDA,@PAGINA)
                ?PADR("NOME",29)+PADR("ENDERECO",40)
                ?? PADR("TELEFONE",10)
                ?CAB
                LFLAG:=.F.
        END
        ? PADR(SICATEL->NOME,29) + PADR(SICATEL->ENDERECO,40)  
        ?? PADR(SICATEL->TELEFONE,10)                             
        IF ++LINHA>FOLHA
                        ?CAB
                        IF!SAIDA
                                EJECT
                        ELSE
                                WAIT PADC("PRESSIONE UMA TECL_ATU",80)
                        END
                        LINHA:=1
                        LFLAG:=.T.
        ELSEIF INKEY()==27
                        EXIT
        END
        SICATEL->(DBSKIP())
END
?CAB
IF !SAIDA
        EJECT
        SET PRINT OFF
        SET CONSOLE ON
ELSE
        WAIT PADC("PRESSIONE UMA TECL_ATU",80,"#")
        CLS
END 
RETURN nil NIL

FUNCTION MENS(CVAR)
SCROLL (22,06,22,75)
@ 23,6 SAY "MENSAGEM:"+CVAR
RETURN nil .T.

//----------------------------------------------------

FUNCTION MOLDSOMB(NLINS,NCOLS,NLINI,NCOLI)
@ NLINS+1,NCOLS+1,NLINI,NCOLI BOX REPLICATE(CHR(177),9)
SCROLL(NLINS,NCOLS,NLINI-1,NCOLI-1) 
@ NLINS,NCOLS TO NLINI-1,NCOLI-1 DOUBLE
RETURN nil NIL

//---------------------------------------------------
FUNCTION VERIMPR()
WHILE !ISPRINTER()
        @ 21,20 SAY"LIGUE A IMPRESSORA E PRECIONE QUALQUER TECLA !");
       COLOR "W+*/N"
        IF INKEY(0)==27
                INDEX ON COD TO SICATEL1
                EXIT
        END
END
RETURN nil NIL
//-----------------------------------------------------

FUNCTION CABE80(EMPRESA,REL,MODULO,SAIDA,PAGINA)
LOCAL FOLHA,LIGA,DESL,EXPANDE,NORMAL
LOCAL CAB1:="*"+REPLICATE("=",78)+"*"
IF !SAIDA
        LIGA :=CHR(27)+CHR(69)
        DESL :=CHR(27)+CHR(70)
        EXPANDE :=CHR(14)
        NORMAL :=CHR(20)
ELSE
        LIGA :=DESL :=""
        EXPANDE :=NORMAL :=SPACE(7)
END
?CAB1
?"|"+SPACE(28)+" | "+MODULO+SPACE(37 - LEN(MODULO))+DTOC(DATE());
+" |"
?"|"+LIGA+EXPANDE+SPACE(14-LEN(EMPRESA))+EMPRESA+DESL+NORMAL+;
" | "+SPACE(37)+TIME()+" |"
?"|"+SPACE(28)+" | "+LIGA+REL+DESL+SPACE(37-LEN(REL));
+"PAG. "+STR(++PAGINA,3)+" |"
?CAB1
RETURN nil NIL

//--------------------------------------------------
***
********************************** fim ************************************
***************************************************************************
*** Funcao Muda Config

FUNCTION CONFIG()
*modifica o config.sys se: 
*FILES <=16
*BUFFERS<=85

CLEAR
CR=CHR(13)+CHR(10)
BUFFERDEF="16"
FILESDEF="85"
ALTERADO=.F.
CONFIG=GETENV("COMSPEC")
DRCONFIG=LEFT(CONFIG,3)+"CONFIG.SYS"
IF FILE("&DRCONFIG.")
        PES=UPPER(MEMOREAD("&DRCONFIG."))
        OKB=AT("BUFFERS",PES)
        ARQHANDLE=FOPEN("&DRCONFIG",2)
        IF ARQHANDLE =-1
                ? " NAO FOI POSSIVEL A ABERTURA DO ARQUIVO &DRCONFIG."
               RETURN nil
        ENDIF
        IF OKB=0
                TEXTO="BUFFERS="+BUFFERDEF+CR+CHR(26)
                FSEEK(ARQHANDLE,-1,2)
                E=BIN2I(FREADSTR(ARQHANDLE,1))
                IF E =26
                        FSEEK(ARQHANDLE,-1,2)
                ENDIF
                OK=FWRITE(ARQHANDLE,TEXTO)
                IF OK < LEN (TEXTO)
                        ? " ERRO NA GRAVACAO (FWRITE) : ",FERROR()
                ENDIF
                ALTERADO = .T.
        ELSE
                CHECAB=SUBS(PES,OKB,20)
                IGUALB=AT("=",CHECAB)
                FIMB=AT(CHR(10),CHECAB)
                VALBUF=VAL(SUBS(CHECAB,IGUALB+1,10))
                IF VALBUF<VAL(BUFFERDEF)
                        FSEEK(ARQHANDLE,OKB+IGUALB-1,0)
                        FWRITE(ARQHANDLE,BUFFERDEF+CR+SUBS(PES,OKB+FIMB))
                        ALTERADO= .T.
                ENDIF
        ENDIF
        FCLOSE(ARQHANDLE)
        CLEAR
        PES=UPPER(MEMOREAD("&DRCONFIG."))
        OKF=AT("FILES",PES)
        ARQHANDLE=FOPEN("&DRCONFIG",2)
        IF ARQHANDLE = -1
                ? " NAO FOI POSSIVEL A ABERTURA DO ARQUIVO &DRCONFIG."
                RETURN nil
        ENDIF
        IF OKF =0
                TEXTO= "FILES=" +FILESDEF+CR+CHR(26)
                FSEEK(ARQHANDLE,-1,2)
                E=BIN2I(FREADSTR(ARQHANDLE,1))
                IF E=26
                        FSEEK(ARQHANDLE,-1,2)
                ENDIF
                OK=FWRITE(ARQHANDLE,TEXTO)
                IF OK < LEN(TEXTO)
                        ? " ERRO DE GRAVACAO (FWRITE) : " , FERROR()
                ENDIF        
                ALTERADO= .T.
        ELSE
                CHECAF=SUBS(PES,OKF,20)
                IGUALF=AT("=",CHECAF)
                FIMF=AT(CHR(10),CHECAF)
                VALFIL=VAL(SUBS(CHECAF,IGUALF+1,10))
                IF VALFIL<VAL(FILESDEF)
                        FSEEK(ARQHANDLE,OKF+IGUALF-1,0)
                        FWRITE(ARQHANDLE,FILESDEF+CR+SUBS(PES,OKF+FIMF))
                        ALTERADO=.T.
                ENDIF
        ENDIF
        FCLOSE(ARQHANDLE)
ELSE  
        ARQHANDLE = FCREATE("&DRCONFIG.")
        IF ARQHANDLE = -1
                ? " NAO FOI POSSIVEL CRIAR O ARQUIVO &DRCONFIG. "
                RETURN nil
        ENDIF

TEXTO= "FILES="+FILESDEF+CR+"BUFFERS="+BUFFERDEF+CR+CHR(26)
        FSEEK(ARQHANDLE, 0 ,2)  
        FWRITE(ARQHANDLE,TEXTO)
        FCLOSE(ARQHANDLE)
        ALTERADO = .T.
ENDIF
IF ALTERADO
        SET COLOR TO W/B
        @ 9,14 TO 21,66 DOUBLE
        @ 9,25 SAY "CONTEUDO ATUAL DO CONFIG.SYS"
        @ 22,14 SAY "PRESSIONE <ESC> PARA CONTINUAR.."
        MEMOEDIT(MEMOREAD("&DRCONFIG."),10,15,20,65,.F.)
        SET COLOR TO R*/W
        @ 4,14 CLEAR TO 8,66
        @ 4,14 TO 8,66
        SET COLOR TO R/W
        SET CURSOR OFF
        @ 5,15 SAY " O MICRO DEVE SER DESLIGADO E "
        @ 6,15 SAY " LIGADO NOVAMENTE PARA QUE AS "
        @ 7,15 SAY " ALTERACOES PASSEM A TER EFEITO"
        INKEY(0)
        SET COLOR TO
        SET CURSOR ON
        CLEAR
ELSE
        TCONFIG=SAVESCREEN(23,10,23,78)
        @ 22,10 SAY "CONFIG.SYS CHECADO...AGUARDE !!")
        INKEY(3)
        RESTSCREEN(23,10,23,78,TCONFIG)
ENDIF
***
********************************** fim ************************************
***************************************************************************
*** Funcao Acesso MS-DOS
FUNCTION ACESDOS
SET CURSOR ON
TAD = SAVESCREEN(00,00,24,79)
IF MEMORY(0) < 30 && MEMORIA NECESSARIA PARA O COMMAND.COM
   @ 04,23 CLEAR TO 08,54
   @ 04,23 TO 08,54
   @ 05,25 SAY " A memoria disponivel nao e"
   @ 06,25 SAY " suficiente  para  rodar  o"
   @ 07,25 SAY " COMMAND.COM, Tecle Return."
   SET CURSOR  OFF
   SET CONSOLE OFF
   WAIT ""
   SET CONSOLE ON
   SET CURSOR  ON
   RESTSCREEN(00,00,24,79,TAD)
   RETURN nil
ENDIF
CLEAR
SET COLOR TO N/W
@ 01,0 SAY "Para retornar ao sistema, digite EXIT e tecle <ENTER>."
SET COLOR TO
RUN C:\COMMAND.COM
RESTSCREEN(00,00,24,79,TAD)
RETURN nil
***
********************************** fim ************************************
