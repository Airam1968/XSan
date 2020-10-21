********************** procedimentos ********************************
FUNCTION MOLDURA2(TOPO,ESQ,DIR,BAIXO,DUPLO,TEXTO)
IF PCOUNT() <6
   @ 23,10 SAY "NUMERO DE PARAMETROS INVALIDO"
   RETURN nil(NIL)
ENDIF
IF DUPLO
   @ TOPO,ESQ CLEAR TO DIR,BAIXO
   @ TOPO,ESQ TO DIR,BAIXO DOUBLE
ELSE
   @ TOPO,ESQ CLEAR TO DIR,BAIXO
   @ TOPO,ESQ TO DIR,BAIXO
ENDIF
IF LEN(TEXTO)>0
   TAM=BAIXO-ESQ
   NOVAPOS=(TAM-LEN(TEXTO))/2
   @ TOPO,ESQ+NOVAPOS SAY TEXTO
ENDIF
RETURN nil(NIL)
*
*
*          

PROCEDURE PTELA
*  monta tela padrao
   PARAMETERS pTITA,pTITB,pTITC,pBAR
   pES01="Janeiro"
   pES02="Fevereiro"
   pES03="Marco"
   pES04="Abril"
   pES05="Maio"
   pES06="Junho"
   pES07="Julho"
   pES08="Agosto"
   pES09="Setembro"
   pES10="Outubro"
   pES11="Novembro"
   pES12="Dezembro"
   pESEXT=SUBSTR(sisDATA,4,2)
   pEXT=SUBSTR(sisDATA,1,2)+" de "+pES&pESEXT+" de "+SUBSTR(sisDATA,7,4)
   SET INTE ON
   @ 04,00 TO 04,79
   @ 01,00 SAY "Empresa :" GET pTITA
   @ 02,00 SAY "Sistema :" GET pTITB
   @ 02,56 SAY pEXT
   @ 22,00 TO 22,79
   CLEAR GETS
   SET INTE OFF
RETURN nil
*
*
*
*
*
*
PROCEDURE PLIMPA
*  limpa a tela (os 2 quadros).
   @ 05,00 CLEAR TO 21,79
   @ 23,00 CLEAR TO 23,79
RETURN nil
*
*
*
PROCEDURE PLQUA
*  limpa os quadros de acordo com o formato.
   PARAMETERS pQUADRO
   pPOS= .F.
   IF SUBSTR(pQUADRO,1,1)="1"
      @ 05,00 CLEAR TO 21,79
      pPOS= .T.
   ENDIF
   IF SUBSTR(pQUADRO,2,1)="1"
      @ 23,00 CLEAR TO 23,79
      pPOS= .T.
   ENDIF
   IF .NOT. pPOS
      PLQUA("01")
      PMENSAG("ERRO DO SISTEMA, INFORME O RESPONSAVEL",2,1
      PPARA()
      pPOS= .T.
   ENDIF
RETURN nil
*
*
*
PROCEDURE PMSGR
*  mensagem rapida piscando(obs:tamanho maximo da mensagem=79).
   PARAMETERS pMSG,pPISCA
   IF EMPTY(pMSG) .OR. EMPTY(pPISCA)
      PLQUA("01")
      PMENSAG("ERRO DO SISTEMA, INFORME O RESPONSAVEL",2,1
      PPARA()
   ENDIF
   pCONT=1
   @ 23,00
   DO WHILE pCONT<=pPISCA
      SET COLOR TO W/N,W+/R
      @ 23,00 GET pMSG
      CLEAR GETS
      TONE(3900,2)
      @ 23,00
      pCONT=pCONT+1
   ENDDO
RETURN nil
*
*
*
PROCEDURE PMENSAG
   * imprime uma mensagem na tela na linha de mensagens
   PARAMETERS pMENSAG
   @ 23,00
   @ 23,00 SAY pMENSAG
RETURN nil
*
*
*
PROCEDURE PTEMPO
*  tempo para mensagens em tela.
   PARAMETERS pT
   pC=0
   DO WHILE pC<=pT
      pC=pC+1
   ENDDO
RETURN nil
*
*
*
PROCEDURE PSENHA
   * verifica as senhas dos usuarios.
   PARAMETERS pSENHA
   PUBLIC sisSIS,sisMOD,sisNIV,sisTRUE
   FIND &pSENHA
   IF .NOT. FOUND()
      sisTRUE= .F.
   ELSE
      IF CTOD(sisDATA)>VALIDADE
         PMSGR( "SENHA DESATIVADA",1
         sisTRUE= .F.
         DELETE
         PACK
      ELSE
         sisTRUE= .T.
         sisSIS=SIS
         sisMOD=MOD
         sisNIV=NIV
      ENDIF
   ENDIF
   USE
RETURN nil
*
*
*
PROCEDURE PPERG
   * perguntas com respostas do tipo (S/N).
   PARAMETERS pPERG
   PUBLIC sisRESP
   sisRESP=SPACE(1)
   DO WHILE .NOT. sisRESP $ "SN"
      @ 23,00 SAY pPERG GET sisRESP PICT "!")
      READ
   ENDDO
   @ 23,00 CLEAR TO 23,79
RETURN nil
*
*
*
PROCEDURE POPCAO
   * seleciona opcao de menu.
   PARAMETERS pFAIXA             && faixa de possiveis opcoes.
   SET INTE ON
   sisOP=SPACE(1)
   DO WHILE .NOT. sisOP $ pFAIXA
      sisOP=SPACE(1)
      @ 23,00 SAY " Escolha sua Opcao: "
      @ 23,22 SAY "[ ]"
      @ 23,23 GET sisOP PICT "!")
      READ
   ENDDO
   @ 23,00 CLEAR TO 23,79
   SET INTE OFF
RETURN nil
*
*
*
PROCEDURE PTIT
   * centraliza titulo em tela.
   PARAMETERS pTIT
   SET COLOR TO W/N+
   @ 03,00
   @ 03,(80-LEN(pTIT))/2 SAY pTIT
   CLEAR GETS
   SET COLOR TO
   SET COLOR TO W/N,+W/R
RETURN nil
*
*
*
PROCEDURE PCENTRA
   * centraliza frase na linha desejada.
   PARAMETERS pFRASE,pLIN
   @ pLIN,(80-LEN(pFRASE))/2 SAY pFRASE
RETURN nil
*
*
*
PROCEDURE PPARA
   * interrompe o processamento esperando qualquer tecla.
   SET CONSOLE OFF
   WAIT
   SET CONSOLE ON
RETURN nil
*
*
*
PROCEDURE PMOLDURA
   * monta moldura em tela.
   PARAMETERS pLINI,pLINF,pCOLI,pCOLF
   @ pLINI-1,pCOLI-2 TO pLINF+1,pCOLF+2 DOUBLE
   pTAM=pCOLF-pCOLI+1
   pMOLD=REPLICATE(CHR(177),pTAM)
   pC=pLINI
   DO WHILE pC<=pLINF
      @ pC,pCOLI SAY pMOLD
      pC=pC+1
   ENDDO
   @ pLINI+1,pCOLI+2 CLEAR TO pLINF-1,pCOLF-2
RETURN nil
*
*
*
PROCEDURE PDESENV
   * mensagem de modulo em desenvolvimento
   DO PLQUA WITH "0111"
   PMSGR( " Aten‡Æo ! ",3
   PMENSAG("M‡dulo em Desenvolvimento, tecle [ENTER] p/ continuar"
   PPARA()
RETURN nil
*
PROCEDURE PDESATIV
   * mensagem de modulo desativado
   DO PLQUA WITH "0111"
   PMSGR( " Aten‡Æo ! ",3
   PMENSAG("m‡dulo",2,1
   PMENSAG("Desativado",2,2
   PMENSAG("tecle [ENTER]",4,1
   PMENSAG("p/ continuar",4,2
   PPARA()
RETURN nil
*
*
PROCEDURE PDATAEXT
   * retorna data por extenso
   PARAMETERS pDATA
   PUBLIC sisEXT
   pES01="Janeiro"
   pES02="Fevereiro"
   pES03="Marco"
   pES04="Abril"
   pES05="Maio"
   pES06="Junho"
   pES07="Julho"
   pES08="Agosto"
   pES09="Setembro"
   pES10="Outubro"
   pES11="Novembro"
   pES12="Dezembro"
   pESEXT=SUBSTR(pDATA,4,2)
   sisEXT=SUBSTR(pDATA,1,2)+" de "+pES&pESEXT+" de "+SUBSTR(pDATA,7,4)
RETURN nil
*
*
*
PROCEDURE PTECLA
   * procedimento para PAUSA e INTERRUPCAO da impressao.
   * para implementa-lo coloque os sequintes comandos apos o skip em
   * seu programa.
   * DO PTECLA
   * IF sisINTERR
   *    comandos de fechamento do programa
   *    RETURN nil
   * ENDIF
   *
   sisINTERR= .F.
   SET DEVI TO SCREEN
   DO PLQUA WITH "0100"
   @ 23,00 SAY " [ P ] - PAUSA "
   @ 23,20 SAY " [ I ] - INTERROMPE "
   pTEC1=INKEY()
   IF UPPER(CHR(pTEC1))="P"
      SET COLOR TO BGRB+*/R
      @ 23,00 SAY " [ P ] - PAUSA "
      PPARA()
      SET COLOR TO
      @ 23,00 SAY " [ P ] - PAUSA "
   ELSE
      IF UPPER(CHR(pTEC1))="I"
         SET COLOR TO W+/GN
         @ 23,20 SAY " [ I ] - INTERROMPE "
         DO PTEMPO WITH 500
         SET COLOR TO
         @ 23,20 SAY " [ I ] - INTERROMPE "
         PLIMPA()
         sisINTERR=.T.
         RETURN nil
      ENDIF
   ENDIF
   @ 23,00 SAY " [ P ] - PAUSA "
   @ 23,20 SAY " [ I ] - INTERROMPE "
   SET DEVI TO PRINT
RETURN nil
*
*
*
PROCEDURE PIMPR
   * rotina de acusacao de erro c/impressora
   DO WHILE .NOT. ISPRINTER()
      DO PLQUA WITH "0100"
      SET COLOR TO W/N,W*+/R
      PMSGR( "Impressora Desconectada",1
      SET COLOR TO W/N,+W/R
   ENDDO
RETURN nil
*
*
*
PROCEDURE PVERIFMES
   * rotina de indicacao do periodo (MES/ANO) extenso mes
   PUBLIC pEXT
   PARAMETERS pMES,pANO
   pES01="Janeiro"
   pES02="Fevereiro"
   pES03="Marco"
   pES04="Abril"
   pES05="Maio"
   pES06="Junho"
   pES07="Julho"
   pES08="Agosto"
   pES09="Setembro"
   pES10="Outubro"
   pES11="Novembro"
   pES12="Dezembro"
   pESEXT=pMES
   pEXT=pES&pESEXT+"/"+pANO
RETURN nil
*
*
*
PROCEDURE RODAPE
* procedimento de impressao de rodape especial com usuarios
PARAMETERS eTIPO,eFORM,eRELAT
PUBLIC eRODAPE_1,eRODAPE_2
IF eTIPO = 0
   SELE 10
   USE RELATOR INDEX RELATOR
   SEEK eRELAT
   IF FOUND()
      IF eFORM = 80
         eRODAPE_1=SPACE(135)
         eRODAPE_2=SPACE(135)
         eTAM=0
         eTAM=LEN(USUARIO)/6
         eVEZES=1
         eINICIO=1
         eRODAPE_1="USUARIOS : "
         eRODAPE_2="         : "
         DO WHILE eTAM >= eVEZES
            IF eVEZES <= 6
               eRODAPE_1=eRODAPE_1+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ELSE
               eRODAPE_2=eRODAPE_2+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ENDIF
            eVEZES=eVEZES+1
            eINICIO=eINICIO+10
         ENDDO
      ELSE
         eRODAPE_1=SPACE(210)
         eRODAPE_2=SPACE(210)
         eTAM=0
         eTAM=LEN(USUARIO)/10
         eVEZES=1
         eINICIO=1
         eRODAPE_1="USUARIOS : "
         eRODAPE_2="         : "
         DO WHILE eTAM >= eVEZES
            IF eVEZES <= 11
               eRODAPE_1=eRODAPE_1+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ELSE
               eRODAPE_2=eRODAPE_2+SUBSTR(USUARIO,eINICIO,10)+SPACE(7)
            ENDIF
            eVEZES=eVEZES+1
            eINICIO=eINICIO+10
         ENDDO
      ENDIF
   ELSE
      eRODAPE_1="**** Codigo de Relatorio nao Reconhecido ****"
      eRODAPE_2="     **** Verifique o Cadastramento ****"
   ENDIF
   USE
ELSE
   IF eFORM = 80
      @ 61,01 SAY REPLICATE("=",123)+" U.A.I."+REPLICATE("=",5)
      @ 62,01 SAY eRODAPE_1
      @ 63,01 SAY eRODAPE_2
      @ 00,00 SAY sisLETRAN+" "
      ePAG=ePAG+1
   ELSE
      @ 61,01 SAY REPLICATE("=",195)+" U.A.I."+REPLICATE("=",8)
      @ 62,01 SAY eRODAPE_1
      @ 63,01 SAY eRODAPE_2
      @ 00,00 SAY sisLETRAN+" "
      ePAG=ePAG+1
   ENDIF
ENDIF
RETURN nil
*
*
*
PROCEDURE PNUMEXT
PUBLIC pNUM_EXT1,pNUM_EXT2,pNUM_EXT3
PARAMETERS pNUMERO,pTAMANHO
pN01  = "UM "
pN02  = "DOIS "
pN03  = "TRES "
pN04  = "QUATRO "
pN05  = "CINCO "
pN06  = "SEIS "
pN07  = "SETE "
pN08  = "OITO "
pN09  = "NOVE "
pN00  = " "
pN10  = "DEZ "
pN11  = "ONZE "
pN12  = "DOZE "
pN13  = "TREZE "
pN14  = "QUATORZE "
pN15  = "QUINZE "
pN16  = "DEZESSEIS "
pN17  = "DEZESETE "
pN18  = "DEZOITO "
pN19  = "DEZENOVE "
pN20  = "VINTE "
pN30  = "TRINTA "
pN40  = "QUARENTA "
pN50  = "CINQUENTA "
pN60  = "SESSENTA "
pN70  = "SETENTA "
pN80  = "OITENTA "
pN90  = "NOVENTA "
pN000 = " "
pN100 = "CENTO "
pN200 = "DUZENTOS "
pN300 = "TREZENTOS "
pN400 = "QUATROCENTOS "
pN500 = "QUINHENTOS "
pN600 = "SEISCENTOS "
pN700 = "SETECENTOS "
pN800 = "OITOCENTOS "
pN900 = "NOVECENTOS "
pMIL  = "MIL "
pMILHAO   = "MILHAO "
pMILHOES  = "MILHOES "
pMOEDA    = "REAIS "
pMOEDA1   = "REAL "
pCENTAVO  = "CENTAVO "
pCENTAVOS = "CENTAVOS "
pNUMERO1  = STR(pNUMERO,13,2)
pCENTS    = STR((pNUMERO-INT(pNUMERO))*100,2)
pNUMERO   = INT(pNUMERO)
pCONTADOR = 1
P1 = SUBSTR(pNUMERO1,2,3)
P2 = SUBSTR(pNUMERO1,5,3)
P3 = SUBSTR(pNUMERO1,8,3)
P4 = pCENTS
pTEXTO1 = ""
pTEXTO2 = ""
pTEXTO3 = ""
pTEXTO4 = ""
pLINHA1 = ""
pLINHA2 = ""
pLINHA3 = ""
IF VAL(P1) > 0
   pSTATUS1 = IIF((P1) > 1,"2","1")
ELSE
   pSTATUS1 = ""
ENDIF
DO WHILE pCONTADOR <= 4
   pITEM  = STR(pCONTADOR,1)
   pGRUPO = "P"+STR(pCONTADOR,1)
   pEXTENSO = IIF(&pGRUPO="000","",LTRIM(&pGRUPO))
   DO CASE
      CASE LEN(pEXTENSO) = 3
         IF pEXTENSO = "100"
            pTEXTO&pITEM = pTEXTO&pITEM + "CEM "
            pCONTADOR = pCONTADOR + 1
            LOOP
         ENDIF
         IF RIGHT(pEXTENSO,2) = "00"
            TAB = "pN"+SUBSTR(pEXTENSO,1,1)+"00"
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            pCONTADOR = pCONTADOR + 1
            LOOP
         ENDIF
         TAB = "pN" + LEFT(pEXTENSO,1)+"00" 
         pTEXTO&pITEM = &TAB + "E "
         pDEZENA = VAL(RIGHT(pEXTENSO,2))
         IF pDEZENA < 20
            TAB = "pN"+RIGHT(pEXTENSO,2)
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            pCONTADOR = pCONTADOR + 1
            LOOP
         ELSE
            TAB = "pN"+SUBSTR(pEXTENSO,2,1)+"0"
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            TAB = "pN0"+RIGHT(pEXTENSO,1)
            pUNIDADE = VAL(RIGHT(pEXTENSO,1))
            IF pUNIDADE > 0
               pTEXTO&pITEM = pTEXTO&pITEM + IIF(VAL(pEXTENSO) > 10,"E ","")
            ENDIF
            pTEXTO&pITEM = pTEXTO&pITEM+IIF(TAB = "pN00","",&TAB)
         ENDIF
      CASE LEN(pEXTENSO) = 2
         pDEZENA = VAL(RIGHT(pEXTENSO,2))
         IF pDEZENA < 20
            TAB = "pN"+RIGHT(pEXTENSO,2)
            pTEXTO&pITEM = pTEXTO&pITEM+&TAB
            pCONTADOR = pCONTADOR + 1
            LOOP
         ELSE
            TAB = "pN"+SUBSTR(pEXTENSO,1,1)+"0"
            pTEXTO&pITEM = pTEXTO&pITEM + &TAB
            TAB = "pN0"+RIGHT(pEXTENSO,1)
            pUNIDADE1 = VAL(RIGHT(pEXTENSO,1))
            IF pUNIDADE1 > 0
               pTEXTO&pITEM = pTEXTO&pITEM + "E "
            ENDIF
            pTEXTO&pITEM = pTEXTO&pITEM+IIF(TAB="pN00","",&TAB)
         ENDIF
      CASE LEN(pEXTENSO) = 1
         TAB = "pN0"+RIGHT(pEXTENSO,1)
         pTEXTO&pITEM = pTEXTO&pITEM+IIF(TAB="pN00","",&TAB)
      OTHERWISE
         pTEXTO&pITEM = ""
   ENDCASE
   pCONTADOR = pCONTADOR + 1
ENDDO
IF VAL(P1+P2+P3) = 0 .AND. VAL(P4) <> 0 
   pFINAL = pTEXTO4+(IIF(VAL(P4)=1,pCENTAVO,pCENTAVOS))
ENDIF
pFINAL = IIF(LEN(pTEXTO1)=0,"",pTEXTO1+IIF(pSTATUS1="1",pMILHAO,pMILHOES))+IIF(LEN(pTEXTO2)=0,"",pTEXTO2+pMIL)+pTEXTO3+IIF(VAL(P2+P3)=0,"DE ","")
pFINAL = pFINAL+IIF(VAL(P1+P2+P3)=0,"",IIF(VAL(P1+P2+P3)=1,pMOEDA1,pMOEDA))
pFINAL = pFINAL+IIF(VAL(P4)=0,"",IIF(VAL(P1+P2+P3)=0,"","E ")+pTEXTO4+IIF(VAL(P4)=1,pCENTAVO,pCENTAVOS))
* Ajusta Extenso para pTAMANHO
pLINHA   = pTAMANHO
pTEXTO1  = pFINAL
pINICIAL = 1
pFINAL2  = pLINHA
pTESTE   = .T.
pITEM    = "1"
IF LEN(pTEXTO&pITEM) > pLINHA
   DO WHILE VAL(pITEM) <= 3
      DO WHILE pFINAL2 < LEN(pTEXTO&pITEM)
         pBRANCO = SUBSTR(pTEXTO&pITEM,pFINAL2,1)
         IF pBRANCO <> " "
            pFINAL2 = pFINAL2 - 1
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      pLINHA&pITEM = LEFT(pTEXTO&pITEM,pFINAL2)
      pITEMV = pITEM
      pITEM  = STR(VAL(pITEM)+1,1)
      pTEXTO&pITEM = RIGHT(pTEXTO&pITEMV,LEN(pTEXTO&PITEMV)-pFINAL2)
   ENDDO
ELSE
   pLINHA1 = pFINAL
ENDIF                        
pNUM_EXT1=pLINHA1+REPLICATE("*",pLINHA-LEN(pLINHA1))
pNUM_EXT2=pLINHA2+REPLICATE("*",pLINHA-LEN(pLINHA2))
IF pLINHA2 = pLINHA3
   pNUM_EXT3=REPLICATE("*",pLINHA)
ELSE
   pNUM_EXT3=pLINHA3+REPLICATE("*",pLINHA-LEN(pLINHA3))
ENDIF
* fim procedimento
*
*
*
PROCEDURE HELP(p,l,v)
*SET KEY 28 TO
J_ANTERIOR:=SAVESCREEN(0,0,24,79)
CORVELHA=SETCOLOR()
SETCOLOR("W/N,+W/BG")
ARQ_ATUAL:=ALIAS()
IF .NOT. FILE("help1.dbf")
   PPERG ( "ARQUIVO DE HELP INEXISTENTE. CRIA UM NOVO ?"
   IF sisRESP = "S"
      IF sisUSUARIO = "MASTER"
         RESTSCREEN(0,0,24,79,J_ANTERIOR)
         SET KEY 28 TO help()
      ENDIF
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
   MOLDURA2(topo,esquerda,baixo,direita,.t.," AJUDA DE USUARIO - ESC para encerar ")
   memoedit(textoajuda,topo+1,esquerda+1,baixo-1,direita-1,.f.)
   restscreen(topo,esquerda,topo+2,direita+30,tl)
ELSE
   IF sisUSUARIO = "MASTER"
      PLQUA("01")
      PPERG ( "NAO EXISTE HELP PARA ESSE CAMPO/TELA. CRIA UM NOVO (S/N) ?"
      IF sisRESP = "N"
         RESTSCREEN(0,0,24,79,J_ANTERIOR)
         SET KEY 28 TO HELP
         USE
         IF LEN(ARQ_ATUAL)<>0
            SELECT(ARQ_ATUAL)
         ENDIF
         SET COLOR TO
         SET COLOR TO W/N,+W/BG
         RETURN nil
      ENDIF
   ELSE
      USE HELP1 INDEX HELP1 NEW
      GOTO TOP
      CHAVEHELP=SUBSTR(P,1,10)
      LOCATE FOR SUBSTR(CHAVE,1,10) = CHAVEHELP
      IF .NOT. EOF()
         tl=savescreen(topo,esquerda,topo+2,direita+30)
         MOLDURA2(topo,esquerda,baixo,direita,.t.," AJUDA DE USUARIO - ESC para encerar ")
         memoedit(textoajuda,topo+1,esquerda+1,baixo-1,direita-1,.f.)
         restscreen(topo,esquerda,topo+2,direita+30,tl)
      ELSE
         PLQUA("01")
         PMSGR( "Acesso Negado !",3)
         RETURN nil
      ENDIF
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
         PLQUA("01")
         PMENSAG("POSICIONE O CURSOR ONDE SERA O CANTO SUPERIOR "
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
         PLQUA("01")
         PMENSAG("POSICIONE O CURSOR ONDE SERA O CANTO inferior "
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
         PPERG ( "A POSICAO ESTA CORRETA (S/N) ? ")
         IF sisRESP = "N"
            RESTORE SCREEN FROM TELAHELP
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      PLQUA("01")
      PMENSAG("PRESSIONE CRTL+W PARA TERMINAR"
      @ TOPO,ESQUERDA TO BAIXO,DIREITA
      HELPTEMP=MEMOEDIT(TEXTOAJUDA,TTOP+1,TESQ+1,TBAI-1,TDIR-1,.T.)
      IF LASTKEY() = 27
         PLQUA("01")
         PMENSAG("CRIACAO DO HELP CANCELADA POR [ESC]"
         PPARA()
         EXIT
      ENDIF
      PPERG ( "Confirma o Texto (S/N) ? ")
      IF sisRESP = "N"
         PPERG ( "Cancela a Criacao do HELP (S/N) ? ")
         IF sisRESP = "N"
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
*
*
*
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
*
*
*
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
   @ LS,CS,LI,CI BOX Q[TIPO]
ELSE
   @ LS,CS,LI,CI BOX Q[TIPO]
ENDIF
*
*
*
FUNCTION TESTACGC(pCGC)
pCGC2    = ALLTRIM(pCGC)
pTAM_CGC = LEN(pCGC2)
pAUX_CGC = SPACE(14)
pAUX = 1
DO WHILE pAUX <= pTAM_CGC
   IF SUBSTR(pCGC2,pAUX,1) = "." .OR. SUBSTR(pCGC2,pAUX,1) = "-" .OR. SUBSTR(pCGC2,pAUX,1) = "/" .OR. SUBSTR(pCGC2,pAUX,1) = " ";
       .OR. SUBSTR(pCGC2,pAUX,1) = "\" .OR. SUBSTR(pCGC2,pAUX,1) = "," .OR. SUBSTR(pCGC2,pAUX,1) = ";"
      pAUX = pAUX + 1
   ELSE
      pAUX_CGC = ALLTRIM(pAUX_CGC) + SUBSTR(pCGC2,pAUX,1)
      pAUX = pAUX + 1
   ENDIF
ENDDO
pCGC = ALLTRIM(pAUX_CGC)
pCGC2=SPACE(14)
pNUM1=SUBSTR(pCGC,1,1)+SUBSTR(pCGC,3,1)+SUBSTR(pCGC,5,1)+SUBSTR(pCGC,7,1)
pNUM1=ALLTRIM(STR(VAL(pNUM1)*2.5))
pNUM1=VAL(SUBSTR(pNUM1,1,1))+VAL(SUBSTR(pNUM1,2,1))+VAL(SUBSTR(pNUM1,3,1))+VAL(SUBSTR(pNUM1,4,1))+VAL(SUBSTR(pNUM1,5,1))
pNUM2=ALLTRIM(SUBSTR(pCGC2,2,1)+SUBSTR(pCGC2,4,1)+SUBSTR(pCGC2,6,1)+SUBSTR(pCGC2,8,1))
pNUM2=VAL(SUBSTR(pNUM2,1,1))+VAL(SUBSTR(pNUM2,2,1))+VAL(SUBSTR(pNUM2,3,1))+VAL(SUBSTR(pNUM2,4,1))
pNUM3=pNUM1+pNUM2
IF pNUM3 <> INT(pNUM3/10)*10
   RETURN nil .F.
ELSE
   RETURN nil .T.
ENDIF
*
*
*
FUNCTION TESTACPF(pCPF)
pCPF2    = ALLTRIM(pCPF)
pTAM_CPF = LEN(pCPF2)
pAUX_CPF = SPACE(14)
pAUX = 1
DO WHILE pAUX <= pTAM_CPF
   IF SUBSTR(pCPF2,pAUX,1) = "." .OR. SUBSTR(pCPF2,pAUX,1) = "-" .OR. SUBSTR(pCPF2,pAUX,1) = "/" .OR. SUBSTR(pCPF2,pAUX,1) = " ";
       .OR. SUBSTR(pCPF2,pAUX,1) = "\" .OR. SUBSTR(pCPF2,pAUX,1) = "," .OR. SUBSTR(pCPF2,pAUX,1) = ";"
      pAUX = pAUX + 1
   ELSE
      pAUX_CPF = ALLTRIM(pAUX_CPF) + SUBSTR(pCPF2,pAUX,1)
      pAUX = pAUX + 1
   ENDIF
ENDDO
pCPF = ALLTRIM(pAUX_CPF)
pCONTADOR=1
DO WHILE pCONTADOR <= 9
   D = "0"+STR(pCONTADOR,1)
   D2&D = VAL(SUBSTR(pCPF,pCONTADOR,1))
   pCONTADOR = pCONTADOR + 1
ENDDO
DF4 = 10*D201+9*D202+8*D203+7*D204+6*D205+5*D206+4*D207+3*D208+2*D209
DF5 = DF4 / 11
DF6 = INT(DF5)*11
RESTO1 = DF4 - DF6
IF RESTO1 = 0 .OR. RESTO1 = 1
   PRIDIG = 0
ELSE
   PRIDIG = 11 - RESTO1
ENDIF
pCONTADOR = 1
DO WHILE pCONTADOR <= 9
   D = "0"+STR(pCONTADOR,1)
   D2&D = VAL(SUBSTR(pCPF,pCONTADOR,1))
   pCONTADOR = pCONTADOR + 1
ENDDO
DF4=11*D201+10*D202+9*D203+8*D204+7*D205+6*D206+5*D207+4*D208+3*D209+2*PRIDIG
DF5 = DF4 / 11
DF6 = INT(DF5) * 11
RESTO1 = DF4-DF6
IF RESTO1 = 0 .OR. RESTO1 = 1
   SEGDIG = 0
ELSE
   SEGDIG = 11 - RESTO1
ENDIF
PRIDIG = STR(PRIDIG,1)
SEGDIG = STR(SEGDIG,1)
IF PRIDIG <> SUBSTR(pCPF,10,1) .OR. SEGDIG <> SUBSTR(pCPF,11,1)
   RETURN nil .F.
ELSE
   RETURN nil .T.
ENDIF
*
*
*
******************************  fim  *****************************
