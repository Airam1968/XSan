*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CAIX.PRG
* Data        : 22/11/2000
* Objetivo    : RELATORIO DE CONTROLE DE CAIXA DIARIO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CAIX()
SAVE SCREEN TO TELA04
PTIT( "RELATORIO DE CONTROLE DE CAIXA DIARIO")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA04
   RETURN nil
ENDIF
SELE 8
USE FORNECED INDEX FORNECED
GOTO TOP
SELE 7
USE BANCOS INDEX BANCOS
GOTO TOP
SELE 4
USE CLASSE_D INDEX CLASSE_D
GOTO TOP
SELE 5
USE CLASSE_R INDEX CLASSE_R
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ? " GET eCONTA PICT "@R 999"
   READ
   SELE 2
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA <> 0
         PLQUA("01")
         PMSGR( "Atená∆o !",3)
         PMENSAG("Conta N∆o Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eIMPRIME = .F.
PLQUA("01")
PPERG ( "Deseja informacoes de descricao (S/N) ? ")
IF sisRESP = "S"
   eIMPRIME = .T.
ENDIF
eCOD_BANCO  = COD_BANCO
eNOME_CONTA = ALLTRIM(COD_AGENCI)+" - "+ALLTRIM(COD_CC)
eLIMITE     = LIMITE
SELE 7
SEEK eCOD_BANCO
eNOME_BANCO = ALLTRIM(NOME_BANCO)
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Data : " GET eDATA_INI
*   @ 23,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") && .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv†lidas !",3)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
SELE 6
eCONT  = STR(eCONTA,5)
eZERO = "00"
eARQ  = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
eIND1 = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
USE &eARQ INDEX &eIND1
GOTO TOP
SET FILTER TO DATA = eDATA_INI
GOTO TOP
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
   ENDIF     
PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
PPARA()
DO PIMPR
SET DEVICE TO PRINT
eDATA_AUX = DATA
eSUB_ENTR = 0
eSUB_SAID = 0
eSUB_SALD = 0
eTOT_ENTR = 0
eTOT_SAID = 0
eTOT_SALD = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,120 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,121 SAY "Relat: SAN004"
   @ 03,25  SAY sisLETRAE+"CONTROLE CAIXA DIA "+DTOC(eDATA_INI)+sisLETRAC
   @ 04,00  SAY "Banco\Conta : "+eNOME_BANCO+" - "+eNOME_CONTA
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,00  SAY "CHEQUE"
   @ 06,20  SAY "DESCRICAO"
   @ 06,50  SAY "O.C."
   @ 06,60  SAY "CODIGO"
   @ 06,70  SAY "ENTRADAS"
   @ 06,90  SAY "SAIDAS"
   @ 06,110 SAY "SALDO"
   @ 06,130 SAY "CONF."
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 56
      IF eDATA_AUX = DATA
         @ LIN,00  SAY NUM_CHEQUE PICT "@R 99999999"
         @ LIN,12  SAY SUBSTR(DESCRICAO,1,28)
         @ LIN,50  SAY ALLTRIM(NUM_COMPRA)
         @ LIN,62  SAY ALLTRIM(STR(EMPREENDIM,10))+"/"+ALLTRIM(STR(CLASSE,10))
         @ LIN,70  SAY ENTRADAS   PICT "@R 999,999,999.99"
         @ LIN,90  SAY SAIDAS     PICT "@R 999,999,999.99"
         @ LIN,110 SAY SALDO_F    PICT "@R 999,999,999.99"
         @ LIN,130 SAY "_____"
         LIN = LIN + 1
         IF eIMPRIME
            @ LIN,12  SAY SUBSTR(DESCRICA2,1,28)
            LIN = LIN + 1
         ENDIF
         eTOT_ENTR = eTOT_ENTR + ENTRADAS
         eTOT_SAID = eTOT_SAID + SAIDAS
         eSUB_ENTR = eSUB_ENTR + ENTRADAS
         eSUB_SAID = eSUB_SAID + SAIDAS
         eSALDO_F  = SALDO_F
         SKIP
         DO PTECLA
         IF sisINTERR
            CLOSE ALL
            RETURN nil
         ENDIF
      ELSE
         @ LIN,70  SAY REPLICATE("_",55)
         LIN = LIN + 1
         @ LIN,02  SAY "SubTotal: "
         @ LIN,70  SAY eSUB_ENTR  PICT "@R 999,999,999.99"
         @ LIN,90  SAY eSUB_SAID  PICT "@R 999,999,999.99"
         @ LIN,109 SAY eSALDO_F   PICT "@R 999,999,999.99"
         LIN = LIN + 2
         eSALDO_F  = SALDO_F
         eSUB_ENTR = 0
         eSUB_SAID = 0
         eSUB_SALD = 0
         eDATA_AUX = DATA
      ENDIF
   ENDDO
   IF EOF()
      @ LIN,70  SAY REPLICATE("_",55)
      LIN = LIN + 1
      @ LIN,02  SAY "SubTotal: "
      @ LIN,70  SAY eSUB_ENTR  PICT "@R 999,999,999.99"
      @ LIN,90  SAY eSUB_SAID  PICT "@R 999,999,999.99"
      @ LIN,109 SAY eSALDO_F   PICT "@R 999,999,999.99"
      LIN = LIN + 2
      @ LIN,70  SAY REPLICATE("_",55)
      LIN = LIN + 1
      @ LIN,02  SAY "Totais : "
      @ LIN,70  SAY eTOT_ENTR  PICT "@R 999,999,999.99"
      @ LIN,90  SAY eTOT_SAID  PICT "@R 999,999,999.99"
      @ LIN,109 SAY eSALDO_F   PICT "@R 999,999,999.99"
      LIN = LIN + 2
      IF LIN >= 53
         @ 58,00 SAY REPLICATE("=",135)
         @ 59,104 SAY sisTIT3
         @ 01,00 SAY " "+sisLETRAN
         @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
         @ 01,119 SAY "Data : "+dtoc(DATE())
         @ 02,00  SAY "Sistema : "+sisTIT2
         @ 02,121 SAY "Relat: SAN004"
         @ 03,25  SAY sisLETRAE+"CONTROLE CAIXA DIA "+DTOC(eDATA_INI)+sisLETRAC
         @ 04,00  SAY "Banco\Conta : "+eNOME_BANCO+" - "+eNOME_CONTA
         @ 05,00  SAY REPLICATE("=",135)
         @ 06,00  SAY "CHEQUE"
         @ 06,20  SAY "DESCRICAO"
         @ 06,50  SAY "O.C."
         @ 06,60  SAY "CODIGO"
         @ 06,70  SAY "ENTRADAS"
         @ 06,90  SAY "SAIDAS"
         @ 06,110 SAY "SALDO"
         @ 06,130 SAY "CONF."
         @ 07,00 SAY REPLICATE("=",135)
         LIN = 8
      ENDIF
   ENDIF
   @ 58,00 SAY REPLICATE("=",135)
   @ 59,104 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
