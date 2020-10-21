*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_RECA.PRG
* Data        : 01/01/99
* Objetivo    : Relatorio dos Resumo de Contas         
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_RECA()
SAVE SCREEN TO TELA04
PTIT( "RELATORIO DE RESUMO DE CONTAS")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA04
   RETURN nil
ENDIF
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
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,02 SAY "Data Resumo : " GET eDATA_INI
   READ
   IF eDATA_INI = CTOD("") 
      PLQUA("01")
      PMSGR( "Data Invalida !",3)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	
IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"RESUMO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "RESUMO.TXT"+SPACE(10) 
ENDIF    

eSAIDA   = SPACE(10)
DO WHILE .T.
   PLQUA("01")
   PPERG ( "Deseja extrato em arquivo texto no Drive (X:) (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
	  @ 23,00 SAY "CONFIRME O DESTINO  : " GET eARQUIVO
      READ
      eARQUIVO = ALLTRIM(eARQUIVO)
      eSAIDA = "ARQUIVO"
      EXIT
   ELSE
      DO PIMPR
      eSAIDA = "IMPRESSORA"
      EXIT
   ENDIF
ENDDO
SELE 2
GOTO TOP
      IF eSAIDA = "ARQUIVO"
         SET PRINT ON
         SET DEVICE TO PRINT
         SET PRINTER TO &eARQUIVO
      ELSE
         PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
         PPARA()
         DO PIMPR
         SET DEVICE TO PRINT
      ENDIF
eSUB_SALD = 0
eSUB_APLD = 0
eSUB_APLB = 0
eSUB_TOTG = 0
eTOT_TOTG = 0
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,30  SAY sisLETRAE+"RESUMO DE CONTAS DE "+DTOC(eDATA_INI)+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "DATA"
   @ 06,10  SAY "CONTA"
   @ 06,20  SAY "SALDO FINAL"
   @ 06,40  SAY "APLIC. DESBL."
   @ 06,60  SAY "APLIC. BLOQ."
   @ 06,80  SAY "TOTAL GERAL"
   @ 07,00  SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      SELE 2
      eCONT  = STR(CODIGO,5)
      eZERO = "00"
      eARQ  = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT) 
      eIND1 = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONT))))+ALLTRIM(eCONT)
      SELE 6
      USE &eARQ INDEX &eIND1
      GOTO TOP
      SEEK eDATA_INI
      IF FOUND()
         DO WHILE DATA = eDATA_INI .OR. .NOT. EOF()
            SKIP
         ENDDO
         SKIP -1
         @ LIN,00  SAY DATA
         @ LIN,10  SAY CONTA      PICT "999999"
         @ LIN,20  SAY SALDO_F    PICT "@R 999,999,999.99"
         @ LIN,40  SAY APLICACAOD PICT "@R 999,999,999.99"
         @ LIN,60  SAY APLICACAOB PICT "@R 999,999,999.99"
         eSUB_TOTG = 0
         eSUB_TOTG = (SALDO_F + APLICACAOD + APLICACAOB)
         @ LIN,80  SAY eSUB_TOTG  PICT "@R 999,999,999.99"
         LIN = LIN + 1
         eSUB_SALD = eSUB_SALD + SALDO_F 
         eSUB_APLD = eSUB_APLD + APLICACAOD
         eSUB_APLB = eSUB_APLB + APLICACAOB
         eTOT_TOTG = eTOT_TOTG + eSUB_TOTG
      ELSE
         SKIP -1
         @ LIN,00  SAY DATA
         @ LIN,10  SAY CONTA      PICT "999999"
         @ LIN,20  SAY SALDO_F    PICT "@R 999,999,999.99"
         @ LIN,40  SAY APLICACAOD PICT "@R 999,999,999.99"
         @ LIN,60  SAY APLICACAOB PICT "@R 999,999,999.99"
         eSUB_TOTG = 0
         eSUB_TOTG = (SALDO_F + APLICACAOD + APLICACAOB)
         @ LIN,80  SAY eSUB_TOTG  PICT "@R 999,999,999.99"
         LIN = LIN + 1
         eSUB_SALD = eSUB_SALD + SALDO_F 
         eSUB_APLD = eSUB_APLD + APLICACAOD
         eSUB_APLB = eSUB_APLB + APLICACAOB
         eTOT_TOTG = eTOT_TOTG + eSUB_TOTG
      ENDIF
      SELE 2
      SKIP
            DO PTECLA
            IF sisINTERR
               IF eSAIDA = "ARQUIVO"
                  SET PRINT OFF
                  SET DEVICE TO SCREEN
               ELSE
                  SET DEVICE TO SCREEN
               ENDIF
               CLOSE ALL
               RETURN nil
            ENDIF
   ENDDO
   IF EOF()
      @ LIN,00  SAY REPLICATE("_",135)
      LIN = LIN + 1
      @ LIN,02  SAY "Totais  : "
      @ LIN,20  SAY eSUB_SALD  PICT "@R 999,999,999.99"
      @ LIN,40  SAY eSUB_APLD  PICT "@R 999,999,999.99"
      @ LIN,60  SAY eSUB_APLB  PICT "@R 999,999,999.99"
      @ LIN,80  SAY eTOT_TOTG  PICT "@R 999,999,999.99"
      LIN = LIN + 2
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
