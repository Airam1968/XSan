*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_EMPR.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE EMPREENDIMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_EMPR()
SAVE SCREEN TO TELA03
PTIT( "RELATORIO DE EMPREENDIMENTOS")
SELE 1
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 2
USE TIPO_PRO INDEX TIPO_PRO
GOTO TOP
SELE 1
GOTO TOP

eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"PRODUTO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "PRODUTO.TXT"+SPACE(10) 
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
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† Extrato Deste Per°odo !")
      PPARA()
   ELSE
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
   ENDIF   
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,120 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,121 SAY "Relat: SAN007"
   @ 03,40  SAY sisLETRAE+"RELATORIO DE EMPREENDIMENTOS"+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "CODIGO"
   @ 06,15  SAY "TIPO"
   @ 06,45  SAY "NOME PRODUTO"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 57
      @ LIN,02  SAY CODIGO PICT "@R 99999"
      eCODIGO = TIPO
      SELE 2
      GOTO TOP
      SEEK eCODIGO
      @ LIN,15  SAY DESCRICAO
      SELE 1
      @ LIN,45  SAY DESCRICAO
      LIN = LIN + 1
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
   @ 58,00 SAY REPLICATE("=",135)
   @ 59,104 SAY sisTIT3
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
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
