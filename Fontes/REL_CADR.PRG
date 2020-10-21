*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CADR.PRG
* Data        : 01/01/99
* Objetivo    : IMPRESSAO DA RELATORIO CADASTRO DE CLIENTES RESUMO
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
************************* inicio ************************************
DO PPERG WITH 'Confirma (S/N) ?'
IF sisRESP = 'S'
   DO PTIT WITH 'RELATORIO CADASTRO RESUMO'
   DO PLQUA WITH '01'
   eTIPO = SPACE(1)
   @ 23,02 SAY 'Ordem Alfab‚tica ou Num‚rica (A/N)  : ' GET eTIPO PICT 'X' VALID eTIPO $('AN')
   READ
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
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
   DO PMENSAG WITH 'Posicione o Formulario 80 Colunas e Tecle [ENTER]'
   DO PPARA
   IF eTIPO = 'A'
      SELECT 1
      USE CONTATO INDEX CONTATO,NUM_CLIE
      GO TOP
*     SKIP
   ELSE
      SELECT 1
      USE CONTATO INDEX NUM_CLIE,CONTATO
      GO TOP
*     SKIP
   ENDIF
   IF EOF()
      PLQUA("01")
      PMSGR( "Aten‡Æo !",3)
      PMENSAG("NÆo H  Relat¢rio para esta Sele‡Æo !")
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
      @ 01,00 SAY sisLETRAC+'Empresa : '+sisTIT1
      @ 01,115 SAY 'Data : '+dtoc(DATE())
      @ 02,00 SAY 'Sistema : '+sisTIT2
      @ 02,115 SAY 'Relat: SAN001'
      @ 03,40 SAY sisLETRAE+'CADASTRO DE CLIENTES RESUMO'+sisLETRAC
      @ 04,00 SAY REPLICATE('=',135)
      @ 05,02  SAY 'CODIGO'
      @ 05,10  SAY 'NOME'
      @ 05,50  SAY 'ENDERECO'
      @ 05,90  SAY 'CIDADE'
      @ 05,112 SAY 'TELEFONE'
      @ 06,00 SAY REPLICATE('=',135)
      LIN = 7
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         @ LIN,01  SAY NUM_CLIE PICT '@R 99999999'
         @ LIN,10  SAY ALLTRIM(CLIENTE) PICT '@S39'
         IF OPCAO = 'R'
            @ LIN,50  SAY ALLTRIM(END_R) PICT '@S39'
            @ LIN,90  SAY ALLTRIM(CIDADE_R) PICT '@S21'
            @ LIN,112 SAY TEL_R_1 PICT '@R (XX) XXXXX-XXXXX - XXXXXX'
            @ LIN,133 SAY 'R'
         ELSE
            @ LIN,50  SAY ALLTRIM(END_C) PICT '@S39'
            @ LIN,90  SAY ALLTRIM(CIDADE_C) PICT '@S21'
            @ LIN,112 SAY TEL_C_1 PICT '@R (XX) XXXXX-XXXXX - XXXXXX'
            @ LIN,133 SAY 'C'
         ENDIF
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
      @ 56,00 SAY REPLICATE('=',135)
      @ 57,95 SAY sisTIT3
*      @ 01,00 SAY ' '+sisLETRAN
   ENDDO
ENDIF
IF eSAIDA = "ARQUIVO"
   SET PRINT TO
   SET PRINT OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINT TO
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
RETURN nil
***************************** fim ***********************************
