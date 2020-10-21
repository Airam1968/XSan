*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_TIPO.PRG
* Data        : 01/01/99
* Objetivo    : Relatário de Tipo de Itens de Estoque
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_INDM()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO TAXAS MENSAIS")
   SELE 2
   USE INDICES INDEX INDICES
   GOTO TOP
   SELE 1
   USE TAXA_MES INDEX TAXA_MES
   GOTO TOP
   SELE 2
   eINDICE = SPACE(12)
   PLQUA("01")
   @ 23,02 SAY "Escolha o Indice : "
   GO TOP
   DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="XXXXXXXXXXX"
   CAB[1]="    "
   DBEDIT(23,21,23,34,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eINDICE = NOME
   SELE 1
   GOTO TOP
	eARQUIVO = SPACE(60)
	IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
		eARQUIVO = sisDRIVE_REL+":"
	ELSE
		eARQUIVO = "C:"
	ENDIF	
	
	IF sisPASTA_REL <> SPACE(30)
		eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"INDICESMENSAIS.TXT"+SPACE(10)  
	ELSE
		eARQUIVO = ALLTRIM(eARQUIVO) + "INDICESMENSAIS.TXT"+SPACE(10) 
	ENDIF        
   PLQUA("01")
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   NPAG = 1
   DO WHILE .NOT. EOF()
      @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00  SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SCL001"
      @ 03,30  SAY sisLETRAE+"RELATORIO DE TABELA DE INDICE"+sisLETRAC
      @ 04,00  SAY "Indice : "+ALLTRIM(eINDICE) 
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,00  SAY "|"
      @ 06,02  SAY "MES/ANO"
      @ 06,15  SAY "TAXA"
      @ 06,25  SAY "|"
      @ 06,27  SAY "MES/ANO"
      @ 06,40  SAY "TAXA"
      @ 06,50  SAY "|"
      @ 06,52  SAY "MES/ANO"
      @ 06,65  SAY "TAXA"
      @ 06,75  SAY "|"
      @ 06,77  SAY "MES/ANO"
      @ 06,90  SAY "TAXA"
      @ 06,100 SAY "|"
      @ 06,102 SAY "MES/ANO"
      @ 06,115 SAY "TAXA"
      @ 06,130 SAY "|"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE LIN <= 55 .AND. .NOT. EOF()
         eREGISTRO = RECNO()
         @ LIN,02  SAY MES+"/"+ANO
         @ LIN,15  SAY &eINDICE PICT "@R 999.9999"
         SKIP 48
         @ LIN,25  SAY "|"
         @ LIN,27  SAY MES+"/"+ANO
         @ LIN,40  SAY &eINDICE PICT "@R 999.9999"
         GOTO eREGISTRO
         SKIP 96
         @ LIN,50  SAY "|"
         @ LIN,52  SAY MES+"/"+ANO
         @ LIN,65  SAY &eINDICE PICT "@R 999.9999"
         GOTO eREGISTRO
         SKIP 144
         @ LIN,75  SAY "|"
         @ LIN,77  SAY MES+"/"+ANO
         @ LIN,90  SAY &eINDICE PICT "@R 999.9999"
         GOTO eREGISTRO
         SKIP 192
         @ LIN,100 SAY "|"
         @ LIN,102 SAY MES+"/"+ANO
         @ LIN,115 SAY &eINDICE PICT "@R 999.9999"
         @ LIN,130 SAY "|"
         LIN = LIN + 1
         GOTO eREGISTRO
	 SKIP
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF	
      ENDDO
      GOTO eREGISTRO
      SKIP 240
      NPAG = NPAG + 1
      @ 56,00  SAY REPLICATE("=",132)
      @ 57,80  SAY sisTIT3
      @ 00,00  SAY sisLETRAN+" "
   ENDDO
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************

