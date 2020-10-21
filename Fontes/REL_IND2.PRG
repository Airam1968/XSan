*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_IND2.PRG
* Data        : 01/12/2001
* Objetivo    : Relatário Indices Mensais Acumulados
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_IND2()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO TAXAS MENSAIS ACUMULADAS")
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
   eMES_INI = 0
   eMES_FIM = 0
   eANO_INI = 0
   eANO_FIM = 0
   DO WHILE .T.
      PLQUA("01")
      @ 23,00 SAY "Informe o Mes de Inicio : " GET eMES_INI PICT "99"
      @ 23,30 SAY "Informe o Ano de Inicio : " GET eANO_INI PICT "9999"
      READ
      PLQUA("01")
      @ 23,00 SAY "Informe o Mes de Fim    : " GET eMES_FIM PICT "99"
      @ 23,30 SAY "Informe o Ano de Fim    : " GET eANO_FIM PICT "9999"
      READ
      IF (eANO_FIM < eANO_INI) .AND. (eMES_FIM < eMES_INI)
         PLQUA("01")
         PMSGR( "Valores Inv†lidos !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   eMES_INI   = STR(eMES_INI,2)
   eMES_FIM   = STR(eMES_FIM,2)
   eANO_INI   = STR(eANO_INI,4)
   eANO_FIM   = STR(eANO_FIM,4)
   eMES_INI   = IIF(LEN(ALLTRIM(eMES_INI)) = 1,"0"+ALLTRIM(eMES_INI),ALLTRIM(eMES_INI))
   eMES_FIM   = IIF(LEN(ALLTRIM(eMES_FIM)) = 1,"0"+ALLTRIM(eMES_FIM),ALLTRIM(eMES_FIM))
   eACUMULADO = 1
   SELE 1
   SET FILTER TO (ANO+MES >= eANO_INI+eMES_INI) .AND. (ANO+MES <= eANO_FIM+eMES_FIM)
   GOTO TOP
   IF EOF()
      PLQUA("01")
	  PMENSAG("N∆o H† taxas para o Per°odo selecionado, Tecle [ENTER]")
      PPARA()
      SET DEVICE TO SCREEN
      CLOSE ALL
      RETURN nil
   ENDIF
	eARQUIVO = SPACE(60)
	IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
		eARQUIVO = sisDRIVE_REL+":"
	ELSE
		eARQUIVO = "C:"
	ENDIF	
	
	IF sisPASTA_REL <> SPACE(30)
		eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"INDICES.TXT"+SPACE(10)  
	ELSE
		eARQUIVO = ALLTRIM(eARQUIVO) + "INDICES.TXT"+SPACE(10) 
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
      @ 03,30  SAY sisLETRAE+"RELATORIO DE TABELA DE INDICE ACUMULADOS"+sisLETRAC
      @ 04,00  SAY "Indice : "+ALLTRIM(eINDICE)
      @ 04,40  SAY "Periodo de : "+eMES_INI+"/"+eANO_INI+" ate "+eMES_FIM+"/"+eANO_FIM
      @ 05,00  SAY REPLICATE("=",135)
      @ 06,00  SAY "|"
      @ 06,02  SAY "MES/ANO"
      @ 06,15  SAY "TAXA"
      @ 06,30  SAY "ACUMULADO"
      @ 06,130 SAY "|"
      @ 07,00 SAY REPLICATE("=",135)
      LIN = 8
      DO WHILE LIN <= 55 .AND. .NOT. EOF()
         @ LIN,02  SAY MES+"/"+ANO
         @ LIN,15  SAY &eINDICE   PICT "@R 999.9999"
         eACUMULADO = eACUMULADO * ((&eINDICE / 100) + 1)
         @ LIN,30  SAY ((eACUMULADO - 1) * 100) PICT "@R 999.9999"
         LIN = LIN + 1
	 SKIP
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF	
      ENDDO
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

