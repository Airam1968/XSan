*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CCOR.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CONTAS CORRENTES   
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CCOR()
SAVE SCREEN TO TELA07
PTIT( "RELATORIO CADASTRO DE CONTAS CORRENTES")
SELE 1
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 1
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
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,120 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,121 SAY "Relat: SAN007"
   @ 03,40  SAY sisLETRAE+"CADASTRO DE CONTAS CORRENTES"+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "CODIGO"
   @ 06,14  SAY "COD.BANCO"
   @ 06,28  SAY "COD.AGENCIA"
   @ 06,42  SAY "COD.C/C"
   @ 06,70  SAY "NOME AGENCIA"
   @ 06,100 SAY "ESTADO"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 57
      @ LIN,02  SAY CODIGO     PICT "@R 99999"
      @ LIN,14  SAY COD_BANCO  PICT "@R 99999"
      @ LIN,28  SAY COD_AGENCI PICT "@R 99999"
      @ LIN,42  SAY COD_CC    
      @ LIN,70  SAY NOME_AGENC
      @ LIN,100 SAY ESTADO      
      LIN = LIN + 1
      SKIP
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   @ 58,00 SAY REPLICATE("=",135)
   @ 59,104 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA07
RETURN nil
*************************  fim  *********************************
