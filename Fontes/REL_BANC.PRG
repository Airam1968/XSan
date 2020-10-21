*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_BANC.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE BANCOS CADASTRADOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_BANC()
SAVE SCREEN TO TELA07
PTIT( "RELATORIO DE CADASTRO DE BANCOS")
SELE 1
USE BANCOS INDEX BANCOS
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
   @ 03,40  SAY sisLETRAE+"CADASTRO DE BANCOS"+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "CODIGO FEBRABAM"
   @ 06,30  SAY "NOME BANCO"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 57
      @ LIN,05  SAY COD_BANCO PICT "@R 99999"
      @ LIN,30  SAY NOME_BANCO
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
