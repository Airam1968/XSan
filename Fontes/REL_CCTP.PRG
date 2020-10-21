*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CCTP.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CLIENTES DO CONTAS A RECEBER   
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CCTP()
SAVE SCREEN TO TELA07
PTIT( "RELATORIO CADASTRO DE CLIENTES")
PLQUA("01")
PPERG ( "Deseja o Relat¢rio em Ordem Alfab‚tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE CLIENTES INDEX CLIENTES
   GOTO TOP
ELSE
   SELE 1
   USE CLIENTES 
   INDEX ON NOME TO TEMP_CLI
   GOTO TOP
ENDIF
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
   @ 03,40  SAY sisLETRAE+"CADASTRO DE CLIENTES DO CONTAS A RECEBER"+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "CODIGO"
   @ 06,14  SAY "CLIENTE"
   @ 06,64  SAY "ENDERECO"
   @ 06,114 SAY "TELEFONE"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 57
      @ LIN,02  SAY CODIGO     PICT "@R 99999"
      @ LIN,14  SAY SUBSTR(NOME,1,45)
      @ LIN,64  SAY SUBSTR(END,1,48)
      @ LIN,114 SAY TEL_1 PICT "@R (XX) XXXXX-XXXXX"
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
