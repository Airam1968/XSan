*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CLAD.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE CLASSE DE DESPESAS               
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CLAD()
SAVE SCREEN TO TELA07
PTIT( "RELATORIO CLASSE DE DESPESAS")
PLQUA("01")
PPERG ( "Deseja Consulta por Ordem Alfab‚tica (S/N) ? ")
IF sisRESP = "N"
   SELE 1
   USE CLASSE_D INDEX CLASSE_D
   GOTO TOP
   eINCLUI = .T.
ELSE
   SELE 1
   USE CLASSE_D
   INDEX ON DESCRICAO TO TEM_CLAD
   GOTO TOP
   USE CLASSE_D INDEX TEM_CLAD,CLASSE_D
   GOTO TOP
   eINCLUI = .F.
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
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN007"
   @ 03,40  SAY sisLETRAE+"CADASTRO DE CLASSES DE DESPESAS"+sisLETRAC
   @ 05,00  SAY REPLICATE("=",135)
   @ 06,02  SAY "CODIGO"
   @ 06,14  SAY "DESCRICAO CLASSE"
   @ 07,00 SAY REPLICATE("=",135)
   LIN = 8
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      @ LIN,02  SAY CODIGO     PICT "@R 99999"
      @ LIN,14  SAY DESCRICAO          
      LIN = LIN + 1
      SKIP
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA07
RETURN nil
*************************  fim  *********************************
